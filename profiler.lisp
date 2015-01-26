;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defmacro with-profiling (options &body body)
  `(%with-profiling (lambda () ,@body) ,@options))

;;; This gets baked into the profile interrupt handler.
(defvar *profile-buffer* nil
  "A cons of (simple-array . fixnum), holding the buffer itself and
an offset into the buffer. Should be allocated in static space.")

(defvar *profile-sample-interval* 1000)

(defvar *profile-buffer-default-size* 1048000
  "Number of entries to store in the profile buffer.")

(defun initialize-profile-buffer ()
  (setf *profile-buffer* (cons-in-area (make-array *profile-buffer-default-size*
                                                   :area :static)
                                       0
                                       :static)))

(defun reset-profiler ()
  (when *profile-buffer*
    (setf (cdr *profile-buffer*) 0))
  t)

(define-interrupt-handler profile-timer-interrupt (buffer)
  (let* ((sg (stack-group-resumer (current-stack-group)))
         (buffer-array (car buffer))
         (buffer-size (%simple-1d-array-length buffer-array))
         (start (cdr buffer))
         (position start)
         (sg-pointer (ash (%pointer-field sg) 4))
         (csp (memref-unsigned-byte-64 sg-pointer 3))
         (cfp (memref-unsigned-byte-64 csp 0)))
    ;; Skip the stack frame created by the interrupt handler.
    (setf cfp (memref-unsigned-byte-64 cfp 0))
    ;; Must have room for at least one buffer entry.
    (when (< start buffer-size)
      ;; Set the first value to the entry length.
      (setf (svref buffer-array position) 0)
      (incf position)
      (flet ((push-value (value)
               (when (< position buffer-size)
                 (setf (svref buffer-array position) value)
                 (incf (cdr buffer))
                 (incf (svref buffer-array start))
                 (incf position))))
        (push-value sg)
        ;; Iterate over the stack.
        (do ((i 0 (1+ i))
             (fp cfp (memref-unsigned-byte-64 fp 0)))
            ((= fp 0))
          (push-value (memref-t fp -2)))
        ;; Push NIL to terminate.
        (push-value nil)))))

(defun install-profile-handler (buffer)
  ;; PC PIT.
  (setf (isa-pic-interrupt-handler 0) (make-interrupt-handler 'profile-timer-interrupt buffer)))

(defun %with-profiling (fn)
  (unless *profile-buffer*
    (initialize-profile-buffer)
    (install-profile-handler *profile-buffer*))
  (unwind-protect
       (progn
         (configure-pit *profile-sample-interval*)
         ;; Enable PIT IRQ, starting the profiler.
         (setf (isa-pic-irq-mask 0) nil)
         (funcall fn))
    ;; Disable the PIT IRQ, stopping the profiler.
    (setf (isa-pic-irq-mask 0) t)))

(defun profiler-data (&optional (buffer *profile-buffer*))
  (do ((i 0 (1+ i))
       (current-thing '())
       (entries '()))
      ((>= i (cdr buffer))
       (when current-thing
         (push current-thing entries))
       (nreverse entries))
    (cond ((and (not (equal (aref (car buffer) i) "NIL"))
                (or (stringp (aref (car buffer) i))
                    (functionp (aref (car buffer) i))))
           (push (aref (car buffer) i) current-thing))
          (current-thing
           (push current-thing entries)
           (setf current-thing '())))))

(defun treeify-profile (profile)
  (let ((top-level (make-hash-table :test 'equal)))
    (labels ((insert-element (current e)
               (when e
                 (multiple-value-bind (node nodep)
                     (gethash (first e) current)
                   (unless nodep
                     (setf node (list 0 0 (make-hash-table :test 'equal))
                           (gethash (first e) current) node))
                   (if (rest e)
                       (incf (first node))
                       (incf (second node)))
                   (insert-element (third node) (rest e))))))
      (mapc (lambda (e) (insert-element top-level e)) profile))
    top-level))

(defun graph-tree (tree-hash &key (minimum 0))
  (let ((total-samples 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (incf total-samples (first v))
               (incf total-samples (second v)))
             tree-hash)
    (labels ((frob (tree-hash depth)
               (let ((data '()))
                 (maphash (lambda (k v) (push (list* k v) data)) tree-hash)
                 (setf data (sort data '> :key (lambda (x) (+ (second x) (third x)))))
                 (dolist (x data)
                   (let ((total (+ (second x) (third x))))
                     (when (and minimum (>= total minimum))
                       (dotimes (i depth) (write-char #\Space))
                       (format t "~A ~D ~D  ~D%~%" (first x) (second x) (third x)
                               (/ (* total 100) total-samples))
                       (frob (fourth x) (1+ depth))))))))
      (frob tree-hash 0))))

(defun graph-profile (&rest args)
  (apply 'graph-tree (treeify-profile (profiler-data)) args))

(defun dump-profile-data (&optional stream)
  (when *profile-buffer*
    (let ((buf (car *profile-buffer*))
          (n-elts (cdr *profile-buffer*)))
      (dotimes (i n-elts)
        (let ((value (aref buf i)))
          (typecase value
            (integer (write value :stream stream))
            (t (write (format nil "~S" value) :stream stream)))
          (terpri stream))))))

(defun read-profile (path)
  (let ((output '())
        (current '()))
    (with-open-file (file path)
      (loop (let ((thing (read file nil)))
              (etypecase thing
                (null (return))
                (integer
                 ;; Start a new entry.
                 (when current
                   (push current output)
                   (setf current '()))
                 ;; First entry is the stack group, skip it.
                 (read file))
                (string
                 (push thing current))))))
    (when current (push current output))
    (nreverse output)))

(defun profile-caller-to-callees (prof)
  (let ((info (make-hash-table :test 'equal)))
    (dolist (entry prof)
      (do ((i entry (cdr i)))
          ((null (cdr i)))
        (pushnew (second i) (gethash (first i) info) :test #'string=)))
    info))

(defun profiler-caller-callee-edge-weights (prof)
  (let ((info (make-hash-table :test 'equal)))
    (dolist (entry prof)
      (do ((i entry (cdr i)))
          ((null (cdr i)))
        (incf (gethash (cons (first i) (second i)) info 0))))
    info))

(defun profiler-call-counts (prof)
  (let ((info (make-hash-table :test 'equal)))
    (dolist (entry prof)
      (dolist (subentry entry)
        (incf (gethash subentry info 0))))
    info))

(defun profiler-leaf-call-counts (prof)
  (let ((info (make-hash-table :test 'equal)))
    (dolist (entry prof)
      (incf (gethash (first (last entry)) info 0)))
    info))

(defun save-profile (&optional (file "profile.data"))
  (with-open-file (s file :direction :output :if-exists :supersede)
    (dump-profile-data s)))
