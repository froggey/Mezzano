(in-package #:sys.int)

(declaim (special * ** ***
                  + ++ +++
                  / // ///
                  -))

(defun repl ()
  (let ((* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil)
        (+ nil) (++ nil) (+++ nil)
        (- nil))
    (loop
       (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
         (fresh-line)
         (write-char #\>)
         (let ((form (read)))
           (fresh-line)
           (let ((result (multiple-value-list (let ((- form))
                                                (eval form)))))
             (setf *** **
                   ** *
                   * (first result)
                   /// //
                   // /
                   / result
                   +++ ++
                   ++ +
                   + form)
             (when result
               (dolist (v result)
                 (fresh-line)
                 (write v)))))))))

(defun go ()
  (sys.net::net-setup)
  (sys.graphics::register-screen :bochs *bochs-framebuffer* *bochs-back-buffer* 'bochs-flip-buffer)
  (sys.graphics::invoke-graphics))

(defun hexdump-range (start end &optional stream)
  (when (< start end)
    (write-sequence (make-array (- end start)
                                :element-type '(unsigned-byte 8)
                                :memory (+ #x8000000000 start))
                    stream)))

(defun colourful-write-string (string)
  (let ((n-colours (length sys.graphics::*colour-names*)))
    (dotimes (i (length string))
      (let* ((ch (char string i))
             (glyph (map-unifont ch)))
        (when glyph
          (sys.graphics::bitset-argb-xrgb-mask-1 16 16 (cdr (nth (rem i n-colours) sys.graphics::*colour-names*))
                                                 (make-array (list 16 (truncate (length glyph) 16)) :displaced-to glyph) 0 0
                                                 *bochs-framebuffer* 50 (+ 50 (* i 16))))))))

(defmacro with-deferred-gc (options &body body)
  "Execute BODY with the garbage collector deferred.
BODY must not allocate!"
  `(progn ,@body))


(defgeneric documentation (x doc-type))
(defgeneric (setf documentation) (new-value x doc-type))

(defmethod documentation (x doc-type) nil)
(defmethod (setf documentation) (new-value x doc-type) new-value)

;; (%cpuid leaf ecx) -> eax ebx ecx edx
(sys.int::define-lap-function %cpuid ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax 3)
  (sys.lap-x86:mov64 :rcx :r9)
  (sys.lap-x86:sar64 :rcx 3)
  (sys.lap-x86:cpuid)
  (sys.lap-x86:lea64 :r8 ((:rax 8)))
  (sys.lap-x86:lea64 :r9 ((:rbx 8)))
  (sys.lap-x86:lea64 :r10 ((:rcx 8)))
  (sys.lap-x86:lea64 :r11 ((:rdx 8)))
  (sys.lap-x86:mov32 :ecx #.(ash 4 3))
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun cpuid (leaf &optional (rcx 0))
  (check-type leaf (unsigned-byte 32))
  (check-type rcx (unsigned-byte 32))
  (%cpuid leaf rcx))

(defun make-string (size &key initial-element (element-type 'character))
  (if initial-element
      (make-array size :element-type element-type :initial-element initial-element)
      (make-array size :element-type element-type)))

(defvar *loaded-adsdf-systems* '())

(defun map (result-type function first-sequence &rest more-sequences)
  (let* ((sequences (cons first-sequence more-sequences))
         (n-results (reduce 'min (mapcar 'length sequences))))
    (flet ((map-body (accum-fn)
             (dotimes (i n-results)
               (funcall accum-fn
                        (apply function
                               (mapcar (lambda (seq)
                                         (elt seq i))
                                       sequences))))))
      (cond ((null result-type)
             ;; No result is accumulated, NIL is returned.
             (map-body (lambda (value) (declare (ignore value)))))
            ((subtypep result-type 'list)
             ;; Generating a list.
             (let* ((head (cons nil nil))
                    (tail head))
               (map-body (lambda (value)
                           (setf (cdr tail) (cons value nil)
                                 tail (cdr tail))))
               (cdr head)))
            ((subtypep result-type 'vector)
             (multiple-value-bind (element-type array-dimensions)
                 (parse-array-type (typeexpand result-type))
               (when (eql element-type '*) (setf element-type 't))
             (let* ((expected-length (cond ((eql array-dimensions '*) n-results)
                                           ((eql (first array-dimensions) '*) n-results)
                                           (t (first array-dimensions))))
                    (result-vector (make-array n-results :element-type (if (eql element-type '*) 't element-type)))
                    (position 0))
               (unless (eql n-results expected-length)
                 (error 'simple-type-error
                        :expected-type `(eql ,n-results)
                        :datum expected-length
                        :format-control "Result-type restricted to ~D elements, but ~D elements provided"
                        :format-arguments (list expected-length n-results)))
               (map-body (lambda (value)
                           (setf (aref result-type position) value)
                           (incf position)))
               result-vector)))
            (t (error "~S is not a subtype of SEQUENCE." result-type))))))
