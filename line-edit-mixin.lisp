(defpackage :mezzanine.line-editor
  (:use :cl)
  (:export #:line-edit-mixin
           #:*line-editor-command-table*
           #:define-command
           #:buffer
           #:cursor-position))

(in-package :mezzanine.line-editor)

(defclass line-edit-mixin ()
  ((%line-buffer :initform nil :accessor buffer)
   (%cursor-position :accessor cursor-position)
   (%output-progress :initform nil :accessor output-progress)
   (%line-start-position :accessor line-start-position)
   (%line-end-position :accessor line-end-position)))

(defvar *line-editor-command-table* (make-hash-table))

(defmacro define-command (name (stream keys) &body body)
  (when (not (listp keys))
    (setf keys (list keys)))
  `(progn
     (defun ,name (,stream)
       ,@body)
     ,@(loop for key in keys
          collect `(setf (gethash ',key *line-editor-command-table*) ',name))))

(define-command forward-char (stream (#\C-F #\Right-Arrow))
  "Move forward one character."
  (unless (eql (cursor-position stream) (length (buffer stream)))
    (incf (cursor-position stream))))

(define-command backward-char (stream (#\C-B #\Left-Arrow))
  "Move backward one character."
  (unless (zerop (cursor-position stream))
    (decf (cursor-position stream))))

(defun next-word (stream position)
  "Starting from POSITION, return the end of the next word."
  ;; Move past non-alphanumeric characters.
  (loop
     (when (or (zerop position)
               (alphanumericp (char (buffer stream) position)))
       (return))
     (incf position))
  ;; Now past alphanumeric characters.
  (loop
     (when (or (zerop position)
               (not (alphanumericp (char (buffer stream) position))))
       (return))
     (incf position))
  position)

(defun previous-word (stream position)
  "Starting from POSITION, return the start of the previous word."
  ;; Move past non-alphanumeric characters.
  (loop
     (when (or (zerop position)
               (alphanumericp (char (buffer stream) (1- position))))
       (return))
     (decf position))
  ;; Now past alphanumeric characters.
  (loop
     (when (or (zerop position)
               (not (alphanumericp (char (buffer stream) (1- position)))))
       (return))
     (decf position))
  position)

(define-command forward-word (stream (#\M-F #\M-Right-Arrow))
  "Move forward one word."
  (setf (cursor-position stream) (next-word stream (cursor-position stream))))

(define-command backward-word (stream (#\M-B #\M-Left-Arrow))
  "Move backward one word."
  (setf (cursor-position stream) (previous-word stream (cursor-position stream))))

(define-command move-end-of-line (stream (#\C-E #\End))
  "Move to end of line."
  (setf (cursor-position stream) (length (buffer stream))))

(define-command move-beginning-of-line (stream (#\C-A #\Home))
  "Move the start of line."
  (setf (cursor-position stream) 0))

(define-command forward-delete-char (stream (#\Delete #\C-D))
  "Delete one character forward."
  (unless (eql (cursor-position stream) (length (buffer stream)))
    (replace (buffer stream) (buffer stream)
             :start1 (cursor-position stream)
             :start2 (1+ (cursor-position stream)))
    (decf (fill-pointer (buffer stream)))))

(define-command backward-delete-char (stream #\Backspace)
  "Delete one character backward."
  (unless (zerop (cursor-position stream))
    ;; Shuffle characters backward.
    (decf (cursor-position stream))
    (replace (buffer stream) (buffer stream)
             :start1 (cursor-position stream)
             :start2 (1+ (cursor-position stream)))
    (decf (fill-pointer (buffer stream)))))

(define-command forward-delete-word (stream #\M-D)
  "Delete one word forward."
  (let ((end (next-word stream (cursor-position stream))))
    (replace (buffer stream) (buffer stream)
             :start1 (cursor-position stream)
             :start2 end)
    (decf (fill-pointer (buffer stream)) (- end (cursor-position stream)))))

(define-command backward-delete-word (stream (#\C-Backspace #\C-W))
  "Delete one word backward."
  (let ((start (previous-word stream (cursor-position stream))))
    (replace (buffer stream) (buffer stream)
             :start1 start
             :start2 (cursor-position stream))
    (decf (fill-pointer (buffer stream)) (- (cursor-position stream) start))
    (setf (cursor-position stream) start)))

(define-command delete-to-end-of-line (stream #\C-K)
  "Delete characters from the cursor to the end of the line."
  (unless (eql (cursor-position stream) (length (buffer stream)))
    (setf (fill-pointer (buffer stream)) (cursor-position stream))))

(define-command break-into-debugger (stream #\C-C)
  "Enter the debugger using BREAK."
  (break))

(define-command abort-input (stream #\C-G)
  "Abort input, call the nearest ABORT restart."
  (when (find-restart 'abort)
    (setf (buffer stream) nil)
    (abort)))

(defmethod sys.gray:stream-unread-char ((stream line-edit-mixin) character)
  (decf (output-progress stream)))

(defun redraw-line (stream)
  (destructuring-bind (x y)
      (line-start-position stream)
    (sys.int::stream-clear-between stream x y (first (line-end-position stream)) (second (line-end-position stream)))
    (sys.int::stream-move-to stream x y)
    (dotimes (i (cursor-position stream))
      (write-char (char (buffer stream) i) stream))
    (multiple-value-bind (cx cy)
        (sys.int::stream-cursor-pos stream)
      (dotimes (i (- (length (buffer stream)) (cursor-position stream)))
        (write-char (char (buffer stream) (+ (cursor-position stream) i)) stream))
      (setf (line-end-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream)))
      (sys.int::stream-move-to stream cx cy))))

(defmethod sys.gray:stream-read-char :around ((stream line-edit-mixin))
  (cond ((and (output-progress stream)
              (not (eql (output-progress stream) (length (buffer stream)))))
         (prog1
             (char (buffer stream) (output-progress stream))
           (incf (output-progress stream))))
        (t (when (or (not (buffer stream))
                     (eql (output-progress stream) (length (buffer stream))))
             ;; Starting or restarting a line read.
             (setf (output-progress stream) nil
                   (cursor-position stream) 0
                   (buffer stream) (make-array 50 :element-type 'character :fill-pointer 0 :adjustable t)
                   (line-start-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream))
                   (line-end-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream))))
           (loop
              (let* ((ch (call-next-method))
                     (fn (gethash ch *line-editor-command-table*)))
                (cond ((eql ch #\Newline) ;; Submit the line.
                       ;; ### Maybe turn this into a proper command.
                       (when (not (eql (cursor-position stream) (length (buffer stream))))
                         (setf (cursor-position stream) (length (buffer stream)))
                         (redraw-line stream))
                       (vector-push-extend #\Newline (buffer stream))
                       (write-char #\Newline stream)
                       (setf (output-progress stream) 0)
                       (return (sys.gray:stream-read-char stream)))
                      (fn
                       (funcall fn stream)
                       (redraw-line stream))
                      ((not (graphic-char-p ch))) ; ignore non-graphics characters.
                      ((eql (cursor-position stream) (length (buffer stream)))
                       ;; At end of line, don't need to redraw.
                       (vector-push-extend ch (buffer stream))
                       (write-char ch stream)
                       (incf (cursor-position stream))
                       (setf (line-end-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream))))
                      (t ;; Writing into the middle of the line requires a redraw.
                       ;; Ensure space.
                       (vector-push-extend ch (buffer stream))
                       ;; Shuffle.
                       (replace (buffer stream) (buffer stream)
                                :start1 (1+ (cursor-position stream))
                                :start2 (cursor-position stream))
                       ;; Insert character.
                       (setf (char (buffer stream) (cursor-position stream)) ch)
                       ;; Advance cursor.
                       (incf (cursor-position stream))
                       ;; Redraw.
                       (redraw-line stream))))))))
