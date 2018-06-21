;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

;;; I/O customization variables.

(defclass cold-stream (sys.gray:fundamental-character-input-stream
                       sys.gray:fundamental-character-output-stream)
  ())

(defparameter *cold-stream* (make-instance 'cold-stream))

(defparameter *terminal-io* *cold-stream*
  "A bi-directional stream connected to the user's console.")
(defparameter *debug-io* (make-synonym-stream '*terminal-io*)
  "Interactive debugging stream.")
(defparameter *error-output* (make-synonym-stream '*terminal-io*)
  "Warning and non-interactive error stream.")
(defparameter *query-io* (make-synonym-stream '*terminal-io*)
  "User interaction stream.")
(defparameter *standard-input* (make-synonym-stream '*terminal-io*)
  "Default input stream.")
(defparameter *standard-output* (make-synonym-stream '*terminal-io*)
  "Default output stream.")
(defparameter *trace-output* (make-synonym-stream '*terminal-io*))

(setf (symbol-global-value '*terminal-io*) *cold-stream*)

;;; Cold stream methods.

(defmethod sys.gray:stream-read-char ((stream cold-stream))
  (or (cold-read-char stream) :eof))

(defmethod sys.gray:stream-listen ((stream cold-stream))
  (cold-listen stream))

(defmethod sys.gray:stream-unread-char ((stream cold-stream) character)
  (cold-unread-char character stream))

(defmethod sys.gray:stream-write-char ((stream cold-stream) character)
  (cold-write-char character stream))

(defmethod sys.gray:stream-clear-input ((stream cold-stream))
  (cold-clear-input stream))

(defmethod sys.gray:stream-start-line-p ((stream cold-stream))
  (cold-start-line-p stream))

(defmethod sys.gray:stream-line-column ((stream cold-stream))
  (cold-line-column stream))

(defmethod sys.gray:stream-line-length ((stream cold-stream))
  (cold-line-length stream))

(defun streamp (object)
  (typep object 'stream))

(defgeneric stream-with-edit (stream fn))
(defgeneric stream-cursor-pos (stream))
(defgeneric stream-character-width (stream character))
(defgeneric stream-compute-motion (stream string &optional start end initial-x initial-y))
(defgeneric stream-clear-between (stream start-x start-y end-x end-y))
(defgeneric stream-move-to (stream x y))

(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (body-forms declares)
      (parse-declares body)
    `(let ((,var ,stream))
       (declare ,@declares)
       (unwind-protect
            (progn ,@body-forms)
         (close ,var)))))

(defmacro with-open-file ((var filespec &rest options) &body body)
  (multiple-value-bind (body-forms declares)
      (parse-declares body)
    (let ((abortp (gensym "ABORTP")))
      `(let ((,var (open ,filespec ,@options))
             (,abortp t))
         (declare ,@declares)
         (unwind-protect
              (multiple-value-prog1
                  (progn ,@body-forms)
                (setf ,abortp nil))
           (when ,var
             (close ,var :abort ,abortp)))))))

(defun frob-stream (stream default)
  (cond ((eql stream 'nil)
         default)
        ((eql stream 't)
         *terminal-io*)
        (t
         ;; TODO: check that the stream is open.
         (check-type stream stream)
         stream)))

(defun frob-input-stream (stream)
  (frob-stream stream *standard-input*))

(defun frob-output-stream (stream)
  (frob-stream stream *standard-output*))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((b (sys.gray:stream-read-byte (frob-input-stream stream))))
    (check-type b (or integer (eql :eof)))
    (if (eql b :eof)
        (if eof-error-p
            (error 'end-of-file :stream stream)
            eof-value)
        b)))

(defun write-byte (byte stream)
  (sys.gray:stream-write-byte (frob-output-stream stream) byte))

(defun read-sequence (sequence stream &key (start 0) end)
  (sys.gray:stream-read-sequence (frob-input-stream stream) sequence
                                 start end))

(defun write-sequence (sequence stream &key (start 0) end)
  (sys.gray:stream-write-sequence (frob-output-stream stream) sequence
                                  start end))

(defun file-position (stream &optional (position-spec nil position-spec-p))
  (check-type stream stream)
  (cond (position-spec-p
         (check-type position-spec (or (integer 0) (member :start :end)))
         (sys.gray:stream-file-position stream position-spec))
        (t
         (sys.gray:stream-file-position stream))))

(defun file-length (stream)
  (check-type stream stream)
  (sys.gray:stream-file-length stream))

(defun file-string-length (stream object)
  (check-type stream stream)
  (check-type object (or string character))
  (when (characterp object)
    (setf object (string object)))
  (sys.gray:stream-file-string-length stream object))

(defmacro with-stream-editor ((stream recursive-p) &body body)
  "Activate the stream editor functionality for STREAM."
  `(%with-stream-editor ,stream ,recursive-p (lambda () (progn ,@body))))

(defun %with-stream-editor (stream recursive-p fn)
  (cond ((typep stream 'synonym-stream)
         (%with-stream-editor (symbol-value (synonym-stream-symbol stream)) recursive-p fn))
        (recursive-p
         (funcall fn))
        (t (stream-with-edit stream fn))))

(defmethod stream-with-edit ((stream stream) fn)
  (funcall fn))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let* ((s (frob-input-stream stream))
         (c (sys.gray:stream-read-char s)))
    (check-type c (or character (eql :eof)))
    (cond ((eql c :eof)
           (when eof-error-p
             (error 'end-of-file :stream s))
           eof-value)
          (c))))

(defun read-char-no-hang (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let* ((s (frob-input-stream stream))
         (c (sys.gray:stream-read-char-no-hang s)))
    (check-type c (or character (eql :eof) null))
    (cond ((eql c :eof)
           (when eof-error-p
             (error 'end-of-file :stream s))
           eof-value)
          (c))))

(defun read-line (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (setf input-stream (frob-input-stream input-stream))
  (multiple-value-bind (line missing-newline-p)
      (sys.gray:stream-read-line input-stream)
    (if (and (zerop (length line))
             missing-newline-p)
        (if eof-error-p
            (error 'end-of-file :stream input-stream)
            (values eof-value t))
        (values line missing-newline-p))))

(defun unread-char (character &optional stream)
  (let ((s (frob-input-stream stream)))
    (check-type character character)
    (sys.gray:stream-unread-char s character)
    nil))

(defun peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  (check-type peek-type (or (eql t) (eql nil) character))
  (let ((s (frob-input-stream stream)))
    (cond ((eql peek-type nil)
           (let ((ch (sys.gray:stream-peek-char s)))
             (cond ((eql ch :eof)
                    (if eof-error-p
                        (error 'end-of-file :stream s)
                        eof-value))
                   (t ch))))
          ((eql peek-type t)
           (loop
              for ch = (read-char s eof-error-p nil recursive-p)
              until (or (not ch)
                        (not (sys.int::whitespace[2]p ch)))
              finally (return (cond (ch
                                     (unread-char ch s)
                                     ch)
                                    (t
                                     eof-value)))))
          ((characterp peek-type)
           (loop
              for ch = (read-char s eof-error-p nil recursive-p)
              until (or (not ch)
                        (char= ch peek-type))
              finally (return (cond (ch
                                     (unread-char ch s)
                                     ch)
                                    (t
                                     eof-value))))))))

(defun clear-input (&optional stream)
  (sys.gray:stream-clear-input (frob-input-stream stream))
  nil)

(defun finish-output (&optional output-stream)
  (sys.gray:stream-finish-output (frob-output-stream output-stream))
  nil)

(defun force-output (&optional output-stream)
  (sys.gray:stream-force-output (frob-output-stream output-stream))
  nil)

(defun clear-output (&optional output-stream)
  (sys.gray:stream-clear-output (frob-output-stream output-stream))
  nil)

(defun write-char (character &optional stream)
  (let ((s (frob-output-stream stream)))
    (check-type character character)
    (sys.gray:stream-write-char s character)
    character))

(defun start-line-p (&optional stream)
  (sys.gray:stream-start-line-p (frob-output-stream stream)))

(defun advance-to-column (column &optional stream)
  (sys.gray:stream-advance-to-column (frob-output-stream stream) column))

(defun line-column (&optional stream)
  (sys.gray:stream-line-column (frob-output-stream stream)))

(defun line-length (&optional stream)
  (sys.gray:stream-line-length (frob-output-stream stream)))

(defun listen (&optional input-stream)
  (sys.gray:stream-listen (frob-input-stream input-stream)))

(defclass case-correcting-stream (sys.gray:fundamental-character-output-stream)
  ((stream :initarg :stream)
   (case :initarg :case)
   (position :initform :initial))
  (:documentation "Convert all output to the specified case.
CASE may be one of:
:UPCASE - Convert to uppercase.
:DOWNCASE - Convert to lowercase.
:INVERT - Invert the case.
:TITLECASE - Capitalise the start of each word, downcase the remaining letters.
:SENTENCECASE - Capitalise the start of the first word."))

(defun make-case-correcting-stream (stream case)
  (make-instance 'case-correcting-stream
                 :stream stream
                 :case case))

(defun case-correcting-write (character stream)
  (ecase (slot-value stream 'case)
    (:upcase (write-char (char-upcase character) (slot-value stream 'stream)))
    (:downcase (write-char (char-downcase character) (slot-value stream 'stream)))
    (:invert (write-char (if (upper-case-p character)
                             (char-downcase character)
                             (char-upcase character))
                         (slot-value stream 'stream)))
    (:titlecase
     (ecase (slot-value stream 'position)
       ((:initial :after-word)
        (if (alphanumericp character)
            (progn
              (setf (slot-value stream 'position) :mid-word)
              (write-char (char-upcase character) (slot-value stream 'stream)))
            (write-char character (slot-value stream 'stream))))
       (:mid-word
        (unless (alphanumericp character)
          (setf (slot-value stream 'position) :after-word))
        (write-char (char-downcase character) (slot-value stream 'stream)))))
    (:sentencecase
     (if (eql (slot-value stream 'position) :initial)
         (if (alphanumericp character)
             (progn
               (setf (slot-value stream 'position) nil)
               (write-char (char-upcase character) (slot-value stream 'stream)))
             (write-char character (slot-value stream 'stream)))
         (write-char (char-downcase character) (slot-value stream 'stream))))))

(defmethod sys.gray:stream-write-char ((stream case-correcting-stream) character)
  (case-correcting-write character stream))

(defclass simple-edit-mixin ()
  ((edit-buffer :initform nil)
   (edit-offset :initform nil)
   (edit-handler :initform nil)))

(defmethod sys.gray:stream-read-char :around ((stream simple-edit-mixin))
  (let ((buffer (slot-value stream 'edit-buffer))
        (offset (slot-value stream 'edit-offset)))
    (if (and buffer (< offset (fill-pointer buffer)))
        (prog1 (aref buffer offset)
          (incf (slot-value stream 'edit-offset)))
        (do () (nil)
          (let ((ch (call-next-method)))
            (when ch
              (cond ((or (graphic-char-p ch) (eql #\Newline ch))
                     (when buffer
                       (vector-push-extend ch buffer)
                       (incf (slot-value stream 'edit-offset)))
                     (return (write-char ch stream)))
                    ((eql #\Backspace ch)
                     (when (slot-value stream 'edit-handler)
                       (funcall (slot-value stream 'edit-handler) ch))))))))))

(defmethod sys.gray:stream-clear-input :before ((stream simple-edit-mixin))
  (when (slot-value stream 'edit-buffer)
    (setf (fill-pointer (slot-value stream 'edit-buffer)) 0
          (slot-value stream 'edit-offset) 0)))

(defmethod stream-with-edit ((stream simple-edit-mixin) fn)
  (let ((old-buffer (slot-value stream 'edit-buffer))
        (old-offset (slot-value stream 'edit-offset))
        (old-handler (slot-value stream 'edit-handler))
        (buffer (make-array 100
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (unwind-protect
         (multiple-value-bind (start-x start-y)
             (stream-cursor-pos stream)
           (setf (slot-value stream 'edit-buffer) buffer)
           (do () (nil)
            again
             (flet ((handler (ch)
                      (when (> (fill-pointer buffer) 0)
                        (decf (fill-pointer buffer))
                        (multiple-value-bind (x y)
                            (stream-compute-motion stream
                                                   buffer
                                                   0 nil
                                                   start-x start-y)
                          (multiple-value-bind (cx cy) (stream-cursor-pos stream)
                            (stream-clear-between stream x y cx cy))
                          (stream-move-to stream x y)))
                      (go again)))
               (setf (slot-value stream 'edit-offset) 0
                     (slot-value stream 'edit-handler) #'handler)
               (return (funcall fn)))))
      (setf (slot-value stream 'edit-buffer) old-buffer
            (slot-value stream 'edit-offset) old-offset
            (slot-value stream 'edit-handler) old-handler))))

(defun y-or-n-p (&optional control &rest arguments)
  (declare (dynamic-extent arguments))
  (when control
    (fresh-line *query-io*)
    (apply 'format *query-io* control arguments)
    (write-char #\Space *query-io*))
  (format *query-io* "(Y or N) ")
  (loop
     (clear-input *query-io*)
     (let ((c (read-char *query-io*)))
       (when (char-equal c #\Y)
         (return t))
       (when (char-equal c #\N)
         (return nil)))
     (fresh-line *query-io*)
     (format *query-io* "Please respond with \"y\" or \"n\". ")))

(defun yes-or-no-p (&optional control &rest arguments)
  (declare (dynamic-extent arguments))
  (when control
    (fresh-line *query-io*)
    (apply 'format *query-io* control arguments)
    (write-char #\Space *query-io*))
  (format *query-io* "(Yes or No) ")
  (loop
     (clear-input *query-io*)
     (let ((line (read-line *query-io*)))
       (when (string-equal line "yes")
         (return t))
       (when (string-equal line "no")
         (return nil)))
     (fresh-line *query-io*)
     (format *query-io* "Please respond with \"yes\" or \"no\". ")))

(defun write-string (string &optional stream &key (start 0) end)
  (check-type string string)
  (sys.gray:stream-write-string (frob-output-stream stream) string start end)
  string)

(defun terpri (&optional stream)
  (sys.gray:stream-terpri (frob-output-stream stream))
  nil)

(defun fresh-line (&optional stream)
  (sys.gray:stream-fresh-line (frob-output-stream stream)))
