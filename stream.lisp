(in-package #:sys.int)

;;; The base stream class. All other stream classes should
;;; inherit from this to get basic functionallity.
(defclass stream-object (stream standard-object)
  ((unread-char :initform nil)))

(defstruct (synonym-stream
             (:constructor make-synonym-stream (symbol)))
  symbol)

(defun streamp (object)
  (or (synonym-stream-p object)
      (cold-stream-p object)
      (typep object 'stream-object)))

(setf (get 'stream 'type-symbol) 'streamp)

(defgeneric stream-read-char (stream))
(defgeneric stream-write-char (character stream))
(defgeneric stream-start-line-p (stream))

(defun frob-stream (stream default)
  (cond ((synonym-stream-p stream)
         (frob-stream (symbol-value (synonym-stream-symbol stream)) default))
        ((eql stream 'nil)
         (frob-stream default default))
        ((eql stream 't)
         (frob-stream *terminal-io* default))
        ;; TODO: check that the stream is open.
        (t (check-type stream stream)
           stream)))

(defun frob-input-stream (stream)
  (frob-stream stream *standard-input*))

(defun frob-output-stream (stream)
  (frob-stream stream *standard-output*))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((s (frob-input-stream stream)))
    (cond ((cold-stream-p s)
           (or (cold-read-char s)
               (when eof-error-p
                 (error 'end-of-file :stream s))
               eof-value))
          ((slot-value s 'unread-char)
           (prog1 (slot-value s 'unread-char)
             (setf (slot-value s 'unread-char) nil)))
          (t (or (stream-read-char s)
                 (when eof-error-p
                   (error 'end-of-file :stream s))
                 eof-value)))))

(defun unread-char (character &optional (stream *standard-input*))
  (let ((s (frob-input-stream stream)))
    (check-type character character)
    (cond ((cold-stream-p s)
           (cold-unread-char character stream))
          ((slot-value s 'unread-char)
           (error "Multiple unread-char!"))
          (t (setf (slot-value s 'unread-char) character)))
    nil))

(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((s (frob-input-stream stream)))
    (cond ((eql peek-type nil)
           (let ((ch (read-char s eof-error-p eof-value recursive-p)))
             (unread-char ch s)
             ch))
          ((eql peek-type t)
           (do ((ch (read-char s eof-error-p eof-value recursive-p)
                    (read-char s eof-error-p eof-value recursive-p)))
               ((not (sys.int::whitespace[2]p ch))
                (unread-char ch s)
                ch)))
          ((characterp peek-type)
           (error "TODO: character peek."))
          (t (error "Bad peek type ~S." peek-type)))))

(defun write-char (character &optional (stream *standard-output*))
  (let ((s (frob-output-stream stream)))
    (check-type character character)
    (cond ((cold-stream-p s)
           (cold-write-char character s))
          (t (stream-write-char character s)))
    character))

(defun start-line-p (&optional (stream *standard-output*))
  (let ((s (frob-output-stream stream)))
    (cond ((cold-stream-p s)
           (cold-start-line-p s))
          (t (stream-start-line-p s)))))
