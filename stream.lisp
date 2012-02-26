(in-package #:sys.int)

;;; The base stream class. All other stream classes should
;;; inherit from this to get basic functionallity.
(defclass stream-object (stream standard-object)
  ((unread-char :initform nil)))

(defstruct (synonym-stream
             (:constructor make-synonym-stream (symbol)))
  symbol)

(defvar *terminal-io* :terminal-io-is-uninitialized
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

(defun streamp (object)
  (or (synonym-stream-p object)
      (cold-stream-p object)
      (typep object 'stream-object)))

(setf (get 'stream 'type-symbol) 'streamp)

(defgeneric stream-read-char (stream))
(defgeneric stream-write-char (character stream))
(defgeneric stream-start-line-p (stream))
(defgeneric stream-close (stream abort))

(defun frob-stream (stream &optional (default :bad-stream))
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

(defmethod stream-start-line-p ((stream stream-object))
  nil)

(defun close (stream &key abort)
  (let ((s (frob-stream stream)))
    (cond ((cold-stream-p s)
           (cold-close stream abort))
          (t (stream-close stream abort)))
    t))

(defmethod stream-close ((stream stream-object) abort)
  t)

(defclass string-output-stream (stream-object)
  ((element-type :initarg :element-type)
   (string :initform nil)))

(defun make-string-output-stream (&key (element-type 'character))
  (make-instance 'string-output-stream :element-type element-type))

(defun get-output-stream-string (string-output-stream)
  (check-type string-output-stream string-output-stream)
  (prog1 (or (slot-value string-output-stream 'string)
             (make-array 0 :element-type (slot-value stream 'element-type)))
    (setf (slot-value string-output-stream 'string) nil)))

(defun string-output-stream-write-char (character stream)
  (unless (slot-value stream 'string)
    (setf (slot-value stream 'string) (make-array 8
                                                  :element-type (slot-value stream 'element-type)
                                                  :adjustable t
                                                  :fill-pointer 0)))
  (vector-push-extend character (slot-value stream 'string)))

(defmethod stream-write-char (character (stream string-output-stream))
  (string-output-stream-write-char character stream))

;; TODO: declares and other stuff.
(defmacro with-output-to-string ((var) &body body)
  `(let ((,var (make-string-output-stream)))
     (unwind-protect (progn ,@body)
       (close ,var))
     (get-output-stream-string ,var)))
