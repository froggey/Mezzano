;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

;;; TODO: Call write-string, terpri, start-line-p(?), fresh-line, advance-to-column, listen, read-line, read-char-no-hang, peek-char

(defpackage :sys.gray
  (:use :cl)
  (:export
   ;; Gray Streams classes.
   :fundamental-stream
   :fundamental-input-stream
   :fundamental-output-stream
   :fundamental-binary-stream
   :fundamental-character-stream
   :fundamental-binary-input-stream
   :fundamental-binary-output-stream
   :fundamental-character-input-stream
   :fundamental-character-output-stream
   ;; Methods common to all streams.
   :stream-element-type
   :close
   :stream-file-position
   :stream-file-length
   :open-stream-p
   :input-stream-p
   :output-stream-p
   ;; Input stream methods.
   :stream-clear-input
   :stream-read-sequence
   ;; Output stream methods.
   :stream-clear-output
   :stream-finish-output
   :stream-force-output
   :stream-write-sequence
   ;;; Binary stream methods.
   :stream-read-byte
   :stream-write-byte
   ;;; Character input stream methods.
   :stream-peek-char
   :stream-read-char-no-hang
   :stream-read-char
   :stream-read-line
   :stream-listen
   :stream-unread-char
   ;;; Character output stream methods.
   :stream-advance-to-column
   :stream-fresh-line
   :stream-line-column
   :stream-line-length
   :stream-start-line-p
   :stream-terpri
   :stream-write-char
   :stream-write-string
   ;; Extensions.
   :unread-char-mixin
   :stream-display
   ))

(in-package :sys.gray)

;;; Gray Streams classes.

(defclass fundamental-stream (stream standard-object)
  ((%openp :initform t))
  (:documentation "The base class for all Gray streams."))

(defclass fundamental-input-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray input streams."))

(defclass fundamental-output-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray output streams."))

(defclass fundamental-binary-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray streams whose element-type is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-character-stream (fundamental-stream)
  ()
  (:documentation "A superclass of all Gray streams whose element-type is a subtype of character."))

(defclass fundamental-binary-input-stream (fundamental-input-stream fundamental-binary-stream)
  ()
  (:documentation "A superclass of all Gray input streams whose element-type is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-binary-output-stream (fundamental-output-stream fundamental-binary-stream)
  ()
  (:documentation "A superclass of all Gray output streams whose element-type is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-character-input-stream (fundamental-input-stream fundamental-character-stream)
  ()
  (:documentation "A superclass of all Gray input streams whose element-type is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-character-output-stream (fundamental-output-stream fundamental-character-stream)
  ()
  (:documentation "A superclass of all Gray output streams whose element-type is a subtype of character."))

;;; Generic functions for Gray Streams.

(defgeneric stream-element-type (stream))
(defgeneric close (stream &key abort))
(defgeneric open-stream-p (stream))
(defgeneric input-stream-p (stream))
(defgeneric output-stream-p (stream))
(defgeneric stream-file-position (stream &optional position-spec))
(defgeneric stream-file-length (stream))
(defgeneric stream-clear-input (stream))
(defgeneric stream-read-sequence (stream seq &optional start end))
(defgeneric stream-clear-output (stream))
(defgeneric stream-finish-output (stream))
(defgeneric stream-force-output (stream))
(defgeneric stream-write-sequence (stream seq &optional start end))
(defgeneric stream-read-byte (stream))
(defgeneric stream-write-byte (stream integer))
(defgeneric stream-peek-char (stream))
(defgeneric stream-read-char-no-hang (stream))
(defgeneric stream-read-char (stream))
(defgeneric stream-read-line (stream))
(defgeneric stream-listen (stream))
(defgeneric stream-unread-char (stream character))
(defgeneric stream-advance-to-column (stream column))
(defgeneric stream-fresh-line (stream))
(defgeneric stream-line-column (stream))
(defgeneric stream-line-length (stream))
(defgeneric stream-start-line-p (stream))
(defgeneric stream-terpri (stream))
(defgeneric stream-write-char (stream character))
(defgeneric stream-write-string (stream string &optional start end))

(defgeneric stream-display (stream object))

(defclass unread-char-mixin ()
  ((unread-char :initform nil))
  (:documentation "Mixin to add dumb UNREAD-CHAR support to a stream."))

(defmethod sys.gray:stream-read-char :around ((stream unread-char-mixin))
  (if (slot-value stream 'unread-char)
      (prog1 (slot-value stream 'unread-char)
        (setf (slot-value stream 'unread-char) nil))
      (call-next-method)))

(defmethod sys.gray:stream-read-char-no-hang :around ((stream unread-char-mixin))
  (if (slot-value stream 'unread-char)
      (prog1 (slot-value stream 'unread-char)
        (setf (slot-value stream 'unread-char) nil))
      (call-next-method)))

(defmethod sys.gray:stream-unread-char ((stream unread-char-mixin) character)
  (when (slot-value stream 'unread-char)
    (error "Multiple UNREAD-CHAR"))
  (setf (slot-value stream 'unread-char) character))

(defmethod sys.gray:stream-listen :around ((stream unread-char-mixin))
  (or (slot-value stream 'unread-char)
      (call-next-method)))

(defmethod stream-clear-input :before ((stream unread-char-mixin))
  (setf (slot-value stream 'unread-char) nil))

(defmethod stream-display (stream object)
  (format stream "~S" object))

(defmethod stream-line-column ((stream sys.int::cold-stream))
  (sys.int::cold-line-column stream))

(defmethod stream-line-length ((stream sys.int::cold-stream))
  (sys.int::cold-line-length stream))

(defmethod stream-line-column ((stream fundamental-character-output-stream))
   nil)

(defmethod stream-line-length ((stream fundamental-character-output-stream))
  nil)

(in-package :sys.int)

(defclass file-stream (stream) ())

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
(defparameter *trace-output* (make-synonym-stream '*terminal-io*))

(defparameter *fundamental-stream-class* (find-class 'sys.gray:fundamental-stream))

(defun streamp (object)
  (or (synonym-stream-p object)
      (cold-stream-p object)
      (and (boundp '*fundamental-stream-class*)
           (std-instance-p object)
           (member *fundamental-stream-class* (mezzano.clos:class-precedence-list (std-instance-class object))))))

(setf (get 'stream 'type-symbol) 'streamp)

(defgeneric stream-with-edit (stream fn))
(defgeneric stream-cursor-pos (stream))
(defgeneric stream-character-width (stream character))
(defgeneric stream-compute-motion (stream string &optional start end initial-x initial-y))
(defgeneric stream-clear-between (stream start-x start-y end-x end-y))
(defgeneric stream-move-to (stream x y))

(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (body-forms declares)
      (parse-declares body)
    (let ((abortp (gensym "ABORTP")))
      `(let ((,var ,stream)
             (,abortp t))
         (declare ,@declares)
         (unwind-protect
              (multiple-value-prog1
                  (progn ,@body-forms)
                (setf ,abortp nil))
           (when ,var
             (close ,var :abort ,abortp)))))))

(defmacro with-open-file ((stream filespec &rest options) &body body)
  `(with-open-stream (,stream (open ,filespec ,@options))
     ,@body))

(defmacro with-input-from-string ((var string &key (start 0) end index) &body body)
  (cond (index
         (multiple-value-bind (body-forms declares)
             (parse-declares body)
           `(with-open-stream (,var (make-string-input-stream ,string ,start ,end))
              (declare ,@declares)
              (multiple-value-prog1
                  (progn ,@body-forms)
                (setf ,index (string-input-stream-position ,var))))))
        (t
         `(with-open-stream (,var (make-string-input-stream ,string ,start ,end))
            ,@body))))

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

(defun follow-synonym-stream (stream)
  (do () ((not (synonym-stream-p stream)) stream)
    (setf stream (symbol-value (synonym-stream-symbol stream)))))

(defmethod stream-element-type ((stream cold-stream))
  'character)

(defmethod stream-element-type ((stream synonym-stream))
  (stream-element-type (synonym-stream-symbol stream)))

(defmethod sys.gray:stream-line-column ((stream synonym-stream))
  (sys.gray:stream-line-column (follow-synonym-stream stream)))

(defmethod sys.gray:stream-line-length ((stream synonym-stream))
  (sys.gray:stream-line-length (follow-synonym-stream stream)))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((b (sys.gray:stream-read-byte (follow-synonym-stream stream))))
    (check-type b (or integer (eql :eof)))
    (if (eql b :eof)
        (if eof-error-p
            (error 'end-of-file :stream stream)
            eof-value)
        b)))

(defun write-byte (byte stream)
  (sys.gray:stream-write-byte (follow-synonym-stream stream) byte))

(defun read-sequence (sequence stream &key (start 0) end)
  (sys.gray:stream-read-sequence (follow-synonym-stream stream) sequence
                                 start (or end (length sequence))))

(defun generic-read-sequence (sequence stream start end)
  (let ((n (- end start)))
    (if (and (subtypep (stream-element-type stream) 'character)
             (or (listp sequence)
                 (not (subtypep (array-element-type sequence) 'unsigned-byte))))
        (dotimes (i n end)
          (let ((elt (read-char stream nil)))
            (if elt
                (setf (aref sequence (+ start i)) elt)
                (return (+ start i)))))
        (dotimes (i n end)
          (let ((elt (read-byte stream nil)))
            (if elt
                (setf (aref sequence (+ start i)) elt)
                (return (+ start i))))))))

(defmethod sys.gray:stream-read-sequence ((stream stream) sequence &optional start end)
  (generic-read-sequence sequence stream start end))

(defun write-sequence (sequence stream &key (start 0) end)
  (sys.gray:stream-write-sequence (follow-synonym-stream stream) sequence
                                  start (or end (length sequence))))

(defun generic-write-sequence (sequence stream start end)
  (let ((n (- end start)))
    (if (subtypep (stream-element-type stream) 'character)
        (dotimes (i n)
          (write-char (aref sequence (+ start i)) stream))
        (dotimes (i n)
          (write-byte (aref sequence (+ start i)) stream)))))

(defmethod sys.gray:stream-write-sequence ((stream stream) sequence &optional start end)
  (generic-write-sequence sequence stream start end))

(defun file-position (stream &optional (position-spec nil position-spec-p))
  (cond (position-spec-p
         (check-type position-spec (or (integer 0) (member :start :end)))
         (when (eql position-spec :start)
           (setf position-spec 0))
         (sys.gray:stream-file-position (follow-synonym-stream stream) position-spec))
        (t (sys.gray:stream-file-position (follow-synonym-stream stream)))))

(defun file-length (stream)
  (sys.gray:stream-file-length (follow-synonym-stream stream)))

(defmacro with-stream-editor ((stream recursive-p) &body body)
  "Activate the stream editor functionality for STREAM."
  `(%with-stream-editor ,stream ,recursive-p (lambda () (progn ,@body))))

(defun %with-stream-editor (stream recursive-p fn)
  (cond ((synonym-stream-p stream)
         (%with-stream-editor (symbol-value (synonym-stream-symbol stream)) recursive-p fn))
        ((or (cold-stream-p stream) recursive-p)
         (funcall fn))
        (t (stream-with-edit stream fn))))

(defmethod stream-with-edit ((stream stream) fn)
  (funcall fn))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((s (frob-input-stream stream)))
    (cond ((cold-stream-p s)
           (or (cold-read-char s)
               (when eof-error-p
                 (error 'end-of-file :stream s))
               eof-value))
          (t (let ((c (sys.gray:stream-read-char s)))
               (check-type c (or character (eql :eof)))
               (cond ((eql c :eof)
                      (when eof-error-p
                        (error 'end-of-file :stream s))
                      eof-value)
                     (c)))))))

(defun read-char-no-hang (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((s (frob-input-stream stream)))
    (cond ((cold-stream-p s)
           (or (cold-read-char s)
               (when eof-error-p
                 (error 'end-of-file :stream s))
               eof-value))
          (t (let ((c (sys.gray:stream-read-char-no-hang s)))
               (check-type c (or character (eql :eof) null))
               (cond ((eql c :eof)
                      (when eof-error-p
                        (error 'end-of-file :stream s))
                      eof-value)
                     (c)))))))

(defun read-line (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (with-stream-editor (input-stream recursive-p)
    (do ((result (make-array 80 :element-type 'character :adjustable t :fill-pointer 0))
         (c (read-char input-stream nil nil recursive-p)
            (read-char input-stream nil nil recursive-p)))
        ((or (null c)
             (eql c #\Newline))
         (if (and (null c) (eql (length result) 0))
             ;; At EOF and no data read.
             (if eof-error-p
                 (error 'end-of-file :stream input-stream)
                 (values eof-value t))
             (values result (null c))))
      (vector-push-extend c result))))

(defun unread-char (character &optional (stream *standard-input*))
  (let ((s (frob-input-stream stream)))
    (check-type character character)
    (cond ((cold-stream-p s)
           (cold-unread-char character s))
          (t (sys.gray:stream-unread-char s character)))
    nil))

(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((s (frob-input-stream stream)))
    (cond ((eql peek-type nil)
           (let ((ch (read-char s eof-error-p nil recursive-p)))
             (cond (ch
                    (unread-char ch s)
                    ch)
                   (t
                    eof-value))))
          ((eql peek-type t)
           (do ((ch (read-char s eof-error-p nil recursive-p)
                    (read-char s eof-error-p nil recursive-p)))
               ((or (not ch)
                    (not (sys.int::whitespace[2]p ch)))
                (cond (ch
                       (unread-char ch s)
                       ch)
                      (t
                       eof-value)))))
          ((characterp peek-type)
           (error "TODO: character peek."))
          (t (error "Bad peek type ~S." peek-type)))))

(defun clear-input (&optional (stream *standard-input*))
  (let ((s (frob-input-stream stream)))
    (cond ((cold-stream-p s)
           (cold-clear-input s))
          (t (sys.gray:stream-clear-input s))))
  nil)

(defun finish-output (&optional (output-stream *standard-output*))
  (let ((s (frob-output-stream output-stream)))
    (cond ((cold-stream-p s) nil)
          (t (sys.gray:stream-finish-output s)))))

(defun force-output (&optional (output-stream *standard-output*))
  (let ((s (frob-output-stream output-stream)))
    (cond ((cold-stream-p s) nil)
          (t (sys.gray:stream-force-output s)))))

(defun clear-output (&optional (output-stream *standard-output*))
  (let ((s (frob-output-stream output-stream)))
    (cond ((cold-stream-p s) nil)
          (t (sys.gray:stream-clear-output s)))))

(defun write-char (character &optional (stream *standard-output*))
  (let ((s (frob-output-stream stream)))
    (check-type character character)
    (cond ((cold-stream-p s)
           (cold-write-char character s))
          (t (sys.gray:stream-write-char s character)))
    character))

(defun start-line-p (&optional (stream *standard-output*))
  (let ((s (frob-output-stream stream)))
    (cond ((cold-stream-p s)
           (cold-start-line-p s))
          (t (sys.gray:stream-start-line-p s)))))

(defmethod close ((stream stream) &key abort)
  t)

(defmethod close :after ((stream sys.gray:fundamental-stream) &key abort)
  (declare (ignore abort))
  (setf (slot-value stream 'sys.gray::%openp) nil))

(defmethod open-stream-p ((stream synonym-stream))
  (open-stream-p (follow-synonym-stream stream)))

(defmethod open-stream-p ((stream sys.gray:fundamental-stream))
  (slot-value stream 'sys.gray::%openp))

(defmethod input-stream-p ((stream sys.gray:fundamental-stream))
  'nil)

(defmethod input-stream-p ((stream sys.gray:fundamental-input-stream))
  't)

(defmethod input-stream-p ((stream synonym-stream))
  (input-stream-p (follow-synonym-stream stream)))

(defmethod output-stream-p ((stream sys.gray:fundamental-stream))
  'nil)

(defmethod output-stream-p ((stream sys.gray:fundamental-output-stream))
  't)

(defmethod output-stream-p ((stream synonym-stream))
  (output-stream-p (follow-synonym-stream stream)))

(defun listen (&optional (input-stream *standard-input*))
  (let ((s (frob-input-stream input-stream)))
    (cond ((cold-stream-p s)
           (cold-listen s))
          (t (sys.gray:stream-listen s)))))

(defmethod sys.gray:stream-listen ((stream stream))
  t)

(defmethod print-object ((object synonym-stream) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "for ~S" (synonym-stream-symbol object))))

(defclass string-stream (sys.gray:fundamental-character-stream) ())

(defclass string-output-stream (sys.gray:fundamental-character-output-stream
                                string-stream)
  ((element-type :initarg :element-type)
   (string :initarg :string))
  (:default-initargs :string nil))

(defun make-string-output-stream (&key (element-type 'character))
  (make-instance 'string-output-stream :element-type element-type))

(defun get-output-stream-string (string-output-stream)
  (check-type string-output-stream string-output-stream)
  (prog1 (or (slot-value string-output-stream 'string)
             (make-array 0 :element-type (slot-value string-output-stream 'element-type)))
    (setf (slot-value string-output-stream 'string) nil)))

(defun string-output-stream-write-char (character stream)
  (unless (slot-value stream 'string)
    (setf (slot-value stream 'string) (make-array 8
                                                  :element-type (slot-value stream 'element-type)
                                                  :adjustable t
                                                  :fill-pointer 0)))
  (vector-push-extend character (slot-value stream 'string)))

(defmethod sys.gray:stream-write-char ((stream string-output-stream) character)
  (string-output-stream-write-char character stream))

;; TODO: declares and other stuff.
(defmacro with-output-to-string ((var &optional string-form &key (element-type ''character)) &body body)
  (if string-form
      `(let ((,var (make-string-output-stream :element-type ,element-type)))
         (setf (slot-value ,var 'string) ,string-form)
         (unwind-protect (progn ,@body)
           (close ,var)))
      `(let ((,var (make-string-output-stream :element-type ,element-type)))
         (unwind-protect (progn ,@body)
           (close ,var))
         (get-output-stream-string ,var))))

(defun write-to-string (object &key
                                 (array *print-array*)
                                 (base *print-base*)
                                 (case *print-case*)
                                 (circle *print-circle*)
                                 (escape *print-escape*)
                                 (gensym *print-gensym*)
                                 (length *print-length*)
                                 (level *print-level*)
                                 (lines *print-lines*)
                                 (miser-width *print-miser-width*)
                                 (pprint-dispatch *print-pprint-dispatch*)
                                 (pretty *print-pretty*)
                                 (radix *print-radix*)
                                 (readably *print-readably*)
                                 (right-margin *print-right-margin*))
  (with-output-to-string (s)
    (write object :stream s
           :array array
           :base base
           :case case
           :circle circle
           :escape escape
           :gensym gensym
           :length length
           :level level
           :lines lines
           :miser-width miser-width
           :pprint-dispatch pprint-dispatch
           :pretty pretty
           :radix radix
           :readably readably
           :right-margin right-margin)))

(defun princ-to-string (object)
  (with-output-to-string (s)
    (princ object s)))

(defun prin1-to-string (object)
  (with-output-to-string (s)
    (prin1 object s)))

(defclass broadcast-stream (sys.gray:fundamental-character-output-stream)
  ((streams :initarg :streams :reader broadcast-stream-streams)))

(defun make-broadcast-stream (&rest streams)
  (make-instance 'broadcast-stream :streams streams))

(defmethod sys.gray:stream-write-char ((stream broadcast-stream) character)
  (dolist (s (broadcast-stream-streams stream))
    (write-char character s)))

(defmethod sys.gray:stream-write-byte ((stream broadcast-stream) byte)
  (dolist (s (broadcast-stream-streams stream))
    (write-byte byte s)))

(defclass echo-stream (sys.gray:fundamental-character-output-stream
                       sys.gray:fundamental-character-input-stream
                       sys.gray:unread-char-mixin)
  ((input-stream :initarg :input-stream
                 :reader echo-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader echo-stream-output-stream)))

(defun make-echo-stream (input-stream output-stream)
  (make-instance 'echo-stream
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod sys.gray:stream-write-char ((stream echo-stream) character)
  (write-char character (echo-stream-output-stream stream)))

(defmethod sys.gray:stream-read-char ((stream echo-stream))
  (let ((c (read-char (echo-stream-input-stream stream) nil)))
    (when c
      (write-char c (echo-stream-output-stream stream)))))

(defclass two-way-stream (sys.gray:fundamental-character-output-stream
                          sys.gray:fundamental-character-input-stream
                          sys.gray:unread-char-mixin)
  ((input-stream :initarg :input-stream
                 :reader two-way-stream-input-stream)
   (output-stream :initarg :output-stream
                  :reader two-way-stream-output-stream)))

(defun make-two-way-stream (input-stream output-stream)
  (make-instance 'two-way-stream
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod sys.gray:stream-write-char ((stream two-way-stream) character)
  (write-char character (two-way-stream-output-stream stream)))

(defmethod sys.gray:stream-read-char ((stream two-way-stream))
  (read-char (two-way-stream-input-stream stream) nil))

(defclass concatenated-stream (sys.gray:fundamental-character-input-stream
                               sys.gray:unread-char-mixin)
  ((streams :initarg :streams)))

(defun make-concatenated-stream (&rest input-streams)
  (dolist (s input-streams)
    (assert (input-stream-p s)))
  (make-instance 'concatenated-stream :streams input-streams))

(defun concatenated-stream-streams (concatenated-stream)
  (check-type concatenated-stream concatenated-stream)
  (slot-value concatenated-stream 'streams))

(defmethod sys.gray:stream-read-char ((stream concatenated-stream))
  (loop
     (when (endp (concatenated-stream-streams stream))
       (return :eof))
     (let ((ch (read-char (first (concatenated-stream-streams stream)) nil)))
       (when ch
         (return ch))
       ;; Reached end of this stream. Pop streams.
       (pop (slot-value stream 'streams)))))

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

(defclass shadow-stream (sys.gray:fundamental-binary-output-stream
                         sys.gray:fundamental-binary-input-stream
                         sys.gray:fundamental-character-output-stream
                         sys.gray:fundamental-character-input-stream
                         sys.gray:unread-char-mixin)
  ((primary-stream
    :initarg :primary
    :reader shadow-stream-primary)
   (shadow-streams
    :initarg :shadows
    :initform '()
    :reader shadow-stream-shadows)))

(defmethod sys.gray:stream-read-char ((stream shadow-stream))
  (let ((c (read-char (shadow-stream-primary stream) nil)))
    (when c
      (dolist (s (shadow-stream-shadows stream))
        (write-char c s)))
    c))

(defmethod sys.gray:stream-write-char ((stream shadow-stream) character)
  (write-char character (shadow-stream-primary stream))
  (dolist (s (shadow-stream-shadows stream))
    (write-char character s)))

(defmethod close ((stream shadow-stream) &key abort)
  (close (shadow-stream-primary stream) :abort abort))

(defmethod sys.gray:stream-listen ((stream shadow-stream))
  (listen (shadow-stream-primary stream)))

(defmethod sys.gray:stream-clear-input ((stream shadow-stream))
  (clear-input (shadow-stream-primary stream)))

(defmethod sys.gray:stream-start-line-p ((stream shadow-stream))
  (sys.gray:stream-start-line-p (shadow-stream-primary stream)))

(defmethod stream-with-edit ((stream shadow-stream) fn)
  (stream-with-edit (shadow-stream-primary stream) fn))

(defmethod stream-cursor-pos ((stream shadow-stream))
  (stream-cursor-pos (shadow-stream-primary stream)))

(defmethod stream-character-width ((stream shadow-stream) character)
  (stream-character-width (shadow-stream-primary stream) character))

(defmethod stream-compute-motion ((stream shadow-stream) string &optional start end initial-x initial-y)
  (stream-compute-motion (shadow-stream-primary stream) string start end initial-x initial-y))

(defmethod stream-clear-between ((stream shadow-stream) start-x start-y end-x end-y)
  (stream-clear-between (shadow-stream-primary stream) start-x start-y end-x end-y))

(defmethod stream-move-to ((stream shadow-stream) x y)
  (stream-move-to (shadow-stream-primary stream) x y))

(defmethod sys.gray:stream-read-byte ((stream shadow-stream))
  (read-byte (shadow-stream-primary stream)))

(defmethod sys.gray:stream-write-byte ((stream shadow-stream) byte)
  (write-byte byte (shadow-stream-primary stream)))

(defmethod sys.gray:stream-read-sequence ((stream shadow-stream) sequence &optional start end)
  (read-sequence sequence (shadow-stream-primary stream) start end))

(defmethod sys.gray:stream-write-sequence ((stream shadow-stream) sequence &optional start end)
  (write-sequence sequence (shadow-stream-primary stream) start end))

(defmethod sys.gray:stream-file-position ((stream shadow-stream) &optional (fp nil fpp))
  (if fpp
      (file-position (shadow-stream-primary stream) fp)
      (file-position (shadow-stream-primary stream))))

(defmethod sys.gray:stream-line-column ((stream shadow-stream))
  (sys.gray:stream-line-column (shadow-stream-primary stream)))

(defmethod stream-element-type ((stream shadow-stream))
  (stream-element-type (shadow-stream-primary stream)))

(defclass string-input-stream (sys.gray:fundamental-character-input-stream
                               sys.gray:unread-char-mixin
                               string-stream)
  ((string :initarg :string)
   (start :initarg :start)
   (end :initarg :end)))

(defun make-string-input-stream (string &optional (start 0) end)
  (make-instance 'string-input-stream
                 :string string
                 :start start
                 :end (or end (length string))))

(defun string-input-stream-position (stream)
  (slot-value stream 'start))

(defmethod sys.gray:stream-read-char ((stream string-input-stream))
  (if (< (slot-value stream 'start) (slot-value stream 'end))
      (prog1 (char (slot-value stream 'string)
                   (slot-value stream 'start))
        (incf (slot-value stream 'start)))
      :eof))

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

;;; Gray stream default methods.

(defmethod stream-element-type ((stream sys.gray:fundamental-character-stream))
  'character)

(defmethod sys.gray:stream-clear-input ((stream sys.gray:fundamental-input-stream))
  nil)

(defun read-sequence-common (stream seq start end reader)
  (unless end (setf end (length seq)))
  (dotimes (i (- end start) end)
    (let ((elt (funcall reader stream)))
      (when (eql elt :eof)
        (return (+ start i)))
      (setf (elt seq (+ start i)) elt))))

(defmethod sys.gray:stream-read-sequence ((stream sys.gray:fundamental-character-input-stream) seq &optional (start 0) end)
  (read-sequence-common stream seq start end 'sys.gray:stream-read-char))

(defmethod sys.gray:stream-read-sequence ((stream sys.gray:fundamental-binary-input-stream) seq &optional (start 0) end)
  (read-sequence-common stream seq start end 'sys.gray:stream-read-byte))

(defmethod sys.gray:stream-clear-output ((stream sys.gray:fundamental-output-stream))
  nil)

(defmethod sys.gray:stream-finish-output ((stream sys.gray:fundamental-output-stream))
  nil)

(defmethod sys.gray:stream-force-output ((stream sys.gray:fundamental-output-stream))
  nil)

(defun write-sequence-common (stream seq start end writer)
  (unless end (setf end (length seq)))
  (dotimes (i (- end start))
    (funcall writer stream (elt seq (+ start i))))
  seq)

(defmethod sys.gray:stream-write-sequence ((stream sys.gray:fundamental-character-output-stream) seq &optional (start 0) end)
  (write-sequence-common stream seq start end 'sys.gray:stream-write-char))

(defmethod sys.gray:stream-write-sequence ((stream sys.gray:fundamental-binary-output-stream) seq &optional (start 0) end)
  (write-sequence-common stream seq start end 'sys.gray:stream-write-byte))

(defmethod sys.gray:stream-peek-char ((stream sys.gray:fundamental-character-input-stream))
  (let ((ch (sys.gray:stream-read-char stream)))
    (cond ((eql ch :eof) :eof)
          (t (unread-char ch stream)
             ch))))

(defmethod sys.gray:stream-read-char-no-hang ((stream sys.gray:fundamental-character-input-stream))
  (sys.gray:stream-read-char stream))

(defmethod sys.gray:stream-read-line ((stream sys.gray:fundamental-character-input-stream))
  (do ((result (make-array 120 :element-type 'character :adjustable t :fill-pointer 0))
       (c (sys.gray:stream-read-char stream)
          (sys.gray:stream-read-char stream)))
      ((or (eql c :eof)
           (eql c #\Newline))
       (values result (eql c :eof)))
    (vector-push-extend c result)))

(defmethod sys.gray:stream-listen ((stream sys.gray:fundamental-character-input-stream))
  (let ((ch (sys.gray:stream-read-char-no-hang stream)))
    (cond ((or (eql ch :eof)
               (not ch))
           nil)
          (t (sys.gray:stream-unread-char stream ch)
             t))))

(defmethod sys.gray:stream-advance-to-column ((stream sys.gray:fundamental-character-output-stream) column)
  (let ((current (sys.gray:stream-line-column stream)))
    (when current
      (dotimes (i (- column current))
        (sys.gray:stream-write-char stream #\Newline))
      t)))

(defmethod sys.gray:stream-fresh-line ((stream sys.gray:fundamental-character-output-stream))
  (unless (sys.gray:stream-start-line-p stream)
    (sys.gray:stream-terpri stream)))

(defmethod sys.gray:stream-line-column ((stream sys.gray:fundamental-character-output-stream))
  nil)

(defmethod sys.gray:stream-line-column ((stream synonym-stream))
  (sys.gray:stream-line-column (follow-synonym-stream stream)))

(defmethod sys.gray:stream-start-line-p ((stream sys.gray:fundamental-character-output-stream))
  (let ((column (sys.gray:stream-line-column stream)))
    (and column (zerop column))))

(defmethod sys.gray:stream-terpri ((stream sys.gray:fundamental-character-output-stream))
  (sys.gray:stream-write-char stream #\Newline))

(defmethod sys.gray:stream-write-string ((stream sys.gray:fundamental-character-output-stream) string &optional (start 0) end)
  (unless end (setf end (length string)))
  (dotimes (i (- end start))
    (sys.gray:stream-write-char stream (char string (+ start i))))
  string)
