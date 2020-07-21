;;;; Definitions of Gray streams classes and functions.

(defpackage :mezzano.gray
  (:use :cl)
  (:local-nicknames (:sys.int :mezzano.internals))
  (:export
   ;; Gray Streams classes.
   #:fundamental-stream
   #:fundamental-input-stream
   #:fundamental-output-stream
   #:fundamental-binary-stream
   #:fundamental-character-stream
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   ;; Methods common to all streams.
   #:stream-element-type
   #:close
   #:stream-file-position
   #:stream-file-length
   #:stream-file-string-length
   #:open-stream-p
   #:input-stream-p
   #:output-stream-p
   #:interactive-stream-p
   #:stream-external-format
   ;; Input stream methods.
   #:stream-clear-input
   #:stream-read-sequence
   ;; Output stream methods.
   #:stream-clear-output
   #:stream-finish-output
   #:stream-force-output
   #:stream-write-sequence
   ;;; Binary stream methods.
   #:stream-read-byte
   #:stream-read-byte-no-hang
   #:stream-write-byte
   #:stream-listen-byte
   ;;; Character input stream methods.
   #:stream-peek-char
   #:stream-read-char-no-hang
   #:stream-read-char
   #:stream-read-line
   #:stream-listen
   #:stream-unread-char
   ;;; Character output stream methods.
   #:stream-advance-to-column
   #:stream-fresh-line
   #:stream-line-column
   #:stream-line-length
   #:stream-start-line-p
   #:stream-terpri
   #:stream-write-char
   #:stream-write-string
   ;; Extensions.
   #:unread-char-mixin
   #:stream-display
   #:stream-open-p
   #:external-format-string-length
   ))

(in-package :mezzano.gray)

;;; The standard stream class.

(defclass stream () ())

;;; Gray Streams classes.

(defclass fundamental-stream (stream)
  ((%openp :initform t :accessor stream-open-p))
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
(defgeneric stream-external-format (stream))
(defgeneric close (stream &key abort))
(defgeneric open-stream-p (stream))
(defgeneric input-stream-p (stream))
(defgeneric output-stream-p (stream))
(defgeneric interactive-stream-p (stream))
(defgeneric stream-file-position (stream &optional position-spec))
(defgeneric stream-file-length (stream))
(defgeneric stream-file-string-length (stream string))
(defgeneric stream-clear-input (stream))
(defgeneric stream-read-sequence (stream seq &optional start end))
(defgeneric stream-clear-output (stream))
(defgeneric stream-finish-output (stream))
(defgeneric stream-force-output (stream))
(defgeneric stream-write-sequence (stream seq &optional start end))
(defgeneric stream-read-byte (stream))
(defgeneric stream-read-byte-no-hang (stream))
(defgeneric stream-write-byte (stream integer))
(defgeneric stream-listen-byte (stream))
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

;;; Default Gray methods

(defmethod stream-display (stream (object fundamental-character-output-stream))
  (format stream "~S" object))

(defmethod stream-line-column ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-line-length ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-read-sequence ((stream fundamental-input-stream) sequence &optional (start 0) end)
  (setf end (or end (length sequence)))
  (let ((n (- end start)))
    ;; For bivalent streams, perform character reads unless reading into an integer vector.
    (if (and (subtypep (stream-element-type stream) 'character)
             (not (and (vectorp sequence)
                       (subtypep (array-element-type sequence) 'integer))))
        (dotimes (i n end)
          (let ((elt (read-char stream nil)))
            (if elt
                (setf (elt sequence (+ start i)) elt)
                (return (+ start i)))))
        (dotimes (i n end)
          (let ((elt (read-byte stream nil)))
            (if elt
                (setf (elt sequence (+ start i)) elt)
                (return (+ start i))))))))

(defmethod stream-write-sequence ((stream fundamental-output-stream) sequence &optional (start 0) end)
  (setf end (or end (length sequence)))
  (let ((n (- end start)))
    (if (and (subtypep (stream-element-type stream) 'character)
             (not (and (vectorp sequence)
                       (subtypep (array-element-type sequence) 'integer))))
        (dotimes (i n)
          (write-char (elt sequence (+ start i)) stream))
        (dotimes (i n)
          (write-byte (elt sequence (+ start i)) stream))))
  sequence)

(defmethod close ((stream fundamental-stream) &key abort)
  (declare (ignore abort))
  (setf (stream-open-p stream) nil)
  t)

(defmethod open-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod open-stream-p ((stream fundamental-stream))
  (stream-open-p stream))

(defmethod input-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod input-stream-p ((stream fundamental-stream))
  'nil)

(defmethod input-stream-p ((stream fundamental-input-stream))
  't)

(defmethod output-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod output-stream-p ((stream fundamental-stream))
  'nil)

(defmethod output-stream-p ((stream fundamental-output-stream))
  't)

(defmethod interactive-stream-p ((stream t))
  (error 'type-error :expected-type 'stream :datum stream))

(defmethod interactive-stream-p ((stream fundamental-stream))
  'nil)

(defmethod stream-element-type ((stream fundamental-character-stream))
  'character)

(defgeneric external-format-string-length (external-format string))

(defmethod external-format-string-length ((external-format (eql :default)) string)
  (length (sys.int::encode-utf-8-string string :eol-style :lf)))

(defmethod external-format-string-length ((external-format (eql :utf-8)) string)
  (length (sys.int::encode-utf-8-string string :eol-style :lf)))

(defmethod stream-file-string-length ((stream fundamental-character-output-stream) string)
  (external-format-string-length (stream-external-format stream) string))

(defmethod stream-clear-input ((stream fundamental-input-stream))
  nil)

(defmethod stream-clear-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-finish-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-force-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-read-byte-no-hang ((stream fundamental-binary-input-stream))
  (ecase (stream-listen-byte stream)
    ((:eof) :eof)
    ((t) (read-byte stream nil :eof))
    ((nil) nil)))

(defmethod stream-listen-byte ((stream fundamental-binary-input-stream))
  t)

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((ch (read-char stream nil :eof)))
    (cond ((eql ch :eof) :eof)
          (t (unread-char ch stream)
             ch))))

(defmethod stream-read-char-no-hang ((stream fundamental-character-input-stream))
  (read-char stream nil :eof))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (loop
     with result = (make-array 120 :element-type 'character :adjustable t :fill-pointer 0)
     for c = (read-char stream nil :eof)
     until (or (eql c :eof)
               (eql c #\Newline))
     do (vector-push-extend c result)
     finally (return (values result (eql c :eof)))))

(defmethod stream-listen ((stream fundamental-character-input-stream))
  (let ((ch (read-char-no-hang stream nil :eof)))
    (cond ((or (eql ch :eof)
               (not ch))
           nil)
          (t (unread-char ch stream)
             t))))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream) column)
  (let ((current (sys.int::line-column stream)))
    (when current
      (dotimes (i (- column current))
        (write-char stream #\Newline))
      t)))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (cond ((sys.int::start-line-p stream)
         nil)
        (t
         (terpri stream)
         t)))

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (let ((column (sys.int::line-column stream)))
    (and column (zerop column))))

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (write-char #\Newline stream))

(defmethod stream-write-string ((stream fundamental-character-output-stream) string &optional (start 0) end)
  (write-sequence string stream :start start :end end)
  string)

(defmethod stream-file-position ((stream fundamental-stream) &optional position-spec)
  (declare (ignore position-spec))
  nil)

;;; Unread-char mixin.

(defclass unread-char-mixin ()
  ((unread-char :initform nil))
  (:documentation "Mixin to add simple UNREAD-CHAR support to a stream."))

(defmethod stream-read-char :around ((stream unread-char-mixin))
  (if (slot-value stream 'unread-char)
      (prog1 (slot-value stream 'unread-char)
        (setf (slot-value stream 'unread-char) nil))
      (call-next-method)))

(defmethod stream-read-char-no-hang :around ((stream unread-char-mixin))
  (if (slot-value stream 'unread-char)
      (prog1 (slot-value stream 'unread-char)
        (setf (slot-value stream 'unread-char) nil))
      (call-next-method)))

(defmethod stream-unread-char ((stream unread-char-mixin) character)
  (when (slot-value stream 'unread-char)
    (error "Multiple UNREAD-CHAR"))
  (setf (slot-value stream 'unread-char) character))

(defmethod stream-listen :around ((stream unread-char-mixin))
  (or (slot-value stream 'unread-char)
      (call-next-method)))

(defmethod stream-clear-input :before ((stream unread-char-mixin))
  (setf (slot-value stream 'unread-char) nil))
