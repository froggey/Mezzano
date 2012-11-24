(defpackage #:sys.gray
  (:use #:cl)
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
   #:stream-write-byte
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
   )
  ;; Import a few functions that match exactly with gray streams.
  (:import-from #:sys.int
                #:stream-clear-input
                #:stream-finish-output
                #:stream-force-output
                #:stream-clear-output
                #:stream-listen
                #:stream-start-line-p))

(in-package #:sys.gray)

;;; Gray Streams classes.

(defclass fundamental-stream (sys.int::stream-object)
  ()
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

;;; Methods common to all streams.

(defmethod stream-element-type ((stream fundamental-character-stream))
  'character)

(defgeneric stream-file-position (stream &optional position-spec))

(defmethod sys.int::stream-file-position ((stream fundamental-stream))
  (stream-file-position stream))
(defmethod sys.int::stream-set-file-position ((stream fundamental-stream) new-position)
  (stream-file-position stream new-position))

;;; Input stream methods.

(defgeneric stream-read-sequence (stream seq &optional start end))

(defmethod stream-read-sequence ((stream fundamental-character-input-stream) seq &optional (start 0) end)
  (unless end (setf end (length seq)))
  (dotimes (i (- end start) end)
    (let ((elt (stream-read-char stream)))
      (when (eql elt :eof)
        (return (+ start i)))
      (setf (aref seq (+ start i)) elt))))

(defmethod stream-read-sequence ((stream fundamental-binary-input-stream) seq &optional (start 0) end)
  (unless end (setf end (length seq)))
  (dotimes (i (- end start) end)
    (let ((elt (stream-read-byte stream)))
      (when (eql elt :eof)
        (return (+ start i)))
      (setf (aref seq (+ start i)) elt))))

(defmethod sys.int::stream-read-sequence (sequence (stream fundamental-input-stream) start end)
  (stream-read-sequence stream sequence start end))

;;; Output stream methods.

(defmethod stream-clear-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-finish-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-force-output ((stream fundamental-output-stream))
  nil)

(defgeneric stream-write-sequence (stream seq &optional start end))

;;; The chapter on Gray Streams in SBCL's manual doesn't say anything about default
;;; methods, but SBCL provides some anyway.

(defmethod stream-write-sequence ((stream fundamental-character-output-stream) seq &optional (start 0) end)
  (typecase seq
    (string (stream-write-string stream seq start end))
    (t (unless end (setf end (length seq)))
       (dotimes (i (- end start))
         (stream-write-char stream (aref seq (+ start i))))
       seq)))

(defmethod stream-write-sequence ((stream fundamental-binary-output-stream) seq &optional (start 0) end)
  (unless end (setf end (length seq)))
  (dotimes (i (- end start))
    (stream-write-byte stream (aref seq (+ start i))))
  seq)

(defmethod sys.int::stream-write-sequence (sequence (stream fundamental-output-stream) start end)
  (stream-write-sequence stream sequence start end))

;;; Binary stream methods.

(defgeneric stream-read-byte (stream))

(defmethod sys.int::stream-read-byte ((stream fundamental-binary-input-stream))
  (let ((byte (stream-read-byte stream)))
    (if (eql byte :eof)
        nil
        byte)))

(defgeneric stream-write-byte (stream integer))

(defmethod sys.int::stream-write-byte (byte (stream fundamental-binary-output-stream))
  (stream-write-byte stream byte))

;;; Character input stream methods.

;;; ### Never called.
(defgeneric stream-peek-char (stream))

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((ch (stream-read-char stream)))
    (cond ((eql ch :eof) :eof)
          (t (stream-unread-char stream ch)
             ch))))

;;; ### Never called.
(defgeneric stream-read-char-no-hang (stream))

(defmethod stream-read-char-no-hang ((stream fundamental-character-input-stream))
  (stream-read-char stream))

(defgeneric stream-read-char (stream))

(defmethod sys.int::stream-read-char ((stream fundamental-character-input-stream))
  (let ((ch (stream-read-char stream)))
    (if (eql ch :eof)
        nil
        ch)))

;;; ### Never called.
(defgeneric stream-read-line (stream))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (do ((result (make-array 120 :element-type 'character :adjustable t :fill-pointer 0))
       (c (stream-read-char input-stream)
          (stream-read-char input-stream)))
      ((or (eql c :eof)
           (eql c #\Newline))
       (values result (eql c :eof)))
    (vector-push-extend c result)))

(defmethod stream-listen ((stream fundamental-character-input-stream))
  (let ((ch (stream-read-char-no-hang stream)))
    (cond (ch (stream-unread-char stream ch)
              t)
          (t nil))))

;;; ### Never called.
(defgeneric stream-unread-char (stream character))

;;; Character output stream methods.

;;; ### Never called.
(defgeneric stream-advance-to-column (stream column))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream) column)
  (let ((current (stream-line-column stream)))
    (when current
      (dotimes (i (- column current))
        (stream-write-char stream #\Newline))
      t)))

;;; ### Never called.
(defgeneric stream-fresh-line (stream))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)))

;;; ### Never called.
(defgeneric stream-line-column (stream))

;;; ### Never called.
(defgeneric stream-line-length (stream))

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (let ((column (stream-line-column stream)))
    (and column (zerop column))))

;;; ### Never called.
(defgeneric stream-terpri (stream))

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (stream-write-char stream #\Newline))

(defgeneric stream-write-char (stream character))

(defmethod sys.int::stream-write-char (character (stream fundamental-character-output-stream))
  (stream-write-char stream character))

;;; ### Never called.
(defgeneric stream-write-string (stream string &optional start end))

(defmethod stream-write-string ((stream fundamental-character-output-stream) string &optional (start 0) end)
  (unless end (setf end (length string)))
  (dotimes (i (- end start))
    (stream-write-char stream (char string (+ start i))))
  string)
