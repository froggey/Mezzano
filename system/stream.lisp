;;;; Stream related functions and variables

(in-package :mezzano.internals)

;;; I/O customization variables.

(defclass cold-stream (mezzano.gray:fundamental-character-input-stream
                       mezzano.gray:fundamental-character-output-stream)
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

(defparameter *cold-stream-is-line-buffered* t)
;; This uses a list of buffers instead of a weak hash table so
;; that GET-COLD-STREAM-BUFFER can flush any dead entries.
(defparameter *cold-stream-buffers* '())
(defparameter *cold-stream-lock* (mezzano.supervisor:make-mutex '*cold-stream-buffers*))

(defstruct cold-stream-buffer
  thread
  data
  column)

(defmacro with-cold-stream-buffer ((buffer) &body body)
  `(let ((buffer (get-cold-stream-buffer)))
     ,@body))

(defun get-cold-stream-buffer ()
  (mezzano.supervisor:with-mutex (*cold-stream-lock*)
    (do ((self (mezzano.supervisor:current-thread))
         (entry nil)
         (i *cold-stream-buffers*)
         (prev nil))
        ((endp i)
         (when (not entry)
           (setf entry (make-cold-stream-buffer
                        :thread (make-weak-pointer self)
                        :data (make-array 100
                                          :element-type 'character
                                          :fill-pointer 0
                                          :adjustable t
                                          :area :wired)
                        :column 0))
           (push entry *cold-stream-buffers*))
         entry)
      (let ((thread (weak-pointer-value (cold-stream-buffer-thread (first i)))))
        (when (eql thread self)
          (setf entry (first i)))
        (cond (thread
               (setf prev i))
              (t
               ;; Entry is dead, flush & remove it.
               (when (not (zerop (length (cold-stream-buffer-data (first i)))))
                 (mezzano.supervisor::debug-write-string (cold-stream-buffer-data (first i))))
               (if prev
                   (setf (rest prev) (rest i))
                   (setf *cold-stream-buffers* (rest i)))))
        (setf i (rest i))))))

(defmethod mezzano.gray:stream-read-char ((stream cold-stream))
  (or (cold-read-char stream) :eof))

(defmethod mezzano.gray:stream-listen ((stream cold-stream))
  (cold-listen stream))

(defmethod mezzano.gray:stream-unread-char ((stream cold-stream) character)
  (cold-unread-char character stream))

(defmethod mezzano.gray:stream-clear-input ((stream cold-stream))
  (cold-clear-input stream))

(defun cold-stream-buffer-flush (buffer)
  (let ((data (cold-stream-buffer-data buffer)))
    (mezzano.supervisor::debug-write-string data)
    (setf (fill-pointer data) 0)))

(defmethod mezzano.gray:stream-write-char ((stream cold-stream) character)
  (cond (*cold-stream-is-line-buffered*
         (with-cold-stream-buffer (buffer)
           (vector-push-extend character (cold-stream-buffer-data buffer))
           (cond ((eql character #\Newline)
                  (setf (cold-stream-buffer-column buffer) 0)
                  (cold-stream-buffer-flush buffer))
                 (t
                  (incf (cold-stream-buffer-column buffer))))))
        (t
         (cold-write-char character stream))))

(defmethod mezzano.gray:stream-start-line-p ((stream cold-stream))
  (cond (*cold-stream-is-line-buffered*
         (with-cold-stream-buffer (buffer)
           (zerop (cold-stream-buffer-column buffer))))
        (t
         (cold-start-line-p stream))))

(defmethod mezzano.gray:stream-line-column ((stream cold-stream))
  (cond (*cold-stream-is-line-buffered*
         (with-cold-stream-buffer (buffer)
           (cold-stream-buffer-column buffer)))
        (t
         (cold-line-column stream))))

(defmethod mezzano.gray:stream-finish-output ((stream cold-stream))
  (when *cold-stream-is-line-buffered*
    (with-cold-stream-buffer (buffer)
      (cold-stream-buffer-flush buffer))))

(defmethod mezzano.gray:stream-force-output ((stream cold-stream))
  (finish-output stream))

(defmethod mezzano.gray:stream-clear-output ((stream cold-stream))
  (when *cold-stream-is-line-buffered*
    (with-cold-stream-buffer (buffer)
      (setf (fill-pointer buffer) 0))))

(defmethod mezzano.gray:stream-line-length ((stream cold-stream))
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

(defun listen-byte (&optional input-stream)
  ;; Note: Unlike STREAM-LISTEN, STREAM-LISTEN-BYTE may return :EOF
  ;; to indicate that the stream is at EOF. This should be equivalent to NIL.
  ;; It is used in the default implementation of STREAM-READ-BYTE-NO-HANG.
  (let ((result (mezzano.gray:stream-listen-byte (frob-input-stream input-stream))))
    (cond ((or (eql result :eof)
               (not result))
           nil)
          (t))))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((b (mezzano.gray:stream-read-byte (frob-input-stream stream))))
    (check-type b (or integer (eql :eof)))
    (if (eql b :eof)
        (if eof-error-p
            (error 'end-of-file :stream stream)
            eof-value)
        b)))

(defun read-byte-no-hang (stream &optional (eof-error-p t) eof-value)
  (let* ((s (frob-input-stream stream))
         (b (mezzano.gray:stream-read-byte-no-hang s)))
    (check-type b (or integer (eql :eof) null))
    (cond ((eql b :eof)
           (when eof-error-p
             (error 'end-of-file :stream s))
           eof-value)
          (b))))

(defun write-byte (byte stream)
  (mezzano.gray:stream-write-byte (frob-output-stream stream) byte))

(defun read-sequence (sequence stream &key (start 0) end)
  (mezzano.gray:stream-read-sequence (frob-input-stream stream) sequence
                                 start end))

(defun write-sequence (sequence stream &key (start 0) end)
  (mezzano.gray:stream-write-sequence (frob-output-stream stream) sequence
                                  start end)
  sequence)

(defun file-position (stream &optional (position-spec nil position-spec-p))
  (check-type stream stream)
  (cond (position-spec-p
         (check-type position-spec (or (integer 0) (member :start :end)))
         (mezzano.gray:stream-file-position stream position-spec))
        (t
         (mezzano.gray:stream-file-position stream))))

(defun file-length (stream)
  (check-type stream stream)
  (mezzano.gray:stream-file-length stream))

(defun file-string-length (stream object)
  (check-type stream stream)
  (check-type object (or string character))
  (when (characterp object)
    (setf object (string object)))
  (mezzano.gray:stream-file-string-length stream object))

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
         (c (mezzano.gray:stream-read-char s)))
    (check-type c (or character (eql :eof)))
    (cond ((eql c :eof)
           (when eof-error-p
             (error 'end-of-file :stream s))
           eof-value)
          (c))))

(defun read-char-no-hang (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let* ((s (frob-input-stream stream))
         (c (mezzano.gray:stream-read-char-no-hang s)))
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
      (mezzano.gray:stream-read-line input-stream)
    (if (and (zerop (length line))
             missing-newline-p)
        (if eof-error-p
            (error 'end-of-file :stream input-stream)
            (values eof-value t))
        (values line missing-newline-p))))

(defun unread-char (character &optional stream)
  (let ((s (frob-input-stream stream)))
    (check-type character character)
    (mezzano.gray:stream-unread-char s character)
    nil))

(defun peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  (check-type peek-type (or (eql t) (eql nil) character))
  (let ((s (frob-input-stream stream)))
    (cond ((eql peek-type nil)
           (let ((ch (mezzano.gray:stream-peek-char s)))
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
  (mezzano.gray:stream-clear-input (frob-input-stream stream))
  nil)

(defun finish-output (&optional output-stream)
  (mezzano.gray:stream-finish-output (frob-output-stream output-stream))
  nil)

(defun force-output (&optional output-stream)
  (mezzano.gray:stream-force-output (frob-output-stream output-stream))
  nil)

(defun clear-output (&optional output-stream)
  (mezzano.gray:stream-clear-output (frob-output-stream output-stream))
  nil)

(defun write-char (character &optional stream)
  (let ((s (frob-output-stream stream)))
    (check-type character character)
    (mezzano.gray:stream-write-char s character)
    character))

(defun start-line-p (&optional stream)
  (mezzano.gray:stream-start-line-p (frob-output-stream stream)))

(defun advance-to-column (column &optional stream)
  (mezzano.gray:stream-advance-to-column (frob-output-stream stream) column))

(defun line-column (&optional stream)
  (mezzano.gray:stream-line-column (frob-output-stream stream)))

(defun line-length (&optional stream)
  (mezzano.gray:stream-line-length (frob-output-stream stream)))

(defun listen (&optional input-stream)
  (mezzano.gray:stream-listen (frob-input-stream input-stream)))

(defclass case-correcting-stream (mezzano.gray:fundamental-character-output-stream)
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

(defmethod mezzano.gray:stream-write-char ((stream case-correcting-stream) character)
  (case-correcting-write character stream))

(defclass simple-edit-mixin ()
  ((edit-buffer :initform nil)
   (edit-offset :initform nil)
   (edit-handler :initform nil)))

(defmethod mezzano.gray:stream-read-char :around ((stream simple-edit-mixin))
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

(defmethod mezzano.gray:stream-clear-input :before ((stream simple-edit-mixin))
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
                      (declare (ignore ch))
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

(defclass binary-output-stream (mezzano.gray:fundamental-binary-output-stream)
  ((element-type :initarg :element-type :reader binary-output-stream-element-type)
   (vector :initarg :vector :accessor binary-output-stream-vector))
  (:default-initargs :vector nil))

(defun make-binary-output-stream (&key (element-type '(unsigned-byte 8)) (vector nil vectorp))
  (when vectorp
    (when (not (and (vectorp vector)
                    (array-has-fill-pointer-p vectorp)))
      (error "~S must be a vector with a fill-pointer" vectorp)))
  (when (not (subtypep element-type 'integer))
    (error "Element-type ~S must be a subtype of INTEGER" element-type))
  (make-instance 'binary-output-stream :element-type element-type :vector vector))

(defun get-output-stream-vector (binary-output-stream)
  (check-type binary-output-stream binary-output-stream)
  (prog1 (or (binary-output-stream-vector binary-output-stream)
             (make-array 0 :element-type (vector-output-stream-element-type binary-output-stream)))
    (setf (binary-output-stream-vector binary-output-stream) nil)))

(defmethod mezzano.gray:stream-write-byte ((stream binary-output-stream) integer)
  (unless (binary-output-stream-vector stream)
    (setf (binary-output-stream-vector stream)
          (make-array 8
                      :element-type (binary-output-stream-element-type stream)
                      :adjustable t
                      :fill-pointer 0)))
  (vector-push-extend integer (binary-output-stream-vector stream)))

(defmethod mezzano.gray:stream-write-sequence ((stream binary-output-stream) seq &optional (start 0) end)
  (setf end (or end (length seq)))
  (let ((n-bytes (- end start)))
    (unless (binary-output-stream-vector stream)
      (setf (binary-output-stream-vector stream)
            (make-array (max n-bytes 8)
                        :element-type (binary-output-stream-element-type stream)
                        :adjustable t
                        :fill-pointer 0)))
    (let* ((output (binary-output-stream-vector stream))
           (current-length (length output))
           (new-length (+ (length output) n-bytes)))
      (when (< (array-dimension output 0) new-length)
        (adjust-array output new-length))
      (setf (fill-pointer output) new-length)
      (replace output seq
               :start1 current-length
               :start2 start
               :end2 end)
      seq)))

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
  (mezzano.gray:stream-write-string (frob-output-stream stream) string start end)
  string)

(defun terpri (&optional stream)
  (mezzano.gray:stream-terpri (frob-output-stream stream))
  nil)

(defun fresh-line (&optional stream)
  (mezzano.gray:stream-fresh-line (frob-output-stream stream)))

;;; Location tracking stream. See reader.lisp for the other half.

(defgeneric location-tracking-stream-line (stream)
  (:documentation "Return STREAM's current line, or NIL.
Lines are counted from 1.")
  (:method (stream) nil))
(defgeneric location-tracking-stream-character (stream)
  (:documentation "Return STREAM's current character index, or NIL.
Characters are counted from 0.")
  (:method (stream) nil))

(defgeneric location-tracking-stream-location (stream)
  (:documentation "Return a SOURCE-LOCATION indicating the current location in the stream. Returns NIL if location tracking is unavailable.
This should only fill in the START- slots and ignore the END- slots.")
  (:method (stream) nil))

(defclass location-tracking-stream (mezzano.gray:fundamental-character-input-stream)
  ((%stream :initarg :stream :reader location-tracking-stream-stream)
   (%namestring :initarg :namestring :reader location-tracking-stream-namestring)
   (%line :initarg :line :accessor location-tracking-stream-line)
   (%character :initarg :character :accessor location-tracking-stream-character)
   (%unread-character :accessor location-tracking-stream-unread-character))
  (:default-initargs :character 0 :line 1))

(defmethod location-tracking-stream-location ((stream location-tracking-stream))
  (make-source-location
   :file (location-tracking-stream-namestring stream)
   :top-level-form-number *top-level-form-number*
   :position (let ((inner (location-tracking-stream-stream stream)))
               (if (typep inner 'file-stream)
                   (file-position inner)
                   nil))
   :line (location-tracking-stream-line stream)
   :character (location-tracking-stream-character stream)))

(defmethod mezzano.gray:stream-read-char ((stream location-tracking-stream))
  (let ((ch (read-char (location-tracking-stream-stream stream) nil :eof)))
    (cond ((eql ch :eof))
          ((eql ch #\Newline)
           (incf (location-tracking-stream-line stream))
           (setf (location-tracking-stream-unread-character stream)
                 (location-tracking-stream-character stream))
           (setf (location-tracking-stream-character stream) 0))
          (t
           (setf (location-tracking-stream-unread-character stream)
                 (location-tracking-stream-character stream))
           (incf (location-tracking-stream-character stream))))
    ch))

(defmethod mezzano.gray:stream-unread-char ((stream location-tracking-stream) character)
  (when (eql character #\Newline)
    (decf (location-tracking-stream-line stream)))
  (setf (location-tracking-stream-character stream)
        (location-tracking-stream-unread-character stream))
  (unread-char character (location-tracking-stream-stream stream)))
