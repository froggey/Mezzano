;;;; External formats

(in-package :mezzano.internals)

(defparameter *default-external-format* :utf-8)

(defun string-to-octets (sequence &key (start 0) end (eol-style :crlf) nul-terminate coding)
  (when (or (not coding) (eql coding :default))
    (setf coding *default-external-format*))
  (ecase coding
    (:utf-8 (encode-utf-8-string sequence
                                 :start start
                                 :end end
                                 :eol-style eol-style
                                 :nul-terminate nul-terminate))))

(defun octets-to-string (sequence &key (start 0) end (eol-style :crlf) nul-terminate coding)
  (when (or (not coding) (eql coding :default))
    (setf coding *default-external-format*))
  (ecase coding
    (:utf-8 (decode-utf-8-string sequence
                                 :start start
                                 :end end
                                 :eol-style eol-style
                                 :nul-terminate nul-terminate))))

(defun utf-8-decode-leader (leader)
  "Break a UTF-8 leader byte apart into sequence length (minus one) and code-point so far."
  (cond ((eql (logand leader #b10000000) 0)
         (values 0 leader))
        ((eql (logand leader #b11100000) #b11000000)
         (values 1 (logand leader #b00011111)))
        ((eql (logand leader #b11110000) #b11100000)
         (values 2 (logand leader #b00001111)))
        ((eql (logand leader #b11111000) #b11110000)
         (values 3 (logand leader #b00000111)))))

(defun utf-8-continuation-byte-p (byte)
  (eql (logand byte #b11000000) #b10000000))

(defun size-encoded-utf-8-string (sequence &key (start 0) end (eol-style :crlf) nul-terminate)
  (setf end (or end (length sequence)))
  (let ((n-bytes 0)
        (eol-size (ecase eol-style
                    ((:crlf :lfcr) 2)
                    ((:lf :lf-ignore-cr :cr) 1))))
    (dotimes (i (- end start))
      (let ((c (char sequence (+ start i))))
        (cond
          ((eql c #\Newline)
           (incf n-bytes eol-size))
          (t
           (let ((code (char-code c)))
             (unless (zerop (sys.int::char-bits c))
               (setf code (char-code #\REPLACEMENT_CHARACTER)))
             (unless (and (<= 0 code #x1FFFFF)
                          (not (<= #xD800 code #xDFFF)))
               (setf code (char-code #\REPLACEMENT_CHARACTER)))
             (cond ((<= code #x7F)
                    (incf n-bytes 1))
                   ((<= #x80 code #x7FF)
                    (incf n-bytes 2))
                   ((or (<= #x800 code #xD7FF)
                        (<= #xE000 code #xFFFF))
                    (incf n-bytes 3))
                   ((<= #x10000 code #x10FFFF)
                    (incf n-bytes 4))))))))
    (when nul-terminate
      (incf n-bytes))
    n-bytes))

(defun encode-utf-8-string (sequence &key (start 0) end (eol-style :crlf) nul-terminate)
  (setf end (or end (length sequence)))
  ;; Precompute the size of the resulting vector to avoid having to resize it.
  (let ((bytes (make-array (size-encoded-utf-8-string
                            sequence
                            :start start
                            :end end
                            :eol-style eol-style
                            :nul-terminate nul-terminate)
                           :element-type '(unsigned-byte 8)))
        (offset 0))
    (declare (optimize speed)
             (type string sequence)
             (type (simple-array (unsigned-byte 8) (*)) bytes)
             (type fixnum offset))
    (flet ((out (byte)
             (setf (aref bytes offset) byte)
             (incf offset)))
      (loop
         for i fixnum from start below end
         for c = (char sequence i)
         do
           (cond
             ((eql c #\Newline)
              (ecase eol-style
                (:crlf
                 (out #x0D)
                 (out #x0A))
                (:lfcr
                 (out #x0A)
                 (out #x0D))
                ((:lf :lf-ignore-cr)
                 (out #x0A))
                (:cr
                 (out #x0D))))
             (t (let ((code (char-code c)))
                  (unless (zerop (sys.int::char-bits c))
                    (setf code (char-code #\REPLACEMENT_CHARACTER)))
                  (unless (and (<= 0 code #x1FFFFF)
                               (not (<= #xD800 code #xDFFF)))
                    (setf code (char-code #\REPLACEMENT_CHARACTER)))
                  (cond ((<= code #x7F)
                         (out code))
                        ((<= #x80 code #x7FF)
                         (out (logior (ash (logand code #x7C0) -6) #xC0))
                         (out (logior (logand code #x3F) #x80)))
                        ((or (<= #x800 code #xD7FF)
                             (<= #xE000 code #xFFFF))
                         (out (logior (ash (logand code #xF000) -12) #xE0))
                         (out (logior (ash (logand code #xFC0) -6) #x80))
                         (out (logior (logand code #x3F) #x80)))
                        ((<= #x10000 code #x10FFFF)
                         (out (logior (ash (logand code #x1C0000) -18) #xF0))
                         (out (logior (ash (logand code #x3F000) -12) #x80))
                         (out (logior (ash (logand code #xFC0) -6) #x80))
                         (out (logior (logand code #x3F) #x80))))))))
      (when nul-terminate
        (out 0)))
    bytes))

(defun decode-utf-8-string (sequence &key (start 0) end (eol-style :crlf) nul-terminate)
  (setf end (or end (length sequence)))
  (let ((position start)
        (string (make-array (- end start)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
        (eol-state nil))
    (flet ((next ()
             (cond ((< position end)
                    (prog1
                        (elt sequence position)
                      (incf position)))
                   (t nil))))
      (loop
         (let ((leader (next)))
           (when (not leader)
             ;; End of text.
             (when (and nul-terminate
                        (not (eql (length string) 0))
                        (eql (char string (1- (length string))) (code-char 0)))
               (vector-pop string))
             (return string))
           (multiple-value-bind (len code-point)
               (utf-8-decode-leader leader)
             (dotimes (i len)
               (let ((byte (next)))
                 (when (or (not byte)
                           (not (utf-8-continuation-byte-p byte)))
                   (setf code-point (char-code #\REPLACEMENT_CHARACTER))
                   (return))
                 (setf code-point (logior (ash code-point 6)
                                          (ldb (byte 6 0) byte)))))
             (when (not (unicode-scalar-value-p code-point))
               (setf code-point (char-code #\REPLACEMENT_CHARACTER)))
             (let ((char (or (code-char code-point)
                             #\REPLACEMENT_CHARACTER)))
             (ecase eol-style
               (:cr
                (when (eql char #\Cr) (setf char #\Newline))
                (vector-push-extend char string))
               (:lf
                (when (eql char #\Lf) (setf char #\Newline))
                (vector-push-extend char string))
               (:crlf
                (cond (eol-state
                       ;; Saw CR, expecing LF.
                       (setf eol-state nil)
                       (cond ((eql char #\Lf)
                              (vector-push-extend #\Newline string))
                             (t
                              ;; Something else, release the Cr and whatever this is.
                              (vector-push-extend #\Cr string)
                              (vector-push-extend char string))))
                      (t
                       (cond ((eql char #\Cr)
                              (setf eol-state t))
                             (t
                              (vector-push-extend char string))))))
               (:lfcr
                (cond (eol-state
                       ;; Saw LF, expecing CR.
                       (setf eol-state nil)
                       (cond ((eql char #\Cr)
                              (vector-push-extend #\Newline string))
                             (t
                              ;; Something else, release the Lf and whatever this is.
                              (vector-push-extend #\Lf string)
                              (vector-push-extend char string))))
                      (t
                       (cond ((eql char #\Lf)
                              (setf eol-state t))
                             (t
                              (vector-push-extend char string))))))))))))))

(defclass external-format ()
  ((%coding :initarg :coding :reader external-format-coding)
   (%eol-style :initarg :eol-style :reader external-format-eol-style
               :type (member :lf-ignore-cr :cr :lf :crlf :lfcr))
   (%pending-read-character :initform nil)
   (%accumulated-code-point :initform nil)
   (%code-point-bytes-remaining :initform nil)
   (%accumulated-eol :initform nil))
  (:default-initargs :eol-style :lf))

(defmethod print-object ((instance external-format) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S :Eol-Stype ~S"
            (external-format-coding instance)
            (external-format-eol-style instance))))

(defun make-external-format (element-type external-format &rest default-initargs)
  (when (type-equal element-type '(unsigned-byte 8))
    (when (not (eql external-format :default))
      (error "Unsupported external format ~S for (UNSIGNED-BYTE 8) element-type. Only :DEFAULT is supported."
             external-format))
    (return-from make-external-format :default))
  (when (not (type-equal element-type 'character))
    (error "Unsupported element-type ~S. Supported element types are (UNSIGNED-BYTE 8) and CHARACTER."
           element-type))
  (when (typep external-format 'external-format)
    (return-from make-external-format
      (apply #'make-instance 'external-format
             (append default-initargs
                     (list :coding (external-format-coding external-format)
                           :eol-style (external-format-eol-style external-format))))))
  (when (eql external-format :default)
    (setf external-format *default-external-format*))
  (when (and (consp external-format)
             (eql (first external-format) :default))
    (error ":DEFAULT cannot be specified as the name of complex external-format specifier ~S" external-format))
  (multiple-value-bind (name initargs)
      (if (consp external-format)
          (values (first external-format) (rest external-format))
          (values external-format '()))
    (assert (eql name :utf-8))
    (apply #'make-instance
           'external-format
           :coding name
           (append initargs
                   default-initargs))))

(deftype unicode-code-point ()
  `(integer 0 #x0010FFFF))

(defun unicode-scalar-value-p (code)
  (and (typep code 'unicode-code-point)
       (not (<= #xD800 code #xDFFF)) ; UTF-16 surrogates.
       ;; Noncharacters.
       (not (<= #xFDD0 code #xFDEF))
       ;; The final two code points in each plane are noncharacters.
       (not (eql (logand code #xFFFE) #xFFFE))))

(defun external-format-read-internal-code-point (external-format source read-fn)
  ;; Drive the receive state machine through to read an entire character or
  ;; until there are no more bytes available.
  (when (not (slot-value external-format '%accumulated-code-point))
    ;; At the start, looking to read a leader byte.
    (let ((next-byte (funcall read-fn source)))
      (when (eql next-byte :eof)
        ;; At EOF and on a code point boundary.
        (return-from external-format-read-internal-code-point :eof))
      (when (not next-byte)
        ;; No bytes available.
        (return-from external-format-read-internal-code-point nil))
      (multiple-value-bind (len code-point)
          (utf-8-decode-leader next-byte)
        (when (not len)
          ;; Error decoding, read a replacement char.
          (return-from external-format-read-internal-code-point :error))
        (setf (slot-value external-format '%accumulated-code-point) code-point)
        (setf (slot-value external-format '%code-point-bytes-remaining) len))))
  (loop
     (when (eql (slot-value external-format '%code-point-bytes-remaining) 0)
       ;; Read an entire character.
       (let ((cp (slot-value external-format '%accumulated-code-point)))
         (setf (slot-value external-format '%accumulated-code-point) nil)
         (return (if (unicode-scalar-value-p cp)
                     (code-char cp)
                     :error))))
     (let ((continuation (funcall read-fn source)))
       (when (eql continuation :eof)
         ;; At EOF partway through a character, this is a partial character
         ;; and an error.
         (return-from external-format-read-internal-code-point :error))
       (when (not continuation)
         ;; Nothing pending, leave the partially read character in place.
         (return nil))
       (when (not (utf-8-continuation-byte-p continuation))
         ;; This is either a random byte or the start of a new character.
         ;; Either way the current character is corrupt.
         ;; Return an error, but attempt to reset the state machine based
         ;; on this character. If it's not a leader, then the two broken
         ;; code points will be mashed together into a single error otherwise
         ;; the stream will resynchronize.
         (multiple-value-bind (len code-point)
             (utf-8-decode-leader continuation)
           (cond (len
                  ;; Valid leader, restart reading continuation bytes.
                  (setf (slot-value external-format '%accumulated-code-point) code-point)
                  (setf (slot-value external-format '%code-point-bytes-remaining) len))
                 (t
                  ;; Invalid, reset and drop this byte.
                  (setf (slot-value external-format '%accumulated-code-point) nil)))
           (return :error)))
       (decf (slot-value external-format '%code-point-bytes-remaining))
       (setf (slot-value external-format '%accumulated-code-point)
             (logior (ash (slot-value external-format '%accumulated-code-point) 6)
                     (logand continuation #b00111111))))))

(defun external-format-cr/lf-processing (external-format source read-fn first-char second-char)
  (when (and (slot-value external-format '%accumulated-eol)
             (not (eql (slot-value external-format '%accumulated-eol) t)))
    ;; Previously saw some weird character after CR, return that.
    (return-from external-format-cr/lf-processing
      (prog1 (slot-value external-format '%accumulated-eol)
        (setf (slot-value external-format '%accumulated-eol) nil))))
  (when (not (slot-value external-format '%accumulated-eol))
    ;; Waiting for the first char.
    (let ((ch (external-format-read-internal-code-point external-format source read-fn)))
      (when (not (eql ch first-char))
        ;; Not the first char, just return it.
        (return-from external-format-cr/lf-processing ch)))
    (setf (slot-value external-format '%accumulated-eol) t))
  ;; Saw the first char, waiting for the second
  (let ((ch (external-format-read-internal-code-point external-format source read-fn)))
    (cond ((not ch)
           ;; waiting for character.
           nil)
          ((eql ch :eof)
           ;; At end of file, return just the first char without converting it.
           (setf (slot-value external-format '%accumulated-eol) nil)
           first-char)
          ((eql ch second-char)
           ;; Got a complete sequence.
           (setf (slot-value external-format '%accumulated-eol) nil)
           #\Newline)
          (t
           ;; Saw something else (a character or error), return the first char
           ;; and accumulate whatever the mystery character is.
           (setf (slot-value external-format '%accumulated-eol) ch)
           first-char))))

(defun external-format-read-internal (external-format source read-fn)
  (ecase (external-format-eol-style external-format)
    (:lf-ignore-cr
     ;; Pass LF (Newline) through unmodified, drop all CR (Return) characters.
     (loop
        (let ((ch (external-format-read-internal-code-point external-format source read-fn)))
          (when (not (eql ch #\Cr))
            (return ch)))))
    (:lf
     (external-format-read-internal-code-point external-format source read-fn))
    (:cr
     ;; Rewrite CR characters to Newline
     (let ((ch (external-format-read-internal-code-point external-format source read-fn)))
       (if (eql ch #\Cr)
           #\Newline
           ch)))
    (:crlf
     (external-format-cr/lf-processing external-format source read-fn #\Cr #\Lf))
    (:lfcr
     (external-format-cr/lf-processing external-format source read-fn #\Lf #\Cr))))

(defun external-format-listen (external-format source)
  (when (slot-value external-format '%pending-read-character)
    (return-from external-format-listen t))
  (let ((ch (external-format-read-internal external-format source 'mezzano.gray:stream-read-byte-no-hang)))
    (case ch
      ((nil :eof) nil)
      ((:error)
       (setf (slot-value external-format '%pending-read-character)
             #\REPLACEMENT_CHARACTER)
       t)
      (t
       (setf (slot-value external-format '%pending-read-character) ch)
       t))))

(defun external-format-read-char-no-hang (external-format source)
  (when (slot-value external-format '%pending-read-character)
    (return-from external-format-read-char-no-hang
      (prog1
          (slot-value external-format '%pending-read-character)
        (setf (slot-value external-format '%pending-read-character) nil))))
  (let ((ch (external-format-read-internal external-format source 'mezzano.gray:stream-read-byte-no-hang)))
    (case ch
      ((nil) nil)
      ((:eof) :eof)
      ((:error)
       #\REPLACEMENT_CHARACTER)
      (t
       ch))))

(defun external-format-read-char (external-format source)
  (when (slot-value external-format '%pending-read-character)
    (return-from external-format-read-char
      (prog1
          (slot-value external-format '%pending-read-character)
        (setf (slot-value external-format '%pending-read-character) nil))))
  (let ((ch (external-format-read-internal external-format source 'mezzano.gray:stream-read-byte)))
    (case ch
      ((nil) (error "Impossible!")) ; only happens in listen/-no-hang
      ((:eof) :eof)
      ((:error)
       #\REPLACEMENT_CHARACTER)
      (t
       ch))))

(defun external-format-read-sequence (external-format source sequence &optional (start 0) end)
  (when (not end)
    (setf end (length sequence)))
  (dotimes (i (- end start) end)
    (let ((elt (external-format-read-char external-format source)))
      (if (eql elt :eof)
          (return (+ start i))
          (setf (elt sequence (+ start i)) elt)))))

(defun external-format-clear-input (external-format)
  (setf (slot-value external-format '%pending-read-character) nil
        (slot-value external-format '%accumulated-code-point) nil
        (slot-value external-format '%code-point-bytes-remaining) nil
        (slot-value external-format '%accumulated-eol) nil))

(defun external-format-write-char (external-format source character)
  (external-format-write-sequence external-format source (string character)))

(defun external-format-write-sequence (external-format source sequence &optional (start 0) end)
  (mezzano.gray:stream-write-sequence
   source
   (encode-utf-8-string sequence
                        :start start
                        :end end
                        :eol-style (external-format-eol-style external-format))))

(defclass external-format-mixin ()
  ((%external-format :initarg :external-format :reader stream-external-format)))

(defmethod mezzano.gray:stream-listen ((stream external-format-mixin))
  (external-format-listen
   (stream-external-format stream)
   stream))

(defmethod mezzano.gray:stream-read-char ((stream external-format-mixin))
  (external-format-read-char
   (stream-external-format stream)
   stream))

(defmethod mezzano.gray:stream-read-char-no-hang ((stream external-format-mixin))
  (external-format-read-char-no-hang
   (stream-external-format stream)
   stream))

(defmethod mezzano.gray:stream-read-sequence ((stream external-format-mixin) sequence &optional (start 0) end)
  ;; Like the default stream-read-sequence, default to reading characters
  ;; unless the vector is an integer vector.
  (if (typep sequence '(vector (unsigned-byte 8)))
      (call-next-method)
      (external-format-read-sequence
       (stream-external-format stream)
       stream
       sequence start end)))

(defmethod mezzano.gray:stream-write-char ((stream external-format-mixin) character)
  (external-format-write-char
   (stream-external-format stream)
   stream
   character)
  character)

(defmethod mezzano.gray:stream-write-sequence ((stream external-format-mixin) sequence &optional (start 0) end)
  (if (typep sequence '(vector (unsigned-byte 8)))
      (call-next-method)
      (external-format-write-sequence
       (stream-external-format stream)
       stream
       sequence start end)))

(defmethod stream-element-type ((stream external-format-mixin))
  'character)

(defmethod mezzano.gray:stream-clear-input :after ((stream external-format-mixin))
  (external-format-clear-input (stream-external-format stream)))

(defmethod mezzano.gray:stream-file-position :after ((stream external-format-mixin) &optional (position-spec nil position-specp))
  (declare (ignore position-spec))
  (when position-specp
    (external-format-clear-input (stream-external-format stream))))

(defmethod mezzano.gray:external-format-string-length ((external-format external-format) string)
  (length (encode-utf-8-string string :eol-style (external-format-eol-style external-format))))
