(in-package :unzip)

(defstruct gzip-header
  compression-method
  flags
  modification-time
  extra-flags
  os
  part-number
  extra-field
  file-name
  file-comment
  encryption-header)

(defconstant +unix-epoch+
    (encode-universal-time 0 0 0  1 1 1970  0))

(defun prtime (ut)
  (multiple-value-bind (sec min hour day month year) (decode-universal-time ut)
    (format nil "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defun read-gzip-header (input)
  (unless (and (= #x1F (read-byte input))
               (= #x8B (read-byte input)))
    (error "No gzip header here."))
  (let (flags)
    (make-gzip-header
     :compression-method  (read-byte input)
     :flags               (setq flags (read-byte input))
     :modification-time   (+ +unix-epoch+ (read-unsigned-byte/le 32 input))
     :extra-flags         (read-byte input)
     :os                  (read-byte input)
     :part-number         (if (logbitp 1 flags) (read-unsigned-byte/le 16 input))
     :extra-field
     (if (logbitp 2 flags)
         (let* ((n (read-unsigned-byte/le 16 input))
                (r (make-array n :element-type '(unsigned-byte 8))))
           (read-sequence r input)
           r))
     :file-name           (if (logbitp 3 flags) (read-zero-terminated-string input))
     :file-comment        (if (logbitp 4 flags) (read-zero-terminated-string input))
     :encryption-header 
     (if (logbitp 5 flags)
         (let ((r (make-array 12 :element-type '(unsigned-byte 8))))
           (read-sequence r input)
           r)) )))

;;;;;;;

(defstruct zlib-header
  compression-method
  compression-info
  compression-level
  dictid)

(defun read-zlib-header (input)
  (let ((cmf (read-byte input))
        (flg (read-byte input)))
    (unless (= 0 (mod (+ (* 256 cmf) flg) 31))
      (error "Is #x~2,'0x #x~2,'0x really a ZLIB header?"
             cmf flg))
    (make-zlib-header
     :compression-method (ldb (byte 4 0) cmf)
     :compression-info   (ldb (byte 4 4) cmf)
     :compression-level  (ldb (byte 2 6) cmf)
     :dictid             (and (logbitp 5 flg)
                              (read-unsigned-byte 32 input))) ))
      


;;;;;;;;;

(defclass crc32-checksum ()
  ((crc :initform 0)))

(declaim (type (simple-array (unsigned-byte 32) (256)) *crc-table*))

(defvar *crc-table*
    (let ((res (make-array 256 :element-type '(unsigned-byte 32))))
      (dotimes (n 256)
        (let ((c n))
          (dotimes (k 8)
            (if (logbitp 0 c)
                (setf c (logxor #xEDB88320 (ash c -1)))
              (setf c (ash c -1))))
          (setf (aref res n) c)))
      res)
  "Table of CRCs of all 8-bit messages")

;; Update a running CRC with the bytes buf[0..len-1]--the CRC
;; should be initialized to all 1's, and the transmitted value
;; is the 1's complement of the final running CRC (see the
;; crc() routine below)).

(defmethod update-checksum ((self crc32-checksum) buf start end)
  (let ((crc (logxor #xFFFFFFFF (slot-value self 'crc))))
    (declare (type fixnum start end)
             (type (unsigned-byte 32) crc)
             (type (simple-array (unsigned-byte 8) (*)) buf)
             (optimize (speed 3) (safety 0)))
    (let ((table *crc-table*))
      (declare (type (simple-array (unsigned-byte 32) (256)) *crc-table*))
      (do ((i start (+ i 1)))
          ((>= i end))
        (declare (type fixnum i))
        (setf crc (logxor (aref table (logand #xFF (logxor crc (aref buf i))))
                          (ash crc -8)))))
    (setf (slot-value self 'crc) (logxor #xFFFFFFFF crc))))

(defmethod checksum-integer ((self crc32-checksum))
  (slot-value self 'crc))

;;;;

(defclass adler32-checksum ()
  ((s1 :type (unsigned-byte 16)
       :initform 1)
   (s2 :type (unsigned-byte 16)
       :initform 0)))

;; largest prime smaller than 65536
(defconstant +adler-base+ 65521)

(defmethod update-checksum ((self adler32-checksum) buffer start end)
  (let ((s1 (slot-value self 's1))
        (s2 (slot-value self 's2)))
    (declare (type (unsigned-byte 16) s1 s2)
             (type (simple-array (unsigned-byte 8) (*)) buffer)
             (type fixnum start end))
    (do ((i start (+ i 1)))
        ((>= i end)
         (setf (slot-value self 's1) (mod s1 +adler-base+)
               (slot-value self 's2) (mod s2 +adler-base+)))
      (declare (type fixnum i))
      (setf s1 (mod (+ s1 (aref buffer i)) #x10000)
            s2 (mod (+ s1 s2) #x10000)))))

(defmethod checksum-integer ((self adler32-checksum))
  (dpb (slot-value self 's2)
       (byte 16 16)
       (slot-value self 's1)))

;;;;;;;;;

(defclass inflate-stream ()
  ((buffer             :accessor buffer-of)
   (bptr               :accessor bptr-of              :initform 0)
   (bend               :accessor bend-of              :initform 0)
   (inflator           :accessor inflator-of          :initarg :inflator)
   (eofp               :accessor eofp-of              :initform nil)
   (uncompressed-size  :accessor uncompressed-size-of :initform 0)
   (format             :accessor format-of            :initarg :format)
   (input              :accessor input-of             :initarg :input)
   (checksum           :accessor checksum-of          :initarg :checksum)))

(defmethod inflate-stream-undeflow ((stream inflate-stream))
  (cond ((eofp-of stream)
         :eof)
        (t
         (multiple-value-bind (buffer start end donep)
             (step-inflator (inflator-of stream))
           (setf (eofp-of stream)   donep
                 (buffer-of stream) buffer
                 (bptr-of stream)   (+ 1 start)
                 (bend-of stream)   end)
           (when (checksum-of stream)
             (update-checksum (checksum-of stream)
                              buffer start end))
           (incf (uncompressed-size-of stream) (- end start))
           (when donep
             (case (format-of stream)
               ((:gzip)
                (let (have wanted)
                  (unless (= (setq wanted
                                   (ldb (byte 32 0) ;xxx
                                        (identity ;;lognot
                                         (read-unsigned-byte/le 32 (input-of stream)))))
                             (setq have
                                   (checksum-integer (checksum-of stream))))
                    (warn "CRC error in ~S [have #x~8,'0X, wanted #x~8,'0X]."
                           stream have wanted)))
                (let ((n (read-unsigned-byte/le 32 (input-of stream)))
                      (m (ldb (byte 32 0) (uncompressed-size-of stream))))
                  (unless (= n m)
                    (error "Uncompressed size of data, ~D, does not match ~D." 
                           n m))))
               ((:zlib)
                (unless (= (read-unsigned-byte 32 (input-of stream))
                           (checksum-integer (checksum-of stream)))
                  (warn "CRC error in ~S." stream) ))))
           (if (= start end)
               :eof
             (aref buffer start))))))

(defclass binary-inflate-stream (inflate-stream 
                                 fundamental-binary-input-stream)
  () )

(defmethod stream-read-byte ((stream binary-inflate-stream))
  (cond ((>= (bptr-of stream)
             (bend-of stream))
         (inflate-stream-undeflow stream))
        (t
         (prog1 
             (aref (buffer-of stream) (bptr-of stream))
           (incf (bptr-of stream))))))

(defclass character-input-for-binary-streams-mixin (fundamental-character-input-stream)
  ((lookahead :initform nil :accessor lookahead-of)))

(defmethod stream-read-char ((stream character-input-for-binary-streams-mixin))
  (let ((la (lookahead-of stream)))
    (if la
        (prog1
            la
          (setf (lookahead-of stream) nil))
      (let ((b (read-byte stream nil nil)))
        (if (null b)
            :eof
          (code-char b))))))

(defmethod stream-unread-char ((stream character-input-for-binary-streams-mixin) ch)
  (setf (lookahead-of stream) ch))

(defclass character-inflate-stream (character-input-for-binary-streams-mixin
                                    binary-inflate-stream)
  () )


;;;;

(defun make-inflating-stream (input
                              &key (element-type 'character)
                                   (format :raw)
                                   (checksum nil))
  (let ((valid-formats '(:raw :gzip :zlib)))
    (unless (member format valid-formats)
      (error "~S is not a valid inflate stream format; ~
              Use ~{~S~#[~; or ~:;, ~]~}."
             format valid-formats)))
  ;;
  (case format
    (:gzip
     (read-gzip-header input))
    (:zlib
     (read-zlib-header input)) )
  ;;
  (let ((inflator (make-inflator-state
                   :input input)))
    (make-instance
        (cond ((subtypep element-type 'character)
               'character-inflate-stream)
              ((subtypep element-type '(unsigned-byte 8))
               'binary-inflate-stream)
              (t
               (error "Unsupported element type: ~S" element-type)))
      :input    input
      :format   format
      :inflator inflator
      :checksum (case format
                  (:zlib (make-instance 'adler32-checksum))
                  (:gzip (make-instance 'crc32-checksum))
                  (:raw  checksum)))))

;;;;

(defmethod copy-stream ((input stream)
                        (output stream)
                        &key (buffer-size (* 8 1024)))
  (let ((buffer (make-array buffer-size
                            :element-type (stream-element-type input))))
    (declare (dynamic-extent buffer))
    (do ((n (read-sequence buffer input)
            (read-sequence buffer input)))
        ((= n 0))
      (write-sequence buffer output :end n))))

(defun zcat (filename-or-stream)
  (cond ((streamp filename-or-stream)
         (copy-stream (make-inflating-stream filename-or-stream
                                             :format :gzip)
                      *standard-output*))
        (t
         (with-open-file (i filename-or-stream
                            :element-type '(unsigned-byte 8))
           (copy-stream (make-inflating-stream i :format :gzip)
                        *standard-output*)))))

;;; References
;;
;;  - RFC1950, ZLIB Compressed Data Format Specification version 3.3
;;  - RFC1951, DEFLATE Compressed Data Format Specification version 1.3
;;  - RFC1952, GZIP file format specification version 4.3
;;
