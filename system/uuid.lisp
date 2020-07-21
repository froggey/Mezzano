;;======================================================================
;; UUID Layout and Byte Order (per RFC 4122)
;;
;;     Field                      Data Type         Octet #
;; time-low               unsigned 32 bit integer     0-3
;; time-mid               unsigned 16 bit integer     4-5
;; time-hi-and-version    unsigned 16 bit interger    6-7
;; clock-seq-hi-and-
;;     reserved           unsigned  8 bit integer       8
;; clock-seq-low          unsigned  8 bit integer       9
;; node                   unsigned 48 bit integer    10-15
;;
;; All of the field are read/written as big endian
;;
;;======================================================================

(in-package :mezzano.uuid)

(defun generate-uuid (&key (result-type 'string))
  "Generate version 4 UUID from pseudo-random numbers. Formats supported ~
   are: list, vector or string."
  ;; Set bits 6 & 7 of clock-seq-hi-and-reserved to 0 and 1 respectively
  ;; Set the four most significant bits (12 - 15) of
  ;; time-hi-and-version field to #b0100
  (ecase result-type
    (list
     (loop
        for i from 0 to 15
        collect (case i
                  (6 (logior #x40 (random 16)))
                  (8 (logior (random 64) #x80))
                  (t (random 256)))))
    (vector
     (let ((uuid (make-array 16 :element-type '(unsigned-byte 8))))
       (dotimes (i 16)
         (setf (aref uuid i) (case i
                               (6 (logior #x40 (random 16)))
                               (8 (logior (random 64) #x80))
                               (t (random 256)))))
       uuid))
    (string
     (loop
        with result = (make-string 36 :initial-element #\-)
        with idx = 0
        for i from 0 to 15
        for value = (case i
                      (6 (logior #x40 (random 16)))
                      (8 (logior (random 64) #x80))
                      (t (random 256)))
        for digit-1 = (char-downcase (digit-char (logand (ash value -4) #x0F) 16))
        for digit-2 = (char-downcase (digit-char (logand value #x0F) 16))
        do
          (setf (elt result idx) digit-1
                (elt result (1+ idx)) digit-2)
          (incf idx 2)
          (when (member idx '(8 13 18 23))
            (incf idx))
        finally
          (return result)))))

(defun generate-uuid-buffer (buffer &key (offset 0))
  "Generate version 4 UUID in a buffer from pseudo-random numbers."
  ;; Set bits 6 & 7 of clock-seq-hi-and-reserved to 0 and 1 respectively
  ;; Set the four most significant bits (12 - 15) of
  ;; time-hi-and-version field to #b0100
  (dotimes (i 16)
    (setf (aref buffer (+ offset i)) (case i
                                       (6 (logior #x40 (random 16)))
                                       (8 (logior (random 64) #x80))
                                       (t (random 256)))))
  buffer)

(defun uuid-buffer->string (buffer &key (offset 0))
  "Convert uuid in a buffer to a string (Uniform Resource Name)"
  (loop
     with result = (make-string 36 :initial-element #\-)
     with idx = 0
     for i from 0 to 15
     for value = (aref buffer (+ offset i))
     for digit-1 = (char-downcase (digit-char (logand (ash value -4) #x0F) 16))
     for digit-2 = (char-downcase (digit-char (logand value #x0F) 16))
     do
       (setf (elt result idx) digit-1
             (elt result (1+ idx)) digit-2)
       (incf idx 2)
       (when (member idx '(8 13 18 23))
         (incf idx))
     finally
       (return result)))

(defun format-uuid (stream object &optional colon-p at-sign-p)
  "Format a UUID, for use with ~/"
  (declare (ignore colon-p at-sign-p))
  (write-string (uuid-buffer->string object) stream))

(defun uuid-string-valid-p (uuid)
  (and (stringp uuid)
       (= (length uuid) 36)
       (loop
          for i from 0 to 35 do
            (cond ((member i '(8 13 18 23))
                   (when (not (eql (char uuid i) #\-))
                     (return NIL)))
                  (T
                   (when (not (digit-char-p (char uuid i) 16))
                     (return NIL))))
          finally
            (return T))))

(defun string->uuid-buffer (uuid buffer &key (offset 0))
  "Convert a string (Uniform Resource Name) to a uuid in a buffer"
  (assert (uuid-string-valid-p uuid))
  (loop
     with idx = 0
     for i from 0 to 15
     do
       (setf (aref buffer (+ offset i))
             (parse-integer uuid :radix 16 :start idx :end (+ idx 2)))
       (incf idx 2)
       (when (member idx '(8 13 18 23))
         (incf idx)))
  buffer)

(defun string->uuid (uuid)
  "Convert a string (Uniform Resource Name) to a uuid in a new buffer"
  (assert (uuid-string-valid-p uuid))
  (string->uuid-buffer uuid (make-array 16 :element-type '(unsigned-byte 8))))
