(in-package :bitio)

;; Assumptions:
;; 1) The individual bits of an octet have canonical positions of:
;; [2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0]
;; within the octet. We label the left side of the octect as the MSBit and the
;; right side as the LSBit. The bit position of the right most bit is 0.
;;
;; 2) We assume that when reading octets from fast-io, all octets are in the
;; canonical form.
;;
;; 3) A canonical integer has its bits in the form of:
;; [2^N ... 2^3 2^2 2^1 2^0]
;; The bit position of the right most bit is 0.


(defun fill-read-stable (bitio &optional (eof-error-p T) (eof-value NIL))
  "This function asserts that the stable is empty, then fills it with a
single octet from the stream. Returns T if stable was filled or NIL if it
wasn't (due to an EOF)."
  (assert (zerop (num-bits-in-stable bitio)))

  (let ((result (bitio/read-octet bitio eof-error-p eof-value)))
    (cond
      ;; Oops, had an EOF
      ((equal result eof-value)
       (setf (num-bits-in-stable bitio) 0
             (read-bit-stable bitio) 0)
       NIL)
      ;; all good.
      (t
       (setf (num-bits-in-stable bitio) 8
             (read-bit-stable bitio) result)
       T))))

(defun fast-path/octet-aligned-bit-read (bitio bit-read-count bit-endian
                                         &optional (eof-error-p T)
                                           (eof-value NIL))
  "This function assumes the read-bit-stable is empty in the BITIO stream
and BIT-READ-COUNT is a multiple of 8.  It reads BIT-READ-COUNT bits
from the BITIO stream (reading the actual bits in each octet in
BIT-ENDIAN manner) and then places them into an integer that it returns.
The integer is in canonical form with the first bit read in the MSBit
position."
  (when (zerop bit-read-count)
    (return-from fast-path/octet-aligned-bit-read (values 0 0)))

  (let* ((original-required-octets (/ bit-read-count 8))
         (required-octets original-required-octets)
         (value 0)
         (num-octets-read 0)
         (num-octets-to-read
           (min required-octets (length (octet-read-buffer bitio))))
         (endian-func (if (eq bit-endian :be) #'identity #'octet-reverse)))

    (loop
      :while (> num-octets-to-read 0)
      :do
         ;; use a fast read-sequence to get the octet into the backing buffer.
         (let ((octets-read (funcall (%bitio/read-sequence bitio)
                                     (octet-read-buffer bitio)
                                     (octet-stream bitio)
                                     :start 0 :end num-octets-to-read)))

           ;; place the bit endian manipulated octets into the right place in
           ;; the value
           (loop :for i :below octets-read
                 :do (setf (ldb (byte 8 (- (* (1- original-required-octets) 8)
                                           (* (+ num-octets-read i) 8)))
                                value)
                           (funcall endian-func
                                    (aref (octet-read-buffer bitio) i))))

           ;; account for the octets I just read.
           (decf required-octets octets-read)
           (incf num-octets-read octets-read)

           ;; If there was a short read, we fixup the value and we're done.
           (when (/= octets-read num-octets-to-read)
             ;; short read case, must have hit eof
             (return-from fast-path/octet-aligned-bit-read
               (values (if (> num-octets-read 0)
                           ;; Shift into canonical form to remove
                           ;; unreadable bits.
                           (ash value (- (- (* original-required-octets 8)
                                            (* num-octets-read 8))))
                           ;; But if they insist upon the read again, they
                           ;; get the eof-value they supplied.
                           eof-value)
                       (* num-octets-read 8))))

           (setf num-octets-to-read (min required-octets
                                         (length (octet-read-buffer bitio))))))

    (values value (* num-octets-read 8))))


;; Return two values:
;; the value holding the bits in canonical form
;; the number of bits read
(defun consume-read-bit-stable (bitio bit-read-count bit-endian)
  (when (zerop (num-bits-in-stable bitio))
    (return-from consume-read-bit-stable (values 0 0)))

  ;; Figure out how many bits to read from the read-bit-stable
  (let* ((num-selected-bits (min (num-bits-in-stable bitio)
                                 bit-read-count))
         (bit-start-position (if (eq bit-endian :le)
                                 0
                                 (- (num-bits-in-stable bitio)
                                    num-selected-bits)))
         (byte-specification (byte num-selected-bits bit-start-position)))

    (let ((value 0))
      ;; First, we grab the bits we need out of the stable and put them in
      ;; the :le end of the value
      (setf value (ldb byte-specification (read-bit-stable bitio)))
      ;; Then, depending on bit endian, we clean up the value and bit stable.
      (case bit-endian
        (:le
         ;; Shift the bit stable to the right by the number of bits I collected.
         ;; This gets rid of those bits in the stable.
         (setf (read-bit-stable bitio)
               (ash (read-bit-stable bitio) (- num-selected-bits)))
         ;; Then reverse the bits, cause they are to come out in :le order
         (let ((result (integer-reverse value num-selected-bits)))
           ;; remove the accounting of the bits from the stable.
           (decf (num-bits-in-stable bitio) num-selected-bits)
           (values result num-selected-bits)))

        (:be
         ;; Zero out the :be stable bits we just took out.
         (setf (ldb byte-specification (read-bit-stable bitio))
               0)
         ;; remove the accounting of the bits from the stable.
         (decf (num-bits-in-stable bitio) num-selected-bits)
         (values value num-selected-bits))))))

(defun slow-path/octet-unaligned-bit-read (bitio bit-read-count bit-endian
                                           &optional (eof-error-p T)
                                             (eof-value NIL))

  ;; This work occurs in three phases.

  (let ((bits-remaining-to-read bit-read-count))
    ;; Phase I, we consume anything we need from the stable.
    ;; Note: we can only consume up to the amount in the stable, so
    ;; if there is an EOF we cannot detect it here yet.
    (multiple-value-bind (prefix-stable-bits num-prefix-stable-bits-read)
        (consume-read-bit-stable bitio bits-remaining-to-read bit-endian)

      (decf bits-remaining-to-read num-prefix-stable-bits-read)

      ;; Determine how many complete octets we can read and
      ;; how many left over bits we have with respect to an octet width.
      (multiple-value-bind (octets-to-read bits-left-over)
          (floor bits-remaining-to-read 8)

        ;; Phase II, Fast read that many octets
        ;; Note: Here is the first place we can discover an EOF.
        (multiple-value-bind (octet-bits num-octet-bits-read)
            (fast-path/octet-aligned-bit-read bitio
                                              (* octets-to-read 8)
                                              bit-endian
                                              eof-error-p
                                              eof-value)
          (decf bits-remaining-to-read num-octet-bits-read)

          ;; Short read / EOF check.
          ;; Detect if we reached a short read or EOF during the fast
          ;; read attempt.
          (cond
            ;; oops! EOF Found on octet read
            ((equal octet-bits eof-value)
             ;; Check if we had read any bits from the stable...
             (if (> num-prefix-stable-bits-read 0)
                 ;; We did! So return them as a short read.
                 (return-from slow-path/octet-unaligned-bit-read
                   (values prefix-stable-bits num-prefix-stable-bits-read))
                 ;; We didn't! So return the true eof.
                 (return-from slow-path/octet-unaligned-bit-read
                   (values octet-bits num-octet-bits-read))))

            ;; Octet-read ok, but short read just before EOF.
            ((/= num-octet-bits-read (* octets-to-read 8))
             ;; Here we're assuming we may or may not have read
             ;; something from the stable.
             (return-from slow-path/octet-unaligned-bit-read
               (values (logior octet-bits
                               (ash prefix-stable-bits num-octet-bits-read))
                       (+ num-octet-bits-read num-prefix-stable-bits-read)))))


          ;; Phase III, if there are bits-left-over, they will always fit
          ;; into a octet, so fill the stable...
          (when (> bits-left-over 0)
            (let ((eof-p (fill-read-stable bitio eof-error-p eof-value)))
              ;; However, we check for EOF.
              (unless eof-p
                (return-from slow-path/octet-unaligned-bit-read
                  ;; We discovered that we can't fill the remaining
                  ;; bits because we hit an EOF while getting the
                  ;; octet.
                  ;;
                  ;; So, determine if we have any bits ready to go, and
                  ;; if not, then bail with the true eof value, otherwise,
                  ;; return the necessary bits. And the EOF will get processed
                  ;; in the next call.
                  (if (> (+ num-octet-bits-read num-prefix-stable-bits-read) 0)
                      ;; return whatever bits we copuld have read up to now.
                      (values
                       (logior octet-bits
                               (ash prefix-stable-bits num-octet-bits-read))
                       (+ num-octet-bits-read num-prefix-stable-bits-read))
                      ;; we're done, eof found, full stop.
                      (values eof-value 0))))))


          (assert (eql bits-remaining-to-read bits-left-over))

          ;; And consume any bits-left-over. We can't get an EOF consuming
          ;; from the stable once it has been filled.
          (multiple-value-bind (suffix-stable-bits
                                num-suffix-stable-bits-read)
              (consume-read-bit-stable bitio bits-remaining-to-read bit-endian)

            ;; Now, we have three groups of bits, assemble them into the
            ;; answer we seek.
            (let ((result
                    (logior suffix-stable-bits
                            (ash octet-bits
                                 num-suffix-stable-bits-read)
                            (ash prefix-stable-bits
                                 (+ num-suffix-stable-bits-read
                                    num-octet-bits-read)))))

              (values result (+ num-prefix-stable-bits-read
                                num-octet-bits-read
                                num-suffix-stable-bits-read)))))))))

;; EXPORT
;; This is the thing that gets the bits from the octet stream. It returns them
;; as a values of the integer and how many bits read. Return a values of
;; the bits in canonical order in an integer, and how many bits are in that
;; canonical integer.
(defun read-bits (bitio bit-read-count
                  &key bit-endian (eof-error-p T) (eof-value NIL))
  "Before describing how this function works, we'll describe the form the
octets are in from the underlying octet-stream that the BITIO instance
contains. The octets are in a canonical form, with bits written left to right
and given labels:

      octet:  [2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0]
  bit label:  [ a   b   c   d   e   f   g   h ]

We call bit 'a' the MSBit and bit 'h' the LSBit of the octet.

So, when we strip off bits from the octets, we look at BIT-ENDIAN to
determine which side of the octet we are getting the bits.  If it
is :BE, we take the bits from the MSBit side of the octet, and if it
is :LE we take the bits form the LSBit side of the octet.

An example call of taking 8 bits from the BIT-ENDIAN :BE direction
will result in the function returns two values: the unsigned integer
with the bits in these positions: 'abcdefgh' and the number of bits read,
in this case 8.

In a different scenario, we might read 5 bits :BE, to return the values:
 ('abcde' 5), and then read 3 bits :LE, which returns the values: ('hgf' 3).

Suppose we read 12 bits :LE from the start of an octet boundary. The
underlying octet stream might look like this in canonical form with
the left octet being the next octet ready to read:

[abcdefgh][ijklmnop][...]...

Then, we read 12 bits in this order: hgfedcbaponm and return the
values of it and 12 as the bits read. This read consumes the first
octet starting from the :LE side, then half of the second octet
starting from the :LE side, leaving the bits 'ijkl' in the second
octet to be read in the next read-bits call, however you want to
read them, plus any additional bits from additional octet later in the
stream.

It is not required that you read 8 bit multiples or that those reads are
aligned to the underlying octet boundaries in the stream. But, if
EOF-ERROR-P and EOF-VALUE are used as in READ, you can know if you hit
EOF properly. If you try to read X number of bits, but hit EOF while
getting them, the bits of the short read will be returned and the number
of successful bits read returned. It is recommended that you check the
number of bits you expected to read to ensure the value is what you
expect."

  (let ((bit-endian (or bit-endian (default-bit-endian bitio)))
        (bit-read-count (or bit-read-count (default-bits-per-byte bitio))))
    (when (zerop bit-read-count)
      (return-from read-bits (values 0 0)))

    (cond
      ;; This is a fast path read of the bits.
      ((and
        ;; nothing in the stable.
        (zerop (num-bits-in-stable bitio))
        ;; asking to read a divisible by 8 number of bits (so I can get them
        ;; as complete octets from the octet-stream)
        (zerop (mod bit-read-count 8)))

       (fast-path/octet-aligned-bit-read bitio
                                         bit-read-count
                                         bit-endian
                                         eof-error-p eof-value))

      ;; This is the slow path, where I may need to handle partial octets
      ;; and bits stored in the stable.
      (t
       (slow-path/octet-unaligned-bit-read bitio
                                           bit-read-count bit-endian
                                           eof-error-p eof-value)))))

;; EXPORT
(defun read-one-byte (bitio
                      &key bits-per-byte bit-endian (eof-error-p T) (eof-value NIL))
  "Read a single unsigned 'byte' from the bitio stream. You must
specify the BIT-ENDIAN mode (:BE or :LE, See READ-BITS) and how
big the byte is in bits: BITS-PER-BYTE. You can supply the optional
keywords EOF-ERROR-P and EOF-VALUE as in READ-BYTE). The returned
value is always unsigned. If the number of bits requested is more than
is in the stream, you will get a short read of bits, so it is
recommended to check the return value to ensure you got the number of
bits you expected."
  (read-bits bitio
             (or bits-per-byte (default-bits-per-byte bitio))
             :bit-endian (or bit-endian (default-bit-endian bitio))
             :eof-error-p eof-error-p
             :eof-value eof-value))

;; EXPORT
;; TODO: Check what happens when short read ends in a partially available byte,
;; what is the right action to do in that case?
(defun read-bytes (bitio seq &key bit-endian bits-per-byte (start 0) end)
  "This reads UNSIGNED 'bytes' into SEQ given :START and :END keywords.
The default span is the entire sequence. BIT-ENDIAN is how the
individual bits are read from the octet stream, and bits-per-byte is how
many bits wide a 'byte' is in the stream. Return how many elements
have been read. The sequence is destructively modified. At EOF
conditions, a short read will happen for the last element read (and
there is no notification of this) or the function will return 0.
NOTE: This function is similar to CL's READ-SEQUENCE except it only will read
the unsigned byte as defined in the function call arguments."
  (let* ((end (if (null end) (length seq) end))
         (bit-endian (or bit-endian (default-bit-endian bitio)))
         (bits-per-byte (or bits-per-byte (default-bits-per-byte bitio)))
         (total-octets-to-read (- end start))
         (required-octets total-octets-to-read)
         (num-octets-to-read (min total-octets-to-read
                                  (length (octet-read-buffer bitio))))
         (num-octets-read 0))

    (cond
      ((and (octet-read-boundary-p bitio) (= bits-per-byte 8))
       ;; Very fast path: reading exactly octets into an array
       (loop
         :while (> num-octets-to-read 0)
         :do
            (let ((octets-read (funcall (%bitio/read-sequence bitio)
                                        (octet-read-buffer bitio)
                                        (octet-stream bitio)
                                        :start 0 :end num-octets-to-read)))

              ;; Fix up endianess, if needed
              (when (eq bit-endian :le)
                (loop :for i :below octets-read
                      :do (setf (aref (octet-read-buffer bitio) i)
                                (octet-reverse (aref (octet-read-buffer bitio)
                                                     i)))))

              ;; copy into user's sequence
              (loop :for i :below octets-read
                    :do (setf (aref seq (+ start num-octets-read i))
                              (aref (octet-read-buffer bitio) i)))

              (incf num-octets-read octets-read)
              (decf required-octets octets-read)

              ;; short read, near eof
              (when (/= octets-read num-octets-to-read)
                (return-from read-bytes num-octets-read))

              (setf num-octets-to-read
                    (min required-octets
                         (length (octet-read-buffer bitio))))))

       num-octets-read)

      (t
       ;; This could be optimized in certain cases to use a sequence read.
       ;; But that work hasn't been done yet. So this path still can be slow.
       (loop :for num-read :from 0
             :for i :from start :below end
             :for the-byte = (read-bits bitio bits-per-byte
                                        :bit-endian bit-endian
                                        :eof-error-p NIL
                                        :eof-value :eof)
             :do (when (equal the-byte :eof)
                   (return-from read-bytes num-read))
                 (incf num-read)
                 (setf (aref seq i) the-byte))

       (length seq)))))


(defun sign-extend (potential-signed-value bit-width)
  (logior (* (ldb (byte 1 (1- bit-width)) potential-signed-value)
             (- (expt 2 bit-width)))
          potential-signed-value))

;; EXPORT
;; This thing interprets bits read by read-bits as a signed or unsigned
;; integer as appropriate.
(defun read-integer (bitio
                     &key
                       ;; next one is passed to bit-read.
                       bit-endian
                       ;; This is related to endianess of the integer.
                       byte-endian
                       ;; Bytes have this many bits in them.
                       bits-per-byte
                       ;; Default number of bytes to read
                       (num-bytes 4)
                       ;; T for unsigned, NIL for signed.
                       (unsignedp T))
  "This function reads 1 or more bytes where each byte is defined by
BITS-PER-BYTE and BIT-ENDIAN. BITS-PER-BYTE indicates how many bits are in
the byte and it defaults to 8. BIT-ENDIAN defines how to read those
bits from the octet stream. It defaults to :BE (big endian), See
READ-BITS for further explanation.  After the bytes are read, they
are arranged according to BYTE-ENDIAN in the traditional meaning of
multi-byte integers and that defaults to :LE.  Then depending
on UNSIGNEDP, defaulting to T, the value is either returned unsigned
or treated as a twos complement number and possibly turned negative by
sign extension. The integer is returned.  NOTE: The arguments don't
have to require that you read multiple of 8 bits to assemble the
number."

  (let ((value 0)
        (bit-endian (or bit-endian (default-bit-endian bitio)))
        (byte-endian (or byte-endian (default-byte-endian bitio)))
        (bits-per-byte (or bits-per-byte (default-bits-per-byte bitio))))
    (ecase byte-endian
      (:be (loop
             :for i :from (* (1- num-bytes) bits-per-byte) :downto 0 :by bits-per-byte
             :for byte = (read-bits bitio bits-per-byte :bit-endian bit-endian)
             :do (setf (ldb (byte bits-per-byte i) value) byte)))
      (:le (loop
             :for i :below (* bits-per-byte num-bytes) :by bits-per-byte
             :for byte = (read-bits bitio bits-per-byte :bit-endian bit-endian)
             :do (setf (ldb (byte bits-per-byte i) value) byte))))
    (if unsignedp
        value
        (sign-extend value (* num-bytes bits-per-byte)))))


;; EXPORT
(defun octet-read-boundary-p (bitio)
  "Return T if the reading of the bit stream is at an octet boundary.
NIL otherwise. If at EOF, return T, since techically, it is a boundary."
  (zerop (num-bits-in-stable bitio)))
