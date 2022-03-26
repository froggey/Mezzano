(in-package :bitio)

(defun dbgval (val fmt &rest args)
  ;; TODO: Fix to accept the number of bits that SHOULD be printed
  ;; out, and zero pad with additional format args as appropriate.
  ;; TODO, and fix the wonky FMT usage and ordering.
  (apply #'format t fmt args)
  (format t ": #x~X (#b~B)~%" val val)
  (finish-output))

(defun test-make-bitio/fast-io (fiobuf &key (octet-read-buffer-size 4096))
  (format t "  BITIO has ~A read-buffer size.~%" octet-read-buffer-size)
  (make-bitio fiobuf #'fast-io:fast-read-byte
              ;; canonicalize fast-io's fast-read-sequence to look
              ;; like clhs' read-sequence. They differ in lambda
              ;; lists.
              (lambda (vec buffer &key (start 0) (end nil))
                (funcall #'fast-io:fast-read-sequence vec buffer start end))
              :octet-read-buffer-size octet-read-buffer-size))

(defun test-make-bitio/clhs (fiobuf &key (octet-read-buffer-size 4096))
  (make-bitio fiobuf #'read-byte #'read-sequence
              :octet-read-buffer-size octet-read-buffer-size))

(defun make-octet-vector ()
  (make-array 10 :element-type '(unsigned-byte 8)
                 :initial-contents '(#x5c
                                     #xf6 #xee
                                     #x79 #x9a #xde
                                     #xff #xf2 #x88 #x02)))

(defun test-read-bits (bitio num-bits-to-read bit-endian expected-value
                       &optional
                         (expected-bits-to-have-been-read num-bits-to-read)
                         (eof-error-p T)
                         (eof-value NIL))
  (multiple-value-bind (value bit-read-count)
      (read-bits bitio num-bits-to-read
                 :bit-endian bit-endian
                 :eof-error-p eof-error-p
                 :eof-value eof-value)

    (format t "  -> Expecting[~D bits]: #x~X~%"
            num-bits-to-read expected-value)

    (format t "     Actually Read[~D bits, bit ~(~S~)]: #x~X~%"
            bit-read-count bit-endian value)

    (if (and (not eof-error-p) (equal value eof-value))
        ;; In this case, expected value is known to be the eof-value!
        (assert (eql expected-bits-to-have-been-read bit-read-count))
        ;; In this case, we either check a full value or a truncated one.
        (assert (and (equalp expected-value value)
                     (eql expected-bits-to-have-been-read bit-read-count))))))

(defun test-read-one-byte (bitio bits-per-byte bit-endian expected-value
                           &optional
                             (expected-bits-to-have-been-read bits-per-byte)
                             (eof-error-p T)
                             (eof-value NIL))
  (multiple-value-bind (value bit-read-count)
      (read-one-byte bitio
                     :bits-per-byte bits-per-byte
                     :bit-endian bit-endian
                     :eof-error-p eof-error-p
                     :eof-value eof-value)
    (dbgval value (format nil "~D bits ~(~S~) should be #x~X"
                          bits-per-byte bit-endian expected-value))

    (if (and (not eof-error-p) (equal value eof-value))
        ;; In this case, expected value is known to be the eof-value!
        (assert (eql expected-bits-to-have-been-read bit-read-count))
        ;; In this case, we either check a full value or a truncated one.
        (assert (and (equalp expected-value value)
                     (eql expected-bits-to-have-been-read bit-read-count))))))


(defun test-read-integer (bitio num-bytes bits-per-byte
                          bit-endian byte-endian unsignedp expected-value)
  (let ((value (read-integer bitio
                             :bit-endian bit-endian
                             :byte-endian byte-endian
                             :num-bytes num-bytes
                             :bits-per-byte bits-per-byte
                             :unsignedp unsignedp)))
    (dbgval value (format nil "read-integer (num-bytes: ~A, bits-per-byte: ~A, bit-endian: ~A, byte-endian: ~A, unsignedp: ~A): [#x~X] should be #x~X"
                          num-bytes bits-per-byte bit-endian byte-endian unsignedp
                          value expected-value))

    ;; Currently I ignore eof-error-p and eof-value, I need to think about
    ;; how to add that in.

    (assert (equalp expected-value value))))


;; Note "byte" doesn't necessarily mean 8 bit octets!
(defun test-read-bytes (bitio seq bit-endian bits-per-byte
                        expected-seq &key (start 0) end)

  (let ((num-parts-read (read-bytes bitio
                                    seq
                                    :bit-endian bit-endian
                                    :bits-per-byte bits-per-byte
                                    :start start
                                    :end end)))
    (let ((*print-right-margin* 9999))
      (format t "seq(bits-per-byte: ~A, bit-endian: ~A, seq ~X [start: ~A, end: ~A]) should be ~X~%"
              bits-per-byte bit-endian seq start end expected-seq))

    (assert (eql (length expected-seq) num-parts-read))
    ;; Check the sequence range we're supposed to have read is ok.
    (loop :for i :from start :below (if (null end) (length seq) end)
          :do (assert (eql (aref seq i) (aref expected-seq i))))))


(defmacro do-test (stream-kind octet-read-buffer-size octet-vector-sym
                   bitio-sym title-msg detail-msg &body body)
  (let ((fiobuf (gensym)))
    `(fast-io:with-fast-input (,fiobuf ,octet-vector-sym)
       (format t "Test: [~A] ~A~%" , stream-kind ,title-msg)
       (format t "  DESC: ~A~%" ,detail-msg)
       (let ((,bitio-sym
               (funcall (function ,(ecase stream-kind
                                     (:fast-io 'test-make-bitio/fast-io)
                                     (:clhs-io 'test-make-bitio/clhs)))
                        ,fiobuf
                        :octet-read-buffer-size ,octet-read-buffer-size)))
         ,@body
         ))))

(defun doit ()
  (let ((octet-vector (make-octet-vector)))
    (format t "Test Octet vector: ~X~%" octet-vector)

    ;; We test these functions:
    ;; make-bitio, read-bits, read-one-byte, read-integer, read-bytes,
    ;; octet-read-boundary-p

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all aligned 1 octet reads, octet buffer larger, no eof, bit :be"
      (test-read-bits bitio 8 :be #x5C)
      (test-read-bits bitio 8 :be #xF6)
      (test-read-bits bitio 8 :be #xEE)
      (test-read-bits bitio 8 :be #x79))

    (do-test :fast-io 1 octet-vector bitio "READ-BITS"
        "all aligned 1 octet reads, octet buffer size 1, no eof, bit :be"
      (test-read-bits bitio 8 :be #x5C)
      (test-read-bits bitio 8 :be #xF6)
      (test-read-bits bitio 8 :be #xEE)
      (test-read-bits bitio 8 :be #x79))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all aligned reads, octet buffer larger, no eof, bit :be"
      (test-read-bits bitio 8 :be #x5C)
      (test-read-bits bitio 16 :be #xF6EE)
      (test-read-bits bitio 24 :be #x799ADE)
      (test-read-bits bitio 32 :be #xFFF28802))

    (do-test :fast-io 3 octet-vector bitio "READ-BITS"
        "all aligned reads, octet buffer smaller, no eof, bit :be"
      (test-read-bits bitio 8 :be #x5C)
      (test-read-bits bitio 16 :be #xF6EE)
      (test-read-bits bitio 24 :be #x799ADE)
      (test-read-bits bitio 32 :be #xFFF28802))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all aligned reads, octet buffer larger, no eof, bit :le"
      (test-read-bits bitio 8 :le #x3A)
      (test-read-bits bitio 16 :le #x6F77)
      (test-read-bits bitio 24 :le #x9E597B)
      (test-read-bits bitio 32 :le #xFF4F1140))

    (do-test :fast-io 11 octet-vector bitio "READ-BITS"
        "all aligned reads, octet buffer larger size, no eof, bit :be"
      (test-read-bits bitio 80 :be #x5CF6EE799ADEFFF28802))

    (do-test :fast-io 10 octet-vector bitio "READ-BITS"
        "all aligned reads, octet buffer same size, no eof, bit :be"
      (test-read-bits bitio 80 :be #x5CF6EE799ADEFFF28802))

    (do-test :fast-io 9 octet-vector bitio "READ-BITS"
        "all aligned reads, octet buffer smaller size, no eof, bit :be"
      (test-read-bits bitio 80 :be #x5CF6EE799ADEFFF28802))

    (do-test :fast-io 1 octet-vector bitio "READ-BITS"
        "all aligned reads, octet buffer size of 1, no eof, bit :be"
      (test-read-bits bitio 80 :be #x5CF6EE799ADEFFF28802))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all unaligned reads, octet buffer larger, no eof, bit :be"
      (test-read-bits bitio 1 :be #b0)
      (test-read-bits bitio 1 :be #b1)
      (test-read-bits bitio 1 :be #b0)
      (test-read-bits bitio 1 :be #b1)
      (test-read-bits bitio 1 :be #b1)
      (test-read-bits bitio 1 :be #b1)
      (test-read-bits bitio 1 :be #b0)
      (test-read-bits bitio 1 :be #b0))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all unaligned reads, octet buffer larger, no eof, bit :le"
      ;; Consume 1 octet worth of information
      (test-read-bits bitio 1 :le #b0)
      (test-read-bits bitio 1 :le #b0)
      (test-read-bits bitio 1 :le #b1)
      (test-read-bits bitio 1 :le #b1)
      (test-read-bits bitio 1 :le #b1)
      (test-read-bits bitio 1 :le #b0)
      (test-read-bits bitio 1 :le #b1)
      (test-read-bits bitio 1 :le #b0))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all unaligned reads, octet buffer larger, no eof, bit :le/:be"
      ;; Consume 1 octet :le bits first, then :be of the remaining bits.
      (test-read-bits bitio 1 :le #b0)
      (test-read-bits bitio 1 :le #b0)
      (test-read-bits bitio 1 :le #b1)
      (test-read-bits bitio 1 :le #b1)
      (test-read-bits bitio 1 :be #b0)
      (test-read-bits bitio 1 :be #b1)
      (test-read-bits bitio 1 :be #b0)
      (test-read-bits bitio 1 :be #b1))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all unaligned reads, octet buffer larger, no eof, bit :be"
      ;; Consume 1 octet
      (test-read-bits bitio 4 :be #b0101)
      (test-read-bits bitio 4 :be #b1100)

      ;; Consume 1 octet
      (test-read-bits bitio 3 :be #b111)
      (test-read-bits bitio 5 :be #b10110)

      ;; Consume 1 octet
      (test-read-bits bitio 2 :be #b11)
      (test-read-bits bitio 6 :be #b101110)

      ;; Consume 1 octet
      (test-read-bits bitio 1 :be #b0)
      (test-read-bits bitio 7 :be #b1111001)

      ;; Consume 1 octet
      (test-read-bits bitio 2 :be #b10)
      (test-read-bits bitio 2 :be #b01)
      (test-read-bits bitio 2 :be #b10)
      (test-read-bits bitio 2 :be #b10)

      ;; Consume 3 octets hopping boundaries
      (test-read-bits bitio 4 :be #xd)
      (test-read-bits bitio 8 :be #xef)
      (test-read-bits bitio 8 :be #xff)
      (test-read-bits bitio 4 :be #x2)

      ;; Consume 2 octets hopping boundaries
      (test-read-bits bitio 7 :be #b1000100)
      (test-read-bits bitio 2 :be #b00)
      (test-read-bits bitio 7 :be #b0000010))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "mixed w/inferred aligned read, octet buffer larger, no eof, bit :be"
      ;; Consume .5 octet
      (test-read-bits bitio 4 :be #x5)

      ;; Consume .5 octet, 8 octets, .5 octet
      ;; This infers a fast read of 8 octets in the middle of the 72 bits.
      (test-read-bits bitio 72 :be #xcf6ee799adefff2880)

      ;; Consume .5 octet
      (test-read-bits bitio 4 :be #x2))

    (do-test :fast-io 3 octet-vector bitio "READ-BITS"
        "mixed w/inferred aligned read, octet buffer size 3, no eof, bit :be"
      ;; Consume .5 octet
      (test-read-bits bitio 4 :be #x5)

      ;; Consume .5 octet, 8 octets, .5 octet
      ;; This infers a fast read of 8 octets in the middle of the 72 bits.
      (test-read-bits bitio 72 :be #xcf6ee799adefff2880)

      ;; Consume .5 octet
      (test-read-bits bitio 4 :be #x2))

    (do-test :fast-io 4096 octet-vector bitio "READ-BITS"
        "all aligned read, octet buffer size larger, eof, bit :be"
      ;; Consume 10 legal octets, and then one more (80 bits plus 8
      ;; bits) resulting in EOF.  Expected value here is the
      ;; expected truncated return.
      (test-read-bits bitio 88 :be #x5cf6ee799adefff28802 80 NIL :eof)
      ;; Here we blatently read an :eof again, so we expect to see an :eof
      (test-read-bits bitio 80 :be :eof 0 NIL :eof))

    (do-test :fast-io 7 octet-vector bitio "READ-BITS"
        "all aligned read, octet buffer size smaller, eof, bit :be"
      ;; Consume 10 legal octets, and then one more (80 bits plus 8
      ;; bits) resulting in EOF.  Expected value here is the
      ;; expected truncated return.
      (test-read-bits bitio 88 :be #x5cf6ee799adefff28802 80 NIL :eof)
      ;; Here we blatently read an :eof again, so we expect to see an :eof
      (test-read-bits bitio 80 :be :eof 0 NIL :eof))

    (do-test :fast-io 1 octet-vector bitio "READ-BITS"
        "all aligned read, octet buffer size 1, eof, bit :be"
      ;; Consume 10 legal octets, and then one more (80 bits plus 8
      ;; bits) resulting in EOF.  Expected value here is the
      ;; expected truncated return.
      (test-read-bits bitio 88 :be #x5cf6ee799adefff28802 80 NIL :eof)
      ;; Here we blatently read an :eof again, so we expect to see an :eof
      (test-read-bits bitio 80 :be :eof 0 NIL :eof))

    (format t "Case: read-bits, slow read path with eof, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        ;; Consume .5 octet
        (test-read-bits bitio 4 :be #x5)

        ;; Consume .5 octet, 9 legal octets, then 1 more octet which is EOF
        ;; The value is the expected truncated read.
        (test-read-bits bitio 84 :be #xcf6ee799adefff28802 76 NIL :eof)

        ;; Consume .5 octets, but get :eof
        (test-read-bits bitio 4 :be :eof 0 NIL :eof)

        ;; Consume 1 octet, but get :eof
        (test-read-bits bitio 8 :be :eof 0 NIL :eof)

        ;; Consume 1.5 octet, but get :eof
        (test-read-bits bitio 12 :be :eof 0 NIL :eof)

        ))

    (format t "Case: read-bits, slow read path with eof2, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        ;; Consume .5 octet, 9 legal octets. This leaves 1 for the stable.
        (test-read-bits bitio 76 :be #x5cf6ee799adefff2880)

        ;; Consume 1 octet, but get an expected short read of 4 bits
        (test-read-bits bitio 8 :be #x2 4 NIL :eof)

        ;; Consume .5 octet, but get eof
        (test-read-bits bitio 4 :be :eof 0 NIL :eof)

        ))

    (format t "Case: read-one-byte, bit-endian: :be, 8 bits wide~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        ;; Consume 1 octet as 1 byte.
        (test-read-one-byte bitio 8 :be #x5c)

        ))

    (format t "Case: read-one-byte, bit-endian: :be, 12 bits wide~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        ;; Consume 1.5 octets, as 1 byte
        (test-read-one-byte bitio 12 :be #x5cf)
        ;; Consume 1.5 octets, as 1 byte
        (test-read-one-byte bitio 12 :be #x6ee)

        ))

    ;; This one may look non-intuitive...
    (format t "Case: read-one-byte, bit-endian: :le, 12 bits wide~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        ;; Consume 1.5 octets, as 1 byte
        (test-read-one-byte bitio 12 :le #x3a6)
        ;; Consume 1.5 octets, as 1 byte
        (test-read-one-byte bitio 12 :le #xf77)

        ))

    (format t "Case: read-bytes, bit-width 4, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) .5))
                               :element-type '(unsigned-byte 4)
                               :initial-element 0)))

          (test-read-bytes bitio seq :be 4
                           #(#x5 #xc #xf #x6 #xe #xe #x7 #x9 #x9 #xa
                             #xd #xe #xf #xf #xf #x2 #x8 #x8 #x0 #x2)))
        ))

    (format t "Case: read-bytes, bit-width 4, bit-endian :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) .5))
                               :element-type '(unsigned-byte 4)
                               :initial-element 0)))

          (test-read-bytes bitio seq :le 4
                           (map 'vector
                                (lambda (x) (integer-reverse x 4))
                                #(#xc #x5 #x6 #xf #xe #xe #x9 #x7
                                  #xa #x9 #xe #xd #xf #xf #x2 #xf
                                  #x8 #x8 #x2 #x0))))
        ))

    (format t "Case: read-bytes, bit-width 8, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1))
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))

          (test-read-bytes bitio seq :be 8
                           #(#x5c #xf6 #xee #x79 #x9a
                             #xde #xff #xf2 #x88 #x02)))
        ))

    (format t "Case: read-bytes, bit-width 8, bit-endian :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1))
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))

          (test-read-bytes bitio seq :le 8
                           (map 'vector
                                (lambda (x) (integer-reverse x 8))
                                #(#x5c #xf6 #xee #x79 #x9a
                                  #xde #xff #xf2 #x88 #x02))))
        ))

    (format t "Case: read-bytes, bit-width 12, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1.5))
                               :element-type '(unsigned-byte 12)
                               :initial-element 0)))

          (test-read-bytes bitio seq :be 12
                           #(#x5cf #x6ee #x799 #xade #xfff #x288 #x02)))
        ))

    (format t "Case: read-bytes, bit-width 16, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 2))
                               :element-type '(unsigned-byte 16)
                               :initial-element 0)))

          (test-read-bytes bitio seq :be 16
                           #(#x5cf6 #xee79 #x9ade #xfff2 #x8802)))
        ))

    (format t "Case: read-integer, case 1~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 1 8 :be :le T #x5c)
        (test-read-integer bitio 2 8 :be :le T #xeef6)
        (test-read-integer bitio 3 8 :be :le T #xde9a79)
        (test-read-integer bitio 4 8 :be :le T #x0288f2ff)
        ))

    (format t "Case: read-integer, case 2~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 1 8 :be :be T #x5c)
        (test-read-integer bitio 2 8 :be :be T #xf6ee)
        (test-read-integer bitio 3 8 :be :be T #x799ade)
        (test-read-integer bitio 4 8 :be :be T #xfff28802)
        ))

    (format t "Case: read-integer, case 3~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 1 4 :be :be T #x5)
        (test-read-integer bitio 1 4 :le :be T #x3)
        (test-read-integer bitio 1 8 :le :be T #x6f)
        (test-read-integer bitio 1 8 :be :be T #xee)
        (test-read-integer bitio 1 8 :le :be NIL
                           (sign-extend (integer-reverse #x79 8) 8))
        (test-read-integer bitio 1 8 :be :be NIL
                           (sign-extend #x9a 8))

        (test-read-integer bitio 1 4 :be :be T #xd)
        ;; intentional misaligned octet read...
        (test-read-integer bitio 4 8 :be :be T #xefff2880)
        (test-read-integer bitio 1 4 :be :be T #x2)

        ))

    (format t "Case: read-integer, case 4~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 4 8 :be :be T #x5cf6ee79)
        (test-read-integer bitio 4 8 :be :le T #xf2ffde9a)
        ))

    (format t "Case: read-integer, case 5~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 4 12 :be :be T #x5cf6ee799ade)
        ))

    (format t "Case: read-integer, case 6~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 4 12 :le :be T #x3a6f779e597b)
        (test-read-integer bitio 4 8 :be :be T #xfff28802)
        ))

    (format t "Case: read-integer, case 7~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 4 16 :be :be T #x5cf6ee799adefff2)
        ))

    (format t "Case: read-integer, case 8~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (test-make-bitio/fast-io fiobuf)))
        (test-read-integer bitio 4 16 :be :le T #xfff29adeee795cf6)
        ))

    ;; Test a non fast-io stream
    (format t "Case: wrapping regular CL octet stream~%")
    (with-open-file (fin (asdf:system-relative-pathname :bitio "binfile")
                         :direction :input
                         :element-type '(unsigned-byte 8)
                         :if-does-not-exist :error)
      ;; wrap fin stream with a bitio stream.
      (let ((bitio (test-make-bitio/clhs fin)))
        (test-read-bits bitio 88 :be #x000102030405060708090a)
        ))

    ;; Test a fast-io stream backed by a file.
    (format t "Case: wrapping fast-io octet stream from a file~%")
    (with-open-file (fin (asdf:system-relative-pathname :bitio "binfile")
                         :direction :input
                         :element-type '(unsigned-byte 8)
                         :if-does-not-exist :error)
      (fast-io:with-fast-input (fin-fast
                                (make-array 0 :element-type '(unsigned-byte 8))
                                fin)
        ;; wrap fin stream with a bitio stream.
        (let ((bitio (test-make-bitio/fast-io fin-fast)))
          (test-read-bits bitio 88 :be #x000102030405060708090a)
          )))

    (format t "All done.~%")
    ))
