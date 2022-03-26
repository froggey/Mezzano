(in-package :chipz)

;;; bzip2's decompress.c looks relatively simple, but a great deal of
;;; complexity and cleverness is hidden behind C preprpocessor macro.
;;; The single biggest help in understand what is going on behind the
;;; macros is to read "Coroutines in C" by Simon Tatham:
;;;
;;;  http://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
;;;
;;; decompress.c is using the same technique described in the paper,
;;; although with a slightly different implementation.
;;;
;;; Lisp, fortunately/alas, does not admit the same sort of techniques
;;; that C does--at least not expressed exactly the same way.  So our
;;; translation naturally differs in some places.  For example, to make
;;; it easier to figure out how much state we have to preserve, we
;;; choose to read more in at one time than decompress.c--the magic
;;; number header all at once or the bits for the mapping table in
;;; larger chunks than 1 bit at a time, for instance.

;;; Reading things in larger chunks than bits means that we have to do
;;; bit-reversal of various quantities.

(defun reverse-ub4 (x)
  (let ((table (load-time-value (make-array 16 :element-type 'fixnum
                                            :initial-contents '(0 8 4 12
                                                                2 10 6 14
                                                                1 9 5 13
                                                                3 11 7 15)))))
    (aref table x)))

(defun reverse-ub8 (x)
  (logior (ash (reverse-ub4 (ldb (byte 4 0) x)) 4)
          (reverse-ub4 (ldb (byte 4 4) x))))

(defun reverse-ub16 (x)
  (logior (ash (reverse-ub8 (ldb (byte 8 0) x)) 8)
          (reverse-ub8 (ldb (byte 8 8) x))))
(defmacro transition-to (next-state)
  `(progn
     (setf (bzip2-state-state state) #',next-state)
     (,next-state state)))

(defvar *dummy-vec* (make-array #.+bz-max-alpha-size+ :element-type '(unsigned-byte 32)))

(defstruct (bzip2-state
             (:include decompression-state)
             (:constructor %make-bzip2-state))
  ;; For doing the final run-length decoding.
  (out-ch 0 :type (unsigned-byte 8))
  (out-len 0 :type (integer 0 260))
  (block-randomized-p nil)
  (rntogo 0 :type (unsigned-byte 32))
  (rntpos 0 :type (unsigned-byte 32))

  (100k-block-size 1 :type (integer 1 9))
  (small-decompression-p nil)
  (current-block-number 0)

  ;; For undoing the Burrows-Wheeler transform.  */
  (original-pointer 0)
  (t-position 0 :type (integer 0 (900000)))
  (k0 0)
  (unzftab (make-array 256 :element-type '(unsigned-byte 32))
           :type (simple-array (unsigned-byte 32) (256)))
  (n-blocks-used 0)
  (cftab (make-array 257 :element-type '(unsigned-byte 32))
         :type (simple-array (unsigned-byte 32) (257)))
  (cftab-copy (make-array 257 :element-type '(unsigned-byte 32))
              :type (simple-array (unsigned-byte 32) (257)))

  ;; For undoing the Burrows-Wheeler transform (FAST).
  (tt (make-array 0 :element-type '(unsigned-byte 32))
      :type (simple-array (unsigned-byte 32) (*)))

  ;; Stored and calculated CRCs.
  (stored-block-crc 0 :type (unsigned-byte 32))
  (stored-combined-crc 0 :type (unsigned-byte 32))
  (calculated-block-crc #xffffffff :type (unsigned-byte 32))
  (calculated-combined-crc 0 :type (unsigned-byte 32))

  ;; Map of bytes used in block ("mapping table").
  (n-in-use 0 :type (integer 0 256))
  (in-use (make-array 256 :initial-element nil)
          :type (simple-array t (256)))
  ;; This was a byte array; we have chosen to make it a simple integer
  ;; and index it with LOGBITP.
  (in-use-16 0 :type (unsigned-byte 16))
  (seq-to-unseq (make-array 256 :element-type '(unsigned-byte 8))
                :type (simple-array (unsigned-byte 8) (256)))

  ;; For decoding the MTF values.
  (mtfa (make-array +mtfa-size+ :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (#.+mtfa-size+)))
  (mtfbase (make-array (/ 256 +mtfl-size+) :element-type '(unsigned-byte 16))
           :type (simple-array (unsigned-byte 16) (#.(/ 256 +mtfl-size+))))
  (selector (make-array +bz-max-selectors+ :element-type '(unsigned-byte 8))
            :type (simple-array (unsigned-byte 8) (#.+bz-max-selectors+)))
  (selector-mtf (make-array +bz-max-selectors+ :element-type '(unsigned-byte 8))
                :type (simple-array (unsigned-byte 8) (#.+bz-max-selectors+)))
  (len (make-array '(#.+bz-n-groups+ #.+bz-max-alpha-size+)
                   :element-type '(unsigned-byte 8))
       :type (simple-array (unsigned-byte 8) (#.+bz-n-groups+ #.+bz-max-alpha-size+)))
  (mtf-continuation nil :type (or null function))

  (limit #1=(let ((w (make-array +bz-n-groups+)))
           (dotimes (i +bz-n-groups+ w)
             (setf (aref w i) (make-array +bz-max-alpha-size+
                                          :element-type '(unsigned-byte 32)))))
         :type (simple-array t (#.+bz-n-groups+)))
  (base #1#
        :type (simple-array t (#.+bz-n-groups+)))
  (perm #1#
        :type (simple-array t (#.+bz-n-groups+)))
  (min-lengths (make-array #.+bz-n-groups+ :element-type '(unsigned-byte 32))
               :type (simple-array (unsigned-byte 32) (#.+bz-n-groups+)))

  ;; Save variables for scalars in the decompression code.
  (i 0)
  (j 0)
  (alpha-size 0 :type (integer 0 258))
  (n-groups 0)
  (n-selectors 0)
  (EOB 0 :type (integer 0 257))
  ;; FIXME: check on the declarations for these three.
  (group-number 0 :type fixnum)
  (group-position 0 :type fixnum)
  (lval 0 :type fixnum)
  (nblockMAX 0 :type (integer 0 900000))
  (nblock 0 :type (integer 0 (900000)))
  (es 0 :type fixnum)
  (N 0 :type fixnum)
  (curr 0 :type (integer 0 20))
  (zn 0 :type (integer 0 20))
  (zvec 0 :type (integer 0 #.(expt 2 20)))
  (g-minlen 0 :type (integer 0 23))
  (g-limit *dummy-vec*
           :type (simple-array (unsigned-byte 32) (#.+bz-max-alpha-size+)))
  (g-base *dummy-vec*
          :type (simple-array (unsigned-byte 32) (#.+bz-max-alpha-size+)))
  (g-perm *dummy-vec*
          :type (simple-array (unsigned-byte 32) (#.+bz-max-alpha-size+))))

(defmethod print-object ((object bzip2-state) stream)
  (print-unreadable-object (object stream)
    (format stream "Bzip2 state bits: ~X/~D input: ~D/~D output ~D/~D"
            (bzip2-state-bits object)
            (bzip2-state-n-bits object)
            (bzip2-state-input-index object)
            (bzip2-state-input-end object)
            (bzip2-state-output-index object)
            (bzip2-state-output-end object))))

(defun make-maps (state)
  (declare (type bzip2-state state))
  (loop with n-in-use = 0
     with in-use-table = (bzip2-state-in-use state)
     with seq-to-unseq = (bzip2-state-seq-to-unseq state)
     for i from 0 below 256
     when (aref in-use-table i)
     do (setf (aref seq-to-unseq n-in-use) i
              n-in-use (1+ n-in-use))
     finally
       (return (setf (bzip2-state-n-in-use state) n-in-use))))

(defun make-decode-tables (state group min-len max-len alpha-size)
  (declare (type bzip2-state state))
  (let* ((limit (aref (bzip2-state-limit state) group))
         (base (aref (bzip2-state-base state) group))
         (perm (aref (bzip2-state-perm state) group))
         (len (bzip2-state-len state))
         (rmi (array-row-major-index len group 0)))
    (loop with pp = 0
       for i from min-len to max-len
       do (dotimes (j alpha-size)
            (when (= (row-major-aref len (+ rmi j)) i)
              (setf (aref perm pp) j)
              (incf pp))))
    (loop for i from 0 below +bz-max-code-len+
       do (setf (aref base i) 0
                (aref limit i) 0))
    (loop for i from 0 below alpha-size
       do (incf (aref base (1+ (row-major-aref len (+ i rmi))))))
    (loop for i from 1 below +bz-max-code-len+
       do (incf (aref base i)
                (aref base (1- i))))
    (loop with vec = 0
       for i from min-len to max-len
       do (incf vec (- (aref base (1+ i))
                       (aref base i)))
         (setf (aref limit i) (1- vec)
               vec (ash vec 1)))
    (loop for i from (+ min-len 1) to max-len
       do (setf (aref base i)
                (- (ash (1+ (aref limit (1- i))) 1)
                   (aref base i))))))

(defun undo-rle-obuf-to-output (state)
  (declare (optimize speed))
  (cond
    ((bzip2-state-block-randomized-p state)
     (error 'bzip2-randomized-blocks-unimplemented))
    (t
     (let ((calculated-block-crc (bzip2-state-calculated-block-crc state))
           (out-ch (bzip2-state-out-ch state))
           (out-len (bzip2-state-out-len state))
           (n-blocks-used (bzip2-state-n-blocks-used state))
           (k0 (bzip2-state-k0 state))
           (k1 0)
           (tt (bzip2-state-tt state))
           (t-position (bzip2-state-t-position state))
           (nblockpp (1+ (bzip2-state-nblock state)))
           (output (bzip2-state-output state))
           (index (bzip2-state-output-index state))
           (end (bzip2-state-output-end state)))
       (declare (type (unsigned-byte 32) calculated-block-crc))
       (declare (type (integer 0 260) out-len))
       (declare (type (unsigned-byte 8) k0 k1))
       (declare (type (integer 0 900000) n-blocks-used nblockpp))
       (declare (type (unsigned-byte 32) t-position))
       (macrolet ((get-fast ()
                    `(prog2
                         (setf t-position (aref tt t-position))
                         (logand t-position #xff)
                       (setf t-position (ash t-position -8)))))
         (tagbody
          START
            ;; "try to finish existing run"
            (when (zerop out-len)
              (go GRAB-MORE))
            (loop
               (when (= index end)
                 (go FINISH))
               (when (= out-len 1)
                 (go LEN-EQUAL-ONE))
               (setf (aref output index) out-ch)
               (setf calculated-block-crc
                     (logand #xffffffff
                             (logxor (ash calculated-block-crc 8)
                                     (aref +bzip2-crc32-table+
                                           (logxor (ash calculated-block-crc -24) out-ch)))))
               (decf out-len)
               (incf index))
          LEN-EQUAL-ONE
            (when (= index end)
              (setf out-len 1)
              (go FINISH))
            (setf (aref output index) out-ch)
            (setf calculated-block-crc
                  (logand #xffffffff
                          (logxor (ash calculated-block-crc 8)
                                  (aref +bzip2-crc32-table+
                                        (logxor (ash calculated-block-crc -24) out-ch)))))
            (incf index)
          GRAB-MORE
            ;; "Only caused by corrupt data stream?"
            (when (> n-blocks-used nblockpp)
              (return-from undo-rle-obuf-to-output t))
            (when (= n-blocks-used nblockpp)
              (setf out-len 0)
              (go FINISH))
            (setf out-ch k0)
               
            (setf k1 (get-fast))
            (incf n-blocks-used)
            (unless (= k1 k0)
              (setf k0 k1)
              (go LEN-EQUAL-ONE))
            (when (= n-blocks-used nblockpp)
              (go LEN-EQUAL-ONE))

            (setf out-len 2)
            (setf k1 (get-fast))
            (incf n-blocks-used)
            (when (= n-blocks-used nblockpp)
              (go CONTINUE))
            (unless (= k1 k0)
              (setf k0 k1)
              (go CONTINUE))

            (setf out-len 3)
            (setf k1 (get-fast))
            (incf n-blocks-used)
            (when (= n-blocks-used nblockpp)
              (go CONTINUE))
            (unless (= k1 k0)
              (setf k0 k1)
              (go CONTINUE))

            (setf k1 (get-fast))
            (incf n-blocks-used)
            (setf out-len (+ k1 4))
            (setf k0 (get-fast))
            (incf n-blocks-used)
          CONTINUE
            (go START)
          FINISH)
       
            #+nil
            (incf (bzip2-state-total-out state)
                  (- index (bzip2-state-output-index state) ))
            ;; Restore cached values.
            (setf (bzip2-state-calculated-block-crc state) calculated-block-crc
                  (bzip2-state-out-ch state) out-ch
                  (bzip2-state-out-len state) out-len
                  (bzip2-state-n-blocks-used state) n-blocks-used
                  (bzip2-state-k0 state) k0
                  (bzip2-state-t-position state) t-position
                  (bzip2-state-output-index state) index)
          nil)))))

;;; decompress.c has various logic relating to whether the user has
;;; chosen "small" decompression, which uses less memory.  We're just
;;; going to be memory-intensive and always pick the large option.  Maybe
;;; someday we can come back and add the small option.

(defun %bzip2-state-machine (state)
  (declare (type bzip2-state state))
  (declare (optimize speed))
  (labels (
           (read-bits (n state)
             (declare (type (integer 0 32) n))
             (declare (type bzip2-state state))
             (prog1
               ;; We don't use (BYTE N (- ...)) here because doing it
               ;; this way is ~10% faster on SBCL.
               (ldb (byte n 0)
                    (ash (bzip2-state-bits state)
                         (the (integer -31 0)
                           (- n (bzip2-state-n-bits state)))))
               (decf (bzip2-state-n-bits state) n)))

           (ensure-bits (n state)
             (declare (type (integer 0 32) n))
             (declare (type bzip2-state state))
             (let ((bits (bzip2-state-bits state))
                   (n-bits (bzip2-state-n-bits state))
                   (input-index (bzip2-state-input-index state)))
               (declare (type (unsigned-byte 32) bits))
               (loop while (< n-bits n)
                  when (>= input-index (bzip2-state-input-end state))
                  do (progn
                       (setf (bzip2-state-bits state) bits
                             (bzip2-state-n-bits state) n-bits
                             (bzip2-state-input-index state) input-index)
                       (throw 'bzip2-done nil))
                  do (let ((byte (aref (bzip2-state-input state) input-index)))
                       (declare (type (unsigned-byte 8) byte))
                       (setf bits
                             (logand #xffffffff (logior (ash bits 8) byte)))
                       (incf n-bits 8)
                       (incf input-index))
                  finally (setf (bzip2-state-bits state) bits
                                (bzip2-state-n-bits state) n-bits
                                (bzip2-state-input-index state) input-index))))

           (ensure-and-read-bits (n state)
             (ensure-bits n state)
             (read-bits n state))

           (bzip2-header (state)
             (declare (type bzip2-state state))
             (let ((header-field (ensure-and-read-bits 32 state)))
               (declare (type (unsigned-byte 32) header-field))
               (unless (and (= (ldb (byte 8 24) header-field) +bz-header-b+)
                            (= (ldb (byte 8 16) header-field) +bz-header-z+)
                            (= (ldb (byte 8 8) header-field) +bz-header-h+))
                 (error 'invalid-bzip2-magic))
               (let ((block-size-magic-byte (ldb (byte 8 0) header-field)))
                 (unless (<= (+ +bz-header-0+ 1)
                             block-size-magic-byte
                             (+ +bz-header-0+ 9))
                   (error 'invalid-bzip2-magic))
                 (setf (bzip2-state-100k-block-size state) (- block-size-magic-byte
                                                              +bz-header-0+))
                 ;; BZIP2 SMALL
                 (setf (bzip2-state-tt state)
                       (make-array (* (bzip2-state-100k-block-size state) +100k+)
                                   :element-type '(unsigned-byte 32)))
                 (transition-to bzip2-block-header1))))

           (bzip2-block-header1 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (case byte
                 (#x17 (transition-to bzip2-end-header2))
                 (#x31 (transition-to bzip2-block-header2))
                 (t (error 'invalid-bzip2-data)))))

           (bzip2-block-header2 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x41)
                   (transition-to bzip2-block-header3)
                   (error 'invalid-bzip2-data))))

           (bzip2-block-header3 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x59)
                   (transition-to bzip2-block-header4)
                   (error 'invalid-bzip2-data))))

           (bzip2-block-header4 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x26)
                   (transition-to bzip2-block-header5)
                   (error 'invalid-bzip2-data))))

           (bzip2-block-header5 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x53)
                   (transition-to bzip2-block-header6)
                   (error 'invalid-bzip2-data))))

           (bzip2-block-header6 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (unless (= byte #x59)
                 (error 'invalid-bzip2-data))
               (incf (bzip2-state-current-block-number state))
               (transition-to bzip2-block-crc32)))

           (bzip2-block-crc32 (state)
             (declare (type bzip2-state state))
             (let ((crc32 (ensure-and-read-bits 32 state)))
               (setf (bzip2-state-stored-block-crc state) crc32)
               (transition-to bzip2-block-randombit)))

           (bzip2-block-randombit (state)
             (declare (type bzip2-state state))
             (let ((randomized-p (ensure-and-read-bits 1 state)))
               (setf (bzip2-state-block-randomized-p state) (= randomized-p 1))
               (transition-to bzip2-original-pointer)))

           (bzip2-original-pointer (state)
             (declare (type bzip2-state state))
             (let ((original-pointer (ensure-and-read-bits 24 state)))
               (unless (<= 0 original-pointer
                           (+ 10 (* (bzip2-state-100k-block-size state) +100k+)))
                 (error 'invalid-bzip2-data))
               (setf (bzip2-state-original-pointer state) original-pointer)
               (transition-to bzip2-mapping-table1)))

           (bzip2-mapping-table1 (state)
             (declare (type bzip2-state state))
             (let ((in-use-16 (reverse-ub16 (ensure-and-read-bits 16 state))))
               (setf (bzip2-state-in-use-16 state) in-use-16)
               (setf (bzip2-state-i state) 0)
               (fill (bzip2-state-in-use state) nil)
               (transition-to bzip2-mapping-table2)))

           (bzip2-mapping-table2 (state)
             (declare (type bzip2-state state))
             (loop with in-use-16 = (bzip2-state-in-use-16 state)
                with in-use-table = (bzip2-state-in-use state)
                while (< (bzip2-state-i state) 16)
                when (logbitp (bzip2-state-i state) in-use-16)
                do (let ((in-use (reverse-ub16 (ensure-and-read-bits 16 state))))
                     (dotimes (i 16)
                       (setf (aref in-use-table (+ (* (bzip2-state-i state) 16)
                                                   i))
                             (logbitp i in-use))))
                do
                  (incf (bzip2-state-i state)))
             (let ((n-in-use (make-maps state)))
               (when (zerop n-in-use)
                 (error 'invalid-bzip2-data))
               (setf (bzip2-state-alpha-size state)
                     (+ n-in-use 2))
               (transition-to bzip2-selector1)))

           (bzip2-selector1 (state)
             (declare (type bzip2-state state))
             (let ((n-groups (ensure-and-read-bits 3 state)))
               (unless (<= 3 n-groups 6)
                 (error 'invalid-bzip2-data))
               (setf (bzip2-state-n-groups state) n-groups)
               (transition-to bzip2-selector2)))

           (bzip2-selector2 (state)
             (declare (type bzip2-state state))
             (let ((n-selectors (ensure-and-read-bits 15 state)))
               (unless (plusp n-selectors)
                 (error 'invalid-bzip2-data))
               (setf (bzip2-state-n-selectors state) n-selectors)
               (setf (bzip2-state-i state) 0)
               (transition-to bzip2-selector3a)))

           (bzip2-selector3a (state)
             (declare (type bzip2-state state))
             (setf (bzip2-state-j state) 0)
             (transition-to bzip2-selector3b))

           (bzip2-selector3b (state)
             (declare (type bzip2-state state))
             (loop
                do (let ((bit (ensure-and-read-bits 1 state)))
                     (when (zerop bit) (loop-finish))
                     (when (>= (incf (bzip2-state-j state))
                               (bzip2-state-n-groups state))
                       (error 'invalid-bzip2-data)))
                finally 
                  (setf (aref (bzip2-state-selector-mtf state)
                              (bzip2-state-i state))
                        (bzip2-state-j state)))
             (if (< (incf (bzip2-state-i state))
                    (bzip2-state-n-selectors state))
                 (transition-to bzip2-selector3a)
                 (transition-to bzip2-selector-undo-mtf-values)))

           (bzip2-selector-undo-mtf-values (state)
             (declare (type bzip2-state state))
             (let ((pos (make-array +bz-n-groups+ 
                                    :element-type '(unsigned-byte 8)))
                   (n-groups (bzip2-state-n-groups state))
                   (n-selectors (bzip2-state-n-selectors state))
                   (selector-table (bzip2-state-selector state))
                   (selector-mtf (bzip2-state-selector-mtf state)))
               (declare (dynamic-extent pos))
               (dotimes (i n-groups)
                 (setf (aref pos i) i))
               (dotimes (i n-selectors)
                 (let* ((v (aref selector-mtf i))
                        (tmp (aref pos v)))
                   (loop until (zerop v)
                      do (setf (aref pos v) (aref pos (1- v)))
                      (decf v))
                   (setf (aref pos 0) tmp)
                   (setf (aref selector-table i) tmp)))
               (setf (bzip2-state-j state) 0)
               (transition-to bzip2-coding-tables-groups-loop)))

           (bzip2-coding-tables-groups-loop (state)
             (declare (type bzip2-state state))
             (cond
               ((< (bzip2-state-j state) (bzip2-state-n-groups state))
                (setf (bzip2-state-curr state) (ensure-and-read-bits 5 state)
                      (bzip2-state-i state) 0)
                (transition-to bzip2-coding-tables-alpha-loop))
               (t
                (transition-to bzip2-create-huffman-decode-tables))))

           (bzip2-coding-tables-alpha-loop (state)
             (declare (type bzip2-state state))
             (unless (<= 1 (bzip2-state-curr state) 20)
               (error 'invalid-bzip2-data))
             (let ((uc (ensure-and-read-bits 1 state)))
               (cond
                 ((zerop uc)
                  (setf (aref (bzip2-state-len state) (bzip2-state-j state) (bzip2-state-i state))
                        (bzip2-state-curr state))
                  (cond
                    ((< (incf (bzip2-state-i state))
                        (bzip2-state-alpha-size state))
                     (bzip2-coding-tables-alpha-loop state))
                    (t
                     (incf (bzip2-state-j state))
                     (transition-to bzip2-coding-tables-groups-loop))))
                 (t
                  (transition-to bzip2-coding-tables-alpha-loop2)))))

           (bzip2-coding-tables-alpha-loop2 (state)
             (declare (type bzip2-state state))
             (let ((uc (ensure-and-read-bits 1 state)))
               (if (zerop uc)
                   (incf (bzip2-state-curr state))
                   (decf (bzip2-state-curr state)))
               (transition-to bzip2-coding-tables-alpha-loop)))

           (bzip2-create-huffman-decode-tables (state)
             (declare (type bzip2-state state))
             (loop with n-groups = (bzip2-state-n-groups state)
                with len = (bzip2-state-len state)
                for x from 0 below n-groups
                do (loop with minLen = 32
                      with maxLen = 0
                      with alpha-size = (bzip2-state-alpha-size state)
                      for y from 0 below alpha-size
                      do (let ((xy (aref len x y)))
                           (setf maxLen (max maxLen xy)
                                 minLen (min minLen xy)))
                      finally
                        (make-decode-tables state x minLen maxLen alpha-size)
                        (setf (aref (bzip2-state-min-lengths state) x) minLen))
                finally
                  ;; We're not 'returning' anything here, we're just
                  ;; forcing this call to be in tail position.
                  (return (transition-to bzip2-initialize-mtf-values))))

           (bzip2-initialize-mtf-values (state)
             (declare (type bzip2-state state))
             (loop
                with kk = (1- +mtfa-size+)
                with mtfa = (bzip2-state-mtfa state)
                with mtfbase = (bzip2-state-mtfbase state)
                initially
                  (setf (bzip2-state-EOB state) (1+ (bzip2-state-n-in-use state))
                        (bzip2-state-nblockMAX state) (* 100000 (bzip2-state-100k-block-size state))
                        (bzip2-state-group-number state) -1
                        (bzip2-state-group-position state) 0)
                  (fill (bzip2-state-unzftab state) 0)
                for i from (1- (floor 256 +mtfl-size+)) downto 0
                do (loop for j from (1- +mtfl-size+) downto 0
                      do
                        (setf (aref mtfa kk) (+ (* i +mtfl-size+) j))
                        (decf kk)
                      finally
                        (setf (aref mtfbase i) (1+ kk)))
                finally
                  (setf (bzip2-state-nblock state) 0
                        (bzip2-state-mtf-continuation state) #'bzip2-enter-mtf-decode-loop)
                  ;; We're not 'returning' anything here, we're just
                  ;; forcing this call to be in tail position.
                  (return (transition-to bzip2-get-mtf-value))))

           (bzip2-get-mtf-value (state)
             (declare (type bzip2-state state))
             (when (zerop (bzip2-state-group-position state))
               (when (>= (incf (bzip2-state-group-number state))
                         (bzip2-state-n-selectors state))
                 (error 'invalid-bzip2-data))
               (let ((s (aref (bzip2-state-selector state)
                              (bzip2-state-group-number state))))
                 (setf (bzip2-state-group-position state) +bz-g-size+
                       (bzip2-state-g-minlen state) (aref (bzip2-state-min-lengths state) s)
                       (bzip2-state-g-limit state) (aref (bzip2-state-limit state) s)
                       (bzip2-state-g-perm state) (aref (bzip2-state-perm state) s)
                       (bzip2-state-g-base state) (aref (bzip2-state-base state) s))))
             (decf (bzip2-state-group-position state))
             (setf (bzip2-state-zn state) (bzip2-state-g-minlen state))
             (transition-to bzip2-get-mtf-value1))

           (bzip2-get-mtf-value1 (state)
             (declare (type bzip2-state state))
             (let ((zvec (ensure-and-read-bits (bzip2-state-zn state) state)))
               (setf (bzip2-state-zvec state) zvec)
               (transition-to bzip2-get-mtf-value2)))

           (bzip2-get-mtf-value2 (state)
             (declare (type bzip2-state state))
             (when (> (bzip2-state-zn state) 20)
               (error 'invalid-bzip2-data))
             (cond
               ((<= (bzip2-state-zvec state)
                    (aref (bzip2-state-g-limit state)
                          (bzip2-state-zn state)))
                (transition-to bzip2-get-mtf-value-done))
               (t
                (incf (bzip2-state-zn state))
                (transition-to bzip2-get-mtf-value3))))

           (bzip2-get-mtf-value3 (state)
             (declare (type bzip2-state state))
             (let ((zj (ensure-and-read-bits 1 state)))
               (setf (bzip2-state-zvec state)
                     (logior (ash (bzip2-state-zvec state) 1) zj))
               (transition-to bzip2-get-mtf-value2)))

           (bzip2-get-mtf-value-done (state)
             (declare (type bzip2-state state))
             (let* ((g-base (bzip2-state-g-base state))
                    (zn (bzip2-state-zn state))
                    (zvec (bzip2-state-zvec state))
                    (index (- zvec (aref g-base zn))))
               (when (or (< index 0) (>= index +bz-max-alpha-size+))
                 (error 'invalid-bzip2-data))
               (setf (bzip2-state-lval state)
                     (aref (bzip2-state-g-perm state) index))
               (let ((f (bzip2-state-mtf-continuation state)))
                 (declare (type function f))
                 (setf (bzip2-state-state state) f)
                 (funcall f state))))

           (bzip2-enter-mtf-decode-loop (state)
             (declare (type bzip2-state state))
             (let ((next-sym (bzip2-state-lval state)))
               (cond
                 ((= next-sym (bzip2-state-EOB state))
                  (transition-to bzip2-prepare-cftab))
                 ((or (= next-sym +bz-runa+) (= next-sym +bz-runb+))
                  (setf (bzip2-state-es state) -1
                        (bzip2-state-N state) 1)
                  (transition-to bzip2-decode-rle-sequence))
                 (t
                  (transition-to bzip2-runc)))))

           (bzip2-decode-rle-sequence (state)
             (declare (type bzip2-state state))
             (let ((next-sym (bzip2-state-lval state)))
               (cond
                 ((= next-sym +bz-runa+)
                  (incf (bzip2-state-es state) (bzip2-state-N state)))
                 ((= next-sym +bz-runb+)
                  (incf (bzip2-state-es state) (* (bzip2-state-N state) 2))))
               (setf (bzip2-state-N state) (* (bzip2-state-N state) 2))
               (setf (bzip2-state-mtf-continuation state) #'bzip2-maybe-finish-rle-sequence)
               (transition-to bzip2-get-mtf-value)))

           (bzip2-maybe-finish-rle-sequence (state)
             (declare (type bzip2-state state))
             (let ((next-sym (bzip2-state-lval state)))
               (if (or (= next-sym +bz-runa+) (= next-sym +bz-runb+))
                   (transition-to bzip2-decode-rle-sequence)
                   (transition-to bzip2-finish-rle-sequence))))

           (bzip2-finish-rle-sequence (state)
             (declare (type bzip2-state state))
             (let ((uc (aref (bzip2-state-seq-to-unseq state)
                             (aref (bzip2-state-mtfa state)
                                   (aref (bzip2-state-mtfbase state) 0)))))
               (incf (aref (bzip2-state-unzftab state) uc)
                     (incf (bzip2-state-es state)))
               (if (bzip2-state-small-decompression-p state)
                   (error 'bzip2-small-decompression-unimplemented)
                   (loop with nblock = (bzip2-state-nblock state)
                      with nblockMAX = (bzip2-state-nblockMAX state)
                      with tt = (bzip2-state-tt state)
                      repeat (bzip2-state-es state)
                      do
                        (when (>= nblock nblockMAX)
                          (error 'invalid-bzip2-data))
                        (setf (aref tt nblock) uc)
                        (incf nblock)
                      finally
                        (setf (bzip2-state-nblock state) nblock)
                        ;; We're not 'returning' anything here, we're
                        ;; just forcing this call to be in tail
                        ;; position.
                        (return (transition-to bzip2-enter-mtf-decode-loop))))))

           (bzip2-runc (state)
             (declare (type bzip2-state state))
             (let ((next-sym (bzip2-state-lval state))
                   (uc 0))
               (when (>= (bzip2-state-nblock state)
                         (bzip2-state-nblockMAX state))
                 (error 'invalid-bzip2-data))
               (let ((mtfbase (bzip2-state-mtfbase state))
                     (mtfa (bzip2-state-mtfa state))
                     (nn (1- next-sym)))
                 (cond
                   ((< nn +mtfl-size+)
                    ;; "avoid general-case expense"
                    (let ((pp (aref mtfbase 0)))
                      (setf uc (aref mtfa (+ pp nn)))
                      (replace mtfa mtfa :start1 (1+ pp) :end1 (+ pp nn 1)
                               :start2 pp :end2 (+ pp nn))
                      (setf (aref mtfa pp) uc)))
                   (t
                    ;; "general case"
                    (let* ((lno (truncate nn +mtfl-size+))
                           (off (rem nn +mtfl-size+))
                           (pp (+ (aref mtfbase lno) off)))
                      (setf uc (aref mtfa pp))
                      (loop while (> pp (aref mtfbase lno))
                         do (setf (aref mtfa pp) (aref mtfa (1- pp)))
                         (decf pp))
                      (incf (aref mtfbase lno))
                      (loop for x from lno above 0
                         do 
                         (setf (aref mtfa (decf (aref mtfbase x)))
                               (aref mtfa (+ (aref mtfbase (1- x)) (1- +mtfl-size+)))))
                      (setf (aref mtfa (decf (aref mtfbase 0))) uc)
                      (when (zerop (aref mtfbase 0))
                        (loop with kk = (1- +mtfa-size+)
                           for ii from (1- (floor 256 +mtfl-size+)) downto 0
                           do (loop for jj from (1- +mtfl-size+) downto 0
                                 do (setf (aref mtfa kk)
                                          (aref mtfa (+ (aref mtfbase ii) jj)))
                                 (decf kk))
                           (setf (aref mtfbase ii) (1+ kk)))))))
                 (incf (aref (bzip2-state-unzftab state)
                             (aref (bzip2-state-seq-to-unseq state) uc)))
                 (if (bzip2-state-small-decompression-p state)
                     (error 'bzip2-small-decompression-unimplemented)
                     (setf (aref (bzip2-state-tt state) (bzip2-state-nblock state))
                           (aref (bzip2-state-seq-to-unseq state) uc)))
                 (incf (bzip2-state-nblock state))
                 (setf (bzip2-state-mtf-continuation state) #'bzip2-enter-mtf-decode-loop)
                 (transition-to bzip2-get-mtf-value))))

           (bzip2-prepare-cftab (state)
             (declare (type bzip2-state state))
             (when (or (minusp (bzip2-state-original-pointer state))
                       (>= (bzip2-state-original-pointer state)
                           (bzip2-state-nblock state)))
               (error 'invalid-bzip2-data))
             (let ((cftab (bzip2-state-cftab state))
                   (unzftab (bzip2-state-unzftab state)))
               (setf (aref cftab 0) 0)
               (replace cftab unzftab :start1 1 :end1 257 :start2 0 :end2 256)
               (loop for i from 1 to 256
                  do (incf (aref cftab i) (aref cftab (1- i))))
               (loop with nblock = (bzip2-state-nblock state)
                  for i from 0 to 256
                  unless (<= 0 (aref cftab i) nblock)
                  do (error 'invalid-bzip2-data))
               (setf (bzip2-state-out-len state) 0
                     (bzip2-state-out-ch state) 0
                     (bzip2-state-calculated-block-crc state) #xffffffff)
               (loop with nblock = (bzip2-state-nblock state)
                  with tt = (bzip2-state-tt state)
                  for i from 0 below nblock
                  do (let ((uc (logand (aref tt i) #xff)))
                       (setf (aref tt (aref cftab uc))
                             (logior (aref tt (aref cftab uc)) (ash i 8)))
                       (incf (aref cftab uc)))
                  finally
                    (setf (bzip2-state-t-position state)
                          (ash (aref tt (bzip2-state-original-pointer state)) -8))
                    (setf (bzip2-state-n-blocks-used state) 0)
                    (cond
                      ((bzip2-state-block-randomized-p state)
                       (error 'bzip2-randomized-blocks-unimplemented))
                      (t
                       (setf (bzip2-state-t-position state) (aref tt (bzip2-state-t-position state))
                             (bzip2-state-k0 state) (logand #xff (bzip2-state-t-position state))
                             (bzip2-state-t-position state) (ash (bzip2-state-t-position state) -8))
                       (incf (bzip2-state-n-blocks-used state))))
                    ;; We're not 'returning' anything here, we're just
                    ;; forcing this call to be in tail position.
                    (return (transition-to bzip2-output)))))

           (bzip2-output (state)
             (declare (type bzip2-state state))
             (let ((corruptp (undo-rle-obuf-to-output state)))
               (when corruptp
                 (error 'invalid-bzip2-data))
               (unless (and (= (bzip2-state-n-blocks-used state)
                               (1+ (bzip2-state-nblock state)))
                            (zerop (bzip2-state-out-len state)))
                 (throw 'bzip2-done :ok))
               (let ((stored (bzip2-state-stored-block-crc state))
                     (calculated (bzip2-state-calculated-block-crc state)))
                 (setf calculated (logand #xffffffff (lognot calculated)))
                 (setf (bzip2-state-calculated-block-crc state) calculated)
                 (unless (= calculated stored)
                   (error 'checksum-mismatch
                          :stored stored
                          :computed calculated
                          :kind :crc32))
                 (setf (bzip2-state-calculated-combined-crc state)
                       (logand #xffffffff
                               (logior (ash (bzip2-state-calculated-combined-crc state) 1)
                                       (ash (bzip2-state-calculated-combined-crc state) -31))))
                 (setf (bzip2-state-calculated-combined-crc state)
                       (logand #xffffffff
                               (logxor (bzip2-state-calculated-combined-crc state)
                                       calculated)))
                 (transition-to bzip2-block-header1))))

           (bzip2-end-header2 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x72)
                   (transition-to bzip2-end-header3)
                   (error 'invalid-bzip2-data))))

           (bzip2-end-header3 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x45)
                   (transition-to bzip2-end-header4)
                   (error 'invalid-bzip2-data))))

           (bzip2-end-header4 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x38)
                   (transition-to bzip2-end-header5)
                   (error 'invalid-bzip2-data))))

           (bzip2-end-header5 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (if (= byte #x50)
                   (transition-to bzip2-end-header6)
                   (error 'invalid-bzip2-data))))

           (bzip2-end-header6 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (unless (= byte #x90)
                 (error 'invalid-bzip2-data))
               (setf (bzip2-state-stored-combined-crc state) 0)
               (transition-to bzip2-stored-combined-crc32-1)))

           (bzip2-stored-combined-crc32-1 (state)
             (declare (type bzip2-state state))
             (setf (bzip2-state-stored-combined-crc state)
                   (ensure-and-read-bits 8 state))
             (transition-to bzip2-stored-combined-crc32-2))

           (bzip2-stored-combined-crc32-2 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (setf (bzip2-state-stored-combined-crc state)
                     (logand #xffffffff
                             (logior (ash (bzip2-state-stored-combined-crc state) 8)
                                     byte)))
               (transition-to bzip2-stored-combined-crc32-3)))

           (bzip2-stored-combined-crc32-3 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (setf (bzip2-state-stored-combined-crc state)
                     (logand #xffffffff
                             (logior (ash (bzip2-state-stored-combined-crc state) 8)
                                     byte)))
               (transition-to bzip2-stored-combined-crc32-4)))

           (bzip2-stored-combined-crc32-4 (state)
             (declare (type bzip2-state state))
             (let ((byte (ensure-and-read-bits 8 state)))
               (setf (bzip2-state-stored-combined-crc state)
                     (logand #xffffffff
                             (logior (ash (bzip2-state-stored-combined-crc state) 8)
                                     byte)))
               (unless (= (bzip2-state-stored-combined-crc state)
                          (bzip2-state-calculated-combined-crc state))
                 (error 'checksum-mismatch
                        :stored (bzip2-state-stored-combined-crc state)
                        :computed (bzip2-state-calculated-combined-crc state)
                        :kind :crc32))
               (setf (bzip2-state-done state) t)
               (transition-to bzip2-done)))

           (bzip2-done (state)
             (declare (ignore state))
             (throw 'bzip2-done t))
           )
    (unless (bzip2-state-state state)
      (setf (bzip2-state-state state) #'bzip2-header))
    (funcall (the function (bzip2-state-state state)) state)))

(defun %bzip2-decompress (state input output &key (input-start 0) input-end
                          (output-start 0) output-end)
  (declare (type bzip2-state state))
  (let* ((input-end (or input-end (length input)))
         (output-end (or output-end (length output))))
    (setf (bzip2-state-input state) input
          (bzip2-state-input-start state) input-start
          (bzip2-state-input-index state) input-start
          (bzip2-state-input-end state) input-end
          (bzip2-state-output state) output
          (bzip2-state-output-start state) output-start
          (bzip2-state-output-index state) output-start
          (bzip2-state-output-end state) output-end)
    (catch 'bzip2-done
      (%bzip2-state-machine state))
    (values (- (bzip2-state-input-index state) input-start)
            (- (bzip2-state-output-index state) output-start))))

(defun make-bzip2-state ()
  (let ((state (%make-bzip2-state)))
    (setf (dstate-checksum state) (make-crc32)
          (dstate-update-checksum state) #'update-crc32)
    state))
