(in-package :chipz)

(defun update-window (state)
  (declare (type inflate-state state))
  (let* ((output (inflate-state-output state))
         (start (inflate-state-output-start state))
         (index (inflate-state-output-index state))
         (n-bytes-to-copy (- index start))
         (window (inflate-state-window state))
         (window-index (inflate-state-window-index state)))
    (cond
      ((>= n-bytes-to-copy (length window))
       ;; can "flush" the window
       (setf (inflate-state-window-index state) 0)
       (replace window output :start2 (- index (length window))
                :end2 index))
      (t
       (let ((window-space (- (length window) window-index)))
         (cond
           ((> n-bytes-to-copy window-space)
            (replace window output :start1 window-index
                     :start2 start :end2 index)
            (replace window output
                     :start2 (+ start window-space)
                     :end2 index)
            (setf (inflate-state-window-index state)
                  (- n-bytes-to-copy window-space)))
           (t
            (replace window output :start1 window-index
                     :start2 start :end2 index)
            (setf (inflate-state-window-index state)
                  (mod (+ window-index n-bytes-to-copy) (length window))))))))))

;;; This is used behind-the-scenes to do efficient buffer->buffer
;;; decompression.  Everything user-visible that's related to
;;; decompression ultimately comes down to this function.
(defun %inflate (state input output &key (input-start 0) input-end
                (output-start 0) output-end)
  "Decompresses data in INPUT between INPUT-START and INPUT-END
and places the result in OUTPUT between OUTPUT-START and
OUTPUT-END.  -START and -END arguments follow the convention of
the sequence functions.  Returns the number of bytes pulled from
the input and the number of bytes written to the output."
  (declare (type inflate-state state))
  (let* ((input-end (or input-end (length input)))
         (output-end (or output-end (length output))))
    (setf (inflate-state-input state) input
          (inflate-state-input-start state) input-start
          (inflate-state-input-index state) input-start
          (inflate-state-input-end state) input-end
          (inflate-state-output state) output
          (inflate-state-output-start state) output-start
          (inflate-state-output-index state) output-start
          (inflate-state-output-end state) output-end)
    (catch 'inflate-done
      (%inflate-state-machine state))
    (update-window state)
    (when (dstate-update-checksum state)
      (funcall (dstate-update-checksum state)
               (dstate-checksum state) output output-start
               (inflate-state-output-index state)))
    (values (- (inflate-state-input-index state) input-start)
            (- (inflate-state-output-index state) output-start))))


(defun record-code-length (state value)
  (setf (aref (inflate-state-code-lengths state)
              (aref *code-length-code-order*
                    (inflate-state-n-values-read state))) value)
  (incf (inflate-state-n-values-read state)))


;;; internal inflate function

(defun %inflate-state-machine (state)
  (declare (type inflate-state state))
  ;; The reasoning behind this monstrosity is that we were using
  ;; SYMBOL-FUNCTION to drive a nice elegant state machine...except that
  ;; SYMBOL-FUNCTION is a horrible thing to call in inner loops.  So we
  ;; changed to this, which avoids the function lookup overhead.
  (labels (
           (read-bits (n state)
             (declare (type (integer 0 32) n))
             (declare (type inflate-state state))
             (prog1 (ldb (byte n 0) (inflate-state-bits state))
               (setf (inflate-state-bits state)
                     (ash (inflate-state-bits state) (- n)))
               (decf (inflate-state-n-bits state) n)))

           (ensure-bits (n state)
             (declare (type (integer 0 32) n))
             (declare (type inflate-state state))
             (let ((bits (inflate-state-bits state))
                   (n-bits (inflate-state-n-bits state))
                   (input-index (inflate-state-input-index state)))
               (declare (type (unsigned-byte 32) bits))
               (loop while (< n-bits n)
                  when (>= input-index (inflate-state-input-end state))
                  do (progn
                       (setf (inflate-state-bits state) bits
                             (inflate-state-n-bits state) n-bits
                             (inflate-state-input-index state) input-index)
                       (throw 'inflate-done nil))
                  do (let ((byte (aref (inflate-state-input state) input-index)))
                       (declare (type (unsigned-byte 8) byte))
                       (setf bits
                             (logand #xffffffff (logior (ash byte n-bits) bits)))
                       (incf n-bits 8)
                       (incf input-index))
                  finally (setf (inflate-state-bits state) bits
                                (inflate-state-n-bits state) n-bits
                                (inflate-state-input-index state) input-index))))

           (ensure-and-read-bits (n state)
             (ensure-bits n state)
             (read-bits n state))

           (align-bits-bytewise (state)
             (declare (type inflate-state state))
             (let ((n-bits (inflate-state-n-bits state)))
               (decf (inflate-state-n-bits state) (rem n-bits 8))
               (setf (inflate-state-bits state)
                     (ash (inflate-state-bits state)
                          (- (rem n-bits 8))))
               (values)))

           (decode-value (table state)
             (declare (type huffman-decode-table table))
             (declare (type inflate-state state))
             (declare (optimize (speed 3)))
             (ensure-bits (hdt-bits table) state)
             (let ((bits (inflate-state-bits state)))
               (declare (type (unsigned-byte 32) bits))
               (do ((counts (hdt-counts table))
                    (len 1 (1+ len))
                    (first 0 (probably-the-fixnum (ash first 1)))
                    (code 0 (probably-the-fixnum (ash code 1))))
                   ((>= len +max-code-length+) nil)
                 (declare (type (and fixnum (integer 0 *)) first code))
                 ;; We would normally do this with READ-BITS, but DECODE-VALUE
                 ;; is a hotspot in profiles along with this would-be call to
                 ;; READ-BITS, so we inline it all here.
                 (setf code (logior code (logand bits 1))
                       bits (ash bits -1))
                 (let ((count (aref counts len)))
                   (when (< (- code count) first)
                     (setf (inflate-state-bits state) bits)
                     (decf (inflate-state-n-bits state) len)
                     (return-from decode-value (aref (hdt-symbols table)
                                                     (probably-the-fixnum 
                                                      (+ (aref (hdt-offsets table) (1- len))
                                                         (- code first))))))
                   (setf first
                         (probably-the-fixnum (+ first count)))))))

           (read-dynamic-table (state decoder n-values)
             (declare (type inflate-state state))
             (loop with lengths = (inflate-state-code-lengths state)
                while (< (inflate-state-n-values-read state) n-values)
                do (ensure-bits (+ (hdt-bits decoder) 7) state)
                (let ((value (decode-value decoder state)))
                  (cond
                    ((< value 16)
                     (setf (aref lengths (inflate-state-n-values-read state)) value)
                     (incf (inflate-state-n-values-read state)))
                    (t
                     (let ((len 0) (sym 0))
                       (cond
                         ((= value 16)
                          (setf sym (aref lengths (1- (inflate-state-n-values-read state))))
                          (setf len (+ 3 (read-bits 2 state))))
                         ((= value 17)
                          (setf len (+ 3 (read-bits 3 state))))
                         ((= value 18)
                          (setf len (+ 11 (read-bits 7 state)))))
                       (fill lengths sym :start (inflate-state-n-values-read state)
                             :end (+ (inflate-state-n-values-read state) len))
                       (incf (inflate-state-n-values-read state) len)))))
                finally (progn
                          (assert (= n-values (inflate-state-n-values-read state)))
                          (return (construct-huffman-decode-table lengths n-values)))))

           ;; Basic starter functions.
           (done (state)
             (declare (ignore state))
             (throw 'inflate-done t))

           (block-type (state)
             (cond
               ((inflate-state-final-block-p state)
                (align-bits-bytewise state)
                (setf (inflate-state-state state)
                      (ecase (inflate-state-data-format state)
                        (deflate
                         (setf (inflate-state-done state) t)
                            #'done)
                        (zlib #'check-zlib-adler32)
                        (gzip #'gzip-crc32))))
               (t
                (ensure-bits 3 state)
                (setf (inflate-state-final-block-p state) (= 1 (read-bits 1 state)))
                (ecase (read-bits 2 state)
                  (#.+block-no-compress+
                   (setf (inflate-state-state state) #'uncompressed-block)
                   (uncompressed-block state))
                  (#.+block-fixed-codes+
                   (setf (inflate-state-literal/length-table state)
                         *fixed-literal/length-table*
                         (inflate-state-distance-table state)
                         *fixed-distance-table*
                         (inflate-state-state state) #'literal/length)
                   (literal/length state))
                  (#.+block-dynamic-codes+
                   (setf (inflate-state-state state) #'dynamic-tables)
                   (dynamic-tables state))
                  (#.+block-invalid+
                   (error 'reserved-block-type-error))))))

;;; processing uncompressed blocks

           (uncompressed-block (state)
             (align-bits-bytewise state)
             (let* ((len (ensure-and-read-bits 16 state))
                    (nlen (ensure-and-read-bits 16 state)))
               (unless (zerop (logand len nlen))
                 ;; Apparently Adobe's PDF generator(s) get this wrong, so let the
                 ;; user continue on if they choose to do so.
                 (cerror "Use the invalid stored block length."
                         'invalid-stored-block-length-error))
               (setf (inflate-state-length state) len
                     (inflate-state-state state) #'copy-bytes)))

           (copy-bytes (state)
             (declare (type inflate-state state))
             (if (zerop (inflate-state-length state))
                 (setf (inflate-state-state state) #'block-type)
                 (let ((n-copied-bytes (min (inflate-state-length state)
                                            (- (inflate-state-input-end state)
                                               (inflate-state-input-index state))
                                            (- (inflate-state-output-end state)
                                               (inflate-state-output-index state)))))
                   (cond
                     ((zerop n-copied-bytes) (throw 'inflate-done nil))
                     (t
                      (replace (inflate-state-output state)
                               (inflate-state-input state)
                               :start1 (inflate-state-output-index state)
                               :end1 (+ (inflate-state-output-index state)
                                        n-copied-bytes)
                               :start2 (inflate-state-input-index state)
                               :end2 (+ (inflate-state-input-index state)
                                        n-copied-bytes))
                      (incf (inflate-state-input-index state) n-copied-bytes)
                      (incf (inflate-state-output-index state) n-copied-bytes)
                      (decf (inflate-state-length state) n-copied-bytes)))))
             (values))

;;; dynamic block compression tables

           (dynamic-tables (state)
             (declare (type inflate-state state))
             (ensure-bits 14 state)
             (setf (inflate-state-n-length-codes state) (+ (read-bits 5 state) 257)
                   (inflate-state-n-distance-codes state) (+ (read-bits 5 state) 1)
                   (inflate-state-n-codes state) (+ (read-bits 4 state) 4)
                   (inflate-state-n-values-read state) 0
                   (inflate-state-state state) #'dynamic-code-lengths)
             (dynamic-code-lengths state))

           (dynamic-code-lengths (state)
             (declare (type inflate-state state))
             (loop while (< (inflate-state-n-values-read state)
                            (inflate-state-n-codes state))
                do (ensure-bits 3 state)
                (record-code-length state (read-bits 3 state)))
             (loop while (< (inflate-state-n-values-read state) +max-n-code-lengths+)
                do (record-code-length state 0))
             (setf (inflate-state-codes-table state)
                   (construct-huffman-decode-table (inflate-state-code-lengths state)
                                                   +max-n-code-lengths+)
                   (inflate-state-n-values-read state) 0
                   (inflate-state-state state) #'dynamic-literal/length-table)
             (dynamic-literal/length-table state))

           (dynamic-literal/length-table (state)
             (declare (type inflate-state state))
             (setf (inflate-state-literal/length-table state)
                   (read-dynamic-table state (inflate-state-codes-table state)
                                       (inflate-state-n-length-codes state))
                   (inflate-state-n-values-read state) 0
                   (inflate-state-state state) #'dynamic-distance-table)
             (dynamic-distance-table state))

           (dynamic-distance-table (state)
             (declare (type inflate-state state))
             (setf (inflate-state-distance-table state)
                   (read-dynamic-table state (inflate-state-codes-table state)
                                       (inflate-state-n-distance-codes state))
                   (inflate-state-state state) #'literal/length)
             (literal/length state))

;;; normal operation on compressed blocks

           (literal/length (state)
             (declare (type inflate-state state))
             (let ((value (decode-value (inflate-state-literal/length-table state)
                                        state)))
               (declare (type (integer 0 288) value))
               (cond
                 ((< value 256)
                  (setf (inflate-state-length state) value
                        (inflate-state-state state) #'literal)
                  (literal state))
                 ((> value 256)
                  (setf (inflate-state-length-code state) (- value 257)
                        (inflate-state-state state) #'length-code)
                  (length-code state))
                 (t #+nil (= value 256)
                    (setf (inflate-state-state state) #'block-type)
                    (block-type state)))))

           (literal (state)
             (declare (type inflate-state state))
             (cond
               ((= (inflate-state-output-index state)
                   (inflate-state-output-end state)) (throw 'inflate-done nil))
               (t (setf (aref (inflate-state-output state)
                              (inflate-state-output-index state))
                        (inflate-state-length state))
                  (incf (inflate-state-output-index state))
                  (setf (inflate-state-state state) #'literal/length)
                  (literal/length state))))

           (length-code (state)
             (declare (type inflate-state state))
             (let* ((length-code (inflate-state-length-code state))
                    (length-extra (ensure-and-read-bits (n-length-extra-bits length-code) state)))
               (setf (inflate-state-length state)
                     (+ (length-base length-code) length-extra)
                     (inflate-state-state state) #'distance)
               (distance state)))

           (distance (state)
             (declare (type inflate-state state))
             (let ((value (decode-value (inflate-state-distance-table state)
                                        state)))
               (setf (inflate-state-distance state) value
                     (inflate-state-state state) #'distance-extra)
               (distance-extra state)))

           (distance-extra (state)
             (declare (type inflate-state state))
             (let* ((bits (n-distance-extra-bits (inflate-state-distance state)))
                    (distance-extra (if (zerop bits)
                                        0
                                        (ensure-and-read-bits bits state))))
               (setf (inflate-state-distance state)
                     (+ (distance-base (inflate-state-distance state)) distance-extra)
                     (inflate-state-state state) #'copy-match)
               (copy-match state)))

           (copy-match (state)
             (declare (type inflate-state state))
             (let* ((distance (inflate-state-distance state))
                    (length (inflate-state-length state))
                    (start (inflate-state-output-start state))
                    (index (inflate-state-output-index state))
                    (end (inflate-state-output-end state))
                    (window-index (inflate-state-window-index state))
                    (n-bytes-to-copy (min length (- end index))))
               (when (= index end)
                 (throw 'inflate-done nil))
               (flet ((frob-by-copying-from (copy-source copy-index n-bytes-to-copy)
                        (declare (type (simple-array (unsigned-byte 8) (*)) copy-source))
                        (decf (inflate-state-length state) n-bytes-to-copy)
                        (incf (inflate-state-output-index state) n-bytes-to-copy)
                        (loop with output = (inflate-state-output state)
                           for i from index below (the fixnum (+ index n-bytes-to-copy))
                           for j from copy-index below (the fixnum (+ copy-index n-bytes-to-copy))
                           do (setf (aref output i) (aref copy-source j)))))
                 (cond
                   ((<= distance (- index start))
                    ;; we are within the output we have produced
                    (frob-by-copying-from (inflate-state-output state)
                                          (- index distance)
                                          n-bytes-to-copy))
                   (t
                    (let ((copy-index (+ (- window-index distance) (- index start))))
                      (cond
                        ((not (minusp copy-index))
                         ;; we are within the non-wraparound portion of the window
                         ;;
                         ;; can only copy up to the window's index, though
                         (let ((n-bytes-to-copy (min n-bytes-to-copy (- window-index copy-index))))
                           (frob-by-copying-from (inflate-state-window state)
                                                 copy-index
                                                 n-bytes-to-copy)))
                        (t
                         ;; we are within the wraparound portion of the window
                         (let* ((copy-index (+ copy-index
                                               (length (inflate-state-window state))))
                                (n-bytes-to-copy (min n-bytes-to-copy
                                                      (- (length (inflate-state-window state))
                                                         copy-index))))
                           (frob-by-copying-from (inflate-state-window state)
                                                 copy-index
                                                 n-bytes-to-copy)))))))
                 (when (zerop (inflate-state-length state))
                   (setf (inflate-state-state state) #'literal/length)
                   (literal/length state)))))

           ;; GZIP
           (gzip-header-id (state)
             (declare (type inflate-state state))
             (let ((header-field (ensure-and-read-bits 16 state)))
               (unless (and (= (ldb (byte 8 0) header-field) #x1f)
                            (= (ldb (byte 8 8) header-field) #x8b))
                 (error 'invalid-gzip-header-error))
               (setf (inflate-state-state state) #'gzip-cm)
               (gzip-cm state)))

           (gzip-cm (state)
             (declare (type inflate-state state))
             (let ((cm-byte (ensure-and-read-bits 8 state)))
               (setf (inflate-state-header state)
                     (make-instance 'gzip-header :compression-method cm-byte)
                     (inflate-state-state state) #'gzip-flags)
               (gzip-flags state)))

           (gzip-flags (state)
             (declare (type inflate-state state))
             (let ((flags-byte (ensure-and-read-bits 8 state)))
               (setf (flags (inflate-state-header state)) flags-byte
                     (inflate-state-state state) #'gzip-mtime)
               (gzip-mtime state)))

           (gzip-mtime (state)
             (declare (type inflate-state state))
             (let ((mtime (ensure-and-read-bits 32 state)))
               (setf (mtime (inflate-state-header state)) mtime
                     (inflate-state-state state) #'gzip-xfl)
               (gzip-xfl state)))

           (gzip-xfl (state)
             (declare (type inflate-state state))
             (let ((xfl-byte (ensure-and-read-bits 8 state)))
               (setf (extra-flags (inflate-state-header state)) xfl-byte
                     (inflate-state-state state) #'gzip-os)
               (gzip-os state)))

           (gzip-os (state)
             (declare (type inflate-state state))
             (let ((os-byte (ensure-and-read-bits 8 state)))
               (setf (os (inflate-state-header state)) os-byte
                     (inflate-state-state state) #'gzip-xlen-len)
               (gzip-xlen-len state)))

           (gzip-xlen-len (state)
             (declare (type inflate-state state))
             (let ((flags (flags (inflate-state-header state))))
               (cond
                 ((logbitp +gzip-flag-extra+ flags)
                  (error "gzip extra field not supported yet"))
                 (t
                  (setf (inflate-state-state state) #'gzip-fname)
                  (gzip-fname state)))))

           (gzip-fname (state)
             (declare (type inflate-state state))
             (process-gzip-zero-terminated-field state +gzip-flag-name+
                                                 #'filename #'(setf filename)
                                                 #'gzip-fcomment))

           (gzip-fcomment (state)
             (declare (type inflate-state state))
             (process-gzip-zero-terminated-field state +gzip-flag-comment+
                                                 #'comment #'(setf comment)
                                                 #'gzip-crc16))

           (process-gzip-zero-terminated-field (state control-bit
                                                      slot set-slot
                                                      next-state)
             (let ((header (inflate-state-header state)))
               (cond
                 ((logbitp control-bit (flags header))
                  (let ((byte (ensure-and-read-bits 8 state)))
                    (cond
                      ((zerop byte)
                       ;; the end, convert to sane form
                       (funcall set-slot
                                (coerce (funcall slot header)
                                        '(vector (unsigned-byte 8)))
                                header)
                       (setf (inflate-state-state state) next-state))
                      (t
                       ;; wish we could use PUSH here
                       (funcall set-slot
                                (cons byte (funcall slot header))
                                header)))))
                 (t
                  (setf (inflate-state-state state) next-state)))
               (values)))

           (gzip-crc16 (state)
             (declare (type inflate-state state))
             (let ((header (inflate-state-header state)))
               (when (logbitp +gzip-flag-crc+ (flags header))
                 (let ((crc16 (ensure-and-read-bits 16 state)))
                   ;; FIXME: would be good to perform integrity checking here
                   (declare (ignore crc16))))
               (setf (inflate-state-state state) #'block-type)
               (block-type state)))

           (gzip-crc32 (state)
             (declare (type inflate-state state))
             (let ((stored (ensure-and-read-bits 32 state))
                   (crc32 (copy-crc32 (inflate-state-checksum state))))
               (update-crc32 crc32
                             (inflate-state-output state)
                             (inflate-state-output-start state)
                             (inflate-state-output-index state))
               (unless (= stored (produce-crc32 crc32))
                 (error 'invalid-checksum-error
                        :stored stored
                        :computed (produce-crc32 crc32)
                        :kind :crc32))
               (setf (inflate-state-state state) #'gzip-isize)
               (gzip-isize state)))

           (gzip-isize (state)
             (declare (type inflate-state state))
             (let ((isize (ensure-and-read-bits 32 state)))
               (declare (ignore isize))
               (setf (inflate-state-state state) #'done)
               (setf (inflate-state-done state) t)))

           ;; ZLIB
           (zlib-cmf (state)
             (declare (type inflate-state state))
             (let ((cmf-byte (ensure-and-read-bits 8 state)))
               (setf (inflate-state-header state)
                     (make-instance 'zlib-header :cmf cmf-byte)
                     (inflate-state-state state) #'zlib-flags)
               (zlib-flags state)))

           (zlib-flags (state)
             (declare (type inflate-state state))
             (let ((flags-byte (ensure-and-read-bits 8 state))
                   (header (inflate-state-header state)))
               ;; check
               (unless (zerop (mod (+ (* (cmf header) 256) flags-byte) 31))
                 (error 'invalid-zlib-header-error))
               (setf (flags header) flags-byte
                     (inflate-state-state state) #'zlib-fdict)
               (zlib-fdict state)))

           (zlib-fdict (state)
             (declare (type inflate-state state))
             (let* ((header (inflate-state-header state))
                    (flags-byte (flags header)))
               (when (logbitp +zlib-flag-fdict+ flags-byte)
                 (let ((fdict (ensure-and-read-bits 32 state)))
                   (setf (fdict header) fdict)))
               (setf (inflate-state-state state) #'block-type)
               (block-type state)))

           (check-zlib-adler32 (state)
             (declare (type inflate-state state))
             (let ((stored (let ((x (ensure-and-read-bits 32 state)))
                             (logior (ash (ldb (byte 8 0) x) 24)
                                     (ash (ldb (byte 8 8) x) 16)
                                     (ash (ldb (byte 8 16) x) 8)
                                     (ldb (byte 8 24) x))))
                   (adler32 (copy-adler32 (inflate-state-checksum state))))
               (update-adler32 adler32
                               (inflate-state-output state)
                               (inflate-state-output-start state)
                               (inflate-state-output-index state))
               (unless (= stored
                          (produce-adler32 adler32))
                 (error 'invalid-checksum-error
                        :stored stored
                        :computed (produce-adler32 adler32)
                        :kind :adler32))
               (setf (inflate-state-done state) t
                     (inflate-state-state state) #'done)))
           )
    (unless (inflate-state-state state)
       (setf (inflate-state-state state)
             (ecase (inflate-state-data-format state)
               (deflate #'block-type)
               (zlib #'zlib-cmf)
               (gzip #'gzip-header-id))))
    (loop (funcall (inflate-state-state state) state))))
