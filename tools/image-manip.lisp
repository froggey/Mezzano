(in-package :cl-user)

(defun read-image-header (stream offset)
  (let ((header (make-array 4096 :element-type '(unsigned-byte 8))))
    (file-position stream offset)
    (read-sequence header stream)
    header))

(defparameter *mezzano-magic* #(#x00 #x4D #x65 #x7A #x7A #x61 #x6E #x69 #x6E #x65 #x49 #x6D #x61 #x67 #x65 #x00))

(defun decode-image-header (header)
  ;; Check the magic number and version numbers match.
  (when (not (every 'eql (subseq header 0 16) *mezzano-magic*))
    (error "Image header has wrong magic at start."))
  (let ((major (nibbles:ub16ref/le header 32))
        (minor (nibbles:ub16ref/le header 34)))
    (when (not (and (eql major 0)
                    (eql minor 23)))
      (error "Image has unsupported protocol version ~D.~D.~%" major minor))
    (let* ((uuid (subseq header 16 32))
           (entry-fref (nibbles:ub64ref/le header 40))
           (initial-thread (nibbles:ub64ref/le header 48))
           (nil-value (nibbles:ub64ref/le header 56))
           (architecture (nibbles:ub32ref/le header 64))
           (bml4 (nibbles:ub64ref/le header 96))
           (freelist (nibbles:ub64ref/le header 104)))
      (list :uuid uuid
            :major-version major
            :minor-version minor
            :entry-fref entry-fref
            :initial-thread initial-thread
            :nil-value nil-value
            :architecture (case architecture
                            (1 :x86-64)
                            (2 :arm64)
                            (t `(:unknown ,architecture)))
            :bml4 bml4
            :freelist freelist))))

(defun encode-architecture (arch)
  (ecase arch
    (:x86-64 1)
    (:arm64 2)))

(defun encode-image-header (header)
  (let ((data (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace data *mezzano-magic*)
    (replace data (getf header :uuid) :start1 16)
    (setf (nibbles:ub16ref/le data 32) (getf header :major-version)
          (nibbles:ub16ref/le data 34) (getf header :minor-version))
    (setf (nibbles:ub64ref/le data 40) (getf header :entry-fref))
    (setf (nibbles:ub64ref/le data 48) (getf header :initial-thread))
    (setf (nibbles:ub64ref/le data 56) (getf header :nil-value))
    (setf (nibbles:ub32ref/le data 64) (encode-architecture (getf header :architecture)))
    (setf (nibbles:ub64ref/le data 96) (getf header :bml4))
    (setf (nibbles:ub64ref/le data 104) (getf header :freelist))
    data))

(defun decode-block-map-entry (entry)
  (let ((block-id (ldb (byte 54 8) entry)))
    (cond ((zerop block-id)
           (assert (zerop entry))
           '())
          (t
           (let ((presentp (logbitp 0 entry))
                 (writep (logbitp 1 entry))
                 (zero-fill-p (logbitp 2 entry))
                 (wiredp (logbitp 4 entry)))
             (assert (zerop (logand entry (lognot #x3FFFFFFFFFFFFF17))))
             (when (eql block-id (1- (ash 1 54)))
               (setf block-id :lazy))
             (list :block-id block-id
                   :presentp presentp
                   :writep writep
                   :zero-fill-p zero-fill-p
                   :wiredp wiredp))))))

(defun read-block-map (stream offset bml4-block)
  (let ((block-map (make-hash-table)))
    (labels ((one-level (block-id fn address address-level-shift)
               (let ((disk-block (make-array 4096 :element-type '(unsigned-byte 8))))
                 (file-position stream (+ offset (* block-id 4096)))
                 (read-sequence disk-block stream)
                 (loop
                    for i by 8 below 4096
                    for j from 0
                    for entry = (nibbles:ub64ref/le disk-block i)
                    when (not (zerop (ldb (byte 54 8) entry)))
                    do
                      (funcall fn
                               entry
                               (logior address (ash j address-level-shift))))))
             (level-0 (block-id address)
               (setf (gethash address block-map) (decode-block-map-entry block-id)))
             (level-1 (block-id address)
               (one-level (ldb (byte 54 8) block-id) #'level-0 address 12))
             (level-2 (block-id address)
               (one-level (ldb (byte 54 8) block-id) #'level-1 address 21))
             (level-3 (block-id address)
               (one-level (ldb (byte 54 8) block-id) #'level-2 address 30)))
      (one-level bml4-block #'level-3 0 39)
      block-map)))

(defun encode-block-info (info)
  (cond (info
         (let ((block-id (getf info :block-id)))
           (when (eql block-id :lazy)
             (setf block-id (1- (ash 1 54))))
           (logior (ash block-id 8)
                   (if (getf info :presentp)
                       #b00000001
                       #b00000000)
                   (if (getf info :writep)
                       #b00000010
                       #b00000000)
                   (if (getf info :zero-fill-p)
                       #b00000100
                       #b00000000)
                   (if (getf info :wiredp)
                       #b00010000
                       #b00000000))))
        (t
         0)))

(defun encode-block-map (block-table)
  (let ((bml4 (make-array 512 :initial-element nil)))
    (maphash (lambda (virtual-address info)
               (let ((bml4e (ldb (byte 9 39) virtual-address))
                     (bml3e (ldb (byte 9 30) virtual-address))
                     (bml2e (ldb (byte 9 21) virtual-address))
                     (bml1e (ldb (byte 9 12) virtual-address)))
                 (unless (aref bml4 bml4e)
                   (setf (aref bml4 bml4e) (make-array 512 :initial-element nil)))
                 (let ((bml3 (aref bml4 bml4e)))
                   (unless (aref bml3 bml3e)
                     (setf (aref bml3 bml3e) (make-array 512 :initial-element nil)))
                   (let ((bml2 (aref bml3 bml3e)))
                     (unless (aref bml2 bml2e)
                       (setf (aref bml2 bml2e) (make-array 512 :initial-element nil)))
                     (let ((bml1 (aref bml2 bml2e)))
                       (assert (not (aref bml1 bml1e)))
                       (setf (aref bml1 bml1e) (encode-block-info info)))))))
             block-table)
    bml4))

(defun blocks-required-for-block-map (bml4)
  (labels ((count-tables (array)
             (let ((total 1))
               (dotimes (i (length array))
                 (when (arrayp (aref array i))
                   (incf total (count-tables (aref array i)))))
               total)))
    (count-tables bml4)))

(defun flatten-block-map (bml4 first-block)
  (let* ((block-count (blocks-required-for-block-map bml4))
         (next-block-id first-block)
         (array-index 0)
         (block-table (make-array (* 4096 block-count)
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
    ;; Write each level into the block table.
    (labels ((write-level (level)
               (let ((my-index array-index)
                     (my-block next-block-id))
                 (incf array-index 4096)
                 (incf next-block-id)
                 (dotimes (i 512)
                   (let ((entry (aref level i)))
                     (cond ((integerp entry)
                            (setf (nibbles:ub64ref/le block-table (+ my-index (* i 8))) entry))
                           (entry
                            (setf (nibbles:ub64ref/le block-table (+ my-index (* i 8))) (ash (write-level entry) 8))))))
                 my-block)))
      (write-level bml4)
      block-table)))

(defun write-block-map (output-stream output-offset bml4)
  (let* ((bml4-block (truncate (- (file-position output-stream) output-offset) 4096))
         (table (flatten-block-map bml4 bml4-block)))
    (write-sequence table output-stream)
    bml4-block))

(defun flatten-image-stream (input-stream output-stream &key (input-offset 0) (output-offset 0) output-uuid)
  (let* ((input-header (decode-image-header (read-image-header input-stream input-offset)))
         (block-map (read-block-map input-stream input-offset (getf input-header :bml4)))
         (output-block-map (make-hash-table))
         ;; Sort blocks by address.
         (sorted-blocks (sort (alexandria:hash-table-alist block-map) #'<
                              :key #'first))
         (output-bml4 nil)
         (output-freelist-block nil))
    ;; Copy blocks from the input to the output.
    (file-position output-stream (+ output-offset 4096))
    (loop
       with block-buffer = (make-array 4096 :element-type '(unsigned-byte 8))
       for (address . info) in sorted-blocks
       for block-id = (getf info :block-id)
       do
         (setf (gethash address output-block-map) (copy-list info))
         (setf (getf (gethash address output-block-map) :block-id) (truncate (- (file-position output-stream) output-offset) 4096))
         (file-position input-stream (+ input-offset (* block-id 4096)))
         (read-sequence block-buffer input-stream)
         (write-sequence block-buffer output-stream))
    ;; Generate the output's block map.
    (setf output-bml4 (write-block-map output-stream output-offset (encode-block-map output-block-map)))
    ;; Write the free list. A single allocated entry covering the whole image.
    (setf output-freelist-block (truncate (- (file-position output-stream) output-offset) 4096))
    (let* ((freelist-data (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
           (total-size (- (+ (file-position output-stream) 4096) output-offset)))
      (setf (nibbles:ub64ref/le freelist-data 0) 0
            (nibbles:ub64ref/le freelist-data 8) (ash (truncate total-size 4096) 1))
      (write-sequence freelist-data output-stream))
    ;; Generate a new header.
    (let ((output-header (copy-list input-header)))
      (cond ((null output-uuid)
             (setf (getf output-header :uuid) (cold-generator::generate-uuid)))
            ((stringp output-uuid)
             (setf (getf output-header :uuid) (cold-generator::parse-uuid output-uuid)))
            ((not (eql output-uuid t))
             (setf (getf output-header :uuid) output-uuid)))
      (setf (getf output-header :bml4) output-bml4
            (getf output-header :freelist) output-freelist-block)
      (file-position output-stream (+ output-offset 0))
      (write-sequence (encode-image-header output-header) output-stream))
    output-stream))

(defun flatten-image (input-path output-path &rest args)
  (with-open-file (input-stream input-path :element-type '(unsigned-byte 8))
    (with-open-file (output-stream output-path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (apply #'flatten-image-stream input-stream output-stream args))))

(defun image-uuid (image-path &key (offset 0))
  (with-open-file (image image-path :element-type '(unsigned-byte 8))
    (let* ((header (decode-image-header (read-image-header image offset))))
      (getf header :uuid))))
