;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(declaim (inline memref-ub16/le memref-ub16/be))
(defun memref-ub16/le (base &optional (index 0))
  (sys.int::memref-unsigned-byte-16 base index))

(defun memref-ub16/be (base)
  (logior (ash (sys.int::memref-unsigned-byte-8 base) 8)
          (sys.int::memref-unsigned-byte-8 (+ base 1))))

(declaim (inline memref-ub32/le memref-ub32/be))
(defun memref-ub32/le (base &optional (index 0))
  (sys.int::memref-unsigned-byte-32 base index))

(defun memref-ub32/be (base)
  (logior (ash (sys.int::memref-unsigned-byte-8 base) 24)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 1)) 16)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 2)) 8)
          (sys.int::memref-unsigned-byte-8 (+ base 3))))

(declaim (inline memref-ub64/le memref-ub64/be))
(defun memref-ub64/le (base &optional (index 0))
  (sys.int::memref-unsigned-byte-64 base index))

(defun memref-ub64/be (base)
  (logior (ash (sys.int::memref-unsigned-byte-8 base) 56)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 1)) 48)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 2)) 40)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 3)) 32)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 4)) 24)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 5)) 16)
          (ash (sys.int::memref-unsigned-byte-8 (+ base 6)) 8)
          (sys.int::memref-unsigned-byte-8 (+ base 7))))

(defmacro with-pages ((virtual-address n-pages &rest options) &body body)
  (let ((n-pages-sym (gensym "N-PAGES"))
        (page (gensym "PAGE")))
    `(let* ((,n-pages-sym ,n-pages)
            (,page (allocate-physical-pages ,n-pages-sym ,@options))
            (,virtual-address (when ,page
                                (convert-to-pmap-address (* ,page +4k-page-size+)))))
       (unwind-protect
            (progn ,@body)
         (when ,page
           (release-physical-pages ,page ,n-pages-sym))))))

(defun read-disk-partition (device lba n-sectors buffer)
  (funcall (disk-read-fn (partition-disk device))
           (disk-device (partition-disk device))
           (+ (partition-offset device) lba)
           n-sectors
           buffer))

(defun write-disk-partition (device lba n-sectors buffer)
  (funcall (disk-write-fn (partition-disk device))
           (disk-device (partition-disk device))
           (+ (partition-offset device) lba)
           n-sectors
           buffer))

(defun flush-disk-partition (device buffer)
  (funcall (disk-flush-fn (partition-disk device))
           (disk-device (partition-disk device))))

(defun detect-disk-partitions ()
  (dolist (disk (all-disks))
    ;; Search for a GPT, then a PC MBR.
    (or (detect-gpt-partition-table disk)
        (detect-mbr-partition-table disk)
        (detect-iso9660-partition-table disk))))

(defun check-gpt-header-signature (page-addr)
  (loop
     for i from 0
     for m in '(#x45 #x46 #x49 #x20 #x50 #x41 #x52 #x54)
     when (not (eql (sys.int::memref-unsigned-byte-8 (+ page-addr i) 0) m))
     do (return nil)
     finally (return t)))

(defun process-gpt-partition-table-entry (disk offset i entry-size sector-buffer)
  (let ((sector-size (disk-sector-size disk)))
    (multiple-value-bind (sector-index byte-offset)
        (truncate (* i entry-size) sector-size)
      (unless (disk-read disk (+ offset sector-index) 1 sector-buffer)
        (panic "Unable to read GPT entry block " (+ offset sector-index) " on disk " disk))
      (let* ((base (+ sector-buffer byte-offset))
             (first-lba (memref-ub64/le (+ base #x20) 0))
             (last-lba (memref-ub64/le (+ base #x28) 0))
             (size (- (1+ last-lba) first-lba)))
        (when (loop
                 for i from 0 below 16
                 for system-id = (sys.int::memref-unsigned-byte-8 (+ base i) 0)
                            then (sys.int::memref-unsigned-byte-8 (+ base i) 0)
                 when (not (eql system-id 0))
                 do (return t)
                 finally (return nil))
          (debug-print-line "Detected partition " i " on disk " disk ". Start: " first-lba " size: " size)
          (register-disk (make-partition :disk disk
                                         :offset first-lba
                                         :id i
                                         :type system-id)
                         (disk-writable-p disk)
                         size
                         sector-size
                         (disk-max-transfer disk)
                         'read-disk-partition
                         'write-disk-partition
                         'flush-disk-partition
                         nil))))))

(defun detect-gpt-partition-table (disk)
  (let* ((sector-size (disk-sector-size disk))
         (pages-per-sector (ceiling sector-size +4k-page-size+))
         (found-table-p nil))
    (with-pages (page-addr pages-per-sector
                           :mandatory-p "DETECT-DISK disk buffer")
      ;; GPT is stored on LBA 1, protective MBR on LBA 0.
      (unless (disk-read disk 1 1 page-addr)
        (panic "Unable to read second block on disk " disk))
      (when (and (>= sector-size 512)
                 (check-gpt-header-signature page-addr))
        ;; Found, scan partitions.
        ;; FIXME: Deal with GPT tables that exceed the sector size.
        ;; FIXME: Little-endian reads.
        ;; FIXME: Verify the CRC & other fields.
        (setf found-table-p t)
        (debug-print-line "Detected GPT on disk " disk)
        (let ((offset (memref-ub64/le (+ page-addr #x48)))
              (num-entries (memref-ub32/le (+ page-addr #x50)))
              (entry-size (memref-ub32/le (+ page-addr #x54))))
          (dotimes (i num-entries)
            (process-gpt-partition-table-entry
             disk offset i entry-size page-addr))))
      found-table-p)))

(defun decode-ebr (page-addr)
  (if (and (eql (sys.int::memref-unsigned-byte-8 page-addr #x1FE) #x55)
           (eql (sys.int::memref-unsigned-byte-8 page-addr #x1FF) #xAA))
    (values (sys.int::memref-unsigned-byte-8 (+ page-addr #x1BE) 4)
            (memref-ub32/le (+ page-addr #x1BE 8))
            (memref-ub32/le (+ page-addr #x1BE 12))
            (memref-ub32/le (+ page-addr #x1CE 8)))
    (values 0 0 0 0)))

(defun detect-mbr-partition-table (disk)
  (let* ((sector-size (disk-sector-size disk))
         (pages-per-sector (ceiling sector-size +4k-page-size+))
         (found-table-p nil)
         (ebr-lba nil))
    (with-pages (page-addr pages-per-sector
                           :mandatory-p "DETECT-DISK disk buffer")
      (unless (disk-read disk 0 1 page-addr)
        (panic "Unable to read first block on disk " disk))
      (when (and (>= sector-size 512)
                 (eql (sys.int::memref-unsigned-byte-8 page-addr #x1FE) #x55)
                 (eql (sys.int::memref-unsigned-byte-8 page-addr #x1FF) #xAA))
        ;; Found, scan partitions.
        (setf found-table-p t)
        (debug-print-line "Detected MBR style parition table on disk " disk)
        (dotimes (i 4)
          (let ((part-type (sys.int::memref-unsigned-byte-8 (+ page-addr #x1BE (* 16 i) 4)))
                (start-lba (memref-ub32/le (+ page-addr #x1BE (* 16 i) 8)))
                (size (memref-ub32/le (+ page-addr #x1BE (* 16 i) 12))))
            (when (and (not (eql part-type 0))
                       (not (eql size 0)))
              (debug-print-line "Detected partition " i " on disk " disk ". Start: " start-lba " size: " size)
              (register-disk (make-partition :disk disk
                                             :offset start-lba
                                             :id i
                                             :type part-type)
                             (disk-writable-p disk)
                             size
                             sector-size
                             (disk-max-transfer disk)
                             'read-disk-partition
                             'write-disk-partition
                             'flush-disk-partition
                             nil)
              (when (or (eql part-type #x05) (eql part-type #x0F))
                (setf ebr-lba start-lba)))))
        ;; Handle extended partition documentation at:
        ;; https://thestarman.pcministry.com/asm/mbr/PartTables.htm
        (when ebr-lba
          (unless (disk-read disk ebr-lba 1 page-addr)
            ;; What do to with this error?
            )
          (loop with part-type and data-offset and size and ebr-offset
             with part-num = 4
             do (setf (values part-type data-offset size ebr-offset)
                      (decode-ebr page-addr))
             unless (or (eql data-offset 0)
                        (eql size 0))
             do
               (progn
                 (debug-print-line "Extended partition " part-num
                                   " on disk " disk
                                   ". Type: " part-type
                                   " start: " (+ ebr-lba data-offset)
                                   " size: " size)
                 (register-disk (make-partition :disk disk
                                                :offset (+ ebr-lba data-offset)
                                                :id part-num
                                                :type part-type)
                                (disk-writable-p disk)
                                size
                                sector-size
                                (disk-max-transfer disk)
                                'read-disk-partition
                                'write-disk-partition
                                'flush-disk-partition
                                nil)
                 (incf part-num))
             if (eql ebr-offset 0)
             do (return nil)
             else do
               (progn
                 (setf ebr-lba (+ ebr-lba ebr-offset))
                 (unless (disk-read disk ebr-lba 1 page-addr)
                   ;; what to do with this error?
                   )))))
      found-table-p)))

(defun find-iso9660-primary-volume-descriptor (disk buffer)
  (loop
     ;; The Volume Descriptor Set starts on sector 16/offset 32kb.
     ;; Limit to searching the first 128 entries.
     for sector from #x10 below (+ #x10 128)
     do
       (unless (disk-read disk sector 1 buffer)
         (return nil))
       ;; Check identifier 'CD001' and version
       (unless (and (eql (sys.int::memref-unsigned-byte-8 buffer 1) #x43)
                       (eql (sys.int::memref-unsigned-byte-8 buffer 2) #x44)
                       (eql (sys.int::memref-unsigned-byte-8 buffer 3) #x30)
                       (eql (sys.int::memref-unsigned-byte-8 buffer 4) #x30)
                       (eql (sys.int::memref-unsigned-byte-8 buffer 5) #x31)
                       (eql (sys.int::memref-unsigned-byte-8 buffer 6) #x01))
         (return nil))
       ;; Check type.
       (case (sys.int::memref-unsigned-byte-8 buffer 0)
         (#x01 ; Primary Volume Descriptor.
          (return sector))
         (#xFF ; Volume Descriptor Set Terminator.
          (return nil)))))

(defun detect-iso9660-partition-table (disk)
  (let* ((sector-size (disk-sector-size disk))
         (pages-per-sector (ceiling sector-size +4k-page-size+))
         (found-table-p nil))
    (unless (eql sector-size 2048)
      (return-from detect-iso9660-partition-table nil))
    (with-pages (page-addr pages-per-sector
                           :mandatory-p "DETECT-DISK disk buffer")
      ;; Search for a primary volume descriptor.
      (let ((primary-volume (find-iso9660-primary-volume-descriptor disk page-addr)))
        (unless primary-volume
          (return-from detect-iso9660-partition-table nil))
        (debug-print-line "Detected ISO9660 primary volume descriptor at sector " primary-volume " on disk " disk)
        ;; Treat every file in the root directory as a partition.
        (let ((root-extent (memref-ub32/le (+ page-addr 156 2)))
              (root-length (memref-ub32/le (+ page-addr 156 10)))
              (n-entries 0))
          (debug-print-line "Root directory at " root-extent "/" root-length)
          (loop
             for sector from 0 below (ceiling root-length 2048)
             do
               (unless (disk-read disk (+ root-extent sector) 1 page-addr)
                 (return nil))
               (do ((offset 0))
                   ((>= offset 2048))
                 (let ((rec-len (sys.int::memref-unsigned-byte-8 (+ page-addr offset 0)))
                       (extent (memref-ub32/le (+ page-addr offset 2)))
                       (length (memref-ub32/le (+ page-addr offset 10)))
                       (flags (sys.int::memref-unsigned-byte-8 (+ page-addr offset 25))))
                   (when (zerop rec-len)
                     ;; Reached last record.
                     (return))
                   (debug-print-line "Root entry at " extent "/" length " flags: " flags)
                   (when (eql (logand flags #b11101111) #b00000000) ; Ignore the protection bit.
                     ;; Valid file.
                     (register-disk (make-partition :disk disk
                                                    :offset extent
                                                    :id (incf n-entries)
                                                    :type nil)
                                    nil
                                    (ceiling length 2048)
                                    2048
                                    (disk-max-transfer disk)
                                    'read-disk-partition
                                    'write-disk-partition
                                    'flush-disk-partition
                                    nil))
                   (incf offset rec-len)))))))))
