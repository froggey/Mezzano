;;;; Copyright (c) 2017-2018 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.
;;;; For now support reading fat32 FS and some write operations.

(defpackage :mezzano.fat-file-system
  (:use :cl :mezzano.file-system :mezzano.file-system-cache :mezzano.disk)
  (:local-nicknames (:sys.int :mezzano.internals))
  (:export))

(in-package :mezzano.fat-file-system)

(defconstant +bootable-partition-signature+ #xAA55)

(defclass fat-base ()
  ((%boot-jump :initarg :boot-jump :accessor fat-%boot-jump)
   (%oem-name :initarg :oem-name :accessor fat-%oem-name :type string)
   (%bytes-per-sector :initarg :bytes-per-sector :accessor fat-%bytes-per-sector :type (unsigned-byte 16))
   (%sectors-per-cluster :initarg :sectors-per-cluster :accessor fat-%sectors-per-cluster :type (unsigned-byte 8))
   (%n-reserved-sectors :initarg :n-reserved-sectors :accessor fat-%n-reserved-sectors :type (unsigned-byte 16))
   (%n-fats :initarg :n-fats :accessor fat-%n-fats :type (unsigned-byte 8))
   (%n-sectors16 :initarg :n-sectors16 :accessor fat-%n-sectors16 :type (unsigned-byte 16))
   (%media-type :initarg :media-type :accessor fat-%media-type :type (unsigned-byte 8))
   (%sectors-per-fat :initarg :sectors-per-fat :accessor fat-%sectors-per-fat :type (unsigned-byte 16))
   (%sectors-per-track :initarg :sectors-per-track :accessor fat-%sectors-per-track :type (unsigned-byte 16))
   (%n-heads/sides :initarg :n-heads/sides :accessor fat-%n-heads/sides :type (unsigned-byte 16))
   (%n-hidden-sectors :initarg :n-hidden-sectors :accessor fat-%n-hidden-sectors :type (unsigned-byte 32))
   (%n-sectors32 :initarg :n-sectors32 :accessor fat-%n-sectors32 :type (unsigned-byte 32))
   (%drive-n :initarg :drive-n :accessor fat-%drive-n)
   (%signature :initarg :signature :accessor fat-%signature :type (unsigned-byte 8))
   (%volume-id :initarg :volume-id :accessor fat-%volume-id :type (unsigned-byte 32))
   (%volume-label :initarg :volume-label :accessor fat-%volume-label :type string)
   (%fat-type-label :initarg :fat-type-label :accessor fat-%fat-type-label :type string)
   (%boot-code :initarg :boot-code :accessor fat-%boot-code)
   (%n-clusters :initarg :max-cluser :accessor fat-%n-clusters)
   (%fat-dirty-bits :initarg :fat-dirty-bits :accessor fat-%fat-dirty-bits)))

(defun check-boot-jump (boot-jump)
  (let ((n0 (aref boot-jump 0)))
    (if (or (and (= n0 #xEB)
                 (= (aref boot-jump 2) #x90))
            (= n0 #xE9))
        boot-jump
        (error "Bad boot-jump : ~a .
Valid forms are : #(#xEB x #x90) and #(#xE9 x x).
X is for some 1 byte number." boot-jump))))

(let ((valid-bytes-per-sector '(512 1024 2048 4096)))
  (defun check-bytes-per-sector (bytes-per-sector)
    "Ensure that bytes-per-sector is valid."
    (if (member bytes-per-sector valid-bytes-per-sector)
        bytes-per-sector
        (error "Bad bytes-per-sector : ~a .
Valid bytes-per-sector are 512, 1024, 2048 and 4096" bytes-per-sector))))

(let ((valid-sectors-per-cluster '(1 2 4 8 16 32 64 128)))
  (defun check-sectors-per-cluster (sectors-per-cluster bytes-per-sector)
    (cond ((not (member sectors-per-cluster valid-sectors-per-cluster))
           (error "Bad sectors-per-cluster : ~a .
Valid bytes-per-sector are 1,2,4,8,16,32,64,128" sectors-per-cluster))
          ((> (* sectors-per-cluster bytes-per-sector)
              65536)
           (error "Error sectors-per-cluster * bytes-per-sector > 64KiB .
sectors-per-cluster= ~a bytes-per-sector= ~a" sectors-per-cluster bytes-per-sector))
          (t sectors-per-cluster))))

(let ((valid-media-type '(#xF0 #xF8 #xF9 #xFA #xFB #xFC #xFD #xFE #xFF)))
  (defun check-media-type (media-type)
    (if (member media-type valid-media-type)
        media-type
        (error "Bad media-type : ~a .
Valid media-type ara #xF0 #xF8 #xF9 #xFA #xFB #xFC #xFD #xFE #xFF" media-type))))

(defun read-fat-base (ffs sector)
  (let ((bytes-per-sector (sys.int::ub16ref/le sector 11))
        (n-fats (aref sector 16)))
    (assert (>= n-fats 1))
    (setf (fat-%boot-jump ffs) (check-boot-jump (subseq sector 0 3))
          (fat-%oem-name ffs) (map 'string #'code-char (subseq sector 3 11))
          (fat-%bytes-per-sector ffs) (check-bytes-per-sector bytes-per-sector)
          (fat-%sectors-per-cluster ffs) (check-sectors-per-cluster (aref sector 13) bytes-per-sector)
          (fat-%n-reserved-sectors ffs) (sys.int::ub16ref/le sector 14)
          (fat-%n-fats ffs) n-fats
          (fat-%n-sectors16 ffs) (sys.int::ub16ref/le sector 19)
          (fat-%media-type ffs) (check-media-type (aref sector 21))
          (fat-%sectors-per-track ffs) (sys.int::ub16ref/le sector 24)
          (fat-%n-heads/sides ffs) (sys.int::ub16ref/le sector 26)
          (fat-%n-hidden-sectors ffs) (sys.int::ub32ref/le sector 28)
          (fat-%n-sectors32 ffs) (sys.int::ub32ref/le sector 32)
          (fat-%n-clusters ffs) (+ (compute-cluster-count sector) 2))))

(defclass fat12 (fat-base)
  ((%n-root-entry :initarg :n-root-entry :accessor fat-%n-root-entry :type (unsigned-byte 16))))

(defun check-bps (bps)
  (unless (= bps +bootable-partition-signature+)
    (error "Bad bps : ~a .
Valid bps are ~a" bps +bootable-partition-signature+)))

(defun read-fat12/16-structure (sector ffs-type)
  (let ((ffs (make-instance ffs-type
                            :sectors-per-fat (sys.int::ub16ref/le sector 22)
                            :drive-n (aref sector 36) ; Operating system specific
                            :boot-code (subseq sector 62 510))))
    (setf (fat-%n-root-entry ffs) (sys.int::ub16ref/le sector 17)
          (fat-%signature ffs) (aref sector 38)
          (fat-%volume-id ffs) (sys.int::ub32ref/le sector 39)
          (fat-%volume-label ffs) (map 'string #'code-char (subseq sector 43 54))
          (fat-%fat-type-label ffs) (map 'string #'code-char (subseq sector 54 60)))
    (read-fat-base ffs sector)
    (assert (or (not (zerop (fat-%n-sectors16 ffs)))
                (not (zerop (fat-%n-sectors32 ffs)))))
    (check-bps (sys.int::ub16ref/le sector 510))
    ffs))

(defclass fat16 (fat12)
  ())

(defclass fat32 (fat-base)
  ((%flags :initarg :flags :accessor fat32-%flags)
   (%fat-version :initarg :fat-version :accessor fat32-%fat-version)
   (%root-cluster :initarg :root-cluster :accessor fat32-%root-cluster :type (unsigned-byte 32))
   (%fat-info :initarg :fat-info :accessor fat32-%fat-info :type (unsigned-byte 16))
   (%backup-boot-sector :initarg :backup-boot-sector :accessor fat32-%backup-boot-sector :type (unsigned-byte 16))))

(defun check-fat-type-label32 (fat-type-label)
  (if (string= "FAT32   " fat-type-label)
      fat-type-label
      (error "Bad fat-type-label : ~a .
Valid media-type ara 'FAT32   ' " fat-type-label)))

(defun read-fat32-structure (sector)
  (let* ((ffs (make-instance 'fat32
                             :sectors-per-fat (sys.int::ub32ref/le sector 36)
                             :flags (sys.int::ub16ref/le sector 40)
                             ;; high byte is major revision number and low byte is minor revision number
                             :fat-version (sys.int::ub16ref/le sector 42)
                             :root-cluster (sys.int::ub32ref/le sector 44)
                             :fat-info (sys.int::ub16ref/le sector 48)
                             :backup-boot-sector (sys.int::ub16ref/le sector 50)
                             :drive-n (aref sector 64) ; Operating system specific
                             :boot-code (subseq sector 90 510))))
    (when (/= (sys.int::ub16ref/le sector 22) 0)
      (error "Bad FATsz16 - got ~D should be 0" (sys.int::ub16ref/le sector 22)))
    (setf (fat-%signature ffs) (aref sector 66)
          (fat-%volume-id ffs) (sys.int::ub32ref/le sector 67)
          (fat-%volume-label ffs) (map 'string #'code-char (subseq sector 71 82))
          (fat-%fat-type-label ffs) (check-fat-type-label32 (map 'string #'code-char
                                                                 (subseq sector 82 90))))
    (read-fat-base ffs sector)
    (assert (and (zerop (fat-%n-sectors16 ffs))
                 (not (zerop (fat-%n-sectors32 ffs)))))
    (check-bps (sys.int::ub16ref/le sector 510))
    ffs))

;; (defstruct fs-info
;;   (lead-signature nil :type (unsigned-byte 32))
;;   (reserved-0 nil) ; 4 480
;;   (structure-signature nil :type (unsigned-byte 32))
;;   (last-free-cluster nil :type (unsigned-byte 32))
;;   (next-free-cluster nil :type (unsigned-byte 32))
;;   (reserved-1 nil) ; 496 12
;;   (trail-signature nil :type (unsigned-byte 32)))

;; (defconstant +lead-signature+ #x41615252)
;; (defconstant +structure-signature+ #x61417272)
;; (defconstant +trail-signature+ #xAA550000)

;; (defun check-lead-signature (lead-signature)
;;   (if (= lead-signature +lead-signature+)
;;       lead-signature
;;       (error "Bad lead-signature : ~a .
;; Valid lead-signature is ~a" lead-signature +lead-signature+)))

;; (defun check-structure-signature (structure-signature)
;;   (if (= structure-signature +structure-signature+)
;;       structure-signature
;;       (error "Bad structure-signature : ~a .
;; Valid structure-signature is ~a" structure-signature +structure-signature+)))

;; (defun check-trail-signature (trail-signature)
;;   (if (= trail-signature +trail-signature+)
;;       trail-signature
;;       (error "Bad trail-signature : ~a .
;; Valid trail-signature is ~a" trail-signature +trail-signature+)))

;; (defun read-fat32-info-structure (disk fat)
;;   (let* ((sector (block-device-read-sector disk (fat-%fat-info fat) 1))
;;          (lead-signature (sys.int::ub32ref/le sector 0))
;;          (structure-signature (sys.int::ub32ref/le sector 484))
;;          (trail-signature (sys.int::ub32ref/le sector 508)))
;;     (check-lead-signature lead-signature)
;;     (check-structure-signature structure-signature)
;;     (check-trail-signature trail-signature)
;;     (make-fs-info
;;      :lead-signature lead-signature
;;      :reserved-0 nil
;;      :structure-signature structure-signature
;;      :last-free-cluster (sys.int::ub32ref/le sector 488)
;;      :next-free-cluster (sys.int::ub32ref/le sector 492)
;;      :reserved-1 nil
;;      :trail-signature trail-signature)))

;; (defun write-fat32-info-structure (disk fat fat32-info)
;;   (let ((sector (make-array (fat-%bytes-per-sector fat)
;;                             :area :wired :element-type '(unsigned-byte 8))))
;;     (setf (sys.int::ub32ref/le sector 0) (fs-info-lead-signature fat32-info)
;;           (sys.int::ub32ref/le sector 484) (fs-info-structure-signature fat32-info)
;;           (sys.int::ub32ref/le sector 488) (fs-info-last-free-cluster fat32-info)
;;           (sys.int::ub32ref/le sector 492) (fs-info-next-free-cluster fat32-info)
;;           (sys.int::ub32ref/le sector 508) (fs-info-trail-signature fat32-info))
;;     (block-device-write-sector disk (fat-%fat-info fat) sector 1)))

(defmethod read-fat (disk (fat12 fat12) &optional fat-array)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat12)
        :with fat-sectors := (ceiling (* 3 (fat-%n-clusters fat12))
                                      (* 2 (fat-%bytes-per-sector fat12)))
        :with file-allocation-table := (block-device-read-sector disk fat-offset fat-sectors)
        :with fat := (if fat-array
                         fat-array
                         (make-array (floor (* 2 (length file-allocation-table)) 3)))
        :for offset :from 0 :by 3 :below (- (length file-allocation-table) 2)
        :for i :from 0 :by 2
        :for byte-24 = (logior (aref file-allocation-table offset)
                               (ash (aref file-allocation-table (1+ offset)) 8)
                               (ash (aref file-allocation-table (+ 2 offset)) 16))
        :for cluster0 := (logand byte-24 #xFFF)
        :for cluster1 := (ldb (byte 12 12) byte-24)
        :do (setf (aref fat i) cluster0
                  (aref fat (1+ i)) cluster1)
        :finally
         (setf (fat-%fat-dirty-bits fat12) (make-array 1
                                                       :element-type '(unsigned-byte 32)
                                                       :initial-element 0))
         (return fat)))

(defun write-fat-sectors (disk flags ffs fat-offset fat-sector buf)
  ;; flags bit 7 = 0 means write all FATs
  ;; flags bit 7 = 1 means write only the FAT specified by bits 0-3
  ;; flags are really only defined for FAT32, so FAT12 and FAT16 always pass in 0
  (if (logbitp 7 flags)
      ;; write just one FAT
      (block-device-write disk
                          (+ fat-offset
                             fat-sector
                             (* (logand flags #x0F) (fat-%sectors-per-fat ffs)))
                          1
                          buf)
      ;; write all FATs
      (loop
         repeat (fat-%n-fats ffs)
         for sector = (+ fat-offset fat-sector) then
           (+ sector (fat-%sectors-per-fat ffs))
         do
           (block-device-write disk sector 1 buf))))

(defmethod write-fat (disk (fat12 fat12) fat)
  (let* ((dirty-bits (fat-%fat-dirty-bits fat12))
         (sector-size (block-device-sector-size disk))
         (fat-offset (fat-%n-reserved-sectors fat12))
         ;; +3 to allow copy of entries to run off the end of the buffer a little
         (buf (make-array (+ sector-size 3) :element-type '(unsigned-byte 8))))
    ;; (length dirty-bits) = 1, so no word index loop here
    (let ((bits (aref dirty-bits 0)))
      ;; 1.5 bytes * 4087 entries = 6130.5 bytes / 512 bytes per sector = 11.97 sectors
      (dotimes (bit-idx 12)
        (when (logbitp bit-idx bits)
          (let* ((sector bit-idx)
                 (byte-offset (* sector sector-size)))
            (multiple-value-bind (word-offset/2 rem) (truncate byte-offset 3)
              (case rem
                (0 ;; sector starts at beginning of fat entry
                 (do ((fat-idx (ash word-offset/2 1) (+ fat-idx 2))
                      (buf-idx 0 (+ buf-idx 3)))
                     ((> buf-idx sector-size))
                   (let ((byte-24 (logior (aref fat fat-idx)
                                          (ash (aref fat (1+ fat-idx)) 12))))
                     ;; byte-24 is pair of 12-bit cluster numbers - write as little endian
                     (setf (aref buf buf-idx)       (ldb (byte 8  0) byte-24)
                           (aref buf (+ buf-idx 1)) (ldb (byte 8  8) byte-24)
                           (aref buf (+ buf-idx 2)) (ldb (byte 8 16) byte-24)))))
                (1 ;; sector starts at upper 4-bits of 1st entry
                 (let* ((fat-idx (ash word-offset/2 1))
                        (byte-24 (logior (aref fat fat-idx)
                                         (ash (aref fat (1+ fat-idx)) 12))))
                   (setf (aref buf 0) (ldb (byte 8  8) byte-24)
                         (aref buf 1) (ldb (byte 8 16) byte-24)))
                 (do ((fat-idx (+ (ash word-offset/2 1) 2) (+ fat-idx 2))
                      (buf-idx 2 (+ buf-idx 3)))
                     ((> buf-idx sector-size))
                   (let ((byte-24 (logior (aref fat fat-idx)
                                          (ash (aref fat (1+ fat-idx)) 12))))
                     ;; byte-24 is pair of 12-bit cluster numbers - write as little endian
                     (setf (aref buf buf-idx)       (ldb (byte 8  0) byte-24)
                           (aref buf (+ buf-idx 1)) (ldb (byte 8  8) byte-24)
                           (aref buf (+ buf-idx 2)) (ldb (byte 8 16) byte-24)))))
                (2 ;; sector starts at upper 8-bits of 2nd entry
                 (let* ((fat-idx (ash word-offset/2 1))
                        (byte-24 (logior (aref fat fat-idx)
                                         (ash (aref fat (1+ fat-idx)) 12))))
                   (setf (aref buf 0) (ldb (byte 8 16) byte-24)))
                 (do ((fat-idx (+ (ash word-offset/2 1) 2) (+ fat-idx 2))
                      (buf-idx 1 (+ buf-idx 3)))
                     ((> buf-idx sector-size))
                   (let ((byte-24 (logior (aref fat fat-idx)
                                          (ash (aref fat (1+ fat-idx)) 12))))
                     ;; byte-24 is pair of 12-bit cluster numbers - write as little endian
                     (setf (aref buf buf-idx)       (ldb (byte 8  0) byte-24)
                           (aref buf (+ buf-idx 1)) (ldb (byte 8  8) byte-24)
                           (aref buf (+ buf-idx 2)) (ldb (byte 8 16) byte-24)))))))
            (write-fat-sectors disk 0 fat12 fat-offset sector buf)))))
    (setf (aref dirty-bits 0) 0)))

(defmethod read-fat (disk (fat16 fat16) &optional fat-array)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat16)
        :with fat-sectors := (ceiling (* 2 (fat-%n-clusters fat16)) (fat-%bytes-per-sector fat16))
        :with file-allocation-table := (block-device-read-sector disk fat-offset fat-sectors)
        :with fat := (if fat-array
                         fat-array
                         (make-array (list (ash (length file-allocation-table) -1))))
        :for offset :from 0 :by 2 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (sys.int::ub16ref/le file-allocation-table offset)
        :do (setf (aref fat i) cluster-n)
        :finally
         (setf (fat-%fat-dirty-bits fat16) (make-array (ceiling (fat-%sectors-per-fat fat16) 32)
                                                       :element-type '(unsigned-byte 32)
                                                       :initial-element 0))
         (return fat)))

(defmethod write-fat (disk (fat16 fat16) fat)
  (let* ((dirty-bits (fat-%fat-dirty-bits fat16))
         (sector-size (block-device-sector-size disk))
         (clusters-per-sector (ash sector-size -1))
         (fat-offset (fat-%n-reserved-sectors fat16))
         (buf (make-array sector-size :element-type '(unsigned-byte 8))))
    (dotimes (word-idx (length dirty-bits))
      (when (/= (aref dirty-bits word-idx) 0)
        (let ((bits (aref dirty-bits word-idx)))
          (dotimes (bit-idx 32)
            (when (logbitp bit-idx bits)
              (let* ((sector (+ (* word-idx 32) bit-idx))
                     (word-offset (* sector clusters-per-sector)))
                (dotimes (i clusters-per-sector)
                  (setf (sys.int::ub16ref/le buf (* 2 i)) (aref fat (+ word-offset i))))
                (write-fat-sectors disk 0 fat16 fat-offset sector buf)))))
        (setf (aref dirty-bits word-idx) 0)))))

(defmethod read-fat (disk (fat32 fat32) &optional fat-array)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat32)
        :with fat-sectors := (ceiling (* 4 (fat-%n-clusters fat32)) (fat-%bytes-per-sector fat32))
        :with file-allocation-table := (block-device-read-sector disk fat-offset fat-sectors)
        :with fat := (if fat-array
                         fat-array
                         (make-array (list (ash (length file-allocation-table) -2))))
        :for offset :from 0 :by 4 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (sys.int::ub32ref/le file-allocation-table offset)
        :do (setf (aref fat i) cluster-n)
        :finally
          (setf (fat-%fat-dirty-bits fat32) (make-array (ceiling (fat-%sectors-per-fat fat32) 32)
                                                        :element-type '(unsigned-byte 32)
                                                        :initial-element 0))
          (return fat)))

(defmethod write-fat (disk (fat32 fat32) fat)
  (let* ((dirty-bits (fat-%fat-dirty-bits fat32))
         (sector-size (block-device-sector-size disk))
         (clusters-per-sector (ash sector-size -2))
         (fat-offset (fat-%n-reserved-sectors fat32))
         (buf (make-array sector-size :element-type '(unsigned-byte 8))))
    (dotimes (word-idx (length dirty-bits))
      (when (/= (aref dirty-bits word-idx) 0)
        (let ((bits (aref dirty-bits word-idx)))
          (dotimes (bit-idx 32)
            (when (logbitp bit-idx bits)
              (let* ((sector (+ (* word-idx 32) bit-idx))
                     (word-offset (* sector clusters-per-sector)))
                (dotimes (i clusters-per-sector)
                  (setf (sys.int::ub32ref/le buf (* 4 i)) (aref fat (+ word-offset i))))
                (write-fat-sectors disk (fat32-%flags fat32) fat32 fat-offset sector buf)))))
        (setf (aref dirty-bits word-idx) 0)))))

(defun fat-value (fat idx)
  ;; For FAT32, only 28-bits are valid but it's safe to use the
  ;; mask for FAT12 and FAT16 - so don't need to use a method
  (logand (aref fat idx) #x0FFFFFFF))

(defmethod (setf fat-value) (value (fat12 fat12) fat idx)
  (setf (aref fat idx) value)
  ;; set dirty bit
  (let* ((dirty-bits (fat-%fat-dirty-bits fat12))
         ;; faster version of (truncate (* idx 1.5))
         (byte-offset (+ idx (ash idx -1)))
         (sector1 (floor byte-offset (fat-%bytes-per-sector fat12)))
         (sector2 (floor (1+ byte-offset) (fat-%bytes-per-sector fat12))))
    (multiple-value-bind (word-idx bit-idx) (floor sector1 32)
      (setf (aref dirty-bits word-idx)
            (dpb 1 (byte 1 bit-idx) (aref dirty-bits word-idx))))
    (when (/= sector1 sector2)
      (multiple-value-bind (word-idx bit-idx) (floor sector2 32)
        (setf (aref dirty-bits word-idx)
              (dpb 1 (byte 1 bit-idx) (aref dirty-bits word-idx)))))))

(defmethod (setf fat-value) (value (fat16 fat16) fat idx)
  (setf (aref fat idx) value)
  ;; set dirty bit
  (let* ((dirty-bits (fat-%fat-dirty-bits fat16))
         (byte-offset (ash idx 1))
         (sector (floor byte-offset (fat-%bytes-per-sector fat16))))
    (multiple-value-bind (word-idx bit-idx) (floor sector 32)
      (setf (aref dirty-bits word-idx)
            (dpb 1 (byte 1 bit-idx) (aref dirty-bits word-idx))))))

(defmethod (setf fat-value) (value (fat32 fat32) fat idx)
  (setf (aref fat idx) (logior (logand (aref fat idx) #xF0000000)
                               (logand value #x0FFFFFFF)))
  ;; set dirty bit
  (let* ((dirty-bits (fat-%fat-dirty-bits fat32))
         (byte-offset (ash idx 2))
         (sector (floor byte-offset (fat-%bytes-per-sector fat32))))
    (multiple-value-bind (word-idx bit-idx) (floor sector 32)
      (setf (aref dirty-bits word-idx)
            (dpb 1 (byte 1 bit-idx) (aref dirty-bits word-idx))))))

(defmethod root-dir-sectors ((fat12 fat12))
  (floor (/ (+ (ash (fat-%n-root-entry fat12) 5)
               (1- (fat-%bytes-per-sector fat12)))
            (fat-%bytes-per-sector fat12))))

(defmethod root-dir-sectors ((fat32 fat32)) 0)

(defmethod last-cluster-value ((fat12 fat12))
  #xFF8)

(defmethod last-cluster-value ((fat16 fat16))
  #xFFF8)

(defmethod last-cluster-value ((fat32 fat32))
  #xFFFFFF8)

(defun first-data-sector (ffs)
  (+ (fat-%n-reserved-sectors ffs)
     (* (fat-%n-fats ffs)
        (fat-%sectors-per-fat ffs))
     (root-dir-sectors ffs)))

(defun first-sector-of-cluster (ffs cluster-n)
  (+ (* (- cluster-n 2)
        (fat-%sectors-per-cluster ffs))
     (first-data-sector ffs)))

(defun bytes-per-cluster (ffs)
  (* (fat-%sectors-per-cluster ffs)
     (fat-%bytes-per-sector ffs)))

;; FAT[0] = Media Type, FAT[1] = EOC
(defun next-free-cluster (ffs fat &optional (start 2))
  (loop :for offset :from start :below (fat-%n-clusters ffs)
        :for cluster-n := (fat-value fat offset)
        :when (zerop cluster-n)
        :return offset))

(defun get-fat-time ()
  "Return time and date in fat format"
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (values (logior (ash second -1)
                    (ash minute 5)
                    (ash hour 11))
            (logior date
                    (ash month 5)
                    (ash (- year 1980) 9)))))

;;; bit offsets
(defconstant +attribute-read-only+ 0)
(defconstant +attribute-hidden+ 1)
(defconstant +attribute-system+ 2)
(defconstant +attribute-volume-id+ 3)
(defconstant +attribute-directory+ 4)
(defconstant +attribute-archive+ 5)

(defun file-p (directory offset)
  (= (ldb (byte 1 +attribute-archive+)
          (read-attributes directory offset))
     1))

(defun directory-p (directory offset)
  (= (ldb (byte 1 +attribute-directory+)
          (read-attributes directory offset))
     1))

(defun read-file (ffs disk start-cluster fat)
  (let* ((spc (fat-%sectors-per-cluster ffs))
         (n-clusters (do ((cluster-n start-cluster (fat-value fat cluster-n))
                          (cluster-count 0 (1+ cluster-count)))
                         ((>= cluster-n (last-cluster-value ffs)) cluster-count)))
         (sector-size (block-device-sector-size disk))
         (result (make-array (* sector-size spc n-clusters) :element-type '(unsigned-byte 8))))
    (do ((cluster-n start-cluster (fat-value fat cluster-n))
         (n-cluster 0 (1+ n-cluster)))
        ((>= cluster-n (last-cluster-value ffs)) result)
      (block-device-read disk
                         (first-sector-of-cluster ffs cluster-n)
                         spc
                         result
                         :offset (* n-cluster spc sector-size)))))

(defmethod read-root-directory (disk (fat12 fat12) fat)
  (declare (ignore fat))
  (let* ((root-dir-start (+ (fat-%n-reserved-sectors fat12)
                            (* (fat-%n-fats fat12)
                               (fat-%sectors-per-fat fat12))))
         (n-sectors (root-dir-sectors fat12))
         (result (make-array (* n-sectors (fat-%bytes-per-sector fat12))
                             :element-type '(unsigned-byte 8))))
    (block-device-read disk root-dir-start n-sectors result)
    result))

(defmethod read-root-directory (disk (fat32 fat32) fat)
  (read-file fat32 disk (fat32-%root-cluster fat32) fat))

(defun write-file (ffs disk start-cluster fat array file-length)
  (let* ((spc (fat-%sectors-per-cluster ffs))
         (bytes-per-cluster (* spc (block-device-sector-size disk))))
    (do ((cluster-n start-cluster (fat-value fat cluster-n))
         (last-cluster 0)
         (byte-offset 0 (+ byte-offset bytes-per-cluster)))
        ((or (>= byte-offset file-length)
             (>= cluster-n (last-cluster-value ffs)))
         (when (> file-length byte-offset)
           (do ((cluster-n (next-free-cluster ffs fat 3)
                           (next-free-cluster ffs fat (1+ cluster-n)))
                (byte-offset byte-offset (+ byte-offset bytes-per-cluster)))
               ((>= byte-offset file-length)
                (setf (fat-value ffs fat last-cluster) (last-cluster-value ffs))
                (write-fat disk ffs fat))
             (block-device-write disk
                                 (first-sector-of-cluster ffs cluster-n)
                                 spc
                                 array
                                 :offset byte-offset)
             (setf (fat-value ffs fat last-cluster) cluster-n)
             (setf last-cluster cluster-n)))
         T)
      (setf last-cluster cluster-n)
      (block-device-write disk
                          (first-sector-of-cluster ffs cluster-n)
                          spc
                          array
                          :offset byte-offset))))

(defun write-directory (ffs disk start-cluster fat array)
  (cond (start-cluster
         (write-file ffs disk start-cluster fat array (length array)))
        ((typep ffs 'fat32)
         (write-file ffs disk (fat32-%root-cluster ffs) fat array (length array)))
        (T
         (let* ((root-dir-start (+ (fat-%n-reserved-sectors ffs)
                                   (* (fat-%n-fats ffs)
                                      (fat-%sectors-per-fat ffs))))
                (n-sectors (root-dir-sectors ffs)))
           (block-device-write disk root-dir-start n-sectors array)))))

(defun read-attributes (directory offset)
  (aref directory (+ offset 11)))

(defun (setf read-attributes) (attributes directory offset)
  (setf (aref directory (+ offset 11)) attributes))

(defun read-reserved (directory offset)
  (aref directory (+ offset 12)))

(defun (setf read-reserved) (reserved directory offset)
  (setf (aref directory (+ offset 12)) reserved))

(defun read-creation-time-tenth (directory offset)
  (aref directory (+ offset 13)))

(defun (setf read-creation-time-tenth) (tenth directory offset)
  (setf (aref directory (+ offset 13)) tenth))

(defun read-creation-time (directory offset)
  (sys.int::ub16ref/le directory (+ offset 14)))

(defun (setf read-creation-time) (time directory offset)
  (setf (sys.int::ub16ref/le directory (+ offset 14)) time))

(defun read-creation-date (directory offset)
  (sys.int::ub16ref/le directory (+ offset 16)))

(defun (setf read-creation-date) (date directory offset)
  (setf (sys.int::ub16ref/le directory (+ offset 16)) date))

(defun read-last-access-date (directory offset)
  (sys.int::ub16ref/le directory (+ offset 18)))

(defun (setf read-last-access-date) (date directory offset)
  (setf (sys.int::ub16ref/le directory (+ offset 18)) date))

(defun read-write-time (directory offset)
  (sys.int::ub16ref/le directory (+ offset 22)))

(defun (setf read-write-time) (time directory offset)
  (setf (sys.int::ub16ref/le directory (+ offset 22)) time))

(defun read-write-date (directory offset)
  (sys.int::ub16ref/le directory (+ offset 24)))

(defun (setf read-write-date) (date directory offset)
  (setf (sys.int::ub16ref/le directory (+ offset 24)) date))

(defun read-first-cluster (directory offset)
  (logior (ash (sys.int::ub16ref/le directory (+ 20 offset)) 16)
          (ash (sys.int::ub16ref/le directory (+ 26 offset)) 0)))

(defun (setf read-first-cluster) (cluster-n directory offset)
  (setf (sys.int::ub16ref/le directory (+ offset 20)) (ldb (byte 16 16) cluster-n)
        (sys.int::ub16ref/le directory (+ offset 26)) (ldb (byte 16 0) cluster-n)))

(defun read-file-length (directory offset)
  (sys.int::ub32ref/le directory (+ 28 offset)))

(defun (setf read-file-length) (size directory offset)
  (setf (sys.int::ub32ref/le directory (+ offset 28)) size))

(defun checksum (array offset)
  "Return checksum of sort name"
  (loop :with checksum := 0
        :for i :from offset :to (+ 10 offset)
        :for octet := (aref array i)
        :do (setf checksum (ldb (byte 8 0)
                                (+ (ash checksum -1)
                                   (ash (logand checksum 1) 7) ; rotate
                                   octet))) ; add next name byte
        :finally (return checksum)))

(defun fat-to-universal-time (tenths time date)
  (encode-universal-time (+ (ash (ldb (byte 5 0) time) 1)
                            (floor tenths 100))
                         (ldb (byte 6 5) time)
                         (ldb (byte 5 11) time)
                         (ldb (byte 5 0) date)
                         (ldb (byte 4 5) date)
                         (+ 1980 (ldb (byte 7 9) date))))

(defun encode-fat-date (date month year)
  (logior date (ash month 5) (ash (- year 1980) 9)))

(defun encode-fat-time (second minute hour)
  (logior (round second 2) (ash minute 5) (ash hour 11)))

(defun next-file (directory offset)
  "Return offset of next file/directory"
  (do ((i offset (+ 32 i)))
      ((<= (length directory) i) nil)
    (let ((first-byte (aref directory i)))
      (when (zerop first-byte)
        (return nil))
      (unless (= #xE5 first-byte)
        (unless (= #x0F (aref directory (+ 11 i)))
          (return i))))))

(defmacro do-files ((var) directory finally &body body)
  `(do ((,var (next-file ,directory 0) (next-file ,directory (+ 32 ,var))))
       ((null ,var)
        ,finally)
     ,@body))

(defun read-short-name (directory offset)
  ;; Handle 8.3 names
  ;; DIR_NTRes (aref directory 12) specifies if all of the name chars
  ;; or all of the extensiton chars are lower case ascii.
  (let ((name (make-string 12 :initial-element #\Space))
        (name-lower-p (logbitp 3 (aref directory (+ offset 12))))
        (ext-lower-p (logbitp 4 (aref directory (+ offset 12))))
        (first (aref directory offset))
        (idx 1))
    ;; First character is special
    (setf (elt name 0) (cond ((= first #x05) (code-char #xE5))
                             (name-lower-p (char-downcase (code-char first)))
                             (T (code-char first))))
    ;; Copy chars for 8 part
    (loop
       for i from (1+ offset) to (+ 7 offset)
       for char = (code-char (aref directory i))
       when (eql char #\Space) do
         (return)
       do
         (setf (elt name idx) (if name-lower-p (char-downcase char) char))
         (incf idx))
    ;; Copy chars for 3 part, if there is any
    (when (/= (aref directory (+ 8 offset)) (char-code #\Space))
      (setf (elt name idx) #\.)
      (incf idx)
      (loop
         for i from (+ 8 offset) to (+ 10 offset)
         for char = (code-char (aref directory i))
         when (eql char #\Space) do
           (return)
         do
           (setf (elt name idx) (if ext-lower-p (char-downcase char) char))
           (incf idx)))
    ;; Delete extra spaces from the right end
    (string-right-trim '(#\Space) name)))

(defun write-short-name (directory offset name type name-length type-length)
  (loop for idx from 0 to (1- name-length) do
       (setf (aref directory (+ idx offset))
             (char-code (elt name idx))))
  (loop for idx from name-length to 7 do
       (setf (aref directory (+ idx offset)) #x20))
  (loop for idx from 0 to (1- type-length) do
       (setf (aref directory (+ idx offset 8))
             (char-code (elt type idx))))
  (loop for idx from type-length to 2 do
       (setf (aref directory (+ idx offset 8)) #x20)))

(defun legal-short-name-value (value)
  (or (<= #x41 value #x5A)    ;; A - Z
      (<= #x30 value #x39)    ;; 0 - 9
      ;; ! #  $ % &  ' (  ) - @ ^ _ ` { } ~
      (member value '(#x21 #x23 #x24 #x25 #x26 #x27 #x28 #x29
                      #x2D #x40 #x5E #x5F #x60 #x7B #x7D #x7E))))

(defun add-numeric-tail (name num)
  (cond ((= num 1)
         (let* ((length (length name)))
           (cond ((= length 8)
                  (setf (char name 6) #\~
                        (char name 7) #\1))
                 ((= length 7)
                  (setf (char name 6) #\~)
                  (setf name (concatenate 'string name "1")))
                 (T
                  (setf name (concatenate 'string name "~1"))))))
        (T
         (multiple-value-bind (width rem) (truncate (log num 10))
           (cond ((= rem 0.0)
                  ;; num is multiple of ten need to add another digit
                  (let ((pos (position #\~ name))
                        (num-string (format nil "~D" num)))
                    (if (< (+ pos width) 7)
                        (setf name (concatenate 'string name " ")
                              (subseq name (1+ pos)) num-string)
                        (setf (char name (1- pos)) #\~
                              (subseq name pos) num-string))))
                 (T
                  (setf (subseq name (1+ (position #\~ name)))
                        (format nil "~D" num)))))))
  name)

(defun make-short-name (pathname-name pathname-type file)
  (let ((name (string-upcase pathname-name))
        (type (string-upcase pathname-type))
        (lossy NIL)
        (short-names NIL))
    (flet ((dup-name-p (file name type)
             (when (not short-names)
               (do-files (offset) file
                   NIL
                   (push (read-short-name file offset) short-names)))
             (let ((short-name (if (= (length type) 0)
                                   name
                                   (concatenate 'string name "." type))))
               (member short-name short-names :test #'string=))))
      ;; convert invalid chars to _
      ;; not sure this is correct because it converts spaces and periods to _,
      ;; but later code processes these characters specially.
      (loop
         for idx from 0 below (length name)
         when (not (legal-short-name-value (char-int (char name idx)))) do
           (setf (char name idx) #\_
                 lossy T))
      (loop
         for idx from 0 below (length type)
         when (not (legal-short-name-value (char-int (char type idx)))) do
           (setf (char type idx) #\_
                 lossy T))
      ;; strip all blanks (leading, following and embedded) and leading periods
      (setf name (string-left-trim "." (remove " " name))
            type (remove " " type))
      ;; trim name to first "." or eight chars which ever is first
      (let ((length (min (length name)
                         (or (position "." name) 8)
                         8)))
        (setf name (subseq name 0 length)))
      ;; trim type to first 3 chars
      (when (> (length type) 3)
        (setf type (subseq type 0 3)))
      ;; add numeric tail
      (when (or lossy
                (> (length pathname-name) 8)
                (> (length pathname-type) 3)
                (dup-name-p file name type))
        (setf name (add-numeric-tail name 1))
        (loop
           for num = 2 then (1+ num)
           when (not (dup-name-p file name type)) do (return)
           do
             (setf name (add-numeric-tail name num)))))
    (values name type)))

(defun read-long-name-section (directory offset)
  (let ((name (make-string 13))
        (idx 0))
    (flet ((add-chars (start end)
             (loop
                for i from (+ offset start) by 2 to (+ offset end)
                for octet = (sys.int::ub16ref/le directory i)
                when (or (= octet 0) (= octet #xFFFF)) do (return)
                do
                  (setf (elt name idx) (code-char octet))
                  (incf idx))))
      (add-chars 1 10)
      (add-chars 14 25)
      (add-chars 28 31))
    (if (= idx 13)
        name
        (subseq name 0 idx))))

(defun read-long-name (directory start checksum)
  (let ((name ""))
    (loop
       for offset = start then (- offset 32)
       ;; This is really an error - should not have run off the beginning
       when (< offset 0) do (return)
       ;; This is really an error - checksum should match
       when (/= (aref directory (+ offset 13)) checksum) do (return)
       do
         (setf name (concatenate 'string name
                                 (read-long-name-section directory offset)))
       ;; If this is the last block, exit the loop
       when (logbitp 6 (aref directory offset)) do (return))
    name))

(defun short-name-p (directory file)
  (or (= file 0) ;; first directory entry, can't have long name
      ;; previous entry free
      (= (aref directory (+ file -32)) #xE5)
      ;; attribute not equal long name
      (/= (aref directory (+ file -32 11)) #x0F)))

(defun read-file-name (directory file)
  (if (short-name-p directory file)
      (read-short-name directory file)
      (read-long-name directory (+ file -32) (checksum directory file))))

(defun read-name-and-type (directory file)
  (let* ((full-name (read-file-name directory file))
         (dot-pos (position #\. full-name :from-end T)))
    (if (or (null dot-pos)
            (= (1+ dot-pos) (length full-name))
            (= dot-pos 0))
        (values full-name NIL)
        (values (subseq full-name 0 dot-pos) (subseq full-name (1+ dot-pos))))))

(defun free-file-entry (directory start)
  (when (not (short-name-p directory start))
    ;; mark long name entries as free
    (loop
       for offset = (- start 32) then (- offset 32)
       with checksum = (checksum directory start)
       ;; This is really an error - should not have run off the beginning
       when (< offset 0) do (return)
       ;; This is really an error - checksum should match
       when (/= (aref directory (+ offset 13)) checksum) do (return)
       do
         (let ((first-byte (aref directory offset)))
           (setf (aref directory offset) #xE5)
           (when (logbitp 6 first-byte)
             (return)))))
  ;; mark short name directory entry as free
  (setf (aref directory start) #xE5))

(defun deallocate-file (ffs fat directory offset)
  (do ((cluster-n (read-first-cluster directory offset) next-cluster-n)
       (next-cluster-n))
      ((>= cluster-n (last-cluster-value ffs)))
    (setf next-cluster-n (fat-value fat cluster-n)
          (fat-value ffs fat cluster-n) 0)))

(defun remove-file (directory start disk cluster-n ffs fat)
  ;; Update FAT
  (deallocate-file ffs fat directory start)
  ;; mark directory entry (or entries) as free
  (free-file-entry directory start)
  ;; Write to disk
  (write-fat disk ffs fat)
  (write-directory ffs disk cluster-n fat directory))

(defun expand-directory (directory cluster-size)
  (let ((new-dir (make-array (+ (length directory) cluster-size)
                             :element-type '(unsigned-byte 8))))
    (dotimes (idx (length directory))
      (setf (aref new-dir idx) (aref directory idx)))
    (loop
       for idx from (length directory) below (length new-dir)
       do (setf (aref new-dir idx) 0))
    new-dir))

(defun next-space (directory offset cluster-size)
  "Return offset of next space and directory (possibly expanded)"
  (do ((i offset (+ 32 i)))
      ((<= (length directory) i)
       (values i (expand-directory directory cluster-size)))
    (let ((first-byte (aref directory i)))
      (when (or (zerop first-byte)
                (= #xE5 first-byte))
        (return-from next-space (values i directory))))))

(defun next-n-spaces (directory n cluster-size)
  "Return offset of next n contiguous spaces and directory (possibly expanded)"
  (multiple-value-bind (start directory) (next-space directory 0 cluster-size)
    (do ((cnt 0 (1+ cnt))
         (last start next)
         (next))
        ((= cnt n) (values start directory))
      (multiple-value-setq (next directory)
        (next-space directory (+ last 32) cluster-size))
      (when (/= (+ last 32) next)
        ;; not contiguous, start over
        (setf cnt 0
              start next)))))

(defun long-name-p (pathname-name pathname-type name-length type-length)
  (cond ((and (string= pathname-name ".") (= type-length 0)) NIL)
        ((and (string= pathname-name "..") (= type-length 0)) NIL)
        (T (or (> name-length 8)
               (> type-length 3)
               ;; check for legal characters in namen
               (loop
                  for char across pathname-name
                  for value = (char-int char)
                  when (not (legal-short-name-value value)) do (return T)
                  finally NIL)
               (loop
                  for char across pathname-type
                  for value = (char-int char)
                  when (not (legal-short-name-value value)) do (return T)
                  finally NIL)))))

(defun create-directory-entry (directory pathname-name pathname-type previous-p cluster-size)
  ;; Handle previous-p
  (cond ((and pathname-type previous-p)
         (setf pathname-type (concatenate 'string pathname-type "~")))
        (previous-p
         (setf pathname-name (concatenate 'string pathname-name "~"))))
  ;; determine if the entry is a short name or long name entry
  (let ((name-length (length pathname-name))
        (type-length (length pathname-type)))
    (cond ((long-name-p pathname-name pathname-type name-length type-length)
           ;; long name entry
           (let* ((total-length (+ name-length
                                   (if (= type-length 0) 0 (1+ type-length))))
                  (num-entries (1+ (ceiling (/ total-length 13))))
                  (long-name (if (= type-length 0)
                                 pathname-name
                                 (concatenate 'string
                                              pathname-name
                                              "."
                                              pathname-type)))
                  (start-offset))
             (multiple-value-setq (start-offset directory)
               (next-n-spaces directory num-entries cluster-size))
             (let ((end-offset (+ start-offset (ash (1- num-entries) 5))))
               (multiple-value-bind (name type)
                   (make-short-name pathname-name pathname-type directory)
                 (write-short-name directory
                                   end-offset
                                   name
                                   type
                                   (length name)
                                   (length type)))
               (loop
                  for char across long-name
                  with checksum = (checksum directory end-offset)
                  with offset = end-offset
                  with idx = 32
                  with seq-num = 1
                  do
                  ;; finished last directory entry, move to previous
                    (when (= idx 32)
                      (decf offset 32)
                      (setf (aref directory offset) seq-num
                            ;; Attributes
                            (aref directory (+ offset 11)) #x0F
                            ;; Type
                            (aref directory (+ offset 12)) #x00
                            (aref directory (+ offset 13)) checksum
                            ;; Set first cluster to Zero
                            (aref directory (+ offset 26)) #x00
                            (aref directory (+ offset 27)) #x00)
                      (incf seq-num)
                      (setf idx 1))
                    (setf (aref directory (+ idx offset)) (char-code char)
                          (aref directory (+ idx offset 1)) 0)
                    (incf idx 2)
                    (when (= idx 11) (setf idx 14))
                    (when (= idx 26) (setf idx 28))
                  finally
                  ;; long name is multiple of 13 characters - no NUL termination
                    (when (= idx 32) (return))
                  ;; null termination, then #xFFFF for remaining slots
                    (setf (aref directory (+ idx offset)) 0
                          (aref directory (+ idx offset 1)) 0)
                    (loop
                       (incf idx 2)
                       (when (= idx 11) (setf idx 14))
                       (when (= idx 26) (setf idx 28))
                       (when (= idx 32) (return))
                       (setf (aref directory (+ idx offset)) #xFF
                             (aref directory (+ idx offset 1)) #xFF)))
               ;; set last entry flag - really want logiorf here
               (incf (aref directory start-offset) #x40)
               (values end-offset directory))))
          (T
           ;; short name entry
           (multiple-value-bind (offset directory)
               (next-space directory 0 cluster-size)
             (write-short-name directory
                               offset
                               pathname-name
                               pathname-type
                               name-length
                               type-length)
             (values offset directory))))))

(defun create-file (host directory cluster-n pathname-name pathname-type previous-p attributes)
  "Create file/directory"
  (multiple-value-bind (time date) (get-fat-time)
    (let* ((offset)
           (ffs (fat-structure host))
           (fat (fat host))
           (cluster-size (bytes-per-cluster ffs))
           (cluster-number (next-free-cluster ffs fat 3))
           (block-device (file-host-mount-device host)))
      ;; Terminate cluster list (allocate one cluster to the file)
      (setf (fat-value ffs fat cluster-number) (last-cluster-value ffs))
      (multiple-value-setq (offset directory)
        (create-directory-entry directory pathname-name pathname-type previous-p
                                cluster-size))
      (flet ((fill-in-entry (directory offset cluster-number)
               (setf (read-attributes directory offset) attributes
                     (read-reserved directory offset) 0
                     (read-creation-time-tenth directory offset) 0
                     (read-creation-time directory offset) time
                     (read-creation-date directory offset) date
                     (read-last-access-date directory offset) date
                     (read-write-time directory offset) time
                     (read-write-date directory offset) date
                     (read-first-cluster directory offset) cluster-number
                     (read-file-length directory offset) 0)))
        ;; fill in directory entry for new file
        (fill-in-entry directory offset cluster-number)
        (when (directory-p directory offset)
          ;; create new directory with "." and ".." entries
          ;; don't need second result of create-directory-entry because
          ;; new-dir is empty and won't be expanded
          (let ((new-dir (make-array cluster-size
                                     :area :wired
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0))
                ;; (null cluster-n) => root dir, when ".." is root dir,
                ;; spec requires 0 for ".." cluster
                (dot-dot-cluster (or cluster-n 0)))
            (fill-in-entry new-dir
                           (create-directory-entry new-dir "." "" NIL 0)
                           cluster-number)
            (fill-in-entry new-dir
                           (create-directory-entry new-dir ".." "" NIL 0)
                           dot-dot-cluster)
            ;; Write to disk
            (write-directory ffs block-device cluster-number fat new-dir)))
        ;; Write parent directory to disk
        (write-directory ffs block-device cluster-n fat directory)
        ;; Write fat
        (write-fat block-device ffs fat)
        ;; Return cluster-number, possibly new directory array and offset
        (values cluster-number directory offset)))))

;;; Host integration

(defclass fat-host (file-host-mount-mixin file-system-host)
  ((%name :initarg :name
          :reader host-name)
   (%lock :initarg :lock
          :reader fat-host-lock)
   (fat-structure :initarg :fat-structure
                  :accessor fat-structure)
   (fat32-info :initarg :fat32-info
               :reader fat32-info)
   (fat :initarg :fat
        :accessor fat))
  (:default-initargs :lock (mezzano.supervisor:make-mutex "Local File Host lock")))

(defmethod host-default-device ((host fat-host))
  nil)

(defun init-host (host block-device sector-0-buf ffs-type)
  (setf (fat-structure host) (if (eq ffs-type 'fat32)
                                 (read-fat32-structure sector-0-buf)
                                 (read-fat12/16-structure sector-0-buf ffs-type))
        (fat host) (read-fat block-device (fat-structure host))
        (file-host-mount-state host) :mounted
        (file-host-mount-device host) block-device))

(defmethod mount-host ((host fat-host) block-device)
  (multiple-value-bind (uuid label sector-0-buf)
      (probe-block-device block-device)
    (declare (ignore label))
    (when (and uuid (string-equal uuid (file-host-mount-args host)))
      (init-host host block-device sector-0-buf (type-of (fat-structure host)))
      T)))

(defmethod create-host ((class (eql :fat-host)) block-device name-alist)
  (multiple-value-bind (uuid label sector-0-buf)
      (probe-block-device block-device)
    (when uuid
      (let ((name (cadr (assoc uuid name-alist :test #'string-equal))))
        (when (and (null name)
                   (not (string-equal label "NO NAME    ")))
          ;; no host name found, try using label
          (multiple-value-bind (host-name valid-p)
              (make-host-name label :replace-invalid-characters t)
            (when valid-p
              (setf name host-name))))
        (when (null name)
          ;; Fall back on the UUID
          (multiple-value-bind (host-name valid-p)
              (make-host-name (concatenate 'string "FAT-" uuid))
            (when valid-p
              (setf name host-name))))
        (when name
          (when (find-host name nil)
            (error "fat host ~A already mounted~%" name))
          (let* ((cluster-count (compute-cluster-count sector-0-buf))
                 ;; In code below, <, 4085 and 65525 are correct see
                 ;; Microsoft Doc.
                 (ffs-type (cond ((< cluster-count 4085) 'fat12)
                                 ((< cluster-count 65525) 'fat16)
                                 (T 'fat32)))
                 (host (make-instance 'fat-host
                                      :name (string-upcase (string name))
                                      :fat32-info nil
                                      :mount-args uuid)))
            (init-host host block-device sector-0-buf ffs-type)
            (setf (mezzano.file-system:find-host name) host))
          name)))))

;; According to jdebp (Jonathan de Boyne Pollard)'s Frequently Given
;; Answers, the question of how to determine if a partition contains a
;; FAT file system is answered at the following URL:
;; https://jdebp.eu/FGA/determining-filesystem-type.html#FSTypeDefinitive
;;
;; The following two routines detect BPB (Bios Partition Block)
;; version 7.0 and version 4.0 for FAT file systems. Where buffer is
;; the contents of sector 0 of the partition.

(defun bpb-v7-p (buffer)
  (and (member (aref buffer #x42) '(#x28 #x29))
       (string= (map 'string #'code-char (subseq buffer #x52 #x5A))
                "FAT32   ")))

(defun bpb-v4-p (buffer)
  (and (member (aref buffer #x26) '(#x28 #x29))
       (member (map 'string #'code-char (subseq buffer #x36 #x3E))
               '("FAT     " "FAT16   " "FAT12   ") :test #'string=)))

;; According to the Microsoft document "Microsoft Extensible Firmware
;; Initiative FAT32 File System Specification, FAT: General Overview
;; of On-Disk Format", the correct (and only) way to determine if a
;; FAT file system is FAT12, FAT16 or FAT32 by computing the cluster
;; count.
;;
;; if cluster-count < 4085 then it is FAT12
;; else if cluster-count < 65525 then it is FAT16
;; else it is FAT32
;;
;; "<", 4085 and 65525 are the correct symbol/values.
;;
;; Due to errors in non-Microsoft applications, when creating a FAT
;; file system it is wise to avoid cluster counts that are near 4085
;; and 65525. The cluster count can be adjusted by varying the number
;; of data sectors and the number of sectors per cluster.
;;
;; The function below computes the cluster count using the algorithm
;; described in the document listed above. Where buffer is the
;; contents of sector 0 of the partition.

(defun compute-cluster-count (buffer)
  (let* ((bytes-per-sector (sys.int::ub16ref/le buffer 11))
         (n-root-entries (sys.int::ub16ref/le buffer 17))
         (root-dir-sectors (ceiling (* 32 n-root-entries) bytes-per-sector))
         (fat-size (let ((fs16 (sys.int::ub16ref/le buffer 22)))
                     (if (/= fs16 0) fs16 (sys.int::ub32ref/le buffer 36))))
         (total-sectors (let ((ts16 (sys.int::ub16ref/le buffer 19)))
                          (if (/= ts16 0) ts16 (sys.int::ub32ref/le buffer 32))))
         (data-sectors (- total-sectors
                          (+ (sys.int::ub16ref/le buffer 14)  ;; Rserved Sectors
                             (* fat-size (aref buffer 16))
                             root-dir-sectors))))
    (floor data-sectors (aref buffer 13))))

(defun probe-block-device (block-device)
  "If the block-device contains a FAT file system, return the Volume ID and the sector - buffer, otherwise NIL"
  ;; read first sector and check for FAT file system
  (let* ((sector-size (block-device-sector-size block-device))
         (buffer (make-array sector-size :element-type '(unsigned-byte 8))))
    (block-device-read block-device 0 1 buffer)
    (cond ((bpb-v7-p buffer)      ;; Check for version 7.0 BPB
           (values (format nil "~4,'0X-~4,'0X"
                           (sys.int::ub16ref/le buffer 69)
                           (sys.int::ub16ref/le buffer 67))
                   (map 'string #'code-char (subseq buffer 71 82))
                   buffer))
          ((bpb-v4-p buffer)      ;; Check for version 4.0 BPB
           (values (format nil "~4,'0X-~4,'0X"
                           (sys.int::ub16ref/le buffer 41)
                           (sys.int::ub16ref/le buffer 39))
                   (map 'string #'code-char (subseq buffer 43 54))
                   buffer))
          (T (values NIL NIL NIL)))))

(defun parse-simple-file-path (host namestring)
  (let ((start 0)
        (end (length namestring))
        (directory '())
        (name nil)
        (type nil))
    (when (eql start end)
      (return-from parse-simple-file-path (make-pathname :host host)))
    (cond ((eql (char namestring start) #\>)
           (push :absolute directory)
           (incf start))
          (t (push :relative directory)))
    ;; Last element is the name.
    (do* ((x (sys.int::explode #\> namestring start end) (cdr x)))
         ((null (cdr x))
          (let* ((name-element (car x))
                 (end (length name-element)))
            (unless (zerop (length name-element))
              ;; Find the last dot.
              (let ((dot-position (position #\. name-element :from-end t)))
                (cond ((and dot-position (not (zerop dot-position)))
                       (setf type (subseq name-element (1+ dot-position) end))
                       (setf name (subseq name-element 0 dot-position)))
                      (t (setf name (subseq name-element 0 end))))))))
      (let ((dir (car x)))
        (cond ((or (string= "" dir)
                   (string= "." dir)))
              ((string= ".." dir)
               (push :up directory))
              ((string= "*" dir)
               (push :wild directory))
              ((string= "**" dir)
               (push :wild-inferiors directory))
              (t (push dir directory)))))
    (when (string= name "*") (setf name :wild))
    (when (string= type "*") (setf type :wild))
    (make-pathname :host host
                   :directory (nreverse directory)
                   :name name
                   :type type
                   :version :newest)))

(defmethod parse-namestring-using-host ((host fat-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (parse-simple-file-path host namestring))

(defmethod namestring-using-host ((host fat-host) pathname)
  (when (pathname-device pathname)
    (error 'no-namestring-error
           :pathname pathname
           :format-control "Pathname has a device component"))
  (let ((dir (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname))
        (version (pathname-version pathname)))
    (with-output-to-string (s)
      (when (eql (first dir) :absolute)
        (write-char #\> s))
      (dolist (sub-dir (rest dir))
        (cond
          ((stringp sub-dir) (write-string sub-dir s))
          ((eql sub-dir :up) (write-string ".." s))
          ((eql sub-dir :wild) (write-char #\* s))
          ((eql sub-dir :wild-inferiors) (write-string "**" s))
          (t (error 'no-namestring-error
                    :pathname pathname
                    :format-control "Invalid directory component ~S."
                    :format-arguments (list sub-dir))))
        (write-char #\> s))
      (cond ((eql name :wild)
             (write-char #\* s))
            (name
             (write-string name s)))
      (when type
        (write-char #\. s)
        (if (eql type :wild)
            (write-char #\* s)
            (write-string type s)))
      (when (eql version :previous)
        (write-char #\~ s))
      s)))

(defclass fat-file-stream (file-cache-stream)
  ((pathname :initarg :pathname :reader file-stream-pathname)
   (host :initarg :host :reader host)
   (if-exists :initarg :if-exists :reader if-exists)
   ;; File position where the buffer data starts.
   (first-cluster :initarg :first-cluster :accessor first-cluster)
   (abort-action :initarg :abort-action :accessor abort-action)))

(defclass fat-file-character-stream (fat-file-stream
                                     file-cache-character-stream)
  ())

(defmacro do-cluster (cluster start-cluster fat ffs (&rest vars) finally &body body)
  `(do ((,cluster ,start-cluster (fat-value ,fat ,cluster))
        ,@vars)
       ((>= ,cluster (last-cluster-value ,ffs))
        ,finally)
     ,@body))

(defun read-cluster-n (ffs disk start-cluster fat cluster-n)
  (do-cluster cluster start-cluster fat ffs
      ((n 0 (1+ n)))
      nil
    (when (= n cluster-n)
      (return (block-device-read-sector disk
                                        (first-sector-of-cluster ffs cluster)
                                        (fat-%sectors-per-cluster ffs))))))

(defun write-cluster-n (ffs disk start-cluster fat buffer cluster-n)
  (do-cluster cluster start-cluster fat ffs
      ((n 0 (1+ n)))
      nil
    (when (= n cluster-n)
      (block-device-write-sector disk
                                 (first-sector-of-cluster ffs cluster)
                                 buffer
                                 (fat-%sectors-per-cluster ffs)))))

(defmethod allocate-new-block ((stream fat-file-stream) block-n)
  (let* ((host (host stream))
         (ffs (fat-structure host))
         (fat (fat host))
         (disk (file-host-mount-device host)))
    (let ((next-cluster (next-free-cluster ffs fat 3)))
      (when next-cluster
        (do-cluster cluster (first-cluster stream) fat ffs
            ((last-cluster 0)
             (n 0 (1+ n)))
            (cond ((= block-n n)
                   (setf (fat-value ffs fat last-cluster) next-cluster
                         (fat-value ffs fat next-cluster) (last-cluster-value ffs))
                   (write-fat disk ffs fat)
                   (make-array `(,(bytes-per-cluster ffs)) :element-type '(unsigned-byte 8)))
                  (t
                   (error "Allocate-new-block can't skip blocks")))
          (setf last-cluster cluster))))))

(defmethod read-file-block ((stream fat-file-stream) block-n)
  (let ((host (host stream)))
    (read-cluster-n (fat-structure host)
                    (file-host-mount-device host)
                    (first-cluster stream)
                    (fat host)
                    block-n)))

(defmethod write-file-block ((stream fat-file-stream) buffer block-n)
  (let ((host (host stream)))
    (write-cluster-n (fat-structure host)
                     (file-host-mount-device host)
                     (first-cluster stream)
                     (fat host)
                     buffer
                     block-n)))

(defmacro with-fat-host-locked ((host) &body body)
  `(mezzano.supervisor:with-mutex ((fat-host-lock ,host))
     ,@body))

(defun file-name (pathname)
  "Take pathname and return file name."
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname))
        (previous-p (eql (pathname-version pathname) :previous)))
    (unless (or (eql :wild name)
                (eql :wild type))
      (cond ((and type previous-p)
             (concatenate 'string name "." type "~"))
            (type
             (concatenate 'string name "." type))
            (previous-p
             (concatenate 'string name "~"))
            (T
             name)))))

(defun open-file-metadata (host pathname)
  (loop :with ffs := (fat-structure host)
        :with fat := (fat host)
        :with disk := (file-host-mount-device host)
        :with directory-cluster := NIL
        :with directory := (read-root-directory disk ffs fat)
        :with file-name := (file-name pathname)
        :for directory-name :in (rest (pathname-directory pathname))
        :do (do-files (start) directory
              (return-from open-file-metadata nil)
              (when (and (string= directory-name (read-file-name directory start))
                         (directory-p directory start))
                (setf directory-cluster (read-first-cluster directory start)
                      directory (read-file ffs
                                           disk
                                           (read-first-cluster directory start)
                                           fat))
                (return)))
        :finally (do-files (start) directory
                   (return-from open-file-metadata (values directory directory-cluster))
                     (when (string= file-name (read-file-name directory start))
                     (return-from open-file-metadata (values directory directory-cluster start))))))

(defmethod open-using-host ((host fat-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Non-absolute pathname."))
  (when (eql element-type :default)
    (setf element-type 'character))
  (when (not (or (and (eql element-type 'character)
                      (eql external-format :utf-8))
                 (eql external-format :default)))
    (error "Unsupported external format ~S." external-format))
  (when (not (pathname-name pathname))
    (error 'simple-file-error
           :pathname pathname
           :format-control "I've been through the desert on a file with no name."))
  (when (and (string-equal (pathname-type pathname) "directory")
             (member direction '(:output :io)))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Cannot open directories for output."))
  (with-fat-host-locked (host)
    (multiple-value-bind (dir-array dir-cluster file-offset) (open-file-metadata host pathname)
      (when (null dir-array)
        (error 'simple-file-error
               :pathname pathname
               :format-control "Directory ~S does not exist."
               :format-arguments (list pathname)))
      (when (and file-offset (directory-p dir-array file-offset)
                 (member direction '(:output :io)))
        (error 'simple-file-error
               :pathname pathname
               :format-control "Cannot open directories for output."))
      (let ((ffs (fat-structure host))
            (fat (fat host))
            (disk (file-host-mount-device host))
            (dirty-bit nil)
            (abort-action nil)
            (buffer nil)
            (first-cluster)
            (block-n -1)
            (file-length 0)
            (file-position 0))
        (cond ((null file-offset)
               (ecase if-does-not-exist
                 (:error
                  (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A does not exist."
                         :format-arguments (list pathname)))
                 (:create
                  (multiple-value-setq (first-cluster dir-array file-offset)
                    (create-file host dir-array dir-cluster (pathname-name pathname) (pathname-type pathname) nil (ash 1 +attribute-archive+)))
                  (setf buffer (make-array `(,(bytes-per-cluster ffs)) :element-type '(unsigned-byte 8))
                        block-n 0
                        dirty-bit t
                        abort-action :delete))
                 ((nil)
                  (return-from open-using-host nil))))
              ((eql direction :input)
               (setf first-cluster (read-first-cluster dir-array file-offset)
                     file-length (read-file-length dir-array file-offset)))
              (t (ecase if-exists
                   (:error (error 'simple-file-error
                                  :pathname pathname
                                  :format-control "File ~A exists."
                                  :format-arguments (list pathname)))
                   ((:new-version :rename :rename-and-delete)
                    (error 'simple-file-error
                           :pathname pathname
                           :format-control ":if-exists ~S not implemented."
                           :format-arguments (list if-exists)))
                   (:supersede
                    (let ((next-cluster (next-free-cluster ffs fat 3)))
                      (when next-cluster
                        (setf buffer (make-array `(,(bytes-per-cluster ffs)) :element-type '(unsigned-byte 8))
                              first-cluster next-cluster
                              block-n 0
                              dirty-bit t
                              (fat-value ffs fat next-cluster) (last-cluster-value ffs))
                        (write-fat disk ffs fat))
                      (make-array `(,(bytes-per-cluster ffs)) :element-type '(unsigned-byte 8))))
                   (:overwrite
                    (setf first-cluster (read-first-cluster dir-array file-offset)
                          file-length (read-file-length dir-array file-offset)))
                   (:append
                    (setf first-cluster (read-first-cluster dir-array file-offset)
                          file-length (read-file-length dir-array file-offset)
                          file-position (read-file-length dir-array file-offset)))
                   ((nil) (return-from open-using-host nil)))))
        ;; Done processing arguements - now open the file
        (cond ((or (eql element-type :default)
                   (subtypep element-type 'character))
               (make-instance 'fat-file-character-stream
                              :pathname pathname
                              :host host
                              :direction direction
                              :if-exists if-exists
                              :block-size (bytes-per-cluster ffs)
                              :dirty-bit dirty-bit
                              :block-n block-n
                              :buffer buffer
                              :first-cluster first-cluster
                              :position file-position
                              :length file-length
                              :abort-action abort-action
                              :external-format (sys.int::make-external-format 'character external-format)))
              ((and (subtypep element-type '(unsigned-byte 8))
                    (subtypep '(unsigned-byte 8) element-type))
               (assert (eql external-format :default) (external-format))
               (make-instance 'fat-file-stream
                              :pathname pathname
                              :host host
                              :direction direction
                              :if-exists if-exists
                              :block-size (bytes-per-cluster ffs)
                              :dirty-bit dirty-bit
                              :block-n block-n
                              :buffer buffer
                              :first-cluster first-cluster
                              :position file-position
                              :length file-length
                              :abort-action abort-action))
              (t (error "Unsupported element-type ~S." element-type)))))))

(defun force-pathname-name (pathname)
  (let ((orig-directory (pathname-directory pathname)))
    (if (and (null (pathname-name pathname))
             (null (pathname-type pathname)))
        (make-pathname :host (pathname-host pathname)
                       :device (pathname-device pathname)
                       :directory (butlast orig-directory 1)
                       :name (car (last orig-directory 1))
                       :type NIL
                       :version NIL)
        pathname)))

(defun force-directory-only (pathname)
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (if name
        (let ((dir-name (if type
                            (concatenate 'string name "." type)
                            name)))
          (make-pathname :host (pathname-host pathname)
                         :device (pathname-device pathname)
                         :directory (append (pathname-directory pathname)
                                            (list dir-name))
                         :name NIL
                         :type NIL
                         :version NIL))
        pathname)))

(defmethod probe-using-host ((host fat-host) pathname)
  (let ((new-pathname (force-pathname-name pathname)))
    (multiple-value-bind (directory directory-cluster offset)
        (open-file-metadata host new-pathname)
      (declare (ignore directory-cluster))
      (cond ((null offset)
             nil)
            ((directory-p directory offset)
             (force-directory-only pathname))
            (T
             pathname)))))

(defun match-in-directory (disk ffs fat dir-array dir-list pathname)
  (cond ((null dir-list)
         (when (and (not (pathname-name pathname))
                    (not (pathname-type pathname)))
           (return-from match-in-directory (list pathname)))
         (let ((match-name (pathname-name pathname))
               (match-type (pathname-type pathname))
               (result '()))
           (do-files (offset) dir-array
               NIL
               (multiple-value-bind (name type)
                   (read-name-and-type dir-array offset)
                 (when (and (string/= name ".")
                            (string/= name "..")
                            (or (eql match-name :wild)
                                (equalp match-name name))
                            (or (eql match-type :wild)
                                (equalp match-type type)))
                   (let ((filename (make-pathname :name name
                                                  :type type
                                                  :defaults pathname)))
                     (if (directory-p dir-array offset)
                         (push (force-directory-only filename)
                               result)
                         (push filename result))))))
           result))
        ((eql (car dir-list) :wild)
         (let ((directory (pathname-directory pathname))
               (rest-of-dir-list (cdr dir-list))
               (result '()))
           (cond ((and (null rest-of-dir-list)
                       (not (pathname-name pathname))
                       (not (pathname-type pathname)))
                  (do-files (offset) dir-array
                      NIL
                      (let ((name (read-file-name dir-array offset)))
                        (when (and (string/= name ".")
                                   (string/= name "..")
                                   (directory-p dir-array offset))
                          (push (make-pathname :directory (append
                                                           (butlast directory)
                                                           (list name))
                                               :defaults pathname)
                                result)))))
                 (T
                  (let ((start-of-dir (butlast directory (length dir-list))))
                    (do-files (offset) dir-array
                        NIL
                        (let ((name (read-file-name dir-array offset)))
                          (when (and (string/= name ".")
                                     (string/= name "..")
                                     (directory-p dir-array offset))
                            (setf result
                                  (append
                                   (match-in-directory
                                    disk ffs fat
                                    (read-file ffs disk
                                               (read-first-cluster dir-array offset)
                                               fat)
                                    rest-of-dir-list
                                    (make-pathname :directory (append start-of-dir
                                                                      (list name)
                                                                      rest-of-dir-list)
                                                   :defaults pathname))
                                   result))))))))
           result))
        ((eql (car dir-list) :wild-inferiors)
         (let* ((directory (pathname-directory pathname))
                (start-of-dir (butlast directory (length dir-list)))
                (rest-of-dir-list (cdr dir-list))
                (result '()))
           (do-files (offset) dir-array
               NIL
               (let ((name (read-file-name dir-array offset)))
                 (when (and (string/= name ".")
                            (string/= name "..")
                            (directory-p dir-array offset))
                   (let ((new-dir-array (read-file ffs disk
                                                   (read-first-cluster dir-array offset)
                                                   fat)))
                     (setf result
                           (append
                            ;; check directory against rest of pathname
                            (match-in-directory
                             disk ffs fat
                             new-dir-array
                             rest-of-dir-list
                             (make-pathname :directory (append start-of-dir
                                                               (list name)
                                                               rest-of-dir-list)
                                            :defaults pathname))
                            ;; check directory against :wild-inferiors
                            (match-in-directory
                             disk ffs fat
                             new-dir-array
                             dir-list
                             (make-pathname :directory (append start-of-dir
                                                               (list name)
                                                               dir-list)
                                            :defaults pathname))
                            result))))))
           (setf result
                 (append (match-in-directory
                          disk ffs fat
                          dir-array
                          rest-of-dir-list
                          (make-pathname :directory (append start-of-dir
                                                            rest-of-dir-list)
                                         :defaults pathname))
                         result))
           result))
        (T ;; Exact match (TODO: Wild strings).
         (let ((match-name (car dir-list)))
           (do-files (offset) dir-array
               NIL
               (let ((name (read-file-name dir-array offset)))
                 (when (and (string/= name ".")
                            (string/= name "..")
                            (string= match-name name)
                            (directory-p dir-array offset))
                   (return-from match-in-directory
                     (match-in-directory
                      disk ffs fat
                      (read-file ffs disk
                                 (read-first-cluster dir-array offset)
                                 fat)
                      (cdr dir-list)
                      pathname)))))))))

(defmethod directory-using-host ((host fat-host) pathname &key)
  (let ((disk (file-host-mount-device host))
        (ffs (fat-structure host))
        (fat (fat host))
        (dir-list (pathname-directory pathname))
        (path (if (or (null (pathname-device pathname))
                      (eq (pathname-device pathname) :wild))
                  (make-pathname :device NIL :defaults pathname)
                  (error 'simple-file-error
                         :pathname pathname
                         :format-control "Pathname has a device component"))))
    (when (eql dir-list :wild)
      (setf dir-list '(:absolute :wild-inferiors)))
    (when (not (typep dir-list '(cons (eql :absolute))))
      (error 'simple-file-error
             :pathname pathname
             :format-control "Non-absolute pathname."))
    (with-fat-host-locked (host)
      (remove-duplicates
       (match-in-directory disk ffs fat
                           (read-root-directory disk ffs fat)
                           (cdr dir-list)
                           path)
       :test #'equal))))

(defmethod ensure-directories-exist-using-host ((host fat-host) pathname &key verbose)
  ;; TODO verbose
  (declare (ignore verbose))
  (assert (eql (first (pathname-directory pathname)) :absolute) (pathname) "Absoute pathname required.")
  (loop :with created := NIL
        :with ffs := (fat-structure host)
        :with fat := (fat host)
        :with disk := (file-host-mount-device host)
        :with directory-cluster := NIL
        :with directory := (read-root-directory disk ffs fat)
        :for directory-name :in (rest (pathname-directory pathname))
        :do (do-files (start) directory
                (setf directory-cluster (create-file host directory directory-cluster directory-name nil
                                                     (eql (pathname-version pathname) :previous)
                                                     (ash 1 +attribute-directory+))
                      directory (read-file ffs disk directory-cluster fat)
                      created T)
              (when (string= directory-name (read-file-name directory start))
                (setf directory-cluster (read-first-cluster directory start)
                      directory (read-file ffs disk (read-first-cluster directory start) fat))
                (return t)))
        :finally (return created)))

(defmethod rename-file-using-host ((host fat-host) source dest)
  (assert (eql (first (pathname-directory source)) :absolute) (source) "Absoute pathname required.")
  (assert (eql (first (pathname-directory dest)) :absolute) (dest) "Absoute pathname required.")
  (if (string= (namestring source)
               (namestring dest))
      t
      (multiple-value-bind (source-dir source-cluster source-start) (open-file-metadata host source)
        (when (null source-start)
          (error 'simple-file-error
                 :pathname source
                 :format-control "File ~A does not exist. ~S"
                 :format-arguments (list source (file-name source))))
        (multiple-value-bind (dest-dir dest-cluster dest-start) (open-file-metadata host dest)
          (let ((ffs (fat-structure host))
                (fat (fat host))
                (disk (file-host-mount-device host))
                (dest-offset))
            (when dest-start
              (remove-file dest-dir dest-start disk dest-cluster ffs fat))
            (multiple-value-setq (dest-offset dest-dir)
              (create-directory-entry dest-dir
                                      (pathname-name dest)
                                      (pathname-type dest)
                                      (eql (pathname-version dest) :previous)
                                      (bytes-per-cluster ffs)))
            ;; copy meta data
            (replace dest-dir source-dir
                     :start1 (+ dest-offset 11)
                     :start2 (+ source-start 11)
                     :end2 (+ source-start 32))
            (cond ((equalp (pathname-directory source)
                           (pathname-directory dest))
                   ;; source and destination are the same directory, only update and write the
                   ;; destination directory (already modified above)
                   ;; remove source file entry
                   (free-file-entry dest-dir source-start)
                   (write-directory ffs disk dest-cluster fat dest-dir))
                  (T
                   ;; source and destination are different directories
                   ;; remove source file entry
                   (free-file-entry source-dir source-start)
                   ;; write both directories
                   (write-directory ffs disk source-cluster fat source-dir)
                   (write-directory ffs disk dest-cluster fat dest-dir))))))))

(defmethod file-properties-using-host ((host fat-host) path)
  (multiple-value-bind (parent-dir parent-cluster file-offset)
      (open-file-metadata host path)
    (declare (ignore parent-cluster))
    (when (null file-offset)
      (error 'simple-file-error
             :pathname path
             :format-control "File ~A does not exist. ~S"
             :format-arguments (list path (file-name path))))
    (let ((attr (read-attributes parent-dir file-offset)))
      (list :write-date
            (fat-to-universal-time 0
                                   (read-write-time parent-dir file-offset)
                                   (read-write-date parent-dir file-offset))
            :access-date
            (fat-to-universal-time 0
                                   0
                                   (read-last-access-date parent-dir file-offset))
            :create-date
            (fat-to-universal-time (read-creation-time-tenth parent-dir file-offset)
                                   (read-creation-time parent-dir file-offset)
                                   (read-creation-date parent-dir file-offset))
            :read-only (logbitp +attribute-read-only+ attr)
            :hidden (logbitp +attribute-hidden+ attr)
            :system (logbitp +attribute-system+ attr)))))

(defmethod set-file-properties-using-host
    ((host fat-host) path &key (write-date NIL write-date-p)
                            (access-date NIL access-date-p)
                            (read-only NIL read-only-p)
                            (hidden NIL hidden-p)
                            (system NIL system-p))
  (multiple-value-bind (parent-dir parent-cluster file-offset)
      (open-file-metadata host path)
    (when (null file-offset)
      (error 'simple-file-error
             :pathname path
             :format-control "File ~A does not exist. ~S"
             :format-arguments (list path (file-name path))))
    (when write-date-p
      (multiple-value-bind (second minute hour date month year)
          (decode-universal-time write-date)
        (setf (read-write-date parent-dir file-offset)
              (encode-fat-date date month year)
              (read-write-time parent-dir file-offset)
              (encode-fat-time second minute hour))
        (when (not access-date-p)
          (setf (read-last-access-date parent-dir file-offset)
                (read-write-date parent-dir file-offset)))))
    (when access-date-p
      (multiple-value-bind (second minute hour date month year)
          (decode-universal-time access-date)
        (declare (ignore second minute hour))
        (setf (read-last-access-date parent-dir file-offset)
              (encode-fat-date date month year))))
    (let ((attr (read-attributes parent-dir file-offset)))
      (when read-only-p
        (setf attr (if read-only
                       (logior attr (ash 1 +attribute-read-only+))
                       (logandc2 attr (ash 1 +attribute-read-only+)))))
      (when hidden-p
        (setf attr (if hidden
                       (logior attr (ash 1 +attribute-hidden+))
                       (logandc2 attr (ash 1 +attribute-hidden+)))))
      (when system-p
        (setf attr (if system
                       (logior attr (ash 1 +attribute-system+))
                       (logandc2 attr (ash 1 +attribute-system+)))))
      (setf (read-attributes parent-dir file-offset) attr))
    (write-directory (fat-structure host)
                     (file-host-mount-device host)
                     parent-cluster
                     (fat host)
                     parent-dir)))

(defmethod delete-file-using-host ((host fat-host) path &key)
  (let* ((disk (file-host-mount-device host))
         (ffs (fat-structure host))
         (fat (fat host)))
    (multiple-value-bind (parent-dir parent-cluster file-offset) (open-file-metadata host path)
      (when (null file-offset)
        (error 'simple-file-error
               :pathname path
               :format-control "File ~A does not exist. ~S"
               :format-arguments (list path (file-name path))))
      (when (directory-p parent-dir file-offset)
        (error 'simple-file-error
               :pathname path
               :format-control "~S is a directory, couldn't delete"
               :format-arguments (list path )))
      (remove-file parent-dir file-offset disk parent-cluster ffs fat))))

(defmethod delete-directory-using-host ((host fat-host) path &key recursive)
  (let* ((disk (file-host-mount-device host))
         (ffs (fat-structure host))
         (fat (fat host))
         (new-path (force-pathname-name path)))
    (labels ((%delete-directory (parent-dir dir-offset)
               (let* ((dir-cluster (read-first-cluster parent-dir dir-offset))
                      (directory (read-file ffs disk dir-cluster fat)))
                 (when (next-file directory (* 2 32))
                   ;; directory not empty
                   (if recursive
                       (do-files (offset) directory
                           nil
                           (cond ((or (= offset 0) (= offset 32))
                                  ;; ignore "." and ".." entries
                                  )
                                 ((directory-p directory offset)
                                  (%delete-directory directory offset))
                                 ((file-p directory offset)
                                  ;; free file clusters
                                  (deallocate-file ffs fat directory offset))
                                 (T
                                  (error 'simple-file-error
                                         :pathname path
                                         :format-control "Deleting ~S Unknown type entry type for ~A."
                                         :foramt-arguments (path (read-file-name directory offset))))))
                       (error 'simple-file-error
                              :pathname path
                              :format-control "Directory ~A not empty."
                              :format-arguments (list path))))
                 ;; free directory clusters
                 (deallocate-file ffs fat parent-dir dir-offset))))
      (multiple-value-bind (parent-dir parent-cluster dir-offset)
          (open-file-metadata host new-path)
        (when (null dir-offset)
          (error 'simple-file-error
                 :pathname path
                 :format-control "Directory ~A does not exist."
                 :format-arguments (list path)))
        (when (not (directory-p parent-dir dir-offset))
          (error 'simple-file-error
                 :pathname path
                 :format-control "~A is not a directory."
                 :format-arguments (list path)))
        (handler-case
            (progn
              ;; free directory clusters (possibly recursively)
              (%delete-directory parent-dir dir-offset)
              ;; mark directory entry free
              (free-file-entry parent-dir dir-offset))
          (error (condition)
            ;; not able to finish, undo all the work by re-reading fat
            (read-fat disk ffs fat)
            ;; pass the error on
            (error condition)))
        ;; Write to disk
        (write-fat disk ffs fat)
        (write-directory ffs disk parent-cluster fat parent-dir)))
    (force-directory-only path)))

(defmethod expunge-directory-using-host ((host fat-host) path &key)
  (declare (ignore host path))
  t)

(defmethod stream-truename ((stream fat-file-stream))
  (file-stream-pathname stream))

(defmethod close ((stream fat-file-stream) &key abort)
  (let* ((host (host stream))
         (ffs (fat-structure host))
         (fat (fat host))
         (disk (file-host-mount-device host))
         (block-device (file-host-mount-device host)))
    (multiple-value-bind (parent-dir parent-cluster file-offset)
        (open-file-metadata host (file-stream-pathname stream))
      (cond ((not abort)
             (multiple-value-bind (time date) (get-fat-time)
               (when (output-stream-p stream)
                 (finish-output stream)
                 (when (eql (if-exists stream) :supersede)
                   (deallocate-file ffs fat parent-dir file-offset)
                   (setf (read-first-cluster parent-dir file-offset) (first-cluster stream))
                   (write-fat disk ffs fat))
                 (setf (read-write-time parent-dir file-offset) time
                       (read-write-date parent-dir file-offset) date
                       (read-file-length parent-dir file-offset) (file-length stream)))
               (setf (read-last-access-date parent-dir file-offset) date)
               ;; Write to disk new metadata
               (write-directory ffs block-device parent-cluster fat parent-dir)))
            (t
             (cond ((eql (if-exists stream) :supersede)
                    (setf (read-first-cluster parent-dir file-offset) (first-cluster stream))
                    (deallocate-file ffs fat parent-dir file-offset)
                    (write-fat disk ffs fat))
                   ((eql (abort-action stream) :delete)
                    (remove-file parent-dir file-offset block-device parent-cluster ffs fat)))))))
  t)

;; Register the fat file system host as a block device host
(register-block-device-host-type :fat-host)
