;;;; Copyright (c) 2017-2018 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.
;;;; For now support reading fat32 FS and some write operations.

(defpackage :mezzano.fat32-file-system
  (:use :cl :mezzano.file-system :mezzano.file-system-cache :mezzano.disk)
  (:export)
  (:import-from :sys.int
                #:explode))

(in-package :mezzano.fat32-file-system)

(defconstant +bootable-partition-signature+ #xAA55)

(defclass fat-base ()
  ((%boot-jump :initarg :boot-jump :accessor fat-%boot-jump)
   (%oem-name :initarg :oem-name :accessor fat-%oem-name :type string)
   (%bytes-per-sector :initarg :bytes-per-sector :accessor fat-%bytes-per-sector :type (unsigned-byte 16))
   (%sectors-per-cluster :initarg :sectors-per-cluster :accessor fat-%sectors-per-cluster :type (unsigned-byte 8))
   (%n-reserved-sectors :initarg :n-reserved-sectors :accessor fat-%n-reserved-sectors :type (unsigned-byte 16))
   (%n-fats :initarg :n-fats :accessor fat-%n-fats :type (unsigned-byte 8))
   (%n-root-entry :initarg :n-root-entry :accessor fat-%n-root-entry :type (unsigned-byte 16))
   (%n-sectors16 :initarg :n-sectors16 :accessor fat-%n-sectors16 :type (unsigned-byte 16))
   (%media-type :initarg :media-type :accessor fat-%media-type :type (unsigned-byte 8))
   (%sectors-per-fat16 :initarg :sectors-per-fat16 :accessor fat-%sectors-per-fat :type (unsigned-byte 16))
   (%sectors-per-track :initarg :sectors-per-track :accessor fat-%sectors-per-track :type (unsigned-byte 16))
   (%n-heads/sides :initarg :n-heads/sides :accessor fat-%n-heads/sides :type (unsigned-byte 16))
   (%n-hidden-sectors :initarg :n-hidden-sectors :accessor fat-%n-hidden-sectors :type (unsigned-byte 32))
   (%n-sectors32 :initarg :n-sectors32 :accessor fat-%n-sectors32 :type (unsigned-byte 32))))

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

(defun read-fat32-base (fat32 sector)
  (let ((bytes-per-sector (sys.int::ub16ref/le sector 11))
        (n-fats (aref sector 16)))
    (assert (>= n-fats 1))
    (setf (fat-%boot-jump fat32) (check-boot-jump (subseq sector 0 3))
          (fat-%oem-name fat32) (map 'string #'code-char (subseq sector 3 11))
          (fat-%bytes-per-sector fat32) (check-bytes-per-sector bytes-per-sector)
          (fat-%sectors-per-cluster fat32) (check-sectors-per-cluster (aref sector 13) bytes-per-sector)
          (fat-%n-reserved-sectors fat32) (sys.int::ub16ref/le sector 14)
          (fat-%n-fats fat32) n-fats
          (fat-%n-root-entry fat32) (sys.int::ub16ref/le sector 17)
          (fat-%n-sectors16 fat32) (sys.int::ub16ref/le sector 19)
          (fat-%media-type fat32) (check-media-type (aref sector 21))
          (fat-%sectors-per-fat fat32) (sys.int::ub16ref/le sector 22)
          (fat-%sectors-per-track fat32) (sys.int::ub16ref/le sector 24)
          (fat-%n-heads/sides fat32) (sys.int::ub16ref/le sector 26)
          (fat-%n-hidden-sectors fat32) (sys.int::ub32ref/le sector 28)
          (fat-%n-sectors32 fat32) (sys.int::ub32ref/le sector 32))))

(defclass fat12 (fat-base)
  ((%drive-n :initarg :drive-n :accessor fat12-%drive-n)
   (%signature :initarg :signature :accessor fat12-%signature :type (unsigned-byte 8))
   (%volume-id :initarg :volume-id :accessor fat12-%volume-id :type (unsigned-byte 32))
   (%volume-label :initarg :volume-label :accessor fat12-%volume-label :type string)
   (%fat-type-label :initarg :fat-type-label :accessor fat12-%fat-type-label :type string)
   (%boot-code :initarg :boot-code :accessor fat12-%boot-code)))

(defun check-signature (signature)
  (if (or (= signature #x28)
          (= signature #x29))
      signature
      (error "Bad signature : ~x .
Valid signature are #x28 and #x29")))

(defun check-bps (bps)
  (unless (= bps +bootable-partition-signature+)
    (error "Bad bps : ~a .
Valid bps are ~a" bps +bootable-partition-signature+)))

(defun read-fat12-structure (disk)
  (let* ((sector (block-device-read-sector disk 0 1))
         (fat12 (make-instance 'fat12
                               :drive-n (aref sector 36) ; Operating system specific
                               :signature (check-signature (aref sector 38))
                               :volume-id (sys.int::ub32ref/le sector 39)
                               :volume-label (map 'string #'code-char (subseq sector 43 54))
                               :fat-type-label (map 'string #'code-char (subseq sector 54 60))
                               :boot-code (subseq sector 62 510))))
    (read-fat32-base fat12 sector)
    (assert (or (not (zerop (fat-%n-sectors16 fat12)))
                (not (zerop (fat-%n-sectors32 fat12)))))
    (check-bps (sys.int::ub16ref/le sector 510))
    fat12))

(defclass fat16 (fat12)
  ())

(defun read-fat16-structure (disk)
  (let* ((sector (block-device-read-sector disk 0 1))
         (fat16 (make-instance 'fat16
                               :drive-n (aref sector 36) ; Operating system specific
                               :signature (check-signature (aref sector 38))
                               :volume-id (sys.int::ub32ref/le sector 39)
                               :volume-label (map 'string #'code-char (subseq sector 43 54))
                               :fat-type-label (map 'string #'code-char (subseq sector 54 60))
                               :boot-code (subseq sector 62 510))))
    (read-fat32-base fat16 sector)
    (assert (or (not (zerop (fat-%n-sectors16 fat16)))
                (not (zerop (fat-%n-sectors32 fat16)))))
    (check-bps (sys.int::ub16ref/le sector 510))
    fat16))

(defclass fat32 (fat-base)
  ((%sectors-per-fat32 :initarg :sectors-per-fat32 :accessor fat32-%sectors-per-fat :type (unsigned-byte 32))
   (%flags :initarg :flags :accessor fat32-%flags)
   (%fat-version :initarg :fat-version :accessor fat32-%fat-version)
   (%root-cluster :initarg :root-cluster :accessor fat32-%root-cluster :type (unsigned-byte 32))
   (%fat-info :initarg :fat-info :accessor fat32-%fat-info :type (unsigned-byte 16))
   (%backup-boot-sector :initarg :backup-boot-sector :accessor fat32-%backup-boot-sector :type (unsigned-byte 16))
   (%drive-n :initarg :drive-n :accessor fat32-%drive-n :type (unsigned-byte 8))
   (%signature :initarg :signature :accessor fat32-%signature :type (unsigned-byte 8))
   (%volume-id :initarg :volume-id :accessor fat32-%volume-id :type (unsigned-byte 32))
   (%volume-label :initarg :volume-label :accessor fat32-%volume-label :type string)
   (%fat-type-label :initarg :fat-type-label :accessor fat32-%fat-type-label :type string)
   (%boot-code :initarg :boot-code :accessor fat32-%boot-code)))

(defun check-fat-type-label32 (fat-type-label)
  (if (string= "FAT32   " fat-type-label)
      fat-type-label
      (error "Bad fat-type-label : ~a .
Valid media-type ara 'FAT32   ' " fat-type-label)))

(defun read-fat32-structure (disk)
  (let* ((sector (block-device-read-sector disk 0 1))
         (fat32 (make-instance 'fat32
                               :sectors-per-fat32 (sys.int::ub32ref/le sector 36)
                               :flags (sys.int::ub16ref/le sector 40)
                               ;; high byte is major revision number and low byte is minor revision number
                               :fat-version (sys.int::ub16ref/le sector 42)
                               :root-cluster (sys.int::ub32ref/le sector 44)
                               :fat-info (sys.int::ub16ref/le sector 48)
                               :backup-boot-sector (sys.int::ub16ref/le sector 50)
                               :drive-n (aref sector 64) ; Operating system specific
                               :signature (check-signature (aref sector 66))
                               :volume-id (sys.int::ub32ref/le sector 67)
                               :volume-label (map 'string #'code-char (subseq sector 71 82))
                               :fat-type-label (check-fat-type-label32 (map 'string #'code-char
                                                                            (subseq sector 82 90)))
                               :boot-code (subseq sector 90 510))))
    (read-fat32-base fat32 sector)
    (assert (and (zerop (fat-%n-sectors16 fat32))
                 (not (zerop (fat-%n-sectors32 fat32)))))
    (check-bps (sys.int::ub16ref/le sector 510))
    fat32))

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
        :with file-allocation-table := (block-device-read-sector disk fat-offset (fat-%sectors-per-fat fat12))
        :with fat := (if fat-array
                         fat-array
                         (make-array (list (floor (/ (ash (length file-allocation-table) 3) 12)))))
        :for offset :from 0 :by 3 :below (- (length file-allocation-table) 2)
        :for i :from 0 :by 2
        :for byte0 := (aref file-allocation-table offset)
        :for byte1 := (aref file-allocation-table (1+ offset))
        :for byte2 := (aref file-allocation-table (+ 2 offset))
        :for cluster0 := (dpb (ldb (byte 4 0) byte1) (byte 4 8) byte0)
        :for cluster1 := (dpb (ldb (byte 4 4) byte1) (byte 4 8) byte2)
        :do (setf (aref fat i) cluster0
                  (aref fat (1+ i)) cluster1)
        :finally (return fat)))

(defmethod write-fat (disk (fat12 fat12) fat)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat12)
        :with file-allocation-table := (block-device-read-sector disk fat-offset (fat-%sectors-per-fat fat12))
        :for offset :from 0 :by 3 :below (- (length file-allocation-table) 2)
        :for i :from 0 :by 2
        :for cluster0 := (aref fat i)
        :for cluster1 := (aref fat (1+ i))
        :for byte0 := (ldb (byte 8 0) cluster0)
        :for byte1 := (dpb (ldb (byte 4 8) cluster1)
                           (byte 4 4)
                           (ldb (byte 4 8) cluster0))
        :for byte2 := (ldb (byte 8 0) cluster1)
        :do (setf (aref file-allocation-table offset) byte0
                  (aref file-allocation-table (1+ offset)) byte1
                  (aref file-allocation-table (+ 2 offset)) byte2)
        :finally (block-device-write-sector disk fat-offset file-allocation-table (fat-%sectors-per-fat fat12))))

(defmethod read-fat (disk (fat16 fat16) &optional fat-array)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat16)
        :with file-allocation-table := (block-device-read-sector disk fat-offset (fat-%sectors-per-fat fat16))
        :with fat := (if fat-array
                         fat-array
                         (make-array (list (ash (length file-allocation-table) -1))))
        :for offset :from 0 :by 2 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (sys.int::ub16ref/le file-allocation-table offset)
        :do (setf (aref fat i) cluster-n)
        :finally (return fat)))

(defmethod write-fat (disk (fat16 fat16) fat)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat16)
        :with file-allocation-table := (block-device-read-sector disk fat-offset (fat-%sectors-per-fat fat16))
        :for offset :from 0 :by 2 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (aref fat i)
        :do (setf (sys.int::ub16ref/le file-allocation-table offset) cluster-n)
        :finally (block-device-write-sector disk fat-offset file-allocation-table (fat-%sectors-per-fat fat16))))

(defmethod read-fat (disk (fat32 fat32) &optional fat-array)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat32)
        :with file-allocation-table := (block-device-read-sector disk fat-offset (fat32-%sectors-per-fat fat32))
        :with fat := (if fat-array
                         fat-array
                         (make-array (list (ash (length file-allocation-table) -2))))
        :for offset :from 0 :by 4 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (sys.int::ub32ref/le file-allocation-table offset)
        :do (setf (aref fat i) cluster-n)
        :finally (return fat)))

(defmethod write-fat (disk (fat32 fat32) fat)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat32)
        :with file-allocation-table := (block-device-read-sector disk fat-offset (fat32-%sectors-per-fat fat32))
        :for offset :from 0 :by 4 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (aref fat i)
        :do (setf (sys.int::ub32ref/le file-allocation-table offset) cluster-n)
        :finally (block-device-write-sector disk fat-offset file-allocation-table (fat32-%sectors-per-fat fat32))))

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

(defmethod first-data-sector ((fat12 fat12))
  (+ (fat-%n-reserved-sectors fat12)
     (* (fat-%n-fats fat12)
        (fat-%sectors-per-fat fat12))
     (root-dir-sectors fat12)))

(defmethod first-data-sector ((fat32 fat32))
  (+ (fat-%n-reserved-sectors fat32)
     (* (fat-%n-fats fat32)
        (fat32-%sectors-per-fat fat32))
     (root-dir-sectors fat32)))

(defun first-root-dir-sector (fat12)
  (- (first-data-sector fat12)
     (root-dir-sectors fat12)))

(defmethod first-root-dir-cluster ((fat12 fat12))
  (/ (+ (first-root-dir-sector fat12)
        (- (first-data-sector fat12))
        (ash (fat-%sectors-per-cluster fat12) 1))
     (fat-%sectors-per-cluster fat12)))

(defmethod first-root-dir-cluster ((fat32 fat32))
  (fat32-%root-cluster fat32))

(defun first-sector-of-cluster (fat32 cluster-n)
  (+ (* (- cluster-n 2)
        (fat-%sectors-per-cluster fat32))
     (first-data-sector fat32)))

(defun bytes-per-cluster (fat32)
  (* (fat-%sectors-per-cluster fat32)
     (fat-%bytes-per-sector fat32)))

(defun next-free-cluster (fat &optional (start 0))
  (loop :for offset :from start :below (length fat)
        :for cluster-n := (aref fat offset)
        :when (zerop cluster-n)
        :return offset))

(defun get-fat32-time ()
  "Return time and date in fat32 format"
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

(defun read-file (fat32 disk start-cluster fat)
  (let* ((spc (fat-%sectors-per-cluster fat32))
         (n-clusters (do ((cluster-n start-cluster (aref fat cluster-n))
                          (cluster-count 0 (1+ cluster-count)))
                         ((>= cluster-n (last-cluster-value fat32)) cluster-count)))
         (sector-size (block-device-sector-size disk))
         (result (make-array (* sector-size spc n-clusters) :element-type '(unsigned-byte 8))))
    (do ((cluster-n start-cluster (aref fat cluster-n))
         (n-cluster 0 (1+ n-cluster)))
        ((>= cluster-n (last-cluster-value fat32)) result)
      (block-device-read disk
                         (first-sector-of-cluster fat32 cluster-n)
                         spc
                         result
                         :offset (* n-cluster spc sector-size)))))

(defun write-file (fat32 disk start-cluster fat array)
  (let* ((spc (fat-%sectors-per-cluster fat32))
         (sector-size (block-device-sector-size disk)))
    (do ((cluster-n start-cluster (aref fat cluster-n))
         (last-cluster 0)
         (n-cluster 0 (1+ n-cluster)))
        ((>= cluster-n (last-cluster-value fat32))
         (if (> (length array)
                (* n-cluster spc sector-size))
             (do ((cluster-n (next-free-cluster fat 3)
                             (next-free-cluster fat (1+ cluster-n)))
                  (i 0 (1+ i)))
                 ((= (length array)
                     (* (+ i n-cluster) spc sector-size))
                  (setf (aref fat last-cluster) (last-cluster-value fat32))
                  (write-fat disk fat32 fat))
               (block-device-write disk
                                   (first-sector-of-cluster fat32 cluster-n)
                                   spc
                                   array
                                   :offset (* (+ i n-cluster) spc sector-size))
               (setf (aref fat last-cluster) cluster-n
                     last-cluster cluster-n))
             t))
      (setf last-cluster cluster-n)
      (block-device-write disk
                          (first-sector-of-cluster fat32 cluster-n)
                          spc
                          array
                          :offset (* n-cluster spc sector-size)))))

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
  (let ((name (make-string 12 :initial-element #\Space))
        (first (aref directory offset))
        (idx 1))
    ;; First character is special
    (setf (elt name 0) (if (= first #x05) (code-char #xE5) (code-char first)))
    ;; Copy chars for 8 part
    (loop
       for i from (1+ offset) to (+ 7 offset)
       for char = (code-char (aref directory i))
       when (eql char #\Space) do
         (return)
       do
         (setf (elt name idx) char)
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
           (setf (elt name idx) char)
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
                    (format t "pos ~D, width ~D~%" pos width)
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
               (member short-name short-names :test #'string=)))
           )
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
  (let ((name (make-string 13 :initial-element #\Space))
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
    (string-right-trim  '(#\Space) name)))

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

(defun remove-file (directory start disk cluster-n fat32 fat)
  ;; Update FAT
  (do ((cluster-n (read-first-cluster directory start)))
      ((>= cluster-n (last-cluster-value fat32)))
    (let ((next (aref fat cluster-n)))
      (setf (aref fat cluster-n) 0
            cluster-n next)))
  ;; mark directory entry (or entries) as free
  (free-file-entry directory start)
  ;; Write to disk
  (write-fat disk fat32 fat)
  (write-file fat32 disk cluster-n fat directory))

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
  "Return offset of next n contiguous spaces and directory possibly expanded)"
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
                  with seq-num = 1 do
                  ;; finished last directory entry, move to previous
                    (when (= idx 32)
                      (decf offset 32)
                      (setf (aref directory offset) seq-num
                            (aref directory (+ offset 11)) #x0F
                            (aref directory (+ offset 12)) #x00
                            (aref directory (+ offset 13)) checksum)
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
  (multiple-value-bind (time date) (get-fat32-time)
    (let ((offset)
          (cluster-size (bytes-per-cluster (fat-structure host)))
          (cluster-number (next-free-cluster (fat host) 3)))
      ;; Terminate cluster list (allocate one cluster to the file)
      (setf (aref (fat host) cluster-number)
            (last-cluster-value (fat-structure host)))
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
                                     :initial-element 0)))
            (fill-in-entry new-dir
                           (create-directory-entry new-dir "." "" NIL 0)
                           cluster-number)
            (fill-in-entry new-dir
                           (create-directory-entry new-dir ".." "" NIL 0)
                           cluster-n)
            ;; Write to disk
            (write-file (fat-structure host)
                        (partition host)
                        cluster-number
                        (fat host) new-dir)))
        ;; Write parent directory to disk
        (write-file (fat-structure host) (partition host) cluster-n (fat host) directory)
        ;; Write fat
        (write-fat (partition host) (fat-structure host) (fat host))
        ;; Return cluster-number
        cluster-number))))

;;; Host integration

(defclass fat32-host ()
  ((%name :initarg :name
          :reader host-name)
   (%lock :initarg :lock
          :reader fat32-host-lock)
   (partition :initarg :partition
              :reader partition)
   (fat-structure :initarg :fat-structure
                  :reader fat-structure)
   (fat32-info :initarg :fat32-info
               :reader fat32-info)
   (fat :initarg :fat
        :reader fat))
  (:default-initargs :lock (mezzano.supervisor:make-mutex "Local File Host lock")))

(defmethod host-default-device ((host fat32-host))
  nil)

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
    (do* ((x (explode #\> namestring start end) (cdr x)))
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

(defmethod parse-namestring-using-host ((host fat32-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (parse-simple-file-path host namestring))

(defmethod namestring-using-host ((host fat32-host) pathname)
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

(defclass fat32-file-stream (mezzano.gray:fundamental-binary-input-stream
                             mezzano.gray:fundamental-binary-output-stream
                             file-cache-stream
                             file-stream)
  ((pathname :initarg :pathname :reader file-stream-pathname)
   (host :initarg :host :reader host)
   ;; File position where the buffer data starts.
   (buffer-position :initarg :buffer-position
                    :initform 0
                    :accessor buffer-position)
   (abort-action :initarg :abort-action :accessor abort-action)))

(defclass fat32-file-character-stream (mezzano.gray:fundamental-character-input-stream
                                       mezzano.gray:fundamental-character-output-stream
                                       file-cache-character-stream
                                       fat32-file-stream
                                       mezzano.gray:unread-char-mixin)
  ())

(defmacro with-fat32-host-locked ((host) &body body)
  `(mezzano.supervisor:with-mutex ((fat32-host-lock ,host))
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
  (loop :with fat32 := (fat-structure host)
        :with disk := (partition host)
        :with directory-cluster := (first-root-dir-cluster fat32)
        :with directory := (read-file fat32 disk (first-root-dir-cluster fat32) (fat host))
        :with file-name := (file-name pathname)
        :for directory-name :in (rest (pathname-directory pathname))
        :do (do-files (start) directory
              (return-from open-file-metadata nil)
              (when (and (string= directory-name (read-file-name directory start))
                         (directory-p directory start))
                (setf directory-cluster (read-first-cluster directory start)
                      directory (read-file fat32
                                           disk
                                           (read-first-cluster directory start)
                                           (fat host)))
                (return)))
        :finally (do-files (start) directory
                   (return-from open-file-metadata (values directory directory-cluster))
                     (when (string= file-name (read-file-name directory start))
                     (return-from open-file-metadata (values directory directory-cluster start))))))

(defun open-file (host pathname)
  (multiple-value-bind (parent-dir parent-cluster file-offset) (open-file-metadata host pathname)
    (declare (ignore parent-cluster))
    (when file-offset
      (return-from open-file
        (values (read-file (fat-structure host)
                           (partition host)
                           (read-first-cluster parent-dir file-offset)
                           (fat host))
                (read-first-cluster parent-dir file-offset)
                (read-file-length parent-dir file-offset))))))

(defmethod open-using-host ((host fat32-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (with-fat32-host-locked (host)
    (let ((buffer nil)
          (buffer-position 0)
          (file-position 0)
          (file-length 0)
          (created-file-p nil)
          (abort-action nil))
      (multiple-value-bind (file-data file-cluster file-size) (open-file host pathname)
        (if file-size
            (setf buffer file-data
                  buffer-position file-cluster
                  file-length file-size)
            (ecase if-does-not-exist
              (:error (error 'simple-file-error
                             :pathname pathname
                             :format-control "File ~A does not exist. ~S"
                             :format-arguments (list pathname (file-name pathname))))
              (:create (multiple-value-bind (parent-dir parent-cluster file-offset) (open-file-metadata host pathname)
                         (declare (ignore file-offset))
                         (if parent-dir
                             (setf buffer (make-array (bytes-per-cluster (fat-structure host)) :initial-element 0)
                                   buffer-position (create-file host parent-dir parent-cluster
                                                                (pathname-name pathname)
                                                                (pathname-type pathname)
                                                                (eql (pathname-version pathname) :previous)
                                                                (ash 1 +attribute-archive+))
                                   created-file-p t
                                   abort-action :delete)
                             (error 'simple-file-error
                                    :pathname pathname
                                    :format-control "File ~A does not exist. ~S"
                                    :format-arguments (list pathname (file-name pathname)))))))))
      (when (and (not created-file-p) (member direction '(:output :io)))
        (ecase if-exists
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A exists."
                         :format-arguments (list pathname)))
          (:new-version (error ":new-version not suported in fat32."))
          ;; TODO :rename and :rename-and-delete
          ((:rename
            :rename-and-delete)
           (error ":rename and :rename-and-delete are not implemented.")
           (when t
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not rename ~S."
                    :format-arguments (list pathname)))
           (when t
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not supersede ~S."
                    :format-arguments (list pathname))))
          ;; TODO :supersede
          (:supersede
           (error ":supersede is not implemented")
           (setf abort-action :delete)
           (when nil
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not supersede ~S."
                    :format-arguments (list pathname))))
          ((:overwrite) t)
          ((:append) (setf file-position file-length))
          ((nil) (return-from open-using-host nil))))
      (cond ((or (eql element-type :default)
                 (subtypep element-type 'character))
             (make-instance 'fat32-file-character-stream
                            :pathname pathname
                            :host host
                            :direction direction
                            :buffer buffer
                            :buffer-position buffer-position
                            :position file-position
                            :length file-length
                            :abort-action abort-action
                            :external-format (sys.int::make-external-format 'character external-format)))
            ((and (subtypep element-type '(unsigned-byte 8))
                  (subtypep '(unsigned-byte 8) element-type))
             (assert (eql external-format :default) (external-format))
             (make-instance 'fat32-file-stream
                            :pathname pathname
                            :host host
                            :direction direction
                            :buffer buffer
                            :buffer-position buffer-position
                            :position file-position
                            :length file-length
                            :abort-action abort-action))
            (t (error "Unsupported element-type ~S." element-type))))))

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
  (if (pathname-name pathname)
      (make-pathname :host (pathname-host pathname)
                     :device (pathname-device pathname)
                     :directory (append (pathname-directory pathname)
                                        (list (pathname-name pathname)))
                     :name NIL
                     :type NIL
                     :version NIL)
      pathname))

(defmethod probe-using-host ((host fat32-host) pathname)
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

(defmethod directory-using-host ((host fat32-host) pathname &key)
  (let ((file-data (open-file-metadata host pathname))
        (stack '()))
    (do-files (file) file-data
        t
        (let ((file-name (read-file-name file-data file)))
          (when (and (string/= file-name ".")
                     (string/= file-name ".."))
            (push (parse-simple-file-path host
                                          (format nil
                                                  (if (file-p file-data file)
                                                      "~a~a"
                                                      "~a~a>")
                                                  (directory-namestring pathname)
                                                  file-name))
                  stack))))
    stack))

(defmethod ensure-directories-exist-using-host ((host fat32-host) pathname &key verbose)
  ;; TODO verbose
  (declare (ignore verbose))
  (assert (eql (first (pathname-directory pathname)) :absolute) (pathname) "Absoute pathname required.")
  (loop :with created := NIL
        :with fat32 := (fat-structure host)
        :with fat := (fat host)
        :with disk := (partition host)
        :with directory-cluster := (first-root-dir-cluster fat32)
        :with directory := (read-file fat32 disk (first-root-dir-cluster fat32) fat)
        :for directory-name :in (rest (pathname-directory pathname))
        :do (do-files (start) directory
                (setf directory-cluster (create-file host directory directory-cluster directory-name nil
                                                     (eql (pathname-version pathname) :previous)
                                                     (ash 1 +attribute-directory+))
                      directory (read-file fat32 disk directory-cluster fat)
                      created T)
              (when (string= directory-name (read-file-name directory start))
                (setf directory-cluster (read-first-cluster directory start)
                      directory (read-file fat32 disk (read-first-cluster directory start) fat))
                (return t)))
        :finally (return created)))

(defmethod rename-file-using-host ((host fat32-host) source dest)
  (assert (eql (first (pathname-directory source)) :absolute) (source) "Absoute pathname required.")
  (assert (eql (first (pathname-directory dest)) :absolute) (dest) "Absoute pathname required.")
  (if (string= (namestring source)
               (namestring dest))
      t
      (multiple-value-bind (source-dir source-cluster source-start) (open-file-metadata host source)
        (assert source-start (source-start) "Source file not found. ~s" source)
        (multiple-value-bind (dest-dir dest-cluster dest-start) (open-file-metadata host dest)
          (assert (not dest-start) (dest-start) "Destination file alredy exist. ~s" dest)
          (let ((dest-offset))
            (multiple-value-setq (dest-offset dest-dir)
              (create-directory-entry dest-dir
                                      (pathname-name dest)
                                      (pathname-type dest)
                                      (eql (pathname-version dest) :previous)
                                      (bytes-per-cluster (fat-structure host))))
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
                   (write-file (fat-structure host) (partition host) dest-cluster (fat host) dest-dir))
                  (T
                   ;; source and destination are different directories
                   ;; remove source file entry
                   (free-file-entry source-dir source-start)
                   ;; write both directories
                   (write-file (fat-structure host) (partition host) source-cluster (fat host) source-dir)
                   (write-file (fat-structure host) (partition host) dest-cluster (fat host) dest-dir))))))))

(defmethod file-write-date-using-host ((host fat32-host) path)
  (multiple-value-bind (parent-dir parent-cluster file-offset) (open-file-metadata host path)
    (declare (ignore parent-cluster))
    (assert file-offset (file-offset) "File not found. ~s" path)
    (let ((time (read-write-time parent-dir file-offset))
          (date (read-write-date parent-dir file-offset)))
      (encode-universal-time (ash (ldb (byte 5 0) time) 1)
                             (ldb (byte 6 5) time)
                             (ldb (byte 5 11) time)
                             (ldb (byte 5 0) date)
                             (ldb (byte 4 5) date)
                             (+ 1980 (ldb (byte 7 9) date))))))

(defmethod delete-file-using-host ((host fat32-host) path &key)
  (let* ((disk (partition host))
         (fat32 (fat-structure host))
         (fat (fat host)))
    (multiple-value-bind (parent-dir parent-cluster file-offset) (open-file-metadata host path)
      (assert file-offset (file-offset) "File not found. ~s" path)
      (remove-file parent-dir file-offset disk parent-cluster fat32 fat))))

(defmethod delete-directory-using-host ((host fat32-host) path &key recursive)
  (let* ((disk (partition host))
         (fat32 (fat-structure host))
         (fat (fat host))
         (new-path (force-pathname-name path)))
    (labels ((%delete-directory (parent-dir dir-offset)
               (let* ((dir-cluster (read-first-cluster parent-dir dir-offset))
                      (directory (read-file fat32 disk dir-cluster fat)))
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
                                  (do ((cluster-n (read-first-cluster directory offset)))
                                      ((>= cluster-n (last-cluster-value fat32)))
                                    (let ((next (aref fat cluster-n)))
                                      (setf (aref fat cluster-n) 0
                                            cluster-n next))))
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
                 (do ((cluster-n (read-first-cluster parent-dir dir-offset)))
                     ((>= cluster-n (last-cluster-value fat32)))
                   (let ((next (aref fat cluster-n)))
                     (setf (aref fat cluster-n) 0
                           cluster-n next))))))
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
            (read-fat disk fat32 fat)
            ;; pass the error on
            (error condition)))
        ;; Write to disk
        (write-fat disk fat32 fat)
        (write-file fat32 disk parent-cluster fat parent-dir)))
    (force-directory-only path)))

(defmethod expunge-directory-using-host ((host fat32-host) path &key)
  (declare (ignore host path))
  t)

(defmethod stream-truename ((stream fat32-file-stream))
  (file-stream-pathname stream))

(defmethod close ((stream fat32-file-stream) &key abort)
  (let ((host (host stream))
        (file-length (file-length* stream)))
    (multiple-value-bind (parent-dir parent-cluster file-offset)
        (open-file-metadata host (file-stream-pathname stream))
      (cond ((not abort)
             (multiple-value-bind (time date) (get-fat32-time)
               (when (member (direction stream) '(:output :io))
                 (write-file (fat-structure host)
                             (partition host)
                             (buffer-position stream)
                             (fat host)
                             (buffer stream))
                 (setf (read-write-time parent-dir file-offset) time
                       (read-write-date parent-dir file-offset) date
                       (read-file-length parent-dir file-offset) file-length))
               (setf (read-last-access-date parent-dir file-offset) date)
               ;; Write to disk new metadata
               (write-file (fat-structure host) (partition host) parent-cluster (fat host) parent-dir)))
            (t
             (when (eql (abort-action stream) :delete)
               (remove-file parent-dir file-offset (partition host) parent-cluster (fat-structure host) (fat host)))))))
  t)
