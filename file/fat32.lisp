;;;; Copyright (c) 2017-2018 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.
;;;; For now support reading fat32 FS and some write operations.

(defpackage :mezzano.fat32-file-system
  (:use :cl :mezzano.file-system :mezzano.file-system-cache :mezzano.disk-file-system)
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
  (let* ((sector (read-sector disk 0 1))
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
  (let* ((sector (read-sector disk 0 1))
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
  (let* ((sector (read-sector disk 0 1))
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
;;   (let* ((sector (read-sector disk (fat-%fat-info fat) 1))
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
;;     (write-sector disk (fat-%fat-info fat) sector 1)))

(defmethod read-fat (disk (fat12 fat12))
  (loop :with fat-offset := (fat-%n-reserved-sectors fat12)
        :with file-allocation-table := (read-sector disk fat-offset (fat-%sectors-per-fat fat12))
        :with fat := (make-array (list (floor (/ (ash (length file-allocation-table) 3) 12))))
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
        :with file-allocation-table := (read-sector disk fat-offset (fat-%sectors-per-fat fat12))
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
        :finally (write-sector disk fat-offset file-allocation-table (fat-%sectors-per-fat fat12))))

(defmethod read-fat (disk (fat16 fat16))
  (loop :with fat-offset := (fat-%n-reserved-sectors fat16)
        :with file-allocation-table := (read-sector disk fat-offset (fat-%sectors-per-fat fat16))
        :with fat := (make-array (list (ash (length file-allocation-table) -1)))
        :for offset :from 0 :by 2 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (sys.int::ub16ref/le file-allocation-table offset)
        :do (setf (aref fat i) cluster-n)
        :finally (return fat)))

(defmethod write-fat (disk (fat16 fat16) fat)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat16)
        :with file-allocation-table := (read-sector disk fat-offset (fat-%sectors-per-fat fat16))
        :for offset :from 0 :by 2 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (aref fat i)
        :do (setf (sys.int::ub16ref/le file-allocation-table offset) cluster-n)
        :finally (write-sector disk fat-offset file-allocation-table (fat-%sectors-per-fat fat16))))

(defmethod read-fat (disk (fat32 fat32))
  (loop :with fat-offset := (fat-%n-reserved-sectors fat32)
        :with file-allocation-table := (read-sector disk fat-offset (/ (fat32-%sectors-per-fat fat32)
                                                                       (fat-%n-fats fat32)))
        :with fat := (make-array (list (ash (length file-allocation-table) -2)))
        :for offset :from 0 :by 4 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (sys.int::ub32ref/le file-allocation-table offset)
        :do (setf (aref fat i) cluster-n)
        :finally (return fat)))

(defmethod write-fat (disk (fat32 fat32) fat)
  (loop :with fat-offset := (fat-%n-reserved-sectors fat32)
        :with file-allocation-table := (read-sector disk fat-offset (/ (fat32-%sectors-per-fat fat32)
                                                                       (fat-%n-fats fat32)))
        :for offset :from 0 :by 4 :below (length file-allocation-table)
        :for i :from 0
        :for cluster-n := (aref fat i)
        :do (setf (sys.int::ub32ref/le file-allocation-table offset) cluster-n)
        :finally (write-sector disk fat-offset file-allocation-table (/ (fat32-%sectors-per-fat fat32)
                                                                        (fat-%n-fats fat32)))))

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
        :return (ash offset -2)))

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
         (sector-size (mezzano.supervisor:disk-sector-size disk))
         (result (make-array (* sector-size spc n-clusters) :element-type '(unsigned-byte 8)))
         (temp-buf (make-array (* spc sector-size) :element-type '(unsigned-byte 8) :area :wired)))
    (do ((cluster-n start-cluster (aref fat cluster-n))
         (n-cluster 0 (1+ n-cluster)))
        ((>= cluster-n (last-cluster-value fat32)) result)
      (multiple-value-bind (successp error-reason)
          (mezzano.supervisor:disk-read disk (first-sector-of-cluster fat32 cluster-n) spc temp-buf)
        (when (not successp)
          (error "Disk read error: ~S" error-reason)))
      (replace result temp-buf :start1 (* n-cluster spc sector-size)))))

(defun write-file (fat32 disk start-cluster fat array)
  (let* ((spc (fat-%sectors-per-cluster fat32))
         (sector-size (mezzano.supervisor:disk-sector-size disk))
         (temp-buf (make-array (* spc sector-size) :element-type '(unsigned-byte 8) :area :wired)))
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
               (replace temp-buf array :start2 (* (+ i n-cluster) spc sector-size))
               (multiple-value-bind (successp error-reason)
                   (mezzano.supervisor:disk-write disk (first-sector-of-cluster fat32 cluster-n) spc temp-buf)
                 (when (not successp)
                   (error "Disk write error: ~S" error-reason)))
               (setf (aref fat last-cluster) cluster-n
                     last-cluster cluster-n))
             t))
      (setf last-cluster cluster-n)
      (replace temp-buf array :start2 (* n-cluster spc sector-size))
      (multiple-value-bind (successp error-reason)
          (mezzano.supervisor:disk-write disk (first-sector-of-cluster fat32 cluster-n) spc temp-buf)
        (when (not successp)
          (error "Disk write error: ~S" error-reason))))))

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
  (with-output-to-string (s)
    (loop :for i :from offset :to (+ 10 offset)
          :for octet := (aref directory i)
          :do (write-char (code-char octet) s))))

(defun (setf read-short-name) (short-name directory offset)
  (loop :for i :from offset :to (+ 10 offset)
        :for char :across short-name
        :do (setf (aref directory i) (char-code char))))

(defmacro do-file ((var start) directory finally &body body)
  (alexandria:with-gensyms (checksum order order1)
    `(do ((,checksum (checksum ,directory ,start))
          (,order -1 (1+ ,order))
          (,order1 0 (aref ,directory ,var))
          (,var ,start (- ,var 32)))
         ((or (> 0 ,var)
              (= (+ #x40 ,order)
                 ,order1))
          ,finally)
       (when (= ,checksum
                (aref ,directory (+ 13 ,var)))
         ,@body))))

(defun read-long-name (directory start)
  (let ((name ""))
    (do-file (i start) directory
      name
      (setf name
            (concatenate 'string name
                         (with-output-to-string (sub-name)
                           (flet ((add (start end)
                                    (loop :for offset :from (+ start i) :by 2 :to (+ end i)
                                          :for octet := (sys.int::ub16ref/le directory offset)
                                          :when (and (/= octet 0)
                                                     (/= octet 65535))
                                          :do (write-char (code-char octet) sub-name))))
                             (add 1 10)
                             (add 14 25)
                             (add 28 31))))))))

(defun (setf read-long-name) (long-name directory start-offset)
  (flet ((set (start end name-offset name-length)
           (loop :for disk-offset :from (+ start start-offset) :by 2 :to (+ start end start-offset)
                 :for name-offset :from name-offset :by 1
                 :do (setf (sys.int::ub16ref/le directory disk-offset)
                           (cond ((= name-offset name-length)
                                  #x0000)
                                 ((< name-offset name-length)
                                  (char-code (aref long-name name-offset)))
                                 (t #xFFFF))))))
    ;; Write long name part starting from last part
    (loop :with name-length := (length long-name)
          :with n := (ceiling (/ name-length 13))
          :with checksum := (checksum directory (+ (ash n 5) start-offset))
          :repeat n
          :for part-n :from n :by -1
          :for name-offset := (* (1- part-n) 13)
          :for order := (+ part-n #x40) :then part-n
          :do (progn (set 1 8 name-offset name-length)
                     (set 14 10 (+ 5 name-offset) name-length)
                     (set 28 2 (+ 11 name-offset) name-length)
                     (setf (aref directory start-offset) order
                           (aref directory (+ start-offset 11)) #x0F ;attributes
                           (aref directory (+ start-offset 12)) 0
                           (aref directory (+ start-offset 13)) checksum
                           (sys.int::ub16ref/le directory (+ start-offset 26)) 0
                           start-offset (next-space directory start-offset))))))

(defun read-file-name (directory file)
  (let ((long-name (read-long-name directory file)))
    (if (string= "" long-name)
        (let ((file-name (read-short-name directory file)))
          (with-output-to-string (name)
            (loop :for i :from 0 :to 7
                  :never (char= #\Space
                                (aref file-name i))
                  :do (write-char (aref file-name i) name))
            (unless (char= #\Space
                           (aref file-name 8))
              (write-char #\. name)
              (loop :for i :from 8 :to 10
                    :never (char= #\Space
                                  (aref file-name i))
                    :do (write-char (aref file-name i) name)))))
        long-name)))

(defun remove-file (directory start disk cluster-n fat32 fat)
  (do-file (i start) directory
    (progn
      ;; Remove first part of file.
      (setf (aref directory start) #xE5)
      ;; Update FAT
      (do ((cluster-n (read-first-cluster directory start)))
          ((>= cluster-n (last-cluster-value fat32)) t)
        (let ((next (aref fat cluster-n)))
          (setf (aref fat cluster-n) 0
                cluster-n next)))
      ;; Write to disk
      (write-fat disk fat32 fat)
      (write-file fat32 disk cluster-n fat directory))
    ;; Remove rest of file.
    (setf (aref directory cluster-n) #xE5)))

(defun next-space (directory offset)
  "Return offset of next space"
  (do ((i offset (+ 32 i)))
      ((<= (length directory) i) nil)
    (let ((first-byte (aref directory i)))
      (when (or (zerop first-byte)
                (= #xE5 first-byte))
        (return i)))))

(defun next-n-spaces (directory n)
  "Return offset of next n contiguous spaces"
  (do ((i 0 (1+ i))
       (j (next-space directory 0) (next-space directory (+ 32 j)))
       (r (next-space directory 0)))
      ((= n i) r)
    (unless (= r (- j (ash i 5)))
      (setf i 0
            r j))))

(defun make-short-name (pathname-name pathname-type file)
  (let ((short-name (make-string 11 :initial-element #\Space)))
    (loop :for char-n :from 0 :to 7
          :for char :across pathname-name
          :do (setf (aref short-name char-n)
                    char))
    (when pathname-type
      (loop :for char-n :from 8 :to 10
            :for char :across pathname-type
            :do (setf (aref short-name char-n)
                      char)))
    ;; Check for short name collision
    (do-files (offset) file
      short-name
      ;; TODO: name collision resolution
      (when (string= short-name (read-short-name file offset))
        (error "Short name ~A does alredy exist.~A~%Short name collision resolution not implemented" short-name)))))

(defun create-file (host file cluster-n pathname-name pathname-type attributes)
  "Create file/directory"
  (multiple-value-bind (time date) (get-fat32-time)
    (let* ((name (concatenate 'string pathname-name "." pathname-type))
           (short-name (make-short-name pathname-name pathname-type file))
           (name-length (length name))
           (start-offset (next-n-spaces file (if (> name-length 11)
                                                 (1+ (ceiling (/ name-length 13)))
                                                 1)))
           (end-offset (+ start-offset (ash (if (> name-length 11)
                                                (ceiling (/ name-length 13))
                                                0)
                                            5)))
           (cluster-number (next-free-cluster (fat host) 3)))
      (flet ((set-short-name (name file start-offset cluster-number)
               (setf (read-short-name file start-offset) name
                     (read-attributes file start-offset) attributes
                     (read-reserved file start-offset) 0
                     (read-creation-time-tenth file start-offset) 0
                     (read-creation-time file start-offset) time
                     (read-creation-date file start-offset) date
                     (read-last-access-date file start-offset) date
                     (read-write-time file start-offset) time
                     (read-write-date file start-offset) date
                     (read-first-cluster file start-offset) cluster-number
                     (read-file-length file start-offset) 0)))
        ;; Write short name part
        (set-short-name short-name file end-offset cluster-number)
        ;; Write long name parts only if needed
        (unless (and (>= 8 (length pathname-name))
                     (>= 3 (length pathname-type)))
          (setf (read-long-name file start-offset) name))
        ;; Make directory files . and ..
        (when (directory-p file end-offset)
          (let ((directory (make-array (bytes-per-cluster (fat-structure host))
                                       :area :wired :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
            (set-short-name ".          " directory 0 cluster-number)
            (set-short-name "..         " directory 32 cluster-n)
            ;; Write to disk
            (write-file (fat-structure host)
                        (partition host)
                        cluster-number
                        (fat host) directory))))
      ;; Write to disk
      (write-file (fat-structure host) (partition host) cluster-n (fat host) file)
      ;; Update fat
      (setf (aref (fat host) cluster-number) (last-cluster-value (fat-structure host)))
      (write-fat (partition host) (fat-structure host) (fat host))
      ;; Return cluster-number
      cluster-number)))

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
        (type (pathname-type pathname)))
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
  (unless (or (eql :wild (pathname-name pathname))
              (eql :wild (pathname-type pathname)))
    (if (pathname-type pathname)
        (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
        (pathname-name pathname))))

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
                (return start)))
        :finally (do-files (start) directory
                   (return-from open-file-metadata (values directory directory-cluster))
                   (when (and (string= file-name (read-file-name directory start))
                              (file-p directory start))
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
             (assert (member external-format '(:default :utf-8))
                     (external-format))
             (make-instance 'fat32-file-character-stream
                            :pathname pathname
                            :host host
                            :direction direction
                            :buffer buffer
                            :buffer-position buffer-position
                            :position file-position
                            :length file-length))
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
                            :length file-length))
            (t (error "Unsupported element-type ~S." element-type))))))

(defmethod probe-using-host ((host fat32-host) pathname)
  (multiple-value-bind (file-data file-cluster file-length) (open-file host pathname)
    (declare (ignore file-data file-cluster))
    (if file-length pathname nil)))

(defmethod directory-using-host ((host fat32-host) pathname &key)
  (let ((file-data (open-file-metadata host pathname))
        (stack '()))
    (do-files (file) file-data
      t
      (push (parse-simple-file-path host
                                    (format nil
                                            (if (file-p file-data file)
                                                "~a~a"
                                                "~a~a>")
                                            (directory-namestring pathname)
                                            (read-file-name file-data file)))
            stack))
    stack))

(defmethod ensure-directories-exist-using-host ((host fat32-host) pathname &key verbose)
  ;; TODO verbose
  (declade (ignore verbose))
  (assert (eql (first (pathname-directory pathname)) :absolute) (pathname) "Absoute pathname required.")
  (loop :with fat32 := (fat-structure host)
        :with fat := (fat host)
        :with disk := (partition host)
        :with directory-cluster := (fat32-root-cluster fat32)
        :with directory := (read-file fat32 disk (fat32-root-cluster fat32) fat)
        :for directory-name :in (rest (pathname-directory pathname))
        :do (do-files (start) directory
              (setf directory-cluster (create-file host directory directory-cluster directory-name nil
                                                   (ash 1 +attribute-directory+))
                    directory (read-file fat32 disk directory-cluster fat))
              (when (string= directory-name (read-file-name directory start))
                (setf directory-cluster (read-first-cluster directory start)
                      directory (read-file fat32 disk (read-first-cluster directory start) fat))
                (return t)))))

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
          (do-file (i source-start) source-dir
            (let ((start (if (< i 0)
                             source-start
                             (+ 32 i))))
              (if (string= (file-name source)
                           (file-name dest))
                  (let ((metadata (subseq source-dir start (+ 32 source-start))))
                    (replace dest-dir metadata :start1 (next-n-spaces dest-dir (/ (length metadata) 32))))
                  ;; Write short name part
                  (let* ((pathname-name (pathname-name dest))
                         (pathname-type (pathname-type dest))
                         (name (concatenate 'string pathname-name "." pathname-type))
                         (short-name (make-short-name pathname-name pathname-type dest-dir))
                         (name-length (length name))
                         (start-offset (next-n-spaces dest-dir (if (> name-length 11)
                                                                   (1+ (ceiling
                                                                        (/ name-length 13)))
                                                                   1)))
                         (end-offset (+ start-offset (ash (if (> name-length 11)
                                                              (ceiling (/ name-length 13))
                                                              0)
                                                          5))))
                    ;; Copy short part
                    (let ((metadata (subseq source-dir source-start (+ 32 source-start))))
                      (replace dest-dir metadata :start1 end-offset))
                    ;; Change short name
                    (setf (read-short-name dest-dir end-offset) short-name)
                    ;; Write long name parts only if needed
                    (unless (and (>= 8 (length pathname-name))
                                 (>= 3 (length pathname-type)))
                      (setf (read-long-name dest-dir start-offset) name))))
              (if (equalp (pathname-directory source)
                          (pathname-directory dest))
                  (loop :for part-offset :from start :to source-start :by 32
                        :do (setf (aref dest-dir part-offset) #xE5))
                  (progn (loop :for part-offset :from start :to source-start :by 32
                               :do (setf (aref source-dir part-offset) #xE5))
                         (write-file (fat-structure host) (partition host) source-cluster (fat host) source-dir)))
              (write-file (fat-structure host) (partition host) dest-cluster (fat host) dest-dir)))))))

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

(defmethod expunge-directory-using-host ((host fat32-host) path &key)
  (declare (ignore host path))
  t)

(defmethod stream-truename ((stream fat32-file-stream))
  (file-stream-pathname stream))

(defmethod close ((stream fat32-file-stream) &key abort)
  (cond ((not abort)
         (let* ((host (host stream))
                (file-length (file-length* stream)))
           (multiple-value-bind (parent-dir parent-cluster file-offset)
               (open-file-metadata host (file-stream-pathname stream))
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
               (setf (read-last-access-date parent-dir file-offset) date))
             ;; Write to disk new metadata
             (write-file (fat-structure host) (partition host) parent-cluster (fat host) parent-dir))))
        ;; TODO Implement abort-action :delete
        (t (error "Aborted close not suported")))
  t)
