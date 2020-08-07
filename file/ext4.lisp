;;;; Copyright (c) 2018-2018 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.
;;;; This implementation can read from ext2, ext3 and ext4.

(defpackage :mezzano.ext4-file-system
  (:use :cl :mezzano.file-system :mezzano.file-system-cache :mezzano.disk :iterate)
  (:local-nicknames (:sys.int :mezzano.internals)
                    (:sup :mezzano.supervisor)
                    (:uuid :mezzano.uuid))
  (:export))

(in-package :mezzano.ext4-file-system)

;; Compatible feature set flags.
(defconstant +compat-dir-prealloc+ 0)
(defconstant +compat-imagic-inodes+ 1)
(defconstant +compat-has-journal+ 2)
(defconstant +compat-ext-attr+ 3)
(defconstant +compat-resize-inode+ 4)
(defconstant +compat-dir-index+ 5)
(defconstant +compat-lazy-bg+ 6)
(defconstant +compat-exclude-inode+ 7)
(defconstant +compat-exclude-bitmap+ 8)
(defconstant +compat-sparse-super2+ 9)

;; Incompatible feature set.
(defconstant +incompat-compression+ 0)
(defconstant +incompat-filetype+ 1)
(defconstant +incompat-recover+ 2)
(defconstant +incompat-journal-dev+ 3)
(defconstant +incompat-meta-bg+ 4)
(defconstant +incompat-extents+ 6)
(defconstant +incompat-64bit+ 7)
(defconstant +incompat-mmp+ 8)
(defconstant +incompat-flex-bg+ 9)
(defconstant +incompat-ea-inode+ 10)
(defconstant +incompat-dirdata+ 12)
(defconstant +incompat-csum-seed+ 13)
(defconstant +incompat-largedir+ 14)
(defconstant +incompat-inline-data+ 15)
(defconstant +incompat-encrypt+ 16)

;; Readonly-compatible feature set.
(defconstant +ro-compat-sparse-super+ 0)
(defconstant +ro-compat-large-file+ 1)
(defconstant +ro-compat-btree-dir+ 2)
(defconstant +ro-compat-huge-file+ 3)
(defconstant +ro-compat-gdt-csum+ 4)
(defconstant +ro-compat-dir-nlink+ 5)
(defconstant +ro-compat-extra-isize+ 6)
(defconstant +ro-compat-has-snapshot+ 7)
(defconstant +ro-compat-quota+ 8)
(defconstant +ro-compat-bigalloc+ 9)
(defconstant +ro-compat-metadata-csum+ 10)
(defconstant +ro-compat-replica+ 11)
(defconstant +ro-compat-readonly+ 12)
(defconstant +ro-compat-project+ 13)

;; Inode modes
(defconstant +inode-others-execute+ 0 "Others members may execute")
(defconstant +inode-others-write+ 1 "Others members may write")
(defconstant +inode-others-read+ 2 "Others members may read")
(defconstant +inode-group-execute+ 3 "Group members may execute")
(defconstant +inode-group-write+ 4 "Group members may write")
(defconstant +inode-group-execute+ 5 "Group members may read")
(defconstant +inode-owner-execute+ 6 "Owner may execute")
(defconstant +inode-owner-write+ 7 "Owner may write")
(defconstant +inode-owner-read+ 8 "Owner may read")
(defconstant +inode-sticky-bit+ 9 "Sticky bit")
(defconstant +inode-gid+ 10 "Group ID")
(defconstant +inode-uid+ 11 "User ID")
;; Inode file types, they are mutually-exclusive
(defconstant +inode-fifo+ #x1)
(defconstant +inode-character-device+ #x2)
(defconstant +inode-directory+ #x4)
(defconstant +inode-block-device+ #x6)
(defconstant +inode-regular-file+ #x8)
(defconstant +inode-symbolic-link+ #xA)
(defconstant +inode-socket+ #xC)

;; Inode flags
(defconstant +sync-flag+ 3 "All writes to the file must be synchronous")
(defconstant +immutable-flag+ 4 "File is immutable")
(defconstant +append-flag+ 5 "File can only be appended")
(defconstant +noatime-flag+ 7 "Do not update access time")
(defconstant +encrypt-flag+ 11 "Encrypted inode")
(defconstant +hashed-indexes-flag+ 12 "Directory has hashed indexes")
(defconstant +imagic-flag+ 13 "AFS magic directory")
(defconstant +journal-data-flag+ 14 "File data must always be written through the journal")
(defconstant +notail-flag+ 15 "File tail should not be merged (not used by ext4)")
(defconstant +dirsync-flag+ 16 "All directory entry data should be written synchronously")
(defconstant +topdir-flag+ 17 "Top of directory hierarchy")
(defconstant +huge-file-flag+ 18 "This is a huge file")
(defconstant +extents-flag+ 19 "Inode uses extents")
(defconstant +ea-inode-flag+ 21 "Inode stores a large extended attribute value in its data blocks")
(defconstant +inline-data-flag+ 28 "Inode has inline data")

;; Directory file types
(defconstant +unknown-type+ #x0)
(defconstant +regular-file-type+ #x1)
(defconstant +directory-type+ #x2)
(defconstant +character-device-type+ #x3)
(defconstant +block-device-type+ #x4)
(defconstant +fifo-type+ #x5)
(defconstant +socket-type+ #x6)
(defconstant +symbolic-link-type+ #x7)

(defstruct superblock
  (inodes-count nil :type (unsigned-byte 32))
  (blocks-count nil :type (unsigned-byte 64))
  (r-blocks-count nil :type (unsigned-byte 64))
  (free-blocks-count nil :type (unsigned-byte 64))
  (free-inodes-count nil :type (unsigned-byte 32))
  (first-data-block nil :type (unsigned-byte 32))
  (log-block-size nil :type (unsigned-byte 32))
  (log-cluster-size nil :type (unsigned-byte 32))
  (blocks-per-group nil :type (unsigned-byte 32))
  (clusters-per-group nil :type (unsigned-byte 32))
  (inodes-per-group nil :type (unsigned-byte 32))
  (mtime nil :type (unsigned-byte 32))
  (wtime nil :type (unsigned-byte 32))
  (mnt-count nil :type (unsigned-byte 32))
  (max-mnt-count nil :type (unsigned-byte 32))
  (magic nil :type (unsigned-byte 16))
  (state nil :type (unsigned-byte 16))
  (errors nil :type (unsigned-byte 16))
  (minor-rev-level nil :type (unsigned-byte 16))
  (lastcheck nil :type (unsigned-byte 32))
  (checkinterval nil :type (unsigned-byte 32))
  (creator-os nil :type (unsigned-byte 32))
  (rev-level nil :type (unsigned-byte 32))
  (def-resuid nil :type (unsigned-byte 16))
  (def-resgid nil :type (unsigned-byte 16))
  (first-ino nil :type (unsigned-byte 32))
  (inode-size nil :type (unsigned-byte 16))
  (block-group-nr nil :type (unsigned-byte 16))
  (feature-compat nil :type (unsigned-byte 32))
  (feature-incompat nil :type (unsigned-byte 32))
  (feature-ro-compat nil :type (unsigned-byte 32))
  (uuid nil :type string)
  (volume-name nil :type string)
  (last-mounted nil :type string)
  (algorithm-bitmap nil :type (unsigned-byte 32))
  (prealloc-blocks nil :type (unsigned-byte 8))
  (prealloc-dir-blocks nil :type (unsigned-byte 8))
  (reserved-gdt-blocks nil :type (unsigned-byte 16))
  (journal-uuid nil :type string)
  (journal-inum nil :type (unsigned-byte 32))
  (journal-dev nil :type (unsigned-byte 32))
  (last-orphan nil :type (unsigned-byte 32))
  (hash-seed nil)
  (def-hash-version nil :type (unsigned-byte 8))
  (jnl-backup-type nil :type (unsigned-byte 8))
  (desc-size nil :type (unsigned-byte 16))
  (default-mount-options nil :type (unsigned-byte 32))
  (first-meta-bg nil :type (unsigned-byte 32))
  (mkfs-time nil :type (unsigned-byte 32))
  (jnl-blocks nil)
  (min-extra-isize nil :type (unsigned-byte 16))
  (want-extra-isize nil :type (unsigned-byte 16))
  (flags nil :type (unsigned-byte 32))
  (raid-stride nil :type (unsigned-byte 16))
  (mmp-interval nil :type (unsigned-byte 16))
  (mmp-block nil :type (unsigned-byte 64))
  (raid-stripe-width nil :type (unsigned-byte 32))
  (log-groups-per-flex nil :type (unsigned-byte 8))
  (checksum-type nil :type (unsigned-byte 8))
  (reserved-pad nil :type (unsigned-byte 16))
  (kbytes-written nil :type (unsigned-byte 64))
  (snapshot-inum nil :type (unsigned-byte 32))
  (snapshot-id nil :type (unsigned-byte 32))
  (snapshot-r-blocks-count nil :type (unsigned-byte 64))
  (snapshot-list nil :type (unsigned-byte 32))
  (error-count nil :type (unsigned-byte 32))
  (first-error-time nil :type (unsigned-byte 32))
  (first-error-ino nil :type (unsigned-byte 32))
  (first-error-block nil :type (unsigned-byte 64))
  (first-error-func nil)
  (first-error-line nil :type (unsigned-byte 32))
  (last-error-time nil :type (unsigned-byte 32))
  (last-error-ino nil :type (unsigned-byte 32))
  (last-error-line nil :type (unsigned-byte 32))
  (last-error-block nil :type (unsigned-byte 64))
  (last-error-func nil)
  (mount-opts nil)
  (usr-quota-inum nil :type (unsigned-byte 32))
  (grp-quota-inum nil :type (unsigned-byte 32))
  (overhead-blocks nil :type (unsigned-byte 32))
  (backup-bgs nil)
  (encrypt-algos nil)
  (encrypt-pw-salt nil)
  (lpf-ino nil :type (unsigned-byte 32))
  (prj-quota-inum nil :type (unsigned-byte 32))
  (checksum-seed nil :type (unsigned-byte 32))
  (reserved nil)
  (checksum nil :type (unsigned-byte 32)))

(let ((implemented (list +incompat-filetype+
                         +incompat-extents+
                         +incompat-64bit+
                         +incompat-flex-bg+
                         +incompat-inline-data+)))
  (defun check-feature-incompat (feature-incompat)
    (cond ((logbitp +incompat-recover+ feature-incompat)
           (format t "ext4 file system mount failed: filesystem needs recovery~%")
           NIL)
          ((not (logbitp +incompat-filetype+ feature-incompat))
           (format t "ext4 file system mount failed: +incompat-filetype+ is required~%")
           NIL)
          (T
           (loop :with result := feature-incompat
                 :for feature :in implemented
                 :when (logbitp feature feature-incompat)
                 :do (decf result (ash 1 feature))
                 :finally (if (zerop result)
                              (return t)
                              (format t "ext4 file system mount failed: ~
                                         Requires incompatible features not implemented: #b~b~%" result)))))))

(defun read-superblock (disk)
  ;; check to see if the disk is big enough to hold a superblock
  (when (> (block-device-n-sectors disk) 4)
    (let* ((superblock (block-device-read-sector disk 2 2))
           (magic (sys.int::ub16ref/le superblock 56))
           (feature-incompat (sys.int::ub32ref/le superblock 96)))
      (when (and (= magic #xEF53) (check-feature-incompat feature-incompat))
        (make-superblock :inodes-count (sys.int::ub32ref/le superblock 0)
                         :blocks-count (logior (ash (sys.int::ub32ref/le superblock 336) 64)
                                               (sys.int::ub32ref/le superblock 4))
                         :r-blocks-count (logior (ash (sys.int::ub32ref/le superblock 340) 64)
                                                 (sys.int::ub32ref/le superblock 8))
                         :free-blocks-count (logior (ash (sys.int::ub32ref/le superblock 344) 64)
                                                    (sys.int::ub32ref/le superblock 12))
                         :free-inodes-count (sys.int::ub32ref/le superblock 16)
                         :first-data-block (sys.int::ub32ref/le superblock 20)
                         :log-block-size (sys.int::ub32ref/le superblock 24)
                         :log-cluster-size (sys.int::ub32ref/le superblock 28)
                         :blocks-per-group (sys.int::ub32ref/le superblock 32)
                         :clusters-per-group (sys.int::ub32ref/le superblock 36)
                         :inodes-per-group (sys.int::ub32ref/le superblock 40)
                         :mtime (sys.int::ub32ref/le superblock 44)
                         :wtime (sys.int::ub32ref/le superblock 48)
                         :mnt-count (sys.int::ub16ref/le superblock 52)
                         :max-mnt-count (sys.int::ub16ref/le superblock 54)
                         :magic magic
                         :state (sys.int::ub16ref/le superblock 58)
                         :errors (sys.int::ub16ref/le superblock 60)
                         :minor-rev-level (sys.int::ub16ref/le superblock 62)
                         :lastcheck (sys.int::ub32ref/le superblock 64)
                         :checkinterval (sys.int::ub32ref/le superblock 68)
                         :creator-os (sys.int::ub32ref/le superblock 72)
                         :rev-level (sys.int::ub32ref/le superblock 76)
                         :def-resuid (sys.int::ub16ref/le superblock 80)
                         :def-resgid (sys.int::ub16ref/le superblock 82)
                         :first-ino (sys.int::ub32ref/le superblock 84)
                         :inode-size (sys.int::ub16ref/le superblock 88)
                         :block-group-nr (sys.int::ub16ref/le superblock 90)
                         :feature-compat (sys.int::ub32ref/le superblock 92)
                         :feature-incompat feature-incompat
                         :feature-ro-compat (sys.int::ub32ref/le superblock 100)
                         :uuid  (uuid:uuid-buffer->string superblock :offset 104)
                         :volume-name (map 'string #'code-char (subseq superblock 120 136))
                         :last-mounted (map 'string #'code-char (subseq superblock 136 200))
                         :algorithm-bitmap (sys.int::ub32ref/le superblock 200)
                         :prealloc-blocks (aref superblock 204)
                         :prealloc-dir-blocks (aref superblock 205)
                         :reserved-gdt-blocks (sys.int::ub16ref/le superblock 206)
                         :journal-uuid (uuid:uuid-buffer->string superblock :offset 208)
                         :journal-inum (sys.int::ub32ref/le superblock 224)
                         :journal-dev (sys.int::ub32ref/le superblock 228)
                         :last-orphan (sys.int::ub32ref/le superblock 232)
                         :hash-seed (make-array '(4) :element-type '(unsigned-byte 32)
                                                :initial-contents (list (sys.int::ub32ref/le superblock 236)
                                                                        (sys.int::ub32ref/le superblock 240)
                                                                        (sys.int::ub32ref/le superblock 244)
                                                                        (sys.int::ub32ref/le superblock 248)))
                         :def-hash-version (aref superblock 252)
                         :jnl-backup-type (aref superblock 253)
                         :desc-size (sys.int::ub16ref/le superblock 254)
                         :default-mount-options (sys.int::ub32ref/le superblock 256)
                         :first-meta-bg (sys.int::ub32ref/le superblock 260)
                         :mkfs-time (sys.int::ub32ref/le superblock 264)
                         :jnl-blocks (make-array '(17) :element-type '(unsigned-byte 32)
                                                 :initial-contents (loop :for i :from 268 :to 332 :by 4
                                                                      :collect (sys.int::ub32ref/le superblock i)))
                         :min-extra-isize (sys.int::ub16ref/le superblock 348)
                         :want-extra-isize (sys.int::ub16ref/le superblock 350)
                         :flags (sys.int::ub32ref/le superblock 352)
                         :raid-stride (sys.int::ub16ref/le superblock 356)
                         :mmp-interval (sys.int::ub16ref/le superblock 358)
                         :mmp-block (sys.int::ub64ref/le superblock 360)
                         :raid-stripe-width (sys.int::ub32ref/le superblock 368)
                         :log-groups-per-flex (aref superblock 372)
                         :checksum-type (aref superblock 373)
                         :reserved-pad (sys.int::ub16ref/le superblock 374)
                         :kbytes-written (sys.int::ub64ref/le superblock 376)
                         :snapshot-inum (sys.int::ub32ref/le superblock 384)
                         :snapshot-id (sys.int::ub32ref/le superblock 388)
                         :snapshot-r-blocks-count (sys.int::ub64ref/le superblock 392)
                         :snapshot-list (sys.int::ub32ref/le superblock 400)
                         :error-count (sys.int::ub32ref/le superblock 404)
                         :first-error-time (sys.int::ub32ref/le superblock 408)
                         :first-error-ino (sys.int::ub32ref/le superblock 412)
                         :first-error-block (sys.int::ub64ref/le superblock 416)
                         :first-error-func (make-array '(32) :element-type '(unsigned-byte 8)
                                                       :initial-contents (loop :for i :from 424 :to 455
                                                                            :collect (aref superblock i)))
                         :first-error-line (sys.int::ub32ref/le superblock 456)
                         :last-error-time (sys.int::ub32ref/le superblock 460)
                         :last-error-ino (sys.int::ub32ref/le superblock 464)
                         :last-error-line (sys.int::ub32ref/le superblock 468)
                         :last-error-block (sys.int::ub64ref/le superblock 472)
                         :last-error-func (make-array '(32) :element-type '(unsigned-byte 8)
                                                      :initial-contents (loop :for i :from 480 :to 511
                                                                           :collect (aref superblock i)))
                         :mount-opts (make-array '(64) :element-type '(unsigned-byte 8)
                                                 :initial-contents (loop :for i :from 512 :to 575
                                                                      :collect (aref superblock i)))
                         :usr-quota-inum (sys.int::ub32ref/le superblock 576)
                         :grp-quota-inum (sys.int::ub32ref/le superblock 580)
                         :overhead-blocks (sys.int::ub32ref/le superblock 584)
                         :backup-bgs (make-array '(2) :element-type '(unsigned-byte 32)
                                                 :initial-contents (loop :for i :from 588 :to 592 :by 4
                                                                      :collect (sys.int::ub32ref/le superblock i)))
                         :encrypt-algos (make-array '(4) :element-type '(unsigned-byte 8)
                                                    :initial-contents (loop :for i :from 596 :to 599
                                                                         :collect (aref superblock i)))
                         :encrypt-pw-salt (make-array '(16) :element-type '(unsigned-byte 8)
                                                      :initial-contents (loop :for i :from 600 :to 615
                                                                           :collect (aref superblock i)))
                         :lpf-ino (sys.int::ub32ref/le superblock 616)
                         :prj-quota-inum (sys.int::ub32ref/le superblock 620)
                         :checksum-seed (sys.int::ub32ref/le superblock 624)
                         :reserved (make-array '(98) :element-type '(unsigned-byte 32)
                                               :initial-contents (loop :for i :from 628 :to 1016 :by 4
                                                                    :collect (sys.int::ub32ref/le superblock i)))
                         :checksum (sys.int::ub32ref/le superblock 1020))))))

(defun bytes-per-block (superblock)
  (ash 1024 (superblock-log-block-size superblock)))

(defun sectors-per-block (disk superblock)
  (/ (bytes-per-block superblock)
     (block-device-sector-size disk)))

(defun read-block (disk superblock block-n &optional (n-blocks 1))
  (let* ((n-sectors (sectors-per-block disk superblock))
         (sector-n (* n-sectors
                      (if (= (superblock-log-block-size superblock) 1)
                          (1+ block-n) block-n))))
    (block-device-read-sector disk sector-n (* n-blocks n-sectors))))

(defun block-group (inode-n superblock)
  "Return block group that an inode lives in"
  (floor (/ (1- inode-n) (superblock-inodes-per-group superblock))))

(defun index (inode-n superblock)
  "Return index of an inode"
  (mod (1- inode-n) (superblock-inodes-per-group superblock)))

(defun offset (inode-n superblock)
  "Return byte address within the inode table"
  (* (index inode-n superblock) (superblock-inode-size superblock)))

(defun n-block-groups (superblock)
  (let ((n-block-groups (ceiling (/ (superblock-blocks-count superblock) (superblock-blocks-per-group superblock)))))
    (assert (= (ceiling (/ (superblock-inodes-count superblock) (superblock-inodes-per-group superblock)))
               n-block-groups))
    n-block-groups))

(defstruct block-group-descriptor
  (block-bitmap)
  (inode-bitmap)
  (inode-table)
  (free-blocks-count)
  (free-inodes-count)
  (used-dirs-count)
  (flags nil)
  (exclude-bitmap nil)
  (block-bitmap-csum nil)
  (inode-bitmap-csum nil)
  (itable-unused nil)
  (checksum nil)
  (reserved nil))

(defun read-block-group-descriptor (superblock block offset)
  (if (logbitp +incompat-64bit+ (superblock-feature-incompat superblock))
      (make-block-group-descriptor :block-bitmap (logior (ash (sys.int::ub32ref/le block (+ 32 offset)) 32)
                                                         (sys.int::ub32ref/le block (+ 0 offset)))
                                   :inode-bitmap (logior (ash (sys.int::ub32ref/le block (+ 36 offset)) 32)
                                                         (sys.int::ub32ref/le block (+ 4 offset)))
                                   :inode-table (logior (ash (sys.int::ub32ref/le block (+ 40 offset)) 32)
                                                        (sys.int::ub32ref/le block (+ 8 offset)))
                                   :free-blocks-count (logior (ash (sys.int::ub16ref/le block (+ 44 offset)) 16)
                                                              (sys.int::ub16ref/le block (+ 12 offset)))
                                   :free-inodes-count (logior (ash (sys.int::ub16ref/le block (+ 46 offset)) 16)
                                                              (sys.int::ub16ref/le block (+ 14 offset)))
                                   :used-dirs-count (logior (ash (sys.int::ub16ref/le block (+ 48 offset)) 16)
                                                            (sys.int::ub16ref/le block (+ 16 offset)))
                                   :flags (sys.int::ub16ref/le block (+ 18 offset))
                                   :exclude-bitmap (logior (ash (sys.int::ub32ref/le block (+ 52 offset)) 32)
                                                           (sys.int::ub32ref/le block (+ 20 offset)))
                                   :block-bitmap-csum (logior (ash (sys.int::ub16ref/le block (+ 56 offset)) 16)
                                                              (sys.int::ub32ref/le block (+ 24 offset)))
                                   :inode-bitmap-csum (logior (ash (sys.int::ub16ref/le block (+ 58 offset)) 16)
                                                              (sys.int::ub32ref/le block (+ 26 offset)))
                                   :itable-unused (logior (ash (sys.int::ub16ref/le block (+ 50 offset)) 16)
                                                          (sys.int::ub16ref/le block (+ 28 offset)))
                                   :checksum (sys.int::ub16ref/le block (+ 30 offset))
                                   :reserved (sys.int::ub32ref/le block (+ 60 offset)))
      (make-block-group-descriptor :block-bitmap (sys.int::ub32ref/le block (+ 0 offset))
                                   :inode-bitmap (sys.int::ub32ref/le block (+ 4 offset))
                                   :inode-table (sys.int::ub32ref/le block (+ 8 offset))
                                   :free-blocks-count (sys.int::ub16ref/le block (+ 12 offset))
                                   :free-inodes-count (sys.int::ub16ref/le block (+ 14 offset))
                                   :used-dirs-count (sys.int::ub16ref/le block (+ 16 offset)))))

(defun read-block-group-descriptor-table (disk superblock)
  (make-array (list (n-block-groups superblock)) :initial-contents
              (iter (with block-group-size := (if (logbitp +incompat-64bit+ (superblock-feature-incompat superblock))
                                                  (superblock-desc-size superblock) 32))
                    (with n-octets := (* block-group-size (n-block-groups superblock)))
                    (with block := (read-block disk superblock
                                               (+ (superblock-first-data-block superblock) 1)
                                               (ceiling (/ n-octets
                                                           (block-device-sector-size disk)
                                                           (sectors-per-block disk superblock)))))
                    (for offset :from 0 :below n-octets :by block-group-size)
                    (collecting (read-block-group-descriptor superblock block offset)))))

(defun read-block-bitmap (disk superblock bgds)
  (let ((n-blocks (if (logbitp +incompat-flex-bg+ (superblock-feature-incompat superblock))
                      (expt 2 (superblock-log-groups-per-flex superblock)) 1)))
    (read-block disk superblock (block-group-descriptor-block-bitmap bgds) n-blocks)))

(defun read-inode-bitmap (disk superblock bgds)
  (let ((n-blocks (if (logbitp +incompat-flex-bg+ (superblock-feature-incompat superblock))
                      (expt 2 (superblock-log-groups-per-flex superblock)) 1)))
    (read-block disk superblock (block-group-descriptor-inode-bitmap bgds) n-blocks)))

(defstruct inode
  (mode nil :type (unsigned-byte 16))
  (uid nil :type (unsigned-byte 16))
  (size nil :type (unsigned-byte 32))
  (atime nil :type (unsigned-byte 32))
  (ctime nil :type (unsigned-byte 32))
  (mtime nil :type (unsigned-byte 32))
  (dtime nil :type (unsigned-byte 32))
  (gid nil :type (unsigned-byte 16))
  (links-count nil :type (unsigned-byte 16))
  (blocks nil :type (unsigned-byte 32))
  (flags nil :type (unsigned-byte 32))
  (osd1 nil :type (unsigned-byte 32))
  (block nil)
  (generation nil :type (unsigned-byte 32))
  (file-acl nil :type (unsigned-byte 32))
  (dir-acl nil :type (unsigned-byte 32))
  (faddr nil :type (unsigned-byte 32))
  (osd2 nil :type (unsigned-byte 96)))

(defun read-inode (disk superblock bgdt inode-n)
  (let* ((bgds (aref bgdt (block-group inode-n superblock)))
         (n-blocks (if (logbitp +incompat-flex-bg+ (superblock-feature-incompat superblock))
                       (expt 2 (superblock-log-groups-per-flex superblock)) 1))
         (block (read-block disk
                            superblock
                            (+ (block-group-descriptor-inode-table bgds)
                               (floor (/ (offset inode-n superblock)
                                         (bytes-per-block superblock))))
                            n-blocks))
         (offset (mod (offset inode-n superblock) (bytes-per-block superblock))))
    (make-inode :mode (sys.int::ub16ref/le block (+ 0 offset))
                :uid (sys.int::ub16ref/le block (+ 2 offset))
                :size (sys.int::ub32ref/le block (+ 4 offset))
                :atime (sys.int::ub32ref/le block (+ 8 offset))
                :ctime (sys.int::ub32ref/le block (+ 12 offset))
                :mtime (sys.int::ub32ref/le block (+ 16 offset))
                :dtime (sys.int::ub32ref/le block (+ 20 offset))
                :gid (sys.int::ub16ref/le block (+ 24 offset))
                :links-count (sys.int::ub16ref/le block (+ 26 offset))
                :blocks (sys.int::ub32ref/le block (+ 28 offset))
                :flags (sys.int::ub32ref/le block (+ 32 offset))
                :osd1 (sys.int::ub32ref/le block (+ 36 offset))
                :block (subseq block (+ 40 offset) (+ 100 offset))
                :generation (sys.int::ub32ref/le block (+ 100 offset))
                :file-acl (sys.int::ub32ref/le block (+ 104 offset))
                :dir-acl (sys.int::ub32ref/le block (+ 108 offset))
                :faddr (sys.int::ub32ref/le block (+ 112 offset))
                :osd2 (logior (ash (sys.int::ub32ref/le block (+ 124 offset)) 64)
                              (sys.int::ub64ref/le block (+ 116 offset))))))

(defstruct linked-directory-entry
  (inode nil :type (unsigned-byte 32))
  (rec-len nil :type (unsigned-byte 16))
  (name-len nil :type (unsigned-byte 8))
  (file-type nil :type (unsigned-byte 8))
  (name nil :type string))

(defun read-linked-directory-entry (block offset)
  (let ((name-len (aref block (+ 6 offset))))
    (make-linked-directory-entry :inode (sys.int::ub32ref/le block (+ 0 offset))
                                 :rec-len (sys.int::ub16ref/le block (+ 4 offset))
                                 :name-len name-len
                                 :file-type (aref block (+ 7 offset))
                                 :name (map 'string #'code-char
                                            (subseq block
                                                    (+ 8 offset)
                                                    (+ 8 offset name-len))))))

(defstruct extent-header
  (magic nil :type (unsigned-byte 16))
  (entries nil :type (unsigned-byte 16))
  (max nil :type (unsigned-byte 16))
  (depth nil :type (unsigned-byte 16)))

(defun read-extent-header (inode-block)
  (let ((magic (sys.int::ub16ref/le inode-block 0)))
    (assert (= #xF30A magic))
    (make-extent-header :magic magic
                        :entries (sys.int::ub16ref/le inode-block 2)
                        :max (sys.int::ub16ref/le inode-block 4)
                        :depth (sys.int::ub16ref/le inode-block 6))))

(defstruct extent
  (n-block nil :type (unsigned-byte 32))
  (length nil :type (unsigned-byte 16))
  (start-block nil :type (unsigned-byte 48)))

(defun read-extent (inode-block offset)
  (let ((length (sys.int::ub16ref/le inode-block (+ 4 offset))))
    (when (> length 32768)
      ;; TODO: support uninitialized extent
      (error "Uninitialized extent not suported"))
    (make-extent :n-block (sys.int::ub16ref/le inode-block offset)
                 :length length
                 :start-block (logior (ash (sys.int::ub16ref/le inode-block (+ 6 offset)) 32)
                                      (sys.int::ub32ref/le inode-block (+ 8 offset))))))

(defun follow-pointer (disk superblock block-n fn n-indirection)
  (if (zerop n-indirection)
      (funcall fn (read-block disk superblock block-n))
      (iter (with i-block := (read-block disk superblock block-n))
            (for offset :from 0 :below (bytes-per-block superblock) :by 4)
            (for block-n := (sys.int::ub32ref/le i-block offset))
            (unless (and (zerop block-n))
              (follow-pointer disk superblock block-n fn (1- n-indirection))))))

(defun do-file (fn disk superblock inode)
  (let* ((inode-block (inode-block inode))
         (inode-flags (inode-flags inode)))
    (cond ((and (logbitp +incompat-extents+ (superblock-feature-incompat superblock))
                (logbitp +extents-flag+ inode-flags))
           ;; TODO: Add support for extent-header-depth not equal to 0
           (let ((extent-header (read-extent-header inode-block)))
             (unless (zerop (extent-header-depth extent-header))
               (error "Not 0 depth extents nodes not implemented"))
             (iter (for offset :from 12 :by 12)
                   (for extent := (read-extent inode-block offset))
                   (repeat (extent-header-entries extent-header))
                   (iter (for block-n :from (extent-start-block extent))
                         (repeat (extent-length extent))
                         (funcall fn (read-block disk superblock block-n))))))
          ((logbitp +inline-data-flag+ inode-flags) ;; fixme: it need to return 1 block sized array
           (funcall fn inode-block))
          (t
           (iter (for offset :from 0 :below 48 :by 4)
                 (for block-n := (sys.int::ub32ref/le inode-block offset))
                 (never (zerop block-n))
                 (follow-pointer disk superblock block-n fn 0))
           (iter (for offset :from 48 :below 60 :by 4)
                 (for indirection :from 1)
                 (for block-n := (sys.int::ub32ref/le inode-block offset))
                 (never (zerop block-n))
                 (follow-pointer disk superblock block-n fn indirection))))))

(defun read-block-n (disk superblock inode block-n)
  (let ((n -1))
    (do-file #'(lambda (block)
                 (when (= (incf n) block-n)
                   (return-from read-block-n block)))
      disk superblock inode)))

(defmacro do-files ((arg var) disk superblock inode finally &body body)
  `(unless (do-file (lambda (,arg)
                      (do ((,var 0 (+ ,var (sys.int::ub16ref/le ,arg (+ 4 ,var)))))
                          ((= ,var (bytes-per-block superblock)) nil)
                        ,@body))
             ,disk ,superblock ,inode)
     ,finally))

;;; Host integration

(defclass ext4-host (file-host-mount-mixin file-system-host)
  ((%name :initarg :name
          :reader host-name)
   (%lock :initarg :lock
          :reader ext4-host-lock)
   (%superblock :initarg :superblock
                :accessor superblock)
   (%bgdt :initarg :bgdt
          :accessor bgdt))
  (:default-initargs :lock (mezzano.supervisor:make-mutex "Local File Host lock")))

(defmethod host-default-device ((host ext4-host))
  nil)

(defun init-host (host block-device superblock)
  (setf (superblock host) superblock
        (bgdt host) (read-block-group-descriptor-table block-device superblock)
        (file-host-mount-state host) :mounted
        (file-host-mount-device host) block-device))

(defmethod mount-host ((host ext4-host) block-device)
  (let ((superblock (read-superblock block-device)))
    (when (and superblock (string-equal (superblock-uuid superblock)
                                        (file-host-mount-args host)))
      (init-host host block-device superblock)
      T)))

(defmethod create-host ((class (eql :ext4-host)) block-device name-alist)
  (let* ((superblock (read-superblock block-device))
         (uuid (and superblock (superblock-uuid superblock))))
    (when uuid
      (let ((name (cadr (assoc uuid name-alist :test #'string-equal))))
        (when (null name)
          ;; no host name found, try using volume name
          (multiple-value-bind (host-name valid-p)
              (make-host-name (superblock-volume-name superblock)
                              :replace-invalid-characters t)
            (when valid-p
              (setf name host-name))))
        (when (null name)
          ;; Fall back on the UUID
          (multiple-value-bind (host-name valid-p)
              (make-host-name (concatenate 'string "EXT4-" uuid))
            (when valid-p
              (setf name host-name))))
        (when name
          (when (find-host name nil)
            (error "ext4 host ~A already mounted~%" name))
          (let ((host (make-instance 'ext4-host
                                     :name (string-upcase (string name))
                                     :mount-args uuid)))
            (init-host host block-device superblock)
            (setf (mezzano.file-system:find-host name) host))
          name)))))

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

(defmethod parse-namestring-using-host ((host ext4-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (parse-simple-file-path host namestring))

(defmethod namestring-using-host ((host ext4-host) pathname)
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

(defun file-name (pathname)
  "Take pathname and return file name."
  (unless (or (eql :wild (pathname-name pathname))
              (eql :wild (pathname-type pathname)))
    (if (pathname-type pathname)
        (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
        (pathname-name pathname))))

(defun find-file (host pathname)
  (loop :with disk := (file-host-mount-device host)
        :with superblock := (superblock host)
        :with bgdt := (bgdt host)
        :with inode := (read-inode disk superblock bgdt 2)
        :with file-name := (file-name pathname)
        :for directory :in (rest (pathname-directory pathname))
        :do (block do-files
              (do-files (block offset) disk superblock inode
                (error 'simple-file-error
                       :pathname pathname
                       :format-control "Directory ~A not found. ~S"
                       :format-arguments (list directory pathname))
                (let ((directory-entry (read-linked-directory-entry block offset)))
                  (when (string= directory (linked-directory-entry-name directory-entry))
                    (setf inode (read-inode disk superblock bgdt (linked-directory-entry-inode directory-entry)))
                    (return-from do-files t)))))
        :finally
        (if (null file-name)
            (return inode)
            (block do-files
              (do-files (block offset) disk superblock inode nil
                (let ((directory-entry (read-linked-directory-entry block offset)))
                  (when (string= file-name (linked-directory-entry-name directory-entry))
                    (return-from find-file
                      (read-inode disk superblock bgdt (linked-directory-entry-inode directory-entry))))))))))

(defclass ext-file-stream (file-cache-stream)
  ((pathname :initarg :pathname :reader file-stream-pathname)
   (host :initarg :host :reader host)
   (file-inode :initarg :file-inode :accessor file-inode)
   (abort-action :initarg :abort-action :accessor abort-action)))

(defclass ext-file-character-stream (ext-file-stream
                                     file-cache-character-stream)
  ())

(defmethod read-file-block ((stream ext-file-stream) block-n)
  (let ((host (host stream)))
    (read-block-n (file-host-mount-device host)
                  (superblock host)
                  (file-inode stream)
                  block-n)))

(defmethod write-file-block ((stream ext-file-stream) buffer block-n)
  nil)

(defmacro with-ext4-host-locked ((host) &body body)
  `(mezzano.supervisor:with-mutex ((ext4-host-lock ,host))
     ,@body))

(defmethod open-using-host ((host ext4-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (with-ext4-host-locked (host)
    (let ((file-inode (find-file host pathname))
          (file-position 0)
          (file-length 0)
          (created-file nil)
          (abort-action nil))
      (if file-inode
          (setf file-length (inode-size file-inode))
          (ecase if-does-not-exist
            (:error (error 'simple-file-error
                           :pathname pathname
                           :format-control "File ~A does not exist. ~S"
                           :format-arguments (list pathname (file-name pathname))))
            (:create (setf created-file t
                           abort-action :delete)
             (error ":create not implemented"))))
      (when (and (not created-file) (member direction '(:output :io)))
        (error ":output :io not implemented"))
      (cond ((or (eql element-type :default)
                 (subtypep element-type 'character))
             (make-instance 'ext-file-character-stream
                            :pathname pathname
                            :host host
                            :direction direction
                            :file-inode file-inode
                            :block-size (bytes-per-block (superblock host))
                            :position file-position
                            :length file-length
                            :abort-action abort-action
                            :external-format (sys.int::make-external-format 'character external-format)))
            ((and (subtypep element-type '(unsigned-byte 8))
                  (subtypep '(unsigned-byte 8) element-type))
             (assert (eql external-format :default) (external-format))
             (make-instance 'ext-file-stream
                            :pathname pathname
                            :host host
                            :direction direction
                            :file-inode file-inode
                            :block-size (bytes-per-block (superblock host))
                            :position file-position
                            :length file-length
                            :abort-action abort-action))
            (t (error "Unsupported element-type ~S." element-type))))))

(defmethod probe-using-host ((host ext4-host) pathname)
  (multiple-value-bind (inode) (find-file host pathname)
    (if inode t nil)))

(defmethod directory-using-host ((host ext4-host) pathname &key)
  (let ((inode (find-file host pathname))
        (disk (file-host-mount-device host))
        (superblock (superblock host))
        (stack '())
        (path (directory-namestring pathname)))
    (do-files (block offset) disk superblock inode t
      (let* ((file (read-linked-directory-entry block offset))
             (file-name (linked-directory-entry-name file))
             (file-type (linked-directory-entry-file-type file)))
        (unless (or (= +unknown-type+ file-type)
                    (string= "." file-name)
                    (string= ".." file-name))
          (push (parse-simple-file-path host
                                        (format nil
                                                (if (= +directory-type+ file-type)
                                                    "~a~a>"
                                                    "~a~a")
                                                path
                                                file-name))
                stack))))
    (return-from directory-using-host stack)))

;; (defmethod ensure-directories-exist-using-host ((host ext4-host) pathname &key verbose))

;; (defmethod rename-file-using-host ((host ext4-host) source dest))

;; (defmethod file-properties-using-host ((host ext4-host) path))

;; (defmethod set-file-properties-using-host ((host ext4-host) path &key))

;; (defmethod delete-file-using-host ((host ext4-host) path &key))

(defmethod expunge-directory-using-host ((host ext4-host) path &key)
  (declare (ignore host path))
  t)

(defmethod stream-truename ((stream ext-file-stream))
  (file-stream-pathname stream))

(defmethod close ((stream ext-file-stream) &key abort)
  t)

;; Register the ext4 file system host as a block device host
(register-block-device-host-type :ext4-host)
