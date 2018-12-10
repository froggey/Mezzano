;;;; Copyright (c) 2018-2018 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.
;;;; This implementation does support some ext read operations

(defpackage :mezzano.ext-file-system
  (:use :cl :mezzano.file-system :iterate)
  (:export)
  (:import-from :sys.int
                #:explode))

(in-package :mezzano.ext-file-system)

;; Compatible feature set flags.
(defconstant +compat-dir-prealloc+ #x1)
(defconstant +compat-imagic-inodes+ #x2)
(defconstant +compat-has-journal+ #x4)
(defconstant +compat-ext-attr+ #x8)
(defconstant +compat-resize-inode+ #x10)
(defconstant +compat-dir-index+ #x20)
(defconstant +compat-lazy-bg+ #x40)
(defconstant +compat-exclude-inode+ #x80)
(defconstant +compat-exclude-bitmap+ #x100)
(defconstant +compat-sparse-super2+ #x200)

;; Incompatible feature set.
(defconstant +incompat-compression+ #x1)
(defconstant +incompat-filetype+ #x2)
(defconstant +incompat-recover+ #x4)
(defconstant +incompat-journal-dev+ #x8)
(defconstant +incompat-meta-bg+ #x10)
(defconstant +incompat-extents+ #x40)
(defconstant +incompat-64bit+ #x80)
(defconstant +incompat-mmp+ #x100)
(defconstant +incompat-flex-bg+ #x200)
(defconstant +incompat-ea-inode+ #x400)
(defconstant +incompat-dirdata+ #x1000)
(defconstant +incompat-csum-seed+ #x2000)
(defconstant +incompat-largedir+ #x4000)
(defconstant +incompat-inline-data+ #x8000)
(defconstant +incompat-encrypt+ #x10000)

;; Readonly-compatible feature set.
(defconstant +ro-compat-sparse-super+ #x1)
(defconstant +ro-compat-large-file+ #x2)
(defconstant +ro-compat-btree-dir+ #x4)
(defconstant +ro-compat-huge-file+ #x8)
(defconstant +ro-compat-gdt-csum+ #x10)
(defconstant +ro-compat-dir-nlink+ #x20)
(defconstant +ro-compat-extra-isize+ #x40)
(defconstant +ro-compat-has-snapshot+ #x80)
(defconstant +ro-compat-quota+ #x100)
(defconstant +ro-compat-bigalloc+ #x200)
(defconstant +ro-compat-metadata-csum+ #x400)
(defconstant +ro-compat-replica+ #x800)
(defconstant +ro-compat-readonly+ #x1000)
(defconstant +ro-compat-project+ #x2000)

;; Directory file types
(defconstant +unknown-type+ #x0)
(defconstant +regular-file-type+ #x1)
(defconstant +directory-type+ #x2)
(defconstant +character-device-type+ #x3)
(defconstant +block-device-type+ #x4)
(defconstant +fifo-type+ #x5)
(defconstant +socket-type+ #x6)
(defconstant +symbolic-link-type+ #x7)

(defun read-sector (disk start-sector n-sectors)
  "Read n sectors from disk"
  (let* ((sector-size (mezzano.supervisor:disk-sector-size disk))
         (result (make-array (* sector-size n-sectors) :element-type '(unsigned-byte 8)))
         (temp-buf (make-array sector-size :element-type '(unsigned-byte 8) :area :wired)))
    (dotimes (offset n-sectors)
      (multiple-value-bind (successp error-reason)
          (mezzano.supervisor:disk-read disk (+ start-sector offset) 1 temp-buf)
        (when (not successp)
          (error "Disk read error: ~S" error-reason)))
      (replace result temp-buf :start1 (* offset sector-size)))
    result))

(defstruct superblock
  (inodes-count nil :type (unsigned-byte 32))
  (blocks-count nil :type (unsigned-byte 32))
  (r-blocks-count nil :type (unsigned-byte 32))
  (free-blocks-count nil :type (unsigned-byte 32))
  (free-inodes-count nil :type (unsigned-byte 32))
  (first-data-block nil :type (unsigned-byte 32))
  (log-block-size nil :type (unsigned-byte 32))
  (log-frag-size nil :type (unsigned-byte 32))
  (blocks-per-group nil :type (unsigned-byte 32))
  (frags-per-group nil :type (unsigned-byte 32))
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
  ;; EXT2-DYNAMIC-REV Specific
  (first-ino nil :type (unsigned-byte 32))
  (inode-size nil :type (unsigned-byte 16))
  (block-group-nr nil :type (unsigned-byte 16))
  (feature-compat nil :type (unsigned-byte 32))
  (feature-incompat nil :type (unsigned-byte 32))
  (feature-ro-compat nil :type (unsigned-byte 32))
  (uuid nil :type (unsigned-byte 128))
  (volume-name nil :type string)
  (last-mounted nil :type string)
  (algo-bitmap nil :type (unsigned-byte 32))
  ;; Performance Hints
  (prealloc-blocks nil :type (unsigned-byte 8))
  (prealloc-dir-blocks nil :type (unsigned-byte 8))
  ;; Journaling Support
  (journal-uuid nil :type (unsigned-byte 128))
  (journal-inum nil :type (unsigned-byte 32))
  (journal-dev nil :type (unsigned-byte 32))
  (last-orphan nil :type (unsigned-byte 32))
  ;; Directory Indexing Support
  (hash-seed nil)
  (def-hash-version nil :type (unsigned-byte 8))
  ;; Other options
  (default-mount-options nil :type (unsigned-byte 32))
  (first-meta-bg nil :type (unsigned-byte 32)))

(defun check-magic (magic)
  (unless (= magic #xEF53)
    (error "Bad magic : #x~x.
  Valid magic value is #xEF53." magic)))

(defun check-feature-incompat (feature-incompat)
  (unless (= #x2 (logand #b11111011111011111 feature-incompat))
    (error "Some feature not implemented from Incompatible feature set~%~b"
           (- #b11111011111011111 feature-incompat))))

(defun read-superblock (disk)
  (let* ((superblock (read-sector disk 2 2))
         (magic (sys.int::ub16ref/le superblock 56))
         (feature-incompat (sys.int::ub32ref/le superblock 96)))
    (check-magic magic)
    (check-feature-incompat feature-incompat)
    (make-superblock :inodes-count (sys.int::ub32ref/le superblock 0)
                     :blocks-count (sys.int::ub32ref/le superblock 4)
                     :r-blocks-count (sys.int::ub32ref/le superblock 8)
                     :free-blocks-count (sys.int::ub32ref/le superblock 12)
                     :free-inodes-count (sys.int::ub32ref/le superblock 16)
                     :first-data-block (sys.int::ub32ref/le superblock 20)
                     :log-block-size (sys.int::ub32ref/le superblock 24)
                     :log-frag-size (sys.int::ub32ref/le superblock 28)
                     :blocks-per-group (sys.int::ub32ref/le superblock 32)
                     :frags-per-group (sys.int::ub32ref/le superblock 36)
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
                     :uuid (logior (ash (sys.int::ub64ref/le superblock 112) 64)
                                   (sys.int::ub64ref/le superblock 104))
                     :volume-name (map 'string #'code-char (subseq superblock 120 136))
                     :last-mounted (map 'string #'code-char (subseq superblock 136 200))
                     :algo-bitmap (sys.int::ub32ref/le superblock 200)
                     :prealloc-blocks (aref superblock 204)
                     :prealloc-dir-blocks (aref superblock 205)
                     :journal-uuid (logior (ash (sys.int::ub64ref/le superblock 216) 64)
                                           (sys.int::ub64ref/le superblock 208))
                     :journal-inum (sys.int::ub32ref/le superblock 224)
                     :journal-dev (sys.int::ub32ref/le superblock 228)
                     :last-orphan (sys.int::ub32ref/le superblock 232)
                     :hash-seed (make-array '(4) :element-type '(unsigned-byte 32)
                                                 :initial-contents (list (sys.int::ub32ref/le superblock 236)
                                                                         (sys.int::ub32ref/le superblock 240)
                                                                         (sys.int::ub32ref/le superblock 244)
                                                                         (sys.int::ub32ref/le superblock 248)))
                     :def-hash-version (aref superblock 252)
                     :default-mount-options (sys.int::ub32ref/le superblock 256)
                     :first-meta-bg (sys.int::ub32ref/le superblock 260))))

(defun block-size (disk superblock)
  "Take disk and superblock, return block size in disk sectors"
  (/ (ash 1024 (superblock-log-block-size superblock))
     (mezzano.supervisor:disk-sector-size disk)))

(defun read-block (disk superblock block-n &optional (n-blocks 1))
  (let* ((block-size (block-size disk superblock))
         (sector-n (* block-size
                      (if (= (superblock-log-block-size superblock) 1)
                          (1+ block-n) block-n))))
    (read-sector disk sector-n (* n-blocks block-size))))

(defun block-size-in-bytes (disk superblock)
  "Take disk and superblock, return block size in bytes"
  (ash 1024 (superblock-log-block-size superblock)))

(defun block-group (inode-n superblock)
  "Return block group that an inode lives in"
  (floor (/ (1- inode-n) (superblock-inodes-per-group superblock))))

(defun index (inodee-n superblock)
  "Return index of an inode"
  (mod (1- inodee-n) (superblock-inodes-per-group superblock)))

(defun offset (inode-n superblock)
  "Return byte address within the inode table"
  (* (index inode-n superblock) (superblock-inode-size superblock)))

(defun n-block-groups (superblock)
  (let ((tmp (ceiling (/ (superblock-inodes-count superblock) (superblock-inodes-per-group superblock)))))
    (assert (= tmp (ceiling (/ (superblock-blocks-count superblock) (superblock-blocks-per-group superblock)))))
    (ceiling (/ (superblock-blocks-count superblock) (superblock-blocks-per-group superblock)))))

(defstruct block-group-descriptor
  (block-bitmap nil :type (unsigned-byte 32))
  (inode-bitmap nil :type (unsigned-byte 32))
  (inode-table nil :type (unsigned-byte 32))
  (free-blocks-count nil :type (unsigned-byte 16))
  (free-inodes-count nil :type (unsigned-byte 16))
  (used-dirs-count nil :type (unsigned-byte 16)))

(defun read-block-group-descriptor (block offset)
  (make-block-group-descriptor :block-bitmap (sys.int::ub32ref/le block (+ 0 offset))
                               :inode-bitmap (sys.int::ub32ref/le block (+ 4 offset))
                               :inode-table (sys.int::ub32ref/le block (+ 8 offset))
                               :free-blocks-count (sys.int::ub16ref/le block (+ 12 offset))
                               :free-inodes-count (sys.int::ub16ref/le block (+ 14 offset))
                               :used-dirs-count (sys.int::ub16ref/le block (+ 16 offset))))

(defun read-block-group-descriptor-table (disk superblock)
  (make-array (list (n-block-groups superblock)) :initial-contents
              (iter (with n-octets := (* 32 (n-block-groups superblock)))
                (with block := (read-block disk superblock 1
                                           (/ n-octets
                                              (mezzano.supervisor:disk-sector-size disk)
                                              (block-size disk superblock))))
                (for offset :from 0 :below n-octets :by 32)
                (collecting (read-block-group-descriptor block offset)))))

(defun read-block-bitmap (disk superblock bgds)
  (read-block disk superblock (block-group-descriptor-block-bitmap bgds)))

(defun read-inode-bitmap (disk superblock bgds)
  (read-block disk superblock (block-group-descriptor-inode-bitmap bgds)))

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
         (block (read-block disk superblock (+ (block-group-descriptor-inode-table bgds)
                                               (floor (/ (offset inode-n superblock)
                                                         (block-size-in-bytes disk superblock))))))
         (offset (mod (offset inode-n superblock) (block-size-in-bytes disk superblock))))
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
                :block (iter (for i :from (+ 40 offset) :to (+ 96 offset) :by 4)
                         (collecting (sys.int::ub32ref/le block i)))
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
  (let* ((name-len (aref block (+ 6 offset))))
    (make-linked-directory-entry :inode (sys.int::ub32ref/le block (+ 0 offset))
                                 :rec-len (sys.int::ub16ref/le block (+ 4 offset))
                                 :name-len name-len
                                 :file-type (aref block (+ 7 offset))
                                 :name (map 'string #'code-char
                                            (subseq block
                                                    (+ 8 offset)
                                                    (+ 8 offset name-len))))))

(defun follow-pointer (disk superblock block-n fn n-indirection)
  (if (zerop n-indirection)
      (funcall fn (read-block disk superblock block-n))
      (iter (with i-block := (read-block disk superblock block-n))
        (for offset :from 0 :below (block-size-in-bytes disk superblock) :by 4)
        (for block-n := (sys.int::ub32ref/le i-block offset))
        (unless (and (zerop block-n))
          (follow-pointer disk superblock block-n fn (1- n-indirection))))))

(defun do-file (fn disk superblock bgdt inode-n)
  (let* ((inode-block (inode-block (read-inode disk superblock bgdt inode-n))))
    (iter (for block-n := (pop inode-block))
      (for n :from 0 :to 10)
      (unless (and (zerop block-n))
        (follow-pointer disk superblock block-n fn 0)))

    (let ((block-n (pop inode-block)))
      (unless (and (zerop block-n))
        (follow-pointer disk superblock block-n fn 1)))

    (let ((block-n (pop inode-block)))
      (unless (and (zerop block-n))
        (follow-pointer disk superblock block-n fn 2)))

    (let ((block-n (pop inode-block)))
      (unless (and (zerop block-n))
        (follow-pointer disk superblock block-n fn 3)))))

(defun read-file (disk superblock bgdt inode-n)
  (let ((blocks))
    (do-file #'(lambda (block)
                 (push block blocks))
      disk superblock bgdt inode-n)
    (iter (with block-size := (block-size-in-bytes disk superblock))
      (with result := (make-array (list (* block-size (length blocks))) :element-type '(unsigned-byte 8)))
      (for block :in blocks)
      (for offset :from 0 :by block-size)
      (replace result block :start1 offset)
      (finally (return result)))))

(defmacro do-files ((arg var) disk superblock bgdt inode-n finally &body body)
  `(unless (do-file (lambda (,arg)
                      (do ((,var 0 (+ ,var (sys.int::ub16ref/le ,arg (+ 4 ,var)))))
                          ((= ,var (block-size-in-bytes disk superblock)) nil)
                        ,@body))
             ,disk ,superblock ,bgdt ,inode-n)
     ,finally))

;;; Host integration

(defclass ext-host ()
  ((%name :initarg :name
          :reader host-name)
   (%lock :initarg :lock
          :reader ext-host-lock)
   (partition :initarg :partition
              :reader partition)
   (superblock :initarg :superblock
               :reader superblock)
   (bgdt :initarg :bgdt
         :reader bgdt))
  (:default-initargs :lock (mezzano.supervisor:make-mutex "Local File Host lock")))

(defmethod host-default-device ((host ext-host))
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

(defmethod parse-namestring-using-host ((host ext-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (parse-simple-file-path host namestring))

(defmethod namestring-using-host ((host ext-host) pathname)
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
  (loop :with disk := (partition host)
        :with superblock := (superblock host)
        :with bgdt := (bgdt host)
        :with inode-n := 2
        :with file-name := (file-name pathname)
        :for directory :in (rest (pathname-directory pathname))
        :do (block do-files
              (do-files (block offset) disk superblock bgdt inode-n
                        (error 'simple-file-error
                               :pathname pathname
                               :format-control "Directory ~A not found. ~S"
                               :format-arguments (list directory pathname))
                (when (string= directory (linked-directory-entry-name (read-linked-directory-entry block offset)))
                  (setf inode-n (linked-directory-entry-inode (read-linked-directory-entry block offset)))
                  (return-from do-files t))))
        :finally
        (if (null file-name)
            (return inode-n)
            (block do-files
              (do-files (block offset) disk superblock bgdt inode-n nil
                (when (string= file-name (linked-directory-entry-name (read-linked-directory-entry block offset)))
                  (return-from find-file (linked-directory-entry-inode (read-linked-directory-entry block offset)))))))))

(defclass ext-file-stream (sys.gray:fundamental-binary-input-stream
                           sys.gray:fundamental-binary-output-stream
                           file-stream)
  ((pathname :initarg :pathname :reader file-stream-pathname)
   (host :initarg :host :reader host)
   (direction :initarg :direction :reader direction)
   (file-inode :initarg :file-inode :accessor file-inode)
   (buffer :initarg :buffer :accessor buffer)
   (buffer-offset :initarg :buffer-offset :accessor buffer-offset)
   (abort-action :initarg :abort-action :accessor abort-action)))

(defclass ext-file-character-stream (sys.gray:fundamental-character-input-stream
                                     sys.gray:fundamental-character-output-stream
                                     ext-file-stream
                                     sys.gray:unread-char-mixin)
  ())

(defmacro with-ext-host-locked ((host) &body body)
  `(mezzano.supervisor:with-mutex ((ext-host-lock ,host))
     ,@body))

;; WIP
(defmethod open-using-host ((host ext-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (with-ext-host-locked (host)
    (let ((file-inode nil)
          (buffer nil)
          (buffer-offset 0)
          (created-file nil)
          (abort-action nil))
      (let ((inode-n (find-file host pathname)))
        (if inode-n
            (setf file-inode (read-inode (partition host) (superblock host) (bgdt host) inode-n)
                  buffer (read-file (partition host) (superblock host) (bgdt host) inode-n))
            (ecase if-does-not-exist
              (:error (error 'simple-file-error
                             :pathname pathname
                             :format-control "File ~A does not exist. ~S"
                             :format-arguments (list pathname (file-name pathname))))
              (:create (setf created-file t
                             abort-action :delete)
               (error ":create not implemented")))))
      (when (and (not created-file) (member direction '(:output :io)))
        (error ":output :io not implemented"))
      (let ((stream (cond ((or (eql element-type :default)
                               (subtypep element-type 'character))
                           (assert (member external-format '(:default :utf-8))
                                   (external-format))
                           (make-instance 'ext-file-character-stream
                                          :pathname pathname
                                          :host host
                                          :direction direction
                                          :file-inode file-inode
                                          :buffer buffer
                                          :buffer-offset buffer-offset
                                          :abort-action abort-action))
                          ((and (subtypep element-type '(unsigned-byte 8))
                                (subtypep '(unsigned-byte 8) element-type))
                           (assert (eql external-format :default) (external-format))
                           (make-instance 'ext-file-stream
                                          :pathname pathname
                                          :host host
                                          :direction direction
                                          :file-inode file-inode
                                          :buffer buffer
                                          :buffer-offset buffer-offset
                                          :abort-action abort-action))
                          (t (error "Unsupported element-type ~S." element-type)))))
        stream))))

(defmethod probe-using-host ((host ext-host) pathname)
  (multiple-value-bind (inode-n) (find-file host pathname)
    (if inode-n t nil)))

;; WIP
(defmethod directory-using-host ((host ext-host) pathname &key)
  (let ((inode-n (find-file host pathname))
        (disk (partition host))
        (superblock (superblock host))
        (bgdt (bgdt host))
        (stack '())
        (path (directory-namestring pathname)))
    (do-files (block offset) disk superblock bgdt inode-n t
      (let* ((file (read-linked-directory-entry block offset))
             (type (linked-directory-entry-file-type file)))
        (unless (= +unknown-type+ type)
          (push (parse-simple-file-path host
                                        (format nil
                                                (if (= +directory-type+ type)
                                                    "~a~a>"
                                                    "~a~a")
                                                path
                                                (linked-directory-entry-name file)))
                stack))))
    (return-from directory-using-host stack)))

;; (defmethod ensure-directories-exist-using-host ((host ext-host) pathname &key verbose))

;; (defmethod rename-file-using-host ((host ext-host) source dest))

;; (defmethod file-write-date-using-host ((host ext-host) path))

;; (defmethod delete-file-using-host ((host ext-host) path &key))

(defmethod expunge-directory-using-host ((host ext-host) path &key)
  (declare (ignore host path))
  t)

(defmethod stream-truename ((stream ext-file-stream))
  (file-stream-pathname stream))

(defmethod sys.gray:stream-element-type ((stream ext-file-stream))
  '(unsigned-byte 8))

(defmethod sys.gray:stream-element-type ((stream ext-file-character-stream))
  'character)

(defmethod sys.gray:stream-external-format ((stream ext-file-stream))
  :default)

(defmethod sys.gray:stream-external-format ((stream ext-file-character-stream))
  :utf-8)

(defmethod input-stream-p ((stream ext-file-stream))
  (member (direction stream) '(:input :io)))

(defmethod output-stream-p ((stream ext-file-stream))
  (member (direction stream) '(:output :io)))

;; (defmethod sys.gray:stream-write-byte ((stream ext-file-stream) byte)
;;   (assert (member (direction stream) '(:output :io)))
;;   (error "Not implemented"))

(defmethod sys.gray:stream-read-byte ((stream ext-file-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((offset (buffer-offset stream)))
    (incf (buffer-offset stream))
    (if (<= (inode-size (file-inode stream))
            offset)
        :eof
        (aref (buffer stream) offset))))

;; (defmethod sys.gray:stream-write-sequence ((stream ext-file-stream) sequence &optional (start 0) end)
;;   (assert (member (direction stream) '(:output :io)))
;;   (error "Not implemented"))

(defmethod sys.gray:stream-read-sequence ((stream ext-file-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((end2 (min end (inode-size (file-inode stream)))))
    (replace sequence (buffer stream) :start1 start :end1 end :start2 0 :end2 end2)
    end2))

;; (defmethod sys.gray:stream-write-char ((stream ext-file-character-stream) char)
;;   (assert (member (direction stream) '(:output :io)))
;;   (error "Not implemented"))

(defmethod sys.gray:stream-read-char ((stream ext-file-character-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((offset (buffer-offset stream)))
    (incf (buffer-offset stream))
    (if (<= (inode-size (file-inode stream))
            offset)
        :eof
        (code-char (aref (buffer stream) offset)))))

;; (defmethod sys.gray:stream-write-sequence ((stream ext-file-character-stream) sequence &optional (start 0) end)
;;   (assert (member (direction stream) '(:output :io)))
;;   (error "Not implemented"))

(defmethod sys.gray:stream-read-sequence ((stream ext-file-character-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((end2 (min end (inode-size (file-inode stream)))))
    (loop :for n1 :from start :to (1- end)
          :for n2 :to (1- end2)
          :do (setf (aref sequence n1)
                    (code-char (aref (buffer stream) n2))))
    end2))

(defmethod sys.gray:stream-file-position ((stream ext-file-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (setf (buffer-offset stream) (case position-spec
                                        (:start 0)
                                        (:end (inode-size (file-inode stream)))
                                        (t position-spec))))
        (t (buffer-offset stream))))

(defmethod sys.gray:stream-file-length ((stream ext-file-stream))
  (inode-size (file-inode stream)))

(defmethod close ((stream ext-file-stream) &key abort)
  t)

;;; testing
;; Mount partition
;; (let* ((disk-name "EXT2")
;;        (disk (nth 3 (mezzano.supervisor:all-disks)))
;;        (superblock (read-superblock disk))
;;        (bgdt (read-block-group-descriptor-table disk superblock))
;;        (instance (make-instance 'ext-host
;;                                 :name disk-name
;;                                 :partition disk
;;                                 :superblock superblock
;;                                 :bgdt bgdt)))
;;   (setf (mezzano.file-system:find-host disk-name)
;;         instance))

;; (let ((path #P"EXT2:>Mezzano>supervisor>acpi.lisp"))
;;   ;; Read some file
;;   (time
;;    (with-open-file (file path)
;;      (loop for byte = (read-byte file nil nil)
;;            while byte do (write-char (code-char byte))))))
