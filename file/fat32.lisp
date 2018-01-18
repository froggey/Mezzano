;;;; Copyright (c) 2017-2018 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.
;;;; For now only support reading fat32 FS.

(defpackage :mezzano.fat32-file-system
  (:use :cl :mezzano.file-system)
  (:export #:read-fat32-structure
           #:read-fat32-info-structure
           #:read-sector
           #:read-cluster
           #:read-file
           #:file-p
           #:read-dir-from-file
           #:read-dir-from-cluster
           #:decode-time
           #:decode-date))

(in-package :mezzano.fat32-file-system)

(defconstant +bootable-partition-signature+ #xAA55)

(defun read-fat32-structure (sector)
  (let ((boot-jmp (subseq sector 0 3))
        (bytes-per-sector (sys.int::ub16ref/le sector 11))
        (sectors-per-cluster (aref sector 13))
        (table-count (aref sector 16))
        (root-entry-count (sys.int::ub16ref/le sector 17))
        (total-sectors16 (sys.int::ub16ref/le sector 19))
        (media-type (aref sector 21))
        (table-size-16 (sys.int::ub16ref/le sector 22))
        (total-sectors32 (sys.int::ub32ref/le sector 32))
        (fat-type-label (map 'string #'code-char (subseq sector 82 90)))
        (bps (sys.int::ub16ref/le sector 510)))
    (check-boot-jmp boot-jmp)
    (check-bytes-per-sector bytes-per-sector)
    (check-sectors-per-cluster sectors-per-cluster bytes-per-sector)
    (assert (>= table-count 1))
    (assert (= 0 root-entry-count))
    (assert (= 0 total-sectors16))
    (check-media-type media-type)
    (assert (= table-size-16 0))
    (assert (not (zerop total-sectors32)))
    (check-fat-type-label32 fat-type-label)
    (check-bps bps)
    (values
     (make-fat32
      :boot-jmp boot-jmp
      :oem-name (map 'string #'code-char (subseq sector 3 11))
      :bytes-per-sector bytes-per-sector
      :sectors-per-cluster sectors-per-cluster
      :reserved-sector-count (sys.int::ub16ref/le sector 14)
      :table-count table-count
      :root-entry-count root-entry-count
      :total-sectors16 total-sectors16
      :media-type media-type
      :table-size-16 table-size-16
      :sectors-per-track (sys.int::ub16ref/le sector 24)
      :head-side-count (sys.int::ub16ref/le sector 26)
      :hidden-sector-count (sys.int::ub32ref/le sector 28)
      :total-sectors32 total-sectors32
      :table-size-32 (sys.int::ub32ref/le sector 36)
      :extended-flags (sys.int::ub16ref/le sector 40)
      ;; high byte is major revision number and low byte is minor revision number
      :fat-version (sys.int::ub16ref/le sector 42)
      :root-cluster (sys.int::ub32ref/le sector 44)
      :fat-info (sys.int::ub16ref/le sector 48)
      :backup-BS-sector (sys.int::ub16ref/le sector 50)
      :reserved-0 (logior (ash (sys.int::ub32ref/le sector 60) 64)
                          (sys.int::ub64ref/le sector 52))
      :drive-number (aref sector 64) ; Operating system specific
      :reserved-1 (aref sector 65)
      :boot-signature (aref sector 66)
      :volume-id (sys.int::ub32ref/le sector 67)
      :volume-label (map 'string #'code-char (subseq sector 71 82))
      :fat-type-label fat-type-label
      
      :bc nil
      :bps bps))))

(defun check-boot-jmp (boot-jmp)
  (let ((n0 (aref boot-jmp 0))
        (n2 (aref boot-jmp 2)))
    (if (or (and (= n0 #xEB)
                 (= n2 #x90))
            (= n0 #xE9))
        boot-jmp
        (error "Bad boot-jmp : ~a .
Valid forms are : #(#xEB x #x90) and #(#xE9 x x).
X is for some 1 byte number." boot-jmp))))

(defun check-bytes-per-sector (bytes-per-sector)
  "Ensure that bytes-per-sector is valid."
  (loop :for n :in '(512 1024 2048 4096)
     :do (when (= n bytes-per-sector)
           (return bytes-per-sector))
     :finally (error "Bad bytes-per-sector : ~a .
Valid bytes-per-sector are 512, 1024, 2048 and 4096" bytes-per-sector)))

(defun check-sectors-per-cluster (sectors-per-cluster bytes-per-sector)
  (loop :for n :in '(1 2 4 8 16 32 64 128)
     :do (when (= n sectors-per-cluster)
           (return sectors-per-cluster))
     :finally (error "Bad sectors-per-cluster : ~a .
Valid bytes-per-sector are 1,2,4,8,16,32,64,128" sectors-per-cluster))
  (when (> (* sectors-per-cluster bytes-per-sector)
           32768)
    (error "Error sectors-per-cluster * bytes-per-sector > 32KiB .
sectors-per-cluster= ~a bytes-per-sector= ~a" sectors-per-cluster bytes-per-sector))
  sectors-per-cluster)

(defun check-media-type (media-type)
  (loop :for n :in '(#xF0 #xF8 #xF9 #xFA #xFB #xFC #xFD #xFE #xFF)
     :do (when (= n media-type)
           (return media-type))
     :finally (error "Bad media-type : ~a .
Valid media-type ara #xF0 #xF8 #xF9 #xFA #xFB #xFC #xFD #xFE #xFF" media-type)))

(defun check-fat-type-label32 (fat-type-label)
  (if (string= "FAT32   " fat-type-label)
      fat-type-label
      (error "Bad fat-type-label : ~a .
Valid media-type ara 'FAT32   ' " fat-type-label)))

(defun check-bps (bps)
  (unless (= bps +bootable-partition-signature+)
    (error "Bad bps : ~a .
Valid bps are ~a" bps +bootable-partition-signature+))
  bps)

(defstruct fat32
  (boot-jmp nil)
  (oem-name nil :type string)
  (bytes-per-sector nil :type (unsigned-byte 16))
  (sectors-per-cluster nil :type (unsigned-byte 8))
  (reserved-sector-count nil :type (unsigned-byte 16))
  (table-count nil :type (unsigned-byte 8))
  (root-entry-count nil :type (unsigned-byte 16))
  (total-sectors16 nil :type (unsigned-byte 16))
  (media-type nil :type (unsigned-byte 8))
  (table-size-16 nil :type (unsigned-byte 16))
  (sectors-per-track nil :type (unsigned-byte 16))
  (head-side-count nil :type (unsigned-byte 16))
  (hidden-sector-count nil :type (unsigned-byte 32))
  (total-sectors32 nil :type (unsigned-byte 32))
  ;;fat32
  (table-size-32 nil :type (unsigned-byte 32))
  (extended-flags nil)
  (fat-version nil)
  (root-cluster nil :type (unsigned-byte 32))
  (fat-info nil :type (unsigned-byte 16))
  (backup-BS-sector nil :type (unsigned-byte 16))
  (reserved-0 nil)
  (drive-number nil :type (unsigned-byte 8))
  (reserved-1 nil)
  (boot-signature nil :type (unsigned-byte 8))
  (volume-id nil :type (unsigned-byte 32))
  (volume-label nil :type string)
  (fat-type-label nil :type string)
  (bc nil) ; 90 420
  (bps nil :type (unsigned-byte 32)))

(defconstant +lead-signature+ #x41615252)
(defconstant +structure-signature+ #x61417272)
(defconstant +trail-signature+ #xAA550000)

(defun read-fat32-info-structure (sector)
  (let ((lead-signature (sys.int::ub32ref/le sector 0))
        (structure-signature (sys.int::ub32ref/le sector 484))
        (trail-signature (sys.int::ub32ref/le sector 508)))
    (check-lead-signature lead-signature)
    (check-structure-signature structure-signature)
    (check-trail-signature trail-signature)
    (make-fs-info
     :lead-signature lead-signature
     :reserved-0 nil
     :structure-signature structure-signature
     :last-free-cluster (sys.int::ub32ref/le sector 488)
     :next-free-cluster (sys.int::ub32ref/le sector 492)
     :reserved-1 nil
     :trail-signature trail-signature)))

(defun check-lead-signature (lead-signature)
  (if (= lead-signature +lead-signature+)
      lead-signature
      (error "Bad lead-signature : ~a .
Valid lead-signature is ~a" lead-signature +lead-signature+)))

(defun check-structure-signature (structure-signature)
  (if (= structure-signature +structure-signature+)
      structure-signature
      (error "Bad structure-signature : ~a .
Valid structure-signature is ~a" structure-signature +structure-signature+)))

(defun check-trail-signature (trail-signature)
  (if (= trail-signature +trail-signature+)
      trail-signature
      (error "Bad trail-signature : ~a .
Valid trail-signature is ~a" trail-signature +trail-signature+)))

(defstruct fs-info
  (lead-signature nil :type (unsigned-byte 32))
  (reserved-0 nil) ; 4 480
  (structure-signature nil :type (unsigned-byte 32))
  (last-free-cluster nil :type (unsigned-byte 32))
  (next-free-cluster nil :type (unsigned-byte 32))
  (reserved-1 nil) ; 496 12
  (trail-signature nil :type (unsigned-byte 32)))

(defun read-sector (disk sector &optional (n 1))
  (let* ((array (make-array (* n (mezzano.supervisor:disk-sector-size disk)) :area :wired
                            :element-type '(unsigned-byte 8))))
    (mezzano.supervisor:disk-read disk
                                  sector
                                  n
                                  array)
    array))

(defun read-cluster (fat32 disk n)
  (let* ((spc (fat32-sectors-per-cluster fat32))
         (array (make-array (* spc (mezzano.supervisor:disk-sector-size disk)) :area :wired
                            :element-type '(unsigned-byte 8))))
    (mezzano.supervisor:disk-read disk
                                  n
                                  spc
                                  array)
    array))

;;; bit offset
(defconstant +attribute-read-only+ 0)
(defconstant +attribute-hidden+ 1)
(defconstant +attribute-system+ 2)
(defconstant +attribute-volume-id+ 3)
(defconstant +attribute-directory+ 4)
(defconstant +attribute-archive+ 5)

(defun read-file (file disk fat32)
  (when (file-p file)
    (read-sector disk
                 (first-sector-of-cluster fat32
                                          (virtual-dir-first-cluster file))
                 (+ 2 (file-size-in-sectors file fat32)))))

(defun file-p (file)
  (let* ((n (virtual-dir-attributes file))
         (attribute-bit (ldb (byte 1 +attribute-directory+) n)))
    (if (= attribute-bit 1)
        nil file)))

(defun read-dir-from-file (fat32 disk file)
  (let ((cluster (read-cluster fat32
                               disk
                               (first-sector-of-cluster fat32
                                                        (virtual-dir-first-cluster file)))))
    (read-dir-from-cluster cluster fat32)))

(defun read-dir-from-cluster (cluster fat32)
  (loop :for i :from 0 :to (1- (/ (* (fat32-sectors-per-cluster fat32)
                                     (fat32-bytes-per-sector fat32))
                                  32))
     :with stack := nil
     :with res := nil
     :do
     (let ((first-byte (aref cluster (* i 32))))
       (unless  (or (zerop first-byte)
                    (= #xE5 first-byte))
         (if (= #x0F (aref cluster (+ 11 (* i 32))))
             (push (make-long-dir
                    :order (aref cluster (* i 32))
                    :name-0 (subseq cluster
                                    (+ 1 (* i 32))
                                    (+ 11 (* i 32)))
                    :attributes (aref cluster (+ 11 (* i 32)))
                    :type (aref cluster (+ 12 (* i 32)))
                    :checksum (aref cluster (+ 13 (* i 32)))
                    :name-1 (subseq cluster
                                    (+ 14 (* i 32))
                                    (+ 26 (* i 32)))
                    :first-cluster-low (sys.int::ub16ref/le cluster (+ 26 (* i 32)))
                    :name-2 (subseq cluster
                                    (+ 28 (* i 32))
                                    (+ 32 (* i 32))))
                   stack)
             (progn
               (push
                (decode-dir
                 (push (make-dir
                        :name (map 'string #'code-char
                                   (subseq cluster (* i 32)
                                           (+ 11 (* i 32))))
                        :attributes (aref cluster (+ 11 (* i 32)))
                        :reserved nil
                        :time-tenth (aref cluster (+ 13 (* i 32)))
                        :time (subseq cluster (+ 14 (* i 32)) (+ 16 (* i 32)))
                        :date (subseq cluster (+ 16 (* i 32)) (+ 18 (* i 32)))
                        :access-date (subseq cluster (+ 18 (* i 32)) (+ 20 (* i 32)))
                        :first-cluster-high (sys.int::ub16ref/le cluster (+ 20 (* i 32)))
                        :write-time (subseq cluster (+ 22 (* i 32)) (+ 24 (* i 32)))
                        :write-date (subseq cluster (+ 24 (* i 32)) (+ 26 (* i 32)))
                        :first-cluster-low (sys.int::ub16ref/le cluster (+ 26 (* i 32)))
                        :file-size (sys.int::ub32ref/le cluster (+ 28 (* i 32))))
                       stack)
                 fat32)
                res)
               (setf stack nil)))))
     :finally (return res)))

(defun decode-dir (list fat32)
  (loop :for file :in list
     :with name := ""
     :with attributes
     :with time-tenth
     :with time
     :with date
     :with access-date
     :with write-time
     :with write-date
     :with first-cluster
     :with file-size
     :do (if (long-dir-p file)
             (loop for octet across (concatenate 'vector
                                                 (long-dir-name-0 file)
                                                 (long-dir-name-1 file)
                                                 (long-dir-name-2 file))
                :unless (or (= octet 0)
                            (= octet 255))
                :do (setf name
                          (concatenate 'string name
                                       (format nil "~a" (code-char octet)))))
             (setf attributes (dir-attributes file)
                   time-tenth (dir-time-tenth file)
                   time (dir-time file)
                   date (dir-date file)
                   access-date (dir-access-date file)
                   write-time (dir-write-time file)
                   write-date (dir-write-date file)
                   first-cluster (logior (ash (dir-first-cluster-high file) 16)
                                         (ash (dir-first-cluster-low file) 0))
                   file-size (dir-file-size file)))
     :finally (return
                (make-virtual-dir
                 :name name
                 :attributes attributes
                 :time-tenth time-tenth
                 :time time
                 :date date
                 :access-date access-date
                 :write-time write-time
                 :write-date write-date
                 :first-cluster first-cluster
                 :file-size file-size))))

;;; bits 0-4 2-second count, 5-10 minutes, 11-15 hours
(defun decode-time (array)
  (let ((n (sys.int::ub16ref/le array 0)))
    (values (ldb (byte 5 0) n)
            (ldb (byte 6 5) n)
            (ldb (byte 5 11) n))))

;;; bits 0-4 day of month, 5-8 month of year, 9-15 count of years from 1980
(defun decode-date (array)
  (let ((n (sys.int::ub16ref/le array 0)))
    (values (ldb (byte 5 0) n)
            (ldb (byte 4 5) n)
            (ldb (byte 7 9) n))))

(defstruct virtual-dir
  (name nil)
  (attributes nil :type (unsigned-byte 8))
  (time-tenth nil :type (unsigned-byte 8))
  (time nil :type vector)
  (date nil :type vector)
  (access-date nil :type vector)
  (write-time nil :type vector)
  (write-date nil :type vector)
  (first-cluster nil :type (unsigned-byte 32))
  (file-size nil :type (unsigned-byte 32)))

(defstruct dir
  (name nil)
  (attributes nil :type (unsigned-byte 8))
  (reserved nil) ; 12 1
  (time-tenth nil :type (unsigned-byte 8))
  (time nil :type vector)
  (date nil :type vector)
  (access-date nil :type vector)
  (first-cluster-high nil :type (unsigned-byte 16))
  (write-time nil :type vector)
  (write-date nil :type vector)
  (first-cluster-low nil :type (unsigned-byte 16))
  (file-size nil :type (unsigned-byte 32)))

(defstruct long-dir
  (order nil :type (unsigned-byte 8))
  (name-0 nil :type vector)
  (attributes nil :type (unsigned-byte 8))
  (type nil :type (unsigned-byte 8))
  (checksum nil :type (unsigned-byte 8))
  (name-1 nil :type vector)
  (first-cluster-low nil :type (unsigned-byte 16))
  (name-2 nil :type vector))

(defun root-dir-sectors (fat32)
  0 ;for fat32 is always 0
  ;; (ceiling
  ;;  (/ (+ (* (fat32-root-entry-count fat32) 32)
  ;;        (1- (fat32-bytes-per-sector fat32)))
  ;;     (fat32-bytes-per-sector fat32)))
  )

(defun data-sectors (fat32)
  (- (fat32-total-sectors32 fat32)
     (+ (fat32-reserved-sector-count fat32)
        (* (fat32-table-count fat32)
           (fat32-table-size-32 fat32))
        (root-dir-sectors fat32))))

(defun total-clusters (fat32)
  (floor
   (/ (data-sectors fat32)
      (fat32-sectors-per-cluster fat32))))

(defun first-data-sector (fat32)
  (+ (fat32-reserved-sector-count fat32)
     (* (fat32-table-count fat32)
        (fat32-table-size-32 fat32))
     (root-dir-sectors fat32)))

(defun first-root-dir-sector (fat32)
  (- (first-data-sector fat32)
     (root-dir-sectors fat32)))

(defun first-sector-of-cluster (fat32 cluster)
  (+ (* (- cluster 2)
        (fat32-sectors-per-cluster fat32))
     (first-data-sector fat32)))

(defun file-size-in-sectors (file fat32)
  (ceiling
   (/ (virtual-dir-file-size file)
      (fat32-bytes-per-sector fat32))))

;;;; Host integration

(defclass fat32-host ()
  ((%name :initarg :name
          :reader host-name)
   (%lock :initarg :lock
          :reader fat32-host-lock)
   (partition :initarg :partition
              :reader partition)
   (fat32-structure :initarg :fat32-structure
                    :reader fat32-structure)
   (fat32-info :initarg :fat32-info
               :reader fat32-info))
  (:default-initargs :lock (mezzano.supervisor:make-mutex "Local File Host lock")))

(defmethod host-default-device ((host fat32-host))
  nil)

(defmethod parse-namestring-using-host ((host fat32-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (let ((start 0)
        (end (length namestring))
        (directory '())
        (name "")
        (type ""))

    (loop for i from start to (1- end)
       for char = (aref namestring i)
       :do (when (char= char #\>)
             (unless (< i 1)
               (progn (push (subseq namestring (1+ start) i) directory)
                      (setf start i)))))

    (loop for i from start to (1- end)
       for char = (aref namestring i)
       :initially (setf name (subseq namestring (1+ start) end))
       :do (cond ((char= char #\.)
                  (progn (setf name (subseq namestring (1+ start) i)
                               type (subseq namestring (1+ i) end))
                         (return)))))

    (make-pathname :host host
                   :directory directory
                   :name name
                   :type type
                   :version :newest)))

(defmethod unparse-pathname (pathname (host fat32-host))
  (let ((dir (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname)))
    (values dir
            (if (string= "" type)
                name
                (format nil "~a.~a" name type)))))

(defclass fat32-file-stream (sys.gray:fundamental-binary-input-stream
                             sys.gray:fundamental-binary-output-stream
                             file-stream)
  ((path :initarg :path :reader path)
   (pathname :initarg :pathname :reader file-stream-pathname)
   (host :initarg :host :reader host)
   (direction :initarg :direction :reader direction)
   ;; Buffer itself.
   (read-buffer :initarg :read-buffer
                :accessor read-buffer)
   ;; File position where the buffer data starts.
   (read-buffer-position :initarg :read-buffer-position
                         :initform 0
                         :accessor read-buffer-position)
   ;; Current offset into the buffer.
   (read-buffer-offset :initarg :read-buffer-offset
                       :initform 0
                       :accessor read-buffer-offset)
   ;; File size
   (read-buffer-size :initarg :read-buffer-size
                     :initform 0
                     :accessor read-buffer-size)
   ;; Write buffer.
   (write-buffer :initform nil :accessor write-buffer)
   (write-buffer-position :accessor write-buffer-position)
   (write-buffer-offset :accessor write-buffer-offset)
   (abort-action :initarg :abort-action :accessor abort-action)))

(defclass fat32-file-character-stream (sys.gray:fundamental-character-input-stream
                                       sys.gray:fundamental-character-output-stream
                                       fat32-file-stream
                                       sys.gray:unread-char-mixin)
  ())

(defmacro with-fat32-host-locked ((host) &body body)
  `(mezzano.supervisor:with-mutex ((fat32-host-lock ,host))
     ,@body))

;; WIP Don't require to load entire file
(defmethod open-using-host ((host fat32-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)

  (with-fat32-host-locked (host)
    (let* ((disk (partition host))
           (fat32 (fat32-structure host))
           (buffer nil)
           (root-dir (mezzano.fat32-file-system:read-dir-from-cluster
                      (mezzano.fat32-file-system:read-cluster
                       fat32 disk
                       (mezzano.fat32-file-system::first-root-dir-sector fat32))
                      fat32))
           (read-buffer-size 0))

      (when (member direction '(:output :io))
        (error "Feature not implemented"))

      (multiple-value-bind (directory name) (unparse-pathname pathname host)
        (loop for dir-name in directory
           with dir = root-dir
           do (loop for file in dir
                 do (when (string= dir-name (virtual-dir-name file))
                      (setf dir
                            (read-dir-from-file (fat32-structure host)
                                                (partition host)
                                                file))))
           finally (loop for file in dir
                      do (when (string= name (virtual-dir-name file))
                           (setf buffer (mezzano.fat32-file-system:read-file file disk fat32)
                                 read-buffer-size (virtual-dir-file-size file))
                           (return))
                      finally (ecase if-does-not-exist
                                (:error (error 'simple-file-error
                                               :pathname pathname
                                               :format-control "File ~A does not exist. ~S"
                                               :format-arguments (list pathname name)))
                                (:create (error "Feature not implemented"))))))

      (let ((stream (cond ((or (eql element-type :default)
                               (subtypep element-type 'character))
                           (assert (member external-format '(:default :utf-8))
                                   (external-format))
                           (make-instance 'fat32-file-character-stream
                                          :direction direction
                                          :read-buffer buffer
                                          :read-buffer-position -1
                                          :read-buffer-size read-buffer-size))
                          ((and (subtypep element-type '(unsigned-byte 8))
                                (subtypep '(unsigned-byte 8) element-type))
                           (assert (eql external-format :default) (external-format))
                           (make-instance 'fat32-file-stream
                                          :direction direction
                                          :read-buffer buffer
                                          :read-buffer-position -1
                                          :read-buffer-size read-buffer-size))
                          (t (error "Unsupported element-type ~S." element-type)))))
        stream))))

(defmethod sys.gray:stream-element-type ((stream fat32-file-stream))
  '(unsigned-byte 8))

(defmethod sys.gray:stream-element-type ((stream fat32-file-character-stream))
  'character)

(defmethod sys.gray:stream-write-byte ((stream fat32-file-stream) byte)
  (error "Feature not implemented"))

(defmethod sys.gray:stream-read-byte ((stream fat32-file-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((char (aref (read-buffer stream)
                    (+ (read-buffer-position stream)
                       (incf (read-buffer-offset stream))))))
    (if (< (read-buffer-size stream)
           (read-buffer-offset stream))
        :eof
        char)))

(defmethod sys.gray:stream-read-sequence ((stream fat32-file-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (error "Feature not implemented"))

(defmethod sys.gray:stream-write-char ((stream fat32-file-character-stream) char)
  (error "Feature not implemented"))

(defmethod sys.gray:stream-read-sequence ((stream fat32-file-character-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (error "Feature not implemented"))

(defmethod sys.gray:stream-read-char ((stream fat32-file-character-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((char (aref (read-buffer stream)
                    (+ (read-buffer-position stream)
                       (incf (read-buffer-offset stream))))))
    (if (< (read-buffer-size stream)
           (read-buffer-offset stream))
        :eof
        (code-char char))))

(defmethod sys.gray:stream-file-position ((stream fat32-file-stream) &optional (position-spec nil position-specp))
  (error "Feature not implemented"))

(defmethod sys.gray:stream-file-length ((stream fat32-file-stream))
  (error "Feature not implemented"))

;;;; testing
#|
;; Mount partition
(let* ((partition-name "FAT32") ; Put your name here
       (partition-number 3) ; Change to your partition number
       (partition (nth partition-number (mezzano.supervisor:all-disks)))
       (fat32-structure (mezzano.fat32-file-system:read-fat32-structure
                         (mezzano.fat32-file-system:read-sector partition 0)))
       (fat32-info (mezzano.fat32-file-system:read-fat32-info-structure
                    (mezzano.fat32-file-system:read-sector
                     partition
                     (mezzano.fat32-file-system::fat32-fat-info fat32-structure))))
       (instance (make-instance 'fat32-host
                                :name partition-name
                                :partition partition
                                :fat32-structure fat32-structure
                                :fat32-info fat32-info)))
  (setf (mezzano.file-system:find-host partition-name)
        instance))

;; Read some file
(with-open-file (file #P"FOO:>dir1>log-1") ; put your file path
  (loop for line = (read-line file nil nil)
     while line do (print line)))
|#
