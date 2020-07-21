;;;; Writes an in-memory image to a file

(defpackage :mezzano.cold-generator.write
  (:use :cl)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:ser #:mezzano.cold-generator.serialize)
                    (#:util #:mezzano.cold-generator.util)
                    (#:sys.int #:mezzano.internals))
  (:export #:image-header
           #:image-header-uuid
           #:image-header-entry-fref
           #:image-header-initial-thread
           #:image-header-nil
           #:image-header-architecture
           #:write-image))

(in-package :mezzano.cold-generator.write)

(defclass image-header ()
  ((%uuid :initarg :uuid :reader image-header-uuid)
   (%entry-fref :initarg :entry-fref :reader image-header-entry-fref)
   (%initial-thread :initarg :initial-thread :reader image-header-initial-thread)
   (%nil :initarg :nil :reader image-header-nil)
   (%architecture :initarg :architecture :reader image-header-architecture)))

;; Bump allocator for allocating space in the image file.
(defvar *store-bump*)

(defun write-image-disk-header (stream header-path image-size)
  "Write the disk header, if any, and size the image appropriately."
  (let* ((image-header-data (when header-path
                              (util:load-binary-file header-path)))
         (image-offset (if image-header-data
                           (length image-header-data)
                           0)))
    (when (and image-header-data
               (not image-size))
      ;; When a header is used, a full disk image is being created, not
      ;; a stand-alone image.
      ;; Set a reasonably sensible default image size if none was provided.
      (setf image-size (* 4 1024 1024 1024)))
    (when image-size
      (decf image-size image-offset)
      (format t ";; Generating ~:D byte image.~%" image-size))
    (when image-header-data
      (format t ";; Using ~S as the image header.~%" header-path))
    (when image-header-data
      (write-sequence image-header-data stream)
      ;; Update the size of the third partition entry, the Mezzano partiton.
      (file-position stream #x1EA)
      (nibbles:write-ub32/le (truncate image-size 512) stream))
    ;; Pad image out to the proper size
    (when image-size
      (file-position stream (1- (+ image-offset image-size)))
      (write-byte 0 stream))
    (file-position stream image-offset)
    image-offset))

(defun write-image-header (image image-offset stream bml4-block freelist-block header)
  (let ((encoded-header (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Magic.
    (replace encoded-header #(#x00 #x4D #x65 #x7A #x7A #x61 #x6E #x69 #x6E #x65 #x49 #x6D #x61 #x67 #x65 #x00)
             :start1 0)
    ;; UUID.
    (replace encoded-header (image-header-uuid header) :start1 16)
    ;; Major boot protocol version.
    (setf (nibbles:ub16ref/le encoded-header 32) 0)
    ;; Minor boot protocol version.
    (setf (nibbles:ub16ref/le encoded-header 34) 26)
    ;; Entry fref.
    (setf (nibbles:ub64ref/le encoded-header 40) (image-header-entry-fref header))
    ;; Initial thread.
    (setf (nibbles:ub64ref/le encoded-header 48) (image-header-initial-thread header))
    ;; NIL.
    (setf (nibbles:ub64ref/le encoded-header 56) (image-header-nil header))
    ;; Architecture.
    (setf (nibbles:ub32ref/le encoded-header 64)
          (ecase (image-header-architecture header)
            (:x86-64 sys.int::+llf-arch-x86-64+)
            (:arm64 sys.int::+llf-arch-arm64+)))
    ;; 65-72 free.
    ;; Initial stack pointer.
    (setf (nibbles:ub64ref/le encoded-header 72)
          (ser:image-initial-stack-pointer image))
    ;; 80-96 free.
    ;; Top-level block map.
    (setf (nibbles:ub64ref/le encoded-header 96) (/ bml4-block #x1000))
    ;; Free block list.
    (setf (nibbles:ub64ref/le encoded-header 104) (/ freelist-block #x1000))
    ;; Write it out.
    (file-position stream image-offset)
    (write-sequence encoded-header stream)))

(defun add-page-to-block-map (bml4 block virtual-address flags)
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
          (setf (aref bml1 bml1e) (logior (ash block sys.int::+block-map-id-shift+)
                                          flags)))))))

(defun add-region-to-block-map (bml4 store-base virtual-base size flags)
  (dotimes (i size)
    (add-page-to-block-map bml4 (+ store-base i) (+ virtual-base (* i #x1000)) flags)))

(defun write-areas (image stream image-offset bml4)
  "Save areas to the file and update the block map"
  (flet ((save (area flags)
           (let ((store-position *store-bump*))
             (incf *store-bump* (ser:area-size (ser:image-wired-area image)))
             (file-position stream (+ image-offset store-position))
             (write-sequence (ser:area-data area) stream)
             (add-region-to-block-map
              bml4
              (/ store-position #x1000)
              (ser:area-base area)
              (/ (ser:area-size area) #x1000)
              (logior sys.int::+block-map-present+
                      sys.int::+block-map-writable+
                      flags)))))
    (save (ser:image-wired-area image) sys.int::+block-map-wired+)
    (save (ser:image-pinned-area image) 0)
    (save (ser:image-general-area image) sys.int::+block-map-track-dirty+)
    (save (ser:image-cons-area image) sys.int::+block-map-track-dirty+)
    (save (ser:image-wired-function-area image) sys.int::+block-map-wired+)
    (save (ser:image-function-area image) 0)))

(defun write-stacks (image bml4)
  (ser:do-image-stacks (base size image)
    (let ((store-position *store-bump*))
      (incf *store-bump* size)
      (add-region-to-block-map
       bml4
       (/ store-position #x1000)
       base
       (/ size #x1000)
       (logior sys.int::+block-map-present+
               sys.int::+block-map-writable+
               sys.int::+block-map-zero-fill+
               sys.int::+block-map-wired+)))))

(defun find-object-origin-for-card-table (area offset)
  "Find the largest object start that is <= OFFSET in AREA."
  ;; Binary search through the table looking for the first element that is
  ;; greater than OFFSET. The target element is the one immediately before it.
  (do* ((starts (ser:area-object-starts area))
        (low 0)
        (high (length starts)))
       ((eql low high)
        (aref starts (1- low)))
    (let ((mid (truncate (+ low high) 2)))
      (cond ((<= (aref starts mid) offset)
             (setf low (1+ mid)))
            (t
             (setf high mid))))))

(defun write-card-table (image stream image-offset bml4)
  (flet ((doit (area)
           (let* ((start (ser:area-base area))
                  (size (ser:area-size area))
                  (table (make-array (* (/ size sys.int::+card-size+) sys.int::+card-table-entry-size+)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0))
                  (store-base *store-bump*))
             (assert (zerop (rem start sys.int::+allocation-minimum-alignment+)))
             (assert (zerop (rem size sys.int::+allocation-minimum-alignment+)))
             (loop
                for i below size by sys.int::+card-size+
                for object-start-offset = (find-object-origin-for-card-table area i)
                for offset = (- object-start-offset i)
                do
                  (assert (not (plusp offset)))
                  (assert (not (logtest offset 15)))
                  (setf (nibbles:ub32ref/le table (* (/ i sys.int::+card-size+) sys.int::+card-table-entry-size+))
                        (cross-cl:dpb (min (1- (expt 2 (cross-cl:byte-size sys.int::+card-table-entry-offset+)))
                                           (/ (- offset) 16))
                                      sys.int::+card-table-entry-offset+
                                      0)))
             (file-position stream (+ image-offset store-base))
             (write-sequence table stream)
             (incf *store-bump* (length table))
             (add-region-to-block-map
              bml4
              (/ store-base #x1000)
              (+ sys.int::+card-table-base+
                 (* (/ start sys.int::+card-size+) sys.int::+card-table-entry-size+))
              (/ (* (/ size sys.int::+card-size+) sys.int::+card-table-entry-size+) #x1000)
              (logior sys.int::+block-map-present+
                      sys.int::+block-map-writable+
                      sys.int::+block-map-wired+)))))
    (doit (ser:image-wired-area image))
    (doit (ser:image-pinned-area image))
    (doit (ser:image-general-area image))
    (doit (ser:image-cons-area image))
    (doit (ser:image-wired-function-area image))
    (doit (ser:image-function-area image))))

(defun write-block-map (s image-offset block-offset level)
  (let ((data (make-array #x1000 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i 512)
      (let ((e (aref level i)))
        (etypecase e
          (null)
          (vector
           ;; Next level
           (let* ((next-block *store-bump*))
             (incf *store-bump* #x1000)
             (write-block-map s image-offset next-block e)
             (setf (nibbles:ub64ref/le data (* i 8)) (ash (/ next-block #x1000) sys.int::+block-map-id-shift+))))
          ((unsigned-byte 64)
           ;; Value.
           (setf (nibbles:ub64ref/le data (* i 8)) e)))))
    (file-position s (+ image-offset block-offset))
    (write-sequence data s)))

(defun write-freelist (stream image-offset freelist-block)
  ;; Create the freelist.
  ;; One entry, allocating our storage area.
  (let ((freelist-data (make-array #x1000 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (nibbles:ub64ref/le freelist-data 0) 0
          (nibbles:ub64ref/le freelist-data 8) (ash (/ *store-bump* #x1000) 1))
    (file-position stream (+ image-offset freelist-block))
    (write-sequence freelist-data stream)
    ;; Zap the *store-bump* to prevent further allocations.
    (setf *store-bump* :bug-too-late-to-allocate)))

(defun write-image (image stream image-header &key disk-header-path image-size)
  (let ((image-offset (write-image-disk-header stream disk-header-path image-size))
        (*store-bump* #x3000) ; header is 4k, followed by bml4 and freelist.
        (bml4-block #x1000)
        (bml4 (make-array 512 :initial-element nil))
        (freelist-block #x2000))
    (write-image-header image image-offset stream bml4-block freelist-block image-header)
    (write-areas image stream image-offset bml4)
    (write-stacks image bml4)
    (write-card-table image stream image-offset bml4)
    (write-block-map stream image-offset bml4-block bml4)
    (write-freelist stream image-offset freelist-block))
  (values))
