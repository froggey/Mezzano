;;;; Persistent storage management.

(in-package :mezzanine.supervisor)

(defvar *store-freelist-lock*)

;;; On-disk log-structured freelist.
(defvar *store-freelist-block*)
(defvar *store-freelist-block-offset*)

;;; In-memory freelist linked list
(defvar *store-freelist-head*)
(defvar *store-freelist-tail*)

;;; Free metadata objects.
(defvar *store-freelist-metadata-freelist*)
(defvar *store-freelist-metadata-freelist-lock*)

(macrolet ((field (name offset &key (type 't) (accessor 'sys.int::%array-like-ref-t))
             (let ((field-name (intern (format nil "+FREELIST-METADATA-~A+" (symbol-name name))
                                       (symbol-package name)))
                   (accessor-name (intern (format nil "FREELIST-METADATA-~A" (symbol-name name))
                                          (symbol-package name))))
               `(progn
                  (defconstant ,field-name ,offset)
                  (defun ,accessor-name (md)
                    (sys.int::memref-t md ,field-name))
                  (defun (setf ,accessor-name) (value md)
                    (setf (sys.int::memref-t md ,field-name) value))))))
  (field start 0)
  (field stuff 1)
  (field next  2)
  (field prev  3))

(defun freelist-metadata-end (md)
  (ldb (byte 60 1) (freelist-metadata-stuff md)))

(defun (setf freelist-metadata-end) (value md)
  (setf (ldb (byte 60 1) (freelist-metadata-stuff md)) value))

(defun freelist-metadata-free-p (md)
  (logbitp 0 (freelist-metadata-stuff md)))

(defun (setf freelist-metadata-free-p) (value md)
  (setf (ldb (byte 1 0) (freelist-metadata-stuff md)) (if value 1 0)))

(defconstant +freelist-metadata-size+ 32)

(defun freelist-alloc-metadata (start end freep)
  (with-symbol-spinlock (*store-freelist-metadata-freelist-lock*)
    (when (not *store-freelist-metadata-freelist*)
      ;; Repopulate freelist.
      (let* ((frame (or (allocate-physical-pages 1)
                        (progn (debug-write-line "Aiee. No memory.")
                               (loop))))
             (addr (+ +physical-map-base+ (ash frame 12))))
        (dotimes (i (truncate #x1000 +freelist-metadata-size+))
          (setf (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 0) 0
                (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 1) 0
                (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 2) 0
                (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 3) 0)
          (setf (freelist-metadata-next (+ addr (* i +freelist-metadata-size+))) *store-freelist-metadata-freelist*
                *store-freelist-metadata-freelist* (+ addr (* i +freelist-metadata-size+))))))
    (let ((new *store-freelist-metadata-freelist*))
      (setf *store-freelist-metadata-freelist* (freelist-metadata-next new))
      (setf (freelist-metadata-start new) start
            (freelist-metadata-end new) end
            (freelist-metadata-free-p new) freep
            (freelist-metadata-next new) '()
            (freelist-metadata-prev new) '())
      new)))

(defun freelist-free-metadata (md)
  (with-symbol-spinlock (*store-freelist-metadata-freelist-lock*)
    (setf (freelist-metadata-next md) *store-freelist-metadata-freelist*
          *store-freelist-metadata-freelist* md)))

(defun store-freelist-log (start n-blocks freep)
  "Internal function. Append an allocation to the on-disk freelist log.
Should be called with the freelist lock held."
  (let ((blk (read-cached-block *store-freelist-block*)))
    ;; TODO: See if the previous log entry can be extended (or shrunk?).
    (setf (sys.int::memref-unsigned-byte-64 blk (* *store-freelist-block-offset* 2)) start
          (sys.int::memref-unsigned-byte-64 blk (1+ (* *store-freelist-block-offset* 2))) (logior (ash n-blocks 1)
                                                                                                  (if freep 1 0)))
    (incf *store-freelist-block-offset*)
    (when (eql *store-freelist-block-offset* 255)
      ;; This block is now full, allocate a new one.
      (do ((range *store-freelist-head* (freelist-metadata-next range)))
          ((null range)
           ;; TODO: Compact the freelist or something here.
           (error "No space for new freelist log entries!"))
        (when (freelist-metadata-free-p range)
          (let* ((new-block (freelist-metadata-start range))
                 (data (zero-cached-block new-block)))
            (store-insert-range new-block 1 nil)
            ;; First entry covers the new block.
            (setf (sys.int::memref-unsigned-byte-64 data 0) new-block
                  (sys.int::memref-unsigned-byte-64 data 1) (logior (ash 1 1) 1))
            ;; Set next block pointer.
            (setf (sys.int::memref-unsigned-byte-64 blk 511) new-block)
            ;; Advance.
            (setf *store-freelist-block-offset* 1
                  *store-freelist-block* new-block))
          (return))))))

(defun adjust-freelist-range-start (range new-start)
  (cond ((freelist-metadata-prev range)
         (setf (freelist-metadata-end (freelist-metadata-prev range)) new-start))
        (t ;; Have to split.
         (let ((new (freelist-alloc-metadata (freelist-metadata-start range)
                                             new-start
                                             (not (freelist-metadata-free-p range)))))
           (setf *store-freelist-head* new
                 (freelist-metadata-prev range) new
                 (freelist-metadata-next new) range))))
  (setf (freelist-metadata-start range) new-start))

(defun adjust-freelist-range-end (range new-end)
  (cond ((freelist-metadata-next range)
         (setf (freelist-metadata-start (freelist-metadata-next range)) new-end))
        (t ;; Have to split.
         (let ((new (freelist-alloc-metadata new-end
                                             (freelist-metadata-end range)
                                             (not (freelist-metadata-free-p range)))))
           (setf *store-freelist-tail* new
                 (freelist-metadata-next range) new
                 (freelist-metadata-prev new) range))))
  (setf (freelist-metadata-end range) new-end))

(defun store-insert-range (start n-blocks freep)
  "Internal function. Insert a new range into the in-memory store freelist.
Should be called with the freelist lock held."
  ;; Search for the entry containing this range.
  (do ((end (+ start n-blocks))
       (range *store-freelist-head*
              (freelist-metadata-next range)))
      ((null range)
       (error "Tried to insert bad range."))
    (when (and (<= (freelist-metadata-start range) start)
               (<= end (freelist-metadata-end range)))
      (when (eql (not (not freep)) (freelist-metadata-free-p range))
        (error "Tried to free a free range or tried to allocate an allocated range."))
      (cond
        ((and (eql (freelist-metadata-start range) start)
              (eql (freelist-metadata-end range) end))
         ;; This range will be shrunk to nothingness.
         ;; Merge the next/prev ranges.
         (cond ((and (freelist-metadata-next range)
                     (freelist-metadata-prev range))
                ;; Both sides exist. Pick one to free and enlarge the other.
                (setf (freelist-metadata-prev (freelist-metadata-next range)) (freelist-metadata-prev (freelist-metadata-prev range))
                      (freelist-metadata-start (freelist-metadata-next range)) (freelist-metadata-start (freelist-metadata-prev range)))
                (unless (freelist-metadata-prev (freelist-metadata-prev range))
                  (setf *store-freelist-head* (freelist-metadata-next range)))
                (freelist-free-metadata (freelist-metadata-prev range))
                (freelist-free-metadata range))
               ((freelist-metadata-next range)
                (setf (freelist-metadata-prev (freelist-metadata-next range)) '()
                      (freelist-metadata-start (freelist-metadata-next range)) (freelist-metadata-start range)
                      *store-freelist-head* (freelist-metadata-next range))
                (freelist-free-metadata range))
               ((freelist-metadata-prev range)
                (setf (freelist-metadata-next (freelist-metadata-prev range)) '()
                      (freelist-metadata-end (freelist-metadata-prev range)) (freelist-metadata-end range)
                      *store-freelist-tail* (freelist-metadata-prev range))
                (freelist-free-metadata range))
               (t ;; Neither side exists! Just flip the free bit.
                (setf (freelist-metadata-free-p range) freep))))
        ((eql (freelist-metadata-start range) start)
         ;; Shrink, leaving end the same.
         (adjust-freelist-range-start range end))
        ((eql (freelist-metadata-end range) end)
         ;; Shrink, leaving start the same.
         (adjust-freelist-range-end range start))
        (t ;; Shrink from both sides. Feel the squeeze.
         (adjust-freelist-range-start range end)
         (adjust-freelist-range-end range start)))
      (return))))

(defun store-free (start n-blocks)
  (with-symbol-spinlock (*store-freelist-lock*)
    (store-insert-range start n-blocks t)
    (store-freelist-log start n-blocks t)))

(defun store-alloc (n-blocks)
  (with-symbol-spinlock (*store-freelist-lock*)
    ;; Find a free range large enough.
    (do ((range *store-freelist-head* (freelist-metadata-next range)))
        ((null range)
         nil)
      (let ((size (- (freelist-metadata-end range) (freelist-metadata-start range))))
        (when (and (freelist-metadata-free-p range)
                   (<= n-blocks size))
          (let ((start (freelist-metadata-start range)))
            (store-insert-range start n-blocks nil)
            (store-freelist-log start n-blocks nil)
            (return start)))))))

(defun process-one-freelist-block (block-id)
  (let* ((blk (read-cached-block block-id))
         (next (sys.int::memref-unsigned-byte-64 blk 511)))
    (dotimes (i 255)
      (let* ((start (sys.int::memref-unsigned-byte-64 blk (* i 2)))
             (size (sys.int::memref-unsigned-byte-64 blk (1+ (* i 2))))
             (freep (logbitp 0 size)))
        (setf size (ash size -1))
        (when (zerop size)
          (return-from process-one-freelist-block
            (values i nil)))
        (store-insert-range start size freep)))
    (assert (not (zerop next)) () "Corrupt freelist! No next block.")
    (values nil next)))

(defun initialize-store-freelist (n-store-blocks freelist-block)
  (setf *store-freelist-metadata-freelist* '()
        *store-freelist-metadata-freelist-lock* :unlocked)
  (setf *store-freelist-head* (freelist-alloc-metadata 0 n-store-blocks t)
        *store-freelist-tail* *store-freelist-head*
        *store-freelist-lock* :unlocked)
  (loop
     (multiple-value-bind (last-entry-offset next-block)
         (process-one-freelist-block freelist-block)
       (cond (next-block
              (setf freelist-block next-block))
             (t (setf *store-freelist-block* freelist-block
                      *store-freelist-block-offset* last-entry-offset)
                (return))))))
