;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Persistent storage management.

(in-package :mezzano.supervisor)

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

(defvar *store-freelist-n-free-blocks*)
(defvar *store-freelist-total-blocks*)

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
      (let* ((frame (allocate-physical-pages 1 "store freelist metadata"))
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
      (let* ((new-block (or (store-alloc-1 1)
                            ;; TODO: Compact the freelist or something here.
                            (error "No space for new freelist log entries!")))
             (data (zero-cached-block new-block)))
        ;; First entry covers the new block.
        (setf (sys.int::memref-unsigned-byte-64 data 0) new-block
              (sys.int::memref-unsigned-byte-64 data 1) (logior (ash 1 1) 0))
        ;; Set next block pointer.
        (setf (sys.int::memref-unsigned-byte-64 blk 511) new-block)
        ;; Advance.
        (setf *store-freelist-block-offset* 1
              *store-freelist-block* new-block)))))

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

(defun split-freelist-range (range start end)
  (let ((before (freelist-alloc-metadata (freelist-metadata-start range) start
                                         (freelist-metadata-free-p range)))
        (after (freelist-alloc-metadata end (freelist-metadata-end range)
                                        (freelist-metadata-free-p range))))
    ;; Insert before.
    (cond ((freelist-metadata-prev range)
           (setf (freelist-metadata-next (freelist-metadata-prev range)) before))
          (t (setf *store-freelist-head* before)))
    (setf (freelist-metadata-prev before) (freelist-metadata-prev range)
          (freelist-metadata-next before) range
          (freelist-metadata-prev range) before)
    ;; Insert after.
    (cond ((freelist-metadata-next range)
           (setf (freelist-metadata-prev (freelist-metadata-next range)) after))
          (t (setf *store-freelist-tail* after)))
    (setf (freelist-metadata-next after) (freelist-metadata-next range)
          (freelist-metadata-prev after) range
          (freelist-metadata-next range) after)
    ;; Update range.
    (setf (freelist-metadata-free-p range) (not (freelist-metadata-free-p range))
          (freelist-metadata-start range) start
          (freelist-metadata-end range) end)))

(defun store-insert-range (start n-blocks freep)
  "Internal function. Insert a new range into the in-memory store freelist.
Should be called with the freelist lock held."
  ;; Search for the entry containing this range.
  #+(or)(debug-print-line "Inserting range " start "-" (+ start n-blocks) ":" freep " into freelist")
  #+(or)(do ((range *store-freelist-head*
              (freelist-metadata-next range)))
      ((null range))
    (debug-print-line "Range: " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (freelist-metadata-free-p range)))
  (do ((end (+ start n-blocks))
       (range *store-freelist-head*
              (freelist-metadata-next range)))
      ((null range)
       (panic "Tried to insert bad range " start "-" end ":" freep))
    (when (and (<= (freelist-metadata-start range) start)
               (<= end (freelist-metadata-end range)))
      (when (eql (not (not freep)) (freelist-metadata-free-p range))
        (do ((range *store-freelist-head*
                  (freelist-metadata-next range)))
          ((null range))
        (debug-print-line "Range: " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (freelist-metadata-free-p range)))
        (panic "Tried to free a free range or tried to allocate an allocated range. "
               start "-" end ":" freep " " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (freelist-metadata-free-p range)))
      (cond
        ((and (eql (freelist-metadata-start range) start)
              (eql (freelist-metadata-end range) end))
         ;; This range will be shrunk to nothingness.
         ;; Merge the next/prev ranges.
         (cond ((and (freelist-metadata-next range)
                     (freelist-metadata-prev range))
                ;; Both sides exist. Pick one to free and enlarge the other.
                (let ((before (freelist-metadata-prev range))
                      (after (freelist-metadata-next range)))
                  (setf (freelist-metadata-prev after) (freelist-metadata-prev before)
                        (freelist-metadata-start after) (freelist-metadata-start before))
                  (cond ((freelist-metadata-prev before)
                         (setf (freelist-metadata-next (freelist-metadata-prev before)) after))
                        (t (setf *store-freelist-head* after)))
                  (freelist-free-metadata before)
                  (freelist-free-metadata range)))
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
         (split-freelist-range range start end)))
      #+(or)(debug-print-line "After")
      #+(or)(do ((range *store-freelist-head*
                  (freelist-metadata-next range)))
          ((null range))
        (debug-print-line "Range: " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (freelist-metadata-free-p range)))
      (return))))

(defun store-free (start n-blocks)
  (with-symbol-spinlock (*store-freelist-lock*)
    (incf *store-freelist-n-free-blocks* n-blocks)
    (store-insert-range start n-blocks t)
    (store-freelist-log start n-blocks t)))

(defun store-alloc-1 (n-blocks)
  "Allocate from the in-memory freelist only. The freelist lock must be held."
  ;; Find a free range large enough.
  (do ((range *store-freelist-head* (freelist-metadata-next range)))
      ((null range)
       nil)
    (let ((size (- (freelist-metadata-end range) (freelist-metadata-start range))))
      (when (and (freelist-metadata-free-p range)
                 (<= n-blocks size))
        (let ((start (freelist-metadata-start range)))
          (store-insert-range start n-blocks nil)
          (decf *store-freelist-n-free-blocks* n-blocks)
          (return start))))))

(defun store-alloc (n-blocks)
  (with-symbol-spinlock (*store-freelist-lock*)
    (store-alloc-1 n-blocks)))

(defun process-one-freelist-block (block-id)
  (let* ((blk (read-block-from-disk block-id))
         (next (sys.int::memref-unsigned-byte-64 blk 511)))
    (dotimes (i 255)
      (let* ((start (sys.int::memref-unsigned-byte-64 blk (* i 2)))
             (size (sys.int::memref-unsigned-byte-64 blk (1+ (* i 2))))
             (freep (logbitp 0 size)))
        (setf size (ash size -1))
        (when (zerop size)
          (debug-print-line " freelist processing complete final " block-id ":" i)
          (return-from process-one-freelist-block
            (values i nil)))
        (cond (freep
               (incf *store-freelist-n-free-blocks* size))
              (t
               (decf *store-freelist-n-free-blocks* size)))
        (debug-print-line " insert freelist entry " start ":" size " " (if freep "free" "allocated"))
        (store-insert-range start size freep)))
    (assert (not (zerop next)) () "Corrupt freelist! No next block.")
    (debug-print-line " next freelist block " next)
    (values nil next)))

(defun initialize-store-freelist (n-store-blocks freelist-block)
  (setf *store-freelist-metadata-freelist* '()
        *store-freelist-metadata-freelist-lock* :unlocked)
  (setf *store-freelist-head* (freelist-alloc-metadata 0 n-store-blocks t)
        *store-freelist-tail* *store-freelist-head*
        *store-freelist-lock* :unlocked
        *store-freelist-n-free-blocks* n-store-blocks
        *store-freelist-total-blocks* n-store-blocks)
  (loop
     (multiple-value-bind (last-entry-offset next-block)
         (process-one-freelist-block freelist-block)
       (cond (next-block
              (setf freelist-block next-block))
             (t (setf *store-freelist-block* freelist-block
                      *store-freelist-block-offset* last-entry-offset)
                (read-cached-block *store-freelist-block*)
                (return)))))
  (do ((range *store-freelist-head*
              (freelist-metadata-next range)))
      ((null range))
    (debug-print-line "Range: " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (freelist-metadata-free-p range))))

(defun store-statistics ()
  "Return two values: The number of blocks free, and the total number of blocks."
  (with-symbol-spinlock (*store-freelist-lock*)
    (values *store-freelist-n-free-blocks*
            *store-freelist-total-blocks*)))

(defun regenerate-store-freelist ()
  (let ((first-block nil))
    (with-symbol-spinlock (*store-freelist-lock*)
      ;; Resulting freelist needs to fit in one block.
      ;; TODO: fix this. Figure out how long it is and preallocate all blocks.
      (do ((range *store-freelist-head* (freelist-metadata-next range))
           (total-allocated-ranges 0))
          ((null range)
           (when (>= total-allocated-ranges 250)
             (return-from regenerate-store-freelist nil)))
        (when (not (freelist-metadata-free-p range))
          (incf total-allocated-ranges)))
      (setf first-block (store-alloc-1 1))
      (when (not first-block)
        (return-from regenerate-store-freelist))
      (zero-cached-block first-block)
      (setf *store-freelist-block* first-block
            *store-freelist-block-offset* 0)
      (do ((range *store-freelist-head* (freelist-metadata-next range)))
          ((null range))
        (when (not (freelist-metadata-free-p range))
          (store-freelist-log (freelist-metadata-start range)
                              (- (freelist-metadata-end range) (freelist-metadata-start range))
                              nil))))
    (do ((range *store-freelist-head*
                (freelist-metadata-next range)))
        ((null range))
      (debug-print-line "Range: " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (freelist-metadata-free-p range)))
    ;; Update header.
    (let ((header (read-cached-block 0)))
      (setf (sys.int::memref-unsigned-byte-64 header 13) first-block))
    t))
