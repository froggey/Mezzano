;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Persistent storage management.
;;;; This is completely protected by *VM-LOCK*. It must be held before calling
;;;; STORE-ALLOC, STORE-FREE, or REGENERATE-STORE-FREELIST.

;;; External API:
;;; STORE-ALLOC - Allocate blocks from the store.
;;; STORE-FREE - Release blocks back to the store.
;;; STORE-STATISTICS - Return information about the store.

(in-package :mezzano.supervisor)

;;; In-memory freelist linked list
(defvar *store-freelist-head*)
(defvar *store-freelist-tail*)

(defvar *store-deferred-freelist-head*)

;;; Free metadata objects.
(defvar *store-freelist-metadata-freelist*)
(defvar *store-freelist-n-free-metadata*)
(defvar *store-freelist-recursive-metadata-allocation*)
(defconstant +store-freelist-metadata-soft-limit+ 16)

;;; Block counts.
(defvar *store-freelist-n-free-blocks*)
(defvar *store-freelist-n-deferred-free-blocks*)
(defvar *store-freelist-total-blocks*)

(macrolet ((field (name offset)
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

(defun store-refill-metadata ()
  ;; Repopulate freelist.
  (let* ((frame (let ((*store-freelist-recursive-metadata-allocation* t))
                  (pager-allocate-page :other)))
         (addr (+ +physical-map-base+ (ash frame 12))))
    (dotimes (i (truncate #x1000 +freelist-metadata-size+))
      (setf (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 0) 0
            (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 1) 0
            (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 2) 0
            (sys.int::memref-unsigned-byte-64 (+ addr (* i +freelist-metadata-size+)) 3) 0)
      (incf *store-freelist-n-free-metadata*)
      (setf (freelist-metadata-next (+ addr (* i +freelist-metadata-size+))) *store-freelist-metadata-freelist*
            *store-freelist-metadata-freelist* (+ addr (* i +freelist-metadata-size+))))))

(defun freelist-alloc-metadata (start end freep)
  (ensure *store-freelist-metadata-freelist* "No available freelist metadata objects!")
  (let ((new *store-freelist-metadata-freelist*))
    (setf *store-freelist-metadata-freelist* (freelist-metadata-next new))
    (setf (freelist-metadata-start new) start
          (freelist-metadata-end new) end
          (freelist-metadata-free-p new) freep
          (freelist-metadata-next new) '()
          (freelist-metadata-prev new) '())
    (decf *store-freelist-n-free-metadata*)
    new))

(defun freelist-free-metadata (md)
  (incf *store-freelist-n-free-metadata*)
  (setf (freelist-metadata-next md) *store-freelist-metadata-freelist*
        *store-freelist-metadata-freelist* md))

(defun store-maybe-refill-metadata ()
  (when (and (not *store-freelist-recursive-metadata-allocation*)
             (< *store-freelist-n-free-metadata*
                +store-freelist-metadata-soft-limit+))
    (store-refill-metadata)))

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
  "Internal function. Insert a new range into the in-memory store freelist."
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
  (ensure (mutex-held-p *vm-lock*) "*VM-LOCK* must be held when freeing store.")
  (store-maybe-refill-metadata)
  (incf *store-freelist-n-free-blocks* n-blocks)
  (store-insert-range start n-blocks t))

;; TODO: Be smarter here, check for overlaps in the deferred list and the main freelist.
(defun store-deferred-free (start n-blocks)
  (ensure (mutex-held-p *vm-lock*) "*VM-LOCK* must be held when freeing store.")
  (store-maybe-refill-metadata)
  ;;(debug-print-line "Deferred store free " start " " n-blocks)
  (let ((end (+ start n-blocks)))
    ;; FIXME: This should be a sorted list, like the normal freelist!
    (do ((range *store-deferred-freelist-head* (freelist-metadata-next range)))
        ((null range)
         (let ((e (freelist-alloc-metadata start (+ start n-blocks) t)))
           (incf *store-freelist-n-deferred-free-blocks* n-blocks)
           (setf (freelist-metadata-next e) *store-deferred-freelist-head*
                 *store-deferred-freelist-head* e)))
      (when (eql (freelist-metadata-start range) end)
        (setf (freelist-metadata-start range) start)
        (return))
      (when (eql (freelist-metadata-end range) start)
        (setf (freelist-metadata-end range) end)
        (return)))))

(defun store-alloc (n-blocks)
  "Allocate from the in-memory freelist only. The freelist lock must be held."
  (ensure (mutex-held-p *vm-lock*) "*VM-LOCK* must be held when allocating store.")
  (store-maybe-refill-metadata)
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

(defun process-one-freelist-block (block-id)
  (with-disk-block (blk block-id)
    (let ((next (sys.int::memref-unsigned-byte-64 blk 511)))
      (dotimes (i 255)
        (store-maybe-refill-metadata)
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
      (values nil next))))

(defun dump-store-freelist ()
  (do ((range *store-freelist-head*
              (freelist-metadata-next range)))
      ((null range))
    (debug-print-line "Range: " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (freelist-metadata-free-p range))))

(defun initialize-store-freelist (n-store-blocks freelist-block)
  (setf *store-freelist-metadata-freelist* '()
        *store-freelist-recursive-metadata-allocation* nil
        *store-freelist-n-free-metadata* 0)
  (store-refill-metadata)
  (setf *store-freelist-head* (freelist-alloc-metadata 0 n-store-blocks t)
        *store-freelist-tail* *store-freelist-head*
        *store-deferred-freelist-head* nil
        *store-freelist-n-free-blocks* n-store-blocks
        *store-freelist-n-deferred-free-blocks* 0
        *store-freelist-total-blocks* n-store-blocks)
  (loop
     (multiple-value-bind (last-entry-offset next-block)
         (process-one-freelist-block freelist-block)
       (when (not next-block)
         (return))
       (setf freelist-block next-block)))
  (dump-store-freelist)
  (debug-print-line *store-freelist-n-free-blocks* "/" *store-freelist-total-blocks* " store blocks free at boot"))

(defun store-statistics ()
  "Return three values: The number of blocks free, the total number of blocks, and the number of deferred free blocks."
  ;; Disable interrupts to avoid smearing if a snapshot is taken between the two reads.
  (safe-without-interrupts ()
    (values *store-freelist-n-free-blocks*
            *store-freelist-total-blocks*
            *store-freelist-n-deferred-free-blocks*)))

;; FIXME: Deal with the deferred freelist as well.
(defun regenerate-store-freelist ()
  (ensure (mutex-held-p *vm-lock*) "*VM-LOCK* must be held in REGENERATE-STORE-FREELIST.")
  (let ((disk-block (or (store-alloc 1)
                        (panic "Unable to allocate store for freelist.")))
        (memory-block (+ +physical-map-base+ (ash (pager-allocate-page) 12))))
    ;; Allocate a *whole* bunch of spare metadata entries, need to do this without actually modifing
    ;; the freelist.
    (let ((n-deferred-ranges 0))
      (do ((range *store-deferred-freelist-head* (freelist-metadata-next range)))
          ((null range))
        (incf n-deferred-ranges))
      (loop
         while (< *store-freelist-n-free-metadata* (* n-deferred-ranges 2))
         do (store-refill-metadata)))
    ;; Temporarily release all deferred pages back to the freelist.
    (do ((range *store-deferred-freelist-head* (freelist-metadata-next range)))
      ((null range))
    (let ((start (freelist-metadata-start range))
          (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range))))
      (debug-print-line "Free deferred range " (freelist-metadata-start range) "-" (freelist-metadata-end range))
      (store-insert-range start n-blocks t)))
    ;; Resulting freelist needs to fit in one block.
    ;; TODO: fix this. Figure out how long it is and preallocate all blocks.
    (do ((range *store-freelist-head* (freelist-metadata-next range))
         (total-allocated-ranges 0))
        ((null range)
         (when (>= total-allocated-ranges 250)
           (dump-store-freelist)
           (panic "FIXME: Freelist too large. Can't rebuild freelist.")))
      (when (not (freelist-metadata-free-p range))
        (incf total-allocated-ranges)))
    (zeroize-page memory-block)
    ;; Write every allocated region to the block.
    (do ((range *store-freelist-head* (freelist-metadata-next range))
         (offset 0))
        ((null range))
      (when (not (freelist-metadata-free-p range))
        (let ((start (freelist-metadata-start range))
              (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range)))
              (free nil))
          (let ((blk memory-block))
            ;; Address.
            (setf (sys.int::memref-unsigned-byte-64 memory-block (* offset 2)) start)
            ;; Size and free bit (clear).
            (setf (sys.int::memref-unsigned-byte-64 memory-block (1+ (* offset 2))) (ash n-blocks 1))
            (incf offset)))))
    ;; And unfree the deferred pages.
    (do ((range *store-deferred-freelist-head* (freelist-metadata-next range)))
        ((null range))
      (let ((start (freelist-metadata-start range))
            (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range))))
        (debug-print-line "Unfree deferred range " (freelist-metadata-start range) "-" (freelist-metadata-end range))
        (store-insert-range start n-blocks nil)))
    (values disk-block memory-block)))

(defun store-release-deferred-blocks (block-list)
  (do ((range block-list (freelist-metadata-next range)))
      ((null range))
    (let ((start (freelist-metadata-start range))
          (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range))))
      (debug-print-line "Release deferred range " (freelist-metadata-start range) "-" (freelist-metadata-end range))
      (store-free start n-blocks))))
