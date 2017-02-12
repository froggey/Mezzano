;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Persistent storage management.
;;;; This is completely protected by *VM-LOCK*. It must be held before calling
;;;; STORE-ALLOC, STORE-FREE, or REGENERATE-STORE-FREELIST.

;;; External API:
;;; STORE-ALLOC - Allocate blocks from the store.
;;; STORE-FREE - Release blocks back to the store.
;;; STORE-STATISTICS - Return information about the store.

(in-package :mezzano.supervisor)

(sys.int::defglobal *verbose-store*)

;;; In-memory freelist linked list
(sys.int::defglobal *store-freelist-head*)
(sys.int::defglobal *store-freelist-tail*)

(sys.int::defglobal *store-deferred-freelist-head*)

;;; Free metadata objects.
(sys.int::defglobal *store-freelist-metadata-freelist*)
(sys.int::defglobal *store-freelist-n-free-metadata*)
(defvar *store-freelist-recursive-metadata-allocation*)
(defconstant +store-freelist-metadata-soft-limit+ 16)

;;; Block counts.
(sys.int::defglobal *store-freelist-n-free-blocks*)
(sys.int::defglobal *store-freelist-n-deferred-free-blocks*)
(sys.int::defglobal *store-freelist-total-blocks*)

;; Use a macro instead of macrolet to define these functions
;; because the compiler doesn't generate inlining info when there's
;; a non-null environment.
(defmacro define-freelist-metadata-field (name offset)
  (let ((field-name (intern (format nil "+FREELIST-METADATA-~A+" (symbol-name name))
                            (symbol-package name)))
        (accessor-name (intern (format nil "FREELIST-METADATA-~A" (symbol-name name))
                               (symbol-package name))))
    `(progn
       (defconstant ,field-name ,offset)
       (declaim (inline ,accessor-name (setf ,accessor-name)))
       (defun ,accessor-name (md)
         (sys.int::memref-t md ,field-name))
       (defun (setf ,accessor-name) (value md)
         (setf (sys.int::memref-t md ,field-name) value)))))

(define-freelist-metadata-field start 0)
(define-freelist-metadata-field stuff 1)
(define-freelist-metadata-field next  2)
(define-freelist-metadata-field prev  3)

(declaim (inline freelist-metadata-end (setf freelist-metadata-end)))

(defun freelist-metadata-end (md)
  (ldb (byte 60 1) (freelist-metadata-stuff md)))

(defun (setf freelist-metadata-end) (value md)
  (setf (ldb (byte 60 1) (freelist-metadata-stuff md)) value))

(declaim (inline freelist-metadata-free-p (setf freelist-metadata-free-p)))

(defun freelist-metadata-free-p (md)
  (logbitp 0 (freelist-metadata-stuff md)))

(defun (setf freelist-metadata-free-p) (value md)
  (setf (ldb (byte 1 0) (freelist-metadata-stuff md)) (if value 1 0)))

(defconstant +freelist-metadata-size+ 32)

(defun store-refill-metadata ()
  ;; Repopulate freelist.
  (let* ((frame (let ((*store-freelist-recursive-metadata-allocation* t))
                  (pager-allocate-page :other)))
         (addr (convert-to-pmap-address (ash frame 12))))
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
         (let ((before (freelist-metadata-prev range))
               (after (freelist-metadata-next range)))
           (cond ((and before after)
                  ;; Both sides exist. Pick one to free and enlarge the other.
                  (setf (freelist-metadata-prev after) (freelist-metadata-prev before)
                        (freelist-metadata-start after) (freelist-metadata-start before))
                  (cond ((freelist-metadata-prev before)
                         (setf (freelist-metadata-next (freelist-metadata-prev before)) after))
                        (t (setf *store-freelist-head* after)))
                  (freelist-free-metadata before)
                  (freelist-free-metadata range))
                 (after
                  (setf (freelist-metadata-prev after) '()
                        (freelist-metadata-start after) (freelist-metadata-start range)
                        *store-freelist-head* after)
                  (freelist-free-metadata range))
                 (before
                  (setf (freelist-metadata-next before) '()
                        (freelist-metadata-end before) (freelist-metadata-end range)
                        *store-freelist-tail* before)
                  (freelist-free-metadata range))
                 (t ;; Neither side exists! Just flip the free bit.
                  (setf (freelist-metadata-free-p range) freep)))))
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
  (ensure (not (eql start sys.int::+block-map-id-lazy+)) "Tried to free lazy block.")
  (ensure (mutex-held-p *vm-lock*) "*VM-LOCK* must be held when freeing store.")
  (store-maybe-refill-metadata)
  (incf *store-freelist-n-free-blocks* n-blocks)
  (store-insert-range start n-blocks t))

;; TODO: Be smarter here, check for overlaps in the deferred list and the main freelist.
(defun store-deferred-free (start n-blocks)
  (ensure (not (eql start sys.int::+block-map-id-lazy+)) "Tried to free lazy block.")
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
            (when *verbose-store*
              (debug-print-line " freelist processing complete final " block-id ":" i))
            (return-from process-one-freelist-block
              (values i nil)))
          (cond (freep
                 (incf *store-freelist-n-free-blocks* size))
                (t
                 (decf *store-freelist-n-free-blocks* size)))
          (when *verbose-store*
            (debug-print-line " insert freelist entry " start ":" size " " (if freep "free" "allocated")))
          (store-insert-range start size freep)))
      (assert (not (zerop next)) () "Corrupt freelist! No next block.")
      (when *verbose-store*
        (debug-print-line " next freelist block " next))
      (values nil next))))

(defun dump-store-freelist ()
  (do ((range *store-freelist-head*
              (freelist-metadata-next range)))
      ((null range))
    (debug-print-line "Range: " (freelist-metadata-start range) "-" (freelist-metadata-end range) ":" (if (freelist-metadata-free-p range) "free" "used"))))

(defun initialize-store-freelist (n-store-blocks freelist-block)
  (when (not (boundp '*verbose-store*))
    (setf *verbose-store* nil))
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
  (debug-print-line "Store freelist block is " freelist-block)
  (loop
     (multiple-value-bind (last-entry-offset next-block)
         (process-one-freelist-block freelist-block)
       (when (not next-block)
         (return))
       (setf freelist-block next-block)))
  (when *verbose-store*
    (dump-store-freelist))
  (debug-print-line *store-freelist-n-free-blocks* "/" *store-freelist-total-blocks* " store blocks free at boot"))

(defun initialize-freestanding-store ()
  (when (not (boundp '*verbose-store*))
    (setf *verbose-store* nil))
  (setf *store-freelist-metadata-freelist* '()
        *store-freelist-recursive-metadata-allocation* nil
        *store-freelist-n-free-metadata* 0)
  (store-refill-metadata)
  (setf *store-freelist-head* nil
        *store-freelist-tail* nil
        *store-deferred-freelist-head* nil
        *store-freelist-n-free-blocks* 0
        *store-freelist-n-deferred-free-blocks* 0
        ;; Prevent division by zero in ROOM.
        *store-freelist-total-blocks* 1))

(defun store-statistics ()
  "Return three values: The number of blocks free, the total number of blocks, and the number of deferred free blocks."
  ;; Disable interrupts to avoid smearing if a snapshot is taken between the two reads.
  (safe-without-interrupts ()
    (values *store-freelist-n-free-blocks*
            *store-freelist-total-blocks*
            *store-freelist-n-deferred-free-blocks*)))

(defun regenerate-store-freelist ()
  (ensure (mutex-held-p *vm-lock*) "*VM-LOCK* must be held in REGENERATE-STORE-FREELIST.")
  (let ((n-deferred-ranges 0)
        (n-real-ranges 0)
        (free-block-list nil)
        (used-block-list nil))
    (do ((range *store-deferred-freelist-head* (freelist-metadata-next range)))
        ((null range))
      (incf n-deferred-ranges))
    (do ((range *store-freelist-head* (freelist-metadata-next range)))
        ((null range))
      (incf n-real-ranges))
    (let ((estimated-count (1+ (ceiling (+ n-deferred-ranges n-real-ranges) 255))))
      (debug-print-line n-deferred-ranges " deferred ranges. " n-real-ranges " real ranges. Estimated "
                        estimated-count " freelist blocks required.")
      ;; Allocate blocks and pages.
      (dotimes (i estimated-count)
        (let ((memory (convert-to-pmap-address (ash (pager-allocate-page) 12)))
              (disk-block (or (store-alloc 1)
                              (panic "Unable to allocate new freelist entries!"))))
          (setf (sys.int::memref-t memory 0) free-block-list
                (sys.int::memref-t memory 1) disk-block
                free-block-list memory))))
    ;; Preallocate a *whole* bunch of free metadata entries.
    ;; Do this to avoid having to allocate & potentially swap things out when adding the deferred
    ;; free pages to the freelist.
    (loop
       while (< *store-freelist-n-free-metadata* (* n-deferred-ranges 2))
       do (store-refill-metadata))
    ;; Temporarily release all deferred pages back to the freelist.
    (do ((range *store-deferred-freelist-head* (freelist-metadata-next range)))
        ((null range))
      (let ((start (freelist-metadata-start range))
            (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range))))
        (when *verbose-store*
          (debug-print-line "Free deferred range " (freelist-metadata-start range) "-" (freelist-metadata-end range)))
        (store-insert-range start n-blocks t)))
    (when *verbose-store*
      (dump-store-freelist))
    ;; Write every allocated region to the block.
    (do ((range *store-freelist-head* (freelist-metadata-next range))
         (current-block nil)
         (current-page nil)
         (offset 0))
        ((null range)
         ;; Finish this block.
         (setf (sys.int::memref-t current-page 510) used-block-list
               (sys.int::memref-t current-page 511) current-block
               used-block-list current-page))
      (when (eql offset 255)
        ;; This block is full, attach to used block list and grab a fresh block.
        (setf (sys.int::memref-t current-page 510) used-block-list
              (sys.int::memref-t current-page 511) current-block
              used-block-list current-page)
        ;; Advance to next block.
        (setf offset 0)
        (setf current-block nil))
      (when (not current-block)
        (ensure free-block-list)
        (setf current-page free-block-list
              current-block (sys.int::memref-t current-page 1)
              free-block-list (sys.int::memref-t current-page 0))
        (zeroize-page current-page)
        (when *verbose-store*
          (debug-print-line "Next freelist block " current-block)))
      (when (not (freelist-metadata-free-p range))
        (let ((start (freelist-metadata-start range))
              (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range))))
          (when *verbose-store*
            (debug-print-line "Next is " start " " n-blocks))
          ;; Address.
          (setf (sys.int::memref-unsigned-byte-64 current-page (* offset 2)) start)
          ;; Size and free bit (clear).
          (setf (sys.int::memref-unsigned-byte-64 current-page (1+ (* offset 2))) (ash n-blocks 1))
          (incf offset))))
    ;; FIXME! Allocated blocks that weren't used have been leaked here.
    ;; USED-BLOCK-LIST now contains a list of freelist blocks in reverse order.
    ;; Write them to disk.
    (let ((last-block 0))
      (loop
         (when (not used-block-list)
           (return))
         (let* ((current-page used-block-list)
                (current-block (sys.int::memref-t current-page 511)))
           (setf used-block-list (sys.int::memref-t current-page 510))
           (setf (sys.int::memref-unsigned-byte-64 current-page 510) 0
                 (sys.int::memref-unsigned-byte-64 current-page 511) last-block)
           (snapshot-write-disk current-block current-page)
           (setf last-block current-block)))
      ;; And unfree the deferred pages.
      (do ((range *store-deferred-freelist-head* (freelist-metadata-next range)))
          ((null range))
        (let ((start (freelist-metadata-start range))
              (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range))))
          (when *verbose-store*
            (debug-print-line "Unfree deferred range " (freelist-metadata-start range) "-" (freelist-metadata-end range)))
          (store-insert-range start n-blocks nil)))
      ;; Freelist is back to normal, safe to free the other blocks/pages now.
      (loop
         (when (not free-block-list)
           (return))
         (let* ((page free-block-list)
                (block (sys.int::memref-t page 1)))
           (setf free-block-list (sys.int::memref-t page 0))
           (free-page page)
           (store-free block 1)))
      ;; All done. Return the address of the starting freelist block.
      last-block)))

(defun store-release-deferred-blocks (block-list)
  (do ((range block-list (freelist-metadata-next range)))
      ((null range))
    (let ((start (freelist-metadata-start range))
          (n-blocks (- (freelist-metadata-end range) (freelist-metadata-start range))))
      (when *verbose-store*
        (debug-print-line "Release deferred range " (freelist-metadata-start range) "-" (freelist-metadata-end range)))
      (store-free start n-blocks))))
