;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(sys.int::defglobal *paranoid-allocation*)

(sys.int::defglobal sys.int::*wired-area-base*)
(sys.int::defglobal sys.int::*wired-area-bump*)
(sys.int::defglobal sys.int::*wired-area-free-bins*)
(sys.int::defglobal sys.int::*pinned-area-base*)
(sys.int::defglobal sys.int::*pinned-area-bump*)
(sys.int::defglobal sys.int::*pinned-area-free-bins*)

(sys.int::defglobal sys.int::*bytes-allocated-to-stacks*)
(sys.int::defglobal sys.int::*wired-stack-area-bump*)
(sys.int::defglobal sys.int::*stack-area-bump*)

(sys.int::defglobal sys.int::*general-area-gen0-bump*)
(sys.int::defglobal sys.int::*general-area-gen0-limit*)
(sys.int::defglobal sys.int::*general-area-gen1-bump*)
(sys.int::defglobal sys.int::*general-area-gen1-limit*)
(sys.int::defglobal sys.int::*general-area-bump*)
(sys.int::defglobal sys.int::*general-area-limit*)
(sys.int::defglobal sys.int::*cons-area-gen0-bump*)
(sys.int::defglobal sys.int::*cons-area-gen0-limit*)
(sys.int::defglobal sys.int::*cons-area-gen1-bump*)
(sys.int::defglobal sys.int::*cons-area-gen1-limit*)
(sys.int::defglobal sys.int::*cons-area-bump*)
(sys.int::defglobal sys.int::*cons-area-limit*)
(sys.int::defglobal sys.int::*generation-size-ratio*)

(sys.int::defglobal sys.int::*dynamic-mark-bit*)

(sys.int::defglobal *allocation-fudge*)

(sys.int::defglobal *allocator-lock*)
(sys.int::defglobal *general-area-expansion-granularity*)
(sys.int::defglobal *cons-area-expansion-granularity*)

(sys.int::defglobal *general-fast-path-hits*)
(sys.int::defglobal *general-allocation-count*)
(sys.int::defglobal *cons-fast-path-hits*)
(sys.int::defglobal *cons-allocation-count*)

(sys.int::defglobal *bytes-consed*)

(defvar *maximum-allocation-attempts* 5
  "GC this many times before giving up on an allocation.")

(sys.int::defglobal *enable-allocation-profiling*)
(defvar *allocation-profile-hook* nil)

(defun log-allocation-profile-entry (words)
  (when (and *enable-allocation-profiling*
             *allocation-profile-hook*)
    (let ((hook *allocation-profile-hook*)
          (*allocation-profile-hook* nil))
      (funcall hook words))))

(defun freelist-entry-next (entry)
  (sys.int::memref-t entry 1))

(defun (setf freelist-entry-next) (value entry)
  (setf (sys.int::memref-t entry 1) value))

(defun freelist-entry-size (entry)
  (ash (sys.int::memref-unsigned-byte-64 entry 0) (- sys.int::+object-data-shift+)))

(defun first-run-initialize-allocator ()
  (setf sys.int::*gc-in-progress* nil
        sys.int::*gc-enable-logging* nil
        sys.int::*pinned-mark-bit* 0
        sys.int::*dynamic-mark-bit* (dpb sys.int::+address-generation-2-a+
                                         sys.int::+address-generation+
                                         0)
        sys.int::*general-area-gen0-bump* 0
        sys.int::*general-area-gen0-limit* 0
        sys.int::*general-area-gen1-bump* 0
        sys.int::*general-area-gen1-limit* 0
        sys.int::*cons-area-gen0-bump* 0
        sys.int::*cons-area-gen0-limit* 0
        sys.int::*cons-area-gen1-bump* 0
        sys.int::*cons-area-gen1-limit* 0
        *enable-allocation-profiling* nil
        *general-area-expansion-granularity* sys.int::+allocation-minimum-alignment+
        *cons-area-expansion-granularity* sys.int::+allocation-minimum-alignment+
        *general-fast-path-hits* 0
        *general-allocation-count* 0
        *cons-fast-path-hits* 0
        *cons-allocation-count* 0
        *bytes-consed* 0
        *allocator-lock* (mezzano.supervisor:make-mutex "Allocator")
        *allocation-fudge* (* 8 1024 1024)
        sys.int::*generation-size-ratio* 2))

(defun verify-freelist (start base end)
  (do ((freelist start (freelist-entry-next freelist))
       (prev nil freelist))
      ((null freelist))
    (unless (and
             ;; A freelist entry must fall within area limits.
             (<= base freelist)
             (< freelist end)
             (<= (+ freelist (* (freelist-entry-size freelist) 8)) end)
             ;; Must have a non-zero size.
             (not (zerop (freelist-entry-size freelist)))
             ;; Must have the correct object tag.
             (eql (ldb (byte sys.int::+object-type-size+ sys.int::+object-type-shift+)
                       (sys.int::memref-unsigned-byte-64 freelist 0))
                  sys.int::+object-tag-freelist-entry+)
             ;; Must have a fixnum link, or be the end of the list.
             (or (sys.int::fixnump (freelist-entry-next freelist))
                 (not (freelist-entry-next freelist)))
             ;; Must be after the end of the previous freelist entry.
             (or (not prev)
                 (> freelist (+ prev (* (freelist-entry-size prev) 8)))))
      (mezzano.supervisor:panic "Corrupt freelist."))))

(defun set-allocated-object-header (address tag data mark-bit)
  ;; Be careful to avoid bignum consing here. Some functions can have a
  ;; data value larger than a fixnum when shifted.
  (setf (sys.int::memref-unsigned-byte-32 address 0) (logior mark-bit
                                                             (ash tag sys.int::+object-type-shift+)
                                                             (ash (ldb (byte (- 32 sys.int::+object-data-shift+) 0) data) sys.int::+object-data-shift+))
        (sys.int::memref-unsigned-byte-32 address 1) (ldb (byte 32 (- 32 sys.int::+object-data-shift+)) data)))

;; Simple first-fit binning freelist allocator for pinned areas.
(defun %allocate-from-freelist-area (tag data words bins)
  (let ((log2-len (integer-length words)))
    ;; Loop over each bin from log2-len up to 64 looking for a freelist entry that's large enough.
    (loop
       (when (>= log2-len 64)
         (return nil))
       ;; Traverse this bin.
       (do ((freelist (svref bins log2-len) (freelist-entry-next freelist))
            (prev nil freelist))
           ((null freelist))
         (let ((size (freelist-entry-size freelist)))
           (when (>= size words)
             ;; This freelist entry is large enough, use it.
             ;; Remove it from the bin.
             (cond (prev
                    (setf (freelist-entry-next prev) (freelist-entry-next freelist)))
                   (t
                    (setf (svref bins log2-len) (freelist-entry-next freelist))))
             (when (not (eql size words))
               ;; Entry is too large, split it.
               ;; Always create new entries with the pinned mark bit
               ;; set. A GC will flip it, making all the freelist
               ;; entries unmarked. No object can ever point to a freelist entry, so
               ;; they will never be marked during a gc.
               (let* ((new-size (- size words))
                      (new-bin (integer-length new-size))
                      (next (+ freelist (* words 8))))
                 (setf (sys.int::memref-unsigned-byte-64 next 0) (logior sys.int::*pinned-mark-bit*
                                                                         (ash sys.int::+object-tag-freelist-entry+ sys.int::+object-type-shift+)
                                                                         (ash (- size words) sys.int::+object-data-shift+))
                       (sys.int::memref-t next 1) (svref bins new-bin))
                 (setf (svref bins new-bin) next)
                 ;; Update the card table starts for any pages
                 ;; that this new freelist entry crosses.
                 ;; TODO: Make this more efficient.
                 (loop
                    for card from (mezzano.supervisor::align-up next sys.int::+card-size+) below (+ next (* new-size 8)) by sys.int::+card-size+
                    for delta = (- next card)
                    do (setf (sys.int::card-table-offset card)
                             (if (<= delta (- (* (1- (ash 1 (byte-size sys.int::+card-table-entry-offset+))) 16)))
                                 nil
                                 delta)))))
             ;; Write object header.
             (set-allocated-object-header freelist tag data sys.int::*pinned-mark-bit*)
             ;; Clear data.
             (sys.int::%fill-words (+ freelist 8) 0 (1- words))
             ;; Return address.
             (return-from %allocate-from-freelist-area freelist))))
       (incf log2-len))))

(defun %allocate-from-pinned-area-1 (tag data words)
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:with-mutex (*allocator-lock*)
      (mezzano.supervisor:with-pseudo-atomic
        (when *paranoid-allocation*
          (verify-freelist sys.int::*pinned-area-freelist* sys.int::*pinned-area-base* sys.int::*pinned-area-bump*))
        (let ((address (%allocate-from-freelist-area tag data words sys.int::*pinned-area-free-bins*)))
          (when address
            (sys.int::%%assemble-value address sys.int::+tag-object+)))))))

(defun finish-expand-pinned-area (grow-by)
  (let ((len (truncate grow-by 8))
        (final-entry (sys.int::base-address-of-internal-pointer
                      (- sys.int::*pinned-area-bump* 16)))
        (new-address sys.int::*pinned-area-bump*))
    (cond ((eql (ash (sys.int::memref-unsigned-byte-8 final-entry) (- sys.int::+object-type-shift+))
                sys.int::+object-tag-freelist-entry+)
           ;; Final entry in the area is a freelist entry, extend it by the new amount.
           (let* ((existing-len (ash (sys.int::memref-unsigned-byte-64 final-entry 0) (- sys.int::+object-data-shift+)))
                  (existing-bin (integer-length existing-len))
                  (new-len (+ len existing-len))
                  (new-bin (integer-length new-len)))
             (when (not (eql new-bin existing-bin))
               ;; Bin changed, need to remove from the old bin and reinsert into the new.
               (loop
                  with prev = nil
                  with curr = (svref sys.int::*pinned-area-free-bins* existing-bin)
                  do
                    (when (not curr)
                      (mezzano.supervisor:panic "Can't find freelist entry " final-entry " in bin " existing-bin))
                    (when (eql curr final-entry)
                      (cond (prev
                             (setf (sys.int::memref-t prev 1) (sys.int::memref-t final-entry 1)))
                            (t
                             (setf (svref sys.int::*pinned-area-free-bins* existing-bin) (sys.int::memref-t final-entry 1))))
                      (return))
                    (setf prev curr
                          curr (sys.int::memref-t curr 1)))
               (setf (sys.int::memref-t final-entry 1) (svref sys.int::*pinned-area-free-bins* new-bin)
                     (svref sys.int::*pinned-area-free-bins* new-bin) final-entry))
             ;; Update header with the new length.
             (setf (sys.int::memref-unsigned-byte-64 final-entry 0) (sys.int::make-freelist-header new-len)))
           (setf new-address final-entry))
          (t
           ;; Create a new freelist entry at the end.
           (let ((bin (integer-length len)))
             (setf (sys.int::memref-unsigned-byte-64 sys.int::*pinned-area-bump* 0) (sys.int::make-freelist-header len)
                   (sys.int::memref-t sys.int::*pinned-area-bump* 1) (svref sys.int::*pinned-area-free-bins* bin))
             (setf (svref sys.int::*pinned-area-free-bins* bin) sys.int::*pinned-area-bump*))))
    ;; Update card table pointers for the new free cards
    (loop
       for card from (sys.int::align-up new-address sys.int::+card-size+) below (+ new-address grow-by) by sys.int::+card-size+
       for delta = (- new-address card)
       do (setf (sys.int::card-table-offset card)
                (if (<= delta (- (* (1- (ash 1 (byte-size sys.int::+card-table-entry-offset+))) 16)))
                    nil
                    delta)))
    (incf sys.int::*pinned-area-bump* grow-by)))

(defun %allocate-from-pinned-area (tag data words)
  (loop
     with inhibit-gc = nil
     for i from 0 do
       (let ((result (%allocate-from-pinned-area-1 tag data words)))
         (when result
           (return result)))
       (when (not (eql i 0))
         ;; The GC has been run at least once, try enlarging the pinned area.
         (let ((grow-by (* words 8)))
           (incf grow-by (1- sys.int::+allocation-minimum-alignment+))
           (setf grow-by (logand (lognot (1- sys.int::+allocation-minimum-alignment+))
                                 grow-by))
           (mezzano.supervisor:without-footholds
             (mezzano.supervisor:with-mutex (*allocator-lock*)
               (mezzano.supervisor:with-pseudo-atomic
                   (when (mezzano.supervisor:allocate-memory-range
                          sys.int::*pinned-area-bump*
                          grow-by
                          (logior sys.int::+block-map-present+
                                  sys.int::+block-map-writable+
                                  sys.int::+block-map-zero-fill+
                                  sys.int::+block-map-track-dirty+))
                     (when sys.int::*gc-enable-logging*
                       (mezzano.supervisor:debug-print-line "Expanded pinned area by " grow-by))
                     ;; Success.
                     (finish-expand-pinned-area grow-by)
                     (setf inhibit-gc t)))))))
       (when (> i *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (cond (inhibit-gc
              (setf inhibit-gc nil))
             (t
              (sys.int::gc :full t)))))

(defun %allocate-from-wired-area-unlocked (tag data words)
  (when *paranoid-allocation*
    (verify-freelist sys.int::*wired-area-freelist* sys.int::*wired-area-base* sys.int::*wired-area-bump*))
  (let ((address (%allocate-from-freelist-area tag data words sys.int::*wired-area-free-bins*)))
    (when address
      (sys.int::%%assemble-value address sys.int::+tag-object+))))

(defun %allocate-from-wired-area-1 (tag data words)
  (when (or (not (boundp '*allocator-lock*))
            (eql mezzano.supervisor::*world-stopper*
                 (mezzano.supervisor:current-thread)))
    (return-from %allocate-from-wired-area-1
      (%allocate-from-wired-area-unlocked tag data words)))
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:with-mutex (*allocator-lock*)
      (mezzano.supervisor:with-pseudo-atomic
        (%allocate-from-wired-area-unlocked tag data words)))))

(defun %allocate-from-wired-area (tag data words)
  (loop
     for i from 0 do
       (let ((result (%allocate-from-wired-area-1 tag data words)))
         (when result
           (return result)))
       (when (> i *maximum-allocation-attempts*)
         (error 'storage-condition))
       (sys.int::gc :full t)))

(defun with-live-objects-helper (&rest objects)
  (declare (ignore objects)))

(defmacro with-live-objects (objects &body body)
  "Hold OBJECTS live during the extent of BODY."
  (let ((syms (loop
                 for obj in objects
                 collect (gensym))))
    `(let ,(loop
              for obj in objects
              for sym in syms
              collect (list sym obj))
       (multiple-value-prog1
           (progn ,@body)
         (with-live-objects-helper ,@syms)))))

(defun mangle-pinned/wired-cons (object)
  ;; Convert an object-tagged cons into a cons-tagged cons.
  (with-live-objects (object)
    (let ((addr (sys.int::lisp-object-address object)))
      (sys.int::%%assemble-value (+ (logand addr (lognot #b1111)) 16)
                                 sys.int::+tag-cons+))))

(defun %cons-in-pinned-area (car cdr)
  (let ((object (mangle-pinned/wired-cons
                 (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4))))
    (setf (car object) car
          (cdr object) cdr)
    object))

(defun %cons-in-wired-area (car cdr)
  (let ((object (mangle-pinned/wired-cons
                 (%allocate-from-wired-area sys.int::+object-tag-cons+ 0 4))))
    (setf (car object) car
          (cdr object) cdr)
    object))

#-(or x86-64 arm64)
(defun %allocate-from-general-area (tag data words)
  (sys.int::%atomic-fixnum-add-symbol '*general-allocation-count* 1)
  (%slow-allocate-from-general-area tag data words))

#-(or x86-64 arm64)
(defun %do-allocate-from-general-area (tag data words)
  (cond ((> (+ sys.int::*general-area-gen0-bump* (* words 8)) sys.int::*general-area-gen0-limit*)
         (values tag data words t))
        (t
         ;; Enough size, allocate here.
         (let ((addr (logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                             (dpb sys.int::+address-generation-0+ sys.int::+address-generation+ 0)
                             sys.int::*general-area-gen0-bump*)))
           (incf sys.int::*general-area-gen0-bump* (* words 8))
           ;; Write object header.
           (set-allocated-object-header addr tag data 0)
           (sys.int::%%assemble-value addr sys.int::+tag-object+)))))

(defun dynamic-area-size ()
  (+ sys.int::*general-area-gen0-limit*
     sys.int::*general-area-gen1-limit*
     sys.int::*general-area-limit*
     sys.int::*cons-area-gen0-limit*
     sys.int::*cons-area-gen1-limit*
     sys.int::*cons-area-limit*))

(defun static-area-size ()
  (+ (- sys.int::*wired-area-bump* sys.int::*wired-area-base*)
     (- sys.int::*pinned-area-bump* sys.int::*pinned-area-base*)))

(defun card-table-size ()
  (* (truncate (+ (dynamic-area-size)
                  (static-area-size))
               sys.int::+card-size+)
     sys.int::+card-table-entry-size+))

(defun total-normal-usage ()
  (+ (dynamic-area-size)
     (static-area-size)
     sys.int::*bytes-allocated-to-stacks*
     (card-table-size)))

(defun additional-memory-required-for-gc ()
  ;; A worst-case GC cycle will not free any memory and will
  ;; copy DYNAMIC-AREA-SIZE bytes up a generation. The collection itself
  ;; requires this many bytes during the GC cycle.
  (+ (dynamic-area-size)
     ;; Plus card table mappings for it.
     (* (truncate (dynamic-area-size) sys.int::+card-size+)
        sys.int::+card-table-entry-size+)
     ;; And some extra, just in case.
     *allocation-fudge*))

(defun store-free-bytes ()
  ;; Actual number of bytes free in the store, available for allocation.
  (* (- (mezzano.supervisor:store-statistics)
        mezzano.supervisor::*store-fudge-factor*)
     #x1000))

(defun bytes-remaining ()
  ;; Memory already committed to the dynamic areas will be counted
  ;; in store-free-bytes. Only count the additional memory required
  ;; for GC here.
  ;; If it wasn't then dynamic-area-size would need to be multiplied by 2.
  (- (store-free-bytes) (additional-memory-required-for-gc)))

;; TODO: Might be worth collecting more frequently to reduce the amount of work
;; each gc needs to do. Shorter pauses, but more overall gc time.
(defun expand-allocation-area (name required-minimum-expansion granularity-symbol limit-symbol address-tag)
  (setf required-minimum-expansion (sys.int::align-up required-minimum-expansion sys.int::+allocation-minimum-alignment+))
  (let* ((current-limit (sys.int::symbol-global-value limit-symbol))
         (remaining (sys.int::align-down (bytes-remaining) sys.int::+allocation-minimum-alignment+))
         (expansion (max required-minimum-expansion
                         (sys.int::symbol-global-value granularity-symbol)))
         ;; Dynamic areas need twice the space for collection.
         (effective-expansion (* expansion 2)))
    (when (< remaining effective-expansion)
      ;; Expansion exceeds remaining, reset it.
      (setf expansion (max required-minimum-expansion sys.int::+allocation-minimum-alignment+)
            effective-expansion (* expansion 2)))
    (when sys.int::*gc-enable-logging*
      (mezzano.supervisor:debug-print-line "Expanding " name " area by " expansion " [remaining " remaining "]"))
    (cond ((and (>= remaining effective-expansion)
                (mezzano.supervisor:allocate-memory-range
                 (logior (dpb sys.int::+address-generation-0+ sys.int::+address-generation+ 0)
                         (ash address-tag sys.int::+address-tag-shift+)
                         current-limit)
                 expansion
                 (logior sys.int::+block-map-present+
                         sys.int::+block-map-writable+
                         sys.int::+block-map-zero-fill+)))
           ;; Double expansion granularity for next time.
           (setf (sys.int::symbol-global-value granularity-symbol) (* expansion 2))
           ;; Atomically store the new limit, other CPUs may be reading the value.
           (sys.int::%atomic-fixnum-add-symbol limit-symbol expansion)
           (when sys.int::*gc-enable-logging*
             (mezzano.supervisor:debug-print-line "new remaining: " (bytes-remaining)))
           t)
          (t
           ;; Expansion failed, either not enough space or rejected by the pager.
           (when sys.int::*gc-enable-logging*
             (mezzano.supervisor:debug-print-line "A-M-R failed."))
           nil))))

(defun %slow-allocate-from-general-area (tag data words)
  (let ((gc-count 0))
    (tagbody
     OUTER-LOOP
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:with-mutex (*allocator-lock*)
           (mezzano.supervisor:with-pseudo-atomic
             (tagbody
              INNER-LOOP
                (multiple-value-bind (result ignore1 ignore2 failurep)
                    (%do-allocate-from-general-area tag data words)
                  (declare (ignore ignore1 ignore2))
                  (when (not failurep)
                    (return-from %slow-allocate-from-general-area
                      result)))
                ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
                ;; Running the GC cannot be done when pseudo-atomic.
                (cond ((expand-allocation-area :general
                                               (* words 8)
                                               '*general-area-expansion-granularity*
                                               'sys.int::*general-area-gen0-limit*
                                               sys.int::+address-tag-general+)
                       ;; Successfully expanded the area. Retry the allocation.
                       (go INNER-LOOP))
                      (t
                       ;; No memory do expand, bail out and run the GC.
                       ;; This cannot be done when pseudo-atomic.
                       (when sys.int::*gc-enable-logging*
                         (mezzano.supervisor:debug-print-line "General area expansion failed, performing GC."))
                       (go DO-GC)))))))
     DO-GC
       ;; Must occur outside the locks.
       (when (> gc-count *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (incf gc-count)
       (sys.int::gc :full (not (eql gc-count 1)))
       (go OUTER-LOOP))))

(defun %allocate-object (tag data size area)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (log-allocation-profile-entry size)
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (sys.int::%atomic-fixnum-add-symbol '*bytes-consed* (* words 8))
    (ecase area
      ((nil)
       (%allocate-from-general-area tag data words))
      (:pinned
       (%allocate-from-pinned-area tag data words))
      (:wired
       (%allocate-from-wired-area tag data words)))))

(defun sys.int::cons-in-area (car cdr &optional area)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (ecase area
    ((nil)
     (cons car cdr))
    (:pinned
     (log-allocation-profile-entry 2)
     (sys.int::%atomic-fixnum-add-symbol '*bytes-consed* 32)
     (%cons-in-pinned-area car cdr))
    (:wired
     (log-allocation-profile-entry 2)
     (sys.int::%atomic-fixnum-add-symbol '*bytes-consed* 32)
     (%cons-in-wired-area car cdr))))

#-(or x86-64 arm64)
(defun cons (car cdr)
  (sys.int::%atomic-fixnum-add-symbol '*cons-allocation-count* 1)
  (sys.int::%atomic-fixnum-add-symbol '*bytes-consed* 16)
  (slow-cons car cdr))

#-(or x86-64 arm64)
(defun do-cons (car cdr)
  (cond ((> (+ sys.int::*cons-area-gen0-bump* 16) sys.int::*cons-area-gen0-limit*)
         (values car cdr t))
        (t
         ;; Enough size, allocate here.
         (let* ((addr (logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                              (dpb sys.int::+address-generation-0+ sys.int::+address-generation+ 0)
                              sys.int::*cons-area-gen0-bump*))
                (val (sys.int::%%assemble-value addr sys.int::+tag-cons+)))
           (incf sys.int::*cons-area-gen0-bump* 16)
           (setf (car val) car
                 (cdr val) cdr)
           val))))

(defun slow-cons (car cdr)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (log-allocation-profile-entry 2)
  (let ((gc-count 0))
    (tagbody
     OUTER-LOOP
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:with-mutex (*allocator-lock*)
           (mezzano.supervisor:with-pseudo-atomic
             (tagbody
              INNER-LOOP
                ;; Call the real allocator.
                (multiple-value-bind (result blah failurep)
                    (do-cons car cdr)
                  (declare (ignore blah))
                  (when (not failurep)
                    (return-from slow-cons result)))
                ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
                ;; Running the GC cannot be done when pseudo-atomic.
                (cond ((expand-allocation-area :cons
                                               16
                                               '*cons-area-expansion-granularity*
                                               'sys.int::*cons-area-gen0-limit*
                                               sys.int::+address-tag-cons+)
                       ;; Successfully expanded the area Retry the allocation.
                       (go INNER-LOOP))
                      (t
                       ;; No memory do expand, bail out and run the GC.
                       ;; This cannot be done when pseudo-atomic.
                       (when sys.int::*gc-enable-logging*
                         (mezzano.supervisor:debug-print-line "Cons area expansion failed, performing GC."))
                       (go DO-GC)))))))
     DO-GC
       ;; Must occur outside the locks.
       (when (> gc-count *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (incf gc-count)
       (sys.int::gc :full (not (eql gc-count 1)))
       (go OUTER-LOOP))))

(defun sys.int::make-simple-vector (size &optional area)
  (%allocate-object sys.int::+object-tag-array-t+ size size area))

(defun sys.int::%make-struct (size &optional area)
  (%allocate-object sys.int::+object-tag-structure-object+ size size area))

(defun sys.int::make-closure (function environment &optional area)
  "Allocate a closure object."
  (check-type function function)
  (let* ((closure (%allocate-object sys.int::+object-tag-closure+ 3 3 area))
         (entry-point (sys.int::%object-ref-unsigned-byte-64
                       function
                       sys.int::+function-entry-point+)))
    (setf
     ;; Entry point
     (sys.int::%object-ref-unsigned-byte-64 closure sys.int::+function-entry-point+) entry-point
     ;; Initialize constant pool
     (sys.int::%object-ref-t closure sys.int::+closure-function+) function
     (sys.int::%object-ref-t closure 2) environment)
    closure))

(defun make-symbol (name)
  (check-type name string)
  ;; FIXME: Copy name into the wired area and unicode normalize it.
  (let* ((symbol (%allocate-object sys.int::+object-tag-symbol+ 0 6 :wired))
         (global-value (%allocate-object sys.int::+object-tag-array-t+ 3 3 :wired)))
    (setf (svref global-value sys.int::+symbol-value-cell-symbol+) symbol)
    (setf (svref global-value sys.int::+symbol-value-cell-value+) (sys.int::%unbound-value))
    (setf (sys.int::%object-ref-t symbol sys.int::+symbol-name+) name)
    (setf (sys.int::%object-ref-t symbol sys.int::+symbol-value+) global-value)
    (setf (sys.int::%object-ref-t symbol sys.int::+symbol-function+) nil
          (symbol-plist symbol) '()
          (symbol-package symbol) nil)
    (setf (sys.int::%object-ref-t symbol sys.int::+symbol-type+) 't)
    symbol))

(defun copy-symbol (symbol &optional copy-properties)
  (check-type symbol symbol)
  (let ((new-sym (make-symbol (symbol-name symbol))))
    (when copy-properties
      (when (boundp symbol)
        (setf (symbol-value new-sym) (symbol-value symbol)))
      (when (fboundp symbol)
        (setf (symbol-function new-sym) (symbol-function symbol)))
      (setf (symbol-plist new-sym) (copy-list (symbol-plist symbol))))
    new-sym))

;;; This is used by the bignum code so that bignums and fixnums don't have
;;; to be directly compared.
(defun sys.int::%make-bignum-from-fixnum (n)
  (let ((bignum (sys.int::%make-bignum-of-length 1)))
    (setf (sys.int::%object-ref-signed-byte-64 bignum 0) n)
    bignum))

(defun sys.int::%make-bignum-of-length (words &optional area)
  (%allocate-object sys.int::+object-tag-bignum+ words words area))

(defun sys.int::allocate-std-instance (class slots layout &optional area)
  (let ((value (%allocate-object sys.int::+object-tag-std-instance+ 3 3 area)))
    (setf (sys.int::std-instance-class value) class
          (sys.int::std-instance-slots value) slots
          (sys.int::std-instance-layout value) layout)
    value))

(defun sys.int::make-function-with-fixups (tag machine-code fixups constants gc-info &optional wired)
  (let* ((mc-size (ceiling (+ (length machine-code) 16) 16))
         (gc-info-size (ceiling (length gc-info) 8))
         (pool-size (length constants))
         (total (+ (* mc-size 2) pool-size gc-info-size)))
    (assert (< mc-size (ash 1 (byte-size sys.int::+function-header-code-size+))))
    (assert (< pool-size (ash 1 (byte-size sys.int::+function-header-pool-size+))))
    (assert (< (length gc-info) (ash 1 (byte-size sys.int::+function-header-metadata-size+))))
    (when (oddp total)
      (incf total))
    (let* ((object (%allocate-object tag
                                     (logior (dpb mc-size sys.int::+function-header-code-size+ 0)
                                             (dpb pool-size sys.int::+function-header-pool-size+ 0)
                                             (dpb (length gc-info) sys.int::+function-header-metadata-size+ 0))
                                     (1- total) ; subtract header.
                                     (if wired :wired :pinned)))
           (address (ash (sys.int::%pointer-field object) 4)))
      ;; Initialize entry point.
      (setf (sys.int::%object-ref-unsigned-byte-64 object sys.int::+function-entry-point+) (+ address 16))
      ;; Initialize code.
      (dotimes (i (length machine-code))
        (setf (sys.int::memref-unsigned-byte-8 address (+ i 16)) (aref machine-code i)))
      ;; Apply fixups.
      (dolist (fixup fixups)
        (let ((value (case (car fixup)
                       ((nil t)
                        (sys.int::lisp-object-address (car fixup)))
                       (:undefined-function
                        (sys.int::lisp-object-address (sys.int::%undefined-function)))
                       (:closure-trampoline
                        (sys.int::lisp-object-address (sys.int::%closure-trampoline)))
                       (:unbound-value
                        (sys.int::lisp-object-address (sys.int::%unbound-value)))
                       (:funcallable-instance-trampoline
                        (sys.int::lisp-object-address (sys.int::%funcallable-instance-trampoline)))
                       (t (error "Unsupported fixup ~S." (car fixup))))))
          (dotimes (i 4)
            (setf (sys.int::memref-unsigned-byte-8 address (+ (cdr fixup) i))
                  (logand (ash value (* i -8)) #xFF)))))
      ;; Initialize constant pool.
      (dotimes (i (length constants))
        (setf (sys.int::memref-t (+ address (* mc-size 16)) i) (aref constants i)))
      ;; Initialize GC info.
      (let ((gc-info-offset (+ address (* mc-size 16) (* pool-size 8))))
        (dotimes (i (length gc-info))
          (setf (sys.int::memref-unsigned-byte-8 gc-info-offset i) (aref gc-info i))))
      object)))

(defun sys.int::make-function (machine-code constants gc-info &optional wired)
  (sys.int::make-function-with-fixups sys.int::+object-tag-function+ machine-code '() constants gc-info wired))

(defun sys.int::allocate-funcallable-std-instance (function class slots layout &optional area)
  "Allocate a funcallable instance."
  (check-type function function)
  (let* ((object (%allocate-object sys.int::+object-tag-funcallable-instance+
                                   5
                                   5
                                   area))
         (entry-point (sys.int::%object-ref-unsigned-byte-64
                       (sys.int::%funcallable-instance-trampoline)
                       sys.int::+function-entry-point+)))
    (setf
     ;; Entry point. F-I trampoline.
     ;; TODO: If FUNCTION is an +object-tag-function+, then the entry point could point directly at it.
     (sys.int::%object-ref-unsigned-byte-64 object sys.int::+function-entry-point+) entry-point
     ;; Function and other bits.
     (sys.int::%object-ref-t object sys.int::+funcallable-instance-function+) function
     (sys.int::%object-ref-t object sys.int::+funcallable-instance-class+) class
     (sys.int::%object-ref-t object sys.int::+funcallable-instance-slots+) slots
     (sys.int::%object-ref-t object sys.int::+funcallable-instance-layout+) layout)
    object))

(defun sys.int::make-weak-pointer (key &optional (value key) finalizer area)
  ;; Hold VALUE as long as KEY is live.
  ;; Call FINALIZER when the weak-pointer dies.
  ;; Disallow weak pointers to objects with dynamic-extent allocation.
  (check-type finalizer (or null function))
  (assert (or (sys.int::immediatep key)
              (not (eql (ldb (byte sys.int::+address-tag-size+ sys.int::+address-tag-shift+)
                             (sys.int::lisp-object-address key))
                        sys.int::+address-tag-stack+)))
          (key)
          "Weak pointers to object with dynamic-extent allocation not supported.")
  (let ((object (%allocate-object sys.int::+object-tag-weak-pointer+
                                  ;; Set the live bit in the header before setting the key cell.
                                  ;; If a GC occurs during initialization then the key will
                                  ;; remain live because MAKE-WEAK-POINTER has a strong reference
                                  ;; to it.
                                  ;; %ALLOCATE-OBJECT will initialize the key cell to some
                                  ;; safe object (probably 0).
                                  (ash 1 sys.int::+weak-pointer-header-livep+)
                                  5
                                  area)))
    (when finalizer
      ;; Atomically add to the finalizer list.
      (loop
         for prev-finalizer = sys.int::*known-finalizers*
         do
           (setf (sys.int::%object-ref-t object sys.int::+weak-pointer-finalizer-link+)
                 prev-finalizer)
           (when (eql (sys.int::cas (sys.int::symbol-global-value 'sys.int::*known-finalizers*)
                                    prev-finalizer
                                    object)
                      prev-finalizer)
             (return))))
    ;; Order carefully, KEY must be set last or the GC might finalize this
    ;; too soon.
    (setf (sys.int::%object-ref-t object sys.int::+weak-pointer-value+) value
          (sys.int::%object-ref-t object sys.int::+weak-pointer-finalizer+) finalizer
          (sys.int::%object-ref-t object sys.int::+weak-pointer-key+) key)
    object))

(defun sys.int::make-ratio (numerator denominator)
  (let ((value (%allocate-object sys.int::+object-tag-ratio+ 0 2 nil)))
    (setf (sys.int::%object-ref-t value sys.int::+ratio-numerator+) numerator
          (sys.int::%object-ref-t value sys.int::+ratio-denominator+) denominator)
    value))

;;; In the supervisor package for historical reasons.
(in-package :mezzano.supervisor)

(defstruct (stack
             (:constructor %make-stack (base size))
             (:area :wired))
  base
  size)

(defun %allocate-stack-1 (aligned-size actual-size bump-sym)
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:with-mutex (mezzano.runtime::*allocator-lock*)
      (when (< (mezzano.runtime::bytes-remaining) size)
        (sys.int::gc :full t))
      (prog1 (logior (+ (symbol-value bump-sym) #x200000) ; + 2MB for guard page
                     (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+))
        (incf (symbol-value bump-sym) aligned-size)))))

(defconstant +stack-guard-size+ #x200000)
(defconstant +stack-region-alignment+ #x200000)

;; TODO: Actually allocate virtual memory.
(defun %allocate-stack (size &optional wired)
  (declare (sys.c::closure-allocation :wired))
  (setf size (align-up size #x1000))
  (let* ((gc-count 0)
         (stack-address nil)
         (stack (%make-stack nil size)))
    ;; Allocate the stack object & finalizer up-front to prevent any issues
    ;; if the system runs out of memory while allocating the stack.
    (sys.int::make-weak-pointer stack stack
                                (lambda ()
                                  (when stack-address
                                    (release-memory-range stack-address size)
                                    (sys.int::%atomic-fixnum-add-symbol 'sys.int::*bytes-allocated-to-stacks* (- size))))
                                :wired)
    (tagbody
     RETRY
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:with-mutex (mezzano.runtime::*allocator-lock*)
           (when (< (mezzano.runtime::bytes-remaining) size)
             (go DO-GC))
           ;; This is where the stack starts in virtual memory.
           (let* ((bump (+ +stack-guard-size+
                           (if wired
                               sys.int::*wired-stack-area-bump*
                               sys.int::*stack-area-bump*)))
                  (addr (logior bump
                                (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+))))
             ;; Allocate backing mmory.
             (when (not (allocate-memory-range addr size
                                               (logior sys.int::+block-map-present+
                                                       sys.int::+block-map-writable+
                                                       sys.int::+block-map-zero-fill+
                                                       (if wired
                                                           sys.int::+block-map-wired+
                                                           0))))
               (go DO-GC))
             ;; Memory actually allocated, now update bump pointers.
             (if wired
                 (setf sys.int::*wired-stack-area-bump* (align-up (+ bump size) +stack-region-alignment+))
                 (setf sys.int::*stack-area-bump* (align-up (+ bump size) +stack-region-alignment+)))
             (sys.int::%atomic-fixnum-add-symbol 'sys.int::*bytes-allocated-to-stacks* size)
             (setf (stack-base stack) addr
                   ;; Notify the finalizer that the stack has been allocated & should be freed.
                   stack-address addr)
             (return-from %allocate-stack stack))))
     DO-GC
       (when (> gc-count mezzano.runtime::*maximum-allocation-attempts*)
         (error 'storage-condition))
       (incf gc-count)
       (debug-print-line "No memory for stack, calling GC.")
       (sys.int::gc :full t)
       (go RETRY))))

;;; Card table.
;;; This would be in gc.lisp, but it needs to be wired.

(in-package :sys.int)

;; Card table offsets are only valid for pinned/wired objects.
;; They are only needed for converting function return addresses
;; to function objects, and functions can only be allocated in
;; the pinned/wired areas.

(defun card-table-offset (address)
  ;; 16-bit accesses are used here to avoid interfering with
  ;; accesses to the the high half containing the flag bits.
  (let* ((offset (memref-unsigned-byte-16 +card-table-base+
                                          ;; Multiply by 2 because this is a 16 bit access.
                                          (* (truncate address +card-size+) 2))))
    (cond ((eql offset (1- (ash 1 (byte-size +card-table-entry-offset+))))
           nil)
          (t
           (- (* offset 16))))))

(defun (setf card-table-offset) (value address)
  (cond (value
         (assert (not (plusp value)))
         (assert (not (logtest value 15)))
         (assert (< (- (* (1- (ash 1 (byte-size sys.int::+card-table-entry-offset+))) 16)) value))
         (setf (memref-unsigned-byte-16 +card-table-base+
                                        (* (truncate address +card-size+) 2))
               (truncate (- value) 16)))
        (t
         (setf (memref-unsigned-byte-16 +card-table-base+
                                        (* (truncate address +card-size+) 2))
               (1- (ash 1 (byte-size +card-table-entry-offset+))))))
  value)

(defun card-table-dirty-gen (address)
  (let* ((cte (memref-unsigned-byte-32 +card-table-base+ (truncate address +card-size+)))
         (gen (ldb +cart-table-entry-dirty-gen+ cte)))
    (if (eql gen 0)
        nil
        (1- gen))))

(defun (setf card-table-dirty-gen) (value address)
  (assert (member value '(nil 0 1 2)))
  (let ((index (truncate address +card-size+))
        (entry (if value
                   (1+ value)
                   0)))
    (loop
       ;; TODO: Atomic or/and, instead of this cas loop.
       (let* ((original-cte (memref-unsigned-byte-32 +card-table-base+ index))
              (new-cte (dpb entry +cart-table-entry-dirty-gen+ original-cte)))
         (when (eq (cas (memref-unsigned-byte-32 +card-table-base+ index)
                        original-cte
                        new-cte)
                    original-cte)
           (return)))))
  value)
