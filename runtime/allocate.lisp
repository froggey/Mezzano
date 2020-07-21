;;;; Object allocation

(in-package :mezzano.runtime)

(sys.int::defglobal *paranoid-allocation*)

(sys.int::defglobal sys.int::*wired-area-base*)
(sys.int::defglobal sys.int::*wired-area-bump*)
(sys.int::defglobal sys.int::*wired-area-free-bins*)
(sys.int::defglobal sys.int::*wired-area-usage*)
(sys.int::defglobal sys.int::*pinned-area-base*)
(sys.int::defglobal sys.int::*pinned-area-bump*)
(sys.int::defglobal sys.int::*pinned-area-free-bins*)
(sys.int::defglobal sys.int::*pinned-area-usage*)

(sys.int::defglobal sys.int::*bytes-allocated-to-stacks*)
(sys.int::defglobal sys.int::*wired-stack-area-bump*)
(sys.int::defglobal sys.int::*stack-area-bump*)

(sys.int::defglobal sys.int::*general-area-young-gen-bump*)
(sys.int::defglobal sys.int::*general-area-young-gen-limit*)
(sys.int::defglobal sys.int::*general-area-old-gen-bump*)
(sys.int::defglobal sys.int::*general-area-old-gen-limit*)
(sys.int::defglobal sys.int::*cons-area-young-gen-bump*)
(sys.int::defglobal sys.int::*cons-area-young-gen-limit*)
(sys.int::defglobal sys.int::*cons-area-old-gen-bump*)
(sys.int::defglobal sys.int::*cons-area-old-gen-limit*)

(sys.int::defglobal sys.int::*function-area-base*)
(sys.int::defglobal sys.int::*wired-function-area-limit*)
(sys.int::defglobal sys.int::*wired-function-area-free-bins*)
(sys.int::defglobal sys.int::*wired-function-area-usage*)
(sys.int::defglobal sys.int::*function-area-limit*)
(sys.int::defglobal sys.int::*function-area-free-bins*)
(sys.int::defglobal sys.int::*function-area-usage* 0)

;; A major GC will be performed when the old generation
;; is this much larger than the young generation.
(sys.int::defglobal sys.int::*generation-size-ratio*)

(sys.int::defglobal sys.int::*young-gen-newspace-bit*)
(sys.int::defglobal sys.int::*young-gen-newspace-bit-raw*)
(sys.int::defglobal sys.int::*old-gen-newspace-bit*)

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
        sys.int::*young-gen-newspace-bit* 0
        sys.int::*young-gen-newspace-bit-raw* 0
        sys.int::*old-gen-newspace-bit* 0
        sys.int::*general-area-young-gen-bump* 0
        sys.int::*general-area-young-gen-limit* 0
        sys.int::*cons-area-young-gen-bump* 0
        sys.int::*cons-area-young-gen-limit* 0
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
        sys.int::*generation-size-ratio* 2)
  (setf mezzano.supervisor::*dma-buffer-virtual-address-bump* #x0000204000000000))

(defun set-allocated-object-header (address tag data)
  ;; Be careful to avoid bignum consing here. Some functions can have a
  ;; data value larger than a fixnum when shifted.
  (setf (sys.int::memref-unsigned-byte-32 address 0) (logior (ash tag sys.int::+object-type-shift+)
                                                             (ash (ldb (byte (- 32 sys.int::+object-data-shift+) 0) data) sys.int::+object-data-shift+))
        (sys.int::memref-unsigned-byte-32 address 1) (ldb (byte 32 (- 32 sys.int::+object-data-shift+)) data)))

(defun %freelist-allocate-internal (freelist prev size log2-len tag data words bins)
  ;; Remove it from the bin.
  (cond (prev
         (setf (freelist-entry-next prev) (freelist-entry-next freelist)))
        (t
         (setf (svref bins log2-len) (freelist-entry-next freelist))))
  (when (not (eql size words))
    ;; Entry is too large, split it.
    (let* ((new-size (- size words))
           (new-bin (integer-length new-size))
           (next (+ freelist (* words 8))))
      (setf (sys.int::memref-unsigned-byte-64 next 0) (logior (ash sys.int::+object-tag-freelist-entry+ sys.int::+object-type-shift+)
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
  (set-allocated-object-header freelist tag data)
  ;; Clear data.
  (sys.int::%fill-words (+ freelist 8) 0 (1- words))
  ;; Return address.
  freelist)

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
             (return-from %allocate-from-freelist-area
               (%freelist-allocate-internal freelist prev size log2-len tag data words bins)))))
       (incf log2-len))))

(defun %allocate-from-pinned-area-1 (tag data words)
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
      (mezzano.supervisor:with-mutex (*allocator-lock*)
        (mezzano.supervisor:with-pseudo-atomic
          (let ((address (%allocate-from-freelist-area tag data words sys.int::*pinned-area-free-bins*)))
            (when address
              (incf sys.int::*pinned-area-usage* words)
              (sys.int::%%assemble-value address sys.int::+tag-object+))))))))

(defun finish-expand-freelist-area (grow-by limit-sym bins)
  (let ((len (truncate grow-by 8))
        (final-entry (sys.int::base-address-of-internal-pointer
                      (- (sys.int::symbol-global-value limit-sym) 16)))
        (new-address (sys.int::symbol-global-value limit-sym)))
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
                  with curr = (svref bins existing-bin)
                  do
                    (when (not curr)
                      (mezzano.supervisor:panic "Can't find freelist entry " final-entry " in bin " existing-bin))
                    (when (eql curr final-entry)
                      (cond (prev
                             (setf (sys.int::memref-t prev 1) (sys.int::memref-t final-entry 1)))
                            (t
                             (setf (svref bins existing-bin) (sys.int::memref-t final-entry 1))))
                      (return))
                    (setf prev curr
                          curr (sys.int::memref-t curr 1)))
               (setf (sys.int::memref-t final-entry 1) (svref bins new-bin)
                     (svref bins new-bin) final-entry))
             ;; Update header with the new length.
             (setf (sys.int::memref-unsigned-byte-64 final-entry 0) (sys.int::make-freelist-header new-len)))
           (setf new-address final-entry))
          (t
           ;; Create a new freelist entry at the end.
           (let ((bin (integer-length len)))
             (setf (sys.int::memref-unsigned-byte-64 new-address 0) (sys.int::make-freelist-header len)
                   (sys.int::memref-t new-address 1) (svref bins bin))
             (setf (svref bins bin) new-address))))
    ;; Update card table pointers for the new free cards
    (loop
       for card from (sys.int::align-up new-address sys.int::+card-size+) below (+ new-address grow-by) by sys.int::+card-size+
       for delta = (- new-address card)
       do (setf (sys.int::card-table-offset card)
                (if (<= delta (- (* (1- (ash 1 (byte-size sys.int::+card-table-entry-offset+))) 16)))
                    nil
                    delta)))
    (incf (sys.int::symbol-global-value limit-sym) grow-by)))

(defun update-allocation-time (start-time)
  (incf (mezzano.supervisor:thread-allocation-time
         (mezzano.supervisor:current-thread))
        (mezzano.supervisor:high-precision-time-units-to-internal-time-units
         (- (mezzano.supervisor:get-high-precision-timer) start-time))))

(defun %allocate-from-pinned-area (tag data words)
  (loop
     with start-time = (mezzano.supervisor:get-high-precision-timer)
     with inhibit-gc = nil
     for i from 0 do
       (let ((result (%allocate-from-pinned-area-1 tag data words)))
         (when result
           (update-allocation-time start-time)
           (return result)))
       (when (not (eql i 0))
         ;; The GC has been run at least once, try enlarging the pinned area.
         (let ((grow-by (* words 8)))
           (incf grow-by (1- sys.int::+allocation-minimum-alignment+))
           (setf grow-by (logand (lognot (1- sys.int::+allocation-minimum-alignment+))
                                 grow-by))
           (when sys.int::*gc-enable-logging*
             (mezzano.supervisor:debug-print-line
              "Expanding PINNED area by " grow-by))
           (mezzano.supervisor:without-footholds
             (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
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
                     (finish-expand-freelist-area grow-by 'sys.int::*pinned-area-bump* sys.int::*pinned-area-free-bins*)
                     (setf inhibit-gc t))))))))
       (when (> i *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (cond (inhibit-gc
              (setf inhibit-gc nil))
             (t
              (sys.int::%gc :reason :pinned :major-required t :full (not (zerop i)))))))

(defun %allocate-from-wired-area-unlocked (tag data words)
  (let ((address (%allocate-from-freelist-area tag data words sys.int::*wired-area-free-bins*)))
    (when address
      (incf sys.int::*wired-area-usage* words)
      (sys.int::%%assemble-value address sys.int::+tag-object+))))

(defun %allocate-from-wired-area-1 (tag data words)
  (when (or (not (boundp '*allocator-lock*))
            (eql mezzano.supervisor::*world-stopper*
                 (mezzano.supervisor:current-thread)))
    (return-from %allocate-from-wired-area-1
      (%allocate-from-wired-area-unlocked tag data words)))
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
      (mezzano.supervisor:with-mutex (*allocator-lock*)
        (mezzano.supervisor:with-pseudo-atomic
          (%allocate-from-wired-area-unlocked tag data words))))))

(defun %allocate-from-wired-area (tag data words)
  (loop
     with start-time = (mezzano.supervisor:get-high-precision-timer)
     for i from 0 do
       (let ((result (%allocate-from-wired-area-1 tag data words)))
         (when result
           (update-allocation-time start-time)
           (return result)))
       (when (> i *maximum-allocation-attempts*)
         (error 'storage-condition))
       (sys.int::%gc :reason :wired :major-required t :full (not (zerop i)))))

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
  (cond ((> (+ sys.int::*general-area-young-gen-bump* (* words 8)) sys.int::*general-area-young-gen-limit*)
         (values tag data words t))
        (t
         ;; Enough size, allocate here.
         (let ((addr (logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                             sys.int::*young-gen-newspace-bit*
                             sys.int::*general-area-young-gen-bump*)))
           (incf sys.int::*general-area-young-gen-bump* (* words 8))
           ;; Write object header.
           (set-allocated-object-header addr tag data)
           (sys.int::%%assemble-value addr sys.int::+tag-object+)))))

(defun dynamic-area-size ()
  (+ sys.int::*general-area-young-gen-limit*
     sys.int::*general-area-old-gen-limit*
     sys.int::*cons-area-young-gen-limit*
     sys.int::*cons-area-old-gen-limit*))

(defun static-area-size ()
  (+ (- sys.int::*wired-area-bump* sys.int::*wired-area-base*)
     (- sys.int::*pinned-area-bump* sys.int::*pinned-area-base*)
     (- sys.int::*function-area-base* sys.int::*wired-function-area-limit*)
     (- sys.int::*function-area-limit* sys.int::*function-area-base*)))

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
     ;; And mark bits for static space.
     (/ (static-area-size) sys.int::+octets-per-mark-bit+ 8)
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
                 (logior sys.int::*young-gen-newspace-bit*
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
  (let ((gc-count 0)
        (start-time (mezzano.supervisor:get-high-precision-timer)))
    (tagbody
     OUTER-LOOP
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
           (mezzano.supervisor:with-mutex (*allocator-lock*)
             (mezzano.supervisor:with-pseudo-atomic
               (tagbody
                INNER-LOOP
                  (multiple-value-bind (result ignore1 ignore2 failurep)
                      (%do-allocate-from-general-area tag data words)
                    (declare (ignore ignore1 ignore2))
                    (when (not failurep)
                      (update-allocation-time start-time)
                      (return-from %slow-allocate-from-general-area
                        result)))
                  ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
                  ;; Running the GC cannot be done when pseudo-atomic.
                  (cond ((expand-allocation-area :general
                                                 (* words 8)
                                                 '*general-area-expansion-granularity*
                                                 'sys.int::*general-area-young-gen-limit*
                                                 sys.int::+address-tag-general+)
                         ;; Successfully expanded the area. Retry the allocation.
                         (go INNER-LOOP))
                        (t
                         ;; No memory do expand, bail out and run the GC.
                         ;; This cannot be done when pseudo-atomic.
                         (when sys.int::*gc-enable-logging*
                           (mezzano.supervisor:debug-print-line "General area expansion failed, performing GC."))
                         (go DO-GC))))))))
     DO-GC
       ;; Must occur outside the locks.
       (when (> gc-count *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (incf gc-count)
       (sys.int::%gc :reason :general :full (not (eql gc-count 1)))
       (go OUTER-LOOP))))

(defun %allocate-object (tag data size area)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (log-allocation-profile-entry size)
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (let ((bytes (* words 8)))
      (sys.int::%atomic-fixnum-add-symbol '*bytes-consed* bytes)
      ;; ### This won't accurately track if the thread gets footholded
      ;; partway through the add...
      (incf (mezzano.supervisor:thread-bytes-consed
             (mezzano.supervisor:current-thread))
            bytes))
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
  (cond ((> (+ sys.int::*cons-area-young-gen-bump* 16) sys.int::*cons-area-young-gen-limit*)
         (values car cdr t))
        (t
         ;; Enough size, allocate here.
         (let* ((addr (logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                              sys.int::*young-gen-newspace-bit*
                              sys.int::*cons-area-young-gen-bump*))
                (val (sys.int::%%assemble-value addr sys.int::+tag-cons+)))
           (incf sys.int::*cons-area-young-gen-bump* 16)
           (setf (car val) car
                 (cdr val) cdr)
           val))))

(defun slow-cons (car cdr)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (log-allocation-profile-entry 2)
  (let ((gc-count 0)
        (start-time (mezzano.supervisor:get-high-precision-timer)))
    (tagbody
     OUTER-LOOP
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
           (mezzano.supervisor:with-mutex (*allocator-lock*)
             (mezzano.supervisor:with-pseudo-atomic
               (tagbody
                INNER-LOOP
                  ;; Call the real allocator.
                  (multiple-value-bind (result blah failurep)
                      (do-cons car cdr)
                    (declare (ignore blah))
                    (when (not failurep)
                      (update-allocation-time start-time)
                      (return-from slow-cons result)))
                  ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
                  ;; Running the GC cannot be done when pseudo-atomic.
                  (cond ((expand-allocation-area :cons
                                                 16
                                                 '*cons-area-expansion-granularity*
                                                 'sys.int::*cons-area-young-gen-limit*
                                                 sys.int::+address-tag-cons+)
                         ;; Successfully expanded the area Retry the allocation.
                         (go INNER-LOOP))
                        (t
                         ;; No memory do expand, bail out and run the GC.
                         ;; This cannot be done when pseudo-atomic.
                         (when sys.int::*gc-enable-logging*
                           (mezzano.supervisor:debug-print-line "Cons area expansion failed, performing GC."))
                         (go DO-GC))))))))
     DO-GC
       ;; Must occur outside the locks.
       (when (> gc-count *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (incf gc-count)
       (sys.int::%gc :reason :cons :full (not (eql gc-count 1)))
       (go OUTER-LOOP))))

(defun sys.int::make-simple-vector (size &optional area)
  (%allocate-object sys.int::+object-tag-array-t+ size size area))

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
  (let* ((symbol (%allocate-object sys.int::+object-tag-symbol+ 0 5 :wired)))
    (setf (sys.int::%object-ref-t symbol sys.int::+symbol-name+) name)
    (setf (sys.int::%object-ref-t symbol sys.int::+symbol-value+) nil)
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

(defun sys.int::%make-bignum-of-length (words)
  (%allocate-object sys.int::+object-tag-bignum+ words words nil))

(defun %allocate-function-1 (tag data words wiredp)
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
      (mezzano.supervisor:with-mutex (*allocator-lock*)
        (mezzano.supervisor:with-pseudo-atomic
          (let ((address (%allocate-from-freelist-area
                          tag data words
                          (if wiredp
                              sys.int::*wired-function-area-free-bins*
                              sys.int::*function-area-free-bins*))))
            (when address
              (if wiredp
                  (incf sys.int::*wired-function-area-usage* words)
                  (incf sys.int::*function-area-usage* words))
              (sys.int::%%assemble-value address sys.int::+tag-object+))))))))

(defun expand-function-area (words wiredp)
  "Returns true if the area was successfully expanded."
  (when wiredp
    ;; TODO: Implement expanding the wired function area.
    (error 'storage-condition))
  ;; Try enlarging the area.
  (let ((grow-by (* words 8)))
    (incf grow-by (1- sys.int::+allocation-minimum-alignment+))
    (setf grow-by (logand (lognot (1- sys.int::+allocation-minimum-alignment+))
                          grow-by))
    (when sys.int::*gc-enable-logging*
      (mezzano.supervisor:debug-print-line
       "Expanding FUNCTION area by " grow-by))
    (mezzano.supervisor:without-footholds
      (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
        (mezzano.supervisor:with-mutex (*allocator-lock*)
          (mezzano.supervisor:with-pseudo-atomic
            (when (mezzano.supervisor:allocate-memory-range
                   sys.int::*function-area-limit*
                   grow-by
                   (logior sys.int::+block-map-present+
                           sys.int::+block-map-writable+
                           sys.int::+block-map-zero-fill+
                           sys.int::+block-map-track-dirty+))
              (when sys.int::*gc-enable-logging*
                (mezzano.supervisor:debug-print-line "Expanded function area by " grow-by))
              ;; Success.
              (finish-expand-freelist-area grow-by 'sys.int::*function-area-limit* sys.int::*function-area-free-bins*)
              t)))))))

;; Also used for allocating function-references
(defun %allocate-function (tag data words wiredp)
  ;; Force 32-byte alignment.
  (setf words (sys.int::align-up words 4))
  (loop
     with start-time = (mezzano.supervisor:get-high-precision-timer)
     with inhibit-gc = nil
     for i from 0 do
       (let ((result (%allocate-function-1 tag data words wiredp)))
         (when result
           (sys.int::%atomic-fixnum-add-symbol '*bytes-consed* (* words 8))
           (update-allocation-time start-time)
           (return result)))
       (when (not (eql i 0))
         ;; The GC has been run at least once.
         (when (expand-function-area words wiredp)
           (setf inhibit-gc t)))
       (when (> i *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (cond (inhibit-gc
              (setf inhibit-gc nil))
             (t
              (sys.int::%gc :reason (if wiredp :wired-function :function) :major-required t :full (not (zerop i)))
              ;; Try to keep a reasonable amount of space available after collection.
              ;; However, this doesn't account for fragmentation.
              (let* ((words-used (if wiredp
                                     sys.int::*wired-function-area-usage*
                                     sys.int::*function-area-usage*))
                     (words-committed (truncate (if wiredp
                                                    (- sys.int::*function-area-base* sys.int::*wired-function-area-limit*)
                                                    (- sys.int::*function-area-limit* sys.int::*function-area-base*))
                                                8))
                     (words-avail (- words-committed words-used))
                     (bytes-avail (* words-avail 8)))
                (when (and (< bytes-avail sys.int::+allocation-minimum-alignment+) ; chosen arbitrarily
                           (not wiredp)) ; todo.
                  (when (expand-function-area words wiredp)
                    (setf inhibit-gc t))))))))

(defun sys.int::make-function (tag machine-code fixups constants gc-info &optional wired)
  (let* ((mc-size (ceiling (+ (length machine-code) 16) 16))
         (gc-info-size (ceiling (length gc-info) 8))
         (pool-size (length constants))
         (total (+ (* mc-size 2) pool-size gc-info-size)))
    (assert (< mc-size (ash 1 (byte-size sys.int::+function-header-code-size+))))
    (assert (< pool-size (ash 1 (byte-size sys.int::+function-header-pool-size+))))
    (assert (< (length gc-info) (ash 1 (byte-size sys.int::+function-header-metadata-size+))))
    (let* ((object (%allocate-function
                    tag
                    (logior (dpb mc-size sys.int::+function-header-code-size+ 0)
                            (dpb pool-size sys.int::+function-header-pool-size+ 0)
                            (dpb (length gc-info) sys.int::+function-header-metadata-size+ 0))
                    total
                    wired))
           (address (sys.int::object-base-address object)))
      ;; Initialize entry point.
      (setf (sys.int::%object-ref-unsigned-byte-64 object sys.int::+function-entry-point+) (+ address 16))
      ;; Initialize code.
      (dotimes (i (length machine-code))
        (setf (sys.int::%object-ref-unsigned-byte-8 object (+ i 8))
              (aref machine-code i)))
      ;; Apply fixups.
      (loop
         for (fixup . byte-offset) in fixups
         do (etypecase fixup
              (symbol
               (let ((value (case fixup
                              ((nil t)
                               (sys.int::lisp-object-address fixup))
                              (:unbound-value
                               (sys.int::lisp-object-address (sys.int::%unbound-value)))
                              (:symbol-binding-cache-sentinel
                               (sys.int::lisp-object-address (sys.int::%symbol-binding-cache-sentinel)))
                              (:layout-instance-header
                               (sys.int::lisp-object-address (sys.int::%layout-instance-header)))
                              (t (error "Unsupported fixup ~S." fixup)))))
                 (setf (sys.int::%object-ref-unsigned-byte-32-unscaled object (+ -8 byte-offset))
                       value)))
              (sys.int::function-reference
               (let* ((entry (%object-slot-address fixup sys.int::+fref-code+))
                      (absolute-origin (+ address byte-offset 4))
                      (value (- entry absolute-origin)))
                 (check-type value (signed-byte 32))
                 (setf (sys.int::%object-ref-signed-byte-32-unscaled object (+ -8 byte-offset))
                       value)))))
      ;; Initialize constant pool.
      (let ((constant-pool-base (1- (* mc-size 2))))
        (dotimes (i (length constants))
          (setf (sys.int::%object-ref-t object (+ constant-pool-base i))
                (aref constants i))))
      ;; Initialize GC info.
      (let ((gc-info-offset (+ -8 (* mc-size 16) (* pool-size 8))))
        (dotimes (i (length gc-info))
          (setf (sys.int::%object-ref-unsigned-byte-8 object (+ gc-info-offset i)) (aref gc-info i))))
      object)))

(defun sys.int::%allocate-instance (layout)
  (%allocate-object sys.int::+object-tag-instance+
                    (sys.int::lisp-object-address layout)
                    (sys.int::layout-heap-size layout)
                    (sys.int::layout-area layout)))

(declaim (special sys.int::*funcallable-instance-trampoline*))

(defun sys.int::%allocate-funcallable-instance (function layout)
  "Allocate a funcallable instance."
  (check-type function function)
  ;; Layout heap size must be at least 2, to hold the entry point and function.
  ;; TODO: Verify that LAYOUT more thoroughly.
  (assert (>= (sys.int::layout-heap-size layout) 2))
  (let ((object (%allocate-object sys.int::+object-tag-funcallable-instance+
                                  (sys.int::lisp-object-address layout)
                                  (sys.int::layout-heap-size layout)
                                  (sys.int::layout-area layout)))
        (entry-point (sys.int::%object-ref-unsigned-byte-64
                      sys.int::*funcallable-instance-trampoline*
                      sys.int::+function-entry-point+)))
    (setf
     ;; Entry point. F-I trampoline.
     ;; TODO: If FUNCTION is an +object-tag-function+, then the entry point could point directly at it.
     (sys.int::%object-ref-unsigned-byte-64 object sys.int::+function-entry-point+) entry-point
     ;; Function
     (sys.int::%object-ref-t object sys.int::+funcallable-instance-function+) function)
    object))

(defun dynamic-extent-p (object)
  "Returns true if OBJECT has dynamic extent."
  (and (not (sys.int::immediatep object))
       (eql (ldb (byte sys.int::+address-tag-size+ sys.int::+address-tag-shift+)
                 (sys.int::lisp-object-address object))
            sys.int::+address-tag-stack+)))

(defun sys.int::make-weak-pointer (key &key (value key) finalizer area (weakness :key))
  ;; Hold VALUE as long as KEY is live.
  ;; Call FINALIZER when the weak-pointer dies.
  ;; Disallow weak pointers to objects with dynamic-extent allocation.
  (check-type finalizer (or null function))
  (assert (and (not (dynamic-extent-p key))
               (not (dynamic-extent-p value)))
          (key value)
          "Weak pointers to objects with dynamic-extent allocation not supported.")
  (check-type weakness (member :key :value :key-and-value :key-or-value))
  (let ((object (%allocate-object sys.int::+object-tag-weak-pointer+
                                  ;; Set the live bit in the header before setting the key cell.
                                  ;; If a GC occurs during initialization then the key will
                                  ;; remain live because MAKE-WEAK-POINTER has a strong reference
                                  ;; to it.
                                  ;; %ALLOCATE-OBJECT will initialize the key cell to some
                                  ;; safe object (probably 0).
                                  (logior (ash 1 sys.int::+weak-pointer-header-livep+)
                                          (dpb (sys.int::encode-weak-pointer-weakness weakness)
                                               sys.int::+weak-pointer-header-weakness+
                                               0))
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
    ;; Keep KEY & VALUE live until the weak pointer has been fully filled in.
    (with-live-objects (key value)
      (setf (sys.int::%object-ref-t object sys.int::+weak-pointer-key+) key
            (sys.int::%object-ref-t object sys.int::+weak-pointer-value+) value
            (sys.int::%object-ref-t object sys.int::+weak-pointer-finalizer+) finalizer))
    object))

;;; In the supervisor package for historical reasons.
(in-package :mezzano.supervisor)

(defstruct (stack
             (:constructor %make-stack (base size))
             (:area :wired))
  base
  size)

(defconstant +stack-guard-size+ #x200000
  "Size of the hard guard area.
This area exists below the stack and is never allocated or mapped.")
(defconstant +stack-region-alignment+ #x200000)

;; TODO: Actually allocate virtual memory.
(defun %allocate-stack (size &optional wired)
  (declare (mezzano.compiler::closure-allocation :wired))
  (setf size (align-up size #x1000))
  (let* ((gc-count 0)
         (stack-address nil)
         (stack (%make-stack nil size)))
    ;; Allocate the stack object & finalizer up-front to prevent any issues
    ;; if the system runs out of memory while allocating the stack.
    (sys.int::make-weak-pointer
     stack
     :finalizer (lambda ()
                  (when stack-address
                    (release-memory-range stack-address size)
                    (sys.int::%atomic-fixnum-add-symbol 'sys.int::*bytes-allocated-to-stacks* (- size))))
     :area :wired)
    (tagbody
     RETRY
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
           (unwind-protect
                (progn
                  ;; Don't acquire the allocator lock if the world is stopped.
                  ;; This happens when allocating stacks for CPUs during boot.
                  (when (not (eql mezzano.supervisor::*world-stopper* (mezzano.supervisor:current-thread)))
                    (acquire-mutex mezzano.runtime::*allocator-lock*))
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
                    ;; Flush the stack object so it doesn't get held live by the finalizer closure.
                    (let ((s stack))
                      (setf stack nil)
                      (return-from %allocate-stack s))))
             (when (mutex-held-p mezzano.runtime::*allocator-lock*)
               (release-mutex mezzano.runtime::*allocator-lock*)))))
     DO-GC
       (when (> gc-count mezzano.runtime::*maximum-allocation-attempts*)
         (error 'storage-condition))
       (incf gc-count)
       (sys.int::%gc :reason :stack :full t)
       (go RETRY))))

;;; Card table.
;;; This would be in gc.lisp, but it needs to be wired.

(in-package :mezzano.internals)

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
         (gen (ldb +card-table-entry-dirty-gen+ cte)))
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
              (new-cte (dpb entry +card-table-entry-dirty-gen+ original-cte)))
         (when (eq (cas (memref-unsigned-byte-32 +card-table-base+ index)
                        original-cte
                        new-cte)
                    original-cte)
           (return)))))
  value)
