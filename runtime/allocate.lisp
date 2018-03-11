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
(sys.int::defglobal sys.int::*general-area-gen0-max-limit*)
(sys.int::defglobal sys.int::*general-area-gen1-bump*)
(sys.int::defglobal sys.int::*general-area-gen1-limit*)
(sys.int::defglobal sys.int::*general-area-gen1-max-limit*)
(sys.int::defglobal sys.int::*general-area-bump*)
(sys.int::defglobal sys.int::*general-area-limit*)
(sys.int::defglobal sys.int::*cons-area-gen0-bump*)
(sys.int::defglobal sys.int::*cons-area-gen0-limit*)
(sys.int::defglobal sys.int::*cons-area-gen0-max-limit*)
(sys.int::defglobal sys.int::*cons-area-gen1-bump*)
(sys.int::defglobal sys.int::*cons-area-gen1-limit*)
(sys.int::defglobal sys.int::*cons-area-gen1-max-limit*)
(sys.int::defglobal sys.int::*cons-area-bump*)
(sys.int::defglobal sys.int::*cons-area-limit*)

(sys.int::defglobal sys.int::*dynamic-mark-bit*)

(sys.int::defglobal *allocation-fudge*)

(sys.int::defglobal *allocator-lock*)
(sys.int::defglobal *general-area-expansion-granularity*)
(sys.int::defglobal *cons-area-expansion-granularity*)

(defconstant +minimum-expansion-granularity+
  (* sys.int::+allocation-minimum-alignment+ 2))

(sys.int::defglobal *general-fast-path-hits*)
(sys.int::defglobal *general-allocation-count*)
(sys.int::defglobal *cons-fast-path-hits*)
(sys.int::defglobal *cons-allocation-count*)

(sys.int::defglobal *bytes-consed*)

(defvar *maximum-allocation-attempts* 5
  "GC this many times before giving up on an allocation.")

(defvar *enable-allocation-profiling* nil)
(defvar *allocation-profile*)

;; Use a seperate function here, as the compiler seems to want to
;; hoist allocation outside the *ENABLE-ALLOCATION-PROFILING* test.
;; This generates data in the same format as the statistical profiler.
(defun log-allocation-profile-entry-1 ()
  (let ((*enable-allocation-profiling* nil))
    (vector-push-extend :start *allocation-profile*)
    (vector-push-extend (get-internal-real-time) *allocation-profile*)
    (vector-push-extend (mezzano.supervisor:current-thread) *allocation-profile*)
    (vector-push-extend :active *allocation-profile*)
    (vector-push-extend :allocation *allocation-profile*)
    (sys.int::map-backtrace
     (lambda (i fp)
       (let* ((return-address (sys.int::memref-signed-byte-64 fp 1))
              (fn (sys.int::return-address-to-function return-address))
              (fn-address (logand (sys.int::lisp-object-address fn) -16))
              (offset (- return-address fn-address)))
         (vector-push-extend fn *allocation-profile*)
         (vector-push-extend offset *allocation-profile*))))))

(defun log-allocation-profile-entry ()
  (when *enable-allocation-profiling*
    (log-allocation-profile-entry-1)))

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
        sys.int::*general-area-gen0-max-limit* (* 32 1024 1024)
        sys.int::*general-area-gen1-bump* 0
        sys.int::*general-area-gen1-limit* 0
        sys.int::*general-area-gen1-max-limit* (* 128 1024 1024)
        sys.int::*cons-area-gen0-bump* 0
        sys.int::*cons-area-gen0-limit* 0
        sys.int::*cons-area-gen0-max-limit* (* 32 1024 1024)
        sys.int::*cons-area-gen1-bump* 0
        sys.int::*cons-area-gen1-limit* 0
        sys.int::*cons-area-gen1-max-limit* (* 128 1024 1024)
        *enable-allocation-profiling* nil
        *general-area-expansion-granularity* (* 32 1024 1024)
        *cons-area-expansion-granularity* (* 32 1024 1024)
        *general-fast-path-hits* 0
        *general-allocation-count* 0
        *cons-fast-path-hits* 0
        *cons-allocation-count* 0
        *bytes-consed* 0
        *allocator-lock* (mezzano.supervisor:make-mutex "Allocator")
        *allocation-fudge* (* 8 1024 1024)
        sys.int::*bytes-allocated-to-stacks* sys.int::*wired-stack-area-bump*))

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

(defun %allocate-from-pinned-area (tag data words)
  (log-allocation-profile-entry)
  (loop
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
                                  sys.int::+block-map-zero-fill+))
                     (when sys.int::*gc-enable-logging*
                       (mezzano.supervisor:debug-print-line "Expanded pinned area by " grow-by))
                     ;; Success. Advance the pinned area limit and slap down a freelist header.
                     ;; Drop into the GC to rebuild the freelist properly.
                     (setf (sys.int::memref-unsigned-byte-64 sys.int::*pinned-area-bump* 0) (sys.int::make-freelist-header (truncate grow-by 8))
                           (sys.int::memref-t sys.int::*pinned-area-bump* 1) nil)
                     (incf sys.int::*pinned-area-bump* grow-by)))))))
       (when (> i *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (sys.int::gc :full t)))

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
  (log-allocation-profile-entry)
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

(defun bytes-remaining-before-full-gc ()
  (let* ((dynamic-area-size (+ sys.int::*general-area-limit*
                               sys.int::*general-area-gen0-limit*
                               sys.int::*general-area-gen1-limit*
                               sys.int::*cons-area-limit*
                               sys.int::*cons-area-gen0-limit*
                               sys.int::*cons-area-gen1-limit*))
         ;; Memory already committed to the dynamic areas will be counted
         ;; in store-free-bytes. Only count the additional memory required
         ;; for GC here.
         (required-for-gc (+ dynamic-area-size
                             *allocation-fudge*))
         (store-free-bytes (* (- (mezzano.supervisor:store-statistics)
                                 mezzano.supervisor::*store-fudge-factor*)
                              #x1000)))
    (mezzano.supervisor:debug-print-line "g0 " sys.int::*general-area-gen0-limit*)
    (mezzano.supervisor:debug-print-line "g1 " sys.int::*general-area-gen1-limit*)
    (mezzano.supervisor:debug-print-line "g2 " sys.int::*general-area-limit*)
    (mezzano.supervisor:debug-print-line "c0 " sys.int::*cons-area-gen0-limit*)
    (mezzano.supervisor:debug-print-line "c1 " sys.int::*cons-area-gen1-limit*)
    (mezzano.supervisor:debug-print-line "c2 " sys.int::*cons-area-limit*)
    (mezzano.supervisor:debug-print-line "af " *allocation-fudge*)
    (mezzano.supervisor:debug-print-line "fb " store-free-bytes)
    (- store-free-bytes (* required-for-gc 2))))

(defun expand-allocation-area-1 (name granularity limit-symbol address-tag)
  (let ((current-limit (sys.int::symbol-global-value limit-symbol))
        (expansion granularity)
        (remaining (bytes-remaining-before-full-gc)))
    (when sys.int::*gc-enable-logging*
      (mezzano.supervisor:debug-print-line "Expanding " name " area by " expansion " [remaining " remaining "]"))
    ;; These are semispace areas, safely collecting them require twice
    ;; as much memory.
    (when (< remaining (* expansion 2))
      (when sys.int::*gc-enable-logging*
        (mezzano.supervisor:debug-print-line "Expansion exceeds bytes remaining"))
      (return-from expand-allocation-area-1 nil))
    (when (not (mezzano.supervisor:allocate-memory-range
                (logior (dpb sys.int::+address-generation-0+ sys.int::+address-generation+ 0)
                        (ash address-tag sys.int::+address-tag-shift+)
                        current-limit)
                expansion
                (logior sys.int::+block-map-present+
                        sys.int::+block-map-writable+
                        sys.int::+block-map-zero-fill+)))
      (when sys.int::*gc-enable-logging*
        (mezzano.supervisor:debug-print-line "A-M-R newspace failed."))
      (return-from expand-allocation-area-1 nil))
    ;; Atomically store the new limit, other CPUs may be reading the value.
    (sys.int::%atomic-fixnum-add-symbol limit-symbol expansion)
    (when sys.int::*gc-enable-logging*
      (mezzano.supervisor:debug-print-line "new remaining: " (bytes-remaining-before-full-gc))))
  t)

(defun expand-allocation-area (name required-minimum-expansion granularity-symbol limit-symbol soft-max-symbol address-tag)
  (setf required-minimum-expansion (sys.int::align-up required-minimum-expansion #x200000))
  (let ((granularity (sys.int::symbol-global-value granularity-symbol)))
    (cond ((>= (sys.int::symbol-global-value limit-symbol)
               (sys.int::symbol-global-value soft-max-symbol))
           nil)
          ((expand-allocation-area-1 name
                                     (max (sys.int::symbol-global-value granularity-symbol)
                                          required-minimum-expansion)
                                     limit-symbol
                                     address-tag)
           (setf (sys.int::symbol-global-value granularity-symbol) (* granularity 2))
           t)
          ((not (eql granularity +minimum-expansion-granularity+))
           ;; Retry expanding with a minimal granularity.
           (setf (sys.int::symbol-global-value granularity-symbol) +minimum-expansion-granularity+)
           (expand-allocation-area-1 name (max required-minimum-expansion +minimum-expansion-granularity+) limit-symbol address-tag))
          (t
           nil))))

(defun %slow-allocate-from-general-area (tag data words)
  (log-allocation-profile-entry)
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
                                               'sys.int::*general-area-gen0-max-limit*
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
       (sys.int::gc)
       (go OUTER-LOOP))))

(defun %allocate-object (tag data size area)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
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
     (sys.int::%atomic-fixnum-add-symbol '*bytes-consed* 32)
     (%cons-in-pinned-area car cdr))
    (:wired
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
  (log-allocation-profile-entry)
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
                                               'sys.int::*cons-area-gen0-max-limit*
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
       (sys.int::gc)
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

(defun card-table-dirty-p (address)
  (let ((cte (memref-unsigned-byte-32 +card-table-base+ (truncate address +card-size+))))
    (logbitp +cart-table-entry-dirty+ cte)))

(defun (setf card-table-dirty-p) (value address)
  (let ((index (truncate address +card-size+)))
    (loop
       ;; TODO: Atomic or/and, instead of this cas loop.
       (let* ((original-cte (memref-unsigned-byte-32 +card-table-base+ index))
              (new-cte (if value
                           (logior original-cte (ash 1 +cart-table-entry-dirty+))
                           (logand original-cte (lognot (ash 1 +cart-table-entry-dirty+))))))
         (when (eq (cas (memref-unsigned-byte-32 +card-table-base+ index)
                        original-cte
                        new-cte)
                    original-cte)
           (return)))))
  value)
