;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(defvar sys.int::*wired-area-bump*)
(defvar sys.int::*wired-area-freelist*)
(defvar sys.int::*pinned-area-bump*)
(defvar sys.int::*pinned-area-freelist*)
(defvar sys.int::*general-area-bump*)
(defvar sys.int::*general-area-limit*)
(defvar sys.int::*cons-area-bump*)
(defvar sys.int::*cons-area-limit*)
(defvar sys.int::*stack-area-bump*)
(defvar sys.int::*memory-expansion-remaining*)

(defvar *wired-allocator-lock*)
(defvar *allocator-lock*)
(defvar *general-area-expansion-granularity* (* 4 1024 1024))
(defvar *cons-area-expansion-granularity* (* 4 1024 1024))

(defun freelist-entry-next (entry)
  (sys.int::memref-t entry 1))

(defun (setf freelist-entry-next) (value entry)
  (setf (sys.int::memref-t entry 1) value))

(defun freelist-entry-size (entry)
  (ash (sys.int::memref-unsigned-byte-64 entry 0) (- sys.int::+array-length-shift+)))

(defun first-run-initialize-allocator ()
  (setf *wired-allocator-lock* :unlocked
        sys.int::*gc-in-progress* nil
        sys.int::*pinned-mark-bit* 0
        sys.int::*dynamic-mark-bit* 0
        sys.int::*general-area-limit* (logand (+ sys.int::*general-area-bump* #x1FFFFF) (lognot #x1FFFFF))
        sys.int::*cons-area-limit* (logand (+ sys.int::*cons-area-bump* #x1FFFFF) (lognot #x1FFFFF))
        *allocator-lock* (mezzano.supervisor:make-mutex "Allocator")))

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
             (eql (ldb (byte sys.int::+array-type-size+ sys.int::+array-type-shift+)
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
                                                             (ash tag sys.int::+array-type-shift+)
                                                             (ash (ldb (byte (- 32 sys.int::+array-length-shift+) 0) data) sys.int::+array-length-shift+))
        (sys.int::memref-unsigned-byte-32 address 1) (ldb (byte 32 (- 32 sys.int::+array-length-shift+)) data)))

;;; FIXME: The pinned/general/cons allocators must somehow initialize their objects with the
;;; allocator lock released. taking a pagefault with it taken is bad, as it will cause
;;; IRQs to be reenabled. What to do? Make it a mutex? actaully reasonable, but a bit heavy?

;; Simple first-fit freelist allocator for pinned areas.
(defun %allocate-from-pinned-area (tag data words freelist-symbol)
  ;; Traverse the freelist.
  (do ((freelist (symbol-value freelist-symbol) (freelist-entry-next freelist))
       (prev nil freelist))
      ((null freelist)
       ;; No memory. Run a GC cycle, try the allocation again, then enlarge the area.
       ;; TODO...
       (mezzano.supervisor:panic "No memory!!!"))
    (let ((size (freelist-entry-size freelist)))
      (when (>= size words)
        ;; This freelist entry is large enough, use it.
        (let ((next (cond ((eql size words)
                           ;; Entry is exactly the right size.
                           (freelist-entry-next freelist))
                          (t
                           ;; Entry is too large, split it.
                           ;; Always create new entries with the pinned mark bit
                           ;; set. A GC will flip it, making all the freelist
                           ;; entries unmarked. No object can ever point to a freelist entry, so
                           ;; they will never be marked during a gc.
                           (let ((next (+ freelist (* words 8))))
                             (setf (sys.int::memref-unsigned-byte-64 next 0) (logior sys.int::*pinned-mark-bit*
                                                                                     (ash sys.int::+object-tag-freelist-entry+ sys.int::+array-type-shift+)
                                                                                     (ash (- size words) sys.int::+array-length-shift+))
                                   (sys.int::memref-t next 1) (freelist-entry-next freelist))
                             next)))))
          ;; Update the prev's next pointer.
          (cond (prev
                 (setf (freelist-entry-next prev) next))
                (t
                 (setf (symbol-value freelist-symbol) next))))
        ;; Write object header.
        (set-allocated-object-header freelist tag data sys.int::*pinned-mark-bit*)
        ;; Clear data.
        (dotimes (i (1- words))
          (setf (sys.int::memref-unsigned-byte-64 freelist (1+ i)) 0))
        ;; Return address.
        (return-from %allocate-from-pinned-area freelist)))))

(defun %allocate-object (tag data size area)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (ecase area
      ((nil)
       (mezzano.supervisor:without-footholds
         (tagbody
          OUTER-LOOP
            (mezzano.supervisor:with-mutex (*allocator-lock*)
              (tagbody
               INNER-LOOP
                 (return-from %allocate-object
                   (mezzano.supervisor:with-pseudo-atomic
                     (when (> (+ sys.int::*general-area-bump* (* words 8)) sys.int::*general-area-limit*)
                       (go EXPAND-AREA))
                     ;; Enough size, allocate here.
                     (let ((addr (logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                                         sys.int::*general-area-bump*
                                         sys.int::*dynamic-mark-bit*)))
                       (incf sys.int::*general-area-bump* (* words 8))
                       ;; Write array header.
                       (set-allocated-object-header addr tag data 0)
                       (sys.int::%%assemble-value addr sys.int::+tag-object+))))
               EXPAND-AREA
                 ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
                 ;; Cannot be done when pseudo-atomic.
                 ;; Divide granularity by two because this is a semispace area. Need twice as much memory.
                 (let ((expansion (logand (truncate *general-area-expansion-granularity* 2) (lognot #xFFF))))
                   (mezzano.supervisor::debug-print-line "Expanding general area by " expansion)
                   (when (< sys.int::*memory-expansion-remaining* (* expansion 2))
                     ;; Not enough memory, abandon ship, do a gc and then attempt the allocation again.
                     (go DO-GC))
                   (decf sys.int::*memory-expansion-remaining* (* expansion 2))
                   ;; Do new & oldspace allocations seperately, this interacts better with the freelist.
                   (mezzano.supervisor:allocate-memory-range
                    (logior sys.int::*dynamic-mark-bit*
                            (ash sys.int::+address-tag-general+
                                 sys.int::+address-tag-shift+)
                            sys.int::*general-area-limit*)
                    expansion
                    (logior sys.int::+block-map-present+
                            sys.int::+block-map-writable+
                            sys.int::+block-map-zero-fill+))
                   (mezzano.supervisor:allocate-memory-range
                    (logior (logxor sys.int::*dynamic-mark-bit*
                                    (ash 1 sys.int::+address-newspace/oldspace-bit+))
                            (ash sys.int::+address-tag-general+
                                 sys.int::+address-tag-shift+)
                            sys.int::*general-area-limit*)
                    expansion
                    sys.int::+block-map-zero-fill+)
                   (incf sys.int::*general-area-limit* expansion))
                 (go INNER-LOOP)))
          DO-GC
            ;; Must occur outside the locks.
            (sys.int::gc)
            (go OUTER-LOOP))))
      (:pinned
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:with-mutex (*allocator-lock*)
           (mezzano.supervisor:with-pseudo-atomic
             (when *paranoid-allocation*
               (verify-freelist sys.int::*pinned-area-freelist* (* 2 1024 1024 1024) sys.int::*pinned-area-bump*))
             (sys.int::%%assemble-value
              (%allocate-from-pinned-area tag data words 'sys.int::*pinned-area-freelist*)
              sys.int::+tag-object+)))))
      (:wired
       (mezzano.supervisor::safe-without-interrupts (tag data words)
         (mezzano.supervisor:with-symbol-spinlock (*wired-allocator-lock*)
           (when *paranoid-allocation*
             (verify-freelist sys.int::*wired-area-freelist* (* 2 1024 1024) sys.int::*wired-area-bump*))
           (sys.int::%%assemble-value
            (%allocate-from-pinned-area tag data words 'sys.int::*wired-area-freelist*)
            sys.int::+tag-object+)))))))

(defun sys.int::cons-in-area (car cdr &optional area)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (ecase area
    ((nil) (cons car cdr))
    (:pinned
     (mezzano.supervisor:without-footholds
       (mezzano.supervisor:with-mutex (*allocator-lock*)
         (mezzano.supervisor:with-pseudo-atomic
           (when *paranoid-allocation*
             (verify-freelist sys.int::*pinned-area-freelist* (* 2 1024 1024 1024) sys.int::*pinned-area-bump*))
           (let ((val (sys.int::%%assemble-value
                       (+ (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4 'sys.int::*pinned-area-freelist*) 16)
                       sys.int::+tag-cons+)))
             (setf (car val) car
                   (cdr val) cdr)
             val)))))
    (:wired
     (mezzano.supervisor::safe-without-interrupts (car cdr)
       (mezzano.supervisor:with-symbol-spinlock (*wired-allocator-lock*)
         (when *paranoid-allocation*
           (verify-freelist sys.int::*wired-area-freelist* (* 2 1024 1024) sys.int::*wired-area-bump*))
         (let ((val (sys.int::%%assemble-value
                     (+ (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4 'sys.int::*wired-area-freelist*) 16)
                     sys.int::+tag-cons+)))
           (setf (car val) car
                 (cdr val) cdr)
           val))))))

(sys.int::define-lap-function cons ((car cdr))
  ;; Attempt to quickly allocate a cons. Will call SLOW-CONS if things get too hairy.
  ;; This is not even remotely SMP safe.
  ;; R8 = car; R9 = cdr
  ;; Big hammer, disable interrupts. Faster than taking locks & stuff.
  (sys.lap-x86:cli)
  ;; Check argument count.
  (sys.lap-x86:cmp64 :rcx #.(ash 2 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH)
  ;; Check *GC-IN-PROGRESS*.
  (sys.lap-x86:mov64 :rax (:constant sys.int::*gc-in-progress*))
  (sys.lap-x86:cmp64 (:object :rax #.sys.int::+symbol-value+) nil)
  (sys.lap-x86:jne SLOW-PATH)
  ;; Grovel directly in the allocator mutex to make sure that it isn't held.
  (sys.lap-x86:mov64 :rax (:constant *allocator-lock*))
  (sys.lap-x86:mov64 :rax (:object :rax #.sys.int::+symbol-value+))
  (sys.lap-x86:cmp64 (:object :rax 5) nil) ; mutex-owner
  (sys.lap-x86:jne SLOW-PATH)
  ;; Fetch current bump pointer.
  (sys.lap-x86:mov64 :rax (:constant sys.int::*cons-area-bump*))
  (sys.lap-x86:mov64 :rbx (:object :rax #.sys.int::+symbol-value+))
  ;; + 16, size of cons.
  ;; Keep the old bump pointer, that's the address of the cons.
  (sys.lap-x86:lea64 :rsi (:rbx #.(ash 16 #.sys.int::+n-fixnum-bits+)))
  ;; Test against limit.
  (sys.lap-x86:mov64 :rdx (:constant sys.int::*cons-area-limit*))
  (sys.lap-x86:cmp64 :rsi (:object :rdx #.sys.int::+symbol-value+))
  (sys.lap-x86:ja SLOW-PATH)
  ;; Enough space.
  ;; Update the bump pointer.
  (sys.lap-x86:mov64 (:object :rax #.sys.int::+symbol-value+) :rsi)
  ;; Generate the cons object.
  ;; Unfixnumize address.
  (sys.lap-x86:shr64 :rbx #.sys.int::+n-fixnum-bits+)
  ;; Set address bits and the tag bits.
  (sys.lap-x86:mov64 :rax #.(logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                                    sys.int::+tag-cons+))
  (sys.lap-x86:or64 :rbx :rax)
  ;; Set mark bit.
  (sys.lap-x86:mov64 :rax (:constant sys.int::*dynamic-mark-bit*))
  (sys.lap-x86:mov64 :rax (:object :rax #.sys.int::+symbol-value+))
  (sys.lap-x86:shr64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:or64 :rbx :rax)
  ;; RBX now holds a valid cons, with the CAR and CDR set to zero.
  ;; It is safe to turn interrupts on again.
  (sys.lap-x86:sti)
  ;; Initialize the CAR & CDR with interrupts on because touching them may
  ;; trigger paging.
  (sys.lap-x86:mov64 (:car :rbx) :r8)
  (sys.lap-x86:mov64 (:cdr :rbx) :r9)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :r8 :rbx)
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  (sys.lap-x86:sti)
  ;; Tail call into SLOW-CONS.
  (sys.lap-x86:mov64 :r13 (:function slow-cons))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(defun slow-cons (car cdr)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (mezzano.supervisor:without-footholds
    (tagbody
     OUTER-LOOP
       (mezzano.supervisor:with-mutex (*allocator-lock*)
         (tagbody
          INNER-LOOP
            (return-from slow-cons
              (mezzano.supervisor:with-pseudo-atomic
                (when (> (+ sys.int::*cons-area-bump* 16) sys.int::*cons-area-limit*)
                  (go EXPAND-AREA))
                ;; Enough size, allocate here.
                (let* ((addr (logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                                     sys.int::*cons-area-bump*
                                     sys.int::*dynamic-mark-bit*))
                       (val (sys.int::%%assemble-value addr sys.int::+tag-cons+)))
                  (incf sys.int::*cons-area-bump* 16)
                  (setf (car val) car
                        (cdr val) cdr)
                  val)))
          EXPAND-AREA
            ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
            ;; Cannot be done when pseudo-atomic.
            ;; Divide granularity by two because this is a semispace area. Need twice as much memory.
            (let ((expansion (logand (truncate *cons-area-expansion-granularity* 2) (lognot #xFFF))))
              (mezzano.supervisor::debug-print-line "Expanding cons area by " expansion)
              (when (< sys.int::*memory-expansion-remaining* (* expansion 2))
                ;; Not enough memory, abandon ship, do a gc and then attempt the allocation again.
                (go DO-GC))
              (decf sys.int::*memory-expansion-remaining* (* expansion 2))
              ;; Do new & oldspace allocations seperately, this interacts better with the freelist.
              ;; Allocate newspace.
              (mezzano.supervisor:allocate-memory-range
               (logior sys.int::*dynamic-mark-bit*
                       (ash sys.int::+address-tag-cons+
                            sys.int::+address-tag-shift+)
                       sys.int::*cons-area-limit*)
               expansion
               (logior sys.int::+block-map-present+
                       sys.int::+block-map-writable+
                       sys.int::+block-map-zero-fill+))
              ;; Allocate oldspace.
              (mezzano.supervisor:allocate-memory-range
               (logior (logxor sys.int::*dynamic-mark-bit*
                               (ash 1 sys.int::+address-newspace/oldspace-bit+))
                       (ash sys.int::+address-tag-cons+
                            sys.int::+address-tag-shift+)
                       sys.int::*cons-area-limit*)
               expansion
               sys.int::+block-map-zero-fill+)
              (incf sys.int::*cons-area-limit* expansion))
            (go INNER-LOOP)))
     DO-GC
       ;; Must occur outside the locks.
       (sys.int::gc)
       (go OUTER-LOOP))))

(defun sys.int::make-simple-vector (size &optional area)
  (%allocate-object sys.int::+object-tag-array-t+ size size area))

(defun sys.int::%make-struct (size &optional area)
  (%allocate-object sys.int::+object-tag-structure-object+ size size area))

(defun sys.int::make-closure (function environment &optional area)
  "Allocate a closure object."
  (check-type function function)
  (let* ((closure (%allocate-object sys.int::+object-tag-closure+ #x2000100 5 area))
         (entry-point (sys.int::%array-like-ref-unsigned-byte-64
                       function
                       sys.int::+function-entry-point+)))
    (setf
     ;; Entry point
     (sys.int::%array-like-ref-unsigned-byte-64 closure sys.int::+function-entry-point+) entry-point
     ;; Initialize constant pool
     (sys.int::%array-like-ref-t closure sys.int::+closure-function+) function
     (sys.int::%array-like-ref-t closure 2) environment)
    closure))

(defun make-symbol (name)
  (check-type name string)
  ;; FIXME: Copy name into the wired area and unicode normalize it.
  (let* ((symbol (%allocate-object sys.int::+object-tag-symbol+ 0 5 :wired)))
    (setf (sys.int::%array-like-ref-t symbol sys.int::+symbol-name+) name)
    (makunbound symbol)
    (setf (sys.int::%array-like-ref-t symbol sys.int::+symbol-function+) nil
          (symbol-plist symbol) nil
          (symbol-package symbol) nil)
    symbol))

(defun sys.int::%allocate-array-like (tag word-count length &optional area)
  (%allocate-object tag length word-count area))

(sys.int::define-lap-function sys.int::%%make-bignum-128-rdx-rax ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push :rdx)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:mov64 :r13 (:function sys.int::%make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:pop (:r8 #.(+ (- sys.int::+tag-object+) 8)))
  (sys.lap-x86:pop (:r8 #.(+ (- sys.int::+tag-object+) 16)))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%make-bignum-64-rax ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push 0)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r13 (:function sys.int::%make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:pop (:r8 #.(+ (- sys.int::+tag-object+) 8)))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret))

;;; This is used by the bignum code so that bignums and fixnums don't have
;;; to be directly compared.
(defun sys.int::%make-bignum-from-fixnum (n)
  (let ((bignum (sys.int::%make-bignum-of-length 1)))
    (setf (sys.int::%array-like-ref-signed-byte-64 bignum 0) n)
    bignum))

(defun sys.int::%make-bignum-of-length (words &optional area)
  (%allocate-object sys.int::+object-tag-bignum+ words words area))

(defun sys.int::allocate-std-instance (class slots &optional area)
  (let ((value (%allocate-object sys.int::+object-tag-std-instance+ 2 2 area)))
    (setf (sys.int::std-instance-class value) class
          (sys.int::std-instance-slots value) slots)
    value))

(defun sys.int::make-function-with-fixups (tag machine-code fixups constants gc-info &optional wired)
  (let* ((mc-size (ceiling (+ (length machine-code) 16) 16))
         (gc-info-size (ceiling (length gc-info) 8))
         (pool-size (length constants))
         (total (+ (* mc-size 2) pool-size gc-info-size)))
    (when (oddp total)
      (incf total))
    (let* ((object (%allocate-object tag
                                     (logior (ash mc-size 8)
                                             (ash pool-size 24)
                                             (ash (length gc-info) 40))
                                     total
                                     (if wired :wired :pinned)))
           (address (ash (sys.int::%pointer-field object) 4)))
      ;; Initialize entry point.
      (setf (sys.int::%array-like-ref-unsigned-byte-64 object sys.int::+function-entry-point+) (+ address 16))
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
                       (:unbound-tls-slot
                        (sys.int::lisp-object-address (sys.int::%unbound-tls-slot)))
                       (:unbound-value
                        (sys.int::lisp-object-address (sys.int::%unbound-value)))
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

(defun sys.int::allocate-funcallable-std-instance (function class slots)
  "Allocate a funcallable instance."
  (check-type function function)
  (let* ((object (%allocate-object sys.int::+object-tag-funcallable-instance+
                                   ;; MC size (2 2-word units)
                                   ;; constant pool size (4 entries)
                                   ;; GC info size (0 octets)
                                   #x00000004000200
                                   8
                                   :pinned))
         (address (ash (sys.int::%pointer-field object) 4))
         (entry-point (sys.int::%array-like-ref-unsigned-byte-64 function sys.int::+function-entry-point+)))
    (setf
     ;; Entry point
     (sys.int::%array-like-ref-unsigned-byte-64 object sys.int::+function-entry-point+) (+ address 16)
     ;; The code.
     ;; mov :rbx (:rip 17)/pool[1]
     ;; jmp (:rip 3)/pool[0]
     (sys.int::%array-like-ref-unsigned-byte-32 object 2) #x111D8B48
     (sys.int::%array-like-ref-unsigned-byte-32 object 3) #xFF000000
     (sys.int::%array-like-ref-unsigned-byte-32 object 4) #x00000325
     (sys.int::%array-like-ref-unsigned-byte-32 object 5) #xCCCCCC00
     ;; entry-point and constant pool entries.
     (sys.int::%array-like-ref-unsigned-byte-64 object sys.int::+funcallable-instance-entry-point+) entry-point
     (sys.int::%array-like-ref-t object sys.int::+funcallable-instance-function+) function
     (sys.int::%array-like-ref-t object sys.int::+funcallable-instance-class+) class
     (sys.int::%array-like-ref-t object sys.int::+funcallable-instance-slots+) slots)
    object))
