(in-package :mezzanine.runtime)

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
        *allocator-lock* (mezzanine.supervisor:make-mutex "Allocator")))

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
      (mezzanine.supervisor:panic "Corrupt freelist."))))

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
       (mezzanine.supervisor:panic "No memory!!!"))
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
        (setf (sys.int::memref-unsigned-byte-64 freelist 0)
              (logior sys.int::*pinned-mark-bit*
                      (ash tag sys.int::+array-type-shift+)
                      (ash data sys.int::+array-length-shift+)))
        ;; Clear data.
        (dotimes (i (1- words))
          (setf (sys.int::memref-unsigned-byte-64 freelist (1+ i)) 0))
        ;; Return address.
        (return-from %allocate-from-pinned-area freelist)))))

(defun %allocate-object (tag data size area)
  (when sys.int::*gc-in-progress*
    (mezzanine.supervisor:panic "Allocating during GC!"))
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (ecase area
      ((nil)
       (tagbody
        OUTER-LOOP
          (mezzanine.supervisor:with-mutex (*allocator-lock*)
            (tagbody
             INNER-LOOP
               (return-from %allocate-object
                 (mezzanine.supervisor:with-pseudo-atomic
                   (when (> (+ sys.int::*general-area-bump* (* words 8)) sys.int::*general-area-limit*)
                     (go EXPAND-AREA))
                   ;; Enough size, allocate here.
                   (let ((addr (logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                                       sys.int::*general-area-bump*
                                       sys.int::*dynamic-mark-bit*)))
                     (incf sys.int::*general-area-bump* (* words 8))
                     ;; Write array header.
                     (setf (sys.int::memref-unsigned-byte-64 addr 0)
                           (logior (ash tag sys.int::+array-type-shift+)
                                   (ash data sys.int::+array-length-shift+)))
                     (sys.int::%%assemble-value addr sys.int::+tag-object+))))
             EXPAND-AREA
               ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
               ;; Cannot be done when pseudo-atomic.
               ;; Divide granularity by two because this is a semispace area. Need twice as much memory.
               (let ((expansion (logand (truncate *general-area-expansion-granularity* 2) (lognot #xFFF))))
                 (mezzanine.supervisor::debug-print-line "Expanding general area by " expansion)
                 (when (< sys.int::*memory-expansion-remaining* (* expansion 2))
                   ;; Not enough memory, abandon ship, do a gc and then attempt the allocation again.
                   (go DO-GC))
                 (decf sys.int::*memory-expansion-remaining* (* expansion 2))
                 ;; Do new & oldspace allocations seperately, this interacts better with the freelist.
                 (mezzanine.supervisor:with-mutex (mezzanine.supervisor::*vm-lock*)
                   ;; Allocate newspace.
                   (dotimes (i (truncate expansion #x1000))
                     (mezzanine.supervisor::allocate-new-block-for-virtual-address
                      (logior sys.int::*dynamic-mark-bit*
                              (ash sys.int::+address-tag-general+
                                   sys.int::+address-tag-shift+)
                              (+ sys.int::*general-area-limit*
                                 (* i #x1000)))
                      (logior sys.int::+block-map-present+
                              sys.int::+block-map-writable+
                              sys.int::+block-map-zero-fill+)))
                   ;; Allocate oldspace.
                   (dotimes (i (truncate expansion #x1000))
                     (mezzanine.supervisor::allocate-new-block-for-virtual-address
                      (logior (logxor sys.int::*dynamic-mark-bit*
                                      (ash 1 sys.int::+address-mark-bit+))
                              (ash sys.int::+address-tag-general+
                                   sys.int::+address-tag-shift+)
                              (+ sys.int::*general-area-limit*
                                 (* i #x1000)))
                      sys.int::+block-map-zero-fill+)))
                 (incf sys.int::*general-area-limit* expansion))
               (go INNER-LOOP)))
        DO-GC
          ;; Must occur outside the locks.
          (sys.int::gc)
          (go OUTER-LOOP)))
      (:pinned
       (mezzanine.supervisor:with-mutex (*allocator-lock*)
         (mezzanine.supervisor:with-pseudo-atomic
           (when *paranoid-allocation*
             (verify-freelist sys.int::*pinned-area-freelist* (* 2 1024 1024 1024) sys.int::*pinned-area-bump*))
           (sys.int::%%assemble-value
            (%allocate-from-pinned-area tag data words 'sys.int::*pinned-area-freelist*)
            sys.int::+tag-object+))))
      (:wired
       (mezzanine.supervisor:with-symbol-spinlock (*wired-allocator-lock*)
         (when *paranoid-allocation*
           (verify-freelist sys.int::*wired-area-freelist* (* 2 1024 1024) sys.int::*wired-area-bump*))
         (sys.int::%%assemble-value
          (%allocate-from-pinned-area tag data words 'sys.int::*wired-area-freelist*)
          sys.int::+tag-object+))))))

(defun sys.int::cons-in-area (car cdr &optional area)
  (when sys.int::*gc-in-progress*
    (mezzanine.supervisor:panic "Allocating during GC!"))
  (ecase area
    ((nil) (cons car cdr))
    (:pinned
     (mezzanine.supervisor:with-mutex (*allocator-lock*)
       (mezzanine.supervisor:with-pseudo-atomic
         (when *paranoid-allocation*
           (verify-freelist sys.int::*pinned-area-freelist* (* 2 1024 1024 1024) sys.int::*pinned-area-bump*))
         (let ((val (sys.int::%%assemble-value
                     (+ (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4 'sys.int::*pinned-area-freelist*) 16)
                     sys.int::+tag-cons+)))
           (setf (car val) car
                 (cdr val) cdr)
           val))))
    (:wired
     (mezzanine.supervisor:with-symbol-spinlock (*wired-allocator-lock*)
       (when *paranoid-allocation*
         (verify-freelist sys.int::*wired-area-freelist* (* 2 1024 1024) sys.int::*wired-area-bump*))
       (let ((val (sys.int::%%assemble-value
                   (+ (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4 'sys.int::*wired-area-freelist*) 16)
                   sys.int::+tag-cons+)))
         (setf (car val) car
               (cdr val) cdr)
         val)))))

(defun cons (car cdr)
  (when sys.int::*gc-in-progress*
    (mezzanine.supervisor:panic "Allocating during GC!"))
  (tagbody
   OUTER-LOOP
     (mezzanine.supervisor:with-mutex (*allocator-lock*)
       (tagbody
        INNER-LOOP
          (return-from cons
            (mezzanine.supervisor:with-pseudo-atomic
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
            (mezzanine.supervisor::debug-print-line "Expanding cons area by " expansion)
            (when (< sys.int::*memory-expansion-remaining* (* expansion 2))
              ;; Not enough memory, abandon ship, do a gc and then attempt the allocation again.
              (go DO-GC))
            (decf sys.int::*memory-expansion-remaining* (* expansion 2))
            ;; Do new & oldspace allocations seperately, this interacts better with the freelist.
            (mezzanine.supervisor:with-mutex (mezzanine.supervisor::*vm-lock*)
              ;; Allocate newspace.
              (dotimes (i (truncate expansion #x1000))
                (mezzanine.supervisor::allocate-new-block-for-virtual-address
                 (logior sys.int::*dynamic-mark-bit*
                         (ash sys.int::+address-tag-cons+
                              sys.int::+address-tag-shift+)
                         (+ sys.int::*cons-area-limit*
                            (* i #x1000)))
                 (logior sys.int::+block-map-present+
                         sys.int::+block-map-writable+
                         sys.int::+block-map-zero-fill+)))
              ;; Allocate oldspace.
              (dotimes (i (truncate expansion #x1000))
                (mezzanine.supervisor::allocate-new-block-for-virtual-address
                 (logior (logxor sys.int::*dynamic-mark-bit*
                                 (ash 1 sys.int::+address-mark-bit+))
                         (ash sys.int::+address-tag-cons+
                              sys.int::+address-tag-shift+)
                         (+ sys.int::*cons-area-limit*
                            (* i #x1000)))
                 sys.int::+block-map-zero-fill+)))
            (incf sys.int::*cons-area-limit* expansion))
          (go INNER-LOOP)))
   DO-GC
     ;; Must occur outside the locks.
     (sys.int::gc)
     (go OUTER-LOOP)))

(defun sys.int::make-simple-vector (size &optional area)
  (%allocate-object sys.int::+object-tag-array-t+ size size area))

(defun sys.int::%make-struct (size &optional area)
  (%allocate-object sys.int::+object-tag-structure-object+ size size area))

(defun sys.int::make-closure (function environment &optional area)
  "Allocate a closure object."
  (check-type function function)
  (let* ((closure (%allocate-object sys.int::+object-tag-closure+ #x2000100 5 area))
         (entry-point (sys.int::%array-like-ref-unsigned-byte-64 function 0)))
    (setf
     ;; Entry point
     (sys.int::%array-like-ref-unsigned-byte-64 closure 0) entry-point
     ;; Initialize constant pool
     (sys.int::%array-like-ref-t closure 1) function
     (sys.int::%array-like-ref-t closure 2) environment)
    closure))

(defun make-symbol (name)
  (check-type name string)
  ;; FIXME: Copy name into the wired area and unicode normalize it.
  (let* ((symbol (%allocate-object sys.int::+object-tag-symbol+ 0 5 :wired)))
    ;; symbol-name.
    (setf (sys.int::%array-like-ref-t symbol 0) name)
    (makunbound symbol)
    (setf (sys.int::symbol-fref symbol) nil
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
      (setf (sys.int::%array-like-ref-unsigned-byte-64 object 0) (+ address 16))
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
         (entry-point (sys.int::%array-like-ref-unsigned-byte-64 function 0)))
    (setf
     ;; Entry point
     (sys.int::%array-like-ref-unsigned-byte-64 object 0) (+ address 16)
     ;; The code.
     ;; mov :rbx (:rip 17)/pool[1]
     ;; jmp (:rip 3)/pool[0]
     (sys.int::%array-like-ref-unsigned-byte-32 object 2) #x111D8B48
     (sys.int::%array-like-ref-unsigned-byte-32 object 3) #xFF000000
     (sys.int::%array-like-ref-unsigned-byte-32 object 4) #x00000325
     (sys.int::%array-like-ref-unsigned-byte-32 object 5) #xCCCCCC00
     ;; entry-point and constant pool entries.
     (sys.int::%array-like-ref-unsigned-byte-64 object 3) entry-point
     (sys.int::%array-like-ref-t object 4) function
     (sys.int::%array-like-ref-t object 5) class
     (sys.int::%array-like-ref-t object 6) slots)
    object))
