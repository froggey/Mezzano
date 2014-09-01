(in-package :mezzanine.runtime)

;; Note: The cold generator will create a few of these structs to
;; describe the initial areas. If they layout changes, then it
;; must be updated.
(defstruct (area (:area :wired))
  base
  size
  ;; Actually a freelist pointer when allocating from a pinned area.
  bump
  type
  extent
  next)

;;; Lists of the various areas for default allocating from.
(defvar *wired-area-chain*)
(defvar *wired-2g-area-chain*)
(defvar *pinned-area-chain*)
(defvar *dynamic-area-chain*)
(defvar *dynamic-area-full-chain*)
(defvar *dynamic-cons-area-chain*)
(defvar *dynamic-cons-area-full-chain*)

(defvar *allocator-lock*)

(defun freelist-entry-next (entry)
  (sys.int::%array-like-ref-t entry 0))

(defun (setf freelist-entry-next) (value entry)
  (setf (sys.int::%array-like-ref-t entry 0) value))

(defun freelist-entry-size (entry)
  (sys.int::%object-header-data entry))

(defun first-run-initialize-allocator ()
  (setf *allocator-lock* :unlocked
        *wired-area-chain* nil
        *wired-2g-area-chain* nil
        *pinned-area-chain* nil
        *dynamic-area-chain* nil
        *dynamic-area-full-chain* nil
        *dynamic-cons-area-chain* nil
        *dynamic-cons-area-full-chain* nil)
  (dotimes (i (sys.int::%object-header-data sys.int::*initial-areas*))
    (let ((area (svref sys.int::*initial-areas* i)))
      (ecase (area-type area)
        (:wired
         (setf (area-next area) *wired-area-chain*
               *wired-area-chain* area))
        (:wired-2g
         (setf (area-next area) *wired-2g-area-chain*
               *wired-2g-area-chain* area))
        (:pinned
         (setf (area-next area) *pinned-area-chain*
               *pinned-area-chain* area))
        (:dynamic
         (setf (area-next area) *dynamic-area-chain*
               *dynamic-area-chain* area))
        (:dynamic-cons
         (setf (area-next area) *dynamic-cons-area-chain*
               *dynamic-cons-area-chain* area))
        (:stack)))))

;; Simple first-fit freelist allocator for pinned areas.
(defun %allocate-from-pinned-area (tag data words chain-symbol wiredp 2g)
  (do ((area (symbol-value chain-symbol) (area-next area)))
      ((null area)
       ;; No memory. Run a GC cycle, try the allocation again, then allocate a new area.
       (error "No memory!!!"))
    ;; Traverse the area's freelist.
    (do ((freelist (area-bump area) (freelist-entry-next freelist))
         (prev nil freelist))
        ((null freelist))
      (let ((size (freelist-entry-size freelist))
            (addr (logand (sys.int::lisp-object-address freelist) -16)))
        (when (>= size words)
          ;; This freelist entry is large enough, use it.
          (let ((next (cond ((eql size words)
                             ;; Entry is exactly the right size.
                             (freelist-entry-next freelist))
                            (t
                             ;; Entry is too large, split it.
                             (let ((next (+ addr (* words 8))))
                               (setf (sys.int::memref-unsigned-byte-64 next 0) (logior (ash sys.int::+object-tag-freelist-entry+ sys.int::+array-type-shift+)
                                                                                       (ash (- size words) sys.int::+array-length-shift+))
                                     (sys.int::memref-t next 1) (freelist-entry-next freelist))
                               (sys.int::%%assemble-value next sys.int::+tag-object+))))))
            ;; Update the prev's next pointer.
            (cond (prev
                   (setf (freelist-entry-next freelist) next))
                  (t
                   (setf (area-bump area) next))))
          ;; Write object header.
          (setf (sys.int::memref-unsigned-byte-64 addr 0)
                (logior (ash tag sys.int::+array-type-shift+)
                        (ash data sys.int::+array-length-shift+)))
          ;; Clear data.
          (dotimes (i (1- words))
            (setf (sys.int::memref-unsigned-byte-64 addr (1+ i)) 0))
          ;; Return address.
          (return-from %allocate-from-pinned-area addr))))))

(defun %allocate-from-dynamic-area (tag data words)
  ;; FIXME: Large objects.
  (do ((area *dynamic-area-chain* *dynamic-area-chain*))
      ((null area)
       ;; No memory. Run a GC cycle, try the allocation again, then allocate a new area.
       (error "No memory!!!"))
    (let* ((bump (area-bump area))
           (next-bump (+ bump (* words 8))))
      (when (<= next-bump (area-size area))
        ;; Enough size, allocate here.
        (let ((addr (+ (area-base area) bump)))
          (setf (area-bump area) next-bump)
          ;; Write array header.
          (setf (sys.int::memref-unsigned-byte-64 addr 0)
                (logior (ash tag sys.int::+array-type-shift+)
                        (ash data sys.int::+array-length-shift+)))
          (return addr)))
      ;; This area is full. Put it on the full chain.
      (setf *dynamic-area-chain* (area-next area)
            (area-next area) *dynamic-area-full-chain*
            *dynamic-area-full-chain* area))))

(defun %allocate-object (tag data size area)
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (ecase area
      ((nil)
       (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
         (sys.int::%%assemble-value
          (%allocate-from-dynamic-area tag data words)
          sys.int::+tag-object+)))
      (:pinned
       (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
         (sys.int::%%assemble-value
          (%allocate-from-pinned-area tag data words '*pinned-area-chain* nil t)
          sys.int::+tag-object+)))
      (:wired
       (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
         (sys.int::%%assemble-value
          (%allocate-from-pinned-area tag data words '*wired-area-chain* t nil)
          sys.int::+tag-object+)))
      (:wired-2g
       (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
         (sys.int::%%assemble-value
          (%allocate-from-pinned-area tag data words '*wired-2g-area-chain* t t)
          sys.int::+tag-object+))))))

(defun sys.int::cons-in-area (car cdr &optional area)
  (ecase area
    ((nil) (cons car cdr))
    (:wired
     (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
       (let ((val (sys.int::%%assemble-value
                   (+ (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4 '*wired-area-chain* t nil) 16)
                   sys.int::+tag-cons+)))
         (setf (car val) car
               (cdr val) cdr)
         val)))
    (:pinned
     (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
       (let ((val (sys.int::%%assemble-value
                   (+ (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4 '*pinned-area-chain* nil nil) 16)
                   sys.int::+tag-cons+)))
         (setf (car val) car
               (cdr val) cdr)
         val)))
    (:wired-2g
     (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
       (let ((val (sys.int::%%assemble-value
                   (+ (%allocate-from-pinned-area sys.int::+object-tag-cons+ 0 4 '*wired-area-chain* t t) 16)
                   sys.int::+tag-cons+)))
         (setf (car val) car
               (cdr val) cdr)
         val)))))

(defun cons (car cdr)
  (mezzanine.supervisor:with-symbol-spinlock (*allocator-lock*)
    (do ((area *dynamic-cons-area-chain* *dynamic-cons-area-chain*))
        ((null area)
         ;; No memory. Run a GC cycle, try the allocation again, then allocate a new area.
         (error "No memory!!!"))
      (let* ((bump (area-bump area))
             (next-bump (+ bump 16)))
        (when (<= next-bump (area-size area))
          ;; Enough size, allocate here.
          (let* ((addr (+ (area-base area) bump))
                 (val (sys.int::%%assemble-value addr sys.int::+tag-cons+)))
            (setf (area-bump area) next-bump)
            (setf (car val) car
                  (cdr val) cdr)
            (return val))))
      ;; This area is full. Put it on the full chain.
      (setf *dynamic-cons-area-chain* (area-next area)
            (area-next area) *dynamic-cons-area-full-chain*
            *dynamic-cons-area-full-chain* area))))

(defun sys.int::make-simple-vector (size &optional area)
  (%allocate-object sys.int::+object-tag-array-t+ size size area))

(defun sys.int::%make-struct (size &optional area)
  (%allocate-object sys.int::+object-tag-structure-object+ size size area))

(defun sys.int::make-closure (function environment &optional area)
  "Allocate a closure object."
  (check-type function function)
  (mezzanine.supervisor:with-gc-deferred
    (let* ((closure (%allocate-object sys.int::+object-tag-closure+ #x2000100 5 area))
           (entry-point (sys.int::%array-like-ref-unsigned-byte-64 function 0)))
      (setf
       ;; Entry point
       (sys.int::%array-like-ref-unsigned-byte-64 closure 0) entry-point
       ;; Initialize constant pool
       (sys.int::%array-like-ref-t closure 1) function
       (sys.int::%array-like-ref-t closure 2) environment)
      closure)))

(defun make-symbol (name)
  (check-type name string)
  ;; FIXME: Copy name into the wired area and unicode normalize it.
  (mezzanine.supervisor:with-gc-deferred
    (let* ((symbol (%allocate-object sys.int::+object-tag-symbol+ 0 5 :wired)))
      ;; symbol-name.
      (setf (sys.int::%array-like-ref-t symbol 0) name)
      (makunbound symbol)
      (setf (sys.int::symbol-fref symbol) nil
            (symbol-plist symbol) nil
            (symbol-package symbol) nil)
      symbol)))

(defun sys.int::%allocate-array-like (tag word-count length &optional area)
  (%allocate-object tag length word-count area))

(define-lap-function sys.int::%%make-bignum-128-rdx-rax ()
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

(define-lap-function sys.int::%%make-bignum-64-rax ()
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
