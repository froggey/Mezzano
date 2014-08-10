(in-package :mezzanine.supervisor)

;;; FIXME: Should not be here.
;;; >>>>>>

(defun sys.int::current-thread ()
  (current-thread))

(defun integerp (object)
  (sys.int::fixnump object))

(defun sys.int::%coerce-to-callable (object)
  (etypecase object
    (function object)
    (symbol
     (sys.int::%array-like-ref-t
      (sys.int::%array-like-ref-t object sys.c::+symbol-function+)
      sys.int::+fref-function+))))

;; Hardcoded string accessor, the support stuff for arrays doesn't function at this point.
(defun char (string index)
  (assert (sys.int::character-array-p string) (string))
  (let ((data (sys.int::%array-like-ref-t string 0)))
    (assert (and (<= 0 index)
                 (< index (sys.int::%object-header-data data)))
            (string index))
    (code-char
     (case (sys.int::%object-tag data)
       (#.sys.int::+object-tag-array-unsigned-byte-8+
        (sys.int::%array-like-ref-unsigned-byte-8 data index))
       (#.sys.int::+object-tag-array-unsigned-byte-16+
        (sys.int::%array-like-ref-unsigned-byte-16 data index))
       (#.sys.int::+object-tag-array-unsigned-byte-32+
        (sys.int::%array-like-ref-unsigned-byte-32 data index))
       (t 0)))))

(defun length (sequence)
  (if (sys.int::character-array-p sequence)
      (sys.int::%array-like-ref-t sequence 3)
      nil))

(defun code-char (code)
  (sys.int::%%assemble-value (ash code 4) sys.int::+tag-character+))

(defun char-code (character)
  (logand (ash (sys.int::lisp-object-address character) -4) #x1FFFFF))

(declaim (special sys.int::*newspace* sys.int::*newspace-offset*))

(defvar *allocator-lock*)

;; TODO?
(defmacro with-gc-deferred (&body body)
  `(progn
     ,@body))

(defun %allocate-object (tag data size area)
  (declare (ignore area))
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (with-spinlock (*allocator-lock*)
      ;; Assume we have enough memory to do the allocation...
      ;; And that the memory is already zero initialized.
      (let ((addr (+ sys.int::*newspace* (ash sys.int::*newspace-offset* 3))))
        (incf sys.int::*newspace-offset* words)
        ;; Write array header.
        (setf (sys.int::memref-unsigned-byte-64 addr 0)
              (logior (ash tag sys.int::+array-type-shift+)
                      (ash data sys.int::+array-length-shift+)))
        (sys.int::%%assemble-value addr sys.int::+tag-object+)))))

(defun sys.int::make-simple-vector (size &optional area)
  (%allocate-object sys.int::+object-tag-array-t+ size size area))

(defun sys.int::%make-struct (size &optional area)
  (%allocate-object sys.int::+object-tag-structure-object+ size size area))

(defun sys.int::cons-in-area (car cdr &optional area)
  (declare (ignore area))
  (with-spinlock (*allocator-lock*)
    ;; Assume we have enough memory to do the allocation...
    (let ((addr (+ sys.int::*newspace* (ash sys.int::*newspace-offset* 3))))
      (incf sys.int::*newspace-offset* 2)
      ;; Set car/cdr.
      (setf (sys.int::memref-t addr 0) car
            (sys.int::memref-t addr 1) cdr)
      (sys.int::%%assemble-value addr sys.int::+tag-cons+))))

(defun stack-base (stack)
  (car stack))

(defun stack-size (stack)
  (cdr stack))

(defun %allocate-stack (style size)
  ;; Page align stacks.
  (incf size #xFFF)
  (setf size (logand size (lognot #xFFF)))
  (let ((addr (with-spinlock (*allocator-lock*)
                (prog1 sys.int::*stack-bump-pointer*
                  (incf sys.int::*stack-bump-pointer* size)))))
    (sys.int::cons-in-area addr size :wired)))

;; TODO.
(defun sleep (seconds)
  nil)

(sys.int::define-lap-function sys.int::%%coerce-fixnum-to-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:cvtsi2ss64 :xmm0 :rax)
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%float-+ ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Add.
  (sys.lap-x86:addss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%float-- ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Add.
  (sys.lap-x86:subss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%float-< ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Compare.
  (sys.lap-x86:ucomiss :xmm0 :xmm1)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:mov64 :r9 t)
  (sys.lap-x86:cmov64b :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(defun sys.int::generic-+ (x y)
  (cond ((or (floatp x)
             (floatp y))
         (when (sys.int::fixnump x)
           (setf x (sys.int::%%coerce-fixnum-to-float x)))
         (when (sys.int::fixnump y)
           (setf y (sys.int::%%coerce-fixnum-to-float y)))
         (sys.int::%%float-+ x y))
        (t (error "Unsupported argument combination."))))

(defun sys.int::generic-- (x y)
  (cond ((or (floatp x)
             (floatp y))
         (when (sys.int::fixnump x)
           (setf x (sys.int::%%coerce-fixnum-to-float x)))
         (when (sys.int::fixnump y)
           (setf y (sys.int::%%coerce-fixnum-to-float y)))
         (sys.int::%%float-- x y))
        (t (error "Unsupported argument combination."))))

(defun sys.int::generic-< (x y)
  (cond ((or (floatp x)
             (floatp y))
         (when (sys.int::fixnump x)
           (setf x (sys.int::%%coerce-fixnum-to-float x)))
         (when (sys.int::fixnump y)
           (setf y (sys.int::%%coerce-fixnum-to-float y)))
         (sys.int::%%float-< x y))
        (t (error "Unsupported argument combination."))))

(defun sys.int::generic-> (x y)
  (sys.int::generic-< y x))

(defun sys.int::generic-<= (x y)
  (not (sys.int::generic-< y x)))

(defun sys.int::generic->= (x y)
  (not (sys.int::generic-< x y)))

(defun sys.int::raise-undefined-function (fref)
  (debug-write-string "Undefined function ")
  (let ((name (sys.int::%array-like-ref-t fref sys.int::+fref-name+)))
    (cond ((consp name)
           (debug-write-string "(")
           (debug-write-string (symbol-name (car name)))
           (debug-write-string " ")
           (debug-write-string (symbol-name (car (cdr name))))
           (debug-write-line ")"))
          (t (debug-write-line (symbol-name name)))))
  (sys.int::%sti)
  (loop))

(defun sys.int::raise-unbound-error (symbol)
  (debug-write-string "Unbound symbol ")
  (debug-write-line (symbol-name symbol))
  (sys.int::%sti)
  (loop))

(defun endp (list)
  (null list))

(defvar sys.int::*active-catch-handlers*)
(defun sys.int::%catch (tag fn)
  ;; Catch is used in low levelish code, so must avoid allocation.
  (let ((vec (sys.c::make-dx-simple-vector 3)))
    (setf (svref vec 0) sys.int::*active-catch-handlers*
          (svref vec 1) tag
          (svref vec 2) (flet ((exit-fn (values)
                                 (return-from sys.int::%catch (values-list values))))
                          (declare (dynamic-extent (function exit-fn)))
                          #'exit-fn))
    (let ((sys.int::*active-catch-handlers* vec))
      (funcall fn))))

(defun sys.int::%throw (tag values)
  (do ((current sys.int::*active-catch-handlers* (svref current 0)))
      ((not current)
       (error 'bad-catch-tag-error :tag tag))
    (when (eq (svref current 1) tag)
      (funcall (svref current 2) values))))

(defvar *tls-lock*)
(defvar sys.int::*next-symbol-tls-slot*)
(defconstant +maximum-tls-slot+ (1+ +thread-tls-slots-end+))
(defun sys.int::%allocate-tls-slot (symbol)
  (with-spinlock (*tls-lock*)
    ;; Make sure that another thread didn't allocate a slot while we were waiting for the lock.
    (cond ((zerop (ldb (byte 16 10) (sys.int::%array-like-ref-unsigned-byte-64 symbol -1)))
           (when (>= sys.int::*next-symbol-tls-slot* +maximum-tls-slot+)
             (error "Critial error! TLS slots exhausted!"))
           (let ((slot sys.int::*next-symbol-tls-slot*))
             (incf sys.int::*next-symbol-tls-slot*)
             ;; Twiddle TLS bits directly in the symbol header.
             (setf (ldb (byte 16 10) (sys.int::%array-like-ref-unsigned-byte-64 symbol -1)) slot)
             slot))
          (t (ldb (byte 16 10) (sys.int::%array-like-ref-unsigned-byte-64 symbol -1))))))

;;; <<<<<<

(defun sys.int::bootloader-entry-point ()
  (initialize-initial-thread)
  ;; FIXME: Should be done by cold generator
  (setf *allocator-lock* :unlocked
        *tls-lock* :unlocked
        sys.int::*active-catch-handlers* 'nil)
  (initialize-interrupts)
  (initialize-i8259)
  (sys.int::%sti)
  (initialize-debug-serial #x3F8 4)
  (debug-write-line "Hello, Debug World!")
  (initialize-ata)
  (initialize-threads)
  (make-thread
   (lambda ()
     (debug-write-line "Reading page...")
     (ata-read (car *ata-devices*) 0 8 (logior (ash -1 48) #x800000001000))
     (debug-write-line "Complete!")
     (debug-write-line "Writing page...")
     (ata-write (car *ata-devices*) 0 8 (logior (ash -1 48) #x800000000000))
     (debug-write-line "Complete!"))
   :name "Main thread")
  (finish-initial-thread))
