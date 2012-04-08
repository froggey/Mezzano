(in-package #:sys.int)

(declaim (special *oldspace* *newspace* *newspace-offset* *semispace-size*
                  *oldspace-paging-bits* *newspace-paging-bits*))
(declaim (special *static-bump-pointer* *static-mark-bit*))
(declaim (special *verbose-gc*))
(setf *verbose-gc* nil)

(defvar *gc-in-progress* nil)

;;; FIXME: Should use unwind-protect but that conses!!!
;;; TODO: Require that this can never nest (ie interrupts are on "all" the time).
(defmacro with-interrupts-disabled (options &body code)
  `(let ((istate (%interrupt-state)))
     (%cli)
     (prog1 (progn ,@code) (when istate (%sti)))))

;;; FIXME: Don't use with-interrupts-disabled.
;;; Suppress preemption (SBCL pseudo-atomic-like operation).

(defun room (&optional (verbosity :default))
  (fresh-line)
  (format t "Dynamic space: ~:D/~:D words allocated (~D%).~%"
          *newspace-offset* *semispace-size*
          (truncate (* *newspace-offset* 100) *semispace-size*))
  ;; FIXME: The static area is only the same size as a semispace through coincience.
  (format t "Static space: ~:D/~:D words allocated (~D%).~%"
          (ceiling (- *static-bump-pointer* #x200000) 8) *semispace-size*
          (truncate (* (ceiling (- *static-bump-pointer* #x200000) 8) 100) *semispace-size*))
  (values))

(defun gc ()
  "Run a garbage-collection cycle."
  (with-interrupts-disabled ()
    (%gc)))

(declaim (inline oldspace-pointer-p))
(defun oldspace-pointer-p (address)
  (<= *oldspace*
      address
      (+ 1 *oldspace* (ash *semispace-size* 3))))

(declaim (inline newspace-pointer-p))
(defun newspace-pointer-p (address)
  (<= *newspace*
      address
      (+ 1 *newspace* (ash *semispace-size* 3))))

(declaim (inline static-pointer-p))
(defun static-pointer-p (address)
  (< address #x80000000))

(defun scan-error (object transport)
  (declare (ignore object transport))
  (emergency-halt "unscannable object"))

(defun scan-generic (object transportp size)
  (let* ((old-address (ash (%pointer-field object) 4))
         (new-address old-address)
         (first-value (memref-t old-address 0)))
    ;; Check for a forwarding pointer.
    (when (eql (%tag-field first-value) +tag-gc-forward+)
      (return-from scan-generic
        (%%assemble-value (ash (%pointer-field first-value) 4) (%tag-field object))))
    (when transportp
      ;; Leave a forwarding pointer.
      (setf new-address (+ *newspace* (ash *newspace-offset* 3)))
      ;; Copy fields without updating them.
      ;; This prevents SCAN-GENERIC from breaking when it's copying a symbol
      ;; that it uses.
      (%fast-copy new-address old-address (ash size 3))
      (setf (memref-t old-address 0) (%%assemble-value new-address +tag-gc-forward+))
      (incf *newspace-offset* size))
    (setf (memref-t new-address 0) (gc-object first-value))
    (dotimes (i (1- size))
      (setf (memref-t new-address (1+ i))
            (gc-object (memref-t old-address (1+ i)))))
    (%%assemble-value new-address (%tag-field object))))

(defun scan-cons (object transportp)
  (scan-generic object transportp 2))

(defun scan-symbol (object transportp)
  (scan-generic object transportp 6))

(defun scan-array-header (object transportp)
  (scan-generic object transportp 4))

(defun scan-std-instance (object transportp)
  (scan-generic object transportp 2))

(defun scan-array-t (object transportp)
  (let* ((old-address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 old-address 0))
         (length (ldb (byte 56 8) header))
         (word-count (1+ length))
         (new-address old-address))
    (when (hash-table-p object)
      (setf (hash-table-rehash-required object) 't))
    (when (oddp word-count) (incf word-count))
    (when transportp
      (setf new-address (+ *newspace* (ash *newspace-offset* 3)))
      (incf *newspace-offset* word-count)
      ;; Set the GC flag and leave a forwarding pointer.
      (setf (memref-unsigned-byte-64 old-address 0) (logior new-address 1)))
    (setf (memref-unsigned-byte-64 new-address 0) header)
    (dotimes (i length)
      (setf (memref-t new-address (1+ i))
            (gc-object (memref-t old-address (1+ i)))))
    (%%assemble-value new-address +tag-array-like+)))

(defun scan-stack-group (object transportp)
  (when transportp
    (emergency-halt "Attempting to transport a stack-group!"))
  (let ((address (ash (%pointer-field object) 4)))
    (dotimes (i 511)
      (let ((x (memref-t address (1+ i))))
        (when (not (eql (%tag-field x) +tag-unbound-value+))
          (setf (memref-t address (1+ i)) (gc-object x)))))
    ;; Scan the data/binding stacks only when the sg is not active.
    (when (not (eql (logand (memref-t address 2) +stack-group-state-mask+) +stack-group-active+))
      (emergency-halt "todo: scan-stack-group for inactive stack-groups"))
    object))

(defun scan-numeric-array (object transportp width)
  (when (not transportp)
    ;; Numeric arrays have nothing to scan so just return.
    (return-from scan-numeric-array object))
  (let* ((old-address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 old-address 0))
         (length (ldb (byte 56 8) header))
         (word-count (1+ (ceiling (* length width) 64)))
         (new-address (+ *newspace* (ash *newspace-offset* 3))))
    (when (oddp word-count) (incf word-count))
    (incf *newspace-offset* word-count)
    ;; Set the GC flag and leave a forwarding pointer.
    (setf (memref-unsigned-byte-64 old-address 0) (logior new-address 1))
    ;; Just copy data over.
    (setf (memref-unsigned-byte-64 new-address 0) header)
    (%fast-copy (+ new-address 8) (+ old-address 8) (ash (1- word-count) 3))
    (%%assemble-value new-address +tag-array-like+)))

(defun scan-array-1 (object transportp)
  (scan-numeric-array object transportp 1))

(defun scan-array-2 (object transportp)
  (scan-numeric-array object transportp 2))

(defun scan-array-4 (object transportp)
  (scan-numeric-array object transportp 4))

(defun scan-array-8 (object transportp)
  (scan-numeric-array object transportp 8))

(defun scan-array-16 (object transportp)
  (scan-numeric-array object transportp 16))

(defun scan-array-32 (object transportp)
  (scan-numeric-array object transportp 32))

(defun scan-array-64 (object transportp)
  (scan-numeric-array object transportp 64))

(defun scan-array-128 (object transportp)
  (scan-numeric-array object transportp 128))

(defun scan-array-256 (object transportp)
  (scan-numeric-array object transportp 256))

(defun scan-array-like (object transportp)
  (let* ((address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 address 0)))
    ;; Check the GC bit.
    (when (logtest header 1)
      (return-from scan-array-like
        (%%assemble-value (logand header -2)
                          +tag-array-like+)))
    ;; Dispatch again based on the type.
    (case (ldb (byte 5 1) header)
      (0  (scan-array-t object transportp)) ; simple-vector
      (1  (scan-array-8 object transportp)) ; base-char
      (2  (scan-array-32 object transportp)) ; character
      (3  (scan-array-1 object transportp)) ; bit
      (4  (scan-array-2 object transportp)) ; unsigned-byte 2
      (5  (scan-array-4 object transportp)) ; unsigned-byte 4
      (6  (scan-array-8 object transportp)) ; unsigned-byte 8
      (7  (scan-array-16 object transportp)) ; unsigned-byte 16
      (8  (scan-array-32 object transportp)) ; unsigned-byte 32
      (9  (scan-array-64 object transportp)) ; unsigned-byte 64
      (10 (scan-array-1 object transportp)) ; signed-byte 1
      (11 (scan-array-2 object transportp)) ; signed-byte 2
      (12 (scan-array-4 object transportp)) ; signed-byte 4
      (13 (scan-array-8 object transportp)) ; signed-byte 8
      (14 (scan-array-16 object transportp)) ; signed-byte 16
      (15 (scan-array-32 object transportp)) ; signed-byte 32
      (16 (scan-array-64 object transportp)) ; signed-byte 64
      (17 (scan-array-32 object transportp)) ; single-float
      (18 (scan-array-64 object transportp)) ; double-float
      (19 (scan-array-128 object transportp)) ; long-float
      (20 (scan-array-128 object transportp)) ; xmm-vector
      (21 (scan-array-64 object transportp)) ; complex single-float
      (22 (scan-array-128 object transportp)) ; complex double-float
      (23 (scan-array-256 object transportp)) ; complex long-float
      (24 (scan-error object transportp)) ; unused (24)
      (25 (scan-array-64 object transportp)) ; bignum
      (26 (scan-error object transportp)) ; unused (26)
      (27 (scan-error object transportp)) ; unused (27)
      (28 (scan-error object transportp)) ; unused (28)
      (29 (scan-error object transportp)) ; unused (29)
      (30 (scan-stack-group object transportp))
      (31 (scan-array-t object transportp)) ; struct
      (t (scan-error object transportp)))))

(defun scan-function (object transportp)
  (when transportp
    (emergency-halt "Attempting to transport a function!"))
  ;; Scan the constant pool.
  (let* ((address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 address 0))
         (mc-size (ash (ldb (byte 16 16) header) 1))
         (pool-size (ldb (byte 16 32) header)))
    (dotimes (i pool-size)
      (setf (memref-t address (+ mc-size i))
            (gc-object (memref-t address (+ mc-size i)))))
    object))

(defmacro with-gc-trace ((object prefix) &body body)
  (let ((object-sym (gensym))
        (result-sym (gensym)))
    `(let ((,object-sym ,object))
       (when *verbose-gc*
         (gc-trace ,object-sym #\> ,prefix))
       (let ((,result-sym (progn ,@body)))
         (when *verbose-gc*
           (gc-trace ,result-sym #\~ ,prefix)
           (gc-trace ,object-sym #\< ,prefix))
         ,result-sym))))

#+nil(defmacro with-gc-trace ((object prefix) &body body)
  (declare (ignore object prefix))
  `(progn ,@body))

(defun scan-object (object transportp)
  (case (%tag-field object)
    (0  (scan-error object transportp)) ; even-fixnum
    (1  (scan-cons object transportp))
    (2  (scan-symbol object transportp))
    (3  (scan-array-header object transportp))
    (4  (scan-std-instance object transportp))
    (5  (scan-error object transportp)) ; unused
    (6  (scan-error object transportp)) ; unused
    (7  (scan-array-like object transportp))
    (8  (scan-error object transportp)) ; odd-fixnum
    (9  (scan-error object transportp)) ; unused
    (10 (scan-error object transportp)) ; character
    (11 (scan-error object transportp)) ; unused
    (12 (scan-function object transportp))
    (13 (scan-error object transportp)) ; unused
    (14 (scan-error object transportp)) ; unbound-value
    (15 (scan-error object transportp))))

(defun scan-and-transport (object)
  (with-gc-trace (object #\d)
    (scan-object object t)))

(defun mark-static-object (object)
  (let ((address (ash (%pointer-field object) 4)))
    (when (eql (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
      ;; Object has already been marked.
      (return-from mark-static-object))
    (setf (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
    (with-gc-trace (object #\s)
      (scan-object object nil))))

(defun gc-object (object)
  (when (or (fixnump object)
            (eql (%tag-field object) +tag-unbound-value+))
    ;; Attempting to shift the pointer-field of negative fixnums
    ;; about will cause a fixnum overflow.
    ;; TLS-unbound tags also have the same issue.
    (return-from gc-object object))
  (let ((address (ash (%pointer-field object) 4)))
    (cond ((characterp object)
           ;; Do nothing with immediate objects.
           object)
          ((oldspace-pointer-p address)
           ;; Object is in oldspace, transport to newspace.
           (scan-and-transport object))
          ((newspace-pointer-p address)
           ;; Object is already in newspace.
           ;; Can this ever happen??
           object)
          ((static-pointer-p address)
           ;; Object is in the static area, mark and scan.
           (mark-static-object object)
           object)
          (t
           ;; Assume the pointer is on the stack.
           ;; TODO: Track scanned stack objects. Allocate a cons with dynamic-extent
           ;; and push on some symbol.
           (scan-object object nil)))))

;;; Arguments and MV return are to force the data registers on to the stack.
;;; This does not work for RBX or R13, but RBX is smashed by the function
;;; return and R13 will be holding a newspace pointer by the time this
;;; function is called.
(defun transport-registers-and-stack (a1 a2 a3 a4 a5)
  (let* ((sg-pointer (ash (%pointer-field (%%current-stack-group)) 4))
	 (ds-base (memref-unsigned-byte-64 sg-pointer 7))
	 (ds-size (memref-unsigned-byte-64 sg-pointer 8))
	 (bs-base (memref-unsigned-byte-64 sg-pointer 9))
	 (bs-size (memref-unsigned-byte-64 sg-pointer 10))
         (binding-stack-pointer (memref-unsigned-byte-64 sg-pointer 1))
         (data-stack-pointer (ash (%%get-data-stack-pointer) 3)))
    (mumble "Scanning data stack")
    (dotimes (i (ash (- (+ ds-base ds-size) data-stack-pointer) -3))
      (setf (memref-t data-stack-pointer i)
            (gc-object (memref-t data-stack-pointer i))))
    (mumble "Scanning binding stack")
    (dotimes (i (ash (- (+ bs-base bs-size) binding-stack-pointer) -3))
      (setf (memref-t binding-stack-pointer i)
            (gc-object (memref-t binding-stack-pointer i)))))
  (values a1 a2 a3 a4 a5))

(defun %gc ()
  (when *gc-in-progress*
    (error "Nested GC?!"))
  (let ((*gc-in-progress* t)
        (old-offset *newspace-offset*))
    (mumble "GC in progress...")
    ;; Allow access to the soon-to-be-newspace.
    (setf (ldb (byte 2 0) (memref-unsigned-byte-32 *oldspace-paging-bits* 0)) 3)
    ;; Flip.
    (psetf *oldspace* *newspace*
           *newspace* *oldspace*
           *oldspace-paging-bits* *newspace-paging-bits*
           *newspace-paging-bits* *oldspace-paging-bits*
           *newspace-offset* 0
           *static-mark-bit* (logxor *static-mark-bit* 1))
    ;; Transport the major root, NIL.
    (gc-object 'nil)
    ;; Transport registers, the data stack and the binding stack.
    (transport-registers-and-stack 1 2 3 4 5)
    ;; Make oldspace inaccessible.
    (setf (ldb (byte 2 0) (memref-unsigned-byte-32 *oldspace-paging-bits* 0)) 0)
    ;; Flush TLB.
    (setf (%cr3) (%cr3))
    (mumble "complete")
    t))

;;; This is the fundamental dynamic allocation function.
;;; It ensures there is enough space on the dynamic heap to
;;; allocate WORDS words of memory and returns a fixnum address
;;; to the allocated memory. It violates GC invariants by twiddling
;;; *newspace-offset* without clearing memory, so must be called
;;; with the GC defered (currently by using WITH-INTERRUPTS-DISABLED)
;;; and the caller must clear the returned memory before reenabling the GC.
;;; Additionally, the number of words to allocate must be even to ensure
;;; correct alignment.
(defun %raw-allocate (words)
  (when (> (+ *newspace-offset* words) *semispace-size*)
    (%gc)
    (when (> (+ *newspace-offset* words) *semispace-size*)
      ;; Oh dear. No memory.
      (emergency-halt "Out of memory.")))
  (prog1 (+ *newspace* (ash *newspace-offset* 3))
    (incf *newspace-offset* words)))

(defun cons (car cdr)
  (with-interrupts-disabled ()
    (let ((cons (%%assemble-value (%raw-allocate 2) +tag-cons+)))
      (setf (car cons) car
            (cdr cons) cdr)
      cons)))

(defun allocate-std-instance (class slots)
  (with-interrupts-disabled ()
    (let ((value (%%assemble-value (%raw-allocate 2) +tag-std-instance+)))
      (setf (std-instance-class value) class
            (std-instance-slots value) slots)
      value)))

(defun %allocate-array-like (tag word-count length)
  "Allocate a array-like object. All storage is initialized to zero.
WORD-COUNT must be the number of words used to store the data, not including
the header word. LENGTH is the number of elements in the array."
  (with-interrupts-disabled ()
    ;; Align and account for the header word.
    (if (oddp word-count)
        (incf word-count 1)
        (incf word-count 2))
    (let ((address (%raw-allocate word-count)))
      ;; Clear memory.
      (dotimes (i word-count)
        (setf (memref-unsigned-byte-64 address i) 0))
      ;; Set header word.
      (setf (memref-unsigned-byte-64 address 0)
            (logior (ash length 8) (ash tag 1)))
      ;; Return value.
      (%%assemble-value address +tag-array-like+))))

(defun %make-struct (length)
  (%allocate-array-like 31 length length))

(defun make-closure (function environment)
  "Allocate a closure object."
  (check-type function function)
  (with-interrupts-disabled ()
    (let ((address (+ *static-bump-pointer* 16)))
      (incf *static-bump-pointer* (+ (* 6 8) 16))
      ;; Initialize the static header word.
      (setf (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
      ;; Initialize and clear constant slots.
      ;; Function tag, flags and MC size.
      (setf (memref-unsigned-byte-32 address 0) #x00020001
            ;; Constant pool size and slot count.
            (memref-unsigned-byte-32 address 1) #x00000002
            (memref-unsigned-byte-32 address 2) #x00000000
            ;; The code.
            ;; mov64 :rbx (:rip 21)/pool[1]
            (memref-unsigned-byte-32 address 3) #x151D8B48
            ;; jmp (:rip 7)/pool[0]
            (memref-unsigned-byte-32 address 4) #xFF000000
            (memref-unsigned-byte-32 address 5) #x00000725
            (memref-unsigned-byte-32 address 6) #xCCCCCC00
            (memref-unsigned-byte-32 address 7) #xCCCCCCCC
            ;; Constant pool.
            (memref-unsigned-byte-32 address 8) 0
            (memref-unsigned-byte-32 address 9) 0
            (memref-unsigned-byte-32 address 10) 0
            (memref-unsigned-byte-32 address 11) 0)
      (let ((value (%%assemble-value address +tag-function+)))
        ;; Initialize constant pool
        (setf (memref-t address 4) function
              (memref-t address 5) environment)
        value))))

(defun make-interpreted-function (interpreter lambda env &optional name)
  "Allocate a closure object."
  (check-type interpreter function)
  (with-interrupts-disabled ()
    (let ((address (+ *static-bump-pointer* 16)))
      (incf *static-bump-pointer* (+ (* 8 8) 16))
      ;; Initialize the static header word.
      (setf (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
      ;; Initialize and clear constant slots.
      ;; Function tag, flags and MC size.
      (setf (memref-unsigned-byte-32 address 0) #x00020002
            ;; Constant pool size and slot count.
            (memref-unsigned-byte-32 address 1) #x00000004
            (memref-unsigned-byte-32 address 2) #x00000000
            ;; The code.
            ;; mov64 :rbx (:rip 21)/pool[1]
            (memref-unsigned-byte-32 address 3) #x151D8B48
            ;; jmp (:rip 7)/pool[0]
            (memref-unsigned-byte-32 address 4) #xFF000000
            (memref-unsigned-byte-32 address 5) #x00000725
            (memref-unsigned-byte-32 address 6) #xCCCCCC00
            (memref-unsigned-byte-32 address 7) #xCCCCCCCC)
      (let ((value (%%assemble-value address +tag-function+)))
        ;; Initialize constant pool
        (setf (memref-t address 4) interpreter
              (memref-t address 5) lambda
              (memref-t address 6) env
              (memref-t address 7) name)
        value))))

(defun %make-array-header (dimensions fill-pointer info storage)
  (with-interrupts-disabled ()
    (let ((value (%%assemble-value (%raw-allocate 4) +tag-array-header+)))
      (setf (%array-header-dimensions value) dimensions
            (%array-header-fill-pointer value) fill-pointer
            (%array-header-info value) info
            (%array-header-storage value) storage)
      value)))

(defun make-symbol (name)
  (check-type name string)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate 6))
           (symbol (%%assemble-value address +tag-symbol+)))
      ;; symbol-name.
      (setf (memref-t address 0) (sys.int::simplify-string name))
      (makunbound symbol)
      (%fmakunbound symbol)
      (setf (symbol-plist symbol) nil
            (symbol-package symbol) nil
            (%symbol-flags symbol) 0)
      symbol)))

(define-lap-function %%make-bignum-128-rdx-rax ()
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push :rdx)
  ;; Allocate a 2 word bignum.
  (sys.lap-x86:mov64 :rcx 8)
  (sys.lap-x86:mov64 :r8 32) ; fixnum 4 (ugh)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 (:r8) #.(logior (ash 2 8) (ash +array-type-bignum+ 1)))
  ;; Set values.
  (sys.lap-x86:pop (:r8 16))
  (sys.lap-x86:pop (:r8 8))
  ;; pointer to value.
  (sys.lap-x86:or64 :r8 #.+tag-array-like+)
  ;; GC back on.
  (sys.lap-x86:popf)
  ;; Single value return
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:ret))

(define-lap-function %%make-bignum-64-rax ()
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:push 0) ; alignment
  (sys.lap-x86:push :rax)
  ;; Allocate a 1 word bignum.
  (sys.lap-x86:mov64 :rcx 8)
  (sys.lap-x86:mov64 :r8 16) ; fixnum 2 (ugh)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 (:r8) #.(logior (ash 1 8) (ash +array-type-bignum+ 1)))
  ;; Set values.
  (sys.lap-x86:pop (:r8 8))
  ;; realign stack.
  (sys.lap-x86:pop :rax)
  ;; pointer to value.
  (sys.lap-x86:or64 :r8 #.+tag-array-like+)
  ;; GC back on.
  (sys.lap-x86:popf)
  ;; Single value return
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:ret))

;;; This is used by the bignum code so that bignums and fixnums don't have
;;; to be directly compared.
(defun %make-bignum-from-fixnum (n)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate 2)))
      (setf (memref-unsigned-byte-64 address 0) (logior (ash 1 8) (ash +array-type-bignum+ 1))
            (memref-unsigned-byte-64 address 1) n)
      (%%assemble-value address +tag-array-like+))))

(defun %allocate-stack (length)
  (when (oddp length)
    (incf length))
  (prog1 *bump-pointer*
    (incf *bump-pointer* (* length 8))))
