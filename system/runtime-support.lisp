(in-package :sys.int)

(setf sys.lap-x86:*function-reference-resolver* #'function-reference)

(defun proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (special (dolist (var (rest declaration-specifier))
               (setf (system:symbol-mode var) :special)))
    (constant (dolist (var (rest declaration-specifier))
                (setf (system:symbol-mode var) :constant)))
    (inline
     (dolist (name (rest declaration-specifier))
       (multiple-value-bind (sym indicator)
           (if (symbolp name)
               (values name 'inline-mode)
               (values (second name) 'setf-inline-mode))
         (setf (get sym indicator) t))))
    (notinline
     (dolist (name (rest declaration-specifier))
       (multiple-value-bind (sym indicator)
           (if (symbolp name)
               (values name 'inline-mode)
               (values (second name) 'setf-inline-mode))
         (setf (get sym indicator) nil))))))

(defun system:symbol-mode (symbol)
  (svref #(nil :special :constant :symbol-macro)
         (ldb (byte 2 8) (%array-like-ref-unsigned-byte-64 symbol -1))))

(defun (setf system:symbol-mode) (value symbol)
  (setf (ldb (byte 2 8) (%array-like-ref-unsigned-byte-64 symbol -1))
        (ecase value
          ((nil) +symbol-mode-nil+)
          ((:special) +symbol-mode-special+)
          ((:constant) +symbol-mode-constant+)
          ((:symbol-macro) +symbol-mode-symbol-macro+)))
  value)

(defun variable-information (symbol)
  (symbol-mode symbol))

(defun sys.c::function-inline-info (name)
  (multiple-value-bind (sym mode-name form-name)
      (if (symbolp name)
          (values name 'inline-mode 'inline-form)
          (values (second name) 'setf-inline-mode 'setf-inline-form))
    (values (get sym mode-name)
            (get sym form-name))))

(defun apply (function arg &rest more-args)
  (declare (dynamic-extent more-args))
  (check-type function (or function symbol) "a function-designator")
  (when (symbolp function)
    (setf function (symbol-function function)))
  (cond (more-args
         ;; Convert (... (final-list ...)) to (... final-list...)
         (do* ((arg-list (cons arg more-args))
               (i arg-list (cdr i)))
              ((null (cddr i))
               (setf (cdr i) (cadr i))
               (%apply function arg-list))))
        (t (%apply function arg))))

;;; Support function for APPLY.
;;; Takes a function & a list of arguments.
;;; The function must be a function, but type-checking
;;; will be performed on the argument list.
;;; FIXME: should enforce CALL-ARGUMENTS-LIMIT.
(define-lap-function %apply ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Function goes in R13.
  (sys.lap-x86:mov64 :r13 :r8)
  ;; Argument count.
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Words pushed for alignment.
  (sys.lap-x86:xor32 :edi :edi)
  ;; Check for no arguments.
  (sys.lap-x86:cmp64 :r9 nil)
  (sys.lap-x86:je do-call)
  ;; Unpack the list.
  ;; Known to have at least one cons, so we can drop directly into the body.
  (sys.lap-x86:mov64 :rbx :r9)
  unpack-loop
  (:gc :frame :pushed-values-register :rcx)
  ;; Typecheck list, part 2. consp
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne list-type-error)
  ;; Push car & increment arg count
  (sys.lap-x86:push (:car :rbx))
  (:gc :frame :pushed-values-register :rcx :pushed-values 1)
  (sys.lap-x86:add32 :ecx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :pushed-values-register :rcx)
  ;; Advance.
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  ;; Typecheck list, part 1. null
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:jne unpack-loop)
  ;; Arguments have been pushed on the stack in reverse.
  ;; Ensure the stack is misaligned.
  ;; Misalign because 5 registers will be popped off, leaving
  ;; the stack correctly aligned.
  (sys.lap-x86:test64 :rsp 8)
  (sys.lap-x86:jnz stack-aligned)
  ;; Don't push anything extra if there are 5 or fewer args.
  ;; They will all be popped off.
  (sys.lap-x86:cmp32 :ecx #.(ash 5 +n-fixnum-bits+)) ; fixnum 5
  (sys.lap-x86:jbe stack-aligned)
  ;; Reversing will put this at the end of the stack, out of the way.
  (sys.lap-x86:push 0)
  (:gc :frame :pushed-values-register :rcx :pushed-values 1)
  (sys.lap-x86:add32 :ecx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :pushed-values-register :rcx)
  (sys.lap-x86:add32 :edi #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  stack-aligned
  ;; RCX = n arguments. (fixnum)
  ;; RDX = left offset, RAX = right offset.
  (sys.lap-x86:lea32 :eax (:ecx #.(ash -1 +n-fixnum-bits+)))
  (sys.lap-x86:shr32 :eax #.+n-fixnum-bits+)
  (sys.lap-x86:shl32 :eax 3) ; * 8
  (sys.lap-x86:xor32 :edx :edx)
  (sys.lap-x86:jmp reverse-test)
  reverse-loop
  ;; Swap stack+rax & stack+rdx
  (sys.lap-x86:mov64 :r8 (:rsp :rax))
  (sys.lap-x86:mov64 :r9 (:rsp :rdx))
  (sys.lap-x86:mov64 (:rsp :rax) :r9)
  (sys.lap-x86:mov64 (:rsp :rdx) :r8)
  ;; Advance offsets.
  (sys.lap-x86:add32 :edx 8)
  (sys.lap-x86:sub32 :eax 8)
  reverse-test
  ;; Stop when RDX > RAX.
  (sys.lap-x86:cmp32 :eax :edx)
  (sys.lap-x86:ja reverse-loop)
  ;; Drop the word pushed for alignment (if any).
  (sys.lap-x86:sub32 :ecx :edi)
  ;; Put arguments into registers.
  ;; Always at least one argument by this point.
  (sys.lap-x86:pop :r8)
  (:gc :frame :pushed-values-register :rcx :pushed-values -1)
  (sys.lap-x86:cmp32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r9)
  (:gc :frame :pushed-values-register :rcx :pushed-values -2)
  (sys.lap-x86:cmp32 :ecx #.(ash 2 +n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r10)
  (:gc :frame :pushed-values-register :rcx :pushed-values -3)
  (sys.lap-x86:cmp32 :ecx #.(ash 3 +n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r11)
  (:gc :frame :pushed-values-register :rcx :pushed-values -4)
  (sys.lap-x86:cmp32 :ecx #.(ash 4 +n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r12)
  (:gc :frame :pushed-values-register :rcx :pushed-values -5)
  ;; Everything is ready. Call the function!
  do-call
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8)))
  (:gc :frame)
  ;; Finish up & return.
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret)
  ;; R8 = function, R9 = arg-list.
  ;; (raise-type-error arg-list 'proper-list)
  list-type-error
  (:gc :frame)
  (sys.lap-x86:mov64 :r8 :r9)
  (sys.lap-x86:mov64 :r9 (:constant proper-list))
  (sys.lap-x86:mov64 :r13 (:function raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 +n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2))

;;; TODO: This requires a considerably more flexible mechanism.
;;; 12 is where the TLS slots in a stack group start.
;;; NOTE: Is set by initialize-lisp during cold boot.
(defvar *next-symbol-tls-slot*)
(defconstant +maximum-tls-slot+ 512)
(defun %allocate-tls-slot (symbol)
  (when (>= *next-symbol-tls-slot* +maximum-tls-slot+)
    (error "Critial error! TLS slots exhausted!"))
  (let ((slot *next-symbol-tls-slot*))
    (incf *next-symbol-tls-slot*)
    ;; Twiddle TLS bits directly in the symbol header.
    (setf (ldb (byte 16 10) (%array-like-ref-unsigned-byte-64 symbol -1)) slot)
    slot))

(defun %symbol-tls-slot (symbol)
  (ldb (byte 16 8) (%symbol-flags symbol)))

(defun symbol-tls-slot (symbol)
  (let ((slot (ldb (byte 16 8) (%symbol-flags symbol))))
    (if (zerop slot) nil slot)))

(defun funcall (function &rest arguments)
  (declare (dynamic-extent arguments))
  (apply function arguments))

(defun values (&rest values)
  (declare (dynamic-extent values))
  (values-list values))

(defun constantly (value)
  (lambda (&rest arguments)
    (declare (ignore arguments))
    value))

(defun macro-function (symbol &optional env)
  (dolist (e env
           (get symbol '%macro-function))
    (when (eql (first e) :macros)
      (let ((fn (assoc symbol (rest e))))
        (when fn (return (cdr fn)))))))

(defun (setf macro-function) (value symbol &optional env)
  (when env
    (error "TODO: (Setf Macro-function) in environment."))
  (setf (symbol-function symbol) (lambda (&rest r)
                                   (declare (ignore r))
                                   (error 'undefined-function :name symbol))
        (get symbol '%macro-function) value))

;;; Calls to these functions are generated by the compiler to
;;; signal errors.
(defun raise-undefined-function (invoked-through &rest args)
  (error 'undefined-function :name (function-reference-name invoked-through)))

(defun raise-unbound-error (symbol)
  (error 'unbound-variable :name symbol))

(defun raise-type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

(defun %invalid-argument-error (&rest args)
  (declare (ignore args))
  (error 'invalid-arguments))

(defun raise-stack-alignment-error ()
  (error "Stack was misaligned."))

(defun endp (list)
  (cond ((null list) t)
        ((consp list) nil)
        (t (error 'type-error
                  :datum list
                  :expected-type 'list))))

(defun list (&rest args)
  args)

(defun copy-list-in-area (list &optional area)
  (do* ((result (cons nil nil))
        (tail result)
        (l list (cdr l)))
       ((null l)
        (cdr result))
    (setf (cdr tail) (cons-in-area (car l) nil area)
          tail (cdr tail))))

(defun copy-list (list)
  (copy-list-in-area list))

;;; Will be overriden later in the init process.
(defun funcallable-instance-lambda-expression (function)
  (values nil t nil))
(defun funcallable-instance-debug-info (function)
  nil)

(defun function-name (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (ecase (%object-tag function)
      (#.+object-tag-function+ ;; Regular function. First entry in the constant pool.
       (memref-t address (* (logand (ash info -16) #xFFFF) 2)))
      (#.+object-tag-closure+ ;; Closure.
       (function-name (memref-t address 4)))
      (#.+object-tag-funcallable-instance+
       (multiple-value-bind (lambda closurep name)
           (funcallable-instance-lambda-expression function)
         (declare (ignore lambda closurep))
         name)))))

(defun function-lambda-expression (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (ecase (%object-tag function)
      (#.+object-tag-function+ ;; Regular function. First entry in the constant pool.
       (values nil nil (memref-t address (* (logand (ash info -16) #xFFFF) 2))))
      (#.+object-tag-closure+ ;; Closure.
       (values nil t (function-name (memref-t address 4))))
      (#.+object-tag-funcallable-instance+
       (funcallable-instance-lambda-expression function)))))

(defun function-debug-info (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (ecase (%object-tag function)
      (#.+object-tag-function+ ;; Regular function. second entry in the constant pool.
       (memref-t address (1+ (* (logand (ash info -16) #xFFFF) 2))))
      (#.+object-tag-closure+ ;; Closure.
       (function-debug-info (memref-t address 4)))
      (#.+object-tag-funcallable-instance+
       (funcallable-instance-debug-info function)))))

(defun funcallable-std-instance-p (object)
  (and (functionp object)
       (eql (%object-tag object)
            +object-tag-funcallable-instance+)))

(defun funcallable-std-instance-function (funcallable-instance)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16)))
    (memref-t address 4)))
(defun (setf funcallable-std-instance-function) (value funcallable-instance)
  (check-type value function)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16))
         (entry-point (%array-like-ref-unsigned-byte-64 value 0)))
    ;; Fixme: should do this atomically.
    ;; Must update entry-point first to avoid old function from being gc'd away.
    (setf (memref-unsigned-byte-64 address 3) entry-point
          (memref-t address 4) value)))

(defun funcallable-std-instance-class (funcallable-instance)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16)))
    (memref-t address 5)))
(defun (setf funcallable-std-instance-class) (value funcallable-instance)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16)))
    (setf (memref-t address 5) value)))

(defun funcallable-std-instance-slots (funcallable-instance)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16)))
    (memref-t address 6)))
(defun (setf funcallable-std-instance-slots) (value funcallable-instance)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16)))
    (setf (memref-t address 6) value)))

(defun compiled-function-p (object)
  ;; FIXME: interpreted functions (of class sys.eval::interpreted-function) shouldn't return true.
  (functionp object))

(defvar *gensym-counter* 0)
(defun gensym (&optional (thing "G"))
  (check-type thing (or string (integer 0)))
  (if (integerp thing)
      (make-symbol (format nil "G~D" thing))
      (prog1 (make-symbol (format nil "~A~D" thing *gensym-counter*))
        (incf *gensym-counter*))))

;;; TODO: Expand this so it knows about the compiler's constant folders.
(defun constantp (form &optional environment)
  (declare (ignore environment))
  (typecase form
    (symbol (eql (symbol-mode form) :constant))
    (cons (eql (first form) 'quote))
    (t t)))

(defvar *active-catch-handlers* '())
(defun %catch (tag fn)
  (let ((*active-catch-handlers* (cons (cons tag
                                             (lambda (values)
                                               (return-from %catch (values-list values))))
                                       *active-catch-handlers*)))
    (funcall fn)))

(defun %throw (tag values)
  (let ((target (assoc tag *active-catch-handlers* :test 'eq)))
    (if target
        (funcall (cdr target) values)
        (error 'bad-catch-tag-error
               :tag tag))))

(defun %progv (symbols values fn)
  ;; Save the special-stack
  (let ((special-stack (%%special-stack-pointer)))
    ;; Bind each variable.
    (do ((s symbols (rest s))
         (v values (rest v)))
        ((null s))
      (check-type (first s) symbol)
      (%%bind (first s) (if v
                            (first v)
                            (%unbound-value))))
    (multiple-value-prog1 (funcall fn)
      ;; Now pop the special stack. This is not done with unwind-protect,
      ;; because a non-local exit will unwind the stack anyway.
      (%%unwind-to special-stack))))

(defun %%unwind-to (target-special-stack-pointer)
  (declare (suppress-ssp-checking))
  (loop (when (eq target-special-stack-pointer (%%special-stack-pointer))
          (return))
     (assert (< (%%special-stack-pointer) target-special-stack-pointer))
     (etypecase (memref-t (ash (%%special-stack-pointer) +n-fixnum-bits+) 0)
       (symbol
        (%%unbind))
       (simple-vector
        (%%disestablish-block-or-tagbody))
       (function
        (%%disestablish-unwind-protect)))))

(defun function-tag (function)
  (check-type function function)
  (%object-tag function))

(defun function-pool-size (function)
  (check-type function function)
  (let ((address (logand (lisp-object-address function) -16)))
    (memref-unsigned-byte-16 address 2)))

(defun function-code-size (function)
  (check-type function function)
  (let ((address (logand (lisp-object-address function) -16)))
    (* (memref-unsigned-byte-16 address 1) 16)))

(defun function-pool-object (function offset)
  (check-type function function)
  (let ((address (logand (lisp-object-address function) -16))
        (mc-size (truncate (function-code-size function) 8))) ; in words.
    (memref-t address (+ mc-size offset))))

(defun function-code-byte (function offset)
  (check-type function function)
  (let ((address (logand (lisp-object-address function) -16)))
    (memref-unsigned-byte-8 address offset)))

(defun function-gc-info (function)
  "Return the address of and the number of bytes in FUNCTION's GC info."
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (gc-length (memref-unsigned-byte-16 address 3))
         (mc-size (function-code-size function))
         (n-constants (function-pool-size function)))
    (values (+ address mc-size (* n-constants 8)) ; Address.
            gc-length))) ; Length.

(defun decode-function-gc-info (function)
  (multiple-value-bind (address length)
      (function-gc-info function)
    (let ((position 0)
          (result '())
          (register-ids #(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)))
      (flet ((consume (&optional (errorp t))
               (when (>= position length)
                 (when errorp
                   (error "Reached end of GC Info??"))
                 (return-from decode-function-gc-info (reverse result)))
               (prog1 (memref-unsigned-byte-8 address position)
                 (incf position))))
        (loop (let ((address 0)
                    flags-and-pvr
                    mv-and-iabtt
                    (pv 0)
                    (n-layout-bits 0)
                    (layout (make-array 32 :element-type 'bit :adjustable t :fill-pointer 0)))
                ;; Read first byte of address, this is where we can terminate.
                (let ((byte (consume nil))
                      (offset 0))
                  (setf address (ldb (byte 7 0) byte)
                        offset 7)
                  (when (logtest byte #x80)
                    ;; Read remaining bytes.
                    (loop (let ((byte (consume)))
                            (setf (ldb (byte 7 offset) address)
                                  (ldb (byte 7 0) byte))
                            (incf offset 7)
                            (unless (logtest byte #x80)
                              (return))))))
                ;; Read flag/pvr byte
                (setf flags-and-pvr (consume))
                ;; Read mv-and-iabtt
                (setf mv-and-iabtt (consume))
                ;; Read vs32 pv.
                (let ((shift 0))
                  (loop
                     (let ((b (consume)))
                       (when (not (logtest b #x80))
                         (setf pv (logior pv (ash (logand b #x3F) shift)))
                         (when (logtest b #x40)
                           (setf pv (- pv)))
                         (return))
                       (setf pv (logior pv (ash (logand b #x7F) shift)))
                       (incf shift 7))))
                ;; Read vu32 n-layout bits.
                (let ((shift 0))
                  (loop
                     (let ((b (consume)))
                       (setf n-layout-bits (logior n-layout-bits (ash (logand b #x7F) shift)))
                       (when (not (logtest b #x80))
                         (return))
                       (incf shift 7))))
                ;; Consume layout bits.
                (dotimes (i (ceiling n-layout-bits 8))
                  (let ((byte (consume)))
                    (dotimes (j 8)
                      (vector-push-extend (ldb (byte 1 j) byte) layout))))
                (setf (fill-pointer layout) n-layout-bits)
                (let ((entry '()))
                  (unless (zerop n-layout-bits)
                    (setf (getf entry :layout) layout))
                  (ecase (ldb (byte 2 2) flags-and-pvr)
                    (#b00)
                    (#b01 (setf (getf entry :block-or-tagbody-thunk)
                                (svref register-ids (ldb (byte 4 4) mv-and-iabtt))))
                    (#b10 (setf (getf entry :incoming-arguments)
                                (svref register-ids (ldb (byte 4 4) mv-and-iabtt))))
                    (#b11 (setf (getf entry :incoming-arguments)
                                `(:stack ,(ldb (byte 4 4) mv-and-iabtt)))))
                  (unless (eql (ldb (byte 4 0) mv-and-iabtt) 15)
                    (setf (getf entry :multiple-values)
                          (ldb (byte 4 0) mv-and-iabtt)))
                  (unless (eql (ldb (byte 4 4) flags-and-pvr) 4)
                    (setf (getf entry :pushed-values-register)
                          (svref register-ids (ldb (byte 4 4) flags-and-pvr))))
                  (unless (zerop pv)
                    (setf (getf entry :pushed-values) pv))
                  (when (logtest flags-and-pvr #b0010)
                    (setf (getf entry :interrupt) t))
                  (push (list* address
                               (if (logtest flags-and-pvr #b0001)
                                   :frame
                                   :no-frame)
                               entry)
                        result))))))))

(defun get-structure-type (name &optional (errorp t))
  (or (get name 'structure-type)
      (and errorp
           (error "Unknown structure type ~S." name))))

(defun concat-symbols (&rest symbols)
  (intern (apply 'concatenate 'string (mapcar 'string symbols))))

(defvar *gentemp-counter* 0)

(defun gentemp (&optional (prefix "T") (package *package*))
  (check-type prefix string)
  (do () (nil)
    (let ((name (format nil "~A~D" prefix (incf *gentemp-counter*))))
      (multiple-value-bind (x status)
          (find-symbol name package)
        (declare (ignore x))
        (unless status (return (intern name package)))))))

(defun special-operator-p (symbol)
  (check-type symbol symbol)
  (member symbol '(block catch eval-when flet function go if labels
                   let let* load-time-value locally macrolet
                   multiple-value-call multiple-value-prog1
                   progn progv quote return-from setq symbol-macrolet
                   tagbody the throw unwind-protect)))

(defun %array-like-p (object)
  (eql (%tag-field object) +tag-object+))

(defun %array-like-header (object)
  (memref-unsigned-byte-64 (ash (%pointer-field object) 4) 0))

(defun %array-like-type (object)
  (logand (1- (ash 1 +array-type-size+))
          (ash (%array-like-header object) (- +array-type-shift+))))

(define-lap-function values-list ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:sub64 :rsp 16) ; 2 slots
  (sys.lap-x86:cmp32 :ecx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  ;; RBX = iterator, (:stack 0) = list.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:mov64 (:stack 0) :r8)
  (:gc :frame :layout #*10)
  ;; ECX = value count.
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Pop into R8.
  ;; If LIST is NIL, then R8 must be NIL, so no need to
  ;; set R8 to NIL in the 0-values case.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r8 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R9.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r9 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R10.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r10 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R11.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r11 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R12.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r12 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 +tag-object+)
                               (* +stack-group-offset-mv-slots+ 8)))
  (:gc :frame :layout #*10 :multiple-values 0)
  unpack-loop
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:cmp32 :ecx #.(ash (+ +stack-group-mv-slots-size+ 5) +n-fixnum-bits+))
  (sys.lap-x86:jae too-many-values)
  (sys.lap-x86:mov64 :r13 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (:gc :frame :layout #*10 :multiple-values 1)
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (:gc :frame :layout #*10 :multiple-values 0)
  (sys.lap-x86:add64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:jmp unpack-loop)
  done
  (sys.lap-x86:leave)
  (:gc :no-frame :multiple-values 0)
  (sys.lap-x86:ret)
  type-error
  (:gc :frame :layout #*10)
  (sys.lap-x86:mov64 :r8 (:stack 0))
  (sys.lap-x86:mov64 :r9 (:constant proper-list))
  (sys.lap-x86:mov64 :r13 (:function raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 +n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in list ~S."))
  (sys.lap-x86:mov64 :r9 (:stack 0))
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 +n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2)
  bad-arguments
  (:gc :frame)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%invalid-argument-error))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2))
