(in-package :sys.int)

(defun proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (special (dolist (var (rest declaration-specifier))
               (setf (system:symbol-mode var) :special)))
    (constant (dolist (var (rest declaration-specifier))
                (setf (system:symbol-mode var) :constant)))
    (inline
     (dolist (name (rest declaration-specifier))
       (let ((sym (function-symbol name)))
         (setf (get sym 'inline-mode) t))))
    (notinline
     (dolist (name (rest declaration-specifier))
       (let ((sym (function-symbol name)))
         (setf (get sym 'inline-mode) nil))))))

(defun system:symbol-mode (symbol)
  (svref #(nil :special :constant :symbol-macro)
         (ldb (byte 2 0) (%symbol-flags symbol))))

(defun (setf system:symbol-mode) (value symbol)
  (setf (ldb (byte 2 0) (%symbol-flags symbol))
        (ecase value
          ((nil) +symbol-mode-nil+)
          ((:special) +symbol-mode-special+)
          ((:constant) +symbol-mode-constant+)
          ((:symbol-macro) +symbol-mode-symbol-macro+)))
  value)

(defun variable-information (symbol)
  (symbol-mode symbol))

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
  (sys.lap-x86:mov64 :rbp :rsp)
  ;; Function goes in R13.
  (sys.lap-x86:mov64 :r13 :r8)
  ;; Argument count.
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Check for no arguments.
  (sys.lap-x86:cmp64 :r9 nil)
  (sys.lap-x86:je do-call)
  ;; Unpack the list.
  (sys.lap-x86:mov64 :rbx :r9)
  (sys.lap-x86:jmp unpack-test)
  unpack-loop
  ;; Typecheck list, part 2. consp
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne list-type-error)
  ;; Push car & increment arg count
  (sys.lap-x86:push (:car :rbx))
  (sys.lap-x86:add64 :rcx 8) ; fixnum 1
  ;; Advance.
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  unpack-test
  ;; Typecheck list, part 1. null
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:jne unpack-loop)
  ;; Arguments have been pushed on the stack in reverse.
  ;; RCX = n arguments.
  ;; RDX = left offset, RAX = right offset.
  (sys.lap-x86:lea64 :rax (:rcx -8))
  (sys.lap-x86:xor32 :edx :edx)
  ;; Ensure the stack is misaligned.
  ;; Misalign because 5 registers will be popped off, leaving
  ;; the stack correctly aligned.
  (sys.lap-x86:test64 :rsp 8)
  (sys.lap-x86:jnz reverse-test)
  ;; Don't push anything extra if there are 5 or fewer args.
  ;; They will all be popped off.
  (sys.lap-x86:cmp64 :rcx #.(* 5 8)) ; fixnum 5
  (sys.lap-x86:jbe reverse-test)
  ;; Reversing will put this at the end of the stack, out of the way.
  (sys.lap-x86:push 0)
  (sys.lap-x86:add64 :rax 8)
  (sys.lap-x86:jmp reverse-test)
  reverse-loop
  ;; Swap stack+rax & stack+rdx
  (sys.lap-x86:mov64 :r8 (:rsp :rax))
  (sys.lap-x86:mov64 :r9 (:rsp :rdx))
  (sys.lap-x86:mov64 (:rsp :rax) :r9)
  (sys.lap-x86:mov64 (:rsp :rdx) :r8)
  ;; Advance offsets.
  (sys.lap-x86:add64 :rdx 8)
  (sys.lap-x86:sub64 :rax 8)
  reverse-test
  ;; Stop when RDX > RAX
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:ja reverse-loop)
  ;; Put arguments into registers.
  ;; Always at least one argument by this point.
  (sys.lap-x86:pop :r8)
  (sys.lap-x86:cmp64 :rcx 8)
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r9)
  (sys.lap-x86:cmp64 :rcx 16)
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r10)
  (sys.lap-x86:cmp64 :rcx 24)
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r11)
  (sys.lap-x86:cmp64 :rcx 32)
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r12)
  ;; Everything is ready. Call the function!
  do-call
  (sys.lap-x86:call :r13)
  ;; Finish up & return.
  (sys.lap-x86:leave)
  (sys.lap-x86:ret)
  ;; R8 = function, R9 = arg-list.
  ;; (raise-type-error arg-list 'proper-list)
  list-type-error
  (sys.lap-x86:mov64 :r8 :r9)
  (sys.lap-x86:mov64 :r9 (:constant proper-list))
  (sys.lap-x86:mov64 :r13 (:constant raise-type-error))
  (sys.lap-x86:mov32 :ecx 16) ; fixnum 2
  (sys.lap-x86:call (:symbol-function :r13))
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
    (setf (ldb (byte 16 8) (%symbol-flags symbol)) slot)
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

(defun fboundp (name)
  (%fboundp (function-symbol name)))

(defun fmakunbound (name)
  (%fmakunbound (function-symbol name))
  name)

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
  ;; Convert setf-symbols back to (setf foo).
  (when (and (symbolp invoked-through)
             (get invoked-through 'setf-symbol-backlink))
    (setf invoked-through `(setf ,(get invoked-through 'setf-symbol-backlink))))
  (error 'undefined-function :name invoked-through))

(defun raise-undefined-function-via-%symbol-function (invoked-through)
  ;; Convert setf-symbols back to (setf foo).
  (when (and (symbolp invoked-through)
             (get invoked-through 'setf-symbol-backlink))
    (setf invoked-through `(setf ,(get invoked-through 'setf-symbol-backlink))))
  (error 'undefined-function :name invoked-through))

(defun raise-unbound-error (symbol)
  (error 'unbound-variable :name symbol))

(defun raise-type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

(defun %invalid-argument-error (&rest args)
  (declare (ignore args))
  (error 'invalid-arguments))

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
    (ecase (logand info #xFF)
      (#.+function-type-function+ ;; Regular function. First entry in the constant pool.
       (memref-t address (* (logand (ash info -16) #xFFFF) 2)))
      (#.+function-type-closure+ ;; Closure.
       (function-name (memref-t address 4)))
      (#.+function-type-funcallable-instance+
       (multiple-value-bind (lambda closurep name)
           (funcallable-instance-lambda-expression function)
         (declare (ignore lambda closurep))
         name)))))

(defun function-lambda-expression (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (ecase (logand info #xFF)
      (#.+function-type-function+ ;; Regular function. First entry in the constant pool.
       (values nil nil (memref-t address (* (logand (ash info -16) #xFFFF) 2))))
      (#.+function-type-closure+ ;; Closure.
       (values nil t (function-name (memref-t address 4))))
      (#.+function-type-funcallable-instance+
       (funcallable-instance-lambda-expression function)))))

(defun function-debug-info (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (ecase (logand info #xFF)
      (#.+function-type-function+ ;; Regular function. second entry in the constant pool.
       (memref-t address (1+ (* (logand (ash info -16) #xFFFF) 2))))
      (#.+function-type-closure+ ;; Closure.
       (function-debug-info (memref-t address 4)))
      (#.+function-type-funcallable-instance+
       (funcallable-instance-debug-info function)))))

(defun funcallable-std-instance-p (object)
  (when (functionp object)
    (let* ((address (logand (lisp-object-address object) -16))
           (info (memref-unsigned-byte-64 address 0)))
      (eql (ldb (byte 8 0) info) +function-type-funcallable-instance+))))

(defun funcallable-std-instance-function (funcallable-instance)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16)))
    (memref-t address 4)))
(defun (setf funcallable-std-instance-function) (value funcallable-instance)
  (check-type value function)
  (assert (funcallable-std-instance-p funcallable-instance) (funcallable-instance))
  (let* ((address (logand (lisp-object-address funcallable-instance) -16)))
    (setf (memref-t address 4) value)))

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
  (when (functionp object)
    (let* ((address (logand (lisp-object-address object) -16))
           (info (memref-unsigned-byte-64 address 0)))
      (not (eql (logand info #xFF) +function-type-interpreted-function+)))))

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
                            (%%assemble-value 0 +tag-unbound-value+))))
    (multiple-value-prog1 (funcall fn)
      ;; Now pop the special stack. This is not done with unwind-protect,
      ;; because a non-local exit will unwind the stack anyway.
      (%%unwind-to special-stack))))

(defun %%unwind-to (target-special-stack-pointer)
  (declare (suppress-ssp-checking))
  (loop (when (eq target-special-stack-pointer (%%special-stack-pointer))
          (return))
     (assert (< (%%special-stack-pointer) target-special-stack-pointer))
     (etypecase (memref-t 0 (%%special-stack-pointer))
       (symbol
        (%%unbind))
       (simple-vector
        (%%disestablish-block-or-tagbody))
       (function
        (%%disestablish-unwind-protect)))))

(defun function-tag (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (ldb (byte 8 0) info)))

(defun function-pool-size (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (ldb (byte 16 32) info)))

(defun function-code-size (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (* (ldb (byte 16 16) info) 16)))

(defun function-pool-object (function offset)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0))
         (mc-size (* (ldb (byte 16 16) info) 2))) ; in words.
    (memref-t address (+ mc-size offset))))

(defun function-code-byte (function offset)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (memref-unsigned-byte-8 address offset)))

(defun function-gc-info (function)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (info (memref-unsigned-byte-64 address 0)))
    (values (ldb (byte 16 48) info) ; length
            (memref-unsigned-byte-32 address 2)))) ; offset

(defun function-gc-info-entry (function entry-number)
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (length (memref-unsigned-byte-16 address 3))
         (offset (memref-unsigned-byte-32 address 2)))
    (check-type entry-number (integer 0))
    (assert (< entry-number length))
    (values (memref-unsigned-byte-32 (+ address offset) (* entry-number 2))
            (memref-unsigned-byte-32 (+ address offset) (1+ (* entry-number 2))))))

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
  (eql (%tag-field object) +tag-array-like+))

(defun %array-like-header (object)
  (memref-unsigned-byte-64 (ash (%pointer-field object) 4) 0))

(defun %array-like-type (object)
  (logand (1- (ash 1 +array-type-size+))
          (ash (%array-like-header object) (- +array-type-shift+))))

(define-lap-function values-list ()
  (sys.lap-x86:push :rbp)
  (sys.lap-x86:mov64 :rbp :rsp)
  (sys.lap-x86:sub64 :rsp 16) ; 2 slots
  (sys.lap-x86:cmp32 :ecx 8) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  ;; RBX = iterator, (:stack 1) = list.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:mov64 (:stack 1) :r8)
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
  (sys.lap-x86:add64 :rcx 8) ; fixnum 1
  ;; Pop into R9.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r9 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx 8) ; fixnum 1
  ;; Pop into R10.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r10 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx 8) ; fixnum 1
  ;; Pop into R11.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r11 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx 8) ; fixnum 1
  ;; Pop into R12.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r12 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx 8) ; fixnum 1
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 +tag-array-like+)
                               (* +stack-group-offset-mv-slots+ 8)))
  unpack-loop
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:cmp32 :ecx #.(* (+ +stack-group-mv-slots-size+ 5) 8))
  (sys.lap-x86:jae too-many-values)
  (sys.lap-x86:mov64 :r13 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx 8) ; fixnum 1
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:jmp unpack-loop)
  done
  (sys.lap-x86:leave)
  (sys.lap-x86:ret)
  type-error
  (sys.lap-x86:mov64 :r8 (:stack 1))
  (sys.lap-x86:mov64 :r9 (:constant proper-list))
  (sys.lap-x86:mov64 :r13 (:constant raise-type-error))
  (sys.lap-x86:mov32 :ecx 16) ; fixnum 2
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in list ~S."))
  (sys.lap-x86:mov64 :r9 (:stack 1))
  (sys.lap-x86:mov64 :r13 (:constant error))
  (sys.lap-x86:mov32 :ecx 16) ; fixnum 2
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:ud2)
  bad-arguments
  (sys.lap-x86:mov64 :r13 (:constant sys.int::%invalid-argument-error))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:ud2))
