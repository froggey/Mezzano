;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.x86-64)

(defclass x86-instruction (mezzano.compiler.backend::backend-instruction)
  ((%inputs :initarg :inputs :reader mezzano.compiler.backend::instruction-inputs)
   (%outputs :initarg :outputs :reader mezzano.compiler.backend::instruction-outputs)
   (%opcode :initarg :opcode :reader x86-instruction-opcode)
   (%operands :initarg :operands :reader x86-instruction-operands)))

(defmethod mezzano.compiler.backend::replace-all-registers ((instruction x86-instruction) substitution-function)
  (setf (slot-value instruction '%inputs) (mapcar substitution-function (slot-value instruction '%inputs)))
  (setf (slot-value instruction '%outputs) (mapcar substitution-function (slot-value instruction '%outputs)))
  (setf (slot-value instruction '%operands)
        (loop
           for operand in (slot-value instruction '%operands)
           collect (cond ((typep operand 'virtual-register)
                          (funcall substitution-function operand))
                         ((and (consp operand)
                               (not (member (first operand) '(:constant :function))))
                          (mapcar substitution-function operand))
                         (t operand)))))

(defmethod mezzano.compiler.backend::print-instruction ((instruction x86-instruction))
  (format t "   ~S~%"
          `(:x86 ,(x86-instruction-opcode instruction) ,(x86-instruction-operands instruction))))

(defclass x86-branch-instruction (mezzano.compiler.backend::terminator-instruction)
  ((%opcode :initarg :opcode :reader x86-instruction-opcode)
   (%target :initarg :target :reader x86-branch-target)))

(defmethod mezzano.compiler.backend::instruction-inputs ((instruction x86-branch-instruction))
  '())

(defmethod mezzano.compiler.backend::instruction-outputs ((instruction x86-branch-instruction))
  '())

(defmethod mezzano.compiler.backend::replace-all-registers ((instruction x86-branch-instruction) substitution-function)
  )

(defmethod mezzano.compiler.backend::print-instruction ((instruction x86-branch-instruction))
  (format t "   ~S~%"
          `(:x86-branch ,(x86-instruction-opcode instruction) ,(x86-branch-target instruction))))

(defun resolve-constant (register defs)
  (let ((register-defs (gethash register defs)))
    (cond ((and register-defs
                (typep (first register-defs) 'constant-instruction)
                (endp (rest register-defs)))
           (values (constant-value (first register-defs)) t))
          (t
           (values nil nil)))))

(defun maybe-constant-operand (operand defs)
  (multiple-value-bind (value validp)
      (resolve-constant operand defs)
    (cond (validp
           (cond ((member value '(nil t))
                  value)
                 ((and (sys.c::fixnump value)
                       (typep (mezzano.compiler.codegen.x86-64::fixnum-to-raw value)
                              '(signed-byte 32)))
                  (mezzano.compiler.codegen.x86-64::fixnum-to-raw value))
                 ((characterp value)
                  (mezzano.compiler.codegen.x86-64::character-to-raw value))
                 (t
                  `(:constant ,value))))
          (t
           operand))))

(defun consumed-by-p (definition consumer uses defs)
  "Return true if all DEFINITION's outputs are only used by CONSUMER."
  (dolist (out (mezzano.compiler.backend::instruction-outputs definition)
           t)
    (when (typep out 'virtual-register)
      (let ((out-defs (gethash out defs))
            (out-uses (gethash out uses)))
        (format t "Out: ~S  defs: ~S  uses: ~S~%" out out-defs out-uses)
        ;; Must have one definition.
        (when (not (and out-defs
                        (eql (first out-defs) definition)
                        (endp (rest out-defs))))
          (return nil))
        ;; Must have one or zero uses
        (when (not (or (endp out-uses)
                       (and out-uses
                            (eql (first out-uses) consumer)
                            (endp (rest out-uses)))))
          (return nil))))))

(defun lower (backend-function)
  (multiple-value-bind (uses defs)
      (mezzano.compiler.backend::build-use/def-maps backend-function)
    (do* ((inst (mezzano.compiler.backend::first-instruction backend-function) next-inst)
          (next-inst (mezzano.compiler.backend::next-instruction backend-function inst) (if inst (mezzano.compiler.backend::next-instruction backend-function inst))))
         ((null inst))
      (cond ((and (typep inst 'eq-instruction)
                  (typep next-inst 'branch-instruction)
                  (consumed-by-p inst next-inst uses defs))
             ;; (branch (eq lhs rhs) target) => (cmp lhs rhs) (bcc target)
             (let* ((eq-inst inst)
                    (branch-inst next-inst)
                    (lhs (eq-lhs eq-inst))
                    (rhs (eq-rhs eq-inst))
                    (true-rhs (maybe-constant-operand rhs defs)))
               (mezzano.compiler.backend::insert-before
                backend-function eq-inst
                (make-instance 'x86-instruction
                               :opcode 'lap:cmp64
                               :operands (list lhs true-rhs)
                               :inputs (if (eql true-rhs rhs)
                                           (list lhs rhs)
                                           (list lhs))
                               :outputs '()))
               (mezzano.compiler.backend::insert-before
                backend-function eq-inst
                (make-instance 'x86-branch-instruction
                               :opcode (if (typep next-inst 'branch-true-instruction)
                                           'lap:je
                                           'lap:jne)
                               :target (branch-target next-inst)))
               ;; Point next to the instruction after the branch.
               (setf next-inst (mezzano.compiler.backend::next-instruction backend-function next-inst))
               ;; Remove the eq & branch.
               (mezzano.compiler.backend::remove-instruction backend-function eq-inst)
               (mezzano.compiler.backend::remove-instruction backend-function branch-inst)))
            ((and (typep inst 'undefined-function-p-instruction)
                  (typep next-inst 'branch-instruction)
                  (consumed-by-p inst next-inst uses defs))
             ;; (branch (undefined-function-p lhs rhs) target) => (cmp lhs rhs) (bcc target)
             (let* ((branch-inst next-inst)
                    (value (undefined-function-p-value inst)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:cmp64
                               :operands (list value :undefined-function)
                               :inputs (list value)
                               :outputs '()))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-branch-instruction
                               :opcode (if (typep next-inst 'branch-true-instruction)
                                           'lap:je
                                           'lap:jne)
                               :target (branch-target next-inst)))
               ;; Point next to the instruction after the branch.
               (setf next-inst (mezzano.compiler.backend::next-instruction backend-function next-inst))
               ;; Remove the eq & branch.
               (mezzano.compiler.backend::remove-instruction backend-function inst)
               (mezzano.compiler.backend::remove-instruction backend-function branch-inst)))
            ((typep inst 'object-get-t-instruction)
             ;; (object-get-t dest obj constant-index) => (mov dest (obj ind))
             (let* ((dest (object-get-destination inst))
                    (obj (object-get-object inst))
                    (index (object-get-index inst)))
               (multiple-value-bind (index-value index-validp)
                   (resolve-constant index defs)
                 (when (and index-validp
                            (typep index-value '(signed-byte 29)))
                   (mezzano.compiler.backend::insert-before
                    backend-function inst
                    (make-instance 'x86-instruction
                                   :opcode 'lap:mov64
                                   :operands (list dest `(:object ,obj ,index-value))
                                   :inputs (list obj)
                                   :outputs (list dest)))
                   (mezzano.compiler.backend::remove-instruction backend-function inst)))))
            ((typep inst 'object-set-t-instruction)
             ;; (object-set-t value obj constant-index) => (mov (obj ind) value)
             (let* ((value (object-set-value inst))
                    (obj (object-set-object inst))
                    (index (object-set-index inst)))
               (multiple-value-bind (index-value index-validp)
                   (resolve-constant index defs)
                 (when (and index-validp
                            (typep index-value '(signed-byte 29)))
                   (mezzano.compiler.backend::insert-before
                    backend-function inst
                    (make-instance 'x86-instruction
                                   :opcode 'lap:mov64
                                   :operands (list `(:object ,obj ,index-value) value)
                                   :inputs (list value obj)
                                   :outputs (list)))
                   (mezzano.compiler.backend::remove-instruction backend-function inst)))))))))

(defun peephole (backend-function)
  (do* ((inst (mezzano.compiler.backend::first-instruction backend-function) next-inst)
        (next-inst (mezzano.compiler.backend::next-instruction backend-function inst) (if inst (mezzano.compiler.backend::next-instruction backend-function inst))))
       ((null inst))
    (cond ((and (typep inst 'move-instruction)
                (eql (move-source inst) (move-destination inst)))
           ;; Delete (move foo foo)
           (mezzano.compiler.backend::remove-instruction backend-function inst))
          ((and (typep inst 'move-instruction)
                (typep next-inst 'spill-instruction)
                (eql (move-destination inst) (spill-source next-inst)))
           ;; (move t reg) (spill vreg t) => (spill vreg reg)
           (setf (spill-source next-inst) (move-source inst))
           (mezzano.compiler.backend::remove-instruction backend-function inst))
          ((and (typep inst 'fill-instruction)
                (typep next-inst 'move-instruction)
                (eql (fill-destination inst) (move-source next-inst)))
           ;; (fill t vreg) (move reg t) => (fill reg vreg)
           (setf (fill-destination inst) (move-destination next-inst))
           (mezzano.compiler.backend::remove-instruction backend-function next-inst)
           (setf next-inst inst))
          ((and (typep inst 'spill-instruction)
                (typep next-inst 'fill-instruction)
                (eql (spill-destination inst) (fill-source next-inst)))
           ;; (spill vreg r1) (fill r2 vreg) => (spill vreg r2) (move r2 r1)
           (when (not (eql (spill-source inst) (fill-destination next-inst)))
             (mezzano.compiler.backend::insert-after
              backend-function inst
              (make-instance 'move-instruction
                             :destination (fill-destination next-inst)
                             :source (spill-source inst))))
           (mezzano.compiler.backend::remove-instruction backend-function next-inst)
           (setf next-inst inst)))))

(defvar *emitted-lap*)
(defvar *stack-layout*)
(defvar *saved-multiple-values*)
(defvar *dx-root-visibility*)
(defvar *current-frame-layout*)
(defvar *prepass-data*)

(defun emit (&rest instructions)
  (dolist (i instructions)
    (push i *emitted-lap*)))

(defun emit-gc-info (&rest metadata)
  (emit `(:gc :frame :layout ,*current-frame-layout* ,@metadata)))

(defgeneric lap-prepass (backend-function instruction uses defs)
  (:method (backend-function instruction uses defs) nil))
(defgeneric emit-lap (backend-function instruction uses defs))

(defun compute-stack-layout (backend-function uses defs)
  (declare (ignore backend-function uses))
  (let ((layout (loop
                   for vreg being the hash-keys of defs using (hash-value vreg-defs)
                   when (not (and vreg-defs
                                  (endp (rest vreg-defs))
                                  (typep (first vreg-defs) 'save-multiple-instruction)))
                   collect vreg)))
    (make-array (length layout)
                :adjustable t
                :fill-pointer t
                :initial-contents layout)))

(defun allocate-stack-slots (count &key (livep t))
  (assert (not (boundp '*current-frame-layout*)) ()
          "Allocating stack slots after stack frame layout.")
  (dotimes (i count)
    (vector-push-extend (if livep :live :raw) *stack-layout*))
  (length *stack-layout*))

(defun effective-address (vreg)
  `(:stack ,(or (position vreg *stack-layout*)
                (error "Missing stack slot for vreg ~S" vreg))))

(defun to-lap (backend-function)
  (multiple-value-bind (uses defs)
      (mezzano.compiler.backend::build-use/def-maps backend-function)
    (let ((*stack-layout* (compute-stack-layout backend-function uses defs))
          (*saved-multiple-values* (make-hash-table))
          (*dx-root-visibility* (make-hash-table))
          (*prepass-data* (make-hash-table)))
      (dolist (inst-or-label (mezzano.compiler.backend::backend-function-code backend-function))
        (when (not (symbolp inst-or-label))
          (lap-prepass backend-function inst-or-label uses defs)))
      (let ((*emitted-lap* '())
            (*current-frame-layout* (coerce (loop
                                               for elt across *stack-layout*
                                               collect (if (eql elt :raw)
                                                           0
                                                           1))
                                            'simple-bit-vector))
            (mv-flow (mezzano.compiler.backend::multiple-value-flow backend-function :x86-64)))
        ;; Create stack frame.
        (emit `(:gc :no-frame :incoming-arguments :rcx :layout #*0)
              `(lap:push :rbp)
              `(:gc :no-frame :incoming-arguments :rcx :layout #*00)
              `(lap:mov64 :rbp :rsp)
              `(:gc :frame :incoming-arguments :rcx))
        (let ((stack-size (length *current-frame-layout*)))
          (when (oddp stack-size)
            (incf stack-size))
          (when (not (zerop stack-size))
            (emit `(lap:sub64 :rsp ,(* stack-size 8))))
          (loop
             for i from 0
             for elt across *current-frame-layout*
             do (when (not (zerop elt))
                  (emit `(lap:mov64 (:stack ,i) nil)))))
        (emit-gc-info :incoming-arguments :rcx)
        (dolist (inst-or-label (mezzano.compiler.backend::backend-function-code backend-function))
          (cond ((symbolp inst-or-label)
                 (push inst-or-label *emitted-lap*))
                (t
                 (emit `(:comment ,(format nil "~S"  inst-or-label)))
                 (when (not (eql inst-or-label (mezzano.compiler.backend::first-instruction backend-function)))
                   (if (eql (gethash inst-or-label mv-flow) :multiple)
                       (emit-gc-info :multiple-values 0)
                       (emit-gc-info)))
                 (emit-lap backend-function inst-or-label uses defs))))
        (reverse *emitted-lap*)))))

;; FIXME: Don't recompute contours for each save instruction.
(defmethod lap-prepass (backend-function (instruction save-multiple-instruction) uses defs)
  (let ((contours (mezzano.compiler.backend::dynamic-contours backend-function)))
    ;; Allocate dx-root & stack pointer save slots
    (let ((dx-root (allocate-stack-slots 1))
          (saved-stack-pointer (allocate-stack-slots 1 :livep nil)))
      (setf (gethash (save-multiple-context instruction) *saved-multiple-values*)
            (cons dx-root saved-stack-pointer))
      (dolist (region (gethash instruction contours))
        (when (typep region 'begin-nlx-instruction)
          (push dx-root (gethash region *dx-root-visibility*))
          (format t "Added dx root ~S for ~S to ~S / ~S~%" dx-root instruction region (nlx-context region)))))))

(defmethod emit-lap (backend-function (instruction argument-setup-instruction) uses defs)
  ;; Check the argument count.
  ;; TODO
  ;; Spill count/fref.
  (flet ((usedp (reg)
           (or (typep reg 'mezzano.compiler.backend::physical-register)
               (not (endp (gethash reg uses))))))
    (when (usedp (argument-setup-fref instruction))
      (emit `(lap:mov64 ,(effective-address (argument-setup-fref instruction)) :r13)))
    (when (usedp (argument-setup-count instruction))
      (emit `(lap:mov64 ,(effective-address (argument-setup-count instruction)) :rcx)))
    ;; Arguments are delivered in registers, and then on the caller's stack.
    ;; Stack arguments are currently copied from the caller's stack into the
    ;; callee's stack, however this could be avoided for required arguments
    ;; via the incoming-arguments gc mechanism. This does not apply to optional
    ;; arguments as they may or may not exist on the caller's stack.
    (let ((stack-argument-index 0))
      ;; Stack &required args.
      (loop
         for req in (argument-setup-required instruction)
         do (when (and (typep req 'virtual-register)
                       (usedp req))
              (emit `(lap:mov64 :r13 (:rbp ,(* (+ stack-argument-index 2) 8))))
              (emit `(lap:mov64 ,(effective-address req) :r13))
              (incf stack-argument-index)))
      ;; &optional processing.
      (loop
         for i from (length (argument-setup-required instruction))
         for opt in (argument-setup-optional instruction)
         do (when (usedp opt)
              (emit `(lap:cmp64 :rcx ,(ash i sys.int::+n-fixnum-bits+))
              (cond ((typep opt 'virtual-register)
                     ;; Load from stack.
                     (emit `(lap:mov64 :r13 nil)
                           `(lap:cmov64ge :r13 (:rbp ,(* (+ stack-argument-index 2) 8)))
                           `(lap:mov64 ,(effective-address opt) :r13))
                     (incf stack-argument-index))
                    (t
                     ;; Load into register.
                     (emit `(lap:cmov64ge ,opt (:constant nil)))))))))
    ;; &rest generation.
    (when (and (argument-setup-rest instruction)
               (usedp (argument-setup-rest instruction)))
      ;; Only emit when used.
      (emit-dx-rest-list instruction)
      (emit `(lap:mov64 ,(effective-address (argument-setup-rest instruction)) :r13)))))

(defmethod emit-lap (backend-function (instruction move-instruction) uses defs)
  (emit `(lap:mov64 ,(move-destination instruction) ,(move-source instruction))))

(defmethod emit-lap (backend-function (instruction spill-instruction) uses defs)
  (emit `(lap:mov64 ,(effective-address (spill-destination instruction)) ,(spill-source instruction))))

(defmethod emit-lap (backend-function (instruction fill-instruction) uses defs)
  (emit `(lap:mov64 ,(fill-destination instruction) ,(effective-address (fill-source instruction)))))

(defmethod emit-lap (backend-function (instruction x86-instruction) uses defs)
  (emit (list* (x86-instruction-opcode instruction)
               (x86-instruction-operands instruction))))

(defmethod emit-lap (backend-function (instruction x86-branch-instruction) uses defs)
  (emit (list (x86-instruction-opcode instruction)
              (x86-branch-target instruction))))

(defmethod emit-lap (backend-function (instruction constant-instruction) uses defs)
  (let ((value (constant-value instruction))
        (dest (constant-destination instruction)))
    (cond ((eql value 0)
           (emit `(lap:xor64 ,dest ,dest)))
          ((eql value 'nil)
           (emit `(lap:mov64 ,dest nil)))
          ((eql value 't)
           (emit `(lap:mov64 ,dest t)))
          ((sys.c::fixnump value)
           (emit `(lap:mov64 ,dest ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw value))))
          ((characterp value)
           (emit `(lap:mov64 ,dest ,(mezzano.compiler.codegen.x86-64::character-to-raw value))))
          (t
           (emit `(lap:mov64 ,dest (:constant ,value)))))))

(defmethod emit-lap (backend-function (instruction return-multiple-instruction) uses defs)
  (emit `(lap:leave)
        ;; Don't use emit-gc-info, using a custom layout.
        `(:gc :no-frame :layout #*0 :multiple-values 0)
        `(lap:ret)))

(defmethod emit-lap (backend-function (instruction jump-instruction) uses defs)
  (emit `(lap:jmp ,(jump-target instruction))))

(defmethod emit-lap (backend-function (instruction branch-true-instruction) uses defs)
  (emit `(lap:cmp64 ,(branch-value instruction) nil)
        `(lap:jne ,(branch-target instruction))))

(defmethod emit-lap (backend-function (instruction branch-false-instruction) uses defs)
  (emit `(lap:cmp64 ,(branch-value instruction) nil)
        `(lap:je ,(branch-target instruction))))

(defun call-argument-setup (call-arguments)
  (let* ((stack-args (nthcdr 5 call-arguments))
         (n-stack-args (length stack-args))
         (n-args (length call-arguments)))
    (when (oddp n-stack-args)
      (incf n-stack-args))
    (when (not (zerop n-stack-args))
      (emit `(lap:sub64 :rsp ,(* n-stack-args 8))))
    (loop
       for arg in stack-args
       for i from 0
       do
         (emit `(lap:mov64 :r13 ,(effective-address arg))
               `(lap:mov64 (:rsp ,(* i 8)) :r13))
         (emit-gc-info :pushed-values (1+ i)))
    (typecase n-args
      ((eql 0)
       (emit `(lap:xor32 :ecx :ecx)))
      ((unsigned-byte 30)
       (emit `(lap:mov32 :ecx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw (length call-arguments)))))
      (t
       (emit `(lap:mov64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw (length call-arguments))))))))

(defun call-argument-teardown (call-arguments)
  (let* ((stack-args (nthcdr 5 call-arguments))
         (n-stack-args (length stack-args)))
    (when (oddp n-stack-args)
      (incf n-stack-args))
    (when (not (zerop n-stack-args))
      (emit `(lap:add64 :rsp ,(* n-stack-args 8))))))

(defmethod emit-lap (backend-function (instruction call-instruction) uses defs)
  (call-argument-setup (call-arguments instruction))
  (emit `(lap:mov64 :r13 (:function ,(call-function instruction)))
        `(lap:call (:object :r13 ,sys.int::+fref-entry-point+)))
  (call-argument-teardown (call-arguments instruction)))

(defmethod emit-lap (backend-function (instruction call-multiple-instruction) uses defs)
  (call-argument-setup (call-arguments instruction))
  (emit `(lap:mov64 :r13 (:function ,(call-function instruction)))
        `(lap:call (:object :r13 ,sys.int::+fref-entry-point+)))
  (emit-gc-info :multiple-values 0)
  (call-argument-teardown (call-arguments instruction)))

(defmethod lap-prepass (backend-function (instruction multiple-value-funcall-multiple-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 1 :livep nil)))

(defmethod emit-lap (backend-function (instruction multiple-value-funcall-multiple-instruction) uses defs)
  (let ((stack-pointer-save-area (gethash instruction *prepass-data*)))
    (emit `(lap:mov64 (:stack ,stack-pointer-save-area) :rsp))
    ;; Copy values in the sg-mv area to the stack. RCX holds the number of values to copy +5
    (let ((loop-head (gensym))
          (loop-exit (gensym))
          (clear-loop-head (gensym)))
      ;; RAX = n values to copy (count * 8).
      (emit `(lap:lea64 :rax ((:rcx ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+))) ,(- (* 5 8))))
            `(lap:cmp64 :rax 0)
            `(lap:jle ,loop-exit)
            `(lap:sub64 :rsp :rax)
            `(lap:and64 :rsp ,(lognot 8))
            ;; Clear stack slots.
            `(lap:mov64 :rdx :rax)
            clear-loop-head
            `(lap:mov64 (:rsp :rdx -8) 0)
            `(lap:sub64 :rdx 8)
            `(lap:jnz ,clear-loop-head)
            ;; Copy values.
            `(lap:mov64 :rdi :rsp)
            `(lap:mov32 :esi ,(+ (- 8 sys.int::+tag-object+)
                                         ;; fixme. should be +thread-mv-slots-start+
                                         (* #+(or)sys.int::+stack-group-offset-mv-slots+ 32 8))))
      ;; Switch to the right GC mode.
      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx :multiple-values 0)
      (emit loop-head
            `(lap:gs)
            `(lap:mov64 :r13 (:rsi))
            `(lap:mov64 (:rdi) :r13)
            `(lap:add64 :rdi 8)
            `(lap:add64 :rsi 8)
            `(lap:sub64 :rax 8)
            `(lap:jae ,loop-head)
            loop-exit)
      ;; All done with the MV area.
      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx))
    (emit `(lap:call (:object :rbx ,sys.int::+function-entry-point+)))
    ;; Restore the stack pointer.
    ;; No special NLX handling required as non-local exits already restore
    ;; the stack pointer.
    (emit `(lap:mov64 :rsp (:stack ,stack-pointer-save-area)))))

(defmethod lap-prepass (backend-function (instruction begin-nlx-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 4 :livep nil)))

(defmethod emit-lap (backend-function (instruction begin-nlx-instruction) uses defs)
  (let ((control-info (gethash instruction *prepass-data*))
        (jump-table (gensym))
        (over (gensym)))
    (emit `(lap:lea64 :rax (:rip ,jump-table))
          `(lap:mov64 (:stack ,(+ control-info 3)) :rax)
          `(lap:gs)
          `(lap:mov64 :rax (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+))
          `(lap:mov64 (:stack ,(+ control-info 2)) :rax)
          `(lap:mov64 (:stack ,(+ control-info 1)) :rsp)
          `(lap:mov64 (:stack ,(+ control-info 0)) :rbp)
          `(lap:lea64 ,(nlx-context instruction) (:stack ,(+ control-info 3))))
        ;; FIXME: Emit jump table as trailer.
    (emit `(lap:jmp ,over)
          jump-table)
    (dolist (target (begin-nlx-targets instruction))
      (emit `(:d64/le (- ,target ,jump-table))))
    (emit over)))

(defmethod emit-lap (backend-function (instruction finish-nlx-instruction) uses defs)
  )

(defun emit-nlx-entry (region multiple-values-p)
  (if multiple-values-p
      (emit-gc-info :block-or-tagbody-thunk :rax :multiple-values 0)
      (emit-gc-info :block-or-tagbody-thunk :rax))
  (emit `(lap:mov64 :rsp (:rax 16))
        `(lap:mov64 :rbp (:rax 24)))
  ;; Flush any dx roots that were invalidated by this exit.
  (dolist (dx-root (gethash region *dx-root-visibility*))
    (emit `(lap:mov64 (:stack ,dx-root) nil))))

(defmethod emit-lap (backend-function (instruction nlx-entry-instruction) uses defs)
  (emit-nlx-entry (nlx-region instruction) nil))

(defmethod emit-lap (backend-function (instruction nlx-entry-multiple-instruction) uses defs)
  (emit-nlx-entry (nlx-region instruction) t))

(defmethod emit-lap (backend-function (instruction save-multiple-instruction) uses defs)
  (let* ((save-data (gethash (save-multiple-context instruction) *saved-multiple-values*))
         (sv-save-area (car save-data))
         (saved-stack-pointer (cdr save-data))
         (save-done (gensym "VALUES-SAVE-DONE"))
         (save-loop-head (gensym "VALUES-SAVE-LOOP")))
    ;; Allocate an appropriately sized DX simple vector.
    ;; Add one for the header, then round the count up to an even number.
    (emit `(lap:lea64 :rax (:rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 2))))
    (emit `(lap:and64 :rax ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw (lognot 1))))
    ;; Save RSP.
    (emit `(lap:mov64 (:stack ,saved-stack-pointer) :rsp))
    ;; Adjust RSP. rax to raw * 8.
    (emit `(lap:shl64 :rax ,(- 3 sys.int::+n-fixnum-bits+)))
    (emit `(lap:sub64 :rsp :rax))
    ;; Write the simple-vector header.
    (emit `(lap:mov64 :rax :rcx))
    (emit `(lap:shl64 :rax ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
    (emit `(lap:mov64 (:rsp) :rax))
    ;; Clear the SV body. Don't modify RCX, needed for MV GC info.
    (let ((clear-loop-head (gensym "MVP1-CLEAR-LOOP"))
          (clear-loop-end (gensym "MVP1-CLEAR-LOOP-END")))
      (emit `(lap:mov64 :rdx :rcx))
      (emit `(lap:test64 :rdx :rdx))
      (emit `(lap:jz ,clear-loop-end))
      (emit `(lap:lea64 :rdi (:rsp 8)))
      (emit `(lap:xor32 :eax :eax))
      (emit clear-loop-head)
      (emit `(lap:stos64))
      (emit `(lap:sub64 :rdx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1)))
      (emit `(lap:jnz ,clear-loop-head))
      (emit clear-loop-end))
    ;; Create & save the DX root value.
    (emit `(lap:lea64 :rax (:rsp ,sys.int::+tag-dx-root-object+)))
    (emit `(lap:mov64 (:stack ,sv-save-area) :rax))
    ;; Save MV registers.
    (loop
       for reg in '(:r8 :r9 :r10 :r11 :r12)
       for offset from 0
       do
         (emit `(lap:cmp64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw offset)))
         (emit `(lap:jle ,save-done))
       ;; 1+ to skip header.
         (emit `(lap:mov64 (:rsp ,(* (1+ offset) 8)) ,reg)))
    ;; Save values in the MV area.
    ;; Number of values remaining.
    (emit `(lap:mov64 :rax :rcx))
    (emit `(lap:sub64 :rax ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 5)))
    (emit `(lap:jle ,save-done))
    ;; Save into the simple-vector.
    (emit `(lap:lea64 :rdi (:rsp ,(* 6 8)))) ; skip header and registers.
    ;; Load from the MV area.
    (emit `(lap:mov64 :rsi ,(+ (- 8 sys.int::+tag-object+)
                                       ;; fixme. should be +thread-mv-slots-start+
                                       (* #+(or)sys.int::+stack-group-offset-mv-slots+ 32 8))))
    ;; Save the values into a simple-vector.
    (emit save-loop-head)
    (emit `(lap:gs))
    (emit `(lap:mov64 :rbx (:rsi)))
    (emit `(lap:mov64 (:rdi) :rbx))
    (emit `(lap:add64 :rsi 8))
    (emit `(lap:add64 :rdi 8))
    (emit `(lap:sub64 :rax ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1)))
    (emit `(lap:jnz ,save-loop-head))
    ;; Finished saving values.
    (emit save-done)))

(defmethod emit-lap (backend-function (instruction restore-multiple-instruction) uses defs)
  (let* ((save-data (gethash (restore-multiple-context instruction) *saved-multiple-values*))
         (sv-save-area (car save-data))
         (saved-stack-pointer (cdr save-data)))
    ;; Create a normal object from the saved dx root.
    (emit `(lap:mov64 :rax (:stack ,sv-save-area)))
    (emit `(lap:lea64 :r8 (:rax ,(- sys.int::+tag-object+
                                            sys.int::+tag-dx-root-object+))))
    ;; Call helper.
    (emit `(lap:mov32 :ecx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1)))
    (emit `(lap:mov64 :r13 (:function sys.int::values-simple-vector)))
    (emit `(lap:call (:object :r13 ,sys.int::+fref-entry-point+)))
    (emit-gc-info :multiple-values 0)
    ;; Kill the dx root and restore the old stack pointer.
    (emit `(lap:mov64 (:stack ,sv-save-area) nil))
    (emit `(lap:mov64 :rsp (:stack ,saved-stack-pointer)))))

(defmethod emit-lap (backend-function (instruction forget-multiple-instruction) uses defs)
  (let* ((save-data (gethash (forget-multiple-context instruction) *saved-multiple-values*))
         (sv-save-area (car save-data))
         (saved-stack-pointer (cdr save-data)))
    ;; Kill the dx root and restore the old stack pointer.
    (emit `(lap:mov64 (:stack ,sv-save-area) nil))
    (emit `(lap:mov64 :rsp (:stack ,saved-stack-pointer)))))

(defmethod lap-prepass (backend-function (instruction push-special-stack-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 4)))

(defmethod emit-lap (backend-function (instruction push-special-stack-instruction) uses defs)
  (let ((slots (gethash instruction *prepass-data*)))
    ;; Flush slots.
    (emit `(lap:mov64 (:stack ,(+ slots 3)) ,(ash 3 sys.int::+object-data-shift+))
          `(lap:mov64 (:stack ,(+ slots 2)) nil)
          `(lap:mov64 (:stack ,(+ slots 1)) nil)
          `(lap:mov64 (:stack ,(+ slots 0)) nil))
    ;; Store bits.
    (emit `(lap:mov64 (:stack ,(+ slots 1)) ,(push-special-stack-a-value instruction))
          `(lap:mov64 (:stack ,(+ slots 0)) ,(push-special-stack-b-value instruction)))
    ;; Store link.
    (emit `(lap:gs)
          `(lap:mov64 :r13 (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+))
          `(lap:mov64 (:stack ,(+ slots 2)) :r13))
    ;; Generate pointer.
    (emit `(lap:lea64 :r13 (:rbp ,(+ (- (* (1+ (+ slots 3))))
                                     sys.int::+tag-object+))))
    ;; Push.
    (emit `(lap:gs)
          `(lap:mov64 (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+) :r13))))

(defmethod emit-lap (backend-function (instruction flush-binding-cache-entry-instruction) uses defs)
  (emit `(lap:mov64 :rax ,(flush-binding-cache-entry-symbol instruction))
        `(sys.lap-x86:shr32 :eax 4)
        `(sys.lap-x86:and32 :eax ,(1- 128)))
  ;; Store the new binding stack entry into the cache entry.
  ;; FIXME: Don't hard-code the register!
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (:object nil 128 :rax) :r13)))

(defmethod emit-lap (backend-function (instruction unbind-instruction) uses defs)
  ;; Top entry in the binding stack is a special variable binding.
  ;; It's a symbol and the current value.
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :rbx (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+)))
  ;; Pop the stack.
  (emit `(sys.lap-x86:mov64 :r13 (:object :rbx 0))
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+) :r13))
  ;; Recompute the symbol hash.
  (emit `(sys.lap-x86:mov64 :rax (:object :rbx ,sys.int::+symbol-value-cell-symbol+))
        `(sys.lap-x86:shr32 :eax 4)
        `(sys.lap-x86:and32 :eax ,(1- 128)))
  ;; Flush the binding cell cache for this entry.
  (let ((after-flush (gensym)))
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:cmp64 (:object nil 128 :rax) :rbx))
    (emit `(sys.lap-x86:jne ,after-flush))
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 (:object nil 128 :rax) 0))
    (emit after-flush)))

(defmethod emit-lap (backend-function (instruction disestablish-block-or-tagbody-instruction) uses defs)
  ;; Top entry in the binding stack is a block or tagbody entry.
  ;; It's a environment simple-vector & an offset.
  ;; Pop the stack & set env[offset] = NIL.
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :rbx (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+))
        `(sys.lap-x86:mov64 :r13 (:object :rbx 1))
        `(sys.lap-x86:mov64 :rcx (:object :rbx 2))
        `(sys.lap-x86:mov64 (:object :r13 0 :rcx ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+))) nil)
        `(sys.lap-x86:mov64 :rbx (:object :rbx 0))
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+) :rbx)))

(defmethod emit-lap (backend-function (instruction disestablish-unwind-protect-instruction) uses defs)
  ;; Top entry in the binding stack is an unwind-protect entry.
  ;; It's a function and environment object.
  ;; Pop the stack & call the function with the environment object.
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :r8 (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+))
        `(sys.lap-x86:mov64 :r13 (:object :r8 1))
        `(sys.lap-x86:mov64 :rbx (:object :r8 2))
        `(sys.lap-x86:mov64 :r8 (:object :r8 0))
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,mezzano.compiler.codegen.x86-64::+binding-stack-gs-offset+) :r8)
        `(sys.lap-x86:xor32 :ecx :ecx)
        `(sys.lap-x86:call (:object :r13 0))))
