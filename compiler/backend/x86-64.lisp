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

(defmethod mezzano.compiler.backend::successors (function (instruction x86-branch-instruction))
  (list (first (mezzano.compiler.backend::skip-symbols (rest (member instruction (mezzano.compiler.backend::backend-function-code function)))))
        (first (mezzano.compiler.backend::skip-symbols (member (x86-branch-target instruction) (mezzano.compiler.backend::backend-function-code function))))))

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
        ;(format t "Out: ~S  defs: ~S  uses: ~S~%" out out-defs out-uses)
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
            ((and (typep inst 'fixnum-<-instruction)
                  (typep next-inst 'branch-instruction)
                  (consumed-by-p inst next-inst uses defs))
             ;; (branch (fixnum-< lhs rhs) target) => (cmp lhs rhs) (bcc target)
             (let* ((fixnum-<-inst inst)
                    (branch-inst next-inst)
                    (lhs (fixnum-<-lhs fixnum-<-inst))
                    (rhs (fixnum-<-rhs fixnum-<-inst))
                    (true-rhs (maybe-constant-operand rhs defs)))
               (mezzano.compiler.backend::insert-before
                backend-function fixnum-<-inst
                (make-instance 'x86-instruction
                               :opcode 'lap:cmp64
                               :operands (list lhs true-rhs)
                               :inputs (if (eql true-rhs rhs)
                                           (list lhs rhs)
                                           (list lhs))
                               :outputs '()))
               (mezzano.compiler.backend::insert-before
                backend-function fixnum-<-inst
                (make-instance 'x86-branch-instruction
                               :opcode (if (typep next-inst 'branch-true-instruction)
                                           'lap:jl
                                           'lap:jnl)
                               :target (branch-target next-inst)))
               ;; Point next to the instruction after the branch.
               (setf next-inst (mezzano.compiler.backend::next-instruction backend-function next-inst))
               ;; Remove the fixnum-< & branch.
               (mezzano.compiler.backend::remove-instruction backend-function fixnum-<-inst)
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
                   (mezzano.compiler.backend::remove-instruction backend-function inst)))))
            ((and (typep inst 'call-instruction)
                  (endp (call-arguments inst))
                  (eql (call-function inst) 'sys.int::read-frame-pointer))
             (let ((result (call-result inst)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:lea64
                               :operands (list result `((:rbp ,(ash 1 sys.int::+n-fixnum-bits+))))
                               :inputs (list)
                               :outputs (list result)))
               (mezzano.compiler.backend::remove-instruction backend-function inst)))))))

(defun peephole (backend-function)
  (do* ((inst (mezzano.compiler.backend::first-instruction backend-function) next-inst)
        (next-inst (mezzano.compiler.backend::next-instruction backend-function inst) (if inst (mezzano.compiler.backend::next-instruction backend-function inst))))
       ((null inst))
    (cond ((and (typep inst 'move-instruction)
                (eql (move-source inst) (move-destination inst)))
           ;; Delete (move foo foo)
           (mezzano.compiler.backend::remove-instruction backend-function inst))
          #+(or)
          ((and (typep inst 'move-instruction)
                (typep next-inst 'spill-instruction)
                (eql (move-destination inst) (spill-source next-inst)))
           ;; (move t reg) (spill vreg t) => (spill vreg reg)
           (setf (spill-source next-inst) (move-source inst))
           (mezzano.compiler.backend::remove-instruction backend-function inst))
          #+(or)
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
  (declare (ignore uses))
  (let ((layout (loop
                   for vreg being the hash-keys of defs using (hash-value vreg-defs)
                   when (not (and vreg-defs
                                  (endp (rest vreg-defs))
                                  (typep (first vreg-defs) 'save-multiple-instruction)))
                   collect vreg)))
    (when (argument-setup-rest (mezzano.compiler.backend::first-instruction backend-function))
      ;; Reserve slot 0 for the saved argument count. Required for &rest list generation.
      (push :raw layout))
    (make-array (length layout)
                :adjustable t
                :fill-pointer t
                :initial-contents layout)))

(defun allocate-stack-slots (count &key (livep t) aligned)
  (assert (not *current-frame-layout*) ()
          "Allocating stack slots after stack frame layout.")
  (when aligned
    (when (oddp count)
      (incf count))
    (when (oddp (length *stack-layout*))
      (vector-push-extend :raw *stack-layout*)))
  (prog1
      (length *stack-layout*)
    (dotimes (i count)
      (vector-push-extend (if livep :live :raw) *stack-layout*))))

(defun effective-address (vreg)
  (check-type vreg virtual-register)
  `(:stack ,(or (position vreg *stack-layout*)
                (error "Missing stack slot for vreg ~S" vreg))))

(defun to-lap (backend-function)
  (multiple-value-bind (uses defs)
      (mezzano.compiler.backend::build-use/def-maps backend-function)
    (let ((*stack-layout* (compute-stack-layout backend-function uses defs))
          (*saved-multiple-values* (make-hash-table))
          (*dx-root-visibility* (make-hash-table))
          (*prepass-data* (make-hash-table))
          (*current-frame-layout* nil))
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

(defmethod lap-prepass (backend-function (instruction argument-setup-instruction) uses defs)
  (when (argument-setup-rest instruction)
    (setf (gethash instruction *prepass-data*) (allocate-stack-slots 1))))

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
              (emit `(lap:cmp64 :rcx ,(ash i sys.int::+n-fixnum-bits+)))
              (cond ((typep opt 'virtual-register)
                     ;; Load from stack.
                     (emit `(lap:mov64 :r13 nil)
                           `(lap:cmov64nle :r13 (:rbp ,(* (+ stack-argument-index 2) 8)))
                           `(lap:mov64 ,(effective-address opt) :r13))
                     (incf stack-argument-index))
                    (t
                     ;; Load into register.
                     (emit `(lap:cmov64le ,opt (:constant nil))))))))
    ;; &rest generation.
    (when (and (argument-setup-rest instruction)
               (usedp (argument-setup-rest instruction)))
      ;; Only emit when used.
      (emit-dx-rest-list instruction)
      (emit `(lap:mov64 ,(effective-address (argument-setup-rest instruction)) :r13)))))

(defun emit-dx-rest-list (argument-setup)
  (let* ((regular-argument-count (+ (length (argument-setup-required argument-setup))
                                    (length (argument-setup-optional argument-setup))))
         (rest-clear-loop-head (gensym "REST-CLEAR-LOOP-HEAD"))
         (rest-loop-head (gensym "REST-LOOP-HEAD"))
         (rest-loop-end (gensym "REST-LOOP-END"))
         (rest-list-done (gensym "REST-LIST-DONE"))
         ;; Number of arguments processed and total number of arguments.
         (saved-argument-count 0)
         (rest-dx-root (gethash argument-setup *prepass-data*)))
    ;; Assemble the rest list into R13.
    ;; RCX holds the argument count.
    ;; R13 is free. Argument registers may or may not be free
    ;; depending on the number of required/optional arguments.
    ;; Number of supplied arguments.
    (emit `(sys.lap-x86:mov64 (:stack ,saved-argument-count) :rcx))
    ;; Tell the GC to used the number of arguments saved on the stack. RCX will
    ;; be used later.
    (emit-gc-info :incoming-arguments saved-argument-count)
    ;; The cons cells are allocated in one single chunk.
    (emit `(sys.lap-x86:mov64 :r13 nil))
    ;; Remove required/optional arguments from the count.
    ;; If negative or zero, the &REST list is empty.
    (cond ((zerop regular-argument-count)
           (emit `(sys.lap-x86:test64 :rcx :rcx))
           (emit `(sys.lap-x86:jz ,rest-list-done)))
          (t
           (emit `(sys.lap-x86:sub64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw regular-argument-count)))
           (emit `(sys.lap-x86:jle ,rest-list-done))))
    ;; Save the length.
    (emit `(sys.lap-x86:mov64 :rdx :rcx))
    ;; Double it, each cons takes two words.
    (emit `(sys.lap-x86:shl64 :rdx 1))
    ;; Add a header word and word of padding so it can be treated like a simple-vector.
    (emit `(sys.lap-x86:add64 :rdx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 2)))
    ;; Fixnum to raw integer * 8.
    (emit `(sys.lap-x86:shl64 :rdx ,(- 3 sys.int::+n-fixnum-bits+)))
    ;; Allocate on the stack.
    (emit `(sys.lap-x86:sub64 :rsp :rdx))
    ;; Generate the simple-vector header. simple-vector tag is zero, doesn't need to be set here.
    (emit `(sys.lap-x86:lea64 :rdx ((:rcx 2) ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1)))) ; *2 as conses are 2 words and +1 for padding word at the start.
    (emit `(sys.lap-x86:shl64 :rdx ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
    (emit `(sys.lap-x86:mov64 (:rsp) :rdx))
    ;; Clear the padding slot.
    (emit `(sys.lap-x86:mov64 (:rsp 8) 0))
    ;; For each cons, clear the car and set the cdr to the next cons.
    (emit `(sys.lap-x86:lea64 :rdi (:rsp 16)))
    (emit `(sys.lap-x86:mov64 :rdx :rcx))
    (emit rest-clear-loop-head)
    (emit `(sys.lap-x86:mov64 (:rdi 0) 0)) ; car
    (emit `(sys.lap-x86:lea64 :rax (:rdi ,(+ 16 sys.int::+tag-cons+))))
    (emit `(sys.lap-x86:mov64 (:rdi 8) :rax)) ; cdr
    (emit `(sys.lap-x86:add64 :rdi 16))
    (emit `(sys.lap-x86:sub64 :rdx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1)))
    (emit `(sys.lap-x86:ja ,rest-clear-loop-head))
    ;; Set the cdr of the final cons to NIL.
    (emit `(sys.lap-x86:mov64 (:rdi -8) nil))
    ;; Create the DX root object for the vector.
    (emit `(sys.lap-x86:lea64 :rax (:rsp ,sys.int::+tag-dx-root-object+)))
    (emit `(sys.lap-x86:mov64 (:stack ,rest-dx-root) :rax))
    ;; It's now safe to write values into the list/vector.
    (emit `(sys.lap-x86:lea64 :rdi (:rsp 16)))
    ;; Add register arguments to the list.
    (loop
       for reg in (nthcdr regular-argument-count '(:r8 :r9 :r10 :r11 :r12))
       do (emit `(sys.lap-x86:mov64 (:rdi) ,reg)
                `(sys.lap-x86:add64 :rdi 16)
                `(sys.lap-x86:sub64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1))
                `(sys.lap-x86:jz ,rest-loop-end)))
    ;; Now add the stack arguments.
    ;; Skip past required/optional arguments on the stack, the saved frame pointer and the return address.
    (emit `(sys.lap-x86:lea64 :rsi (:rbp ,(* (+ (max 0 (- regular-argument-count 5)) 2) 8))))
    (emit rest-loop-head)
    ;; Load from stack.
    (emit `(sys.lap-x86:mov64 :r13 (:rsi)))
    ;; Store into current car.
    (emit `(sys.lap-x86:mov64 (:rdi) :r13))
    ;; Advance.
    (emit `(sys.lap-x86:add64 :rsi 8))
    (emit `(sys.lap-x86:add64 :rdi 16))
    ;; Stop when no more arguments.
    (emit `(sys.lap-x86:sub64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1)))
    (emit `(sys.lap-x86:jnz ,rest-loop-head))
    (emit rest-loop-end)
    ;; There were &REST arguments, create the cons.
    (emit `(sys.lap-x86:lea64 :r13 (:rsp ,(+ 16 sys.int::+tag-cons+))))
    ;; Code above jumps directly here with NIL in R13 when there are no arguments.
    (emit rest-list-done)))

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
    (cond ((typep value 'backend-function)
           (emit `(lap:mov64 ,dest (:constant ,(compile-backend-function value)))))
          ((eql value 0)
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

(defmethod emit-lap (backend-function (instruction return-instruction) uses defs)
  (emit `(lap:mov32 :ecx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1))
        `(lap:leave)
        ;; Don't use emit-gc-info, using a custom layout.
        `(:gc :no-frame :layout #*0)
        `(lap:ret)))

(defmethod emit-lap (backend-function (instruction return-multiple-instruction) uses defs)
  (emit `(lap:leave)
        ;; Don't use emit-gc-info, using a custom layout.
        `(:gc :no-frame :layout #*0 :multiple-values 0)
        `(lap:ret)))

(defmethod emit-lap (backend-function (instruction jump-instruction) uses defs)
  (emit `(lap:jmp ,(jump-target instruction))))

(defmethod emit-lap (backend-function (instruction switch-instruction) uses defs)
  (let ((jump-table (gensym)))
    (emit `(lap:lea64 :rax (:rip ,jump-table))
          `(lap:add64 :rax (:rax (,(switch-value instruction) ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+)))))
          `(lap:jmp :rax))
    (emit jump-table)
    (loop
       for target in (switch-targets instruction)
       do (emit `(:d64/le (- ,target ,jump-table))))))

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

(defmethod emit-lap (backend-function (instruction funcall-instruction) uses defs)
  (call-argument-setup (call-arguments instruction))
  (emit `(lap:call (:object :rbx ,sys.int::+function-entry-point+)))
  (call-argument-teardown (call-arguments instruction)))

(defmethod emit-lap (backend-function (instruction funcall-multiple-instruction) uses defs)
  (call-argument-setup (call-arguments instruction))
  (emit `(lap:call (:object :rbx ,sys.int::+function-entry-point+)))
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
    (emit-gc-info :multiple-values 0)
    ;; Restore the stack pointer.
    ;; No special NLX handling required as non-local exits already restore
    ;; the stack pointer.
    (emit `(lap:mov64 :rsp (:stack ,stack-pointer-save-area)))))

(defmethod lap-prepass (backend-function (instruction multiple-value-funcall-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 1 :livep nil)))

(defmethod emit-lap (backend-function (instruction multiple-value-funcall-instruction) uses defs)
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
    (emit-gc-info)
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

(defun emit-invoke-nlx (instruction)
  (emit `(lap:mov64 :rax ,(nlx-context instruction))
        `(lap:mov64 :rdx (:rax 0))
        `(lap:add64 :rdx (:rdx ,(* (invoke-nlx-index instruction) 8)))
        `(lap:jmp :rdx)))

(defmethod emit-lap (backend-function (instruction invoke-nlx-instruction) uses defs)
  (emit-invoke-nlx instruction))

(defmethod emit-lap (backend-function (instruction invoke-nlx-multiple-instruction) uses defs)
  (emit-invoke-nlx instruction))

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
          (push dx-root (gethash region *dx-root-visibility*)))))))

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

(defmethod emit-lap (backend-function (instruction multiple-value-bind-instruction) uses defs)
  (loop
     with regs = '(:r8 :r9 :r10 :r11 :r12)
     for i from 0
     for value in (multiple-value-bind-values instruction)
     do
       (cond (regs
              (let ((reg (pop regs)))
                (emit `(lap:cmp64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw i))
                      `(lap:cmov64le ,reg (:constant nil)))))
             (t
              (emit `(lap:mov64 :r13 nil)
                    `(lap:cmp64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw i))
                    `(lap:gs)
                    `(lap:cmov64nle :r13 (,(+ (- 8 sys.int::+tag-object+)
                                             (* (+ #+(or)sys.int::+stack-group-offset-mv-slots+
                                                   32 ; fixme. should be +thread-mv-slots-start+
                                                   (- i 5))
                                                8))))
                    `(lap:mov64 ,(effective-address value) :r13))))))

(defmethod emit-lap (backend-function (instruction values-instruction) uses defs)
  (cond ((endp (values-values instruction))
         (emit `(lap:mov64 :r8 nil)
               `(lap:xor32 :ecx :ecx)))
        (t
         (emit `(lap:mov64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw (min 5 (length (values-values instruction))))))
         (loop
            for value in (nthcdr 5 (values-values instruction))
            for i from 0
            do
              (emit `(lap:mov64 :r13 ,(effective-address value))
                    `(lap:gs)
                    `(lap:mov64 (,(+ (- 8 sys.int::+tag-object+)
                                     ;; fixme. should be +thread-mv-slots-start+
                                     (* #+(or)sys.int::+stack-group-offset-mv-slots+ 32 8)
                                     (* i 8)))
                                :r13))
              (emit-gc-info :multiple-values 1)
              (emit `(lap:add64 :rcx ,(mezzano.compiler.codegen.x86-64::fixnum-to-raw 1)))
              (emit-gc-info :multiple-values 0)))))

(defun reify-condition (result-reg true-cmov-inst)
  (emit `(lap:mov64 ,result-reg nil)
        `(,true-cmov-inst ,result-reg (:constant t))))

(defmethod emit-lap (backend-function (instruction eq-instruction) uses defs)
  (emit `(lap:cmp64 ,(eq-lhs instruction) ,(eq-rhs instruction)))
  (reify-condition (eq-result instruction) 'lap:cmov64e))

(defmethod emit-lap (backend-function (instruction fixnum-<-instruction) uses defs)
  (emit `(lap:cmp64 ,(fixnum-<-lhs instruction) ,(fixnum-<-rhs instruction)))
  (reify-condition (fixnum-<-result instruction) 'lap:cmov64l))

(defmethod emit-lap (backend-function (instruction object-get-t-instruction) uses defs)
  (emit `(lap:mov64 ,(object-get-destination instruction)
                    (:object ,(object-get-object instruction)
                             0
                             ,(object-get-index instruction)
                             4))))

(defmethod emit-lap (backend-function (instruction object-set-t-instruction) uses defs)
  (emit `(lap:mov64 (:object ,(object-set-object instruction)
                             0
                             ,(object-set-index instruction)
                             4)
                    ,(object-set-value instruction))))

(defmethod lap-prepass (backend-function (instruction push-special-stack-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 4 :aligned t)))

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
    (emit `(lap:lea64 :r13 (:rbp ,(+ (- (* (1+ (+ slots 3)) 8))
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

(defmethod lap-prepass (backend-function (instruction make-dx-simple-vector-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots (1+ (make-dx-simple-vector-size instruction)) :aligned t)))

(defmethod emit-lap (backend-function (instruction make-dx-simple-vector-instruction) uses defs)
  (let* ((slots (gethash instruction *prepass-data*))
         (size (make-dx-simple-vector-size instruction))
         (words (1+ size)))
    (when (oddp words)
      (incf words))
    ;; Initialize the header.
    (emit `(lap:mov64 (:stack ,(+ slots words -1)) ,(ash size sys.int::+object-data-shift+)))
    ;; Generate pointer.
    (emit `(lap:lea64 ,(make-dx-simple-vector-result instruction) (:rbp ,(+ (- (* (1+ (+ slots words -1)) 8))
                                                                            sys.int::+tag-object+))))))

(defmethod lap-prepass (backend-function (instruction make-dx-closure-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 4 :aligned t)))

(defmethod emit-lap (backend-function (instruction make-dx-closure-instruction) uses defs)
  (let ((slots (gethash instruction *prepass-data*)))
    (emit `(lap:lea64 :rax (:stack ,(+ slots 4 -1)))
          ;; Closure tag and size.
          `(lap:mov32 (:rax) ,(logior (ash 3 sys.int::+object-data-shift+)
                                      (ash sys.int::+object-tag-closure+
                                           sys.int::+object-type-shift+)))
          ;; Constant pool size and slot count.
          `(lap:mov32 (:rax 4) #x00000002)
          ;; Entry point is CODE's entry point.
          `(lap:mov64 :rcx (:object ,(make-dx-closure-function instruction) 0))
          `(lap:mov64 (:rax 8) :rcx)
          ;; Clear constant pool.
          `(lap:mov64 (:rax 16) nil)
          `(lap:mov64 (:rax 24) nil))
    (emit `(lap:lea64 ,(make-dx-closure-result instruction) (:rax ,sys.int::+tag-object+)))
    ;; Initiaize constant pool.
    (emit `(lap:mov64 (:object ,(make-dx-closure-result instruction) 1) ,(make-dx-closure-function instruction))
          `(lap:mov64 (:object ,(make-dx-closure-result instruction) 2) ,(make-dx-closure-environment instruction)))))

(defun compile-backend-function-1 (backend-function)
  (mezzano.compiler.backend::canonicalize-call-operands backend-function)
  (mezzano.compiler.backend::canonicalize-argument-setup backend-function)
  (mezzano.compiler.backend::canonicalize-nlx-values backend-function)
  (mezzano.compiler.backend::canonicalize-values backend-function)
  (mezzano.compiler.backend.x86-64::lower backend-function)
  (mezzano.compiler.backend::remove-unused-instructions backend-function)
  (multiple-value-bind (live-in live-out)
      (mezzano.compiler.backend::compute-liveness backend-function)
    (let ((order (mezzano.compiler.backend::instructions-reverse-postorder backend-function))
          (arch :x86-64))
      (multiple-value-bind (registers spilled instantaneous-registers)
          (mezzano.compiler.backend::linear-scan-allocate
           backend-function
           order
           (mezzano.compiler.backend::build-live-ranges backend-function order arch live-in live-out)
           arch
           live-in live-out)
        (mezzano.compiler.backend::rewrite-after-allocation backend-function registers spilled instantaneous-registers))))
  (mezzano.compiler.backend.x86-64::peephole backend-function))

(defun compile-backend-function-2 (backend-function)
  (sys.int::assemble-lap
   (to-lap backend-function)
   (backend-function-name backend-function)
   nil
   nil
   :x86-64))

(defun compile-backend-function (backend-function)
  (compile-backend-function-1 backend-function)
  (compile-backend-function-2 backend-function))
