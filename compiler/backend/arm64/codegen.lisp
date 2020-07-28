;;;; ARM64 code generation

(in-package :mezzano.compiler.backend.arm64)

(defvar *emitted-lap*)
(defvar *stack-layout*)
(defvar *spill-locations*)
(defvar *saved-multiple-values*)
(defvar *dx-root-visibility*)
(defvar *current-frame-layout*)
(defvar *prepass-data*)
(defvar *labels*)
(defvar *literals*)
(defvar *literals/128*)

(defun resolve-label (label)
  (or (gethash label *labels*)
      (error "Unknown label ~S" label)))

(defun emit (&rest instructions)
  (dolist (i instructions)
    (push i *emitted-lap*)))

(defun emit-debug-info (info spill-locations)
  (emit `(:debug ,(loop
                     for (variable location repr) in info
                     collect (list* (c:lexical-variable-name variable)
                                    (if (typep location 'ir:virtual-register)
                                        (or (gethash location spill-locations)
                                            (error "Missing stack slot for spilled virtual ~S" location))
                                        location)
                                    repr
                                    (if (getf (c:lexical-variable-plist variable)
                                              'c::hide-from-debug-info)
                                        (list :hidden t)
                                        ()))))))

(defun emit-gc-info (&rest metadata)
  (emit `(:gc :frame :layout ,*current-frame-layout* ,@metadata)))

(defgeneric lap-prepass (backend-function instruction uses defs)
  (:method (backend-function instruction uses defs) nil))
(defgeneric emit-lap (backend-function instruction uses defs))

(defvar *target*)

;; TODO: Sort the layout so stack slots for values are all together and trim
;; the gc layout bitvector. #*111 instead of #*0010101
(defun compute-stack-layout (backend-function spill-locations stack-layout)
  (when (ir:argument-setup-rest (ir:first-instruction backend-function))
    ;; Reserve slot 0 for the saved argument count. Required for &rest list generation.
    (loop
       for vreg being the hash-keys of spill-locations
       do (incf (gethash vreg spill-locations)))
    (let ((tmp (make-array (1+ (length stack-layout)) :adjustable t :fill-pointer t)))
      (replace tmp stack-layout :start1 1)
      (setf (aref tmp 0) :raw)
      (setf stack-layout tmp)))
  (let ((environment-slot nil))
    (when (c:lambda-information-environment-layout
           (ir::ast backend-function))
      (setf environment-slot (c:lexical-variable-name
                              (first (c:lambda-information-environment-layout
                                      (ir::ast backend-function))))))
    (values stack-layout
            spill-locations
            environment-slot)))

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
      (vector-push-extend (if livep :value :raw) *stack-layout*))))

(defun vreg-stack-slot (vreg)
  (check-type vreg ir:virtual-register)
  (or (gethash vreg *spill-locations*)
      (error "Missing stack slot for vreg ~S" vreg)))

(defun fetch-literal (value)
  (let ((pos (or (position value *literals*)
                 (vector-push-extend value *literals*))))
    `(:pc (+ literal-pool ,(* pos 8)))))

(defun code-for-load-literal (dest value)
  (cond ((keywordp value)
         `(lap:ldr ,dest ,(fetch-literal value)))
        ((zerop value)
         `(lap:orr ,dest :xzr :xzr))
        ((<= 0 value 65535)
         `(lap:movz ,dest ,value))
        (t
         `(lap:ldr ,dest ,(fetch-literal value)))))

(defun load-literal (dest value)
  (emit (code-for-load-literal dest value)))

(defun fetch-literal/128 (value)
  (let ((pos (or (position value *literals/128*)
                 (vector-push-extend value *literals/128*))))
    `(:pc (+ literal-pool/128 ,(* pos 16)))))

(defun load-literal/128 (dest value)
  (cond ((keywordp value)
         (emit `(lap:ldr ,dest ,(fetch-literal/128 value))))
        ((zerop value)
         (list `(lap:orr ,dest :xzr :xzr)))
        ((<= 0 value 65535)
         (list `(lap:movz ,dest ,value)))
        (t
         (emit `(lap:ldr ,dest ,(fetch-literal/128 value))))))

(defun control-stack-frame-offset (slot)
  "Convert a control stack slot number to an offset."
  (- (* (1+ slot) 8)))

(defun code-for-reg-immediate-mem-op (inst reg base offset &optional temp)
  (when (not temp)
    (setf temp :x12))
  (cond ((or (<= -256 offset 255)
             (and (<= 0 offset 16380)
                  (zerop (logand offset #b111))))
         (list `(,inst ,reg (,base ,offset))))
        (t
         (append (list (code-for-load-literal temp offset))
                 (list `(,inst ,reg (,base ,temp)))))))

(defun object-slot-displacement (slot &optional scale)
  (+ (- sys.int::+tag-object+) 8 (* slot (or scale 8))))

(defun emit-object-op (inst reg base slot &optional scale)
  (apply #'emit (code-for-reg-immediate-mem-op
                 inst reg base (object-slot-displacement slot scale))))

(defun emit-object-load (reg base &key (slot 0))
  (emit-object-op 'lap:ldr reg base slot))

(defun emit-object-store (reg base &key (slot 0))
  (emit-object-op 'lap:str reg base slot))

(defun code-for-stack-op (inst reg slot &optional temp)
  (code-for-reg-immediate-mem-op inst reg :x29 (control-stack-frame-offset slot) temp))

(defun emit-stack-op (inst reg slot &optional temp)
  (apply #'emit (code-for-stack-op inst reg slot temp)))

(defun emit-stack-load (reg slot &optional temp)
  (emit-stack-op 'lap:ldr reg slot temp))

(defun emit-stack-store (reg slot &optional temp)
  (emit-stack-op 'lap:str reg slot temp))

(defmethod ir:perform-target-lap-generation (backend-function debug-map spill-locations stack-layout (*target* c:arm64-target))
  (multiple-value-bind (uses defs)
      (ir::build-use/def-maps backend-function)
    (multiple-value-bind (*stack-layout* *spill-locations* environment-slot)
        (compute-stack-layout backend-function spill-locations stack-layout)
      (let ((*saved-multiple-values* (make-hash-table :test 'eq))
            (*dx-root-visibility* (make-hash-table :test 'eq))
            (*prepass-data* (make-hash-table :test 'eq))
            (*current-frame-layout* nil)
            (*labels* (make-hash-table :test 'eq)))
        (ir:do-instructions (inst-or-label backend-function)
          (cond ((typep inst-or-label 'ir:label)
                 (setf (gethash inst-or-label *labels*) (mezzano.lap:make-label)))
                (t
                 (lap-prepass backend-function inst-or-label uses defs))))
        (let ((*emitted-lap* '())
              (*current-frame-layout* (coerce (loop
                                                 for elt across *stack-layout*
                                                 collect (if (not (eql elt :value))
                                                             0
                                                             1))
                                              'simple-bit-vector))
              (*literals* (make-array 8 :adjustable t :fill-pointer 0))
              (*literals/128* (make-array 8 :adjustable t :fill-pointer 0))
              (mv-flow (ir::multiple-value-flow backend-function *target*)))
          ;; Create stack frame.
          (emit 'entry-point
                `(:debug ())
                `(:gc :no-frame :incoming-arguments :rcx :layout #*)
                `(lap:stp :x29 :x30 (:pre :sp -16))
                `(:gc :no-frame :incoming-arguments :rcx :layout #*00)
                `(lap:add :x29 :sp :xzr)
                `(:gc :frame :incoming-arguments :rcx))
          (let ((stack-size (length *current-frame-layout*)))
            (when (oddp stack-size)
              (incf stack-size))
            (when (not (zerop stack-size))
              (cond ((> (* stack-size 8) 4095)
                     (load-literal :x9 (* stack-size 8))
                     (emit `(lap:sub :sp :sp :x9)))
                    (t
                     (emit `(lap:sub :sp :sp ,(* stack-size 8))))))
            (loop
               for i from 0
               for elt across *current-frame-layout*
               do (when (not (zerop elt))
                    (emit-stack-store :x26 i))))
          (emit-gc-info :incoming-arguments :rcx)
          (ir:do-instructions (inst-or-label backend-function)
            (emit-debug-info (gethash inst-or-label debug-map '()) *spill-locations*)
            (cond ((typep inst-or-label 'ir:label)
                   (push (gethash inst-or-label *labels*) *emitted-lap*))
                  (t
                   (when (not (eql inst-or-label (ir:first-instruction backend-function)))
                     (if (eql (gethash inst-or-label mv-flow) :multiple)
                         (emit-gc-info :multiple-values 0)
                         (emit-gc-info)))
                   (emit-lap backend-function inst-or-label uses defs))))
          (emit '(:align 16)
                'literal-pool/128)
          (loop for value across *literals/128* do
               (emit `(:d64/le ,(ldb (byte 64 0) value))
                     `(:d64/le ,(ldb (byte 64 64) value))))
          (emit 'literal-pool)
          (loop for value across *literals* do
               (emit `(:d64/le ,value)))
          (values (reverse *emitted-lap*)
                  environment-slot))))))

(defmethod lap-prepass (backend-function (instruction ir:argument-setup-instruction) uses defs)
  (when (ir:argument-setup-rest instruction)
    (setf (gethash instruction *prepass-data*) (allocate-stack-slots 1))))


;;  '((:eq  :ne lap:b.eq lap:csel.eq)
;;    (:ne  :eq lap:b.ne lap:csel.ne)
;;    (:cs  :cc lap:b.cs lap:csel.cs)
;;    (:cc  :cs lap:b.cc lap:csel.cc)
;;    (:mi  :pl lap:b.mi lap:csel.mi)
;;    (:pl  :mi lap:b.pl lap:csel.pl)
;;    (:vs  :vc lap:b.vs lap:csel.vs)
;;    (:vc  :vs lap:b.vc lap:csel.vc)
;;    (:hi  :ls lap:b.hi lap:csel.hi)
;;    (:ls  :hi lap:b.ls lap:csel.ls)
;;    (:ge  :lt lap:b.ge lap:csel.ge)
;;    (:lt  :ge lap:b.lt lap:csel.lt)
;;    (:gt  :le lap:b.gt lap:csel.gt)
;;    (:le  :ge lap:b.le lap:csel.le))
;; nl = ge
;; na = ls

(defmethod emit-lap (backend-function (instruction ir:argument-setup-instruction) uses defs)
  ;; Check the argument count.
  (let ((args-ok (gensym)))
    (flet ((emit-arg-error ()
             ;; If this is a closure, then it must have been invoked using
             ;; the closure calling convention and the closure object will
             ;; still be in RBX. For non-closures, reconstruct the function
             ;; object and put that in RBX.
             (when (not (c:lambda-information-environment-arg (mezzano.compiler.backend::ast backend-function)))
               (emit `(lap:adr :x6 (+ (- entry-point 16) ,sys.int::+tag-object+))))
             (emit `(lap:ldp :x29 :x30 (:post :sp 16))
                   `(:gc :no-frame :incoming-arguments :rcx :layout #*)
                   `(lap:named-tail-call sys.int::raise-invalid-argument-error)
                   args-ok)
             (emit-gc-info :incoming-arguments :rcx)))
      ;; FIXME: Support more than 2047 arguments (subs immediate limit).
      (cond ((ir:argument-setup-rest instruction)
             ;; If there are no required parameters, then don't generate a lower-bound check.
             (when (ir:argument-setup-required instruction)
               ;; Minimum number of arguments.
               (emit `(lap:subs :xzr :x5 ,(c::fixnum-to-raw
                                           (length (ir:argument-setup-required instruction))))
                     `(lap:b.ge ,args-ok))
               (emit-arg-error)))
            ((and (ir:argument-setup-required instruction)
                  (ir:argument-setup-optional instruction))
             ;; A range.
             (emit `(lap:sub :x9 :x5 ,(c::fixnum-to-raw
                                       (length (ir:argument-setup-required instruction))))
                   `(lap:subs :xzr :x9 ,(c::fixnum-to-raw
                                         (length (ir:argument-setup-optional instruction))))
                  `(lap:b.ls ,args-ok))
             (emit-arg-error))
            ((ir:argument-setup-optional instruction)
             ;; Maximum number of arguments.
             (emit `(lap:subs :xzr :x5 ,(c::fixnum-to-raw
                                         (length (ir:argument-setup-optional instruction))))
                   `(lap:b.ls ,args-ok))
             (emit-arg-error))
            ((ir:argument-setup-required instruction)
             ;; Exact number of arguments.
             (emit `(lap:subs :xzr :x5 ,(c::fixnum-to-raw
                                         (length (ir:argument-setup-required instruction))))
                   `(lap:b.eq ,args-ok))
             (emit-arg-error))
            ;; No arguments
            (t
             (emit `(lap:cbz :x5 ,args-ok))
             (emit-arg-error)))))
  ;; Spill count.
  (flet ((usedp (reg)
           (or (typep reg 'mezzano.compiler.backend.register-allocator::physical-register)
               (not (endp (gethash reg uses))))))
    (when (usedp (ir:argument-setup-count instruction))
      (emit-stack-store :x5 (vreg-stack-slot (ir:argument-setup-count instruction))))
    ;; Arguments are delivered in registers, and then on the caller's stack.
    ;; Stack arguments are currently copied from the caller's stack into the
    ;; callee's stack, however this could be avoided for required arguments
    ;; via the incoming-arguments gc mechanism. This does not apply to optional
    ;; arguments as they may or may not exist on the caller's stack.
    (let ((stack-argument-index 0))
      ;; Stack &required args.
      (loop
         for req in (ir:argument-setup-required instruction)
         do (when (typep req 'ir:virtual-register)
              (when (usedp req)
                (emit `(lap:ldr :x7 (:x29 ,(* (+ stack-argument-index 2) 8))))
                (emit-stack-store :x7 (vreg-stack-slot req)))
              (incf stack-argument-index)))
      ;; &optional processing.
      (loop
         for i from (length (ir:argument-setup-required instruction))
         for opt in (ir:argument-setup-optional instruction)
         do
           (when (usedp opt)
             (emit `(lap:subs :xzr :x5 ,(ash i sys.int::+n-fixnum-bits+))))
           (cond ((typep opt 'ir:virtual-register)
                  ;; Load from stack.
                  (when (usedp opt)
                    (let ((over (gensym "OVER")))
                      ;; Skip the load and store when not present, the stack slot will have been filled with NIL
                      ;; during frame init.
                      (emit `(lap:b.le ,over)
                            `(lap:ldr :x7 (:x29 ,(* (+ stack-argument-index 2) 8))))
                      (emit-stack-store :x7 (vreg-stack-slot opt))
                      (emit over)))
                  (incf stack-argument-index))
                 (t
                  ;; Load into register.
                  (emit `(lap:csel.le ,opt :x26 ,opt))))))
    ;; &rest generation.
    (when (and (ir:argument-setup-rest instruction)
               (usedp (ir:argument-setup-rest instruction)))
      ;; Only emit when used.
      (emit-dx-rest-list instruction)
      (emit-stack-store :x7 (vreg-stack-slot (ir:argument-setup-rest instruction))))))

(defun emit-dx-rest-list (argument-setup)
  (let* ((regular-argument-count (+ (length (ir:argument-setup-required argument-setup))
                                    (length (ir:argument-setup-optional argument-setup))))
         (rest-clear-loop-head (gensym "REST-CLEAR-LOOP-HEAD"))
         (rest-loop-head (gensym "REST-LOOP-HEAD"))
         (rest-loop-end (gensym "REST-LOOP-END"))
         (rest-list-done (gensym "REST-LIST-DONE"))
         ;; Number of arguments processed and total number of arguments.
         (saved-argument-count 0)
         (rest-dx-root (gethash argument-setup *prepass-data*)))
    ;; Assemble the rest list into X7.
    ;; X5 holds the argument count.
    ;; X6 and X7 are free. Argument registers may or may not be free
    ;; depending on the number of required/optional arguments.
    ;; Number of supplied arguments.
    (emit-stack-store :x5 saved-argument-count)
    ;; Tell the GC to used the number of arguments saved on the stack. X5 will
    ;; be used later.
    (emit-gc-info :incoming-arguments saved-argument-count)
    ;; The cons cells are allocated in one single chunk.
    (emit `(lap:orr :x7 :xzr :x26))
    ;; Remove required/optional arguments from the count.
    ;; If negative or zero, the &REST list is empty.
    (cond ((zerop regular-argument-count)
           (emit `(lap:cbz :x5 ,rest-list-done)))
          (t
           (emit `(lap:subs :x5 :x5 ,(c::fixnum-to-raw regular-argument-count)))
           (emit `(lap:b.le ,rest-list-done))))
    ;; Save the length, and double it. Each cons takes two words.
    (emit `(lap:add :x10 :xzr :x5 :lsl 1))
    ;; Add a header word and word of padding so it can be treated like a simple-vector.
    (emit `(lap:add :x10 :x10 ,(c::fixnum-to-raw 2)))
    ;; Allocate on the stack, converting fixnum to raw integer * 8.
    (emit `(lap:sub :sp :sp :x10 :lsl ,(- 3 sys.int::+n-fixnum-bits+)))
    ;; Generate the simple-vector header. simple-vector tag is zero, doesn't need to be set here.
    ;; *2 as conses are 2 words and +1 for padding word at the start.
    (emit `(lap:add :x10 :x5 :x5)
          `(lap:add :x10 :x10 ,(c::fixnum-to-raw 1)))
    (emit `(lap:add :x10 :xzr :x10 :lsl ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
    (emit `(lap:str :x10 (:sp)))
    ;; Clear the padding slot.
    (emit `(lap:str :xzr (:sp 8)))
    ;; For each cons, clear the car and set the cdr to the next cons.
    (emit `(lap:add :x12 :sp 16))
    (emit `(lap:orr :x10 :xzr :x5))
    (emit rest-clear-loop-head)
    (emit `(lap:add :x9 :x12 ,(+ 16 sys.int::+tag-cons+)))
    (emit `(lap:str :xzr (:post :x12 8))) ; car
    (emit `(lap:str :x9 (:post :x12 8))) ; cdr
    (emit `(lap:subs :x10 :x10 ,(c::fixnum-to-raw 1)))
    (emit `(lap:b.hi ,rest-clear-loop-head))
    ;; Set the cdr of the final cons to NIL.
    (emit `(lap:str :x26 (:x12 -8)))
    ;; Create the DX root object for the vector.
    (emit `(lap:add :x9 :sp ,sys.int::+tag-dx-root-object+))
    (emit-stack-store :x9 rest-dx-root)
    ;; It's now safe to write values into the list/vector.
    (emit `(lap:add :x12 :sp 16))
    ;; Add register arguments to the list.
    (loop
       for reg in (nthcdr regular-argument-count '(:x0 :x1 :x2 :x3 :x4))
       do (emit `(lap:str ,reg (:post :x12 16))
                `(lap:subs :x5 :x5 ,(c::fixnum-to-raw 1))
                `(lap:b.eq ,rest-loop-end)))
    ;; Now add the stack arguments.
    ;; Skip past required/optional arguments on the stack, the saved frame pointer and the return address.
    (emit `(lap:add :x11 :x29 ,(* (+ (max 0 (- regular-argument-count 5)) 2) 8)))
    (emit rest-loop-head)
    ;; Load from stack.
    (emit `(lap:ldr :x7 (:post :x11 8)))
    ;; Store into current car.
    (emit `(lap:str :x7 (:post :x12 16)))
    ;; Stop when no more arguments.
    (emit `(lap:subs :x5 :x5 ,(c::fixnum-to-raw 1)))
    (emit `(lap:b.ne ,rest-loop-head))
    (emit rest-loop-end)
    ;; There were &REST arguments, create the cons.
    (emit `(lap:add :x7 :sp ,(+ 16 sys.int::+tag-cons+)))
    ;; Code above jumps directly here with NIL in R13 when there are no arguments.
    (emit rest-list-done)))

(defmethod emit-lap (backend-function (instruction ir:move-instruction) uses defs)
  (ecase (lap::register-class (ir:move-destination instruction))
    (:gpr-64
     (ecase (lap::register-class (ir:move-source instruction))
       (:gpr-64
        (emit `(lap:orr ,(ir:move-destination instruction) :xzr ,(ir:move-source instruction))))
       (:fp-128
        (emit `(lap:fmov ,(ir:move-destination instruction) ,(lap::convert-width (ir:move-source instruction) 64))))))
    (:fp-128
     (ecase (lap::register-class (ir:move-source instruction))
       (:gpr-64
        (emit `(lap:fmov ,(lap::convert-width (ir:move-destination instruction) 64) ,(ir:move-source instruction))))
       #+(or)
       (:fp-128
        (emit `(lap:orr.16b ,(ir:move-destination instruction) ,(ir:move-source instruction) ,(ir:move-source instruction))))))))

(defmethod emit-lap (backend-function (instruction ir:swap-instruction) uses defs)
  (let ((lhs (ir:swap-lhs instruction))
        (rhs (ir:swap-rhs instruction)))
    (when (not (eql lhs rhs))
      (assert (eql (lap::register-class lhs) (lap::register-class rhs)))
      (ecase (lap::register-class rhs)
        (:gpr-64
         (emit `(lap:eor ,lhs ,lhs ,rhs)
               `(lap:eor ,rhs ,rhs ,lhs)
               `(lap:eor ,lhs ,lhs ,rhs)))
        #+(or)
        (:fp-128
         (emit `(lap:eor.16b ,lhs ,lhs ,rhs)
               `(lap:eor.16b ,rhs ,rhs ,lhs)
               `(lap:eor.16b ,lhs ,lhs ,rhs)))))))

(defmethod emit-lap (backend-function (instruction ir:spill-instruction) uses defs)
  (ecase (ir:virtual-register-kind (ir:spill-destination instruction))
    ((:value :integer)
     (ecase (lap::register-class (ir:spill-source instruction))
       (:gpr-64
        (emit-stack-store (ir:spill-source instruction)
                          (vreg-stack-slot (ir:spill-destination instruction))))
       (:fp-128
        (emit-stack-store (lap::convert-width (ir:spill-source instruction) 64)
                          (vreg-stack-slot (ir:spill-destination instruction))))))
    (:single-float
     (ecase (lap::register-class (ir:spill-source instruction))
       (:gpr-64
        (emit-stack-store (lap::convert-width (ir:spill-source instruction) 32)
                          (vreg-stack-slot (ir:spill-destination instruction))))
       (:fp-128
        (emit-stack-store (lap::convert-width (ir:spill-source instruction) 32)
                          (vreg-stack-slot (ir:spill-destination instruction))))))
    (:double-float
     (ecase (lap::register-class (ir:spill-source instruction))
       (:gpr-64
        (emit-stack-store (lap::convert-width (ir:spill-source instruction) 64)
                          (vreg-stack-slot (ir:spill-destination instruction))))
       (:fp-128
        (emit-stack-store (lap::convert-width (ir:spill-source instruction) 64)
                          (vreg-stack-slot (ir:spill-destination instruction))))))))

(defmethod emit-lap (backend-function (instruction ir:fill-instruction) uses defs)
  (ecase (ir:virtual-register-kind (ir:fill-source instruction))
    ((:value :integer)
     (ecase (lap::register-class (ir:fill-destination instruction))
       (:gpr-64
        (emit-stack-load (ir:fill-destination instruction)
                         (vreg-stack-slot (ir:fill-source instruction))))
       (:fp-128
        (emit-stack-load (lap::convert-width (ir:fill-destination instruction) 64)
                         (vreg-stack-slot (ir:fill-source instruction))))))
    (:single-float
     (ecase (lap::register-class (ir:fill-destination instruction))
       (:gpr-64
        (emit-stack-load (lap::convert-width (ir:fill-destination instruction) 32)
                         (vreg-stack-slot (ir:fill-source instruction))))
       (:fp-128
        (emit-stack-load (lap::convert-width (ir:fill-destination instruction) 32)
                         (vreg-stack-slot (ir:fill-source instruction))))))
    (:double-float
     (ecase (lap::register-class (ir:fill-destination instruction))
       (:gpr-64
        (emit-stack-load (lap::convert-width (ir:fill-destination instruction) 64)
                         (vreg-stack-slot (ir:fill-source instruction))))
       (:fp-128
        (emit-stack-load (lap::convert-width (ir:fill-destination instruction) 64)
                         (vreg-stack-slot (ir:fill-source instruction))))))))

(defmethod emit-lap (backend-function (instruction arm64-instruction) uses defs)
  (when (arm64-instruction-prefix instruction)
    (emit (arm64-instruction-prefix instruction)))
  (let ((real-operands (loop
                          for op in (arm64-instruction-operands instruction)
                          collect (cond ((and (consp op) (eql (first op) :literal/128))
                                         (fetch-literal/128 (second op)))
                                        ((and (consp op) (eql (first op) :literal))
                                         (fetch-literal (second op)))
                                        (t op)))))
    (emit (list* (arm64-instruction-opcode instruction) real-operands))))

(defmethod emit-lap (backend-function (instruction arm64-branch-instruction) uses defs)
  (emit (list (arm64-instruction-opcode instruction)
              (resolve-label (arm64-branch-true-target instruction))))
  (emit (list 'lap:b
              (resolve-label (arm64-branch-false-target instruction)))))

(defmethod emit-lap (backend-function (instruction ir:constant-instruction) uses defs)
  (let ((value (ir:constant-value instruction))
        (dest (ir:constant-destination instruction)))
    (cond ((typep value 'ir:backend-function)
           (emit `(lap:ldr ,dest (:constant ,(ir:compile-backend-function value *target*)))))
          ((c::fixnump value)
           (load-literal dest (c::fixnum-to-raw value)))
          ((characterp value)
           (load-literal dest (c::character-to-raw value)))
          ((eql value 'nil)
           (emit `(lap:orr ,dest :xzr :x26)))
          (t
           (emit `(lap:ldr ,dest (:constant ,value)))))))

(defmethod emit-lap (backend-function (instruction ir:return-instruction) uses defs)
  (load-literal :x5 (c::fixnum-to-raw 1))
  (emit `(lap:add :sp :x29 0)
        ;; Don't use emit-gc-info, using a custom layout.
        `(:gc :frame :multiple-values 0)
        `(lap:ldp :x29 :x30 (:post :sp 16))
        `(:gc :no-frame :multiple-values 0)
        `(lap:ret)))

(defmethod emit-lap (backend-function (instruction ir:return-multiple-instruction) uses defs)
  (emit `(lap:add :sp :x29 0)
        ;; Don't use emit-gc-info, using a custom layout.
        `(:gc :frame :multiple-values 0)
        `(lap:ldp :x29 :x30 (:post :sp 16))
        `(:gc :no-frame :multiple-values 0)
        `(lap:ret)))

(defmethod emit-lap (backend-function (instruction ir:unreachable-instruction) uses defs)
  (emit `(lap:hlt 42)))

(defmethod emit-lap (backend-function (instruction ir:jump-instruction) uses defs)
  (when (not (eql (ir:next-instruction backend-function instruction)
                  (ir:jump-target instruction)))
    (emit `(lap:b ,(resolve-label (ir:jump-target instruction))))))

(defmethod emit-lap (backend-function (instruction ir:switch-instruction) uses defs)
  (let ((jump-table (gensym)))
    (emit `(lap:adr :x9 ,jump-table)
          `(lap:add :x10 :xzr ,(ir:switch-value instruction) :lsl ,(1- (integer-length (/ 8 (ash 1 sys.int::+n-fixnum-bits+)))))
          `(lap:ldr :x10 (:x9 :x10))
          `(lap:add :x9 :x9 :x10)
          `(lap:br :x9))
    (emit jump-table)
    (loop
       for target in (ir:switch-targets instruction)
       do (emit `(:d64/le (- ,(resolve-label target) ,jump-table))))))

(defmethod emit-lap (backend-function (instruction ir:branch-instruction) uses defs)
  (emit `(lap:subs :xzr ,(ir:branch-value instruction) :x26)
        `(lap:b.ne ,(resolve-label (ir:branch-true-target instruction)))
        `(lap:b ,(resolve-label (ir:branch-false-target instruction)))))

(defun call-argument-setup (call-arguments)
  (let* ((stack-args (nthcdr 5 call-arguments))
         (n-stack-args (length stack-args))
         (n-args (length call-arguments)))
    (when (oddp n-stack-args)
      (incf n-stack-args))
    (when (not (zerop n-stack-args))
      (emit `(lap:sub :sp :sp ,(* n-stack-args 8))))
    (loop
       for arg in stack-args
       for i from 0
       do
         (emit-stack-load :x7 (vreg-stack-slot arg))
         (emit `(lap:str :x7 (:sp ,(* i 8))))
         (emit-gc-info :pushed-values (1+ i)))
    (load-literal :x5 (c::fixnum-to-raw n-args))))

(defun call-argument-teardown (call-arguments)
  (let* ((stack-args (nthcdr 5 call-arguments))
         (n-stack-args (length stack-args)))
    (when (oddp n-stack-args)
      (incf n-stack-args))
    (when (not (zerop n-stack-args))
      (emit `(lap:add :sp :sp ,(* n-stack-args 8))))))

(defmethod emit-lap (backend-function (instruction ir:call-instruction) uses defs)
  (call-argument-setup (ir:call-arguments instruction))
  (emit `(lap:named-call ,(ir:call-function instruction)))
  (call-argument-teardown (ir:call-arguments instruction)))

(defmethod emit-lap (backend-function (instruction ir:call-multiple-instruction) uses defs)
  (call-argument-setup (ir:call-arguments instruction))
  (emit `(lap:named-call ,(ir:call-function instruction)))
  (emit-gc-info :multiple-values 0)
  (call-argument-teardown (ir:call-arguments instruction)))

(defmethod emit-lap (backend-function (instruction ir:tail-call-instruction) uses defs)
  (call-argument-setup (ir:call-arguments instruction))
  (cond ((<= (length (ir:call-arguments instruction)) 5)
         (emit `(lap:add :sp :x29 0)
               ;; Don't use emit-gc-info, using a custom layout.
               `(:gc :frame :multiple-values 0)
               `(lap:ldp :x29 :x30 (:post :sp 16))
               `(:gc :no-frame :multiple-values 0)
               `(lap:named-tail-call ,(ir:call-function instruction))))
        (t
         (emit `(lap:named-call ,(ir:call-function instruction)))
         (emit-gc-info :multiple-values 0)
         (emit `(lap:add :sp :x29 0)
               ;; Don't use emit-gc-info, using a custom layout.
               `(:gc :frame :multiple-values 0)
               `(lap:ldp :x29 :x30 (:post :sp 16))
               `(:gc :no-frame :multiple-values 0)
               `(lap:ret)))))

(defmethod emit-lap (backend-function (instruction ir:tail-funcall-instruction) uses defs)
  (call-argument-setup (ir:call-arguments instruction))
  (emit-object-load :x9 :x6 :slot sys.int::+function-entry-point+)
  (cond ((<= (length (ir:call-arguments instruction)) 5)
         (emit `(lap:add :sp :x29 0)
               ;; Don't use emit-gc-info, using a custom layout.
               `(:gc :frame :multiple-values 0)
               `(lap:ldp :x29 :x30 (:post :sp 16))
               `(:gc :no-frame :multiple-values 0)
               `(lap:br :x9)))
        (t
         (emit `(lap:blr :x9))
         (emit-gc-info :multiple-values 0)
         (emit `(lap:add :sp :x29 0)
               ;; Don't use emit-gc-info, using a custom layout.
               `(:gc :frame :multiple-values 0)
               `(lap:ldp :x29 :x30 (:post :sp 16))
               `(:gc :no-frame :multiple-values 0)
               `(lap:ret)))))

(defmethod emit-lap (backend-function (instruction ir:funcall-instruction) uses defs)
  (call-argument-setup (ir:call-arguments instruction))
  (emit-object-load :x9 :x6 :slot sys.int::+function-entry-point+)
  (emit `(lap:blr :x9))
  (call-argument-teardown (ir:call-arguments instruction)))

(defmethod emit-lap (backend-function (instruction ir:funcall-multiple-instruction) uses defs)
  (call-argument-setup (ir:call-arguments instruction))
  (emit-object-load :x9 :x6 :slot sys.int::+function-entry-point+)
  (emit `(lap:blr :x9))
  (emit-gc-info :multiple-values 0)
  (call-argument-teardown (ir:call-arguments instruction)))

(defmethod lap-prepass (backend-function (instruction ir:multiple-value-funcall-multiple-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 1 :livep nil)))

(defmethod emit-lap (backend-function (instruction ir:multiple-value-funcall-multiple-instruction) uses defs)
  (let ((stack-pointer-save-area (gethash instruction *prepass-data*)))
    (emit `(lap:add :x9 :sp 0))
    (emit-stack-store :x9 stack-pointer-save-area)
    ;; Copy values in the sg-mv area to the stack. RCX holds the number of values to copy +5
    (let ((loop-head (gensym))
          (loop-exit (gensym))
          (clear-loop-head (gensym)))
      ;; X9 = n values to copy (count * 8).
      (emit `(lap:add :x9 :xzr :x5 :lsl ,(1- (integer-length (/ 8 (ash 1 sys.int::+n-fixnum-bits+)))))
            `(lap:subs :x9 :x9 (* 5 8))
            `(lap:b.le ,loop-exit)
            `(lap:and :x10 :x9 ,(lognot 15))
            `(lap:sub :sp :sp :x10)
            ;; Clear stack slots.
            `(lap:add :x10 :sp :x9)
            clear-loop-head
            `(lap:str :xzr (:x10 -8))
            `(lap:subs :x10 :x10 8)
            `(lap:subs :xzr :sp :x10)
            `(lap:b.ne ,clear-loop-head)
            ;; Copy values.
            `(lap:add :x12 :sp :xzr)
            `(lap:movz :x11 ,(+ (- 8 sys.int::+tag-object+)
                                (* mezzano.supervisor::+thread-mv-slots+ 8))))
      ;; Switch to the right GC mode.
      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx :multiple-values 0)
      (emit loop-head
            `(lap:ldr :x6 (:x28 :x11))
            `(lap:str :x6 (:x12))
            `(lap:add :x12 :x12 8)
            `(lap:add :x11 :x11 8)
            `(lap:subs :x9 :x9 8)
            `(lap:b.cc ,loop-head)
            loop-exit)
      ;; All done with the MV area.
      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx))
    (emit-object-load :x9 :x6 :slot sys.int::+function-entry-point+)
    (emit `(lap:blr :x9))
    (emit-gc-info :multiple-values 0)
    ;; Restore the stack pointer.
    ;; No special NLX handling required as non-local exits already restore
    ;; the stack pointer.
    (emit-stack-load :x9 stack-pointer-save-area)
    (emit `(lap:add :sp :x9 0))))

(defmethod lap-prepass (backend-function (instruction ir:multiple-value-funcall-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 1 :livep nil)))

(defmethod emit-lap (backend-function (instruction ir:multiple-value-funcall-instruction) uses defs)
  (let ((stack-pointer-save-area (gethash instruction *prepass-data*)))
    (emit `(lap:add :x9 :sp 0))
    (emit-stack-store :x9 stack-pointer-save-area)
    ;; Copy values in the sg-mv area to the stack. RCX holds the number of values to copy +5
    (let ((loop-head (gensym))
          (loop-exit (gensym))
          (clear-loop-head (gensym)))
      ;; X9 = n values to copy (count * 8).
      (emit `(lap:add :x9 :xzr :x5 :lsl ,(1- (integer-length (/ 8 (ash 1 sys.int::+n-fixnum-bits+)))))
            `(lap:subs :x9 :x9 (* 5 8))
            `(lap:b.le ,loop-exit)
            `(lap:and :x10 :x9 ,(lognot 15))
            `(lap:sub :sp :sp :x10)
            ;; Clear stack slots.
            `(lap:add :x10 :sp :x9)
            clear-loop-head
            `(lap:str :xzr (:x10 -8))
            `(lap:subs :x10 :x10 8)
            `(lap:subs :xzr :sp :x10)
            `(lap:b.ne ,clear-loop-head)
            ;; Copy values.
            `(lap:add :x12 :sp :xzr)
            `(lap:movz :x11 ,(+ (- 8 sys.int::+tag-object+)
                                (* mezzano.supervisor::+thread-mv-slots+ 8))))
      ;; Switch to the right GC mode.
      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx :multiple-values 0)
      (emit loop-head
            `(lap:ldr :x6 (:x28 :x11))
            `(lap:str :x6 (:x12))
            `(lap:add :x12 :x12 8)
            `(lap:add :x11 :x11 8)
            `(lap:subs :x9 :x9 8)
            `(lap:b.cc ,loop-head)
            loop-exit)
      ;; All done with the MV area.
      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx))
    (emit-object-load :x9 :x6 :slot sys.int::+function-entry-point+)
    (emit `(lap:blr :x9))
    (emit-gc-info)
    ;; Restore the stack pointer.
    ;; No special NLX handling required as non-local exits already restore
    ;; the stack pointer.
    (emit-stack-load :x9 stack-pointer-save-area)
    (emit `(lap:add :sp :x9 0))))

(defmethod lap-prepass (backend-function (instruction ir:begin-nlx-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 4 :livep nil)))

(defmethod emit-lap (backend-function (instruction ir:begin-nlx-instruction) uses defs)
  (let ((control-info (gethash instruction *prepass-data*))
        (jump-table (gensym))
        (over (gensym)))
    ;; Construct jump info.
    (emit `(lap:adr :x9 ,jump-table))
    (emit-stack-store :x9 (+ control-info 3))
    (emit-object-load :x9 :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)
    (emit-stack-store :x9 (+ control-info 2))
    (emit `(lap:add :x9 :sp :xzr))
    (emit-stack-store :x9 (+ control-info 1))
    (emit-stack-store :x29 (+ control-info 0))
    ;; Save in the environment.
    (load-literal :x9 (control-stack-frame-offset (+ control-info 3)))
    (emit `(lap:add ,(ir:nlx-context instruction) :x29 :x9))
    ;; FIXME: Emit jump table as trailer.
    (emit `(lap:b ,over)
          jump-table)
    (dolist (target (ir:begin-nlx-targets instruction))
      (emit `(:d64/le (- ,(resolve-label target) ,jump-table))))
    (emit over)))

(defmethod emit-lap (backend-function (instruction ir:finish-nlx-instruction) uses defs)
  )

(defun emit-nlx-entry (region multiple-values-p)
  ;; Custom layout, don't use emit-gc-info.
  ;; The NLX hasn't completed yet and we're still running with the previous
  ;; rbp/rsp. It's always safe to use a plain :frame at this point as there
  ;; is no need to keep anything in the frame alive.
  (emit (if multiple-values-p
            `(:gc :frame :multiple-values 0)
            `(:gc :frame)))
  ;; Flush any dx roots that were invalidated by this exit.
  ;; Flushing before restoring the stack means that the
  ;; stack pointer will always be below the live/not-yet-flushed roots.
  (emit `(lap:ldr :x10 (:x9 24))) ; rbp
  (dolist (dx-root (gethash region *dx-root-visibility*))
    (apply #'emit (code-for-reg-immediate-mem-op 'lap:str :x26 :x10 (control-stack-frame-offset dx-root))))
  (if multiple-values-p
      (emit-gc-info :block-or-tagbody-thunk :rax :multiple-values 0)
      (emit-gc-info :block-or-tagbody-thunk :rax))
  (emit `(lap:ldr :x10 (:x9 16))
        `(lap:add :sp :x10 :xzr)
        `(lap:ldr :x29 (:x9 24))))

(defmethod emit-lap (backend-function (instruction ir:nlx-entry-instruction) uses defs)
  (emit-nlx-entry (ir:nlx-region instruction) nil))

(defmethod emit-lap (backend-function (instruction ir:nlx-entry-multiple-instruction) uses defs)
  (emit-nlx-entry (ir:nlx-region instruction) t))

(defun emit-invoke-nlx (instruction)
  (emit `(lap:orr :x9 :xzr ,(ir:nlx-context instruction))
        `(lap:ldr :x10 (:x9 0))
        `(lap:ldr :x11 (:x10 ,(* (ir:invoke-nlx-index instruction) 8)))
        `(lap:add :x10 :x10 :x11)
        `(lap:br :x10)))

(defmethod emit-lap (backend-function (instruction ir:invoke-nlx-instruction) uses defs)
  (emit-invoke-nlx instruction))

(defmethod emit-lap (backend-function (instruction ir:invoke-nlx-multiple-instruction) uses defs)
  (emit-invoke-nlx instruction))

;; FIXME: Don't recompute contours for each save instruction.
(defmethod lap-prepass (backend-function (instruction ir:save-multiple-instruction) uses defs)
  (let ((contours (ir::dynamic-contours backend-function)))
    ;; Allocate dx-root & stack pointer save slots
    (let ((dx-root (allocate-stack-slots 1))
          (saved-stack-pointer (allocate-stack-slots 1 :livep nil)))
      (setf (gethash instruction *saved-multiple-values*)
            (cons dx-root saved-stack-pointer))
      (dolist (region (gethash instruction contours))
        (when (typep region 'ir:begin-nlx-instruction)
          (push dx-root (gethash region *dx-root-visibility*)))))))

(defmethod emit-lap (backend-function (instruction ir:save-multiple-instruction) uses defs)
  (let* ((save-data (gethash instruction *saved-multiple-values*))
         (sv-save-area (car save-data))
         (saved-stack-pointer (cdr save-data))
         (save-done (gensym "VALUES-SAVE-DONE"))
         (save-loop-head (gensym "VALUES-SAVE-LOOP")))
    ;; Allocate an appropriately sized DX simple vector.
    ;; Add one for the header, then round the count up to an even number.
    (emit `(lap:add :x9 :x5 ,(c::fixnum-to-raw 2)))
    (emit `(lap:and :x9 :x9 ,(c::fixnum-to-raw (lognot 1))))
    ;; Save SP.
    (emit `(lap:add :x10 :sp :xzr))
    (emit-stack-store :x10 saved-stack-pointer)
    ;; Adjust RSP. rax to raw * 8.
    (emit `(lap:add :x9 :xzr :x9 :lsl ,(- 3 sys.int::+n-fixnum-bits+)))
    (emit `(lap:sub :sp :sp :x9))
    ;; Write the simple-vector header.
    (emit `(lap:add :x9 :xzr :x5 :lsl ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
    (emit `(lap:str :x9 (:sp)))
    ;; Clear the SV body. Don't modify RCX, needed for MV GC info.
    (let ((clear-loop-head (gensym "MVP1-CLEAR-LOOP"))
          (clear-loop-end (gensym "MVP1-CLEAR-LOOP-END")))
      (emit `(lap:orr :x10 :xzr :x5))
      (emit `(lap:cbz :x10 ,clear-loop-end))
      (emit `(lap:add :x11 :sp 8))
      (emit clear-loop-head)
      (emit `(lap:str :xzr (:post :x11 8)))
      (emit `(lap:sub :x10 :x10 ,(c::fixnum-to-raw 1)))
      (emit `(lap:cbnz :x10 ,clear-loop-head))
      (emit clear-loop-end))
    ;; Create & save the DX root value.
    (emit `(lap:add :x9 :sp ,sys.int::+tag-dx-root-object+))
    (emit-stack-store :x9 sv-save-area)
    ;; Save MV registers.
    (loop
       for reg in '(:x0 :x1 :x2 :x3 :x4)
       for offset from 0
       do
         (emit `(lap:subs :xzr :x5 ,(c::fixnum-to-raw offset)))
         (emit `(lap:b.le ,save-done))
       ;; 1+ to skip header.
         (emit `(lap:str ,reg (:sp ,(* (1+ offset) 8)))))
    ;; Save values in the MV area.
    ;; Number of values remaining.
    (emit `(lap:subs :x9 :x5 ,(c::fixnum-to-raw 5)))
    (emit `(lap:b.le ,save-done))
    ;; Save into the simple-vector.
    (emit `(lap:add :x12 :sp ,(* 6 8))) ; skip header and registers.
    ;; Load from the MV area.
    (emit `(lap:add :x11 :x28 ,(+ (- 8 sys.int::+tag-object+)
                                  (* mezzano.supervisor::+thread-mv-slots+ 8))))
    ;; Save the values into a simple-vector.
    (emit save-loop-head)
    (emit `(lap:ldr :x6 (:post :x11 8)))
    (emit `(lap:str :x6 (:post :x12 8)))
    (emit `(lap:subs :x9 :x9 ,(c::fixnum-to-raw 1)))
    (emit `(lap:b.ne ,save-loop-head))
    ;; Finished saving values.
    (emit save-done)))

(defmethod emit-lap (backend-function (instruction ir:restore-multiple-instruction) uses defs)
  (let* ((save-data (gethash (ir:restore-multiple-context instruction) *saved-multiple-values*))
         (sv-save-area (car save-data))
         (saved-stack-pointer (cdr save-data)))
    ;; Create a normal object from the saved dx root.
    (emit-stack-load :x9 sv-save-area)
    (emit `(lap:add :x0 :x9 ,(- sys.int::+tag-object+
                                sys.int::+tag-dx-root-object+)))
    ;; Call helper.
    (load-literal :x5 (c::fixnum-to-raw 1))
    (emit `(lap:named-call sys.int::values-simple-vector))
    (emit-gc-info :multiple-values 0)
    ;; Kill the dx root and restore the old stack pointer.
    (emit-stack-store :x26 sv-save-area)
    (emit-stack-load :x9 saved-stack-pointer)
    (emit `(lap:add :sp :x9 :xzr))))

(defmethod emit-lap (backend-function (instruction ir:forget-multiple-instruction) uses defs)
  (let* ((save-data (gethash (ir:forget-multiple-context instruction) *saved-multiple-values*))
         (sv-save-area (car save-data))
         (saved-stack-pointer (cdr save-data)))
    ;; Kill the dx root and restore the old stack pointer.
    (emit-stack-store :x26 sv-save-area)
    (emit-stack-load :x9 saved-stack-pointer)
    (emit `(lap:add :sp :x9 :xzr))))

(defmethod emit-lap (backend-function (instruction ir:multiple-value-bind-instruction) uses defs)
  (loop
     with regs = '(:x0 :x1 :x2 :x3 :x4)
     for i from 0
     for value in (ir:multiple-value-bind-values instruction)
     do
       (emit `(lap:subs :xzr :x5 ,(c::fixnum-to-raw i)))
       (cond (regs
              (let ((reg (pop regs)))
                (emit `(lap:csel.le ,reg :x26 ,reg))))
             (t
              (emit `(lap:orr :x7 :xzr :x26))
              (let ((over (gensym)))
                (emit `(lap:b.le ,over))
                (emit-object-load :x7 :x28
                                  :slot (+ mezzano.supervisor::+thread-mv-slots+
                                           (- i 5)))
                (emit over)
                (emit-stack-store :x7 (vreg-stack-slot value)))))))

(defmethod emit-lap (backend-function (instruction ir:values-instruction) uses defs)
  (cond ((endp (ir:values-values instruction))
         (emit `(lap:orr :x0 :xzr :x26))
         (load-literal :x5 0))
        (t
         (load-literal :x5 (c::fixnum-to-raw (min 5 (length (ir:values-values instruction)))))
         (loop
            for value in (nthcdr 5 (ir:values-values instruction))
            for i from 0
            do
              (emit-stack-load :x7 (vreg-stack-slot value))
              (emit-object-store :x7 :x28 :slot (+ mezzano.supervisor::+thread-mv-slots+ i))
              (emit-gc-info :multiple-values 1)
              (emit `(lap:add :x5 :x5 ,(c::fixnum-to-raw 1)))
              (emit-gc-info :multiple-values 0)))))

(defmethod lap-prepass (backend-function (instruction ir:push-special-stack-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 4 :aligned t)))

(defmethod emit-lap (backend-function (instruction ir:push-special-stack-instruction) uses defs)
  (let ((slots (gethash instruction *prepass-data*))
        (frame-reg (ir:push-special-stack-frame instruction)))
    ;; Store header.
    (load-literal :x9 (logior (ash 3 sys.int::+object-data-shift+)
                              (ash (ir:push-special-stack-tag instruction)
                                   sys.int::+object-type-shift+)))
    (emit-stack-store :x9  (+ slots 3))
    ;; Store bits.
    (emit-stack-store (ir:push-special-stack-a-value instruction) (+ slots 1))
    (emit-stack-store (ir:push-special-stack-b-value instruction) (+ slots 0))
    ;; Store link. Misuses frame-reg slightly.
    (emit-object-load frame-reg :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)
    (emit-stack-store frame-reg (+ slots 2))
    ;; Generate pointer.
    (load-literal :x9 (+ (control-stack-frame-offset (+ slots 3))
                         sys.int::+tag-object+))
    (emit `(lap:add ,frame-reg :x29 :x9))
    ;; Push.
    (emit-object-store frame-reg :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)))

(defmethod emit-lap (backend-function (instruction ir:flush-binding-cache-entry-instruction) uses defs)
  (emit `(lap:add :x9 :xzr ,(ir:flush-binding-cache-entry-symbol instruction) :lsr 1)
        `(lap:and :x9 :x9 ,(ash (1- mezzano.supervisor::+thread-symbol-cache-size+) 3)))
  ;; Store the new binding stack entry into the cache entry.
  (emit `(lap:add :x9 :x9 ,(object-slot-displacement mezzano.supervisor::+thread-symbol-cache+))
        `(lap:str ,(ir:flush-binding-cache-entry-new-value instruction) (:x28 :x9))))

(defmethod emit-lap (backend-function (instruction ir:unbind-instruction) uses defs)
  ;; Top entry in the binding stack is a special variable binding.
  ;; It's a symbol and the current value.
  (emit-object-load :x6 :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)
  ;; Pop the stack.
  (emit-object-load :x7 :x6 :slot 0)
  (emit-object-store :x7 :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)
  ;; Recompute the symbol hash.
  (emit-object-load :x9 :x6 :slot sys.int::+symbol-value-cell-symbol+)
  (emit `(lap:add :x9 :xzr :x9 :lsr 1)
        `(lap:and :x9 :x9 ,(ash (1- mezzano.supervisor::+thread-symbol-cache-size+) 3))
        `(lap:add :x9 :x9 ,(object-slot-displacement mezzano.supervisor::+thread-symbol-cache+)))
  ;; Flush the binding cell cache for this entry.
  (let ((after-flush (gensym)))
    (emit `(lap:ldr :x10 (:x28 :x9))
          `(lap:subs :xzr :x10 :x6)
          `(lap:b.ne ,after-flush))
    (load-literal :x10 :symbol-binding-cache-sentinel)
    (emit `(lap:str :x10 (:x28 :x9)))
    (emit after-flush)))

(defmethod emit-lap (backend-function (instruction ir:disestablish-block-or-tagbody-instruction) uses defs)
  ;; Top entry in the binding stack is a block or tagbody entry.
  ;; It's a environment simple-vector & an offset.
  ;; Pop the stack & set env[offset] = NIL.
  (emit-object-load :x6 :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)
  (emit-object-load :x7 :x6 :slot 1)
  (emit-object-load :x9 :x6 :slot 2)
  (emit `(lap:add :x9 :xzr :x9 :lsl 2)
        `(lap:sub :x9 :x9 ,(- (object-slot-displacement 0)))
        `(lap:str :x26 (:x7 :x9)))
  (emit-object-load :x6 :x6 :slot 0)
  (emit-object-store :x6 :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+))

(defmethod emit-lap (backend-function (instruction ir:disestablish-unwind-protect-instruction) uses defs)
  ;; Top entry in the binding stack is an unwind-protect entry.
  ;; It's a function and environment object.
  ;; Pop the stack & call the function with the environment object.
  (emit-object-load :x0 :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)
  (emit-object-load :x7 :x0 :slot 1) ; function
  (emit-object-load :x6 :x0 :slot 2) ; environment
  ;; Pop stack.
  (emit-object-load :x0 :x0 :slot 0) ; link
  (emit-object-store :x0 :x28 :slot mezzano.supervisor::+thread-special-stack-pointer+)
  (load-literal :x5 0)
  (emit-object-load :x9 :x7 :slot sys.int::+function-entry-point+)
  (emit `(lap:blr :x9)))

(defmethod lap-prepass (backend-function (instruction ir:make-dx-simple-vector-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots (1+ (ir:make-dx-simple-vector-size instruction)) :aligned t)))

(defmethod emit-lap (backend-function (instruction ir:make-dx-simple-vector-instruction) uses defs)
  (let* ((slots (gethash instruction *prepass-data*))
         (size (ir:make-dx-simple-vector-size instruction))
         (words (1+ size)))
    (when (oddp words)
      (incf words))
    ;; Initialize the header.
    (load-literal :x9 (ash size sys.int::+object-data-shift+))
    (emit-stack-store :x9 (+ slots words -1))
    ;; Generate pointer.
    (load-literal :x9 (+ (control-stack-frame-offset (+ slots words -1))
                         sys.int::+tag-object+))
    (emit `(lap:add ,(ir:make-dx-simple-vector-result instruction) :x29 :x9))))

(defmethod lap-prepass (backend-function (instruction ir:make-dx-cons-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 2 :aligned t)))

(defmethod emit-lap (backend-function (instruction ir:make-dx-cons-instruction) uses defs)
  (let* ((slots (gethash instruction *prepass-data*)))
    ;; Generate pointer.
    (load-literal :x9 (+ (control-stack-frame-offset (+ slots 2 -1))
                         sys.int::+tag-cons+))
    (emit `(lap:add ,(ir:make-dx-cons-result instruction) :x29 :x9))))

(defmethod lap-prepass (backend-function (instruction ir:make-dx-closure-instruction) uses defs)
  (setf (gethash instruction *prepass-data*) (allocate-stack-slots 4 :aligned t)))

(defmethod emit-lap (backend-function (instruction ir:make-dx-closure-instruction) uses defs)
  (let ((slots (gethash instruction *prepass-data*)))
    ;; Closure tag and size.
    (load-literal :x9 (logior (ash 3 sys.int::+object-data-shift+)
                              (ash sys.int::+object-tag-closure+
                                   sys.int::+object-type-shift+)))
    (emit-stack-store :x9 (+ slots 3))
    ;; Entry point is CODE's entry point.
    (emit-object-load :x9 (ir:make-dx-closure-function instruction))
    (emit-stack-store :x9 (+ slots 2))
    ;; Store function & environment.
    (emit-stack-store (ir:make-dx-closure-function instruction) (+ slots 1))
    (emit-stack-store (ir:make-dx-closure-environment instruction) (+ slots 0))
    ;; Generate pointer.
    (load-literal :x9 (+ (control-stack-frame-offset (+ slots 3))
                         sys.int::+tag-object+))
    (emit `(lap:add ,(ir:make-dx-closure-result instruction) :x29 :x9))))

(defmethod emit-lap (backend-function (instruction ir:box-fixnum-instruction) uses defs)
  (emit `(lap:add ,(ir:box-destination instruction) :xzr ,(ir:box-source instruction) :lsl ,sys.int::+n-fixnum-bits+)))

(defmethod emit-lap (backend-function (instruction ir:unbox-fixnum-instruction) uses defs)
  (emit `(lap:add ,(ir:unbox-destination instruction) :xzr ,(ir:unbox-source instruction) :asr ,sys.int::+n-fixnum-bits+)))

(defmethod emit-lap (backend-function (instruction ir:unbox-unsigned-byte-64-instruction) uses defs)
  (let ((bignum-path (gensym))
        (out (gensym)))
    (emit `(lap:tbnz ,(ir:unbox-source instruction) 0 ,bignum-path)
          `(lap:add ,(ir:unbox-destination instruction) :xzr ,(ir:unbox-source instruction) :asr ,sys.int::+n-fixnum-bits+)
          `(lap:b ,out)
          bignum-path)
    (emit-object-load (ir:unbox-destination instruction) (ir:unbox-source instruction) :slot 0)
    (emit out)))

(defmethod emit-lap (backend-function (instruction ir:unbox-signed-byte-64-instruction) uses defs)
  (let ((bignum-path (gensym))
        (out (gensym)))
    (emit `(lap:tbnz ,(ir:unbox-source instruction) 0 ,bignum-path)
          `(lap:add ,(ir:unbox-destination instruction) :xzr ,(ir:unbox-source instruction) :asr ,sys.int::+n-fixnum-bits+)
          `(lap:b ,out)
          bignum-path)
    (emit-object-load (ir:unbox-destination instruction) (ir:unbox-source instruction) :slot 0)
    (emit out)))

;; TODO: Do this without a temporary integer register.
(defmethod emit-lap (backend-function (instruction ir:box-single-float-instruction) uses defs)
  (ecase (lap::register-class (ir:box-source instruction))
    (:gpr-64
     (emit `(lap:orr :x9 :xzr ,(ir:box-source instruction))))
    (:fp-128
     (emit `(lap:fmov :w9 ,(lap::convert-width (ir:box-source instruction) 32)))))
  (emit `(lap:add :x9 :xzr :x9 :lsl 32)
        `(lap:add ,(ir:box-destination instruction) :x9 ,(logior sys.int::+tag-immediate+
                                                                 (dpb sys.int::+immediate-tag-single-float+
                                                                      sys.int::+immediate-tag+
                                                                      0)))))

(defmethod emit-lap (backend-function (instruction ir:unbox-single-float-instruction) uses defs)
  (ecase (lap::register-class (ir:box-source instruction))
    (:gpr-64
     (emit `(lap:add ,(ir:unbox-destination instruction) :xzr ,(ir:unbox-source instruction) :lsr 32)))
    (:fp-128
     (emit `(lap:add :x9 :xzr ,(ir:unbox-source instruction) :lsr 32))
     (emit `(lap:fmov ,(lap::convert-width (ir:unbox-destination instruction) 32) :w9)))))

(defmethod emit-lap (backend-function (instruction ir:unbox-double-float-instruction) uses defs)
  (emit-object-load (lap::convert-width (ir:unbox-destination instruction) 64)
                    (ir:unbox-source instruction)))

(defmethod emit-lap (backend-function (instruction ir:debug-instruction) uses defs)
  nil)

(defmethod emit-lap (backend-function (instruction ir:spice-instruction) uses defs)
  nil)
