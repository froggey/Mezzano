;;;; ARM64 compiler backend.

(in-package :mezzano.compiler.backend.arm64)

;;; Wrapper around an arbitrary arm64 instruction.
(defclass arm64-instruction (ir:backend-instruction)
  ((%inputs :initarg :inputs :reader ir:instruction-inputs)
   (%outputs :initarg :outputs :reader ir:instruction-outputs)
   (%opcode :initarg :opcode :reader arm64-instruction-opcode)
   (%operands :initarg :operands :reader arm64-instruction-operands)
   (%clobbers :initarg :clobbers :reader arm64-instruction-clobbers)
   (%early-clobber :initarg :early-clobber :reader arm64-instruction-early-clobber)
   (%prefix :initarg :prefix :reader arm64-instruction-prefix)
   ;; When true, the input registers are moved to the output registers, to simulate a rmw instruction.
   (%fake-3op :initarg :fake-3op :reader arm64-instruction-fake-3op))
  (:default-initargs :clobbers '() :early-clobber nil :prefix nil :fake-3op nil))

(defmethod ra:instruction-clobbers ((instruction arm64-instruction) (architecture c:arm64-target))
  (arm64-instruction-clobbers instruction))

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction arm64-instruction) (architecture c:arm64-target))
  (not (arm64-instruction-early-clobber instruction)))

(defmethod ir:replace-all-registers ((instruction arm64-instruction) substitution-function)
  (setf (slot-value instruction '%inputs) (mapcar substitution-function (slot-value instruction '%inputs)))
  (setf (slot-value instruction '%outputs) (mapcar substitution-function (slot-value instruction '%outputs)))
  (setf (slot-value instruction '%operands)
        (loop
           for operand in (slot-value instruction '%operands)
           collect (cond ((typep operand 'ir:virtual-register)
                          (funcall substitution-function operand))
                         ((and (consp operand)
                               (not (member (first operand) '(:constant :function))))
                          (mapcar substitution-function operand))
                         (t operand)))))

(defmethod ir:print-instruction ((instruction arm64-instruction))
  (format t "   ~S~%"
          `(:arm64 ,(arm64-instruction-opcode instruction) ,(arm64-instruction-operands instruction))))

;;; Wrapper around arm64 branch instructions.
(defclass arm64-branch-instruction (ir:terminator-instruction)
  ((%opcode :initarg :opcode :accessor arm64-instruction-opcode)
   (%true-target :initarg :true-target :accessor arm64-branch-true-target)
   (%false-target :initarg :false-target :accessor arm64-branch-false-target)))

(defmethod ir:successors (function (instruction arm64-branch-instruction))
  (list (arm64-branch-true-target instruction)
        (arm64-branch-false-target instruction)))

(defmethod ir:instruction-inputs ((instruction arm64-branch-instruction))
  '())

(defmethod ir:instruction-outputs ((instruction arm64-branch-instruction))
  '())

(defmethod ir:replace-all-registers ((instruction arm64-branch-instruction) substitution-function)
  '())

(defmethod ir:print-instruction ((instruction arm64-branch-instruction))
  (format t "   ~S~%"
          `(:arm64-branch ,(arm64-instruction-opcode instruction)
                          ,(arm64-branch-true-target instruction)
                          ,(arm64-branch-false-target instruction))))

(defclass arm64-atomic-instruction (ir:backend-instruction)
  ((%opcode :initarg :opcode :reader arm64-instruction-opcode)
   (%old-value :initarg :old-value :accessor arm64-atomic-old-value)
   (%index :initarg :index :accessor arm64-atomic-index)
   (%rhs :initarg :rhs :accessor arm64-atomic-rhs)))

(defmethod ra:instruction-clobbers ((instruction arm64-atomic-instruction) (architecture c:arm64-target))
  '(:x9 :x1))

(defmethod ir:instruction-inputs ((instruction arm64-atomic-instruction))
  (list (arm64-atomic-index instruction)
        (arm64-atomic-rhs instruction)))

(defmethod ir:instruction-outputs ((instruction arm64-atomic-instruction))
  (list (arm64-atomic-old-value instruction)))

(defmethod ir:replace-all-registers ((instruction arm64-atomic-instruction) substitution-function)
  (setf (arm64-atomic-old-value instruction) (funcall substitution-function (arm64-atomic-old-value instruction)))
  (setf (arm64-atomic-index instruction) (funcall substitution-function (arm64-atomic-index instruction)))
  (setf (arm64-atomic-rhs instruction) (funcall substitution-function (arm64-atomic-rhs instruction))))

(defmethod ir:print-instruction ((instruction arm64-atomic-instruction))
  (format t "   ~S~%"
          `(:arm64-atomic ,(arm64-instruction-opcode instruction)
                          ,(arm64-atomic-old-value instruction)
                          ,(arm64-atomic-index instruction)
                          ,(arm64-atomic-rhs instruction))))

(defclass arm64-cas-instruction (ir:backend-instruction)
  ((%new-value :initarg :new-value :accessor arm64-cas-new-value)
   (%old-value :initarg :old-value :accessor arm64-cas-old-value)
   (%result :initarg :result :accessor arm64-cas-result)
   (%current-value :initarg :current-value :accessor arm64-cas-current-value)
   (%index :initarg :index :accessor arm64-cas-index)
   (%success-order :initarg :success-order :accessor arm64-cas-success-order)
   (%failure-order :initarg :failure-order :accessor arm64-cas-failure-order)))

(defmethod ra:instruction-clobbers ((instruction arm64-cas-instruction) (architecture c:arm64-target))
  '(:x9 :x1))

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction arm64-cas-instruction) (architecture c:arm64-target))
  ;; Outputs may clobber inputs!
  nil)

(defmethod ir:instruction-inputs ((instruction arm64-cas-instruction))
  (list (arm64-cas-new-value instruction)
        (arm64-cas-old-value instruction)
        (arm64-cas-index instruction)))

(defmethod ir:instruction-outputs ((instruction arm64-cas-instruction))
  (list (arm64-cas-result instruction)
        (arm64-cas-current-value instruction)))

(defmethod ir:replace-all-registers ((instruction arm64-cas-instruction) substitution-function)
  (setf (arm64-cas-new-value instruction) (funcall substitution-function (arm64-cas-new-value instruction)))
  (setf (arm64-cas-old-value instruction) (funcall substitution-function (arm64-cas-old-value instruction)))
  (setf (arm64-cas-current-value instruction) (funcall substitution-function (arm64-cas-current-value instruction)))
  (setf (arm64-cas-result instruction) (funcall substitution-function (arm64-cas-result instruction)))
  (setf (arm64-cas-index instruction) (funcall substitution-function (arm64-cas-index instruction))))

(defmethod ir:print-instruction ((instruction arm64-cas-instruction))
  (format t "   ~S~%"
          `(:arm64-cas    ,(arm64-cas-new-value instruction)
                          ,(arm64-cas-old-value instruction)
                          ,(arm64-cas-current-value instruction)
                          ,(arm64-cas-result instruction)
                          ,(arm64-atomic-index instruction)
                          ,(arm64-atomic-rhs instruction)
                          ,(arm64-cas-success-order instruction)
                          ,(arm64-cas-failure-order instruction))))

(defclass arm64-casp-instruction (ir:backend-instruction)
  ((%result :initarg :result :accessor arm64-cas-result)
   (%index :initarg :index :accessor arm64-cas-index)
   (%success-order :initarg :success-order :accessor arm64-cas-success-order)
   (%failure-order :initarg :failure-order :accessor arm64-cas-failure-order)))

(defmethod ra:instruction-clobbers ((instruction arm64-casp-instruction) (architecture c:arm64-target))
  '(:x9 :x0 :x1 :x2 :x3 :x4 :x6 :x7))

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction arm64-casp-instruction) (architecture c:arm64-target))
  ;; Outputs may clobber inputs!
  nil)

(defmethod ir:instruction-inputs ((instruction arm64-casp-instruction))
  ;; object
  ;; old-1/old-2
  ;; new-1/new-2
  (list (arm64-cas-index instruction)))

(defmethod ir:instruction-outputs ((instruction arm64-casp-instruction))
  ;; value-1/value-2
  (list (arm64-cas-result instruction)))

(defmethod ir:replace-all-registers ((instruction arm64-casp-instruction) substitution-function)
  (setf (arm64-cas-result instruction) (funcall substitution-function (arm64-cas-result instruction)))
  (setf (arm64-cas-index instruction) (funcall substitution-function (arm64-cas-index instruction))))

(defmethod ir:print-instruction ((instruction arm64-casp-instruction))
  (format t "   ~S~%"
          `(:arm64-dcas   ,(arm64-cas-result instruction)
                          ,(arm64-cas-index instruction)
                          ,(arm64-cas-success-order instruction)
                          ,(arm64-cas-failure-order instruction))))

(defclass arm64-cas-mem-instruction (ir:backend-instruction)
  ((%width :initarg :width :reader arm64-cas-mem-width)
   (%address :initarg :address :accessor arm64-cas-mem-address)
   (%new-value :initarg :new-value :accessor arm64-cas-new-value)
   (%old-value :initarg :old-value :accessor arm64-cas-old-value)
   (%current-value :initarg :current-value :accessor arm64-cas-current-value)
   (%success-order :initarg :success-order :accessor arm64-cas-success-order)
   (%failure-order :initarg :failure-order :accessor arm64-cas-failure-order)))

(defmethod ra:instruction-clobbers ((instruction arm64-cas-mem-instruction) (architecture c:arm64-target))
  '())

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction arm64-cas-mem-instruction) (architecture c:arm64-target))
  ;; Outputs may clobber inputs!
  nil)

(defmethod ir:instruction-inputs ((instruction arm64-cas-mem-instruction))
  (list (arm64-cas-mem-address instruction)
        (arm64-cas-new-value instruction)
        (arm64-cas-old-value instruction)))

(defmethod ir:instruction-outputs ((instruction arm64-cas-mem-instruction))
  (list (arm64-cas-current-value instruction)))

(defmethod ir:replace-all-registers ((instruction arm64-cas-mem-instruction) substitution-function)
  (setf (arm64-cas-mem-address instruction) (funcall substitution-function (arm64-cas-mem-address instruction)))
  (setf (arm64-cas-new-value instruction) (funcall substitution-function (arm64-cas-new-value instruction)))
  (setf (arm64-cas-old-value instruction) (funcall substitution-function (arm64-cas-old-value instruction)))
  (setf (arm64-cas-current-value instruction) (funcall substitution-function (arm64-cas-current-value instruction))))

(defmethod ir:print-instruction ((instruction arm64-cas-mem-instruction))
  (format t "   ~S~%"
          `(:arm64-cas-mem ,(arm64-cas-mem-width instruction)
                           ,(arm64-cas-mem-address instruction)
                           ,(arm64-cas-new-value instruction)
                           ,(arm64-cas-old-value instruction)
                           ,(arm64-cas-current-value instruction)
                           ,(arm64-cas-success-order instruction)
                           ,(arm64-cas-failure-order instruction))))

;; Expects object in :x1
(defclass arm64-ld/st-multiple-instruction (ir:backend-instruction)
  ((%opcode :initarg :opcode :reader arm64-instruction-opcode)
   (%size :initarg :size :reader arm64-ld/st-multiple-size)
   (%direction :initarg :direction :accessor arm64-ld/st-multiple-direction)
   (%registers :initarg :registers :accessor arm64-ld/st-multiple-registers)
   (%index :initarg :index :accessor arm64-ld/st-multiple-index)
   (%scale :initarg :scale :accessor arm64-ld/st-multiple-scale)))

(defmethod ra:instruction-clobbers ((instruction arm64-ld/st-multiple-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction arm64-ld/st-multiple-instruction) (architecture c:arm64-target))
  t)

(defmethod ir:instruction-inputs ((instruction arm64-ld/st-multiple-instruction))
  (list* :x1
         (arm64-ld/st-multiple-index instruction)
         (when (eql (arm64-ld/st-multiple-direction instruction) :store)
           (arm64-ld/st-multiple-registers instruction))))

(defmethod ir:instruction-outputs ((instruction arm64-ld/st-multiple-instruction))
  (when (eql (arm64-ld/st-multiple-direction instruction) :load)
    (arm64-ld/st-multiple-registers instruction)))

(defmethod ir:replace-all-registers ((instruction arm64-ld/st-multiple-instruction) substitution-function)
  (loop for reg on (arm64-ld/st-multiple-registers instruction)
        do (setf (car reg) (funcall substitution-function (car reg))))
  (setf (arm64-ld/st-multiple-index instruction) (funcall substitution-function (arm64-ld/st-multiple-index instruction))))

(defmethod ir:print-instruction ((instruction arm64-ld/st-multiple-instruction))
  (format t "   ~S~%"
          `(:arm64-ld/st-multiple
            ,(arm64-instruction-opcode instruction)
            ,(arm64-ld/st-multiple-registers instruction)
            ,(arm64-ld/st-multiple-index instruction)
            ,(arm64-ld/st-multiple-scale instruction))))

(defclass box-advsimd-instruction (ir:box-instruction)
  ((%header :initarg :header :reader advsimd-pack-header)))

(defmethod ir:box-type ((instruction box-advsimd-instruction))
  ;; TODO: Not strictly true, this is a specific subtype, but
  ;; we don't really know which at this point. I don't think
  ;; it'll ever actually matter, since box/unbox instructions
  ;; are only generated by the builtins.
  'mezzano.simd:simd-pack)

(defmethod ir:print-instruction ((instruction box-advsimd-instruction))
  (format t "   ~S~%"
          `(:box-advsimd
            ,(ir:box-destination instruction)
            ,(ir:box-source instruction)
            ,(advsimd-pack-header instruction))))

(defclass unbox-advsimd-instruction (ir:unbox-instruction)
  ())

(defmethod ir:box-type ((instruction unbox-advsimd-instruction))
  ;; TODO: Not strictly true, this is a specific subtype, but
  ;; we don't really know which at this point. I don't think
  ;; it'll ever actually matter, since box/unbox instructions
  ;; are only generated by the builtins.
  'mezzano.simd:simd-pack)

(defmethod ir:print-instruction ((instruction unbox-advsimd-instruction))
  (format t "   ~S~%"
          `(:unbox-advsimd
            ,(ir:unbox-destination instruction)
            ,(ir:unbox-source instruction))))

(defun lower-complicated-box-instructions (backend-function)
  (do* ((inst (ir:first-instruction backend-function) next-inst)
        (next-inst (ir:next-instruction backend-function inst)
                   (if inst
                       (ir:next-instruction backend-function inst)
                       nil)))
       ((null inst))
    (multiple-value-bind (box-function box-register)
        (typecase inst
          (ir:box-unsigned-byte-64-instruction
           (values 'mezzano.runtime::%%make-unsigned-byte-64-x10 :x10))
          (ir:box-signed-byte-64-instruction
           (values 'mezzano.runtime::%%make-signed-byte-64-x10 :x10))
          (ir:box-double-float-instruction
           (values 'sys.int::%%make-double-float-x10 :x10))
          (box-advsimd-instruction
           (values 'mezzano.simd.arm64::%%make-simd-pack-q0 :q0)))
      (when box-function
        (let* ((value (ir:box-source inst))
               (result (ir:box-destination inst)))
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:move-instruction
                          :destination box-register
                          :source value))
          (when (typep inst 'box-advsimd-instruction)
            (ir:insert-before
             backend-function inst
             (make-instance 'ir:constant-instruction
                            :destination :x1
                            :value (advsimd-pack-header inst))))
          (ir:insert-before
           backend-function inst
           (make-instance 'arm64-instruction
                          :opcode 'lap:named-call
                          :operands (list box-function)
                          :inputs (list* box-register
                                         (if (typep inst 'box-advsimd-instruction)
                                             (list :x1)
                                             '()))
                          :outputs (list :x0)
                          :clobbers '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                                      :x8 :x9 :x10 :x11 :x12 :x15
                                      :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
                                      :x24 :x25
                                      :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
                                      :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
                                      :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
                                      :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31)))
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:move-instruction
                          :destination result
                          :source :x0))
          (ir:remove-instruction backend-function inst))))))

(defmethod ir:perform-target-lowering (backend-function (target c:arm64-target))
  (lower-builtins backend-function))

(defun code-for-direct-vector-imm (dst value)
  ;; We've got a few options here.
  ;; movi can produce 1, 2, or 4 byte constants with one of the values
  ;; filled in with an imm8. The other option is an 8 byte constant
  ;; where each byte is populated from a single bit in the source imm8.
  (let ((qwordp (= (ldb (byte 64   0) value) (ldb (byte 64  64) value)))
        ;; True when all :s lanes are identical
        (dwordp (= (ldb (byte 32   0) value) (ldb (byte 32  32) value)
                   (ldb (byte 32  64) value) (ldb (byte 32  96) value)))
        ;; True when all :h lanes are identical
        (wordp  (= (ldb (byte 16   0) value) (ldb (byte 16  16) value)
                   (ldb (byte 16  32) value) (ldb (byte 16  48) value)
                   (ldb (byte 16  64) value) (ldb (byte 16  80) value)
                   (ldb (byte 16  96) value) (ldb (byte 16 112) value)))
        ;; True when all :b lanes are identical
        (bytep  (= (ldb (byte  8   0) value) (ldb (byte  8   8) value)
                   (ldb (byte  8  16) value) (ldb (byte  8  24) value)
                   (ldb (byte  8  32) value) (ldb (byte  8  40) value)
                   (ldb (byte  8  48) value) (ldb (byte  8  56) value)
                   (ldb (byte  8  64) value) (ldb (byte  8  72) value)
                   (ldb (byte  8  80) value) (ldb (byte  8  88) value)
                   (ldb (byte  8  96) value) (ldb (byte  8 104) value)
                   (ldb (byte  8 112) value) (ldb (byte  8 120) value))))
    (cond
      ;; dword
      ;; TODO: There are more possible variants with the :MSL shift type,
      ;; and also mvni
      ((and dwordp
            (zerop (dpb 0 (byte 8 0) (ldb (byte 32 0) value))))
       `(lap:movi.v :4s ,dst ,(ldb (byte 8 0) value) :lsl 0))
      ((and dwordp
            (zerop (dpb 0 (byte 8 8) (ldb (byte 32 0) value))))
       `(lap:movi.v :4s ,dst ,(ldb (byte 8 8) value) :lsl 8))
      ((and dwordp
            (zerop (dpb 0 (byte 8 16) (ldb (byte 32 0) value))))
       `(lap:movi.v :4s ,dst ,(ldb (byte 8 16) value) :lsl 16))
      ((and dwordp
             (zerop (dpb 0 (byte 8 24) (ldb (byte 32 0) value))))
       `(lap:movi.v :4s ,dst ,(ldb (byte 8 24) value) :lsl 24))
       ;; word
       ((and wordp
             (zerop (dpb 0 (byte 8 0) (ldb (byte 16 0) value))))
        `(lap:movi.v :8h ,dst ,(ldb (byte 8 0) value) :lsl 0))
       ((and wordp
             (zerop (dpb 0 (byte 8 8) (ldb (byte 16 0) value))))
        `(lap:movi.v :8h ,dst ,(ldb (byte 8 8) value) :lsl 8))
       ;; byte
       (bytep
        `(lap:movi.v :16b ,dst ,(ldb (byte 8 0) value)))
       ;; qword
       ((and qwordp
             (member (ldb (byte 8 0) value) '(0 #xFF))
             (member (ldb (byte 8 8) value) '(0 #xFF))
             (member (ldb (byte 8 16) value) '(0 #xFF))
             (member (ldb (byte 8 24) value) '(0 #xFF))
             (member (ldb (byte 8 32) value) '(0 #xFF))
             (member (ldb (byte 8 40) value) '(0 #xFF))
             (member (ldb (byte 8 48) value) '(0 #xFF))
             (member (ldb (byte 8 56) value) '(0 #xFF)))
        `(lap:movi.v :2d ,dst
                     ,(logior (ldb (byte 1 0) value)
                              (ash (ldb (byte 1 8) value) 1)
                              (ash (ldb (byte 1 16) value) 2)
                              (ash (ldb (byte 1 24) value) 3)
                              (ash (ldb (byte 1 32) value) 4)
                              (ash (ldb (byte 1 40) value) 5)
                              (ash (ldb (byte 1 48) value) 6)
                              (ash (ldb (byte 1 56) value) 7)))))))

(defun lower-vector-loads (backend-function)
  (multiple-value-bind (uses defs)
      (ir::build-use/def-maps backend-function)
    (declare (ignore uses))
    (do* ((inst (ir:first-instruction backend-function) next-inst)
          (next-inst (ir:next-instruction backend-function inst)
                     (if inst
                         (ir:next-instruction backend-function inst)
                         nil)))
         ((null inst))
      ;; Look for simd unbox instructions unboxing a constant vector object.
      (when (and (typep inst 'unbox-advsimd-instruction)
                 (null (rest (gethash (ir:unbox-source inst) defs)))
                 (typep (first (gethash (ir:unbox-source inst) defs)) 'ir:constant-instruction)
                 (typep (ir:constant-value (first (gethash (ir:unbox-source inst) defs)))
                        'mezzano.simd.arm64::advsimd-pack))
        ;; Read the whole pack as a single value.
        (let* ((value (mezzano.simd:simd-pack-element
                       (mezzano.simd:transmute-simd-pack
                        (ir:constant-value (first (gethash (ir:unbox-source inst) defs)))
                        '(unsigned-byte 128) 1)
                       0))
               (direct-code (code-for-direct-vector-imm
                             (ir:unbox-destination inst)
                             value)))
          (when direct-code
            (change-class inst 'arm64-instruction
                          :opcode (first direct-code)
                          :operands (rest direct-code)
                          :inputs '()
                          :outputs (list (ir:unbox-destination inst))
                          :clobbers '()
                          :early-clobber nil
                          :prefix nil
                          :fake-3op nil)))))))

(defun lower-fake-three-operand-instructions (backend-function)
  (ir:do-instructions (inst backend-function)
    (when (and (typep inst 'arm64-instruction)
               (arm64-instruction-fake-3op inst))
      ;; Insert moves from the inputs to the outputs.
      (loop for input in (ir:instruction-inputs inst)
            for output in (ir:instruction-outputs inst)
            do (ir:insert-before backend-function inst
                                 (make-instance 'ir:move-instruction
                                                :source input
                                                :destination output))))))

(defun assign-ld/st-multiple-registers (backend-function)
  (let ((regs #(:q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
                :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
                :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
                :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))
        (load-index 0)
        (store-index 0))
    (ir:do-instructions (inst backend-function)
      (when (and (typep inst 'arm64-ld/st-multiple-instruction)
                 (eql (arm64-ld/st-multiple-direction inst) :load))
        (loop for reg on (arm64-ld/st-multiple-registers inst)
              do
                 (ir:insert-after
                  backend-function inst
                  (make-instance 'ir:move-instruction
                                 :source (aref regs load-index)
                                 :destination (car reg)))
                 (setf (car reg) (aref regs load-index)
                       load-index (rem (1+ load-index) (length regs)))))
      (when (and (typep inst 'arm64-ld/st-multiple-instruction)
                 (eql (arm64-ld/st-multiple-direction inst) :store))
        (loop for reg on (arm64-ld/st-multiple-registers inst)
              do
                 (ir:insert-before
                  backend-function inst
                  (make-instance 'ir:move-instruction
                                 :source (car reg)
                                 :destination (aref regs store-index)))
                 (setf (car reg) (aref regs store-index)
                       store-index (rem (1+ store-index) (length regs))))))))

(defmethod ir:perform-target-lowering-post-ssa (backend-function (target c:arm64-target))
  (lower-vector-loads backend-function)
  (lower-complicated-box-instructions backend-function)
  (lower-fake-three-operand-instructions backend-function)
  (assign-ld/st-multiple-registers backend-function))
