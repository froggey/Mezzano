;;;; DISASSEMBLE for x86-64

(defpackage :mezzano.disassemble
  (:use :cl)
  (:local-nicknames (:sys.int :mezzano.internals)
                    (:sys.lap-x86 :mezzano.lap.x86))
  (:export #:disassemble
           #:disassemble-function
           #:make-disassembler-context
           #:disassembler-context-function
           #:instruction-at
           #:print-instruction
           #:*print-gc-metadata*
           #:*print-debug-metadata*))

(in-package :mezzano.disassemble)

(defvar *print-gc-metadata* t)
(defvar *print-debug-metadata* nil)

(defun disassemble (fn)
  (disassemble-function fn))

(defun defmethod-name-to-method (defmethod-name)
  "Convert a (DEFMETHOD name {qualifiers} (specializers...)) name to a method object."
  (let* ((name (second defmethod-name))
         (qualifiers (subseq defmethod-name 2 (1- (length defmethod-name))))
         (specializers (loop
                          for spec in (first (last defmethod-name))
                          collect (cond ((symbolp spec)
                                         (find-class spec))
                                        ((and (consp spec)
                                              (eql (first spec) 'eql))
                                         `(eql ,(eval (second spec))))
                                        (t
                                         (error "Unknown specializer ~S" spec)))))
         (generic (fdefinition name)))
    (when (not (typep generic 'generic-function))
      (error "Can't resolve ~S: ~S is not a generic-function"
             defmethod-name generic))
    (find-method generic qualifiers specializers)))

(defun disassemble-function (fn &key (gc-metadata *print-gc-metadata*) (debug-metadata *print-debug-metadata*) architecture)
  (when (and (consp fn) (eql (first fn) 'lambda))
    (setf fn (compile nil fn)))
  (when (and (consp fn) (eql (first fn) 'defmethod))
    (setf fn (mezzano.clos::method-fast-function (defmethod-name-to-method fn) nil nil)))
  (when (and (not (functionp fn))
             (not (sys.int::function-reference-p fn)))
    (setf fn (fdefinition fn)))
  (check-type fn (or function sys.int::function-reference))
  (setf architecture (mezzano.compiler::canonicalize-target architecture))
  (let ((*print-gc-metadata* gc-metadata)
        (*print-debug-metadata* debug-metadata)
        (*print-pretty* nil)
        (fundamental-fn (peel-function fn)))
    (cond ((eql fundamental-fn fn)
           (format t "~S:~%" fn))
          (t
           (format t "~S (implemented by ~S):~%" fn fundamental-fn)))
    (disassemble-subfunction fundamental-fn architecture)
    ;; Recursively traverse the function looking for closures and disassemble them too.
    (let ((closures '())
          (worklist (list fundamental-fn)))
      (loop
         until (endp worklist)
         for fn = (pop worklist)
         do
           (when (not (member fn closures))
             (when (not (eql fn fundamental-fn))
               (push fn closures))
             (when (not (sys.int::function-reference-p fn))
               (dotimes (i (sys.int::function-pool-size fn))
                 (let ((entry (sys.int::function-pool-object fn i)))
                   (when (sys.int::%object-of-type-p entry sys.int::+object-tag-function+)
                     (push entry worklist)))))))
      (setf closures (reverse closures))
      (dolist (fn closures)
        (format t "----------~%" fn)
        (format t "~S:~%" fn)
        (disassemble-subfunction fn architecture))))
  nil)

(defun disassemble-subfunction (function architecture)
  (let ((base-address (logand (sys.int::lisp-object-address function) -16))
        (offset (code-initial-offset function))
        (context (make-disassembler-context function architecture))
        (gc-md (if (sys.int::function-reference-p function)
                   '()
                   (sys.int::decode-function-gc-info function)))
        (debug-md (if (sys.int::function-reference-p function)
                      '()
                      (sys.int::decompress-precise-debug-info
                       (sys.int::decode-precise-debug-info
                        function
                        (sys.int::debug-info-precise-variable-data
                         (sys.int::function-debug-info function)))))))
    (loop
       for decoded across (context-instructions context)
       do
         (when (and gc-md
                    (>= offset (first (first gc-md))))
           (when *print-gc-metadata*
             (format t "~7T~8,'0X:~50T~S~%" (+ base-address offset) `(:gc ,@(rest (first gc-md)))))
           (pop gc-md))
         (when (and debug-md
                    (>= offset (first (first debug-md))))
           (when *print-debug-metadata*
             (format t "~7T~8,'0X:~50T~S~%" (+ base-address offset) `(:debug ,@(rest (first debug-md)))))
           (pop debug-md))
         (let ((label (label context offset)))
           (when label
             (format t " L~D" label)))
         (format t "~7T~8,'0X: " (+ base-address offset))
         (cond (decoded
                (format t "~{~2,'0X ~}~50T"
                        (loop
                           repeat (inst-size decoded)
                           for i from offset
                           collect (code-byte function i)))
                (print-instruction context decoded)
                (terpri)
                (incf offset (inst-size decoded)))
               (t
                (format t "<bad ~2,'0X>~%"
                        (code-byte function offset))
                (incf offset 1))))))

(defun peel-function (function)
  "Remove layers of closures and funcallable-instances from FUNCTION."
  (when (sys.int::function-reference-p function)
    (return-from peel-function function))
  (check-type function function)
  (let ((fundamental-fn function))
    (when (sys.int::funcallable-instance-p fundamental-fn)
      (setf fundamental-fn (sys.int::funcallable-instance-function fundamental-fn)))
    (when (sys.int::funcallable-instance-p fundamental-fn)
      ;; Bail out if there are multiple levels to funcallable-instances.
      (error "~S contains a nested funcallable-instance ~S." function fundamental-fn))
    (when (sys.int::closure-p fundamental-fn)
      (setf fundamental-fn (sys.int::%closure-function fundamental-fn)))
    (assert (sys.int::%object-of-type-p fundamental-fn sys.int::+object-tag-function+))
    fundamental-fn))

(defun code-end (function-like)
  (if (sys.int::function-reference-p function-like)
      64 ; Function references are this long.
      (sys.int::function-code-size function-like)))

(defun code-initial-offset (function-like)
  (if (sys.int::function-reference-p function-like)
      32
      16))

(defun code-byte (function-like offset)
  (if (sys.int::function-reference-p function-like)
      (sys.int::%object-ref-unsigned-byte-8 function-like (- offset 8))
      (sys.int::function-code-byte function-like offset)))

(defun make-disassembler-context (function &optional architecture)
  (declare (ignore architecture))
  (setf function (peel-function function))
  (let ((context (make-instance 'disassembler-context :function function))
        (true-end (code-end function))
        (offset (code-initial-offset function)))
      ;; Find the approximate end of the function. The size is rounded up to 16 bytes and
      ;; it's padded with zeros.
      (loop
         (when (not (zerop (code-byte function (1- true-end))))
           (return))
         (decf true-end))
      ;; Disassemble all instructions.
      (handler-case
          (loop
             (when (>= offset true-end)
               (return))
             (let ((inst (disassemble-one-instruction context)))
               (vector-push-extend inst
                                   (context-instructions context))
               (incf offset (if inst
                                (inst-size inst)
                                1))))
        (read-past-end-of-machine-code ()))
      context))

(defun slot-to-thread-slot-name (slot)
  (let* ((thread-struct (find-class 'mezzano.supervisor:thread))
         (thread-slots (mezzano.clos:class-slots thread-struct)))
    (dolist (slot-def thread-slots)
      (let* ((location (mezzano.clos:slot-definition-location slot-def))
             (offset (mezzano.runtime::location-offset location))
             (type (mezzano.runtime::location-type location))
             (size (if (eql type mezzano.runtime::+location-type-t+)
                       8
                       (mezzano.runtime::location-type-scale type)))
             (fv (mezzano.clos:structure-slot-definition-fixed-vector slot-def))
             (end-offset (+ offset (* size (or fv 1)))))
        (cond (fv
               (cond ((eql type mezzano.runtime::+location-type-t+)
                      (when (and (zerop (rem slot 8))
                                 (<= offset slot (1- end-offset)))
                        (return
                          (format nil "thread-~(~A~)+~D"
                                  (mezzano.clos:slot-definition-name slot-def)
                                  (- (truncate slot 8) (mezzano.runtime::location-offset-t location))))))
                     (t
                      (when (<= offset slot (1- end-offset))
                        (return
                          (format nil "thread-~(~A~)+~D"
                                  (mezzano.clos:slot-definition-name slot-def)
                                  (truncate (- slot offset) size)))))))
              (t
               (when (<= offset slot (1- end-offset))
                 (return
                   (format nil "thread-~(~A~)" (mezzano.clos:slot-definition-name slot-def))))))))))

(defun type-tag-to-name (tag)
  (when (zerop (ldb (byte 2 0) tag))
    (format nil "object-tag-~(~A~)" (aref sys.int::*object-tags-to-basic-types* (ash tag -2)))))

(defun print-instruction (context instruction &key (print-annotations t) (print-labels t))
  (let ((annotations '()))
    (format t "(")
    (cond ((eql (inst-opcode instruction) :jump-target)
           (format t ":D64/LE (- L~D L~D)"
                   (label context (first (inst-operands instruction)))
                   (label context (+ (first (inst-operands instruction)) (second (inst-operands instruction))))))
          (t
           (when (inst-lock-prefix instruction)
             (format t "LOCK "))
           (format t "~A" (inst-opcode instruction))
           (when (or (and (member (inst-opcode instruction)
                                  '(sys.lap-x86:cmp8 sys.lap-x86:sub8))
                          (eql (first (inst-operands instruction)) :al)
                          (integerp (second (inst-operands instruction))))
                     (and (eql (inst-opcode instruction) 'sys.lap-x86:cmp8)
                          (typep (first (inst-operands instruction)) 'effective-address)
                          (eql (ea-disp (first (inst-operands instruction))) (- sys.int::+tag-object+))
                          (integerp (second (inst-operands instruction)))))
             ;; Probably a type check.
             (let ((type-tag (type-tag-to-name (second (inst-operands instruction)))))
               (when type-tag
                 (push type-tag annotations))))
           (dolist (operand (inst-operands instruction))
             (format t " ")
             (cond ((typep operand 'effective-address)
                    (cond ((eql (ea-base operand) :rip)
                           (let* ((address (logand (sys.int::lisp-object-address (context-function context)) -16))
                                  (target (+ (inst-offset instruction)
                                             (inst-size instruction)
                                             (ea-disp operand)))
                                  (pool-index (truncate (- target (code-end (context-function context))) 8))
                                  (label (label context target)))
                             (cond
                               ((and print-labels label)
                                (push (format nil "#x~8,'0X" (+ address target)) annotations)
                                (format t "L~D" label))
                               ((member (inst-opcode instruction) '(sys.lap-x86:call sys.lap-x86:jmp))
                                ;; TODO: Differentiate between direct calls and indirect calls that
                                ;; use an rip-based memory operand.
                                (let* ((abs-addr (+ address target))
                                       ;; Hopefully a real object!
                                       (obj (sys.int::%%assemble-value
                                             (sys.int::base-address-of-internal-pointer abs-addr)
                                             sys.int::+tag-object+)))
                                  (push (format nil "'~S" obj) annotations)
                                  (format t "#x~X" abs-addr)))
                               (t
                                (cond ((and (not (logtest target #b111))
                                            (<= 0 pool-index)
                                            (not (sys.int::function-reference-p (context-function context)))
                                            (< pool-index (sys.int::function-pool-size (context-function context))))
                                       (let ((pool-object (sys.int::function-pool-object (context-function context) pool-index)))
                                         (push
                                          (let ((*print-lines* 1)
                                                (*print-length* 2)
                                                (*print-level* 2))
                                            (format nil "'~S" pool-object))
                                          annotations)))
                                      ((and (eql (inst-opcode instruction) 'sys.lap-x86:lea64)
                                            (eql target sys.int::+tag-object+))
                                       ;; The function itself, used for invalid args handling.
                                       (push (format nil "'~S" (context-function context)) annotations)))
                                (format t "(:RIP #x~X)" (+ address target))))))
                          ((and (eql (ea-base operand) :rbp)
                                (eql (logand (ea-disp operand) 7) 0)
                                (< (ea-disp operand) 0)
                                (not (ea-index operand)))
                           (format t "(:STACK ~D)" (1- (/ (- (ea-disp operand)) 8))))
                          (t
                           (when (eql (logand (ea-disp operand) 7) 7)
                             (let ((slot (truncate (+ (ea-disp operand) 1) 8)))
                               (if (ea-index operand)
                                   (push (format nil "slot ~D + ~S * ~S" slot (ea-index operand) (ea-scale operand)) annotations)
                                   (push (format nil "slot ~D" slot) annotations))))
                           (when (eql (ea-segment operand) :gs)
                             (let ((thread-slot (slot-to-thread-slot-name (1+ (ea-disp operand)))))
                               (when thread-slot
                                 (push thread-slot annotations))))
                           (when (and (not (ea-index operand))
                                      (eql (ea-disp operand) (- sys.int::+tag-cons+)))
                             (push "car" annotations))
                           (when (and (not (ea-index operand))
                                      (eql (ea-disp operand) (+ (- sys.int::+tag-cons+) 8)))
                             (push "cdr" annotations))
                           (format t "~S" (append (if (ea-segment operand)
                                                      (list (ea-segment operand))
                                                      ())
                                                  (if (ea-base operand)
                                                      (list (ea-base operand))
                                                      ())
                                                  (if (ea-index operand)
                                                      (list (list (ea-index operand) (ea-scale operand)))
                                                      ())
                                                  (if (not (zerop (ea-disp operand)))
                                                      (list (ea-disp operand))
                                                      ()))))))
                   ((integerp operand)
                    (cond ((eql operand (sys.int::lisp-object-address nil))
                           (push (format nil "'~S" nil) annotations))
                          ((eql operand (sys.int::lisp-object-address t))
                           (push (format nil "'~S" t) annotations))
                          ((eql operand (sys.int::lisp-object-address (sys.int::%unbound-value)))
                           (push (format nil "'~S" (sys.int::%unbound-value)) annotations))
                          ((eql operand (sys.int::lisp-object-address (sys.int::%symbol-binding-cache-sentinel)))
                           (push (format nil "'~S" (sys.int::%symbol-binding-cache-sentinel)) annotations))
                          ((eql operand (sys.int::lisp-object-address (sys.int::%layout-instance-header)))
                           (push (format nil "'~S" (sys.int::%layout-instance-header)) annotations))
                          ((not (logbitp 0 operand))
                           (push (format nil "'~D" (ash operand -1)) annotations))
                          ((eql (logand operand 15) sys.int::+tag-immediate+)
                           (push (format nil "'~S" (sys.int::%%assemble-value operand 0)) annotations)))
                    (push (format nil "~D" operand) annotations)
                    (format t "#x~X" operand))
                   (t
                    (format t "~S" operand))))))
    (format t ")")
    (when (and print-annotations annotations)
      (format t "~85T; ~A" (pop annotations))
      (dolist (an annotations)
        (format t ", ~A" an)))))

(defun decode-seg (reg)
  (elt #(:es :cs :ss :ds :fs :gs :invalid :invalid) reg))

(defun decode-gpr8 (reg rex-field)
  (elt #(:al :cl :dl :bl :spl :bpl :sil :dil
         :r8l :r9l :r10l :r11l :r12l :r13l :r14l :r15l)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr8-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr8 r/m rex-field)
      r/m))

(defun decode-gpr16 (reg rex-field)
  (elt #(:ax :cx :dx :bx :sp :bp :si :di
         :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr16-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr16 r/m rex-field)
      r/m))

(defun decode-gpr32 (reg rex-field)
  (elt #(:eax :ecx :edx :ebx :esp :ebp :esi :edi
         :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr32-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr32 r/m rex-field)
      r/m))

(defun decode-gpr64 (reg rex-field)
  (elt #(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
         :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr64-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr64 r/m rex-field)
      r/m))

(defun decode-mmx (reg)
  (elt #(:mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7) reg))

(defun decode-mmx-or-mem (r/m)
  (if (integerp r/m)
      (decode-mmx r/m)
      r/m))

(defun decode-xmm (reg rex-field)
  (elt #(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
         :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)
       (+ reg (if rex-field 8 0))))

(defun decode-xmm-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-xmm r/m rex-field)
      r/m))

(defconstant +modr/m-mod+ (byte 2 6))
(defconstant +modr/m-reg+ (byte 3 3))
(defconstant +modr/m-r/m+ (byte 3 0))

(defconstant +sib-ss+ (byte 2 6))
(defconstant +sib-index+ (byte 3 3))
(defconstant +sib-base+ (byte 3 0))

(defclass instruction ()
  ((%offset :reader inst-offset)
   (%size :reader inst-size)
   (%lock-prefix :initarg :lock-prefix :reader inst-lock-prefix)
   (%opcode :initarg :opcode :reader inst-opcode)
   (%operands :initarg :operands :reader inst-operands)))

(defun make-instruction (opcode &rest operands)
  (make-instance 'instruction :opcode opcode :operands operands))

(defclass effective-address ()
  ((%base :initarg :base :reader ea-base)
   (%index :initarg :index :reader ea-index)
   (%scale :initarg :scale :reader ea-scale)
   (%disp :initarg :disp :reader ea-disp)
   (%segment :initarg :segment :reader ea-segment))
  (:default-initargs :base nil :index nil :scale nil :disp 0 :segment nil))

(defclass disassembler-context ()
  ((%function :initarg :function :reader context-function :reader disassembler-context-function)
   (%offset :accessor context-code-offset)
   (%decoding-jump-table-p :initform nil :accessor decoding-jump-table-p)
   (%label-table :initform (make-hash-table) :reader context-label-table)
   (%instructions :initform (make-array 0 :adjustable t :fill-pointer 0) :reader context-instructions)))

(defmethod initialize-instance :after ((instance disassembler-context) &key function)
  (setf (slot-value instance '%offset) (code-initial-offset function)))

(defun instruction-at (context offset)
  (loop
     for inst across (context-instructions context)
     when (and inst (eql (inst-offset inst) offset))
     do (return inst)))

(defun label (context offset &key createp)
  (let ((table (context-label-table context)))
    (when (and createp
               (not (gethash offset table)))
      (setf (gethash offset table) (hash-table-count table)))
    (values (gethash offset table))))

(define-condition read-past-end-of-machine-code (error) ())

(defun consume-octet (context)
  (when (>= (context-code-offset context) (code-end (context-function context)))
    (error 'read-past-end-of-machine-code))
  (prog1
      (code-byte (context-function context) (context-code-offset context))
    (incf (context-code-offset context))))

(defun consume-word/le (context n-octets signedp)
  (loop
     with result = 0
     for i from 0 by 8
     repeat n-octets
     do
       (setf result (logior result (ash (consume-octet context) i)))
     finally
       (return (if signedp
                   (sys.int::sign-extend result (* n-octets 8))
                   result))))

(defun consume-ub8 (context)
  (consume-octet context))

(defun consume-ub16/le (context)
  (consume-word/le context 2 nil))

(defun consume-ub32/le (context)
  (consume-word/le context 4 nil))

(defun consume-ub64/le (context)
  (consume-word/le context 8 nil))

(defun consume-sb8 (context)
  (sys.int::sign-extend (consume-octet context) 8))

(defun consume-sb16/le (context)
  (consume-word/le context 2 t))

(defun consume-sb32/le (context)
  (consume-word/le context 4 t))

(defun consume-sb64/le (context)
  (consume-word/le context 8 t))

(defun disassemble-modr/m (context info)
  ;; Returns reg, r/m, len.
  ;; r/m will either be an undecoded register or a decoded effective address.
  (let* ((modr/m (consume-ub8 context))
         (rex-b (rex-b info))
         (rex-x (rex-x info))
         (asize (getf info :asize)))
    (values
     (ldb +modr/m-reg+ modr/m)
     (ecase (ldb +modr/m-mod+ modr/m)
       (#b00
        (case (ldb +modr/m-r/m+ modr/m)
          (#b100
           (let ((sib (consume-ub8 context)))
             (cond ((eql (ldb +sib-base+ sib) 5)
                    ;; No base, disp32 follows.
                    (let ((disp32 (consume-sb32/le context)))
                      (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                             (make-instance 'effective-address
                                            :disp disp32
                                            :segment (getf info :segment)))
                            (t
                             (make-instance 'effective-address
                                            :index (if asize
                                                       (decode-gpr32 (ldb +sib-index+ sib) rex-x)
                                                       (decode-gpr64 (ldb +sib-index+ sib) rex-x))
                                            :scale (ash 1 (ldb +sib-ss+ sib))
                                            :disp disp32
                                            :segment (getf info :segment))))))
                   (t
                    (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                           (make-instance 'effective-address
                                          :base (if asize
                                                    (decode-gpr32 (ldb +sib-base+ sib) rex-b)
                                                    (decode-gpr64 (ldb +sib-base+ sib) rex-b))
                                          :segment (getf info :segment)))
                          (t
                           (make-instance 'effective-address
                                          :base (if asize
                                                    (decode-gpr32 (ldb +sib-base+ sib) rex-b)
                                                    (decode-gpr64 (ldb +sib-base+ sib) rex-b))
                                          :index (if asize
                                                     (decode-gpr32 (ldb +sib-index+ sib) rex-x)
                                                     (decode-gpr64 (ldb +sib-index+ sib) rex-x))
                                          :scale (ash 1 (ldb +sib-ss+ sib))
                                          :segment (getf info :segment))))))))
          (#b101
           (let ((disp32 (consume-sb32/le context)))
             (make-instance 'effective-address
                            :base :rip
                            :disp disp32
                            :segment (getf info :segment))))
          (t
           (make-instance 'effective-address
                          :base (if asize
                                    (decode-gpr32 (ldb +modr/m-r/m+ modr/m) rex-b)
                                    (decode-gpr64 (ldb +modr/m-r/m+ modr/m) rex-b))
                          :segment (getf info :segment)))))
       (#b01
        (case (ldb +modr/m-r/m+ modr/m)
          (#b100
           (let ((sib (consume-ub8 context))
                 (disp8 (consume-sb8 context)))
             (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                    (make-instance 'effective-address
                                   :base (if asize
                                             (decode-gpr32 (ldb +sib-base+ sib) rex-b)
                                             (decode-gpr64 (ldb +sib-base+ sib) rex-b))
                                   :disp disp8
                                   :segment (getf info :segment)))
                   (t
                    (make-instance 'effective-address
                                   :base (if asize
                                             (decode-gpr32 (ldb +sib-base+ sib) rex-b)
                                             (decode-gpr64 (ldb +sib-base+ sib) rex-b))
                                   :index (if asize
                                              (decode-gpr32 (ldb +sib-index+ sib) rex-x)
                                              (decode-gpr64 (ldb +sib-index+ sib) rex-x))
                                   :scale (ash 1 (ldb +sib-ss+ sib))
                                   :disp disp8
                                   :segment (getf info :segment))))))
          (t
           (let ((disp8 (consume-sb8 context)))
             (make-instance 'effective-address
                            :base (if asize
                                      (decode-gpr32 (ldb +modr/m-r/m+ modr/m) rex-b)
                                      (decode-gpr64 (ldb +modr/m-r/m+ modr/m) rex-b))
                            :disp disp8
                            :segment (getf info :segment))))))
       (#b10
        (case (ldb +modr/m-r/m+ modr/m)
          (#b100
           (let ((sib (consume-ub8 context))
                 (disp32 (consume-sb32/le context)))
             (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                    (make-instance 'effective-address
                                   :base (if asize
                                             (decode-gpr32 (ldb +sib-base+ sib) rex-b)
                                             (decode-gpr64 (ldb +sib-base+ sib) rex-b))
                                   :disp disp32
                                   :segment (getf info :segment)))
                   (t
                    (make-instance 'effective-address
                                   :base (if asize
                                             (decode-gpr32 (ldb +sib-base+ sib) rex-b)
                                             (decode-gpr64 (ldb +sib-base+ sib) rex-b))
                                   :index (if asize
                                              (decode-gpr32 (ldb +sib-index+ sib) rex-x)
                                              (decode-gpr64 (ldb +sib-index+ sib) rex-x))
                                   :scale (ash 1 (ldb +sib-ss+ sib))
                                   :disp disp32
                                   :segment (getf info :segment))))))
          (t
           (let ((disp32 (consume-sb32/le context)))
             (make-instance 'effective-address
                            :base (if asize
                                      (decode-gpr32 (ldb +modr/m-r/m+ modr/m) rex-b)
                                      (decode-gpr64 (ldb +modr/m-r/m+ modr/m) rex-b))
                            :disp disp32
                            :segment (getf info :segment))))))
       (#b11
        (ldb +modr/m-r/m+ modr/m))))))

(defparameter *instruction-table*
  #((decode-eb-gb sys.lap-x86:add8) ; 00
    (decode-ev-gv sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    (decode-gb-eb sys.lap-x86:add8)
    (decode-gv-ev sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    (decode-al-ib sys.lap-x86:add8)
    (decode-ax-iz sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:or8) ; 08
    (decode-ev-gv sys.lap-x86:or16 sys.lap-x86:or32 sys.lap-x86:or64)
    (decode-gb-eb sys.lap-x86:or8)
    (decode-gv-ev sys.lap-x86:or16 sys.lap-x86:or32 sys.lap-x86:or64)
    (decode-al-ib sys.lap-x86:or8)
    (decode-ax-iz sys.lap-x86:or16 sys.lap-x86:or32 sys.lap-x86:or64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:adc8) ; 10
    (decode-ev-gv sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    (decode-gb-eb sys.lap-x86:adc8)
    (decode-gv-ev sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    (decode-al-ib sys.lap-x86:adc8)
    (decode-ax-iz sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:sbb8) ; 18
    (decode-ev-gv sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    (decode-gb-eb sys.lap-x86:sbb8)
    (decode-gv-ev sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    (decode-al-ib sys.lap-x86:sbb8)
    (decode-ax-iz sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:and8) ; 20
    (decode-ev-gv sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    (decode-gb-eb sys.lap-x86:and8)
    (decode-gv-ev sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    (decode-al-ib sys.lap-x86:and8)
    (decode-ax-iz sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:sub8) ; 28
    (decode-ev-gv sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    (decode-gb-eb sys.lap-x86:sub8)
    (decode-gv-ev sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    (decode-al-ib sys.lap-x86:sub8)
    (decode-ax-iz sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:xor8) ; 30
    (decode-ev-gv sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    (decode-gb-eb sys.lap-x86:xor8)
    (decode-gv-ev sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    (decode-al-ib sys.lap-x86:xor8)
    (decode-ax-iz sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:cmp8) ; 38
    (decode-ev-gv sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)
    (decode-gb-eb sys.lap-x86:cmp8)
    (decode-gv-ev sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)
    (decode-al-ib sys.lap-x86:cmp8)
    (decode-ax-iz sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)
    nil
    nil
    nil ; 40 (start of REX)
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 48
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    (decode+rgv64 sys.lap-x86:push) ; 50
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:pop) ; 58
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    nil ; 60 (various prefixes and obsolete instructions)
    nil
    nil
    (decode-movsx32)
    nil
    nil
    nil
    nil
    (decode-iz sys.lap-x86:push) ; 68
    (decode-gv-ev-iz sys.lap-x86:imul16 sys.lap-x86:imul32 sys.lap-x86:imul64)
    (decode-ib sys.lap-x86:push)
    (decode-gv-ev-ib sys.lap-x86:imul16 sys.lap-x86:imul32 sys.lap-x86:imul64)
    nil
    nil
    nil
    nil
    (decode-jb sys.lap-x86:jo) ; 70
    (decode-jb sys.lap-x86:jno)
    (decode-jb sys.lap-x86:jb)
    (decode-jb sys.lap-x86:jae)
    (decode-jb sys.lap-x86:jz)
    (decode-jb sys.lap-x86:jnz)
    (decode-jb sys.lap-x86:jbe)
    (decode-jb sys.lap-x86:ja)
    (decode-jb sys.lap-x86:js) ; 78
    (decode-jb sys.lap-x86:jns)
    (decode-jb sys.lap-x86:jp)
    (decode-jb sys.lap-x86:jnp)
    (decode-jb sys.lap-x86:jl)
    (decode-jb sys.lap-x86:jge)
    (decode-jb sys.lap-x86:jle)
    (decode-jb sys.lap-x86:jg)
    (decode-eb-ib *group-1*) ; 80
    (decode-ev-iz *group-1*)
    nil
    (decode-ev-ib *group-1*)
    (decode-eb-gb sys.lap-x86:test8)
    (decode-ev-gv sys.lap-x86:test16 sys.lap-x86:test32 sys.lap-x86:test64)
    (decode-eb-gb sys.lap-x86:xchg8)
    (decode-ev-gv sys.lap-x86:xchg16 sys.lap-x86:xchg32 sys.lap-x86:xchg64)
    (decode-eb-gb sys.lap-x86:mov8) ; 88
    (decode-ev-gv sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-gb-eb sys.lap-x86:mov8)
    (decode-gv-ev sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-ev-sw)
    (decode-gv-ev nil sys.lap-x86:lea32 sys.lap-x86:lea64)
    (decode-sw-ev)
    (decode-group-1a)
    (decode-xchg+r) ; 90
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    nil ; 98
    (decode-simple-sized sys.lap-x86:cwd sys.lap-x86:cdq sys.lap-x86:cqo)
    nil
    (decode-simple sys.lap-x86:fwait)
    (decode-simple sys.lap-x86:pushf)
    (decode-simple sys.lap-x86:popf)
    (decode-simple sys.lap-x86:sahf)
    (decode-simple sys.lap-x86:lahf)
    (decode-al-ob sys.lap-x86:mov8) ; A0
    (decode-ax-ov sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-ob-al sys.lap-x86:mov8)
    (decode-ov-ax sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-simple sys.lap-x86:movs8)
    (decode-simple-sized sys.lap-x86:movs16 sys.lap-x86:movs32 sys.lap-x86:movs64)
    (decode-simple sys.lap-x86:cmps8)
    (decode-simple-sized sys.lap-x86:cmps16 sys.lap-x86:cmps32 sys.lap-x86:cmps64)
    (decode-al-ib sys.lap-x86:test8) ; A8
    (decode-ax-iz sys.lap-x86:test16 sys.lap-x86:test32 sys.lap-x86:test64)
    (decode-simple sys.lap-x86:stos8)
    (decode-simple-sized sys.lap-x86:stos16 sys.lap-x86:stos32 sys.lap-x86:stos64)
    (decode-simple sys.lap-x86:lods8)
    (decode-simple-sized sys.lap-x86:lods16 sys.lap-x86:lods32 sys.lap-x86:lods64)
    (decode-simple sys.lap-x86:scas8)
    (decode-simple-sized sys.lap-x86:scas16 sys.lap-x86:scas32 sys.lap-x86:scas64)
    (decode-mov+r-ib) ; B0
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-iv) ; B8
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-eb-ib *group-2*) ; C0
    (decode-ev-ib *group-2*)
    nil
    (decode-simple sys.lap-x86:ret)
    nil
    nil
    (decode-eb-ib *group-11*)
    (decode-ev-iz *group-11*)
    nil ; C8
    (decode-simple sys.lap-x86:leave)
    nil
    (decode-simple sys.lap-x86:retf)
    nil
    (decode-ib sys.lap-x86:int)
    nil
    (decode-simple sys.lap-x86:iret)
    (decode-eb-1 *group-2*) ; D0
    (decode-ev-1 *group-2*)
    (decode-eb-cl *group-2*)
    (decode-ev-cl *group-2*)
    nil
    nil
    nil
    nil
    nil ; D8
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; E0
    nil
    nil
    nil
    (decode-io sys.lap-x86:in8 sys.lap-x86:in8 nil)
    (decode-io sys.lap-x86:in16 sys.lap-x86:in32 nil)
    (decode-io sys.lap-x86:out8 sys.lap-x86:out8 nil)
    (decode-io sys.lap-x86:out16 sys.lap-x86:out32 nil)
    (decode-jz sys.lap-x86:call) ; E8
    (decode-jz sys.lap-x86:jmp)
    nil
    (decode-jb sys.lap-x86:jmp)
    (decode-io sys.lap-x86:in8 sys.lap-x86:in8 :dx)
    (decode-io sys.lap-x86:in16 sys.lap-x86:in32 :dx)
    (decode-io sys.lap-x86:out8 sys.lap-x86:out8 :dx)
    (decode-io sys.lap-x86:out16 sys.lap-x86:out32 :dx)
    nil ; F0
    nil
    nil
    nil
    (decode-simple sys.lap-x86:hlt)
    (decode-simple sys.lap-x86:cmc)
    (decode-group-3-eb)
    (decode-group-3-ev)
    (decode-simple sys.lap-x86:clc) ; F8
    (decode-simple sys.lap-x86:stc)
    (decode-simple sys.lap-x86:cli)
    (decode-simple sys.lap-x86:sti)
    (decode-simple sys.lap-x86:cld)
    (decode-simple sys.lap-x86:std)
    nil
    (decode-ev64 *group-5*)))

(defparameter *extended-instruction-table*
  #((decode-group-6) ; 00
    (decode-group-7)
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 08
    nil
    nil
    (decode-simple sys.lap-x86:ud2)
    nil
    nil
    nil
    nil
    (decode-v-w nil ; sys.lap-x86:movups
     nil ; sys.lap-x86:movupd
     sys.lap-x86:movsd sys.lap-x86:movss) ; 10
    (decode-w-v nil ; sys.lap-x86:movups
     nil ; sys.lap-x86:movupd
     sys.lap-x86:movsd sys.lap-x86:movss)
    ;; FIXME: For MOVHLPS, if the R/M operand is memory then the instruction is actually MOVLPS.
    (decode-v-w sys.lap-x86:movhlps nil nil nil)
    nil
    (decode-v-w sys.lap-x86:unpcklps sys.lap-x86:unpcklpd nil nil)
    (decode-v-w sys.lap-x86:unpckhps sys.lap-x86:unpckhpd nil nil)
    (decode-v-w sys.lap-x86:movlhps nil nil nil)
    nil
    nil ; 18
    nil
    nil
    nil
    nil
    nil
    nil
    (decode-nop-ev)
    (decode-mov-r-c) ; 20
    (decode-mov-r-d)
    (decode-mov-c-r)
    (decode-mov-d-r)
    nil
    nil
    nil
    nil
    nil ; 28
    nil
    (decode-cvt-2a)
    nil
    (decode-cvt-2c)
    (decode-cvt-2d)
    (decode-v-w sys.lap-x86:ucomiss sys.lap-x86:ucomisd nil nil)
    nil
    (decode-simple sys.lap-x86:wrmsr) ; 30
    (decode-simple sys.lap-x86:rdtsc)
    (decode-simple sys.lap-x86:rdmsr)
    (decode-simple sys.lap-x86:rdpmc)
    (decode-simple sys.lap-x86:sysenter)
    (decode-simple sys.lap-x86:sysexit)
    nil
    nil
    nil ; 38
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    (decode-gv-ev sys.lap-x86:cmov16o sys.lap-x86:cmov32o sys.lap-x86:cmov64o) ; 40
    (decode-gv-ev sys.lap-x86:cmov16no sys.lap-x86:cmov32no sys.lap-x86:cmov64no)
    (decode-gv-ev sys.lap-x86:cmov16b sys.lap-x86:cmov32b sys.lap-x86:cmov64b)
    (decode-gv-ev sys.lap-x86:cmov16ae sys.lap-x86:cmov32ae sys.lap-x86:cmov64ae)
    (decode-gv-ev sys.lap-x86:cmov16z sys.lap-x86:cmov32z sys.lap-x86:cmov64z)
    (decode-gv-ev sys.lap-x86:cmov16nz sys.lap-x86:cmov32nz sys.lap-x86:cmov64nz)
    (decode-gv-ev sys.lap-x86:cmov16be sys.lap-x86:cmov32be sys.lap-x86:cmov64be)
    (decode-gv-ev sys.lap-x86:cmov16a sys.lap-x86:cmov32a sys.lap-x86:cmov64a)
    (decode-gv-ev sys.lap-x86:cmov16s sys.lap-x86:cmov32s sys.lap-x86:cmov64s) ; 48
    (decode-gv-ev sys.lap-x86:cmov16ns sys.lap-x86:cmov32ns sys.lap-x86:cmov64ns)
    (decode-gv-ev sys.lap-x86:cmov16p sys.lap-x86:cmov32p sys.lap-x86:cmov64p)
    (decode-gv-ev sys.lap-x86:cmov16np sys.lap-x86:cmov32np sys.lap-x86:cmov64np)
    (decode-gv-ev sys.lap-x86:cmov16l sys.lap-x86:cmov32l sys.lap-x86:cmov64l)
    (decode-gv-ev sys.lap-x86:cmov16ge sys.lap-x86:cmov32ge sys.lap-x86:cmov64ge)
    (decode-gv-ev sys.lap-x86:cmov16le sys.lap-x86:cmov32le sys.lap-x86:cmov64le)
    (decode-gv-ev sys.lap-x86:cmov16g sys.lap-x86:cmov32g sys.lap-x86:cmov64g)
    nil ; 50
    (decode-v-w sys.lap-x86:sqrtps sys.lap-x86:sqrtpd sys.lap-x86:sqrtsd sys.lap-x86:sqrtss)
    nil
    nil
    (decode-v-w sys.lap-x86:andps sys.lap-x86:andpd nil nil)
    (decode-v-w sys.lap-x86:andnps sys.lap-x86:andnpd nil nil)
    (decode-v-w sys.lap-x86:orps sys.lap-x86:orpd nil nil)
    (decode-v-w sys.lap-x86:xorps sys.lap-x86:xorpd nil nil)
    (decode-v-w sys.lap-x86:addps sys.lap-x86:addpd sys.lap-x86:addsd sys.lap-x86:addss) ; 58
    (decode-v-w sys.lap-x86:mulps sys.lap-x86:mulpd sys.lap-x86:mulsd sys.lap-x86:mulss)
    (decode-v-w sys.lap-x86:cvtps2pd sys.lap-x86:cvtpd2ps sys.lap-x86:cvtsd2ss sys.lap-x86:cvtss2sd)
    (decode-v-w sys.lap-x86:cvtdq2ps sys.lap-x86:cvtps2dq nil sys.lap-x86:cvttps2dq)
    (decode-v-w sys.lap-x86:subps sys.lap-x86:subpd sys.lap-x86:subsd sys.lap-x86:subss)
    (decode-v-w sys.lap-x86:minps sys.lap-x86:minpd sys.lap-x86:minsd sys.lap-x86:minss)
    (decode-v-w sys.lap-x86:divps sys.lap-x86:divpd sys.lap-x86:divsd sys.lap-x86:divss)
    (decode-v-w sys.lap-x86:maxps sys.lap-x86:maxpd sys.lap-x86:maxsd sys.lap-x86:maxss)
    (decode-pq-qd sys.lap-x86:punpcklbw sys.lap-x86:punpcklbw nil nil) ; 60
    (decode-pq-qd sys.lap-x86:punpcklwd sys.lap-x86:punpcklwd nil nil)
    (decode-pq-qd sys.lap-x86:punpckldq sys.lap-x86:punpckldq nil nil)
    (decode-pq-qd sys.lap-x86:packsswb sys.lap-x86:packsswb nil nil)
    (decode-pq-qd sys.lap-x86:pcmpgtb sys.lap-x86:pcmpgtb nil nil)
    (decode-pq-qd sys.lap-x86:pcmpgtw sys.lap-x86:pcmpgtw nil nil)
    (decode-pq-qd sys.lap-x86:pcmpgtd sys.lap-x86:pcmpgtd nil nil)
    (decode-pq-qd sys.lap-x86:packuswb sys.lap-x86:packuswb nil nil)
    (decode-pq-qd sys.lap-x86:punpckhbw sys.lap-x86:punpckhbw nil nil) ; 68
    (decode-pq-qd sys.lap-x86:punpckhwd sys.lap-x86:punpckhwd nil nil)
    (decode-pq-qd sys.lap-x86:punpckhdq sys.lap-x86:punpckhdq nil nil)
    (decode-pq-qd sys.lap-x86:packssdw sys.lap-x86:packssdw nil nil)
    nil
    nil
    (decode-pd-ed/q sys.lap-x86:movd sys.lap-x86:movq)
    (decode-pq-qq sys.lap-x86:movq sys.lap-x86:movdqa nil sys.lap-x86:movdqu)
    nil ; 70
    (decode-simd-shift #(nil nil sys.lap-x86:psrlw nil sys.lap-x86:psraw nil sys.lap-x86:psllw nil))
    (decode-simd-shift #(nil nil sys.lap-x86:psrld nil sys.lap-x86:psrad nil sys.lap-x86:pslld nil))
    (decode-simd-shift #(nil nil sys.lap-x86:psrlq nil nil nil sys.lap-x86:psllq nil))
    (decode-pq-qd sys.lap-x86:pcmpeqb sys.lap-x86:pcmpeqb nil nil)
    (decode-pq-qd sys.lap-x86:pcmpeqw sys.lap-x86:pcmpeqw nil nil)
    (decode-pq-qd sys.lap-x86:pcmpeqd sys.lap-x86:pcmpeqd nil nil)
    nil
    nil ; 78
    nil
    nil
    nil
    nil
    nil
    (decode-ed/q-pd sys.lap-x86:movd sys.lap-x86:movq)
    (decode-qq-pq sys.lap-x86:movq sys.lap-x86:movdqa nil sys.lap-x86:movdqu)
    (decode-jz sys.lap-x86:jo) ; 80
    (decode-jz sys.lap-x86:jno)
    (decode-jz sys.lap-x86:jb)
    (decode-jz sys.lap-x86:jae)
    (decode-jz sys.lap-x86:jz)
    (decode-jz sys.lap-x86:jnz)
    (decode-jz sys.lap-x86:jbe)
    (decode-jz sys.lap-x86:ja)
    (decode-jz sys.lap-x86:js) ; 88
    (decode-jz sys.lap-x86:jns)
    (decode-jz sys.lap-x86:jp)
    (decode-jz sys.lap-x86:jnp)
    (decode-jz sys.lap-x86:jl)
    (decode-jz sys.lap-x86:jge)
    (decode-jz sys.lap-x86:jle)
    (decode-jz sys.lap-x86:jg)
    nil ; 90
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 98
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; A0
    nil
    (decode-simple sys.lap-x86:cpuid)
    (decode-ev-gv sys.lap-x86:bt16 sys.lap-x86:bt32 sys.lap-x86:bt64)
    (decode-ev-gv-ib sys.lap-x86:shld16 sys.lap-x86:shld32 sys.lap-x86:shld64)
    (decode-ev-gv-cl sys.lap-x86:shld16 sys.lap-x86:shld32 sys.lap-x86:shld64)
    nil
    nil
    nil ; A8
    nil
    nil
    (decode-ev-gv sys.lap-x86:bts16 sys.lap-x86:bts32 sys.lap-x86:bts64)
    (decode-ev-gv-ib sys.lap-x86:shrd16 sys.lap-x86:shrd32 sys.lap-x86:shrd64)
    (decode-ev-gv-cl sys.lap-x86:shrd16 sys.lap-x86:shrd32 sys.lap-x86:shrd64)
    (decode-group-15)
    nil
    (decode-eb-gb sys.lap-x86:cmpxchg) ; B0
    (decode-ev-gv sys.lap-x86:cmpxchg sys.lap-x86:cmpxchg sys.lap-x86:cmpxchg)
    nil
    (decode-ev-gv sys.lap-x86:btr16 sys.lap-x86:btr32 sys.lap-x86:btr64)
    nil
    nil
    (decode-movzx8)
    (decode-movzx16)
    nil ; B8
    nil
    (decode-ev-ib *group-8*)
    (decode-ev-gv sys.lap-x86:btc16 sys.lap-x86:btc32 sys.lap-x86:btc64)
    nil
    nil
    (decode-movsx8)
    (decode-movsx16)
    (decode-eb-gb sys.lap-x86:xadd8) ; C0
    (decode-ev-gv sys.lap-x86:xadd16 sys.lap-x86:xadd32 sys.lap-x86:xadd64)
    (decode-sse-cmp)
    nil
    nil
    nil
    (decode-v-w-ib sys.lap-x86:shufps sys.lap-x86:shufpd nil nil)
    (decode-group-9)
    (decode-bswap) ; C8
    (decode-bswap)
    (decode-bswap)
    (decode-bswap)
    (decode-bswap)
    (decode-bswap)
    (decode-bswap)
    (decode-bswap)
    nil ; D0
    (decode-pq-qq sys.lap-x86:psrlw sys.lap-x86:psrlw nil nil)
    (decode-pq-qq sys.lap-x86:psrld sys.lap-x86:psrld nil nil)
    (decode-pq-qq sys.lap-x86:psrlq sys.lap-x86:psrlq nil nil)
    (decode-pq-qq sys.lap-x86:paddq sys.lap-x86:paddq nil nil)
    (decode-pq-qq sys.lap-x86:pmullw sys.lap-x86:pmullw nil nil)
    nil
    nil
    (decode-pq-qq sys.lap-x86:psubusb sys.lap-x86:psubusb nil nil) ; D8
    (decode-pq-qq sys.lap-x86:psubusw sys.lap-x86:psubusw nil nil)
    (decode-pq-qq sys.lap-x86:pminub sys.lap-x86:pminub nil nil)
    (decode-pq-qq sys.lap-x86:pand sys.lap-x86:pand nil nil)
    (decode-pq-qq sys.lap-x86:paddusb sys.lap-x86:paddusb nil nil)
    (decode-pq-qq sys.lap-x86:paddusw sys.lap-x86:paddusw nil nil)
    (decode-pq-qq sys.lap-x86:pmaxub sys.lap-x86:pmaxub nil nil)
    (decode-pq-qq sys.lap-x86:pandn sys.lap-x86:pandn nil nil)
    (decode-pq-qq sys.lap-x86:pavgb sys.lap-x86:pavgb nil nil) ; E0
    (decode-pq-qq sys.lap-x86:psraw sys.lap-x86:psraw nil nil)
    (decode-pq-qq sys.lap-x86:psrad sys.lap-x86:psrad nil nil)
    (decode-pq-qq sys.lap-x86:pavgw sys.lap-x86:pavgw nil nil)
    (decode-pq-qq sys.lap-x86:pmulhuw sys.lap-x86:pmulhuw nil nil)
    (decode-pq-qq sys.lap-x86:pmulhw sys.lap-x86:pmulhw nil nil)
    nil
    nil
    (decode-pq-qq sys.lap-x86:psubsb sys.lap-x86:psubsb nil nil) ; E8
    (decode-pq-qq sys.lap-x86:psubsw sys.lap-x86:psubsw nil nil)
    (decode-pq-qq sys.lap-x86:pminsw sys.lap-x86:pminsw nil nil)
    (decode-pq-qq sys.lap-x86:por sys.lap-x86:por nil nil)
    (decode-pq-qq sys.lap-x86:paddsb sys.lap-x86:paddsb nil nil)
    (decode-pq-qq sys.lap-x86:paddsw sys.lap-x86:paddsw nil nil)
    (decode-pq-qq sys.lap-x86:pmaxsw sys.lap-x86:pmaxsw nil nil)
    (decode-pq-qq sys.lap-x86:pxor sys.lap-x86:pxor nil nil)
    nil ; F0
    (decode-pq-qq sys.lap-x86:psllw sys.lap-x86:psllw nil nil)
    (decode-pq-qq sys.lap-x86:pslld sys.lap-x86:pslld nil nil)
    (decode-pq-qq sys.lap-x86:psllq sys.lap-x86:psllq nil nil)
    (decode-pq-qq sys.lap-x86:pmuludq sys.lap-x86:pmuludq nil nil)
    (decode-pq-qq sys.lap-x86:pmaddwd sys.lap-x86:pmaddwd nil nil)
    (decode-pq-qq sys.lap-x86:psadbw sys.lap-x86:psadbw nil nil)
    nil
    (decode-pq-qq sys.lap-x86:psubb sys.lap-x86:psubb nil nil) ; F8
    (decode-pq-qq sys.lap-x86:psubw sys.lap-x86:psubw nil nil)
    (decode-pq-qq sys.lap-x86:psubd sys.lap-x86:psubd nil nil)
    (decode-pq-qq sys.lap-x86:psubq sys.lap-x86:psubq nil nil)
    (decode-pq-qq sys.lap-x86:paddb sys.lap-x86:paddb nil nil)
    (decode-pq-qq sys.lap-x86:paddw sys.lap-x86:paddw nil nil)
    (decode-pq-qq sys.lap-x86:paddd sys.lap-x86:paddd nil nil)
    nil))

(defparameter *group-1*
  #((sys.lap-x86:add8 sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    (sys.lap-x86:or8  sys.lap-x86:or16  sys.lap-x86:or32  sys.lap-x86:or64)
    (sys.lap-x86:adc8 sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    (sys.lap-x86:sbb8 sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    (sys.lap-x86:and8 sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    (sys.lap-x86:sub8 sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    (sys.lap-x86:xor8 sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    (sys.lap-x86:cmp8 sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)))

(defparameter *group-2*
  #((sys.lap-x86:rol8 sys.lap-x86:rol16 sys.lap-x86:rol32 sys.lap-x86:rol64)
    (sys.lap-x86:ror8 sys.lap-x86:ror16 sys.lap-x86:ror32 sys.lap-x86:ror64)
    (sys.lap-x86:rcl8 sys.lap-x86:rcl16 sys.lap-x86:rcl32 sys.lap-x86:rcl64)
    (sys.lap-x86:rcr8 sys.lap-x86:rcr16 sys.lap-x86:rcr32 sys.lap-x86:rcr64)
    (sys.lap-x86:shl8 sys.lap-x86:shl16 sys.lap-x86:shl32 sys.lap-x86:shl64)
    (sys.lap-x86:shr8 sys.lap-x86:shr16 sys.lap-x86:shr32 sys.lap-x86:shr64)
    nil
    (sys.lap-x86:sar8 sys.lap-x86:sar16 sys.lap-x86:sar32 sys.lap-x86:sar64)))

(defparameter *group-5*
  #(nil
    nil
    sys.lap-x86:call
    nil
    sys.lap-x86:jmp
    nil
    sys.lap-x86:push
    nil))

(defparameter *group-8*
  #(nil
    nil
    nil
    nil
    (nil sys.lap-x86:bt16 sys.lap-x86:bt32 sys.lap-x86:bt64)
    (nil sys.lap-x86:bts16 sys.lap-x86:bts32 sys.lap-x86:bts64)
    (nil sys.lap-x86:btr16 sys.lap-x86:btr32 sys.lap-x86:btr64)
    (nil sys.lap-x86:btc16 sys.lap-x86:btc32 sys.lap-x86:btc64)))

(defparameter *group-11*
  #((sys.lap-x86:mov8 sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    nil
    nil
    nil
    nil
    nil
    nil
    nil))

(defun rex-w (info)
  (logbitp 3 (getf info :rex 0)))

(defun rex-r (info)
  (logbitp 2 (getf info :rex 0)))

(defun rex-x (info)
  (logbitp 1 (getf info :rex 0)))

(defun rex-b (info)
  (logbitp 0 (getf info :rex 0)))

(defun operand-size (info)
  (cond ((rex-w info) 64)
        ((getf info :osize) 16)
        (t 32)))

(defun decode+rgv64 (context info opcode)
  (declare (ignore context))
  (make-instruction opcode
                    (decode-gpr64 (ldb (byte 3 0) (getf info :opcode))
                                  (rex-b info))))

(defun decode-bswap (context info)
  (declare (ignore context))
  (let ((rex-b (rex-b info))
        (reg (ldb (byte 3 0) (getf info :opcode))))
    (ecase (operand-size info)
      (64 (make-instruction 'sys.lap-x86:bswap (decode-gpr64 reg rex-b)))
      (32 (make-instruction 'sys.lap-x86:bswap (decode-gpr32 reg rex-b)))
      (16 nil))))

(defun decode-ib (context info opcode)
  (declare (ignore info))
  (make-instruction opcode
                    (consume-sb8 context)))

(defun decode-iz (context info opcode)
  (declare (ignore info))
  (make-instruction opcode
                    (consume-sb32/le context)))

(defmacro define-modr/m-decoder (name (context info opcode r/m reg) &body body)
  `(defun ,name (,context ,info opcode16 opcode32 opcode64)
     (multiple-value-bind (,reg ,r/m)
         (disassemble-modr/m ,context ,info)
       (ecase (operand-size ,info)
         (64 (and opcode64
                  (let ((,opcode opcode64)
                        (,reg (decode-gpr64 ,reg (rex-r ,info)))
                        (,r/m (decode-gpr64-or-mem ,r/m (rex-b ,info))))
                    ,@body)))
         (32 (and opcode32
                  (let ((,opcode opcode32)
                        (,reg (decode-gpr32 ,reg (rex-r ,info)))
                        (,r/m (decode-gpr32-or-mem ,r/m (rex-b ,info))))
                    ,@body)))
         (16
          (and opcode16
               (let ((,opcode opcode16)
                     (,reg (decode-gpr16 ,reg (rex-r ,info)))
                     (,r/m (decode-gpr16-or-mem ,r/m (rex-b ,info))))
                 ,@body)))))))

(define-modr/m-decoder decode-ev-gv-ib (context info opcode r/m reg)
  (let ((imm (consume-sb8 context)))
    (make-instruction opcode r/m reg imm)))

(define-modr/m-decoder decode-ev-gv-cl (context info opcode r/m reg)
  (make-instruction opcode r/m reg :cl))

(define-modr/m-decoder decode-ev-gv (context info opcode r/m reg)
  (make-instruction opcode r/m reg))

(defun decode-ev-sw (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction 'sys.lap-x86:movseg
                      (ecase (operand-size info)
                        (64 (decode-gpr64-or-mem r/m (rex-b info)))
                        (32 (decode-gpr32-or-mem r/m (rex-b info)))
                        (16 (decode-gpr16-or-mem r/m (rex-b info))))
                      (decode-seg reg))))

(defun decode-sw-ev (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction 'sys.lap-x86:movseg
                      (decode-seg reg)
                      (ecase (operand-size info)
                        (64 (decode-gpr64-or-mem r/m (rex-b info)))
                        (32 (decode-gpr32-or-mem r/m (rex-b info)))
                        (16 (decode-gpr16-or-mem r/m (rex-b info)))))))

(define-modr/m-decoder decode-gv-ev-ib (context info opcode r/m reg)
  (let ((imm (consume-sb8 context)))
    (make-instruction opcode reg r/m imm)))

(define-modr/m-decoder decode-gv-ev-iz (context info opcode r/m reg)
  (let ((imm (consume-sb32/le context)))
    (make-instruction opcode reg r/m imm)))

(define-modr/m-decoder decode-gv-ev (context info opcode r/m reg)
  (make-instruction opcode reg r/m))

(defun decode-ev64 (context info group-table)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((opcode (aref (symbol-value group-table) reg)))
      (cond (opcode
             (make-instruction opcode
                               (decode-gpr64-or-mem r/m (rex-b info))))
            (t nil)))))

(defun decode-al-ib (context info opcode)
  (declare (ignore info))
  (make-instruction opcode :al (consume-ub8 context)))

(defun decode-ax-iz (context info opcode16 opcode32 opcode64)
  (let* ((osize (operand-size info))
         (imm (if (eql osize 16)
                  (consume-sb16/le context)
                  (consume-sb32/le context))))
    (ecase osize
      (64 (make-instruction opcode64 :rax imm))
      (32 (make-instruction opcode32 :eax imm))
      (16 (make-instruction opcode16 :ax imm)))))

(defun decode-eb-ib (context info group-table)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((opcodes (aref (symbol-value group-table) reg)))
      (cond (opcodes
             (and (first opcodes)
                  (make-instruction (first opcodes)
                                    (decode-gpr8-or-mem r/m (rex-b info))
                                    (consume-ub8 context))))
            (t nil)))))

(defun decode-eb-gb (context info opcode)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction opcode
                      (decode-gpr8-or-mem r/m (rex-b info))
                      (decode-gpr8 reg (rex-r info)))))

(defun decode-gb-eb (context info opcode)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction opcode
                      (decode-gpr8 reg (rex-r info))
                      (decode-gpr8-or-mem r/m (rex-b info)))))

(defun decode-eb-1 (context info group-table)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((opcodes (aref (symbol-value group-table) reg)))
      (cond (opcodes
             (and (first opcodes)
                  (make-instruction (fourth opcodes)
                                    (decode-gpr8-or-mem r/m (rex-b info))
                                    1)))
            (t nil)))))

(defun decode-eb-cl (context info group-table)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((opcodes (aref (symbol-value group-table) reg)))
      (cond (opcodes
             (and (first opcodes)
                  (make-instruction (fourth opcodes)
                                    (decode-gpr8-or-mem r/m (rex-b info))
                                    :cl)))
            (t nil)))))

(defmacro define-modr/m-group-decoder (name (context info opcode r/m reg) &body body)
  `(defun ,name (,context ,info group-table)
     (multiple-value-bind (,reg ,r/m)
         (disassemble-modr/m ,context ,info)
       (let ((opcodes (aref (symbol-value group-table) reg)))
         (ecase (operand-size ,info)
           (64 (and (fourth opcodes)
                    (let ((,opcode (fourth opcodes))
                          (,r/m (decode-gpr64-or-mem ,r/m (rex-b ,info))))
                      ,@body)))
           (32 (and (third opcodes)
                    (let ((,opcode (third opcodes))
                          (,r/m (decode-gpr32-or-mem ,r/m (rex-b ,info))))
                      ,@body)))
           (16
            (and (second opcodes)
                 (let ((,opcode (second opcodes))
                       (,r/m (decode-gpr16-or-mem ,r/m (rex-b ,info))))
                   ,@body))))))))

(define-modr/m-group-decoder decode-ev-1 (context info opcode r/m reg)
  (make-instruction opcode r/m 1))

(define-modr/m-group-decoder decode-ev-cl (context info opcode r/m reg)
  (make-instruction opcode r/m :cl))

(define-modr/m-group-decoder decode-ev-ib (context info opcode r/m reg)
  (let ((imm (consume-sb8 context)))
    (make-instruction opcode r/m imm)))

(define-modr/m-group-decoder decode-ev-iz (context info opcode r/m reg)
  (let ((imm (if (eql (operand-size info) 16)
                 (consume-sb16/le context)
                 (consume-sb32/le context))))
    (make-instruction opcode r/m imm)))

(defun decode-jb (context info opcode)
  (declare (ignore info))
  (make-instruction opcode (make-instance 'effective-address
                                          :base :rip
                                          :disp (consume-sb8 context))))

(defun decode-jz (context info opcode)
  (declare (ignore info))
  (make-instruction opcode (make-instance 'effective-address
                                          :base :rip
                                          :disp (consume-sb32/le context))))

(defun decode-group-1a (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (case reg
      (0
       (make-instruction 'sys.lap-x86:pop
                         (decode-gpr64-or-mem r/m (rex-b info))))
      (t nil))))

(defun decode-group-3-eb (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (case reg
      (0
       (let ((imm (consume-sb8 context)))
         (make-instruction 'sys.lap-x86:test8
                           (decode-gpr8-or-mem r/m (rex-b info))
                           imm)))
      (2
       (make-instruction 'sys.lap-x86:not8
                         (decode-gpr8-or-mem r/m (rex-b info))))
      (3
       (make-instruction 'sys.lap-x86:neg8
                         (decode-gpr8-or-mem r/m (rex-b info))))
      (4
       (make-instruction 'sys.lap-x86:mul8
                         (decode-gpr8-or-mem r/m (rex-b info))))
      (5
       (make-instruction 'sys.lap-x86:imul8
                         (decode-gpr8-or-mem r/m (rex-b info))))
      #+(or)
      (6
       (make-instruction 'sys.lap-x86:div8
                         (decode-gpr8-or-mem r/m (rex-b info))))
      (7
       (make-instruction 'sys.lap-x86:idiv8
                         (decode-gpr8-or-mem r/m (rex-b info))))
      (t nil))))

(defun decode-group-3-ev (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (multiple-value-bind (opcode16 opcode32 opcode64 has-imm)
        (case reg
          (0 (values 'sys.lap-x86:test16 'sys.lap-x86:test32 'sys.lap-x86:test64 t))
          (2 (values 'sys.lap-x86:not16 'sys.lap-x86:not32 'sys.lap-x86:not64))
          (3 (values 'sys.lap-x86:neg16 'sys.lap-x86:neg32 'sys.lap-x86:neg64))
          (4 (values 'sys.lap-x86:mul16 'sys.lap-x86:mul32 'sys.lap-x86:mul64))
          (5 (values 'sys.lap-x86:imul16 'sys.lap-x86:imul32 'sys.lap-x86:imul64))
          #+(or)
          (6 (values 'sys.lap-x86:div16 'sys.lap-x86:div32 'sys.lap-x86:div64))
          (7 (values 'sys.lap-x86:idiv16 'sys.lap-x86:idiv32 'sys.lap-x86:idiv64)))
      (ecase (operand-size info)
        (64
         (and opcode64
              (if has-imm
                  (make-instruction opcode64 (decode-gpr64-or-mem r/m (rex-b info)) (consume-sb32/le context))
                  (make-instruction opcode64 (decode-gpr64-or-mem r/m (rex-b info))))))
        (32
         (and opcode32
              (if has-imm
                  (make-instruction opcode32 (decode-gpr32-or-mem r/m (rex-b info)) (consume-sb32/le context))
                  (make-instruction opcode32 (decode-gpr32-or-mem r/m (rex-b info))))))
        (16
         (and opcode16
              (if has-imm
                  (make-instruction opcode16 (decode-gpr16-or-mem r/m (rex-b info)) (consume-sb16/le context))
                  (make-instruction opcode16 (decode-gpr16-or-mem r/m (rex-b info))))))))))

(defun decode-group-6 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((opcode (elt #(nil
                         sys.lap-x86:str
                         nil
                         sys.lap-x86:ltr
                         nil
                         nil
                         nil
                         nil)
                       reg)))
      (when opcode
        (make-instruction opcode
                          (decode-gpr64-or-mem r/m (rex-b info)))))))

(defun decode-group-7 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((opcode (elt #(sys.lap-x86:sgdt
                         sys.lap-x86:sidt
                         sys.lap-x86:lgdt
                         sys.lap-x86:lidt
                         nil
                         nil
                         nil
                         sys.lap-x86:invlpg)
                       reg)))
      (when opcode
        (cond ((and (eql opcode 'sys.lap-x86:invlpg)
                    (integerp r/m))
               (make-instruction 'sys.lap-x86:swapgs))
              (t
               (make-instruction opcode
                                 (decode-gpr64-or-mem r/m (rex-b info)))))))))

(defun decode-group-15 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((opcode (elt #(sys.lap-x86:fxsave
                         sys.lap-x86:fxrstor
                         sys.lap-x86:ldmxcsr
                         sys.lap-x86:stmxcsr
                         nil
                         nil
                         nil
                         nil)
                       reg)))
      (when opcode
        (make-instruction opcode
                          (decode-gpr64-or-mem r/m (rex-b info)))))))

(defun decode-group-9 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (case reg
      (1
       (if (rex-w info)
           (make-instruction 'sys.lap-x86:cmpxchg16b
                             (decode-gpr64-or-mem r/m (rex-b info)))
           #+(or)
           (make-instruction 'sys.lap-x86:cmpxchg8b
                             (decode-gpr64-or-mem r/m (rex-b info)))))
      (t nil))))

(defun decode-simd-shift (context info insts)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((imm (consume-ub8 context)))
      (let ((decoded-reg (if (getf info :osize)
                             (decode-xmm r/m (rex-b info))
                             (decode-mmx r/m)))
            (inst (elt insts reg)))
        (if inst
            (make-instruction inst decoded-reg imm)
            nil)))))

(defun decode-nop-ev (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (declare (ignore reg))
    (make-instruction 'sys.lap-x86:nop (decode-gpr64-or-mem r/m (rex-b info)))))

(defun decode-mov-r-c (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (when (integerp r/m)
      (make-instruction 'sys.lap-x86:movcr
                        (elt #(:cr0 :cr1 :cr2 :cr3 :cr4 :cr5 :cr6 :cr7) reg)
                        (decode-gpr64 r/m (rex-b info))))))

(defun decode-mov-c-r (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (when (integerp r/m)
      (make-instruction 'sys.lap-x86:movcr
                        (decode-gpr64 r/m (rex-b info))
                        (elt #(:cr0 :cr1 :cr2 :cr3 :cr4 :cr5 :cr6 :cr7) reg)))))

(defun decode-mov-r-d (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (when (integerp r/m)
      (make-instruction 'sys.lap-x86:movdr
                        (elt #(:dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7) reg)
                        (decode-gpr64 r/m (rex-b info))))))

(defun decode-mov-d-r (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (when (integerp r/m)
      (make-instruction 'sys.lap-x86:movdr
                        (decode-gpr64 r/m (rex-b info))
                        (elt #(:dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7) reg)))))

(defun decode-mov+r-ib (context info)
  (let ((reg (ldb (byte 3 0) (getf info :opcode))))
    (make-instruction 'sys.lap-x86:mov8
                      (decode-gpr8 reg (rex-b info))
                      (consume-ub8 context))))

(defun decode-mov+r-iv (context info)
  (let ((reg (ldb (byte 3 0) (getf info :opcode))))
    (ecase (operand-size info)
      (64 (make-instruction 'sys.lap-x86:mov64
                            (decode-gpr64 reg (rex-b info))
                            (consume-sb64/le context)))
      (32 (make-instruction 'sys.lap-x86:mov32
                            (decode-gpr32 reg (rex-b info))
                            (consume-sb32/le context)))
      (16 (make-instruction 'sys.lap-x86:mov16
                            (decode-gpr16 reg (rex-b info))
                            (consume-sb16/le context))))))

(defun decode-al-ob (context info opcode)
  (declare (ignore info))
  (make-instruction opcode
                    :al
                    (make-instance 'effective-address
                                   :disp (consume-sb64/le context))))

(defun decode-ax-ov (context info opcode16 opcode32 opcode64)
  (multiple-value-bind (opcode reg)
      (ecase (operand-size info)
        (64 (values opcode64 :rax))
        (32 (values opcode32 :eax))
        (16 (values opcode16 :ax)))
    (make-instruction opcode
                      reg
                      (make-instance 'effective-address
                                     :disp (consume-sb64/le context)))))

(defun decode-ob-al (context info opcode)
  (declare (ignore info))
  (make-instruction opcode
                    (make-instance 'effective-address
                                   :disp (consume-sb64/le context))
                    :al))

(defun decode-ov-ax (context info opcode16 opcode32 opcode64)
  (multiple-value-bind (opcode reg)
      (ecase (operand-size info)
        (64 (values opcode64 :rax))
        (32 (values opcode32 :eax))
        (16 (values opcode16 :ax)))
    (make-instruction opcode
                      (make-instance 'effective-address
                                     :disp (consume-sb64/le context))
                      reg)))

(defun decode-movzx8 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction 'sys.lap-x86:movzx8
                      (ecase (operand-size info)
                        (64 (decode-gpr64 reg (rex-r info)))
                        (32 (decode-gpr32 reg (rex-r info)))
                        (16 (decode-gpr16 reg (rex-r info))))
                      (decode-gpr8-or-mem r/m (rex-b info)))))

(defun decode-movzx16 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction 'sys.lap-x86:movzx16
                      (ecase (operand-size info)
                        (64 (decode-gpr64 reg (rex-r info)))
                        (32 (decode-gpr32 reg (rex-r info)))
                        (16 (decode-gpr16 reg (rex-r info))))
                      (decode-gpr16-or-mem r/m (rex-b info)))))

(defun decode-movsx8 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction 'sys.lap-x86:movsx8
                      (ecase (operand-size info)
                        (64 (decode-gpr64 reg (rex-r info)))
                        (32 (decode-gpr32 reg (rex-r info)))
                        (16 (decode-gpr16 reg (rex-r info))))
                      (decode-gpr8-or-mem r/m (rex-b info)))))

(defun decode-movsx16 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction 'sys.lap-x86:movsx16
                      (ecase (operand-size info)
                        (64 (decode-gpr64 reg (rex-r info)))
                        (32 (decode-gpr32 reg (rex-r info)))
                        (16 (decode-gpr16 reg (rex-r info))))
                      (decode-gpr16-or-mem r/m (rex-b info)))))

(defun decode-movsx32 (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction 'sys.lap-x86:movsx32
                      (ecase (operand-size info)
                        (64 (decode-gpr64 reg (rex-r info)))
                        (32 (decode-gpr32 reg (rex-r info)))
                        (16 (decode-gpr16 reg (rex-r info))))
                      (decode-gpr32-or-mem r/m (rex-b info)))))

(defun decode-xchg+r (context info)
  (declare (ignore context))
  (let ((reg (ldb (byte 3 0) (getf info :opcode))))
    (cond ((and (eql reg 0)
                (not (rex-w info))
                (not (rex-b info)))
           (if (eql (getf info :rep) #xF3)
               (make-instruction 'sys.lap-x86:pause)
               (make-instruction 'sys.lap-x86:nop)))
          (t
           (ecase (operand-size info)
             (64 (make-instruction 'sys.lap-x86:xchg64
                                   :rax
                                   (decode-gpr64 reg (rex-b info))))
             (32 (make-instruction 'sys.lap-x86:xchg32
                                   :eax
                                   (decode-gpr32 reg (rex-b info))))
             (16 (make-instruction 'sys.lap-x86:xchg16
                                   :ax
                                   (decode-gpr16 reg (rex-b info)))))))))

(defun decode-pd-ed/q (context info opcode32 opcode64)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction (if (rex-w info)
                          opcode64
                          opcode32)
                      (if (getf info :osize)
                          (decode-xmm reg (rex-r info))
                          (decode-mmx reg))
                      (if (rex-w info)
                          (decode-gpr64-or-mem r/m (rex-b info))
                          (decode-gpr32-or-mem r/m (rex-b info))))))

(defun decode-ed/q-pd (context info opcode32 opcode64)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (make-instruction (if (rex-w info)
                          opcode64
                          opcode32)
                      (if (eql (getf info :rep) #xF3)
                          (decode-xmm-or-mem r/m (rex-b info))
                          (if (rex-w info)
                              (decode-gpr64-or-mem r/m (rex-b info))
                              (decode-gpr32-or-mem r/m (rex-b info))))
                      (if (or (eql (getf info :rep) #xF3)
                              (getf info :osize))
                          (decode-xmm reg (rex-r info))
                          (decode-mmx reg)))))

(defun decode-pq-qd (context info opcode opcode-66 opcode-f2 opcode-f3)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond ((getf info :osize)
           (and opcode-66
                (make-instruction opcode-66
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF2)
           (and opcode-f2
                (make-instruction opcode-f2
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF3)
           (and opcode-f3
                (make-instruction opcode-f3
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          (t
           (and opcode
                (make-instruction opcode
                                  (decode-mmx reg)
                                  (decode-mmx-or-mem r/m)))))))

(defun decode-pq-qq (context info opcode opcode-66 opcode-f2 opcode-f3)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond ((getf info :osize)
           (and opcode-66
                (make-instruction opcode-66
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF2)
           (and opcode-f2
                (make-instruction opcode-f2
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF3)
           (and opcode-f3
                (make-instruction opcode-f3
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          (t
           (and opcode
                (make-instruction opcode
                                  (decode-mmx reg)
                                  (decode-mmx-or-mem r/m)))))))

(defun decode-qq-pq (context info opcode opcode-66 opcode-f2 opcode-f3)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond ((getf info :osize)
           (and opcode-66
                (make-instruction opcode-66
                                  (decode-xmm-or-mem r/m (rex-b info))
                                  (decode-xmm reg (rex-r info)))))
          ((eql (getf info :rep) #xF2)
           (and opcode-f2
                (make-instruction opcode-f2
                                  (decode-xmm-or-mem r/m (rex-b info))
                                  (decode-xmm reg (rex-r info)))))
          ((eql (getf info :rep) #xF3)
           (and opcode-f3
                (make-instruction opcode-f3
                                  (decode-xmm-or-mem r/m (rex-b info))
                                  (decode-xmm reg (rex-r info)))))
          (t
           (and opcode
                (make-instruction opcode
                                  (decode-mmx-or-mem r/m)
                                  (decode-mmx reg)))))))

(defun decode-w-v (context info opcode opcode-66 opcode-f2 opcode-f3)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond ((getf info :osize)
           (and opcode-66
                (make-instruction opcode-66
                                  (decode-xmm-or-mem r/m (rex-b info))
                                  (decode-xmm reg (rex-r info)))))
          ((eql (getf info :rep) #xF2)
           (and opcode-f2
                (make-instruction opcode-f2
                                  (decode-xmm-or-mem r/m (rex-b info))
                                  (decode-xmm reg (rex-r info)))))
          ((eql (getf info :rep) #xF3)
           (and opcode-f3
                (make-instruction opcode-f3
                                  (decode-xmm-or-mem r/m (rex-b info))
                                  (decode-xmm reg (rex-r info)))))
          (t
           (and opcode
                (make-instruction opcode
                                  (decode-xmm-or-mem r/m (rex-b info))
                                  (decode-xmm reg (rex-r info))))))))


(defun decode-v-w (context info opcode opcode-66 opcode-f2 opcode-f3)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond ((getf info :osize)
           (and opcode-66
                (make-instruction opcode-66
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF2)
           (and opcode-f2
                (make-instruction opcode-f2
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF3)
           (and opcode-f3
                (make-instruction opcode-f3
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info)))))
          (t
           (and opcode
                (make-instruction opcode
                                  (decode-xmm reg (rex-r info))
                                  (decode-xmm-or-mem r/m (rex-b info))))))))

(defun decode-v-w-ib (context info opcode opcode-66 opcode-f2 opcode-f3)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((imm (consume-ub8 context)))
      (cond ((getf info :osize)
             (and opcode-66
                  (make-instruction opcode-66
                                    (decode-xmm reg (rex-r info))
                                    (decode-xmm-or-mem r/m (rex-b info))
                                    imm)))
            ((eql (getf info :rep) #xF2)
             (and opcode-f2
                  (make-instruction opcode-f2
                                    (decode-xmm reg (rex-r info))
                                    (decode-xmm-or-mem r/m (rex-b info))
                                    imm)))
            ((eql (getf info :rep) #xF3)
             (and opcode-f3
                  (make-instruction opcode-f3
                                    (decode-xmm reg (rex-r info))
                                    (decode-xmm-or-mem r/m (rex-b info))
                                    imm)))
            (t
             (and opcode
                  (make-instruction opcode
                                    (decode-xmm reg (rex-r info))
                                    (decode-xmm-or-mem r/m (rex-b info))
                                    imm)))))))

(defun decode-cvt-2a (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond #+(or)
          ((getf info :osize)
           (make-instruction 'sys.lap-x86:cvtpi2pd
                             (decode-xmm reg (rex-r info))
                             (decode-mmx-or-mem r/m (rex-b info))))
          ((eql (getf info :rep) #xF2)
           (if (rex-w info)
               (make-instruction 'sys.lap-x86:cvtsi2sd64
                                 (decode-xmm reg (rex-r info))
                                 (decode-gpr64-or-mem r/m (rex-b info)))
               nil
               #+(or)
               (make-instruction 'sys.lap-x86:cvtsi2sd32
                                 (decode-xmm reg (rex-r info))
                                 (decode-gpr32-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF3)
           (if (rex-w info)
               (make-instruction 'sys.lap-x86:cvtsi2ss64
                                 (decode-xmm reg (rex-r info))
                                 (decode-gpr64-or-mem r/m (rex-b info)))
               nil
               #+(or)
               (make-instruction 'sys.lap-x86:cvtsi2ss64
                                 (decode-xmm reg (rex-r info))
                                 (decode-gpr32-or-mem r/m (rex-b info)))))
          #+(or)
          (t
           (make-instruction 'sys.lap-x86:cvtpi2ps
                             (decode-xmm reg (rex-r info))
                             (decode-mmx-or-mem r/m (rex-b info)))))))

(defun decode-cvt-2c (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond #+(or)
          ((getf info :osize)
           (make-instruction 'sys.lap-x86:cvttpd2pi
                             (decode-mmx reg (rex-r info))
                             (decode-xmm-or-mem r/m (rex-b info))))
          ((eql (getf info :rep) #xF2)
           (if (rex-w info)
               (make-instruction 'sys.lap-x86:cvttsd2si64
                                 (decode-gpr64 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))
               nil
               #+(or)
               (make-instruction 'sys.lap-x86:cvttsd2si32
                                 (decode-gpr32 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF3)
           (if (rex-w info)
               (make-instruction 'sys.lap-x86:cvttss2si64
                                 (decode-gpr64 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))
               nil
               #+(or)
               (make-instruction 'sys.lap-x86:cvttss2si64
                                 (decode-gpr32 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))))
          #+(or)
          (t
           (make-instruction 'sys.lap-x86:cvttps2pi
                             (decode-mmx reg (rex-r info))
                             (decode-xmm-or-mem r/m (rex-b info)))))))

(defun decode-cvt-2d (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (cond #+(or)
          ((getf info :osize)
           (make-instruction 'sys.lap-x86:cvtpd2pi
                             (decode-mmx reg (rex-r info))
                             (decode-xmm-or-mem r/m (rex-b info))))
          ((eql (getf info :rep) #xF2)
           (if (rex-w info)
               (make-instruction 'sys.lap-x86:cvtsd2si64
                                 (decode-gpr64 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))
               nil
               #+(or)
               (make-instruction 'sys.lap-x86:cvtsd2si32
                                 (decode-gpr32 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))))
          ((eql (getf info :rep) #xF3)
           (if (rex-w info)
               (make-instruction 'sys.lap-x86:cvtss2si64
                                 (decode-gpr64 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))
               nil
               #+(or)
               (make-instruction 'sys.lap-x86:cvtss2si64
                                 (decode-gpr32 reg (rex-r info))
                                 (decode-xmm-or-mem r/m (rex-b info)))))
          #+(or)
          (t
           (make-instruction 'sys.lap-x86:cvtps2pi
                             (decode-mmx reg (rex-r info))
                             (decode-xmm-or-mem r/m (rex-b info)))))))

(defun decode-sse-cmp (context info)
  (multiple-value-bind (reg r/m)
      (disassemble-modr/m context info)
    (let ((imm (consume-ub8 context)))
      (flet ((doit (instructions)
               (if (>= imm 8)
                   nil
                   (make-instruction (elt instructions imm)
                                     (decode-xmm reg (rex-r info))
                                     (decode-xmm-or-mem r/m (rex-b info))))))
        (cond ((getf info :osize) ; packed double
               (doit '(sys.lap-x86:cmpeqpd
                       sys.lap-x86:cmpltpd
                       sys.lap-x86:cmplepd
                       sys.lap-x86:cmpunordpd
                       sys.lap-x86:cmpnewpd
                       sys.lap-x86:cmpnltpd
                       sys.lap-x86:cmpnlepd
                       sys.lap-x86:cmpordpd)))
              ((eql (getf info :rep) #xF2) ; scalar double
               (doit '(sys.lap-x86:cmpeqsd
                       sys.lap-x86:cmpltsd
                       sys.lap-x86:cmplesd
                       sys.lap-x86:cmpunordsd
                       sys.lap-x86:cmpnewsd
                       sys.lap-x86:cmpnltsd
                       sys.lap-x86:cmpnlesd
                       sys.lap-x86:cmpordsd)))
              ((eql (getf info :rep) #xF3) ; scalar single
               (doit '(sys.lap-x86:cmpeqss
                       sys.lap-x86:cmpltss
                       sys.lap-x86:cmpless
                       sys.lap-x86:cmpunordss
                       sys.lap-x86:cmpnewss
                       sys.lap-x86:cmpnltss
                       sys.lap-x86:cmpnless
                       sys.lap-x86:cmpordss)))
              (t ; packed single
               (doit '(sys.lap-x86:cmpeqps
                       sys.lap-x86:cmpltps
                       sys.lap-x86:cmpleps
                       sys.lap-x86:cmpunordps
                       sys.lap-x86:cmpnewps
                       sys.lap-x86:cmpnltps
                       sys.lap-x86:cmpnleps
                       sys.lap-x86:cmpordps))))))))

(defun decode-io (context info opcode16 opcode32 port-is-dx)
  (let ((port (if port-is-dx
                  :dx
                  (consume-ub8 context))))
    (make-instruction (if (getf info :osize)
                          opcode16
                          opcode32)
                      port)))

(defun decode-simple (context info opcode)
  (declare (ignore context info))
  (make-instruction opcode))

(defun decode-simple-sized (context info opcode16 opcode32 opcode64)
  (declare (ignore context opcode16))
  (if (rex-w info)
      (make-instruction opcode64)
      (make-instruction opcode32)))

(defun disassemble-one-instruction-1 (context)
  (let* ((byte (consume-ub8 context))
         (table *instruction-table*)
         (info '()))
    ;; Read ordinary prefixes
    (loop
       (case byte
         ((#xF2 #xF3)
          (setf (getf info :rep) byte))
         (#x2E
          (setf (getf info :segment) :cs))
         (#x36
          (setf (getf info :segment) :ss))
         (#x3E
          (setf (getf info :segment) :ds))
         (#x26
          (setf (getf info :segment) :es))
         (#x64
          (setf (getf info :segment) :fs))
         (#x65
          (setf (getf info :segment) :gs))
         (#x66
          (setf (getf info :osize) byte))
         (#x67
          (setf (getf info :asize) byte))
         (#xF0
          (setf (getf info :lock) byte))
         (t
          (return)))
       (setf byte (consume-ub8 context)))
    (when (eql (logand byte #xF0) #x40)
      ;; REX prefix.
      (setf (getf info :rex) byte)
      (setf byte (consume-ub8 context)))
    (when (eql byte #x0F)
      (setf table *extended-instruction-table*)
      (setf byte (consume-ub8 context)))
    (let ((entry (aref table byte)))
      (setf (getf info :opcode) byte)
      (cond (entry
             (let ((inst (apply (first entry) context info (rest entry))))
               (when inst
                 (setf (slot-value inst '%lock-prefix) (getf info :lock)))
               inst))
            (t nil)))))

(defun disassemble-one-instruction (context)
  (let* ((start (context-code-offset context))
         (code-start 16)
         (code-end (code-end (context-function context))))
    (cond ((decoding-jump-table-p context)
           (let* ((dest (consume-ub64/le context))
                  (absolute-dest (+ (decoding-jump-table-p context) dest)))
             ;; Best guess...
             ;; If the destination is outside the function's code,
             ;; then this is probably past the end of the table.
             (cond ((<= code-start absolute-dest code-end)
                    (let ((inst (make-instruction :jump-target (decoding-jump-table-p context) dest)))
                      (setf (slot-value inst '%offset) start
                            (slot-value inst '%size) 8)
                      (label context absolute-dest :createp t)
                      inst))
                   (t
                    ;; Rewind & try again.
                    (setf (decoding-jump-table-p context) nil
                          (slot-value context '%offset) start)
                    (disassemble-one-instruction context)))))
          (t
           (let ((inst (disassemble-one-instruction-1 context)))
             (cond (inst
                    ;; Hack - Guess where jump tables start.
                    (when (and (eql (inst-opcode inst) 'sys.lap-x86:jmp)
                               (equal (inst-operands inst) '(:rax)))
                      (label context (context-code-offset context) :createp t)
                      (setf (decoding-jump-table-p context) (context-code-offset context)))
                    (setf (slot-value inst '%offset) start
                          (slot-value inst '%size) (- (context-code-offset context) start))
                    ;; Update labels.
                    (dolist (operand (inst-operands inst))
                      (when (and (typep operand 'effective-address)
                                 (eql (ea-base operand) :rip)
                                 (<= code-start
                                     (+ (context-code-offset context) (ea-disp operand))
                                     (1- code-end)))
                        (label context
                               (+ (context-code-offset context)
                                  (ea-disp operand))
                               :createp t)))
                    inst)
                   (t
                    (setf (slot-value context '%offset) (1+ start))
                    nil)))))))
