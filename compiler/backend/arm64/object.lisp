;;;; Object and memory related ARM64 builtins.

(in-package :mezzano.compiler.backend.arm64)

(define-builtin sys.int::%value-has-tag-p ((object (:constant tag (typep tag '(unsigned-byte 4)))) :eq :has-wrapper nil)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp object #b1111)
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp tag)
                         :inputs (list temp)
                         :outputs '()))))

(define-builtin mezzano.runtime::%%object-of-type-p ((object (:constant object-tag (typep object-tag '(unsigned-byte 6)))) :eq :has-wrapper nil)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header #b11111100)
                         :inputs (list header)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash object-tag sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin mezzano.runtime::%%simple-1d-array-p ((object) :ls)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header #b11111100)
                         :inputs (list header)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash sys.int::+last-simple-1d-array-object-tag+
                                                        sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin sys.int::%instance-or-funcallable-instance-p ((object) :eq)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header (ash (logxor (1- (ash 1 sys.int::+object-type-size+))
                                                                  sys.int::+object-tag-instance+
                                                                  sys.int::+object-tag-funcallable-instance+)
                                                          sys.int::+object-type-shift+))
                         :inputs (list header)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash (logand sys.int::+object-tag-instance+
                                                                sys.int::+object-tag-funcallable-instance+)
                                                        sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin sys.int::%%object-ref-unsigned-byte-8-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    ;; Need to use :w9 and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldrb
                         :operands (list :w9 `(,object ,unboxed-index))
                         :inputs (list object unboxed-index)
                         :outputs (list :x9)
                         :clobbers '(:x9)))
    (emit (make-instance 'ir:move-instruction
                         :source :x9
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-8-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    ;; Need to use :w9 and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :x9))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:strb
                         :operands (list :w9 `(,object ,unboxed-index))
                         :inputs (list :x9 object unboxed-index)
                         :outputs (list)
                         :clobbers '()))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-32-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    ;; Need to use :w9 and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list :w9 `(,object ,unboxed-index))
                         :inputs (list object unboxed-index)
                         :outputs (list :x9)
                         :clobbers '(:x9)))
    (emit (make-instance 'ir:move-instruction
                         :source :x9
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-32-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    ;; Need to use :w9 and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :x9))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:str
                         :operands (list :w9 `(,object ,unboxed-index))
                         :inputs (list :x9 object unboxed-index)
                         :outputs (list)
                         :clobbers '()))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%object-ref-unsigned-byte-64-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp `(,object ,unboxed-index))
                         :inputs (list object unboxed-index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%object-ref-unsigned-byte-64-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:str
                         :operands (list temp `(,object ,unboxed-index))
                         :inputs (list temp object unboxed-index)
                         :outputs (list)
                         :clobbers '()))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%object-ref-signed-byte-64-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp `(,object ,unboxed-index))
                         :inputs (list object unboxed-index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-signed-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%object-ref-signed-byte-64-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index-1 (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index-1))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sub
                         :operands (list unboxed-index unboxed-index-1 (- (+ 8 (- sys.int::+tag-object+))))
                         :inputs (list unboxed-index-1)
                         :outputs (list unboxed-index)))
    (emit (make-instance 'ir:unbox-signed-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:str
                         :operands (list temp `(,object ,unboxed-index))
                         :inputs (list temp object unboxed-index)
                         :outputs (list)
                         :clobbers '()))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%object-ref-t ((object index) result)
  (cond ((constant-value-p index 'fixnum)
         (let ((disp (object-slot-displacement (fetch-constant-value index))))
           (cond ((or (<= -256 disp 255)
                      (and (<= 0 disp 16380)
                           (zerop (logand disp #b111))))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list result `(,object ,disp))
                                       :inputs (list object)
                                       :outputs (list result))))
                 (t
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list :x12 `(:literal ,disp))
                                       :inputs (list)
                                       :outputs (list :x12)))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list result `(,object :x12))
                                       :inputs (list object :x12)
                                       :outputs (list result)))))))
        (t
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:add
                              :operands (list :x12 :xzr index :lsl 2)
                              :inputs (list index)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:sub
                              :operands (list :x12 :x12 (- (+ 8 (- sys.int::+tag-object+))))
                              :inputs (list :x12)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:ldr
                              :operands (list result `(,object :x12))
                              :inputs (list object :x12)
                              :outputs (list result))))))

(define-builtin (setf sys.int::%object-ref-t) ((value object index) result)
  (cond ((constant-value-p index 'fixnum)
         (let ((disp (object-slot-displacement (fetch-constant-value index))))
           (cond ((or (<= -256 disp 255)
                      (and (<= 0 disp 16380)
                           (zerop (logand disp #b111))))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:str
                                       :operands (list value `(,object ,disp))
                                       :inputs (list value object)
                                       :outputs (list))))
                 (t
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list :x12 `(:literal ,disp))
                                       :inputs (list)
                                       :outputs (list :x12)))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:str
                                       :operands (list value `(,object :x12))
                                       :inputs (list value object :x12)
                                       :outputs (list)))))))
        (t
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:add
                              :operands (list :x12 :xzr index :lsl 2)
                              :inputs (list index)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:sub
                              :operands (list :x12 :x12 (- (+ 8 (- sys.int::+tag-object+))))
                              :inputs (list :x12)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:str
                              :operands (list value `(,object :x12))
                              :inputs (list value object :x12)
                              :outputs (list)))))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

(define-builtin sys.int::%object-header-data ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register))
        (temp2 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp2 temp1 (lognot (1- (ash 1 sys.int::+object-data-shift+))))
                         :inputs (list temp1)
                         :outputs (list temp2)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list result :xzr temp2 :lsr (- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+))
                         :inputs (list temp2)
                         :outputs (list result)))))

(define-builtin mezzano.runtime::%car ((cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:ldr
                       :operands (list result `(:car ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%cdr ((cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:ldr
                       :operands (list result `(:cdr ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin (setf mezzano.runtime::%car) ((value cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:str
                       :operands (list value `(:car ,cons))
                       :inputs (list cons value)
                       :outputs (list)))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

(define-builtin (setf mezzano.runtime::%cdr) ((value cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:str
                       :operands (list value `(:cdr ,cons))
                       :inputs (list cons value)
                       :outputs (list)))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))
