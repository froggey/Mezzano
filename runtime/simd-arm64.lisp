;;;; AdvSIMD support for arm64.
;;;; This file contains the types and machinery for defining
;;;; vector & vector operations. The actual functions & vectors
;;;; are defined in simd-arm64-instructions.lisp

(in-package :mezzano.simd.arm64)

;;; Runtime support

;; These are all the possible simd pack types supported by advsimd.
;; They're, to some extent, mutually compatible, in that they can
;; call be reinterpret-cast'd between each other.
(deftype advsimd-pack ()
  `(or (simd:simd-pack single-float 4)
       (simd:simd-pack double-float 2)
       (simd:simd-pack (unsigned-byte 8) 16)
       (simd:simd-pack (unsigned-byte 16) 8)
       (simd:simd-pack (unsigned-byte 32) 4)
       (simd:simd-pack (unsigned-byte 64) 2)
       (simd:simd-pack (signed-byte 8) 16)
       (simd:simd-pack (signed-byte 16) 8)
       (simd:simd-pack (signed-byte 32) 4)
       (simd:simd-pack (signed-byte 64) 2)))

;; These are types expected to be immediates
(deftype imm1 () '(unsigned-byte 1))
(deftype imm2 () '(unsigned-byte 2))
(deftype imm3 () '(unsigned-byte 3))
(deftype imm4 () '(unsigned-byte 4))
(deftype imm5 () '(unsigned-byte 5))
(deftype imm6 () '(unsigned-byte 6))
(deftype imm7 () '(unsigned-byte 7))
(deftype imm8 () '(unsigned-byte 8))

;; Called by the compiler to box vectors.
;; Header data should be boxed fixnum in x1.
(int::define-lap-function %%make-simd-pack-q0 ()
  (:gc :no-frame :layout #*)
  (a64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*00)
  (a64:add :x29 :sp :xzr)
  (:gc :frame)
  (a64:str :q0 (:pre :sp -16))
  (a64:mov :x5 #.(ash 4 int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (a64:mov :x0 #.(ash int::+object-tag-simd-pack+
                      int::+n-fixnum-bits+))
  ;; Header data already in x1.
  ;; Words.
  (a64:mov :x2 #.(ash 3 int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (a64:mov :x3 :x26)
  ;; Allocate object.
  (a64:named-call mezzano.runtime::%allocate-object)
  ;; Set data.
  (a64:ldr :q0 (:post :sp 16))
  (a64:str :q0 (:object :x0 1))
  ;; Single-value return.
  (a64:mov :x5 #.(ash 1 int::+n-fixnum-bits+)) ; fixnum 1
  (a64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #*)
  (a64:ret))

;;; Operation definition

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun virtual-register-kind-for-type (type-name)
    (ecase type-name
      (f32 :single-float)
      (f64 :double-float)
      ((u8 u16 u32 u64 s8 s16 s32 s64) :integer)
      ((f32.4 f64.2
        u8.16 u16.8 u32.4 u64.2
        s8.16 s16.8 s32.4 s64.2)
       :advsimd)))
  (defun box-instruction-fragment (type-name source destination)
    (let ((inst (ecase type-name
                  (f32 'mezzano.compiler.backend:box-single-float-instruction)
                  (f64 'mezzano.compiler.backend:box-double-float-instruction)
                  ((u8 u16 u32 s8 s16 s32) 'mezzano.compiler.backend:box-fixnum-instruction)
                  (u64 'mezzano.compiler.backend:box-unsigned-byte-64-instruction)
                  (s64 'mezzano.compiler.backend:box-signed-byte-64-instruction)
                  ((f32.4 f64.2
                    u8.16 u16.8 u32.4 u64.2
                    s8.16 s16.8 s32.4 s64.2)
                   'c.a64::box-advsimd-instruction))))
      `(make-instance ',inst
                      :source ,source
                      :destination ,destination
                      ,@(when (eql inst 'c.a64::box-advsimd-instruction)
                          `(:header ,(ecase type-name
                                       (f32.4 (simd::encode-simd-pack-header 'single-float 4))
                                       (f64.2 (simd::encode-simd-pack-header 'double-float 2))
                                       (u8.16 (simd::encode-simd-pack-header '(unsigned-byte 8) 16))
                                       (u16.8 (simd::encode-simd-pack-header '(unsigned-byte 16) 8))
                                       (u32.4 (simd::encode-simd-pack-header '(unsigned-byte 32) 4))
                                       (u64.2 (simd::encode-simd-pack-header '(unsigned-byte 64) 2))
                                       (s8.16 (simd::encode-simd-pack-header '(signed-byte 8) 16))
                                       (s16.8 (simd::encode-simd-pack-header '(signed-byte 16) 8))
                                       (s32.4 (simd::encode-simd-pack-header '(signed-byte 32) 4))
                                       (s64.2 (simd::encode-simd-pack-header '(signed-byte 64) 2))))))))
  (defun unbox-instruction-fragment (type-name source destination)
    `(make-instance ',(ecase type-name
                        (f32 'mezzano.compiler.backend:unbox-single-float-instruction)
                        (f64 'mezzano.compiler.backend:unbox-double-float-instruction)
                        ((u8 u16 u32 s8 s16 s32) 'mezzano.compiler.backend:unbox-fixnum-instruction)
                        (u64 'mezzano.compiler.backend:unbox-unsigned-byte-64-instruction)
                        (s64 'mezzano.compiler.backend:unbox-signed-byte-64-instruction)
                        ((f32.4 f64.2
                          u8.16 u16.8 u32.4 u64.2
                          s8.16 s16.8 s32.4 s64.2)
                         'c.a64::unbox-advsimd-instruction))
                    :source ,source
                    :destination ,destination))
  (defun scalar-promotion (type)
    (case type
      (f32.4 (values 'f32 '%f32.4-broadcast))
      (f64.2 (values 'f64 '%f64.2-broadcast))
      (u8.16 (values 'u8  '%u8.16-broadcast))
      (u16.8 (values 'u16 '%u16.8-broadcast))
      (u32.4 (values 'u32 '%u32.4-broadcast))
      (u64.2 (values 'u64 '%u64.2-broadcast))
      (s8.16 (values 's8  '%s8.16-broadcast))
      (s16.8 (values 's16 '%s16.8-broadcast))
      (s32.4 (values 's32 '%s32.4-broadcast))
      (s64.2 (values 's64 '%s64.2-broadcast))))
  ;; As we generate transform permutations rather than being clever in the transform function,
  ;; we need to make sure the number of arguments stays low (ideally 0, 1, or 2. maybe 3 in a pinch)
  ;; More than that will blow up to a huge number of permutations.
  ;; Unfortunately the types are lost by the time we get to body of the transform.
  (defun generate-permutation-list (types)
    (if types
        (let ((inner (generate-permutation-list (cdr types))))
          (multiple-value-bind (scalar promotor)
              (scalar-promotion (car types))
            (cond (scalar
                   ;; Type needs to be permuted
                   (append (loop for list in inner
                                 collect (list* (list (car types) nil) list))
                           (loop for list in inner
                                 collect (list* (list scalar promotor (car types)) list))))
                  (t
                   ;; Otherwise fine
                   (loop for list in inner
                         collect (list* (list (car types) nil) list))))))
        '(())))
  (defun generate-transform-permutations (name internal-name
                                          result-types value-types
                                          result-names value-names
                                          shiftp)
    (declare (ignorable result-names))
    `(progn
       ,@(loop for permuted-value-types in (generate-permutation-list value-types)
               collect `(c::define-transform ,name (,@(loop for (ty) in permuted-value-types
                                                            for name in value-names
                                                            collect `(,name ,(immediate-actual-type ty shiftp))))
                            ((:optimize (= safety 0) (= speed 3)))
                          `(the ,',(if (rest result-types)
                                       `(values ,@result-types)
                                       (first result-types))
                                (c::call ,',internal-name
                                         ,,@(loop for name in value-names
                                                  for (nil promotor vector-type) in permuted-value-types
                                                  collect (if promotor
                                                              ``(the ,',vector-type
                                                                     (c::call ,',promotor ,,name))
                                                              name))))))))
  (defun immediatep (type)
    (member type '(imm1 imm2 imm3 imm4 imm5 imm6 imm7 imm8)))
  (defun immediate-max (type)
    (ecase type
      (imm1 2)
      (imm2 4)
      (imm3 8)
      (imm4 16)
      (imm5 32)
      (imm6 64)
      (imm7 128)
      (imm8 256)))
  (defun immediate-actual-type (type shiftp)
    (if (and shiftp (immediatep type))
        (ecase type
          (imm1 `(integer 1 2))
          (imm2 `(integer 1 4))
          (imm3 `(integer 1 8))
          (imm4 `(integer 1 16))
          (imm5 `(integer 1 32))
          (imm6 `(integer 1 64))
          (imm7 `(integer 1 128))
          (imm8 `(integer 1 256)))
        type))
  (defun expand-immediates (internal-name value-types value-names shiftp)
    (labels ((recur (args value-types value-names)
               (cond ((endp value-names)
                      `(,internal-name ,@(reverse args)))
                     ((immediatep (first value-types))
                      (let ((imm-ty (first value-types))
                            (imm-name (first value-names)))
                        `(ecase ,imm-name
                           ,@(loop for i from (if shiftp 1 0) below (+ (immediate-max imm-ty) (if shiftp 1 0))
                                   collect `(,i ,(recur (list* i args) (rest value-types) (rest value-names)))))))
                     (t
                      (recur (list* (first value-names) args)
                             (rest value-types)
                             (rest value-names))))))
      (recur '() value-types value-names))))

;; This big scary macro takes in a bunch of names, arguments, types, along with an opcode & operand
;; and produces all the machinery needed to compile that operation efficiently & inline, for most cases.

(defmacro define-op (name result-types value-types result-names value-names opcode operands &key shiftp rmw (export t))
  (let ((boxed-values (loop for name in value-names
                            collect (make-symbol (format nil "~A-BOXED" (string name)))))
        (boxed-results (loop for name in result-names
                             collect (make-symbol (format nil "~A-BOXED" (string name)))))
        (internal-name (intern (format nil "%~A" name)
                               (symbol-package name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(when export
             `((export ',name)))
         ,(generate-transform-permutations
           name internal-name
           result-types value-types
           result-names value-names
           shiftp)
         (c::mark-as-constant-foldable ',name)
         (c::mark-as-constant-foldable ',internal-name)
         (c.a64::define-builtin ,internal-name (,(loop for boxed-name in boxed-values
                                                       for name in value-names
                                                       for ty in value-types
                                                       collect (if (immediatep ty)
                                                                   `(:constant ,name (typep ,name ',(immediate-actual-type ty shiftp)))
                                                                   boxed-name))
                                                ,boxed-results
                                                :has-wrapper nil)
           (let (,@(loop for ty in value-types
                         for name in value-names
                         unless (immediatep ty)
                         collect `(,name (make-instance
                                          'mezzano.compiler.backend:virtual-register
                                          :kind ,(virtual-register-kind-for-type ty))))
                 ,@(loop for ty in result-types
                         for name in result-names
                         collect `(,name (make-instance
                                          'mezzano.compiler.backend:virtual-register
                                          :kind ,(virtual-register-kind-for-type ty)))))
             ;; Unbox values
             ,@(loop for ty in value-types
                     for unboxed in value-names
                     for boxed in boxed-values
                     unless (immediatep ty)
                     collect `(c.a64::emit
                               ,(unbox-instruction-fragment ty boxed unboxed)))
             (c.a64::emit
              (make-instance 'c.a64::arm64-instruction
                             :opcode ',opcode
                             :operands ,operands
                             :inputs (list ,@value-names)
                             :outputs (list ,@result-names)
                             ,@(when rmw
                                 `(:fake-3op t))))
             ;; Box results
             ,@(loop for ty in result-types
                     for unboxed in result-names
                     for boxed in boxed-results
                     collect `(c.a64::emit
                               ,(box-instruction-fragment ty unboxed boxed))))))
       ;; A wrapper for the internal-name is needed so the compiler can do constant-folding through it.
       (defun ,internal-name ,value-names
         ,(expand-immediates internal-name value-types value-names shiftp))
       (defun ,name ,value-names
         (,internal-name
         ,@(loop for ty in value-types
                 for name in value-names
                 collect (if (immediatep ty)
                             name
                             `(,ty ,name))))))))

(defmacro %define-aref-transforms (aref row-major-aref %row-major-aref scalar-type vector-type n-lanes count setter)
  "Generate wrapper functions and transforms to transform a call to aref/row-major-aref to the appropriate builtin."
  (let ((values (when setter
                  (if (eql count 1)
                      '(value)
                      (loop repeat count
                            collect (gensym "VALUE"))))))
    `(progn
       ;; TODO: Support other non-1D arrays
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(loop for permute in (generate-permutation-list (loop repeat count collect vector-type))
               collect
                 `(c::define-transform ,row-major-aref (,@(loop for value in values
                                                                for (ty) in permute
                                                                collect `(,value ,ty))
                                                        (vector (simple-array ,scalar-type (*)) array-type)
                                                        (index fixnum index-type))
                      ((:optimize (= safety 0) (= speed 3)))
                    `(the ,',(if (eql count 1)
                                 vector-type
                                 `(values ,@(loop repeat count collect vector-type)))
                          (progn
                            ,(c::insert-bounds-check vector array-type index index-type :adjust ,(1- (* n-lanes count)))
                            (c::call ,',%row-major-aref
                                     ,,@(loop for (ty broadcast) in permute
                                              for value in values
                                              collect (if broadcast ``(c::call ,',broadcast ,,value) value))
                                     ,vector ,index))))))
       (defun ,aref (,@values array &rest subscripts)
         ;; FIXME: Since we're loading N elements out of the array, we should check bounds
         ;; on the last subscript.
         (funcall #',row-major-aref ,@values array (apply #'array-row-major-index array subscripts)))
       ;; TODO: Support arbitrary non-simple arrays.
       (defun ,row-major-aref (,@values array index)
         (check-type array (simple-array ,scalar-type *))
         (assert (<= 0 index))
         (assert (<= (+ index ,(* n-lanes count)) (array-total-size array)))
         (funcall #',%row-major-aref
                  ,@(loop for val in values collect `(,vector-type ,val))
                  (if (typep array '(simple-array * (*)))
                      array ; 1D simple array
                      ;; Otherwise fetch the underlying 1D storage array
                      (int::%object-ref-t array ,int::+complex-array-storage+))
                  index)))))

;;; Generate aref accessors for the given type.

(defmacro define-aref (aref row-major-aref scalar-type vector-type n-lanes)
  (let ((%row-major-aref
          (intern (format nil "%~A" row-major-aref) (symbol-package row-major-aref))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export '(,aref ,row-major-aref))

         (c.a64::define-builtin ,%row-major-aref ((array index) result
                                                  :has-wrapper nil)
           (let ((unboxed-result (make-instance
                                  'mezzano.compiler.backend:virtual-register
                                  :kind :advsimd)))
             (c.a64::with-builtin-object-access (ea ea-inputs array index ,(/ 16 n-lanes))
               (c.a64::emit
                (make-instance 'c.a64::arm64-instruction
                               :opcode 'a64:ldr
                               :operands (list unboxed-result ea)
                               :inputs ea-inputs
                               :outputs (list unboxed-result))))
             (c.a64::emit
              (make-instance 'c.a64::box-advsimd-instruction
                             :source unboxed-result
                             :destination result
                             :header (simd::encode-simd-pack-header ',scalar-type ,n-lanes)))))
         (c.a64::define-builtin (setf ,%row-major-aref) ((value array index) result
                                                         :has-wrapper nil)
           (let ((unboxed-value (make-instance
                                 'mezzano.compiler.backend:virtual-register
                                 :kind :advsimd)))
             (c.a64::emit
              (make-instance 'c.a64::unbox-advsimd-instruction
                             :source value
                             :destination unboxed-value))
             (c.a64::with-builtin-object-access (ea ea-inputs array index ,(/ 16 n-lanes))
               (c.a64::emit
                (make-instance 'c.a64::arm64-instruction
                               :opcode 'a64:str
                               :operands (list unboxed-value ea)
                               :inputs (list* unboxed-value ea-inputs)
                               :outputs '())))
             (c.a64::emit
              (make-instance 'mezzano.compiler.backend:move-instruction
                             :source value
                             :destination result))))

         (c::define-aref-transform ,aref ,row-major-aref ,scalar-type))

       (defun ,%row-major-aref (array index)
         (,%row-major-aref array index))
       (defun (setf ,%row-major-aref) (value array index)
         (setf (,%row-major-aref array index) value))

       (%define-aref-transforms ,aref ,row-major-aref ,%row-major-aref ,scalar-type ,vector-type ,n-lanes 1 nil)
       (%define-aref-transforms (setf ,aref) (setf ,row-major-aref) (setf ,%row-major-aref) ,scalar-type ,vector-type ,n-lanes 1 t))))

;;; Generate aref accessors for the given type, using the multiple structures
;;; load/store instructions to access multiple registers worth of data at once.

(defmacro define-aref-multiple (scalar-type vector-type n-lanes count interleaved in-suffix-p)
  (let ((name (intern (format nil "~A-AREF-~A~:[~;-INTERLEAVED~]~@[-IN-~A~]"
                              vector-type count interleaved (if in-suffix-p scalar-type))
                      (symbol-package vector-type)))
        (row-major-name (intern (format nil "~A-ROW-MAJOR-AREF-~A~:[~;-INTERLEAVED~]~@[-IN-~A~]"
                                        vector-type count interleaved (if in-suffix-p scalar-type))
                                (symbol-package vector-type)))
        (%row-major-name (intern (format nil "%~A-ROW-MAJOR-AREF-~A~:[~;-INTERLEAVED~]~@[-IN-~A~]"
                                         vector-type count interleaved (if in-suffix-p scalar-type))
                                 (symbol-package vector-type)))
        (setter-name (intern (format nil "SET-~A-AREF-~A~:[~;-INTERLEAVED~]~@[-IN-~A~]"
                                     vector-type count interleaved (if in-suffix-p scalar-type))
                             (symbol-package vector-type)))
        (row-major-setter-name (intern (format nil "SET-~A-ROW-MAJOR-AREF-~A~:[~;-INTERLEAVED~]~@[-IN-~A~]"
                                               vector-type count interleaved (if in-suffix-p scalar-type))
                                       (symbol-package vector-type)))
        (%row-major-setter-name (intern (format nil "%SET-~A-ROW-MAJOR-AREF-~A~:[~;-INTERLEAVED~]~@[-IN-~A~]"
                                                vector-type count interleaved (if in-suffix-p scalar-type))
                                        (symbol-package vector-type)))
        (values (loop for i below count
                      collect (intern (format nil "V~D" i) (symbol-package vector-type))))
        (results (loop for i below count
                       collect (make-symbol (format nil "R~D" i))))
        (unboxed-values (loop for i below count
                              collect (make-symbol (format nil "V~D-UNBOXED" i))))
        (elt-size (ecase vector-type
                    ((u8.16 s8.16) :16b)
                    ((u16.8 s16.8) :8h)
                    ((u32.4 s32.4 f32.4) :4s)
                    ((u64.2 s64.2 f64.2) :2d)))
        (elt-scale (ecase scalar-type
                    ((u8 s8) 1)
                    ((u16 s16) 2)
                    ((u32 s32 f32) 4)
                    ((u64 s64 f64) 8))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export '(,name ,row-major-name))

         (c.a64::define-builtin ,%row-major-name
             ((array index) ,results :has-wrapper nil)
           (let (,@(loop for v in unboxed-values
                         collect `(,v (make-instance
                                       'mezzano.compiler.backend:virtual-register
                                       :kind :advsimd))))
             (c.a64::emit
              (make-instance 'mezzano.compiler.backend:move-instruction
                             :source array
                             :destination :x1))
             (c.a64::emit
              (make-instance 'c.a64::arm64-ld/st-multiple-instruction
                             :opcode ',(if interleaved
                                           (ecase count
                                             (1 'a64:ld1)
                                             (2 'a64:ld2)
                                             (3 'a64:ld3)
                                             (4 'a64:ld4))
                                           'a64:ld1)
                             :size ,elt-size
                             :direction :load
                             :registers (list ,@unboxed-values)
                             :index index
                             :scale ,elt-scale))
             ,@(loop for v in unboxed-values
                     for r in results
                     collect `(c.a64::emit
                               ,(box-instruction-fragment vector-type v r)))))

         (c.a64::define-builtin ,%row-major-setter-name
             ((,@values array index) ,results :has-wrapper nil)
           (let (,@(loop for v in unboxed-values
                         collect `(,v (make-instance
                                       'mezzano.compiler.backend:virtual-register
                                       :kind :advsimd))))
             ,@(loop for v in values
                     for u in unboxed-values
                     collect `(c.a64::emit
                               (make-instance 'c.a64::unbox-advsimd-instruction
                                              :source ,v
                                              :destination ,u)))
             (c.a64::emit
              (make-instance 'mezzano.compiler.backend:move-instruction
                             :source array
                             :destination :x1))
             (c.a64::emit
              (make-instance 'c.a64::arm64-ld/st-multiple-instruction
                             :opcode ',(if interleaved
                                           (ecase count
                                             (1 'a64:st1)
                                             (2 'a64:st2)
                                             (3 'a64:st3)
                                             (4 'a64:st4))
                                           'a64:st1)
                             :size ,elt-size
                             :direction :store
                             :registers (list ,@unboxed-values)
                             :index index
                             :scale ,elt-scale))
             ,@(loop for v in values
                     for r in results
                     collect `(c.a64::emit
                               (make-instance 'mezzano.compiler.backend:move-instruction
                                              :source ,v
                                              :destination ,r)))))

         (c::define-aref-transform ,name ,row-major-name ,scalar-type
           :value-count ,count
           :setter-name ,setter-name
           :row-major-setter-name ,row-major-setter-name))

       (defun ,%row-major-name (array index)
         (,%row-major-name array index))
       (defun ,%row-major-setter-name (,@values array index)
         (,%row-major-setter-name ,@values array index))

       (%define-aref-transforms ,name ,row-major-name ,%row-major-name
                                ,scalar-type ,vector-type ,n-lanes ,count nil)
       (%define-aref-transforms ,setter-name ,row-major-setter-name ,%row-major-setter-name
                                ,scalar-type ,vector-type ,n-lanes ,count t)

       (defsetf ,name (array &rest subscripts) ,values
         `(,',setter-name ,,@values ,array ,@subscripts))
       (defsetf ,row-major-name (array index) ,values
         `(,',row-major-setter-name ,,@values ,array ,index)))))

;;; Generate a broadcast cast function, either returning a vector of the correct type
;;; unchanged, or a scalar broadcast to a vector.

(defmacro define-broadcast-cast (name broadcast-scalar vector-type scalar-type)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name)
       (c::define-transform ,name ((value ,scalar-type))
           ((:optimize (= safety 0) (= speed 3)))
         `(the ,',vector-type
               (c::call ,',broadcast-scalar ,value)))
       (c::define-transform ,name ((value ,vector-type))
           ((:optimize (= safety 0) (= speed 3)))
         `(the ,',vector-type ,value))
       (c::mark-as-constant-foldable ',name))
     (defun ,name (value)
       (etypecase value
         (,scalar-type (,broadcast-scalar value))
         (,vector-type value)))))

(defmacro define-reinterpret-cast (name vector-type scalar-type)
  (let ((internal-scalar-name (intern (format nil "%~A-FROM-~A" name scalar-type)
                                      (symbol-package name)))
        (internal-vector-name (intern (format nil "%~A-FROM-ADVSIMD" name)
                                      (symbol-package name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',name)
         (c::define-transform ,name ((value ,scalar-type))
             ((:optimize (= safety 0) (= speed 3)))
           `(the ,',vector-type
                 (c::call ,',internal-scalar-name ,value)))
         (c::define-transform ,name ((value advsimd-pack))
             ((:optimize (= safety 0) (= speed 3)))
           `(the ,',vector-type
                 (c::call ,',internal-vector-name ,value)))
         (c::mark-as-constant-foldable ',name)
         (c::mark-as-constant-foldable ',internal-scalar-name)
         (c::mark-as-constant-foldable ',internal-vector-name)
         (c.a64::define-builtin ,internal-scalar-name ((boxed-value) boxed-result
                                                       :has-wrapper nil)
           (let ((value (make-instance
                         'mezzano.compiler.backend:virtual-register
                         :kind ,(virtual-register-kind-for-type scalar-type)))
                 (result (make-instance
                          'mezzano.compiler.backend:virtual-register
                          :kind ,(virtual-register-kind-for-type vector-type))))
             ;; Unbox values
             (c.a64::emit
              ,(unbox-instruction-fragment scalar-type 'boxed-value 'value))
             (c.a64::emit
              (make-instance 'mezzano.compiler.backend:move-instruction
                             :source value
                             :destination result))
             ;; Box results
             (c.a64::emit
              ,(box-instruction-fragment vector-type 'result 'boxed-result))))
         (c.a64::define-builtin ,internal-vector-name ((boxed-value) boxed-result
                                                       :has-wrapper nil)
           (let ((value (make-instance
                         'mezzano.compiler.backend:virtual-register
                         :kind ,(virtual-register-kind-for-type vector-type))))
             ;; Unbox values
             (c.a64::emit
              ,(unbox-instruction-fragment vector-type 'boxed-value 'value))
             ;; Box results
             (c.a64::emit
              ,(box-instruction-fragment vector-type 'value 'boxed-result)))))
       ;; A wrapper for the internal-name is needed so the compiler can do constant-folding through it.
       (defun ,internal-scalar-name (value)
         (,internal-scalar-name value))
       (defun ,internal-vector-name (value)
         (,internal-vector-name value))
       (defun ,name (value)
         (etypecase value
           (,scalar-type (,internal-scalar-name value))
           (advsimd-pack (,internal-vector-name value)))))))

(defmacro define-scalar-cast (name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name)
       (c::define-transform ,name ((value ,name))
           ((:optimize (= safety 0) (= speed 3)))
         `(the ,',name ,value))
       (c::mark-as-constant-foldable ',name))
     (defun ,name (value)
       (check-type value ,name)
       value)))

(defmacro define-associative-op (name type base identity)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     ,(cond (identity
             `(progn
                (defun ,name (&rest numbers)
                  (declare (dynamic-extent numbers))
                  (let ((result ,identity))
                    (dolist (n numbers)
                      (setf result (,base result n)))
                    result))
                (define-compiler-macro ,name (&rest numbers)
                  (cond ((null numbers) ',identity)
                        ((null (rest numbers))
                         `(,',type (first numbers)))
                        (t (let ((result (first numbers)))
                             (dolist (n (rest numbers))
                               (setf result `(,',base ,result (,',type ,n))))
                             result))))))
            (t
             `(progn
                (defun ,name (number &rest numbers)
                  (declare (dynamic-extent numbers))
                  (let ((result (,type first-number)))
                    (dolist (n numbers)
                      (setf result (,base result (,type n))))
                    result))
                (define-compiler-macro ,name (&whole whole &rest numbers)
                  (cond ((null numbers) form)
                        ((null (rest numbers))
                         `(,',type ,(first numbers)))
                        (t (let ((result `(,',type ,(first numbers))))
                             (dolist (n (rest numbers))
                               (setf result `(,',base ,result (,',type ,n))))
                             result)))))))))

(defmacro define-reducing-op (name type base identity)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     (defun ,name (number &rest more-numbers)
       (declare (dynamic-extent more-numbers))
       (cond (more-numbers
              (let ((result (,type number)))
                (dolist (n more-numbers)
                  (setf result (,base result (,type n))))
                result))
             (t (,base ,identity (,type number)))))
     (define-compiler-macro ,name (number &rest more-numbers)
       (cond ((null more-numbers)
              `(,',base ,',identity (,',type ,number)))
             (t (let ((result `(,',type number)))
                  (dolist (n more-numbers)
                    (setf result `(,',base ,result (,',type ,n))))
                  result))))))

(defmacro define-make (scalar-type vector-type lane-count insert-fn)
  (let ((args (loop for i below lane-count
                    collect (intern (string (code-char (+ (char-code #\A) i)))
                                    (symbol-package vector-type))))
        (temps (loop for i below lane-count collect (gensym)))
        (name (intern (format nil "MAKE-~A" vector-type) (symbol-package vector-type))))
    `(progn
       (declaim (inline ,name))
       (defun ,name ,args
         (let* ((,(first temps) (,vector-type (,scalar-type ,(first args))))
                ,@(loop for lane from 1
                        for arg in (rest args)
                        for (prev next) on temps
                        collect `(,next (,insert-fn ,prev (,scalar-type ,arg) ,lane))))
           ,(first (last temps)))))))
