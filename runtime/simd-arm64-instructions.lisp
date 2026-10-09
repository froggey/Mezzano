;;;; AdvSIMD support for arm64.
;;;; This file contains the specific vector intrisic definitions.

(in-package :mezzano.simd.arm64)

;;; Float vectors

(deftype f32 () `single-float)
(deftype f32.4 () `(simd:simd-pack f32 4))

(define-scalar-cast f32)
(define-broadcast-cast f32.4 f32.4-broadcast f32.4 f32)
(define-reinterpret-cast f32.4! f32.4 f32)

(define-op f32.4-lane-extract (f32) (f32.4 imm2)
  (result) (value lane) a64:dup.v `(:s ,result ,value ,lane))

(define-op f32.4-lane-insert (f32.4) (f32.4 f32 imm2)
  (result) (original value lane) a64:ins.v `(:s ,result ,lane (:fp-128 ,value) 0)
  :rmw t)

(define-op f32.4-dup (f32.4) (f32.4 imm2)
  (result) (value lane) a64:dup.v `(:4s ,result ,value ,lane))

(define-make f32 f32.4 4 f32.4-lane-insert)

(deftype f64 () `double-float)
(deftype f64.2 () `(simd:simd-pack f64 2))

(define-scalar-cast f64)
(define-broadcast-cast f64.2 f64.2-broadcast f64.2 f64)
(define-reinterpret-cast f64.2! f64.2 f64)

(define-op f64.2-lane-extract (f64) (f64.2 imm1)
  (result) (value lane) a64:dup.v `(:d ,result ,value ,lane))

(define-op f64.2-lane-insert (f64.2) (f64.2 f64 imm1)
  (result) (original value lane) a64:ins.v `(:d ,result ,lane (:fp-128 ,value) 0)
  :rmw t)

(define-op f64.2-dup (f64.2) (f64.2 imm1)
  (result) (value lane) a64:dup.v `(:2d ,result ,value ,lane))

(define-make f64 f64.2 2 f64.2-lane-insert)

(defmacro define-float-op (base-name result-types value-types results values opcode operand &key (export t))
  (flet ((doit (scalar-type vector-type type-keyword)
           (flet ((update-type (type)
                    (case type
                      (scalar scalar-type)
                      (vector vector-type)
                      (t type))))
           (let ((name (intern (format nil "~A~A" vector-type base-name)
                               (symbol-package vector-type))))
             `(define-op ,name
                  ,(mapcar #'update-type result-types)
                ,(mapcar #'update-type value-types)
                ,results ,values
                ,opcode
                (let ((type-keyword ,type-keyword))
                  ,operand)
                :export ,export)))))
    `(progn
       ,(doit 'f32 'f32.4 :4s)
       ,(doit 'f64 'f64.2 :2d))))

(define-float-op -broadcast (vector) (scalar)        (result) (value)   a64:dup.v  `(,type-keyword ,result (:fp-128 ,value) 0) :export nil)
(define-float-op +-two-arg  (vector) (vector vector) (result) (lhs rhs) a64:fadd.v `(,result ,lhs ,rhs ,type-keyword) :export nil)
(define-float-op *-two-arg  (vector) (vector vector) (result) (lhs rhs) a64:fmul.v `(,result ,lhs ,rhs ,type-keyword) :export nil)
(define-float-op --two-arg  (vector) (vector vector) (result) (lhs rhs) a64:fsub.v `(,result ,lhs ,rhs ,type-keyword) :export nil)

(define-associative-op f32.4+ f32.4 f32.4+-two-arg (f32.4 0.0f0))
(define-associative-op f32.4* f32.4 f32.4*-two-arg (f32.4 1.0f0))
(define-reducing-op    f32.4- f32.4 f32.4--two-arg (f32.4 0.0f0))
(define-associative-op f64.2+ f64.2 f64.2+-two-arg (f64.2 0.0d0))
(define-associative-op f64.2* f64.2 f64.2*-two-arg (f64.2 1.0d0))
(define-reducing-op    f64.2- f64.2 f64.2--two-arg (f64.2 0.0d0))

(define-aref f32.4-aref f32.4-row-major-aref f32 f32.4 4)

(define-aref-multiple f32 f32.4 4 2 nil nil)
(define-aref-multiple f32 f32.4 4 2 t nil)
(define-aref-multiple f32 f32.4 4 3 nil nil)
(define-aref-multiple f32 f32.4 4 3 t nil)
(define-aref-multiple f32 f32.4 4 4 nil nil)
(define-aref-multiple f32 f32.4 4 4 t nil)

(define-aref f64.2-aref f64.2-row-major-aref f64 f64.2 2)

(define-aref-multiple f64 f64.2 2 2 nil nil)
(define-aref-multiple f64 f64.2 2 2 t nil)
(define-aref-multiple f64 f64.2 2 3 nil nil)
(define-aref-multiple f64 f64.2 2 3 t nil)
(define-aref-multiple f64 f64.2 2 4 nil nil)
(define-aref-multiple f64 f64.2 2 4 t nil)

;;; Integer vectors

(defmacro define-integer-vector (scalar-type vector-type bit-width lane-count signedp)
  (flet ((name (suffix)
           (intern (format nil "~A~A" vector-type suffix) (symbol-package vector-type))))
    (multiple-value-bind (full-width half-width base-width lane-imm shift-imm gpr-width wider)
        (ecase bit-width
          (8  (values :16b :8b :b 'imm4 'imm3 :gp-32 (if signedp 's16.8 'u16.8)))
          (16 (values :8h  :4h :h 'imm3 'imm4 :gp-32 (if signedp 's32.4 'u32.4)))
          (32 (values :4s  :2s :s 'imm2 'imm5 :gp-32 (if signedp 's64.2 'u64.2)))
          (64 (values :2d  nil :d 'imm1 'imm6 :gp-64 nil)))
      `(progn
         (deftype ,scalar-type () '(,(if signedp 'signed-byte 'unsigned-byte) ,bit-width))
         (deftype ,vector-type () '(simd:simd-pack ,scalar-type ,lane-count))

         (define-scalar-cast ,scalar-type)
         (define-broadcast-cast ,vector-type ,(name "-BROADCAST") ,vector-type ,scalar-type)
         (define-reinterpret-cast ,(name "!") ,vector-type ,scalar-type)

         (define-op ,(name "-BROADCAST")           (,vector-type) (,scalar-type)              (result) (value)       a64:dup.v   `(,',full-width ,result (,',gpr-width ,value)) :export nil)
         (define-op ,(name "-DUP")                 (,vector-type) (,vector-type ,lane-imm)    (result) (value lane)  a64:dup.v   `(,',full-width ,result ,value ,lane))
         (define-op ,(name "-INS")                 (,vector-type) (,vector-type ,lane-imm ,vector-type ,lane-imm)
           (result) (original dst-lane src src-lane)  a64:ins.v
           `(,',base-width ,result ,dst-lane ,src ,src-lane)
           :rmw t)
         (define-op ,(name "-LANE-EXTRACT")        (,scalar-type) (,vector-type ,lane-imm)    (result) (value lane)
             ,(if (and signedp (not (eql bit-width 64))) 'a64:smov.v 'a64:umov.v)
             ,(cond (signedp ``(,',base-width ,result ,value ,lane)) ; force 64-bit dest
                    (t       ``(,',base-width (,',gpr-width ,result) ,value ,lane))))
         (define-op ,(name "-LANE-INSERT")         (,vector-type) (,vector-type ,scalar-type ,lane-imm)
           (result) (original value lane) a64:ins.v
           `(,',base-width ,result ,lane (,',gpr-width ,value)))

         (define-make ,scalar-type ,vector-type ,lane-count ,(name "-LANE-INSERT"))

         (define-op ,(name "-NOT")                 (,vector-type) (,vector-type)              (result) (value)       a64:not.v   `(,result ,value :16b))
         (define-op ,(name "-AND-TWO-ARG")         (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:and.v   `(,result ,lhs ,rhs :16b) :export nil)
         (define-op ,(name "+-TWO-ARG")            (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:add.v   `(,result ,lhs ,rhs ,',full-width) :export nil)
         (define-op ,(name "+-SATURATING-TWO-ARG") (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:sqadd.v 'a64:uqadd.v) `(,result ,lhs ,rhs ,',full-width) :export nil)
         (define-op ,(name "--TWO-ARG")            (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:sub.v   `(,result ,lhs ,rhs ,',full-width) :export nil)
         (define-op ,(name "-SHIFTR")              (,vector-type) (,vector-type ,shift-imm)   (result) (value shift) ,(if signedp 'a64:sshr.v 'a64:ushr.v) `(,result ,value ,shift ,',full-width) :shiftp t)

         ,@(when wider
             `((define-op ,(name "*-LONG")         (,wider)       (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:smull.v 'a64:umull.v) `(,result ,lhs ,rhs ,',half-width))
               (define-op ,(name "*-LONG-HI")      (,wider)       (,vector-type ,vector-type) (result) (lhs rhs)     ,(if signedp 'a64:smull2.v 'a64:umull2.v) `(,result ,lhs ,rhs ,',full-width))
               (define-op ,(name (format nil "-FROM-~A" wider)) (,vector-type) (,wider)       (result) (value)       a64:xtn.v   `(,result ,value ,',half-width))
               (define-op ,(name (format nil "-FROM-~A-HI" wider)) (,vector-type) (,vector-type ,wider) (result) (lhs rhs) a64:xtn2.v `(,result ,rhs ,',full-width) :rmw t)))

         (define-associative-op ,(name "+")            ,vector-type ,(name "+-TWO-ARG") (,vector-type 0))
         (define-associative-op ,(name "+-SATURATING") ,vector-type ,(name "+-SATURATING-TWO-ARG") (,vector-type 0))
         (define-reducing-op    ,(name "-")            ,vector-type ,(name "--TWO-ARG") (,vector-type 1))
         ,@(unless (eql bit-width 64)
             `((define-op ,(name "*-TWO-ARG")      (,vector-type) (,vector-type ,vector-type) (result) (lhs rhs)     a64:mul.v   `(,result ,lhs ,rhs ,',full-width) :export nil)
               (define-associative-op ,(name "*")      ,vector-type ,(name "*-TWO-ARG") (,vector-type 1))))
         (define-associative-op ,(name "-AND")         ,vector-type ,(name "-AND-TWO-ARG") (,vector-type ,(if signedp -1 (1- (ash 1 bit-width)))))

         (define-aref ,(name "-AREF") ,(name "-ROW-MAJOR-AREF") ,scalar-type ,vector-type ,lane-count)

         (define-aref-multiple ,scalar-type ,vector-type ,lane-count 2 nil nil)
         (define-aref-multiple ,scalar-type ,vector-type ,lane-count 2 t nil)
         (define-aref-multiple ,scalar-type ,vector-type ,lane-count 3 nil nil)
         (define-aref-multiple ,scalar-type ,vector-type ,lane-count 3 t nil)
         (define-aref-multiple ,scalar-type ,vector-type ,lane-count 4 nil nil)
         (define-aref-multiple ,scalar-type ,vector-type ,lane-count 4 t nil)))))

(define-integer-vector u8  u8.16 8  16 nil)
(define-integer-vector u16 u16.8 16 8  nil)
(define-integer-vector u32 u32.4 32 4  nil)
(define-integer-vector u64 u64.2 64 2  nil)

(define-integer-vector s8  s8.16 8  16 t)
(define-integer-vector s16 s16.8 16 8  t)
(define-integer-vector s32 s32.4 32 4  t)
(define-integer-vector s64 s64.2 64 2  t)

;; This allows reading a u8.16 vector from a (simple-array ub32), which are used throughout
;; the graphics system. I'm still thinking what shape I want this interface to take, so
;; it's only defined for this case.
(define-aref-multiple u32 u8.16 4 4 t t)
