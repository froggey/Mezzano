;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defun apply-transforms (lambda target-architecture)
  (apply-transforms-1 lambda target-architecture))

(defgeneric apply-transforms-1 (form target-architecture))

(defmethod apply-transforms-1 ((form lexical-variable) target-architecture)
  form)

(defmethod apply-transforms-1 ((form lambda-information) target-architecture)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (apply-transforms-1 (second arg) target-architecture)))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (apply-transforms-1 (second arg) target-architecture)))
    (setf (lambda-information-body form)
          (apply-transforms-1 (lambda-information-body form) target-architecture)))
  form)

(defmethod apply-transforms-1 ((form ast-call) target-architecture)
 (let* ((matching-transform (match-transform form 't target-architecture))
        (new-form (if matching-transform
                      (apply-transform matching-transform (arguments form) form)
                      nil)))
   (cond (new-form
          (change-made)
          new-form)
         (t
          (setf (arguments form) (loop
                                    for arg in (arguments form)
                                    collect (apply-transforms-1 arg target-architecture)))
          form))))

(defmethod apply-transforms-1 ((form ast-block) target-architecture)
  (setf (body form) (apply-transforms-1 (body form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-function) target-architecture)
  (declare (ignore target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-go) target-architecture)
  (setf (info form) (apply-transforms-1 (info form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-if) target-architecture)
  (setf (test form) (apply-transforms-1 (test form) target-architecture)
        (if-then form) (apply-transforms-1 (if-then form) target-architecture)
        (if-else form) (apply-transforms-1 (if-else form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-let) target-architecture)
  (setf (bindings form) (loop
                           for (var initform) in (bindings form)
                           collect (list var (apply-transforms-1 initform target-architecture))))
  (setf (body form) (apply-transforms-1 (body form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-multiple-value-bind) target-architecture)
  (setf (value-form form) (apply-transforms-1 (value-form form) target-architecture)
        (body form) (apply-transforms-1 (body form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-multiple-value-call) target-architecture)
  (setf (function-form form) (apply-transforms-1 (function-form form) target-architecture)
        (value-form form) (apply-transforms-1 (value-form form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-multiple-value-prog1) target-architecture)
  (setf (value-form form) (apply-transforms-1 (value-form form) target-architecture)
        (body form) (apply-transforms-1 (body form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-progn) target-architecture)
  (setf (forms form) (loop
                        for subform in (forms form)
                        collect (apply-transforms-1 subform target-architecture)))
  form)

(defmethod apply-transforms-1 ((form ast-quote) target-architecture)
  (declare (ignore target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-return-from) target-architecture)
  (setf (value form) (apply-transforms-1 (value form) target-architecture)
        (info form) (apply-transforms-1 (info form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-setq) target-architecture)
  (setf (value form) (apply-transforms-1 (value form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-tagbody) target-architecture)
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (apply-transforms-1 statement target-architecture))))
  form)

(defmethod apply-transforms-1 ((form ast-the) target-architecture)
  (cond ((typep (value form) 'ast-call)
         (let* ((matching-transform (match-transform (value form) (the-type form) target-architecture))
                (new-form (if matching-transform
                              (apply-transform matching-transform
                                               (arguments (value form))
                                               (value form))
                              nil)))
           (cond (new-form
                  (change-made)
                  (setf (value form) new-form))
                 (t
                  (setf (value form) (apply-transforms-1 (value form) target-architecture))))))
        (t
         (setf (value form) (apply-transforms-1 (value form) target-architecture))))
  form)

(defmethod apply-transforms-1 ((form ast-unwind-protect) target-architecture)
  (setf (protected-form form) (apply-transforms-1 (protected-form form) target-architecture))
  (setf (cleanup-function form) (apply-transforms-1 (cleanup-function form) target-architecture))
  form)

(defmethod apply-transforms-1 ((form ast-jump-table) target-architecture)
  (setf (value form) (apply-transforms-1 (value form) target-architecture))
  (setf (targets form) (loop
                          for target in (targets form)
                          collect (apply-transforms-1 target target-architecture)))
  form)

(defclass transform ()
  ((%function :initarg :function :reader transform-function)
   (%lambda-list :initarg :lambda-list :reader transform-lambda-list)
   (%argument-types :initarg :argument-types :reader transform-argument-types)
   (%result-type :initarg :result-type :reader transform-result-type)
   (%optimize :initarg :optimize :reader transform-optimize)
   (%architecture :initarg :architecture :reader transform-architecture)
   (%body :initarg :body :reader transform-body)
   (%documentation :initarg :documentation :reader transform-documentation))
  (:default-initargs
   :result-type 't
    :optimize '()
    :architecture 't
    :documentation nil))

(defmethod print-object ((object transform) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S -> ~S"
            (transform-function object)
            (mapcar #'list (transform-lambda-list object) (transform-argument-types object))
            (transform-result-type object))))

(defvar *transforms* '())

(defmacro define-transform (function lambda-list options &body body)
  (let ((lambda-parameters (loop
                              for l in lambda-list
                              collect (if (symbolp l)
                                          l
                                          (first l)))))
    `(register-transform (make-instance 'transform
                                        :function ',function
                                        :lambda-list ',lambda-parameters
                                        :argument-types ',(loop
                                                             for l in lambda-list
                                                             collect (if (symbolp l)
                                                                         't
                                                                         (or (second l) 't)))
                                        :body (lambda ,lambda-parameters
                                                (declare (ignorable ,@lambda-parameters))
                                                ,@body)
                                        ,@(loop for (option . rest) in options
                                             collect option
                                             collect (ecase option
                                                       (:documentation `',(first rest))
                                                       (:result-type `',(first rest))
                                                       (:optimize `',rest)
                                                       (:architecture `',rest)))))))

(defun register-transform (transform)
  (push transform *transforms*))

(defun match-optimize-settings (call optimize-qualities)
  (dolist (setting optimize-qualities
           t)
    (when (not (funcall (first setting)
                        (if (integerp (second setting))
                            (second setting)
                            (optimize-quality call (second setting)))
                        (if (integerp (third setting))
                            (third setting)
                            (optimize-quality call (third setting)))))
      (return nil))))

(defun match-transform-type (transform-type type)
  (and (not (compiler-subtypep type 'nil))
       (compiler-subtypep type transform-type)))

(defun match-transform-argument (transform-type argument)
  (cond ((eql transform-type 't))
        ((typep argument 'ast-the)
         (match-transform-type transform-type (the-type argument)))
        ((typep argument 'ast-quote)
         (typep (value argument) transform-type))))

(defun match-one-transform (transform call result-type target-architecture)
  (and (equal (transform-function transform) (ast-name call))
       (or (eql (transform-architecture transform) 't)
           (member target-architecture (transform-architecture transform)))
       (match-optimize-settings call (transform-optimize transform))
       (match-transform-type (transform-result-type transform) result-type)
       (eql (length (arguments call)) (length (transform-lambda-list transform)))
       (every #'match-transform-argument
              (transform-argument-types transform)
              (arguments call))))

(defun match-transform (call result-type target-architecture)
  (dolist (transform *transforms* nil)
    (when (match-one-transform transform call result-type target-architecture)
      (return transform))))

(defun apply-transform (transform arguments inherit)
  ;; Enforce left-to-right order of evaluation for arguments.
  (let* ((temps (loop
                   for arg in arguments
                   collect (make-instance 'lexical-variable
                                          :inherit arg
                                          :name (gensym)
                                          :definition-point *current-lambda*
                                          :use-count 1)))
         (new-form (apply (transform-body transform) temps)))
    (if new-form
        (ast `(let ,(loop
                       for temp in temps
                       for arg in arguments
                       collect (list temp arg))
                ,new-form)
             inherit)
        nil)))

;;; Unboxed fixnum arithmetic.
;;; These only apply at safety 0 as they can produce invalid values which
;;; can damage the system.

(defmacro define-fast-fixnum-transform-arith-two-arg (binary-fn fast-fn &key (result 'fixnum))
  `(define-transform ,binary-fn ((lhs fixnum) (rhs fixnum))
      ((:result-type ,result)
       (:optimize (= safety 0) (= speed 3)))
     `(the fixnum (call ,',fast-fn ,lhs ,rhs))))

(define-fast-fixnum-transform-arith-two-arg sys.int::binary-+ %fast-fixnum-+)
(define-fast-fixnum-transform-arith-two-arg sys.int::binary-- %fast-fixnum--)
(define-fast-fixnum-transform-arith-two-arg sys.int::binary-* %fast-fixnum-*)
(define-fast-fixnum-transform-arith-two-arg sys.int::%truncate %fast-fixnum-truncate)
(define-fast-fixnum-transform-arith-two-arg rem %fast-fixnum-rem)
(define-fast-fixnum-transform-arith-two-arg sys.int::binary-logior %fast-fixnum-logior :result t)
(define-fast-fixnum-transform-arith-two-arg sys.int::binary-logxor %fast-fixnum-logxor :result t)
(define-fast-fixnum-transform-arith-two-arg sys.int::binary-logand %fast-fixnum-logand :result t)
(define-fast-fixnum-transform-arith-two-arg mezzano.runtime::%fixnum-left-shift %fast-fixnum-left-shift)

(define-transform mezzano.runtime::%fixnum-right-shift (lhs (rhs (eql 0)))
    ((:optimize (= safety 0) (= speed 3)))
  lhs)

(define-transform mezzano.runtime::generic-right-shift (lhs (rhs (eql 0)))
    ((:optimize (= safety 0) (= speed 3)))
  lhs)

;;; Fixnum comparisons.

(define-transform sys.int::binary-= ((lhs fixnum) (rhs fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `(call eq ,lhs ,rhs))

(define-transform sys.int::binary-< ((lhs fixnum) (rhs fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `(call mezzano.runtime::%fixnum-< ,lhs ,rhs))

(define-transform sys.int::binary->= ((lhs fixnum) (rhs fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call mezzano.runtime::%fixnum-< ,lhs ,rhs)))

(define-transform sys.int::binary-> ((lhs fixnum) (rhs fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `(call mezzano.runtime::%fixnum-< ,rhs ,lhs))

(define-transform sys.int::binary-<= ((lhs fixnum) (rhs fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call mezzano.runtime::%fixnum-< ,rhs ,lhs)))

;;; Single-Float arithmetic.

(defmacro define-fast-single-float-transform-arith-two-arg (binary-fn fast-fn)
  `(define-transform ,binary-fn ((lhs single-float) (rhs single-float))
      ((:optimize (= safety 0) (= speed 3)))
     `(the single-float (call ,',fast-fn ,lhs ,rhs))))

(define-fast-single-float-transform-arith-two-arg sys.int::binary-+ sys.int::%%single-float-+)
(define-fast-single-float-transform-arith-two-arg sys.int::binary-- sys.int::%%single-float--)
(define-fast-single-float-transform-arith-two-arg sys.int::binary-* sys.int::%%single-float-*)
(define-fast-single-float-transform-arith-two-arg sys.int::binary-/ sys.int::%%single-float-/)

(define-transform float ((number fixnum) (prototype single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the single-float
        (call mezzano.runtime::%%coerce-fixnum-to-single-float ,number)))

(define-transform float ((number fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `(the single-float (call mezzano.runtime::%%coerce-fixnum-to-single-float ,number)))

(define-transform sys.int::%truncate ((number single-float) (divisor (eql 1)))
    ((:optimize (= safety 0) (= speed 3))
     (:result-type fixnum))
  `(call sys.int::%%truncate-single-float ,number))

(define-transform sys.int::binary-= ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call eq ,lhs ,rhs))

(define-transform sys.int::binary-< ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%single-float-< ,lhs ,rhs))

(define-transform sys.int::binary->= ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call sys.int::%%single-float-< ,lhs ,rhs)))

(define-transform sys.int::binary-> ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%single-float-< ,rhs ,lhs))

(define-transform sys.int::binary-<= ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call sys.int::%%single-float-< ,rhs ,lhs)))

;;; Double-Float arithmetic.

(defmacro define-fast-double-float-transform-arith-two-arg (binary-fn fast-fn)
  `(define-transform ,binary-fn ((lhs double-float) (rhs double-float))
      ((:optimize (= safety 0) (= speed 3)))
     `(the double-float (call ,',fast-fn ,lhs ,rhs))))

(define-fast-double-float-transform-arith-two-arg sys.int::binary-+ sys.int::%%double-float-+)
(define-fast-double-float-transform-arith-two-arg sys.int::binary-- sys.int::%%double-float--)
(define-fast-double-float-transform-arith-two-arg sys.int::binary-* sys.int::%%double-float-*)
(define-fast-double-float-transform-arith-two-arg sys.int::binary-/ sys.int::%%double-float-/)

(define-transform float ((number fixnum) (prototype double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the double-float
        (call mezzano.runtime::%%coerce-fixnum-to-double-float ,number)))

(define-transform sys.int::binary-= ((lhs double-float) (rhs double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call eq ,lhs ,rhs))

(define-transform sys.int::binary-< ((lhs double-float) (rhs double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%double-float-< ,lhs ,rhs))

(define-transform sys.int::binary->= ((lhs double-float) (rhs double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call sys.int::%%double-float-< ,lhs ,rhs)))

(define-transform sys.int::binary-> ((lhs double-float) (rhs double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%double-float-< ,rhs ,lhs))

(define-transform sys.int::binary-<= ((lhs double-float) (rhs double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call sys.int::%%double-float-< ,rhs ,lhs)))

;;; Fast array accesses. Unbounded and may attack at any time.
;;; Currently only functional on simple 1d arrays.

(defmacro define-fast-array-transform (type accessor)
  `(progn
     (define-transform sys.int::aref-1 ((array (simple-array ,type (*))) index)
         ((:optimize (= safety 0) (= speed 3)))
       `(progn
          (call sys.int::%bounds-check ,array ,index)
          (the ,',type (call ,',accessor ,array ,index))))
     (define-transform (setf sys.int::aref-1) (value (array (simple-array ,type (*))) index)
         ((:optimize (= safety 0) (= speed 3)))
       `(progn
          (call sys.int::%bounds-check ,array ,index)
          (the ,',type (call (setf ,',accessor) ,value ,array ,index))))))

(define-fast-array-transform t sys.int::%object-ref-t)
(define-fast-array-transform fixnum sys.int::%object-ref-t)
(define-fast-array-transform (unsigned-byte 64) sys.int::%object-ref-unsigned-byte-64)
(define-fast-array-transform (unsigned-byte 32) sys.int::%%object-ref-unsigned-byte-32)
(define-fast-array-transform (unsigned-byte 16) sys.int::%%object-ref-unsigned-byte-16)
(define-fast-array-transform (unsigned-byte 8) sys.int::%%object-ref-unsigned-byte-8)
(define-fast-array-transform (signed-byte 64) sys.int::%object-ref-signed-byte-64)
(define-fast-array-transform (signed-byte 32) sys.int::%%object-ref-signed-byte-32)
(define-fast-array-transform (signed-byte 16) sys.int::%%object-ref-signed-byte-16)
(define-fast-array-transform (signed-byte 8) sys.int::%%object-ref-signed-byte-8)
(define-fast-array-transform single-float sys.int::%%object-ref-single-float)
(define-fast-array-transform double-float sys.int::%%object-ref-double-float)

(define-transform length ((sequence (and (simple-array * (*))
                                         (not (simple-array character (*))))))
    ((:optimize (= safety 0) (= speed 3)))
  `(the (integer 0 ,array-dimension-limit) (call sys.int::%object-header-data ,sequence)))

;;; Misc transforms.

(defmacro define-type-predicate-transform (predicate type)
  `(progn
     (define-transform ,predicate ((object ,type))
         ((:optimize (= safety 0) (= speed 3)))
       `'t)
     (define-transform ,predicate ((object (not ,type)))
         ((:optimize (= safety 0) (= speed 3)))
       `'nil)))

(define-type-predicate-transform consp cons)
(define-type-predicate-transform vectorp vector)
(define-type-predicate-transform sys.int::fixnump fixnum)

(define-transform sys.int::%coerce-to-callable ((object function))
    ((:optimize (= safety 0) (= speed 3)))
  object)

(define-transform sys.int::enable-unsafe-struct-access ()
    ((:optimize (= safety 0)))
  `'t)

(define-transform sys.int::enable-unsafe-struct-access ()
    ((:optimize (/= safety 0)))
  `'nil)
