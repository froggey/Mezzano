;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defun apply-transforms (lambda target-architecture)
  (with-metering (:type-transforms)
    (apply-transforms-1 lambda target-architecture)))

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

(defvar *transforms* (make-hash-table :test 'equal))

(defmacro define-transform (function lambda-list options &body body)
  (let* ((lambda-parameters (loop
                              for l in lambda-list
                              collect (if (symbolp l)
                                          l
                                          (first l))))
         (temps (loop
                   for l in lambda-parameters
                   collect (gensym (string l)))))
    `(register-transform (make-instance 'transform
                                        :function ',function
                                        :lambda-list ',lambda-parameters
                                        :argument-types ',(loop
                                                             for l in lambda-list
                                                             collect (if (symbolp l)
                                                                         't
                                                                         (or (second l) 't)))
                                        :body (lambda ,temps
                                                (declare (ignorable ,@temps)
                                                         (sys.int::lambda-name (transform ,function)))
                                                (let (,@(loop
                                                           for tmp in temps
                                                           for l in lambda-parameters
                                                           collect (list l `(first ,tmp)))
                                                      ,@(loop
                                                           for tmp in temps
                                                           for l in lambda-list
                                                           for (name type tyvar) = (if (symbolp l)
                                                                                       (list l)
                                                                                       l)
                                                           when tyvar
                                                           collect (list tyvar `(second ,tmp))))
                                                  (declare (ignorable ,@lambda-parameters))
                                                  ,@body))
                                        ,@(loop for (option . rest) in options
                                             collect option
                                             collect (ecase option
                                                       (:documentation `',(first rest))
                                                       (:result-type `',(first rest))
                                                       (:optimize `',rest)
                                                       (:architecture `',rest)))))))

(defun register-transform (transform)
  (push transform (gethash (transform-function transform) *transforms*)))

(defun match-optimize-settings (call optimize-qualities)
  (dolist (setting optimize-qualities
           t)
    (unless (funcall (first setting)
                        (if (integerp (second setting))
                            (second setting)
                            (optimize-quality call (second setting)))
                        (if (integerp (third setting))
                            (third setting)
                            (optimize-quality call (third setting))))
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

(defun value-type (value)
  (cond ((typep value 'ast-the)
         (the-type value))
        ((typep value 'ast-quote)
         `(eql ,(value value)))
        (t t)))

(defun match-one-transform (transform call result-type target-architecture)
  (and (or (eql (transform-architecture transform) 't)
           (member target-architecture (transform-architecture transform)))
       (match-optimize-settings call (transform-optimize transform))
       (match-transform-type (transform-result-type transform) result-type)
       (eql (length (arguments call)) (length (transform-lambda-list transform)))
       (every #'match-transform-argument
              (transform-argument-types transform)
              (arguments call))))

(defun match-transform (call result-type target-architecture)
  (let ((name (ast-name call)))
    (when (or (eql name 'sys.int::binary-logand)
              (not (eql (second (assoc name (ast-inline-declarations call))) 'notinline)))
      (dolist (transform (gethash name *transforms*) nil)
        (when (match-one-transform transform call result-type target-architecture)
          (return transform))))))

(defun apply-transform (transform arguments inherit)
  ;; Enforce left-to-right order of evaluation for arguments.
  (let* ((temps (loop
                   for arg in arguments
                   collect (list (make-instance 'lexical-variable
                                                :inherit arg
                                                :name (gensym)
                                                :definition-point *current-lambda*
                                                :use-count 1)
                                 (value-type arg))))
         (new-form (apply (transform-body transform) temps)))
    (if new-form
        (ast `(let ,(loop
                       for temp in temps
                       for arg in arguments
                       collect (list (first temp) arg))
                ,new-form)
             inherit)
        nil)))

;;; Unboxed fixnum arithmetic.
;;; These only apply at safety 0 as they can produce invalid values which
;;; can damage the system if the tpe declarations are incorrect;.

(defmacro define-fast-fixnum-transform-arith-two-arg (binary-fn fast-fn &key (result 'fixnum))
  `(define-transform ,binary-fn ((lhs fixnum) (rhs fixnum))
      ((:result-type ,result)
       (:optimize (= safety 0) (= speed 3)))
     `(the fixnum (call ,',fast-fn ,lhs ,rhs))))

(define-fast-fixnum-transform-arith-two-arg sys.int::binary-+ %fast-fixnum-+)
(define-fast-fixnum-transform-arith-two-arg mezzano.runtime::%fixnum-+ %fast-fixnum-+)
(define-fast-fixnum-transform-arith-two-arg sys.int::binary-- %fast-fixnum--)
(define-fast-fixnum-transform-arith-two-arg mezzano.runtime::%fixnum-- %fast-fixnum--)
(define-fast-fixnum-transform-arith-two-arg sys.int::binary-* %fast-fixnum-*)
(define-fast-fixnum-transform-arith-two-arg mezzano.runtime::%fixnum-* %fast-fixnum-*)
(define-fast-fixnum-transform-arith-two-arg sys.int::%truncate %fast-fixnum-truncate)
(define-fast-fixnum-transform-arith-two-arg mezzano.runtime::%fixnum-truncate %fast-fixnum-truncate)
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

(defmacro define-fast-single-float-transform-arith-two-arg (binary-fn generic-fn fast-fn)
  `(progn
     (define-transform ,binary-fn ((lhs single-float) (rhs single-float))
         ((:optimize (= safety 0) (= speed 3)))
       `(the single-float (call ,',fast-fn ,lhs ,rhs)))
     (define-transform ,generic-fn ((lhs single-float) (rhs single-float))
         ((:optimize (= safety 0) (= speed 3)))
       `(the single-float (call ,',fast-fn ,lhs ,rhs)))))

(define-fast-single-float-transform-arith-two-arg sys.int::binary-+ sys.int::generic-+ sys.int::%%single-float-+)
(define-fast-single-float-transform-arith-two-arg sys.int::binary-- sys.int::generic-- sys.int::%%single-float--)
(define-fast-single-float-transform-arith-two-arg sys.int::binary-* sys.int::generic-* sys.int::%%single-float-*)
(define-fast-single-float-transform-arith-two-arg sys.int::binary-/ sys.int::generic-/ sys.int::%%single-float-/)

(define-transform float ((number fixnum) (prototype single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the single-float
        (call mezzano.runtime::%%coerce-fixnum-to-single-float ,number)))

(define-transform float ((number single-float) (prototype single-float))
    ((:optimize (= safety 0) (= speed 3)))
  number)

(define-transform float ((number double-float) (prototype single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the single-float
        (call mezzano.runtime::%%coerce-double-float-to-single-float ,number)))

(define-transform float ((number fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `(the single-float (call mezzano.runtime::%%coerce-fixnum-to-single-float ,number)))

(define-transform sys.int::%truncate ((number single-float) (divisor (eql 1)))
    ((:optimize (= safety 0) (= speed 3))
     (:result-type fixnum))
  `(call sys.int::%%truncate-single-float ,number))

(define-transform sys.int::generic-truncate ((number single-float) (divisor (eql 1)))
    ((:optimize (= safety 0) (= speed 3))
     (:result-type fixnum))
  `(call sys.int::%%truncate-single-float ,number))

(define-transform abs ((number single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the single-float (call sys.int::%%single-float-abs ,number)))

;; Don't use EQ because NaNs are unorderable.
(define-transform sys.int::binary-= ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%single-float-= ,lhs ,rhs))

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

(define-transform sys.int::generic-= ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%single-float-= ,lhs ,rhs))

(define-transform sys.int::generic-< ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%single-float-< ,lhs ,rhs))

(define-transform sys.int::generic->= ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call sys.int::%%single-float-< ,lhs ,rhs)))

(define-transform sys.int::generic-> ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%single-float-< ,rhs ,lhs))

(define-transform sys.int::generic-<= ((lhs single-float) (rhs single-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call not (call sys.int::%%single-float-< ,rhs ,lhs)))

;;; Double-Float arithmetic.

(defmacro define-fast-double-float-transform-arith-two-arg (binary-fn generic-fn fast-fn)
  `(progn
     (define-transform ,binary-fn ((lhs double-float) (rhs double-float))
         ((:optimize (= safety 0) (= speed 3)))
       `(the double-float (call ,',fast-fn ,lhs ,rhs)))
     (define-transform ,generic-fn ((lhs double-float) (rhs double-float))
         ((:optimize (= safety 0) (= speed 3)))
       `(the double-float (call ,',fast-fn ,lhs ,rhs)))))

(define-fast-double-float-transform-arith-two-arg sys.int::binary-+ sys.int::generic-+ sys.int::%%double-float-+)
(define-fast-double-float-transform-arith-two-arg sys.int::binary-- sys.int::generic-- sys.int::%%double-float--)
(define-fast-double-float-transform-arith-two-arg sys.int::binary-* sys.int::generic-* sys.int::%%double-float-*)
(define-fast-double-float-transform-arith-two-arg sys.int::binary-/ sys.int::generic-/ sys.int::%%double-float-/)

(define-transform float ((number fixnum) (prototype double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the double-float
        (call mezzano.runtime::%%coerce-fixnum-to-double-float ,number)))

(define-transform float ((number single-float) (prototype double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the single-float
        (call mezzano.runtime::%%coerce-single-float-to-double-float ,number)))

(define-transform float ((number double-float) (prototype double-float))
    ((:optimize (= safety 0) (= speed 3)))
  number)

(define-transform sys.int::%truncate ((number double-float) (divisor (eql 1)))
    ((:optimize (= safety 0) (= speed 3))
     (:result-type fixnum))
  `(call sys.int::%%truncate-double-float ,number))

(define-transform sys.int::generic-truncate ((number double-float) (divisor (eql 1)))
    ((:optimize (= safety 0) (= speed 3))
     (:result-type fixnum))
  `(call sys.int::%%truncate-double-float ,number))

(define-transform abs ((number double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(the double-float (call sys.int::%%double-float-abs ,number)))

;; Don't use EQ because double floats are not immediates.
(define-transform sys.int::binary-= ((lhs double-float) (rhs double-float))
    ((:optimize (= safety 0) (= speed 3)))
  `(call sys.int::%%double-float-= ,lhs ,rhs))

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

;;; Fast array accesses, avoid type-dispatch in AREF.
;;; These check bounds, but not the type.

(defun insert-bounds-check (array array-type index index-type &key (adjust 0))
  ;; Index type should look like (EQL integer)
  (let ((index-value (and (typep index-type '(cons (eql eql) (cons integer null)))
                          (second index-type)))
        ;; Array type should look like (SIMPLE-ARRAY t (integer))
        (array-limit (and (typep array-type '(cons (eql simple-array) (cons t (cons (cons integer null) null))))
                          (first (third array-type)))))
    (cond ((and index-value
                array-limit
                (<= 0 index-value (1- (- array-limit adjust))))
           ;; Elide bounds check for fixed-sized arrays with a known index.
           ''nil)
          ((zerop adjust)
           `(call sys.int::%bounds-check ,array ,index))
          (t
           ;; FIXME: This isn't quite right, it'll miss cases where INDEX
           ;; is slightly negative.
           `(call sys.int::%bounds-check ,array (call %fast-fixnum-+ ,index ',adjust))))))

(defmacro define-fast-array-transform (type accessor)
  `(progn
     (define-transform row-major-aref ((array (and (simple-array ,type *)
                                                   (not (simple-array ,type (*)))))
                                       (index fixnum))
         ((:optimize (= safety 0) (= speed 3)))
       `(let ((storage (call sys.int::%object-ref-t ,array ',sys.int::+complex-array-storage+)))
          (progn
            (call sys.int::%bounds-check storage ,index)
            (the ,',type (call ,',accessor storage ,index)))))
     (define-transform (setf row-major-aref) (value
                                              (array (and (simple-array ,type *)
                                                          (not (simple-array ,type (*)))))
                                              (index fixnum))
         ((:optimize (= safety 0) (= speed 3)))
       `(let ((storage (call sys.int::%object-ref-t ,array ',sys.int::+complex-array-storage+)))
          (progn
            (call sys.int::%bounds-check storage ,index)
            (the ,',type (call (setf ,',accessor) ,value storage ,index)))))
     (define-transform row-major-aref ((array (simple-array ,type (*)) array-type) (index fixnum index-type))
         ((:optimize (= safety 0) (= speed 3)))
       `(progn
          ,(insert-bounds-check array array-type index index-type)
          (the ,',type (call ,',accessor ,array ,index))))
     (define-transform (setf row-major-aref) (value (array (simple-array ,type (*)) array-type) (index fixnum index-type))
         ((:optimize (= safety 0) (= speed 3)))
       `(progn
          ,(insert-bounds-check array array-type index index-type)
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
(define-fast-array-transform double-float sys.int::%object-ref-double-float)

;;; AREF and AREF-n transforms.

(macrolet ((def (name n)
             (let ((indices (loop
                               repeat n
                               collect (gensym "INDEX"))))
               `(progn
                  (define-transform ,name ((array (array * ,(make-list n :initial-element '*)))
                                           ,@(loop
                                                for index in indices
                                                collect (list index 'fixnum)))
                      ((:optimize (= safety 0) (= speed 3)))
                    `(call row-major-aref ,array (call array-row-major-index ,array ,,@indices)))
                  (define-transform (setf ,name) (value
                                                  (array (array * ,(make-list n :initial-element '*)))
                                                  ,@(loop
                                                       for index in indices
                                                       collect (list index 'fixnum)))
                      ((:optimize (= safety 0) (= speed 3)))
                    `(call (setf row-major-aref) ,value ,array (call array-row-major-index ,array ,,@indices)))))))
  (def aref 0)
  (def aref 1)
  (def aref 2)
  (def aref 3)
  (def aref 4)
  (def sys.int::aref-1 1)
  (def sys.int::aref-2 2)
  (def sys.int::aref-3 3))

;;; ARRAY-ROW-MAJOR-INDEX transforms.
(macrolet ((def (n)
             (let ((indices (loop
                               repeat n
                               collect (gensym "INDEX"))))
               `(define-transform array-row-major-index ((array (array * ,(make-list n :initial-element '*)))
                                                         ,@(loop
                                                              for index in indices
                                                              collect (list index 'fixnum)))
                    ((:optimize (= safety 0) (= speed 3)))
                  ,(if (zerop n)
                       '''0
                       (loop
                          with current = (first indices)
                          for dim from 1
                          for i from 0
                          for index in (rest indices)
                          do
                            (setf current ``(the fixnum
                                                 (call %fast-fixnum-+
                                                       (the fixnum
                                                            (call %fast-fixnum-*
                                                                  ,,current
                                                                  (call array-dimension ,array ',',dim)))
                                                       ,,index)))
                          finally
                            (return current)))))))
  (def 0)
  (def 1)
  (def 2)
  (def 3)
  (def 4))

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

(define-transform sys.int::fixnump ((object fixnum))
    ((:optimize (= safety 0) (= speed 3)))
  `'t)
(define-transform sys.int::fixnump ((object (not integer)))
    ((:optimize (= safety 0) (= speed 3)))
  `'nil)

(define-transform sys.int::%coerce-to-callable ((object function))
    ((:optimize (= safety 0) (= speed 3)))
  object)

(define-transform list ()
    ()
  `'nil)

(define-transform list* (object)
    ()
  object)
