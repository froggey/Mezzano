;;;; Atomic operations

(in-package :mezzano.internals)

(deftype atomic-order ()
  `(member :relaxed :release :acquire :acq-rel :seq-cst))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun cas-failure-order (success-order)
  "Return the appropriate default failure order for SUCCESS-ORDER."
  (ecase success-order
    (:relaxed :relaxed)
    (:acquire :acquire)
    (:release :relaxed)
    (:acq-rel :acquire)
    (:seq-cst :seq-cst)))

;; Returns many values:
;; list of temporary variables
;; list of value-forms whose results those variable must be bound
;; temporary variable for the old value of place
;; temporary variable for the new value of place
;; temporary variable for the success-ordering
;; temporary variable for the failure-ordering
;; temporary variable for the load-ordering
;; temporary variable for the store-ordering
;; form using the aforementioned temporaries which performs the compare-and-swap operation on place
;; form using the aforementioned temporaries with which to perform an atomic read of place
;; form using the aforementioned temporaries with which to perform an atomic write of place
(defun get-atomic-expansion (place &optional environment)
  (let ((expansion (macroexpand place environment)))
    (cond ((symbolp expansion)
           ;; Lexical variables would be ok to cas (captured variables, etc),
           ;; but need a compiler extension (casq).
           (when (not (typep (mezzano.compiler::lookup-variable-in-environment
                              expansion environment)
                             'mezzano.compiler::special-variable))
             (error "CAS on lexical variable ~S not implemented." expansion))
           (ecase (symbol-mode expansion)
             (:constant
              (error "CAS on constant ~S not allowed." expansion))
             (:symbol-macro
              ;; Not possible - symbol macros would have been expanded.
              (error "CAS on symbol-macro ~S not allowed." expansion))
             (:global
              (let ((old (gensym "OLD"))
                    (new (gensym "NEW"))
                    (so (gensym "SUCCESS-ORDER"))
                    (fo (gensym "FAILURE-ORDER"))
                    (ro (gensym "LOAD-ORDER"))
                    (wo (gensym "STORE-ORDER")))
                (values '()
                        '()
                        old new
                        so fo ro wo
                        `(funcall #'(cas symbol-global-value) ,old ,new ,so ,fo ',expansion)
                        `(%atomic-symbol-global-value ',expansion ,ro)
                        `(funcall #'(setf %atomic-symbol-global-value) ,new ',expansion ,wo))))
             ((:special nil)
              ;; Fall back on CAS of SYMBOL-VALUE.
              (let ((old (gensym "OLD"))
                    (new (gensym "NEW"))
                    (so (gensym "SUCCESS-ORDER"))
                    (fo (gensym "FAILURE-ORDER"))
                    (ro (gensym "LOAD-ORDER"))
                    (wo (gensym "STORE-ORDER")))
                (values '()
                        '()
                        old new
                        so fo ro wo
                        `(funcall #'(cas symbol-value) ,old ,new ,so ,fo ',expansion)
                        `(%atomic-symbol-value ',expansion ,ro)
                        `(funcall #'(setf %atomic-symbol-value) ,new ',expansion ,wo))))))
          (t
           ;; All other CAS forms are currently functions!
           (let ((old (gensym "OLD"))
                 (new (gensym "NEW"))
                 (so (gensym "SUCCESS-ORDER"))
                 (fo (gensym "FAILURE-ORDER"))
                 (ro (gensym "LOAD-ORDER"))
                 (wo (gensym "STORE-ORDER"))
                 (vars (loop
                         for arg in (rest expansion)
                         collect (gensym))))
             (multiple-value-bind (read-form write-form)
                 (let* ((name (first expansion))
                        ;; Huge hack! Dig into the compiler's inline expansions
                        ;; to see if we can identify if this is a structure slot
                        ;; access. Use the setter to ensure we respect read-only-p
                        (inline-form (nth-value
                                      1 (mezzano.compiler::function-inline-info
                                         `(setf ,name))))
                        (structure-slot-form
                          (ignore-errors
                           (second (third (fifth inline-form))))))
                   (cond
                     ((and (consp structure-slot-form)
                           (eql (first structure-slot-form) '%struct-slot))
                      ;; It is!
                      (values `(%atomic-struct-slot ,@vars ,@(cddr structure-slot-form) ,ro)
                              `(funcall #'(setf %atomic-struct-slot) ,new ,@vars ,@(cddr structure-slot-form) ,wo)))
                     ((eql name '%object-ref-t)
                      (values `(%atomic-object-ref-t ,@vars ,ro)
                              `(funcall #'(setf %atomic-object-ref-t) ,new ,@vars ,wo)))
                     (t
                      (values `(error "Atomic loads not available for place ~S" ',name)
                              `(error "Atomic stores not available for place ~S" ',name)))))
               (values vars
                       (copy-list (rest expansion))
                       old new
                       so fo ro wo
                       `(funcall #'(cas ,(first expansion)) ,old ,new ,so ,fo ,@vars)
                       read-form
                       write-form)))))))

)

(defmacro cas (place old new &optional success-order failure-order)
  "Synonym for COMPARE-AND-SWAP."
  `(compare-and-swap ,place ,old ,new ,success-order ,failure-order))

(defmacro compare-and-swap (place old new &optional success-order failure-order &environment environment)
  (when (null success-order)
    (setf success-order :seq-cst))
  (when (and (null failure-order)
             (keywordp success-order))
    (setf failure-order (cas-failure-order success-order)))
  (multiple-value-bind (vars vals old-sym new-sym so fo ro wo cas-form)
      (get-atomic-expansion place environment)
    (declare (ignore ro wo))
    `(let (,@(mapcar #'list vars vals)
           (,old-sym ,old)
           (,new-sym ,new)
           (,so ,success-order)
           (,fo ,failure-order))
       ,cas-form)))

(defmacro atomic (place &optional order &environment environment)
  (when (null order)
    (setf order :seq-cst))
  (multiple-value-bind (vars vals old-sym new-sym so fo ro wo cas-form read-form write-form)
      (get-atomic-expansion place environment)
    (declare (ignore old-sym new-sym so fo wo cas-form write-form))
    `(let (,@(mapcar #'list vars vals)
           (,ro ,order))
       ,read-form)))

(define-setf-expander atomic (place &optional order &environment environment)
  (when (null order)
    (setf order :seq-cst))
  (multiple-value-bind (vars vals old-sym new-sym so fo ro wo cas-form read-form write-form)
      (get-atomic-expansion place environment)
    (declare (ignore old-sym so fo cas-form))
    (let ((order-sym (gensym "ORDER")))
      (values (append vars (list order-sym))
              (append vals (list order))
              (list new-sym)
              `(let ((,wo ,order-sym))
                 ,write-form
                 ,new-sym)
              `(let ((,ro ,order-sym))
                 ,read-form)))))

(declaim (inline wrapping-fixnum-+))
(defun wrapping-fixnum-+ (x y)
  "+ on fixnums, implementing 2's complement wrapping behaviour.
Returns a fixnum. X & Y must be fixnums."
  (when (not (fixnump x))
    (raise-type-error x 'fixnum)
    (%%unreachable))
  (when (not (fixnump y))
    (raise-type-error y 'fixnum)
    (%%unreachable))
  ;; FIXME: %FAST-FIXNUM-+ isn't the right function to use.
  ;; The behaviour on overflow is undefined, not wrapping, but the current
  ;; implementation wraps the result.
  (the fixnum (mezzano.compiler::%fast-fixnum-+ x y)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun struct-accessor-info (name &optional environment)
    "Return the structure type and slot name for the struct accessor function NAME.
If NAME does not name a struct accessor, then NIL is returned."
    (declare (ignore environment))
    ;; Grovel around in the inline form to look for a %struct-slot call.
    (ignore-errors
      (let ((inline-form (nth-value 1 (mezzano.compiler::function-inline-info name))))
        ;; Accessors have the form:
        ;; (LAMBDA (OBJECT) ... (BLOCK name (%STRUCT-SLOT OBJECT 'struct-name 'slot-name)))
        (let ((potential-struct-slot-form (first (last (first (last inline-form))))))
          (when (typep potential-struct-slot-form
                       '(cons (eql %struct-slot)
                         (cons (eql object)
                          (cons (cons (eql quote) (cons symbol null))
                           (cons (cons (eql quote) (cons symbol null))
                                 null)))))
            (values (second (third potential-struct-slot-form))
                    (second (fourth potential-struct-slot-form)))))))))

(defmacro define-atomic-rmw-operation (name lambda-list function symbol-function struct-slot-function &key require-fixnum no-result cons-operations documentation)
  (assert (not (and require-fixnum cons-operations)) ()
          "Can't require fixnum type with CAR/CDR slots")
  (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or rest enable-keys keys allow-other-keys aux)
      (error "&REST, &KEYS, and &AUX not permitted in define-atomic-rmw-operation lambda list"))
    ;; Use MAKE-SYMBOL instead of GENSYM to avoid producing a name like PLACE1234.
    ;; This is visible in the lambda list and is shown by Slime.
    (let* ((place (make-symbol "PLACE"))
           (env (gensym "ENV"))
           (all-names (append required (mapcar #'car optional)))
           (all-name-syms (loop for name in all-names collect (gensym (string name)))))
      `(defmacro ,name (&environment ,env ,place ,@lambda-list
                          ,@(if (member '&optional lambda-list)
                                '(order)
                                '(&optional order)))
         ,documentation
         (when (null order)
           (setf order :seq-cst))
         (multiple-value-bind (vars vals old-sym new-sym so fo ro wo cas-form read-form)
             (get-atomic-expansion ,place ,env)
           (declare (ignore wo))
           (let ,(loop
                    for name in all-names
                    for sym in all-name-syms
                    collect (list sym `(gensym ,(string name))))
             `(let (,@(mapcar #'list vars vals)
                    ,,@(loop
                          for name in all-names
                          for sym in all-name-syms
                          collect ``(,,sym ,,name)))
                (let* ((,so ,order)
                       (,fo ,so)
                       (,ro ,so))
                  (declare (ignorable ,so ,fo ,ro))
                  ,(cond
                     ;; If READ-FORM is of the form (SYMBOL-GLOBAL-VALUE 'foo), then we know
                     ;; this is a global symbol and can touch it directly.
                     ((and (typep read-form '(cons (eql mezzano.internals::%atomic-symbol-global-value)
                                              (cons (cons (eql quote) (cons symbol null))
                                               (cons t null))))
                           ,(if require-fixnum
                                `(type-equal (mezzano.runtime::symbol-type (second (second read-form))) 'fixnum ,env)
                                't))
                      (list
                       ',symbol-function
                       `',(second (second read-form))
                       ,@all-name-syms
                       so))
                     ,@(when cons-operations
                         `(((typep read-form '(cons (member car first) (cons t null)))
                            (list
                             ',(car cons-operations)
                             (second read-form)
                             ,@all-name-syms
                             so))
                           ((typep read-form '(cons (member cdr rest) (cons t null)))
                            (list
                             ',(cdr cons-operations)
                             (second read-form)
                             ,@all-name-syms
                             so))))
                     ;; If it names a structure slot accessor, then perform that
                     ;; operation.
                     ((typep read-form '(cons (eql %atomic-struct-slot) t))
                      ;; Try for structure accessors
                      (destructuring-bind (obj struct-name slot-name ord)
                          (rest read-form)
                        (list
                         ',struct-slot-function
                         obj
                         struct-name slot-name
                         ,@all-name-syms
                         ord)))
                     (t
                      ;; Fall back on a CAS loop.
                      ;; TODO: Support directly on struct slots that have been declared fixnum.
                      `(loop
                         for ,old-sym = ,read-form
                         for ,new-sym = ,(list
                                          ',function
                                          old-sym
                                          ,@all-name-syms)
                         when (eq ,cas-form ,old-sym)
                           return ,,(if no-result '(values) 'old-sym))))))))))))

;; TODO: We've got atomic ops that perform these operations directly. Use
;; them instead of expanding to CAS loops.

(define-atomic-rmw-operation atomic-incf (&optional (delta 1))
  wrapping-fixnum-+
  %atomic-fixnum-add-symbol
  %atomic-fixnum-add-struct-slot
  :require-fixnum t
  :documentation "Atomically increment PLACE by DELTA.
PLACE must contain a fixnum and if overflow occurs then the resulting value
will be wrapped as though it were a fixnum-sized signed 2's complement integer.
DELTA must be a fixnum.
Returns the old value of PLACE.")

(defmacro atomic-decf (place &optional (delta 1) order)
  "Atomically decrement PLACE by DELTA.
PLACE must contain a fixnum and if overflow occurs then the resulting value
will be wrapped as though it were a fixnum-sized signed 2's complement integer.
DELTA must be a fixnum.
Returns the old value of PLACE."
  ;; FIXME: This won't work for subtracting MOST-NEGATIVE-FIXNUM.
  ;; Negating it produces (1+ MOST-POSITIVE-FIXNUM)
  `(atomic-incf ,place (- ,delta) ,order))

(define-atomic-rmw-operation atomic-logandf (integer)
  logand
  %atomic-fixnum-logand-symbol
  %atomic-fixnum-logand-struct-slot
  :require-fixnum t
  :no-result t
  :documentation "Atomically perform (setf place (logand place INTEGER)).
PLACE must contain a fixnum and INTEGER must be a fixnum.
Returns no values.")

(define-atomic-rmw-operation atomic-logiorf (integer)
  logior
  %atomic-fixnum-logior-symbol
  %atomic-fixnum-logior-struct-slot
  :require-fixnum t
  :no-result t
  :documentation "Atomically perform (setf place (logior place INTEGER)).
PLACE must contain a fixnum and INTEGER must be a fixnum.
Returns no values.")

(define-atomic-rmw-operation atomic-logxorf (integer)
  logxor
  %atomic-fixnum-logxor-symbol
  %atomic-fixnum-logxor-struct-slot
  :require-fixnum t
  :no-result t
  :documentation "Atomically perform (setf place (logxor place INTEGER)).
PLACE must contain a fixnum and INTEGER must be a fixnum.
Returns no values.")

(define-atomic-rmw-operation atomic-swapf (new-value)
  (lambda (old new) (declare (ignore old)) new)
  %atomic-swap-symbol
  %atomic-swap-struct-slot
  :cons-operations (%atomic-swap-car . %atomic-swap-cdr)
  :documentation "Atomically set PLACE to NEW-VALUE.
Returns the old value of PLACE.")

(defmacro double-compare-and-swap (&environment env place-1 place-2 old-1 old-2 new-1 new-2 &optional success-order failure-order)
  (when (null success-order)
    (setf success-order :seq-cst))
  (when (and (null failure-order)
             (keywordp success-order))
    (setf failure-order (cas-failure-order success-order)))
  (let ((place-1 (macroexpand place-1 env))
        (place-2 (macroexpand place-2 env)))
    (cond ((and (typep place-1 '(cons (member car first) (cons t null)))
                (typep place-2 '(cons (member cdr rest) (cons t null))))
           ;; DCAS on the CAR/CDR of a CONS
           (let ((place-1-list (gensym "PLACE-1"))
                 (place-2-list (gensym "PLACE-2")))
             `(let ((,place-1-list ,(second place-1))
                    (,place-2-list ,(second place-2)))
                (when (not (eq ,place-1-list ,place-2-list))
                  (error "DCAS CAR/CDR used on disjoint objects ~S and ~S"
                         ,place-1-list ,place-2-list))
                (%dcas-cons ,place-1-list ,old-1 ,old-2 ,new-1 ,new-2
                            ,success-order ,failure-order))))
          ((and (typep place-1 '(cons (member cdr rest) (cons t null)))
                (typep place-2 '(cons (member car first) (cons t null))))
           ;; DCAS on the CAR/CDR of a CONS, with the arguments swapped.
           (let ((place-1-list (gensym "PLACE-1"))
                 (place-2-list (gensym "PLACE-2"))
                 (old-1-sym (gensym "OLD-1"))
                 (old-2-sym (gensym "OLD-2"))
                 (new-1-sym (gensym "NEW-1"))
                 (new-2-sym (gensym "NEW-2"))
                 (successp-sym (gensym "SUCCESSP"))
                 (value-1-sym (gensym "VALUE-1"))
                 (value-2-sym (gensym "VALUE-2")))
             `(let ((,place-1-list ,(second place-1))
                    (,place-2-list ,(second place-2))
                    (,old-1-sym ,old-1)
                    (,old-2-sym ,old-2)
                    (,new-1-sym ,new-1)
                    (,new-2-sym ,new-2))
                (when (not (eq ,place-1-list ,place-2-list))
                  (error "DCAS used on disjoint objects ~S and ~S"
                         ,place-1-list ,place-2-list))
                (multiple-value-bind (,successp-sym ,value-1-sym ,value-2-sym)
                    (%dcas-cons ,place-1-list ,old-2-sym ,old-1-sym ,new-2-sym ,new-1-sym
                                ,success-order ,failure-order)
                  (values ,successp-sym ,value-2-sym ,value-1-sym)))))
          ((and (typep place-1 '(cons symbol (cons t null)))
                (typep place-2 '(cons symbol (cons t null))))
           ;; Try for structure accessors
           (multiple-value-bind (place-1-struct place-1-slot)
               (struct-accessor-info (first place-1) env)
             (multiple-value-bind (place-2-struct place-2-slot)
                 (struct-accessor-info (first place-2) env)
               (when (or (not place-1-struct)
                         (not place-2-struct))
                 (error "Unsupported DCAS place pair ~S and ~S" place-1 place-2))
               (let ((place-1-object (gensym "PLACE-1"))
                     (place-2-object (gensym "PLACE-2")))
                 `(let ((,place-1-object ,(second place-1))
                        (,place-2-object ,(second place-2)))
                    (when (not (eq ,place-1-object ,place-2-object))
                      (error "DCAS used on disjoint objects ~S and ~S"
                             ,place-1-object ,place-2-object))
                    (%dcas-struct-slot ,place-1-object
                                       ',place-1-struct ',place-1-slot
                                       ',place-2-struct ',place-2-slot
                                       ,old-1 ,old-2
                                       ,new-1 ,new-2
                                       ,success-order ,failure-order))))))
          (t
           (error "Unsupported DCAS place pair ~S and ~S" place-1 place-2)))))
