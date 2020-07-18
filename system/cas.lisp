;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.internals)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; Follows SBCL's interface, returns six values:
;; list of temporary variables
;; list of value-forms whose results those variable must be bound
;; temporary variable for the old value of place
;; temporary variable for the new value of place
;; form using the aforementioned temporaries which performs the compare-and-swap operation on place
;; form using the aforementioned temporaries with which to perform a volatile read of place
;;
;; A volatile write form might be nice as well, for unlocking spinlocks...
(defun get-cas-expansion (place &optional environment)
  (let ((expansion (macroexpand place environment)))
    (cond ((symbolp expansion)
           ;; Lexical variables would be ok to cas (captured variables, etc),
           ;; but special variables wouldn't be.
           (when (not (eql (symbol-mode expansion) :global))
             (error "CAS on a non-global symbol or variable not supported."))
           (let ((old (gensym "OLD"))
                 (new (gensym "NEW")))
             (values '()
                     '()
                     old
                     new
                     `(funcall #'(cas symbol-global-value) ,old ,new ',expansion)
                     `(symbol-global-value ',expansion))))
          (t
           ;; All other CAS forms are currently functions!
           (let ((old (gensym "OLD"))
                 (new (gensym "NEW"))
                 (vars (loop
                          for arg in (rest expansion)
                          collect (gensym))))
             (values vars
                     (copy-list (rest expansion))
                     old
                     new
                     `(funcall #'(cas ,(first expansion)) ,old ,new ,@vars)
                     `(,(first expansion) ,@vars)))))))

)

(defmacro cas (place old new &environment environment)
  (multiple-value-bind (vars vals old-sym new-sym cas-form)
      (get-cas-expansion place environment)
    `(let (,@(mapcar #'list vars vals)
           (,old-sym ,old)
           (,new-sym ,new))
       ,cas-form)))

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

(defmacro atomic-incf (place &optional (delta 1) &environment environment)
  "Atomically increment PLACE by DELTA.
PLACE must contain a fixnum and if overflow occurs then the resulting value
will be wrapped as though it were a fixnum-sized signed 2's complement integer.
Returns the old value of PLACE."
  (multiple-value-bind (vars vals old-sym new-sym cas-form read-form)
      (get-cas-expansion place environment)
    (let ((delta-sym (gensym "DELTA")))
      ;; If READ-FORM is of the form (SYMBOL-GLOBAL-VALUE 'foo), then we know
      ;; this is a global symbol and can touch it directly.
      ;; Only do this when the symbol has been declaimed fixnum.
      (cond ((and (typep read-form '(cons (eql symbol-global-value)
                                     (cons (cons (eql quote) (cons symbol null))
                                      null)))
                  (type-equal (mezzano.runtime::symbol-type (second (second read-form))) 'fixnum environment))
             `(let ((,delta-sym ,delta))
                (when (not (fixnump ,delta-sym))
                  (raise-type-error ,delta-sym 'fixnum)
                  (%%unreachable))
                (%atomic-fixnum-add-symbol ',(second (second read-form)) ,delta-sym)))
          (t
           ;; Fall back on a CAS loop.
           ;; TODO: Support directly on struct slots that have been declared fixnum.
           `(let (,@(mapcar #'list vars vals)
                  (,delta-sym ,delta))
              (loop
                 for ,old-sym = ,read-form
                 for ,new-sym = (wrapping-fixnum-+ ,old-sym ,delta-sym)
                 when (eq ,cas-form ,old-sym)
                 return ,old-sym)))))))

(defmacro atomic-decf (place &optional (delta 1))
  "Like ATOMIC-INCF, but subtracting."
  `(atomic-incf ,place (- ,delta)))

(defmacro double-compare-and-swap (&environment env place-1 place-2 old-1 old-2 new-1 new-2)
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
                (%dcas-cons ,place-1-list ,old-1 ,old-2 ,new-1 ,new-2))))
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
                    (%dcas-cons ,place-1-list ,old-2-sym ,old-1-sym ,new-2-sym ,new-1-sym)
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
                                       ,new-1 ,new-2))))))
          (t
           (error "Unsupported DCAS place pair ~S and ~S" place-1 place-2)))))
