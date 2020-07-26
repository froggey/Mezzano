;;;; Atomic operations

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
                    (new (gensym "NEW")))
                (values '()
                        '()
                        old
                        new
                        `(funcall #'(cas symbol-global-value) ,old ,new ',expansion)
                        `(symbol-global-value ',expansion))))
             ((:special nil)
              ;; Fall back on CAS of SYMBOL-VALUE.
              (let ((old (gensym "OLD"))
                    (new (gensym "NEW")))
                (values '()
                        '()
                        old
                        new
                        `(funcall #'(cas symbol-value) ,old ,new ',expansion)
                        `(symbol-value ',expansion))))))
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

(defmacro cas (place old new)
  "Synonym for COMPARE-AND-SWAP."
  `(compare-and-swap ,place ,old ,new))

(defmacro compare-and-swap (place old new &environment environment)
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

(defmacro define-atomic-rmw-operation (name lambda-list function symbol-function struct-slot-function &key require-fixnum no-result cons-operations documentation)
  (assert (not (and require-fixnum cons-operations)) ()
          "Can't require fixnum type with CAR/CDR slots")
  (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or enable-keys keys allow-other-keys aux)
      (error "&KEYS and &AUX not permitted in define-atomic-rmw-operation lambda list"))
    ;; Use MAKE-SYMBOL instead of GENSYM to avoid producing a name like PLACE1234.
    ;; This is visible in the lambda list and is shown by Slime.
    (let* ((place (make-symbol "PLACE"))
           (env (gensym "ENV"))
           (all-names (append required (mapcar #'car optional) (if rest (list rest) '())))
           (all-name-syms (loop for name in all-names collect (gensym (string name)))))
      `(defmacro ,name (&environment ,env ,place ,@lambda-list)
         ,documentation
         (multiple-value-bind (vars vals old-sym new-sym cas-form read-form)
             (get-cas-expansion ,place ,env)
           (let ,(loop
                    for name in all-names
                    for sym in all-name-syms
                    collect (list sym `(gensym ,(string name))))
             `(let (,@(mapcar #'list vars vals)
                    ,,@(loop
                          for name in all-names
                          for sym in all-name-syms
                          collect ``(,,sym ,,name)))
                ,(cond
                   ;; If READ-FORM is of the form (SYMBOL-GLOBAL-VALUE 'foo), then we know
                   ;; this is a global symbol and can touch it directly.
                   ((and (typep read-form '(cons (eql symbol-global-value)
                                            (cons (cons (eql quote) (cons symbol null))
                                             null)))
                         ,(if require-fixnum
                              `(type-equal (mezzano.runtime::symbol-type (second (second read-form))) 'fixnum ,env)
                              't))
                    (,(if rest 'list* 'list)
                      ',symbol-function
                      `',(second (second read-form))
                      ,@all-name-syms))
                   ,@(when cons-operations
                       `(((typep read-form '(cons (member car first) (cons t null)))
                          (,(if rest 'list* 'list)
                            ',(car cons-operations)
                            (second read-form)
                            ,@all-name-syms))
                         ((typep read-form '(cons (member cdr rest) (cons t null)))
                          (,(if rest 'list* 'list)
                            ',(cdr cons-operations)
                            (second read-form)
                            ,@all-name-syms))))
                   ;; If it names a structure slot accessor, then perform that
                   ;; operation.
                   ((and (typep read-form '(cons symbol (cons t null)))
                         (struct-accessor-info (first read-form) ,env))
                    ;; Try for structure accessors
                    (multiple-value-bind (struct-name slot-name)
                        (struct-accessor-info (first read-form) ,env)
                      (,(if rest 'list* 'list)
                        ',struct-slot-function
                        (second read-form)
                        `',struct-name `',slot-name
                        ,@all-name-syms)))
                   (t
                    ;; Fall back on a CAS loop.
                    ;; TODO: Support directly on struct slots that have been declared fixnum.
                    `(loop
                        for ,old-sym = ,read-form
                        for ,new-sym = ,(,(if rest 'list* 'list)
                                          ',function
                                          old-sym
                                          ,@all-name-syms)
                        when (eq ,cas-form ,old-sym)
                        return ,,(if no-result '(values) 'old-sym)))))))))))

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

(defmacro atomic-decf (place &optional (delta 1))
  "Atomically decrement PLACE by DELTA.
PLACE must contain a fixnum and if overflow occurs then the resulting value
will be wrapped as though it were a fixnum-sized signed 2's complement integer.
DELTA must be a fixnum.
Returns the old value of PLACE."
  ;; FIXME: This won't work for subtracting MOST-NEGATIVE-FIXNUM.
  ;; Negating it produces (1+ MOST-POSITIVE-FIXNUM)
  `(atomic-incf ,place (- ,delta)))

(define-atomic-rmw-operation atomic-logandf (integer)
  logand
  %atomic-fixnum-logand-symbol
  %atomic-fixnum-logand-struct-slot
  :require-fixnum t
  :no-result t
  :documentation "Atomically perform (setf place (logxor place INTEGER)).
PLACE must contain a fixnum and INTEGER must be a fixnum.
Returns no values.")

(define-atomic-rmw-operation atomic-logiorf (integer)
  logior
  %atomic-fixnum-logior-symbol
  %atomic-fixnum-logior-struct-slot
  :require-fixnum t
  :no-result t
  :documentation "Atomically perform (setf place (logxor place INTEGER)).
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
