;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

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

(defmacro atomic-incf (place &optional (delta 1) &environment environment)
  "Atomically increment PLACE by DELTA.
PLACE must contain a fixnum and if overflow occurs then the resulting value
will be wrapped as though it were a fixnum-sized signed 2's complement integer.
Returns the old value of PLACE."
  (let ((expansion (macroexpand place environment))
        (delta-sym (gensym "DELTA")))
    ;; TODO: Support on struct slots that have been declared fixnum.
    (when (not (and (symbolp expansion)
                    (eql (symbol-mode expansion) :global)
                    (type-equal (mezzano.runtime::symbol-type expansion) 'fixnum)))
      (error "ATOMIC-INCF place ~S not supported. Must be global symbol declaimed fixnum."
             place))
    `(let ((,delta-sym ,delta))
       (when (not (fixnump ,delta-sym))
         (raise-type-error ,delta-sym 'fixnum)
         (%%unreachable))
       (%atomic-fixnum-add-symbol ',expansion ,delta-sym))))

(defmacro atomic-decf (place &optional (delta 1))
  "Like ATOMIC-INCF, but subtracting."
  `(atomic-incf ,place (- ,delta)))
