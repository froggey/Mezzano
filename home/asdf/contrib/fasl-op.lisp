(uiop:define-package :asdf/contrib/fasl-op
  (:use :common-lisp :uiop
        :asdf :asdf/component :asdf/operation :asdf/lisp-action :asdf/bundle))

(in-package :asdf/contrib/fasl-op)

;;; Backward compatibility with pre-3.1.2 names

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun frob-symbol (sym)
    (loop :for dest :in '(:asdf/bundle :asdf/interface) :do
      (uiop/package::ensure-import
       (symbol-name sym) (find-package dest) (symbol-package sym)
       (make-hash-table :test 'equal) (make-hash-table :test 'equal)))))

(defmacro declare-ops (&rest ops)
  `(progn
     ,@(loop :for (compat-name current-name) :in ops :append
             `((defclass ,compat-name (selfward-operation)
                 ((selfward-operation :initform ',current-name :allocation :class)))
               (defmethod output-files ((o ,compat-name) (c component))
                 (output-files (find-operation o ',current-name) c))
               (frob-symbol ',compat-name)))))

(declare-ops
 (fasl-op compile-bundle-op)
 (load-fasl-op load-bundle-op)
 (binary-op deliver-asd-op)
 (monolithic-fasl-op monolithic-compile-bundle-op)
 (monolithic-load-fasl-op monolithic-load-bundle-op)
 (monolithic-binary-op monolithic-deliver-asd-op))
