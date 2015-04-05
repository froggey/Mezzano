;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for symbols.

(in-package :mezzano.runtime)

(declaim (inline symbolp
                 symbol-name
                 symbol-package (setf symbol-package)
                 sys.int::symbol-global-value (setf sys.int::symbol-global-value)
                 sys.int::%cas-symbol-global-value
                 symbol-plist (setf symbol-plist)))

(defun symbolp (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-symbol+))

(defun symbol-name (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (sys.int::%object-ref-t symbol sys.int::+symbol-name+))

(defun symbol-package (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (sys.int::%object-ref-t symbol sys.int::+symbol-package+))

(defun (setf symbol-package) (value symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (setf (sys.int::%object-ref-t symbol sys.int::+symbol-package+) value))

(defun sys.int::symbol-global-value (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (let ((value (sys.int::%object-ref-t symbol sys.int::+symbol-value+)))
    (when (sys.int::%unbound-value-p value)
      (sys.int::raise-unbound-error symbol))
    value))

(defun (setf sys.int::symbol-global-value) (value symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (setf (sys.int::%object-ref-t symbol sys.int::+symbol-value+) value))

(defun sys.int::%cas-symbol-global-value (symbol old new)
  ;; Should this check if the symbol is unbound as well?
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (sys.int::%cas-object symbol sys.int::+symbol-value+ old new))

(defun symbol-plist (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (sys.int::%object-ref-t symbol sys.int::+symbol-plist+))

;; TODO: What kind of type-checking should be done here?
(defun (setf symbol-plist) (value symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (setf (sys.int::%object-ref-t symbol sys.int::+symbol-plist+) value))
