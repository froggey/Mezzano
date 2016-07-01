;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for symbols.

(in-package :mezzano.runtime)

;; Symbol value cell manipulation.

(declaim (inline symbol-value-cell-symbol
                 symbol-value-cell-value
                 (setf symbol-value-cell-value)
                 (sys.int::cas symbol-value-cell-value)
                 symbol-value-cell-boundp
                 symbol-value-cell-makunbound))

(defun symbol-value-cell-symbol (cell)
  (sys.int::%object-ref-t cell sys.int::+symbol-value-cell-symbol+))

(defun symbol-value-cell-value (cell)
  (let ((value (sys.int::%object-ref-t cell sys.int::+symbol-value-cell-value+)))
    (when (sys.int::%unbound-value-p value)
      (sys.int::raise-unbound-error
       (symbol-value-cell-symbol cell)))
    value))

(defun (setf symbol-value-cell-value) (value cell)
  (setf (sys.int::%object-ref-t cell sys.int::+symbol-value-cell-value+) value))

(defun (sys.int::cas symbol-value-cell-value) (old new cell)
  (multiple-value-bind (successp actual-value)
      (sys.int::%cas-object cell sys.int::+symbol-value-cell-value+ old new)
    (declare (ignore successp))
    actual-value))

(defun symbol-value-cell-boundp (cell)
  (not (sys.int::%unbound-value-p (sys.int::%object-ref-t cell sys.int::+symbol-value-cell-value+))))

(defun symbol-value-cell-makunbound (cell)
  (setf (sys.int::%object-ref-t cell sys.int::+symbol-value-cell-value+) (sys.int::%unbound-value)))

;; Symbol functions.

(declaim (inline symbolp
                 symbol-name
                 symbol-package (setf symbol-package)
                 symbol-global-value-cell
                 sys.int::symbol-global-value
                 (setf sys.int::symbol-global-value)
                 (sys.int::cas sys.int::symbol-global-value)
                 sys.int::symbol-value
                 (setf sys.int::symbol-value)
                 (sys.int::cas sys.int::symbol-value)
                 symbol-plist (setf symbol-plist)
                 boundp makunbound))

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

(defun symbol-global-value-cell (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (sys.int::%object-ref-t symbol sys.int::+symbol-value+))

(defun sys.int::symbol-global-value (symbol)
  (symbol-value-cell-value (symbol-global-value-cell symbol)))

(defun (setf sys.int::symbol-global-value) (value symbol)
  (setf (symbol-value-cell-value (symbol-global-value-cell symbol)) value))

(defun (sys.int::cas sys.int::symbol-global-value) (old new symbol)
  (sys.int::cas (symbol-value-cell-value (symbol-global-value-cell symbol))
                old new))

(defun symbol-value-cell (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  ;; Walk the special stack, looking for an associated binding.
  (do ((ssp (sys.int::%%special-stack-pointer)
            (sys.int::%object-ref-t ssp 0)))
      ((null ssp)
       ;; Fall back on the global cell.
       (sys.int::%object-ref-t symbol sys.int::+symbol-value+))
    (when (eq (sys.int::%object-ref-t ssp 1) symbol)
      (return ssp))))

(defun symbol-value (symbol)
  (symbol-value-cell-value (symbol-value-cell symbol)))

(defun (setf symbol-value) (value symbol)
  (setf (symbol-value-cell-value (symbol-value-cell symbol)) value))

(defun (sys.int::cas symbol-value) (old new symbol)
  (sys.int::cas (symbol-value-cell-value (symbol-value-cell symbol))
                old new))

(defun symbol-plist (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (sys.int::%object-ref-t symbol sys.int::+symbol-plist+))

;; TODO: What kind of type-checking should be done here?
(defun (setf symbol-plist) (value symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (setf (sys.int::%object-ref-t symbol sys.int::+symbol-plist+) value))

(defun boundp (symbol)
  (symbol-value-cell-boundp (symbol-value-cell symbol)))

(defun makunbound (symbol)
  (symbol-value-cell-makunbound (symbol-value-cell symbol))
  symbol)
