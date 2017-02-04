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
                 boundp makunbound
                 sys.int::symbol-global-boundp
                 sys.int::symbol-global-makunbound
                 symbol-global-p
                 symbol-constant-p
                 modifying-symbol-value))

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

(defun symbol-type (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (sys.int::%object-ref-t symbol sys.int::+symbol-type+))

(defun (setf symbol-type) (type symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (setf (sys.int::%object-ref-t symbol sys.int::+symbol-type+) type))

(defun check-symbol-value-type (value symbol)
  (let ((type (symbol-type symbol)))
    (when (not (eql type 't))
      (assert (typep value (symbol-type symbol))))))

(defun sys.int::symbol-global-value (symbol)
  (symbol-value-cell-value (symbol-global-value-cell symbol)))

(defun (setf sys.int::symbol-global-value) (value symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (modifying-symbol-value symbol)
  (check-symbol-value-type value symbol)
  (setf (symbol-value-cell-value (symbol-global-value-cell symbol)) value))

(defun (sys.int::cas sys.int::symbol-global-value) (old new symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (modifying-symbol-value symbol)
  (check-symbol-value-type new symbol)
  (sys.int::cas (symbol-value-cell-value (symbol-global-value-cell symbol))
                old new))

(define-compiler-macro symbol-value-cell (symbol)
  `(fast-symbol-value-cell ,symbol))

(defun symbol-value-cell (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (when (symbol-global-p symbol)
    (return-from symbol-value-cell
      (sys.int::%object-ref-t symbol sys.int::+symbol-value+)))
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
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (modifying-symbol-value symbol)
  (check-symbol-value-type value symbol)
  (setf (symbol-value-cell-value (symbol-value-cell symbol)) value))

(defun (sys.int::cas symbol-value) (old new symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (modifying-symbol-value symbol)
  (check-symbol-value-type new symbol)
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
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (modifying-symbol-value symbol)
  (symbol-value-cell-makunbound (symbol-value-cell symbol))
  symbol)

(defun sys.int::symbol-global-boundp (symbol)
  (symbol-value-cell-boundp (symbol-global-value-cell symbol)))

(defun sys.int::symbol-global-makunbound (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (modifying-symbol-value symbol)
  (symbol-value-cell-makunbound (symbol-global-value-cell symbol))
  symbol)

(defun symbol-constant-p (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (eql (ldb sys.int::+symbol-header-mode+ (sys.int::%object-header-data symbol))
       sys.int::+symbol-mode-constant+))

(defun symbol-global-p (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (eql (ldb sys.int::+symbol-header-mode+ (sys.int::%object-header-data symbol))
       sys.int::+symbol-mode-global+))

(defun modifying-symbol-value (symbol)
  (when (symbol-constant-p symbol)
    (cond ((or (keywordp symbol)
               (member symbol '(nil t)))
           (error "Attempting to modify constant symbol ~S" symbol))
          (t
           (cerror "Change the value" "Attempting to modify constant symbol ~S" symbol)))))

(defun sys.int::symbol-mode (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (ecase (ldb sys.int::+symbol-header-mode+ (sys.int::%object-header-data symbol))
    (#.sys.int::+symbol-mode-nil+ nil)
    (#.sys.int::+symbol-mode-special+ :special)
    (#.sys.int::+symbol-mode-constant+ :constant)
    (#.sys.int::+symbol-mode-symbol-macro+ :symbol-macro)
    (#.sys.int::+symbol-mode-global+ :global)))

(defun (setf sys.int::symbol-mode) (value symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (setf (ldb sys.int::+symbol-header-mode+ (sys.int::%object-header-data symbol))
        (ecase value
          ((nil) sys.int::+symbol-mode-nil+)
          ((:special) sys.int::+symbol-mode-special+)
          ((:constant) sys.int::+symbol-mode-constant+)
          ((:symbol-macro) sys.int::+symbol-mode-symbol-macro+)
          ((:global) sys.int::+symbol-mode-global+)))
  value)

(defun sys.int::%atomic-fixnum-add-symbol (symbol value)
  (sys.int::%atomic-fixnum-add-object (symbol-value-cell symbol)
                                      sys.int::+symbol-value-cell-value+
                                      value))
