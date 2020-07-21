;;;; Low-level support functions for symbols.

(in-package :mezzano.runtime)

;; Symbol value cell manipulation.

(declaim (inline symbol-value-cell-p
                 symbol-value-cell-symbol
                 symbol-value-cell-value
                 (setf symbol-value-cell-value)
                 (sys.int::cas symbol-value-cell-value)
                 symbol-value-cell-boundp
                 symbol-value-cell-makunbound))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.int::%define-type-symbol
   'symbol-value-cell
   'symbol-value-cell-p))

(defun symbol-value-cell-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-symbol-value-cell+))

(defun symbol-global-value-cell-p (object)
  (and (symbol-value-cell-p object)
       (eq (sys.int::%object-header-data object) 4)))

(defun symbol-value-cell-symbol (cell)
  ;; Fetch the original global cell, then pull the symbol out of that.
  (sys.int::%object-ref-t
   (sys.int::%object-ref-t cell sys.int::+symbol-value-cell-symbol+)
   3))

(defun symbol-value-cell-value (cell)
  (let ((value (sys.int::%object-ref-t cell sys.int::+symbol-value-cell-value+)))
    (when (sys.int::%unbound-value-p value)
      (sys.int::raise-unbound-error
       (symbol-value-cell-symbol cell))
      (sys.int::%%unreachable))
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
                 sys.int::symbol-global-value
                 (setf sys.int::symbol-global-value)
                 (sys.int::cas sys.int::symbol-global-value)
                 sys.int::symbol-value
                 (setf sys.int::symbol-value)
                 (sys.int::cas sys.int::symbol-value)
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

(defun make-symbol-global-value-cell (symbol)
  (let ((global-value (%allocate-object sys.int::+object-tag-symbol-value-cell+
                                    4 4 :wired)))
    (setf (sys.int::%object-ref-t global-value sys.int::+symbol-value-cell-symbol+) global-value)
    (setf (sys.int::%object-ref-t global-value sys.int::+symbol-value-cell-value+) (sys.int::%unbound-value))
    (setf (sys.int::%object-ref-t global-value 3) symbol)
    global-value))

(defun symbol-global-value-cell (symbol &optional (create t))
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (let ((cell (sys.int::%object-ref-t symbol sys.int::+symbol-value+)))
    (or cell
        (and create
             (let ((new-cell (make-symbol-global-value-cell symbol)))
               ;; Try to atomically update the value cell.
               (multiple-value-bind (successp old-value)
                   (sys.int::%cas-object symbol sys.int::+symbol-value+ nil new-cell)
                 (if successp
                     new-cell
                     old-value)))))))

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
  (let ((sym (gensym)))
    `(let ((,sym ,symbol))
       (when (not (symbolp ,sym))
         (sys.int::raise-type-error ,sym 'symbol)
         (sys.int::%%unreachable))
       (fast-symbol-value-cell ,sym))))

#+x86-64
(defun fast-symbol-value-cell (symbol)
  (fast-symbol-value-cell symbol))

(defun symbol-value-cell (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (%symbol-value-cell-by-cell (symbol-global-value-cell symbol)))

(defun %symbol-value-cell-by-cell (symbol-value-cell)
  (let ((symbol (symbol-value-cell-symbol symbol-value-cell)))
    (when (symbol-global-p symbol)
      (return-from %symbol-value-cell-by-cell
        symbol-value-cell))
    ;; Walk the special stack, looking for an associated binding.
    (do ((ssp (sys.int::%%special-stack-pointer)
              (sys.int::%object-ref-t ssp 0)))
        ((null ssp)
         ;; Fall back on the global cell.
         symbol-value-cell)
      (when (eq (sys.int::%object-ref-t ssp 1) symbol-value-cell)
        (return ssp)))))

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

(defvar *symbol-plists*
  (make-hash-table :test #'eq
                   :synchronized t
                   :enforce-gc-invariant-keys t
                   :weakness :key))

(defun symbol-plist (symbol)
  (check-type symbol symbol)
  (values (gethash symbol *symbol-plists*)))

;; TODO: What kind of type-checking should be done here?
(defun (setf symbol-plist) (value symbol)
  (check-type symbol symbol)
  (if (endp value)
      (remhash symbol *symbol-plists*)
      (setf (gethash symbol *symbol-plists*) value))
  value)

(defun boundp (symbol)
  (when (symbol-global-value-cell symbol nil)
    (symbol-value-cell-boundp (symbol-value-cell symbol))))

(defun makunbound (symbol)
  (sys.int::%type-check symbol sys.int::+object-tag-symbol+ 'symbol)
  (modifying-symbol-value symbol)
  (when (symbol-global-value-cell symbol nil)
    (symbol-value-cell-makunbound (symbol-value-cell symbol)))
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

(declaim (inline sys.int::%atomic-fixnum-add-symbol))
(defun sys.int::%atomic-fixnum-add-symbol (symbol value)
  (when (not (sys.int::fixnump value))
    (sys.int::raise-type-error value 'fixnum))
  (modifying-symbol-value symbol)
  (sys.int::%atomic-fixnum-add-object (symbol-value-cell symbol)
                                      sys.int::+symbol-value-cell-value+
                                      value))

(declaim (inline sys.int::%atomic-fixnum-logand-symbol))
(defun sys.int::%atomic-fixnum-logand-symbol (symbol value)
  (when (not (sys.int::fixnump value))
    (sys.int::raise-type-error value 'fixnum))
  (modifying-symbol-value symbol)
  (sys.int::%atomic-fixnum-logand-object (symbol-value-cell symbol)
                                         sys.int::+symbol-value-cell-value+
                                         value))

(declaim (inline sys.int::%atomic-fixnum-logior-symbol))
(defun sys.int::%atomic-fixnum-logior-symbol (symbol value)
  (when (not (sys.int::fixnump value))
    (sys.int::raise-type-error value 'fixnum))
  (modifying-symbol-value symbol)
  (sys.int::%atomic-fixnum-logior-object (symbol-value-cell symbol)
                                         sys.int::+symbol-value-cell-value+
                                         value))

(declaim (inline sys.int::%atomic-fixnum-logxor-symbol))
(defun sys.int::%atomic-fixnum-logxor-symbol (symbol value)
  (when (not (sys.int::fixnump value))
    (sys.int::raise-type-error value 'fixnum))
  (modifying-symbol-value symbol)
  (sys.int::%atomic-fixnum-logxor-object (symbol-value-cell symbol)
                                         sys.int::+symbol-value-cell-value+
                                         value))

(declaim (inline sys.int::%atomic-swap-symbol))
(defun sys.int::%atomic-swap-symbol (symbol new-value)
  (modifying-symbol-value symbol)
  (check-symbol-value-type new-value symbol)
  (sys.int::%xchg-object (symbol-value-cell symbol)
                         sys.int::+symbol-value-cell-value+
                         new-value))
