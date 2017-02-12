(in-package :sys.c)

;;; Global environment.

(defmethod lookup-variable-in-environment (symbol (environment null))
  (multiple-value-bind (expansion expandedp)
      (sys.int::symbol-macroexpand-1 symbol)
    (if expandedp
        (make-instance 'symbol-macro :name symbol :expansion expansion)
        (make-instance 'special-variable
                       :name symbol
                       :implicitly-declared (not (sys.int::variable-information symbol))))))

(defmethod lookup-function-in-environment (name (environment null))
  (make-instance 'top-level-function :name name))

(defmethod inline-info-in-environment (name (environment null))
  (function-inline-info name))

(defmethod lookup-block-in-environment (tag (environment null))
  nil)

(defmethod lookup-go-tag-in-environment (tag (environment null))
  nil)

(defmethod environment-macro-definitions-only ((environment null))
  nil)

(defmethod compiler-macro-function-in-environment (name (environment null))
  (compiler-macro-function name))

(defmethod macro-function-in-environment (symbol (environment null))
  (macro-function symbol))

(defmethod lookup-variable-declared-type-in-environment (symbol (environment null))
  (mezzano.runtime::symbol-type symbol))

(defmethod optimize-qualities-in-environment ((environment null))
  '())
