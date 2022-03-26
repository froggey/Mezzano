(cl:in-package #:clim-tests)

(def-suite* :mcclim.presentations
  :in :mcclim)


;;; Presentation Types
;;; ============================================================================

(defparameter *presentation-type-supertypes*
  '(;; 23.8.1
    (t)
    ;; NIL is a special case
    (null t) (boolean t) (symbol t) (keyword symbol t) (blank-area t)
    ;; 23.8.2
    (number t) (complex number t) (real number t) (rational real number t)
    (integer rational real number t) (ratio rational real number t)
    (float real number t)
    ;; 23.8.3
    (character t) (string t)
    ;; 23.8.4
    (pathname t)
    ;; 23.8.5
    ((completion nil) t)
    ;; not allowed abbreviations
    ;; (member t) ((member-sequence nil) t) ((member-alist nil) t)
    ((subset-completion nil) t)
    ;; (subset t) ((subset-sequence nil) t) ((subset-alist nil) t)
    ;; 23.8.6
    ((sequence t) t) (sequence-enumerated t)
    ;; 23.8.7
    ;;   OR, AND
    ;; 23.8.8
    ;;   ((token-or-type nil t) t) ((null-or-type t) t) ((type-or-string t) t)
    ;; 23.8.9
    (expression t)
    (form expression t)))

(defun expect-t-t (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is-true yesp)
    (is-true surep))
  #+mcclim
  ;; we can do this because *presentation-type-supertypes* doesn't do
  ;; clever things with type parameters
  (is-true (climi::stupid-subtypep type supertype)))

(defun expect-nil-t (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is-false yesp)
    (is-true surep))
  #+mcclim
  (is-false (climi::stupid-subtypep type supertype)))

(defun expect-nil-nil (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (is-false yesp)
    (is-false surep))
  ;; stupid-subtypep must be conservative in what it reports as
  ;; possibly acceptable.
  #+mcclim
  (is-true (climi::stupid-subtypep type supertype)))

(defun constantly-t (object)
  (declare (ignore object))
  t)

(test presentations.type-relations.1
  (loop for (type . supertypes) in *presentation-type-supertypes*
        do (expect-t-t type type)
        do (expect-t-t nil type)
           ;; if presentation types were "real" (FIXME: work out what
           ;; "real" means) types, then this wouldn't actually be true.
           ;; However, PRESENTATION-SUBTYPEP works by walking up the type
           ;; lattice until it finds a match, and only then checks the type
           ;; parameters.  So even though presentation types (or
           ;; abbreviations) like (MEMBER) actually denote the empty set,
           ;; they are not PRESENTATION-SUBTYPEP NIL.
        do (expect-nil-t type nil)
        do (mapcar (lambda (x) (expect-t-t type x)) supertypes)))

(test presentations.type-relations.2
  (loop for (type) in *presentation-type-supertypes*
        do (expect-t-t type `(and ,type))
        do (expect-t-t `(and ,type) type)
        do (expect-t-t `(and ,type) `(and ,type))
        do (expect-t-t type `(or ,type))
        do (expect-t-t `(or ,type) type)
        do (expect-t-t `(or ,type) `(or ,type))
        do (expect-t-t `(or ,type) `(and ,type))
        do (expect-t-t `(and ,type) `(or ,type))))

(test presentations.type-relations.3
  (loop for (type) in *presentation-type-supertypes*
        do (expect-t-t `(and ,type (satisfies constantly-t)) type)
        do (expect-nil-nil type `(and ,type (satisfies constantly-t)))
        do (expect-t-t `(and ,type (not nil)) type)
        do (expect-nil-nil type `(and ,type (not nil)))))

(test presentations.type-relations.4
  (expect-t-t '(or integer symbol) '(or integer symbol))
  (expect-t-t '(or integer symbol) '(or symbol integer))

  (expect-t-t '(or real complex) 'number)
  #+nil (expect-t-t 'number '(or real complex)))
