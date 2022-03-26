;; Tests for slime-macrostep.  The following are expected failures:

;; - Under CLISP, highlighting of macro sub-forms fails because our
;;   pretty-printer dispatch table hacking causes infinite recursion:
;;   see comment in swank-macrostep.lisp

;; - COLLECT-MACRO-FORMS does not catch compiler macros under CLISP
;;   and ABCL

;; - Under CCL and ECL, compiler macro calls returned by
;;   COLLECT-MACRO-FORMS are not EQ to the original form, and so are
;;   not detected by the tracking pretty-printer mechanism.  This
;;   could be fixed by adding :TEST #'EQUAL to the POSITION call
;;   within MAKE-TRACKING-PPRINT-DISPATCH, at the cost of introducing
;;   false positives.

;; ECL has two other issues:

;;   - it currently lacks a working SLIME defimplementation for
;;     MACROEXPAND-ALL (Github issue #157), without which none of the
;;     expand-in-context stuff works.

;;   - the environments consed up by its WALKER:MACROEXPAND-ALL
;;     function are slightly broken, and do not work when passed to
;;     MACROEXPAND-1 unless fixed up via

;;         (subst 'si::macro 'walker::macro env)

(require 'slime-macrostep)
(require 'slime-tests)
(require 'cl-lib)

(defun slime-macrostep-eval-definitions (definitions)
  (slime-check-top-level)
  (slime-compile-string definitions 0)
  (slime-sync-to-top-level 5))

(defmacro slime-macrostep-with-text (buffer-text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (lisp-mode)
     (save-excursion
       (insert ,buffer-text))
     ,@body))

(defun slime-macrostep-search (form)
  "Search forward for FORM, leaving point at its first character."
  (let ((case-fold-search t)
        (search-spaces-regexp "\\s-+"))
    (re-search-forward (regexp-quote form)))
  (goto-char (match-beginning 0)))



(def-slime-test (slime-macrostep-expand-defmacro)
    (definition buffer-text original expansion)
  "Test that simple macrostep expansion works."
  '(("(defmacro macrostep-dummy-macro (&rest args)
        `(expansion of ,@args))"

     "(progn
        (first body form)
        (second body form)
        (macrostep-dummy-macro (first (argument)) second (third argument))
        (remaining body forms))"

     "(macrostep-dummy-macro (first (argument)) second (third argument))"

     "(expansion of (first (argument)) second (third argument))"))
  (slime-macrostep-eval-definitions definition)
  (slime-macrostep-with-text buffer-text
    (slime-macrostep-search original)
    (macrostep-expand)
    (slime-test-expect "Macroexpansion is correct"
                       expansion
                       (downcase (slime-sexp-at-point))
                       #'slime-test-macroexpansion=)))

(def-slime-test (slime-macrostep-fontify-macros
                 (:fails-for "clisp" "ECL"))
    (definition buffer-text original subform)
  "Test that macro forms in expansions are font-locked"
  '(("(defmacro macrostep-dummy-1 (&rest args)
        `(expansion including (macrostep-dummy-2 ,@args)))
      (defmacro macrostep-dummy-2 (&rest args)
        `(final expansion of ,@args))"

     "(progn
        (first body form)
        (second body form)
        (macrostep-dummy-1 (first (argument)) second (third argument))
        (remaining body forms))"

     "(macrostep-dummy-1 (first (argument)) second (third argument))"

     "(macrostep-dummy-2 (first (argument)) second (third argument))"))
  (slime-macrostep-eval-definitions definition)
  (slime-macrostep-with-text buffer-text
    (slime-macrostep-search original)
    (macrostep-expand)
    (slime-macrostep-search subform)
    (forward-char)                      ; move over open paren
    (slime-check "Head of macro form in expansion is fontified correctly"
        (eq (get-char-property (point) 'font-lock-face)
         'macrostep-macro-face))))

(def-slime-test (slime-macrostep-fontify-compiler-macros
                 (:fails-for "armedbear" "clisp" "ccl" "ECL"))
    (definition buffer-text original subform)
  "Test that compiler-macro forms in expansions are font-locked"
  '(("(defmacro macrostep-dummy-3 (&rest args)
        `(expansion including (macrostep-dummy-4 ,@args)))
      (defun macrostep-dummy-4 (&rest args)
        args)
      (define-compiler-macro macrostep-dummy-4 (&rest args)
        `(compile-time expansion of ,@args))"

     "(progn
        (first body form)
        (second body form)
        (macrostep-dummy-3 first second third)
        (remaining body forms))"

     "(macrostep-dummy-3 first second third)"

     "(macrostep-dummy-4 first second third)"))
  (slime-macrostep-eval-definitions definition)
  (slime-macrostep-with-text buffer-text
    (slime-macrostep-search original)
    (let ((macrostep-expand-compiler-macros t))
      (macrostep-expand))
    (slime-macrostep-search subform)
    (forward-char)                      ; move over open paren
    (slime-check "Head of compiler-macro in expansion is fontified correctly"
        (eq (get-char-property (point) 'font-lock-face)
         'macrostep-compiler-macro-face))))

(def-slime-test (slime-macrostep-expand-macrolet
                 (:fails-for "ECL"))
    (definitions buffer-text expansions)
    "Test that calls to macrolet-defined macros are expanded."
    '((nil
       "(macrolet
            ((test (&rest args) `(expansion of ,@args)))
          (first body form)
          (second body form)
          (test (strawberry pie) and (apple pie))
          (final body form))"
       (("(test (strawberry pie) and (apple pie))"
         "(EXPANSION OF (STRAWBERRY PIE) AND (APPLE PIE))")))

      ;; From swank.lisp:
      (nil
       "(macrolet ((define-xref-action (xref-type handler)
                     `(defmethod xref-doit ((type (eql ,xref-type)) thing)
                        (declare (ignorable type))
                        (funcall ,handler thing))))
          (define-xref-action :calls        #'who-calls)
          (define-xref-action :calls-who    #'calls-who)
          (define-xref-action :references   #'who-references)
          (define-xref-action :binds        #'who-binds)
          (define-xref-action :macroexpands #'who-macroexpands)
          (define-xref-action :specializes  #'who-specializes)
          (define-xref-action :callers      #'list-callers)
          (define-xref-action :callees      #'list-callees))"
       (("(define-xref-action :calls        #'who-calls)"
         "(DEFMETHOD XREF-DOIT ((TYPE (EQL :CALLS)) THING)
            (DECLARE (IGNORABLE TYPE))
            (FUNCALL #'WHO-CALLS THING))")
        ("(define-xref-action :macroexpands #'who-macroexpands)"
         "(DEFMETHOD XREF-DOIT ((TYPE (EQL :MACROEXPANDS)) THING)
            (DECLARE (IGNORABLE TYPE))
            (FUNCALL #'WHO-MACROEXPANDS THING))")
        ("(define-xref-action :callees      #'list-callees)"
         "(DEFMETHOD XREF-DOIT ((TYPE (EQL :CALLEES)) THING)
            (DECLARE (IGNORABLE TYPE))
            (FUNCALL #'LIST-CALLEES THING))")))

      ;; Test expansion of shadowed definitions
      (nil
       "(macrolet
            ((test-macro (&rest forms) (cons 'outer-definition forms)))
          (test-macro first (call))
          (macrolet
              ((test-macro (&rest forms) (cons 'inner-definition forms)))
            (test-macro (second (call)))))"
       (("(test-macro first (call))"
         "(OUTER-DEFINITION FIRST (CALL))")
        ("(test-macro (second (call)))"
         "(INNER-DEFINITION (SECOND (CALL)))")))

      ;; Expansion of macro-defined local macros
      ("(defmacro with-local-dummy-macro (&rest body)
          `(macrolet ((dummy (&rest args) `(expansion (of) ,@args)))
             ,@body))"
       "(with-local-dummy-macro
           (dummy form (one))
           (dummy (form two)))"
       (("(dummy form (one))"
         "(EXPANSION (OF) FORM (ONE))")
        ("(dummy (form two))"
         "(EXPANSION (OF) (FORM TWO))"))))

  (when definitions
    (slime-macrostep-eval-definitions definitions))
  (slime-macrostep-with-text buffer-text
    ;; slime-test-macroexpansion= does not expect tab characters,
    ;; so make sure that Emacs does not insert them
    (let ((indent-tabs-mode nil))
      (cl-loop
       for (original expansion) in expansions
       do
       (goto-char (point-min))
       (slime-macrostep-search original)
       (macrostep-expand)
       (slime-test-expect "Macroexpansion is correct"
                          expansion
                          (slime-sexp-at-point)
                          #'slime-test-macroexpansion=)))))

(def-slime-test (slime-macrostep-fontify-local-macros
                 (:fails-for "clisp" "ECL"))
    ()
    "Test that locally-bound macros are highlighted in expansions."
    '(())
    (slime-macrostep-with-text
        "(macrolet ((frob (&rest args)
                      (if (zerop (length args))
                          nil
                          `(cons ,(car args) (frob ,@(cdr args))))))
           (frob 1 2 3 4 5))"
      (let ((expansions
             '(("(frob 1 2 3 4 5)"
                "(CONS 1 (FROB 2 3 4 5))"
                "(FROB 2 3 4 5)")
               ("(FROB 2 3 4 5)"
                "(CONS 2 (FROB 3 4 5))"
                "(FROB 3 4 5)")
               ("(FROB 3 4 5)"
                "(CONS 3 (FROB 4 5))"
                "(FROB 4 5)")
               ("(FROB 4 5)"
                "(CONS 4 (FROB 5))"
                "(FROB 5)")
               ("(FROB 5)"
                "(CONS 5 (FROB))"
                "(FROB)")
               ;; ("(FROB)"
               ;;  "NIL"
               ;;  nil)
               )))
        (cl-loop for (original expansion subform) in expansions
                 do
                 (goto-char (point-min))
                 (slime-macrostep-search original)
                 (macrostep-expand)
                 (slime-test-expect "Macroexpansion is correct"
                                    expansion
                                    (slime-sexp-at-point)
                                    #'slime-test-macroexpansion=)
                 (when subform
                   (slime-macrostep-search subform)
                   (forward-char)
                   (slime-check "Head of macro form in expansion is fontified correctly"
                       (eq (get-char-property (point) 'font-lock-face)
                        'macrostep-macro-face)))))))

(def-slime-test (slime-macrostep-handle-unreadable-objects)
    (definitions buffer-text subform expansion)
    "Check that macroexpansion succeeds in a context containing unreadable objects."
    '(("(defmacro macrostep-dummy-5 (&rest args)
          `(expansion of ,@args))"
       "(progn
          #<unreadable object>
          (macrostep-dummy-5 quux frob))"
       "(macrostep-dummy-5 quux frob)"
       "(EXPANSION OF QUUX FROB)"))
    (slime-macrostep-eval-definitions definitions)
    (slime-macrostep-with-text buffer-text
      (slime-macrostep-search subform)
      (macrostep-expand)
      (slime-test-expect "Macroexpansion is correct"
                         expansion
                         (slime-sexp-at-point)
                         #'slime-test-macroexpansion=)))

(provide 'slime-macrostep-tests)
