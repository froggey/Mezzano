(require 'slime-enclosing-context)
(require 'slime-tests)
(require 'cl-lib)

(def-slime-test enclosing-context.1
  (buffer-sexpr wished-bound-names wished-bound-functions)
  "Check that finding local definitions work."
  '(("(flet ((,nil ()))
	 (let ((bar 13)
	       (,foo 42))
	   *HERE*))"
     ;; We used to return ,foo here, but we do not anymore.  We
     ;; still return ,nil for the `slime-enclosing-bound-functions',
     ;; though. The first one is used for local M-., whereas the
     ;; latter is used for local autodoc. It does not seem too
     ;; important for local M-. to work on such names. \(The reason
     ;; that it does not work anymore, is that
     ;; `slime-symbol-at-point' now does TRT and does not return a
     ;; leading comma anymore.\)
     ("bar" nil nil)
     ((",nil" "()")))
    ("(flet ((foo ()))
         (quux)
         (bar *HERE*))"
     ("foo")
     (("foo" "()"))))
  (slime-check-top-level)
  (with-temp-buffer
    (let ((tmpbuf (current-buffer)))
      (lisp-mode)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (cl-multiple-value-bind (bound-names points)
	  (slime-enclosing-bound-names)
	(slime-check "Check enclosing bound names"
                     (cl-loop for name in wished-bound-names
                              always (member name bound-names))))
      (cl-multiple-value-bind (fn-names fn-arglists points)
	  (slime-enclosing-bound-functions)
	(slime-check "Check enclosing bound functions"
                     (cl-loop for (name arglist) in wished-bound-functions
                              always (and (member name fn-names)
                                          (member arglist fn-arglists)))))
      )))

(provide 'slime-enclosing-context-tests)
