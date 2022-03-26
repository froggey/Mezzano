(require 'slime-c-p-c)
(require 'slime-tests)

(def-slime-test completions
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p" 
                      "cl:compiler-macro" "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" nil)
      ("swank::compile-file" (("swank::compile-file" 
                               "swank::compile-file-for-emacs"
                               "swank::compile-file-if-needed"
                               "swank::compile-file-output"
                               "swank::compile-file-pathname")
                              "swank::compile-file"))
      ("cl:m-v-l" (("cl:multiple-value-list" "cl:multiple-values-limit") "cl:multiple-value"))
      ("common-lisp" (("common-lisp-user:" "common-lisp:") "common-lisp")))
  (let ((completions (slime-completions prefix)))
    (slime-test-expect "Completion set" expected-completions completions)))

(def-slime-test complete-symbol*
    (buffer-sexp wished-completion &optional chosen-completion fancy unambiguous)
    "Ensure that completions are correctly inserted."
    '(("cl:and" "cl:and")
      ("(cl:and" "(cl:and")
      ("(cl:and)" "(cl:and)")
      ("(cl:and)" "(cl:and)" nil nil t)
      ;; Fancy completion of a form that accepts arguments should
      ;; insert a space after the completed form.
      ("(cl:and)" "(cl:and )" nil t)
      ;; ...but only for symbols in the funcall position.
      ("cl:and" "cl:and" nil t)
      ;; Fancy completion of a form without arguments should insert a
      ;; closing paren.
      ("(cl:get-internal-run-time" "(cl:get-internal-run-time)" nil t)
      ;; ...but only for symbols in the funcall position.
      ("cl:get-internal-run-time" "cl:get-internal-run-time" nil t)
      ("cl:m-v-b" "cl:multiple-value-bind")
      ("cl:m-v-l" "cl:multiple-value-list" "cl:multiple-value-list")
      ;; Fancy completion is only done for unique completions. This is
      ;; not a hard requirement, and might change in the future. This
      ;; test is included merely to document the current behavior.
      ("(cl:m-v-l)" "(cl:multiple-value-list)" "cl:multiple-value-list" t)
      ("cl:mult" "cl:multiple-value-call" "cl:multiple-value-call")
      ("cl:multiple-value" "cl:multiple-value-setq" "cl:multiple-value-setq")
      ("cl:compile" "cl:compile" "cl:compile")
      ("cl:compile" "cl:compile-file" "cl:compile-file")
      ("cl:f-o" "cl:force-output" "cl:force-output")
      ;; When `slime-c-p-c-unambiguous-prefix-p' is non nil,
      ;; `slime-complete-symbol*' will move point back to the
      ;; unambiguous portion of the prefix; however, the final result
      ;; after choosing a completion candidate should be the same.
      ("cl:f-o" "cl:force-output" "cl:force-output" nil t)
      ("(cl:f-o)" "(cl:force-output)" "cl:force-output" nil t)
      ;; Character completions
      ("#\\N" "#\\Newline")
      ("#\\R" "#\\Return" "#\\Return")
      ("#\\R" "#\\Rubout" "#\\Rubout" nil t)
      ;; Keyword completions
      ("(cl:find 'x '() :)" "(cl:find 'x '() :START)" ":START")
      ("(cl:find 'x '() :S)" "(cl:find 'x '() :START)")
      ("(cl:find 'x '() :s)" "(cl:find 'x '() :start)")
      ("(cl:find 'x '() :s)" "(cl:find 'x '() :start)" nil t)
      ("(cl:find 'x '() :t)" "(cl:find 'x '() :test)" ":test")
      ("(cl:find 'x '() :t)" "(cl:find 'x '() :test-not)" ":test-not" nil t))
  (slime-check-top-level)
  (save-window-excursion
    (with-temp-buffer
      (lisp-mode)
      (setq slime-buffer-package "SWANK")
      (insert buffer-sexp)
      (when (eq (char-before) ?\))
        (backward-char))
      (let ((slime-c-p-c-unambiguous-prefix-p unambiguous)
            (slime-complete-symbol*-fancy fancy))
        (if (not fancy)
            (slime-complete-symbol*)
          ;; `slime-complete-symbol*-fancy-bit' may call
          ;; `execute-kbd-macro', which ultimately operates on the
          ;; buffer associated with the selected window, not
          ;; necessarily the current buffer. Call `pop-to-buffer' to
          ;; ensure that the current buffer is in the selected window
          ;; before calling `slime-complete-symbol*'. Fancy completion
          ;; might also kick off a `slime-eval-async' in
          ;; `slime-echo-arglist', so ensure the output is consumed
          ;; with `slime-sync-to-top-level' before continuing.
          (pop-to-buffer (current-buffer))
          (slime-complete-symbol*)
          (slime-sync-to-top-level 1)))
      (when chosen-completion
        (with-selected-window slime-completions-window
          (goto-char (point-min))
          (search-forward chosen-completion)
          (choose-completion)))
      (slime-check-completed-form buffer-sexp wished-completion))))

(def-slime-test complete-form
    (buffer-sexpr wished-completion &optional skip-trailing-test-p)
    ""
    '(("(defmethod arglist-dispatch *HERE*"
       "(defmethod arglist-dispatch (operator arguments) body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct (*HERE*"
       "(with-struct (conc-name names...)" t)
      ("(with-struct (foo. bar baz *HERE*"
       "(with-struct (foo. bar baz names...)" t))
  (slime-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (setq slime-buffer-package "SWANK")
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slime-complete-form)
    (slime-check-completed-form buffer-sexpr wished-completion)

    ;; Now the same but with trailing `)' for paredit users...
    (unless skip-trailing-test-p
      (erase-buffer)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (delete-region (match-beginning 0) (match-end 0))
      (insert ")") (backward-char)
      (slime-complete-form)
      (slime-check-completed-form (concat buffer-sexpr ")") wished-completion))
    ))

(defun slime-check-completed-form (buffer-sexpr wished-completion)
  (slime-test-expect (format "Completed form for `%s' is as expected"
                              buffer-sexpr)
                     wished-completion
                     (buffer-string)
                     'equal))

(provide 'slime-c-p-c-tests)
