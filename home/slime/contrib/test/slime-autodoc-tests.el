(require 'slime-autodoc)
(require 'slime-tests)
(require 'cl-lib)

(defun slime-autodoc-to-string ()
  "Retrieve and return autodoc for form at point."
  (let ((autodoc (car (slime-eval
		       `(swank:autodoc
			 ',(slime-autodoc--parse-context)
			 :print-right-margin
			 ,(window-width (minibuffer-window)))))))
    (if (eq autodoc :not-available)
        :not-available
        (slime-autodoc--canonicalize-whitespace autodoc))))

(defun slime-check-autodoc-at-point (arglist)
  (slime-test-expect (format "Autodoc in `%s' (at %d) is as expected"
                             (buffer-string) (point))
                     arglist
                     (slime-autodoc-to-string)))

(defmacro define-autodoc-tests (&rest specs)
  `(progn
     ,@(cl-loop
        for (buffer-sexpr wished-arglist . options)
        in specs
        for fails-for = (plist-get options :fails-for)
        for skip-trailing-test-p = (plist-get options :skip-trailing-test-p)
        for i from 1
        when (featurep 'ert)
        collect `(define-slime-ert-test ,(intern (format "autodoc-tests-%d" i))
                   ()
                   ,(format "Check autodoc works ok for %s" buffer-sexpr)
                   ,@(if fails-for
                         `(:expected-result
                           '(satisfies
                             (lambda (result)
                               (ert-test-result-type-p
                                result
                                (if (member (slime-lisp-implementation-name)
                                            ',fails-for)
                                    :failed
                                  :passed))))))
                   (slime-sync-to-top-level 0.3)
                   (slime-check-top-level)
                   (with-temp-buffer
                     (setq slime-buffer-package "COMMON-LISP-USER")
                     (lisp-mode)
                     (insert ,buffer-sexpr)
                     (search-backward "*HERE*")
                     (delete-region (match-beginning 0) (match-end 0))
                     (should (equal ,wished-arglist
                                    (slime-autodoc-to-string)))
                     (unless ,skip-trailing-test-p
                       (insert ")") (backward-char)
                       (should (equal ,wished-arglist
                                      (slime-autodoc-to-string)))))
                   (slime-sync-to-top-level 0.3)))))

(define-autodoc-tests
  ;; Test basics
  ("(swank::emacs-connected*HERE*"    "(emacs-connected)")
  ("(swank::emacs-connected *HERE*"   "(emacs-connected)")
  ("(swank::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("(swank::create-socket *HERE*"
   "(create-socket ===> host <=== port &key backlog)")
  ("(swank::create-socket foo *HERE*"
   "(create-socket host ===> port <=== &key backlog)")

  ;; Test that autodoc differentiates between exported and
  ;; unexported symbols.
  ("(swank:create-socket*HERE*" :not-available)

  ;; Test if cursor is on non-existing required parameter
  ("(swank::create-socket foo bar *HERE*"
   "(create-socket host port &key backlog)")

  ;; Test cursor in front of opening parenthesis
  ("(swank::with-struct *HERE*(foo. x y) *struct* body1)"
   "(with-struct (conc-name &rest names) obj &body body)"
   :skip-trailing-test-p t)

  ;; Test variable content display
  ("(progn swank::default-server-port*HERE*"
   "DEFAULT-SERVER-PORT => 4005")

  ;; Test that "variable content display" is not triggered for
  ;; trivial constants.
  ("(swank::create-socket t*HERE*"
   "(create-socket ===> host <=== port &key backlog)")
  ("(swank::create-socket :foo*HERE*"
   "(create-socket ===> host <=== port &key backlog)")

  ;; Test with syntactic sugar
  ("#'(lambda () (swank::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("`(lambda () ,(swank::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("(remove-if #'(lambda () (swank::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("`(remove-if #'(lambda () ,@(swank::create-socket*HERE*"
   "(create-socket host port &key backlog)")

  ;; Test &optional
  ("(swank::symbol-status foo *HERE*"
   "(symbol-status symbol &optional\
 ===> (package (symbol-package symbol)) <===)" :fails-for ("allegro" "ccl"))

  ;; Test context-sensitive autodoc (DEFMETHOD)
  ("(defmethod swank::arglist-dispatch (*HERE*"
   "(defmethod arglist-dispatch\
 (===> operator <=== arguments) &body body)")
  ("(defmethod swank::arglist-dispatch :before (*HERE*"
   "(defmethod arglist-dispatch :before\
 (===> operator <=== arguments) &body body)")

  ;; Test context-sensitive autodoc (APPLY)
  ("(apply 'swank::eval-for-emacs*HERE*"
   "(apply 'eval-for-emacs &optional form buffer-package id &rest args)")
  ("(apply #'swank::eval-for-emacs*HERE*"
   "(apply #'eval-for-emacs &optional form buffer-package id &rest args)" :fails-for ("ccl"))
  ("(apply 'swank::eval-for-emacs foo *HERE*"
   "(apply 'eval-for-emacs &optional form\
 ===> buffer-package <=== id &rest args)")
  ("(apply #'swank::eval-for-emacs foo *HERE*"
   "(apply #'eval-for-emacs &optional form\
 ===> buffer-package <=== id &rest args)" :fails-for ("ccl"))

  ;; Test context-sensitive autodoc (ERROR, CERROR)
  ("(error 'simple-condition*HERE*"
   "(error 'simple-condition &rest arguments\
 &key format-arguments format-control)" :fails-for ("ccl"))
  ("(cerror \"Foo\" 'simple-condition*HERE*"
   "(cerror \"Foo\" 'simple-condition\
 &rest arguments &key format-arguments format-control)"
   :fails-for ("allegro" "ccl"))

  ;; Test &KEY and nested arglists
  ("(swank::with-retry-restart (:msg *HERE*"
   "(with-retry-restart (&key ===> (msg \"Retry.\") <===) &body body)"
   :fails-for ("allegro"))
  ("(swank::with-retry-restart (:msg *HERE*(foo"
   "(with-retry-restart (&key ===> (msg \"Retry.\") <===) &body body)"
   :skip-trailing-test-p t
   :fails-for ("allegro"))
  ("(swank::start-server \"/tmp/foo\" :dont-close *HERE*"
   "(start-server port-file &key (style swank:*communication-style*)\
 ===> (dont-close swank:*dont-close*) <===)"
   :fails-for ("allegro" "ccl"))

  ;; Test declarations and type specifiers
  ("(declare (string *HERE*"
   "(declare (string &rest ===> variables <===))"
   :fails-for ("allegro") :fails-for ("ccl"))
  ("(declare ((string *HERE*"
   "(declare ((string &optional ===> size <===) &rest variables))")
  ("(declare (type (string *HERE*"
   "(declare (type (string &optional ===> size <===) &rest variables))")

  ;; Test local functions
  ("(flet ((foo (x y) (+ x y))) (foo *HERE*" "(foo ===> x <=== y)")
  ("(macrolet ((foo (x y) `(+ ,x ,y))) (foo *HERE*" "(foo ===> x <=== y)")
  ("(labels ((foo (x y) (+ x y))) (foo *HERE*" "(foo ===> x <=== y)")
  ("(labels ((foo (x y) (+ x y))
                 (bar (y) (foo *HERE*"
   "(foo ===> x <=== y)" :fails-for ("cmucl" "sbcl" "allegro" "ccl")))

(def-slime-test autodoc-space
    (input-keys expected-message)
    "Emulate the inserting something followed by the space key
event and verify that the right thing appears in the echo
area (after a short delay)."
    '(("( s w a n k : : o p e r a t o r - a r g l i s t SPC"
       "(operator-arglist name package)"))
  (when noninteractive
    (slime-skip-test "Can't use unread-command-events in batch mode"))
  (let* ((keys (eval `(kbd ,input-keys)))
	 (tag (cons nil nil))
	 (timerfun (lambda (tag) (throw tag nil)))
	 (timer (run-with-timer 0.1 nil timerfun tag)))
    (with-temp-buffer
      (lisp-mode)
      (unwind-protect
	  (catch tag
	    (message nil)
	    (select-window (display-buffer (current-buffer) t))
	    (setq unread-command-events (listify-key-sequence keys))
	    (accept-process-output)
	    (recursive-edit))
	(setq unread-command-events nil)
	(cancel-timer timer))
      (slime-test-expect "Message after SPC"
			 expected-message (current-message))
      (accept-process-output nil (* eldoc-idle-delay 2))
      (slime-test-expect "Message after edloc delay"
			 expected-message (current-message)))))

(provide 'slime-autodoc-tests)
