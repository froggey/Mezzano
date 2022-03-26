(require 'slime-repl)
(require 'slime-tests)
(require 'cl-lib)

(defmacro slime-repl-test-markers (expected-string-spec &rest marker-specs)
  "For (MARKER SIG FORM) in MARKER-SPECS, produce suitable `should' assertions.
The assertions compare values in symbols `expected-MARKER' and
`observed-MARKER'. The former is obtained by searching EXPECTED-STRING-SPEC
for the string sig SIG, the latter by evaling FORM in the test buffer."
  (declare (indent 1))
  (cl-loop
   for (marker signature observer-form) in marker-specs
   for expected-sym = (make-symbol (format "expected-%s" marker))
   for observed-sym = (make-symbol (format "observed-%s" marker))

   collect `(,expected-sym
             (progn (goto-char (point-min))
                    (when (search-forward ,signature nil t)
                      (replace-match "")
                      (point-marker))))
   into expected-bindings
   collect `(,observed-sym ,observer-form)
   into observed-bindings
   collect `(when (and ,observed-sym (not ,expected-sym))
              (ert-fail
               (format "Didn't expect to observe %s, but did and its %s"
                       ',marker ,observed-sym)))
   into assertions
   collect `(when (and (not ,observed-sym) ,expected-sym)
              (ert-fail
               (format "Expected %s to be %s, bit didn't observe anything"
                       ',marker ,expected-sym)))
   into assertions
   collect `(when (and ,observed-sym ,expected-sym)
              (should (= ,observed-sym ,expected-sym)))
   into assertions
   finally
   (return
    `(progn
       (let (,@observed-bindings
             (observed-string (buffer-substring-no-properties (point-min)
                                                              (point-max))))
         (with-current-buffer (get-buffer-create "*slime-repl test buffer*")
           (erase-buffer)
           (insert ,expected-string-spec)
           (let (,@expected-bindings)
             (should
              (equal observed-string (buffer-string)))
             ,@assertions)))))))

(defun slime-check-buffer-contents (_msg expected-string-spec)
  (slime-repl-test-markers expected-string-spec
    (point             "*" (point))
    (output-start      "{" (next-single-property-change
                            (point-min) 'slime-repl-output))
    (output-end        "}" (previous-single-property-change
                            (point-max) 'slime-repl-output))
    (input-start       "[" slime-repl-input-start-mark)
    (point-max         "]" (point-max))
    (next-input-start  "^" nil)))

(def-slime-test package-updating
    (package-name nicknames)
    "Test if slime-lisp-package is updated."
    '(("COMMON-LISP" ("CL"))
      ("KEYWORD" ("" "KEYWORD" "||"))
      ("COMMON-LISP-USER" ("CL-USER")))
  (with-current-buffer (slime-output-buffer)
    (let ((p (slime-eval
              `(swank-repl:listener-eval
                ,(format
                  "(cl:setq cl:*print-case* :upcase)
                   (cl:setq cl:*package* (cl:find-package %S))
                   (cl:package-name cl:*package*)" package-name))
              (slime-lisp-package))))
      (slime-check ("slime-lisp-package is %S." package-name)
        (equal (slime-lisp-package) package-name))
      (slime-check ("slime-lisp-package-prompt-string is in %S." nicknames)
        (member (slime-lisp-package-prompt-string) nicknames)))))

(defmacro with-canonicalized-slime-repl-buffer (&rest body)
  "Evaluate BODY within a fresh REPL buffer. The REPL prompt is
canonicalized to \"SWANK\"---we do actually switch to that
package, though."
  (declare (debug (&rest form)) (indent 0))
  `(let ((%old-prompt% (slime-lisp-package-prompt-string)))
     (unwind-protect
          (progn (with-current-buffer (slime-output-buffer)
                   (setf (slime-lisp-package-prompt-string) "SWANK"))
                 (kill-buffer (slime-output-buffer))
                 (with-current-buffer (slime-output-buffer)
                   ,@body))
       (setf (slime-lisp-package-prompt-string) %old-prompt%))))

(def-slime-test repl-test
    (input result-contents)
    "Test simple commands in the minibuffer."
    '(("(+ 1 2)" "SWANK> (+ 1 2)
3
SWANK> *[]")
      ("(princ 10)" "SWANK> (princ 10)
{10
}10
SWANK> *[]")
      ("(princ 10)(princ 20)" "SWANK> (princ 10)(princ 20)
{1020
}20
SWANK> *[]")
      ("(dotimes (i 10 77) (princ i) (terpri))"
       "SWANK> (dotimes (i 10 77) (princ i) (terpri))
{0
1
2
3
4
5
6
7
8
9
}77
SWANK> *[]")
      ("(abort)" "SWANK> (abort)
; Evaluation aborted on NIL.
SWANK> *[]")
      ("(progn (princ 10) (force-output) (abort))"
       "SWANK> (progn (princ 10) (force-output) (abort))
{10}; Evaluation aborted on NIL.
SWANK> *[]")
      ("(progn (princ 10) (abort))"
       ;; output can be flushed after aborting
       "SWANK> (progn (princ 10) (abort))
{10}; Evaluation aborted on NIL.
SWANK> *[]")
      ("(if (fresh-line) 1 0)"
       "SWANK> (if (fresh-line) 1 0)
{
}1
SWANK> *[]")
      ("(values 1 2 3)" "SWANK> (values 1 2 3)
1
2
3
SWANK> *[]"))
  (with-canonicalized-slime-repl-buffer
    (insert input)
    (slime-check-buffer-contents "Buffer contains input"
                                 (concat "SWANK> [" input "*]"))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-check-buffer-contents "Buffer contains result" result-contents)))

(def-slime-test repl-test-2
    (input result-contents)
    "Test some more simple situations dealing with print-width and stuff"
    '(("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]")
      ;; Two times to test the effect of FRESH-LINE.
      ("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]"))
  (slime-skip-test "Repl test is unstable without the slime-presentations contrib.")
  (slime-test-repl-test input result-contents))

(def-slime-test repl-return
    (before after result-contents)
    "Test if slime-repl-return sends the correct protion to Lisp even
if point is not at the end of the line."
    '(("(+ 1 2)" "" "SWANK> (+ 1 2)
3
SWANK> ")
("(+ 1 " "2)" "SWANK> (+ 1 2)
3
SWANK> ")

("(+ 1\n" "2)" "SWANK> (+ 1
2)
3
SWANK> "))
  (with-canonicalized-slime-repl-buffer
    (insert before)
    (save-excursion (insert after))
    (slime-test-expect "Buffer contains input"
                       (concat "SWANK> " before after)
                       (buffer-string))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result"
                       result-contents (buffer-string))))

(def-slime-test repl-read
    (prompt input result-contents)
    "Test simple commands in the minibuffer."
    '(("(read-line)" "foo" "SWANK> (values (read-line))
foo
\"foo\"
SWANK> ")
      ("(read-char)" "1" "SWANK> (values (read-char))
1
#\\1
SWANK> ")
      ("(read)" "(+ 2 3
4)" "SWANK> (values (read))
\(+ 2 3
4)
\(+ 2 3 4)
SWANK> "))
  (with-canonicalized-slime-repl-buffer
    (insert (format "(values %s)" prompt))
    (call-interactively 'slime-repl-return)
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (insert input)
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result"
                       result-contents (buffer-string))))

(def-slime-test repl-read-lines
    (command inputs final-contents)
    "Test reading multiple lines from the repl."
    '(("(list (read-line) (read-line) (read-line))"
       ("a" "b" "c")
       "SWANK> (list (read-line) (read-line) (read-line))
a
b
c
\(\"a\" \"b\" \"c\")
SWANK> "))
  (with-canonicalized-slime-repl-buffer
    (insert command)
    (call-interactively 'slime-repl-return)
    (dolist (input inputs)
      (slime-wait-condition "reading" #'slime-reading-p 5)
      (insert input)
      (call-interactively 'slime-repl-return))
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result"
                       final-contents
                       (buffer-string)
                       #'equal)))

(def-slime-test repl-type-ahead
    (command input final-contents)
    "Ensure that user input is preserved correctly.
In particular, input inserted while waiting for a result."
    '(("(sleep 0.1)" "foo*" "SWANK> (sleep 0.1)
NIL
SWANK> [foo*]")
      ("(sleep 0.1)" "*foo" "SWANK> (sleep 0.1)
NIL
SWANK> [*foo]")
      ("(progn (sleep 0.1) (abort))" "*foo" "SWANK> (progn (sleep 0.1) (abort))
; Evaluation aborted on NIL.
SWANK> [*foo]"))
  (with-canonicalized-slime-repl-buffer
    (insert command)
    (call-interactively 'slime-repl-return)
    (save-excursion (insert (cl-delete ?* input)))
    (forward-char (cl-position ?* input))
    (slime-sync-to-top-level 5)
    (slime-check-buffer-contents "Buffer contains result" final-contents)))


(def-slime-test interrupt-in-blocking-read
    ()
    "Let's see what happens if we interrupt a blocking read operation."
    '(())
  (slime-skip-test "TODO: skip for now, but analyse this failure!")
  (slime-check-top-level)
  (with-canonicalized-slime-repl-buffer
    (insert "(read-char)")
    (call-interactively 'slime-repl-return)
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (slime-interrupt)
    (slime-wait-condition "Debugger visible"
                          (lambda ()
                            (and (slime-sldb-level= 1)
                                 (get-buffer-window
                                  (sldb-get-default-buffer))))
                          5)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (with-current-buffer (slime-output-buffer)
      (insert "X")
      (call-interactively 'slime-repl-return)
      (slime-sync-to-top-level 5)
      (slime-test-expect "Buffer contains result"
                         "SWANK> (read-char)
X
#\\X
SWANK> " (buffer-string)))))

(def-slime-test move-around-and-be-nasty
    ()
    "Test moving around in repl, and watching attempts to destroy prompt fail"
    '(())
  (slime-skip-test "TODO: Test causes instability for other tests.")
  (slime-check-top-level)
  (with-canonicalized-slime-repl-buffer
    (let ((start (point)))
      (insert "foo")
      (beginning-of-line)
      (should (equal (buffer-substring-no-properties
                      (point-min)
                      (point-max)) "SWANK> foo"))
      (should (equal (point) start))
      (unwind-protect
          (progn
            (let ((inhibit-field-text-motion t))
              (goto-char (line-beginning-position)))
            (should-error (delete-char 1)))
        (goto-char (line-end-position))))))

(def-slime-test mixed-output-and-results
    (prompt eval-input result-contents)
    "Test that output goes to the correct places."
    '(("(princ 123)" (cl:loop repeat 2 do (cl:princ 456)) "SWANK> (princ 123)
123
123
456456
SWANK> "))
  (with-canonicalized-slime-repl-buffer
    (insert prompt)
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-eval eval-input)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result"
                       result-contents (buffer-string))))

(provide 'slime-repl-tests)
