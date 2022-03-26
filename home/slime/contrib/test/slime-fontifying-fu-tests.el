(require 'slime-fontifying-fu)
(require 'slime-tests)

(def-slime-test font-lock-magic (buffer-content)
    "Some testing for the font-lock-magic. *YES* should be
    highlighted as a suppressed form, *NO* should not."

    '(("(defun *NO* (x y) (+ x y))")
      ("(defun *NO*")
      ("*NO*) #-(and) (*YES*) (*NO* *NO*")
      ("\(
\(defun *NO*")
      ("\)
\(defun *NO*
    \(
\)")
      ("#+#.foo
\(defun *NO* (x y) (+ x y))")
      ("#+#.foo
\(defun *NO* (x ")
      ("#+(
\(defun *NO* (x ")
      ("#+(test)
\(defun *NO* (x ")

      ("(eval-when (...)
\(defun *NO* (x ")

      ("(eval-when (...)
#+(and)
\(defun *NO* (x ")

      ("#-(and) (defun *YES* (x y) (+ x y))")
      ("
#-(and) (defun *YES* (x y) (+ x y))
#+(and) (defun *NO* (x y) (+ x y))")

      ("#+(and) (defun *NO* (x y) #-(and) (+ *YES* y))")
      ("#| #+(or) |# *NO*")
      ("#| #+(or) x |# *NO*")
      ("*NO* \"#| *NO* #+(or) x |# *NO*\" *NO*")
      ("#+#.foo (defun foo (bar))
#-(and) *YES* *NO* bar
")
      ("#+(foo) (defun foo (bar))
#-(and) *YES* *NO* bar")
      ("#| #+(or) |# *NO* foo
#-(and) *YES* *NO*")
      ("#- (and)
\(*YES*)
\(*NO*)
#-(and)
\(*YES*)
\(*NO*)")
      ("#+nil (foo)

#-(and)
#+nil (
       asdf *YES* a
            fsdfad)

\( asdf *YES*

       )
\(*NO*)

")
      ("*NO*

#-(and) \(progn
   #-(and)
   (defun *YES* ...)

   #+(and)
   (defun *YES* ...)

   (defun *YES* ...)

   *YES*

   *YES*

   *YES*

   *YES*
\)

*NO*")
      ("#-(not) *YES* *NO*

*NO*

#+(not) *NO* *NO*

*NO*

#+(not a b c) *NO* *NO*

*NO*"))
  (slime-check-top-level)
  (with-temp-buffer
    (insert buffer-content)
    (slime-initialize-lisp-buffer-for-test-suite
     :autodoc t :font-lock-magic t)
    ;; Can't use `font-lock-fontify-buffer' because for the case when
    ;; `jit-lock-mode' is enabled. Jit-lock-mode fontifies only on
    ;; actual display.
    (font-lock-default-fontify-buffer)
    (when (search-backward "*NO*" nil t)
      (slime-test-expect "Not suppressed by reader conditional?"
                         'slime-reader-conditional-face
                         (get-text-property (point) 'face)
                         #'(lambda (x y) (not (eq x y)))))
    (goto-char (point-max))
    (when (search-backward "*YES*" nil t)
      (slime-test-expect "Suppressed by reader conditional?"
                         'slime-reader-conditional-face
                         (get-text-property (point) 'face)))))

(provide 'slime-fontifying-fu-tests)
