(require 'slime-parse)
(require 'slime-tests)

(defun slime-check-buffer-form (result-form)
  (slime-test-expect
   (format "Buffer form correct in `%s' (at %d)" (buffer-string) (point))
   result-form
   (slime-parse-form-upto-point 10)))

(def-slime-test form-up-to-point.1
    (buffer-sexpr result-form &optional skip-trailing-test-p)
    ""
    `(("(char= #\\(*HERE*"
       ("char=" "#\\(" ,slime-cursor-marker))
      ("(char= #\\( *HERE*"
       ("char=" "#\\(" "" ,slime-cursor-marker))
      ("(char= #\\) *HERE*"
       ("char=" "#\\)" "" ,slime-cursor-marker))
      ("(char= #\\*HERE*"
       ("char=" "#\\" ,slime-cursor-marker) t)
      ("(defun*HERE*"
       ("defun" ,slime-cursor-marker))
      ("(defun foo*HERE*"
       ("defun" "foo" ,slime-cursor-marker))
      ("(defun foo (x y)*HERE*"
       ("defun" "foo"
	("x" "y") ,slime-cursor-marker))
      ("(defun foo (x y*HERE*"
       ("defun" "foo"
	("x" "y" ,slime-cursor-marker)))
      ("(apply 'foo*HERE*"
       ("apply" "'foo" ,slime-cursor-marker))
      ("(apply #'foo*HERE*"
       ("apply" "#'foo" ,slime-cursor-marker))
      ("(declare ((vector bit *HERE*"
       ("declare" (("vector" "bit" "" ,slime-cursor-marker))))
      ("(with-open-file (*HERE*"
       ("with-open-file" ("" ,slime-cursor-marker)))
      ("(((*HERE*"
       ((("" ,slime-cursor-marker))))
      ("(defun #| foo #| *HERE*"
       ("defun" "" ,slime-cursor-marker))
      ("(defun #-(and) (bar) f*HERE*"
       ("defun" "f" ,slime-cursor-marker))
      ("(remove-if #'(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") ,slime-cursor-marker)))
      ("`(remove-if ,(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") ,slime-cursor-marker)))
      ("`(remove-if ,@(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") ,slime-cursor-marker))))
  (slime-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slime-check-buffer-form result-form)
    (unless skip-trailing-test-p
      (insert ")") (backward-char)
      (slime-check-buffer-form result-form))
    ))

(provide 'slime-parse-tests)
