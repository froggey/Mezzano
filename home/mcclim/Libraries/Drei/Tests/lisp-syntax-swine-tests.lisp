;;; -*- Mode: Lisp; Package: DREI-TESTS -*-

;;;  (c) copyright 2006-2007 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(cl:in-package :drei-tests)

(in-suite lisp-syntax-tests)

(test arglist-keyword-p
  "Test that the Lisp syntax module can properly recognize lambda
list keywords."
  (mapcar #'(lambda (symbol)
              (is-true (drei-lisp-syntax::lambda-list-keyword-p symbol)))
          lambda-list-keywords)
  (is-false (drei-lisp-syntax::lambda-list-keyword-p '&keyword)))

;; This is to make FiveAM give useful output when tests fail.
(defmacro testing-find-affected-parameters (&body body)
  `(macrolet ((test-find-affected-parameters (lambda-list arg-indices expected-result)
                `(flet ((affected-parameters (lambda-list arg-indices)
                          (mapcar
                           #'(lambda (parameter)
                               (typecase parameter
                                 (drei-lisp-syntax::named-parameter
                                  (drei-lisp-syntax::name parameter))
                                 (drei-lisp-syntax::keyword-parameter
                                  (drei-lisp-syntax::keyword-name parameter))))
                           (drei-lisp-syntax::find-affected-parameters
                            (drei-lisp-syntax::parse-lambda-list
                             (drei-lisp-syntax::cleanup-arglist lambda-list))
                            arg-indices))))
                   (is (equal ,expected-result (affected-parameters ,lambda-list ,arg-indices))))))
     ,@body))

(test find-affected-parameters-1
  "Test the `find-affected-parameters' function for ordinary
lambda lists."
  (testing-find-affected-parameters
    (test-find-affected-parameters '() '((0)) '())
    (test-find-affected-parameters '(a) '((0)) '(a))
    (test-find-affected-parameters '(a) '((1)) '())
    (test-find-affected-parameters '(a b) '((1)) '(b))
    (test-find-affected-parameters '(&optional) '((0)) '())
    (test-find-affected-parameters '(&optional a) '((0)) '(a))
    (test-find-affected-parameters '(&optional a) '((1)) '())
    (test-find-affected-parameters '(&optional a b) '((1)) '(b))
    (test-find-affected-parameters '(a &optional b) '((0)) '(a))
    (test-find-affected-parameters '(a &optional b) '((1)) '(b))
    (test-find-affected-parameters '(&rest a) '((0)) '(a))
    (test-find-affected-parameters '(&rest a) '((1)) '(a))
    (test-find-affected-parameters '(&key a) '((0)) '())
    (test-find-affected-parameters '(&key a) '((1 :a)) '(:a))
    (test-find-affected-parameters '(&key a b) '((1 :b)) '(:b))
    (test-find-affected-parameters '(&key a b) '((1 :c)) '())
    (test-find-affected-parameters '(a b &rest args &key c d) '((3 :d)) '(args :d))))

(test find-affected-parameters-2
  "Test the `find-affected-parameters' function for macro
lambda lists."
  (testing-find-affected-parameters
    (test-find-affected-parameters '((a)) '((0)) '())
    (test-find-affected-parameters '((a)) '((0) (0)) '(a))
    (test-find-affected-parameters '((a)) '((0) (1)) '())
    (test-find-affected-parameters '((a b)) '((0) (1)) '(b))
    (test-find-affected-parameters '(a (b)) '((1)) '())
    (test-find-affected-parameters '(a (b)) '((1) (0)) '(b))
    (test-find-affected-parameters '(a (b)) '((1) (1)) '())
    (test-find-affected-parameters '(a (b c)) '((1) (1)) '(c))
    (test-find-affected-parameters '((&optional a)) '((0) (0)) '(a))
    (test-find-affected-parameters '((&rest a)) '((0) (0)) '(a))
    (test-find-affected-parameters '(&key a) '((1 :a)) '(:a))
    (test-find-affected-parameters '(&key ((:a (a)))) '((1 :a)) '(:a))
    (test-find-affected-parameters '((a b &rest args &key c d)) '((0) (3 :d)) '(args :d))
    (test-find-affected-parameters '(((a b &rest args &key c d))) '((0) (0) (3 :d)) '(args :d))
    (test-find-affected-parameters '(&optional ((a))) '((0) (0)) '(a))
    (test-find-affected-parameters '(&optional ((&optional a))) '((0) (0)) '(a))
    (test-find-affected-parameters '(&key ((:a (a)))) '((1 :a) (0)) '(:a a))
    (test-find-affected-parameters '(&key ((:a (&key b)))) '((1 :a) (1 :b)) '(:a :b))))

(test find-argument-indices-for-operand
  "Test the `find-argument-indices-for-operand' function."
  (macrolet ((test-argument-indices-for-operand (contents operand-index expected-result)
               `(macrolet ((argument-indices (contents operand-index)
                             `(testing-lisp-syntax (,contents :package :drei-tests)
                                (drei-lisp-syntax::find-argument-indices-for-operand
                                 (current-syntax)
                                 (drei-lisp-syntax::form-around (current-syntax) ,operand-index)
                                 (drei-lisp-syntax::form-around (current-syntax) 0)))))
                  (is (equal ',expected-result
                             (argument-indices ,contents ,operand-index))))))
    (test-argument-indices-for-operand "(f)" 1 nil)
    (test-argument-indices-for-operand "(f 1)" 4 ((0)))
    (test-argument-indices-for-operand "(f 1 2)" 6 ((1 1)))
    (test-argument-indices-for-operand "(f 1 2)" 6 ((1 1)))
    (test-argument-indices-for-operand "(f (1))" 5 ((0) (0)))
    (test-argument-indices-for-operand "(f ((1)))" 6 ((0) (0) (0)))
    (test-argument-indices-for-operand "(f (s foo :keyword 'boolean) foo bar baz)"
                                       19 ((0) (3 :keyword)))
    (test-argument-indices-for-operand "(f (s foo :keyword 'boolean) foo bar baz)"
                                       35 ((2 foo)))))

(test find-operand-info
  "Test the `find-operand-info' function."
  (macrolet ((test-find-operand-info (string offset correct-path)
               `(macrolet ((path-of (string offset)
                             `(testing-lisp-syntax (,string :package :drei-tests)
                                (drei-lisp-syntax::find-operand-info
                                 (current-syntax)
                                 ,offset
                                 (drei-lisp-syntax::form-around (current-syntax) 0)))))
                  (is (equal ',correct-path (path-of ,string ,offset))))))
    (test-find-operand-info "(list )" 5 nil)
    (test-find-operand-info "(list )" 6 ((0)))
    (test-find-operand-info "(list " 6 ((0)))
    (test-find-operand-info "(list       )" 10 ((0)))
    (test-find-operand-info "(list 1)" 7 ((0)))
    (test-find-operand-info "(list 1" 7 ((0)))
    (test-find-operand-info "(list 1 )" 8 ((1 1)))
    (test-find-operand-info "(list 1 2)" 8 ((1 1)))
    (test-find-operand-info "(list 1 2" 8 ((1 1)))
    (test-find-operand-info "(lisp-syntax-m1 (s foo :element-type 'boolean) foo bar baz)" 16 ((0)))
    (test-find-operand-info "(lisp-syntax-m1 (s foo :element-type 'boolean) foo bar baz)" 17 ((0) (0)))
    (test-find-operand-info "(lisp-syntax-m1 (s foo :element-type 'boolean) foo bar baz)" 19 ((0) (1 s)))
    (test-find-operand-info "(lisp-syntax-m1 (s foo :element-type 'boolean) foo bar baz)" 51 ((2 foo)))))

;; This is to make FiveAM give useful output when tests fail.
(defmacro testing-indices-match-arglist (&body body)
  `(macrolet ((test-indices-match-arglist (lambda-list arg-indices expected-result)
                `(flet ((indices-match-raw-arglist (lambda-list arg-indices)
                          (drei-lisp-syntax::indices-match-arglist
                           (drei-lisp-syntax::parse-lambda-list
                            (drei-lisp-syntax::cleanup-arglist lambda-list))
                           arg-indices)))
                   (is (equal ,expected-result
                              (indices-match-raw-arglist ',lambda-list ',arg-indices))))))
     ,@body))

(test indices-match-arglist-1
  "Test the `indices-match-arglist' function for ordinary
lambda lists."
  (testing-indices-match-arglist
    (test-indices-match-arglist (a) ((0)) t)
    (test-indices-match-arglist (a) ((1)) nil)
    (test-indices-match-arglist (a b) ((0)) t)
    (test-indices-match-arglist (a b) ((1)) t)
    (test-indices-match-arglist (a b) ((2)) nil)
    (test-indices-match-arglist (&optional a) ((0)) t)
    (test-indices-match-arglist (&optional a) ((1)) nil)
    (test-indices-match-arglist (&optional a b) ((0)) t)
    (test-indices-match-arglist (&optional a b) ((1)) t)
    (test-indices-match-arglist (&optional a b) ((2)) nil)
    (test-indices-match-arglist (&key a) ((0)) t)
    (test-indices-match-arglist (&key a) ((1)) t)
    (test-indices-match-arglist (&key a b) ((0)) t)
    (test-indices-match-arglist (&key a b) ((1)) t)
    (test-indices-match-arglist (&key a b) ((2)) t)
    (test-indices-match-arglist (&rest args) ((0)) t)
    (test-indices-match-arglist (&rest args) ((1)) t)
    (test-indices-match-arglist (&rest args) ((2)) t)))

(test indices-match-arglist-2
  "Test the `indices-match-arglist' function for macro
lambda lists."
  (testing-indices-match-arglist
    (test-indices-match-arglist (&body args) ((0)) t)
    (test-indices-match-arglist (&body args) ((1)) t)
    (test-indices-match-arglist (&body args) ((2)) t)
    (test-indices-match-arglist ((a)) ((0) (0)) t)
    (test-indices-match-arglist ((a)) ((0) (1)) nil)
    (test-indices-match-arglist ((a b)) ((0) (0)) t)
    (test-indices-match-arglist ((a b)) ((0) (1)) t)
    (test-indices-match-arglist ((a b)) ((0) (2)) nil)
    (test-indices-match-arglist ((&optional a)) ((0) (0)) t)
    (test-indices-match-arglist ((&key a)) ((0)) t)
    (test-indices-match-arglist ((&key a)) ((0) (0)) t)
    (test-indices-match-arglist ((&key a)) ((0) (1 :a)) t)
    (test-indices-match-arglist ((&rest args)) ((0) (0)) t)
    (test-indices-match-arglist ((&rest args)) ((0) (1)) t)
    (test-indices-match-arglist ((&rest args)) ((0) (2)) t)))

(swine-test find-direct-operator
  "Test the `find-direct-operator' function."
  (macrolet ((test-find-direct-operator (string offset desired-operator)
               `(macrolet ((direct-operator-of (string offset)
                             `(testing-lisp-syntax (,string :package :drei-tests)
                                (first (form-to-object
                                        (current-syntax)
                                        (drei-lisp-syntax::find-direct-operator
                                         (current-syntax)
                                         (drei-lisp-syntax::form-around (current-syntax) ,offset)))))))
                  (is (eq ,desired-operator (direct-operator-of ,string ,offset))))))
    (test-find-direct-operator "(list 1)" 7 'list)
    (test-find-direct-operator "(list 1 2 3)" 11 'list)
    (test-find-direct-operator "(drei-tests::lisp-syntax-m1 (s \"foo\" :element-type 'boolean) foo bar baz)"
                               30 'drei-tests::lisp-syntax-m1)
    (test-find-direct-operator "(drei-tests::lisp-syntax-m1 (s \"foo\" :element-type 'boolean) foo bar baz)"
                               31 'drei-tests::lisp-syntax-m1)
    (test-find-direct-operator "(drei-tests::lisp-syntax-m1 (s \"foo\" :element-type 'boolean) foo bar baz)"
                               71 'drei-tests::lisp-syntax-m1)))

(swine-test find-applicable-form
  "Test the `find-applicable-form' function."
  (macrolet ((test-find-applicable-form (string offset desired-operator)
               `(macrolet ((applicable-operator-of (string offset)
                             `(testing-lisp-syntax (,string :package :drei-tests)
                                (first (form-to-object
                                        (current-syntax)
                                        (drei-lisp-syntax::find-applicable-form
                                         (current-syntax)
                                         (drei-lisp-syntax::form-around (current-syntax) ,offset)))))))
                  (is (eq ,desired-operator (applicable-operator-of ,string ,offset)))))
             (test-no-find-applicable-form (string offset)
               `(macrolet ((applicable-form-of (string offset)
                             `(testing-lisp-syntax (,string :package :drei-tests)
                                (drei-lisp-syntax::find-applicable-form
                                 (current-syntax)
                                 (drei-lisp-syntax::form-around (current-syntax) ,offset)))))
                  (is-false (applicable-form-of ,string ,offset)))))
    (test-find-applicable-form "(list 1)" 6 'list)
    (test-find-applicable-form "(list 1" 6 'list)
    (test-no-find-applicable-form "(list (1))" 7)
    (test-no-find-applicable-form "(list (1" 7)
    (test-find-applicable-form "(lisp-syntax-m2 :a ())" 19 'lisp-syntax-m2)))

(test relevant-keywords
  "Test the `relevant-keywords' function."
  (macrolet ((test-relevant-keywords (lambda-list arg-indices desired-result)
               `(macrolet ((relevant-keywords-at (lambda-list arg-indices)
                             `(drei-lisp-syntax::relevant-keywords (drei-lisp-syntax::parse-lambda-list ',lambda-list)
                                                                   ',arg-indices)))
                  (is (equal ',desired-result
                             (relevant-keywords-at ,lambda-list ,arg-indices))))))
    (test-relevant-keywords () ((0)) nil)
    (test-relevant-keywords (a) ((0)) nil)
    (test-relevant-keywords (a) ((1)) nil)
    (test-relevant-keywords (&key a b c) ((0)) (:a :b :c))
    (test-relevant-keywords (a &key b c) ((1)) (:b :c))
    (test-relevant-keywords (a &key b c) ((0)) nil)
    (test-relevant-keywords ((&key a b) c) ((0) (0)) (:a :b))
    (test-relevant-keywords ((&key a b) c) ((1)) nil)
    (test-relevant-keywords (&key ((:a (&key b))) c) ((0 :a) (0)) (:b))
    (test-relevant-keywords (&key ((:a (&key b))) c) ((1)) (:a :c))))

(swine-test possible-completions
  "Test the `possible-completions' function."
  (testing-lisp-syntax ("")
    (flet ((find-possible-completions (string &optional operator operands indices)
             (drei-lisp-syntax::possible-completions (current-syntax) operator
                                                     string #.(find-package :common-lisp) operands indices)))
      (is (equal '("lisp-implementation-type" "lisp-implementation-version" "list" "list*"
                   "list-all-packages" "list-length" "listen" "listp")
                 (find-possible-completions "lis")))
      (is (equal '("cl:lisp-implementation-type" "cl:lisp-implementation-version" "cl:list" "cl:list*"
                   "cl:list-all-packages" "cl:list-length" "cl:listen" "cl:listp")
                 (find-possible-completions "cl:lis")))
      (is (equal '("multiple-value-bind")
                 (find-possible-completions "m-v-b")))
      (is-false (find-possible-completions "mvb"))
      (is (equal (find-possible-completions "m-v-c")
                 '("multiple-value-call")))
      (is (equal '("multiple-value-bind")
                 (find-possible-completions "multiple-value-bind")))
      (let ((all-external-symbols-in-cl-and-all-package-names
             (union (loop for sym being the external-symbols in :common-lisp
                       collecting (string sym))
                    (mapcar #'(lambda (string)
                                (format nil "~A:" string))
                            (nconc (mapcar #'package-name (list-all-packages))
                                   (reduce #'append (mapcar #'package-nicknames (list-all-packages))))))))
        (is-false (set-difference (find-possible-completions "")
                                  all-external-symbols-in-cl-and-all-package-names
                                  :test #'string-equal))))))

(swine-test with-code-insight
  "Test the `with-code-insight' macro."
  (testing-lisp-syntax ("(list ")
    (drei-lisp-syntax::with-code-insight 6 (current-syntax) (:operator operator
                                                             :form form
                                                             :this-operand-indices indices
                                                             :operands operands)
      (is (eq 'list operator))
      (is-true (and (drei-lisp-syntax::form-list-p form)
                    (drei-lisp-syntax::form-incomplete-p form)))
      (is (equal '((0)) indices))
      (is-true (null operands))))
  (testing-lisp-syntax ("(list 1 )")
    (drei-lisp-syntax::with-code-insight 8 (current-syntax) (:operator operator
                                                             :form form
                                                             :this-operand-indices indices
                                                             :operands operands)
      (is (eq 'list operator))
      (is-true (and (drei-lisp-syntax::form-list-p form)
                    (drei-lisp-syntax::form-complete-p form)))
      (is (equal '((1 1)) indices))
      (is (equal '(1) operands))))
  (testing-lisp-syntax ("(with-output-to-string (stream string) (list stream))")
    (drei-lisp-syntax::with-code-insight 23 (current-syntax) (:operator operator
                                                              :form form
                                                              :this-operand-indices indices
                                                              :operands operands)
      (is (eq 'with-output-to-string operator))
      (is-true (and (drei-lisp-syntax::form-list-p form)
                    (drei-lisp-syntax::form-complete-p form)))
      (is (equal '((0)) indices))
      (is (equal '((stream string) (list stream)) operands)))))

(swine-test make-instance-form-traits
  "Test the form traits for the `make-instance' function"
  (testing-lisp-syntax ("")
    (flet ((find-possible-completions (string &optional operator operands indices)
             (drei-lisp-syntax::possible-completions (current-syntax) operator
                                                     string #.(find-package :common-lisp) operands indices)))
      (is (equal '("clim:clim-stream-pane")
                 (find-possible-completions "clim:c-s-" 'make-instance '('#:c-s-) '((0)))))
      (is (equal '("lisp-implementation-type" "lisp-implementation-version" "list" "list*"
                   "list-all-packages" "list-length" "listen" "listp")
                 (find-possible-completions "lis" 'make-instance '(#:lis) '((0)))))
      (is-false (find-possible-completions "cl:nonono" 'make-instance '('#:nonono) '((0)))))
    (let ((lambda-list (drei-lisp-syntax::arglist-for-form (current-syntax) 'make-instance '('lisp-syntax-c1))))
      (flet ((takes-keyword-arg (keyword)
               (member keyword (drei-lisp-syntax::keyword-parameters lambda-list)
                :key #'drei-lisp-syntax::keyword-name)))
        (is-true (takes-keyword-arg :foo))
        (is-true (takes-keyword-arg 'bar))))))

(swine-test find-class-form-traits
  "Test the form traits for the `find-class' function"
  (testing-lisp-syntax ("")
    (flet ((find-possible-completions (string &optional operator operands indices)
             (drei-lisp-syntax::possible-completions (current-syntax) operator
                                                     string #.(find-package :common-lisp) operands indices)))
      (is (equal '("clim:clim-stream-pane")
                 (find-possible-completions "clim:c-s-" 'find-class '('#:c-s-) '((0)))))
      (is (equal '("lisp-implementation-type" "lisp-implementation-version" "list" "list*"
                   "list-all-packages" "list-length" "listen" "listp")
                 (find-possible-completions "lis" 'find-class '(#:lis) '((0)))))
      (is-false (find-possible-completions "cl:nonono" 'find-class '('#:nonono) '((0)))))))
