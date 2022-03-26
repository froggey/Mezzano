;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
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

(def-suite automaton-tests :description "The test suite for
CL-AUTOMATON related tests." :in drei-tests)

(in-suite automaton-tests)

(defmacro automaton-test (name &body body)
  (let ((name-string (symbol-name name)))
    (flet ((%dts (prefixes)
	     (loop for v1 in prefixes nconc
		  (loop for v2 in '(t nil) collect
		       `(test ,(intern
                                (concatenate
                                 'string
                                 (symbol-name v1)
                                 (if v2 "" "-LIGHT")
                                 "."
                                 name-string))
			  (let ((automaton::*minimization*
				 ',(intern (symbol-name v1) :automaton))
				(automaton::*minimize-always* ,v2))
			    ,@body))))))
      `(progn ,@(%dts '(hopcroft huffman brzozowski))))))

(automaton-test regexp-automaton.1
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "#"))))
              (and (not (run a ""))
                   (not (run a "#"))
                   a))
            (automaton::minimize (automaton::empty-automaton)))))

(automaton-test regexp-automaton.2
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "foo"))))
              (and (run a "foo") a))
            (automaton::minimize (automaton::string-automaton "foo"))))
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "()"))))
              (and (run a "") (not (run a " ")) a))
            (automaton::minimize (automaton::string-automaton ""))))
  (is-false (automaton-equal
             (regexp-automaton (string-regexp "()"))
             (automaton::minimize (automaton::empty-automaton)))))

(automaton-test regexp-automaton.3
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "c"))))
              (and (run a "c") (not (run a "C")) a))
            (automaton::minimize (automaton::char-automaton (char-code #\c))))))

(automaton-test regexp-automaton.4
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "."))))
              (and (run a "x") (not (run a "xx")) a))
            (automaton::minimize (automaton::any-char-automaton)))))

(automaton-test regexp-automaton.5
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "@"))))
              (and (run a "foo") a))
            (automaton::minimize (automaton::any-string-automaton)))))

(automaton-test regexp-automaton.6
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "<11-15>"))))
              (and (run a "13") (not (run a "10")) (not (run a "16"))
                   (not (run a "20")) (not (run a "011")) a))
            (automaton::minimize (automaton::interval-automaton 11 15 2))))
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "<115-11>"))))
              (and (run a "13") (run a "113") (not (run a "116")) (run a "00114")
                   (run a "20") (not (run a "200")) (run a "011") a))
            (automaton::minimize (automaton::interval-automaton 11 115 0)))))

(automaton-test regexp-automaton.7
  (is-true (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "sub" ht) (automaton::empty-automaton))
             (automaton-equal
              (let ((a (regexp-automaton (string-regexp "<sub>") ht)))
                (and (not (run a "foo")) a))
              (automaton::minimize (automaton::empty-automaton))))))

(automaton-test regexp-automaton.8
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[a-z]"))))
              (and (run a "a") (run a "z") (not (run a "A")) a))
            (automaton::minimize
             (automaton::char-range-automaton (char-code #\a) (char-code #\z)))))
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[a]"))))
              (and (run a "a") (not (run a "A")) a))
            (automaton::minimize (automaton::char-automaton (char-code #\a))))))

(automaton-test regexp-automaton.9
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[a][b][c]"))))
              (and (run a "abc") (not (run a "ab")) (not (run a "a"))
                   (not (run a "A")) a))
            (automaton::minimize (automaton::string-automaton "abc")))))

(automaton-test regexp-automaton.10
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[ab]"))))
              (and (run a "a") (run a "b") (not (run a "ab"))
                   (not (run a "aa")) (not (run a "A")) a))
            (automaton::minimize
             (automaton::aunion
              (automaton::char-automaton (char-code #\a))
              (automaton::char-automaton (char-code #\b)))))))

(automaton-test regexp-automaton.11
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[^a-c0-3]"))))
              (and (run a "d") (not (run a "a")) (not (run a "0"))
                   (run a "4") (not (run a "dd")) (not (run a "00")) a))
            (automaton::minimize
             (automaton::aintersection
              (automaton::any-char-automaton)
              (automaton::acomplement
               (automaton::aunion
                (automaton::char-range-automaton (char-code #\a) (char-code #\c))
                (automaton::char-range-automaton (char-code #\0) (char-code #\3))))))))
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[a^b-c]"))))
              (and (run a "a") (run a "^") (run a "b") (run a "c")
                   (not (run a "d")) (not (run a "ad")) a))
            (automaton::minimize
             (automaton::aunion
              (automaton::aunion
               (automaton::char-automaton (char-code #\a))
               (automaton::char-automaton (char-code #\^)))
              (automaton::char-range-automaton (char-code #\b) (char-code #\c)))))))

(automaton-test regexp-automaton.12
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "~[a-c]"))))
              (and (run a "d") (not (run a "a")) (not (run a "b")) (not (run a "c"))
                   (run a "dd") (run a "cc") (run a "A") a))
            (automaton::minimize
             (automaton::acomplement
              (automaton::char-range-automaton (char-code #\a) (char-code #\c)))))))

(automaton-test regexp-automaton.13
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "f?"))))
              (and (run a "") (run a "f") (not (run a "ff")) (not (run a "F")) a))
            (automaton::minimize
             (automaton::optional (automaton::char-automaton (char-code #\f)))))))

(automaton-test regexp-automaton.14
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "(\"foo\")?"))))
              (and (run a "") (run a "foo") (not (run a "foofoo"))
                   (not (run a "FOO")) a))
            (automaton::minimize
             (automaton::optional (automaton::string-automaton "foo"))))))

(automaton-test regexp-automaton.15
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[a-c]*"))))
              (and (run a "a") (run a "bb") (run a "ccc") (run a "abcabc")
                   (not (run a "d")) (run a "") a))
            (automaton::minimize
             (automaton::repeat
              (automaton::char-range-automaton (char-code #\a) (char-code #\c)))))))

(automaton-test regexp-automaton.16
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "(\"foo\")+"))))
              (and (not (run a "")) (run a "foo") (run a "foofoo")
                   (not (run a "FOO")) a))
            (automaton::minimize
             (automaton::repeat-min (automaton::string-automaton "foo") 1)))))

(automaton-test regexp-automaton.17
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[a-c]{3}"))))
              (and (run a "abc") (run a "aaa") (not (run a "a")) (not (run a "aaaa"))
                   (not (run a "AAA")) a))
            (automaton::minimize
             (automaton::repeat-minmax
              (automaton::char-range-automaton (char-code #\a) (char-code #\c)) 3 3)))))

(automaton-test regexp-automaton.18
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "(~c){1,2}"))))
              (and (run a "aa") (run a "AA") (run a "foofoo") (run a "foo")
                   (not (run a "c")) (run a "cc") (run a "ccc") a))
            (automaton::minimize
             (automaton::repeat-minmax
              (automaton::acomplement
               (automaton::char-automaton (char-code #\c))) 1 2))))
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "~(c{1,2})"))))
              (and (run a "aa") (run a "AA") (run a "foofoo") (run a "foo")
                   (not (run a "c")) (not (run a "cc")) (run a "ccc") a))
            (automaton::minimize
             (automaton::acomplement
              (automaton::repeat-minmax
               (automaton::char-automaton (char-code #\c)) 1 2))))))

(automaton-test regexp-automaton.19
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "[a-z]~[0-9]"))))
              (and (run a "aa") (run a "a") (not (run a "a0"))
                   (not (run a "")) (run a "abc") a))
            (automaton::minimize
             (automaton::aconcatenate
              (automaton::char-range-automaton (char-code #\a) (char-code #\z))
              (automaton::acomplement
               (automaton::char-range-automaton (char-code #\0) (char-code #\9))))))))

(automaton-test regexp-automaton.20
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "(ab+)&(a+b)|c"))))
              (and (run a "ab") (run a "c") (not (run a "abb")) (not (run a "aab")) a))
            (automaton::minimize
             (automaton::aunion
              (automaton::aintersection
               (automaton::aconcatenate
                (automaton::char-automaton (char-code #\a))
                (automaton::repeat-min (automaton::char-automaton (char-code #\b)) 1))
               (automaton::aconcatenate
                (automaton::repeat-min (automaton::char-automaton (char-code #\a)) 1)
                (automaton::char-automaton (char-code #\b))))
              (automaton::char-automaton (char-code #\c)))))))

(automaton-test regexp-automaton.21
  (is-true (automaton-equal
            (let ((a (regexp-automaton (string-regexp "a\"b\"+c"))))
              (and (run a "abc") (run a "abbc") (not (run a "ab")) (not (run a "ac")) a))
            (automaton::minimize
             (automaton::aconcatenate
              (automaton::char-automaton (char-code #\a))
              (automaton::aconcatenate
               (automaton::repeat-min (automaton::string-automaton "b") 1)
               (automaton::char-automaton (char-code #\c))))))))

(automaton-test run.1
  (is-true (let ((a (regexp-automaton (string-regexp "[Cc]limacs"))))
             (and (run a "climacs")
                  (run a "Climacs")
                  (not (run a "Klimaks"))
                  (not (run a "climac"))
                  (not (run a "climax"))))))

(automaton-test run-to-first-match.1
  (is-true (let ((a (regexp-automaton (string-regexp "[a-z]+"))))
             (and (= (run-to-first-match a "abc") 1)
                  (eq (run-to-first-match a "ABC") nil)
                  (eq (run-to-first-match a "000abc") nil)
                  (= (run-to-first-match a "a") 1)
                  (eq (run-to-first-match a "") nil)))))

(automaton-test run-to-first-match.2
  (is-true (let ((a (regexp-automaton (string-regexp "(ab)+"))))
             (and (= (run-to-first-match a "abab") 2)
                  (= (run-to-first-match a "ababac") 2)))))

(automaton-test run-to-first-unmatch.1
  (is-true (let ((a (regexp-automaton (string-regexp "[a-z]+"))))
             (and (= (run-to-first-unmatch a "abc") 3)
                  (eq (run-to-first-unmatch a "ABC") nil)
                  (eq (run-to-first-unmatch a "000abc") nil)
                  (= (run-to-first-unmatch a "a") 1)
                  (eq (run-to-first-unmatch a "") nil)
                  (= (run-to-first-unmatch a "abc9d") 3)))))

(automaton-test run-to-first-unmatch.2
  (is-true (let ((a (regexp-automaton (string-regexp "(ab)+"))))
             (and (= (run-to-first-unmatch a "abab") 2)
                  (= (run-to-first-unmatch a "ababac") 2)))))