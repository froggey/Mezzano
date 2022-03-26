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

(def-suite regexp-tests :description "The test suite for
CL-AUTOMATON regexp related tests." :in automaton-tests)

(in-suite regexp-tests)

(automaton-test string-regexp.1
  (is-true (regexp-equal
            (string-regexp "#")
            (automaton::make-regexp :empty))))

(automaton-test string-regexp.2
  (is-true (regexp-equal
            (string-regexp "foo")
            (make-instance 'automaton::regexp :kind :string :s "foo")))
  (is-true (regexp-equal
            (string-regexp "\"foo\"")
            (make-instance 'automaton::regexp :kind :string :s "foo")))
  (is-true (regexp-equal
            (string-regexp "()")
            (make-instance 'automaton::regexp :kind :string :s ""))))

(automaton-test string-regexp.3
  (is-true (regexp-equal
            (string-regexp "c")
            (make-instance 'automaton::regexp :kind :char :c #\c)))
  (is-true (regexp-equal
            (string-regexp "\c")
            (make-instance 'automaton::regexp :kind :char :c #\c)))
  (is-true (regexp-equal
            (string-regexp "\\c")
            (make-instance 'automaton::regexp :kind :char :c #\c))))

(automaton-test string-regexp.4
  (is-true (regexp-equal
            (string-regexp ".")
            (automaton::make-regexp :anychar))))

(automaton-test string-regexp.5
  (is-true (regexp-equal
            (string-regexp "@")
            (automaton::make-regexp :anystring))))

(automaton-test string-regexp.6
  (is-true (regexp-equal
            (string-regexp "<11-15>")
            (make-instance 'automaton::regexp :kind :interval
                           :minr 11 :maxr 15 :digits 2)))
  (is-true (regexp-equal
            (string-regexp "<11-115>")
            (make-instance 'automaton::regexp :kind :interval
                           :minr 11 :maxr 115 :digits 0)))
  (is-true (regexp-equal
            (string-regexp "<115-11>")
            (make-instance 'automaton::regexp :kind :interval
                           :minr 11 :maxr 115 :digits 0))))

(automaton-test string-regexp.7
  (is-true (regexp-equal
            (string-regexp "<sub>")
            (make-instance 'automaton::regexp :kind :automaton :s "sub"))))

(automaton-test string-regexp.8
  (is-true (regexp-equal
            (string-regexp "[a-z]")
            (make-instance 'automaton::regexp :kind :char-range :from #\a :to #\z)))
  (is-true (regexp-equal
            (string-regexp "[a]")
            (make-instance 'automaton::regexp :kind :char :c #\a))))

(automaton-test string-regexp.9
  (is-true (regexp-equal
            (string-regexp "[a][b][c]")
            (make-instance 'automaton::regexp :kind :string :s "abc"))))

(automaton-test string-regexp.10
  (is-true (regexp-equal
            (string-regexp "[ab]")
            (automaton::make-regexp
             :union (make-instance 'automaton::regexp :kind :char :c #\a)
             (make-instance 'automaton::regexp :kind :char :c #\b)))))

(automaton-test string-regexp.11
  (is-true (regexp-equal
            (string-regexp "[^a-c0-3]")
            (automaton::make-regexp
             :intersection
             (automaton::make-regexp :anychar)
             (automaton::make-regexp
              :complement
              (automaton::make-regexp
               :union
               (make-instance 'automaton::regexp :kind :char-range
                              :from #\a :to #\c)
               (make-instance 'automaton::regexp :kind :char-range
                              :from #\0 :to #\3))))))
  (is-true (regexp-equal
            (string-regexp "[a^b-c]")
            (automaton::make-regexp
             :union
             (automaton::make-regexp
              :union (make-instance 'automaton::regexp :kind :char :c #\a)
              (make-instance 'automaton::regexp :kind :char :c #\^))
             (make-instance 'automaton::regexp :kind :char-range
                            :from #\b :to #\c)))))

(automaton-test string-regexp.12
  (is-true (regexp-equal
            (string-regexp "~[a-c]")
            (automaton::make-regexp
             :complement (make-instance 'automaton::regexp :kind :char-range
                                        :from #\a :to #\c)))))

(automaton-test string-regexp.13
  (is-true (regexp-equal
            (string-regexp "f?")
            (automaton::make-regexp
             :optional (make-instance 'automaton::regexp :kind :char :c #\f)))))

(automaton-test string-regexp.14
  (is-true (regexp-equal
            (string-regexp "(\"foo\")?")
            (automaton::make-regexp
             :optional (make-instance 'automaton::regexp :kind :string :s "foo")))))

(automaton-test string-regexp.15
  (is-true (regexp-equal
            (string-regexp "[a-c]*")
            (automaton::make-regexp
             :repeat (make-instance 'automaton::regexp :kind :char-range
                                    :from #\a :to #\c)))))

(automaton-test string-regexp.16
  (is-true (regexp-equal
            (string-regexp "(\"foo\")+")
            (make-instance
             'automaton::regexp :kind :repeat-min
             :exp1 (make-instance 'automaton::regexp :kind :string :s "foo")
             :minr 1))))

(automaton-test string-regexp.17
  (is-true (regexp-equal
            (string-regexp "[a-c]{3}")
            (make-instance
             'automaton::regexp :kind :repeat-minmax
             :exp1 (make-instance 'automaton::regexp :kind :char-range
                                  :from #\a :to #\c)
             :minr 3 :maxr 3))))

(automaton-test string-regexp.18
  (is-true (regexp-equal
            (string-regexp "(~c){1,2}")
            (make-instance
             'automaton::regexp :kind :repeat-minmax
             :exp1 (automaton::make-regexp
                    :complement (make-instance 'automaton::regexp :kind :char :c #\c))
             :minr 1 :maxr 2))))

(automaton-test string-regexp.19
  (is-true (regexp-equal
            (string-regexp "[a-z]~[0-9]")
            (automaton::make-regexp
             :concatenation
             (make-instance 'automaton::regexp :kind :char-range :from #\a :to #\z)
             (automaton::make-regexp
              :complement (make-instance 'automaton::regexp :kind :char-range
                                         :from #\0 :to #\9))))))

(automaton-test string-regexp.20
  (is-true (regexp-equal
            (string-regexp "(ab+)&(a+b)|c")
            (automaton::make-regexp
             :union
             (automaton::make-regexp
              :intersection
              (automaton::make-regexp
               :concatenation
               (make-instance 'automaton::regexp :kind :char :c #\a)
               (make-instance
                'automaton::regexp :kind :repeat-min
                :exp1 (make-instance 'automaton::regexp :kind :char :c #\b)
                :minr 1))
              (automaton::make-regexp
               :concatenation
               (make-instance
                'automaton::regexp :kind :repeat-min
                :exp1 (make-instance 'automaton::regexp :kind :char :c #\a)
                :minr 1)
               (make-instance 'automaton::regexp :kind :char :c #\b)))
             (make-instance 'automaton::regexp :kind :char :c #\c)))))

(automaton-test string-regexp.21
  (is-true (regexp-equal
            (string-regexp "a\"b\"+c")
            (automaton::make-regexp
             :concatenation
             (make-instance 'automaton::regexp :kind :char :c #\a)
             (automaton::make-regexp
              :concatenation
              (make-instance
               'automaton::regexp :kind :repeat-min
               :exp1 (make-instance 'automaton::regexp :kind :string :s "b")
               :minr 1)
              (make-instance 'automaton::regexp :kind :char :c #\c))))))
