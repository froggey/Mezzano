;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/regex-class.lisp,v 1.44 2009/10/28 07:36:15 edi Exp $

;;; This file defines the REGEX class.  REGEX objects are used to
;;; represent the (transformed) parse trees internally

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defclass regex ()
  ()
  (:documentation "The REGEX base class.  All other classes inherit
from this one."))

(defclass seq (regex)
  ((elements :initarg :elements
             :accessor elements
             :type cons
             :documentation "A list of REGEX objects."))
  (:documentation "SEQ objects represents sequences of regexes.
\(Like \"ab\" is the sequence of \"a\" and \"b\".)"))

(defclass alternation (regex)
  ((choices :initarg :choices
            :accessor choices
            :type cons
            :documentation "A list of REGEX objects"))
  (:documentation "ALTERNATION objects represent alternations of
regexes.  \(Like \"a|b\" ist the alternation of \"a\" or \"b\".)"))

(defclass lookahead (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX object we're checking.")
   (positivep :initarg :positivep
              :reader positivep
              :documentation "Whether this assertion is positive."))
  (:documentation "LOOKAHEAD objects represent look-ahead assertions."))

(defclass lookbehind (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX object we're checking.")
   (positivep :initarg :positivep
              :reader positivep
              :documentation "Whether this assertion is positive.")
   (len :initarg :len
        :accessor len
        :type fixnum
        :documentation "The \(fixed) length of the enclosed regex."))
  (:documentation "LOOKBEHIND objects represent look-behind assertions."))

(defclass repetition (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX that's repeated.")
   (greedyp :initarg :greedyp
            :reader greedyp
            :documentation "Whether the repetition is greedy.")
   (minimum :initarg :minimum
            :accessor minimum
            :type fixnum
            :documentation "The minimal number of repetitions.")
   (maximum :initarg :maximum
            :accessor maximum
            :documentation "The maximal number of repetitions.
Can be NIL for unbounded.")
   (min-len :initarg :min-len
            :reader min-len
            :documentation "The minimal length of the enclosed regex.")
   (len :initarg :len
        :reader len
        :documentation "The length of the enclosed regex.  NIL if
unknown.")
   (min-rest :initform 0
             :accessor min-rest
             :type fixnum
             :documentation "The minimal number of characters which
must appear after this repetition.")
   (contains-register-p :initarg :contains-register-p
                        :reader contains-register-p
                        :documentation "Whether the regex contains a
register."))
  (:documentation "REPETITION objects represent repetitions of regexes."))

(defmethod print-object ((repetition repetition) stream)
  (print-unreadable-object (repetition stream :type t :identity t)
    (princ (regex repetition) stream)))

(defclass register (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The inner regex.")
   (num :initarg :num
        :reader num
        :type fixnum
        :documentation "The number of this register, starting from 0.
This is the index into *REGS-START* and *REGS-END*.")
   (name :initarg :name
         :reader name
         :documentation "Name of this register or NIL."))
  (:documentation "REGISTER objects represent register groups."))

(defmethod print-object ((register register) stream)
  (print-unreadable-object (register stream :type t :identity t)
    (princ (regex register) stream)))

(defclass standalone (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The inner regex."))
  (:documentation "A standalone regular expression."))
  
(defclass back-reference (regex)
  ((num :initarg :num
        :accessor num
        :type fixnum
        :documentation "The number of the register this
reference refers to.")
   (name :initarg :name
         :accessor name
         :documentation "The name of the register this
reference refers to or NIL.")
   (case-insensitive-p :initarg :case-insensitive-p
                       :reader case-insensitive-p
                       :documentation "Whether we check
case-insensitively."))
  (:documentation "BACK-REFERENCE objects represent backreferences."))

(defclass char-class (regex)
  ((test-function :initarg :test-function
                  :reader test-function
                  :type (or function symbol nil)
                  :documentation "A unary function \(accepting a
character) which stands in for the character class and does the work
of checking whether a character belongs to the class."))
  (:documentation "CHAR-CLASS objects represent character classes."))

(defclass str (regex)
  ((str :initarg :str
        :accessor str
        :type string
        :documentation "The actual string.")
   (len :initform 0
        :accessor len
        :type fixnum
        :documentation "The length of the string.")
   (case-insensitive-p :initarg :case-insensitive-p
                       :reader case-insensitive-p
                       :documentation "If we match case-insensitively.")
   (offset :initform nil
           :accessor offset
           :documentation "Offset from the left of the whole
parse tree. The first regex has offset 0. NIL if unknown, i.e. behind
a variable-length regex.")
   (skip :initform nil
         :initarg :skip
         :accessor skip
         :documentation "If we can avoid testing for this
string because the SCAN function has done this already.")
   (start-of-end-string-p :initform nil
                          :accessor start-of-end-string-p
                          :documentation "If this is the unique
STR which starts END-STRING (a slot of MATCHER)."))
  (:documentation "STR objects represent string."))

(defmethod print-object ((str str) stream)
  (print-unreadable-object (str stream :type t :identity t)
    (princ (str str) stream)))

(defclass anchor (regex)
  ((startp :initarg :startp
           :reader startp
           :documentation "Whether this is a \"start anchor\".")
   (multi-line-p :initarg :multi-line-p
                 :initform nil
                 :reader multi-line-p
                 :documentation "Whether we're in multi-line mode,
i.e. whether each #\\Newline is surrounded by anchors.")
   (no-newline-p :initarg :no-newline-p
                 :initform nil
                 :reader no-newline-p
                 :documentation "Whether we ignore #\\Newline at the end."))
  (:documentation "ANCHOR objects represent anchors like \"^\" or \"$\"."))

(defclass everything (regex)
  ((single-line-p :initarg :single-line-p
                  :reader single-line-p
                  :documentation "Whether we're in single-line mode,
i.e. whether we also match #\\Newline."))
  (:documentation "EVERYTHING objects represent regexes matching
\"everything\", i.e. dots."))

(defclass word-boundary (regex)
  ((negatedp :initarg :negatedp
             :reader negatedp
             :documentation "Whether we mean the opposite,
i.e. no word-boundary."))
  (:documentation "WORD-BOUNDARY objects represent word-boundary assertions."))

(defclass branch (regex)
  ((test :initarg :test
         :accessor test
         :documentation "The test of this branch, one of
LOOKAHEAD, LOOKBEHIND, or a number.")
   (then-regex :initarg :then-regex
               :accessor then-regex
               :documentation "The regex that's to be matched if the
test succeeds.")
   (else-regex :initarg :else-regex
               :initform (make-instance 'void)
               :accessor else-regex
               :documentation "The regex that's to be matched if the
test fails."))
  (:documentation "BRANCH objects represent Perl's conditional regular
expressions."))
    
(defclass filter (regex)
  ((fn :initarg :fn
       :accessor fn
       :type (or function symbol)
       :documentation "The user-defined function.")
   (len :initarg :len
        :reader len
        :documentation "The fixed length of this filter or NIL."))
  (:documentation "FILTER objects represent arbitrary functions
defined by the user."))

(defclass void (regex)
  ()
  (:documentation "VOID objects represent empty regular expressions."))

(defmethod initialize-instance :after ((str str) &rest init-args)
  (declare #.*standard-optimize-settings*)
  (declare (ignore init-args))
  "Automatically computes the length of a STR after initialization."
  (let ((str-slot (slot-value str 'str)))
    (unless (typep str-slot
                   #-:lispworks 'simple-string
                   #+:lispworks 'lw:simple-text-string)
      (setf (slot-value str 'str)
            (coerce str-slot
                   #-:lispworks 'simple-string
                   #+:lispworks 'lw:simple-text-string))))
  (setf (len str) (length (str str))))

