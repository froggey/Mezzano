;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/regex-class-util.lisp,v 1.9 2009/09/17 19:17:31 edi Exp $

;;; This file contains some utility methods for REGEX objects.

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

;;; The following four methods allow a VOID object to behave like a
;;; zero-length STR object (only readers needed)

(defmethod len ((void void))
  (declare #.*standard-optimize-settings*)
  0)

(defmethod str ((void void))
  (declare #.*standard-optimize-settings*)
  "")

(defmethod skip ((void void))
  (declare #.*standard-optimize-settings*)
  nil)

(defmethod start-of-end-string-p ((void void))
  (declare #.*standard-optimize-settings*)
  nil)

(defgeneric case-mode (regex old-case-mode)
  (declare #.*standard-optimize-settings*)
  (:documentation "Utility function used by the optimizer (see GATHER-STRINGS).
Returns a keyword denoting the case-(in)sensitivity of a STR or its
second argument if the STR has length 0. Returns NIL for REGEX objects
which are not of type STR."))

(defmethod case-mode ((str str) old-case-mode)
  (declare #.*standard-optimize-settings*)
  (cond ((zerop (len str))
          old-case-mode)
        ((case-insensitive-p str)
          :case-insensitive)
        (t
          :case-sensitive)))

(defmethod case-mode ((regex regex) old-case-mode)
  (declare #.*standard-optimize-settings*)
  (declare (ignore old-case-mode))
  nil)

(defgeneric copy-regex (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Implements a deep copy of a REGEX object."))

(defmethod copy-regex ((anchor anchor))
  (declare #.*standard-optimize-settings*)
  (make-instance 'anchor
                 :startp (startp anchor)
                 :multi-line-p (multi-line-p anchor)
                 :no-newline-p (no-newline-p anchor)))

(defmethod copy-regex ((everything everything))
  (declare #.*standard-optimize-settings*)
  (make-instance 'everything
                 :single-line-p (single-line-p everything)))

(defmethod copy-regex ((word-boundary word-boundary))
  (declare #.*standard-optimize-settings*)
  (make-instance 'word-boundary
                 :negatedp (negatedp word-boundary)))

(defmethod copy-regex ((void void))
  (declare #.*standard-optimize-settings*)
  (make-instance 'void))

(defmethod copy-regex ((lookahead lookahead))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookahead
                 :regex (copy-regex (regex lookahead))
                 :positivep (positivep lookahead)))

(defmethod copy-regex ((seq seq))
  (declare #.*standard-optimize-settings*)
  (make-instance 'seq
                 :elements (mapcar #'copy-regex (elements seq))))

(defmethod copy-regex ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  (make-instance 'alternation
                 :choices (mapcar #'copy-regex (choices alternation))))

(defmethod copy-regex ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test)
      branch
    (make-instance 'branch
                   :test (if (typep test 'regex)
                           (copy-regex test)
                           test)
                   :then-regex (copy-regex (then-regex branch))
                   :else-regex (copy-regex (else-regex branch)))))

(defmethod copy-regex ((lookbehind lookbehind))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookbehind
                 :regex (copy-regex (regex lookbehind))
                 :positivep (positivep lookbehind)
                 :len (len lookbehind)))

(defmethod copy-regex ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  (make-instance 'repetition
                 :regex (copy-regex (regex repetition))
                 :greedyp (greedyp repetition)
                 :minimum (minimum repetition)
                 :maximum (maximum repetition)
                 :min-len (min-len repetition)
                 :len (len repetition)
                 :contains-register-p (contains-register-p repetition)))

(defmethod copy-regex ((register register))
  (declare #.*standard-optimize-settings*)
  (make-instance 'register
                 :regex (copy-regex (regex register))
                 :num (num register)
                 :name (name register)))

(defmethod copy-regex ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (make-instance 'standalone
                 :regex (copy-regex (regex standalone))))

(defmethod copy-regex ((back-reference back-reference))
  (declare #.*standard-optimize-settings*)
  (make-instance 'back-reference
                 :num (num back-reference)
                 :case-insensitive-p (case-insensitive-p back-reference)))

(defmethod copy-regex ((char-class char-class))
  (declare #.*standard-optimize-settings*)
  (make-instance 'char-class
                 :test-function (test-function char-class)))

(defmethod copy-regex ((str str))
  (declare #.*standard-optimize-settings*)
  (make-instance 'str
                 :str (str str)
                 :case-insensitive-p (case-insensitive-p str)))

(defmethod copy-regex ((filter filter))
  (declare #.*standard-optimize-settings*)
  (make-instance 'filter
                 :fn (fn filter)
                 :len (len filter)))

;;; Note that COPY-REGEX and REMOVE-REGISTERS could have easily been
;;; wrapped into one function. Maybe in the next release...

;;; Further note that this function is used by CONVERT to factor out
;;; complicated repetitions, i.e. cases like
;;;   (a)* -> (?:a*(a))?
;;; This won't work for, say,
;;;   ((a)|(b))* -> (?:(?:a|b)*((a)|(b)))?
;;; and therefore we stop REGISTER removal once we see an ALTERNATION.

(defgeneric remove-registers (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns a deep copy of a REGEX (see COPY-REGEX) and
optionally removes embedded REGISTER objects if possible and if the
special variable REMOVE-REGISTERS-P is true."))

(defmethod remove-registers ((register register))
  (declare #.*standard-optimize-settings*)
  (declare (special remove-registers-p reg-seen))
  (cond (remove-registers-p
          (remove-registers (regex register)))
        (t
          ;; mark REG-SEEN as true so enclosing REPETITION objects
          ;; (see method below) know if they contain a register or not
          (setq reg-seen t)
          (copy-regex register))))

(defmethod remove-registers ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  (let* (reg-seen
         (inner-regex (remove-registers (regex repetition))))
    ;; REMOVE-REGISTERS will set REG-SEEN (see method above) if
    ;; (REGEX REPETITION) contains a REGISTER
    (declare (special reg-seen))
    (make-instance 'repetition
                   :regex inner-regex
                   :greedyp (greedyp repetition)
                   :minimum (minimum repetition)
                   :maximum (maximum repetition)
                   :min-len (min-len repetition)
                   :len (len repetition)
                   :contains-register-p reg-seen)))

(defmethod remove-registers ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (make-instance 'standalone
                 :regex (remove-registers (regex standalone))))

(defmethod remove-registers ((lookahead lookahead))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookahead
                 :regex (remove-registers (regex lookahead))
                 :positivep (positivep lookahead)))

(defmethod remove-registers ((lookbehind lookbehind))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookbehind
                 :regex (remove-registers (regex lookbehind))
                 :positivep (positivep lookbehind)
                 :len (len lookbehind)))

(defmethod remove-registers ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test)
      branch
    (make-instance 'branch
                   :test (if (typep test 'regex)
                           (remove-registers test)
                           test)
                   :then-regex (remove-registers (then-regex branch))
                   :else-regex (remove-registers (else-regex branch)))))

(defmethod remove-registers ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  (declare (special remove-registers-p))
  ;; an ALTERNATION, so we can't remove REGISTER objects further down
  (setq remove-registers-p nil)
  (copy-regex alternation))

(defmethod remove-registers ((regex regex))
  (declare #.*standard-optimize-settings*)
  (copy-regex regex))

(defmethod remove-registers ((seq seq))
  (declare #.*standard-optimize-settings*)
  (make-instance 'seq
                 :elements (mapcar #'remove-registers (elements seq))))

(defgeneric everythingp (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns an EVERYTHING object if REGEX is equivalent
to this object, otherwise NIL.  So, \"(.){1}\" would return true
\(i.e. the object corresponding to \".\", for example."))

(defmethod everythingp ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; we might have degenerate cases like (:SEQUENCE :VOID ...)
  ;; due to the parsing process
  (let ((cleaned-elements (remove-if #'(lambda (element)
                                         (typep element 'void))
                                     (elements seq))))
    (and (= 1 (length cleaned-elements))
         (everythingp (first cleaned-elements)))))

(defmethod everythingp ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  (with-slots (choices)
      alternation
    (and (= 1 (length choices))
         ;; this is unlikely to happen for human-generated regexes,
         ;; but machine-generated ones might look like this
         (everythingp (first choices)))))

(defmethod everythingp ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  (with-slots (maximum minimum regex)
      repetition
    (and maximum
         (= 1 minimum maximum)
         ;; treat "<regex>{1,1}" like "<regex>"
         (everythingp regex))))

(defmethod everythingp ((register register))
  (declare #.*standard-optimize-settings*)
  (everythingp (regex register)))

(defmethod everythingp ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (everythingp (regex standalone)))

(defmethod everythingp ((everything everything))
  (declare #.*standard-optimize-settings*)
  everything)

(defmethod everythingp ((regex regex))
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, BACK-REFERENCE, BRANCH, CHAR-CLASS,
  ;; LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY
  nil)

(defgeneric regex-length (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Return the length of REGEX if it is fixed, NIL otherwise."))

(defmethod regex-length ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; simply add all inner lengths unless one of them is NIL
  (loop for sub-regex in (elements seq)
        for len = (regex-length sub-regex)
        if (not len) do (return nil)
        sum len))

(defmethod regex-length ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; only return a true value if all inner lengths are non-NIL and
  ;; mutually equal
  (loop for sub-regex in (choices alternation)
        for old-len = nil then len
        for len = (regex-length sub-regex)
        if (or (not len)
               (and old-len (/= len old-len))) do (return nil)
        finally (return len)))

(defmethod regex-length ((branch branch))
  (declare #.*standard-optimize-settings*)
  ;; only return a true value if both alternations have a length and
  ;; if they're equal
  (let ((then-length (regex-length (then-regex branch))))
    (and then-length
         (eql then-length (regex-length (else-regex branch)))
         then-length)))

(defmethod regex-length ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  ;; we can only compute the length of a REPETITION object if the
  ;; number of repetitions is fixed; note that we don't call
  ;; REGEX-LENGTH for the inner regex, we assume that the LEN slot is
  ;; always set correctly
  (with-slots (len minimum maximum)
      repetition
    (if (and len
             (eql minimum maximum))
      (* minimum len)
      nil)))

(defmethod regex-length ((register register))
  (declare #.*standard-optimize-settings*)
  (regex-length (regex register)))

(defmethod regex-length ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (regex-length (regex standalone)))

(defmethod regex-length ((back-reference back-reference))
  (declare #.*standard-optimize-settings*)
  ;; with enough effort we could possibly do better here, but
  ;; currently we just give up and return NIL
  nil)
    
(defmethod regex-length ((char-class char-class))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-length ((everything everything))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-length ((str str))
  (declare #.*standard-optimize-settings*)
  (len str))

(defmethod regex-length ((filter filter))
  (declare #.*standard-optimize-settings*)
  (len filter))

(defmethod regex-length ((regex regex))
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  ;; WORD-BOUNDARY (which all have zero-length)
  0)

(defgeneric regex-min-length (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the minimal length of REGEX."))

(defmethod regex-min-length ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; simply add all inner minimal lengths
  (loop for sub-regex in (elements seq)
        for len = (regex-min-length sub-regex)
        sum len))

(defmethod regex-min-length ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; minimal length of an alternation is the minimal length of the
  ;; "shortest" element
  (loop for sub-regex in (choices alternation)
        for len = (regex-min-length sub-regex)
        minimize len))

(defmethod regex-min-length ((branch branch))
  (declare #.*standard-optimize-settings*)
  ;; minimal length of both alternations
  (min (regex-min-length (then-regex branch))
       (regex-min-length (else-regex branch))))

(defmethod regex-min-length ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  ;; obviously the product of the inner minimal length and the minimal
  ;; number of repetitions
  (* (minimum repetition) (min-len repetition)))
    
(defmethod regex-min-length ((register register))
  (declare #.*standard-optimize-settings*)
  (regex-min-length (regex register)))
    
(defmethod regex-min-length ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (regex-min-length (regex standalone)))
    
(defmethod regex-min-length ((char-class char-class))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-min-length ((everything everything))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-min-length ((str str))
  (declare #.*standard-optimize-settings*)
  (len str))
    
(defmethod regex-min-length ((filter filter))
  (declare #.*standard-optimize-settings*)
  (or (len filter)
      0))

(defmethod regex-min-length ((regex regex))
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, BACK-REFERENCE, LOOKAHEAD,
  ;; LOOKBEHIND, VOID, and WORD-BOUNDARY
  0)

(defgeneric compute-offsets (regex start-pos)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the offset the following regex would have
relative to START-POS or NIL if we can't compute it. Sets the OFFSET
slot of REGEX to START-POS if REGEX is a STR. May also affect OFFSET
slots of STR objects further down the tree."))

;; note that we're actually only interested in the offset of
;; "top-level" STR objects (see ADVANCE-FN in the SCAN function) so we
;; can stop at variable-length alternations and don't need to descend
;; into repetitions

(defmethod compute-offsets ((seq seq) start-pos)
  (declare #.*standard-optimize-settings*)
  (loop for element in (elements seq)
        ;; advance offset argument for next call while looping through
        ;; the elements
        for pos = start-pos then curr-offset
        for curr-offset = (compute-offsets element pos)
        while curr-offset
        finally (return curr-offset)))

(defmethod compute-offsets ((alternation alternation) start-pos)
  (declare #.*standard-optimize-settings*)
  (loop for choice in (choices alternation)
        for old-offset = nil then curr-offset
        for curr-offset = (compute-offsets choice start-pos)
        ;; we stop immediately if two alternations don't result in the
        ;; same offset
        if (or (not curr-offset)
               (and old-offset (/= curr-offset old-offset)))
          do (return nil)
        finally (return curr-offset)))

(defmethod compute-offsets ((branch branch) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; only return offset if both alternations have equal value
  (let ((then-offset (compute-offsets (then-regex branch) start-pos)))
    (and then-offset
         (eql then-offset (compute-offsets (else-regex branch) start-pos))
         then-offset)))

(defmethod compute-offsets ((repetition repetition) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; no need to descend into the inner regex
  (with-slots (len minimum maximum)
      repetition
    (if (and len
             (eq minimum maximum))
      ;; fixed number of repetitions, so we know how to proceed
      (+ start-pos (* minimum len))
      ;; otherwise return NIL
      nil)))

(defmethod compute-offsets ((register register) start-pos)
  (declare #.*standard-optimize-settings*)
  (compute-offsets (regex register) start-pos))
    
(defmethod compute-offsets ((standalone standalone) start-pos)
  (declare #.*standard-optimize-settings*)
  (compute-offsets (regex standalone) start-pos))
    
(defmethod compute-offsets ((char-class char-class) start-pos)
  (declare #.*standard-optimize-settings*)
  (1+ start-pos))
    
(defmethod compute-offsets ((everything everything) start-pos)
  (declare #.*standard-optimize-settings*)
  (1+ start-pos))
    
(defmethod compute-offsets ((str str) start-pos)
  (declare #.*standard-optimize-settings*)
  (setf (offset str) start-pos)
  (+ start-pos (len str)))

(defmethod compute-offsets ((back-reference back-reference) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; with enough effort we could possibly do better here, but
  ;; currently we just give up and return NIL
  (declare (ignore start-pos))
  nil)

(defmethod compute-offsets ((filter filter) start-pos)
  (declare #.*standard-optimize-settings*)
  (let ((len (len filter)))
    (if len
      (+ start-pos len)
      nil)))

(defmethod compute-offsets ((regex regex) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  ;; WORD-BOUNDARY (which all have zero-length)
  start-pos)
