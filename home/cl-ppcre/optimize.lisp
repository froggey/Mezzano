;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/optimize.lisp,v 1.36 2009/09/17 19:17:31 edi Exp $

;;; This file contains optimizations which can be applied to converted
;;; parse trees.

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

(defgeneric flatten (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Merges adjacent sequences and alternations, i.e. it
transforms #<SEQ #<STR \"a\"> #<SEQ #<STR \"b\"> #<STR \"c\">>> to
#<SEQ #<STR \"a\"> #<STR \"b\"> #<STR \"c\">>. This is a destructive
operation on REGEX."))

(defmethod flatten ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; this looks more complicated than it is because we modify SEQ in
  ;; place to avoid unnecessary consing
  (let ((elements-rest (elements seq)))
    (loop
      (unless elements-rest
        (return))
      (let ((flattened-element (flatten (car elements-rest)))
            (next-elements-rest (cdr elements-rest)))
        (cond ((typep flattened-element 'seq)
                ;; FLATTENED-ELEMENT is a SEQ object, so we "splice"
                ;; it into out list of elements
                (let ((flattened-element-elements
                        (elements flattened-element)))
                  (setf (car elements-rest)
                          (car flattened-element-elements)
                        (cdr elements-rest)
                          (nconc (cdr flattened-element-elements)
                                 (cdr elements-rest)))))
              (t
                ;; otherwise we just replace the current element with
                ;; its flattened counterpart
                (setf (car elements-rest) flattened-element)))
        (setq elements-rest next-elements-rest))))
  (let ((elements (elements seq)))
    (cond ((cadr elements)
            seq)
          ((cdr elements)
            (first elements))
          (t (make-instance 'void)))))

(defmethod flatten ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; same algorithm as above
  (let ((choices-rest (choices alternation)))
    (loop
      (unless choices-rest
        (return))
      (let ((flattened-choice (flatten (car choices-rest)))
            (next-choices-rest (cdr choices-rest)))
        (cond ((typep flattened-choice 'alternation)
                (let ((flattened-choice-choices
                        (choices flattened-choice)))
                  (setf (car choices-rest)
                          (car flattened-choice-choices)
                        (cdr choices-rest)
                          (nconc (cdr flattened-choice-choices)
                                 (cdr choices-rest)))))
              (t
                (setf (car choices-rest) flattened-choice)))
        (setq choices-rest next-choices-rest))))
  (let ((choices (choices alternation)))
    (cond ((cadr choices)
            alternation)
          ((cdr choices)
            (first choices))
          (t (signal-syntax-error "Encountered alternation without choices.")))))

(defmethod flatten ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test then-regex else-regex)
      branch
    (setq test
            (if (numberp test)
              test
              (flatten test))
          then-regex (flatten then-regex)
          else-regex (flatten else-regex))
    branch))

(defmethod flatten ((regex regex))
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or repetition register lookahead lookbehind standalone)
      ;; if REGEX contains exactly one inner REGEX object flatten it
      (setf (regex regex)
              (flatten (regex regex)))
      regex)
    (t
      ;; otherwise (ANCHOR, BACK-REFERENCE, CHAR-CLASS, EVERYTHING,
      ;; LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY)
      ;; do nothing
      regex)))

(defgeneric gather-strings (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Collects adjacent strings or characters into one
string provided they have the same case mode. This is a destructive
operation on REGEX."))

(defmethod gather-strings ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; note that GATHER-STRINGS is to be applied after FLATTEN, i.e. it
  ;; expects SEQ to be flattened already; in particular, SEQ cannot be
  ;; empty and cannot contain embedded SEQ objects
  (let* ((start-point (cons nil (elements seq)))
         (curr-point start-point)
         old-case-mode
         collector
         collector-start
         (collector-length 0)
         skip)
    (declare (fixnum collector-length))
    (loop
      (let ((elements-rest (cdr curr-point)))
        (unless elements-rest
          (return))
        (let* ((element (car elements-rest))
               (case-mode (case-mode element old-case-mode)))
          (cond ((and case-mode
                      (eq case-mode old-case-mode))
                  ;; if ELEMENT is a STR and we have collected a STR of
                  ;; the same case mode in the last iteration we
                  ;; concatenate ELEMENT onto COLLECTOR and remember the
                  ;; value of its SKIP slot
                  (let ((old-collector-length collector-length))
                    (unless (and (adjustable-array-p collector)
                                 (array-has-fill-pointer-p collector))
                      (setq collector
                              (make-array collector-length
                                          :initial-contents collector
                                          :element-type 'character
                                          :fill-pointer t
                                          :adjustable t)
                            collector-start nil))
                    (adjust-array collector
                                  (incf collector-length (len element))
                                  :fill-pointer t)
                    (setf (subseq collector
                                  old-collector-length)
                            (str element)
                          ;; it suffices to remember the last SKIP slot
                          ;; because due to the way MAYBE-ACCUMULATE
                          ;; works adjacent STR objects have the same
                          ;; SKIP value
                          skip (skip element)))
                  (setf (cdr curr-point) (cdr elements-rest)))
                (t
                  (let ((collected-string
                          (cond (collector-start
                                  collector-start)
                                (collector
                                  ;; if we have collected something already
                                  ;; we convert it into a STR
                                  (make-instance 'str
                                                 :skip skip
                                                 :str collector
                                                 :case-insensitive-p
                                                 (eq old-case-mode
                                                     :case-insensitive)))
                                (t nil))))
                    (cond (case-mode
                            ;; if ELEMENT is a string with a different case
                            ;; mode than the last one we have either just
                            ;; converted COLLECTOR into a STR or COLLECTOR
                            ;; is still empty; in both cases we can now
                            ;; begin to fill it anew
                            (setq collector (str element)
                                  collector-start element
                                  ;; and we remember the SKIP value as above
                                  skip (skip element)
                                  collector-length (len element))
                            (cond (collected-string
                                    (setf (car elements-rest)
                                            collected-string
                                          curr-point
                                            (cdr curr-point)))
                                  (t
                                    (setf (cdr curr-point)
                                            (cdr elements-rest)))))
                          (t
                            ;; otherwise this is not a STR so we apply
                            ;; GATHER-STRINGS to it and collect it directly
                            ;; into RESULT
                            (cond (collected-string
                                    (setf (car elements-rest)
                                            collected-string
                                          curr-point
                                            (cdr curr-point)
                                          (cdr curr-point)
                                            (cons (gather-strings element)
                                                  (cdr curr-point))
                                          curr-point
                                            (cdr curr-point)))
                                  (t
                                    (setf (car elements-rest)
                                            (gather-strings element)
                                          curr-point
                                            (cdr curr-point))))
                            ;; we also have to empty COLLECTOR here in case
                            ;; it was still filled from the last iteration
                            (setq collector nil
                                  collector-start nil))))))
          (setq old-case-mode case-mode))))
    (when collector
      (setf (cdr curr-point)
              (cons
               (make-instance 'str
                              :skip skip
                              :str collector
                              :case-insensitive-p
                              (eq old-case-mode
                                  :case-insensitive))
               nil)))
    (setf (elements seq) (cdr start-point))
    seq))

(defmethod gather-strings ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; loop ON the choices of ALTERNATION so we can modify them directly
  (loop for choices-rest on (choices alternation)
        while choices-rest
        do (setf (car choices-rest)
                   (gather-strings (car choices-rest))))
  alternation)

(defmethod gather-strings ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test then-regex else-regex)
      branch
    (setq test
            (if (numberp test)
              test
              (gather-strings test))
          then-regex (gather-strings then-regex)
          else-regex (gather-strings else-regex))
    branch))

(defmethod gather-strings ((regex regex))
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or repetition register lookahead lookbehind standalone)
      ;; if REGEX contains exactly one inner REGEX object apply
      ;; GATHER-STRINGS to it
      (setf (regex regex)
              (gather-strings (regex regex)))
      regex)
    (t
      ;; otherwise (ANCHOR, BACK-REFERENCE, CHAR-CLASS, EVERYTHING,
      ;; LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY)
      ;; do nothing
      regex)))

;; Note that START-ANCHORED-P will be called after FLATTEN and GATHER-STRINGS.

(defgeneric start-anchored-p (regex &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns T if REGEX starts with a \"real\" start
anchor, i.e. one that's not in multi-line mode, NIL otherwise. If
IN-SEQ-P is true the function will return :ZERO-LENGTH if REGEX is a
zero-length assertion."))

(defmethod start-anchored-p ((seq seq) &optional in-seq-p)
  (declare (ignore in-seq-p))
  ;; note that START-ANCHORED-P is to be applied after FLATTEN and
  ;; GATHER-STRINGS, i.e. SEQ cannot be empty and cannot contain
  ;; embedded SEQ objects
  (loop for element in (elements seq)
        for anchored-p = (start-anchored-p element t)
        ;; skip zero-length elements because they won't affect the
        ;; "anchoredness" of the sequence
        while (eq anchored-p :zero-length)
        finally (return (and anchored-p (not (eq anchored-p :zero-length))))))

(defmethod start-anchored-p ((alternation alternation) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  ;; clearly an alternation can only be start-anchored if all of its
  ;; choices are start-anchored
  (loop for choice in (choices alternation)
        always (start-anchored-p choice)))

(defmethod start-anchored-p ((branch branch) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (and (start-anchored-p (then-regex branch))
       (start-anchored-p (else-regex branch))))

(defmethod start-anchored-p ((repetition repetition) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  ;; well, this wouldn't make much sense, but anyway...
  (and (plusp (minimum repetition))
       (start-anchored-p (regex repetition))))

(defmethod start-anchored-p ((register register) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (start-anchored-p (regex register)))

(defmethod start-anchored-p ((standalone standalone) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (start-anchored-p (regex standalone)))

(defmethod start-anchored-p ((anchor anchor) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (and (startp anchor)
       (not (multi-line-p anchor))))

(defmethod start-anchored-p ((regex regex) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or lookahead lookbehind word-boundary void)
      ;; zero-length assertions
      (if in-seq-p
        :zero-length
        nil))
    (filter
      (if (and in-seq-p
               (len regex)
               (zerop (len regex)))
        :zero-length
        nil))
    (t
      ;; BACK-REFERENCE, CHAR-CLASS, EVERYTHING, and STR
      nil)))

;; Note that END-STRING-AUX will be called after FLATTEN and GATHER-STRINGS.

(defgeneric end-string-aux (regex &optional old-case-insensitive-p)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the constant string (if it exists) REGEX
ends with wrapped into a STR object, otherwise NIL.
OLD-CASE-INSENSITIVE-P is the CASE-INSENSITIVE-P slot of the last STR
collected or :VOID if no STR has been collected yet. (This is a helper
function called by END-STRING.)"))

(defmethod end-string-aux ((str str)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (declare (special last-str))
  (cond ((and (not (skip str))          ; avoid constituents of STARTS-WITH
              ;; only use STR if nothing has been collected yet or if
              ;; the collected string has the same value for
              ;; CASE-INSENSITIVE-P
              (or (eq old-case-insensitive-p :void)
                  (eq (case-insensitive-p str) old-case-insensitive-p)))
          (setf last-str str
                ;; set the SKIP property of this STR
                (skip str) t)
          str)
        (t nil)))

(defmethod end-string-aux ((seq seq)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (declare (special continuep))
  (let (case-insensitive-p
        concatenated-string
        concatenated-start
        (concatenated-length 0))
    (declare (fixnum concatenated-length))
    (loop for element in (reverse (elements seq))
          ;; remember the case-(in)sensitivity of the last relevant
          ;; STR object
          for loop-old-case-insensitive-p = old-case-insensitive-p
            then (if skip
                   loop-old-case-insensitive-p
                   (case-insensitive-p element-end))
          ;; the end-string of the current element
          for element-end = (end-string-aux element
                                            loop-old-case-insensitive-p)
          ;; whether we encountered a zero-length element
          for skip = (if element-end
                       (zerop (len element-end))
                       nil)
          ;; set CONTINUEP to NIL if we have to stop collecting to
          ;; alert END-STRING-AUX methods on enclosing SEQ objects
          unless element-end
            do (setq continuep nil)
          ;; end loop if we neither got a STR nor a zero-length
          ;; element
          while element-end
          ;; only collect if not zero-length
          unless skip
            do (cond (concatenated-string
                       (when concatenated-start
                         (setf concatenated-string
                                 (make-array concatenated-length
                                             :initial-contents (reverse (str concatenated-start))
                                             :element-type 'character
                                             :fill-pointer t
                                             :adjustable t)
                               concatenated-start nil))
                       (let ((len (len element-end))
                             (str (str element-end)))
                         (declare (fixnum len))
                         (incf concatenated-length len)
                         (loop for i of-type fixnum downfrom (1- len) to 0
                               do (vector-push-extend (char str i)
                                                      concatenated-string))))
                     (t
                       (setf concatenated-string
                               t
                             concatenated-start
                               element-end
                             concatenated-length
                               (len element-end)
                             case-insensitive-p
                               (case-insensitive-p element-end))))
          ;; stop collecting if END-STRING-AUX on inner SEQ has said so
          while continuep)
    (cond ((zerop concatenated-length)
            ;; don't bother to return zero-length strings
            nil)
          (concatenated-start
            concatenated-start)
          (t
            (make-instance 'str
                           :str (nreverse concatenated-string)
                           :case-insensitive-p case-insensitive-p)))))

(defmethod end-string-aux ((register register)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (end-string-aux (regex register) old-case-insensitive-p))
    
(defmethod end-string-aux ((standalone standalone)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (end-string-aux (regex standalone) old-case-insensitive-p))
    
(defmethod end-string-aux ((regex regex)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (declare (special last-str end-anchored-p continuep))
  (typecase regex
    ((or anchor lookahead lookbehind word-boundary void)
      ;; a zero-length REGEX object - for the sake of END-STRING-AUX
      ;; this is a zero-length string
      (when (and (typep regex 'anchor)
                 (not (startp regex))
                 (or (no-newline-p regex)
                     (not (multi-line-p regex)))
                 (eq old-case-insensitive-p :void))
        ;; if this is a "real" end-anchor and we haven't collected
        ;; anything so far we can set END-ANCHORED-P (where 1 or 0
        ;; indicate whether we accept a #\Newline at the end or not)
        (setq end-anchored-p (if (no-newline-p regex) 0 1)))
      (make-instance 'str
                     :str ""
                     :case-insensitive-p :void))
    (t
      ;; (ALTERNATION, BACK-REFERENCE, BRANCH, CHAR-CLASS, EVERYTHING,
      ;; REPETITION, FILTER)
      nil)))

(defun end-string (regex)
  (declare (special end-string-offset))
  (declare #.*standard-optimize-settings*)
  "Returns the constant string (if it exists) REGEX ends with wrapped
into a STR object, otherwise NIL."
  ;; LAST-STR points to the last STR object (seen from the end) that's
  ;; part of END-STRING; CONTINUEP is set to T if we stop collecting
  ;; in the middle of a SEQ
  (let ((continuep t)
        last-str)
    (declare (special continuep last-str))
    (prog1
      (end-string-aux regex)
      (when last-str
        ;; if we've found something set the START-OF-END-STRING-P of
        ;; the leftmost STR collected accordingly and remember the
        ;; OFFSET of this STR (in a special variable provided by the
        ;; caller of this function)
        (setf (start-of-end-string-p last-str) t
              end-string-offset (offset last-str))))))

(defgeneric compute-min-rest (regex current-min-rest)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the minimal length of REGEX plus
CURRENT-MIN-REST. This is similar to REGEX-MIN-LENGTH except that it
recurses down into REGEX and sets the MIN-REST slots of REPETITION
objects."))

(defmethod compute-min-rest ((seq seq) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (loop for element in (reverse (elements seq))
        for last-min-rest = current-min-rest then this-min-rest
        for this-min-rest = (compute-min-rest element last-min-rest)
        finally (return this-min-rest)))
    
(defmethod compute-min-rest ((alternation alternation) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (loop for choice in (choices alternation)
        minimize (compute-min-rest choice current-min-rest)))

(defmethod compute-min-rest ((branch branch) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (min (compute-min-rest (then-regex branch) current-min-rest)
       (compute-min-rest (else-regex branch) current-min-rest)))

(defmethod compute-min-rest ((str str) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (+ current-min-rest (len str)))
    
(defmethod compute-min-rest ((filter filter) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (+ current-min-rest (or (len filter) 0)))
    
(defmethod compute-min-rest ((repetition repetition) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (setf (min-rest repetition) current-min-rest)
  (compute-min-rest (regex repetition) current-min-rest)
  (+ current-min-rest (* (minimum repetition) (min-len repetition))))

(defmethod compute-min-rest ((register register) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (compute-min-rest (regex register) current-min-rest))
    
(defmethod compute-min-rest ((standalone standalone) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (declare (ignore current-min-rest))
  (compute-min-rest (regex standalone) 0))
    
(defmethod compute-min-rest ((lookahead lookahead) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (compute-min-rest (regex lookahead) 0)
  current-min-rest)
    
(defmethod compute-min-rest ((lookbehind lookbehind) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (compute-min-rest (regex lookbehind) (+ current-min-rest (len lookbehind)))
  current-min-rest)
    
(defmethod compute-min-rest ((regex regex) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or char-class everything)
      (1+ current-min-rest))
    (t
      ;; zero min-len and no embedded regexes (ANCHOR,
      ;; BACK-REFERENCE, VOID, and WORD-BOUNDARY)
      current-min-rest)))
