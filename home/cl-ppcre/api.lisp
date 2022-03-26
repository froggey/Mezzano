;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/api.lisp,v 1.85 2009/09/17 19:17:30 edi Exp $

;;; The external API for creating and using scanners.

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

(defvar *look-ahead-for-suffix* t
  "Controls whether scanners will optimistically look ahead for a
  constant suffix of a regular expression, if there is one.")

(defgeneric create-scanner (regex &key case-insensitive-mode
                                       multi-line-mode
                                       single-line-mode
                                       extended-mode
                                       destructive)
  (:documentation "Accepts a regular expression - either as a
parse-tree or as a string - and returns a scan closure which will scan
strings for this regular expression and a list mapping registers to
their names \(NIL stands for unnamed ones).  The \"mode\" keyword
arguments are equivalent to the imsx modifiers in Perl.  If
DESTRUCTIVE is not NIL, the function is allowed to destructively
modify its first argument \(but only if it's a parse tree)."))

#-:use-acl-regexp2-engine
(defmethod create-scanner ((regex-string string) &key case-insensitive-mode
                                                      multi-line-mode
                                                      single-line-mode
                                                      extended-mode
                                                      destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  ;; parse the string into a parse-tree and then call CREATE-SCANNER
  ;; again
  (let* ((*extended-mode-p* extended-mode)
         (quoted-regex-string (if *allow-quoting*
                                (quote-sections (clean-comments regex-string extended-mode))
                                regex-string))
         (*syntax-error-string* (copy-seq quoted-regex-string)))
    ;; wrap the result with :GROUP to avoid infinite loops for
    ;; constant strings
    (create-scanner (cons :group (list (parse-string quoted-regex-string)))
                    :case-insensitive-mode case-insensitive-mode
                    :multi-line-mode multi-line-mode
                    :single-line-mode single-line-mode
                    :destructive t)))

#-:use-acl-regexp2-engine
(defmethod create-scanner ((scanner function) &key case-insensitive-mode
                                                   multi-line-mode
                                                   single-line-mode
                                                   extended-mode
                                                   destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  (when (or case-insensitive-mode multi-line-mode single-line-mode extended-mode)
    (signal-invocation-error "You can't use the keyword arguments to modify an existing scanner."))
  scanner)

#-:use-acl-regexp2-engine
(defmethod create-scanner ((parse-tree t) &key case-insensitive-mode
                                               multi-line-mode
                                               single-line-mode
                                               extended-mode
                                               destructive)
  (declare #.*standard-optimize-settings*)
  (when extended-mode
    (signal-invocation-error "Extended mode doesn't make sense in parse trees."))
  ;; convert parse-tree into internal representation REGEX and at the
  ;; same time compute the number of registers and the constant string
  ;; (or anchor) the regex starts with (if any)
  (unless destructive
    (setq parse-tree (copy-tree parse-tree)))
  (let (flags)
    (if single-line-mode
      (push :single-line-mode-p flags))
    (if multi-line-mode
      (push :multi-line-mode-p flags))
    (if case-insensitive-mode
      (push :case-insensitive-p flags))
    (when flags
      (setq parse-tree (list :group (cons :flags flags) parse-tree))))
  (let ((*syntax-error-string* nil))
    (multiple-value-bind (regex reg-num starts-with reg-names)
        (convert parse-tree)
      ;; simplify REGEX by flattening nested SEQ and ALTERNATION
      ;; constructs and gathering STR objects
      (let ((regex (gather-strings (flatten regex))))
        ;; set the MIN-REST slots of the REPETITION objects
        (compute-min-rest regex 0)
        ;; set the OFFSET slots of the STR objects
        (compute-offsets regex 0)
        (let* (end-string-offset
               end-anchored-p
               ;; compute the constant string the regex ends with (if
               ;; any) and at the same time set the special variables
               ;; END-STRING-OFFSET and END-ANCHORED-P
               (end-string (end-string regex))
               ;; if we found a non-zero-length end-string we create an
               ;; efficient search function for it
               (end-string-test (and *look-ahead-for-suffix*
                                     end-string
                                     (plusp (len end-string))
                                     (if (= 1 (len end-string))
                                       (create-char-searcher
                                        (schar (str end-string) 0)
                                        (case-insensitive-p end-string))
                                       (create-bmh-matcher
                                        (str end-string)
                                        (case-insensitive-p end-string)))))
               ;; initialize the counters for CREATE-MATCHER-AUX
               (*rep-num* 0)
               (*zero-length-num* 0)
               ;; create the actual matcher function (which does all the
               ;; work of matching the regular expression) corresponding
               ;; to REGEX and at the same time set the special
               ;; variables *REP-NUM* and *ZERO-LENGTH-NUM*
               (match-fn (create-matcher-aux regex #'identity))
               ;; if the regex starts with a string we create an
               ;; efficient search function for it
               (start-string-test (and (typep starts-with 'str)
                                       (plusp (len starts-with))
                                       (if (= 1 (len starts-with))
                                         (create-char-searcher
                                          (schar (str starts-with) 0)
                                          (case-insensitive-p starts-with))
                                         (create-bmh-matcher
                                          (str starts-with)
                                          (case-insensitive-p starts-with))))))
          (declare (special end-string-offset end-anchored-p end-string))
          ;; now create the scanner and return it
          (values (create-scanner-aux match-fn
                                      (regex-min-length regex)
                                      (or (start-anchored-p regex)
                                          ;; a dot in single-line-mode also
                                          ;; implicitly anchors the regex at
                                          ;; the start, i.e. if we can't match
                                          ;; from the first position we won't
                                          ;; match at all
                                          (and (typep starts-with 'everything)
                                               (single-line-p starts-with)))
                                      starts-with
                                      start-string-test
                                      ;; only mark regex as end-anchored if we
                                      ;; found a non-zero-length string before
                                      ;; the anchor
                                      (and end-string-test end-anchored-p)
                                      end-string-test
                                      (if end-string-test
                                          (len end-string)
                                          nil)
                                      end-string-offset
                                      *rep-num*
                                      *zero-length-num*
                                      reg-num)
                  reg-names))))))

#+:use-acl-regexp2-engine
(declaim (inline create-scanner))
#+:use-acl-regexp2-engine
(defmethod create-scanner ((scanner regexp::regular-expression) &key case-insensitive-mode
                                                                     multi-line-mode
                                                                     single-line-mode
                                                                     extended-mode
                                                                     destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  (when (or case-insensitive-mode multi-line-mode single-line-mode extended-mode)
    (signal-invocation-error "You can't use the keyword arguments to modify an existing scanner."))
  scanner)

#+:use-acl-regexp2-engine
(defmethod create-scanner ((parse-tree t) &key case-insensitive-mode
                                               multi-line-mode
                                               single-line-mode
                                               extended-mode
                                               destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  (excl:compile-re parse-tree
                   :case-fold case-insensitive-mode
                   :ignore-whitespace extended-mode
                   :multiple-lines multi-line-mode
                   :single-line single-line-mode
                   :return :index))

(defgeneric scan (regex target-string &key start end real-start-pos)
  (:documentation "Searches TARGET-STRING from START to END and tries
to match REGEX.  On success returns four values - the start of the
match, the end of the match, and two arrays denoting the beginnings
and ends of register matches.  On failure returns NIL.  REGEX can be a
string which will be parsed according to Perl syntax, a parse tree, or
a pre-compiled scanner created by CREATE-SCANNER.  TARGET-STRING will
be coerced to a simple string if it isn't one already.  The
REAL-START-POS parameter should be ignored - it exists only for
internal purposes."))

#-:use-acl-regexp2-engine
(defmethod scan ((regex-string string) target-string
                                       &key (start 0)
                                            (end (length target-string))
                                            ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  ;; note that the scanners are optimized for simple strings so we
  ;; have to coerce TARGET-STRING into one if it isn't already
  (funcall (create-scanner regex-string)
           (maybe-coerce-to-simple-string target-string)
           start end))

#-:use-acl-regexp2-engine
(defmethod scan ((scanner function) target-string
                                    &key (start 0)
                                         (end (length target-string))
                                         ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  (funcall scanner
           (maybe-coerce-to-simple-string target-string)
           start end))

#-:use-acl-regexp2-engine
(defmethod scan ((parse-tree t) target-string
                                &key (start 0)
                                     (end (length target-string))
                                     ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  (funcall (create-scanner parse-tree)
           (maybe-coerce-to-simple-string target-string)
           start end))

#+:use-acl-regexp2-engine
(declaim (inline scan))
#+:use-acl-regexp2-engine
(defmethod scan ((parse-tree t) target-string
                                &key (start 0)
                                     (end (length target-string))
                                     ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  (when (< end start)
    (return-from scan nil))
  (let ((results (multiple-value-list (excl:match-re parse-tree target-string
                                                     :start start
                                                     :end end
                                                     :return :index))))
    (declare (dynamic-extent results))
    (cond ((null (first results)) nil)
          (t (let* ((no-of-regs (- (length results) 2))
                    (reg-starts (make-array no-of-regs
                                            :element-type '(or null fixnum)))
                    (reg-ends (make-array no-of-regs
                                          :element-type '(or null fixnum)))
                    (match (second results)))
               (loop for (start . end) in (cddr results)
                     for i from 0
                     do (setf (aref reg-starts i) start
                              (aref reg-ends i) end))
               (values (car match) (cdr match) reg-starts reg-ends))))))

#-:cormanlisp
(define-compiler-macro scan (&whole form regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  ;; Don't pass &environment to CONSTANTP, it may not be digestable by
  ;; LOAD-TIME-VALUE, e.g., MACROLETs.
  (cond ((constantp regex)
         `(scan (load-time-value (create-scanner ,regex))
                ,target-string ,@rest))
        (t form)))

(defun scan-to-strings (regex target-string &key (start 0)
                                                 (end (length target-string))
                                                 sharedp)
  "Like SCAN but returns substrings of TARGET-STRING instead of
positions, i.e. this function returns two values on success: the whole
match as a string plus an array of substrings (or NILs) corresponding
to the matched registers.  If SHAREDP is true, the substrings may
share structure with TARGET-STRING."
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (unless match-start
      (return-from scan-to-strings nil))
    (let ((substr-fn (if sharedp #'nsubseq #'subseq)))
      (values (funcall substr-fn
                       target-string match-start match-end)
              (map 'vector
                   (lambda (reg-start reg-end)
                     (if reg-start
                       (funcall substr-fn
                                target-string reg-start reg-end)
                       nil))
                   reg-starts
                   reg-ends)))))

#-:cormanlisp
(define-compiler-macro scan-to-strings
    (&whole form regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(scan-to-strings (load-time-value (create-scanner ,regex))
                           ,target-string ,@rest))
        (t form)))

(defmacro register-groups-bind (var-list (regex target-string
                                                &key start end sharedp)
                                &body body)
  "Executes BODY with the variables in VAR-LIST bound to the
corresponding register groups after TARGET-STRING has been matched
against REGEX, i.e. each variable is either bound to a string or to
NIL.  If there is no match, BODY is _not_ executed. For each element
of VAR-LIST which is NIL there's no binding to the corresponding
register group.  The number of variables in VAR-LIST must not be
greater than the number of register groups.  If SHAREDP is true, the
substrings may share structure with TARGET-STRING."
  (with-rebinding (target-string)
    (with-unique-names (match-start match-end reg-starts reg-ends
                                    start-index substr-fn)
      (let ((var-bindings
              (loop for (function var) in (normalize-var-list var-list)
                    for counter from 0
                    when var
                      collect `(,var (let ((,start-index
                                             (aref ,reg-starts ,counter)))
                                       (if ,start-index
                                           (funcall ,function
                                                    (funcall ,substr-fn
                                                             ,target-string
                                                             ,start-index
                                                             (aref ,reg-ends ,counter)))
                                           nil))))))
        `(multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
             (scan ,regex ,target-string :start (or ,start 0)
                                         :end (or ,end (length ,target-string)))
           (declare (ignore ,match-end))
           ,@(unless var-bindings
               `((declare (ignore ,reg-starts ,reg-ends))))
           (when ,match-start
             ,@(if var-bindings
                   `((let* ,(list*
                             `(,substr-fn (if ,sharedp #'nsubseq #'subseq))
                             var-bindings)
                       ,@body))
                   body)))))))

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex
                                 target-string
                                 &optional result-form
                                 &key start end)
                    &body body
                    &environment env)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-START, MATCH-END, REG-STARTS, and
REG-ENDS bound to the four return values of each match in turn.  After
the last match, returns RESULT-FORM if provided or NIL otherwise. An
implicit block named NIL surrounds DO-SCANS; RETURN may be used to
terminate the loop immediately.  If REGEX matches an empty string the
scan is continued one position behind this match. BODY may start with
declarations."
  (with-rebinding (target-string)
    (with-unique-names (%start %end %regex scanner)
      (declare (ignorable %regex scanner))
      ;; the NIL BLOCK to enable exits via (RETURN ...)
      `(block nil
         (let* ((,%start (or ,start 0))
                (,%end (or ,end (length ,target-string)))
                ,@(unless (constantp regex env)
                    ;; leave constant regular expressions as they are -
                    ;; SCAN's compiler macro will take care of them;
                    ;; otherwise create a scanner unless the regex is
                    ;; already a function (otherwise SCAN will do this
                    ;; on each iteration)
                    `((,%regex ,regex)
                      (,scanner (typecase ,%regex
                                  (function ,%regex)
                                  (t (create-scanner ,%regex)))))))
           ;; coerce TARGET-STRING to a simple string unless it is one
           ;; already (otherwise SCAN will do this on each iteration)
           (setq ,target-string
                 (maybe-coerce-to-simple-string ,target-string))
           (loop
            ;; invoke SCAN and bind the returned values to the
            ;; provided variables
            (multiple-value-bind
                (,match-start ,match-end ,reg-starts ,reg-ends)
                (scan ,(cond ((constantp regex env) regex)
                             (t scanner))
                      ,target-string :start ,%start :end ,%end
                      :real-start-pos (or ,start 0))
              ;; declare the variables to be IGNORABLE to prevent the
              ;; compiler from issuing warnings
              (declare
               (ignorable ,match-start ,match-end ,reg-starts ,reg-ends))
              (unless ,match-start
                ;; stop iteration on first failure
                (return ,result-form))
              ;; execute BODY (wrapped in LOCALLY so it can start with
              ;; declarations)
              (locally
                ,@body)
              ;; advance by one position if we had a zero-length match
              (setq ,%start (if (= ,match-start ,match-end)
                              (1+ ,match-end)
                              ,match-end)))))))))

(defmacro do-matches ((match-start match-end regex
                                   target-string
                                   &optional result-form
                                   &key start end)
                      &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-START and MATCH-END bound to the
start/end positions of each match in turn.  After the last match,
returns RESULT-FORM if provided or NIL otherwise.  An implicit block
named NIL surrounds DO-MATCHES; RETURN may be used to terminate the
loop immediately.  If REGEX matches an empty string the scan is
continued one position behind this match.  BODY may start with
declarations."
  ;; this is a simplified form of DO-SCANS - we just provide two dummy
  ;; vars and ignore them
  (with-unique-names (reg-starts reg-ends)
    `(do-scans (,match-start ,match-end
                ,reg-starts ,reg-ends
                ,regex ,target-string
                ,result-form
                :start ,start :end ,end)
      ,@body)))

(defmacro do-matches-as-strings ((match-var regex
                                            target-string
                                            &optional result-form
                                            &key start end sharedp)
                                 &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-VAR bound to the substring of
TARGET-STRING corresponding to each match in turn.  After the last
match, returns RESULT-FORM if provided or NIL otherwise.  An implicit
block named NIL surrounds DO-MATCHES-AS-STRINGS; RETURN may be used to
terminate the loop immediately.  If REGEX matches an empty string the
scan is continued one position behind this match.  If SHAREDP is true,
the substrings may share structure with TARGET-STRING.  BODY may start
with declarations."
  (with-rebinding (target-string)
    (with-unique-names (match-start match-end substr-fn)
      `(let ((,substr-fn (if ,sharedp #'nsubseq #'subseq)))
        ;; simple use DO-MATCHES to extract the substrings
        (do-matches (,match-start ,match-end ,regex ,target-string
                     ,result-form :start ,start :end ,end)
          (let ((,match-var
                  (funcall ,substr-fn
                           ,target-string ,match-start ,match-end)))
            ,@body))))))

(defmacro do-register-groups (var-list (regex target-string
                                              &optional result-form
                                              &key start end sharedp)
                                       &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with the variables in VAR-LIST bound to the
corresponding register groups for each match in turn, i.e. each
variable is either bound to a string or to NIL.  For each element of
VAR-LIST which is NIL there's no binding to the corresponding register
group. The number of variables in VAR-LIST must not be greater than
the number of register groups.  After the last match, returns
RESULT-FORM if provided or NIL otherwise.  An implicit block named NIL
surrounds DO-REGISTER-GROUPS; RETURN may be used to terminate the loop
immediately. If REGEX matches an empty string the scan is continued
one position behind this match.  If SHAREDP is true, the substrings
may share structure with TARGET-STRING.  BODY may start with
declarations."
  (with-rebinding (target-string)
    (with-unique-names (substr-fn match-start match-end
                                  reg-starts reg-ends start-index)
      `(let ((,substr-fn (if ,sharedp
                          #'nsubseq
                          #'subseq)))
        (do-scans (,match-start ,match-end ,reg-starts ,reg-ends
                                ,regex ,target-string
                                ,result-form :start ,start :end ,end)
          (let ,(loop for (function var) in (normalize-var-list var-list)
                      for counter from 0
                      when var
                        collect `(,var (let ((,start-index
                                               (aref ,reg-starts ,counter)))
                                         (if ,start-index
                                           (funcall ,function
                                                    (funcall ,substr-fn
                                                             ,target-string
                                                             ,start-index
                                                             (aref ,reg-ends ,counter)))
                                           nil))))
            ,@body))))))

(defun all-matches (regex target-string
                          &key (start 0)
                               (end (length target-string)))
  "Returns a list containing the start and end positions of all
matches of REGEX against TARGET-STRING, i.e. if there are N matches
the list contains (* 2 N) elements.  If REGEX matches an empty string
the scan is continued one position behind this match."
  (declare #.*standard-optimize-settings*)
  (let (result-list)
    (do-matches (match-start match-end
                 regex target-string
                 (nreverse result-list)
                 :start start :end end)
      (push match-start result-list)
      (push match-end result-list))))

#-:cormanlisp
(define-compiler-macro all-matches (&whole form regex &rest rest)
   "Make sure that constant forms are compiled into scanners at
compile time."
   (cond ((constantp regex)
          `(all-matches (load-time-value (create-scanner ,regex))
                        ,@rest))
         (t form)))

(defun all-matches-as-strings (regex target-string
                                     &key (start 0)
                                          (end (length target-string))
                                          sharedp)
  "Returns a list containing all substrings of TARGET-STRING which
match REGEX. If REGEX matches an empty string the scan is continued
one position behind this match. If SHAREDP is true, the substrings may
share structure with TARGET-STRING."
  (declare #.*standard-optimize-settings*)
  (let (result-list)
    (do-matches-as-strings (match regex target-string (nreverse result-list)
                                  :start start :end end :sharedp sharedp)
      (push match result-list))))

#-:cormanlisp
(define-compiler-macro all-matches-as-strings (&whole form regex &rest rest)
   "Make sure that constant forms are compiled into scanners at
compile time."
   (cond ((constantp regex)
          `(all-matches-as-strings
            (load-time-value (create-scanner ,regex))
            ,@rest))
         (t form)))

(defun split (regex target-string
                    &key (start 0)
                         (end (length target-string))
                         limit
                         with-registers-p
                         omit-unmatched-p
                         sharedp)
  "Matches REGEX against TARGET-STRING as often as possible and
returns a list of the substrings between the matches.  If
WITH-REGISTERS-P is true, substrings corresponding to matched
registers are inserted into the list as well.  If OMIT-UNMATCHED-P is
true, unmatched registers will simply be left out, otherwise they will
show up as NIL.  LIMIT limits the number of elements returned -
registers aren't counted.  If LIMIT is NIL \(or 0 which is
equivalent), trailing empty strings are removed from the result list.
If REGEX matches an empty string the scan is continued one position
behind this match.  If SHAREDP is true, the substrings may share
structure with TARGET-STRING."
  (declare #.*standard-optimize-settings*)
  ;; initialize list of positions POS-LIST to extract substrings with
  ;; START so that the start of the next match will mark the end of
  ;; the first substring
  (let ((pos-list (list start))
        (counter 0))
    ;; how would Larry Wall do it?
    (when (eql limit 0)
      (setq limit nil))
    (do-scans (match-start match-end
               reg-starts reg-ends
               regex target-string nil
               :start start :end end)
      (unless (and (= match-start match-end)
                   (= match-start (car pos-list)))
        ;; push start of match on list unless this would be an empty
        ;; string adjacent to the last element pushed onto the list
        (when (and limit
                   ;; perlfunc(1) says
                   ;;   If LIMIT is negative, it is treated as if
                   ;;   it were instead arbitrarily large;
                   ;;   as many fields as possible are produced.
                   (plusp limit)
                   (>= (incf counter) limit))
          (return))
        (push match-start pos-list)
        (when with-registers-p
          ;; optionally insert matched registers
          (loop for reg-start across reg-starts
                for reg-end across reg-ends
                if reg-start
                  ;; but only if they've matched
                  do (push reg-start pos-list)
                     (push reg-end pos-list)
                else unless omit-unmatched-p
                  ;; or if we're allowed to insert NIL instead
                  do (push nil pos-list)
                     (push nil pos-list)))
        ;; now end of match
        (push match-end pos-list)))
    ;; end of whole string
    (push end pos-list)
    ;; now collect substrings
    (nreverse
     (loop with substr-fn = (if sharedp #'nsubseq #'subseq)
           with string-seen = nil
           for (this-end this-start) on pos-list by #'cddr
           ;; skip empty strings from end of list
           if (or limit
                  (setq string-seen
                          (or string-seen
                              (and this-start
                                   (> this-end this-start)))))
           collect (if this-start
                     (funcall substr-fn
                              target-string this-start this-end)
                     nil)))))

#-:cormanlisp
(define-compiler-macro split (&whole form regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(split (load-time-value (create-scanner ,regex))
                 ,target-string ,@rest))
        (t form)))

(defun string-case-modifier (str from to start end)
  (declare #.*standard-optimize-settings*)
  (declare (fixnum from to start end))
  "Checks whether all words in STR between FROM and TO are upcased,
downcased or capitalized and returns a function which applies a
corresponding case modification to strings.  Returns #'IDENTITY
otherwise, especially if words in the target area extend beyond FROM
or TO.  STR is supposed to be bounded by START and END.  It is assumed
that \(<= START FROM TO END)."
  (case
      (if (or (<= to from)
              (and (< start from)
                   (alphanumericp (char str (1- from)))
                   (alphanumericp (char str from)))
              (and (< to end)
                   (alphanumericp (char str to))
                   (alphanumericp (char str (1- to)))))
        ;; if it's a zero-length string or if words extend beyond FROM
        ;; or TO we return NIL, i.e. #'IDENTITY
        nil
        ;; otherwise we loop through STR from FROM to TO
        (loop with last-char-both-case
              with current-result
              for index of-type fixnum from from below to
              for chr = (char str index)
              do (cond ((not #-:cormanlisp (both-case-p chr)
                             #+:cormanlisp (or (upper-case-p chr)
                                               (lower-case-p chr)))
                         ;; this character doesn't have a case so we
                         ;; consider it as a word boundary (note that
                         ;; this differs from how \b works in Perl)
                         (setq last-char-both-case nil))
                       ((upper-case-p chr)
                         ;; an uppercase character
                         (setq current-result
                                 (if last-char-both-case
                                   ;; not the first character in a 
                                   (case current-result
                                     ((:undecided) :upcase)
                                     ((:downcase :capitalize) (return nil))
                                     ((:upcase) current-result))
                                   (case current-result
                                     ((nil) :undecided)
                                     ((:downcase) (return nil))
                                     ((:capitalize :upcase) current-result)))
                               last-char-both-case t))
                       (t
                         ;; a lowercase character
                         (setq current-result
                                 (case current-result
                                   ((nil) :downcase)
                                   ((:undecided) :capitalize)
                                   ((:downcase) current-result)
                                   ((:capitalize) (if last-char-both-case
                                                    current-result
                                                    (return nil)))
                                   ((:upcase) (return nil)))
                               last-char-both-case t)))
              finally (return current-result)))
    ((nil) #'identity)
    ((:undecided :upcase) #'string-upcase)
    ((:downcase) #'string-downcase)
    ((:capitalize) #'string-capitalize)))

;; first create a scanner to identify the special parts of the
;; replacement string (eat your own dog food...)

(defgeneric build-replacement-template (replacement-string)
  (declare #.*standard-optimize-settings*)
  (:documentation "Converts a replacement string for REGEX-REPLACE or
REGEX-REPLACE-ALL into a replacement template which is an
S-expression."))

#-:cormanlisp
(let* ((*use-bmh-matchers* nil)
       (reg-scanner (create-scanner "\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')")))
  (defmethod build-replacement-template ((replacement-string string))
    (declare #.*standard-optimize-settings*)
    (let ((from 0)
          ;; COLLECTOR will hold the (reversed) template
          (collector '()))
      ;; scan through all special parts of the replacement string
      (do-matches (match-start match-end reg-scanner replacement-string)
        (when (< from match-start)
          ;; strings between matches are copied verbatim
          (push (subseq replacement-string from match-start) collector))
        ;; PARSE-START is true if the pattern matched a number which
        ;; refers to a register
        (let* ((parse-start (position-if #'digit-char-p
                                         replacement-string
                                         :start match-start
                                         :end match-end))
               (token (if parse-start
                        (1- (parse-integer replacement-string
                                           :start parse-start
                                           :junk-allowed t))
                        ;; if we didn't match a number we convert the
                        ;; character to a symbol
                        (case (char replacement-string (1+ match-start))
                          ((#\&) :match)
                          ((#\`) :before-match)
                          ((#\') :after-match)
                          ((#\\) :backslash)))))
          (when (and (numberp token) (< token 0))
            ;; make sure we don't accept something like "\\0"
            (signal-invocation-error "Illegal substring ~S in replacement string."
                                     (subseq replacement-string match-start match-end)))
          (push token collector))
        ;; remember where the match ended
        (setq from match-end))
      (when (< from (length replacement-string))
        ;; push the rest of the replacement string onto the list
        (push (subseq replacement-string from) collector))
      (nreverse collector))))

#-:cormanlisp
(defmethod build-replacement-template ((replacement-function function))
  (declare #.*standard-optimize-settings*)
  (list replacement-function))

#-:cormanlisp
(defmethod build-replacement-template ((replacement-function-symbol symbol))
  (declare #.*standard-optimize-settings*)
  (list replacement-function-symbol))
        
#-:cormanlisp
(defmethod build-replacement-template ((replacement-list list))
  (declare #.*standard-optimize-settings*)
  replacement-list)

;;; Corman Lisp's methods can't be closures... :(
#+:cormanlisp
(let* ((*use-bmh-matchers* nil)
       (reg-scanner (create-scanner "\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')")))
  (defun build-replacement-template (replacement)
    (declare #.*standard-optimize-settings*)
    (typecase replacement
      (string
        (let ((from 0)
              ;; COLLECTOR will hold the (reversed) template
              (collector '()))
          ;; scan through all special parts of the replacement string
          (do-matches (match-start match-end reg-scanner replacement)
            (when (< from match-start)
              ;; strings between matches are copied verbatim
              (push (subseq replacement from match-start) collector))
            ;; PARSE-START is true if the pattern matched a number which
            ;; refers to a register
            (let* ((parse-start (position-if #'digit-char-p
                                             replacement
                                             :start match-start
                                             :end match-end))
                   (token (if parse-start
                            (1- (parse-integer replacement
                                               :start parse-start
                                               :junk-allowed t))
                            ;; if we didn't match a number we convert the
                            ;; character to a symbol
                            (case (char replacement (1+ match-start))
                              ((#\&) :match)
                              ((#\`) :before-match)
                              ((#\') :after-match)
                              ((#\\) :backslash)))))
              (when (and (numberp token) (< token 0))
                ;; make sure we don't accept something like "\\0"
                (signal-invocation-error "Illegal substring ~S in replacement string."
                                         (subseq replacement match-start match-end)))
              (push token collector))
            ;; remember where the match ended
            (setq from match-end))
          (when (< from (length replacement))
            ;; push the rest of the replacement string onto the list
            (push (nsubseq replacement from) collector))
          (nreverse collector)))
      (list
        replacement)
      (t
        (list replacement)))))
        
(defun build-replacement (replacement-template
                          target-string
                          start end
                          match-start match-end
                          reg-starts reg-ends
                          simple-calls
                          element-type)
  (declare #.*standard-optimize-settings*)
  "Accepts a replacement template and the current values from the
matching process in REGEX-REPLACE or REGEX-REPLACE-ALL and returns the
corresponding string."
  ;; the upper exclusive bound of the register numbers in the regular
  ;; expression
  (let ((reg-bound (if reg-starts
                     (array-dimension reg-starts 0)
                     0)))
    (with-output-to-string (s nil :element-type element-type)
      (loop for token in replacement-template
            do (typecase token
                 (string
                   ;; transfer string parts verbatim
                   (write-string token s))
                 (integer
                   ;; replace numbers with the corresponding registers
                   (when (>= token reg-bound)
                     ;; but only if the register was referenced in the
                     ;; regular expression
                     (signal-invocation-error "Reference to non-existent register ~A in replacement string."
                                              (1+ token)))
                   (when (svref reg-starts token)
                     ;; and only if it matched, i.e. no match results
                     ;; in an empty string
                     (write-string target-string s
                                   :start (svref reg-starts token)
                                   :end (svref reg-ends token))))
                 (function
                   (write-string 
                    (cond (simple-calls
                           (apply token
                                  (nsubseq target-string match-start match-end)
                                  (map 'list
                                       (lambda (reg-start reg-end)
                                         (and reg-start
                                              (nsubseq target-string reg-start reg-end)))
                                       reg-starts reg-ends)))
                          (t
                           (funcall token
                                    target-string
                                    start end
                                    match-start match-end
                                    reg-starts reg-ends)))
                    s))
                 (symbol
                   (case token
                     ((:backslash)
                       ;; just a backslash
                       (write-char #\\ s))
                     ((:match)
                       ;; the whole match
                       (write-string target-string s
                                     :start match-start
                                     :end match-end))
                     ((:before-match)
                       ;; the part of the target string before the match
                       (write-string target-string s
                                     :start start
                                     :end match-start))
                     ((:after-match)
                       ;; the part of the target string after the match
                       (write-string target-string s
                                     :start match-end
                                     :end end))
                     (otherwise
                      (write-string
                       (cond (simple-calls
                              (apply token
                                     (nsubseq target-string match-start match-end)
                                     (map 'list
                                          (lambda (reg-start reg-end)
                                            (and reg-start
                                                 (nsubseq target-string reg-start reg-end)))
                                          reg-starts reg-ends)))
                             (t
                              (funcall token
                                       target-string
                                       start end
                                       match-start match-end
                                       reg-starts reg-ends)))
                       s)))))))))

(defun replace-aux (target-string replacement pos-list reg-list start end
                                  preserve-case simple-calls element-type)
  "Auxiliary function used by REGEX-REPLACE and REGEX-REPLACE-ALL.
POS-LIST contains a list with the start and end positions of all
matches while REG-LIST contains a list of arrays representing the
corresponding register start and end positions."
  (declare #.*standard-optimize-settings*)
  ;; build the template once before we start the loop
  (let ((replacement-template (build-replacement-template replacement)))
    (with-output-to-string (s nil :element-type element-type)
      ;; loop through all matches and take the start and end of the
      ;; whole string into account
      (loop for (from to) on (append (list start) pos-list (list end))
            ;; alternate between replacement and no replacement
            for replace = nil then (and (not replace) to)
            for reg-starts = (if replace (pop reg-list) nil)
            for reg-ends = (if replace (pop reg-list) nil)
            for curr-replacement = (if replace
                                     ;; build the replacement string
                                     (build-replacement replacement-template
                                                        target-string
                                                        start end
                                                        from to
                                                        reg-starts reg-ends
                                                        simple-calls
                                                        element-type)
                                     nil)
            while to
            if replace
              do (write-string (if preserve-case
                                 ;; modify the case of the replacement
                                 ;; string if necessary
                                 (funcall (string-case-modifier target-string
                                                                from to
                                                                start end)
                                          curr-replacement)
                                 curr-replacement)
                               s)
            else
              ;; no replacement
              do (write-string target-string s :start from :end to)))))

(defun regex-replace (regex target-string replacement &key
                            (start 0)
                            (end (length target-string))
                            preserve-case
                            simple-calls
                            (element-type #+:lispworks 'lw:simple-char #-:lispworks 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace the first match with REPLACEMENT.  Two values are returned;
the modified string, and T if REGEX matched or NIL otherwise.

  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.

  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)

  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.

  ELEMENT-TYPE is the element type of the resulting string."
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (if match-start
      (values (replace-aux target-string replacement
                           (list match-start match-end)
                           (list reg-starts reg-ends)
                           start end preserve-case
                           simple-calls element-type)
              t)
      (values (subseq target-string start end)
              nil))))

#-:cormanlisp
(define-compiler-macro regex-replace
    (&whole form regex target-string replacement &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(regex-replace (load-time-value (create-scanner ,regex))
                         ,target-string ,replacement ,@rest))
        (t form)))

(defun regex-replace-all (regex target-string replacement &key
                                (start 0)
                                (end (length target-string))
                                preserve-case
                                simple-calls
                                (element-type #+:lispworks 'lw:simple-char #-:lispworks 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace all matches with REPLACEMENT.  Two values are returned; the
modified string, and T if REGEX matched or NIL otherwise.

  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.

  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)

  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.

  ELEMENT-TYPE is the element type of the resulting string."
  (declare #.*standard-optimize-settings*)
  (let ((pos-list '())
        (reg-list '()))
    (do-scans (match-start match-end reg-starts reg-ends regex target-string
                           nil
                           :start start :end end)
      (push match-start pos-list)
      (push match-end pos-list)
      (push reg-starts reg-list)
      (push reg-ends reg-list))
    (if pos-list
      (values (replace-aux target-string replacement
                           (nreverse pos-list)
                           (nreverse reg-list)
                           start end preserve-case
                           simple-calls element-type)
              t)
      (values (subseq target-string start end)
              nil))))

#-:cormanlisp
(define-compiler-macro regex-replace-all
    (&whole form regex target-string replacement &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(regex-replace-all (load-time-value (create-scanner ,regex))
                             ,target-string ,replacement ,@rest))
        (t form)))

#-:cormanlisp
(defmacro regex-apropos-aux ((regex packages case-insensitive &optional return-form)
                             &body body)
  "Auxiliary macro used by REGEX-APROPOS and REGEX-APROPOS-LIST. Loops
through PACKAGES and executes BODY with SYMBOL bound to each symbol
which matches REGEX. Optionally evaluates and returns RETURN-FORM at
the end. If CASE-INSENSITIVE is true and REGEX isn't already a
scanner, a case-insensitive scanner is used."
  (with-rebinding (regex)
    (with-unique-names (scanner %packages next morep hash)
      `(let* ((,scanner (create-scanner ,regex
                                        :case-insensitive-mode
                                        (and ,case-insensitive
                                             (not (functionp ,regex)))))
              (,%packages (or ,packages
                              (list-all-packages)))
              (,hash (make-hash-table :test #'eq)))
         (with-package-iterator (,next ,%packages :external :internal :inherited)
           (loop
             (multiple-value-bind (,morep symbol)
                 (,next)
               (unless ,morep
                 (return ,return-form))
               (unless (gethash symbol ,hash)
                 (when (scan ,scanner (symbol-name symbol))
                   (setf (gethash symbol ,hash) t)
                   ,@body)))))))))

;;; The following two functions were provided by Karsten Poeck

#+:cormanlisp
(defmacro do-with-all-symbols ((variable package-or-packagelist) &body body)
  "Executes BODY with VARIABLE bound to each symbol in
PACKAGE-OR-PACKAGELIST \(a designator for a list of packages) in
turn."
  (with-unique-names (pack-var)
    `(if (listp ,package-or-packagelist)
      (dolist (,pack-var ,package-or-packagelist)
        (do-symbols (,variable ,pack-var)
          ,@body))
      (do-symbols (,variable ,package-or-packagelist)
        ,@body))))

#+:cormanlisp
(defmacro regex-apropos-aux ((regex packages case-insensitive &optional return-form)
                             &body body)
  "Auxiliary macro used by REGEX-APROPOS and REGEX-APROPOS-LIST.
Loops through PACKAGES and executes BODY with SYMBOL bound to each
symbol which matches REGEX.  Optionally evaluates and returns
RETURN-FORM at the end.  If CASE-INSENSITIVE is true and REGEX isn't
already a scanner, a case-insensitive scanner is used."
  (with-rebinding (regex)
    (with-unique-names (scanner %packages hash)
      `(let* ((,scanner (create-scanner ,regex
                         :case-insensitive-mode
                         (and ,case-insensitive
                              (not (functionp ,regex)))))
              (,%packages (or ,packages
                             (list-all-packages)))
              (,hash (make-hash-table :test #'eq)))
        (do-with-all-symbols (symbol ,%packages)
          (unless (gethash symbol ,hash)
            (when (scan ,scanner (symbol-name symbol))
              (setf (gethash symbol ,hash) t)
              ,@body)))
        ,return-form))))

(defun regex-apropos-list (regex &optional packages &key (case-insensitive t))
  (declare #.*standard-optimize-settings*)
  "Similar to the standard function APROPOS-LIST but returns a list of
all symbols which match the regular expression REGEX.  If
CASE-INSENSITIVE is true and REGEX isn't already a scanner, a
case-insensitive scanner is used."
  (let ((collector '()))
    (regex-apropos-aux (regex packages case-insensitive collector)
      (push symbol collector))))

(defun print-symbol-info (symbol)
  "Auxiliary function used by REGEX-APROPOS. Tries to print some
meaningful information about a symbol."
  (declare #.*standard-optimize-settings*)
  (handler-case
    (let ((output-list '()))
      (cond ((special-operator-p symbol)
              (push "[special operator]" output-list))
            ((macro-function symbol)
              (push "[macro]" output-list))
            ((fboundp symbol)
              (let* ((function (symbol-function symbol))
                     (compiledp (compiled-function-p function)))
                (multiple-value-bind (lambda-expr closurep)
                    (function-lambda-expression function)
                  (push
                    (format nil
                            "[~:[~;compiled ~]~:[function~;closure~]]~:[~; ~A~]"
                            compiledp closurep lambda-expr (cadr lambda-expr))
                    output-list)))))
      (let ((class (find-class symbol nil)))
        (when class
          (push (format nil "[class] ~S" class) output-list)))
      (cond ((keywordp symbol)
              (push "[keyword]" output-list))
            ((constantp symbol)
              (push (format nil "[constant]~:[~; value: ~S~]"
                            (boundp symbol) (symbol-value symbol)) output-list))
            ((boundp symbol)
              (push #+(or :lispworks :clisp) "[variable]"
                    #-(or :lispworks :clisp) (format nil "[variable] value: ~S"
                                                   (symbol-value symbol))
                    output-list)))
      #-(or :cormanlisp :clisp)
      (format t "~&~S ~<~;~^~A~@{~:@_~A~}~;~:>" symbol output-list)
      #+(or :cormanlisp :clisp)
      (loop for line in output-list
            do (format t "~&~S ~A" symbol line)))
    (condition ()
      ;; this seems to be necessary due to some errors I encountered
      ;; with LispWorks
      (format t "~&~S [an error occurred while trying to print more info]" symbol))))

(defun regex-apropos (regex &optional packages &key (case-insensitive t))
  "Similar to the standard function APROPOS but returns a list of all
symbols which match the regular expression REGEX.  If CASE-INSENSITIVE
is true and REGEX isn't already a scanner, a case-insensitive scanner
is used."
  (declare #.*standard-optimize-settings*)
  (regex-apropos-aux (regex packages case-insensitive)
    (print-symbol-info symbol))
  (values))

(let* ((*use-bmh-matchers* nil)
       (non-word-char-scanner (create-scanner "[^a-zA-Z_0-9]")))
  (defun quote-meta-chars (string &key (start 0) (end (length string)))
    "Quote, i.e. prefix with #\\\\, all non-word characters in STRING."
    (regex-replace-all non-word-char-scanner string "\\\\\\&"
                       :start start :end end)))

(let* ((*use-bmh-matchers* nil)
       (*allow-quoting* nil)
       (quote-char-scanner (create-scanner "\\\\Q"))
       (section-scanner (create-scanner "\\\\Q((?:[^\\\\]|\\\\(?!Q))*?)(?:\\\\E|$)")))
  (defun quote-sections (string)
    "Replace sections inside of STRING which are enclosed by \\Q and
\\E with the quoted equivalent of these sections \(see
QUOTE-META-CHARS). Repeat this as long as there are such
sections. These sections may nest."
    (flet ((quote-substring (target-string start end match-start
                                           match-end reg-starts reg-ends)
             (declare (ignore start end match-start match-end))
             (quote-meta-chars target-string
                               :start (svref reg-starts 0)
                               :end (svref reg-ends 0))))
      (loop for result = string then (regex-replace-all section-scanner
                                                        result
                                                        #'quote-substring)
            while (scan quote-char-scanner result)
            finally (return result)))))

(let* ((*use-bmh-matchers* nil)
       (comment-scanner (create-scanner "(?s)\\(\\?#.*?\\)"))
       (extended-comment-scanner (create-scanner "(?m:#.*?$)|(?s:\\(\\?#.*?\\))"))
       (quote-token-scanner (create-scanner "\\\\[QE]"))
       (quote-token-replace-scanner (create-scanner "\\\\([QE])")))
  (defun clean-comments (string &optional extended-mode)
    "Clean \(?#...) comments within STRING for quoting, i.e. convert
\\Q to Q and \\E to E.  If EXTENDED-MODE is true, also clean
end-of-line comments, i.e. those starting with #\\# and ending with
#\\Newline."
    (flet ((remove-tokens (target-string start end match-start
                                         match-end reg-starts reg-ends)
             (declare (ignore start end reg-starts reg-ends))
             (loop for result = (nsubseq target-string match-start match-end)
                   then (regex-replace-all quote-token-replace-scanner result "\\1")
                   ;; we must probably repeat this because the comment
                   ;; can contain substrings like \\Q
                   while (scan quote-token-scanner result)
                   finally (return result))))
      (regex-replace-all (if extended-mode
                           extended-comment-scanner
                           comment-scanner)
                         string
                         #'remove-tokens))))

(defun parse-tree-synonym (symbol)
  "Returns the parse tree the SYMBOL symbol is a synonym for.  Returns
NIL is SYMBOL wasn't yet defined to be a synonym."
  (get symbol 'parse-tree-synonym))

(defun (setf parse-tree-synonym) (new-parse-tree symbol)
  "Defines SYMBOL to be a synonm for the parse tree NEW-PARSE-TREE."
  (setf (get symbol 'parse-tree-synonym) new-parse-tree))

(defmacro define-parse-tree-synonym (name parse-tree)
  "Defines the symbol NAME to be a synonym for the parse tree
PARSE-TREE.  Both arguments are quoted."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (parse-tree-synonym ',name) ',parse-tree)))
