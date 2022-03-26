;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/convert.lisp,v 1.57 2009/09/17 19:17:31 edi Exp $

;;; Here the parse tree is converted into its internal representation
;;; using REGEX objects.  At the same time some optimizations are
;;; already applied.

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

;;; The flags that represent the "ism" modifiers are always kept
;;; together in a three-element list. We use the following macros to
;;; access individual elements.

(defmacro case-insensitive-mode-p (flags)
  "Accessor macro to extract the first flag out of a three-element flag list."
  `(first ,flags))

(defmacro multi-line-mode-p (flags)
  "Accessor macro to extract the second flag out of a three-element flag list."
  `(second ,flags))

(defmacro single-line-mode-p (flags)
  "Accessor macro to extract the third flag out of a three-element flag list."
  `(third ,flags))

(defun set-flag (token)
  "Reads a flag token and sets or unsets the corresponding entry in
the special FLAGS list."
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  (case token
    ((:case-insensitive-p)
      (setf (case-insensitive-mode-p flags) t))
    ((:case-sensitive-p)
      (setf (case-insensitive-mode-p flags) nil))
    ((:multi-line-mode-p)
      (setf (multi-line-mode-p flags) t))
    ((:not-multi-line-mode-p)
      (setf (multi-line-mode-p flags) nil))
    ((:single-line-mode-p)
      (setf (single-line-mode-p flags) t))
    ((:not-single-line-mode-p)
      (setf (single-line-mode-p flags) nil))
    (otherwise
      (signal-syntax-error "Unknown flag token ~A." token))))

(defgeneric resolve-property (property)
  (:documentation "Resolves PROPERTY to a unary character test
function.  PROPERTY can either be a function designator or it can be a
string which is resolved using *PROPERTY-RESOLVER*.")
  (:method ((property-name string))
   (funcall *property-resolver* property-name))
  (:method ((function-name symbol))
   function-name)
  (:method ((test-function function))
   test-function))

(defun convert-char-class-to-test-function (list invertedp case-insensitive-p)
  "Combines all items in LIST into test function and returns a
logical-OR combination of these functions.  Items can be single
characters, character ranges like \(:RANGE #\\A #\\E), or special
character classes like :DIGIT-CLASS.  Does the right thing with
respect to case-\(in)sensitivity as specified by the special variable
FLAGS."
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  (let ((test-functions
         (loop for item in list
               collect (cond ((characterp item)
                              ;; rebind so closure captures the right one
                              (let ((this-char item))
                                (lambda (char)
                                  (declare (character char this-char))
                                  (char= char this-char))))
                             ((symbolp item)
                              (case item
                                ((:digit-class) #'digit-char-p)
                                ((:non-digit-class) (complement* #'digit-char-p))
                                ((:whitespace-char-class) #'whitespacep)
                                ((:non-whitespace-char-class) (complement* #'whitespacep))
                                ((:word-char-class) #'word-char-p)
                                ((:non-word-char-class) (complement* #'word-char-p))
                                (otherwise
                                 (signal-syntax-error "Unknown symbol ~A in character class." item))))
                             ((and (consp item)
                                   (eq (first item) :property))
                              (resolve-property (second item)))
                             ((and (consp item)
                                   (eq (first item) :inverted-property))
                              (complement* (resolve-property (second item))))
                             ((and (consp item)
                                   (eq (first item) :range))
                              (let ((from (second item))
                                    (to (third item)))
                                (when (char> from to)
                                  (signal-syntax-error "Invalid range from ~S to ~S in char-class." from to))
                                (lambda (char)
                                  (declare (character char from to))
                                  (char<= from char to))))
                             (t (signal-syntax-error "Unknown item ~A in char-class list." item))))))
    (unless test-functions
      (signal-syntax-error "Empty character class."))
    (cond ((cdr test-functions)           
           (cond ((and invertedp case-insensitive-p)
                  (lambda (char)
                    (declare (character char))
                    (loop with both-case-p = (both-case-p char)
                          with char-down = (if both-case-p (char-downcase char) char)
                          with char-up = (if both-case-p (char-upcase char) nil)
                          for test-function in test-functions
                          never (or (funcall test-function char-down)
                                    (and char-up (funcall test-function char-up))))))
                 (case-insensitive-p
                  (lambda (char)
                    (declare (character char))
                    (loop with both-case-p = (both-case-p char)
                          with char-down = (if both-case-p (char-downcase char) char)
                          with char-up = (if both-case-p (char-upcase char) nil)
                          for test-function in test-functions
                          thereis (or (funcall test-function char-down)
                                      (and char-up (funcall test-function char-up))))))
                 (invertedp
                  (lambda (char)
                    (loop for test-function in test-functions
                          never (funcall test-function char))))
                 (t
                  (lambda (char)
                    (loop for test-function in test-functions
                          thereis (funcall test-function char))))))
          ;; there's only one test-function
          (t (let ((test-function (first test-functions)))
               (cond ((and invertedp case-insensitive-p)
                      (lambda (char)
                        (declare (character char))
                        (not (or (funcall test-function (char-downcase char))
                                 (and (both-case-p char)
                                      (funcall test-function (char-upcase char)))))))
                     (case-insensitive-p
                      (lambda (char)
                        (declare (character char))
                        (or (funcall test-function (char-downcase char))
                            (and (both-case-p char)
                                 (funcall test-function (char-upcase char))))))
                     (invertedp (complement* test-function))
                     (t test-function)))))))

(defun maybe-split-repetition (regex
                               greedyp
                               minimum
                               maximum
                               min-len
                               length
                               reg-seen)
  "Splits a REPETITION object into a constant and a varying part if
applicable, i.e. something like
  a{3,} -> a{3}a*
The arguments to this function correspond to the REPETITION slots of
the same name."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum minimum)
           (type (or fixnum null) maximum))
  ;; note the usage of COPY-REGEX here; we can't use the same REGEX
  ;; object in both REPETITIONS because they will have different
  ;; offsets
  (when maximum
    (when (zerop maximum)
      ;; trivial case: don't repeat at all
      (return-from maybe-split-repetition
        (make-instance 'void)))
    (when (= 1 minimum maximum)
      ;; another trivial case: "repeat" exactly once
      (return-from maybe-split-repetition
        regex)))
  ;; first set up the constant part of the repetition
  ;; maybe that's all we need
  (let ((constant-repetition (if (plusp minimum)
                               (make-instance 'repetition
                                              :regex (copy-regex regex)
                                              :greedyp greedyp
                                              :minimum minimum
                                              :maximum minimum
                                              :min-len min-len
                                              :len length
                                              :contains-register-p reg-seen)
                               ;; don't create garbage if minimum is 0
                               nil)))
    (when (and maximum
               (= maximum minimum))
      (return-from maybe-split-repetition
        ;; no varying part needed because min = max
        constant-repetition))
    ;; now construct the varying part
    (let ((varying-repetition
            (make-instance 'repetition
                           :regex regex
                           :greedyp greedyp
                           :minimum 0
                           :maximum (if maximum (- maximum minimum) nil)
                           :min-len min-len
                           :len length
                           :contains-register-p reg-seen)))
      (cond ((zerop minimum)
              ;; min = 0, no constant part needed
              varying-repetition)
            ((= 1 minimum)
              ;; min = 1, constant part needs no REPETITION wrapped around
              (make-instance 'seq
                             :elements (list (copy-regex regex)
                                             varying-repetition)))
            (t
              ;; general case
              (make-instance 'seq
                             :elements (list constant-repetition
                                             varying-repetition)))))))

;; During the conversion of the parse tree we keep track of the start
;; of the parse tree in the special variable STARTS-WITH which'll
;; either hold a STR object or an EVERYTHING object. The latter is the
;; case if the regex starts with ".*" which implicitly anchors the
;; regex at the start (perhaps modulo #\Newline).

(defun maybe-accumulate (str)
  "Accumulate STR into the special variable STARTS-WITH if
ACCUMULATE-START-P (also special) is true and STARTS-WITH is either
NIL or a STR object of the same case mode. Always returns NIL."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p starts-with))
  (declare (ftype (function (t) fixnum) len))
  (when accumulate-start-p
    (etypecase starts-with
      (str
        ;; STARTS-WITH already holds a STR, so we check if we can
        ;; concatenate
        (cond ((eq (case-insensitive-p starts-with)
                   (case-insensitive-p str))
                ;; we modify STARTS-WITH in place
                (setf (len starts-with)
                        (+ (len starts-with) (len str)))
                ;; note that we use SLOT-VALUE because the accessor
                ;; STR has a declared FTYPE which doesn't fit here
                (adjust-array (slot-value starts-with 'str)
                              (len starts-with)
                              :fill-pointer t)
                (setf (subseq (slot-value starts-with 'str)
                              (- (len starts-with) (len str)))
                        (str str)
                      ;; STR objects that are parts of STARTS-WITH
                      ;; always have their SKIP slot set to true
                      ;; because the SCAN function will take care of
                      ;; them, i.e. the matcher can ignore them
                      (skip str) t))
              (t (setq accumulate-start-p nil))))
      (null
        ;; STARTS-WITH is still empty, so we create a new STR object
        (setf starts-with
                (make-instance 'str
                               :str ""
                               :case-insensitive-p (case-insensitive-p str))
              ;; INITIALIZE-INSTANCE will coerce the STR to a simple
              ;; string, so we have to fill it afterwards
              (slot-value starts-with 'str)
                (make-array (len str)
                            :initial-contents (str str)
                            :element-type 'character
                            :fill-pointer t
                            :adjustable t)
              (len starts-with)
                (len str)
              ;; see remark about SKIP above
              (skip str) t))
      (everything
        ;; STARTS-WITH already holds an EVERYTHING object - we can't
        ;; concatenate
        (setq accumulate-start-p nil))))
  nil)

(declaim (inline convert-aux))
(defun convert-aux (parse-tree)
  "Converts the parse tree PARSE-TREE into a REGEX object and returns
it.  Will also

  - split and optimize repetitions,
  - accumulate strings or EVERYTHING objects into the special variable
    STARTS-WITH,
  - keep track of all registers seen in the special variable REG-NUM,
  - keep track of all named registers seen in the special variable REG-NAMES
  - keep track of the highest backreference seen in the special
    variable MAX-BACK-REF,
  - maintain and adher to the currently applicable modifiers in the special
    variable FLAGS, and
  - maybe even wash your car..."
  (declare #.*standard-optimize-settings*)
  (if (consp parse-tree)
    (convert-compound-parse-tree (first parse-tree) parse-tree)
    (convert-simple-parse-tree parse-tree)))

(defgeneric convert-compound-parse-tree (token parse-tree &key)
  (declare #.*standard-optimize-settings*)
  (:documentation "Helper function for CONVERT-AUX which converts
parse trees which are conses and dispatches on TOKEN which is the
first element of the parse tree.")
  (:method ((token t) (parse-tree t) &key)
   (signal-syntax-error "Unknown token ~A in parse-tree." token)))

(defmethod convert-compound-parse-tree ((token (eql :sequence)) parse-tree &key)
  "The case for parse trees like \(:SEQUENCE {<regex>}*)."
  (declare #.*standard-optimize-settings*)
  (cond ((cddr parse-tree)
         ;; this is essentially like
         ;; (MAPCAR 'CONVERT-AUX (REST PARSE-TREE))
         ;; but we don't cons a new list
         (loop for parse-tree-rest on (rest parse-tree)
               while parse-tree-rest
               do (setf (car parse-tree-rest)
                        (convert-aux (car parse-tree-rest))))
         (make-instance 'seq :elements (rest parse-tree)))
        (t (convert-aux (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :group)) parse-tree &key)
  "The case for parse trees like \(:GROUP {<regex>}*).

This is a syntactical construct equivalent to :SEQUENCE intended to
keep the effect of modifiers local."
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  ;; make a local copy of FLAGS and shadow the global value while we
  ;; descend into the enclosed regexes
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (cond ((cddr parse-tree)
           (loop for parse-tree-rest on (rest parse-tree)
                 while parse-tree-rest
                 do (setf (car parse-tree-rest)
                          (convert-aux (car parse-tree-rest))))
           (make-instance 'seq :elements (rest parse-tree)))
          (t (convert-aux (second parse-tree))))))

(defmethod convert-compound-parse-tree ((token (eql :alternation)) parse-tree &key)
  "The case for \(:ALTERNATION {<regex>}*)."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  ;; we must stop accumulating objects into STARTS-WITH once we reach
  ;; an alternation
  (setq accumulate-start-p nil)
  (loop for parse-tree-rest on (rest parse-tree)
        while parse-tree-rest
        do (setf (car parse-tree-rest)
                 (convert-aux (car parse-tree-rest))))
  (make-instance 'alternation :choices (rest parse-tree)))

(defmethod convert-compound-parse-tree ((token (eql :branch)) parse-tree &key)
  "The case for \(:BRANCH <test> <regex>).

Here, <test> must be look-ahead, look-behind or number; if <regex> is
an alternation it must have one or two choices."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (let* ((test-candidate (second parse-tree))
         (test (cond ((numberp test-candidate)
                      (when (zerop (the fixnum test-candidate))
                        (signal-syntax-error "Register 0 doesn't exist: ~S." parse-tree))
                      (1- (the fixnum test-candidate)))
                     (t (convert-aux test-candidate))))
         (alternations (convert-aux (third parse-tree))))
    (when (and (not (numberp test))
               (not (typep test 'lookahead))
               (not (typep test 'lookbehind)))
      (signal-syntax-error "Branch test must be look-ahead, look-behind or number: ~S." parse-tree))
    (typecase alternations
      (alternation
       (case (length (choices alternations))
         ((0)
          (signal-syntax-error "No choices in branch: ~S." parse-tree))
         ((1)
          (make-instance 'branch
                         :test test
                         :then-regex (first
                                      (choices alternations))))
         ((2)
          (make-instance 'branch
                         :test test
                         :then-regex (first
                                      (choices alternations))
                         :else-regex (second
                                      (choices alternations))))
         (otherwise
          (signal-syntax-error "Too much choices in branch: ~S." parse-tree))))
      (t
       (make-instance 'branch
                      :test test
                      :then-regex alternations)))))

(defmethod convert-compound-parse-tree ((token (eql :positive-lookahead)) parse-tree &key)
  "The case for \(:POSITIVE-LOOKAHEAD <regex>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  ;; keep the effect of modifiers local to the enclosed regex and stop
  ;; accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (make-instance 'lookahead
                   :regex (convert-aux (second parse-tree))
                   :positivep t)))

(defmethod convert-compound-parse-tree ((token (eql :negative-lookahead)) parse-tree &key)
  "The case for \(:NEGATIVE-LOOKAHEAD <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; do the same as for positive look-aheads and just switch afterwards
  (let ((regex (convert-compound-parse-tree :positive-lookahead parse-tree)))
    (setf (slot-value regex 'positivep) nil)
    regex))

(defmethod convert-compound-parse-tree ((token (eql :positive-lookbehind)) parse-tree &key)
  "The case for \(:POSITIVE-LOOKBEHIND <regex>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  ;; keep the effect of modifiers local to the enclosed regex and stop
  ;; accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  (let* ((flags (copy-list flags))
         (regex (convert-aux (second parse-tree)))
         (len (regex-length regex)))
    (declare (special flags))
    ;; lookbehind assertions must be of fixed length
    (unless len
      (signal-syntax-error "Variable length look-behind not implemented \(yet): ~S." parse-tree))
    (make-instance 'lookbehind
                   :regex regex
                   :positivep t
                   :len len)))

(defmethod convert-compound-parse-tree ((token (eql :negative-lookbehind)) parse-tree &key)
  "The case for \(:NEGATIVE-LOOKBEHIND <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; do the same as for positive look-behinds and just switch afterwards
  (let ((regex (convert-compound-parse-tree :positive-lookbehind parse-tree)))
    (setf (slot-value regex 'positivep) nil)
    regex))

(defmethod convert-compound-parse-tree ((token (eql :greedy-repetition)) parse-tree &key (greedyp t))
  "The case for \(:GREEDY-REPETITION|:NON-GREEDY-REPETITION <min> <max> <regex>).

This function is also used for the non-greedy case in which case it is
called with GREEDYP set to NIL as you would expect."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p starts-with))
  ;; remember the value of ACCUMULATE-START-P upon entering
  (let ((local-accumulate-start-p accumulate-start-p))
    (let ((minimum (second parse-tree))
          (maximum (third parse-tree)))
      (declare (fixnum minimum))
      (declare (type (or null fixnum) maximum))
      (unless (and maximum
                   (= 1 minimum maximum))
        ;; set ACCUMULATE-START-P to NIL for the rest of
        ;; the conversion because we can't continue to
        ;; accumulate inside as well as after a proper
        ;; repetition
        (setq accumulate-start-p nil))
      (let* (reg-seen
             (regex (convert-aux (fourth parse-tree)))
             (min-len (regex-min-length regex))
             (length (regex-length regex)))
        ;; note that this declaration already applies to
        ;; the call to CONVERT-AUX above
        (declare (special reg-seen))
        (when (and local-accumulate-start-p
                   (not starts-with)
                   (zerop minimum)
                   (not maximum))
          ;; if this repetition is (equivalent to) ".*"
          ;; and if we're at the start of the regex we
          ;; remember it for ADVANCE-FN (see the SCAN
          ;; function)
          (setq starts-with (everythingp regex)))
        (if (or (not reg-seen)
                (not greedyp)
                (not length)
                (zerop length)
                (and maximum (= minimum maximum)))
          ;; the repetition doesn't enclose a register, or
          ;; it's not greedy, or we can't determine it's
          ;; (inner) length, or the length is zero, or the
          ;; number of repetitions is fixed; in all of
          ;; these cases we don't bother to optimize
          (maybe-split-repetition regex
                                  greedyp
                                  minimum
                                  maximum
                                  min-len
                                  length
                                  reg-seen)
          ;; otherwise we make a transformation that looks
          ;; roughly like one of
          ;;   <regex>* -> (?:<regex'>*<regex>)?
          ;;   <regex>+ -> <regex'>*<regex>
          ;; where the trick is that as much as possible
          ;; registers from <regex> are removed in
          ;; <regex'>
          (let* (reg-seen ; new instance for REMOVE-REGISTERS
                 (remove-registers-p t)
                 (inner-regex (remove-registers regex))
                 (inner-repetition
                  ;; this is the "<regex'>" part
                  (maybe-split-repetition inner-regex
                                          ;; always greedy
                                          t
                                          ;; reduce minimum by 1
                                          ;; unless it's already 0
                                          (if (zerop minimum)
                                            0
                                            (1- minimum))
                                          ;; reduce maximum by 1
                                          ;; unless it's NIL
                                          (and maximum
                                               (1- maximum))
                                          min-len
                                          length
                                          reg-seen))
                 (inner-seq
                  ;; this is the "<regex'>*<regex>" part
                  (make-instance 'seq
                                 :elements (list inner-repetition
                                                 regex))))
            ;; note that this declaration already applies
            ;; to the call to REMOVE-REGISTERS above
            (declare (special remove-registers-p reg-seen))
            ;; wrap INNER-SEQ with a greedy
            ;; {0,1}-repetition (i.e. "?") if necessary
            (if (plusp minimum)
              inner-seq
              (maybe-split-repetition inner-seq
                                      t
                                      0
                                      1
                                      min-len
                                      nil
                                      t))))))))

(defmethod convert-compound-parse-tree ((token (eql :non-greedy-repetition)) parse-tree &key)
  "The case for \(:NON-GREEDY-REPETITION <min> <max> <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; just dispatch to the method above with GREEDYP explicitly set to NIL
  (convert-compound-parse-tree :greedy-repetition parse-tree :greedyp nil))

(defmethod convert-compound-parse-tree ((token (eql :register)) parse-tree &key name)
  "The case for \(:REGISTER <regex>).  Also used for named registers
when NAME is not NIL."
  (declare #.*standard-optimize-settings*)
  (declare (special flags reg-num reg-names))
  ;; keep the effect of modifiers local to the enclosed regex; also,
  ;; assign the current value of REG-NUM to the corresponding slot of
  ;; the REGISTER object and increase this counter afterwards; for
  ;; named register update REG-NAMES and set the corresponding name
  ;; slot of the REGISTER object too
  (let ((flags (copy-list flags))
        (stored-reg-num reg-num))
    (declare (special flags reg-seen named-reg-seen))
    (setq reg-seen t)
    (when name (setq named-reg-seen t))
    (incf (the fixnum reg-num))
    (push name reg-names)
    (make-instance 'register
                   :regex (convert-aux (if name (third parse-tree) (second parse-tree)))
                   :num stored-reg-num
                   :name name)))

(defmethod convert-compound-parse-tree ((token (eql :named-register)) parse-tree &key)
  "The case for \(:NAMED-REGISTER <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; call the method above and use the :NAME keyword argument
  (convert-compound-parse-tree :register parse-tree :name (copy-seq (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :filter)) parse-tree &key)
  "The case for \(:FILTER <function> &optional <length>)."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  ;; stop accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  (make-instance 'filter
                 :fn (second parse-tree)
                 :len (third parse-tree)))

(defmethod convert-compound-parse-tree ((token (eql :standalone)) parse-tree &key)
  "The case for \(:STANDALONE <regex>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  ;; stop accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  ;; keep the effect of modifiers local to the enclosed regex
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (make-instance 'standalone :regex (convert-aux (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :back-reference)) parse-tree &key)
  "The case for \(:BACK-REFERENCE <number>|<name>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p reg-num reg-names max-back-ref))
  (let* ((backref-name (and (stringp (second parse-tree))
                            (second parse-tree)))
         (referred-regs
          (when backref-name
            ;; find which register corresponds to the given name
            ;; we have to deal with case where several registers share
            ;; the same name and collect their respective numbers
            (loop for name in reg-names
                  for reg-index from 0
                  when (string= name backref-name)
                  ;; NOTE: REG-NAMES stores register names in reversed
                  ;; order REG-NUM contains number of (any) registers
                  ;; seen so far; 1- will be done later
                  collect (- reg-num reg-index))))
         ;; store the register number for the simple case
         (backref-number (or (first referred-regs) (second parse-tree))))
    (declare (type (or fixnum null) backref-number))
    (when (or (not (typep backref-number 'fixnum))
              (<= backref-number 0))
      (signal-syntax-error "Illegal back-reference: ~S." parse-tree))
    ;; stop accumulating into STARTS-WITH and increase MAX-BACK-REF if
    ;; necessary
    (setq accumulate-start-p nil
          max-back-ref (max (the fixnum max-back-ref)
                            backref-number))
    (flet ((make-back-ref (backref-number)
             (make-instance 'back-reference
                            ;; we start counting from 0 internally
                            :num (1- backref-number)
                            :case-insensitive-p (case-insensitive-mode-p flags)
                            ;; backref-name is NIL or string, safe to copy
                            :name (copy-seq backref-name))))
      (cond
       ((cdr referred-regs)
        ;; several registers share the same name we will try to match
        ;; any of them, starting with the most recent first
        ;; alternation is used to accomplish matching
        (make-instance 'alternation
                       :choices (loop
                                 for reg-index in referred-regs
                                 collect (make-back-ref reg-index))))
       ;; simple case - backref corresponds to only one register
       (t
        (make-back-ref backref-number))))))

(defmethod convert-compound-parse-tree ((token (eql :regex)) parse-tree &key)
  "The case for \(:REGEX <string>)."
  (declare #.*standard-optimize-settings*)
  (convert-aux (parse-string (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :char-class)) parse-tree &key invertedp)
  "The case for \(:CHAR-CLASS {<item>}*) where item is one of

- a character,
- a character range: \(:RANGE <char1> <char2>), or
- a special char class symbol like :DIGIT-CHAR-CLASS.

Also used for inverted char classes when INVERTEDP is true."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  (let ((test-function
         (create-optimized-test-function
          (convert-char-class-to-test-function (rest parse-tree)
                                               invertedp
                                               (case-insensitive-mode-p flags)))))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function test-function)))

(defmethod convert-compound-parse-tree ((token (eql :inverted-char-class)) parse-tree &key)
  "The case for \(:INVERTED-CHAR-CLASS {<item>}*)."
  (declare #.*standard-optimize-settings*)
  ;; just dispatch to the "real" method
  (convert-compound-parse-tree :char-class parse-tree :invertedp t))

(defmethod convert-compound-parse-tree ((token (eql :property)) parse-tree &key)
  "The case for \(:PROPERTY <name>) where <name> is a string."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (make-instance 'char-class :test-function (resolve-property (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :inverted-property)) parse-tree &key)
  "The case for \(:INVERTED-PROPERTY <name>) where <name> is a string."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (make-instance 'char-class :test-function (complement* (resolve-property (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :flags)) parse-tree &key)
  "The case for \(:FLAGS {<flag>}*) where flag is a modifier symbol
like :CASE-INSENSITIVE-P."
  (declare #.*standard-optimize-settings*)
  ;; set/unset the flags corresponding to the symbols
  ;; following :FLAGS
  (mapc #'set-flag (rest parse-tree))
  ;; we're only interested in the side effect of
  ;; setting/unsetting the flags and turn this syntactical
  ;; construct into a VOID object which'll be optimized
  ;; away when creating the matcher
  (make-instance 'void))

(defgeneric convert-simple-parse-tree (parse-tree)
  (declare #.*standard-optimize-settings*)
  (:documentation "Helper function for CONVERT-AUX which converts
parse trees which are atoms.")
  (:method ((parse-tree (eql :void)))
   (declare #.*standard-optimize-settings*)
   (make-instance 'void))
  (:method ((parse-tree (eql :word-boundary)))
   (declare #.*standard-optimize-settings*)
   (make-instance 'word-boundary :negatedp nil))
  (:method ((parse-tree (eql :non-word-boundary)))
   (declare #.*standard-optimize-settings*)
   (make-instance 'word-boundary :negatedp t))
  (:method ((parse-tree (eql :everything)))
   (declare #.*standard-optimize-settings*)
   (declare (special flags accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'everything :single-line-p (single-line-mode-p flags)))
  (:method ((parse-tree (eql :digit-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function #'digit-char-p))
  (:method ((parse-tree (eql :word-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function #'word-char-p))
  (:method ((parse-tree (eql :whitespace-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function #'whitespacep))
  (:method ((parse-tree (eql :non-digit-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function (complement* #'digit-char-p)))
  (:method ((parse-tree (eql :non-word-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function (complement* #'word-char-p)))
  (:method ((parse-tree (eql :non-whitespace-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function (complement* #'whitespacep)))
  (:method ((parse-tree (eql :start-anchor)))
   ;; Perl's "^"
   (declare #.*standard-optimize-settings*)
   (declare (special flags))
   (make-instance 'anchor :startp t :multi-line-p (multi-line-mode-p flags)))
  (:method ((parse-tree (eql :end-anchor)))
   ;; Perl's "$"
   (declare #.*standard-optimize-settings*)
   (declare (special flags))
   (make-instance 'anchor :startp nil :multi-line-p (multi-line-mode-p flags)))
  (:method ((parse-tree (eql :modeless-start-anchor)))
   ;; Perl's "\A"
   (declare #.*standard-optimize-settings*)
   (make-instance 'anchor :startp t))
  (:method ((parse-tree (eql :modeless-end-anchor)))
   ;; Perl's "$\Z"
   (declare #.*standard-optimize-settings*)
   (make-instance 'anchor :startp nil))
  (:method ((parse-tree (eql :modeless-end-anchor-no-newline)))
   ;; Perl's "$\z"
   (declare #.*standard-optimize-settings*)
   (make-instance 'anchor :startp nil :no-newline-p t))
  (:method ((parse-tree (eql :case-insensitive-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :case-sensitive-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :multi-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :not-multi-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :single-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :not-single-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void)))

(defmethod convert-simple-parse-tree ((parse-tree string))
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  ;; turn strings into STR objects and try to accumulate into
  ;; STARTS-WITH
  (let ((str (make-instance 'str
                            :str parse-tree
                            :case-insensitive-p (case-insensitive-mode-p flags))))
    (maybe-accumulate str)
    str))

(defmethod convert-simple-parse-tree ((parse-tree character))
  (declare #.*standard-optimize-settings*)
  ;; dispatch to the method for strings
  (convert-simple-parse-tree (string parse-tree)))
        
(defmethod convert-simple-parse-tree (parse-tree)
  "The default method - check if there's a translation."
  (declare #.*standard-optimize-settings*)
  (let ((translation (and (symbolp parse-tree) (parse-tree-synonym parse-tree))))
    (if translation
      (convert-aux (copy-tree translation))
      (signal-syntax-error "Unknown token ~A in parse tree." parse-tree))))

(defun convert (parse-tree)
  "Converts the parse tree PARSE-TREE into an equivalent REGEX object
and returns three values: the REGEX object, the number of registers
seen and an object the regex starts with which is either a STR object
or an EVERYTHING object \(if the regex starts with something like
\".*\") or NIL."
  (declare #.*standard-optimize-settings*)
  ;; this function basically just initializes the special variables
  ;; and then calls CONVERT-AUX to do all the work
  (let* ((flags (list nil nil nil))
         (reg-num 0)
         reg-names
         named-reg-seen
         (accumulate-start-p t)
         starts-with
         (max-back-ref 0)
         (converted-parse-tree (convert-aux parse-tree)))
    (declare (special flags reg-num reg-names named-reg-seen
                      accumulate-start-p starts-with max-back-ref))
    ;; make sure we don't reference registers which aren't there
    (when (> (the fixnum max-back-ref)
             (the fixnum reg-num))
      (signal-syntax-error "Backreference to register ~A which has not been defined." max-back-ref))
    (when (typep starts-with 'str)
      (setf (slot-value starts-with 'str)
              (coerce (slot-value starts-with 'str)
                      #+:lispworks 'lw:simple-text-string
                      #-:lispworks 'simple-string)))
    (values converted-parse-tree reg-num starts-with
            ;; we can't simply use *ALLOW-NAMED-REGISTERS*
            ;; since parse-tree syntax ignores it
            (when named-reg-seen
              (nreverse reg-names)))))
