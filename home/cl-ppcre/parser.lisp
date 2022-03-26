;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/parser.lisp,v 1.31 2009/09/17 19:17:31 edi Exp $

;;; The parser will - with the help of the lexer - parse a regex
;;; string and convert it into a "parse tree" (see docs for details
;;; about the syntax of these trees).  Note that the lexer might
;;; return illegal parse trees.  It is assumed that the conversion
;;; process later on will track them down.

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

(defun group (lexer)
  "Parses and consumes a <group>.
The productions are: <group> -> \"\(\"<regex>\")\"
                                \"\(?:\"<regex>\")\"
                                \"\(?>\"<regex>\")\"
                                \"\(?<flags>:\"<regex>\")\"
                                \"\(?=\"<regex>\")\"
                                \"\(?!\"<regex>\")\"
                                \"\(?<=\"<regex>\")\"
                                \"\(?<!\"<regex>\")\"
                                \"\(?\(\"<num>\")\"<regex>\")\"
                                \"\(?\(\"<regex>\")\"<regex>\")\"
                                \"\(?<name>\"<regex>\")\" \(when *ALLOW-NAMED-REGISTERS* is T)
                                <legal-token>
where <flags> is parsed by the lexer function MAYBE-PARSE-FLAGS.
Will return <parse-tree> or \(<grouping-type> <parse-tree>) where
<grouping-type> is one of six keywords - see source for details."
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (open-token flags)
      (get-token lexer)
    (cond ((eq open-token :open-paren-paren)
            ;; special case for conditional regular expressions; note
            ;; that at this point we accept a couple of illegal
            ;; combinations which'll be sorted out later by the
            ;; converter
            (let* ((open-paren-pos (car (lexer-last-pos lexer)))
                   ;; check if what follows "(?(" is a number
                   (number (try-number lexer :no-whitespace-p t))
                   ;; make changes to extended-mode-p local
                   (*extended-mode-p* *extended-mode-p*))
              (declare (fixnum open-paren-pos))
              (cond (number
                      ;; condition is a number (i.e. refers to a
                      ;; back-reference)
                      (let* ((inner-close-token (get-token lexer))
                             (reg-expr (reg-expr lexer))
                             (close-token (get-token lexer)))
                        (unless (eq inner-close-token :close-paren)
                          (signal-syntax-error* (+ open-paren-pos 2)
                                                "Opening paren has no matching closing paren."))
                        (unless (eq close-token :close-paren)
                          (signal-syntax-error* open-paren-pos
                                                "Opening paren has no matching closing paren."))
                        (list :branch number reg-expr)))
                    (t
                      ;; condition must be a full regex (actually a
                      ;; look-behind or look-ahead); and here comes a
                      ;; terrible kludge: instead of being cleanly
                      ;; separated from the lexer, the parser pushes
                      ;; back the lexer by one position, thereby
                      ;; landing in the middle of the 'token' "(?(" -
                      ;; yuck!!
                      (decf (lexer-pos lexer))
                      (let* ((inner-reg-expr (group lexer))
                             (reg-expr (reg-expr lexer))
                             (close-token (get-token lexer)))
                        (unless (eq close-token :close-paren)
                          (signal-syntax-error* open-paren-pos
                                                "Opening paren has no matching closing paren."))
                        (list :branch inner-reg-expr reg-expr))))))
          ((member open-token '(:open-paren
                                :open-paren-colon
                                :open-paren-greater
                                :open-paren-equal
                                :open-paren-exclamation
                                :open-paren-less-equal
                                :open-paren-less-exclamation
                                :open-paren-less-letter)
                   :test #'eq)
            ;; make changes to extended-mode-p local
            (let ((*extended-mode-p* *extended-mode-p*))
              ;; we saw one of the six token representing opening
              ;; parentheses
              (let* ((open-paren-pos (car (lexer-last-pos lexer)))
                     (register-name (when (eq open-token :open-paren-less-letter)
                                      (parse-register-name-aux lexer)))
                     (reg-expr (reg-expr lexer))
                     (close-token (get-token lexer)))
                (when (or (eq open-token :open-paren)
                          (eq open-token :open-paren-less-letter))
                  ;; if this is the "("<regex>")" or "(?"<name>""<regex>")" production we have to
                  ;; increment the register counter of the lexer
                  (incf (lexer-reg lexer)))
                (unless (eq close-token :close-paren)
                  ;; the token following <regex> must be the closing
                  ;; parenthesis or this is a syntax error
                  (signal-syntax-error* open-paren-pos
                                        "Opening paren has no matching closing paren."))
                (if flags
                  ;; if the lexer has returned a list of flags this must
                  ;; have been the "(?:"<regex>")" production
                  (cons :group (nconc flags (list reg-expr)))
                  (if (eq open-token :open-paren-less-letter)
                      (list :named-register register-name
                            reg-expr)
                      (list (case open-token
                              ((:open-paren)
                               :register)
                              ((:open-paren-colon)
                               :group)
                              ((:open-paren-greater)
                               :standalone)
                              ((:open-paren-equal)
                               :positive-lookahead)
                              ((:open-paren-exclamation)
                               :negative-lookahead)
                              ((:open-paren-less-equal)
                               :positive-lookbehind)
                              ((:open-paren-less-exclamation)
                               :negative-lookbehind))
                            reg-expr))))))
          (t
           ;; this is the <legal-token> production; <legal-token> is
           ;; any token which passes START-OF-SUBEXPR-P (otherwise
           ;; parsing had already stopped in the SEQ method)
           open-token))))

(defun greedy-quant (lexer)
  "Parses and consumes a <greedy-quant>.
The productions are: <greedy-quant> -> <group> | <group><quantifier>
where <quantifier> is parsed by the lexer function GET-QUANTIFIER.
Will return <parse-tree> or (:GREEDY-REPETITION <min> <max> <parse-tree>)."
  (declare #.*standard-optimize-settings*)
  (let* ((group (group lexer))
         (token (get-quantifier lexer)))
    (if token
      ;; if GET-QUANTIFIER returned a non-NIL value it's the
      ;; two-element list (<min> <max>)
      (list :greedy-repetition (first token) (second token) group)
      group)))

(defun quant (lexer)
  "Parses and consumes a <quant>.
The productions are: <quant> -> <greedy-quant> | <greedy-quant>\"?\".
Will return the <parse-tree> returned by GREEDY-QUANT and optionally
change :GREEDY-REPETITION to :NON-GREEDY-REPETITION."
  (declare #.*standard-optimize-settings*)
  (let* ((greedy-quant (greedy-quant lexer))
         (pos (lexer-pos lexer))
         (next-char (next-char lexer)))
    (when next-char
      (if (char= next-char #\?)
        (setf (car greedy-quant) :non-greedy-repetition)
        (setf (lexer-pos lexer) pos)))
    greedy-quant))

(defun seq (lexer)
  "Parses and consumes a <seq>.
The productions are: <seq> -> <quant> | <quant><seq>.
Will return <parse-tree> or (:SEQUENCE <parse-tree> <parse-tree>)."
  (declare #.*standard-optimize-settings*)
  (flet ((make-array-from-two-chars (char1 char2)
           (let ((string (make-array 2
                                     :element-type 'character
                                     :fill-pointer t
                                     :adjustable t)))
             (setf (aref string 0) char1)
             (setf (aref string 1) char2)
             string)))
    ;; Note that we're calling START-OF-SUBEXPR-P before we actually try
    ;; to parse a <seq> or <quant> in order to catch empty regular
    ;; expressions
    (if (start-of-subexpr-p lexer)
        (loop with seq-is-sequence-p = nil
              with last-cdr
              for quant = (quant lexer)
              for quant-is-char-p = (characterp quant)
              for seq = quant
              then
              (cond ((and quant-is-char-p (characterp seq))
                     (make-array-from-two-chars seq quant))
                    ((and quant-is-char-p (stringp seq))
                     (vector-push-extend quant seq)
                     seq)
                    ((not seq-is-sequence-p)
                     (setf last-cdr (list quant)
                           seq-is-sequence-p t)
                     (list* :sequence seq last-cdr))
                    ((and quant-is-char-p
                          (characterp (car last-cdr)))
                     (setf (car last-cdr)
                           (make-array-from-two-chars (car last-cdr)
                                                      quant))
                     seq)
                    ((and quant-is-char-p
                          (stringp (car last-cdr)))
                     (vector-push-extend quant (car last-cdr))
                     seq)
                    (t
                     ;; if <seq> is also a :SEQUENCE parse tree we merge
                     ;; both lists into one
                     (let ((cons (list quant)))
                       (psetf last-cdr cons
                              (cdr last-cdr) cons))
                     seq))
              while (start-of-subexpr-p lexer)
              finally (return seq))
        :void)))

(defun reg-expr (lexer)
  "Parses and consumes a <regex>, a complete regular expression.
The productions are: <regex> -> <seq> | <seq>\"|\"<regex>.
Will return <parse-tree> or (:ALTERNATION <parse-tree> <parse-tree>)."
  (declare #.*standard-optimize-settings*)
  (let ((pos (lexer-pos lexer)))
    (case (next-char lexer)
      ((nil)
        ;; if we didn't get any token we return :VOID which stands for
        ;; "empty regular expression"
        :void)
      ((#\|)
        ;; now check whether the expression started with a vertical
        ;; bar, i.e. <seq> - the left alternation - is empty
        (list :alternation :void (reg-expr lexer)))
      (otherwise
        ;; otherwise un-read the character we just saw and parse a
        ;; <seq> plus the character following it
        (setf (lexer-pos lexer) pos)
        (let* ((seq (seq lexer))
               (pos (lexer-pos lexer)))
          (case (next-char lexer)
            ((nil)
              ;; no further character, just a <seq>
              seq)
            ((#\|)
              ;; if the character was a vertical bar, this is an
              ;; alternation and we have the second production
              (let ((reg-expr (reg-expr lexer)))
                (cond ((and (consp reg-expr)
                            (eq (first reg-expr) :alternation))
                        ;; again we try to merge as above in SEQ
                        (setf (cdr reg-expr)
                                (cons seq (cdr reg-expr)))
                        reg-expr)
                      (t (list :alternation seq reg-expr)))))
            (otherwise
              ;; a character which is not a vertical bar - this is
              ;; either a syntax error or we're inside of a group and
              ;; the next character is a closing parenthesis; so we
              ;; just un-read the character and let another function
              ;; take care of it
              (setf (lexer-pos lexer) pos)
              seq)))))))

(defun parse-string (string)
  "Translate the regex string STRING into a parse tree."
  (declare #.*standard-optimize-settings*)
  (let* ((lexer (make-lexer string))
         (parse-tree (reg-expr lexer)))
    ;; check whether we've consumed the whole regex string
    (if (end-of-string-p lexer)
      parse-tree
      (signal-syntax-error* (lexer-pos lexer) "Expected end of string."))))
