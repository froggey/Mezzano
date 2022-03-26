;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/lexer.lisp,v 1.35 2009/09/17 19:17:31 edi Exp $

;;; The lexer's responsibility is to convert the regex string into a
;;; sequence of tokens which are in turn consumed by the parser.
;;;
;;; The lexer is aware of Perl's 'extended mode' and it also 'knows'
;;; (with a little help from the parser) how many register groups it
;;; has opened so far.  (The latter is necessary for interpreting
;;; strings like "\\10" correctly.)

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

(declaim (inline map-char-to-special-class))
(defun map-char-to-special-char-class (chr)
  (declare #.*standard-optimize-settings*)
  "Maps escaped characters like \"\\d\" to the tokens which represent
their associated character classes."
  (case chr
    ((#\d)
      :digit-class)
    ((#\D)
      :non-digit-class)
    ((#\w)
      :word-char-class)
    ((#\W)
      :non-word-char-class)
    ((#\s)
      :whitespace-char-class)
    ((#\S)
      :non-whitespace-char-class)))

(declaim (inline make-lexer-internal))
(defstruct (lexer (:constructor make-lexer-internal))
  "LEXER structures are used to hold the regex string which is
currently lexed and to keep track of the lexer's state."
  (str "" :type string :read-only t)
  (len 0 :type fixnum :read-only t)
  (reg 0 :type fixnum)
  (pos 0 :type fixnum)
  (last-pos nil :type list))

(defun make-lexer (string)
  (declare #-:genera (string string))
  (make-lexer-internal :str (maybe-coerce-to-simple-string string)
                       :len (length string)))

(declaim (inline end-of-string-p))
(defun end-of-string-p (lexer)
  (declare #.*standard-optimize-settings*)
  "Tests whether we're at the end of the regex string."
  (<= (lexer-len lexer)
      (lexer-pos lexer)))

(declaim (inline looking-at-p))
(defun looking-at-p (lexer chr)
  (declare #.*standard-optimize-settings*)
  "Tests whether the next character the lexer would see is CHR.
Does not respect extended mode."
  (and (not (end-of-string-p lexer))
       (char= (schar (lexer-str lexer) (lexer-pos lexer))
              chr)))

(declaim (inline next-char-non-extended))
(defun next-char-non-extended (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns the next character which is to be examined and updates the
POS slot. Does not respect extended mode."
  (cond ((end-of-string-p lexer) nil)
        (t (prog1
               (schar (lexer-str lexer) (lexer-pos lexer))
             (incf (lexer-pos lexer))))))

(defun next-char (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns the next character which is to be examined and updates the
POS slot. Respects extended mode, i.e.  whitespace, comments, and also
nested comments are skipped if applicable."
  (let ((next-char (next-char-non-extended lexer))
        last-loop-pos)
    (loop
      ;; remember where we started
      (setq last-loop-pos (lexer-pos lexer))
      ;; first we look for nested comments like (?#foo)
      (when (and next-char
                 (char= next-char #\()
                 (looking-at-p lexer #\?))
        (incf (lexer-pos lexer))
        (cond ((looking-at-p lexer #\#)
                ;; must be a nested comment - so we have to search for
                ;; the closing parenthesis
                (let ((error-pos (- (lexer-pos lexer) 2)))
                  (unless
                      ;; loop 'til ')' or end of regex string and
                      ;; return NIL if ')' wasn't encountered
                      (loop for skip-char = next-char
                            then (next-char-non-extended lexer)
                            while (and skip-char
                                       (char/= skip-char #\)))
                            finally (return skip-char))
                    (signal-syntax-error* error-pos "Comment group not closed.")))
                (setq next-char (next-char-non-extended lexer)))
              (t
                ;; undo effect of previous INCF if we didn't see a #
                (decf (lexer-pos lexer)))))
      (when *extended-mode-p*
        ;; now - if we're in extended mode - we skip whitespace and
        ;; comments; repeat the following loop while we look at
        ;; whitespace or #\#
        (loop while (and next-char
                         (or (char= next-char #\#)
                             (whitespacep next-char)))
              do (setq next-char
                         (if (char= next-char #\#)
                           ;; if we saw a comment marker skip until
                           ;; we're behind #\Newline...
                           (loop for skip-char = next-char
                                 then (next-char-non-extended lexer)
                                 while (and skip-char
                                            (char/= skip-char #\Newline))
                                 finally (return (next-char-non-extended lexer)))
                           ;; ...otherwise (whitespace) skip until we
                           ;; see the next non-whitespace character
                           (loop for skip-char = next-char
                                 then (next-char-non-extended lexer)
                                 while (and skip-char
                                            (whitespacep skip-char))
                                 finally (return skip-char))))))
      ;; if the position has moved we have to repeat our tests
      ;; because of cases like /^a (?#xxx) (?#yyy) {3}c/x which
      ;; would be equivalent to /^a{3}c/ in Perl
      (unless (> (lexer-pos lexer) last-loop-pos)
        (return next-char)))))

(declaim (inline fail))
(defun fail (lexer)
  (declare #.*standard-optimize-settings*)
  "Moves (LEXER-POS LEXER) back to the last position stored in
\(LEXER-LAST-POS LEXER) and pops the LAST-POS stack."
  (unless (lexer-last-pos lexer)
    (signal-syntax-error "LAST-POS stack of LEXER ~A is empty." lexer))
  (setf (lexer-pos lexer) (pop (lexer-last-pos lexer)))
  nil)

(defun get-number (lexer &key (radix 10) max-length no-whitespace-p)
  (declare #.*standard-optimize-settings*)
  "Read and consume the number the lexer is currently looking at and
return it. Returns NIL if no number could be identified.
RADIX is used as in PARSE-INTEGER. If MAX-LENGTH is not NIL we'll read
at most the next MAX-LENGTH characters. If NO-WHITESPACE-P is not NIL
we don't tolerate whitespace in front of the number."
  (when (or (end-of-string-p lexer)
            (and no-whitespace-p
                 (whitespacep (schar (lexer-str lexer) (lexer-pos lexer)))))
    (return-from get-number nil))
  (multiple-value-bind (integer new-pos)
      (parse-integer (lexer-str lexer)
                     :start (lexer-pos lexer)
                     :end (if max-length
                            (let ((end-pos (+ (lexer-pos lexer)
                                              (the fixnum max-length)))
                                  (lexer-len (lexer-len lexer)))
                              (if (< end-pos lexer-len)
                                end-pos
                                lexer-len))
                            (lexer-len lexer))
                     :radix radix
                     :junk-allowed t)
    (cond ((and integer (>= (the fixnum integer) 0))
            (setf (lexer-pos lexer) new-pos)
            integer)
          (t nil))))

(declaim (inline try-number))
(defun try-number (lexer &key (radix 10) max-length no-whitespace-p)
  (declare #.*standard-optimize-settings*)
  "Like GET-NUMBER but won't consume anything if no number is seen."
  ;; remember current position
  (push (lexer-pos lexer) (lexer-last-pos lexer))
  (let ((number (get-number lexer
                            :radix radix
                            :max-length max-length
                            :no-whitespace-p no-whitespace-p)))
    (or number (fail lexer))))

(declaim (inline make-char-from-code))
(defun make-char-from-code (number error-pos)
  (declare #.*standard-optimize-settings*)
  "Create character from char-code NUMBER. NUMBER can be NIL
which is interpreted as 0. ERROR-POS is the position where
the corresponding number started within the regex string."
  ;; only look at rightmost eight bits in compliance with Perl
  (let ((code (logand #o377 (the fixnum (or number 0)))))
    (or (and (< code char-code-limit)
             (code-char code))
        (signal-syntax-error* error-pos "No character for hex-code ~X." number))))

(defun unescape-char (lexer)
  (declare #.*standard-optimize-settings*)
  "Convert the characters\(s) following a backslash into a token
which is returned. This function is to be called when the backslash
has already been consumed. Special character classes like \\W are
handled elsewhere."
  (when (end-of-string-p lexer)
    (signal-syntax-error "String ends with backslash."))
  (let ((chr (next-char-non-extended lexer)))
    (case chr
      ((#\E)
        ;; if \Q quoting is on this is ignored, otherwise it's just an
        ;; #\E
        (if *allow-quoting*
          :void
          #\E))
      ((#\c)
        ;; \cx means control-x in Perl
        (let ((next-char (next-char-non-extended lexer)))
          (unless next-char
            (signal-syntax-error* (lexer-pos lexer) "Character missing after '\\c'"))
          (code-char (logxor #x40 (char-code (char-upcase next-char))))))
      ((#\x)
        ;; \x should be followed by a hexadecimal char code,
        ;; two digits or less
        (let* ((error-pos (lexer-pos lexer))
               (number (get-number lexer :radix 16 :max-length 2 :no-whitespace-p t)))
          ;; note that it is OK if \x is followed by zero digits
          (make-char-from-code number error-pos)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        ;; \x should be followed by an octal char code,
        ;; three digits or less
        (let* ((error-pos (decf (lexer-pos lexer)))
               (number (get-number lexer :radix 8 :max-length 3)))
          (make-char-from-code number error-pos)))
      ;; the following five character names are 'semi-standard'
      ;; according to the CLHS but I'm not aware of any implementation
      ;; that doesn't implement them
      ((#\t)
        #\Tab)
      ((#\n)
        #\Newline)
      ((#\r)
        #\Return)
      ((#\f)
        #\Page)
      ((#\b)
        #\Backspace)
      ((#\a)
        (code-char 7))                  ; ASCII bell
      ((#\e)
        (code-char 27))                 ; ASCII escape
      (otherwise
        ;; all other characters aren't affected by a backslash
        chr))))

(defun read-char-property (lexer first-char)
  (declare #.*standard-optimize-settings*)
  (unless (eql (next-char-non-extended lexer) #\{)
    (signal-syntax-error* (lexer-pos lexer) "Expected left brace after \\~A." first-char))
  (let ((name (with-output-to-string (out nil :element-type
                                          #+:lispworks 'lw:simple-char #-:lispworks 'character)
                  (loop
                   (let ((char (or (next-char-non-extended lexer)
                                   (signal-syntax-error "Unexpected EOF after \\~A{." first-char))))
                     (when (char= char #\})
                       (return))
                     (write-char char out))))))
    (list (if (char= first-char #\p) :property :inverted-property)
          name)))

(defun collect-char-class (lexer)
  "Reads and consumes characters from regex string until a right
bracket is seen.  Assembles them into a list \(which is returned) of
characters, character ranges, like \(:RANGE #\\A #\\E) for a-e, and
tokens representing special character classes."
  (declare #.*standard-optimize-settings*)
  (let ((start-pos (lexer-pos lexer))         ; remember start for error message
        hyphen-seen
        last-char
        list)
    (flet ((handle-char (c)
             "Do the right thing with character C depending on whether
we're inside a range or not."
             (cond ((and hyphen-seen last-char)
                    (setf (car list) (list :range last-char c)
                          last-char nil))
                   (t
                    (push c list)
                    (setq last-char c)))
             (setq hyphen-seen nil)))
      (loop for first = t then nil
            for c = (next-char-non-extended lexer)
            ;; leave loop if at end of string
            while c
            do (cond
                ((char= c #\\)
                 ;; we've seen a backslash
                 (let ((next-char (next-char-non-extended lexer)))
                   (case next-char
                     ((#\d #\D #\w #\W #\s #\S)
                      ;; a special character class
                      (push (map-char-to-special-char-class next-char) list)
                      ;; if the last character was a hyphen
                      ;; just collect it literally
                      (when hyphen-seen
                        (push #\- list))
                      ;; if the next character is a hyphen do the same
                      (when (looking-at-p lexer #\-)
                        (push #\- list)
                        (incf (lexer-pos lexer)))
                      (setq hyphen-seen nil))
                     ((#\P #\p)
                      ;; maybe a character property
                      (cond ((null *property-resolver*)
                             (handle-char next-char))
                            (t
                             (push (read-char-property lexer next-char) list)
                             ;; if the last character was a hyphen
                             ;; just collect it literally
                             (when hyphen-seen
                               (push #\- list))
                             ;; if the next character is a hyphen do the same
                             (when (looking-at-p lexer #\-)
                               (push #\- list)
                               (incf (lexer-pos lexer)))
                             (setq hyphen-seen nil))))
                     ((#\E)
                      ;; if \Q quoting is on we ignore \E,
                      ;; otherwise it's just a plain #\E
                      (unless *allow-quoting*
                        (handle-char #\E)))
                     (otherwise
                      ;; otherwise unescape the following character(s)
                      (decf (lexer-pos lexer))
                      (handle-char (unescape-char lexer))))))
                (first
                 ;; the first character must not be a right bracket
                 ;; and isn't treated specially if it's a hyphen
                 (handle-char c))
                ((char= c #\])
                 ;; end of character class
                 ;; make sure we collect a pending hyphen
                 (when hyphen-seen
                   (setq hyphen-seen nil)
                   (handle-char #\-))
                 ;; reverse the list to preserve the order intended
                 ;; by the author of the regex string
                 (return-from collect-char-class (nreverse list)))
                ((and (char= c #\-)
                      last-char
                      (not hyphen-seen))
                 ;; if the last character was 'just a character'
                 ;; we expect to be in the middle of a range
                 (setq hyphen-seen t))
                ((char= c #\-)
                 ;; otherwise this is just an ordinary hyphen
                 (handle-char #\-))
                (t
                 ;; default case - just collect the character
                 (handle-char c))))
      ;; we can only exit the loop normally if we've reached the end
      ;; of the regex string without seeing a right bracket
      (signal-syntax-error* start-pos "Missing right bracket to close character class."))))

(defun maybe-parse-flags (lexer)
  (declare #.*standard-optimize-settings*)
  "Reads a sequence of modifiers \(including #\\- to reverse their
meaning) and returns a corresponding list of \"flag\" tokens.  The
\"x\" modifier is treated specially in that it dynamically modifies
the behaviour of the lexer itself via the special variable
*EXTENDED-MODE-P*."
  (prog1
    (loop with set = t
          for chr = (next-char-non-extended lexer)
          unless chr
            do (signal-syntax-error "Unexpected end of string.")
          while (find chr "-imsx" :test #'char=)
          ;; the first #\- will invert the meaning of all modifiers
          ;; following it
          if (char= chr #\-)
            do (setq set nil)
          else if (char= chr #\x)
            do (setq *extended-mode-p* set)
          else collect (if set
                         (case chr
                           ((#\i)
                             :case-insensitive-p)
                           ((#\m)
                             :multi-line-mode-p)
                           ((#\s)
                             :single-line-mode-p))
                         (case chr
                           ((#\i)
                             :case-sensitive-p)
                           ((#\m)
                             :not-multi-line-mode-p)
                           ((#\s)
                             :not-single-line-mode-p))))
    (decf (lexer-pos lexer))))

(defun get-quantifier (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns a list of two values (min max) if what the lexer is looking
at can be interpreted as a quantifier. Otherwise returns NIL and
resets the lexer to its old position."
  ;; remember starting position for FAIL and UNGET-TOKEN functions
  (push (lexer-pos lexer) (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (case next-char
      ((#\*)
        ;; * (Kleene star): match 0 or more times
        '(0 nil))
      ((#\+)
        ;; +: match 1 or more times
        '(1 nil))
      ((#\?)
        ;; ?: match 0 or 1 times
        '(0 1))
      ((#\{)
        ;; one of
        ;;   {n}:   match exactly n times
        ;;   {n,}:  match at least n times
        ;;   {n,m}: match at least n but not more than m times
        ;; note that anything not matching one of these patterns will
        ;; be interpreted literally - even whitespace isn't allowed
        (let ((num1 (get-number lexer :no-whitespace-p t)))
          (if num1
            (let ((next-char (next-char-non-extended lexer)))
              (case next-char
                ((#\,)
                  (let* ((num2 (get-number lexer :no-whitespace-p t))
                         (next-char (next-char-non-extended lexer)))
                    (case next-char
                      ((#\})
                        ;; this is the case {n,} (NUM2 is NIL) or {n,m}
                        (list num1 num2))
                      (otherwise
                        (fail lexer)))))
                ((#\})
                  ;; this is the case {n}
                  (list num1 num1))
                (otherwise
                  (fail lexer))))
            ;; no number following left curly brace, so we treat it
            ;; like a normal character
            (fail lexer))))
      ;; cannot be a quantifier
      (otherwise
        (fail lexer)))))

(defun parse-register-name-aux (lexer)
  "Reads and returns the name in a named register group.  It is
assumed that the starting #\< character has already been read.  The
closing #\> will also be consumed."
  ;; we have to look for an ending > character now
  (let ((end-name (position #\>
                            (lexer-str lexer)
                            :start (lexer-pos lexer)
                            :test #'char=)))
    (unless end-name
      ;; there has to be > somewhere, syntax error otherwise
      (signal-syntax-error* (1- (lexer-pos lexer)) "Opening #\< in named group has no closing #\>."))
    (let ((name (subseq (lexer-str lexer)
                        (lexer-pos lexer)
                        end-name)))
      (unless (every #'(lambda (char)
                         (or (alphanumericp char)
                             (char= #\- char)))
                     name)
        ;; register name can contain only alphanumeric characters or #\-
        (signal-syntax-error* (lexer-pos lexer) "Invalid character in named register group."))
      ;; advance lexer beyond "<name>" part
      (setf (lexer-pos lexer) (1+ end-name))
      name)))

(declaim (inline unget-token))
(defun unget-token (lexer)
  (declare #.*standard-optimize-settings*)
  "Moves the lexer back to the last position stored in the LAST-POS stack."
  (if (lexer-last-pos lexer)
    (setf (lexer-pos lexer)
            (pop (lexer-last-pos lexer)))
    (error "No token to unget \(this should not happen)")))

(defun get-token (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns and consumes the next token from the regex string \(or NIL)."
  ;; remember starting position for UNGET-TOKEN function
  (push (lexer-pos lexer)
        (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (cond (next-char
           (case next-char
             ;; the easy cases first - the following six characters
             ;; always have a special meaning and get translated
             ;; into tokens immediately
             ((#\))
              :close-paren)
             ((#\|)
              :vertical-bar)
             ((#\?)
              :question-mark)
             ((#\.)
              :everything)
             ((#\^)
              :start-anchor)
             ((#\$)
              :end-anchor)
             ((#\+ #\*)
              ;; quantifiers will always be consumend by
              ;; GET-QUANTIFIER, they must not appear here
              (signal-syntax-error* (1- (lexer-pos lexer)) "Quantifier '~A' not allowed." next-char))
             ((#\{)
              ;; left brace isn't a special character in it's own
              ;; right but we must check if what follows might
              ;; look like a quantifier
              (let ((this-pos (lexer-pos lexer))
                    (this-last-pos (lexer-last-pos lexer)))
                (unget-token lexer)
                (when (get-quantifier lexer)
                  (signal-syntax-error* (car this-last-pos)
                                        "Quantifier '~A' not allowed."
                                        (subseq (lexer-str lexer)
                                                (car this-last-pos)
                                                (lexer-pos lexer))))
                (setf (lexer-pos lexer) this-pos
                      (lexer-last-pos lexer) this-last-pos)
                next-char))
             ((#\[)
              ;; left bracket always starts a character class
              (cons  (cond ((looking-at-p lexer #\^)
                            (incf (lexer-pos lexer))
                            :inverted-char-class)
                           (t
                            :char-class))
                     (collect-char-class lexer)))
             ((#\\)
              ;; backslash might mean different things so we have
              ;; to peek one char ahead:
              (let ((next-char (next-char-non-extended lexer)))
                (case next-char
                  ((#\A)
                   :modeless-start-anchor)
                  ((#\Z)
                   :modeless-end-anchor)
                  ((#\z)
                   :modeless-end-anchor-no-newline)
                  ((#\b)
                   :word-boundary)
                  ((#\B)
                   :non-word-boundary)
                  ((#\k)
                   (cond ((and *allow-named-registers*
                               (looking-at-p lexer #\<))
                          ;; back-referencing a named register
                          (incf (lexer-pos lexer))
                          (list :back-reference
                                (parse-register-name-aux lexer)))
                         (t
                          ;; false alarm, just unescape \k
                          #\k)))
                  ((#\d #\D #\w #\W #\s #\S)
                   ;; these will be treated like character classes
                   (map-char-to-special-char-class next-char))
                  ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   ;; uh, a digit...
                   (let* ((old-pos (decf (lexer-pos lexer)))
                          ;; ...so let's get the whole number first
                          (backref-number (get-number lexer)))
                     (declare (fixnum backref-number))
                     (cond ((and (> backref-number (lexer-reg lexer))
                                 (<= 10 backref-number))
                            ;; \10 and higher are treated as octal
                            ;; character codes if we haven't
                            ;; opened that much register groups
                            ;; yet
                            (setf (lexer-pos lexer) old-pos)
                            ;; re-read the number from the old
                            ;; position and convert it to its
                            ;; corresponding character
                            (make-char-from-code (get-number lexer :radix 8 :max-length 3)
                                                 old-pos))
                           (t
                            ;; otherwise this must refer to a
                            ;; backreference
                            (list :back-reference backref-number)))))
                  ((#\0)
                   ;; this always means an octal character code
                   ;; (at most three digits)
                   (let ((old-pos (decf (lexer-pos lexer))))
                     (make-char-from-code (get-number lexer :radix 8 :max-length 3)
                                          old-pos)))
                  ((#\P #\p)
                   ;; might be a named property
                   (cond (*property-resolver* (read-char-property lexer next-char))
                         (t next-char)))
                  (otherwise
                   ;; in all other cases just unescape the
                   ;; character
                   (decf (lexer-pos lexer))
                   (unescape-char lexer)))))
             ((#\()
              ;; an open parenthesis might mean different things
              ;; depending on what follows...
              (cond ((looking-at-p lexer #\?)
                     ;; this is the case '(?' (and probably more behind)
                     (incf (lexer-pos lexer))
                     ;; we have to check for modifiers first
                     ;; because a colon might follow
                     (let* ((flags (maybe-parse-flags lexer))
                            (next-char (next-char-non-extended lexer)))
                       ;; modifiers are only allowed if a colon
                       ;; or a closing parenthesis are following
                       (when (and flags
                                  (not (find next-char ":)" :test #'char=)))
                         (signal-syntax-error* (car (lexer-last-pos lexer))
                                               "Sequence '~A' not recognized."
                                               (subseq (lexer-str lexer)
                                                       (car (lexer-last-pos lexer))
                                                       (lexer-pos lexer))))
                       (case next-char
                         ((nil)
                          ;; syntax error
                          (signal-syntax-error "End of string following '(?'."))
                         ((#\))
                          ;; an empty group except for the flags
                          ;; (if there are any)
                          (or (and flags
                                   (cons :flags flags))
                              :void))
                         ((#\()
                          ;; branch
                          :open-paren-paren)
                         ((#\>)
                          ;; standalone
                          :open-paren-greater)
                         ((#\=)
                          ;; positive look-ahead
                          :open-paren-equal)
                         ((#\!)
                          ;; negative look-ahead
                          :open-paren-exclamation)
                         ((#\:)
                          ;; non-capturing group - return flags as
                          ;; second value
                          (values :open-paren-colon flags))
                         ((#\<)
                          ;; might be a look-behind assertion or a named group, so
                          ;; check next character
                          (let ((next-char (next-char-non-extended lexer)))
                            (cond ((and next-char
                                        (alpha-char-p next-char))
                                   ;; we have encountered a named group
                                   ;; are we supporting register naming?
                                   (unless *allow-named-registers*
                                     (signal-syntax-error* (1- (lexer-pos lexer))
                                                           "Character '~A' may not follow '(?<' (because ~a = NIL)"
                                                           next-char
                                                           '*allow-named-registers*))
                                   ;; put the letter back
                                   (decf (lexer-pos lexer))
                                   ;; named group
                                   :open-paren-less-letter)
                                  (t
                                   (case next-char
                                     ((#\=)
                                      ;; positive look-behind
                                      :open-paren-less-equal)
                                     ((#\!)
                                      ;; negative look-behind
                                      :open-paren-less-exclamation)
                                     ((#\))
                                      ;; Perl allows "(?<)" and treats
                                      ;; it like a null string
                                      :void)
                                     ((nil)
                                      ;; syntax error
                                      (signal-syntax-error "End of string following '(?<'."))
                                     (t
                                      ;; also syntax error
                                      (signal-syntax-error* (1- (lexer-pos lexer))
                                                            "Character '~A' may not follow '(?<'."
                                                            next-char )))))))
                         (otherwise
                          (signal-syntax-error* (1- (lexer-pos lexer))
                                                "Character '~A' may not follow '(?'."
                                                next-char)))))
                    (t
                     ;; if next-char was not #\? (this is within
                     ;; the first COND), we've just seen an opening
                     ;; parenthesis and leave it like that
                     :open-paren)))
             (otherwise
              ;; all other characters are their own tokens
              next-char)))
          ;; we didn't get a character (this if the "else" branch from
          ;; the first IF), so we don't return a token but NIL
          (t
           (pop (lexer-last-pos lexer))
           nil))))

(declaim (inline start-of-subexpr-p))
(defun start-of-subexpr-p (lexer)
  (declare #.*standard-optimize-settings*)
  "Tests whether the next token can start a valid sub-expression, i.e.
a stand-alone regex."
  (let* ((pos (lexer-pos lexer))
         (next-char (next-char lexer)))
    (not (or (null next-char)
             (prog1
               (member (the character next-char)
                       '(#\) #\|)
                       :test #'char=)
               (setf (lexer-pos lexer) pos))))))
