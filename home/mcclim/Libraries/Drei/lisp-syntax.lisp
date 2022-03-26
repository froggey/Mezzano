;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX -*-

;;;  (c) copyright 2005, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2006-2007 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
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

;;; A syntax module for analysing Common Lisp using an LR based
;;; parser.

(in-package :drei-lisp-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience functions and macros.

(defun usable-package (package-designator)
  "Return a usable package based on `package-designator'."
  (or (find-package package-designator)
      *package*))

(defmacro evaluating-interactively (&body body)
  `(handler-case (progn ,@body)
     (end-of-file ()
       (esa:display-message "Unbalanced parentheses in form."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The command table.

(define-syntax-command-table lisp-table
    :errorp nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the syntax object

(declaim (special |initial-state |))    ; defined (class and var) further below

(define-syntax lisp-syntax (lr-syntax-mixin fundamental-syntax)
  ((%package-list :accessor package-list
                  :documentation "An alist mapping the end offset
of (in-package) forms to a string of the package designator in
the form. The list is sorted with the earliest (in-package) forms
last (descending offset).")
   (%base :initform nil
          :documentation "The base which numbers in the buffer are
expected to be in. If the provided value is NIL, the value of
`*read-base*' will be used."
          :type (or null (integer 2 36)))
   (%option-specified-package :accessor option-specified-package
                              :initform nil
                              :documentation "The package
specified in the attribute line (may be overridden
by (in-package) forms). This may be either a string (the name of
the intended package) or a package object.")
   (%image :accessor image
           :initform nil
           :documentation "An image object (or NIL) that
determines where and how Lisp code in the buffer of the
syntax should be run.")
   (%form-before-cache :accessor form-before-cache
                       :initform (make-hash-table :test #'equal))
   (%form-after-cache :accessor form-after-cache
                      :initform (make-hash-table :test #'equal))
   (%form-around-cache :accessor form-around-cache
                       :initform (make-hash-table :test #'equal)))
  (:name "Lisp")
  (:pathname-types "lisp" "lsp" "cl")
  (:command-table lisp-table)
  (:default-initargs :initial-state |initial-state |))

(defgeneric base (syntax)
  (:documentation "Get the base `syntax' should interpret numbers
  in.")
  (:method ((syntax lisp-syntax))
    (or (slot-value syntax '%base)
        *read-base*)))

(defmethod (setf base) (base (syntax lisp-syntax))
  (setf (slot-value syntax '%base) base))

(define-option-for-syntax lisp-syntax "Package" (syntax package-name)
  (let ((specified-package (find-package package-name)))
    (setf (option-specified-package syntax) (or specified-package package-name))))

(define-option-for-syntax lisp-syntax "Base" (syntax base)
  (let ((integer-base (parse-integer base :junk-allowed t)))
    (when integer-base
      (if (typep integer-base '(integer 2 36))
          (setf (base syntax) integer-base)
          (esa:display-message "Invalid base specified: outside the interval 2 to 36.")))))

(defmethod current-attributes-for-syntax append ((syntax lisp-syntax))
  (list (cons :package (or (if (packagep (option-specified-package syntax))
                               (package-name (option-specified-package syntax))
                               (option-specified-package syntax))
                           (package-name (package-at-mark
                                          syntax
                                          (or (caar (last (package-list syntax)))
                                              0)))))
        (cons :base (format nil "~A" (base syntax)))))

(defmethod name-for-info-pane ((syntax lisp-syntax) &key view)
  (format nil "Lisp~@[:~(~A~)~]"
          (provided-package-name-at-mark syntax (if (typep view 'point-mark-view)
                                                    (point view)
                                                    0))))

(defmethod display-syntax-name ((syntax lisp-syntax) (stream extended-output-stream) &key view)
  (princ "Lisp:" stream)                ; FIXME: should be `present'ed
                                        ; as something.
  (let ((package-name (provided-package-name-at-mark syntax (if (typep view 'point-mark-view)
                                                                (point view)
                                                                0))))
    (if (find-package package-name)
        (with-output-as-presentation (stream (find-package package-name) 'expression)
          (princ package-name stream))
        (with-text-face (stream :italic)
          (princ package-name stream)))))

#+clim-without-swank
(defun default-image ()
  "The default image for when the current syntax does not mandate
anything itself (for example if it is not a Lisp syntax)."
  t)

(defgeneric get-usable-image (syntax)
  (:documentation "Get usable image object from `syntax'.")
  (:method (syntax)
    (default-image))
  (:method ((syntax lisp-syntax))
    (or (image syntax)
        (default-image))))

(defconstant +keyword-package+ (find-package :keyword)
  "The KEYWORD package.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Swank interface functions.

(defgeneric eval-string-for-drei (image string package)
  (:documentation "Evaluate `string' in `package'. A single value
is returned: The result of evaluating `string'.")
  (:method (image string package)
    (let ((*package* package))
      (eval-form-for-drei image (read-from-string string)))))

(defgeneric eval-form-for-drei (image form)
  (:documentation "Evaluate `string' in `package'. A single value
is returned: The result of evaluating `string'.")
  (:method (image form)
    (declare (ignore image))
    (eval form)))

(defgeneric compile-string-for-drei (image string package view buffer-mark)
  (:documentation "Compile and evaluate `string' in
`package'. Two values are returned: The result of evaluating
`string' and a list of compiler notes. `Buffer' and `buffer-mark'
will be used for hyperlinking the compiler notes to the source
code.")
  (:method (image (string string) package (view drei-buffer-view)
            (buffer-mark mark))
    (error "Backend insufficient for this operation")))

(defgeneric compile-form-for-drei (image form view buffer-mark)
  (:documentation "Compile and evaluate `form', which must be a
valid Lisp form. Two values are returned: The result of
evaluating `string' and a list of compiler notes. `Buffer' and
`buffer-mark' will be used for hyperlinking the compiler notes to
the source code.")
  (:method (image form (view drei-syntax-view) (buffer-mark mark))
    (compile-string-for-drei image
                             (let ((*print-base* (base (syntax view))))
                               (write-to-string form))
                             *package* view buffer-mark)))

(defgeneric compile-file-for-drei (image filepath package &optional load-p)
  (:documentation "Compile the file at `filepath' in
`package'. If `load-p' is non-NIL, also load the file at
`filepath'. Two values will be returned: the result of compiling
the file and a list of compiler notes.")
  (:method (image filepath package &optional load-p)
    (declare (ignore image filepath package load-p))
    (error "Backend insufficient for this operation")))

(defgeneric macroexpand-for-drei (image form &optional full-p)
  (:documentation "Macroexpand `form' and return result.")
  (:method (image form &optional full-p)
    (declare (ignore image))
    (funcall (if full-p
                 #'macroexpand
                 #'macroexpand-1)
             form)))

(defgeneric find-definitions-for-drei (image symbol)
  (:documentation "Return list of definitions for `symbol'.")
  (:method (image symbol)
    (declare (ignore image symbol))))

(defgeneric get-class-keyword-parameters (image class)
  (:documentation "Get a list of keyword parameters (possibly
along with any default values) that can be used in a
`make-instance' form for `class'.")
  (:method (image class)
    (declare (ignore image class))))

(defgeneric arglist (image symbol)
  (:documentation "Get plain arglist for symbol.")
  (:method (image symbol)
    (declare (ignore image symbol))))

(defgeneric simple-completions (image string default-package)
  (:documentation "Return a list of simple symbol-completions for
`string' in `default-package'.")
  (:method (image string default-package)
    (declare (ignore image string default-package))))

(defgeneric fuzzy-completions (image symbol-name default-package &optional limit)
  (:documentation "Return a list of fuzzy completions for `symbol-name'.")
  (:method (image symbol-name default-package &optional limit)
    (declare (ignore image symbol-name default-package limit))))

;;; Lexing

(define-lexer-state lexer-list-state (lexer-toplevel-state)
  ()
  (:documentation "In this state, the lexer assumes it can skip
    whitespace and should recognize ordinary lexemes of the language"))

(define-lexer-state lexer-string-state ()
  ()
  (:documentation "In this state, the lexer is working inside a string
    delimited by double quote characters."))

(define-lexer-state lexer-line-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a line
    comment (starting with a semicolon."))

(define-lexer-state lexer-long-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a long
    comment delimited by #| and |#."))

(define-lexer-state lexer-escaped-token-state ()
  ()
  (:documentation "In this state, the lexer is accumulating a token
    and an odd number of multiple escapes have been seen."))

(defclass lisp-nonterminal (nonterminal) ())
(defclass form (lisp-nonterminal) ())
(defclass complete-form-mixin () ())
(defclass incomplete-form-mixin () ())

(defclass comment (lisp-nonterminal) ())

(defclass lisp-lexeme (lexeme) ())

(defclass error-lexeme (lisp-lexeme) ())
(defclass literal-object-lexeme (lisp-lexeme literal-object-mixin) ())
(defclass literal-object-error-lexeme (lisp-lexeme literal-object-mixin) ())
(defclass parenthesis-lexeme (lisp-lexeme) ())
(defclass left-parenthesis-lexeme (parenthesis-lexeme) ())
(defclass simple-vector-start-lexeme (lisp-lexeme) ())
(defclass right-parenthesis-lexeme (parenthesis-lexeme) ())
(defclass quote-lexeme (lisp-lexeme) ())
(defclass backquote-lexeme (lisp-lexeme) ())
(defclass comma-lexeme (lisp-lexeme) ())
(defclass comma-at-lexeme (lisp-lexeme) ())
(defclass comma-dot-lexeme (lisp-lexeme) ())
(defclass dot-lexeme (lisp-lexeme) ())
(defclass form-lexeme (form lisp-lexeme) ())
(defclass incomplete-character-lexeme (form-lexeme incomplete-form-mixin) ())
(defclass complete-character-lexeme (form-lexeme complete-form-mixin) ())
(defclass function-lexeme (lisp-lexeme) ())
(defclass line-comment-start-lexeme (lisp-lexeme) ())
(defclass long-comment-start-lexeme (lisp-lexeme) ())
(defclass comment-end-lexeme (lisp-lexeme) ())
(defclass string-start-lexeme (lisp-lexeme) ())
(defclass string-end-lexeme (lisp-lexeme) ())
(defclass word-lexeme (lisp-lexeme) ())
(defclass delimiter-lexeme (lisp-lexeme) ())
(defclass literal-object-delimiter-lexeme (delimiter-lexeme literal-object-lexeme) ())
(defclass text-lexeme (lisp-lexeme) ())
(defclass sharpsign-equals-lexeme (lisp-lexeme) ())
(defclass sharpsign-sharpsign-form (form-lexeme complete-form-mixin) ())
(defclass reader-conditional-positive-lexeme (lisp-lexeme) ())
(defclass reader-conditional-negative-lexeme (lisp-lexeme) ())
(defclass uninterned-symbol-lexeme (lisp-lexeme) ())
(defclass readtime-evaluation-lexeme (lisp-lexeme) ())
(defclass array-start-lexeme (lisp-lexeme) ())
(defclass structure-start-lexeme (lisp-lexeme) ())
(defclass pathname-start-lexeme (lisp-lexeme) ())
(defclass undefined-reader-macro-lexeme (lisp-lexeme) ())
(defclass bit-vector-form (form-lexeme complete-form-mixin) ())
(defclass token-mixin () ())
(defclass number-lexeme (token-mixin form-lexeme complete-form-mixin) ())
(defclass literal-object-form (form-lexeme complete-form-mixin literal-object-mixin) ())
(defclass complete-token-lexeme (token-mixin lisp-lexeme) ())
(defclass multiple-escape-start-lexeme (lisp-lexeme) ())
(defclass multiple-escape-end-lexeme (lisp-lexeme) ())
(defclass incomplete-lexeme (lisp-lexeme incomplete-form-mixin) ())
(defclass unmatched-right-parenthesis-lexeme (lisp-lexeme) ())

(defmethod skip-inter ((syntax lisp-syntax) state scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop when (end-of-buffer-p scan)
            do (return nil)
          until (not (whitespacep syntax (object-after scan)))
          do (fo)
          finally (return t))))

(defmethod lex ((syntax lisp-syntax) (state lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
        (#\( (fo) (make-instance 'left-parenthesis-lexeme))
        (#\) (fo) (make-instance 'unmatched-right-parenthesis-lexeme))
        (#\' (fo) (make-instance 'quote-lexeme))
        (#\; (fo)
             (loop until (or (end-of-buffer-p scan)
                             (end-of-line-p scan)
                             (not (eql (object-after scan) #\;)))
                   do (fo))
             (make-instance 'line-comment-start-lexeme))
        (#\" (fo) (make-instance 'string-start-lexeme))
        (#\` (fo) (make-instance 'backquote-lexeme))
        (#\, (fo)
             (cond ((end-of-buffer-p scan)
                    (make-instance 'incomplete-lexeme))
                   (t
                    (case (object-after scan)
                      (#\@ (fo) (make-instance 'comma-at-lexeme))
                      (#\. (fo) (make-instance 'comma-dot-lexeme))
                      (t (make-instance 'comma-lexeme))))))
        (#\# (fo)
             (cond ((end-of-buffer-p scan)
                    (make-instance 'incomplete-lexeme))
                   (t
                    (let ((prefix 0))
                      (loop until (end-of-buffer-p scan)
                            while (and (characterp (object-after scan))
                                       (digit-char-p (object-after scan)))
                            do (setf prefix
                                     (+ (* 10 prefix)
                                        (digit-char-p (object-after scan))))
                               (fo))
                    (if (or (end-of-buffer-p scan)
                            (not (characterp (object-after scan))))
                        (make-instance 'incomplete-lexeme)
                        (case (object-after scan)
                          ((#\Backspace #\Tab #\Newline #\Linefeed
                                        #\Page #\Return #\Space #\))
                           (fo)
                           (make-instance 'error-lexeme))
                          (#\\ (fo)
                               (cond ((or (end-of-buffer-p scan)
                                          (not (characterp (object-after scan))))
                                      (make-instance 'incomplete-character-lexeme))
                                     ((not (constituentp (object-after scan)))
                                      (fo) (make-instance 'complete-character-lexeme))
                                     (t (loop until (end-of-buffer-p scan)
                                           while (constituentp (object-after scan))
                                           do (fo))
                                        (make-instance 'complete-character-lexeme))))
                          (#\' (fo)
                               (make-instance 'function-lexeme))
                          (#\( (fo)
                               (make-instance 'simple-vector-start-lexeme))
                          (#\* (fo)
                               (loop until (end-of-buffer-p scan)
                                  while (or (eql (object-after scan) #\1)
                                            (eql (object-after scan) #\0))
                                  do (fo))
                               (if (and (not (end-of-buffer-p scan))
                                        (constituentp (object-after scan)))
                                   (make-instance 'error-lexeme)
                                   (make-instance 'bit-vector-form)))
                          (#\: (fo)
                               (make-instance 'uninterned-symbol-lexeme))
                          (#\. (fo)
                               (make-instance 'readtime-evaluation-lexeme))
                          ((#\B #\b #\O #\o #\X #\x)
                           (let ((radix
                                  (ecase (object-after scan)
                                    ((#\B #\b) 2)
                                    ((#\O #\o) 8)
                                    ((#\X #\x) 16))))
                             (fo)
                             (when (and (not (end-of-buffer-p scan))
                                        (char= (object-after scan)
                                               #\-))
                               (fo))
                             (loop until (end-of-buffer-p scan)
                                while (digit-char-p (object-after scan) radix)
                                do (fo)))
                           (if (and (not (end-of-buffer-p scan))
                                    (constituentp (object-after scan)))
                               (make-instance 'error-lexeme)
                               (make-instance 'number-lexeme)))
                          ((#\R #\r)
                           (fo)
                           (cond
                             ((<= 2 prefix 36)
                              (loop until (end-of-buffer-p scan)
                                 while (and (characterp (object-after scan))
                                            (digit-char-p (object-after scan) prefix))
                                 do (fo))
                              (if (and (not (end-of-buffer-p scan))
                                       (constituentp (object-after scan)))
                                  (make-instance 'error-lexeme)
                                  (make-instance 'number-lexeme)))
                             (t (make-instance 'error-lexeme))))
                                        ;((#\C #\c) )
                          ((#\A #\a) (fo)
                           (make-instance 'array-start-lexeme))
                          ((#\S #\s) (fo)
                           (cond ((and (not (end-of-buffer-p scan))
                                       (eql (object-after scan) #\())
                                  (fo)
                                  (make-instance 'structure-start-lexeme))
                                 ((end-of-buffer-p scan)
                                  (make-instance 'incomplete-lexeme))
                                 (t (make-instance 'error-lexeme))))
                          ((#\P #\p) (fo)
                           (make-instance 'pathname-start-lexeme))
                          (#\= (fo)
                               (make-instance 'sharpsign-equals-lexeme))
                          (#\# (fo)
                               (make-instance 'sharpsign-sharpsign-form))
                          (#\+ (fo)
                               (make-instance 'reader-conditional-positive-lexeme))
                          (#\- (fo)
                               (make-instance 'reader-conditional-negative-lexeme))
                          (#\| (fo)
                               (make-instance 'long-comment-start-lexeme))
                          (#\< (fo)
                               (make-instance 'error-lexeme))
                          (t (fo) (make-instance 'undefined-reader-macro-lexeme))))))))
        (#\| (fo) (make-instance 'multiple-escape-start-lexeme))
        (t (cond ((or (constituentp object)
                      (eql object #\\))
                  (lex-token syntax scan))
                 (t (fo) (make-instance 'literal-object-form))))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-list-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
        (#\) (fo) (make-instance 'right-parenthesis-lexeme))
        (t (call-next-method))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-string-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\") (fo) (make-instance 'string-end-lexeme))
            ((eql object #\\)
             (fo)
             (unless (end-of-buffer-p scan)
               (fo))
             (make-instance 'delimiter-lexeme))
            ((constituentp object)
             (loop until (or (end-of-buffer-p scan)
                             (not (constituentp (object-after scan))))
                   do (fo))
             (make-instance 'word-lexeme))
            (t (fo) (make-instance
                     (if (characterp object)
                         'delimiter-lexeme
                         'literal-object-delimiter-lexeme)))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-long-comment-state) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\|)
             (fo)
             (cond ((or (end-of-buffer-p scan)
                        (not (eql (object-after scan) #\#)))
                    (make-instance 'delimiter-lexeme))
                   (t (fo) (make-instance 'comment-end-lexeme))))
            ((eql object #\#)
             (fo)
             (cond ((or (end-of-buffer-p scan)
                        (not (eql (object-after scan) #\|)))
                    (make-instance 'delimiter-lexeme))
                   (t (fo) (make-instance 'long-comment-start-lexeme))))
            ((constituentp object)
             (loop until (or (end-of-buffer-p scan)
                             (not (constituentp (object-after scan))))
                   do (fo))
             (make-instance 'word-lexeme))
            (t (fo) (make-instance
                     (if (characterp object)
                         'delimiter-lexeme
                         'literal-object-delimiter-lexeme)))))))

(defmethod skip-inter ((syntax lisp-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (or (end-of-line-p scan)
                    (not (whitespacep syntax (object-after scan))))
          do (fo)
          finally (return t))))

(defmethod lex ((syntax lisp-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (cond ((end-of-line-p scan)
           (make-instance 'comment-end-lexeme))
          ((constituentp (object-after scan))
           (loop until (or (end-of-buffer-p scan)
                           (not (constituentp (object-after scan))))
                 do (fo))
           (make-instance 'word-lexeme))
          (t (fo) (make-instance
                   (if (characterp (object-before scan))
                       'delimiter-lexeme
                       'literal-object-delimiter-lexeme))))))

(defun lex-token (syntax scan)
  ;; May need more work. Can recognize symbols and numbers. This can
  ;; get very ugly and complicated (out of necessity I believe).
  (let ((could-be-number t)
        sign-seen dot-seen slash-seen nondot-seen number-seen exponent-seen)
    (flet ((fo () (forward-object scan))
           (return-token-or-number-lexeme ()
             (return-from lex-token
               (if (and could-be-number
                        (if exponent-seen
                            nondot-seen t))
                   (if nondot-seen
                       (make-instance 'number-lexeme)
                       (make-instance 'dot-lexeme))
                   (make-instance 'complete-token-lexeme))))
           (this-object ()
             (object-after scan)))
      (tagbody
       START
         (when (end-of-buffer-p scan)
           (return-token-or-number-lexeme))
         (when (constituentp (object-after scan))
           (when (not (eql (this-object) #\.))
             (setf nondot-seen t))
           (cond ((or (eql (this-object) #\+)
                      (eql (this-object) #\-))
                  (when (or sign-seen number-seen slash-seen)
                    (setf could-be-number nil))
                  (setf sign-seen t))
                 ((eql (this-object) #\.)
                  (when (or dot-seen exponent-seen)
                    (setf could-be-number nil))
                  (setf dot-seen t))
                 ((member (this-object)
                          '(#\e #\f #\l #\s #\d)
                          :test #'equalp)
                  (when exponent-seen
                    (setf could-be-number nil))
                  (setf exponent-seen t)
                  (setf number-seen nil)
                  (setf sign-seen nil))
                 ((eql (this-object) #\/)
                  (when (or slash-seen dot-seen exponent-seen)
                    (setf could-be-number nil))
                  (setf slash-seen t))
                 ((not (digit-char-p (this-object)
                                     (base syntax)))
                  (setf could-be-number nil))
                 (t (setf number-seen t)))
           (fo)
           (go START))
         (when (eql (object-after scan) #\\)
           (fo)
           (when (end-of-buffer-p scan)
             (return-from lex-token (make-instance 'incomplete-lexeme)))
           (fo)
           (go START))
         (when (eql (object-after scan) #\|)
           (fo)
           (return-from lex-token (make-instance 'multiple-escape-start-lexeme)))
         (return-token-or-number-lexeme)))))

(defmethod lex ((syntax lisp-syntax) (state lexer-escaped-token-state) scan)
  (let ((bars-seen 0))
    (macrolet ((fo () `(forward-object scan)))
      (flet ((end ()
               (return-from lex
                 (if (oddp bars-seen)
                     (make-instance 'multiple-escape-end-lexeme)
                     (make-instance 'text-lexeme)))))
        (tagbody
         start
           (when (end-of-buffer-p scan)
             (end))
           (when (eql (object-after scan) #\\)
             (fo)
             (when (end-of-buffer-p scan)
               (return-from lex (make-instance 'incomplete-lexeme)))
             (fo)
             (go start))
           (when (eql (object-after scan) #\|)
             (incf bars-seen)
             (fo)
             (go start))
           (if (evenp bars-seen)
               (unless (whitespacep syntax (object-after scan))
                 (fo)
                 (go start))
               (when (constituentp (object-after scan))
                 (fo)
                 (go start)))
           (end))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-error-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (cond ((not (or (end-of-buffer-p scan)
                    (characterp (object-after scan))))
           (fo)
           (make-instance 'literal-object-error-lexeme))
          (t (loop until (end-of-line-p scan)
                do (fo))
             (make-instance 'error-lexeme)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nonterminals

(defclass line-comment (lisp-nonterminal) ())
(defclass long-comment (lisp-nonterminal) ())
(defclass error-symbol (lisp-nonterminal) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defmacro define-lisp-action ((state lexeme) &body body)
  `(defmethod action ((syntax lisp-syntax) (state ,state) (lexeme ,lexeme))
     ,@body))

(defmacro define-new-lisp-state ((state parser-symbol) &body body)
  `(defmethod new-state ((syntax lisp-syntax) (state ,state) (tree ,parser-symbol))
     ,@body))

(define-lisp-action (error-reduce-state (eql nil))
  (throw 'done nil))

;;; the default action for any lexeme is shift
(define-lisp-action (t lisp-lexeme)
  lexeme)

;;; the action on end-of-buffer is to reduce to the error symbol
(define-lisp-action (t (eql nil))
  (reduce-all error-symbol))

;;; the default new state is the error state
(define-new-lisp-state (t parser-symbol) error-state)

;;; the new state when an error-state
(define-new-lisp-state (t error-symbol) error-reduce-state)


;;;;;;;;;;;;;;;; Top-level

#| rules
   form* ->
   form* -> form* form
|#

;;; parse trees
(defclass form* (lisp-nonterminal) ())

(define-parser-state |form* | (lexer-toplevel-state parser-state) ())
(define-parser-state form-may-follow (lexer-toplevel-state parser-state) ())
(define-parser-state |initial-state | (form-may-follow) ())

(define-new-lisp-state (|initial-state | form) |initial-state |)
(define-new-lisp-state (|initial-state | comment) |initial-state |)
;; skip over unmatched right parentheses
(define-new-lisp-state (|initial-state | unmatched-right-parenthesis-lexeme) |initial-state |)

(define-lisp-action (|initial-state | (eql nil))
  (reduce-all form*))

(define-new-lisp-state (|initial-state | form*) |form* | )

(define-lisp-action (|form* | (eql nil))
  (throw 'done nil))

;;;;;;;;;;;;;;;; List

#| rules
   form -> ( form* )
|#

;;; parse trees
(defclass list-form (form) ())
(defclass complete-list-form (list-form complete-form-mixin) ())
(defclass incomplete-list-form (list-form incomplete-form-mixin) ())

(define-parser-state |( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow left-parenthesis-lexeme) |( form* |)
(define-new-lisp-state (|( form* | form) |( form* |)
(define-new-lisp-state (|( form* | comment) |( form* |)
(define-new-lisp-state (|( form* | right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ( form* )
(define-lisp-action (|( form* ) | t)
  (reduce-until-type complete-list-form left-parenthesis-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|( form* | (eql nil))
  (reduce-until-type incomplete-list-form left-parenthesis-lexeme t))

;;;;;;;;;;;;;;;; Cons cell
;; Also (foo bar baz . quux) constructs.
;; (foo bar . baz quux) flagged as an error (too aggressively?).

;;; parse trees
(defclass cons-cell-form (form) ())
(defclass complete-cons-cell-form (cons-cell-form complete-list-form) ())
(defclass incomplete-cons-cell-form (cons-cell-form incomplete-list-form) ())

(define-parser-state |( form* dot-lexeme |
    (lexer-list-state form-may-follow) ())
(define-parser-state |( form* dot-lexeme form |
    (lexer-list-state form-may-follow) ())
(define-parser-state |( form* dot-lexeme form ) |
    (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|( form* | dot-lexeme)
  |( form* dot-lexeme |)
(define-new-lisp-state (|( form* dot-lexeme | form)
  |( form* dot-lexeme form |)
(define-new-lisp-state (|( form* dot-lexeme | comment)
  |( form* dot-lexeme |)
(define-new-lisp-state (|( form* dot-lexeme form | right-parenthesis-lexeme)
  |( form* dot-lexeme form ) |)
(define-new-lisp-state (|( form* dot-lexeme form | comment)
  |( form* dot-lexeme form |)

(define-lisp-action (|( form* dot-lexeme form ) | t)
  (reduce-until-type complete-cons-cell-form left-parenthesis-lexeme))

;;; Reduce at end of buffer.
(define-lisp-action (|( form* dot-lexeme | (eql nil))
  (reduce-until-type incomplete-cons-cell-form left-parenthesis-lexeme t))
(define-lisp-action (|( form* dot-lexeme form | (eql nil))
  (reduce-until-type incomplete-cons-cell-form left-parenthesis-lexeme t))

;;;;;;;;;;;;;;;; Simple Vector

;;; parse trees
(defclass simple-vector-form (list-form) ())
(defclass complete-simple-vector-form (complete-list-form simple-vector-form) ())
(defclass incomplete-simple-vector-form (incomplete-list-form simple-vector-form) ())

(define-parser-state |#( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |#( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow simple-vector-start-lexeme) |#( form* |)
(define-new-lisp-state (|#( form* | form) |#( form* |)
(define-new-lisp-state (|#( form* | comment) |#( form* |)
(define-new-lisp-state (|#( form* | right-parenthesis-lexeme) |#( form* ) |)

;;; reduce according to the rule form -> #( form* )
(define-lisp-action (|#( form* ) | t)
  (reduce-until-type complete-simple-vector-form simple-vector-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#( form* | (eql nil))
  (reduce-until-type incomplete-simple-vector-form simple-vector-start-lexeme t))

;;;;;;;;;;;;;;;; String

;;; parse trees
(defclass string-form (form) ())
(defclass complete-string-form (string-form complete-form-mixin) ())
(defclass incomplete-string-form (string-form incomplete-form-mixin) ())

(define-parser-state |" word* | (lexer-string-state parser-state) ())
(define-parser-state |" word* " | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|" word* | word-lexeme) |" word* |)
(define-new-lisp-state (|" word* | delimiter-lexeme) |" word* |)
(define-new-lisp-state (form-may-follow string-start-lexeme) |" word* |)
(define-new-lisp-state (|" word* | string-end-lexeme) |" word* " |)

;;; reduce according to the rule form -> " word* "
(define-lisp-action (|" word* " | t)
  (reduce-until-type complete-string-form string-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|" word* | (eql nil))
  (reduce-until-type incomplete-string-form string-start-lexeme t))

;;;;;;;;;;;;;;;; Line comment

;;; parse trees
(defclass line-comment-form (comment) ())

(define-parser-state |; word* | (lexer-line-comment-state parser-state) ())
(define-parser-state |; word* NL | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow line-comment-start-lexeme) |; word* |)
(define-new-lisp-state (|; word* | word-lexeme) |; word* |)
(define-new-lisp-state (|; word* | delimiter-lexeme) |; word* |)
(define-new-lisp-state (|; word* | comment-end-lexeme) |; word* NL |)

;;; reduce according to the rule form -> ; word* NL
(define-lisp-action (|; word* NL | t)
  (reduce-until-type line-comment-form line-comment-start-lexeme))

;;;;;;;;;;;;;;;; Long comment

;;; parse trees
(defclass long-comment-form (comment) ())
(defclass complete-long-comment-form (long-comment-form complete-form-mixin) ())
(defclass incomplete-long-comment-form (long-comment-form incomplete-form-mixin) ())

(define-parser-state |#\| word* | (lexer-long-comment-state parser-state) ())
(define-parser-state |#\| word* \|# | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|#\| word* | word-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | delimiter-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | long-comment-start-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | long-comment-form) |#\| word* |)
(define-new-lisp-state (form-may-follow long-comment-start-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | comment-end-lexeme) |#\| word* \|# |)

;;; reduce according to the rule form -> #| word* |#
(define-lisp-action (|#\| word* \|# | t)
  (reduce-until-type complete-long-comment-form long-comment-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#\| word* | (eql nil))
  (reduce-until-type incomplete-long-comment-form long-comment-start-lexeme t))

;;;;;;;;;;;;;;;; Token (number or symbol)

;;; parse trees
(defclass token-form (form token-mixin) ())
(defclass complete-token-form (token-form complete-form-mixin)
  ((%keyword-symbol-p :accessor keyword-symbol-p)
   (%macroboundp :accessor macroboundp)
   (%global-boundp :accessor global-boundp)))
(defclass incomplete-token-form (token-form incomplete-form-mixin) ())

(define-parser-state | complete-lexeme | (lexer-list-state parser-state) ())
(define-parser-state | m-e-start text* | (lexer-escaped-token-state parser-state) ())
(define-parser-state | m-e-start text* m-e-end | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow complete-token-lexeme) | complete-lexeme |)
(define-new-lisp-state (form-may-follow multiple-escape-start-lexeme) | m-e-start text* |)
(define-new-lisp-state (| m-e-start text* | text-lexeme) | m-e-start text* |)
(define-new-lisp-state (| m-e-start text* | multiple-escape-end-lexeme) | m-e-start text* m-e-end |)

;;; reduce according to the rule form -> complete-lexeme
(define-lisp-action (| complete-lexeme | t)
  (reduce-until-type complete-token-form complete-token-lexeme))

;;; reduce according to the rule form -> m-e-start text* m-e-end
(define-lisp-action (| m-e-start text* m-e-end | t)
  (reduce-until-type complete-token-form multiple-escape-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (| m-e-start text* | (eql nil))
  (reduce-until-type incomplete-token-form multiple-escape-start-lexeme t))

;;;;;;;;;;;;;;;; Quote

;;; parse trees
(defclass quote-form (form) ())
(defclass complete-quote-form (quote-form complete-form-mixin) ())
(defclass incomplete-quote-form (quote-form incomplete-form-mixin) ())

(define-parser-state |' | (form-may-follow) ())
(define-parser-state |' form | (lexer-toplevel-state parser-state) ())
(define-parser-state |' incomplete-form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow quote-lexeme) |' |)
(define-new-lisp-state (|' | complete-form-mixin) |' form |)
(define-new-lisp-state (|' | incomplete-form-mixin) |' incomplete-form |)
(define-new-lisp-state (|' | comment) |' |)
(define-new-lisp-state (|' | unmatched-right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ' form
(define-lisp-action (|' form | t)
  (reduce-until-type complete-quote-form quote-lexeme))
(define-lisp-action (|' incomplete-form | t)
  (reduce-until-type incomplete-quote-form quote-lexeme))

(define-lisp-action (|' | right-parenthesis-lexeme)
  (reduce-until-type incomplete-quote-form quote-lexeme))
(define-lisp-action (|' | unmatched-right-parenthesis-lexeme)
  (reduce-until-type incomplete-quote-form quote-lexeme))
(define-lisp-action (|' | (eql nil))
  (reduce-until-type incomplete-quote-form quote-lexeme t))

;;;;;;;;;;;;;;;; Backquote

;;; parse trees
(defclass backquote-form (form) ())
(defclass complete-backquote-form (backquote-form complete-form-mixin) ())
(defclass incomplete-backquote-form (backquote-form incomplete-form-mixin) ())

(define-parser-state |` | (form-may-follow) ())
(define-parser-state |` form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow backquote-lexeme) |` |)
(define-new-lisp-state (|` | complete-form-mixin) |` form |)
(define-new-lisp-state (|` | comment) |` |)
(define-new-lisp-state (|` | unmatched-right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ` form
(define-lisp-action (|` form | t)
  (reduce-until-type complete-backquote-form backquote-lexeme))

(define-lisp-action (|` | right-parenthesis-lexeme)
  (reduce-until-type incomplete-backquote-form backquote-lexeme))
(define-lisp-action (|` | unmatched-right-parenthesis-lexeme)
  (reduce-until-type incomplete-backquote-form backquote-lexeme))
(define-lisp-action (|` | (eql nil))
  (reduce-until-type incomplete-backquote-form backquote-lexeme t))

;;;;;;;;;;;;;;;; Comma

;;; parse trees
(defclass comma-form (form complete-form-mixin) ())
(defclass comma-at-form (form complete-form-mixin) ())
(defclass comma-dot-form (form complete-form-mixin) ())

(define-parser-state |, | (form-may-follow) ())
(define-parser-state |, form | (lexer-toplevel-state parser-state) ())
(define-parser-state |,@ | (form-may-follow) ())
(define-parser-state |,@ form | (lexer-toplevel-state parser-state) ())
(define-parser-state |,. | (form-may-follow) ())
(define-parser-state |,. form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow comma-lexeme) |, |)
(define-new-lisp-state (form-may-follow comma-at-lexeme) |,@ |)
(define-new-lisp-state (form-may-follow comma-dot-lexeme) |,. |)
(define-new-lisp-state (|, | form) |, form |)
(define-new-lisp-state (|, | comment) |, |)
(define-new-lisp-state (|,@ | form) |,@ form |)
(define-new-lisp-state (|,@ | comment) |,@ |)
(define-new-lisp-state (|,. | form) |,. form |)
(define-new-lisp-state (|,. | comment) |,. |)

;;; reduce according to the rule form -> , form
(define-lisp-action (|, form | t)
  (reduce-until-type comma-form comma-lexeme))
(define-lisp-action (|,@ form | t)
  (reduce-until-type comma-at-form comma-at-lexeme))
(define-lisp-action (|,. form | t)
  (reduce-until-type comma-dot-form comma-dot-lexeme))

;;;;;;;;;;;;;;;; Function

;;; parse trees
(defclass function-form (form) ())
(defclass complete-function-form (function-form complete-form-mixin) ())
(defclass incomplete-function-form (function-form incomplete-form-mixin) ())

(define-parser-state |#' | (form-may-follow) ())
(define-parser-state |#' form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow function-lexeme) |#' |)
(define-new-lisp-state (|#' | complete-form-mixin) |#' form |)
(define-new-lisp-state (|#' | comment) |#' |)

;;; reduce according to the rule form -> #' form
(define-lisp-action (|#' form | t)
  (reduce-until-type complete-function-form function-lexeme))
(define-lisp-action (|#' | unmatched-right-parenthesis-lexeme)
  (reduce-until-type incomplete-function-form function-lexeme))
(define-lisp-action (|#' | (eql nil))
  (reduce-until-type incomplete-function-form function-lexeme t))

;;;;;;;;;;;;;;;; Reader conditionals

;;; parse trees
(defclass reader-conditional-form (form)
  ((%conditional-true-p :accessor conditional-true-p)))
(defclass reader-conditional-positive-form (reader-conditional-form) ())
(defclass reader-conditional-negative-form (reader-conditional-form) ())

(define-parser-state |#+ | (form-may-follow) ())
(define-parser-state |#+ form | (form-may-follow) ())
(define-parser-state |#+ form form | (lexer-toplevel-state parser-state) ())
(define-parser-state |#- | (form-may-follow) ())
(define-parser-state |#- form | (form-may-follow) ())
(define-parser-state |#- form form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow reader-conditional-positive-lexeme) |#+ |)
(define-new-lisp-state (|#+ | form) |#+ form |)
(define-new-lisp-state (|#+ form | form) |#+ form form |)
(define-new-lisp-state (|#+ | comment) |#+ |)
(define-new-lisp-state (|#+ form | comment) |#+ form |)
(define-new-lisp-state (form-may-follow reader-conditional-negative-lexeme) |#- |)
(define-new-lisp-state (|#- | form) |#- form |)
(define-new-lisp-state (|#- form | form) |#- form form |)
(define-new-lisp-state (|#- | comment) |#- |)
(define-new-lisp-state (|#- form | comment) |#- form |)

(define-lisp-action (|#+ form form | t)
  (reduce-until-type reader-conditional-positive-form reader-conditional-positive-lexeme))

(define-lisp-action (|#- form form | t)
  (reduce-until-type reader-conditional-negative-form reader-conditional-negative-lexeme))

;;;;;;;;;;;;;;;; uninterned symbol

;;; parse trees
(defclass uninterned-symbol-form (complete-token-form) ())

(define-parser-state |#: | (form-may-follow) ())
(define-parser-state |#: form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow uninterned-symbol-lexeme) |#: |)
(define-new-lisp-state (|#: | form) |#: form |)

;;; reduce according to the rule form -> #: form
(define-lisp-action (|#: form | t)
  (reduce-fixed-number uninterned-symbol-form 2))

;;;;;;;;;;;;;;;; readtime evaluation

;;; parse trees
(defclass readtime-evaluation-form (form complete-form-mixin) ())

(define-parser-state |#. | (form-may-follow) ())
(define-parser-state |#. form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow readtime-evaluation-lexeme) |#. |)
(define-new-lisp-state (|#. | form) |#. form |)
(define-new-lisp-state (|#. | comment) |#. |)

;;; reduce according to the rule form -> #. form
(define-lisp-action (|#. form | t)
  (reduce-until-type readtime-evaluation-form readtime-evaluation-lexeme))

;;;;;;;;;;;;;;;; sharpsign equals

;;; parse trees
(defclass sharpsign-equals-form (form complete-form-mixin) ())

(define-parser-state |#= | (form-may-follow) ())
(define-parser-state |#= form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow sharpsign-equals-lexeme) |#= |)
(define-new-lisp-state (|#= | form) |#= form |)
(define-new-lisp-state (|#= | comment) |#= |)

;;; reduce according to the rule form -> #= form
(define-lisp-action (|#= form | t)
  (reduce-until-type sharpsign-equals-form sharpsign-equals-lexeme))

;;;;;;;;;;;;;;;; array

;;; parse trees
(defclass array-form (form complete-form-mixin) ())

(define-parser-state |#A | (form-may-follow) ())
(define-parser-state |#A form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow array-start-lexeme) |#A |)
(define-new-lisp-state (|#A | form) |#A form |)
(define-new-lisp-state (|#A | comment) |#A |)

;;; reduce according to the rule form -> #A form
(define-lisp-action (|#A form | t)
  (reduce-until-type array-form array-start-lexeme))

;;;;;;;;;;;;;;;; structure

;;; parse trees
(defclass structure-form (list-form) ())
(defclass complete-structure-form (complete-list-form) ())
(defclass incomplete-structure-form (incomplete-list-form) ())

(define-parser-state |#S( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |#S( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow structure-start-lexeme) |#S( form* |)
(define-new-lisp-state (|#S( form* | form) |#S( form* |)
(define-new-lisp-state (|#S( form* | right-parenthesis-lexeme) |#S( form* ) |)

;;; reduce according to the rule form -> #S( form* )
(define-lisp-action (|#S( form* ) | t)
  (reduce-until-type complete-structure-form structure-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#S( form* | (eql nil))
  (reduce-until-type incomplete-structure-form structure-start-lexeme t))


;;;;;;;;;;;;;;;; pathname

;;; NB: #P need not be followed by a string,
;;;  as it could be followed by a #. construct instead (or some other reader macro)

;;; parse trees
(defclass pathname-form (form) ())
(defclass complete-pathname-form (pathname-form complete-form-mixin) ())
(defclass incomplete-pathname-form (pathname-form incomplete-form-mixin) ())

(define-parser-state |#P | (form-may-follow) ())
(define-parser-state |#P form | (lexer-toplevel-state parser-state) ())
(define-parser-state |#P incomplete-form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow pathname-start-lexeme) |#P |)
(define-new-lisp-state (|#P | complete-form-mixin) |#P form |)
(define-new-lisp-state (|#P | incomplete-form-mixin) |#P incomplete-form |)
(define-new-lisp-state (|#P | comment) |#P |)

;;; reduce according to the rule form -> #P form
(define-lisp-action (|#P form | t)
  (reduce-until-type complete-pathname-form pathname-start-lexeme))
(define-lisp-action (|#P incomplete-form | t)
  (reduce-until-type incomplete-pathname-form pathname-start-lexeme))
(define-lisp-action (|#P | (eql nil))
  (reduce-until-type incomplete-pathname-form pathname-start-lexeme t))

;;;;;;;;;;;;;;;; undefined reader macro

;;; parse trees
(defclass undefined-reader-macro-form (form complete-form-mixin) ())

(define-parser-state |#<other> | (form-may-follow) ())
(define-parser-state |#<other> form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow undefined-reader-macro-lexeme) |#<other> |)
(define-new-lisp-state (|#<other> | form) |#<other> form |)

;;; reduce according to the rule form -> #<other> form
(define-lisp-action (|#<other> form | t)
  (reduce-fixed-number undefined-reader-macro-form 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defun package-at-mark (syntax mark-or-offset)
  "Get the specified Lisp package for the syntax. First, an
attempt will be made to find the package specified in
the (in-package) preceding `mark-or-offset'. If none can be
found, return the package specified in the attribute list. If no
package can be found at all, or the otherwise found packages are
invalid, return the value of `*package*'."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let* ((designator (rest (find offset (package-list syntax)
                              :key #'first
                              :test #'>=))))
      (or (handler-case (find-package designator)
            (type-error ()
              nil))
          (let ((osp (option-specified-package syntax)))
            (typecase osp
              (package osp)
              (string (find-package osp))))
          (find-package (option-specified-package syntax))
          *package*))))

(defun provided-package-name-at-mark (syntax mark-or-offset)
  "Get the name of the specified Lisp package for the
syntax. This will return a normalised version of
whatever (in-package) form precedes `mark-or-offset', even if the
package specified in that form does not exist. If no (in-package)
form can be found, return the package specified in the attribute
list. If no such package is specified, return \"CLIM-USER\"."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (flet ((normalise (designator)
             (typecase designator
               (symbol
                (symbol-name designator))
               (string
                designator)
               (package
                (package-name designator)))))
      (let* ((designator (rest (find offset (package-list syntax)
                                :key #'first
                                :test #'>=))))
        (normalise (or designator
                       (option-specified-package syntax)
                       :clim-user))))))

(defmacro with-syntax-package ((syntax offset) &body
                               body)
  "Evaluate `body' with `*package*' bound to a valid package,
  preferably taken from `syntax' based on `offset'."
  `(let ((*package* (package-at-mark ,syntax ,offset)))
     ,@body))

(defun need-to-update-package-list-p (prefix-size suffix-size syntax)
  (let ((low-mark-offset prefix-size)
        (high-mark-offset (- (size (buffer syntax)) suffix-size)))
    (flet ((test (x)
             (let ((start-offset (start-offset x))
                   (end-offset (end-offset x)))
              (when (and (or (<= start-offset
                                 low-mark-offset
                                 end-offset
                                 high-mark-offset)
                             (<= low-mark-offset
                                 start-offset
                                 high-mark-offset
                                 end-offset)
                             (<= low-mark-offset
                                 start-offset
                                 end-offset
                                 high-mark-offset)
                             (<= start-offset
                                 low-mark-offset
                                 high-mark-offset
                                 end-offset))
                         (typep x 'complete-list-form))
                (let ((candidate (first-form (children x))))
                  (and (form-token-p candidate)
                       (eq (form-to-object syntax candidate
                                            :no-error t)
                           'cl:in-package)))))))
      (with-slots (stack-top) syntax
        (or (not (slot-boundp syntax '%package-list))
            (loop
               for child in (children stack-top)
               when (test child)
               do (return t))
            (loop
               for (offset . nil) in (package-list syntax)
               unless (and (>= (size (buffer syntax)) offset)
                           (form-list-p (form-around syntax offset)))
               do (return t)))))))

(defun update-package-list (syntax)
  (setf (package-list syntax) nil)
  (flet ((test (x)
           (when (form-list-p x)
             (let ((candidate (first-form (children x))))
               (and (form-token-p candidate)
                    (eq (form-to-object syntax candidate
                                         :no-error t)
                        'cl:in-package)
                    (second-form (children x))))))
         (extract (x)
           (let ((designator (second-form (children x))))
             (form-to-object syntax designator
                              :no-error t))))
    (with-slots (stack-top) syntax
      (loop for child in (children stack-top)
         when (test child)
         do (push (cons (end-offset child)
                        (extract child))
                  (package-list syntax))))))

(defmethod update-syntax :after ((syntax lisp-syntax) prefix-size suffix-size
                                 &optional begin end)
  (declare (ignore begin end))
  (setf (form-before-cache syntax) (make-hash-table :test #'equal)
        (form-after-cache syntax) (make-hash-table :test #'equal)
        (form-around-cache syntax) (make-hash-table :test #'equal))
  (when (need-to-update-package-list-p prefix-size suffix-size syntax)
    (update-package-list syntax)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; accessing parser forms

(defun first-noncomment (list)
  "Returns the first non-comment in list."
  (find-if-not #'comment-p list))

(defun rest-noncomments (list)
  "Returns the remainder of the list after the first non-comment,
stripping leading comments."
  (loop for rest on list
        count (not (comment-p (car rest)))
          into forms
        until (= forms 2)
        finally (return rest)))

(defun nth-noncomment (n list)
  "Returns the nth non-comment in list."
  (loop for item in list
        count (not (comment-p item))
          into forms
        until (> forms n)
        finally (return item)))

(defun elt-noncomment (list n)
  "Returns the nth non-comment in list."
  (nth-noncomment n list))

(defun second-noncomment (list)
  "Returns the second non-comment in list."
  (nth-noncomment 1 list))

(defun third-noncomment (list)
  "Returns the third non-comment in list."
  (nth-noncomment 2 list))

(defun rest-forms (list)
  "Returns the remainder of the list after the first form,
stripping leading non-forms."
  (loop for rest on list
     count (formp (car rest))
       into forms
     until (= forms 2)
     finally (return rest)))

(defun nth-form (n list)
  "Returns the nth form in list or `nil'."
  (loop for item in list
     count (formp item)
       into forms
     until (> forms n)
     finally (when (> forms n)
               (return item))))

(defun elt-form (list n)
  "Returns the nth form in list or `nil'."
  (nth-form n list))

(defun first-form (list)
  "Returns the first form in list."
  (nth-form 0 list))

(defun second-form (list)
  "Returns the second form in list."
  (nth-form 1 list))

(defun third-form (list)
  "Returns the third formw in list."
  (nth-form 2 list))

(defun form-children (form)
  "Return the children of `form' that are themselves forms."
  (remove-if-not #'formp (children form)))

(defgeneric form-operator (form)
  (:documentation "Return the operator of `form' as a
token. Returns nil if none can be found.")
  (:method (form) nil))

(defmethod form-operator ((form list-form))
  (first-form (rest (children form))))

(defmethod form-operator ((form complete-quote-form))
  (first-form (rest (children (second (children form))))))

(defmethod form-operator ((form complete-backquote-form))
  (first-form (rest (children (second (children form))))))

(defgeneric form-operands (form)
  (:documentation "Returns the operands of `form' as a list of
  tokens. Returns nil if none can be found.")
  (:method (syntax) nil))

(defmethod form-operands ((form list-form))
  (remove-if-not #'formp (rest-forms (children form))))

(defun form-toplevel (syntax form)
  "Return the top-level form of `form'."
  (if (null (parent (parent form)))
      form
      (form-toplevel syntax (parent form))))

(defgeneric form-operator-p (syntax token)
  (:documentation "Return true if `token' is the operator of its
  form. Otherwise, return nil.")
  (:method ((syntax lisp-syntax) (token lisp-lexeme))
    (with-accessors ((pre-token preceding-parse-tree)) token
      (cond ((typep pre-token 'left-parenthesis-lexeme)
             t)
            ((comment-p pre-token)
             (form-operator-p syntax pre-token))
            (t nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful functions for selecting forms based on the mark.

(defun expression-at-mark (syntax mark-or-offset)
  "Return the form closest to `mark-or-offset'."
  (as-offsets ((offset mark-or-offset))
    (flet ((distance (form)
             (min (abs (- (start-offset form) offset))
                  (abs (- (end-offset form) offset)))))
      (reduce #'(lambda (form1 form2)
                  (cond ((null form1) form2)
                        ((null form2) form1)
                        ((> (distance form1) (distance form2))
                         form2)
                        (t form1)))
              (list (form-around syntax offset)
                    (form-after syntax offset)
                    (form-before syntax offset))))))

(defun definition-at-mark (syntax mark-or-offset)
  "Return the top-level form at `mark-or-offset'. If `mark-or-offset' is just after,
or inside, a top-level-form, or if there are no forms after
`mark-or-offset', the top-level-form preceding `mark-or-offset'
is returned. Otherwise, the top-level-form following
`mark-or-offset' is returned."
  (form-toplevel syntax (expression-at-mark syntax mark-or-offset)))

(defun form-of-type-at-mark (syntax mark-or-offset test)
  "Return the form that `mark-or-offset' is inside and for which
`test' returns true, or NIL if no such form exists."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let ((form-around (form-around syntax offset)))
      (when form-around
        (if (and (funcall test form-around)
                 (> offset (start-offset form-around)))
            form-around
            (find-list-parent form-around))))))

(defun list-at-mark (syntax mark-or-offset)
  "Return the list form that `mark-or-offset' is inside, or NIL
if no such form exists."
  (form-of-type-at-mark syntax mark-or-offset #'form-list-p))

(defun symbol-at-mark (syntax mark-or-offset
                       &optional (form-fetcher 'expression-at-mark))
  "Return a symbol token at `mark-or-offset'. This function will
\"unwrap\" quote-forms in order to return the symbol token. If no
symbol token can be found, NIL will be returned. `Form-fetcher'
must be a function with the same signature as `expression-at-mark', and
will be used to retrieve the initial form at `mark'."
  (as-offsets (mark-or-offset)
    (let ((unwrapped-form (fully-unquoted-form
                           (funcall form-fetcher syntax mark-or-offset))))
      (when (form-token-p unwrapped-form)
        unwrapped-form))))

(defun fully-quoted-form (token)
  "Return the top token object for `token', return `token' or the
top quote-form that `token' is buried in. "
  (labels ((ascend (form)
             (cond ((form-quoted-p (parent form))
                    (ascend (parent form)))
                   (t form))))
    (ascend token)))

(defun fully-unquoted-form (token)
  "Return the bottom token object for `token', return `token' or
the form that `token' quotes, peeling away all quote forms."
  (labels ((descend (form)
             (cond ((and (form-quoted-p form)
                         (rest (children form)))
                    (descend (first-form (children form))))
                   (t form))))
    (descend token)))

(defun this-form (syntax mark-or-offset)
  "Return a form at `mark-or-offset'. This function defines which
  forms the COM-FOO-this commands affect."
  (as-offsets ((offset mark-or-offset))
    (or (form-around syntax offset)
        (form-before syntax offset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Querying forms for data

(defmacro define-form-predicate (name (&rest t-classes) &optional documentation)
  "Define a generic function named `name', taking a single
  argument. A default method that returns NIL will be defined,
  and methods returning T will be defined for all classes in
  `t-classes'."
  `(progn
     (defgeneric ,name (form)
       (:documentation ,(or documentation "Check `form' for something."))
       (:method (form) nil))
     ,@(loop for class in t-classes collecting
            `(defmethod ,name ((form ,class))
               t))))

(define-form-predicate formp (form))
(define-form-predicate form-list-p (complete-list-form incomplete-list-form))
(define-form-predicate form-incomplete-p (incomplete-form-mixin))
(define-form-predicate form-complete-p (complete-form-mixin))
(define-form-predicate form-token-p (token-mixin))
(define-form-predicate form-string-p (string-form))
(define-form-predicate form-quoted-p (quote-form backquote-form))
(define-form-predicate form-comma-p (comma-form))
(define-form-predicate form-comma-at-p (comma-at-form))
(define-form-predicate form-comma-dot-p (comma-dot-form))
(define-form-predicate form-character-p (complete-character-lexeme
                                         incomplete-character-lexeme))
(define-form-predicate form-simple-vector-p (simple-vector-form))

(define-form-predicate comment-p (comment))
(define-form-predicate line-comment-p (line-comment-form))
(define-form-predicate long-comment-p (long-comment-form))

(defgeneric form-at-top-level-p (form)
  (:documentation "Return NIL if `form' is not a top-level-form,
  T otherwise.")
  (:method ((form parser-symbol))
    (or (typep (parent form) 'form*)
        (null (parent form)))))

(defgeneric eval-feature-conditional (conditional-form syntax))

(defmethod eval-feature-conditional (conditional-form (syntax lisp-syntax))
  nil)

;; Adapted from slime.el

(defmethod eval-feature-conditional ((conditional token-mixin) (syntax lisp-syntax))
  (let* ((string (form-string syntax conditional))
         (symbol (parse-symbol string :package +keyword-package+)))
    (member symbol *features*)))

(defmethod eval-feature-conditional ((conditional list-form) (syntax lisp-syntax))
  (let ((children (children conditional)))
    (when (third-noncomment children)
      (flet ((eval-fc (conditional)
               (funcall #'eval-feature-conditional conditional syntax)))
        (let* ((type (second-noncomment children))
               (conditionals  (butlast
                               (nthcdr
                                2
                                (remove-if
                                 #'comment-p
                                 children))))
               (type-string (form-string syntax type))
               (type-symbol (parse-symbol type-string :package +keyword-package+)))
          (case type-symbol
            (:and (funcall #'every #'eval-fc conditionals))
            (:or (funcall #'some #'eval-fc conditionals))
            (:not (when conditionals
                    (funcall #'(lambda (f l) (not (apply f l)))
                             #'eval-fc conditionals)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Asking about parse state at some point

(defun in-type-p-in-children (children offset type)
  (loop for child in children
     do (cond ((<= (start-offset child) offset (end-offset child))
               (return (if (typep child type)
                           child
                           (in-type-p-in-children (children child) offset type))))
              ((<= offset (start-offset child))
               (return nil))
              (t nil))))

(defun in-type-p (syntax mark-or-offset type)
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (with-slots (stack-top) syntax
      (if (or (null (start-offset stack-top))
              (> offset (end-offset stack-top))
              (< offset (start-offset stack-top)))
          nil
          (in-type-p-in-children (children stack-top) offset type)))))

(defun in-string-p (syntax mark-or-offset)
  "Return true if `mark-or-offset' is inside a Lisp string."
  (as-offsets ((offset mark-or-offset))
    (let ((string (in-type-p syntax offset 'string-form)))
      (and string
           (< (start-offset string) offset)
           (< offset (end-offset string))))))

(defun in-comment-p (syntax mark-or-offset)
  "Return true if `mark-or-offset' is inside a Lisp
comment (line-based or long form)."
  (as-offsets ((offset mark-or-offset))
    (let ((comment (in-type-p syntax mark-or-offset 'comment)))
      (and comment
           (or (when (typep comment 'line-comment-form)
                 (< (start-offset comment) offset))
               (when (typep comment 'complete-long-comment-form)
                 (< (1+ (start-offset comment) ) offset
                    (1- (end-offset comment))))
               (when (typep comment 'incomplete-long-comment-form)
                 (< (1+ (start-offset comment)) offset)))))))

(defun in-line-comment-p (syntax mark-or-offset)
  "Return true if `mark-or-offset' is inside a Lisp line
comment."
  (as-offsets ((offset mark-or-offset))
    (let ((comment (in-type-p syntax mark-or-offset 'line-comment-form)))
      (when comment
        (< (start-offset comment) offset)))))

(defun in-long-comment-p (syntax mark-or-offset)
  "Return true if `mark-or-offset' is inside a Lisp
long comment."
  (as-offsets ((offset mark-or-offset))
    (let ((comment (in-type-p syntax mark-or-offset 'long-comment-form)))
      (and comment
           (or (if (typep comment 'complete-long-comment-form)
                   (< (1+ (start-offset comment)) offset
                      (1- (end-offset comment)))
                   (< (1+ (start-offset comment)) offset)))))))

(defun in-character-p (syntax mark-or-offset)
  "Return true if `mark-or-offset' is inside a Lisp character lexeme."
  (as-offsets ((offset mark-or-offset))
    (let ((form (form-around syntax offset)))
      (typecase form
        (complete-character-lexeme
         (> (end-offset form) offset (+ (start-offset form) 1)))
        (incomplete-character-lexeme
         (= offset (end-offset form)))))))

(defgeneric at-beginning-of-form-p (syntax form offset)
  (:documentation "Return true if `offset' is at the beginning of
the list-like `form', false otherwise. \"Beginning\" is defined
at the earliest point the contents could be entered, for example
right after the opening parenthesis for a list.")
  (:method ((syntax lisp-syntax) (form form) (offset integer))
    nil)
  (:method :before ((syntax lisp-syntax) (form form) (offset integer))
   (update-parse syntax)))

(defgeneric at-end-of-form-p (syntax form offset)
  (:documentation "Return true if `offset' is at the end of the
list-like `form', false otherwise.")
  (:method ((syntax lisp-syntax) (form form) (offset integer))
    nil)
  (:method :before ((syntax lisp-syntax) (form form) (offset integer))
   (update-parse syntax)))

(defmethod at-beginning-of-form-p ((syntax lisp-syntax) (form list-form)
                                   (offset integer))
  (= offset (1+ (start-offset form))))

(defmethod at-end-of-form-p ((syntax lisp-syntax) (form list-form)
                             (offset integer))
  (= offset (1- (end-offset form))))

(defmethod at-beginning-of-form-p ((syntax lisp-syntax) (form string-form)
                                   (offset integer))
  (= offset (1+ (start-offset form))))

(defmethod at-end-of-form-p ((syntax lisp-syntax) (form string-form)
                             (offset integer))
  (= offset (1- (end-offset form))))

(defmethod at-beginning-of-form-p ((syntax lisp-syntax) (form simple-vector-form)
                                   (offset integer))
  (= offset (+ 2 (start-offset form))))

(defmethod at-end-of-form-p ((syntax lisp-syntax) (form simple-vector-form)
                             (offset integer))
  (= offset (1- (end-offset form))))

(defun location-at-beginning-of-form (syntax mark-or-offset)
  "Return true if the position `mark-or-offset' is at the
beginning of some structural form, false otherwise. \"Beginning\"
is defined by what type of form is at `mark-or-offset', but for a
list form, it would be right after the opening parenthesis."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let ((form-around (form-around syntax offset)))
      (when form-around
        (labels ((recurse (form)
                   (or (at-beginning-of-form-p syntax form offset)
                       (unless (form-at-top-level-p form)
                         (recurse (parent form))))))
          (recurse form-around))))))

(defun location-at-end-of-form (syntax mark-or-offset)
  "Return true if the position `mark-or-offset' is at the
end of some structural form, false otherwise. \"End\"
is defined by what type of form is at `mark-or-offset', but for a
list form, it would be right before the closing parenthesis."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let ((form-around (form-around syntax offset)))
      (when form-around
        (labels ((recurse (form)
                   (or (at-end-of-form-p syntax form offset)
                       (unless (form-at-top-level-p form)
                         (recurse (parent form))))))
          (recurse form-around))))))

(defun at-beginning-of-list-p (syntax mark-or-offset)
  "Return true if the position `mark-or-offset' is at the
beginning of a list-like form, false otherwise. \"Beginning\" is
defined as the earliest point the contents could be entered, for
example right after the opening parenthesis for a list."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let ((form-around (form-around syntax offset)))
      (when (form-list-p form-around)
        (at-beginning-of-form-p syntax form-around offset)))))

(defun at-end-of-list-p (syntax mark-or-offset)
  "Return true if the position `mark-or-offset' is at the end of
a list-like form, false otherwise. \"End\" is defined as the
latest point the contents could be entered, for example right
before the closing parenthesis for a list."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let ((form-around (form-around syntax offset)))
      (when (form-list-p form-around)
        (at-end-of-form-p syntax (form-around syntax offset) offset)))))

(defun at-beginning-of-string-p (syntax mark-or-offset)
  "Return true if the position `mark-or-offset' is at the
beginning of a string form, false otherwise. \"Beginning\" is
right after the opening double-quote."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let ((form-around (form-around syntax offset)))
      (when (form-string-p form-around)
        (at-beginning-of-form-p syntax form-around offset)))))

(defun at-end-of-string-p (syntax mark-or-offset)
  "Return true if the position `mark-or-offset' is at the end of
a list-like form, false otherwise. \"End\" is right before the
ending double-quote."
  (as-offsets ((offset mark-or-offset))
    (update-parse syntax)
    (let ((form-around (form-around syntax offset)))
      (when (form-string-p form-around)
        (at-end-of-form-p syntax form-around offset)))))

(defun at-beginning-of-children-p (form mark-or-offset)
  "Return true if `mark-or-offset' structurally is at the
beginning of (precedes) the children of `form'. True if `form'
has no children."
  (as-offsets ((offset mark-or-offset))
    (let ((first-child (first (form-children form))))
      (and (null first-child)
           (>= (start-offset first-child) offset)))))

(defun at-end-of-children-p (form mark-or-offset)
  "Return true if `mark-or-offset' structurally is at the end
of (is preceded by) the children of `form'. True if `form' has no
children."
  (as-offsets ((offset mark-or-offset))
    (let ((last-child (first (last (form-children form)))))
      (or (null last-child)
          (>= offset (end-offset last-child))))))

(defun structurally-at-beginning-of-list-p (syntax mark-or-offset)
  "Return true if `mark-or-offset' structurally is at the
beginning of (precedes) the children of the enclosing list. False
if there is no enclosing list. True if the list has no children."
  (as-offsets ((offset mark-or-offset))
    (let ((enclosing-list (list-at-mark syntax offset)))
      (and enclosing-list (at-beginning-of-children-p enclosing-list offset)))))

(defun structurally-at-end-of-list-p (syntax mark-or-offset)
  "Return true if `mark-or-offset' structurally is at the end
of (is preceded by) the children of the enclosing list. False if
there is no enclosing list. True of the list has no children."
  (as-offsets ((offset mark-or-offset))
    (let ((enclosing-list (list-at-mark syntax offset)))
      (and enclosing-list (at-end-of-children-p enclosing-list offset)))))

(defun comment-at-mark (syntax mark-or-offset)
  "Return the comment at `mark-or-offset'."
  (in-type-p syntax mark-or-offset 'comment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful functions for modifying forms based on the mark.

(defgeneric replace-symbol-at-mark (syntax mark string)
  (:documentation "Replace the symbol around `mark' with `string'
and move `mark' to after `string'. If there is no symbol at
`mark', insert `string' and move `mark' anyway."))

(defmethod replace-symbol-at-mark ((syntax lisp-syntax) (mark mark)
                                   (string string))
  (let ((token (symbol-at-mark syntax mark)))
    (when token
      (setf (offset mark) (start-offset token))
      (forward-delete-expression mark syntax))
    (insert-sequence mark string)))

(defmethod replace-symbol-at-mark :after ((syntax lisp-syntax)
                                          (mark left-sticky-mark) (string string))
  (forward-object mark (length string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defun cache-symbol-info (syntax symbol-form)
  "Cache information about the symbol `symbol-form' represents,
so that it can be quickly looked up later."
  ;; We don't use `form-to-object' as we want to retrieve information
  ;; even about symbol that are not interned.
  (multiple-value-bind (symbol package)
      (parse-symbol (form-string syntax symbol-form)
       :package (package-at-mark syntax (start-offset symbol-form)))
    (setf (keyword-symbol-p symbol-form) (eq package +keyword-package+)
          (macroboundp symbol-form) (when (eq (first-form (children (parent symbol-form)))
                                              symbol-form)
                                      (or (special-operator-p symbol)
                                          (macro-function symbol)))
          (global-boundp symbol-form) (and (boundp symbol)
                                           (not (constantp symbol))))))

(defun symbol-form-is-keyword-p (syntax symbol-form)
  "Return true if `symbol-form' represents a keyword symbol."
  (if (slot-boundp symbol-form '%keyword-symbol-p)
      (keyword-symbol-p symbol-form)
      (progn (cache-symbol-info syntax symbol-form)
             (keyword-symbol-p symbol-form))))

(defun symbol-form-is-macrobound-p (syntax symbol-form)
  "Return true if `symbol-form' represents a symbol bound to a
macro or special form."
  (if (slot-boundp symbol-form '%macroboundp)
      (macroboundp symbol-form)
      (progn (cache-symbol-info syntax symbol-form)
             (macroboundp symbol-form))))

(defun symbol-form-is-boundp (syntax symbol-form)
  "Return true if `symbol-form' represents a symbol that is
`boundp' and is not a constant."
  (if (slot-boundp symbol-form '%global-boundp)
      (global-boundp symbol-form)
      (progn (cache-symbol-info syntax symbol-form)
             (global-boundp symbol-form))))

(defun cache-conditional-info (syntax form)
  "Cache information about the reader conditional `symbol-form' represents,
so that it can be quickly looked up later."
  (setf (conditional-true-p form)
        (eval-feature-conditional (second-noncomment (children form)) syntax)))

(defun reader-conditional-true (syntax form)
  "Return true if the reader conditional `form' has a true
condition."
  (if (slot-boundp form '%conditional-true-p)
      (conditional-true-p form)
      (progn (cache-conditional-info syntax form)
             (conditional-true-p form))))

(defun parenthesis-highlighter (view form)
  "Return the drawing style with which the parenthesis lexeme
`form' should be highlighted."
  (if (and (typep view 'point-mark-view)
           (active view)
           (or (mark= (point view) (start-offset (parent form)))
               (mark= (point view) (end-offset (parent form))))
           (form-complete-p (parent form)))
      +bold-face-drawing-options+
      +default-drawing-options+))

(defun reader-conditional-rule-fn (positive comment-options)
  "Return a function for use as a syntax highlighting
rule-generator for reader conditionals. If `positive', the
function will be for positive
reader-conditionals. `Comment-options' is the drawing options
object that will be returned when the conditional is not
fulfilled."
  (if positive
      #'(lambda (view form)
          (if (reader-conditional-true (syntax view) form)
              +default-drawing-options+
              (values comment-options t)))
      #'(lambda (view form)
          (if (not (reader-conditional-true (syntax view) form))
              +default-drawing-options+
              (values comment-options t)))))

(define-syntax-highlighting-rules emacs-style-highlighting
  (error-lexeme (*error-drawing-options*))
  (string-form (*string-drawing-options*))
  (comment (*comment-drawing-options*))
  (literal-object-form (:options :function (object-drawer)))
  (complete-token-form (:function #'(lambda (view form)
                                      (cond ((symbol-form-is-keyword-p (syntax view) form)
                                             *keyword-drawing-options*)
                                            ((symbol-form-is-macrobound-p (syntax view) form)
                                             *special-operator-drawing-options*)
                                            ((symbol-form-is-boundp (syntax view) form)
                                             *special-variable-drawing-options*)
                                            (t +default-drawing-options+)))))
  (parenthesis-lexeme (:function #'parenthesis-highlighter))
  (reader-conditional-positive-form
   (:function (reader-conditional-rule-fn t *comment-drawing-options*)))
  (reader-conditional-negative-form
   (:function (reader-conditional-rule-fn nil *comment-drawing-options*))))

(defvar *retro-comment-drawing-options*
  (make-drawing-options :face (make-face :ink +dimgray+))
  "The drawing options used for retro-highlighting in Lisp syntax.")

(define-syntax-highlighting-rules retro-highlighting
  (error-symbol (*error-drawing-options*))
  (string-form (:options :face +italic-face+))
  (comment (*retro-comment-drawing-options*))
  (literal-object-form (:options :function (object-drawer)))
  (complete-token-form (:function #'(lambda (view form)
                                      (cond ((symbol-form-is-macrobound-p (syntax view) form)
                                             +bold-face-drawing-options+)
                                            (t +default-drawing-options+)))))
  (reader-conditional-positive-form
   (:function (reader-conditional-rule-fn t *retro-comment-drawing-options*)))
  (reader-conditional-negative-form
   (:function (reader-conditional-rule-fn nil *retro-comment-drawing-options*)))
  (parenthesis-lexeme (:function #'parenthesis-highlighter)))

(defparameter *syntax-highlighting-rules* 'emacs-style-highlighting
  "The syntax highlighting rules used for highlighting Lisp
syntax.")

(defmethod syntax-highlighting-rules ((syntax lisp-syntax))
  *syntax-highlighting-rules*)

(defmethod invalidate-strokes ((view textual-drei-syntax-view) (syntax lisp-syntax))
  ;; Invalidate the area touched by parenthesis highlighting, if
  ;; applicable. Cheap test to do coarse elimination...
  (when (or (and (not (end-of-buffer-p (point view)))
                 (equal (object-after (point view)) #\())
            (and (not (beginning-of-buffer-p (point view)))
                 (equal (object-before (point view)) #\))))
    ;; Might still be a fake match, so do the semiexpensive proper test.
    (let ((form (form-around syntax (offset (point view)))))
      (when form
        (let ((start-offset (start-offset form))
              (end-offset (end-offset form)))
          (when (or (mark= start-offset (point view))
                    (mark= end-offset (point view)))
            ;; We actually have parenthesis highlighting.
            (list (cons start-offset (1+ start-offset))
                  (cons (1- end-offset) end-offset))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse

(defun form-before-in-children (syntax children offset)
  (update-parse syntax)
  (loop for (first . rest) on children
     if (formp first)
     do
     (cond ((< (start-offset first) offset (end-offset first))
            (return (if (null (children first))
                        nil
                        (form-before-in-children syntax (children first) offset))))
           ((and (>= offset (end-offset first))
                 (or (null (first-form rest))
                     (<= offset (start-offset (first-form rest)))))
            (return (let ((potential-form
                           (form-before-in-children syntax (children (fully-unquoted-form first)) offset)))
                      (if (not (null potential-form))
                          (if (or (<= (end-offset first)
                                      (end-offset potential-form))
                                  (form-incomplete-p (fully-unquoted-form first)))
                              potential-form
                              first)
                          (when (formp first)
                            first)))))
           (t nil))))

(defun form-before (syntax offset)
  (assert (>= (size (buffer syntax)) offset) nil
          "Offset past buffer end")
  (assert (>= offset 0) nil
          "Offset before buffer start")
  (update-parse syntax)
  (or (gethash offset (form-before-cache syntax))
      (setf (gethash offset (form-before-cache syntax))
            (with-slots (stack-top) syntax
              (if (or (null (start-offset stack-top))
                      (<= offset (start-offset stack-top)))
                  nil
                  (form-before-in-children syntax (children stack-top) offset))))))

(defun form-after-in-children (syntax children offset)
  (update-parse syntax)
  (loop for child in children
     if (formp child)
     do (cond ((< (start-offset child) offset (end-offset child))
               (return (if (null (children child))
                           nil
                           (form-after-in-children syntax (children child) offset))))
              ((<= offset (start-offset child))
               (return (let ((potential-form (form-after-in-children syntax (children child) offset)))
                         (if (not (null potential-form))
                             (if (<= (start-offset child)
                                     (start-offset potential-form))
                                 child
                                 potential-form)
                             (when (formp child)
                               child)))))
              (t nil))))

(defun form-after (syntax offset)
  (assert (>= (size (buffer syntax)) offset) nil
          "Offset past buffer end")
  (assert (>= offset 0) nil
          "Offset before buffer start")
  (update-parse syntax)
  (or (gethash offset (form-after-cache syntax))
      (setf (gethash offset (form-after-cache syntax))
            (with-slots (stack-top) syntax
              (if (or (null (start-offset stack-top))
                      (>= offset (end-offset stack-top)))
                  nil
                  (form-after-in-children syntax (children stack-top) offset))))))

(defun form-around-in-children (syntax children offset)
  (update-parse syntax)
  (loop for child in children
     if (formp child)
     do (cond ((or (<= (start-offset child) offset (end-offset child))
                   (= offset (end-offset child))
                   (= offset (start-offset child)))
               (return (if (null (first-form (children child)))
                           child
                           (or (form-around-in-children syntax (children child) offset)
                               child))))
              ((< offset (start-offset child))
               (return nil))
              (t nil))))

(defun form-around (syntax offset)
  (assert (>= (size (buffer syntax)) offset) nil
          "Offset past buffer end")
  (assert (>= offset 0) nil
          "Offset before buffer start")
  (update-parse syntax)
  (or (gethash offset (form-around-cache syntax))
      (setf (gethash offset (form-around-cache syntax))
            (with-slots (stack-top) syntax
              (if (or (null (start-offset stack-top))
                      (> offset (end-offset stack-top))
                      (< offset (start-offset stack-top)))
                  nil
                  (form-around-in-children syntax (children stack-top) offset))))))

(defun find-parent-of-type (form test)
  "Find a parent of `form' for which the function `test' is true
and return it. If a such a parent cannot be found, return nil."
  (let ((parent (parent form)))
    (cond ((null parent)
           nil)
          ((funcall test parent)
           parent)
          (t (find-parent-of-type parent test)))))

(defun find-parent-of-type-offset (form test fn)
  "Find a parent of `form' for which the function `test' is true
and return `fn' applied to this parent form. `Fn' should be a
function that returns an offset when applied to a
form (eg. `start-offset' or `end-offset'). If such a parent
cannot be found, return nil"
  (let ((parent (find-parent-of-type form test)))
    (when parent
      (funcall fn parent))))

(defun find-child-of-type (form test)
  "Find the first child of `form' for which the function `test'
is true and return it. If such a child cannot be found, return
nil."
  (find-if #'(lambda (child)
               (cond ((funcall test child) child)
                     ((formp child) (find-child-of-type child test))))
           (children form)))

(defun find-child-of-type-offset (form test fn)
  "Find the first child of `form' for which the function `test' is true and return `fn' applied to this child.
`Fn' should be a function that returns an offset when applied to
a form (eg. `start-offset' or `end-offset'). If such a child
cannot be found, return nil."
  (let ((child (find-child-of-type form test)))
    (when child
      (funcall fn child))))

(defun find-list-parent (form)
  "Find a list parent of `form' and return it. If a list parent
cannot be found, return nil."
  (find-parent-of-type form #'form-list-p))

(defun find-list-parent-offset (form fn)
  "Find a list parent of `form' and return `fn' applied to this
parent token. `Fn' should be a function that returns an offset
when applied to a token (eg. `start-offset' or `end-offset'). If
a list parent cannot be found, return nil"
  (find-parent-of-type-offset form #'form-list-p fn))

(defun find-list-child (form)
  "Find the first list child of `form' and return it. If a list
child cannot be found, return nil."
  (find-child-of-type form #'form-list-p))

(defun find-list-child-offset (form fn)
  "Find a list child of `form' and return `fn' applied to this child.
`Fn' should be a function that returns an offset when applied to
a form (eg. `start-offset' or `end-offset'). If a list child
cannot be found, return nil."
  (find-child-of-type-offset form #'form-list-p fn))

(defmethod backward-one-expression (mark (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (let ((potential-form (or (form-before syntax (offset mark))
                            (form-around syntax (offset mark)))))
    (loop until (null potential-form)
       do (cond ((= (offset mark) (start-offset potential-form))
                 (setf potential-form
                       (unless (form-at-top-level-p potential-form)
                         (parent potential-form))))
                (t (setf (offset mark) (start-offset potential-form))
                   (return t))))))

(defmethod forward-one-expression (mark (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (let ((potential-form (or (form-after syntax (offset mark))
                            (form-around syntax (offset mark)))))
    (when (not (null potential-form))
      (when (and (not (form-at-top-level-p potential-form))
                 (= (offset mark) (end-offset potential-form)))
        (setf potential-form (parent potential-form)))
      (when (and (not (null potential-form))
                 (not (= (offset mark) (end-offset potential-form))))
        (typecase potential-form
          (reader-conditional-form
           (setf (offset mark) (or (start-offset (first-form (children potential-form)))
                                   (end-offset potential-form))))
          (t (setf (offset mark) (end-offset potential-form))))))))

(defmethod forward-delete-expression (mark (syntax lisp-syntax) &optional (count 1)
                                      (limit-action #'error-limit-action))
  (let ((mark2 (clone-mark mark)))
    (when (and (not (structurally-at-end-of-list-p (current-syntax) mark))
               (forward-expression mark2 syntax count limit-action))
      (delete-region mark mark2)
      t)))

(defmethod backward-delete-expression (mark (syntax lisp-syntax) &optional (count 1)
                                       (limit-action #'error-limit-action))
  (let ((mark2 (clone-mark mark)))
    (when (and (not (structurally-at-end-of-list-p (current-syntax) mark))
               (backward-expression mark2 syntax count limit-action))
      (delete-region mark mark2)
      t)))

(defmethod forward-kill-expression (mark (syntax lisp-syntax) &optional (count 1) concatenate-p
                                    (limit-action #'error-limit-action))
  (let ((start (offset mark)))
    (forward-expression mark syntax count limit-action)
    (unless (mark= mark start)
      (if concatenate-p
          (if (plusp count)
              (kill-ring-concatenating-push
               *kill-ring*
               (region-to-sequence start
                                   mark))
              (kill-ring-reverse-concatenating-push
               *kill-ring*
               (region-to-sequence
                start mark)))
          (kill-ring-standard-push
           *kill-ring*
           (region-to-sequence start mark)))
      (delete-region start mark)
      t)))

(defmethod backward-kill-expression (mark (syntax lisp-syntax) &optional (count 1) concatenate-p
                                     (limit-action #'error-limit-action))
  (let ((start (offset mark)))
    (backward-expression mark syntax count limit-action)
    (unless (mark= mark start)
      (if concatenate-p
          (if (plusp count)
              (kill-ring-concatenating-push *kill-ring*
                                            (region-to-sequence start
                                                                mark))
              (kill-ring-reverse-concatenating-push *kill-ring*
                                                    (region-to-sequence
                                                     start mark)))
          (kill-ring-standard-push *kill-ring*
                                   (region-to-sequence start mark)))
      (delete-region start mark)
      t)))

(defgeneric forward-one-list (mark syntax)
  (:documentation "Move `mark' forward by one list.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-list (mark (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (loop for start = (offset mark)
     then (end-offset potential-form)
     for potential-form = (or (form-after syntax start)
                              (form-around syntax start))
     until (or (null potential-form)
               (and (= start
                       (end-offset potential-form))
                    (null (form-after syntax start))))
     when (form-list-p potential-form)
     do (setf (offset mark) (end-offset potential-form))
     (return t)))

(defgeneric backward-one-list (mark syntax)
  (:documentation "Move `mark' backward by one list.  Return T if
successful, or NIL if the buffer limit was reached."))

(defmethod backward-one-list (mark (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (loop for start = (offset mark)
     then (start-offset potential-form)
     for potential-form = (or (form-before syntax start)
                              (form-around syntax start))
     until (or (null potential-form)
               (and (= start
                       (start-offset potential-form))
                    (null (form-before syntax start))))
     when (form-list-p potential-form)
     do (setf (offset mark) (start-offset potential-form))
     (return t)))

(defun down-list (mark syntax selector next-offset-fn target-offset-fn)
  (update-parse syntax 0 (offset mark))
  (labels ((next (continue-from)
             (find-offset (funcall selector syntax
                                   (funcall next-offset-fn continue-from))))
           (find-offset (potential-form)
             (typecase potential-form
               (list-form (funcall target-offset-fn potential-form))
               (form (or (find-list-child-offset potential-form target-offset-fn)
                         (next potential-form)))
               (null nil)
               (t (next potential-form)))))
    (let ((new-offset (find-offset (funcall selector syntax (offset mark)))))
      (when new-offset
        (setf (offset mark) new-offset)
        t))))

(defmethod forward-one-down ((mark mark) (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (when (down-list mark syntax #'form-after #'end-offset #'start-offset)
    (forward-object mark)))

(defmethod backward-one-down ((mark mark) (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (when (down-list mark syntax #'form-before #'start-offset #'end-offset)
    (backward-object mark)))

(defun up-list (mark syntax fn)
  (update-parse syntax 0 (offset mark))
  (let ((form (form-around syntax (offset mark))))
    (when (if (and (form-list-p form)
                   (/= (start-offset form) (offset mark))
                   (/= (end-offset form) (offset mark)))
              (setf (offset mark) (funcall fn form))
              (let ((new-offset (find-list-parent-offset form fn)))
                (when new-offset
                  (setf (offset mark) new-offset))))
      t)))

(defmethod backward-one-up (mark (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (up-list mark syntax #'start-offset))

(defmethod forward-one-up (mark (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (up-list mark syntax #'end-offset))

(defmethod backward-one-definition ((mark mark) (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (with-slots (stack-top) syntax
    ;; FIXME? This conses! I'm over it already. I don't think it
    ;; matters much, but if someone is bored, please profile it.
    (loop for form in (reverse (children stack-top))
       when (and (formp form)
                 (mark> mark (start-offset form)))
       do (setf (offset mark) (start-offset form))
       and do (return t))))

(defmethod forward-one-definition ((mark mark) (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (with-slots (stack-top) syntax
    (loop for form in (children stack-top)
       when (and (formp form)
                 (mark< mark (end-offset form)))
       do (setf (offset mark) (end-offset form))
       and do (return t))))

(defmethod eval-defun ((mark mark) (syntax lisp-syntax))
  (update-parse syntax 0 (offset mark))
  (with-slots (stack-top) syntax
     (loop for form in (children stack-top)
           when (and (mark<= (start-offset form) mark)
                     (mark<= mark (end-offset form)))
             do (return (eval-form-for-drei
                         (get-usable-image syntax)
                         (form-to-object syntax form :read t))))))

;;; shamelessly replacing SWANK code
;; We first work through the string removing the characters and noting
;; which ones are escaped. We then replace each character with the
;; appropriate case version, according to the readtable.
;; Finally, we extract the package and symbol names.
;; Being in an editor, we are waaay more lenient than the reader.

(defun parse-escapes (string)
  "Return a string and a list of escaped character positions.
Uses part of the READ algorithm in CLTL2 22.1.1."
  (let ((length (length string))
        (index 0)
        irreplaceables chars)
    (tagbody
     step-8
       (unless (< index length) (go end))
       (cond
         ((char/= (char string index) #\\ #\|)
          (push (char string index) chars)
          (incf index)
          (go step-8))
         ((char= (char string index) #\\)
          (push (length chars) irreplaceables)
          (incf index)
          (unless (< index length) (go end))
          (push (char string index) chars)
          (incf index)
          (go step-8))
         ((char= (char string index) #\|)
          (incf index)
          (go step-9)))
     step-9
       (unless (< index length) (go end))
       (cond
         ((char/= (char string index) #\\ #\|)
          (push (length chars) irreplaceables)
          (push (char string index) chars)
          (incf index)
          (go step-9))
         ((char= (char string index) #\\)
          (push (length chars) irreplaceables)
          (incf index)
          (unless (< index length) (go end))
          (push (char string index) chars)
          (incf index)
          (go step-9))
         ((char= (char string index) #\|)
          (incf index)
          (go step-8)))
     end
       (return-from parse-escapes
         (values (coerce (nreverse chars) 'string)
                 (nreverse irreplaceables))))))

(defun invert-cases (string &optional (irreplaceables nil))
  "Returns two flags: unescaped upper-case and lower-case chars in STRING."
  (loop for index below (length string)
       with upper = nil
       with lower = nil
       when (not (member index irreplaceables))
        if (upper-case-p (char string index))
         do (setf upper t) end
        if (lower-case-p (char string index))
         do (setf lower t) end
     finally (return (values upper lower))))

(defun replace-case (string &optional (case (readtable-case *readtable*))
                                      (irreplaceables nil))
  "Convert string according to readtable-case."
  (multiple-value-bind (upper lower) (invert-cases string irreplaceables)
    (loop for index below (length string)
       as char = (char string index) then (char string index)
       if (member index irreplaceables)
         collect char into chars
       else
         collect (ecase case
                   (:preserve char)
                   (:upcase (char-upcase char))
                   (:downcase (char-downcase char))
                   (:invert (cond ((and lower upper) char)
                                  (lower (char-upcase char))
                                  (upper (char-downcase char))
                                  (t char)))) into chars
       finally (return (coerce chars 'string)))))

(defun parse-token (string &optional (case (readtable-case *readtable*)))
  "Extracts the symbol-name and package name from STRING
and whether the symbol-name was separated from the package by a double colon."
  (multiple-value-bind (string irreplaceables) (parse-escapes string)
    (let ((string (replace-case string case irreplaceables))
          package-name symbol-name internalp)
      (loop for index below (length string)
           with symbol-start = 0
           when (and (char= (char string index) #\:)
                     (not (member index irreplaceables)))
                do (setf package-name (subseq string 0 index))
                   (if (and (< (incf index) (length string))
                            (char= (char string index) #\:)
                            (not (member index irreplaceables)))
                       (setf symbol-start (1+ index)
                             internalp t)
                       (setf symbol-start index))
                   (loop-finish)
           finally (setf symbol-name (subseq string symbol-start)))
      (values symbol-name package-name internalp))))

#|
;;; Compare CLHS 23.1.2.1
 (defun test-parse-token ()
  (let ((*readtable* (copy-readtable nil)))
    (format t "READTABLE-CASE  Input         Symbol-name   Token-name~
             ~%------------------------------------------------------~
             ~%")
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (dolist (input '("ZEBRA" "Zebra" "zebra" "\\zebra" "\\Zebra" "z|ebr|a"
                       "|ZE\\bRA|" "ze\\|bra"))
        (format t "~&:~A~16T~A~30T~A~44T~A"
                (string-upcase readtable-case)
                input
                (progn (setf (readtable-case *readtable*) readtable-case)
                       (symbol-name (read-from-string input)))
                (parse-token input readtable-case))))))
|#

(defun form-string (syntax form)
  "Return the string that correspond to `form' in the buffer of
`syntax'."
  (buffer-substring (buffer syntax) (start-offset form) (end-offset form)))

(defun parse-symbol (string &key (package *package*) (case (readtable-case *readtable*)))
  "Find the symbol named STRING.
Return the symbol, the package of the symbol and a flag
indicating whether the symbol was found in the package. Note that
a symbol and a package may be returned even if it was not found
in a package, for example if you do `foo-pkg::bar', where
`foo-pkg' is an existing package but `bar' isn't interned in
it. If the package cannot be found, its name as a string will be
returned in its place."
  (multiple-value-bind (symbol-name package-name)
      (parse-token string case)
    (let ((package (cond ((string= package-name "") +keyword-package+)
                         (package-name              (or (find-package package-name)
                                                        package-name))
                         (t                         package))))
      (multiple-value-bind (symbol status)
          (when (packagep package)
            (find-symbol symbol-name package))
        (if (or symbol status)
            (values symbol package status)
            (values (make-symbol symbol-name) package nil))))))

;;; The following algorithm for reading backquote forms was originally
;;; taken from the example reader macro in Common LISP the Language
;;; 2nd. Ed, and was subsequently optimized and cleaned with some
;;; ideas taken from SBCL's backquote implementation.

;;; Define synonyms for the lisp functions we use, so that we can
;;; eventually recognize them and print them in a pretty-printer-style
;;; way.
(macrolet ((def (b-name name)
             (let ((args (gensym "ARGS")))
               `(defun ,b-name (&rest ,args)
                  (declare (dynamic-extent ,args))
                  (apply #',name ,args)))))
  (def backquote-list list)
  (def backquote-list* list*)
  (def backquote-append append)
  (def backquote-nconc nconc)
  (def backquote-cons cons))

(defun backquote-vector (list)
  (declare (list list))
  (coerce list 'simple-vector))

(defconstant +comma-marker+ (gensym "COMMA")
  "The marker used for identifying commas.")
(defconstant +comma-at-marker+ (gensym "COMMA-AT")
  "The marker used for identifying ,@ contructs.")
(defconstant +comma-dot-marker+ (gensym "COMMA-DOT")
  "The marker used for identifying ,. contructs.")

(defconstant +clobberable-marker+ (gensym "CLOBBERABLE")
  "Marker for a constant that we can safely modify
destructively.")

(defconstant +quote-marker+ 'quote
  "The marker used for identifying quote forms in backquoted
forms.")
(defconstant +quote-nil-marker+ (list +quote-marker+ nil))
(defconstant +list-marker+ 'backquote-list)
(defconstant +append-marker+ 'backquote-append)
(defconstant +list*-marker+ 'backquote-list*)
(defconstant +nconc-marker+ 'backquote-nconc)
(defconstant +vector-marker+ 'backquote-vector)

(defun completely-process-backquote (expression)
  "Considering `expression' a list (or tree) containing comma,
comma-at and comma-dot markers, return an expression constructed
so that it, when evaluated, will behave like a backquoted form is
expected to. `Expression' is treated as being implicitly preceded
by a backquote."
  (remove-backquote-tokens (simplify-backquote (process-backquote expression))))

(defun process-backquote (x)
  (cond ((vectorp x)
         ;; FIXME? Is there a faster way to handle this?
         (list +vector-marker+
               (process-backquote
                (loop for elt across x
                   collecting elt))))
        ((atom x)
         (list +quote-marker+ x))
        ((eq (car x) +comma-marker+) (cadr x))
        ((eq (car x) +comma-at-marker+)
         (error ",@~S after `" (cadr x)))
        ((eq (car x) +comma-dot-marker+)
         (error ",.~S after `" (cadr x)))
        (t (do ((p x (cdr p))
                (q '() (cons (bracket (car p)) q)))
               ((atom p)
                (cons +append-marker+
                      (nreconc q (list (list +quote-marker+ p)))))
             (when (eq (car p) +comma-marker+)
               (unless (null (cddr p)) (error "Malformed ,~S" p))
               (return (cons +append-marker+
                             (nreconc q (list (cadr p))))))
             (when (eq (car p) +comma-at-marker+)
               (error "Dotted ,@~S" p))
             (when (eq (car p) +comma-dot-marker+)
               (error "Dotted ,.~S" p))))))

(defun bracket (x)
  (cond ((atom x)
         (list +list-marker+ (process-backquote x)))
        ((eq (car x) +comma-marker+)
         (list +list-marker+ (cadr x)))
        ((eq (car x) +comma-at-marker+)
         (cadr x))
        ((eq (car x) +comma-dot-marker+)
         (list +clobberable-marker+ (cadr x)))
        (t (list +list-marker+ (process-backquote x)))))

(defun remove-backquote-tokens (x)
  (cond ((atom x) x)
        ((eq (car x) +clobberable-marker+)
         (remove-backquote-tokens (cadr x)))
        ((and (eq (car x) +list*-marker+)
              (consp (cddr x))
              (null (cdddr x)))
         (cons 'cons (maptree #'remove-backquote-tokens (cdr x))))
        (t (maptree #'remove-backquote-tokens x))))

(defun splicing-comma-marker-p (x)
  "True for forms that textually looks like
,@foo or ,.foo."
  (and (consp x)
       (or (eq (car x) +comma-at-marker+)
           (eq (car x) +comma-dot-marker+))))

(defun comma-marker-p (x)
  "This predicate is true of a form that textually looks like
,@foo or ,.foo or just plain ,foo."
  (and (consp x)
       (or (eq (car x) +comma-marker+)
           (eq (car x) +comma-at-marker+)
           (eq (car x) +comma-dot-marker+))))

(defun simplify-backquote (x)
  (if (atom x)
      x
      (let ((x (if (eq (car x) +quote-marker+)
                   x
                   (maptree #'simplify-backquote x))))
        (if (not (eq (car x) +append-marker+))
            x
            (simplify-backquote-args x)))))

(defun simplify-backquote-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result
        nil
        (cond ((atom (car args))
               (attach-backquote-append +append-marker+ (car args) result))
              ((and (eq (caar args) +list-marker+)
                    (notany #'splicing-comma-marker-p (cdar args)))
               (attach-backquote-conses (cdar args) result))
              ((and (eq (caar args) +list-marker+)
                    (notany #'splicing-comma-marker-p (cdar args)))
               (attach-backquote-conses
                (reverse (cdr (reverse (cdar args))))
                (attach-backquote-append +append-marker+
                                         (car (last (car args)))
                                         result)))
              ((and (eq (caar args) +quote-marker+)
                    (consp (cadar args))
                    (not (comma-marker-p (cadar args)))
                    (null (cddar args)))
               (attach-backquote-conses (list (list +quote-marker+
                                                    (caadar args)))
                                        result))
              ((eq (caar args) +clobberable-marker+)
               (attach-backquote-append +nconc-marker+ (cadar args) result))
              (t (attach-backquote-append +append-marker+
                                          (car args)
                                          result)))))
      ((null args) result)))

(defun attach-backquote-conses (items result)
  "The effect of `attach-backquote-conses' is to produce a form as if by
`(list* ,@items ,result) but some simplifications are done on the
fly.

 (LIST* 'a 'b 'c 'd) => '(a b c . d)
 (LIST* a b c 'nil) => (LIST a b c)
 (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g)
 (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)"
  (cond ((and (every #'null-or-quoted items)
              (null-or-quoted result))
         (list +quote-marker+
               (append (mapcar #'cadr items) (cadr result))))
        ((or (null result) (equal result +quote-marker+))
         (cons +list-marker+ items))
        ((and (consp result)
              (or (eq (car result) +list-marker+)
                  (eq (car result) +list*-marker+)))
         (cons (car result) (append items (cdr result))))
        (t (cons +list*-marker+ (append items (list result))))))

(defun null-or-quoted (x)
  (or (null x) (and (consp x)
                    (eq (car x) +quote-marker+))))

(defun attach-backquote-append (op item result)
  "When `attach-backquote-append' is called, the OP should be
`+append-marker+' or `+nconc-marker' This produces a form (op
item result) but some simplifications are done on the fly:

 (op '(a b c) '(d e f g)) => '(a b c d e f g)
 (op item 'nil) => item, provided item is not a splicable frob
 (op item 'nil) => (op item), if item is a splicable frob
 (op item (op a b c)) => (op item a b c)"
  (cond ((and (null-or-quoted item) (null-or-quoted result) (listp (cadr item)))
         (list +nconc-marker+ (append (cadr item) (cadr result))))
        ((or (null result) (equal result +quote-nil-marker+))
         (if (splicing-comma-marker-p item) (list op item) item))
        ((and (consp result) (eq (car result) op))
         (list* (car result) item (cdr result)))
        (t (list op item result))))

(define-condition reader-invoked (condition)
  ((%end-mark :reader end-mark :initarg :end-mark
              :initform (error "You must provide an ending-mark for
the condition")
              :documentation "The position at which the reader
stopped reading, form-to-object conversion should be resumed
from this point.")
   (%object :reader object :initarg :object
            :initform (error "You must provide the object that
was returned by the reader")
            :documentation "The object that was returned by the reader."))
  (:documentation "Signal that the reader has been directly
invoked on the buffer contents, that the object of this condition
should be assumed as the result of the form-conversion."))

(defun invoke-reader (syntax form)
  "Use the system reader to handle `form' and signal a
`reader-invoked' condition with the resulting data."
  (let* ((start-mark (make-buffer-mark (buffer syntax) (start-offset form))))
    (let* ((stream (make-buffer-stream :buffer (buffer syntax)
                                       :start-mark start-mark))
           (object (read-preserving-whitespace stream)))
      (signal 'reader-invoked :end-mark (point stream) :object object))))

(define-condition form-conversion-error (simple-error user-condition-mixin)
  ((syntax :reader syntax :initarg :syntax
           :initform (error "You must provide the syntax the
erroneous form is in."))
   (form :reader form :initarg :form
         :initform (error "You must provide the erroneous form."))
   (problem :reader problem :initarg :problem :initform "invalid form"))
  (:report (lambda (condition stream)
             (format stream "Syntax problem: ~A" (problem condition))))
  (:documentation "This condition (or a subclass) is signalled by
`form-to-object' when a form cannot be converted to a proper Lisp
object."))

(defun form-conversion-error (syntax form problem &rest args)
  "Signal a `form-conversion-error' for the `form' in
`syntax'. `Problem' should be a succint description of why `form'
is invalid and cannot be converted to an object. `Problem' is a
format string, in which case `args' is used as format
parameters."
  (error 'form-conversion-error
         :syntax syntax :form form :problem (apply #'format nil problem args)))

(defmethod handle-drei-condition (drei (condition form-conversion-error))
  (with-minibuffer-stream (minibuffer)
    (let ((*print-escape* nil))
      (princ condition minibuffer)))
  (when (point-mark-view-p (view drei))
    (setf (offset (point (view drei)))
          (start-offset (form condition)))))

;;; Handling labels (#n= and #n#) takes a fair bit of machinery, most
;;; of which is located here. We follow an approach similar to that
;;; found in the SBCL reader, where we replace instances of #n# with a
;;; special unique marker symbol that we replace before returning the
;;; final object. We maintain two tables, one that maps labels to
;;; placerholder symbols and one that maps placeholder symbols to the
;;; concrete objects.

(defvar *labels->placeholders* nil
  "This variable holds an alist mapping labels (as integers) to a
placeholder symbol. It is used for implementing the label reader
macros (#n=foo #n#).")

(defvar *label-placeholders->objects* nil
  "This variable holds an alist mapping placeholder symbols to
the object. It is used for implementing the label reader
macros (#n=foo #n#).")

(defgeneric extract-label (syntax form)
  (:documentation "Get the label of `form' as an integer."))

(defmethod extract-label ((syntax lisp-syntax) (form sharpsign-equals-form))
  (let ((string (form-string syntax (first (children form)))))
    (parse-integer string :start 1 :end (1- (length string)) :radix 10)))

(defmethod extract-label ((syntax lisp-syntax) (form sharpsign-sharpsign-form))
  (let ((string (form-string syntax form)))
    (parse-integer string :start 1 :end (1- (length string)) :radix 10)))

(defun register-form-label (syntax form &rest args)
  "Register the label of `form' and the corresponding placeholder
symbol. `Form' must be a sharpsign-equals form (#n=), and if the
label has already been registered, an error of type
`form-conversion-error' will be signalled. Args will be passed to
`form-to-object' for the creation of the object referred to by
the label. Returns `form' converted to an object."
  (let* ((label (extract-label syntax form))
         (placeholder-symbol (gensym)))
    (when (assoc label *labels->placeholders*)
      (form-conversion-error syntax form "multiply defined label: ~A" label))
    (push (list label placeholder-symbol) *labels->placeholders*)
    (let ((object (apply #'form-to-object syntax
                         (second (children form)) args)))
      (push (list placeholder-symbol object) *label-placeholders->objects*)
      object)))

(defgeneric find-and-register-label (syntax form label limit &rest args)
  (:documentation "Find the object referred to by the integer
value `label' in children of `form' or `form' itself. `Args' will
be passed to `form-to-object' for the creation of the
object. `Limit' is a buffer offset delimiting where not to search
past."))

(defmethod find-and-register-label ((syntax lisp-syntax) (form form)
                                    (label integer) (limit integer) &rest args)
  (find-if #'(lambda (child)
               (when (and (formp child)
                          (< (start-offset form) limit))
                 (apply #'find-and-register-label syntax child label limit args)))
           (children form)))

(defmethod find-and-register-label ((syntax lisp-syntax) (form sharpsign-equals-form)
                                    (label integer) (limit integer) &rest args)
  (when (and (= (extract-label syntax form) label)
             (< (start-offset form) limit))
    (apply #'register-form-label syntax form args)))

(defun ensure-label (syntax form label &rest args)
  "Ensure as best as possible that `label' exist. `Form' is the
form that needs the value of the label, limiting where to end the
search. `Args' will be passed to `form-to-object' if it is
necessary to create a new object for the label."
  (unless (assoc label *labels->placeholders*)
    (apply #'find-and-register-label syntax (form-toplevel syntax form) label (start-offset form) args)))

(defun label-placeholder (syntax form label &optional search-whole-form &rest args)
  "Get the placeholder for `label' (which must be an integer). If
the placeholder symbol cannot be found, the label is undefined,
and an error of type `form-conversion-error' will be
signalled. If `search-whole-form' is true, the entire
top-level-form will be searched for the label reference if it has
not already been seen, upwards from `form', but not past
`form'. `Args' will be passed as arguments to `form-to-object' to
create the labelled object."
  (when search-whole-form
    (apply #'ensure-label syntax form label args))
  (let ((pair (assoc label *labels->placeholders*)))
    (second pair)))

;;; The `circle-subst' function is cribbed from SBCL.

(defvar *sharp-equal-circle-table* nil
  "Objects already seen by `circle-subst'.")

(defun circle-subst (old-new-alist tree)
  "This function is kind of like NSUBLIS, but checks for
circularities and substitutes in arrays and structures as well as
lists. The first arg is an alist of the things to be replaced
assoc'd with the things to replace them."
  (cond ((not (typep tree
                     '(or cons (array t) structure-object standard-object)))
         (let ((entry (find tree old-new-alist :key #'first)))
           (if entry (second entry) tree)))
        ((null (gethash tree *sharp-equal-circle-table*))
         (setf (gethash tree *sharp-equal-circle-table*) t)
         (cond ((typep tree '(or structure-object standard-object))
                ;; I am time and again saved by the MOP as I code
                ;; myself into a corner.
                (let ((class (class-of tree)))
                  (dolist (slotd (c2mop:class-slots class))
                    (when (c2mop:slot-boundp-using-class class tree slotd)
                      (let* ((old (c2mop:slot-value-using-class class tree slotd))
                             (new (circle-subst old-new-alist old)))
                        (unless (eq old new)
                          (setf (c2mop:slot-value-using-class
                                 class tree slotd)
                                new)))))))
               ((vectorp tree)
                (loop for i from 0 below (length tree) do
                     (let* ((old (aref tree i))
                            (new (circle-subst old-new-alist old)))
                       (unless (eq old new)
                         (setf (aref tree i) new)))))
               ((arrayp tree)
                (loop with array-size = (array-total-size tree)
                   for i from 0 below array-size do
                   (let* ((old (row-major-aref tree i))
                          (new (circle-subst old-new-alist old)))
                     (unless (eq old new)
                       (setf (row-major-aref tree i) new)))))
               (t
                (let ((a (circle-subst old-new-alist (car tree)))
                      (d (circle-subst old-new-alist (cdr tree))))
                  (unless (eq a (car tree))
                    (rplaca tree a))
                  (unless (eq d (cdr tree))
                    (rplacd tree d)))))
         tree)
        (t tree)))

(defun replace-placeholders (&rest values)
  "Replace the placeholder symbols in `values' with the real
objects as determined by `*label-placeholders->objects*' and
return the modified `values' as multiple return values."
  (values-list
   (if *label-placeholders->objects*
       (mapcar #'(lambda (value)
                   (let ((*sharp-equal-circle-table* (make-hash-table :test #'eq :size 20)))
                     (circle-subst *label-placeholders->objects* value)))
               values)
       values)))

(defvar *form-to-object-depth* 0
  "This variable is used to keep track of how deeply nested calls
to `form-to-object' are.")

(defgeneric form-to-object (syntax form &key no-error package read backquote-level case)
  (:documentation "Return the Lisp object `form' would become if
read. An attempt will be made to construct objects from
incomplete tokens. Some forms (such as negative
readtime-conditional-forms) may return no values. This function
may signal an error of type `form-conversion-error' if `no-error'
is false and `token' cannot be converted to a Lisp
object. Otherwise, NIL will be returned. Also, if `read' is true
and `no-error' is false, an error of type `form-conversion-error'
will be signalled for incomplete forms.")
  (:method :around ((syntax lisp-syntax) (form form) &key package no-error &allow-other-keys)
           ;; Ensure that every symbol that is READ will be looked up
           ;; in the correct package.
           (flet ((act ()
                    (handler-case
                        (multiple-value-call #'replace-placeholders (call-next-method))
                      (reader-invoked (c)
                        (if (> (offset (end-mark c)) (end-offset form))
                            (signal c)
                            (object c)))
                      (form-conversion-error (e)
                        (unless no-error
                          (error e))))))
             (let ((*form-to-object-depth* (1+ *form-to-object-depth*))
                   (*package* (or package (package-at-mark syntax (start-offset form)))))
               (if (= *form-to-object-depth* 1)
                   (let ((*labels->placeholders* nil)
                         (*label-placeholders->objects* nil))
                     (act))
                   (act)))))
  (:method ((syntax lisp-syntax) (form form) &rest args
            &key no-error &allow-other-keys)
    (unless no-error
      (apply #'no-applicable-method #'form-to-object syntax form args)))
  (:method :before ((syntax lisp-syntax) (form incomplete-form-mixin)
                    &key read no-error &allow-other-keys)
           (when (and read (null no-error))
             (form-conversion-error syntax form "form is incomplete"))))

(defmethod form-to-object ((syntax lisp-syntax) (form error-lexeme)
                           &key &allow-other-keys)
  (form-conversion-error syntax form "invalid syntax"))

;;; The complicated primary structure forms.

(defmethod form-to-object ((syntax lisp-syntax) (form list-form) &rest args
                           &key &allow-other-keys)
  (labels ((recurse (elements)
             (unless (null elements)
               (handler-case
                   (nconc (multiple-value-list
                           (apply #'form-to-object syntax (first elements) args))
                          (recurse (rest elements)))
                 (reader-invoked (c)
                   (let ((remaining-elements (remove (offset (end-mark c)) elements
                                                     :key #'start-offset :test #'>)))
                     (if (and (not (null (rest elements)))
                              (null remaining-elements))
                         (signal c)
                         (cons (object c) (recurse remaining-elements)))))))))
    (recurse (remove-if-not #'formp (children form)))))

(defmethod form-to-object ((syntax lisp-syntax) (form complete-quote-form) &rest args
                           &key (backquote-level 0) &allow-other-keys)
  (if (plusp backquote-level)
      (list +quote-marker+ (apply #'form-to-object syntax (second (children form)) args))
      `',(apply #'form-to-object syntax (second (children form)) args)))

(defmethod form-to-object ((syntax lisp-syntax) (form incomplete-quote-form) &rest args)
  (declare (ignore args))
  ;; Utterly arbitrary, but reasonable in my opinion.
  '(quote))

(defmethod form-to-object ((syntax lisp-syntax) (form backquote-form) &rest args
                           &key (backquote-level 0) &allow-other-keys)
  (let ((backquoted-form (first-form (children form))))
    (when (or (form-comma-p backquoted-form)
              (form-comma-at-p backquoted-form)
              (form-comma-dot-p backquoted-form))
      (form-conversion-error syntax form "comma form cannot follow backquote"))
    (let ((backquoted-obj (apply #'form-to-object syntax backquoted-form
                                 :backquote-level (1+ backquote-level) args)))
      (completely-process-backquote backquoted-obj))))

(defmethod form-to-object ((syntax lisp-syntax) (form comma-form) &rest args
                           &key read (backquote-level 0 backquote-active-p)
                           &allow-other-keys)
  (when (and read (or (null backquote-active-p)
                      (zerop backquote-level)))
    (form-conversion-error syntax form "comma form found outside backquote"))
  (let ((obj (apply #'form-to-object syntax (first-form (children form))
                    :backquote-level (max (1- backquote-level) 0) args)))
    (if (plusp backquote-level)
        (list +comma-marker+ obj)
        obj)))

(defmethod form-to-object ((syntax lisp-syntax) (form comma-at-form) &rest args
                           &key read (backquote-level 0 backquote-active-p)
                           &allow-other-keys)
  (when (and read (or (null backquote-active-p)
                      (zerop backquote-level)))
    (form-conversion-error syntax form "comma-at form found outside backquote"))
  (let ((obj (apply #'form-to-object syntax (first-form (children form))
                    :backquote-level (max (1- backquote-level) 0) args)))
    (if (plusp backquote-level)
        (list +comma-at-marker+ obj)
        obj)))

(defmethod form-to-object ((syntax lisp-syntax) (form comma-dot-form) &rest args
                           &key read (backquote-level 0 backquote-active-p)
                           &allow-other-keys)
  (when (and read (or (null backquote-active-p)
                      (zerop backquote-level)))
    (form-conversion-error syntax form "comma-dot form found outside backquote"))
  (let ((obj (apply #'form-to-object syntax (first-form (children form))
                    :backquote-level (max (1- backquote-level) 0) args)))
    (if (plusp backquote-level)
        (list +comma-dot-marker+ obj)
        obj)))

;;; The atom(-ish) forms.

(defmethod form-to-object ((syntax lisp-syntax) (form complete-token-form)
                           &key read (case (readtable-case *readtable*))
                           &allow-other-keys)
  (multiple-value-bind (symbol package status)
      (parse-symbol (form-string syntax form)
                    :package *package* :case case)
    (values (cond ((and read (null status) (packagep package))
                   (intern (symbol-name symbol) package))
                  (t symbol)))))

(defmethod form-to-object ((syntax lisp-syntax) (form number-lexeme)
                           &key &allow-other-keys)
  (let ((*read-base* (base syntax)))
    (invoke-reader syntax form)))

(defmethod form-to-object ((syntax lisp-syntax) (form simple-vector-form)
                           &key &allow-other-keys)
  (let* ((contents (call-next-method))
         (lexeme-string (form-string syntax (first (children form))))
         (size (parse-integer lexeme-string :start 1
                              :end (1- (length lexeme-string))
                              :junk-allowed t))
         (vector (make-array (or size (length contents)))))
    (loop for cons = contents then (or rest cons)
       for element = (first cons)
       for rest = (rest cons)
       for i below (length vector) do
       (setf (aref vector i) element)
       finally (return vector))))

(defmethod form-to-object ((syntax lisp-syntax) (form incomplete-string-form)
                           &key &allow-other-keys)
  (values (read-from-string (concatenate 'string (form-string syntax form) "\""))))

(defmethod form-to-object ((syntax lisp-syntax) (form complete-string-form)
                           &key &allow-other-keys)
  (invoke-reader syntax form))

(defmethod form-to-object ((syntax lisp-syntax) (form function-form) &rest args)
  (list 'cl:function (apply #'form-to-object syntax (second (children form)) args)))

(defmethod form-to-object ((syntax lisp-syntax) (form complete-character-lexeme)
                           &key &allow-other-keys)
  (or (ignore-errors (values (read-from-string (form-string syntax form))))
      (form-conversion-error syntax form
                             "character ~A not recognized"
                             (form-string syntax form))))

(defmethod form-to-object ((syntax lisp-syntax) (form cons-cell-form) &rest args
                           &key &allow-other-keys)
  (apply #'list* (mapcar #'(lambda (form)
                             (apply #'form-to-object syntax form args))
                         (remove-if-not #'formp (children form)))))

(defmethod form-to-object ((syntax lisp-syntax) (form reader-conditional-positive-form)
                           &key &allow-other-keys)
  (let ((conditional (second-noncomment (children form))))
    (if (eval-feature-conditional conditional syntax)
        (form-to-object syntax (third-noncomment (children form)))
        (values))))

(defmethod form-to-object ((syntax lisp-syntax) (form reader-conditional-negative-form)
                           &key &allow-other-keys)
  (let ((conditional (second-noncomment (children form))))
    (if (not (eval-feature-conditional conditional syntax))
        (form-to-object syntax (third-noncomment (children form)))
        (values))))

(defmethod form-to-object ((syntax lisp-syntax) (form uninterned-symbol-form)
                           &key (case (readtable-case *readtable*)) &allow-other-keys)
  (make-symbol (parse-token (form-string syntax form) case)))

(defmethod form-to-object ((syntax lisp-syntax) (form undefined-reader-macro-form)
                           &key read &allow-other-keys)
  ;; This is likely to malfunction for some really evil reader macros,
  ;; in that case, you need to extend the parser to understand them.
  (when read
    (invoke-reader syntax form)))

(defmethod form-to-object ((syntax lisp-syntax) (form literal-object-form) &key &allow-other-keys)
  (buffer-object (buffer syntax) (start-offset form)))

(defmethod form-to-object ((syntax lisp-syntax) (form pathname-form) &key &allow-other-keys)
  (values (read-from-string (form-string syntax form))))

(defmethod form-to-object ((syntax lisp-syntax) (form incomplete-pathname-form) &rest args &key read &allow-other-keys)
  (if read
      ;; Will cause a reader error (which is what we want).
      (call-next-method)
      ;; Try to create a pathname as much as possible.
      (let ((pathspec-form (second (children form))))
        (pathname (if pathspec-form
                      (apply #'form-to-object syntax pathspec-form
                             ;; Since `pathspec-form' will be
                             ;; incomplete, `read'ing from it is
                             ;; probably bad.
                             :read nil args)
                      "")))))

(defmethod form-to-object ((syntax lisp-syntax) (form complete-function-form) &rest args &key &allow-other-keys)
  (list 'function (apply #'form-to-object syntax (second (children form)) args)))

(defmethod form-to-object ((syntax lisp-syntax) (form bit-vector-form) &key &allow-other-keys)
  (values (read-from-string (form-string syntax form))))

(defmethod form-to-object ((syntax lisp-syntax) (form readtime-evaluation-form)
                           &rest args &key read &allow-other-keys)
  (when read
    (values (eval (apply #'form-to-object syntax (first-form (children form)) args)))))

(defmethod form-to-object ((syntax lisp-syntax) (form sharpsign-equals-form)
                           &rest args)
  (apply #'register-form-label syntax form args))

(defmethod form-to-object ((syntax lisp-syntax) (form sharpsign-sharpsign-form)
                           &rest args)
  (apply #'label-placeholder syntax form (extract-label syntax form) t args))

(defmethod form-to-object ((syntax lisp-syntax) (form array-form)
                           &rest args)
  (let* ((rank-string (form-string syntax (first (children form))))
         (rank (parse-integer rank-string :start 1
                              :end (1- (length rank-string))))
         (array-contents (apply #'form-to-object syntax (second (children form)) args)))
    (labels ((dimensions (rank contents)
               (cond ((= rank 0)
                      nil)
                     ((= rank 1)
                      (list (length contents)))
                     (t
                      (let ((goal (dimensions (1- rank) (first contents))))
                        (dolist (element (rest contents))
                          (unless (equal goal (dimensions (1- rank) element))
                            (form-conversion-error syntax form "jagged multidimensional array")))
                        (cons (length contents) goal))))))
      (make-array (dimensions rank array-contents)
                  :initial-contents array-contents))))

(defgeneric form-equal (syntax form1 form2)
  (:documentation "Compare the objects that `form1' and `form2'
represent, which must be forms of `syntax', for equality under
the same rules as `equal'. This function does not have
side-effects. The semantics of this function are thus equivalent
to a side-effect-less version of (equal (form-to-object syntax
form1 :read t) (form-to-object syntax form2 :read t)). `Form1'
and `form2' may also be strings, in which case they are taken to
be a readable representation of some object.")
  (:method ((syntax lisp-syntax) (form1 string) (form2 string))
    ;; Not strictly correct, but good enough for now.
    (string= form1 form2))
  (:method ((syntax lisp-syntax) (form1 string) (form2 form))
    (form-equal syntax form2 form1))
  (:method ((syntax lisp-syntax) (form1 form) (form2 form))
    nil)
  (:method ((syntax lisp-syntax) (form1 form) (form2 string))
    nil))

(defmethod form-equal ((syntax lisp-syntax)
                       (form1 complete-token-form) (form2 complete-token-form))
  (multiple-value-bind (symbol1 package1 status1)
      (parse-symbol (form-string syntax form1)
       :package (package-at-mark syntax (start-offset form1)))
    (declare (ignore status1))
    (multiple-value-bind (symbol2 package2 status2)
        (parse-symbol (form-string syntax form2)
         :package (package-at-mark syntax (start-offset form2)))
      (declare (ignore status2))
      (and (string= symbol1 symbol2)
           (equal package1 package2)))))

(defmethod form-equal ((syntax lisp-syntax)
                       (form1 complete-token-form) (form2 string))
  (multiple-value-bind (symbol1 package1 status1)
      (parse-symbol (form-string syntax form1)
       :package (package-at-mark syntax (start-offset form1)))
    (declare (ignore status1))
    (multiple-value-bind (symbol2 package2 status2)
        (parse-symbol form2
         :package (package-at-mark syntax (start-offset form1)))
      (declare (ignore status2))
      (and (string= symbol1 symbol2)
           (equal package1 package2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda-list handling.
;;;
;;; Just the infrastructure. The fun processing and analysis stuff is
;;; found in lisp-syntax-swine.lisp. The lambda-list interface is
;;; based on the simplicity of CL pathnames.

(defparameter +cl-lambda-list-keywords+
  lambda-list-keywords)

(defparameter +cl-garbage-keywords+
  '(&whole &environment))

(defun lambda-list-keyword-p (arg)
  "Return true if `arg' is a lambda list keyword, false otherwise."
  (member arg +cl-lambda-list-keywords+))

(defgeneric arglist-for-form (syntax operator &optional arguments)
  (:documentation "Return an arglist for `operator'.")
  (:method ((syntax lisp-syntax) (operator symbol) &optional arguments)
    (declare (ignore arguments))
    (parse-lambda-list (cleanup-arglist (arglist (get-usable-image syntax) operator)))))

(defmethod arglist-for-form ((syntax lisp-syntax) (operator list) &optional arguments)
  (declare (ignore arguments))
  (case (first operator)
    ('cl:lambda (parse-lambda-list (cleanup-arglist (second operator))))))

;; SBCL, and some other implementations I would guess, provides us
;; with an arglist that is too simple, confusing the code
;; analysers. We fix that here.
(defmethod arglist-for-form ((syntax lisp-syntax)
                             (operator (eql 'clim-lisp:defclass))
                             &optional arguments)
  (declare (ignore arguments))
  (parse-lambda-list '(name (&rest superclasses) (&rest slots) &rest options)))

(defmethod arglist-for-form ((syntax lisp-syntax)
                             (operator (eql 'cl:defclass))
                             &optional arguments)
  (declare (ignore arguments))
  (parse-lambda-list '(name (&rest superclasses) (&rest slots) &rest options)))

(defun cleanup-arglist (arglist)
  "Remove elements of `arglist' that we are not interested in,
including implementation-specific lambda list keywords."
  (loop
     for arg in arglist
     with in-&aux                       ; If non-NIL, we are in the
                                        ; &aux parameters that should
                                        ; not be displayed.

     with in-garbage                    ; If non-NIL, the next
                                        ; argument is a garbage
                                        ; parameter that should not be
                                        ; displayed.
     if in-garbage
     do (setf in-garbage nil)
     else if (not in-&aux)
     if (eq arg '&aux)
     do (setf in-&aux t)
     else if (member arg +cl-garbage-keywords+ :test #'eq)
     do (setf in-garbage t)
     else if (listp arg)
     collect (cleanup-arglist arg)
     else
     collect arg))

;; Arglist classes. Some arglist elements are not represented (&env,
;; &aux), because they are not interesting.

(defclass lambda-list ()
  ((%original-lambda-list :initarg :original-lambda-list
                                   :reader original-lambda-list))
  (:documentation "The superclass of all lambda list classes."))

(defmethod print-object ((ll lambda-list) stream)
  (print-unreadable-object (ll stream :type t :identity t)
    (when (compute-applicable-methods #'lambda-list-as-list `(,ll))
      (let ((*print-length* (or *print-length* 10)))
        ;; PRINC because the names of KEYWORD-PARAMETERs are keywords.
        (princ (lambda-list-as-list ll) stream)))))

(defgeneric required-parameters (lambda-list)
  (:documentation "Return a list containing objects representing
the required parameters of `lambda-list'.")
  (:method ((lambda-list lambda-list))
    nil))

(defgeneric optional-parameters (lambda-list)
  (:documentation "Return a list containing objects representing
the optional parameters of `lambda-list'.")
  (:method ((lambda-list lambda-list))
    nil))

(defgeneric keyword-parameters (lambda-list)
  (:documentation "Return a list containing objects representing
the keyword parameters of `lambda-list'.")
  (:method ((lambda-list lambda-list))
    nil))

(defgeneric rest-parameter (lambda-list)
  (:documentation "Return an object representing the rest
parameter of `lambda-list'. If `lambda-list' does not have a rest
parameter, this function will return NIL.")
  (:method ((lambda-list lambda-list))
    nil))

(defgeneric body-parameter (lambda-list)
  (:documentation "Return an object representing the body
parameter of `lambda-list'. If `lambda-list' does not have a body
parameter, this function will return NIL.")
  (:method ((lambda-list lambda-list))
    nil))

(defgeneric all-parameters (lambda-list)
  (:documentation "Return a list of all the parameters in
`lambda-list'.")
  (:method-combination append))

(defun clone-lambda-list-element (object &rest additional-initargs)
  "Create a clone (cloned as much as available initargs permit)
of the `lambda-list' object."
  (apply #'make-instance (class-of object)
         (append additional-initargs
                 ;; Find initargs and values from class. This only
                 ;; works because we know the lambda-list and
                 ;; parameter slots all have initargs.
                 (loop for slot in (c2mop:class-slots (class-of object))
                    for slot-initarg = (first (c2mop:slot-definition-initargs slot))
                    for slot-name = (c2mop:slot-definition-name slot)
                    for slot-boundp = (slot-boundp object slot-name)
                    when (and slot-initarg slot-boundp)
                    nconc (list slot-initarg (slot-value object slot-name))))))

(defgeneric merge-parameters-of-type (lambda-list type parameters)
  (:documentation "Return a new `lambda-list' object based on
`lambda-list' with `parameters' added to the list of parameters
of `type', which must be a keyword symbol naming a parameter
type.")
  (:method :around ((lambda-list lambda-list) type parameters)
           (let* ((result (call-next-method))
                  (minimum-classes (mapcar #'minimum-lambda-list-class
                                           (all-parameters result)))
                  (optimal-subclass (if (null minimum-classes)
                                        (class-of result)
                                        (first (sort minimum-classes #'subtypep)))))
             (if (subtype-compatible-p minimum-classes)
                 (change-class result (if (subtypep (class-of result) optimal-subclass)
                                          (class-of result)
                                          optimal-subclass))
                 (error "Lambda list has incompatible parameters")))))

(defun set-parameter-minimums (parameters start-value incrementer)
  "Return a list of clones of the parameters in `parameters' with
new \"minimum arg offset\" values. `Start-value' is used for the
initial offset, `incrementer' will be called with the previously
used offset to calculate a new offset."
  (loop for parameter in parameters
     for index = start-value then (funcall incrementer index)
     collecting (clone-lambda-list-element parameter :min-arg-index index)))

(defun increment-parameter-minimums (parameters start-value incrementer)
  "Return a list of clones of the parameters in `parameters' with
new \"minimum arg offset\" values, calculated by adding values to
the old ones. `Start-value' is used for the initial offset,
`incrementer' will be called with the previously used offset to
calculate a new offset to add to the old offset."
  (when parameters
    (loop for parameter in parameters
       for index = start-value then (funcall incrementer index)
       collecting (clone-lambda-list-element parameter
                   :min-arg-index (+ (min-arg-index parameter) index)))))

(defun make-lambda-list (&rest args &key (defaults (make-instance 'ordinary-lambda-list))
                         &allow-other-keys)
  "Makes a new lambda-list object from the component arguments,
using `defaults' for the default values. The resulting object
will be of the same class as `defaults'. `Defaults' defaults to
an `ordinary-lambda-list' object containing no parameters."
  (let ((result defaults))
    (loop for (type args) on args by #'cddr
       unless (eq type :defaults)
       do (setf result (merge-parameters-of-type result type args))
       finally (return result))))

(defclass semiordinary-lambda-list (lambda-list)
  ((%required-parameters :initarg :required-parameters
                         :initform nil
                         :reader required-parameters)
   (%optional-parameters :initarg :optional-parameters
                         :initform nil
                         :reader optional-parameters)
   (%keyword-parameters :initarg :keyword-parameters
                        :initform nil
                        :reader keyword-parameters)
   (%allow-other-keys :initarg :allow-other-keys
                      :initform nil
                      :reader allow-other-keys-p)
   (%rest-parameter :initarg :rest-parameter
                    :initform nil
                    :reader rest-parameter))
  (:documentation "The class for lambda lists that are
approximately ordinary (as found in `defun' and simple `defmacro's)."))

(defun positional-parameter-count (lambda-list)
  "Return the number of positional parameters in `lambda-list'."
  (+ (length (required-parameters lambda-list))
     (length (optional-parameters lambda-list))))

(defmethod merge-parameters-of-type ((lambda-list semiordinary-lambda-list)
                                     (type (eql :required-parameters)) (parameters list))
  (clone-lambda-list-element lambda-list :required-parameters parameters))

(defmethod merge-parameters-of-type :around ((lambda-list semiordinary-lambda-list)
                                             (type (eql :required-parameters)) (parameters list))
  (let ((parameter-delta (- (length parameters)
                            (length (required-parameters lambda-list))))
        (new-lambda-list (call-next-method)))
    (clone-lambda-list-element new-lambda-list
     :required-parameters (set-parameter-minimums
                           (required-parameters new-lambda-list) 0 #'1+)
     :optional-parameters (increment-parameter-minimums
                           (optional-parameters new-lambda-list) parameter-delta #'identity)
     :keyword-parameters (increment-parameter-minimums
                          (keyword-parameters new-lambda-list) parameter-delta #'identity)
     :rest-parameter (first (increment-parameter-minimums
                             (listed (rest-parameter new-lambda-list))
                             parameter-delta (constantly 0))))))

(defmethod merge-parameters-of-type ((lambda-list semiordinary-lambda-list)
                                     (type (eql :optional-parameters)) (parameters list))
  (clone-lambda-list-element lambda-list :optional-parameters
                             parameters))

(defmethod merge-parameters-of-type :around ((lambda-list semiordinary-lambda-list)
                                             (type (eql :optional-parameters)) (parameters list))
  (let ((parameter-delta (- (length parameters)
                            (length (optional-parameters lambda-list))))
        (new-lambda-list (call-next-method)))
    (clone-lambda-list-element new-lambda-list
     :optional-parameters (set-parameter-minimums
                           (optional-parameters new-lambda-list)
                           (length (required-parameters new-lambda-list)) #'1+)
     :keyword-parameters (increment-parameter-minimums
                          (keyword-parameters new-lambda-list) parameter-delta #'identity)
     :rest-parameter (first (increment-parameter-minimums
                             (listed (rest-parameter new-lambda-list))
                             parameter-delta (constantly 0))))))

(defmethod merge-parameters-of-type ((lambda-list semiordinary-lambda-list)
                                     (type (eql :keyword-parameters)) (parameters list))
  (clone-lambda-list-element lambda-list
   :keyword-parameters (set-parameter-minimums parameters (positional-parameter-count lambda-list) #'identity)))

(defmethod merge-parameters-of-type ((lambda-list semiordinary-lambda-list)
                                     (type (eql :allow-other-keys)) parameter)
  (check-type parameter boolean)
  (clone-lambda-list-element lambda-list :allow-other-keys parameter))

(defmethod merge-parameters-of-type ((lambda-list semiordinary-lambda-list)
                                     (type (eql :rest-parameter)) parameter)
  (check-type parameter (or null rest-parameter))
  (clone-lambda-list-element lambda-list :rest-parameter
                             (clone-lambda-list-element parameter
                              :min-arg-index (positional-parameter-count lambda-list))))

(defmethod all-parameters append ((lambda-list semiordinary-lambda-list))
  (append (required-parameters lambda-list)
          (optional-parameters lambda-list)
          (keyword-parameters lambda-list)
          (when (rest-parameter lambda-list)
            (list (rest-parameter lambda-list)))))

(defclass ordinary-lambda-list (semiordinary-lambda-list)
  ()
  (:documentation "The class for ordinary lambda lists (as found
in `defun')."))

(defclass macro-lambda-list (semiordinary-lambda-list)
  ((%body-parameter :initarg :body-parameter
                    :initform nil
                    :reader body-parameter))
  (:documentation "The class for macro lambda lists."))

(defmethod initialize-instance :after ((object macro-lambda-list) &key)
  (assert (null (and (rest-parameter object) (body-parameter object))) nil
          "It is not permitted to have both a &rest and a &body argument in a lambda list."))

(defmethod merge-parameters-of-type :around ((lambda-list macro-lambda-list)
                                             (type (eql :required-parameters)) (parameters list))
  (let ((parameter-delta (- (length parameters)
                            (length (required-parameters lambda-list))))
        (new-lambda-list (call-next-method)))
    (clone-lambda-list-element new-lambda-list
     :body-parameter (first (increment-parameter-minimums
                             (listed (body-parameter new-lambda-list))
                             parameter-delta (constantly 0))))))

(defmethod merge-parameters-of-type :around ((lambda-list macro-lambda-list)
                                             (type (eql :optional-parameters)) (parameters list))
  (let ((parameter-delta (- (length parameters)
                            (length (optional-parameters lambda-list))))
        (new-lambda-list (call-next-method)))
    (clone-lambda-list-element new-lambda-list
     :body-parameter (first (increment-parameter-minimums
                             (listed (body-parameter new-lambda-list))
                             parameter-delta (constantly 0))))))

(defmethod merge-parameters-of-type ((lambda-list macro-lambda-list)
                                     (type (eql :rest-parameter)) parameter)
  (check-type parameter (or null rest-parameter))
  (clone-lambda-list-element lambda-list
   :rest-parameter (clone-lambda-list-element
                    parameter :min-arg-index (positional-parameter-count lambda-list))
   :body-parameter nil))

(defmethod merge-parameters-of-type ((lambda-list macro-lambda-list)
                                     (type (eql :body-parameter)) parameter)
  (check-type parameter (or null rest-parameter))
  (clone-lambda-list-element lambda-list
   :body-parameter (clone-lambda-list-element
                    parameter :min-arg-index (positional-parameter-count lambda-list))
   :rest-parameter nil))

(defmethod all-parameters append ((lambda-list macro-lambda-list))
  (when (body-parameter lambda-list)
    (list (body-parameter lambda-list))))

(defclass destructuring-lambda-list (macro-lambda-list)
  ()
  (:documentation "The class for nested inner lambda lists (as in
macros) and `destructuring-bind' (though it's not used for that
here)."))

(defgeneric minimum-lambda-list-class (parameter)
  (:documentation "Return the least specific subclass of
`lambda-list' `parameter' should be in."))

(defclass parameter ()
  ((%min-arg-index :initarg :min-arg-index
                   :initform (error "Must provide a minimum argument index for parameter")
                   :reader min-arg-index
                   :documentation "The minimum index an argument
must have in its (possibly inner) argument list in order to
affect the value of this parameter."))
  (:documentation "The base class for lambda list parameters."))

(defmethod minimum-lambda-list-class ((parameter parameter))
  'lambda-list)

(defclass named-parameter (parameter)
  ((%name :initarg :name
          :initform (error "A name must be provided for a named parameter")
          :reader name))
  (:documentation "The base class for all parameter classes
representing a named parameter."))

(defmethod print-object ((p named-parameter) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (prin1 (name p) stream)))

(defmethod minimum-lambda-list-class ((parameter named-parameter))
  'semiordinary-lambda-list)

(defclass destructuring-parameter (parameter)
  ((%inner-lambda-list :initarg :inner-lambda-list
                       :initform (error "The inner lambda list must be provided for a destructuring parameter")
                       :reader inner-lambda-list))
  (:documentation "The base class used for destructing
parameters/nested lambda lists (as in macros)."))

(defmethod minimum-lambda-list-class ((parameter destructuring-parameter))
  'macro-lambda-list)

(defclass required-parameter (parameter)
  ()
  (:documentation "The class for representing required
parameters."))

(defclass optional-parameter (parameter)
  ((%init-form :initarg :init-form
               :initform nil
               :reader init-form))
  (:documentation "The class for representing optional
parameters."))

(defclass keyword-parameter (optional-parameter)
  ((%keyword-name :initarg :keyword-name
                  :initform (error "A keyword parameter must have a keyword")
                  :reader keyword-name))
  (:documentation "The class for representing keyword parameters."))

(defmethod print-object ((p keyword-parameter) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (prin1 (keyword-name p) stream)))

(defclass destructuring-required-parameter (destructuring-parameter required-parameter)
  ()
  (:documentation "The class for representing required
destructuring parameters in lambda lists. "))

(defclass destructuring-optional-parameter (destructuring-parameter optional-parameter)
  ()
  (:documentation "The class for representing optional
destructuring parameters in lambda lists. "))

(defclass destructuring-keyword-parameter (destructuring-parameter keyword-parameter)
  ()
  (:documentation "The class for representing keyword
destructuring parameters in lambda lists. "))

(defclass named-required-parameter (named-parameter required-parameter)
  ()
  (:documentation "The class representing named mandatory
parameters in a lambda list."))

(defclass named-optional-parameter (named-parameter optional-parameter)
  ()
  (:documentation "The class representing named optional parameters
in a lambda list."))

(defclass rest-parameter (named-parameter)
  ()
  (:documentation "The class representing the &rest parameter in
a lambda list."))

(defclass body-parameter (rest-parameter)
  ()
  (:documentation "The class representing the &body parameter in
a lambda list."))

;; Fine-grained conditions for malformed lambda lists.

(define-condition invalid-lambda-list (condition)
  ((%arglist :reader invalid-lambda-list
             :initarg :arglist
             :initform (error "Must provide an arglist for condition")))
  (:documentation "Subclasses of this condition are signalled
whenever a malformed arglist is found during arglist
processing."))

(define-condition misplaced-element (invalid-lambda-list)
  ((%misplaced-element :reader misplaced-element
                       :initarg :misplaced-element
                       :initform (error "Must provide a misplaced element for condition"))
   (%misplaced-element-index :reader misplaced-element-index
                             :initarg :misplaced-element-index
                             :initform (error "Must provide a misplaced element index for condition")))
  (:report (lambda (condition stream)
             (format stream "Element ~A is not allowed at position ~A in ~A"
                     (misplaced-element condition)
                     (misplaced-element-index condition)
                     (invalid-lambda-list condition))))
  (:documentation "Subclasses of this condition are signalled
whenever some single element of a lambda list is in an misplaced
position."))

(define-condition misplaced-&optional (misplaced-element)
  ()
  (:documentation "Subclasses of this condition are signalled
whenever an &optional argument is misplaced in an argument
list."))

(define-condition &optional-after-&key (misplaced-&optional)
  ()
  (:report (lambda (condition stream)
             (format stream "&optional found after &key in: ~A"
                     (invalid-lambda-list condition))))
  (:documentation "This condition is signalled whenever an
&optional parameter is found after a &key argument in an argument
list."))

(define-condition &optional-after-&rest (misplaced-&optional)
  ()
  (:report (lambda (condition stream)
             (format stream "&optional found after &rest in: ~A"
                     (invalid-lambda-list condition))))
  (:documentation "This condition is signalled whenever an
&optional parameter is found after a &rest argument in an argument
list."))

(define-condition misplaced-&rest (misplaced-element)
  ()
  (:documentation "Subclasses of this condition are signalled
whenever a &rest parameter is misplaced in an argument list."))

(define-condition &rest-after-&key (misplaced-&rest)
  ()
  (:report (lambda (condition stream)
             (format stream "&rest found after &key in: ~A"
                     (invalid-lambda-list condition))))
  (:documentation "This condition is signalled whenever a &rest
parameter is found after a &key argument in a lambda list."))

(define-condition misplaced-&body (misplaced-element)
  ()
  (:documentation "Subclasses of this condition are signalled
whenever a &body parameter is misplaced in an argument list."))

(define-condition &body-after-&key (misplaced-&body)
  ()
  (:report (lambda (condition stream)
             (format stream "&body found after &key in: ~A"
                     (invalid-lambda-list condition))))
  (:documentation "This condition is signalled whenever a &body
parameter is found after a &key argument in a lambda list."))

(define-condition &body-and-&rest-found (misplaced-&body misplaced-&rest)
  ()
  (:report (lambda (condition stream)
             (format stream "&body and &rest found in same lambda list: ~A"
                     (invalid-lambda-list condition))))
  (:documentation "This condition is signalled whenever both a
&body and a &rest parameter is found in the same lambda list."))

(define-condition symbol-after-&allow-other-keys (misplaced-element)
  ()
  (:report (lambda (condition stream)
             (format stream "Element ~A at position ~A is not allowed after &allow-other-keys in ~A"
                     (misplaced-element condition)
                     (misplaced-element-index condition)
                     (invalid-lambda-list condition)))))

(defun make-required-parameter (parameter-data &optional (min-arg-index 0))
  "Parse `parameter-data' as a required parameter and return two
values: an appropriate parameter object and a boolean that is NIL
if the parameter cannot be part of an ordinary lambda list."
  (if (listp parameter-data)
      (values (make-instance 'destructuring-required-parameter
               :min-arg-index min-arg-index
               :inner-lambda-list (parse-lambda-list parameter-data 'destructuring-lambda-list))
              t)
      (make-instance 'named-required-parameter
       :name parameter-data
       :min-arg-index min-arg-index)))

(defun make-&optional-parameter (parameter-data &optional (min-arg-index 0))
  "Parse `parameter-data' as an optional parameter and return two
values: an appropriate parameter object and a boolean that is NIL
if the parameter cannot be part of an ordinary lambda list."
  (cond ((and (listp parameter-data)
              (listp (first parameter-data)))
         (values
          (make-instance 'destructuring-optional-parameter
           :init-form (second parameter-data)
           :min-arg-index min-arg-index
           :inner-lambda-list (parse-lambda-list (first parameter-data)
                                                 'destructuring-lambda-list))
          t))
        ((listp parameter-data)
         (make-instance 'named-optional-parameter
          :init-form (second parameter-data)
          :min-arg-index min-arg-index
          :name (first parameter-data)))
        ((symbolp parameter-data)
         (make-instance 'named-optional-parameter
          :init-form nil
          :min-arg-index min-arg-index
          :name parameter-data))
        (t (error "I have no idea how to handle ~A as an optional parameter in a lambda list" parameter-data))))

(defun make-&key-parameter (parameter-data &optional (min-arg-index 0))
  "Parse `parameter-data' as a keyword parameter and return two
values: an appropriate parameter object and a boolean that is true
if the parameter cannot be part of an ordinary lambda list."
  (cond ((and (listp parameter-data)
              (listp (first parameter-data))
              (listp (second (first parameter-data))))
         (values
          (make-instance 'destructuring-keyword-parameter
           :init-form (second parameter-data)
           :min-arg-index min-arg-index
           :keyword-name (first (first parameter-data))
           :inner-lambda-list (parse-lambda-list (second (first parameter-data))
                                                        'destructuring-lambda-list))
          t))
        ((listp parameter-data)
         (make-instance 'keyword-parameter
          :init-form (second parameter-data)
          :min-arg-index min-arg-index
          :keyword-name (if (listp (first parameter-data))
                            (first (first parameter-data))
                            (intern (string (first parameter-data)) :keyword))))
        ((symbolp parameter-data)
         (make-instance 'keyword-parameter
          :init-form nil
          :min-arg-index min-arg-index
          :keyword-name (intern (symbol-name parameter-data) :keyword)))
        (t (error "I have no idea how to handle ~A as a keyword parameter in a lambda list" parameter-data))))

(defun make-&rest-parameter (parameter-data &optional (min-arg-index 0))
  "Parse `parameter-data' as a rest parameter and return two
values: an appropriate parameter object and a boolean that is true
if the parameter cannot be part of an ordinary lambda list."
  (make-instance 'rest-parameter
   :name parameter-data
   :min-arg-index min-arg-index))

(defun make-&body-parameter (parameter-data &optional (min-arg-index 0))
  "Parse `parameter-data' as a body parameter and return two
values: an appropriate parameter object and a boolean that is true
if the parameter cannot be part of an ordinary lambda list."
  (values
   (make-instance 'body-parameter
    :name parameter-data
    :min-arg-index min-arg-index)
   t))

(defun parse-lambda-list (lambda-list &optional class)
  "Convert a provided `lambda-list' (as a list) to a
`lambda-list' object, and signal errors if the lambda list is
found to be invalid.

This function can handle ordinary lambda lists, generic function
lambda lists, macro lambda lists and extended lambda lists.

If `lambda-list' is an invalid lambda list, an appropriate subclass of
`invalid-lambda-list' will be signalled.

If `class' is non-NIL, it should be the name of a subclass of
`semiordinary-lambda-list'. A lambda list of this class will be
returned. Otherwise, `parse-lambda-list' will figure out the
right class based on the lambda list contents."
  (declare (optimize (debug 3)))
  (let ((ordinary-lambda-list-p t)
        (macro-lambda-list-p t)
        (index 0))
    (labels ((incr-index ()
               (prog1 index (incf index)))
             (update-ordinarity (new-ordinarity)
               (when ordinary-lambda-list-p
                 (setf ordinary-lambda-list-p (not new-ordinarity))))
             (required-parameter (parameter)
               (multiple-value-bind (parameter unordinaryp)
                   (make-required-parameter parameter (incr-index))
                 (prog1 parameter (update-ordinarity unordinaryp))))
             (&optional-parameter (parameter)
               (multiple-value-bind (parameter unordinaryp)
                   (make-&optional-parameter parameter (incr-index))
                 (prog1 parameter (update-ordinarity unordinaryp))))
             (&key-parameter (parameter)
               (multiple-value-bind (parameter unordinaryp)
                   (make-&key-parameter parameter index)
                 (prog1 parameter (update-ordinarity unordinaryp)))))
      (multiple-value-bind (required optional keyword allow-other-keys rest body)
          (macrolet ((in (&key optional key rest body)
                       `(setf in-required nil
                              in-&optional ,optional
                              in-&key ,key
                              in-&rest ,rest
                              in-&body ,body))
                     (misplaced (condition &rest args)
                       `(error ',condition :arglist lambda-list
                                           :misplaced-element element
                                           :misplaced-element-index index
                                           ,@args)))
            (loop
               for element in lambda-list
               with in-required = t
               with in-&optional = nil
               with in-&key = nil
               with in-&rest = nil
               with in-&body = nil
               with saw-&allow-other-keys = nil
               with saw-&rest-or-&body-param = nil
               if saw-&allow-other-keys
               do (misplaced symbol-after-&allow-other-keys)
               else if (lambda-list-keyword-p element)
               do (case element
                    (&optional
                     (cond (in-&key
                            (misplaced &optional-after-&key))
                           (in-&rest
                            (misplaced &optional-after-&rest))
                           (t (in :optional t))))
                    (&key
                     (in :key t))
                    (&rest
                     (cond (in-&key
                            (misplaced &rest-after-&key))
                           (in-&body
                            (misplaced &body-and-&rest-found))
                           (t (in :rest t))))
                    (&body
                     (cond (in-&key
                            (misplaced &body-after-&key))
                           (in-&rest
                            (misplaced &body-and-&rest-found))
                           (t (in :body t))))
                    (&allow-other-keys
                     (setf saw-&allow-other-keys t)))
               else if in-required
               collect (required-parameter element) into required
               else if in-&optional
               collect (&optional-parameter element) into optional
               else if in-&key
               collect (&key-parameter element) into keyword
               else if in-&rest
               if saw-&rest-or-&body-param
               do (misplaced misplaced-element)
               end and
               collect (make-&rest-parameter element index) into rest and
               do (setf saw-&rest-or-&body-param t)
               else if in-&body
               if saw-&rest-or-&body-param
               do (misplaced misplaced-element)
               end and
               do (setf ordinary-lambda-list-p nil) and
               collect (make-&body-parameter element index) into body and
               do (setf saw-&rest-or-&body-param t)
               finally (return (values required optional keyword saw-&allow-other-keys rest body))))
        (assert (not (and body rest)) nil
                "There cannot be both &body and &rest in a lambda list")
        (let ((lambda-list-class (cond (class class)
                                       (ordinary-lambda-list-p 'ordinary-lambda-list)
                                       (macro-lambda-list-p 'macro-lambda-list))))
          (apply #'make-instance lambda-list-class
           :original-lambda-list lambda-list
           :required-parameters required
           :optional-parameters optional
           :keyword-parameters keyword
           :allow-other-keys allow-other-keys
           :rest-parameter (first rest)
           (when (subtypep 'macro-lambda-list lambda-list-class)
             (list :body-parameter (first body)))))))))

(defgeneric lambda-list-as-list (lambda-list)
  (:documentation "Return the list version of the provided lambda
list object. This could be considered \"serialization\" of the
lambda list object."))

(defgeneric serialize-lambda-list-parameter (element)
  (:documentation "Used by `lambda-list-as-list' to convert
lambda list parameter objects to symbols or lists."))

(defmethod serialize-lambda-list-parameter ((element named-parameter))
  (name element))

(defmethod serialize-lambda-list-parameter ((element named-optional-parameter))
  (if (init-form element)
      (list (name element) (init-form element))
      (name element)))

(defmethod serialize-lambda-list-parameter ((element keyword-parameter))
  (if (init-form element)
      (list (keyword-name element) (init-form element))
      (keyword-name element)))

(defmethod serialize-lambda-list-parameter ((element destructuring-required-parameter))
  (lambda-list-as-list (inner-lambda-list element)))

(defmethod serialize-lambda-list-parameter ((element destructuring-optional-parameter))
  (append (list (lambda-list-as-list (inner-lambda-list element)))
          (when (init-form element)
            (list (init-form element)))))

(defmethod serialize-lambda-list-parameter ((element destructuring-keyword-parameter))
  (append (list (keyword-name element)
                (lambda-list-as-list (inner-lambda-list element)))
          (when (init-form element)
            (list (init-form element)))))

;; The following two methods are annoyingly similar.
(defmethod lambda-list-as-list ((lambda-list ordinary-lambda-list))
  (flet ((serialize-parameters (parameters)
           (mapcar #'serialize-lambda-list-parameter parameters)))
    (let ((required (serialize-parameters (required-parameters lambda-list)))
          (optional (serialize-parameters (optional-parameters lambda-list)))
          (rest (rest-parameter lambda-list))
          (keyword (serialize-parameters (keyword-parameters lambda-list)))
          (allow-other-keys (allow-other-keys-p lambda-list)))
      (nconc required
             (when optional
               (cons '&optional optional))
             (when rest
               (list '&rest (serialize-lambda-list-parameter rest)))
             (when keyword
               (cons '&key keyword))
             (when allow-other-keys
               (list '&allow-other-keys))))))

(defmethod lambda-list-as-list ((lambda-list macro-lambda-list))
  (flet ((serialize-parameters (parameters)
           (mapcar #'serialize-lambda-list-parameter parameters)))
    (let ((required (serialize-parameters (required-parameters lambda-list)))
          (optional (serialize-parameters (optional-parameters lambda-list)))
          (rest (rest-parameter lambda-list))
          (body (body-parameter lambda-list))
          (keyword (serialize-parameters (keyword-parameters lambda-list)))
          (allow-other-keys (allow-other-keys-p lambda-list)))
      (nconc required
             (when optional
               (cons '&optional optional))
             (when rest
               (list '&rest (serialize-lambda-list-parameter rest)))
             (when body
               (list '&body (serialize-lambda-list-parameter body)))
             (when keyword
               (cons '&key keyword))
             (when allow-other-keys
               (list '&allow-other-keys))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; indentation

(defgeneric indent-form (syntax tree path))

(defmethod indent-form ((syntax lisp-syntax) (tree form*) path)
  (cond ((or (null path)
             (and (null (cdr path)) (zerop (car path))))
         (values tree 0))
        ((null (cdr path))
         (values (elt-noncomment (children tree) (1- (car path))) 0))
        (t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

(defmethod indent-form ((syntax lisp-syntax) (tree string-form) path)
  (values (form-toplevel syntax tree) 0))

(defmethod indent-form ((syntax lisp-syntax) (tree reader-conditional-form) path)
  (cond ((or (null path)
             (and (null (cdr path)) (zerop (car path))))
         (values tree 0))
        ((null (cdr path))
         (values (first-form (children tree)) 0))))

(defmethod indent-form ((syntax lisp-syntax) (tree readtime-evaluation-form) path)
  (if (null (cdr path))
      (values tree 0)
      (indent-form syntax (elt-form (children tree) 0) (cdr path))))

(defgeneric compute-list-indentation (syntax symbol tree path))

(defmethod indent-form ((syntax lisp-syntax) (tree list-form) path)
  (if (and (= (car path) 1)
           (null (cdr path)))
      ;; Before first element.
      (values tree 1)
      (let ((first-child (elt-noncomment (children tree) 1)))
        (cond ((form-token-p first-child)
               (compute-list-indentation syntax (form-to-object syntax first-child) tree path))
              ((null (cdr path))
               ;; top level
               (if (= (car path) 2)
                   ;; indent like first element
                   (values (elt-noncomment (children tree) 1) 0)
                   ;; indent like second element
                   (values (elt-noncomment (children tree) 2) 0)))
              (t
               ;; inside a subexpression
               (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))))

(defmethod indent-form ((syntax lisp-syntax) (tree simple-vector-form) path)
  (if (= (car path) 1)
      ;; Before first element.
      (values tree 1)
      (cond ((null (cdr path))
             ;; Top level, indent like first element.
             (values (elt-noncomment (children tree) 1) 0))
            (t
             ;; Inside a subexpression.
             (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defmethod indent-form ((syntax lisp-syntax) (tree token-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree error-symbol) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree long-comment-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree pathname-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree quote-form) path)
  (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))

(defmethod indent-form ((syntax lisp-syntax) (tree backquote-form) path)
  (if (null (cdr path))
      (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-form ((syntax lisp-syntax) (tree comma-form) path)
  (if (null (cdr path))
      (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-form ((syntax lisp-syntax) (tree comma-at-form) path)
  (if (null (cdr path))
      (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-form ((syntax lisp-syntax) (tree comma-dot-form) path)
  (if (null (cdr path))
      (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-form ((syntax lisp-syntax) (tree function-form) path)
  (if (null (cdr path))
      (values tree 0)
      (indent-form syntax (elt-form (children tree) 0) (cdr path))))

(defgeneric indent-binding (syntax tree path))

(defmethod indent-binding ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; top level
      (cond ((= (car path) 1)
             ;; before variable, indent 1
             (values tree 1))
            ((= (car path) 2)
             ;; between variable and value
             (values (elt-noncomment (children tree) 1) 0))
            (t
             ;; after value
             (values (elt-noncomment (children tree) 2) 0)))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defgeneric indent-bindings (syntax tree path))

(defmethod indent-bindings ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; entire bind form
      (if (= (car path) 1)
          ;; before first binding, indent 1
          (values tree 1)
          ;; after some bindings, align with first binding
          (values (elt-noncomment (children tree) 1) 0))
      ;; inside a bind form
      (indent-binding syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation ((syntax lisp-syntax) (symbol symbol) tree path)
  (if (null (cdr path))
      ;; top level
      (let* ((arglist (when (fboundp symbol)
                        (arglist-for-form syntax symbol)))
             (body-or-rest-arg (when arglist
                                 (or (body-parameter arglist)
                                     (rest-parameter arglist))))
             (body-or-rest-pos (when body-or-rest-arg
                                 (min-arg-index body-or-rest-arg))))
        (if (and (or (macro-function symbol)
                     (special-operator-p symbol))
                 (and (not (null body-or-rest-pos))
                      (plusp body-or-rest-pos)))
            ;; macro-form with "interesting" arguments.
            (if (>= (- (car path) 2) body-or-rest-pos)
                ;; &body arg.
                (values (elt-noncomment (children tree) 1) 1)
                ;; non-&body-arg.
                (values (elt-noncomment (children tree) 1) 1))
            ;; normal form.
            (call-next-method)))
      ;; inside a subexpression
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation ((syntax lisp-syntax) symbol tree path)
  (if (null (cdr path))
      ;; normal form.
      (if (= (car path) 2)
          ;; indent like first child
          (values (elt-noncomment (children tree) 1) 0)
          ;; indent like second child
          (values (elt-noncomment (children tree) 2) 0))
      ;; inside a subexpression
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

;; TODO: `define-simple-indentor' is not flexible enough for the
;; indentation rules of `progn' and `multiple-value-bind' (and a few
;; others). TODO: Write a more powerful `define-indentor'.

(defmethod compute-list-indentation ((syntax lisp-syntax) (symbol (eql 'progn)) tree path)
  (if (null (cdr path))
      ;; normal form.
      (if (= (car path) 2)
          (values (elt-noncomment (children tree) 1) 1)
          (values (elt-noncomment (children tree) 2) 0))
      ;; inside a subexpression
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation ((syntax lisp-syntax)
                                     (symbol (eql 'multiple-value-bind)) tree path)
  (cond ((null (cdr path))
         (cond
           ;; Value bindings.
           ((= (car path) 2) (values tree 6))
           ;; Values form.
           ((= (car path) 3) (values tree 4))
           ;; First element of body.
           ((= (car path) 4) (values tree 2))
           ;; More body elements, indent to first body element, like
           ;; `progn'.
           (t (values (elt-noncomment (children tree) 4) 0))))
        ((= (car path) 2)
         (indent-list syntax (elt-noncomment (children tree) 2)
                      (cdr path)))
        ((= (car path) 3)
         (indent-form syntax (elt-noncomment (children tree) 3)
                      (cdr path)))
        (t
         (indent-form syntax
                      (elt-noncomment (children tree) (car path))
                      (cdr path)))))

(defmacro define-list-indentor (name element-indentor)
  `(defun ,name (syntax tree path)
     (if (null (cdr path))
         ;; top level
         (if (= (car path) 1)
             ;; indent one more than the list
             (values tree 1)
             ;; indent like the first element
             (values (elt-noncomment (children tree) 1) 0))
         ;; inside an element
         (,element-indentor syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

;;; line up the elements vertically
(define-list-indentor indent-list indent-list)

;;; for now the same as indent-list, but try to do better with
;;; optional parameters with default values
(define-list-indentor indent-ordinary-lambda-list indent-list)
;;; again, can do better
(define-list-indentor indent-macro-lambda-list indent-list)
;;; FIXME: also BOA, DEFSETF, DEFTYPE, SPECIALIZED, GENERIC-FUNCTION,
;;; DESTRUCTURING, DEFINE-MODIFY-MACRO and
;;; DEFINE-METHOD-COMBINATION-ARGUMENTS

(defmacro define-simple-indentor (template)
  `(defmethod compute-list-indentation
       ((syntax lisp-syntax) (symbol (eql ',(car template))) tree path)
     (cond ((null (cdr path))
            (values tree (if (<= (car path) ,(length template)) 4 2)))
           ,@(loop for fun in (cdr template)
                  for i from 2
                  collect `((= (car path) ,i)
                            (,fun syntax (elt-noncomment (children tree) ,i) (cdr path))))
           (t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(define-simple-indentor (prog1 indent-form))
(define-simple-indentor (prog2 indent-form indent-form))
(define-simple-indentor (locally))
(define-simple-indentor (let indent-bindings))
(define-simple-indentor (let* indent-bindings))
(define-simple-indentor (defun indent-list indent-ordinary-lambda-list))
(define-simple-indentor (defmacro indent-list indent-macro-lambda-list))
(define-simple-indentor (with-slots indent-bindings indent-form))
(define-simple-indentor (with-accessors indent-bindings indent-form))
(define-simple-indentor (when indent-form))
(define-simple-indentor (unless indent-form))
(define-simple-indentor (print-unreadable-object indent-list))
(define-simple-indentor (defvar indent-form))
(define-simple-indentor (defparameter indent-form))
(define-simple-indentor (defconstant indent-form))
(define-simple-indentor (lambda indent-ordinary-lambda-list))
(define-simple-indentor (pprint-logical-block indent-list))

;;; non-simple-cases: LOOP, MACROLET, FLET, LABELS

;;; do this better
(define-list-indentor indent-slot-specs indent-list)

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defclass)) tree path)
  (if (null (cdr path))
      ;; top level
      (values tree (if (<= (car path) 3) 4 2))
      (case (car path)
        ((2 3)
         ;; in the class name or superclasses respectively
         (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
        (4
         ;; in the slot specs
         (indent-slot-specs syntax (elt-noncomment (children tree) 4) (cdr path)))
        (t
         ;; this is an approximation, might want to do better
         (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defgeneric)) tree path)
  (if (null (cdr path))
      ;; top level
      (values tree (if (<= (car path) 3) 4 2))
      (case (car path)
        (2
         ;; in the function name
         (indent-list syntax (elt-noncomment (children tree) 2) (cdr path)))
        (3
         ;; in the lambda-list
         (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) 3) (cdr path)))
        (t
         ;; in the options or method specifications
         (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defmethod)) tree path)
  (let ((lambda-list-pos (position-if #'form-list-p
                                      (remove-if #'comment-p (children tree)))))
    (cond ((null (cdr path))
           ;; top level
           (values tree (if (or (null lambda-list-pos)
                                (<= (car path) lambda-list-pos))
                            4
                            2)))
          ((or (null lambda-list-pos)
               (< (car path) lambda-list-pos))
           (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
          ((= (car path) lambda-list-pos)
           (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
          (t
           (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defun indent-clause (syntax tree path)
  (if (null (cdr path))
      ;; top level
      (case (car path)
        (1 (values tree 1))
        (2 (values tree 1))
        (t (values (elt-noncomment (children tree) 2) 0)))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'cond)) tree path)
  (if (null (cdr path))
      ;; top level
      (if (= (car path) 2)
          ;; after `cond'
          (values tree 2)
          ;; indent like the first clause
          (values (elt-noncomment (children tree) 2) 0))
      ;; inside a clause
      (indent-clause syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(macrolet ((def (symbol)
               `(defmethod compute-list-indentation
                 ((syntax lisp-syntax) (symbol (eql ',symbol)) tree path)
                 (if (null (cdr path))
                     (case (car path)
                       (2 (values tree 4))
                       (3 (values tree 2))
                       (t (values (elt-noncomment (children tree) 3) 0)))
                     (indent-clause syntax (elt-noncomment (children tree) (car path)) (cdr path))))))
  (def case)
  (def ccase)
  (def ecase)
  (def typecase)
  (def ctypecase)
  (def etypecase))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'tagbody)) tree path)
  (if (null (cdr path))
      ;; this TOKEN-MIXIN test is not quite right.  It should be a
      ;; test for symbolness of the token, but it shouldn't depend on
      ;; the symbol existing in the current image.  (Arguably, too,
      ;; this is a broken indentation form because it doesn't carry
      ;; over to the implicit tagbodies in macros such as DO.
      (if (form-token-p (elt-noncomment (children tree) (car path)))
          (values tree 2)
          (values tree 4))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-local-function-definition ((syntax lisp-syntax) tree path)
  (cond ((null (cdr path))
         ;; top level
         (cond ((= (car path) 1)
                ;; before name, indent 1
                (values tree 1))
               ((= (car path) 2)
                ;; between name and lambda list, indent 4
                (values (elt-noncomment (children tree) 1) 4))
               (t
                ;; after lambda list, indent 2
                (values (elt-noncomment (children tree) 1) 2))))
        ((= (car path) 1)
         ;; inside lambda list
         (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) 1) (cdr path)))
        (t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

(define-list-indentor indent-local-function-definitions indent-local-function-definition)

(define-simple-indentor (flet indent-local-function-definitions))
(define-simple-indentor (labels indent-local-function-definitions))
(define-simple-indentor (with-open-file indent-list))

;;; CLIM indentation

(define-simple-indentor (clim:with-output-as-presentation indent-list))
(define-simple-indentor (clim:vertically indent-list))
(define-simple-indentor (clim:horizontally indent-list))
(define-simple-indentor (clim:scrolling indent-list))
(define-simple-indentor (clim:with-drawing-options indent-list))
(define-simple-indentor (clim:define-command-table indent-list))
(define-simple-indentor (clim:define-command indent-list indent-list))
(define-simple-indentor (clim:define-application-frame indent-list indent-list))

(defun compute-path-in-trees (trees n offset)
  (cond ((or (null (first-noncomment trees))
             (>= (start-offset (first-noncomment trees)) offset))
         (list n))
        ((or (< (start-offset (first-noncomment trees)) offset (end-offset (first-noncomment trees)))
             (typep (first-noncomment trees) 'incomplete-form-mixin))
         (cons n (compute-path-in-tree (first-noncomment trees) offset)))
        (t (compute-path-in-trees (rest-noncomments trees) (1+ n) offset))))

(defun compute-path-in-tree (tree offset)
  (if (null (children tree))
      '()
      (compute-path-in-trees (children tree) 0 offset)))

(defun compute-path (syntax offset)
  (with-slots (stack-top) syntax
    (compute-path-in-tree stack-top offset)))

(defun real-column-number (mark tab-width)
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop with column = 0
          until (mark= mark mark2)
          do (if (eql (object-after mark2) #\Tab)
                 (loop do (incf column)
                       until (zerop (mod column tab-width)))
                 (incf column))
          do (incf (offset mark2))
          finally (return column))))

(defmethod syntax-line-indentation ((syntax lisp-syntax) mark tab-width)
  (update-parse syntax 0 (offset mark))
  (setf mark (clone-mark mark))
  (beginning-of-line mark)
  (with-slots (stack-top) syntax
    (let ((path (compute-path syntax (offset mark))))
      (multiple-value-bind (tree offset)
          (indent-form syntax stack-top path)
        (setf (offset mark) (start-offset tree))
        (+ (real-column-number mark tab-width)
           offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commenting

(defmethod syntax-line-comment-string ((syntax lisp-syntax))
  ";;; ")

(defmethod comment-region ((syntax lisp-syntax) mark1 mark2)
  (line-comment-region syntax mark1 mark2))

(defmethod uncomment-region ((syntax lisp-syntax) mark1 mark2)
  (line-uncomment-region syntax mark1 mark2))
