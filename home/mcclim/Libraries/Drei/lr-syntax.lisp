;; -*- Mode: Lisp; Package: DREI-LR-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2007 by
;;;           John Q Splittist (splittist@gmail.com)
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

;;; Base lexing and parsing functionality of 
;;; syntax modules for analysing languages

(in-package :drei-lr-syntax)

(defclass lr-syntax-mixin () 
     ((stack-top :initform nil
                 :accessor stack-top)
      (potentially-valid-trees)
      (lookahead-lexeme :initform nil :accessor lookahead-lexeme)
      (current-state)
      (initial-state :initarg :initial-state)
      (current-start-mark)
      (current-size)
      (scan :accessor scan)))

(defmethod initialize-instance :after ((syntax lr-syntax-mixin) &rest args)
  (declare (ignore args))
  (with-accessors ((buffer buffer) (scan scan)) syntax
    (setf scan (make-buffer-mark buffer 0 :left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defgeneric skip-inter (syntax state scan)
  (:documentation "advance scan until the beginning of a new
    lexeme.  Return T if one can be found and NIL otherwise."))

(defgeneric lex (syntax state scan)
  (:documentation "Return the next lexeme starting at scan."))

(defmethod lex :around (syntax state scan)
  (when (skip-inter syntax state scan)
    (let* ((start-offset (offset scan))
	   (lexeme (call-next-method))
	   (new-size (- (offset scan) start-offset)))
      (with-slots (start-mark size) lexeme
	 (setf (offset scan) start-offset)
	 (setf start-mark scan
	       size new-size))
      lexeme)))

(defclass lexer-state ()
  ()
  (:documentation "These states are used to determine how the lexer
    should behave."))

(defmacro define-lexer-state (name superclasses &body body)
  `(defclass ,name (,@superclasses lexer-state)
      ,@body))

(define-lexer-state lexer-error-state ()
  ()
  (:documentation "In this state, the lexer returns error lexemes
    consisting of entire lines of text"))

(define-lexer-state lexer-toplevel-state ()
  ()
  (:documentation "In this state, the lexer assumes it can skip
    whitespace and should recognize ordinary lexemes of the language."))

(defclass parser-symbol ()
  ((start-mark :initform nil :initarg :start-mark :reader start-mark)
   (size :initform nil :initarg :size :reader size)
   (parent :initform nil :accessor parent)
   (children :initform '() :initarg :children :reader children)
   (preceding-parse-tree :initform nil :reader preceding-parse-tree)
   (parser-state :initform nil :initarg :parser-state :reader parser-state)))

(defmethod print-object ((object parser-symbol) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (start-offset object) (end-offset object))))

(defclass literal-object-mixin () ()
  (:documentation "Mixin for parser symbols representing
literal (non-character) objects in the buffer."))

(defun literal-object-p (parser-symbol)
  "Return true if `parser-symbol' is of type
`literal-object-mixin'."
  (typep parser-symbol 'literal-object-mixin))

(defmethod start-offset ((state parser-symbol))
  (let ((mark (start-mark state)))
    (when mark
      (offset mark))))

(defmethod end-offset ((state parser-symbol))
  (with-slots (start-mark size) state
     (when start-mark
       (+ (offset start-mark) size))))

(defgeneric action (syntax state lexeme))
(defgeneric new-state (syntax state parser-symbol))

(defclass parser-state () ())

(defmacro define-parser-state (name superclasses &body body)
  `(progn
     (defclass ,name ,superclasses
	  ,@body)
     (defvar ,name (make-instance ',name))))

(defclass lexeme (parser-symbol) ())

(defclass nonterminal (parser-symbol) ())

(defmethod initialize-instance :after ((parser-symbol nonterminal) &rest args)
  (declare (ignore args))
  (with-slots (children start-mark size) parser-symbol
     (loop for child in children
	   do (setf (parent child) parser-symbol))
     (let ((start (find-if-not #'null children :key #'start-offset))
	   (end (find-if-not #'null children :key #'end-offset :from-end t)))
       (when start
	 (setf start-mark (slot-value start 'start-mark)
	       size (- (end-offset end) (start-offset start)))))))

(defun pop-one (syntax)
  (with-slots (stack-top current-state) syntax
     (with-slots (preceding-parse-tree parser-state) stack-top
	(prog1 stack-top
	       (setf current-state parser-state
		     stack-top preceding-parse-tree)))))

(defun pop-number (syntax how-many)
  (loop with result = '()
	repeat how-many
	do (push (pop-one syntax) result)
	finally (return result)))

(defmacro reduce-fixed-number (symbol nb-children)
  `(let ((result (make-instance ',symbol :children (pop-number syntax ,nb-children))))
     (when (zerop ,nb-children)
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))

(defun pop-until-type (syntax type)
  (with-slots (stack-top) syntax
     (loop with result = '()
	   for child = stack-top
	   do (push (pop-one syntax) result)
	   until (typep child type)
	   finally (return result))))

(defmacro reduce-until-type (symbol type &optional end-of-buffer)
  `(let ((result (make-instance ',symbol
                  :children (pop-until-type syntax ',type))))
     (with-slots (start-mark size) result
       (when (null (children result))
         (with-slots (scan) syntax
           (setf start-mark (clone-mark scan :right)
                 size 0)))
       (when ,end-of-buffer
         (setf size (- (size (buffer syntax))
                       (start-offset result)))))
     result))

(defun pop-all (syntax)
  (with-slots (stack-top) syntax
     (loop with result = '()
	   until (null stack-top)
	   do (push (pop-one syntax) result)
	   finally (return result))))

(defmacro reduce-all (symbol)
  `(let ((result (make-instance ',symbol :children (pop-all syntax))))
     (when (null (children result))
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))

(define-parser-state error-state (lexer-error-state parser-state) ())
(define-parser-state error-reduce-state (lexer-toplevel-state parser-state) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser step

(defgeneric parser-step (syntax))

(defmethod parser-step ((syntax lr-syntax-mixin))
  (with-slots (lookahead-lexeme stack-top current-state scan) syntax
     (setf lookahead-lexeme (lex syntax current-state (clone-mark scan :right)))
     (let* ((new-parser-symbol (action syntax current-state lookahead-lexeme))
	    (new-state (new-state syntax current-state new-parser-symbol)))
       (with-slots (parser-state parser-symbol preceding-parse-tree children) new-parser-symbol
	  (setf parser-state current-state
		current-state new-state
		preceding-parse-tree stack-top
		stack-top new-parser-symbol)))
     (setf (offset scan) (end-offset stack-top))))

(defun prev-tree (tree)
  (assert (not (null tree)))
  (if (null (children tree))
      (preceding-parse-tree tree)
      (car (last (children tree)))))

(defun next-tree (tree)
  (assert (not (null tree)))
  (if (null (parent tree))
      nil
      (let* ((parent (parent tree))
	     (siblings (children parent)))
	(cond ((null parent) nil)
	      ((eq tree (car (last siblings))) parent)
	      (t (loop with new-tree = (cadr (member tree siblings :test #'eq))
		       until (null (children new-tree))
		       do (setf new-tree (car (children new-tree)))
		       finally (return new-tree)))))))

(defun find-last-valid-lexeme (parse-tree offset)
  (cond ((or (null parse-tree) (null (start-offset parse-tree))) nil)
	((> (start-offset parse-tree) offset)
	 (find-last-valid-lexeme (preceding-parse-tree parse-tree) offset))
	((not (typep parse-tree 'lexeme))
	 (find-last-valid-lexeme (car (last (children parse-tree))) offset))
	((>= (end-offset parse-tree) offset)
	 (find-last-valid-lexeme (preceding-parse-tree parse-tree) offset))
	(t parse-tree)))

(defun find-first-potentially-valid-lexeme (parse-trees offset)
  (cond ((null parse-trees) nil)
	((or (null (start-offset (car parse-trees)))
	     (< (end-offset (car parse-trees)) offset))
	 (find-first-potentially-valid-lexeme (cdr parse-trees) offset))
	((not (typep (car parse-trees) 'lexeme))
	 (find-first-potentially-valid-lexeme (children (car parse-trees)) offset))
	((<= (start-offset (car parse-trees)) offset)
	 (loop with tree = (next-tree (car parse-trees))
	       until (or (null tree) (> (start-offset tree) offset))
	       do (setf tree (next-tree tree))
	       finally (return tree)))
	(t (car parse-trees))))

(defun parse-tree-equal (tree1 tree2)
  (and (eq (class-of tree1) (class-of tree2))
       (eq (parser-state tree1) (parser-state tree2))
       (= (end-offset tree1) (end-offset tree2))))

(defmethod print-object ((mark mark) stream)
  (print-unreadable-object (mark stream :type t :identity t)
    (format stream "~s" (offset mark))))

(defun parse-patch (syntax)
  (with-slots (current-state stack-top scan potentially-valid-trees) syntax
    (parser-step syntax)
    (finish-output *trace-output*)
    (cond ((parse-tree-equal stack-top potentially-valid-trees)
           (unless (or (null (parent potentially-valid-trees))
                       (eq potentially-valid-trees
                           (car (last (children (parent potentially-valid-trees))))))
             (loop for tree = (cadr (member potentially-valid-trees
                                            (children (parent potentially-valid-trees))
                                            :test #'eq))
                then (car (children tree))
                until (null tree)
                do (setf (slot-value tree 'preceding-parse-tree)
                         stack-top))
             (setf stack-top (prev-tree (parent potentially-valid-trees))))
           (setf potentially-valid-trees (parent potentially-valid-trees))
           (setf current-state (new-state syntax (parser-state stack-top) stack-top))
           (setf (offset scan) (end-offset stack-top)))
          (t (loop until (or (null potentially-valid-trees)
                             (>= (start-offset potentially-valid-trees)
                                 (end-offset stack-top)))
                do (setf potentially-valid-trees
                         (next-tree potentially-valid-trees)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility functions

(defun invoke-do-parse-symbols-forward (start-offset nearby-symbol fn)
  "Loop across the parse symbols of the syntax, calling `fn' on
any parse symbol that starts at or after
`start-offset'. `Nearby-symbol' is the symbol at which the
iteration will start. First, if `nearby-symbol' is at or after
`start-offset', `fn' will be called on
`nearby-symbol'. Afterwards, the children of `nearby-symbol' will
be looped over. Finally, the process will be repeated for each
sibling of `nearby-symbol'. It is guaranteed that `fn' will not
be called twice for the same parser symbol."
  (labels ((act (parse-symbol previous)
             (when (>= (end-offset parse-symbol) start-offset)
               (when (>= (start-offset parse-symbol) start-offset)
                 (funcall fn parse-symbol))
               (loop for child in (children parse-symbol)
                  unless (eq child previous)
                  do (act child parse-symbol)))
             (unless (or (null (parent parse-symbol))
                         (eq (parent parse-symbol) previous))
               (act (parent parse-symbol) parse-symbol))))
    (act nearby-symbol nearby-symbol)))

(defmacro do-parse-symbols-forward ((symbol start-offset enclosing-symbol)
                                    &body body)
  "Loop across the parse symbols of the syntax, evaluating `body'
with `symbol' bound for each parse symbol that starts at or after
`start-offset'. `enclosing-symbol' is the symbol at which the
iteration will start. First, if `enclosing-symbol' is at or after
`start-offset', `symbol' will be bound to
`enclosing-symbol'. Afterwards, the children of
`enclosing-symbol' will be looped over. Finally, the process will
be repeated for each sibling of `nearby-symbol'. It is guaranteed
that `symbol' will not bound to the same parser symbol twice."
  `(invoke-do-parse-symbols-forward ,start-offset ,enclosing-symbol
                                    #'(lambda (,symbol)
                                        ,@body)))

(defun parser-symbol-containing-offset (syntax offset)
  "Find the most specific (leaf) parser symbol in `syntax' that
contains `offset'. If there is no such parser symbol, return the
stack-top of `syntax'."
  (labels ((check (parser-symbol)
             (cond ((or (and (<= (start-offset parser-symbol) offset)
                             (< offset (end-offset parser-symbol)))
                        (= offset (start-offset parser-symbol)))
                    (return-from parser-symbol-containing-offset
                      (if (null (children parser-symbol))
                          parser-symbol
                          (or (check-children (children parser-symbol))
                              parser-symbol))))
                   (t nil)))
           (check-children (children)
             (find-if #'check children)))
    (or (check-children (children (stack-top syntax)))
        (stack-top syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defmethod update-syntax values-max-min ((syntax lr-syntax-mixin) prefix-size suffix-size
                                         &optional (begin 0) (end (size (buffer syntax))))
  (declare (ignore begin end))
  (let* ((low-mark-offset prefix-size)
	 (high-mark-offset (- (size (buffer syntax)) suffix-size)))
    (when (<= low-mark-offset high-mark-offset)
      (catch 'done
        (with-slots (current-state stack-top scan potentially-valid-trees
                                   initial-state) syntax
          (setf potentially-valid-trees
                (if (null stack-top)
                    nil
                    (find-first-potentially-valid-lexeme (children stack-top)
                                                         high-mark-offset)))
          (setf stack-top (find-last-valid-lexeme stack-top low-mark-offset))
          (setf (offset scan) (if (null stack-top) 0 (end-offset stack-top))
                current-state (if (null stack-top)
                                  initial-state
                                  (new-state syntax
                                             (parser-state stack-top)
                                             stack-top)))
          (loop do (parse-patch syntax)))))
    (values 0 (size (buffer syntax)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General redisplay for LR syntaxes, subclasses of `lr-syntax-mixin'
;;; should be able to easily define some syntax rules, and need not
;;; bother with all this complexity.
;;;
;;;          _______________
;;;         /               \
;;;        /                 \
;;;       /                   \
;;;       |   XXXX     XXXX   |
;;;       |   XXXX     XXXX   |
;;;       |   XXX       XXX   |
;;;       |         X         |
;;;       \__      XXX      __/
;;;         |\     XXX     /|
;;;         | |           | |
;;;         | I I I I I I I |
;;;         |  I I I I I I  |
;;;         \_             _/
;;;           \_         _/
;;;             \_______/
;;;     XXX                    XXX
;;;    XXXXX                  XXXXX
;;;    XXXXXXXXX         XXXXXXXXXX
;;;           XXXXX   XXXXX
;;;              XXXXXXX
;;;           XXXXX   XXXXX
;;;    XXXXXXXXX         XXXXXXXXXX
;;;    XXXXX                  XXXXX
;;;     XXX                    XXX

(defmacro define-syntax-highlighting-rules (name &body rules)
  "Define a set of rules for highlighting a syntax. `Name', which
must be a symbol, is the name of this set of rules, and will be
bound to a function implementing the rules. `Rules' is a list of
rules of the form `(parser-symbol (type args...))', where
`parser-symbol' is a type that might be encountered in a parse
tree for the syntax. The rule specifies how to highlight that
kind of object (and all its children). `Type' can be one of three
special symbols.

  `:face', in which case `args' will be used as arguments to a
  call to `make-face'. The resulting face will be used to draw
  the parsersymbol.

  `:options', in which case `args' will be used as arguments to
  `make-drawing-options'. The resulting options will be used to
  draw the parser symbol.

  `:function', in which case `args' must be a single element, a
  function that takes two arguments. These arguments are the view
  of the syntax and the parser symbol, and the return value of
  this function is the `drawing-options' object that will be used
  to draw the parser-symbol.

Alternatively, `type' can be any object (usually a dynamically
bound symbol), in which case it will be evaluated to get the
drawing options.

`Type' can also be a list, in which case the first element will
be interpreted as described above, and the remaining elements
will be considered keyword arguments. The following keyword
arguments are supported:

  `:sticky': if true, the syntax highlighting options defined by
  this rule will apply to all children as well, effectively
  overriding their options. The default is false. For a
  `:function', `:sticky' will not work. Instead, return a true
  secondary value from the function."
  (check-type name symbol)
  `(progn
     (fmakunbound ',name)
     (defgeneric ,name (view parser-symbol)
       (:method (view (parser-symbol parser-symbol))
         nil))
     ,@(flet ((make-rule-exp (type args)
                             (let ((actual-type (first (listed type))))
                               (destructuring-bind (&key sticky) (rest (listed type))
                                 (case actual-type
                                   (:face `(let ((options (make-drawing-options :face (make-face ,@args))))
                                             #'(lambda (view parser-symbol)
                                                 (declare (ignore view parser-symbol))
                                                 (values options ,sticky))))
                                   (:options `#'(lambda (view parser-symbol)
                                                  (declare (ignore view parser-symbol))
                                                  (values (make-drawing-options ,@args) ,sticky)))
                                   (:function (first args))
                                   (t `#'(lambda (view parser-symbol)
                                           (declare (ignore view parser-symbol))
                                           (values ,actual-type ,sticky))))))))
             (loop for (parser-symbol (type . args)) in rules
                collect `(let ((rule ,(make-rule-exp type args)))
                           (defmethod ,name (view (parser-symbol ,parser-symbol))
                             (funcall rule view parser-symbol)))))))

(define-syntax-highlighting-rules default-syntax-highlighting)

(defgeneric syntax-highlighting-rules (syntax)
  (:documentation "Return the drawing options that should be used
for displaying `parser-symbol's for `syntax'. A method should be
defined on this function for any syntax that wants syntax
highlighting.")
  (:method ((syntax lr-syntax-mixin))
    'default-syntax-highlighting))

(defun get-drawing-options (highlighting-rules view parse-symbol)
  "Get the drawing options with which `parse-symbol' should be
drawn. If `parse-symbol' or the stack-top of syntax, return
NIL. `View' must be a `drei-syntax-view' containing a syntax that
`highlighting-rules' supports."
  (when (and parse-symbol (not (eq (stack-top (syntax view)) parse-symbol)))
    (funcall highlighting-rules view parse-symbol)))

(defstruct (pump-state
             (:constructor make-pump-state
                           (parser-symbol offset drawing-options
                                          highlighting-rules)))
  "A pump state object used in the LR syntax
module. `parser-symbol' is the a parse symbol object `offset' is
in. `Drawing-options' is a stack with elements `(end-offset
drawing-options)', where `end-offset' specifies there the drawing
options specified by `drawing-options' stop. `Highlighting-rules'
is the rules that are used for syntax highlighting."
  parser-symbol offset
  drawing-options highlighting-rules)

(defstruct (drawing-options-frame
             (:constructor make-drawing-options-frame
                           (end-offset drawing-options sticky-p))
             (:conc-name frame-))
  "An entry in the drawing options stack maintained by the
`pump-state' structure. `End-offset' is the end buffer offset
for the frame, `drawing-options' is the drawing options that
should be used until that offset, and if `sticky-p' is true it
will not be possible to put other frames on top of this one in
the stack."
  end-offset drawing-options sticky-p)

(defmethod pump-state-for-offset-with-syntax ((view textual-drei-syntax-view)
                                              (syntax lr-syntax-mixin) (offset integer))
  (update-parse syntax 0 (size (buffer view)))
  (let ((parser-symbol (parser-symbol-containing-offset syntax offset))
        (highlighting-rules (syntax-highlighting-rules syntax)))
    (labels ((initial-drawing-options (parser-symbol)
               (if (null parser-symbol)
                   (make-drawing-options-frame
                    (size (buffer view)) +default-drawing-options+ nil)
                   (multiple-value-bind (drawing-options sticky)
                       (get-drawing-options highlighting-rules view parser-symbol)
                     (if (null drawing-options)
                         (initial-drawing-options (parent parser-symbol))
                         (make-drawing-options-frame (end-offset parser-symbol)
                                                     drawing-options sticky))))))
      (make-pump-state parser-symbol offset
                       (list (initial-drawing-options parser-symbol)
                             (make-drawing-options-frame
                              (1+ (size (buffer view))) +default-drawing-options+ nil))
                       highlighting-rules))))

(defun find-next-stroke-end (view pump-state)
  "Assuming that `pump-state' contains the previous pump state,
find out where the next stroke should end, and possibly push some
drawing options onto `pump-state'."
  (with-accessors ((start-symbol pump-state-parser-symbol)
                   (offset pump-state-offset)
                   (drawing-options pump-state-drawing-options)
                   (highlighting-rules pump-state-highlighting-rules))
      pump-state
    (let* ((line (line-containing-offset view offset))
           (line-end-offset (end-offset line)))
      (flet ((finish (new-offset symbol &optional stroke-drawing-options sticky-p)
               (setf start-symbol symbol)
               (loop until (> (frame-end-offset (first drawing-options))
                              new-offset)
                     do (pop drawing-options))
               (unless (null stroke-drawing-options)
                 (push (if (frame-sticky-p (first drawing-options))
                           (make-drawing-options-frame
                            (end-offset symbol) (frame-drawing-options (first drawing-options)) t)
                           (make-drawing-options-frame
                            (end-offset symbol) stroke-drawing-options sticky-p))
                       drawing-options))
               (return-from find-next-stroke-end new-offset)))
        (cond ((null start-symbol)
               ;; This means that all remaining lines are blank.
               (finish line-end-offset nil))
              ((and (typep start-symbol 'literal-object-mixin)
                    (= offset (start-offset start-symbol)))
               (finish (end-offset start-symbol) start-symbol nil))
              (t
               (or (let* ((current-frame (first drawing-options))
                          (currently-used-options (frame-drawing-options current-frame)))
                     (do-parse-symbols-forward (symbol offset start-symbol)
                       (multiple-value-bind (symbol-drawing-options sticky)
                           (get-drawing-options highlighting-rules view symbol)
                         ;; Remove frames that are no longer applicable...
                         (loop until (> (frame-end-offset (first drawing-options))
                                        (start-offset symbol))
                               do (pop drawing-options))
                         (let ((options-to-be-used (if (frame-sticky-p (first drawing-options))
                                                       (frame-drawing-options (first drawing-options))
                                                       symbol-drawing-options)))
                           (cond ((> (start-offset symbol) line-end-offset)
                                  (finish line-end-offset start-symbol))
                                 ((and (typep symbol 'literal-object-mixin))
                                  (finish (start-offset symbol) symbol
                                          (or symbol-drawing-options
                                              (make-drawing-options :function (object-drawer)))))
                                 ((and (> (start-offset symbol) offset)
                                       (not (drawing-options-equal (or options-to-be-used
                                                                       +default-drawing-options+)
                                                                   currently-used-options))
                                       (if (null symbol-drawing-options)
                                           (>= (start-offset symbol) (frame-end-offset current-frame))
                                           t))
                                  (finish (start-offset symbol) symbol symbol-drawing-options sticky))
                                 ((and (= (start-offset symbol) offset)
                                       symbol-drawing-options
                                       (not (drawing-options-equal
                                             options-to-be-used
                                             (frame-drawing-options (first drawing-options)))))
                                  (finish (start-offset symbol) symbol symbol-drawing-options sticky)))))))
                   ;; If there are no more parse symbols, we just go
                   ;; line-by-line from here. This should mean that all
                   ;; remaining lines are blank.
                   (finish line-end-offset nil))))))))

(defmethod stroke-pump-with-syntax ((view textual-drei-syntax-view)
                                    (syntax lr-syntax-mixin) stroke
                                    (pump-state pump-state))
  ;; `Pump-state' will be destructively modified.
  (prog1 pump-state
    (with-accessors ((offset pump-state-offset)
                     (current-drawing-options pump-state-drawing-options))
        pump-state
      (let ((old-drawing-options (frame-drawing-options (first current-drawing-options)))
            (end-offset (find-next-stroke-end view pump-state))
            (old-offset offset))
        (setf (stroke-start-offset stroke) offset
              (stroke-end-offset stroke) end-offset
              (stroke-drawing-options stroke) old-drawing-options
              offset (if (offset-end-of-line-p (buffer view) end-offset)
                         (1+ end-offset)
                         end-offset))
        ;; Don't use empty strokes, try again...
        (when (= old-offset offset)
          (stroke-pump-with-syntax view syntax stroke pump-state))))))
