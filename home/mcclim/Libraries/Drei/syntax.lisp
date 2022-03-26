;;; -*- Mode: Lisp; Package: DREI-SYNTAX -*-

;;;  (c) copyright 2004, 2005, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)

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

(in-package :drei-syntax)

(defclass syntax (name-mixin observable-mixin)
  ((%buffer :initarg :buffer :reader buffer)
   (%command-table :initarg :command-table
                   :initform (error "A command table has not been provided for this syntax")
                   :reader esa-command-table)
   (%updater-fns :initarg :updater-fns
                 :initform '()
                 :accessor updater-fns
                 :documentation "A list of functions that are
called whenever a syntax function needs up-to-date syntax
information. `Update-syntax' is never called directly by syntax
commands. Each function should take two arguments, integer
offsets into the buffer of the syntax delimiting the region that
must have an up-to-date parse. These arguments should be passed
on to a call to `update-syntax'."))
  (:metaclass modual-class)
  (:documentation "The base class for all syntaxes."))

(defgeneric syntax-command-tables (syntax)
  (:documentation "Returns additional command tables provided by
`syntax'.")
  (:method-combination append :most-specific-last)
  (:method append ((syntax syntax))
           (list (esa-command-table syntax))))

(defun syntaxp (object)
  "Return T if `object' is an instance of a syntax, NIL
  otherwise."
  (typep object 'syntax))

(defun update-parse (syntax &optional (begin 0)
                     (end (size (buffer syntax))))
  "Make sure the parse for `syntax' from offset `begin' to `end'
is up to date. `Begin' and `end' default to 0 and the size of the
buffer of `syntax', respectively."
  (if (null (updater-fns syntax))
      ;; Just call `update-syntax' manually. We assume the entire
      ;; buffer has changed.
      (update-syntax syntax 0 0 begin end)
      (map nil #'(lambda (updater)
                   (funcall updater begin end))
           (updater-fns syntax))))

(define-condition no-such-operation (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Operation unavailable for this syntax")))
  (:documentation "This condition is signaled whenever an attempt is
made to execute an operation that is unavailable for the particular syntax" ))

(defgeneric update-syntax (syntax unchanged-prefix unchanged-suffix
                                  &optional begin end)
  (:documentation "Inform the syntax module that it must update
its view of the buffer. `Unchanged-prefix' and `unchanged-suffix'
indicate what parts of the buffer have not been changed.

`Begin' and `end' are offsets specifying the minimum region of the
buffer that must have an up-to-date parse, defaulting to 0 and the
size of the buffer respectively. It is perfectly valid for a syntax to
ignore these hints and just make sure the entire syntax tree is up to
date, but it *must* make sure at least the region delimited by `begin'
and `end' has an up to date parse.

Returns two values, offsets into the buffer of the syntax, denoting
the buffer region that has an up-to-date parse.")
  (:method-combination values-max-min :most-specific-last))

(defgeneric eval-defun (mark syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax command tables.

(defclass syntax-command-table (standard-command-table)
  ()
  (:documentation "A syntax command table provides facilities for
having frame-specific commands that do not show up when the
syntax is used in other applications than the one it is supposed
to. For example, the Return From Definition command should be
available when Lisp syntax is used in Climacs (or another
editor), but not anywhere else."))

(defgeneric additional-command-tables (editor command-table)
  (:method-combination append)
  (:documentation "Return a list of additional command tables
that should be checked for commands in addition to those
`command-table' inherits from. The idea is that methods are
specialised to `editor' (which is at first a Drei instance), and
that those methods may call the function again recursively with a
new `editor' argument to provide arbitrary granularity for
command-table-selection. For instance, some commands may be
applicable in a situation where the editor is a pane or gadget in
its own right, but not when it functions as an input-editor. In
this case, a method could be defined for `application-frame' as
the `editor' argument, that calls `additional-command-tables'
again with whatever the \"current\" editor instance is. The
default method on this generic function just returns the empty
list.")
  (:method append (editor command-table)
    '()))

(defmethod command-table-inherit-from ((table syntax-command-table))
  "Fetch extra command tables to inherit from (using
`additional-command-tables') as well as the command tables
`table' actually directly inherits from."
  (append (mapcar #'find-command-table
                  (additional-command-tables *application-frame* table))
          (call-next-method)))

(defmacro define-syntax-command-table (name &rest args &key &allow-other-keys)
  "Define a syntax command table class with the provided name, as
well as defining a CLIM command table of the same name. `Args'
will be passed on to `make-command-table'. An :around method on
`command-table-inherit-from' for the defined class will also be
defined. This method will make sure that when an instance of the
syntax command table is asked for its inherited command tables,
it will return those of the defined CLIM command table, as well
as those provided by methods on
`additional-command-tables'. Command tables provided through
`additional-command-tables' will take precence over those
specified in the usual way with :inherit-from."
  `(progn (make-command-table ',name ,@args)
          (defclass ,name (syntax-command-table)
            ())
          (defmethod command-table-inherit-from ((table ,name))
            (append (call-next-method)
                    '(,name)
                    (command-table-inherit-from (find-command-table ',name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commenting

(defgeneric syntax-line-comment-string (syntax)
  (:documentation "string to use at the beginning of a line to
indicate a line comment"))

(defgeneric line-comment-region (syntax mark1 mark2)
  (:documentation "inset a line comment string at the beginning of
every line in the region"))

(defmethod line-comment-region (syntax mark1 mark2)
  (when (mark< mark2 mark1)
    (rotatef mark1 mark2))
  (let ((mark (clone-mark mark1)))
    (unless (beginning-of-line-p mark)
      (end-of-line mark)
      (unless (end-of-buffer-p mark)
	(forward-object mark)))
    (loop while (mark< mark mark2)
	  do (insert-sequence mark (syntax-line-comment-string syntax))
	     (end-of-line mark)
	     (unless (end-of-buffer-p mark)
	       (forward-object mark)))))

(defgeneric line-uncomment-region (syntax mark1 mark2)
  (:documentation "inset a line comment string at the beginning of
every line in the region"))

(defmethod line-uncomment-region (syntax mark1 mark2)
  (when (mark< mark2 mark1)
    (rotatef mark1 mark2))
  (let ((mark (clone-mark mark1)))
    (unless (beginning-of-line-p mark)
      (end-of-line mark)
      (unless (end-of-buffer-p mark)
	(forward-object mark)))
    (loop while (mark< mark mark2)
	  do (when (looking-at mark (syntax-line-comment-string syntax))
	       (delete-range mark (length (syntax-line-comment-string syntax))))
	     (end-of-line mark)
	     (unless (end-of-buffer-p mark)
	       (forward-object mark)))))

(defgeneric comment-region (syntax mark1 mark2)
  (:documentation "turn the region between the two marks into a comment
in the specific syntax.")
  (:method (syntax mark1 mark2) nil))

(defgeneric uncomment-region (syntax mark1 mark2)
  (:documentation "remove comment around region")
  (:method (syntax mark1 mark2) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Name for info-pane

(defgeneric name-for-info-pane (syntax &key &allow-other-keys)
  (:documentation "Return the name that should be used for the
  info-pane for panes displaying a buffer in this syntax.")
  (:method (syntax &key &allow-other-keys)
    (name syntax)))

(defgeneric display-syntax-name (syntax stream &key &allow-other-keys)
  (:documentation "Draw the name of the syntax `syntax' to
`stream'. This is meant to be called for the info-pane.")
  (:method (syntax stream &rest args &key)
    (princ (apply #'name-for-info-pane syntax args) stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax completion

(defparameter *syntaxes* '())

(defvar *default-syntax* nil
  "The name of the default syntax. Must be a symbol.

This syntax will be used by default, when no other syntax is
mandated by file types or attribute lists.")

(defstruct (syntax-description (:type list))
  (name (error "required argument") :type string)
  (class-name (error "required argument") :type symbol)
  (pathname-types nil :type list))

(defmacro define-syntax (class-name superclasses slots &rest options)
  (let ((defclass-options nil)
	(default-initargs nil)
	(name nil)
        (command-table nil)
	(pathname-types nil))
    (dolist (option options)
      (case (car option)
	((:name)
	 (if name
	     (error "More than one ~S option provided to ~S"
		    ':name 'define-syntax)
	     (setf name (cadr option))))
	((:pathname-types)
	 (if pathname-types
	     (error "More than one ~S option provided to ~S"
		    ':pathname-types 'define-syntax)
	     (setf pathname-types (cdr option))))
        ((:command-table)
         (if command-table
             (error "More than one ~S option provided to ~S"
                    ':command-table 'define-syntax)
             (setf command-table `',(cadr option))))
	((:default-initargs)
	 (if default-initargs
	     (error "More than one ~S option provided to ~S"
		    ':default-initargs 'define-syntax)
             (setf default-initargs (cdr option))))
	(t (push (cdr option) defclass-options))))
    (unless name
      (error "~S not supplied to ~S" ':name 'define-syntax))
    ;; FIXME: the :NAME initarg looks, well, a bit generic, and could
    ;; collide with user-defined syntax initargs.  Use
    ;; DREI-SYNTAX::%NAME instead.
    (setf default-initargs (list* :name name default-initargs))
    `(progn
       (push (make-syntax-description
              :name ,name :class-name ',class-name
              :pathname-types ',pathname-types)
             *syntaxes*)
       (defclass ,class-name ,superclasses ,slots
         ,(append '(:default-initargs)
                  (when command-table
                    (list :command-table
                          (once-only (command-table)
                            `(when (find-command-table ,command-table)
                               (if (find-class ,command-table nil)
                                   (make-instance ,command-table :name ,command-table)
                                   ;; It must be just a command table.
                                   (find-command-table ,command-table))))))
                  default-initargs)
         (:metaclass modual-class)
         ,@defclass-options))))

(defgeneric eval-option (syntax name value)
  (:documentation "Evaluate the option `name' with the specified
  `value' for `syntax'.")
  (:method (syntax name value)
    ;; We do not want to error out if an invalid option is
    ;; specified. Signal a condition? For now, silently ignore.
    (declare (ignore syntax name value))))

(defmethod eval-option :around (syntax (name string) value)
  ;; Convert the name to a keyword symbol...
  (eval-option syntax (intern (string-upcase name) (find-package :keyword))
               value))

(defmacro define-option-for-syntax
    (syntax option-name (syntax-symbol value-symbol) &body body)
  "Define an option for the syntax specified by the symbol
  `syntax'. `Option-name' should be a string that will be the
  name of the option. The name will automatically be converted to
  uppercase. When the option is being evaluated, `body' will be
  run, with `syntax-symbol' bound to the syntax object the option
  is being evaluated for, and `value-symbol' bound to the value
  of the option."
  ;; The name is converted to a keyword symbol which is used for all
  ;; further identification.
  (with-gensyms (name)
    (let ((symbol (intern (string-upcase option-name)
                          (find-package :keyword))))
      `(defmethod eval-option ((,syntax-symbol ,syntax)
                               (,name (eql ,symbol))
                               ,value-symbol)
         ,@body))))

(defgeneric current-attributes-for-syntax (syntax)
  (:method-combination append)
  (:method append (syntax)
           (list (cons :syntax (name syntax)))))

(defun make-attribute-line (syntax)
  (apply #'concatenate 'string
         (loop for (name . value) in (current-attributes-for-syntax syntax)
            collect (string-downcase (symbol-name name) :start 1)
            collect ": "
            collect value
            collect "; ")))

#+nil
(defmacro define-syntax (class-name (name superclasses) &body body)
  `(progn (push '(,name . ,class-name) *syntaxes*)
	  (defclass ,class-name ,superclasses
	       ,@body
	    (:default-initargs :name ,name))))

(define-presentation-method accept
    ((type syntax) stream (view textual-view) &key)
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far *syntaxes* '() :action action
			 :name-key #'syntax-description-name
			 :value-key #'syntax-description-class-name))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (declare (ignore success))
    (if (find string *syntaxes* :key #'first :test #'string=)
        (values object type)
        (input-not-of-required-type string type))
    object))

(defun syntax-from-name (syntax)
  (let ((description (find syntax *syntaxes*
			   :key #'syntax-description-name
			   :test #'string-equal)))
    (when description
      (find-class (syntax-description-class-name description)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Incremental Earley parser

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parse tree

(defclass parse-tree ()
  ((start-mark :initform nil :initarg :start-mark :reader start-mark)
   (size :initform nil :initarg :size))
  (:documentation "The base class for all parse trees."))

(defgeneric start-offset (parse-tree)
  (:documentation "The offset in the buffer of the first
character of a parse tree."))

(defmethod start-offset ((tree parse-tree))
  (let ((mark (start-mark tree)))
    (when mark
      (offset mark))))

(defgeneric (setf start-offset) (offset tree))

(defmethod (setf start-offset) ((offset number) (tree parse-tree))
  (let ((mark (start-mark tree)))
    (assert (not (null mark)))
    (setf (offset mark) offset)))

(defmethod (setf start-offset) ((offset mark) (tree parse-tree))
  (with-slots (start-mark) tree
     (if (null start-mark)
	 (setf start-mark (clone-mark offset))
	 (setf (offset start-mark) (offset offset)))))

(defgeneric end-offset (parse-tree)
  (:documentation "The offset in the buffer of the character
following the last one of a parse tree."))

(defmethod end-offset ((tree parse-tree))
  (with-slots (start-mark size) tree
     (when start-mark
       (+ (offset start-mark) size))))

(defgeneric (setf end-offset) (offset tree))

(defmethod (setf end-offset) ((offset number) (tree parse-tree))
  (with-slots (start-mark size) tree
     (assert (not (null start-mark)))
     (setf size (- offset (offset start-mark)))))

(defmethod (setf end-offset) ((offset mark) (tree parse-tree))
  (with-slots (start-mark size) tree
     (assert (not (null start-mark)))
     (setf size (- (offset offset) (offset start-mark)))))

(defmethod buffer ((tree parse-tree))
  (let ((start-mark (start-mark tree)))
    (when start-mark
      (buffer start-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass lexer ()
  ((buffer :initarg :buffer
           :reader buffer
           :documentation "The buffer associated with the
lexer."))
  (:documentation "The base class for all lexers."))

(defgeneric nb-lexemes (lexer)
  (:documentation "Return the number of lexemes in the lexer."))

(defgeneric lexeme (lexer pos)
  (:documentation "Given a lexer and a position, return the
lexeme in that position in the lexer."))

(defgeneric insert-lexeme (lexer pos lexeme)
  (:documentation "Insert a lexeme at the position in the lexer.
All lexemes following POS are moved to one position higher."))

(defgeneric delete-invalid-lexemes (lexer from to)
  (:documentation "Invalidate all lexemes that could have changed
as a result of modifications to the buffer"))

(defgeneric inter-lexeme-object-p (lexer object)
  (:documentation "This generic function is called by the
incremental lexer to determine whether a buffer object is an
inter-lexeme object, typically whitespace. Client code must
supply a method for this generic function."))

(defgeneric skip-inter-lexeme-objects (lexer scan)
  (:documentation "This generic function is called by the
incremental lexer to skip inter-lexeme buffer objects.  The
default method for this generic function increments the scan mark
until the object after the mark is not an inter-lexeme object, or
until the end of the buffer has been reached."))

(defgeneric next-lexeme (lexer scan)
  (:documentation "This generic function is called by the
incremental lexer to get a new lexeme from the buffer.  Client
code must supply a method for this function that specializes on
the lexer class.  It is guaranteed that scan is not at the end of
the buffer, and that the first object after scan is not an
inter-lexeme object.  Thus, a lexeme should always be returned by
this function."))

(defclass incremental-lexer (lexer)
  ((lexemes :initform (make-instance 'standard-flexichain) :reader lexemes))
  (:documentation "A subclass of lexer which maintains the buffer
in the form of a sequence of lexemes that is updated
incrementally."))

(defmethod nb-lexemes ((lexer incremental-lexer))
  (nb-elements (lexemes lexer)))

(defmethod lexeme ((lexer incremental-lexer) pos)
  (element* (lexemes lexer) pos))

(defmethod insert-lexeme ((lexer incremental-lexer) pos lexeme)
  (insert* (lexemes lexer) pos lexeme))

(defmethod delete-invalid-lexemes ((lexer incremental-lexer) from to)
  "delete all lexemes between FROM and TO and return the first invalid
position in the lexemes of LEXER"
  (with-slots (lexemes) lexer
     (let ((start 1)
	   (end (nb-elements lexemes)))
       ;; use binary search to find the first lexeme to delete
       (loop while (< start end)
	     do (let ((middle (floor (+ start end) 2)))
		  (if (mark< (end-offset (element* lexemes middle)) from)
		      (setf start (1+ middle))
		      (setf end middle))))
       ;; delete lexemes
       (loop until (or (= start (nb-elements lexemes))
		       (mark> (start-mark (element* lexemes start)) to))
	     do (delete* lexemes start))
       start)))

(defmethod skip-inter-lexeme-objects ((lexer incremental-lexer) scan)
  (loop until (end-of-buffer-p scan)
	while (inter-lexeme-object-p lexer (object-after scan))
	do (forward-object scan)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; grammar

(defclass rule ()
  ((left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (right-hand-side :initarg :right-hand-side :reader right-hand-side)
   (symbols :initarg :symbols :reader symbols)
   (predict-test :initarg :predict-test :reader predict-test)
   (number)))

(defclass grammar ()
  ((rules :initform nil :accessor rules)
   (hash :initform (make-hash-table) :accessor hash)
   (number-of-rules :initform 0)))

(defmacro grammar-rule ((left-hand-side arrow arglist &body body) &key predict-test)
  (declare (ignore arrow))
  (labels ((var-of (arg)
	     (if (symbolp arg)
		 arg
		 (car arg)))
	   (sym-of (arg)
	     (cond ((symbolp arg) arg)
		   ((= (length arg) 3) (cadr arg))
		   ((symbolp (cadr arg)) (cadr arg))
		   (t (car arg))))
	   (test-of (arg)
	     (cond ((symbolp arg) t)
		   ((= (length arg) 3) (caddr arg))
		   ((symbolp (cadr arg)) t)
		   (t (cadr arg))))
	   (build-rule (arglist body)
	     (if (null arglist)
		 body
		 (let ((arg (car arglist)))
		   `(lambda (,(var-of arg))
		      (when (and (typep ,(var-of arg) ',(sym-of arg))
				 ,(test-of arg))
			,(build-rule (cdr arglist) body)))))))
    `(make-instance 'rule
	:left-hand-side ',left-hand-side
	:right-hand-side
	,(build-rule arglist
		     (if (or (null body)
			     (symbolp (car body)))
			 `(make-instance ',left-hand-side ,@body)
			 `(progn ,@body)))
	:symbols ,(coerce (mapcar #'sym-of arglist) 'vector)
	:predict-test ,predict-test)))


(defmacro grammar (&body body)
  "Create a grammar object from a set of rules."
  (let ((rule (gensym "RULE"))
	(rules (gensym "RULES"))
	(result (gensym "RESULT")))
    `(let* ((,rules (list ,@(loop for rule in body
				  collect `(grammar-rule ,rule))))
	    (,result (make-instance 'grammar)))
       (dolist (,rule ,rules ,result)
	 (add-rule ,rule ,result)))))

(defgeneric add-rule (rule grammar))

(defmethod add-rule (rule (grammar grammar))
  (push rule (rules grammar))
  (setf (slot-value rule 'number) (slot-value grammar 'number-of-rules))
  (incf (slot-value grammar 'number-of-rules))
  (clrhash (hash grammar))
  (let (rhs-symbols)
    (dolist (rule (rules grammar))
      (setf rhs-symbols (union rhs-symbols (coerce (symbols rule) 'list))))
    (dolist (rule (rules grammar))
      (let ((lhs-symbol (left-hand-side rule)))
	(dolist (rhs-symbol rhs-symbols)
	  (when (or (subtypep lhs-symbol rhs-symbol)
		    (subtypep rhs-symbol lhs-symbol))
	    (pushnew rule (gethash rhs-symbol (hash grammar)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defclass parser ()
  ((grammar :initarg :grammar :reader parser-grammar)
   (target :initarg :target :reader target)
   (initial-state :reader initial-state)))

(defclass rule-item ()
  ((parse-trees :initform '() :initarg :parse-trees :reader parse-trees)))


(defclass incomplete-item (rule-item)
  ((orig-state :initarg :orig-state :reader orig-state)
   (predicted-from :initarg :predicted-from :reader predicted-from)
   (rule :initarg :rule :reader rule)
   (dot-position :initarg :dot-position :reader dot-position)
   (suffix :initarg :suffix :reader suffix)))

(defmethod print-object ((item incomplete-item) stream)
  (format stream "[~a ->" (left-hand-side (rule item)))
  (loop for i from 0 below (dot-position item)
	do (format stream " ~a" (aref (symbols (rule item)) i)))
  (format stream " *")
  (loop for i from (dot-position item) below (length (symbols (rule item)))
	do (format stream " ~a" (aref (symbols (rule item)) i)))
  (format stream "]"))

(defun derive-and-handle-item (prev-item parse-tree orig-state to-state)
  (let ((remaining (funcall (suffix prev-item) parse-tree)))
    (cond ((null remaining)
	   nil)
	  ((functionp remaining)
	   (handle-incomplete-item
	    (make-instance 'incomplete-item
	       :orig-state (orig-state prev-item)
	       :predicted-from (predicted-from prev-item)
	       :rule (rule prev-item)
	       :dot-position (1+ (dot-position prev-item))
	       :parse-trees (cons parse-tree (parse-trees prev-item))
	       :suffix remaining)
	    orig-state to-state))
	  (t
	   (let* ((parse-trees (cons parse-tree (parse-trees prev-item)))
		  (start (find-if-not #'null parse-trees
				      :from-end t :key #'start-offset))
		  (end (find-if-not #'null parse-trees :key #'end-offset)))
	     (with-slots (start-mark size) remaining
		(when start
		  (setf start-mark (start-mark start)
			size (- (end-offset end) (start-offset start))))
		(potentially-handle-parse-tree remaining orig-state to-state)))))))

(defun item-equal (item1 item2)
  (declare (optimize speed))
  (and (eq (rule item1) (rule item2))
       (do ((trees1 (parse-trees item1) (cdr trees1))
	    (trees2 (parse-trees item2) (cdr trees2)))
	   ((and (null trees1) (null trees2)) t)
	 (when (or (null trees1) (null trees2))
	   (return nil))
	 (when (not (parse-tree-equal (car trees1) (car trees2)))
	   (return nil)))))

(defun parse-tree-equal (tree1 tree2)
  (eq (class-of tree1) (class-of tree2)))

(defgeneric parse-tree-better (tree1 tree2))

(defmethod parse-tree-better (tree1 tree2)
  nil)

(defclass parser-state ()
  ((parser :initarg :parser :reader parser)
   (incomplete-items :initform (make-hash-table :test #'eq)
		     :reader incomplete-items)
   (parse-trees :initform (make-hash-table :test #'eq)
		:reader parse-trees)
   (last-nonempty-state :initarg :last-nonempty-state :accessor last-nonempty-state)
   (predicted-rules)))

(defmethod initialize-instance :after ((state parser-state) &rest args)
  (declare (ignore args))
  (with-slots (predicted-rules) state
     (setf predicted-rules
	   (make-array (slot-value (parser-grammar (parser state))
				   'number-of-rules)
		       :element-type 'bit
		       :initial-element 0))))

(defun map-over-incomplete-items (state fun)
  (maphash (lambda (key incomplete-items)
	     (loop for incomplete-item in incomplete-items
		   do (funcall fun key incomplete-item)))
	   (incomplete-items state)))

(defun potentially-handle-parse-tree (parse-tree from-state to-state)
  (let ((parse-trees (parse-trees to-state)))
    (flet ((handle-parse-tree ()
	     (map-over-incomplete-items from-state
	       (lambda (orig-state incomplete-item)
		 (derive-and-handle-item incomplete-item parse-tree orig-state to-state)))))
      (cond ((find parse-tree (gethash from-state parse-trees)
		   :test #'parse-tree-better)
	     (setf (gethash from-state parse-trees)
		   (cons parse-tree
			 (remove parse-tree (gethash from-state parse-trees)
				 :test #'parse-tree-better)))
	     (handle-parse-tree))
	    ((find parse-tree (gethash from-state parse-trees)
		   :test (lambda (x y) (or (parse-tree-better y x) (parse-tree-equal y x))))
	     nil)
	    (t (push parse-tree (gethash from-state parse-trees))
	       (handle-parse-tree))))))

(defun predict (item state tokens)
  (dolist (rule (gethash (aref (symbols (rule item)) (dot-position item))
			 (hash (parser-grammar (parser state)))))
    (if (functionp (right-hand-side rule))
	(let ((predicted-rules (slot-value state 'predicted-rules))
	      (rule-number (slot-value rule 'number))
	      (predict-test (predict-test rule)))
	  (when (zerop (sbit predicted-rules rule-number))
	    (setf (sbit predicted-rules rule-number) 1)
	    (when (or (null predict-test)
		      (some predict-test tokens))
	      (handle-and-predict-incomplete-item
	       (make-instance 'incomplete-item
		  :orig-state state
		  :predicted-from item
		  :rule rule
		  :dot-position 0
		  :suffix (right-hand-side rule))
	       state tokens))))
	(potentially-handle-parse-tree (right-hand-side rule) state state)))
  (loop for parse-tree in (gethash state (parse-trees state))
	do (derive-and-handle-item item parse-tree state state)))

(defun handle-incomplete-item (item orig-state to-state)
  (declare (optimize speed))
  (cond ((find item (the list (gethash orig-state (incomplete-items to-state)))
               :test #'item-equal)
	  nil)
        (t
         (push item (gethash orig-state (incomplete-items to-state))))))

(defun handle-and-predict-incomplete-item (item state tokens)
  (declare (optimize speed))
  (cond ((find item (the list (gethash state (incomplete-items state)))
               :test #'item-equal)
	  nil)
        (t
         (push item (gethash state (incomplete-items state)))
	 (predict item state tokens))))

(defmethod initialize-instance :after ((parser parser) &rest args)
  (declare (ignore args))
  (with-slots (grammar initial-state) parser
     (setf initial-state (make-instance 'parser-state :parser parser))
     (setf (last-nonempty-state initial-state) initial-state)
     (loop for rule in (rules grammar)
	   do (when (let ((sym (left-hand-side rule)))
		      (or (subtypep (target parser) sym)
			  (subtypep sym (target parser))))
		(if (functionp (right-hand-side rule))
		    (let ((predicted-rules (slot-value initial-state 'predicted-rules))
			  (rule-number (slot-value rule 'number))
			  (predict-test (predict-test rule)))
		      (when (zerop (sbit predicted-rules rule-number))
			(setf (sbit predicted-rules rule-number) 1)
			(when (null predict-test)
			  (handle-and-predict-incomplete-item
			   (make-instance 'incomplete-item
					  :orig-state initial-state
					  :predicted-from nil
					  :rule rule
					  :dot-position 0
					  :suffix (right-hand-side rule))
			   initial-state nil))))
		    (potentially-handle-parse-tree
		     (right-hand-side rule) initial-state initial-state))))))

(defun state-contains-target-p (state)
  (loop with target = (target (parser state))
	for parse-tree in (gethash (initial-state (parser state))
				   (parse-trees state))
	when (typep parse-tree target)
	  do (return parse-tree)))

(defun advance-parse (parser tokens state)
  (maphash (lambda (from-state items)
	     (declare (ignore from-state))
	     (dolist (item items)
	       (predict item state tokens)))
	   (incomplete-items state))
  (let ((new-state (make-instance 'parser-state :parser parser)))
    (loop for token in tokens
	  do (potentially-handle-parse-tree token state new-state))
    (setf (last-nonempty-state new-state)
	  (if (or (plusp (hash-table-count (incomplete-items new-state)))
		  (state-contains-target-p new-state))
	      new-state
	      (last-nonempty-state state)))
    new-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code for analysing parse stack

(defun parse-stack-top (state)
  "for a given state, return the top of the parse stack, or NIL if the parse stack
is empty in that state."
  (when (plusp (hash-table-count (incomplete-items state)))
    (maphash (lambda (state items)
	       (declare (ignore state))
	       (return-from parse-stack-top (car items)))
	     (incomplete-items state))))

(defun target-parse-tree (state)
  "for a given state, return a target parse tree, or NIL if this state does not
represent a complete parse of the target."
  (state-contains-target-p state))

(defun parse-state-empty-p (state)
  (and (null (parse-stack-top state))
       (null (target-parse-tree state))))

(defun parse-stack-next (parse-stack)
  "given a parse stack frame, return the next frame in the stack."
  (assert (not (null parse-stack)))
  (predicted-from parse-stack))

(defun parse-stack-symbol (parse-stack)
  "given a parse stack frame, return the target symbol of the frame."
  (assert (not (null parse-stack)))
  (left-hand-side (rule parse-stack)))

(defun parse-stack-parse-trees (parse-stack)
  "given a parse stack frame, return a list (in the reverse order of
analysis) of the parse trees recognized.  The return value reveals
internal state of the parser.  Do not alter it!"
  (assert (not (null parse-stack)))
  (parse-trees parse-stack))

(defun map-over-parse-trees (function state)
  (labels ((map-incomplete-item (item)
	     (unless (null (predicted-from item))
	       (map-incomplete-item (predicted-from item)))
	     (loop for parse-tree in (reverse (parse-trees item))
		   do (funcall function parse-tree))))
    (let ((state (last-nonempty-state state)))
      (if (plusp (hash-table-count (incomplete-items state)))
	  (maphash (lambda (state items)
		     (declare (ignore state))
		     (map-incomplete-item (car items))
		     (return-from map-over-parse-trees nil))
		   (incomplete-items state))
	  (funcall function (state-contains-target-p state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax querying functions.

(defgeneric word-constituentp (syntax obj)
  (:documentation "Return T if `obj' is a word constituent
  character in `syntax'.")
  (:method ((syntax syntax) obj)
    nil)
  (:method ((syntax syntax) (obj character))
    (alphanumericp obj)))

(defgeneric whitespacep (syntax obj)
  (:documentation "Return T if `obj' is a whitespace character in
  `syntax'.")
  (:method ((syntax syntax) obj)
    nil)
  (:method ((syntax syntax) (obj character))
    (when (member obj '(#\Space #\Tab #\Newline #\Page #\Return))
      t)))

(defgeneric page-delimiter (syntax)
  (:documentation "Return the object sequence used as a page
  deliminter in `syntax'.")
  (:method ((syntax syntax))
    '(#\Newline #\Page)))

(defgeneric paragraph-delimiter (syntax)
  (:documentation "Return the object sequence used as a paragraph
  deliminter in `syntax'.")
  (:method ((syntax syntax))
    '(#\Newline #\Newline)))

(defgeneric syntax-line-indentation (syntax mark tab-width)
  (:documentation "Return the correct indentation for the line
containing the mark, according to the specified syntax."))
