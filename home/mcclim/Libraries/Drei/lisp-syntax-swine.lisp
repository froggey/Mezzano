;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
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

;;; Functionality designed to aid development of Common Lisp code.

(in-package :drei-lisp-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler note hyperlinking

(defun make-compiler-note (note-list)
  (let ((severity (getf note-list :severity))
        (message (getf note-list :message))
        (location (getf note-list :location))
        (references (getf note-list :references))
        (short-message (getf note-list :short-message)))
    (make-instance
     (ecase severity
       (:error 'error-compiler-note)
       (:read-error 'read-error-compiler-note)
       (:warning 'warning-compiler-note)
       (:style-warning 'style-warning-compiler-note)
       (:note 'note-compiler-note))
     :message message :location location
     :references references :short-message short-message)))

(defclass compiler-note ()
  ((message :initarg :message :initform nil :accessor message)
   (location :initarg :location :initform nil :accessor location)
   (references :initarg :references :initform nil :accessor references)
   (short-message :initarg :short-message :initform nil :accessor short-message))
  (:documentation "The base for all compiler-notes."))

(defclass error-compiler-note (compiler-note) ())

(defclass read-error-compiler-note (compiler-note) ())

(defclass warning-compiler-note (compiler-note) ())

(defclass style-warning-compiler-note (compiler-note) ())

(defclass note-compiler-note (compiler-note) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code interrogation/form analysis

(defgeneric parameter-match-p (parameter arg-indices &key &allow-other-keys)
  (:method ((parameter parameter) (arg-indices list) &key)
    nil)
  (:method :around ((parameter parameter) (arg-indices null) &key)
    nil))

(defmethod parameter-match-p ((parameter named-parameter) (arg-indices list) &key)
  (and (= (caar arg-indices) (min-arg-index parameter))
       (null (rest arg-indices))))

(defmethod parameter-match-p ((parameter rest-parameter) (arg-indices list) &key)
  (and (>= (caar arg-indices) (min-arg-index parameter))
       (null (rest arg-indices))))

(defmethod parameter-match-p ((parameter keyword-parameter) (arg-indices list)
                              &key allow-other-keys)
  (let* ((index (caar arg-indices))
         (preceding-arg-p (rest (first arg-indices)))
         (preceding-arg (first preceding-arg-p)))
    (and (>= index (min-arg-index parameter))
         preceding-arg-p
         (or allow-other-keys
             (eq (keyword-name parameter) preceding-arg)))))

(defmethod parameter-match-p ((parameter destructuring-parameter) (arg-indices list) &key)
  (and (= (caar arg-indices) (min-arg-index parameter))
       (not (null (rest arg-indices)))))

(defmethod parameter-match-p ((parameter destructuring-keyword-parameter)
                              (arg-indices list) &key allow-other-keys)
  (let* ((index (caar arg-indices))
         (preceding-arg-p (rest (first arg-indices)))
         (preceding-arg (first preceding-arg-p)))
    (and (>= index (min-arg-index parameter))
         preceding-arg-p
         (or allow-other-keys
             (eq (keyword-name parameter) preceding-arg)))))

(defun find-affected-parameters (lambda-list arg-indices)
  "Find the parameters of `lambda-list' that would be affected if
an argument was entered at index `arg-indices' in the argument
list of the form with lambda list `lambda-list'. If `arg-indices'
is nil, no calculation will be done (this function will just
return NIL). This function returns two values: The primary value
is a list of symbols that should be emphasized, the secondary
value is a list of symbols that should be highlighted."
  (let ((affected-parameters '()))
    (loop for parameter in (all-parameters lambda-list)
       do (etypecase parameter
            (destructuring-keyword-parameter
             (when (parameter-match-p parameter arg-indices)
               (if (null (rest arg-indices))
                   (push parameter affected-parameters)
                   (setf affected-parameters
                         (nconc (list parameter)
                                (find-affected-parameters (inner-lambda-list parameter)
                                                          (rest arg-indices))
                                affected-parameters)))))
            (keyword-parameter
             (when (parameter-match-p parameter arg-indices)
               (push parameter affected-parameters)))
            (destructuring-parameter
             (when (parameter-match-p parameter arg-indices)
               (setf affected-parameters
                     (nconc (find-affected-parameters (inner-lambda-list parameter)
                                                      (rest arg-indices))
                            affected-parameters))))
            (rest-parameter
             (when (parameter-match-p parameter arg-indices)
               (push parameter affected-parameters)))
            (named-parameter
             (when (parameter-match-p parameter arg-indices)
               (push parameter affected-parameters))))
       finally (return affected-parameters))))

(defgeneric analyze-lambda-list (lambda-list current-arg-indices)
  (:documentation "Analyze lambda list and information about
provided arguments, and provide information for highlighting
it. `Arglist' is the argument list that is to be analyzed,
`current-arg-index' is the argument index where the next argument
would be written (0 is just after the operator).

A single value is returned: a list of parameters that should be
highlighted."))

(defmethod analyze-lambda-list ((lambda-list lambda-list)
                                (current-arg-indices list))
  (find-affected-parameters lambda-list current-arg-indices))

(defun find-argument-indices-for-operand (syntax operand-form full-form)
  "Return a list of argument indices for `argument-form' relative
to the operator of `full-form'. These lists take the form of ((n
x1) (m x2) (p x3)), which means (list-aref form-operand-list n m
p), and where x1...xN is the argument preceding the argument at
the index. If there is no preceding argument, this list may have
just a single element. A list of argument indices can have
arbitrary length (but they are practically always at most 2
elements long)."
  (let ((operator-form (first-form (children full-form))))
    (labels ((worker (operand-form)
               ;; Cannot find index for top-level-form.
               (unless (or (eq operand-form operator-form)
                           (form-at-top-level-p operand-form))
                 (let* ((parent-operator (first-form (children (parent operand-form))))
                        (form-operand-list
                         (remove-if #'(lambda (form)
                                        (or (not (formp form))
                                            (eq form operator-form)))
                                    (children (parent operand-form))))
                        (operand-position (or (position operand-form form-operand-list)
                                              (count-if #'(lambda (form)
                                                            (<= (start-offset form)
                                                                (start-offset operand-form)))
                                                        form-operand-list))))
                   ;; If we find anything, we have to increment the
                   ;; position by 1, since we consider the existance
                   ;; of a first operand to mean point is at operand
                   ;; 2. Likewise, a position of nil is interpreted
                   ;; as 0.
                   (cons (cons (or operand-position 0)
                               (when (and operand-position
                                          (plusp operand-position))
                                 (list (form-to-object
                                        syntax (elt form-operand-list
                                                    (1- operand-position))))))
                         (when (not (eq operator-form parent-operator))
                           (worker (parent operand-form))))))))
      (nreverse (worker operand-form)))))

(defclass placeholder-form (form)
  ((start-offset :accessor start-offset)
   (end-offset :accessor end-offset)))

(defun find-operand-info (syntax mark-or-offset full-form)
  "Returns the path from the operator of `full-form' to the
operand at `mark-or-offset'. If there is no operand at this
position, pretend there is and calculate the path."
  (as-offsets ((offset mark-or-offset))
    (let ((indexing-start-arg
           (let ((candidate (form-around syntax offset)))
             (if (or (and (form-list-p candidate)
                          (not (or (eq candidate (form-before syntax offset))
                                   (eq candidate (form-after syntax offset)))))
                     (null candidate))
                 (let ((obj (make-instance 'placeholder-form)))
                   (setf (parent obj) candidate
                         (start-offset obj) offset
                         (end-offset obj) offset)
                   obj)
                 candidate))))
      (find-argument-indices-for-operand syntax indexing-start-arg full-form))))

(defun valid-operator-p (operator)
  "Check whether or not `operator' is a valid
  operator. `Operator' is considered a valid operator if it is a
  symbol bound to a function, or if it is a lambda expression."
  (cond ((symbolp operator)
         (or (fboundp operator)
             (macro-function operator)
             (special-operator-p operator)))
        ((listp operator)
         (and (eq (first operator) 'cl:lambda)
              (not (null (rest operator)))
              (listp (second operator))))))

(defgeneric indices-match-arglist (lambda-list arg-indices)
  (:documentation "Check whether the argument indices
`arg-indices' could refer to a direct argument for the operator
with the lambda list `lambda-list'. Returns T if they could, NIL
otherwise. Does not check for the validity of keyword
arguments."))

(defmethod indices-match-arglist ((lambda-list semiordinary-lambda-list)
                                  (arg-indices list))
  (and arg-indices
       (null (rest arg-indices))
       (or (> (+ (length (required-parameters lambda-list))
                 (length (optional-parameters lambda-list)))
              (caar arg-indices))
           (rest-parameter lambda-list)
           (keyword-parameters lambda-list))
       t))

(defmethod indices-match-arglist ((lambda-list macro-lambda-list)
                                  (arg-indices list))
  (or (some #'(lambda (parameter)
                (and (parameter-match-p parameter arg-indices
                      :allow-other-keys t)
                     (or (not (typep parameter 'destructuring-parameter))
                         (null (rest arg-indices))
                         (indices-match-arglist (inner-lambda-list parameter)
                                                (rest arg-indices)))))
            (all-parameters lambda-list))
      (call-next-method)))

(defun direct-arg-p (syntax full-form arg-form)
  "Is `arg-form' a direct argument to the operator in
`full-form'? A \"direct argument\" is defined as an argument that
would be directly bound to a symbol when evaluating the operators
body, or as an argument that would be a direct component of a
&body or &rest argument, or as an argument that would be a
destructuring argument."
  (let* ((operator-form (first-form (children full-form)))
         (operator (form-to-object syntax operator-form)))
    (and
     ;; An operator is not an argument to itself.
     (not (eq arg-form operator-form))
     ;; An operator must be valid.
     (valid-operator-p operator)
     ;; The argument must match the operators argument list.
     (indices-match-arglist
      (arglist-for-form syntax operator)
      (find-operand-info syntax (start-offset arg-form) full-form)))))

(defun find-direct-operator (syntax arg-form)
  "Check whether `arg-form' is a direct argument to one of its
parents. If it is, return the form with the operator that
`arg-form' is a direct argument to. If not, return NIL."
  (labels ((recurse (form)
             ;; Check whether `arg-form' is a direct argument to
             ;; the operator of `form'.
             (when (parent form)
               (if (direct-arg-p syntax form arg-form)
                   form
                   (recurse (parent form))))))
    (recurse (parent arg-form))))

(defun find-applicable-form (syntax arg-form)
  "Find the enclosing form that has `arg-form' as a valid
argument. Return NIL if none can be found."
  ;; The algorithm for finding the applicable form:
  ;;
  ;; From `arg-form', we wander up the tree looking at enclosing
  ;; forms, until we find a a form with an operator, the
  ;; form-operator, that has `arg-form' as a direct argument (this is
  ;; checked by comparing argument indices for `arg-form', relative to
  ;; form-operator, with the arglist ofform-operator). However, if
  ;; form-operator itself is a direct argument to one of its parents,
  ;; we ignore it (unless form-operators form-operator is itself a
  ;; direct argument, etc). This is so we can properly handle
  ;; nested/destructuring argument lists such as those found in
  ;; macros.
  (labels ((recurse (candidate-form)
             (if (and (direct-arg-p syntax candidate-form arg-form)
                      (not (find-applicable-form syntax (first-form (children candidate-form)))))
                 candidate-form
                 (unless (form-at-top-level-p candidate-form)
                   (recurse (parent candidate-form))))))
    (unless (form-at-top-level-p arg-form)
      (recurse (parent arg-form)))))

(defgeneric relevant-keywords (lambda-list arg-indices)
  (:documentation "Return a list of the keyword arguments that it
would make sense to use at the position `arg-indices' relative to
the operator that has the argument list `arglist'."))

(defmethod relevant-keywords ((lambda-list semiordinary-lambda-list)
                              (arg-indices list))
  (let ((keyword-parameters (keyword-parameters lambda-list)))
    (when (and arg-indices
	       (null (rest arg-indices))
               keyword-parameters
               (>= (caar arg-indices) (min-arg-index (first keyword-parameters))))
      (mapcar #'keyword-name keyword-parameters))))

(defmethod relevant-keywords ((lambda-list macro-lambda-list)
                              (arg-indices list))
  (or (call-next-method)
      (unless (null (rest arg-indices))
        (let ((parameter (find-if #'(lambda (parameter)
                                      (parameter-match-p parameter arg-indices))
                                  (append (required-parameters lambda-list)
                                          (optional-parameters lambda-list)
                                          (keyword-parameters lambda-list)))))
          (when parameter
            (relevant-keywords (inner-lambda-list parameter) (rest arg-indices)))))))

(defgeneric possible-completions (syntax operator string package operands indices)
  (:documentation "Get the applicable completions for completing
`string' (which should a string of the, possibly partial, symbol
name to be completed) in `package', which is part of a form with
the operator `operator' (which should be a valid operator
object), and which has the operands `operands'. `Indices' should
be the argument indices from the operator to `token' (see
`find-argument-indices-for-operands').")
  (:method ((syntax lisp-syntax) operator (string string)
            (package package) (operands list) (indices list))
    (let ((completions (first (simple-completions (get-usable-image syntax)
                                                  string package))))
      ;; Welcome to the ugly mess! Part of the uglyness is that we
      ;; depend on Swank do to our nonobvious completion (m-v-b ->
      ;; multiple-value-bind).
      (or (when (valid-operator-p operator)
            (let* ((relevant-keywords
                    (relevant-keywords (arglist-for-form syntax operator operands) indices))
                   (keyword-completions (mapcar #'(lambda (a)
                                                    (string-downcase (format nil ":~A" a)))
                                                relevant-keywords)))
              (when relevant-keywords
                ;; We need Swank to get the concrete list of
                ;; possibilities, but after that, we need to filter
                ;; out anything that is not a relevant keyword
                ;; argument. ALSO, if `string' is blank, Swank will
                ;; "helpfully" not put any keyword symbols in
                ;; `completions', thus ruining this entire scheme. SO,
                ;; we have to force Swank to give us a list of keyword
                ;; symbols and use that instead of `completions'. Joy!
                (intersection (mapcar #'string-downcase
                                      (if (string= string "")
                                          (first (simple-completions (get-usable-image syntax)
                                                                     ":" package))
                                          completions))
                              keyword-completions
                              :key #'string-downcase
                              :test #'string=))))
          completions))))

(defgeneric complete-argument-of-type (argument-type syntax string all-completions)
  (:documentation "")
  (:method (argument-type syntax string all-completions)
    all-completions))

(defgeneric modify-argument-list (argument-type syntax arglist arguments arg-position)
  (:documentation "")
  (:method (syntax argument-type arglist arguments arg-position)
    arglist))

(defmacro define-argument-type (name (&optional inherit-from)
                                &rest options)
  "Define an argument type for use in `define-form-traits'."
  (let ((completion-code (rest (assoc :completion options)))
        (modification-code (rest (assoc :arglist-modification options))))
    (assert (or (null completion-code) (= (length (first completion-code)) 3)))
    (assert (or (null modification-code) (= (length (first modification-code)) 4)))
    `(progn
       ,(if (or completion-code inherit-from)
            (let ((lambda-list (if completion-code
                                   (first completion-code)
                                   '(argument-type syntax token all-completions))))
              `(defmethod complete-argument-of-type ((argument-type (eql ',name))
                                                     ,@lambda-list)
                 ,@(or (rest completion-code)
                       `((complete-argument-of-type ',inherit-from ,@lambda-list)))))
            ;; If no completion rule has been specified for this
            ;; type, we must check whether an earlier definition had
            ;; completion rules - if so, remove the method
            ;; implementing the rules.
            `(let ((method (find-method #'complete-argument-of-type nil `((eql ,name) t t t) nil)))
               (when method
                 (remove-method #'complete-argument-of-type method))))
       ,(if (or modification-code inherit-from)
            (let ((lambda-list (if modification-code
                                   (first modification-code)
                                   '(syntax arglist arguments arg-position))))
             `(defmethod modify-argument-list ((argument-type (eql ',name))
                                               ,@lambda-list)
                ,@(or (rest modification-code)
                      `((modify-argument-list ',inherit-from ,@lambda-list)))))
            ;; If no arglist modification rule has been specified
            ;; for this type, we must check whether an earlier
            ;; definition had arglist modification rules - if so,
            ;; remove the method implementing the rules.
            `(let ((method (find-method #'modify-argument-list nil '((eql ,name) t t t t) nil)))
               (when method
                 (remove-method #'modify-argument-list method)))))))

(define-argument-type class-name ()
  (:completion (syntax string all-completions)
               (let ((all-lower (every #'lower-case-p string)))
                 (loop for completion in all-completions
                    when (find-class (ignore-errors (read-from-string completion))
                                     nil)
                    collect (if all-lower
                                (string-downcase completion)
                                completion))))
  (:arglist-modification (syntax arglist arguments arg-position)
                         (if (and (> (length arguments) arg-position)
                                  (listp (elt arguments arg-position))
                                  (> (length (elt arguments arg-position)) 1)
                                  (eq (first (elt arguments arg-position)) 'cl:quote)
                                  (find-class (second (elt arguments arg-position)) nil))
                             (make-lambda-list
                              :defaults arglist
                              :keyword-parameters
                              (mapcar #'(lambda (parameter-data)
                                          (make-&key-parameter
                                           parameter-data
                                           (positional-parameter-count arglist)))
                                      (get-class-keyword-parameters
                                       (get-usable-image syntax)
                                       (elt arguments arg-position))))
                             arglist)))

(define-argument-type package-designator ()
  (:completion (syntax string all-completions)
               (declare (ignore all-completions))
               (let ((keyworded (and (plusp (length string))
                                     (char= (aref string 0) #\:)))
                     (all-upper (every #'upper-case-p string)))
                 (loop for package in (list-all-packages)
                    for package-name = (if keyworded
                                           (concatenate 'string ":" (package-name package))
                                           (package-name package))
                    when (search string package-name
                                 :test #'char-equal
                                 :end2 (min (length string)
                                            (length package-name)))
                    collect (if all-upper
                                package-name
                                (string-downcase package-name))))))

(defmacro define-form-traits ((operator &rest arguments)
                              &key no-typed-completion no-smart-arglist)
  "Define \"traits\" for a form with the operator that is eql to
`operator'. Traits is a common designator for
intelligent (type-aware) completion and intelligent modification
of argument lists (for example, adding keyword arguments for the
initargs of the class being instantiated to the arglist of
`make-instance').

`Arguments' is a lambda-list-like list that describes the types
of the operands of `operator'. You can use the lambda-list
keywords `&rest' and `&key' to tie all, or specific keyword
arguments, to types.

If `no-typed-completion' or `no-smart-arglist' is non-NIL, no
code for performing typed completion or smart arglist
modification will be generated, respectively."
  ;; FIXME: This macro should also define indentation rules.
  (labels ((process-keyword-arg-descs (arguments)
             ;; We expect `arguments' to be a plist mapping keyword
             ;; symbols to type/class designators/names.
             `((t
                (let* ((keyword-indices (loop
                                           for (car . cdr) on indices
                                           if (null cdr)
                                           collect (1+ car)
                                           else collect (first car)))
                       (keyword (apply #'list-aref operands keyword-indices))
                       (type (getf ',arguments keyword)))
                  (if (null type)
                      (call-next-method)
                      (complete-argument-of-type type syntax string all-completions))))))
           (process-arg-descs (arguments index)
             (let ((argument (first arguments)))
               (cond ((null argument)
                      nil)
                     ((eq argument '&rest)
                      `(((>= (caar indices) ,index)
                         (complete-argument-of-type ',(second arguments) syntax string all-completions))))
                     ((eq argument '&key)
                      (process-keyword-arg-descs (rest arguments)))
                     ((listp argument)
                      (cons `((= (caar indices) ,index)
                              ,(if (eq (first argument) 'quote)
                                   `(let ((selected-operand (apply #'list-aref operands (mapcar #'first indices))))
                                      (cond ((and (listp selected-operand)
                                                  (eq (first selected-operand) 'quote))
                                             (complete-argument-of-type ',(second argument) syntax string all-completions))
                                            (t (call-next-method))))
                                   `(cond ((not (null (rest indices)))
                                           (pop indices)
                                           (cond ,@(build-completions-cond-body argument)))
                                          (t (call-next-method)))))
                            (process-arg-descs (rest arguments)
                                               (1+ index))))
                     (t
                      (cons `((= (caar indices) ,index)
                              (complete-argument-of-type ',argument syntax string all-completions))
                            (process-arg-descs (rest arguments)
                                               (1+ index)))))))
           (build-completions-cond-body (arguments)
             (append (process-arg-descs arguments 0)
                     '((t (call-next-method))))))
    `(progn
       (defmethod possible-completions ((syntax lisp-syntax) (operator (eql ',operator)) string package operands indices)
         ,(if no-typed-completion
              '(call-next-method)
              `(let* ((*package* package)
                      (all-completions (call-next-method)))
                 (cond ,@(build-completions-cond-body arguments)))))
       ,(unless no-smart-arglist
                `(defmethod arglist-for-form ((syntax lisp-syntax) (operator (eql ',operator)) &optional arguments)
                   (declare (ignorable arguments))
                   (let ((arglist (call-next-method))
                         (arg-position 0))
                     (declare (ignorable arg-position))
                     ,@(loop for arg in arguments
                          collect `(setf arglist
                                         (modify-argument-list
                                          ',(unlisted arg #'second)
                                          syntax arglist arguments arg-position))
                          collect '(incf arg-position))
                     arglist))))))

(defun invoke-with-code-insight (syntax offset continuation)
  "Call `continuation' with argument-values containing
interesting details about the code at `offset'. If `offset' is
not within a form, everything will be bound to nil. The values
provided are, in order: the form, the forms operator, the indices
to the operand at `offset', or the indices to an operand entered
at that position if none is there, and the operands in the form."
  (update-parse syntax)
  (let* ((form
          ;; Find a form with a valid (fboundp) operator.
          (let ((immediate-form
                 (or (form-before syntax offset)
                     (form-around syntax offset))))
            (unless (null immediate-form)
              (find-applicable-form syntax immediate-form)
              (or (find-applicable-form syntax immediate-form)
                  ;; If nothing else can be found, and `arg-form'
                  ;; is the operator of its enclosing form, we use
                  ;; the enclosing form.
                  (when (and (not (form-at-top-level-p immediate-form))
                             (eq (first-form (children (parent immediate-form))) immediate-form))
                    (parent immediate-form))))))
         ;; If we cannot find a form, there's no point in looking
         ;; up any of this stuff.
         (operator (when (and form (form-list-p form))
                     (form-to-object syntax (form-operator form))))
         (operands (when (and form (form-list-p form))
                     (mapcar #'(lambda (operand)
                                 (when operand
                                   (form-to-object syntax operand)))
                             (form-operands form))))
         (current-operand-indices (when form
                                    (find-operand-info syntax offset form))))
    (funcall continuation form operator current-operand-indices operands)))

(defmacro with-code-insight (mark-or-offset syntax (&key operator form this-operand-indices
                                                         operands)
                             &body body)
  "Evaluate `body' with the provided symbols lexically bound to
interesting details about the code at `mark-or-offset'. If
`mark-or-offset' is not within a form, everything will be bound
to nil."
  (check-type operator symbol)
  (check-type form symbol)
  (check-type this-operand-indices symbol)
  (check-type operands symbol)
  (let ((operator-sym (or operator (gensym)))
        (operands-sym (or operands (gensym)))
        (form-sym (or form (gensym)))
        (this-operand-indices-sym (or this-operand-indices (gensym))))
    `(as-offsets ((offset ,mark-or-offset))
       (invoke-with-code-insight ,syntax offset
                                 #'(lambda (,form-sym ,operator-sym
                                            ,this-operand-indices-sym ,operands-sym)
                                     (declare (ignore ,@(append (unless operator
                                                                  (list operator-sym))
                                                                (unless operands
                                                                  (list operands-sym))
                                                                (unless form
                                                                  (list form-sym))
                                                                (unless this-operand-indices
                                                                  (list this-operand-indices-sym)))))
                                     ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Form trait definitions

(define-form-traits (make-instance 'class-name))
(define-form-traits (find-class 'class-name)
    :no-smart-arglist t)
(define-form-traits (change-class t 'class-name))
(define-form-traits (make-pane 'class-name))
(define-form-traits (make-instances-obsolete 'class-name)
    :no-smart-arglist t)
(define-form-traits (typep t 'class-name))
(define-form-traits (in-package package-designator))
(define-form-traits (clim-lisp:defclass t (&rest class-name))
    :no-smart-arglist t)
(define-form-traits (cl:defclass t (&rest class-name))
    :no-smart-arglist t)
(define-form-traits (define-application-frame t (&rest class-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameter hinting

(defgeneric operator-for-display (operator)
  (:documentation "Return what should be displayed whenever
  `operator' is displayed as an operator.")
  (:method (operator)
    operator))

(defmethod operator-for-display ((operator list))
  (case (first operator)
    ('cl:lambda '|Lambda-Expression|)))

(defun display-lambda-list-to-stream (stream operator lambda-list
                                      &optional highlighted-parameters)
  "Display the operator and lambda list to stream, format as
  appropriate."
  (labels ((show (parameter format-string &rest format-args)
             (with-text-face (stream (if (member parameter highlighted-parameters)
                                      :bold
                                      :roman))
               (apply #'format stream format-string format-args)))
           (show-parameters (parameters)
             (loop for (parameter . next-parameters) on parameters
                do (etypecase parameter
                     (destructuring-keyword-parameter
                      (show parameter ":~A " (keyword-name parameter))
                      (format stream "(")
                      (show-lambda-list (inner-lambda-list parameter))
                      (format stream ")"))
                     (keyword-parameter
                      (show parameter ":~A" (keyword-name parameter)))
                     (destructuring-parameter
                      (format stream "(")
                      (show-lambda-list (inner-lambda-list parameter))
                      (format stream ")"))
                     (named-parameter
                      (show parameter "~A" (name parameter))))
                do (when next-parameters
                     (princ #\Space stream))))
           (show-lambda-list (lambda-list)
             (let ((space-needed (not (null (required-parameters lambda-list)))))
              (show-parameters (required-parameters lambda-list))
              (when (optional-parameters lambda-list)
                (when space-needed
                  (princ #\Space stream))
                (format stream "&OPTIONAL ")
                (show-parameters (optional-parameters lambda-list))
                (setf space-needed t))
              (when (rest-parameter lambda-list)
                (when space-needed
                  (princ #\Space stream))
                (format stream "&REST ")
                (show (rest-parameter lambda-list) "~A" (name (rest-parameter lambda-list)))
                (setf space-needed t))
              (when (body-parameter lambda-list)
                (when space-needed
                  (princ #\Space stream))
                (format stream "&BODY ")
                (show (body-parameter lambda-list) "~A" (name (body-parameter lambda-list)))
                (setf space-needed t))
              (when (keyword-parameters lambda-list)
                (when space-needed
                  (princ #\Space stream))
                (format stream "&KEY ")
                (show-parameters (keyword-parameters lambda-list))))))
    (format stream "(~A" operator)
    (when (all-parameters lambda-list)
      (princ #\Space stream))
    (show-lambda-list lambda-list)
    (format stream ")")))

(defun show-arglist-silent (syntax operator &optional
                            current-arg-indices
                            arguments)
  "Display the arglist for `operator' in the minibuffer, do not
complain if `operator' is not bound to, or is not, a function.

`Current-arg-index' is used to add extra information to the
arglist display. `Arguments' should be either nil or a list of
provided arguments in the form housing symbol.

Returns NIL if an arglist cannot be displayed."
  (let* ((lambda-list (arglist-for-form syntax operator arguments))
         (highlighted-parameters
          (analyze-lambda-list lambda-list current-arg-indices)))
    (esa:with-minibuffer-stream (minibuffer)
      (display-lambda-list-to-stream minibuffer operator
                                     lambda-list highlighted-parameters))))

(defun show-arglist (syntax symbol)
  (unless (and (fboundp symbol)
               (show-arglist-silent syntax symbol))
    (esa:display-message "Function ~a not found." symbol)))

(defun show-arglist-for-form-at-mark (mark syntax)
  "Display the argument list for the operator of `form'. The
list need not be complete. If an argument list cannot be
retrieved for the operator, nothing will be displayed."
  (with-code-insight mark syntax (:operator operator
                                  :this-operand-indices this-operand-indices
                                  :operands operands)
    (when (valid-operator-p operator) 
      (show-arglist-silent syntax operator this-operand-indices operands))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Symbol completion

;;; The following helper stuff is from Swank.

(defun longest-completion (completions)
  "Return the longest completion of `completions', which must be a
list of sequences."
  (untokenize-completion
   (mapcar #'longest-common-prefix
           (transpose-lists (mapcar #'tokenize-completion completions)))))

(defun tokenize-completion (string)
  "Return all substrings of STRING delimited by #\-."
  (loop with end
     for start = 0 then (1+ end)
     until (> start (length string))
     do (setq end (or (position #\- string :start start) (length string)))
     collect (subseq string start end)))

(defun untokenize-completion (tokens)
  (format nil "~{~A~^-~}" tokens))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun transpose-lists (lists)
  "Turn a list-of-lists on its side.
If the rows are of unequal length, truncate uniformly to the shortest.

For example:
\(transpose-lists '((ONE TWO THREE) (1 2)))
  => ((ONE 1) (TWO 2))"
  (cond ((null lists) '())
        ((some #'null lists) '())
        (t (cons (mapcar #'car lists)
                 (transpose-lists (mapcar #'cdr lists))))))

;;; The interface used by the commands.

(defgeneric frame-clear-completions (frame)
  (:documentation "Clear the display completions for `frame'.")
  (:method (frame)
    nil))

(defun clear-completions ()
  (frame-clear-completions *application-frame*))

(defun find-completions (syntax mark-or-offset string)
  "Find completions for the symbol denoted by the string `string'
at `mark-or-offset'. Two values will be returned: the common
leading string of the completions and a list of the possible
completions as strings."
  (let* ((result (with-code-insight mark-or-offset syntax
                     (:operator operator
                                :operands operands
                                :this-operand-indices indices)
                   (let ((completions (possible-completions
                                       syntax operator string
                                       (package-at-mark syntax mark-or-offset)
                                       operands indices)))
                     (list completions (longest-completion completions)))))
         (set (first result))
         (longest (second result)))
    (values longest set)))

(defun find-fuzzy-completions (syntax mark-or-offset string)
    "Find completions for the symbol denoted by the string
`string' at `mark-or-offset'. Two values will be returned: the
common leading string of the completions and a list of the
possible completions as strings. This function uses fuzzy logic
to find completions based on `string'."
  (let* ((set (fuzzy-completions (get-usable-image syntax) string
                                 (package-at-mark syntax mark-or-offset)
                                 10))
         (best (caar set)))
    (values best set)))

(defun complete-symbol-at-mark-with-fn (syntax mark &key (completion-finder #'find-completions)
                                        (complete-blank t))
  "Attempt to find and complete the symbol at `mark' using the
function `completion-finder' to get the list of completions. If
the completion is ambiguous, a list of possible completions will
be displayed. If no symbol can be found at `mark', return NIL. If
there is no symbol at `mark' and `complete-blank' is true (the
default), all symbols available in the current package will be
shown. If `complete-blank' is false, nothing will be shown and
the function will return NIL."
  (let* ((token (form-around syntax (offset mark)))
         (useful-token (and (not (null token))
                            (form-token-p token)
                            (not (= (start-offset token)
                                    (offset mark))))))
    (when (or useful-token complete-blank)
      (multiple-value-bind (longest completions)
          (funcall completion-finder syntax
                   (cond (useful-token
                          (start-offset (fully-quoted-form token)))
                         ((and (form-quoted-p token)
                               (form-incomplete-p token))
                          (start-offset token))
                         (t (offset mark)))
                   (if useful-token
                       (form-string syntax token)
                       ""))
        (cond ((null completions)
               (esa:display-message "No completions found")
               nil)
              ((endp (rest completions))
               (replace-symbol-at-mark syntax mark longest)
               t)
              (t (replace-symbol-at-mark
                  syntax mark
                  (or (when (or useful-token
                                (accept 'boolean
                                 :prompt "You are asking for a list of all exported symbols, proceed?")
                                (return-from complete-symbol-at-mark-with-fn nil))
                        (frame-manager-menu-choose
                         (find-frame-manager)
                         (mapcar
                          #'(lambda (completion)
                              (if (listp completion)
                                  (cons completion
                                        (first completion))
                                  completion))
                          completions)
                         :label "Possible completions"
                         :scroll-bars :vertical))
                      longest))
                 t))))))

(defun complete-symbol-at-mark (syntax mark &optional (complete-blank t))
  "Attempt to find and complete the symbol at `mark'. If the
  completion is ambiguous, a list of possible completions will be
  displayed. If no symbol can be found at `mark', return nil."
  (complete-symbol-at-mark-with-fn syntax mark :complete-blank complete-blank))

(defun fuzzily-complete-symbol-at-mark (syntax mark &optional (complete-blank t))
  "Attempt to find and complete the symbol at `mark' using fuzzy
  completion. If the completion is ambiguous, a list of possible
  completions will be displayed. If no symbol can be found at
  `mark', return nil."
  (complete-symbol-at-mark-with-fn syntax mark
                                   :completion-finder #'find-fuzzy-completions
                                   :complete-blank complete-blank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation and compilation

(defun eval-string (syntax string)
  "Evaluate all expressions in STRING and return a list of
results."
  (with-input-from-string (stream string)
    (loop for form = (read stream nil stream)
       while (not (eq form stream))
       collecting (multiple-value-list
                   (eval-form-for-drei (get-usable-image syntax)
                                          form)))))

(defun eval-region (start end syntax)
  ;; Must be (mark>= end start).
  (with-syntax-package (syntax start)
    (let ((*read-base* (base syntax)))
      (let* ((string (buffer-substring (buffer start)
                                       (offset start)
                                       (offset end)))
             (values (multiple-value-list
                      (eval-string syntax string)))
             ;; Enclose each set of values in {}.
             (result (apply #'format nil "~{{~:[No values~;~:*~{~S~^,~}~]}~}"
                            values)))
        (esa:display-message result)))))

(defclass undefiner ()
  ()
  (:documentation "A base class for classes that contain logic
for undefining Lisp constructs. Subclasses of `undefiner' must
implement the undefiner protocol. An instance of `undefiner'
works on a specific kind of definition (a `defun', `defclass',
`defgeneric', etc)."))

(defgeneric undefiner-type (undefiner)
  (:documentation "Return the kind of definition undefined by
`undefiner'. The return value is a string - a textual,
user-oriented description."))

(defgeneric definition-name (undefiner syntax definition-form)
  (:documentation "Return the name of the definition described by
`definition-form', as per the kind of definition `undefiner'
handles. `Syntax' is the Lisp syntax object that has
`definition-form'. The name returned is an actual Lisp
object. `Form-conversion-error' is signalled if the form
describing the name cannot be converted to an object, or if the
form is otherwise inappropriate."))

(defgeneric undefine (undefiner syntax definition-form)
  (:documentation "Undefine whatever `definition-form' defines,
provided `definition-form' is the kind of definition handled by
`undefiner'. If it isn't, the results are undefined. `Syntax' is
the Lisp syntax object that has `definition-form'."))

(defvar *undefiners* (make-hash-table)
  "A hash table mapping operators to undefiners. The undefiners
are instances of `undefiner'.")

(defun get-undefiner (definition-type)
  "Return the undefiner for `definition-type', which must be a
symbol. Returns NIL if there is no undefiner of the given type."
  (values (gethash definition-type *undefiners*)))

(defun invalid-form-for-type (syntax form type-name)
  "Signal a `form-conversion-error' describing the fact that
`form' cannot define a `type-name'."
  (form-conversion-error syntax form "Form cannot define a ~A." type-name))

(defun invalid-form (undefiner syntax form)
  "Signal a `form-conversion-error' describing the fact that
`form' cannot define whatever kind of definition `undefiner'
handles."
  (invalid-form-for-type syntax form (undefiner-type undefiner)))

(defclass simple-undefiner (undefiner)
  ((%undefiner-type :reader undefiner-type
                    :initform (error "A description must be provided.")
                    :type string
                    :documentation "A textual, user-oriented name
for the type of definition handled by this
undefiner."
                    :initarg :undefiner-type)
   (%undefiner-function :reader undefiner-function
                        :initform (error "An undefiner function must be provided.")
                        :documentation "A function of three
arguments: the syntax object, the name of the definition to be
undefined and the form to be undefined."
                        :initarg :undefiner-function)))

(defmethod definition-name ((undefiner simple-undefiner) (syntax lisp-syntax) (form form))
  (invalid-form undefiner syntax form))

(defmethod definition-name ((undefiner simple-undefiner) (syntax lisp-syntax) (form list-form))
  (if (>= (length (form-children form)) 2)
      (form-to-object syntax (second-form (children form)))
      (call-next-method)))

(defmethod undefine ((undefiner simple-undefiner) (syntax lisp-syntax) (form form))
  (funcall (undefiner-function undefiner) syntax
           (definition-name undefiner syntax form)
           form))

(defmacro define-simple-undefiner (definition-spec (syntax-sym name-sym form-sym) &body body)
  "Define a way to undefine some definition. `Definition-spec' is
the operator (like `defun', `defclass', etc), and `syntax-sym',
`name-sym' and `form-sym' will be bound to the Lisp syntax
instance, the name of the definition to be undefined and the
entire form of the definition, when the undefinition is invoked
by the user. Syntactical problems (such as an incomplete or
invalid `form') should be signalled via `form-conversion-error'."
  (check-type definition-spec (or list symbol))
  (let* ((definition-type (unlisted definition-spec))
         (undefiner-name (if (listp definition-spec)
                             (second definition-spec)
                             (string-downcase definition-type))))
    (check-type definition-type symbol)
    `(setf (gethash ',definition-type *undefiners*)
           (make-instance 'simple-undefiner
            :undefiner-type ,undefiner-name
            :undefiner-function #'(lambda (,syntax-sym ,name-sym ,form-sym)
                                    (declare (ignorable ,syntax-sym ,name-sym ,form-sym))
                                    ,@body)))))

(defclass generic-undefiner (undefiner)
  ((%undefiner-type :reader undefiner-type
                    :initform (error "A description must be provided.")
                    :type string
                    :documentation "A textual, user-oriented name
for the type of definition handled by this
undefiner."
                    :initarg :undefiner-type)
   (%name-function :reader name-function
                   :initform (error "A name retrieval function must be provided.")
                   :documentation "A function of three arguments:
the syntax object and the form to retrieve a name from. Should
return the name as a Lisp object (probably a symbol). Should
signal a `form-conversion-error' if the form cannot define
whatever type this undefiner handles."
                   :initarg :name-function)
   (%undefiner-function :reader undefiner-function
                        :initform (error "An undefiner function must be provided.")
                        :documentation "A function of three
arguments: the syntax object, the name of the definition to be
undefined and the form to be undefined."
                        :initarg :undefiner-function)))

(defmethod definition-name ((undefiner generic-undefiner) (syntax lisp-syntax) (form form))
  (funcall (name-function undefiner) syntax form))

(defmethod undefine ((undefiner generic-undefiner) (syntax lisp-syntax) (form form))
  (funcall (undefiner-function undefiner) syntax
           (definition-name undefiner syntax form)
           form))

(defmacro define-undefiner (definition-spec
                            ((name-syntax-sym name-form-sym) &body name-body)
                            ((undef-syntax-sym undef-name-sym undef-form-sym)
                             &body undefiner-body))
  "Define a way to undefine definitions. `Definition-spec' is the
operator (like `defun', `defclass', etc) and may optionally be a
list, in which case the first element is the operator, and the
second a user-oriented name for the kind of thing defined by the
operator. `Name-body' and `Undefiner-body' will be evaluated to
retrieve the name and perform the undefinition, respectively.

`Name-syntax-sym' and `name-form-sym' will be bound to the Lisp
syntax instance and the entire form of the definition during
evaluation of `name-body'. Syntactical problems (such as an
incomplete or invalid form) should be signalled by an
invocation `(invalid)'

`undef-syntax-sym', `undef-name-sym' and `undef-form-sym' will be
bound to the Lisp syntax instance, the name of the definition to
be undefined and the entire form of the definition when
`undefiner-body' is evaluated. Syntactical problems (such as an
incomplete or invalid form) should be signalled by an
invocation `(invalid)'."
  (check-type definition-spec (or list symbol))
  (let* ((definition-type (unlisted definition-spec))
         (undefiner-name (if (listp definition-spec)
                             (second definition-spec)
                             (string-downcase definition-type))))
    (check-type definition-type symbol)
    `(setf (gethash ',definition-type *undefiners*)
           (make-instance 'generic-undefiner
            :undefiner-type ,undefiner-name
            :name-function #'(lambda (,name-syntax-sym ,name-form-sym)
                               (declare (ignorable ,name-syntax-sym ,name-form-sym))
                               (flet ((invalid ()
                                        (invalid-form-for-type ,name-syntax-sym ,name-form-sym ,undefiner-name)))
                                 (declare (ignorable #'invalid))
                                 ,@name-body))
            :undefiner-function #'(lambda (,undef-syntax-sym ,undef-name-sym ,undef-form-sym)
                                    (declare (ignorable ,undef-syntax-sym ,undef-name-sym ,undef-form-sym))
                                    (flet ((invalid ()
                                             (invalid-form-for-type ,undef-syntax-sym ,undef-form-sym ,undef-name-sym)))
                                      (declare (ignorable #'invalid))
                                      ,@undefiner-body))))))

(define-simple-undefiner (defun "function") (syntax name form)
  (fmakunbound name))

(define-simple-undefiner (defgeneric "generic function") (syntax name form)
  (fmakunbound name))

(define-simple-undefiner (defmacro "macro") (syntax name form)
  (fmakunbound name))

(define-simple-undefiner (cl:defclass "class") (syntax name form)
  (setf (find-class name nil) nil))

(define-simple-undefiner (clim-lisp:defclass "class") (syntax name form)
  (setf (find-class name nil) nil))

(define-simple-undefiner (defmethod "method") (syntax name form)
  (let ((function (fdefinition name)))
    (labels ((get-qualifiers (maybe-qualifiers)
               (unless (or (null maybe-qualifiers)
                           (form-list-p (first maybe-qualifiers)))
                 (cons (form-to-object syntax (first maybe-qualifiers))
                       (get-qualifiers (rest maybe-qualifiers)))))
             (get-specializers (maybe-specializers)
               (cond ((null maybe-specializers)
                      (form-conversion-error syntax form "~A form invalid." 'defmethod))
                     ;; Map across the elements in the lambda list.
                     ((form-list-p (first maybe-specializers))
                      (mapcar #'(lambda (ll-form)
                                  (if (and (form-list-p ll-form)
                                           (second-form (children ll-form)))
                                      (form-to-object syntax (second-form (children ll-form)))
                                      t))
                              (form-children (first maybe-specializers))))
                     ;; Skip the qualifiers to get the lambda-list.
                     (t (get-specializers (rest maybe-specializers))))))
      (remove-method function (find-method function
                                           (get-qualifiers (cddr (form-children form)))
                                           (get-specializers (cddr (form-children form)))
                                           nil)))))

(define-simple-undefiner (defvar "special variable") (syntax name form)
  (makunbound name))

(define-simple-undefiner (defparameter "special variable") (syntax name form)
  (makunbound name))

(define-simple-undefiner (defconstant "constant") (syntax name form)
  (makunbound name))

(define-simple-undefiner (defpackage "package") (syntax name form)
  (delete-package name))

(defun get-listed-name (syntax form)
  "Retrieve the name of `form' under the assumption that the name
is the second element of `form', and if this is a list, the first
element of that list. The secondary value will be true if a name
can be found, false otherwise."
  (if (and (form-list-p form)
           (>= (length (form-children form)) 2))
      (let ((name-form (second (form-children form))))
        (cond ((and (form-list-p name-form)
                    (form-children name-form))
               (values (form-to-object syntax (first (form-children name-form))) t))
              ((form-token-p name-form)
               (values (form-to-object syntax name-form) t))
              (t (values nil nil))))
      (values nil nil)))

;; Cannot recognize the common define-FOO-command macros.
(define-undefiner (define-command "command")
  ((syntax form)
   (multiple-value-bind (name success) (get-listed-name syntax form)
     (if success name (invalid))))
  ((syntax name form)
   ;; Pick out the command table from the define-command form. The
   ;; command may also be in other command tables, but we can't find
   ;; those.
   (let ((name-form (listed (form-to-object syntax (second (form-children form))))))
     (destructuring-bind (ignore &key command-table keystroke &allow-other-keys) name-form
       (declare (ignore ignore))
       (when command-table
         (remove-command-from-command-table name command-table :errorp nil)
         (remove-keystroke-from-command-table command-table keystroke :errorp nil))))
   (fmakunbound name)))

(define-undefiner (define-undefiner "undefiner")
  ((syntax form)
   (multiple-value-bind (name success) (get-listed-name syntax form)
     (if success name (invalid))))
  ((syntax name form)
   (remhash name *undefiners*)))

(define-undefiner (define-simple-undefiner "simple undefiner")
  ((syntax form)
   (multiple-value-bind (name success) (get-listed-name syntax form)
     (if success name (invalid))))
  ((syntax name form)
   (remhash name *undefiners*)))
