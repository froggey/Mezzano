;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-LISTENER; -*-
;;;
;;; Command table and menu definitions
;;;
;;; (C) Copyright 2003,2008 by Andy Hefner (ahefner@gmail.com)
;;; (C) Copyright 2004 by Paolo Amoroso (amoroso@mclink.it)
;;;
;;; See toplevel file 'Copyright' for the copyright details.
;;;

(in-package :clim-listener)


(define-command-table application-commands)

(define-command-table lisp-dev-commands :inherit-from nil) ;; Translators live here
(define-command-table lisp-commands
    :inherit-from (lisp-dev-commands)
    :menu (#+(or) ("ASDF" :menu asdf-commands)))

(define-command-table show-commands :inherit-from (lisp-dev-commands))

(define-command-table filesystem-commands
    :inherit-from (directory-stack-commands)
    :menu (("Directory Stack" :menu directory-stack-commands)))

(define-command-table directory-stack-commands)

;;; Presentation types

(define-presentation-type specializer () :inherit-from 'expression)
(define-presentation-type class () :inherit-from 'specializer)
(define-presentation-type eql-specializer () :inherit-from 'specializer)
(define-presentation-type class-name () :inherit-from 'symbol)
(define-presentation-type slot-definition () :inherit-from 'expression)

(define-presentation-type-abbreviation function-name ()
  `(and expression (satisfies legal-and-fboundp)))

(defun legal-and-fboundp (object)
  (and #+sbcl (sb-int:valid-function-name-p object)
       #-sbcl (typep object '(or symbol (cons (eql setf))))
       (fboundp object)))

(define-presentation-type process () :inherit-from 'expression)
(define-presentation-type generic-function () :inherit-from 't)

(define-presentation-method presentation-typep
    (object (type generic-function))
  (typep object 'generic-function))

(define-presentation-type standard-generic-function ()
  :inherit-from 'generic-function)

(define-presentation-type method ()
  :inherit-from 'expression)

(define-presentation-type standard-method ()
  :inherit-from 'method)

(define-presentation-type directory-stack () :inherit-from 'expression)
(define-presentation-type bytes () :inherit-from 'integer)
(define-presentation-type lisp-memory-usage () :inherit-from 'bytes)

(define-presentation-type package () :inherit-from 'expression)
(define-presentation-method presentation-typep (object (type package))
  (typep object 'package))

(define-presentation-type package-name () :inherit-from 'string)
(define-presentation-method presentation-typep (object (type package-name))
  (find-package object))

;;; Views

(defclass fancy-view (textual-view)
  ((icon-size :initarg :icon-size :initform 16)
   (base-path :initform nil :initarg :base-path)))

;;; Presentation methods

(define-presentation-method present (object (type standard-method)
				     stream (view textual-view)
				     &key &allow-other-keys)
  (let ((name (c2mop:generic-function-name
	       (c2mop:method-generic-function object)))
	(qualifiers (method-qualifiers object))
	(specializers (c2mop:method-specializers object))
	(lambda-list (c2mop:method-lambda-list object))
	(class-of-t (find-class t)))
    (format stream "~S ~{~S ~}(" name qualifiers)
    (multiple-value-bind (required optional rest key allow-other-keys aux key-present)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (declare (ignore allow-other-keys aux))
      (loop
	 for spec in specializers
	 for arg in required
	 for first-time = t then nil
	 do
	   (unless first-time
	     (write-char #\space stream))
	   (if (eq spec class-of-t)
	       (present arg 'symbol :stream stream)
	       (progn
		 (write-char #\( stream)
		 (present arg 'symbol :stream stream)
		 (write-char #\space  stream)
		 (with-output-as-presentation (stream spec 'specializer
                                                      :single-box t)
                   (if (typep spec 'class)
                       (format stream "~S" (class-name spec))
                       (format stream "~S" `(eql ,(c2mop:eql-specializer-object spec)))))
                 (write-char #\) stream))))
      (when optional
	(format stream " &optional ~{~A ~^ ~}" optional))
      (when rest
	(format stream " &rest ~A" (car rest)))
      (when key-present
	(format stream " &key"))
      (loop
	   for arg in key
	   for key-arg = (cond ((symbolp arg)
				(intern (symbol-name arg) :keyword))
			       ((symbolp (car arg))
				(intern (symbol-name (car arg)) :keyword) )
			       (t (caar arg)))
	   do (format stream " ~S" key-arg))
      (write-char #\) stream))))


(define-presentation-method present (object (type generic-function)
                                     stream (view textual-view)
                                     &key &allow-other-keys)
  (princ (c2mop:generic-function-name object) stream))

(define-presentation-method accept
    ((type generic-function) stream (view textual-view) &key)
  ;; generic-functions are a subclass of standard-object, so they can be
  ;; accepted as expressions!
  (let ((fn (accept 'expression
		    :stream stream
		    :view view
		    :history 'generic-function
		    :prompt nil)))
    ;;
    (when (typep fn 'generic-function)
      (return-from accept fn))
    (handler-case
	(fdefinition fn)
      (error ()
	(simple-parse-error "~S is not the name of a generic function."
			    fn)))))

(define-presentation-method present (object (type bytes)
                                     stream (view textual-view)
                                     &key &allow-other-keys)
  (if (zerop object)
      (princ "0" stream)
    (let* ((suffixes '(" bytes" " KB" " MB" " GB" " TB" " PB"))
           (x (floor (realpart (log object 1000))))
           (idx (min x (1- (length suffixes)))))
      (if (zerop idx)
          (format stream "~A bytes" object)
        (format stream "~,1F~A" (/ object (expt 1000 idx)) (nth idx suffixes))))))

;;; Presentation translators

(define-presentation-translator class-name-to-class
  (class-name class lisp-dev-commands
     :documentation ((object stream) (format stream "Class object ~A" object))
     :gesture t)
  (object)
  (find-class object))

(define-presentation-translator symbol-to-class
  (symbol class lisp-dev-commands
     :documentation ((object stream) (format stream "Class object ~A" object))
     :gesture t
     :tester ((object) (not (not (find-class object nil))))
     :tester-definitive t)
  (object)
  (find-class object))

(define-presentation-translator symbol-to-class-name
  (symbol class-name lisp-dev-commands
     :documentation ((object stream) (format stream "Class ~A" object))
     :gesture t
     :tester ((object) (not (not (find-class object nil))))
     :tester-definitive t)
  (object)
  object)

(define-presentation-translator class-to-class-name
  (class class-name lisp-dev-commands
     :documentation ((object stream) (format stream "Class of ~A" object))
     :gesture t)
  (object)
  (class-name object))

(define-presentation-translator expression-to-function-name
  (expression function-name lisp-dev-commands
     :documentation ((object stream) (format stream "~A" object))
     :gesture t
     :tester ((object) (legal-and-fboundp object))
     :tester-definitive t)
  (object)
  object)
(define-presentation-translator symbol-to-function-name
  (symbol function-name lisp-dev-commands
     :documentation ((object stream) (format stream "~A" object))
     :gesture t
     :tester ((object) (legal-and-fboundp object))
     :tester-definitive t)
  (object)
  object)
#+nil ; doesn't work for some reason
(define-presentation-translator sequence-to-function-name
  ((sequence t) function-name lisp-dev-commands
     :documentation ((object stream) (format stream "~A" object))
     :gesture t
     :tester ((object) (legal-and-fboundp object))
     :tester-definitive t)
  (object)
  object)

;;; Application commands

(define-command (com-clear-output :name "Clear Output History"
				  :command-table application-commands
                                  :menu t
				  :provide-output-destination-keyword nil)
    ()
  (window-clear *standard-output*))

;; McCLIM fixme: Shouldn't we be able to activate before the (args) prompt
;; since defaults are defined?
;; FIXME: Disabled input, as it usually seems to hang.
#+(or) ; nonsensical on mezzano
(define-command (com-run :name "Run" :command-table application-commands :menu t)
  ((program 'string :prompt "Command")
   (args '(sequence string) :default '("") :prompt "Arguments"))
  (let ((output-stream *standard-output*))
    (with-text-family (output-stream :fix)
      (if (zerop (length (car args)))
	  (progn (heading "Runnig \"~A\"~%" program)
		 (uiop:run-program program :force-shell nil :output output-stream :input nil
					   :ignore-error-status t :error-output output-stream))
	  (progn (heading "Running \"~A ~{~A ~}\"~%" program args)
		 (uiop:run-program `(,program ,@args) :force-shell nil :output output-stream
						      :input nil :ignore-error-status t
						      :error-output output-stream))))))

;; I could replace this command with a keyword to COM-RUN..
#+(or) ; nonsensical on mezzano
(define-command (com-background-run :name "Background Run"
                                    :menu t
				    :command-table application-commands)
    ((program 'string :prompt "Command")
     (args '(sequence string) :default '("") :prompt "Args"))
  (bt:make-thread #'(lambda ()
                      (if (zerop (length (car args)))
                          (uiop:run-program program)
                          (uiop:run-program `(,program ,@args))))))

#+(or) ; mimedb not included in demo4
(define-command (com-reload-mime-database :name "Reload Mime Database"
                                          :menu t
                                          :command-table application-commands)
    ()
  (progn
    (load-mime-types)
    (load-mailcaps)))

(add-menu-item-to-command-table (find-command-table 'application-commands) nil :divider nil)

(define-command (com-exit :name "Quit"
			  :command-table application-commands
                          :menu t
			  :provide-output-destination-keyword nil)
    ()
  (frame-exit *application-frame*))

;;;; Commands relating to the Lisp environment

(defvar *apropos-list* nil
  "The apropos command stores its output here.")

;; FIXME: Make this a present method specialzed on a view?

(defun apropos-present-symbol (symbol &optional (stream *standard-output*) show-package)
  (multiple-value-bind (style ink)
      (values
       (if (or (fboundp symbol)
               (boundp  symbol)
               (find-class symbol nil))
           (make-text-style *apropos-symbol-bound-family*
                            *apropos-symbol-unbound-face*
                            :normal)
           (make-text-style *apropos-symbol-unbound-family*
                            *apropos-symbol-bound-face*
                            :normal))
       (cond ((eql (symbol-package symbol)
                   (find-package "KEYWORD"))
              (make-rgb-color 0.46 0.0 0.0))
             ((fboundp symbol)        (make-rgb-color 0.0  0.0  0.3))
             ((find-class symbol nil) (make-rgb-color 0.03 0.35 0.48))
             ((boundp symbol)         (make-rgb-color 0.0  0.0  0.0))
             (t                       (make-rgb-color 0.6  0.6  0.6))))
    (with-drawing-options (stream :ink ink :text-style style)
      (with-output-as-presentation (stream symbol 'clim:symbol)
        (if show-package
            (let ((*package* (find-package :common-lisp-user)))
              (format stream "~W" symbol))
            (princ (symbol-name symbol) stream)))
      (when (boundp symbol)
        (format stream " = ")
        (with-drawing-options (stream :ink +olivedrab+ ;; XXX
                                      :text-style (make-text-style :fix :roman :small))
          (let ((object (symbol-value symbol)))
            (present object (presentation-type-of object) :stream stream)))))))

;; These are used by com-apropos to filter the list of symbols according to the domain keyword
(defgeneric apropos-applicable-p (spec symbol))

(defmethod apropos-applicable-p ((spec (eql 'symbols)) symbol) t)

(defmethod apropos-applicable-p ((spec (eql 'classes)) symbol)
  (find-class symbol nil))

(defmethod apropos-applicable-p ((spec (eql 'functions)) symbol)
  (fboundp symbol))

(defmethod apropos-applicable-p ((spec (eql 'variables)) symbol)
  (boundp symbol))

(defmethod apropos-applicable-p ((spec (eql 'command-tables)) symbol)
  (find-command-table symbol :errorp nil))

;(defmethod apropos-applicable-p ((spec (eql 'presentation-type)) symbol)
;  (find-presentation-type-class symbol nil))

(define-command (com-apropos :name "Apropos"
			     :command-table lisp-commands
                             :menu t
			     :provide-output-destination-keyword t)
    ((string 'clim:string :prompt "String")
     &key
     (package '(or package-name package) :prompt "Package" :default nil)
     (domain '(member symbols classes functions variables command-tables) :prompt "Domain" :default 'symbols))
  (let ((real-package (when package
                        (if (typep package 'package)
                            package
                            (find-package package)))))
    (when (and package (not real-package))
      (cerror "Search all packages instead" "No package specified by ~A could be found." package))
    (let ((symbols (remove-if-not (lambda (sym) (apropos-applicable-p domain sym))
                                  (apropos-list string real-package))))
      (dolist (sym symbols)
        (apropos-present-symbol sym *standard-output* t)
        (terpri))
      (setf *apropos-list* symbols)
      (note "Results have been saved to ~W~%" '*apropos-list*))))

(define-command (com-trace :name "Trace"
			   :command-table lisp-commands
                           :menu t
			   :provide-output-destination-keyword nil)
    ((fsym 'function-name :prompt "function name"))
  (if (fboundp fsym)
      (progn
	(eval `(trace ,fsym))
	(format t "~&Tracing ~W.~%" fsym))
    (format t "~&Function ~W is not defined.~%" fsym)))

(define-command (com-untrace :name "Untrace"
			     :command-table lisp-commands
                             :menu t
			     :provide-output-destination-keyword nil)
    ((fsym 'function-name :prompt "function name"))
  (if (fboundp fsym)
      (progn
	(eval `(untrace ,fsym))
	(format t "~&~W will no longer be traced.~%" fsym))
    (format t "~&Function ~W is not defined.~%" fsym)))


(define-command (com-load-file :name "Load File"
                               :command-table lisp-commands
                               :menu t
			       :provide-output-destination-keyword t)
  ((pathname 'pathname :prompt "pathname"))
  (load pathname))

(define-command (com-compile-file :name "Compile File"
                                  :command-table lisp-commands
                                  :menu t
				  :provide-output-destination-keyword t)
  ((pathname 'pathname :prompt "pathname"))
  (compile-file pathname))

(define-command (com-compile-and-load :name "Compile and load"
                                      :command-table lisp-commands
                                      :menu t
				      :provide-output-destination-keyword t)
  ((pathname 'pathname :prompt "pathname"))
  (load (compile-file pathname)))

(define-command (com-room :name "Room"
                          :command-table lisp-commands
                          :menu t
			  :provide-output-destination-keyword t)
  ()
  (room))

(define-presentation-to-command-translator mem-room-translator
  (lisp-memory-usage com-room lisp-commands
                     :gesture :select
                     :documentation "Room"
                     :pointer-documentation "Room")
    (object))


(define-presentation-to-command-translator com-show-class-documentation-translator
    (class-name com-show-class-documentation lisp-commands
                :menu t
                :tester ((object) (not (eq t object)))
                :documentation "Show Class Documentation"
                :pointer-documentation "Show Class Documentation")
    (object)
  (list object))

(define-presentation-to-command-translator com-show-class-subclasses-translator
  (class-name com-show-class-subclasses lisp-commands
              :menu t
              :documentation "Show Class Subclasses"
              :pointer-documentation "Show Class Subclasses")
  (object)
  (list object))


(define-presentation-to-command-translator com-show-class-superclasses-translator
  (class-name com-show-class-superclasses lisp-commands
              :menu t
              :tester ((object) (not (eq t object)))
              :documentation "Show Class Superclasses"
              :pointer-documentation "Show Class Superclasses")
  (object)
  (list object))


(define-presentation-to-command-translator com-show-class-generic-functions-translator
  (class-name com-show-class-generic-functions lisp-commands
              :menu t
              :documentation "Show Class Generic Functions"
              :pointer-documentation "Show Class Generic Functions")
  (object)
  (list object))


(define-presentation-to-command-translator com-show-class-slots-translator
  (class-name com-show-class-slots lisp-commands
              :menu t
              :documentation "Show Class Slots"
              :pointer-documentation "Show Class Slots")
  (object)
  (list object))


;;; CLOS introspection commands

(defun class-grapher (stream class inferior-fun &key (orientation :horizontal))
  "Does the graphing for Show Class Superclasses and Subclasses commands"
  (let ((normal-ink +foreground-ink+)
        (arrow-ink  *graph-edge-ink*)
	(text-style *graph-text-style*))
    (with-drawing-options (stream :text-style text-style)
      (prog1
	;; not sure whether anyone wants the return value...
	(format-graph-from-roots (list class)
				 #'(lambda (class stream)
				     (with-drawing-options (stream :ink normal-ink
								   :text-style text-style)
				       ;; Present class name rather than class here because the printing of the
				       ;; class object itself is rather long and freaks out the pointer doc pane.
				       (with-output-as-presentation (stream (class-name class) 'class-name
                                                                            :single-box t)
					; (surrounding-output-with-border (stream :shape :drop-shadow)
					 (princ (class-name class) stream)))) ;)
				 inferior-fun
				 :stream stream
				 :merge-duplicates t
				 :graph-type :tree
				 :orientation orientation
				 :arc-drawer
				 #'(lambda (stream foo bar x1 y1 x2 y2)
				     (declare (ignore foo bar))
				     (draw-arrow* stream x1 y1 x2 y2 :ink arrow-ink)))
	;; format-graph-from-roots doesn't do this by default...
	(when (typep stream 'pane)
	  (change-space-requirements stream))))))

(defun frob-to-class (spec)
  (if (typep spec 'class)
      spec
    (find-class spec nil)))

(define-command (com-show-class-documentation :name "Show Class Documentation"
                                              :command-table show-commands
                                              :menu "Class Documentation"
                                              :provide-output-destination-keyword t)
    ((class-spec 'class-name :prompt "class"))
  (let ((class (frob-to-class class-spec)))
    (cond
      ((null class) (note "~A is not a defined class." class-spec))
      ((null (documentation class t)) (note "~A has no documentation." class-spec))
      (t (format t "~a" (documentation class t))))))

(define-command (com-show-class-superclasses :name "Show Class Superclasses"
                                             :command-table show-commands
                                             :menu "Class Superclasses"
					     :provide-output-destination-keyword t)
    ((class-spec 'class-name :prompt "class")
     &key
     (orientation 'keyword :prompt "orientation" :default :horizontal))
  (let ((class (frob-to-class class-spec)))
    (if (null class)
	(note "~A is not a defined class." class-spec)
        (class-grapher *standard-output* class #'c2mop:class-direct-superclasses
                       :orientation orientation))))

(define-command (com-show-class-subclasses :name "Show Class Subclasses"
                                           :command-table show-commands
                                           :menu "Class Subclasses"
					   :provide-output-destination-keyword t)
    ((class-spec 'class-name :prompt "class")
     &key
     (orientation 'keyword :prompt "orientation" :default :horizontal))
  (let ((class (frob-to-class class-spec)))
    (if (not (null class))
        (class-grapher *standard-output* class #'c2mop:class-direct-subclasses
                       :orientation orientation)
        (note "~A is not a defined class." class-spec))))


(defun direct-slot-definitions (class slot-name)
  "Given a class and a slot name, returns a list of the direct slot
   definitions for this slot in the order they occur along the CPL."
  (mapcan (lambda (cpl-class)
            (copy-list
             (remove slot-name (c2mop:class-direct-slots cpl-class)
                     :key #'c2mop:slot-definition-name :test-not #'eql)))
          (c2mop:class-precedence-list class)))

(defun present-slot (slot class &key (stream *standard-output*))
  "Formats a slot definition into a table row."
  (let* ((name (c2mop:slot-definition-name slot))
         (type (c2mop:slot-definition-type slot))
         (initargs (c2mop:slot-definition-initargs slot))
         (initfunc (c2mop:slot-definition-initfunction slot))
         (initform (c2mop:slot-definition-initform slot))
         (direct-slots (direct-slot-definitions class name))
         (readers (mapcan (lambda (x) (copy-list (c2mop:slot-definition-readers x))) direct-slots))
         (writers (mapcan (lambda (x) (copy-list (c2mop:slot-definition-writers x))) direct-slots))
         (documentation (first (remove nil (mapcar (lambda (x) (documentation x t)) direct-slots))))
         (*standard-output* stream))

  (macrolet ((with-ink ((var) &body body)
               `(with-drawing-options (t :ink ,(intern (concatenate 'string "*SLOT-" (symbol-name var) "-INK*")))
                     ,@body))
             (fcell ((var align-x &rest cell-opts) &body body)
                `(formatting-cell (t :align-x ,align-x ,@cell-opts)
                   (with-ink (,var) ,@body) )))

    (fcell (name :left)
     (with-output-as-presentation (t slot 'slot-definition :single-box t)
       (princ name))
     (unless (eq type t)
       (fresh-line)
       (with-ink (type) (princ type))))

    (fcell (initargs :right)
      (dolist (x initargs)
        (format t "~W~%" x)))

    (fcell (initform :left)
      (if initfunc
          (format t "~W" initform)
        (italic (stream) (write-string "No initform" stream))))

    (formatting-cell (t :align-x :left)
      (if (not (or readers writers))
          (italic (stream) (write-string "No accessors" stream))
        (progn
          (with-ink (readers)
            (if readers
                (dolist (reader readers)
                  (present reader (presentation-type-of reader))
                  (terpri))
                (italic (stream) (write-string "No readers" stream))))
          (with-ink (writers)
            (if writers
                (dolist (writer writers)
                  (present writer (presentation-type-of writer))
                  (terpri))
              (italic (stream) (write-string "No writers" stream)))))))

    (fcell (documentation :left)
      (when documentation (with-text-family (t :serif) (princ documentation)))) )))


(defun earliest-slot-definer (slot class)
  "Returns the earliest class in the CPL of CLASS which defines SLOT."
  (let ((name (c2mop:slot-definition-name slot)))
    (dolist (class (reverse (c2mop:class-precedence-list class)))
      (dolist (slot-b (c2mop:class-direct-slots class))
        (when (eq name (c2mop:slot-definition-name slot-b))
          (return-from earliest-slot-definer class)))))
  (error "Slot ~W does not appear to be defined in ~W" slot class))

(defun class-sorted-slots (class)
  "Sort the slots in order of definition within the CPL, superclasses first."
  (let ((cpl (c2mop:class-precedence-list class)))
    (sort (copy-list (c2mop:class-slots class))
          (lambda (a b)
            (< (position (earliest-slot-definer a class) cpl)
               (position (earliest-slot-definer b class) cpl))))))

(defun print-slot-table-heading ()
  (formatting-row (t)
    (dolist (name '("Slot name" "Initargs" "Initform" "Accessors"))
      (formatting-cell (t :align-x :center)
        (underlining (t)
          (with-text-family (t :sans-serif)
            (princ name)))))))

(defun present-slot-list (slots class)
  (formatting-table (t)
    (print-slot-table-heading)
    (dolist (slot slots)
      (formatting-row (t)
        (present-slot slot class)))))

(defun friendly-slot-allocation-type (allocation)
  (if (typep allocation 'standard-class)
      (class-name allocation)
    allocation))

(defun present-the-slots (class)
  (let* ((slots (class-sorted-slots class))
         (instance-slots (remove-if (lambda (x) (not (eq :instance (c2mop:slot-definition-allocation x)))) slots))
         (other-slots (set-difference slots instance-slots))
         (allocation-types (remove-duplicates (mapcar #'c2mop:slot-definition-allocation other-slots))))
    (when other-slots
      (underlining (t) (format t "~&Instance Slots~%")))
    (present-slot-list instance-slots class)
    (dolist (alloc allocation-types)
      (underlining (t)
        (format t "~&Allocation: ~A~%" (friendly-slot-allocation-type alloc)))
      (present-slot-list (remove-if (lambda (x)
                                      (not (eq alloc (c2mop:slot-definition-allocation x))))
                                    other-slots)
                         class))))

(define-command (com-show-class-slots :name "Show Class Slots"
                                      :command-table show-commands
                                      :menu "Class Slots"
                                      :provide-output-destination-keyword t)
    ((class-name 'clim:symbol :prompt "class name"))
  (let* ((class (find-class class-name nil))
         (finalized-p (and class
                           (typep class 'standard-class)
                           (progn
                             (c2mop:finalize-inheritance class)
                             (c2mop:class-finalized-p class))))
         (slots (and finalized-p (c2mop:class-slots class))))
    (cond
     ((null class)
      (note "~A is not a defined class.~%" class-name))
     ((not (typep class 'standard-class))
      (note "Class ~A is not a STANDARD-CLASS.~%" class-name))
     ((not finalized-p)
      (note "Class ~A is not finalized." class-name))
     ((null slots)
      (note "~%This class has no slots.~%~%"))
     (t (invoke-as-heading
         (lambda ()
           (format t "~&Slots for ")
           (with-output-as-presentation (t (class-name class) 'class-name :single-box t)
             (princ (class-name class)))))
        (present-the-slots class)))))

(defparameter *ignorable-internal-class-names*
  '(standard-object))

(defun remove-ignorable-classes (classes)
  (remove-if (lambda (c)
               (or (member (class-name c) *ignorable-internal-class-names*)
                   (not (typep c 'standard-class))))
             classes))

(defun x-specializer-direct-generic-functions (specializer)
  ;; This still belongs in CLIM-MOP.
  #+PCL (pcl::specializer-direct-generic-functions specializer)
  #+SBCL (sb-pcl::specializer-direct-generic-functions specializer)
  #+clisp (clos:specializer-direct-generic-functions specializer)
  #+openmcl-partial-mop
  (openmcl-mop:specializer-direct-generic-functions specializer)
  #+scl (clos:specializer-direct-generic-functions specializer)
  #+lispworks (clos:specializer-direct-generic-functions specializer)
  #+allegro (mop:specializer-direct-generic-functions specializer)
  #-(or PCL SBCL scl lispworks clisp openmcl-partial-mop)
  (error "Sorry, not supported in your CL implementation.
See the function X-SPECIALIZER-DIRECT-GENERIC-FUNCTION
if you are interested in fixing this."))

(defun class-funcs (class)
  (remove-duplicates
   (mapcan
    (lambda (class)
      (copy-list (x-specializer-direct-generic-functions class)))
    (remove-ignorable-classes (c2mop:class-precedence-list
                               (c2mop:ensure-finalized class))))))

(defun slot-name-sortp (a b)
  (flet ((slot-name-symbol (x)
           (or (and (consp x)
                    (second x)
                    (symbolp (second x))
                    (second x))
               x)))
    (let ((a (slot-name-symbol a))
          (b (slot-name-symbol b)))
      (if (and (symbolp a) (symbolp b))
          (cond ((not (eq (symbol-package a)
                          (symbol-package b)))
                 (string< (package-name (symbol-package a))
                          (package-name (symbol-package b))))
                (t (string< (symbol-name a)
                            (symbol-name b))))
          (string< (princ-to-string a)
                   (princ-to-string b))))))

#+(or) ; x-specializer-direct-generic-functions not implemented
(define-command (com-show-class-generic-functions
                 :name "Show Class Generic Functions"
                 :command-table show-commands
                 :menu "Class Generic Functions"
		 :provide-output-destination-keyword t)
    ((class-spec 'class-name :prompt "class"))
  (let ((class (frob-to-class class-spec)))
    (if (null class)
        (note "~A is not a defined class." class-spec)
      (let ((funcs (sort (class-funcs class) #'slot-name-sortp
                         :key #'c2mop:generic-function-name)))
        (with-text-size (t :small)
          (format-items funcs
            :printer (lambda (item stream)
                       (present item 'generic-function :stream stream))
            :move-cursor t))))))

(defun method-applicable-to-args-p (method args arg-types)
  (loop
     for specializer in (c2mop:method-specializers method)
     for arg in args
     for arg-type in arg-types
     unless (cond ((eq arg-type :wild)
		   t)
		  ((typep specializer 'c2mop:eql-specializer)
		   (and (not (eq arg arg-type))
			    (eql arg
				 (c2mop:eql-specializer-object
				  specializer))))
		  ((eq arg arg-type)
		   (subtypep arg-type specializer))
		  (t (typep arg specializer)))
     return nil
     finally (return t)))

(defun get-applicable-methods (gf args arg-types)
  (mapcan #'(lambda (method)
	      (when (method-applicable-to-args-p method args arg-types)
		(list method)))
	  (c2mop:generic-function-methods gf)))

(defun sort-methods-by-args (methods arg-types)
  (let ((cpls (mapcar #'(lambda (type)
			  (if (eq type :wild)
			      nil
			      (c2mop:class-precedence-list type)))
		      arg-types)))
    (flet ((sorter (meth1 meth2)
	     (loop
		for spec1 in (c2mop:method-specializers meth1)
		for spec2 in (c2mop:method-specializers meth2)
		for arg-type in arg-types
		for cpl in cpls
		for spec1-cpl = (unless (typep spec1 'c2mop:eql-specializer)
				  (c2mop:class-precedence-list spec1))
		for spec2-cpl = (unless (typep spec1 'c2mop:eql-specializer)
				  (c2mop:class-precedence-list spec2))
		do (cond ((eq spec1 spec2)) ;Keep going
			 ((eq arg-type :wild)
			  (cond ((typep spec1 'c2mop:eql-specializer)
				 (unless (typep spec2
						'c2mop:eql-specializer)
				   (return-from sorter t)))
				((typep spec1 'c2mop:eql-specializer)
				 (return-from sorter nil))
				((subtypep spec1 spec2)
				 (return-from sorter t))
				((subtypep spec2 spec1)
				 (return-from sorter nil))
				;; Types are not related
				(t (let ((cpl-len1 (length spec1-cpl))
					 (cpl-len2 (length spec2-cpl)))
				     (cond ((> cpl-len1 cpl-len2)
					    (return-from sorter t))
					   ((< cpl-len1 cpl-len2)
					    (return-from sorter nil)))))))
			 ;; An actual instance
			 ((typep spec1 'c2mop:eql-specializer)
			  (return-from sorter t))
			 ((typep spec2 'c2mop:eql-specializer)
			  (return-from sorter nil))
			 (t (let ((pos1 (position spec1 cpl))
				  (pos2 (position spec2 cpl)))
			      (cond ((< pos1 pos2)
				     (return-from sorter t))
				    ((> pos1 pos2)
				     (return-from sorter nil))))))
		  finally (return nil))))
      (declare (dynamic-extent #'sorter))
      (sort methods #'sorter))))

(defun show-specialized-methods (gf stream methods arg-types)
  (declare (ignore gf))
  (let ((before nil)
	(after nil)
	(around nil)
	(primary nil))
    (loop
       for meth in methods
       for (qualifier) = (method-qualifiers meth)
       do (case qualifier
	    (:before
	     (push meth before))
	    (:after
	     (push meth after))
	    (:around
	     (push meth around))
	    (t (push meth primary))))
    (setq before (sort-methods-by-args before arg-types))
    (setq after (nreverse (sort-methods-by-args after arg-types)))
    (setq around (sort-methods-by-args around arg-types))
    (setq primary (sort-methods-by-args primary arg-types))
    (flet ((do-meths (kind methods)
	     (with-text-face (stream :italic)
	       (format stream "~A methods:~%" kind))
	     (loop
		for meth in methods
		do
		  (present meth 'standard-method :stream stream)
		  (terpri stream))))
      (do-meths "Before" before)
      (do-meths "Around" around)
      (do-meths "Primary" primary)
      (do-meths "After" after))))

(defun make-gf-specialized-ptype (gf-name)
  (let ((gf gf-name))
    (unless (typep gf 'generic-function)
      (handler-case
	  (setq gf (fdefinition gf-name))
	(error ()
	  (return-from make-gf-specialized-ptype nil))))
    (unless (typep gf 'generic-function)
      (return-from make-gf-specialized-ptype nil))
    (let ((required (alexandria:parse-ordinary-lambda-list
		     (c2mop:generic-function-lambda-list gf))))
      (loop
	 for arg in required
	 collect (make-presentation-type-specifier
		  '(expression)
		  :description (format nil "~A" arg))
	   into args
	 finally (return `(sequence-enumerated ,@args))))))


(define-command (com-show-generic-function
		 :name t
		 :command-table show-commands
                 :menu "Generic Function"
		 :provide-output-destination-keyword t)
    ((gf 'generic-function :prompt "a generic function")
     &key (classes 'boolean :default nil :mentioned-default t)
     (methods 'boolean :default nil :mentioned-default t)
     (specialized (make-gf-specialized-ptype gf)
		  :prompt "function arguments" :default nil
		  :display-default nil))
  (when specialized
    (setq methods t))
  (let ((doc-string (documentation gf t)))
    (with-text-face (*standard-output* :italic)
      (format *standard-output* "Lambda list:~%"))
    (format *standard-output* "~S~%" (c2mop:generic-function-lambda-list
				      gf))
    (when doc-string
      (with-text-face (*standard-output* :italic)
	(format *standard-output* "Documentation:~%~A~&" doc-string)))
    (when classes
      (with-text-face (*standard-output* :italic)
	(format *standard-output* "Classes:~%"))
      (let ((class-list nil)
	    (meths (c2mop:generic-function-methods gf)))
	(loop
	   for m in meths
	   do (loop for arg in (c2mop:method-specializers m)
		 unless (typep arg 'c2mop:eql-specializer)
		 do (pushnew arg class-list)))
	(loop
	   for class in class-list
	   do (progn
		(with-output-as-presentation (*standard-output*
					      (class-name class)
					      'class-name
                                              :single-box t)
		  (format *standard-output*
			  "~S~%" (class-name class)))))))
    (when methods
      (let ((args nil)
	    (arg-types nil))
	(if (null specialized)
	    (setq args
		  (mapcar (constantly :wild)
			  (c2mop:generic-function-argument-precedence-order
			   gf))
		  arg-types
		  args)
	    (loop
	       with arg = nil and arg-type = nil
	       for arg-for-spec in specialized
	       do (setf (values arg arg-type)
			(cond ((eq arg-for-spec '*)
			       (values :wild :wild))
			      ((and (listp arg-for-spec)
				    (eq (car arg-for-spec) 'quote))
			       (values (cadr arg-for-spec)
				       (class-of (cadr arg-for-spec))))
			      ((symbolp arg-for-spec)
			       (let ((class (find-class arg-for-spec)))
				 (values class class)))
			      ((typep arg-for-spec 'standard-class)
			       (values arg-for-spec arg-for-spec))
			      (t (values arg-for-spec
					 (class-of arg-for-spec)))))
	       collect arg into collect-args
	       collect arg-type into collect-arg-types
	       finally (setf (values args arg-types)
			     (values collect-args collect-arg-types))))
	(let ((meths (get-applicable-methods gf args arg-types)))
	  (with-text-face (*standard-output* :italic)
	    (format *standard-output* "Methods:~%"))
	  (show-specialized-methods gf *standard-output* meths arg-types))))))

;;; Add to McCLIM's listener the commands "Show Used Packages" and "Show Package
;;; Users" for displaying package hierarchy graphs.
;;;
;;; Paolo Amoroso <amoroso@mclink.it> -- 27 Aug 2004

(defun symbol-status (sym)
  (nth-value 1 (find-symbol (symbol-name sym)
                            (symbol-package sym))))

(defun portable-internal-symbol-count (package)
  (let ((n 0))
    (do-symbols (symbol package)
      (when (and #+NIL (eq (symbol-package symbol) package)
                 (eq :internal (symbol-status symbol)))
        (incf n)))
    n))

(defun count-internal-symbols (package)
  "Return the number of internal symbols in PACKAGE."
  ;; We take only the first value, the symbol count, and discard the second, the
  ;; hash table capacity
  #+(or cmu scl)  (values (lisp::internal-symbol-count package))
  #+sbcl (values (sb-int:package-internal-symbol-count package))
  #+clisp (svref (sys::%record-ref *package* 1) 2)
  #-(or cmu scl sbcl clisp) (portable-internal-symbol-count package))

(defun portable-external-symbol-count (package)
  (let ((n 0))
    (do-external-symbols (symbol package)
      (declare (ignorable symbol))
      (incf n))
    n))

(defun count-external-symbols (package)
  "Return the number of external symbols in PACKAGE."
  #+(or cmu scl)  (values (lisp::external-symbol-count package))
  #+sbcl (values (sb-int:package-external-symbol-count package))
  #+clisp (svref (sys::%record-ref *package* 0) 2)
  #-(or cmu scl sbcl clisp) (portable-external-symbol-count package))

(defun package-grapher (stream package inferior-fun)
  "Draw package hierarchy graphs for `Show Package Users' and `Show Used Packages'."
  (let ((normal-ink +foreground-ink+)
        (arrow-ink  (make-rgb-color 0.72 0.72 0.72))
	(text-style (make-text-style :fix :roman :normal)))
    (with-drawing-options (stream :text-style text-style)
      (format-graph-from-roots (list package)
                               #'(lambda (package stream)
                                   (let ((internal (count-internal-symbols package))
                                         (external (count-external-symbols package)))
                                     (with-drawing-options (stream :ink (if (plusp external)
                                                                            normal-ink
                                                                            (make-rgb-color 0.4 0.4 0.4))
                                                                   :text-style text-style)
                                       (with-output-as-presentation (stream package 'package
                                                                            :single-box t)
                                         (format stream "~A (~D/~D)" (package-name package) internal external)))))
                               inferior-fun
                               :stream stream
                               :merge-duplicates t
                               :graph-type :tree
                               :orientation :horizontal
                               :arc-drawer
                               #'(lambda (stream foo bar x1 y1 x2 y2)
                                   (declare (ignore foo bar))
                                   (draw-arrow* stream x1 y1 x2 y2 :ink arrow-ink))))))

(define-command (com-show-used-packages :name "Show Used Packages"
                                        :command-table show-commands
                                        :menu "Used Packages"
                                        :provide-output-destination-keyword t)
    ((package-spec '(or package-name package) :prompt "package" :default *package*))
  (let ((real-package (when package-spec
                        (if (typep package-spec 'package)
                            package-spec
                            (find-package package-spec)))))
    (if (packagep real-package)
        (package-grapher *standard-output* real-package #'package-use-list)
        (note "~A is not a package." package-spec))))

(define-command (com-show-package-users :name "Show Package Users"
                                        :command-table show-commands
                                        :menu "Package Users"
                                        :provide-output-destination-keyword t)
    ((package-spec '(or package-name package) :prompt "package" :default *package*))
  (let ((real-package (when package-spec
                        (if (typep package-spec 'package)
                            package-spec
                            (find-package package-spec)))))
    (if (packagep real-package)
        (package-grapher *standard-output* real-package #'package-used-by-list)
        (note "~A is not a package." package-spec))))


;;; Filesystem Commands
;;; -------------------

(defun pathname-printing-name (pathname &optional relative-to)
  (if relative-to
      (native-enough-namestring pathname relative-to)
      (native-namestring pathname)))

(defun pretty-pretty-pathname (pathname stream &optional (relative-to nil))
  (with-output-as-presentation (stream pathname 'clim:pathname :single-box t)
    (let ((icon (icon-of pathname)))
      (when icon (draw-icon stream icon :extra-spacing 3)))
    (princ (pathname-printing-name pathname relative-to) stream))
  (terpri stream))

(defun actual-name (pathname)
  (if (cl-fad:directory-pathname-p pathname)
      (if (stringp (car (last (pathname-directory pathname))))
          (car (last (pathname-directory pathname)))
          (directory-namestring pathname))
      (native-namestring (file-namestring pathname))))

(defun sort-pathnames (list sort-by)
  (case sort-by            ; <--- FIXME
    ('name  (sort list #'string-lessp :key #'actual-name))
    (t list)))

(defun split-sort-pathnames (list group-dirs sort-by)
  (mapcar (lambda (x) (sort-pathnames x sort-by))
          (multiple-value-list
           (if (not group-dirs) (values list)
             (values (remove-if-not #'cl-fad:directory-pathname-p list)
                     (remove-if #'cl-fad:directory-pathname-p list))))))

(defun garbage-name-p (name)
  (when (> (length name) 2)
    (let ((first (elt name 0))
          (last  (elt name (1- (length name)))))
      (or (char= last #\~)
          (and (char= first #\#)
               (char= last  #\#))))))

(defun hidden-name-p (name)
  (and (> (length name) 1) (char= (elt name 0) #\.)))

(defun filter-garbage-pathnames (seq show-hidden hide-garbage)
  (remove-if (lambda (name)
               (or (and (not show-hidden) (hidden-name-p name))
                   (and hide-garbage (garbage-name-p name))))
             seq :key #'actual-name))

;; Change to using an :ICONIC view for pathnames?
(define-command (com-show-directory :name "Show Directory"
				    :command-table filesystem-commands
                                    :menu t
				    :provide-output-destination-keyword t)
    ((pathname 'pathname :prompt "pathname")
     &key
     (sort-by '(member name size modify none) :default 'name)
     (show-hidden  'boolean :default nil :prompt "show hidden")
     (hide-garbage 'boolean :default t   :prompt "hide garbage")
     (show-all     'boolean :default nil :prompt "show all")
     (style '(member :items :list) :default :items :prompt "listing style")
     (group-directories 'boolean :default t :prompt "group directories?")
     (full-names 'boolean :default nil :prompt "show full name?")
     (list-all-direct-subdirectories 'boolean :default nil :prompt "list all direct subdirectories?"))

  (let* ((pathname (probe-file pathname))
         (base-pathname (cl-fad:pathname-directory-pathname pathname))
         (query-pathname (make-pathname :host (pathname-host pathname)
                                        :name (or (pathname-name pathname) :wild)
                                        :type (or (pathname-type pathname) :wild)
                                        :directory :wild
                                        :version (or (pathname-version pathname) :wild)))
         (dir (uiop:while-collecting (files)
		(mapc (lambda (path)
			(when (or (pathname-match-p path query-pathname)
				  (and list-all-direct-subdirectories
				       (cl-fad:directory-pathname-p path)))
			  ;; files is a collector defined above
			  (files (truename path))))
		      (cl-fad:list-directory base-pathname)))))
    (with-text-family (t :sans-serif)
      (invoke-as-heading
       (lambda ()
         (cond
           ((wild-pathname-p pathname)
            (format t "Files matching ")
            (present query-pathname 'pathname))
           (t
            (format t "Contents of ")
            (present pathname 'pathname)))))

      (when (parent-directory pathname)
        (with-output-as-presentation (t (parent-directory pathname)
                                        'clim:pathname :single-box t)
          ;; Workaround new mcclim-images draw-icon silliness using
          ;; table formatter
          (formatting-table (t :move-cursor nil)
            (formatting-row ()
              (formatting-cell ()
                (draw-icon t (standard-icon "up-folder.xpm")
                           :extra-spacing 3)
                (format t "Parent Directory")))))
        ;; Note that the above leaves the cursor positioned at the end
        ;; of the "Parent Directory" line.
        (terpri))

      (dolist (group (split-sort-pathnames dir group-directories sort-by))
        (unless show-all
          (setf group (filter-garbage-pathnames group show-hidden hide-garbage)))
        (ecase style
          (:items
           (abbreviating-format-items
            group
            :row-wise nil :x-spacing "  " :y-spacing 1
            :printer (lambda (x stream)
                       (pretty-pretty-pathname x stream (if full-names
                                                            nil
                                                            base-pathname))))
           (multiple-value-bind (x y) (stream-cursor-position *standard-output*)
             (declare (ignore x))
             (setf (stream-cursor-position *standard-output*) (values 0 y))))
          (:list (dolist (ent group)
                   (let ((ent (merge-pathnames ent pathname)))
                     (pretty-pretty-pathname ent *standard-output* full-names)))))))))

(define-command (com-change-directory :name "Change Directory"
                                      :menu t
                                      :command-table filesystem-commands)
  ((pathname 'pathname :prompt "pathname"))
  (let ((pathname (merge-pathnames
                   ;; helpfully fix things if trailing slash wasn't entered
                   (cl-fad:pathname-as-directory pathname))))
    (if (not (probe-file pathname))
        (note "~A does not exist.~%" pathname)
        (progn
	  (uiop:chdir pathname)
	  (setf *default-pathname-defaults* pathname)))))

(define-command (com-up-directory :name "Up Directory"
                                  :menu t
                                  :command-table filesystem-commands)
  ()
  (let ((parent (parent-directory *default-pathname-defaults*)))
    (when parent
      (uiop:chdir parent)
      (setf *default-pathname-defaults* parent)
      (italic (t)
        (format t "~&The current directory is now ")
        (present (truename parent))
        (terpri)))))

(define-gesture-name :change-directory :pointer-button-press
  (:middle))

(define-presentation-to-command-translator change-directory-translator
  (clim:pathname com-change-directory filesystem-commands :gesture :change-directory
		 :pointer-documentation ((object stream)  (declare (ignore object))
					 (format stream "Change to this directory"))
                 :documentation ((object stream)  (declare (ignore object))
                                 (format stream "Change to this directory"))

		 :tester ((object)
			  (cl-fad:directory-pathname-p object)))
  (object)
  (list object))


;;; External file viewers

(defgeneric mime-type-to-command (mime-type pathname)
  (:documentation "Translates a pathname to an invocation of a CLIM command, typically according to the mime type deduced for that pathname. Returns three values: command, documentation, and pointer documentation."))

;; This pathname translator stuff is really turning into a mess.
;; What I need to do is merge mime types with presentations, and
;; rip all the rest of this shit out.

(defmethod mime-type-to-command (mime-type pathname)
  (declare (ignore mime-type pathname))
  (values nil nil))

(defmethod mime-type-to-command ((mime-type null) pathname)
  (declare (ignore #|mime-type|# pathname))
  (values nil nil))

(defmethod mime-type-to-command ((mime-type symbol) pathname)
  (mime-type-to-command (c2mop:class-prototype (find-class mime-type nil)) pathname))

;; Move these elsewhere.

(defmethod mime-type-to-command ((mime-type text/x-lisp-source) pathname)
  (values `(com-compile-and-load ,pathname)
          "Compile and Load"
          (format nil "Compile and load ~A" pathname)))

(defmethod mime-type-to-command ((mime-type application/x-lisp-fasl) pathname)
  (values `(com-load-file ,pathname)
          "Load"
          (format nil "Load ~A" pathname)))

;; I've taken to doing translator documentation exactly opposite of how the CLIM
;; spec seems to intend. The spec says that the pointer-documentation should be
;; short and quickly computed, and the documentation should be longer and more
;; descriptive. Personally, I like seeing the full the command with the arguments
;; in the pointer-doc window, and something short in right-button menus.
;; So.. yeah.

(defun automagic-translator (pathname)
  "Returns 2 values: the command translation, and a documentation string for the translation."
  (cond ((wild-pathname-p pathname)
         (values `(com-show-directory ,pathname)
                 "Show Matching Files"
                 (format nil "Show Files Matching ~A" pathname)))
        ((not (probe-file pathname))
         (values nil nil nil))
        ((cl-fad:directory-pathname-p pathname)
         (values `(com-show-directory ,pathname)
                 "Show Directory"
                 (format nil "Show Directory ~A" pathname)))
        (t
         (let ((canon-type (first (find (pathname-type pathname)
                                        mezzano.gui.filer::*type-registry*
                                        :test (lambda (x y) (member x y :test #'string=))
                                        :key #'rest))))
           (cond (canon-type
                  (values `(com-view ,pathname)
                          "View"
                          (format nil "View ~A" pathname)))
                 (t
                  nil
                  #+(or) ; these all use run/background run. no good.
                  (multiple-value-bind (command doc pointer-doc)
                      (find-viewspec pathname)
                    (let ((mime-type (pathname-mime-type pathname)))
                      (mv-or
                       (when mime-type (mime-type-to-command mime-type pathname))
                       (when command
                         (values command doc pointer-doc)))))))))))

(define-presentation-translator automagic-pathname-translator
  (clim:pathname clim:command filesystem-commands
                 :gesture :select
                 :priority 2
                 :tester ((object)
                          (automagic-translator object))
                 :documentation ((object stream)
                                 (princ (nth-value 1 (automagic-translator object)) stream))
                 :pointer-documentation ((object stream)
                                         (princ (nth-value 2 (automagic-translator object)) stream)))
  (object)
  (values
   (automagic-translator object)
   'command))

(define-command (com-view :name "View")
  ((pathname 'pathname :prompt "pathname"))
  (let ((pathname (merge-pathnames pathname)))
    (mezzano.gui.filer::view (mezzano.gui.filer::canonical-type-from-pathname-type
                              (pathname-type pathname))
                             pathname)))


;;; The directory stack.

(defvar *directory-stack* nil) ;; FIXME: This should probably be a slot of the frame.

(defun compute-dirstack-command-eligibility (frame)
  (let* ((stack *directory-stack*)
         (state (if stack t nil)))
    (setf (command-enabled 'com-drop-directory frame) state
          (command-enabled 'com-pop-directory  frame) state
          (command-enabled 'com-swap-directory frame) state)))

(defmacro with-directory-stack (() &body body)
  `(prog1
     (if *directory-stack*
         (progn ,@body)
         (note "The directory stack is empty."))
     (compute-dirstack-command-eligibility *application-frame*)))

(define-command (com-push-directory :name "Push Directory"
                                    :menu t
                                    :command-table directory-stack-commands)
  ((pathname 'pathname :prompt "directory"))
  (let ((pathname (merge-pathnames (cl-fad:pathname-as-directory pathname))))
    (if (not (probe-file pathname))
        (note "~A does not exist.~%" pathname)
        (progn (push *default-pathname-defaults* *directory-stack*)
               (com-change-directory pathname))))
  (compute-dirstack-command-eligibility *application-frame*))

(defun comment-on-dir-stack ()
  (if *directory-stack*
      (progn
        (format t "~&The top of the directory stack is now ")
        (present (truename (first *directory-stack*)))
        (terpri))
      (format t "~&The directory stack is now empty.~%")))

(define-command (com-pop-directory :name "Pop Directory"
                                  :menu t
                                  :command-table directory-stack-commands)
  ()
  (with-directory-stack ()
    (com-change-directory (pop *directory-stack*))
    (comment-on-dir-stack)))

(define-command (com-drop-directory :name "Drop Directory"
                                    :menu t
                                    :command-table directory-stack-commands)
  ()
  (with-directory-stack ()
    (setf *directory-stack* (rest *directory-stack*))
    (comment-on-dir-stack)))


(define-command (com-swap-directory :name "Swap Directory"
                                    :menu t
                                    :command-table directory-stack-commands)
  ()
  (with-directory-stack ()
    (psetf (first *directory-stack*) *default-pathname-defaults*
           *default-pathname-defaults* (first *directory-stack*))
    (comment-on-dir-stack)))

(define-command (com-display-directory-stack :name "Display Directory Stack"
                                             :menu t
                                             :command-table directory-stack-commands)
  ()
  (with-directory-stack ()
    (dolist (pathname *directory-stack*)
      (fresh-line)
      (pretty-pretty-pathname pathname *standard-output*)
      (terpri))))

(define-presentation-to-command-translator display-dir-stack-translator
  (directory-stack com-display-directory-stack filesystem-commands :gesture :select)
    (object))

(define-command (com-edit-file :name "Edit File"
                               :menu t
			       :command-table filesystem-commands
			       :provide-output-destination-keyword nil)
  ((pathname 'pathname  :prompt "pathname"))
  (clim-sys:make-process (lambda () (ed pathname))))

(define-presentation-to-command-translator edit-file
  (clim:pathname com-edit-file filesystem-commands :gesture :select
		 :pointer-documentation ((object stream)
					 (format stream "Edit ~A" object))
		 :documentation ((object stream)
                                 (declare (ignore object))
                                 (format stream "Edit File"))
		 :tester ((object)
			  (and (not (wild-pathname-p object))
                               (probe-file object)
                               (pathname-name object)
                               (let ((mime-type (pathname-mime-type object)))
                                 (and mime-type (subtypep mime-type 'text))))))
  (object)
  (list object))

(define-command (com-show-file :name "Show File" :command-table filesystem-commands
                               :menu t
			       :provide-output-destination-keyword t)
  ((object 'pathname :prompt "pathname"))
  (show-file object))

(define-presentation-to-command-translator show-file
  (clim:pathname com-show-file filesystem-commands :gesture :select
		 :pointer-documentation ((object stream)
					 (format stream "Show ~A" object))
		 :documentation ((object stream)
                                 (declare (ignore object))
                                 (format stream "Show File"))
		 :tester ((object)
			  (and (not (wild-pathname-p object))
                               (probe-file object)
                               (pathname-name object)
                               (let ((mime-type (pathname-mime-type object)))
                                 (and mime-type (subtypep mime-type 'text))))))
  (object)
  (list object))

#+(or) ; Doesn't seem to work on png images. stick with the native image-viewer instead
(define-command (com-display-image :name t :command-table filesystem-commands
                                   :menu t
                                   :provide-output-destination-keyword t)
    ((image-pathname 'pathname
      :default (user-homedir-pathname) :insert-default t))
  (if (probe-file image-pathname)
      (let* ((type (funcall (case (readtable-case *readtable*)
                              (:upcase #'string-upcase)
                              (:downcase #'string-downcase)
                              (t #'identity))
                            (pathname-type image-pathname)))
             (format (find-symbol type (find-package :keyword))))
        (handler-case (let ((pattern (make-pattern-from-bitmap-file image-pathname :format format)))
                        (with-room-for-graphics ()
                          (draw-pattern* *standard-output* pattern 0 0)))
          (unsupported-bitmap-format ()
            (format t "Image format ~A not recognized" type))))
      (format t "No such file: ~A" image-pathname)))

#+(or) ; disabled for demo4 because source pathname are machine-specific.
(define-command (com-edit-definition :name "Edit Definition"
				     :command-table lisp-commands
                                     :menu t
				     :provide-output-destination-keyword nil)
  ((function-name 'function-name :prompt "function name"))
  (clim-sys:make-process (lambda () (ed function-name))))

(define-presentation-to-command-translator edit-definition
  (function-name com-edit-definition lisp-commands :gesture :select
	  :pointer-documentation ((object stream)
				  (format stream "Edit Definition of ~A" object))
	  :documentation ((object stream)
                          (declare (ignore object))
                          (format stream "Edit Definition")))
    (object)
  (list object))


(defun show-file (pathname)
  (let ((content (alexandria:read-file-into-string pathname))
	(*standard-output* (open-window-stream :scroll-bars :both
					       :label (namestring pathname))))
    (princ content)
    (terpri)))

;;; Eval

(defun display-evalues (values)
  (labels
      ((present-value (value)
         ;; I would really prefer this to behave as below, as presenting
         ;; things as expressions causes translators applicable to expression
         ;; to override those which would be otherwise applicable (such as
         ;; the set-current-package translator). I retain the use of w-o-a-p,
         ;; swapping the inner/outer presentation types, with the assumption
         ;; that someone (the form reader?) really does want expressions, and
         ;; the presentation-type-of is seldom a subtype of expression.
         ;; Aside from that, the problem with my code below is that it
         ;; will use the default presentation method for the type, which will
         ;; not necessarily print in the fashion expected from the lisp REPL.
         ;; Possibly this +listener-view+ could save the day here, but I'm
         ;; unclear on why it exists.   --Hefner

         ;; Okay, set-current-package translator now mysteriously works, but
         ;; I stand by the notion that 'expression should not be the type of
         ;; the innermost presentation.

         #+(or)
         (with-output-as-presentation (t value 'expression :single-box t)
           (present value (presentation-type-of value) :single-box t))

         (with-output-as-presentation (t value (presentation-type-of value)
                                         :single-box t)
           (present value 'expression))))
    (with-drawing-options (t :ink +olivedrab+)
      (cond ((null values) #+NIL (format t "No values.~%"))
            ((= 1 (length values))
             (present-value (first values))
             (fresh-line))
            (t (do* ((i 0 (1+ i))
                     (items values (rest items))
                     (object (first items) (first items)))
                    ((null items))
               (with-drawing-options (t :ink +limegreen+)
                 (with-text-style (t (make-text-style nil :italic :small))
                   (format t "~A  " i)))
                 (present-value object)
                 (fresh-line)))))))

(defun shuffle-specials (form values)
  (setf +++ ++
        ++  +
        +   form
        /// //
        //  /
        /   values
        *** **
        **  *
        *   (first values)))

;;; The background evaluation feature is neat, but there are thread
;;; safety issues doing output to streams, special variables have
;;; unexpected values, and input doesn't work right due to racing to
;;; read from the event queue. Sadly, I am forced to disable it by
;;; default.

(defparameter *use-background-eval* nil
  "Perform evaluation in a background thread, which can be interrupted.")

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (let ((standard-output *standard-output*)
        (standard-input *standard-input*)
        (debugger-hook *debugger-hook*)
	(application-frame *application-frame*))
    (flet ((evaluate ()
             (let ((- form)
                   (*standard-output* standard-output)
                   (*standard-input* standard-input)
                   (*error-output* standard-output)
                   (*debugger-hook* debugger-hook)
		   (*application-frame* application-frame)
                   error success)
               (if *use-background-eval*
                   (unwind-protect (handler-case (prog1 (cons :values (multiple-value-list (eval form)))
                                                   (setf success t))
                                     (serious-condition (e)
                                       (setf error e)
                                       (error e)))
                     (when (not success)
                       (return-from evaluate (cons :error error))))
                   (cons :values (multiple-value-list (eval form)))))))
      ;; If possible, use a thread for evaluation, permitting us to
      ;; interrupt it.
      (let ((start-time (get-internal-real-time)))
        (destructuring-bind (result . value)
            (if (and *use-background-eval* clim-sys:*multiprocessing-p*)
                (catch 'done
                  (let* ((orig-process (clim-sys:current-process))
                         (evaluating t)
                         (eval-process
                          (clim-sys:make-process
                           #'(lambda ()
                               (let ((result (evaluate)))
                                 (when evaluating
                                   (clim-sys:process-interrupt orig-process
                                                               #'(lambda ()
                                                                   (throw 'done result)))))))))
                    (unwind-protect
                         (handler-case (loop for gesture = (read-gesture)
                                             when (and (typep gesture 'keyboard-event)
                                                       (eq (keyboard-event-key-name gesture) :pause))
                                             do (clim-sys:process-interrupt eval-process #'break))
                           (abort-gesture ()
                             (clim-sys:destroy-process eval-process)
                             (cons :abort (/ (- (get-internal-real-time) start-time)
                                             internal-time-units-per-second))))
                      (setf evaluating nil))))
                (evaluate))
          (ecase result
            (:values
             (fresh-line)
             (shuffle-specials form value)
             (display-evalues value)
             (fresh-line))
            (:error (with-text-style (t (make-text-style nil :italic nil))
                      (if value
                          (with-output-as-presentation (t value 'expression)
                            (format t "Aborted due to ~A: ~A" (type-of value) value))
                          (format t "Aborted for unknown reasons (possibly use of ~A)." 'break))))
            (:abort (with-text-style (t (make-text-style nil :italic nil))
                      (format t "Aborted by user after ~F seconds." value)))))))))

;;; Some CLIM developer commands

(define-command (com-show-command-table :name t
                                        :menu "Command Table"
                                        :command-table show-commands)
    ((table 'clim:command-table :prompt "command table")
     &key
     ;;(locally 'boolean :default nil :mentioned-default t)
     (show-commands 'boolean :default t))
  (let ((our-tables nil)
	(processed-commands (make-hash-table :test #'eq)))
    (do-command-table-inheritance (ct table)
      (let ((commands nil))
	(map-over-command-table-commands
	 #'(lambda (command)
	     (unless (gethash command processed-commands)
	       (push command commands)
	       (setf (gethash command processed-commands) t)))
	 ct
	 :inherited nil)
	(push (cons ct (sort commands
                             (lambda (x y)
                               (string-lessp (command-line-name-for-command x ct :errorp :create)
                                             (command-line-name-for-command y ct :errorp :create)))))
              our-tables)))
    (setq our-tables (nreverse our-tables))

    (when show-commands ;; sure, why not?
      (dolist (foo our-tables)
        (let ((ct       (car foo))
              (commands (cdr foo)))
          (invoke-as-heading
           (lambda ()
             (format t "Command table ")
             (with-output-as-presentation (t ct 'clim:command-table :single-box t)
               (princ (command-table-name ct)))))
          (if commands
              (format-items commands :printer (lambda (cmd s) (present cmd 'clim:command-name :stream s))
                            :move-cursor t)
              (note "Command table is empty.~%~%") ))))))


;;; Various Lisp goodies

(define-presentation-type package ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type package))
  (packagep object))

(define-presentation-method present (object (type package) stream
				     (view textual-view)
				     &key)
  (princ (package-name object) stream))

(define-presentation-method accept ((type package) stream (view textual-view)
				    &key)
  (multiple-value-bind
	(object success)
      (completing-from-suggestions (stream)
	(loop
	   for p in (list-all-packages)
	   do (progn
		(suggest (package-name p) p)
		(loop
		   for n in (package-nicknames p)
		   do (suggest n p)))))
    (if success
	object
	(simple-parse-error "No package"))))

(define-command (com-set-package :name t
                                 :menu t
				 :command-table lisp-commands
				 :provide-output-destination-keyword nil)
    ((p 'package))
  (setf *package* p))

(define-presentation-to-command-translator set-current-package
    (package com-set-package lisp-commands
             :pointer-documentation ((object stream)
                                     (format stream "Set current package to ~A" (package-name object)))
             :documentation ((object stream)
                             (declare (ignore object))
                             (format stream "Set Package"))
             :menu t
             :tester ((object) (not (eql *package* object))))
    (object)
  (list object))
