;;;; DEFSTRUCT.

;;; This needs to be redone with CLOS in mind.

(in-package "SYSTEM.INTERNALS")

(defvar *structure-type-type* nil)

(defun make-struct-type (name slots)
  (let ((x (%make-struct 3)))
    (setf (%struct-slot x 0) *structure-type-type*
	  (%struct-slot x 1) name
	  (%struct-slot x 2) slots)
    x))

(defun structure-name (object)
  (unless (eq (%struct-slot object 0) *structure-type-type*)
    (error 'type-error :datum object :expected-type 'structure-definition))
  (%struct-slot object 1))

(defun structure-slots (object)
  (unless (eq (%struct-slot object 0) *structure-type-type*)
    (error 'type-error :datum object :expected-type 'structure-definition))
  (%struct-slot object 2))

;;; Bootstrap the defstruct system.
(unless *structure-type-type*
  (setf *structure-type-type* (make-struct-type 'structure-definition
						'((name structure-name nil t t)
						  (slots structure-slots nil t t))))
  (setf (%struct-slot *structure-type-type* 0) *structure-type-type*)
  (setf (get 'structure-definition 'structure-type) *structure-type-type*))

(defun parse-defstruct-options (name-and-options)
  (let ((name nil)
	(suppress-constructors nil)
	(constructors '())
	(predicate-name t))
    (if (symbolp name-and-options)
	(setf name name-and-options)
	(progn
	  (setf name (first name-and-options))
	  (dolist (option (rest name-and-options))
	    (cond
	      ;; :constructor or (:constructor)
	      ;; Empty option, ignored.
	      ((or (eq :constructor option)
		   (and (consp option)
			(eq :constructor (first option))
			(null (rest option)))))
	      ;; (:constructor name)
	      ;; A constructor with an explicit name and default argument list.
	      ((and (consp option)
		    (eq :constructor (first option))
		    (cdr option)
		    (null (cddr option)))
	       (if (eq (second option) 'nil)
		   ;; Disable the constructor.
		   (setf suppress-constructors t)
		   (push (second option) constructors)))
	      ;; (:constructor name BOA-lambda-list)
	      ((and (consp option)
		    (eq :constructor (first option))
		    (cddr option)
		    (null (cdddr option)))
	       (if (eq (second option) 'nil)
		   (setf suppress-constructors t)
		   (push (rest option) constructors)))
	      ;; :predicate or (:predicate)
	      ;; Empty option, ignored.
	      ((or (eq :predicate option)
		   (and (consp option)
			(eq :predicate (first option))
			(null (cdr option)))))
	      ;; (:predicate name)
	      ((and (consp option)
		    (eq :predicate (car option))
		    (cdr option)
		    (null (cddr option)))
	       (cond ((eq (second option) 'nil)
		      (setf predicate-name nil))
		     ((null predicate-name)
		      (error "Predicate option ~S conflicts with (:predicate nil) used earlier." option))
		     ((eq predicate-name t)
		      (setf predicate-name (second option)))
		     (t (error "Multiple predicate options supplied."))))
	      ;; :copier or (:copier)
	      ;; Empty option, ignored.
	      ((or (eq :copier option)
		   (and (consp option)
			(eq :copier (first option))
			(null (rest option)))))
	      ;; (:copier name)
	      ((and (consp option)
		    (eq :copier (first option))
		    (cdr option)
		    (null (cddr option)))
	       ;; TODO: disable copier when supported
	       (cond ((eq (second option) 'nil))
		     (t (error "TODO: copier option."))))
	      (t (error "Unsupported DEFSTRUCT option ~S" option))))))
    (values name
	    ;; TODO: conc-name.
	    (concat-symbols name '-)
	    (cond
	      ;; No constructor.
	      (suppress-constructors
	       (when constructors
		 (error "Constructor options supplied conflict with (:constructor nil)."))
	       '())
	      ;; Explicit constructors.
	      (constructors)
	      ;; Default constructor.
	      (t (list (concat-symbols 'make- name))))
	    (cond
	      ;; No predicate.
	      ((null predicate-name)
	       nil)
	      ;; Default predicate.
	      ((eq predicate-name 't)
	       (concat-symbols name '-p))
	      ;; Explicit predicate.
	      (t predicate-name)))))

;; Parses slot-description and produces:
;; (slot-name accessor-name initform type read-only)
(defun parse-defstruct-slot (conc-name slot)
  (if (symbolp slot)
      (list slot (concat-symbols conc-name slot) nil t nil)
      ;; TODO: read-only and type when destructuring-bind exists.
      (list (first slot) (concat-symbols conc-name (first slot)) (second slot) t nil)))

(defun generate-simple-defstruct-constructor (struct-type name)
  (let ((tmp (gensym)))
    `(defun ,name (&key ,@(mapcar (lambda (slot)
				    (list (first slot) (third slot)))
				  (structure-slots struct-type)))
       (let ((,tmp (%make-struct ,(1+ (length (structure-slots struct-type))))))
	 (setf (%struct-slot ,tmp 0) ',struct-type)
	 ,@(let ((n 0))
	     (mapcar (lambda (slot)
		       (setf n (1+ n))
		       `(setf (%struct-slot ,tmp ,n) ,(first slot)))
		     (structure-slots struct-type)))
	 ,tmp))))

(defun generate-defstruct-constructor (struct-type name lambda-list)
  (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore enable-keys allow-other-keys))
    ;; Pick out the slot names and compute the slots without a lambda variable
    (let* ((assigned-slots (append required
				   (mapcar #'first optional)
				   (remove 'nil (mapcar #'third optional))
				   (when rest (list rest))
				   (mapcar #'cadar keys)
				   (remove 'nil (mapcar #'third keys))
				   (mapcar #'first aux)))
	   (default-slots (set-difference (mapcar #'first (structure-slots struct-type)) assigned-slots))
	   (tmp (gensym)))
      `(defun ,name ,lambda-list
	 (let ((,tmp (%make-struct ,(1+ (length (structure-slots struct-type))))))
	   (setf (%struct-slot ,tmp 0) ',struct-type)
	   ,@(let ((n 0))
	       (mapcar (lambda (s)
			 (setf n (1+ n))
			 `(setf (%struct-slot ,tmp ,n) ,(if (member (first s) default-slots)
							    (third s)
							    (first s))))
		       (structure-slots struct-type)))
	   ,tmp)))))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (multiple-value-bind (name conc-name constructors predicate)
      (parse-defstruct-options name-and-options)
    (let* ((slots (mapcar (lambda (s) (parse-defstruct-slot conc-name s)) slot-descriptions))
	   (struct-type (make-struct-type name slots)))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (%defstruct ',struct-type))
	 ,@(when predicate
	      (list `(defun ,predicate (object)
		       (and (structure-object-p object)
			    (eq (%struct-slot object 0) ',struct-type)))))
	 ,@(let ((n 0))
	     (mapcar (lambda (s)
		       (setf n (1+ n))
		       `(progn
			  (defun ,(second s) (object)
			    (the ,(fourth s) (%struct-slot (the ,name object) ,n)))
			  ,@(unless (fifth s)
			      (list `(defun (setf ,(second s)) (new-value object)
				       (funcall #'(setf %struct-slot)
						(the ,(fourth s) new-value)
						(the ,name object)
						,n))))))
		     slots))
	 ,@(mapcar (lambda (x)
		     (if (symbolp x)
			 (generate-simple-defstruct-constructor struct-type x)
			 (generate-defstruct-constructor struct-type (first x) (second x))))
		   constructors)
	 ',name))))