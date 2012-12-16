;;;; DEFSTRUCT.

;;; This needs to be redone with CLOS in mind.

(in-package #:sys.int)

(defvar *structure-type-type* nil)

(defun make-struct-type (name slots parent area)
  (let ((x (%make-struct 5 :static)))
    (setf (%struct-slot x 0) *structure-type-type*
	  (%struct-slot x 1) name
	  (%struct-slot x 2) slots
          (%struct-slot x 3) parent
          (%struct-slot x 4) area)
    x))

(defun structure-name (object)
  (unless (eq (%struct-slot object 0) *structure-type-type*)
    (error 'type-error :datum object :expected-type 'structure-definition))
  (%struct-slot object 1))

(defun structure-slots (object)
  (unless (eq (%struct-slot object 0) *structure-type-type*)
    (error 'type-error :datum object :expected-type 'structure-definition))
  (%struct-slot object 2))

(defun structure-parent (object)
  (unless (eq (%struct-slot object 0) *structure-type-type*)
    (error 'type-error :datum object :expected-type 'structure-definition))
  (%struct-slot object 3))

(defun structure-area (object)
  (unless (eq (%struct-slot object 0) *structure-type-type*)
    (error 'type-error :datum object :expected-type 'structure-definition))
  (%struct-slot object 4))

;;; Bootstrap the defstruct system.
(defun bootstrap-defstruct ()
  (setf *structure-type-type* nil
        *structure-type-type* (make-struct-type 'structure-definition
						'((name structure-name nil t t)
						  (slots structure-slots nil t t)
                                                  (parent structure-parent nil t t)
                                                  (area structure-area nil t t))
                                                nil
                                                :static))
  (setf (%struct-slot *structure-type-type* 0) *structure-type-type*)
  (setf (get 'structure-definition 'structure-type) *structure-type-type*))

(unless *structure-type-type*
  (bootstrap-defstruct))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun parse-defstruct-options (name-and-options)
  (let ((name nil)
	(suppress-constructors nil)
	(constructors '())
	(predicate-name t)
        (copier-name t)
        (area nil)
        (conc-namep t)
        (conc-name nil)
        (included-structure-name nil)
        (included-slot-descriptions nil)
        (included-structure nil)
        (print-object nil)
        (print-function nil)
        (print-object-specializer nil))
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
	       (cond ((eq (second option) 'nil)
		      (setf copier-name nil))
		     ((null copier-name)
		      (error "Copier option ~S conflicts with (:copier nil) used earlier." option))
		     ((eq copier-name t)
		      (setf copier-name (second option)))
		     (t (error "Multiple copier options supplied."))))
              ;; (:area name)
              ((and (consp option)
		    (eq :area (first option))
		    (cdr option)
		    (null (cddr option)))
               (setf area (second option)))
              ;; :conc-name, same as (:conc-name nil). no prefix.
              ((eql option :conc-name)
               (setf conc-namep nil))
              ;; (:conc-name &optional name)
              ((and (consp option)
                    (eql (first option) :conc-name)
                    (null (cddr option)))
               (if (second option)
                   (setf conc-namep t
                         conc-name (second option))
                   (setf conc-namep nil)))
              ((or (eql option :include)
                   (and (consp option)
                        (eql (first option) :include)
                        (null (cdr option))))
               (error "Malformed :INCLUDE option ~S." option))
              ((and (consp option)
                    (eql (first option) :include))
               (when included-structure-name
                 (error "Multiple :INCLUDE options in DEFSTRUCT."))
               (setf included-structure-name (second option)
                     included-slot-descriptions (cddr option)))
              ;; (:print-object) or (:print-function)
              ((and (consp option)
                    (member (first option) '(:print-object :print-function))
                    (null (cdr option)))
               (when (or print-function print-object print-object-specializer)
                 (error "Multiple :PRINT-OBJECT or :PRINT-FUNCTION options specified."))
               (setf print-object-specializer t))
              ;; (:print-object function-name)
              ((and (consp option)
                    (eql (first option) :print-object)
                    (null (cddr option)))
               (when (or print-function print-object print-object-specializer)
                 (error "Multiple :PRINT-OBJECT or :PRINT-FUNCTION options specified."))
               (setf print-object (second option)))
              ;; (:print-function function-name)
              ((and (consp option)
                    (eql (first option) :print-function)
                    (null (cddr option)))
               (when (or print-function print-object print-object-specializer)
                 (error "Multiple :PRINT-OBJECT or :PRINT-FUNCTION options specified."))
               (setf print-function (second option)))
	      (t (error "Unsupported DEFSTRUCT option ~S" option))))))
    (values name
            (when conc-namep
              (intern (string (or conc-name
                                  (concat-symbols name '-)))))
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
	      (t predicate-name))
            area
	    (cond
	      ;; No copier.
	      ((null copier-name)
	       nil)
	      ;; Default copier.
	      ((eq copier-name 't)
	       (concat-symbols 'copy- name))
	      ;; Explicit copier.
	      (t predicate-name))
            included-structure-name included-slot-descriptions
            print-object print-function print-object-specializer)))

;; Parses slot-description and produces:
;; (slot-name accessor-name initform type read-only)
(defun parse-defstruct-slot (conc-name slot)
  (if (symbolp slot)
      (list slot (concat-symbols conc-name slot) nil t nil)
      (destructuring-bind (slot-name &optional slot-initform &key (type 't) read-only)
          slot
        (list slot-name (concat-symbols conc-name slot-name) slot-initform type read-only))))

(defun generate-simple-defstruct-constructor (struct-type name area)
  (let ((tmp (gensym)))
    `(defun ,name (&key ,@(mapcar (lambda (slot)
				    (list (first slot) (third slot)))
				  (structure-slots struct-type)))
       (let ((,tmp (%make-struct ,(1+ (length (structure-slots struct-type))) ',area)))
	 (setf (%struct-slot ,tmp 0) ',struct-type)
	 ,@(let ((n 0))
	     (mapcar (lambda (slot)
		       (setf n (1+ n))
		       `(setf (%struct-slot ,tmp ,n) ,(first slot)))
		     (structure-slots struct-type)))
	 ,tmp))))

(defun generate-defstruct-constructor (struct-type name lambda-list area)
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
	 (let ((,tmp (%make-struct ,(1+ (length (structure-slots struct-type))) ',area)))
	   (setf (%struct-slot ,tmp 0) ',struct-type)
	   ,@(let ((n 0))
	       (mapcar (lambda (s)
			 (setf n (1+ n))
			 `(setf (%struct-slot ,tmp ,n) ,(if (member (first s) default-slots)
							    (third s)
							    (first s))))
		       (structure-slots struct-type)))
	   ,tmp)))))

(defun compute-defstruct-slots (conc-name slot-descriptions included-structure included-slot-descriptions)
  (when included-slot-descriptions
    (error "Included slot-descriptions not supported yet."))
  (append (when included-structure
            (structure-slots included-structure))
          (mapcar (lambda (s)
                    (parse-defstruct-slot conc-name s))
                  slot-descriptions)))

)

(defun structure-type-p (object struct-type)
  (when (structure-object-p object)
    (do ((object-type (%struct-slot object 0) (structure-parent object-type)))
        ((null object-type) nil)
      (when (eq object-type struct-type)
        (return t)))))

(defun copy-structure (structure)
  (assert (structure-object-p structure) (structure) "STRUCTURE is not a structure!")
  (let* ((struct-type (%struct-slot object 0))
         (n-slots (length (structure-slots struct-type)))
         (new (%make-struct (1+ n-slots)
                            (structure-area struct-type))))
    (setf (%struct-slot new 0) struct-type)
    (dotimes (i n-slots)
      (setf (%struct-slot new (1+ i)) (%struct-slot structure (1+ i))))
    new))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (multiple-value-bind (name conc-name constructors predicate area copier
                        included-structure-name included-slot-descriptions
                        print-object print-function print-object-specializer)
      (parse-defstruct-options name-and-options)
    (let* ((included-structure (when included-structure-name
                                 (get-structure-type included-structure-name)))
           (slots (compute-defstruct-slots conc-name
                                           slot-descriptions
                                           included-structure
                                           included-slot-descriptions))
	   (struct-type (or (get name 'structure-type)
                            (make-struct-type name slots included-structure area))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (%defstruct ',struct-type))
	 ,@(when predicate
	      (list `(defun ,predicate (object)
                       (structure-type-p object ',struct-type))))
         ,@(when copier
             (list `(defun ,copier (object)
                      (check-type object ,name)
                      (copy-structure object))))
         ,@(when print-object
             (list `(defmethod print-object ((object ,name) stream)
                      (funcall (function ,print-object) object stream))))
         ,@(when print-function
             (list `(defmethod print-object ((object ,name) stream)
                      (funcall (function ,print-function) object stream 0))))
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
			 (generate-simple-defstruct-constructor struct-type x area)
			 (generate-defstruct-constructor struct-type (first x) (second x) area)))
		   constructors)
	 ',name))))
