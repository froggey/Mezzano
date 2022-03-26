;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.binary-data)

(defvar *in-progress-objects* nil)

(defconstant +null+ (code-char 0))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))


;;; Binary types

(defmacro define-binary-type (name (&rest args) &body spec)
  (with-gensyms (type stream value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
      (declare (ignorable ,@args))
      ,(type-reader-body spec stream))
    (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
      (declare (ignorable ,@args))
      ,(type-writer-body spec stream value)))))

(defun type-reader-body (spec stream)
  (ecase (length spec)
    (1 (destructuring-bind (type &rest args) (mklist (first spec))
         `(read-value ',type ,stream ,@args)))
    (2 (destructuring-bind ((in) &body body)
           (cdr (or (assoc :reader spec)
                    (error "No reader found in ~s" spec)))
         `(let ((,in ,stream)) ,@body)))))

(defun type-writer-body (spec stream value)
  (ecase (length spec)
    (1 (destructuring-bind (type &rest args) (mklist (first spec))
         `(write-value ',type ,stream ,value ,@args)))
    (2 (destructuring-bind ((out v) &body body)
           (cdr (or (assoc :writer spec)
                    (error "No :writer found in ~s" spec)))
         `(let ((,out ,stream) (,v ,value)) ,@body)))))

;;; Enumerations

(defmacro define-enumeration (name (type) &rest mapping)
  (let ((mapping (normalize-mapping mapping)))
    (with-gensyms (in out value)
      `(define-binary-type ,name ()
	 (:reader (,in)
		  (let ((,value (read-value ',type ,in)))
		    (case ,value
		      ,@(loop for (symbol number) in mapping collect `(,number ',symbol))
		      (otherwise (error "No ~a for value: ~a" ',name ,value)))))
	 (:writer (,out ,value)
		  (write-value ',type ,out
			       (case ,value
				 ,@(loop for (symbol number) in mapping collect `(,symbol ,number))
				 (otherwise (error "~a not a legal ~a" ,value ',name)))))))))

(defun normalize-mapping (mapping)
  (loop with number = 0
     for entry in mapping collect
       (typecase entry
	 (symbol
	  (prog1 (list entry number) (incf number)))
	 (cons
	  (let ((actual-number (or (second entry) number)))
	    (prog1 (list (first entry) actual-number)
	      (setf number (1+ actual-number))))))))

;;; Binary classes

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))
       
       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))
       
       ,read-method
       
       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))


;; FIXME 2005-07-27 <peter@gigamonkeys.com> -- one problem with this
;; scheme is that when you instantiate an instance of a tagged class
;; you need to manually fill in the tag slots. This is because the
;; dispatching at the moment is asymmetric--we read values and then
;; use them to determine the type but there's no reverse mapping, from
;; the type to the slot values. For simple tagged classes where the
;; tag is a single slot and there is a simple mapping from literal
;; value to class there ought to be a simple way to use an enumeration
;; type as the slot value and then define a method on
;; INITIALIZE-INSTANCE that fills in the slot with the name of the
;; class. For tagged classes that actually need to do runtime
;; computation to determine the mapping from value to class (such as
;; attributes in Java class files) we could require that the
;; define-tagged-binary-class also include a reverse mapping. Then
;; it's up to the author of the tagged class to make the two mappings
;; symmetric.

(defmacro define-tagged-binary-class (&whole whole name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
      (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
        (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
          (let ((,objectvar
                 (make-instance 
                  ,@(or (cdr (assoc :dispatch options))
                        (error "No :dispatch form found in ~s" whole))
                  ,@(mapcan #'slot->keyword-arg slots))))
            (read-object ,objectvar ,streamvar)
            ,objectvar))))))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;;; Keeping track of inherited slots

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  "Like all slots but works while compiling a new class before slots
and superclasses have been saved."
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

;;; In progress Object stack

(defun current-binary-object ()
  (first *in-progress-objects*))

(defun immediate-parent ()
  (second *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

