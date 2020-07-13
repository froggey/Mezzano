;;;; A collection of CLISP-style weak objects.

(in-package :mezzano.garbage-collection.weak-objects)

;;; TODO: The weak collections should be modified to prune their collections
;;; in a wait-free way.

;;; Weak References
;;;
;;; A weak reference holds a reference to an object without preventing it
;;; from being garbage-collected.

(defstruct (weak-reference
             (:constructor make-weak-reference (object &aux (pointer (make-weak-pointer object)))))
  pointer)

(defun weak-reference-value (weak-reference)
  "Returns the value referenced by WEAK-REFERENCE and true if it is still live, or
NIL and false if it is dead."
  (weak-pointer-value (weak-reference-pointer weak-reference)))

;;; Weak Lists
;;;
;;; A weak list is equivalent to a list of weak references.

(defstruct (weak-list
             (:constructor make-weak-list (list &aux (objects (mapcar #'make-weak-pointer list)))))
  objects)

(defun weak-list-list (weak-list)
  "Return the list of live objects contained by WEAK-LIST."
  ;; Loop over the list, collecting the result and trimming away any
  ;; dead pointers.
  (do* ((i (weak-list-objects weak-list) (cdr i))
        (prev nil)
        (result (cons nil nil))
        (tail result))
       ((endp i)
        (cdr result))
    (multiple-value-bind (value livep)
        (weak-pointer-value (car i))
      (cond (livep
             (setf tail (setf (cdr tail) (cons value nil)))
             (setf prev i))
            (prev
             (setf (cdr prev) (cdr i)))
            (t
             (setf (weak-list-objects weak-list) (cdr i)))))))

(defun (setf weak-list-list) (value weak-list)
  (setf (weak-list-objects weak-list)
        (mapcar #'make-weak-pointer value))
  value)

;;; Weak And Relations

(defstruct (weak-and-relation
             (:constructor make-weak-and-relation (list &aux (objects (mapcar #'make-weak-pointer list)))))
  objects)

(defun weak-and-relation-list (weak-and-relation)
  (loop
     for ptr in (weak-and-relation-objects weak-and-relation)
     collect
       (multiple-value-bind (value livep)
           (weak-pointer-value ptr)
         (when (not livep)
           (setf (weak-and-relation-objects weak-and-relation) '())
           (return '()))
         value)))

;;; Weak Or Relations

(defstruct (weak-or-relation
             (:constructor make-weak-or-relation
                           (list &aux (objects (let ((all (copy-list list)))
                                                 (mapcar (lambda (x) (make-weak-pointer x :value all)) all))))))
  objects)

(defun weak-or-relation-list (weak-or-relation)
  (let ((x (first (weak-or-relation-objects weak-or-relation))))
    (and x (weak-pointer-value x))))

;;; Weak Mappings

(defstruct (weak-mapping
             (:constructor make-weak-mapping (key value &aux (pointer (make-weak-pointer key :value value)))))
  pointer)

(defun weak-mapping-pair (weak-mapping)
  (weak-pointer-pair (weak-mapping-pointer weak-mapping)))

(defun weak-mapping-value (weak-mapping)
  (values (weak-pointer-value (weak-mapping-pointer weak-mapping))))

(defun (setf weak-mapping-value) (value weak-mapping)
  (multiple-value-bind (key livep)
      (weak-pointer-key (weak-mapping-pointer weak-mapping))
    (when livep
      (setf (weak-mapping-pointer weak-mapping)
            (make-weak-pointer key :value value))))
  value)

;;; Weak And Mappings

(defstruct (weak-and-mapping
             (:constructor make-weak-and-mapping (key value &aux (pointer (make-weak-pointer key :value value :weakness :key-and-value)))))
  pointer)

(defun weak-and-mapping-pair (weak-and-mapping)
  (weak-pointer-pair (weak-and-mapping-pointer weak-and-mapping)))

(defun weak-and-mapping-value (weak-and-mapping)
  (values (weak-pointer-value (weak-and-mapping-pointer weak-and-mapping))))

(defun (setf weak-and-mapping-value) (value weak-and-mapping)
  (multiple-value-bind (key livep)
      (weak-pointer-key (weak-and-mapping-pointer weak-and-mapping))
    (when livep
      (setf (weak-and-mapping-pointer weak-and-mapping)
            (make-weak-pointer key :value value :weakness :key-and-value))))
  value)

;;; Weak Or Mappings

(defstruct (weak-or-mapping
             (:constructor make-weak-or-mapping (key value &aux (pointer (make-weak-pointer key :value value :weakness :key-or-value)))))
  pointer)

(defun weak-or-mapping-pair (weak-or-mapping)
  (weak-pointer-pair (weak-or-mapping-pointer weak-or-mapping)))

(defun weak-or-mapping-value (weak-or-mapping)
  (values (weak-pointer-value (weak-or-mapping-pointer weak-or-mapping))))

(defun (setf weak-or-mapping-value) (value weak-or-mapping)
  (multiple-value-bind (key livep)
      (weak-pointer-key (weak-or-mapping-pointer weak-or-mapping))
    (when livep
      (setf (weak-or-mapping-pointer weak-or-mapping)
            (make-weak-pointer key :value value :weakness :key-or-value))))
  value)

;;; Weak Alists.

(defstruct (weak-alist
             (:constructor make-weak-alist
                           (&key (weakness :key) initial-contents
                                 &aux (objects (loop
                                                  for (key . value) in initial-contents
                                                  collect (make-weak-pointer key :value value :weakness weakness))))))
  weakness
  objects)

(defun weak-alist-contents (weak-alist)
  ;; Loop over the list, collecting the result and trimming away any
  ;; dead pointers.
  (do* ((i (weak-alist-objects weak-alist) (cdr i))
        (prev nil)
        (result (cons nil nil))
        (tail result))
       ((endp i)
        (cdr result))
    (multiple-value-bind (key value livep)
        (weak-pointer-pair (car i))
      (cond (livep
             (setf tail (setf (cdr tail) (cons (cons key value) nil)))
             (setf prev i))
            (prev
             (setf (cdr prev) (cdr i)))
            (t
             (setf (weak-alist-objects weak-alist) (cdr i)))))))

(defun (setf weak-alist-contents) (contents weak-alist)
  (setf (weak-alist-objects weak-alist)
        (loop
           for (key . value) in contents
           collect (make-weak-pointer key :value value :weakness (weak-alist-weakness weak-alist))))
  contents)

(defun weak-alist-assoc (item weak-alist &key test test-not key)
  (assoc item (weak-alist-contents weak-alist) :test test :test-not test-not :key key))

(defun weak-alist-rassoc (item weak-alist &key test test-not key)
  (rassoc item (weak-alist-contents weak-alist) :test test :test-not test-not :key key))

(defun weak-alist-value (item weak-alist &key test test-not default)
  (let ((cons (weak-alist-assoc item weak-alist :test test :test-not test-not)))
    (if cons
        (cdr cons)
        default)))

(defun (setf weak-alist-value) (value item weak-alist &key test test-not default)
  (declare (ignore default))
  (mezzano.internals::with-test-test-not (test test-not)
    ;; Loop over the list, trimming away any dead pointers.
    (do* ((i (weak-alist-objects weak-alist) (cdr i))
          (prev nil))
         ((endp i)
          ;; Item not present, add it.
          (push (make-weak-pointer item :value value :weakness (weak-alist-weakness weak-alist))
                (weak-alist-objects weak-alist))
          value)
      (multiple-value-bind (key livep)
          (weak-pointer-key (car i))
        (when (and livep
                   (funcall test item key))
          (setf (car i) (make-weak-pointer key :value value :weakness (weak-alist-weakness weak-alist)))
          (return value))
        (cond (livep
               (setf prev i))
              (prev
               (setf (cdr prev) (cdr i)))
              (t
               (setf (weak-alist-objects weak-alist) (cdr i))))))))
