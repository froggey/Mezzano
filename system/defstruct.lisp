;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; DEFSTRUCT.

;;; This needs to be redone with CLOS in mind.

(in-package :sys.int)

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
        (print-object-specializer nil)
        (type nil)
        (named nil)
        (slot-offsets nil))
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
              ;; :named
              ((eql option :named)
               (setf named t))
              ;; (:type type)
              ((and (consp option)
                    (eql (first option) :type)
                    (= (length option) 2))
               (when type
                 (error "Multiple :TYPE options specified."))
               (setf type (second option))
               (unless (or (eql type 'list) (eql type 'vector)
                           (and (consp type)
                                (eql (first type) 'vector)
                                (= (length type) 2)))
                 (error "Invalid :TYPE option ~S.%" option)))
              ;; :slot-offsets
              ((eql option :slot-offsets)
               (setf slot-offsets t))
              (t (error "Unsupported DEFSTRUCT option ~S" option))))))
    (values name
            (if conc-namep
                (intern (string (or conc-name
                                    (concat-symbols name '-))))
                (make-symbol ""))
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
               (when (or (null type) named)
                 (concat-symbols name '-p)))
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
              (t copier-name))
            included-structure-name included-slot-descriptions
            print-object print-function print-object-specializer
            named type slot-offsets)))

;; Parses slot-description and produces:
;; (slot-name accessor-name initform type read-only)
(defun parse-defstruct-slot (conc-name slot)
  (if (symbolp slot)
      (make-struct-slot-definition slot (concat-symbols conc-name slot) nil 't nil)
      (destructuring-bind (slot-name &optional slot-initform &key (type 't) read-only)
          slot
        (make-struct-slot-definition slot-name (concat-symbols conc-name slot-name) slot-initform type read-only))))

(defun generate-simple-defstruct-constructor (struct-type name area)
  (generate-defstruct-constructor struct-type
                                  name
                                  (list* '&key (mapcar (lambda (slot)
                                                         (list (structure-slot-definition-name slot) (structure-slot-definition-initform slot)))
                                                       (structure-definition-slots struct-type)))
                                  area))

(defun generate-defstruct-constructor (struct-type name lambda-list area)
  (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore enable-keys allow-other-keys))
    ;; Pick out the slot names and compute the slots without a lambda variable
    (let* ((assigned-slots (append required
                                   (mapcar #'first optional)
                                   (remove 'nil (mapcar #'third optional))
                                   (if rest
                                       (list rest)
                                       '())
                                   (mapcar #'cadar keys)
                                   (remove 'nil (mapcar #'third keys))
                                   (mapcar #'first aux)))
           (default-slots (set-difference (mapcar #'structure-slot-definition-name (structure-definition-slots struct-type)) assigned-slots))
           (tmp (gensym)))
      `(defun ,name ,lambda-list
         ,@(loop
              for s in (structure-definition-slots struct-type)
              when (not (member (structure-slot-definition-name s) default-slots))
              collect `(check-type ,(structure-slot-definition-name s) ,(structure-slot-definition-type s)))
         (let ((,tmp (%make-struct ',struct-type)))
           ,@(loop
                for s in (structure-definition-slots struct-type)
                collect `(setf (%struct-slot ,tmp ',struct-type ',s)
                               ,(if (member (structure-slot-definition-name s) default-slots)
                                    (let ((val (gensym (string (structure-slot-definition-name s)))))
                                      `(let ((,val ,(structure-slot-definition-initform s)))
                                         (check-type ,val ,(structure-slot-definition-type s))
                                         ,val))
                                    (structure-slot-definition-name s))))
           ,tmp)))))

(defun generate-defstruct-list/vector-constructor (leader-name slots name lambda-list construction-function)
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
           (default-slots (set-difference (mapcar #'structure-slot-definition-name slots) assigned-slots)))
      `(defun ,name ,lambda-list
         ,@(loop
              for s in slots
              when (not (member (structure-slot-definition-name s) default-slots))
              collect `(check-type ,(structure-slot-definition-name s) ,(structure-slot-definition-type s)))
         (,construction-function
          ,@(when leader-name (list `',leader-name))
          ,@(loop
               for s in slots
               collect (if (member (structure-slot-definition-name s) default-slots)
                           (let ((val (gensym (string (structure-slot-definition-name s)))))
                             `(let ((,val ,(structure-slot-definition-initform s)))
                                (check-type ,val ,(structure-slot-definition-type s))
                                ,val))
                           (structure-slot-definition-name s))))))))

(defun generate-simple-defstruct-list-constructor (leader-name slots name area)
  (generate-defstruct-list/vector-constructor leader-name
                                              slots
                                              name
                                              (list* '&key
                                                     (mapcar (lambda (slot)
                                                               (list (structure-slot-definition-name slot) (structure-slot-definition-initform slot)))
                                                             slots))
                                              'list))

(defun generate-defstruct-list-constructor (leader-name slots name lambda-list area)
  (generate-defstruct-list/vector-constructor leader-name slots name lambda-list 'list))

(defun generate-simple-defstruct-vector-constructor (leader-name slots name area)
  (generate-defstruct-list/vector-constructor leader-name
                                              slots
                                              name
                                              (list* '&key
                                                     (mapcar (lambda (slot)
                                                               (list (structure-slot-definition-name slot) (structure-slot-definition-initform slot)))
                                                             slots))
                                              'vector))

(defun generate-defstruct-vector-constructor (leader-name slots name lambda-list area)
  (generate-defstruct-list/vector-constructor leader-name slots name lambda-list 'vector))

(defun compute-defstruct-slots (conc-name slot-descriptions included-structure included-slot-descriptions)
  (let ((included-slots (when included-structure
                          (mapcar (lambda (x)
                                    (list (structure-slot-definition-name x)
                                          (structure-slot-definition-initform x)
                                          :type (structure-slot-definition-type x)
                                          :read-only (structure-slot-definition-read-only x)))
                                  (structure-definition-slots included-structure)))))
    (dolist (is included-slot-descriptions)
      (let* ((slot-name (first is))
             (def (assoc slot-name included-slots)))
        (assert def () "Included slot definition ~S refers to unknown slot ~S."
                is slot-name)
        (when (rest is)
          (setf (second def) (second is)))))
    (mapcar (lambda (s)
              (parse-defstruct-slot conc-name s))
            (append included-slots
                    slot-descriptions))))

(defun generate-normal-defstruct (name slot-descriptions conc-name constructors predicate area copier
                                  included-structure-name included-slot-descriptions
                                  print-object print-function print-object-specializer slot-offsets)
  (let* ((included-structure (when included-structure-name
                               (get-structure-type included-structure-name)))
         (slots (compute-defstruct-slots conc-name
                                         slot-descriptions
                                         included-structure
                                         included-slot-descriptions))
         (struct-type (or (get-structure-type name nil)
                          (make-struct-definition name slots included-structure area))))
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
       ,@(loop
            for n from 0
            for s in slots
            collect `(progn
                       (declaim (inline ,(structure-slot-definition-accessor s)))
                       (defun ,(structure-slot-definition-accessor s) (object)
                         (%struct-slot object ',struct-type ',s))
                       ,@(unless (structure-slot-definition-read-only s)
                           (list `(declaim (inline (setf ,(structure-slot-definition-accessor s))))
                                 `(defun (setf ,(structure-slot-definition-accessor s)) (new-value object)
                                    (setf (%struct-slot object ',struct-type ',s) new-value))
                                 `(declaim (inline (cas ,(structure-slot-definition-accessor s))))
                                 `(defun (cas ,(structure-slot-definition-accessor s)) (old new object)
                                    (cas (%struct-slot object ',struct-type ',s) old new))))
                       ,@(when slot-offsets
                           `((defconstant ,(concat-symbols "+" (structure-slot-definition-accessor s) "+") ',n)))))
       ,@(loop
            for x in constructors
            collect (if (symbolp x)
                        (generate-simple-defstruct-constructor struct-type x area)
                        (generate-defstruct-constructor struct-type (first x) (second x) area)))
       ',name)))

(defun generate-list-defstruct (name slot-descriptions conc-name constructors predicate area copier
                                included-structure-name included-slot-descriptions
                                print-object print-function print-object-specializer
                                named slot-offsets)
  (when slot-offsets
    (error ":SLOT-OFFSETS with LIST structures not supported yet."))
  (when (or included-structure-name included-slot-descriptions)
    (error "Included LIST structures not supported yet."))
  (when (and predicate (not named))
    (error "Predicate with unnamed LIST structure."))
  (when (or print-object print-function print-object-specializer)
    (error "PRINT-OBJECT and PRINT-FUNCTION with LIST structure not supported yet."))
  (let ((slots (compute-defstruct-slots conc-name
                                        slot-descriptions
                                        nil
                                        included-slot-descriptions)))
    `(progn
       ,@(when predicate
           (list `(defun ,predicate (object)
                    (and (listp object) (eql (first object) ',name)))))
       ,@(when copier
           (list `(defun ,copier (object)
                    (copy-list object))))
       ,@(let ((n (if named 0 -1)))
           (mapcar (lambda (s)
                     (incf n)
                     `(progn
                        (defun ,(structure-slot-definition-accessor s) (object)
                          ,@(when (and named predicate)
                              (list `(check-type object (satisfies ,predicate))))
                          (nth ,n object))
                        ,@(unless (structure-slot-definition-read-only s)
                            (list `(defun (setf ,(structure-slot-definition-accessor s)) (new-value object)
                                     ,@(when (and named predicate)
                                         (list `(check-type object (satisfies ,predicate))))
                                     (setf (nth ,n object) (the ,(structure-slot-definition-type s) new-value)))))))
                   slots))
       ,@(mapcar (lambda (x)
                   (if (symbolp x)
                       (generate-simple-defstruct-list-constructor (when named name) slots x area)
                       (generate-defstruct-list-constructor (when named name) slots (first x) (second x) area)))
                 constructors)
       ',name)))

(defun generate-vector-defstruct (name slot-descriptions conc-name constructors predicate area copier
                                  included-structure-name included-slot-descriptions
                                  print-object print-function print-object-specializer
                                  named inner-type slot-offsets)
  (when slot-offsets
    (error ":SLOT-OFFSETS with VECTOR structures not supported yet."))
  (when (and named (not (eql inner-type 't)))
    (error "Named VECTOR struct with non-T type."))
  (when (or included-structure-name included-slot-descriptions)
    (error "Included VECTOR structures not supported yet."))
  (when (and predicate (not named))
    (error "Predicate with unnamed VECTOR structure."))
  (when (or print-object print-function print-object-specializer)
    (error "PRINT-OBJECT and PRINT-FUNCTION with VECTOR structure not supported yet."))
  (let ((slots (compute-defstruct-slots conc-name
                                        slot-descriptions
                                        nil
                                        included-slot-descriptions)))
    `(progn
       ,@(when predicate
           (list `(defun ,predicate (object)
                    (and (vectorp object)
                         (eql (aref object 0) ',name)))))
       ,@(when copier
           (list `(defun ,copier (object)
                    (make-array (length object)
                                :element-type ',inner-type
                                :initial-contents object))))
       ,@(let ((n (if named 0 -1)))
           (mapcar (lambda (s)
                     (incf n)
                     `(progn
                        (declaim (inline ,(structure-slot-definition-accessor s)))
                        (defun ,(structure-slot-definition-accessor s) (object)
                          ,@(when (and named predicate)
                              (list `(check-type object (satisfies ,predicate))))
                          (svref object ,n))
                        ,@(unless (structure-slot-definition-read-only s)
                            (list `(declaim (inline (setf ,(structure-slot-definition-accessor s))))
                                  `(defun (setf ,(structure-slot-definition-accessor s)) (new-value object)
                                     ,@(when (and named predicate)
                                         (list `(check-type object (satisfies ,predicate))))
                                     (setf (svref object ,n) (the ,(structure-slot-definition-type s) new-value)))
                                  `(declaim (inline (cas ,(structure-slot-definition-accessor s))))
                                  `(defun (cas ,(structure-slot-definition-accessor s)) (old new object)
                                     ,@(when (and named predicate)
                                         (list `(check-type object (satisfies ,predicate))))
                                     (cas (svref object ,n) (the ,(structure-slot-definition-type s) old) (the ,(structure-slot-definition-type s) new)))))))
                   slots))
       ,@(mapcar (lambda (x)
                   (if (symbolp x)
                       (generate-simple-defstruct-vector-constructor (when named name) slots x area)
                       (generate-defstruct-vector-constructor (when named name) slots (first x) (second x) area)))
                 constructors)
       ',name)))
)

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (multiple-value-bind (name conc-name constructors predicate area copier
                        included-structure-name included-slot-descriptions
                        print-object print-function print-object-specializer
                        named type slot-offsets)
      (parse-defstruct-options name-and-options)
    (let ((docstring nil)) ; TODO: do something with this.
      (when (stringp (first slot-descriptions))
        (setf docstring (pop slot-descriptions)))
      (cond
        ((null type)
         (generate-normal-defstruct name slot-descriptions conc-name constructors predicate area copier
                                    included-structure-name included-slot-descriptions
                                    print-object print-function print-object-specializer slot-offsets))
        ((eql type 'list)
         (generate-list-defstruct  name slot-descriptions conc-name constructors predicate area copier
                                   included-structure-name included-slot-descriptions
                                   print-object print-function print-object-specializer
                                   named slot-offsets))
        ((or (eql type 'vector)
             (and (listp type)
                  (eql (first type) 'vector)))
         (generate-vector-defstruct name slot-descriptions conc-name constructors predicate area copier
                                    included-structure-name included-slot-descriptions
                                    print-object print-function print-object-specializer
                                    named (if (listp type)
                                              (second type)
                                              't)
                                    slot-offsets))
        (t (error "Currently unsupported defstruct type ~S." type))))))
