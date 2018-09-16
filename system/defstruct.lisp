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
        (slot-offsets nil)
        (sealed nil))
    (when (not (listp name-and-options))
      (setf name-and-options (list name-and-options)))
    (setf name (first name-and-options))
    (dolist (option (rest name-and-options))
      (when (not (listp option))
        (setf option (list option)))
      (cond
        ;; Standard options.
        ;;
        ;; (:constructor)
        ;; Empty option, ignored.
        ((and (eq :constructor (first option))
              (null (rest option))))
        ;; (:constructor name)
        ;; A constructor with an explicit name and default argument list.
        ((and (eq :constructor (first option))
              (cdr option)
              (null (cddr option)))
         (if (eq (second option) 'nil)
             ;; Disable the constructor.
             (setf suppress-constructors t)
             (push (second option) constructors)))
        ;; (:constructor name BOA-lambda-list)
        ((and (eq :constructor (first option))
              (cddr option)
              (null (cdddr option)))
         (if (eq (second option) 'nil)
             (setf suppress-constructors t)
             (push (rest option) constructors)))
        ;; (:predicate)
        ;; Empty option, ignored.
        ((and (consp option)
              (eq :predicate (first option))
              (null (cdr option))))
        ;; (:predicate name)
        ((and (eq :predicate (car option))
              (cdr option)
              (null (cddr option)))
         (cond ((eq (second option) 'nil)
                (setf predicate-name nil))
               ((null predicate-name)
                (error "Predicate option ~S conflicts with (:predicate nil) used earlier." option))
               ((eq predicate-name t)
                (setf predicate-name (second option)))
               (t (error "Multiple predicate options supplied."))))
        ;; (:copier)
        ;; Empty option, ignored.
        ((and (eq :copier (first option))
              (null (rest option))))
        ;; (:copier name)
        ((and (eq :copier (first option))
              (cdr option)
              (null (cddr option)))
         (cond ((eq (second option) 'nil)
                (setf copier-name nil))
               ((null copier-name)
                (error "Copier option ~S conflicts with (:copier nil) used earlier." option))
               ((eq copier-name t)
                (setf copier-name (second option)))
               (t (error "Multiple copier options supplied."))))
        ;; (:conc-name &optional name)
        ((and (eql (first option) :conc-name)
              (null (cddr option)))
         (if (second option)
             (setf conc-namep t
                   conc-name (second option))
             (setf conc-namep nil)))
        ((and (eql (first option) :include)
              (null (cdr option)))
         (error "Malformed :INCLUDE option ~S." option))
        ((and (consp option)
              (eql (first option) :include))
         (when included-structure-name
           (error "Multiple :INCLUDE options in DEFSTRUCT."))
         (setf included-structure-name (second option)
               included-slot-descriptions (cddr option)))
        ;; (:print-object) or (:print-function)
        ((and (member (first option) '(:print-object :print-function))
              (null (cdr option)))
         (when (or print-function print-object print-object-specializer)
           (error "Multiple :PRINT-OBJECT or :PRINT-FUNCTION options specified."))
         (setf print-object-specializer t))
        ;; (:print-object function-name)
        ((and (eql (first option) :print-object)
              (null (cddr option)))
         (when (or print-function print-object print-object-specializer)
           (error "Multiple :PRINT-OBJECT or :PRINT-FUNCTION options specified."))
         (setf print-object (second option)))
        ;; (:print-function function-name)
        ((and (eql (first option) :print-function)
              (null (cddr option)))
         (when (or print-function print-object print-object-specializer)
           (error "Multiple :PRINT-OBJECT or :PRINT-FUNCTION options specified."))
         (setf print-function (second option)))
        ;; (:named)
        ((and (eql (first option) :named)
              (null (rest option)))
         (setf named t))
        ;; (:type type)
        ((and (eql (first option) :type)
              (= (length option) 2))
         (when type
           (error "Multiple :TYPE options specified."))
         (setf type (second option))
         (unless (or (eql type 'list) (eql type 'vector)
                     (and (consp type)
                          (eql (first type) 'vector)
                          (= (length type) 2)))
           (error "Invalid :TYPE option ~S.%" option)))
        ;; Mezzano options
        ;;
        ;; (:area name)
        ;; Set the allocation area for the structure.
        ;; Instances will be allocated in the specified area.
        ((and (eq :area (first option))
              (cdr option)
              (null (cddr option)))
         (setf area (second option)))
        ;; (:slot-offsets)
        ;; Generate constants of the form +conc-name-slot-name+
        ;; that contain the byte offsets of the slots.
        ((and (eql (first option) :slot-offsets)
              (null (rest option)))
         (setf slot-offsets t))
        ;; (:sealed)
        ;; Prevent this structure from being :included in other structures.
        ((and (eql (first option) :sealed)
              (null (rest option)))
         (setf sealed t))
        (t (error "Unsupported DEFSTRUCT option ~S" option))))
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
            named type slot-offsets sealed)))

(defun compute-struct-slot-accessor-and-size (type)
  (cond ((subtypep type '(unsigned-byte 8))
         (values '%object-ref-unsigned-byte-8-unscaled 1))
        ((subtypep type '(unsigned-byte 16))
         (values '%object-ref-unsigned-byte-16-unscaled 2))
        ((subtypep type '(unsigned-byte 32))
         (values '%object-ref-unsigned-byte-32-unscaled 4))
        ((subtypep type '(unsigned-byte 64))
         (values '%object-ref-unsigned-byte-64-unscaled 8))
        ((subtypep type '(signed-byte 8))
         (values '%object-ref-signed-byte-8-unscaled 1))
        ((subtypep type '(signed-byte 16))
         (values '%object-ref-signed-byte-16-unscaled 2))
        ((subtypep type '(signed-byte 32))
         (values '%object-ref-signed-byte-32-unscaled 4))
        ((and (subtypep type '(signed-byte 64))
              (not (subtypep type 'fixnum)))
         (values '%object-ref-signed-byte-64-unscaled 8))
        ((subtypep type 'single-float)
         (values '%object-ref-single-float-unscaled 4))
        ((subtypep type 'double-float)
         (values '%object-ref-double-float-unscaled 8))
        (t
         (values nil 8))))

;; Parses slot-description and returns a struct slot definition
(defun parse-defstruct-slot (conc-name slot current-index)
  (destructuring-bind (name &optional initform &key (type 't) read-only fixed-vector align)
      (if (symbolp slot)
          (list slot)
          slot)
    (let ((accessor (concat-symbols conc-name name)))
      (check-type fixed-vector (or null (integer 0)))
      (check-type align (member nil 1 2 4 8 16))
      (multiple-value-bind (ref-fn element-size)
          (compute-struct-slot-accessor-and-size type)
        ;; Align current index.
        (let ((effective-alignment (or align element-size)))
          (when (and (not ref-fn) (< effective-alignment element-size))
            ;; Boxed slots must be at least naturally aligned.
            (setf effective-alignment element-size))
          (decf current-index 8) ; Object indices are +8 from the true start of the object, compute 16 byte alignments properly
          (incf current-index (1- effective-alignment))
          (setf current-index (- current-index (mod current-index effective-alignment)))
          (incf current-index 8)
          (values (make-struct-slot-definition name accessor initform type read-only
                                               (or ref-fn '%object-ref-t)
                                               (if ref-fn
                                                   current-index
                                                   ;; %object-ref-t takes a scaled index.
                                                   (truncate current-index 8))
                                               fixed-vector align)
                  current-index
                  (+ current-index (max 1 (* element-size (or fixed-vector 1))))
                  (not ref-fn)))))))

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
                collect (if (structure-slot-definition-fixed-vector s)
                            (let ((itr (gensym))
                                  (value (gensym)))
                              `(let ((,value ,(if (member (structure-slot-definition-name s) default-slots)
                                                  (let ((val (gensym (string (structure-slot-definition-name s)))))
                                                    `(let ((,val ,(structure-slot-definition-initform s)))
                                                       (check-type ,val ,(structure-slot-definition-type s))
                                                       ,val))
                                                  (structure-slot-definition-name s))))
                                 (dotimes (,itr ,(structure-slot-definition-fixed-vector s))
                                   (setf (%struct-vector-slot ,tmp ',struct-type ',(structure-slot-definition-name s) ,itr)
                                         ,value))))
                            `(setf (%struct-slot ,tmp ',struct-type ',(structure-slot-definition-name s))
                                   ,(if (member (structure-slot-definition-name s) default-slots)
                                        (let ((val (gensym (string (structure-slot-definition-name s)))))
                                          `(let ((,val ,(structure-slot-definition-initform s)))
                                             (check-type ,val ,(structure-slot-definition-type s))
                                             ,val))
                                        (structure-slot-definition-name s)))))
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
                                          :read-only (structure-slot-definition-read-only x)
                                          :fixed-vector (structure-slot-definition-fixed-vector x)
                                          :align (structure-slot-definition-align x)))
                                  (structure-definition-slots included-structure)))))
    (dolist (is included-slot-descriptions)
      (let* ((slot-name (first is))
             (def (assoc slot-name included-slots)))
        (assert def () "Included slot definition ~S refers to unknown slot ~S."
                is slot-name)
        (when (rest is)
          (setf (second def) (second is)))))
    (loop
       with current-index = 0
       with layout = (make-array 0 :element-type 'bit :adjustable t)
       for s in (append included-slots
                        slot-descriptions)
       collect (multiple-value-bind (def slot-index next-index boxedp)
                   (parse-defstruct-slot conc-name s current-index)
                 (adjust-array layout (truncate (+ next-index 7) 8) :initial-element 0)
                 (when boxedp
                   (dotimes (i (ceiling (- next-index slot-index) 8))
                     (setf (bit layout (+ (truncate slot-index 8) i)) 1)))
                 (setf current-index next-index)
                 def)
       into slot-defs
       finally (return (values slot-defs
                               (cond ((every (lambda (x) (eql x 1)) layout) t)
                                     ((every (lambda (x) (eql x 0)) layout) nil)
                                     (t layout))
                               (length layout))))))

(defun generate-normal-defstruct-slot-accessor (struct-type slot-definition)
  (let ((slot-name (structure-slot-definition-name slot-definition))
        (accessor-name (structure-slot-definition-accessor slot-definition)))
    `(progn
       (declaim (inline ,accessor-name))
       (defun ,accessor-name (object)
         (%struct-slot object ',struct-type ',slot-name))
       ,@(unless (structure-slot-definition-read-only slot-definition)
           (list `(declaim (inline (setf ,accessor-name)))
                 `(defun (setf ,accessor-name) (new-value object)
                    (setf (%struct-slot object ',struct-type ',slot-name) new-value))
                 `(declaim (inline (cas ,accessor-name)))
                 `(defun (cas ,accessor-name) (old new object)
                    (cas (%struct-slot object ',struct-type ',slot-name) old new)))))))

(defun generate-normal-defstruct-slot-vector-accessor (struct-type slot-definition)
  (let ((slot-name (structure-slot-definition-name slot-definition))
        (accessor-name (structure-slot-definition-accessor slot-definition)))
    `(progn
       (declaim (inline ,accessor-name))
       (defun ,accessor-name (object index)
         (%struct-vector-slot object ',struct-type ',slot-name index))
       ,@(unless (structure-slot-definition-read-only slot-definition)
           (list `(declaim (inline (setf ,accessor-name)))
                 `(defun (setf ,accessor-name) (new-value object index)
                    (setf (%struct-vector-slot object ',struct-type ',slot-name index) new-value))
                 `(declaim (inline (cas ,accessor-name)))
                 `(defun (cas ,accessor-name) (old new object index)
                    (cas (%struct-vector-slot object ',struct-type ',slot-name index) old new)))))))

(defun generate-normal-defstruct (name slot-descriptions conc-name constructors predicate area copier
                                  included-structure-name included-slot-descriptions
                                  print-object print-function print-object-specializer
                                  slot-offsets sealed)
  (let* ((included-structure (when included-structure-name
                               (get-structure-type included-structure-name))))
    (multiple-value-bind (slots layout size)
        (compute-defstruct-slots conc-name
                                         slot-descriptions
                                         included-structure
                                         included-slot-descriptions)
      (let ((struct-type (or (get-structure-type name nil)
                             (make-struct-definition name slots included-structure area size layout sealed))))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (%defstruct ',struct-type))
           ,@(when predicate
               (list `(declaim (inline ,predicate))
                     `(defun ,predicate (object)
                        (typep object ',name))))
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
                for s in slots
                collect (if (structure-slot-definition-fixed-vector s)
                            (generate-normal-defstruct-slot-vector-accessor struct-type s)
                            (generate-normal-defstruct-slot-accessor struct-type s))
                when slot-offsets
                collect `(defconstant ,(concat-symbols "+" (structure-slot-definition-accessor s) "+")
                           ',(if (eql (structure-slot-definition-ref-fn s) '%object-ref-t)
                                 (structure-slot-definition-index s)
                                 (truncate (structure-slot-definition-index s) 8))))
           ,@(loop
                for x in constructors
                collect (if (symbolp x)
                            (generate-simple-defstruct-constructor struct-type x area)
                            (generate-defstruct-constructor struct-type (first x) (second x) area)))
           ',name)))))

(defun generate-list-defstruct (name slot-descriptions conc-name constructors predicate area copier
                                included-structure-name included-slot-descriptions
                                print-object print-function print-object-specializer
                                named slot-offsets sealed)
  (when slot-offsets
    (error ":SLOT-OFFSETS with LIST structures not supported yet."))
  (when sealed
    (error "Cannot use :SEALED on typed structures."))
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
                                  named inner-type slot-offsets sealed)
  (when slot-offsets
    (error ":SLOT-OFFSETS with VECTOR structures not supported yet."))
  (when sealed
    (error "Cannot use :SEALED on typed structures."))
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
                        named type slot-offsets sealed)
      (parse-defstruct-options name-and-options)
    (let ((docstring nil)) ; TODO: do something with this.
      (when (stringp (first slot-descriptions))
        (setf docstring (pop slot-descriptions)))
      (cond
        ((null type)
         (generate-normal-defstruct name slot-descriptions conc-name constructors predicate area copier
                                    included-structure-name included-slot-descriptions
                                    print-object print-function print-object-specializer
                                    slot-offsets sealed))
        ((eql type 'list)
         (generate-list-defstruct  name slot-descriptions conc-name constructors predicate area copier
                                   included-structure-name included-slot-descriptions
                                   print-object print-function print-object-specializer
                                   named slot-offsets sealed))
        ((or (eql type 'vector)
             (and (listp type)
                  (eql (first type) 'vector)))
         (generate-vector-defstruct name slot-descriptions conc-name constructors predicate area copier
                                    included-structure-name included-slot-descriptions
                                    print-object print-function print-object-specializer
                                    named (if (listp type)
                                              (second type)
                                              't)
                                    slot-offsets sealed))
        (t (error "Currently unsupported defstruct type ~S." type))))))
