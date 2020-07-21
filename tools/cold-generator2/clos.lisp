;;;; CLOS bootstrapping.
;;;;
;;;; Generates the initial class hierarchy.

(defpackage :mezzano.cold-generator.clos
  (:use :cl)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:eval #:mezzano.cold-generator.eval)
                    (#:sys.int #:mezzano.internals))
  (:export #:configure-clos))

(in-package :mezzano.cold-generator.clos)

(defvar *primordial-class-table*)

(defun primordial-ensure-class (environment name &rest initargs &key direct-superclasses (metaclass 'standard-class) &allow-other-keys)
  (setf (gethash name *primordial-class-table*)
        (list* :name name
               :hash (1+ (hash-table-count *primordial-class-table*))
               :metaclass metaclass
               :direct-superclasses (cond ((and (endp direct-superclasses)
                                                (eql metaclass (env:translate-symbol environment 'mezzano.clos:standard-class)))
                                           (list (env:translate-symbol environment 'mezzano.clos:standard-object)))
                                          ((and (endp direct-superclasses)
                                                (eql metaclass (env:translate-symbol environment 'mezzano.clos:funcallable-standard-class)))
                                           (list (env:translate-symbol environment 'mezzano.clos:funcallable-standard-object)))
                                          (t
                                           direct-superclasses))
               initargs)))

(defun get-primordial-slot-definition (class-name slot-name)
  (let* ((class-def (gethash class-name *primordial-class-table*))
         (direct-slots (getf class-def :direct-slots))
         (slot (find slot-name direct-slots :key (lambda (direct-slot) (getf direct-slot :name)))))
    (or slot
        (dolist (c (getf class-def :direct-superclasses)
                 (error "Can't find slot ~S in class ~S." slot-name class-name))
          (let ((s (get-primordial-slot-definition c slot-name)))
            (when s
              (return s)))))))

(defun primordial-initialize-instance (environment class-name instance &rest initargs)
  (let* ((class-def (gethash class-name *primordial-class-table*))
         (class-layout (getf class-def :layout)))
    ;; Add default initargs to the list.
    (let ((default-initargs '()))
      (labels ((frob (c)
                 (loop
                    for (initarg form fn) in (getf (gethash c *primordial-class-table*) :direct-default-initargs)
                    do (when (and (not (member initarg initargs))
                                  (not (member initargs default-initargs)))
                         (push initarg default-initargs)
                         (push (funcall fn) default-initargs)))
                 (dolist (super (getf (gethash c *primordial-class-table*) :direct-superclasses))
                   (frob super))))
        (frob class-name))
      (setf initargs (append initargs (nreverse default-initargs))))
    (loop
       for slot-name across class-layout
       for slot-def = (get-primordial-slot-definition class-name slot-name)
       do
         (multiple-value-bind (init-key init-value foundp)
             (get-properties initargs (getf slot-def :initargs))
           (declare (ignore init-key))
           (when (or foundp (getf slot-def :initfunction))
             (setf (primordial-slot-value instance slot-name)
                   (cond (foundp
                          init-value)
                         ((getf slot-def :initfunction) ; use initfunction instead of initform to avoid nil issues
                          (multiple-value-bind (init-value deferred-init-value)
                              (eval:eval-toplevel (getf slot-def :initform) environment)
                            (when deferred-init-value
                              (error "Unable to initialize instance of ~S" class-name))
                            init-value)))))))))

(defun primordial-make-instance (environment class-name &rest initargs)
  (let* ((layout (getf (gethash class-name *primordial-class-table*) :instance-layout))
         (instance (env:allocate-cross-class-instance environment layout)))
    (apply #'primordial-initialize-instance environment class-name instance initargs)
    instance))

(defun primordial-slot-location-in-layout (layout slot-name)
  (loop
     with instance-slots = (sys.int::layout-instance-slots layout)
     for i below (length instance-slots) by 2
     do
       (when (eq (svref instance-slots i) slot-name)
         (return (svref instance-slots (1+ i))))
     finally
       (return nil)))

(defun primordial-slot-value (object slot-name)
  (env:cross-class-instance-slot-value object slot-name))

(defun (setf primordial-slot-value) (value object slot-name)
  (setf (env:cross-class-instance-slot-value object slot-name)
        value))

(defun primordial-slot-boundp (object slot-name)
  (env:cross-class-instance-slot-boundp object slot-name))

(defun primordial-class-of (object)
  (sys.int::layout-class (env:cross-class-instance-layout object)))

(defun primordial-class-metaclass (class-name)
  (getf (gethash class-name *primordial-class-table*) :metaclass))

(defun compute-primordial-slot-layout (environment class-name)
  (let* ((initargs (gethash class-name *primordial-class-table*))
         (layout (getf initargs :layout))
         (metaclass (primordial-class-metaclass class-name)))
    (when (not layout)
      (let ((direct-slots (getf initargs :direct-slots)))
        (setf layout (append (loop
                                for direct-slot in direct-slots
                                when (eql (getf direct-slot :allocation :instance) :instance)
                                collect (getf direct-slot :name))
                             (loop
                                for super in (getf initargs :direct-superclasses)
                                append (loop
                                          for slot-name across (compute-primordial-slot-layout environment super)
                                          collect slot-name))))
        ;; Duplicate or overridden slots are not implemented.
        ;;(format t "~S: ~:S~%" class-name layout)
        (assert (eql (length layout)
                     (length (remove-duplicates layout))))
        (setf layout (make-array (length layout) :initial-contents layout))
        (setf (getf (gethash class-name *primordial-class-table*) :layout) layout)
        ;; Built-in classes don't have layouts.
        (cond ((or (eql metaclass (env:translate-symbol environment 'mezzano.clos:standard-class))
                   (eql metaclass (env:translate-symbol environment 'mezzano.clos:funcallable-standard-class)))
               (let* ((funcallable-offset (if (eql metaclass (env:translate-symbol environment 'mezzano.clos::funcallable-standard-class))
                                              2 ; Skip the first two slots of funcallable instances, used for the function & entry point
                                              0))
                      (instance-slots (env:make-array environment (* (length layout) 2) :area :wired)))
                 (loop
                    for slot-index from (* funcallable-offset 8) by 8
                    for i from 0 by 2
                    for slot-name across layout
                    do (setf (aref instance-slots i) slot-name
                             (aref instance-slots (1+ i)) (mezzano.runtime::make-location mezzano.runtime::+location-type-t+ slot-index)))
                 (setf (getf (gethash class-name *primordial-class-table*) :instance-layout)
                       (sys.int::make-layout :class nil ; Fixed up later.
                                             :obsolete nil
                                             :heap-size (+ funcallable-offset (length layout))
                                             :heap-layout t
                                             :area (first (getf initargs :area))
                                             :instance-slots instance-slots))))
              ((eql metaclass (env:translate-symbol environment 'structure-class))
               ;; Hacks for structure-object, which has no associated structure-definition.
               (let* ((sdef (getf initargs :structure-definition))
                      (instance-slots (env:make-array environment (if sdef (* (length (env:structure-definition-slots sdef)) 2) 0))))
                 (loop
                    for slot in (if sdef (env:structure-definition-slots sdef) '())
                    for i from 0 by 2
                    do (setf (aref instance-slots i) (env:structure-slot-definition-name slot)
                             (aref instance-slots (1+ i)) (env:structure-slot-definition-location slot)))
                 (setf (getf (gethash class-name *primordial-class-table*) :instance-layout)
                       (sys.int::make-layout :class nil ; Fixed up later.
                                             :obsolete nil
                                             :heap-size (or (getf initargs :structure-heap-size) 0) ; hack for structure-object
                                             :heap-layout (getf initargs :structure-heap-layout) ; will be nil for structure-object, fine as it's zero-sized
                                             :area (first (getf initargs :area))
                                             :instance-slots instance-slots)))))))
    layout))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (do () (nil)
      (let ((minimal-elements
	     (remove-if
	      #'(lambda (class)
		  (member class remaining-constraints
			  :key #'cadr))
	      remaining-elements)))
	(when (null minimal-elements)
	  (if (null remaining-elements)
	      (return-from topological-sort result)
	      (error "Inconsistent precedence graph.")))
	(let ((choice (if (null (cdr minimal-elements))
			  (car minimal-elements)
			  (funcall tie-breaker
				   minimal-elements
				   result))))
	  (setq result (append result (list choice)))
	  (setq remaining-elements
		(remove choice remaining-elements))
	  (setq remaining-constraints
		(remove choice
			remaining-constraints
			:test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun primordial-tie-breaker-rule (environment minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (primordial-slot-value cpl-constituent (env:translate-symbol environment 'mezzano.clos::direct-superclasses)))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from primordial-tie-breaker-rule (car common))))))

(defun primordial-collect-superclasses (environment class)
  (list* class
         (union (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::direct-superclasses))
                (reduce #'union
                        (loop
                           for superclass in (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::direct-superclasses))
                           collect (primordial-collect-superclasses environment superclass))
                        :initial-value '()))))

(defun primordial-local-precedence-ordering (environment class)
  (mapcar #'list
          (cons class
                (butlast (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::direct-superclasses))))
          (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::direct-superclasses))))

(defun primordial-compute-class-precedence-list (environment class)
  (let ((classes-to-order (primordial-collect-superclasses environment class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                       (loop
                          for c in classes-to-order
                          appending (primordial-local-precedence-ordering environment c)))
                      (lambda (minimal-elements cpl-so-far)
                        (primordial-tie-breaker-rule environment minimal-elements cpl-so-far)))))

(defun primordial-compute-effective-slot-definition (environment class direct-slots)
  (let ((metaclass (primordial-slot-value (sys.int::layout-class (env:cross-class-instance-layout class))
                                          (env:translate-symbol environment 'mezzano.clos::name))))
    (cond ((eql metaclass 'structure-class)
           (let* ((class-name (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::name)))
                  (class-initargs (gethash class-name *primordial-class-table*))
                  (sdef (getf class-initargs :structure-definition))
                  (slot-def (find (primordial-slot-value (first direct-slots) (env:translate-symbol environment 'mezzano.clos::name))
                                  (env:structure-definition-slots sdef)
                                  :key #'env:structure-slot-definition-name)))
             (primordial-make-instance environment
                                       (env:translate-symbol environment 'mezzano.clos::structure-effective-slot-definition)
                                       :name (env:structure-slot-definition-name slot-def)
                                       :initform (env:structure-slot-definition-initform slot-def)
                                       :type (env:structure-slot-definition-type slot-def)
                                       :read-only (env:structure-slot-definition-read-only slot-def)
                                       :fixed-vector (env:structure-slot-definition-fixed-vector slot-def)
                                       :align (env:structure-slot-definition-align slot-def)
                                       :location (env:structure-slot-definition-location slot-def)
                                       :dcas-sibling (env:structure-slot-definition-dcas-sibling slot-def)
                                       :documentation (primordial-slot-value (first direct-slots) (env:translate-symbol environment 'mezzano.clos::documentation)))))
          (t
           (let ((initer (find-if-not #'null direct-slots
                                      :key (lambda (def) (primordial-slot-value def (env:translate-symbol environment 'mezzano.clos::initfunction))))))
             (primordial-make-instance environment
                                       (env:translate-symbol environment 'mezzano.clos::standard-effective-slot-definition)
                                       :name (primordial-slot-value (first direct-slots) (env:translate-symbol environment 'mezzano.clos::name))
                                       :initform (if initer
                                                     (primordial-slot-value initer (env:translate-symbol environment 'mezzano.clos::initform))
                                                     nil)
                                       :initfunction (if initer
                                                         (primordial-slot-value initer (env:translate-symbol environment 'mezzano.clos::initfunction))
                                                         nil)
                                       ;; TODO: Should make sure type is consistent across direct slots.
                                       :type (primordial-slot-value (first direct-slots) (env:translate-symbol environment 'mezzano.clos::type))
                                       :initargs (remove-duplicates
                                                  (loop
                                                     for direct-slot in direct-slots
                                                     append (primordial-slot-value direct-slot (env:translate-symbol environment 'mezzano.clos::initargs))))
                                       ;; TODO: Should make sure allocation is consistent across direct slots.
                                       :allocation (primordial-slot-value (first direct-slots) (env:translate-symbol environment 'mezzano.clos::allocation))
                                       :documentation (primordial-slot-value (first direct-slots) (env:translate-symbol environment 'mezzano.clos::documentation))))))))

(defun primordial-compute-slots (environment class)
  (let* ((all-slots (loop
                       for c in (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::precedence-list))
                       appending (primordial-slot-value c (env:translate-symbol environment 'mezzano.clos::direct-slots))))
         (all-names (remove-duplicates
                     (mapcar (lambda (def) (primordial-slot-value def (env:translate-symbol environment 'mezzano.clos::name))) all-slots)))
         (effective-slots (mapcar (lambda (name)
                                    (primordial-compute-effective-slot-definition
                                     environment
                                     class
                                     (remove name all-slots
                                             :key (lambda (def) (primordial-slot-value def (env:translate-symbol environment 'mezzano.clos::name)))
                                             :test-not #'eq)))
                                  all-names))
         (metaclass (primordial-slot-value (primordial-class-of class) (env:translate-symbol environment 'mezzano.clos::name))))
    (when (not (eql metaclass 'structure-class))
      (loop
         with next-instance-slot-index = (if (eql metaclass (env:translate-symbol environment 'mezzano.clos:funcallable-standard-class))
                                             16 ; Skip the first two slots of funcallable instances, used for the function & entry point
                                             0)
         for slot in effective-slots
         do
           (when (not (eql (primordial-slot-value slot (env:translate-symbol environment 'mezzano.clos::allocation)) :instance))
             (error "Non-instances slots not supported"))
           (setf (primordial-slot-value slot (env:translate-symbol environment 'mezzano.clos::location))
                 (mezzano.runtime::make-location mezzano.runtime::+location-type-t+ next-instance-slot-index))
           (incf next-instance-slot-index 8)))
    effective-slots))

(defun primordial-compute-default-initargs (environment class)
  (let ((default-initargs '()))
    (dolist (c (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::precedence-list)))
      (loop
         for (initarg form fn) in (primordial-slot-value c (env:translate-symbol environment 'mezzano.clos::direct-default-initargs))
         do (when (not (member initarg default-initargs :key #'first))
              (push (list initarg form fn) default-initargs))))
    (nreverse default-initargs)))

(defun finalize-primordial-class (environment class)
  (when (not (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::finalized-p)))
    (dolist (super (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::direct-superclasses)))
      (push class (primordial-slot-value super (env:translate-symbol environment 'mezzano.clos::direct-subclasses)))
      (finalize-primordial-class environment super))
    ;;(format t "Finalizing class ~S.~%" (primordial-slot-value class 'name))
    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::precedence-list))
          (primordial-compute-class-precedence-list environment class))
    ;;(format t "  Class-Precedence-List: ~:S~%" (mapcar (lambda (x) (primordial-slot-value x 'name)) (primordial-slot-value class 'precedence-list)))
    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::effective-slots))
          (primordial-compute-slots environment class))
    ;;(format t "  Slots: ~:S~%" (mapcar (lambda (x) (primordial-slot-value x 'name)) (primordial-slot-value class 'effective-slots)))
    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::default-initargs))
          (primordial-compute-default-initargs environment class))
    (cond ((eql (primordial-slot-value (primordial-class-of class) (env:translate-symbol environment 'mezzano.clos::name)) 'structure-class)
           (let* ((class-name (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::name)))
                  (class-initargs (gethash class-name *primordial-class-table*))
                  (sdef (getf class-initargs :structure-definition)))
             (cond ((eql class-name 'structure-object)
                    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::parent)) nil)
                    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::has-standard-constructor)) nil)
                    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::source-location)) nil))
                   (t
                    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::parent))
                          (second (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::precedence-list))))
                    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::has-standard-constructor))
                          (env:structure-definition-has-standard-constructor sdef))))))
          (t
           (let ((instance-slots (remove-if-not (lambda (x) (eql (primordial-slot-value x (env:translate-symbol environment 'mezzano.clos::allocation)) :instance))
                                                (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::effective-slots))))
                 (layout (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::slot-storage-layout))))
             ;; Check that the early layout and computed layout match up.
             (cond (layout
                    (assert (eql (length instance-slots) (/ (length (sys.int::layout-instance-slots layout)) 2)))
                    (loop
                       for slot-definition in instance-slots
                       for slot-name = (primordial-slot-value slot-definition (env:translate-symbol environment 'mezzano.clos::name))
                       for slot-location = (primordial-slot-location-in-layout layout slot-name)
                       do
                         (when (not (eql (primordial-slot-value slot-definition (env:translate-symbol environment 'mezzano.clos::location)) slot-location))
                           (error "Instance slots and computed early layout mismatch in class ~S."
                                  (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::name))))))
                   (t
                    (assert (endp instance-slots)))))))
    (setf (primordial-slot-value class (env:translate-symbol environment 'mezzano.clos::finalized-p)) t)))

(defun convert-primordial-direct-slot (environment metaclass direct-slot-definition)
  (apply #'primordial-make-instance
         environment
         (if (eql metaclass (env:translate-symbol environment 'structure-class))
             (env:translate-symbol environment 'mezzano.clos::structure-direct-slot-definition)
             (env:translate-symbol environment 'mezzano.clos:standard-direct-slot-definition))
         direct-slot-definition))

(defun convert-primordial-class (environment class-name)
  (destructuring-bind (&key real-class name hash metaclass direct-superclasses
                            direct-slots direct-default-initargs layout
                            instance-layout area sealed
                            structure-heap-layout structure-heap-size
                            structure-definition
                            source-location)
      (gethash class-name *primordial-class-table*)
    (declare (ignore layout
                     structure-heap-layout structure-heap-size
                     structure-definition
                     source-location))
    (check-type area (or null (cons symbol null)))
    (check-type sealed (or null (cons boolean null)))
#|
    (format t "Converting class ~S~%" name)
    (format t "  Metaclass: ~S~%" metaclass)
    (format t "  Direct-Superclasses: ~:S~%" direct-superclasses)
    (format t "  Direct-Slots: ~:S~%" direct-slots)
    (format t "  Direct-Default-Initargs: ~:S~%" direct-default-initargs)
    ;(format t "  Layout: ~:S~%" layout)
|#
    (let ((converted-direct-slots (loop
                                     for direct-slot in direct-slots
                                     collect (convert-primordial-direct-slot environment metaclass direct-slot))))
      (primordial-initialize-instance environment
                                      metaclass
                                      real-class
                                      :name name
                                      :direct-superclasses (mapcar (lambda (name)
                                                                     (getf (gethash name *primordial-class-table*) :real-class))
                                                                   direct-superclasses))
      (setf (primordial-slot-value real-class (env:translate-symbol environment 'mezzano.clos::direct-slots)) converted-direct-slots
            (primordial-slot-value real-class (env:translate-symbol environment 'mezzano.clos::direct-default-initargs)) direct-default-initargs
            (primordial-slot-value real-class (env:translate-symbol environment 'mezzano.clos::slot-storage-layout)) instance-layout
            (primordial-slot-value real-class (env:translate-symbol environment 'mezzano.clos::allocation-area)) (first area)
            (primordial-slot-value real-class (env:translate-symbol environment 'mezzano.clos::sealed)) (first sealed)
            (primordial-slot-value real-class (env:translate-symbol environment 'mezzano.clos::hash)) hash))))

(defun convert-sdef (environment sdef)
  (declare (ignore environment))
  (let ((name (env:structure-definition-name sdef)))
    (setf (gethash name *primordial-class-table*)
          (list :name name
                :hash (1+ (hash-table-count *primordial-class-table*))
                :metaclass 'structure-class
                :area (list (env:structure-definition-area sdef))
                :sealed (list (env:structure-definition-sealed sdef))
                :structure-heap-layout (env:structure-definition-layout sdef)
                :structure-heap-size (env:structure-definition-size sdef)
                :structure-definition sdef
                :direct-superclasses (list
                                      (cond ((env:structure-definition-parent sdef)
                                             (env:structure-definition-name (env:structure-definition-parent sdef)))
                                            (t
                                             'structure-object)))
                :direct-slots (loop
                                 for slot in (remove-if (lambda (slot)
                                                          (and (env:structure-definition-parent sdef)
                                                               (find (env:structure-slot-definition-name slot)
                                                                     (env:structure-definition-slots (env:structure-definition-parent sdef))
                                                                     :key 'env:structure-slot-definition-name)))
                                                        (env:structure-definition-slots sdef))
                                 collect (list :name (env:structure-slot-definition-name slot)
                                               :type (env:structure-slot-definition-type slot)
                                               :read-only (env:structure-slot-definition-read-only slot)
                                               :structure-slot-location (env:structure-slot-definition-location slot)
                                               ;; FIXME: Need to include an initfunction
                                               :initform (env:structure-slot-definition-initform slot)
                                               :fixed-vector (env:structure-slot-definition-fixed-vector slot)
                                               :align (env:structure-slot-definition-align slot)
                                               :dcas-sibling (env:structure-slot-definition-dcas-sibling slot)
                                               :documentation (env:structure-slot-definition-documentation slot)))))))

(defun configure-clos (environment load-source-file)
  (let* ((*primordial-class-table* (make-hash-table))
         (eval:*ensure-class-handler* #'primordial-ensure-class)
         (forms (funcall load-source-file ; hack, need to sort out how files are compiled
                         environment
                         "tools/cold-generator2/class-definitions.lisp"
                         :package :mezzano.clos
                         :eval t)))
    (assert (endp forms) () "Failed to load class definitions")
    ;; Register structure definitions as primordial classes too.
    (env:do-all-environment-structs (sdef environment)
      (convert-sdef environment sdef))
    ;; Compute slot layouts for each class.
    (maphash (lambda (name def)
               (declare (ignore def))
               (compute-primordial-slot-layout environment name))
             *primordial-class-table*)
    ;; Now that the layouts are known, the real class instances can be created.
    (maphash (lambda (name def)
               (declare (ignore def))
               (let* ((mc-name (primordial-class-metaclass name))
                      (mc (gethash mc-name *primordial-class-table*))
                      (mc-layout (getf mc :instance-layout)))
                 (setf (getf (gethash name *primordial-class-table*) :real-class)
                       (env:allocate-cross-class-instance environment mc-layout))))
             *primordial-class-table*)
    ;; Instance layouts can now be properly associated with their classes.
    (maphash (lambda (name def)
               (declare (ignore name))
               (let ((layout (getf def :instance-layout)))
                 (when layout
                   (setf (sys.int::layout-class layout)
                         (getf def :real-class)))))
             *primordial-class-table*)
    ;; Start defining real classes.
    ;; Initialize every real class object, initializing it and preparing for
    ;; finalization.
    (maphash (lambda (name def)
               (declare (ignore def))
               (convert-primordial-class environment name))
             *primordial-class-table*)
    ;; Now all classes have been initialized to the point where they
    ;; can be finalized.
    (maphash (lambda (name def)
               (declare (ignore name))
               (finalize-primordial-class environment (getf def :real-class)))
             *primordial-class-table*)
    ;; Add classes to the environment's class table.
    (maphash (lambda (name def)
               (setf (env:find-environment-class environment name) (getf def :real-class)))
             *primordial-class-table*)
    ;; Save the initial class table.
    (let ((class-table (env:make-array environment 0 :adjustable t :fill-pointer 0)))
      (maphash (lambda (name def)
                 (vector-push-extend (cons name (getf def :real-class)) class-table))
               *primordial-class-table*)
      (setf (env:cross-symbol-value environment 'mezzano.clos::*initial-class-table*) class-table))
    (setf (env:cross-symbol-value environment 'mezzano.runtime::*structure-class-layout*)
          (primordial-slot-value (env:find-environment-class environment 'structure-class)
                                 (env:translate-symbol environment 'mezzano.clos::slot-storage-layout)))
    (let ((unbound (env:make-structure environment 'mezzano.runtime::unbound-value :tag :unbound-slot)))
      (setf (env:cross-symbol-value environment 'mezzano.clos::*secret-unbound-value*)
            unbound)
      (env:add-special environment :unbound-slot-value unbound))
    (setf (env:cross-symbol-value environment 'mezzano.clos::*next-class-hash-value*)
          (1+ (hash-table-count *primordial-class-table*)))))
