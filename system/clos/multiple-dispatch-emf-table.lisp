;;;; This cache handles the most general case where there are multiple
;;;; relevant arguments and EQL specializers are involved.

(in-package :mezzano.clos)

;;; TODO: If an argument isn't relevant at a particular level, then
;;; avoid including that level in the cache.

(defstruct (emf-cache-level
             (:constructor %make-emf-cache-level))
  eql-specializers
  class-specializers)

(defstruct (emf-cache
             (:constructor %make-emf-cache))
  generic-function
  argument-count
  reordering-table
  top-level
  lock)

(defun make-emf-cache (generic-function)
  (let ((cache (%make-emf-cache
                :lock (mezzano.supervisor:make-mutex `(emf-cache ,generic-function))
                :generic-function generic-function)))
    (clear-emf-cache cache)
    cache))

(defun make-emf-cache-level (generic-function partial-specializers)
  (let* ((depth (length partial-specializers))
         (eql-methods (remove-if-not
                       (lambda (method)
                         (has-eql-specializer-in-reordered-position-p
                          generic-function method depth))
                       (compute-applicable-methods-partial generic-function partial-specializers)))
         (eql-specializers (remove-duplicates
                            (mapcar (lambda (method)
                                      (reordered-method-specializer generic-function method depth))
                                    eql-methods))))
    (%make-emf-cache-level
     :eql-specializers (mezzano.garbage-collection.weak-objects:make-weak-alist
                        :initial-contents (loop
                                             for eql-spec in eql-specializers
                                             collect (cons (eql-specializer-object eql-spec) nil)))
     :class-specializers (make-fast-class-hash-table))))

;; TODO: If there is a custom method on COMPUTE-APPLICABLE-METHODS then
;; this scheme should not be used and the class-cache-only implementation
;; should be used instead.
(defun insert-into-emf-cache (cache arguments value)
  ;; ### I'm a little worried about the locking here. This calls back into
  ;; the MOP machinery to update the cache... Is it possible to reenter?
  (mezzano.supervisor:with-mutex ((emf-cache-lock cache))
    (let* ((gf (emf-cache-generic-function cache))
           (req-args (required-portion gf arguments))
           (n-args (length req-args))
           (level (emf-cache-top-level cache))
           (partial-specializers '()))
      (dotimes (i (1- n-args))
        (let* ((arg (reordered-argument gf arguments i))
               (level-eql-specs (emf-cache-level-eql-specializers level))
               (eql-entry (mezzano.garbage-collection.weak-objects:weak-alist-assoc
                           arg level-eql-specs)))
          (cond (eql-entry
                 ;; This value is used as an EQL specializer.
                 (push-on-end (intern-eql-specializer arg) partial-specializers)
                 (cond ((cdr eql-entry)
                        (setf level (cdr eql-entry)))
                       (t
                        (setf level (make-emf-cache-level gf partial-specializers))
                        (setf (mezzano.garbage-collection.weak-objects:weak-alist-value
                               arg level-eql-specs)
                              level))))
                (t
                 ;; Just a regular class specializer.
                 (let ((class (class-of arg)))
                   (push-on-end class partial-specializers)
                   (when (not (fast-class-hash-table-entry (emf-cache-level-class-specializers level) class))
                     (setf (fast-class-hash-table-entry (emf-cache-level-class-specializers level) class)
                           (make-emf-cache-level gf partial-specializers)))
                   (setf level (fast-class-hash-table-entry (emf-cache-level-class-specializers level) class)))))))
      ;; Last level, where the entry actually is.
      (let* ((arg (reordered-argument gf arguments (1- n-args)))
             (level-eql-specs (emf-cache-level-eql-specializers level))
             (eql-entry (mezzano.garbage-collection.weak-objects:weak-alist-assoc
                         arg level-eql-specs)))
        (cond (eql-entry
               ;; This value is used as an EQL specializer.
               (setf (mezzano.garbage-collection.weak-objects:weak-alist-value
                      arg level-eql-specs)
                     value))
              (t
               ;; Just a regular class specializer.
               (setf (fast-class-hash-table-entry (emf-cache-level-class-specializers level) (class-of arg)) value)))))))

(defun emf-cache-lookup (cache arguments)
  (mezzano.supervisor:with-mutex ((emf-cache-lock cache))
    (let* ((gf (emf-cache-generic-function cache))
           (n-args (emf-cache-argument-count cache))
           (reordering-table (emf-cache-reordering-table cache))
           (level (emf-cache-top-level cache)))
      (flet ((reorder-argument (index)
               (nth (cond (reordering-table
                           (svref reordering-table index))
                          (t index))
                    arguments)))
        (dotimes (i n-args
                  ;; On the final iteration LEVEL will contain the actual entry value.
                  level)
          (let* ((arg (reorder-argument i))
                 (eql-entry (mezzano.garbage-collection.weak-objects:weak-alist-assoc
                             arg (emf-cache-level-eql-specializers level))))
            (cond (eql-entry
                   ;; This value is used as an EQL specializer.
                   (setf level (cdr eql-entry)))
                  (t
                   ;; Just a regular class specializer.
                   (setf level (fast-class-hash-table-entry (emf-cache-level-class-specializers level) (class-of arg)))))
            (when (not level)
              ;; This level is not present (or contains NIL), stop here.
              (return nil))))))))

(defun clear-emf-cache (cache)
  (let* ((gf (emf-cache-generic-function cache))
         (n-args (length (safe-generic-function-relevant-arguments gf)))
         (reordering-table (argument-reordering-table gf)))
    (mezzano.supervisor:with-mutex ((emf-cache-lock cache))
      (setf (emf-cache-argument-count cache) n-args
            (emf-cache-reordering-table cache) reordering-table)
      (setf (emf-cache-top-level cache) (make-emf-cache-level (emf-cache-generic-function cache) '())))))
