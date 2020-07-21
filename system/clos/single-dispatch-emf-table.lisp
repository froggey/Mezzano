;;;; Faster hash table for doing single-dispatch class to effective method lookup.

(in-package :mezzano.clos)

(defstruct (single-dispatch-emf-table
             (:constructor %make-single-dispatch-emf-table (update-lock)))
  update-lock
  (table (make-fast-class-hash-table)))

(defun make-single-dispatch-emf-table (generic-function)
  (%make-single-dispatch-emf-table (mezzano.supervisor:make-mutex `(single-dispatch-emf-cache ,generic-function))))

(defun single-dispatch-emf-table-count (emf-table)
  (fast-class-hash-table-count
   (single-dispatch-emf-table-table emf-table)))

(defun single-dispatch-emf-entry (emf-table class)
  (fast-class-hash-table-entry
   (single-dispatch-emf-table-table emf-table)
   class))

(defun (setf single-dispatch-emf-entry) (value emf-table class)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (setf (fast-class-hash-table-entry
           (single-dispatch-emf-table-table emf-table)
           class)
          value)))

(defun clear-single-dispatch-emf-table (emf-table)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (clear-fast-class-hash-table (single-dispatch-emf-table-table emf-table))))
