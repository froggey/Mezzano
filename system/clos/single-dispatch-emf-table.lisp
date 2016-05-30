;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Faster hash table for doing single-dispatch class to effective method lookup.

(in-package :system.closette)

;; Must be a power of two.
(defvar *default-single-dispatch-emf-table-size* 16)

(defstruct (single-dispatch-emf-table
             (:constructor make-single-dispatch-emf-table))
  (update-lock (mezzano.supervisor:make-mutex "1-dispatch EMF table update lock"))
  (table (make-array *default-single-dispatch-emf-table-size* :initial-element nil))
  (count 0))

(defun single-dispatch-emf-entry (emf-table class)
  (do* ((hash (class-hash class))
        (storage (single-dispatch-emf-table-table emf-table))
        (size (sys.int::simple-vector-length storage))
        ;; This hash implementation is inspired by the Python dict implementation.
        (slot (logand hash #xFFFFFFFF) (logand #xFFFFFFFF (+ (* slot 5) perturb 1)))
        (perturb hash (ash perturb -5)))
       (nil)
    (let* ((offset (rem slot size))
           (slot (svref storage offset)))
      (when (eq slot nil)
        ;; Unbound value marks the end of this run.
        (return nil))
      (when (and (not (eq slot t))
                 (eq class (car slot)))
        (return (cdr slot))))))

(defun get-single-dispatch-emf-table-slot-offset (storage class)
  (do* ((hash (class-hash class))
        (size (sys.int::simple-vector-length storage))
        (free-slot nil)
        ;; This hash implementation is inspired by the Python dict implementation.
        (slot (logand hash #xFFFFFFFF) (logand #xFFFFFFFF (+ (* slot 5) perturb 1)))
        (perturb hash (ash perturb -5)))
       (nil)
    (let* ((offset (rem slot size))
           (slot (svref storage offset)))
      (when (and (not free-slot)
                 (or (eq slot nil)
                     (eq slot t)))
        (setf free-slot offset))
      (when (eq slot nil)
        ;; Unbound value marks the end of this run.
        (return (values nil free-slot)))
      (when (and (not (eq slot t))
                 (eq class (car slot)))
        (return (values slot offset))))))

(defun (setf single-dispatch-emf-entry) (value emf-table class)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (multiple-value-bind (existing-slot slot-offset)
        (get-single-dispatch-emf-table-slot-offset (single-dispatch-emf-table-table emf-table) class)
      (cond
        ((eql value nil)
         ;; Removing a value.
         (when existing-slot
           (decf (single-dispatch-emf-table-count emf-table))
           ;; Replace with a tombstone marker
           (setf (svref (single-dispatch-emf-table-table emf-table) slot-offset) t)))
        (existing-slot
         ;; Updating an existing slot.
         (setf (cdr existing-slot) value))
        ((eql (1+ (single-dispatch-emf-table-count emf-table))
              (length (single-dispatch-emf-table-table emf-table)))
         ;; This would fill the table. Expand it.
         (let* ((old-table (single-dispatch-emf-table-table emf-table))
                (new-table (make-array (* (length old-table) 2) :initial-element nil)))
           (dotimes (i (length old-table))
             (let ((slot (svref old-table i)))
               (when (consp slot)
                 (multiple-value-bind (existing-slot slot-offset)
                     (get-single-dispatch-emf-table-slot-offset new-table (car slot))
                   (assert (not existing-slot))
                   (setf (svref new-table slot-offset) slot)))))
           ;; And the new entry.
           (multiple-value-bind (existing-slot slot-offset)
               (get-single-dispatch-emf-table-slot-offset new-table class)
             (assert (not existing-slot))
             (setf (svref new-table slot-offset) (cons class value)))
           (incf (single-dispatch-emf-table-count emf-table))
           ;; Switch to new table.
           (setf (single-dispatch-emf-table-table emf-table) new-table)))
        (t ;; Adding a new entry.
         (setf (svref (single-dispatch-emf-table-table emf-table) slot-offset) (cons class value))
         (incf (single-dispatch-emf-table-count emf-table))))))
  value)

(defun map-single-dispatch-emf-table (fn emf-table)
  (let ((table (single-dispatch-emf-table-table emf-table)))
    (dotimes (i (length table))
      (let ((slot (svref table i)))
        (when (consp slot)
          (funcall fn (car slot) (cdr slot)))))))

(defun clear-single-dispatch-emf-table (emf-table)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (setf (single-dispatch-emf-table-table emf-table) (make-array *default-single-dispatch-emf-table-size* :initial-element nil)
          (single-dispatch-emf-table-count emf-table) 0)))
