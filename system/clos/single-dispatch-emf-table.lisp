;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Faster hash table for doing single-dispatch class to effective method lookup.

(in-package :mezzano.clos)

;; Must be a power of two.
(defvar *default-single-dispatch-emf-table-size* 16)

(defstruct (single-dispatch-emf-table
             (:constructor make-single-dispatch-emf-table))
  (update-lock (mezzano.supervisor:make-mutex "1-dispatch EMF table update lock"))
  (table nil)
  (count 0))

(defun single-dispatch-emf-entry (emf-table class)
  (declare (optimize speed (safety 0) (debug 1))
           (type single-dispatch-emf-table emf-table))
  (let ((storage (single-dispatch-emf-table-table emf-table)))
    (cond ((consp storage)
           ;; Single entry.
           (if (eql (car storage) class)
               (cdr storage)
               nil))
          ((not storage)
           ;; No entries.
           nil)
          (t
           (locally
               (declare (type simple-vector storage))
             ;; Full hash table.
             (do* ((hash (class-hash class))
                   (size (length storage))
                   (mask (1- size))
                   ;; This hash implementation is inspired by the Python dict implementation.
                   (slot (logand hash #xFFFFFFFF)
                         (logand #xFFFFFFFF
                                 (the fixnum
                                      (+ (the fixnum
                                              (+ (the fixnum
                                                      (* slot 5))
                                                 perturb))
                                         1))))
                   (perturb hash (ash perturb -5)))
                  (nil)
               (declare (type fixnum hash size mask slot perturb))
               (let* ((offset (logand slot mask))
                      (slot (aref storage offset)))
                 (declare (type fixnum offset))
                 (when (eq slot nil)
                   ;; Unbound value marks the end of this run.
                   (return nil))
                 (when (and (not (eq slot t))
                            (eq class (car (the cons slot))))
                   (return (cdr (the cons slot)))))))))))

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
    (cond ((null (single-dispatch-emf-table-table emf-table))
           (when (not (eql value nil))
             (setf (single-dispatch-emf-table-table emf-table) (cons class value))
             (setf (single-dispatch-emf-table-count emf-table) 1)))
          ((consp (single-dispatch-emf-table-table emf-table))
           (cond ((eql (car (single-dispatch-emf-table-table emf-table)) class)
                  (cond (value
                         (setf (cdr (single-dispatch-emf-table-table emf-table)) value))
                        (t
                         (setf (single-dispatch-emf-table-table emf-table) nil)
                         (setf (single-dispatch-emf-table-count emf-table) 0))))
                 (value
                  ;; Promote to full hash-table
                  (let* ((storage (make-array *default-single-dispatch-emf-table-size* :initial-element nil))
                         (existing-entry (single-dispatch-emf-table-table emf-table))
                         (existing-class (car existing-entry))
                         (existing-value (cdr existing-entry)))
                    (multiple-value-bind (existing-slot slot-offset)
                        (get-single-dispatch-emf-table-slot-offset storage existing-class)
                      (declare (ignore existing-slot))
                      (setf (svref storage slot-offset) existing-entry))
                    (multiple-value-bind (existing-slot slot-offset)
                        (get-single-dispatch-emf-table-slot-offset storage class)
                      (declare (ignore existing-slot))
                      (setf (svref storage slot-offset) (cons class value)))
                    (setf (single-dispatch-emf-table-table emf-table) storage)
                    (incf (single-dispatch-emf-table-count emf-table))))))
          (t
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
                (incf (single-dispatch-emf-table-count emf-table))))))))
  value)

(defun map-single-dispatch-emf-table (fn emf-table)
  (let ((table (single-dispatch-emf-table-table emf-table)))
    (cond ((not table)
           nil)
          ((consp table)
           (funcall fn (car table) (cdr table)))
          (t
           (dotimes (i (length table))
             (let ((slot (svref table i)))
               (when (consp slot)
                 (funcall fn (car slot) (cdr slot)))))))))

(defun clear-single-dispatch-emf-table (emf-table)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (setf (single-dispatch-emf-table-table emf-table) nil
          (single-dispatch-emf-table-count emf-table) 0)))
