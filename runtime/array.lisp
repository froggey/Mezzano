;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for arrays.

(in-package :mezzano.runtime)

(defun sys.int::simple-character-array-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-simple-string+))

;;; Access to complex array slots.

(defun sys.int::%complex-array-storage (complex-array)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (sys.int::%object-ref-t complex-array sys.int::+complex-array-storage+))

(defun (setf sys.int::%complex-array-storage) (value complex-array)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (setf (sys.int::%object-ref-t complex-array sys.int::+complex-array-storage+) value))

(defun sys.int::%complex-array-fill-pointer (complex-array)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (sys.int::%object-ref-t complex-array sys.int::+complex-array-fill-pointer+))

(defun (setf sys.int::%complex-array-fill-pointer) (value complex-array)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (setf (sys.int::%object-ref-t complex-array sys.int::+complex-array-fill-pointer+) value))

(defun sys.int::%complex-array-info (complex-array)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (sys.int::%object-ref-t complex-array sys.int::+complex-array-info+))

(defun (setf sys.int::%complex-array-info) (value complex-array)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (setf (sys.int::%object-ref-t complex-array sys.int::+complex-array-info+) value))

(defun sys.int::%complex-array-dimension (complex-array axis)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (sys.int::%bounds-check complex-array axis)
  (sys.int::%object-ref-t complex-array (+ sys.int::+complex-array-axis-0+ axis)))

(defun (setf sys.int::%complex-array-dimension) (value complex-array axis)
  (when (not (sys.int::complex-array-p complex-array))
    (sys.int::type-error complex-array '(and array
                                             (not (simple-array * (*))))))
  (sys.int::%bounds-check complex-array axis)
  (setf (sys.int::%object-ref-t complex-array (+ sys.int::+complex-array-axis-0+ axis)) value))

;;; Simple vectors.

(declaim (inline svref (setf svref) sys.int::%svref (setf sys.int::%svref)))

(defun svref (simple-vector index)
  (sys.int::%type-check simple-vector sys.int::+object-tag-array-t+ 'simple-vector)
  (sys.int::%bounds-check simple-vector index)
  (sys.int::%object-ref-t simple-vector index))

(defun (setf svref) (value simple-vector index)
  (sys.int::%type-check simple-vector sys.int::+object-tag-array-t+ 'simple-vector)
  (sys.int::%bounds-check simple-vector index)
  (setf (sys.int::%object-ref-t simple-vector index) value))

(defun sys.int::%svref (simple-vector index)
  "Fast SVREF. No type check or bounds check.
Used to implement closures."
  (sys.int::%object-ref-t simple-vector index))

(defun (setf sys.int::%svref) (value simple-vector index)
  "Fast SVREF. No type check or bounds check.
Used to implement closures."
  (setf (sys.int::%object-ref-t simple-vector index) value))
