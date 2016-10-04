;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(declaim (inline eql))
(defun eql (x y)
  (or (eq x y)
      (and (sys.int::%value-has-tag-p x sys.int::+tag-object+)
           (sys.int::%value-has-tag-p y sys.int::+tag-object+)
           (eq (sys.int::%object-tag x) (sys.int::%object-tag y))
           (<= sys.int::+first-numeric-object-tag+
               (sys.int::%object-tag x)
               sys.int::+last-numeric-object-tag+)
           (= x y))))

(declaim (inline sys.int::%value-has-tag-p))
(defun sys.int::%value-has-tag-p (value tag)
  (eq (sys.int::%tag-field value) tag))

(declaim (inline sys.int::%type-check))
(defun sys.int::%type-check (object object-tag expected-type)
  (unless (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
               (eq (sys.int::%object-tag object) object-tag))
    (sys.int::raise-type-error object expected-type)))

(declaim (inline lognot))
(defun lognot (integer)
  (logxor integer -1))

;; ARM makes it difficult to detect overflow when shifting left.
(declaim (inline %fixnum-left-shift))
(defun %fixnum-left-shift (integer count)
  (dotimes (i count)
     (setf integer (+ integer integer)))
  integer)

(declaim (inline sys.int::%simple-1d-array-p))
(defun sys.int::%simple-1d-array-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= (sys.int::%object-tag object) sys.int::+last-simple-1d-array-object-tag+)))

(declaim (inline simple-vector-p))
(defun simple-vector-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (eq (sys.int::%object-tag object) sys.int::+object-tag-array-t+)))

(declaim (inline sys.int::character-array-p))
(defun sys.int::character-array-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (let ((tag (sys.int::%object-tag object)))
         (or (eq tag sys.int::+object-tag-simple-string+)
             (eq tag sys.int::+object-tag-string+)))))

(declaim (inline arrayp))
(defun arrayp (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= (sys.int::%object-tag object) sys.int::+last-complex-array-object-tag+)))

(declaim (inline sys.int::complex-array-p))
(defun sys.int::complex-array-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= sys.int::+first-complex-array-object-tag+
           (sys.int::%object-tag object)
           sys.int::+last-complex-array-object-tag+)))

(declaim (inline sys.int::%bounds-check))
(defun sys.int::%bounds-check (object slot)
  (unless (sys.int::fixnump slot)
    (sys.int::raise-type-error slot 'fixnum))
  (unless (< slot (sys.int::%object-header-data object))
    (sys.int::raise-bounds-error object slot)))

(declaim (inline characterp))
(defun characterp (object)
  (sys.int::%value-has-tag-p object sys.int::+tag-character+))

(declaim (inline rem))
(defun rem (number divisor)
  (multiple-value-bind (quot rem)
      (truncate number divisor)
    (declare (ignore quot))
    rem))

(declaim (inline functionp))
(defun functionp (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= sys.int::+first-function-object-tag+
           (sys.int::%object-tag object)
           sys.int::+last-function-object-tag+)))

(declaim (inline sys.int::%object-of-type-p))
(defun sys.int::%object-of-type-p (object object-tag)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (eq (sys.int::%object-tag object) object-tag)))

(defun sys.int::%atomic-fixnum-add-object (object slot delta)
  (prog1
      (sys.int::%object-ref-t object slot)
    (incf (sys.int::%object-ref-t object slot) delta)))

(defun sys.int::%xchg-object (object slot new)
  (prog1
      (sys.int::%object-ref-t object slot)
    (setf (sys.int::%object-ref-t object slot) new)))

(defun sys.int::%cas-object (object slot old new)
  (let ((slot-value (sys.int::%object-ref-t object slot)))
    (values (cond ((eq slot-value old)
                   (setf (sys.int::%object-ref-t object slot) new)
                   t)
                  (t nil))
            slot-value)))

(defun sys.int::%dcas-object (object slot old-1 old-2 new-1 new-2)
  (let ((slot-value-1 (sys.int::%object-ref-t object slot))
        (slot-value-2 (sys.int::%object-ref-t object (1+ slot))))
    (values (cond ((and (eq slot-value-1 old-1)
                        (eq slot-value-2 old-2))
                   (setf (sys.int::%object-ref-t object slot) new-1
                         (sys.int::%object-ref-t object (1+ slot)) new-2)
                   t)
                  (t nil))
            slot-value-1
            slot-value-2)))

(defun sys.int::%copy-words (destination-address source-address count)
  (dotimes (i count)
    (setf (sys.int::memref-t destination-address i)
          (sys.int::memref-t source-address i))))

(defun sys.int::%fill-words (destination-address value count)
  (dotimes (i count)
    (setf (sys.int::memref-t destination-address i) value)))
