;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for structures.

(in-package :mezzano.runtime)

(declaim (inline sys.int::structure-object-p
                 sys.int::%struct-slot
                 (setf sys.int::%struct-slot)
                 sys.int::%cas-struct-slot))

(defun sys.int::structure-object-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-structure-object+))

(defun sys.int::%struct-slot (object slot)
  (sys.int::%type-check object sys.int::+object-tag-structure-object+ 'structure-object)
  (sys.int::%bounds-check object slot)
  (sys.int::%object-ref-t object slot))

(defun (setf sys.int::%struct-slot) (value object slot)
  (sys.int::%type-check object sys.int::+object-tag-structure-object+ 'structure-object)
  (sys.int::%bounds-check object slot)
  (setf (sys.int::%object-ref-t object slot) value))

(defun sys.int::%cas-struct-slot (object slot old new)
  (sys.int::%type-check object sys.int::+object-tag-structure-object+ 'structure-object)
  (sys.int::%bounds-check object slot)
  (sys.int::%cas-object object slot old new))
