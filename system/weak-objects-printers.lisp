;;;; PRINT-OBJECT methods for the weak objects.
;;;;
;;;; Separate from weak-objects.lisp due to bootstrapping constraints.

(in-package :mezzano.garbage-collection.weak-objects)

(defmethod print-object ((instance weak-reference) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S" (weak-reference-value instance))))

(defmethod print-object ((instance weak-list) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~:S" (weak-list-list instance))))

(defmethod print-object ((instance weak-and-relation) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~:S" (weak-and-relation-list instance))))

(defmethod print-object ((instance weak-or-relation) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~:S" (weak-or-relation-list instance))))

(defmethod print-object ((instance weak-mapping) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (multiple-value-bind (key value livep)
        (weak-mapping-pair instance)
      (if livep
          (format stream ":Key ~S :Value ~S" key value)
          (format stream "<dead>")))))

(defmethod print-object ((instance weak-and-mapping) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (multiple-value-bind (key value livep)
        (weak-and-mapping-pair instance)
      (if livep
          (format stream ":Key ~S :Value ~S" key value)
          (format stream "<dead>")))))

(defmethod print-object ((instance weak-or-mapping) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (multiple-value-bind (key value livep)
        (weak-or-mapping-pair instance)
      (if livep
          (format stream ":Key ~S :Value ~S" key value)
          (format stream "<dead>")))))

(defmethod print-object ((instance weak-alist) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~:S" (weak-alist-contents instance))))
