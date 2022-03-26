;;; The base definitions for protocol classes and functions for
;;; spatial trees.

(in-package "SPATIAL-TREES-IMPL")

(defclass spatial-tree ()
  ((root-node :initarg :root-node :accessor root-node)
   (rectfun :initarg :rectfun :reader rectfun)
   (max-per-node :initform 7 :reader max-per-node)
   (min-per-node :initform 3 :reader min-per-node)))
(defmethod print-object ((o spatial-tree) s)
  (print-unreadable-object (o s :type t)
    (format s "~1I~_~W" (root-node o))))

(defclass spatial-tree-node ()
  ((mbr :initarg :mbr)
   (children :initarg :children :accessor children)
   (parent :initarg :parent :accessor parent)))
(defmethod print-object ((o spatial-tree-node) s)
  (print-unreadable-object (o s :type t)
    (when (slot-boundp o 'mbr)
      (format s "~W " (slot-value o 'mbr)))
    (format s "~1I~_~W" (children o))))

(defclass spatial-tree-leaf-node (spatial-tree-node)
  ((children :initarg :records :accessor records)))

(define-condition internal-error (simple-error) ()
  (:report
   (lambda (c s)
     (format s "~@<SPATIAL-TREES internal error: ~
                please report how you got this.~2I~_~?~@:>"
             (simple-condition-format-control c)
             (simple-condition-format-arguments c)))))
(defmacro check (form control &rest args)
  `(assert ,form ()
    'internal-error :format-control ,control :format-arguments (list ,@args)))

(define-condition protocol-error (error)
  ((function :initarg :function :reader protocol-error-function)
   (tree :initarg :tree :reader protocol-error-tree))
  (:report
   (lambda (c s)
     (format s "~@<SPATIAL-TREES protocol error: ~S is unimplemented for ~
                tree ~S.~@:>"
             (protocol-error-function c)
             (protocol-error-tree c)))))

(defmacro define-protocol-function (name lambda-list)
  (let ((method-lambda-list (loop for x in lambda-list
                                  if (eq x 'tree) collect '(tree spatial-tree)
                                  else collect x)))
    `(defgeneric ,name ,lambda-list
      (:method ,method-lambda-list
        (error 'protocol-error :function ',name :tree tree)))))

(define-protocol-function search (object tree))
(define-protocol-function insert (object tree))
(define-protocol-function delete (object tree))

(define-protocol-function choose-leaf (r tree))
(define-protocol-function split-node (tree new node))

(defgeneric make-spatial-tree (kind &rest initargs &key &allow-other-keys))

(defgeneric check-consistency (tree)
  (:method-combination progn))
