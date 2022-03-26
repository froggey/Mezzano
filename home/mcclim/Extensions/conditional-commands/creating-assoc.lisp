(in-package :creating-assoc)

(defmacro creating-assoc (item alist &environment env)
  "assoc that creates the requested alist item on-the-fly if not yet existing"
   (multiple-value-bind (dummies vals new setter getter)
                        (get-setf-expansion alist env)
     (let ((object (gensym "object-")))
       `(let* ((,object ,item)
               ,@(mapcar #'list dummies vals)
               (,(car new) ,getter))
          (prog1
              (or (assoc ,object ,(car new))
                  (first (setq ,(car new) (cons (list ,object) ,(car new)))))
            ,setter)))))

;;; Example:
;;;
;;; (let* ((list '((foo 1))))
;;;             (list (assoc 'foo list)
;;;                   (assoc 'baz list)
;;;                   (creating-assoc 'baz list)
;;;                   (assoc 'baz list)
;;;                   list))
;;; => ((FOO 1)
;;;     NIL
;;;     (BAZ)
;;;     (BAZ)
;;;     ((BAZ) (FOO 1)))

;;;
;;;    (creating-assoc 'baz list)
;;;
;;; expands to:
;;;
;;;    (LET* ((#:|object-15058| 'BAZ) (#:G15057 LIST))
;;;      (PROG1
;;;          (OR (ASSOC #:|object-15058| #:G15057)
;;;              (FIRST (SETQ #:G15057 (CONS (LIST #:|object-15058|) #:G15057))))
;;;        (SETQ LIST #:G15057)))


;;; Have a look at http://paste.lisp.org/display/13846#2 if you want to.
