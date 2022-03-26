;;; -*- Lisp -*-
(in-package #:common-lisp)

(defpackage #:test-preferences-1-asdf-system
  (:use #:common-lisp #:asdf))
(in-package #:asdf)

(defsystem test-preferences-system-1
  :components
  ((:file "test-preferences-1"))
  :in-order-to ((test-op (load-op test-preferences-system-1))))

(defmethod operation-done-p
           ((o test-op)
            (c (eql (find-system 'test-preferences-system-1))))
  (values nil))

(defmethod load-preferences
           ((system (eql (find-system 'test-preferences-system-1)))
            (operation test-op))
  ;; the default load-preferences does nothing for anything other than a
  ;; basic-load-op. So, ... we hack it
  (load (make-pathname
         :name "test-preferences-system-test"
         :type "lisp"
         :defaults *default-pathname-defaults*)))

(defmethod preference-file-for-system/operation
           ((system (eql (find-system 'test-preferences-system-1)))
            (operation load-op))
  (make-pathname
   :name "test-preferences-system-load"
   :type "lisp"
   :defaults *default-pathname-defaults*))

