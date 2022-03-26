;;; -*- lisp -*- system definition

(in-package #:asdf)
;;; I (Joerg Hoehle) totally object ASDF's cluttering my package list
;;; with dozens of tiny definition packages and even more so the
;;; typically empty ASDFNNNN packages. Please give me some package
;;; like ASDF-SYSTEMS or -USER to throw in such trivial definitions.
  
(defsystem :iterate
    :description "Jonathan Amsterdam's iterator/gatherer/accumulator facility"
    :components ((:file "package")
		 (:file "iterate" :depends-on ("package"))))

(defsystem :iterate-pg
    :depends-on (:iterate pg)		; Eric Marsden's pg.lisp
    :components ((:file "iterate-pg")))

(defsystem :iterate-tests
    :depends-on (:iterate #+sbcl sb-rt #-sbcl :rt)
    :components ((:file "iterate-test")))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (find-system ':iterate))))
  (asdf:operate 'asdf:load-op ':iterate-tests)
  (asdf:operate 'asdf:test-op ':iterate-tests))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (find-system ':iterate-tests))))
  (funcall (intern "DO-TESTS" (find-package #+sbcl "SB-RT"
					    #-sbcl "REGRESSION-TEST"))))

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (find-system ':iterate))))
  (provide '#:iterate))

;;; arch-tag: "b8bc9675-313c-11d8-abb9-000c76244c24"
