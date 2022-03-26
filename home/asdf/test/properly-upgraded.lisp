(in-package :asdf-test)

(assert (asymval '#:*file1* :test-package))
(assert (asymval '#:*file3* :test-package))

;; This broke at least when upgrading from 3.1.6-3.1.7 to 3.2.0-3.3.1
(assert-equal (asdf/bundle::gather-type (asdf:make-operation 'asdf:monolithic-lib-op)) :object)

(defparameter *properly-upgraded* t)
