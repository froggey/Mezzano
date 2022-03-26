(cl:defpackage #:clim-tests
  (:use #:clim-lisp #:clim #:clime #:fiveam)
  (:shadowing-import-from #:fiveam #:test)
  (:import-from #:climi #:coordinate=)
  (:export #:run-tests))

(cl:in-package #:clim-tests)

(def-suite :mcclim)

(defun run-tests ()
  (run! :mcclim))
