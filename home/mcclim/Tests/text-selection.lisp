(cl:in-package #:clim-tests)

(def-suite* :mcclim.text-selection
  :in :mcclim)

(test text-selection.smoke
  "Smoke test for the text selection protocol."
  (let ((port (make-instance 'climi::standard-port))
        (sheet-1 (make-instance 'basic-sheet))
        (sheet-2 (make-instance 'basic-sheet))
        (box-1 :testing-selection-1)
        (box-2 :testing-selection-2))
    (defmethod port ((sheet (eql sheet-1))) port)
    (defmethod port ((sheet (eql sheet-2))) port)
    (publish-selection sheet-1 box-1 42 'integer)
    ;; all sheets with same port should have access to same content
    (is (= (request-selection sheet-1 box-1 'integer) 42))
    (is (= (request-selection sheet-2 box-1 'integer) 42))
    ;; types are gracefully matched (but not really verified)
    (is (equal '(42 integer) (multiple-value-list (request-selection sheet-2 box-1 't))))
    (is (equal '(42 integer) (multiple-value-list (request-selection sheet-2 box-1 'integer))))
    (is (null (request-selection sheet-2 box-1 'fixnum)))
    (is (null (request-selection sheet-2 box-1 'sequence)))
    (is (null (request-selection sheet-2 box-1 'bit)))
    ;; ;; it is possible to overwrite box
    (publish-selection sheet-2 box-1 64.2 'float)
    (is (= (request-selection sheet-1 box-1 'float) 64.2))
    (is (= (request-selection sheet-2 box-1 't) 64.2))
    ;; and release it (3rd argument is ignored for sheet methods)
    (release-selection sheet-1 box-1 nil)
    (is (null (request-selection sheet-1 box-1 't)))
    (is (null (request-selection sheet-1 box-1 'null)))
    ;; not cross-box contignency
    (publish-selection sheet-1 box-2 "daniel" 'string)
    (is (stringp (request-selection sheet-1 box-2 'string)))
    (is (null (request-selection sheet-1 box-1 'string)))))
