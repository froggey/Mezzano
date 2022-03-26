(defvar *tddoc* 0)
(incf *tddoc*)
(format t "tddoc loaded ~D time~:P.~%" *tddoc*)

(if (= *tddoc* 1)
    (defsystem "test-defsystem-depends-on-change"
      :defsystem-depends-on ("test-asdf/dep-can-change" "test-asdf/dep-can-disappear"))
    (defsystem "test-defsystem-depends-on-change"
      :defsystem-depends-on ("test-asdf/dep-can-change" "test-asdf/dep-can-appear")))
