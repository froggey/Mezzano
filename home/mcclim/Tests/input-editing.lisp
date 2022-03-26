(cl:in-package #:clim-tests)

(def-suite* :mcclim.input-editing
  :in :mcclim)

(test input-editing.*activation-gestures*
  (is (null *activation-gestures*)))

(test input-editing.simple-parse-error
  (is (subtypep 'simple-parse-error 'parse-error))

  (finishes
    (make-condition 'simple-parse-error
                    :format-control "~A" :format-arguments (list 3)))
  (handler-case
      (simple-parse-error "foo: ~A" 3)
    (simple-parse-error (c)
      (is (search "foo: 3" (format nil "~A" c))))
    (:no-error (&rest values)
      (fail "~S returned ~S" 'simple-parse-error values))))

(test input-editing.input-not-of-required-type
  (is (subtypep 'input-not-of-required-type 'parse-error))

  (let ((c (make-condition 'input-not-of-required-type
                           :string "not an INTEGER" :type 'integer)))
    (is (search "not an INTEGER" (format nil "~A" c))))

  (signals input-not-of-required-type
    (input-not-of-required-type 3 'float)))
