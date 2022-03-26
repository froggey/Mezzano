(defsystem :undefined-variables-throttled
  :components ((:file "fun-with-undefined-locals"
                  :perform (compile-op :around (op c)
                                 (declare (ignorable op c))
                                 (let ((asdf:*compile-file-warnings-behaviour* :ignore))
                                   (call-next-method))))))
