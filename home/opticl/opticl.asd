
(asdf:defsystem :opticl
  :name "opticl"
  :description "A library for representing and processing images"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (alexandria retrospectiff zpng pngload cl-jpeg skippy opticl-core cl-tga)
  :components
  ((:static-file "README.md")
   (:static-file "COPYRIGHT")
   (:file "package")
   (:file "coerce")
   (:file "colors")
   (:file "imageops")
   (:file "invert")
   (:file "transform")
   (:file "convolve")
   (:file "morphology")
   (:file "gamma")
   (:file "shapes")
   (:file "tiff")
   (:file "jpeg")
   (:file "png")
   (:file "pngload")
   (:file "pnm")
   (:file "gif")
   (:file "tga")
   (:file "io")
   (:file "cluster")
   (:file "thresholding"))
  :in-order-to ((test-op (test-op :opticl/test))))


(asdf:defsystem :opticl/test
  :depends-on (:opticl :fiveam)
  :serial t
  :default-component-class cl-source-file
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "opticl-test")
                         (:file "generate-test-images")
                         (:file "tiff-test")
                         (:file "jpeg-test")
                         (:file "png-test"))))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:run! :opticl)))


