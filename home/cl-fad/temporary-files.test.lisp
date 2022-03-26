(in-package :cl-fad-test)

(deftest 'temporary-file 'with-output-to-temporary-file ()
  (let ((pathname (with-output-to-temporary-file (f)
                    (write-string "hello" f))))
    (test-assert (probe-file pathname))
    (test-equal (alexandria:read-file-into-string pathname) "hello")
    (delete-file pathname)))

(deftest 'temporary-file 'with-open-temporary-file-keep ()

  (let ((pathname (with-open-temporary-file (f :keep nil)
                    (pathname f))))
    (test-assert (null (probe-file pathname))))
  (let ((pathname (with-open-temporary-file (f :keep t)
                    (pathname f))))
    (test-assert (probe-file pathname))
    (delete-file pathname))

  (let* ((keep nil)
         (pathname (with-open-temporary-file (f :keep keep)
                     (pathname f))))
    (test-assert (null (probe-file pathname))))
  (let* ((keep t)
         (pathname (with-open-temporary-file (f :keep keep)
                     (pathname f))))
    (test-assert (probe-file pathname))
    (delete-file pathname)))

(deftest 'temporary-file 'template-tests ()
  ;; error is signalled when template does not contain a percent sign.
  (let ((*default-template* "foo"))
    (test-condition (with-open-temporary-file (f :keep nil))
                    'invalid-temporary-pathname-template))
  ;; file name template occurs in generated file name (for logical path name)
  (let* ((*default-template* "temporary-files:bla%.txt")
         (pathname (with-open-temporary-file (f :keep nil)
                     (pathname f))))
    (test-assert (cl-ppcre:scan "(?i)bla.*\\.txt$" (namestring pathname))))
  ;; file name template occurs in generated file name (for pysical path name)
  (let* ((*default-template* (concatenate 'string
                                          (namestring (translate-logical-pathname "temporary-files:"))
                                          "bla%.txt"))
         (pathname (with-open-temporary-file (f :keep nil)
                     (pathname f))))
    (test-assert (cl-ppcre:scan "(?i)bla.*\\.txt$" (namestring pathname)))))

                                                 
    
