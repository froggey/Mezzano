(defpackage #:clim-mop
  (:use #:clos))

;; CLIM expects INPUT-STREAM-P to be a generic function.
(ext:without-package-lock ("GRAY" "COMMON-LISP")
 (unless (typep #'input-stream-p 'generic-function)
   (setf (fdefinition (intern "ORIGINAL-INPUT-STREAM-P" (find-package :gray))) #'input-stream-p)
   (fmakunbound 'input-stream-p)
   (defgeneric input-stream-p (stream)
     (:method ((stream stream)) (funcall (fdefinition (intern "ORIGINAL-OUTPUT-STREAM-P" (find-package :gray))) stream))))

 ;; CLIM expects OUTPUT-STREAM-P to be a generic function.
 (unless (typep #'output-stream-p 'generic-function)
   (setf (fdefinition (intern "ORIGINAL-OUTPUT-STREAM-P" (find-package :gray))) #'output-stream-p)
   (fmakunbound 'output-stream-p)
   (defgeneric output-stream-p (stream)
     (:method ((stream stream)) (funcall (fdefinition (intern "ORIGINAL-OUTPUT-STREAM-P" (find-package :gray))) stream)))))
