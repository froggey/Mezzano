(in-package :cl-user)

(mezzanine.file-system.remote:add-simple-file-host :that-mac-thing '(192 168 1 13))
(setf *default-pathname-defaults* (make-pathname :host :that-mac-thing
                                                 :directory '(:absolute "Users" "henry" "Documents" "Mezzanine")))
(setf mezzanine.file-system::*home-directory* (make-pathname :directory '(:absolute "Users" "henry" "Documents" "Old Mezzanine Stuff" "LispOS-home")))
