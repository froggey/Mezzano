(in-package :cl-fad)

(defmacro defalias (name args realname)
  `(progn
     (defun ,name ,args
       ,(if (eql '&rest (first args))
            `(apply #',realname ,(second args))
            `(,realname ,@args)))
     (define-compiler-macro ,name (&rest args)
       (list* ',realname args))))

(defalias path:dirname (pathname) cl-fad:pathname-directory-pathname)

(defun path:basename (pathname) (pathname (file-namestring pathname)))

(defalias path:-e (pathname) cl-fad:file-exists-p)

(defalias path:-d (directory) cl-fad:directory-exists-p)

(defalias path:catfile (&rest pathnames) cl-fad:merge-pathnames-as-file)

(defalias path:catdir (&rest pathnames) cl-fad:merge-pathnames-as-directory)

(defalias path:= (a b) cl-fad:pathname-equal)

(defalias path:absolute-p (pathname) cl-fad:pathname-absolute-p)

(defalias path:relative-p (pathname) cl-fad:pathname-relative-p)

(defalias path:root-p (pathname) cl-fad:pathname-root-p)

(defalias path:rm-r (pathname) cl-fad:delete-directory-and-files)
