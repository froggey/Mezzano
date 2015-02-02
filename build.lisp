;; Build Instructions
;; ------------------

;; Tested under SBCL 1.2.4
(setf sb-impl::*default-external-format* :utf-8)

(defvar *image-name*          "mezzano")
(defvar *this-directory*      (make-pathname :name nil :type nil :version nil
                                             :defaults #.(or *compile-file-pathname* *load-pathname*)))
(defvar *quicklisp-directory* (merge-pathnames #P"quicklisp/" (user-homedir-pathname) nil))

(load (merge-pathnames #P"setup.lisp" *quicklisp-directory* nil))
(push (merge-pathnames #P"./file-server/" *this-directory* nil) asdf:*central-registry*)
(push (merge-pathnames #P"./"             *this-directory* nil) asdf:*central-registry*)

;; Load the remote filesystem server.
(ql:quickload :lispos-file)
(file-server::spawn-file-server)

;; Load the cross build environment.
(ql:quickload :lispos)

;; Initialize the empty cross environment.
(with-compilation-unit ()
  (sys.c::set-up-cross-compiler)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*supervisor-source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*warm-source-files*))

;; Modifiy ipl.lisp to taste.

;; Build a cold image.
(cold-generator::make-image *image-name* :header-path "tools/disk_header")

;; This will produce a raw disk image called mezzano.image in the current directory.


