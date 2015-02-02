
(defparameter *file-server-ip* '(192 168 7 8)
  "The IP of the host where the file server is running.")

(defparameter *file-server-source-directory*
  (namestring (merge-pathnames "src/Mezzano/" (user-homedir-pathname)))
  "A string containing the full path to the source tree on the host where the file server is running.")

(defparameter *file-server-home-directory*
  (namestring (merge-pathnames "Documents/Mezzano/" (user-homedir-pathname)))
  "A string containing the the home directory containing the libraries.")

;; The *file-server-…-directory* variables contains path in the file
;; server host. the HOST component of those paths will be replaced by
;; "REMOTE" in the IPL to access the file server remotely.  This
;; allows this configuration to be used by install-dependencies.lisp

