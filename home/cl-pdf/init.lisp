;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

(defvar *zlib-loaded* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun find-zlib-path* (name)
  (uffi:find-foreign-library
   name
   *zlib-search-paths*
   :drive-letters '("C" "D" "E")
   :types '("so" "a" "dll" "dylib")))

(defun find-zlib-path ()
  (or (find-zlib-path* "libz")
      (find-zlib-path* "zlib1"))))

#+(or cmu sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((zlib-path (find-zlib-path)))
    (when zlib-path
      (format t "~&;;; Loading ~s" zlib-path)
      (uffi:load-foreign-library zlib-path
                                 :module "zlib" 
                                 :supporting-libraries '("c")))))

