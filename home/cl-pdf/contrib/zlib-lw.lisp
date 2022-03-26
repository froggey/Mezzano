;;; cl-pdf copyright (c) 2002 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;; Lispworks bindings of the zlib compress function
;;; Should be replaced by calls to deflate to avoid those big buffers

(in-package pdf)

;;; Derived from file : "zlib.h"

(fli:register-module "ZLIB" :real-name (if (string= (software-type) "Linux") "zlib.so" "zlib.dll"))

(fli:define-foreign-function (zlib-compress "compress")
    ((dest :pointer)(dest-len (:pointer :long))
     (source :pointer)(source-len :long))
  :language :c
  :calling-convention :cdecl
  :result-type :long
  :module "ZLIB")

(defun compress-string (source)
  (fli:with-dynamic-foreign-objects ()
    (let* ((source-length (length source))
	   (source-fli (fli:convert-to-dynamic-foreign-string source
				      :external-format '(:latin-1 :eol-style :lf)
				      :null-terminated-p nil))
	   (dest-size (+ 12 (ceiling (* source-length 1.01))))
	   (dest-len (fli:allocate-dynamic-foreign-object :type :long
							  :initial-element dest-size))
	   (dest (fli:allocate-dynamic-foreign-object :type '(:unsigned :byte) :nelems dest-size)))
      (let ((res (zlib-compress dest dest-len source-fli source-length)))
	(if (zerop res)
	  (fli:convert-from-foreign-string dest
					   :external-format '(:latin-1 :eol-style :lf)
					   :length (fli:dereference dest-len)
					   :null-terminated-p nil)
	  (error "zlib compress error, code ~d" res))))))


#|
;;;for test only

(fli:define-foreign-function (zlib-uncompress "uncompress")
    ((dest :pointer)(dest-len (:pointer :long))
     (source :pointer)(source-len :long))
  :language :c
  :calling-convention :cdecl
  :result-type :long
  :module "ZLIB")

(defun uncompress-string (source)
  (fli:with-dynamic-foreign-objects ()
    (let* ((source-length (length source))
	   (source-fli (fli:convert-to-dynamic-foreign-string source :external-format '(:latin-1 :eol-style :lf)
							      :null-terminated-p nil))
	   (dest-size 200000) ;adjust as needed
	   (dest-len (fli:allocate-dynamic-foreign-object :type :long :nelems 1
							  :initial-element dest-size))
	   (dest (fli:allocate-dynamic-foreign-object :type '(:unsigned :byte) :nelems dest-size)))
      (fli:with-coerced-pointer (temp) source-fli
	(setf c (loop for i below source-length
		      collect (fli:dereference temp)
			do (fli:incf-pointer temp))))
      (let ((res (zlib-uncompress dest dest-len source-fli source-length)))
	(if (zerop res)
	  (fli:convert-from-foreign-string dest
					   :external-format '(:latin-1 :eol-style :lf)
					   :length (fli:dereference dest-len)
					   :null-terminated-p nil)
	  (error "zlib error, code ~d" res))))))
|#