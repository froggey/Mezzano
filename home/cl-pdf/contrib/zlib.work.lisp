;;; cl-pdf copyright 2002 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

(eval-when (:load-toplevel :execute)
  (unless (uffi:load-foreign-library
	   #-(or macosx darwin)
	   (uffi:find-foreign-library
	    "libz"
	    '("/usr/local/lib/" "/usr/lib/" "/zlib/")
	    :drive-letters '("C" "D" "E")
	    :types '("so" "a" "dll"))
	   #+(or macosx darwin)
	   (uffi:find-foreign-library "z"
				      `(,(pathname-directory *load-pathname*)))
	   :module "zlib" 
	   :supporting-libraries '("c"))
    (warn "Unable to load zlib")))
  
(uffi:def-function ("compress" c-compress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (source-len :long))
  :returning :int
  :module "zlib")

(uffi:def-struct zstream
  (next-in (* :unsigned-char))
  (avail-in :unsigned-int)
  (total-in :unsigned-long)
  (next-out (* :unsigned-char))
  (avail-out :unsigned-int)
  (total-out :unsigned-long)
  (msg (* :unsigned-char))
  (state :long)
  (zalloc :long)
  (zfree :long)
  (opaque :long)
  (data-type :int)
  (alder :unsigned-long)
  (reserved :unsigned-long))

(defconstant +z-no-compression+ 0)
(defconstant +z-best-speed+ 1)
(defconstant +z-best-compression+ 9)
(defconstant +z-default-compression+ -1)

(uffi:def-function ("deflateInit" deflate-init)
    ((stream (* (:struct zstream)))
     (level :int))
  :returning :int
  :module "zlib")

(defconstant +z-no-flush+ 0)
(defconstant +z-sync-flush+ 2)
(defconstant +z-full-flush+ 3)
(defconstant +z-finish+ 4)

(uffi:def-function ("deflate" deflate)
    ((stream (* (:struct zstream)))
     (flush :int))
  :returning :int
  :module "zlib")

(uffi:def-function ("deflateEnd" deflate-end)
    ((stream (* (:struct zstream))))
  :returning :int
  :module "zlib")

(defvar *z-block-threshold* 10000) ;Must be > *z-block-size*

(defun compress (source)
  "Returns two values: array of bytes containing the compressed data
 and the numbe of compressed bytes"
  (if (> (length source) *z-block-threshold*)
      (block-compress source)
      (let* ((sourcelen (length source))
	     (destsize (+ 12 (ceiling (* sourcelen 1.01))))
	     (dest (uffi:allocate-foreign-string destsize :unsigned t))
	     (destlen (uffi:allocate-foreign-object :long)))
	(setf (uffi:deref-pointer destlen :long) destsize)
	(uffi:with-cstring (source-native source)
	  (let ((result (c-compress dest destlen source-native sourcelen))
		(newdestlen (uffi:deref-pointer destlen :long)))
	    (unwind-protect
		 (if (zerop result)
		     (values (uffi:convert-from-foreign-string 
			      dest
			      :length newdestlen
			      :null-terminated-p nil)
			     newdestlen)
		     (error "zlib error, code ~D" result))
	      (progn
		(uffi:free-foreign-object destlen)
		(uffi:free-foreign-object dest))))))))

(defvar *z-block-size* 4096)

(defun block-compress (source)
  "Returns two values: array of bytes containing the compressed data
 and the numbe of compressed bytes"
  (if (<= (length source) *z-block-threshold*)
      (compress source)
      (uffi:with-foreign-object (z-stream 'z-stream)
	(setf (uffi:get-slot-value z-stream 'z-stream 'zalloc)  0)
	(setf (uffi:get-slot-value z-stream 'z-stream 'zfree)   0)
	(setf (uffi:get-slot-value z-stream 'z-stream 'opaque)  0)
	(setf (uffi:get-slot-value z-stream 'z-stream 'alder)   0)
	(setf (uffi:get-slot-value z-stream 'z-stream 'reserved)0)
	(setf (uffi:get-slot-value z-stream 'z-stream 'state)   0)
	(let* ((source-buf (uffi:allocate-foreign-string *z-block-size* :unsigned t))
	       (dest-buf (uffi:allocate-foreign-string *z-block-size* :unsigned t)))
	  (setf (uffi:get-slot-value z-stream 'z-stream 'next-in) source-buf)
	  (setf (uffi:get-slot-value z-stream 'z-stream 'avail-in) 0)
	  (setf (uffi:get-slot-value z-stream 'z-stream 'total-in) 0)
	  (setf (uffi:get-slot-value z-stream 'z-stream 'next-out) dest-buf)
	  (setf (uffi:get-slot-value z-stream 'z-stream 'avail-out) *z-block-size*)
	  (setf (uffi:get-slot-value z-stream 'z-stream 'total-out) 0)
	  (deflate-init z-stream +z-default-compression+)
	  (unwind-protect
	     (loop with length = (length source)
  	           for start from 0 by block-size
		   for block-size = (min *z-block-size* (- length start))
		   do
		   (unless (zerop block-size)
		     (locally (optimize (speed 3) (safety 0)(space 0)(type (simple-string source)))
		       (dotimes (i block-size)
			 (declare (type fixnum i))
			 (setf (uffi:deref-array source-buf '(:array :unsigned-char) i)
			       (aref source (+ i start)))))
		     (setf (uffi:get-slot-value z-stream 'z-stream 'next-in) source-buf)
		     (setf (uffi:get-slot-value z-stream 'z-stream 'avail-in) block-size))

		   (deflate z-stream +z-no-flush+)
		   (if (zerop result)
		       (values (uffi:convert-from-foreign-string 
				dest
				:length newdestlen
				:null-terminated-p nil)
			       newdestlen)
		       (error "zlib error, code ~D" result)))
	    (progn
	      (uffi:free-foreign-object source-buf)
	      (uffi:free-foreign-object dest-buf)))))))
