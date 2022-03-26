(in-package pdf)

(defmethod write-document ((filename pathnames::pathname-internal) &optional (document *document*))
  (with-open-file (s filename :direction :output :if-exists :supersede
		     :external-format +external-format+) 
    (write-document s document))) 

(defmethod make-jpeg-image ((pathname pathnames::pathname-internal)) 
  (make-jpeg-image (read-jpeg-file pathname)))

(defun %read-jpeg-file% (filename)
  (with-open-file (s filename :direction :input :element-type 'unsigned-byte)
    (loop with width and height and nb-components and data
       for marker = (read-byte s)
       if (= marker #xFF) do
	 (setf marker (read-byte s))
	 (cond
	   ((member marker '(#xC0 #xC1 #xC2));SOF markers
	    (read-byte s)(read-byte s) ;size
	    (when (/= (read-byte s) 8) (error "JPEG must have 8bits per component"))
	    (setf height (+ (ash (read-byte s) 8)(read-byte s)))
	    (setf width (+ (ash (read-byte s) 8)(read-byte s)))
	    (setf nb-components (read-byte s))
	    (file-position s :start)
	    (setf data (make-array (file-length s) :element-type '(unsigned-byte 8)))
	    (read-sequence data s)
	    (return (values nb-components width height data)))
	   ((member marker '(#xC3 #xC5 #xC6 #xC7 #xC8 #xC9 #xCA #xCB #xCD #xCE #xCF)) ;unsupported markers
	    (error "Unsupported JPEG format"))
	   ((not (member marker '(#xD0 #xD1 #xD2 #xD3 #xD4 #xD5 #xD6 #xD7 #xD8 #x01))) ;no param markers
	    (file-position s (+ (file-position s)(ash (read-byte s) 8)(read-byte s) -2)))))))

