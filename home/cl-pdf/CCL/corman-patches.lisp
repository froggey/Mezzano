(in-package common-lisp) 

(defun get-standard-pprint-dispatch-table () nil) 
(defun get-standard-readtable () (copy-readtable)) 

(in-package pdf)

(defmacro with-binary-files (&body body)
  `(let ((old-binding (fdefinition 'cl::expand-line-feeds)))
     (unwind-protect
	  (progn (setf (fdefinition 'cl::expand-line-feeds)
		       (lambda (string len) 
			 (let* ((size len))
			   (let ((buf (make-array size :element-type 'byte))) 
			     (do ((i 0 (+ i 1))) 
				 ((= i len)) 
			       (setf (elt buf i) (char-int (char string i))))
			     buf))))
		 ,@body)
       (setf (fdefinition 'cl::expand-line-feeds) old-binding))))

(defun compress-string (source) 
  "Compress the string SOURCE. Returns an array of bytes representing the compressed data."
  (let ((bytes (make-array (length source) :element-type 'byte)))
    (dotimes (i (length source))
      (setf (aref bytes i) (char-code (char source i))))
    (subseq (cl::compress-bytes bytes) 8)))

(defun parse-integer (string 
		      &key (start 0) 
		      (end (length string))
		      (radix 10)
		      (junk-allowed nil)
		      &aux (result 0)
		      (state :initial)
		      (sign 1)
		      c)
  
  (do* ((i start (+ i 1))
	(n 0))
       ((>= i end))
    (setq c (char string i))
    (setq n (digit-char-p c radix))
    (cond
      (n (progn
	   (cond
	     ((eq state :finished) 
	      (if (not junk-allowed)
		  (error "Invalid integer parsed: ~A" string)
		  (progn (setq end i) (return)))))
	   (setq result (+ (* result radix) n))
	   (setq state :collecting)))
	
      ((member c (list (int-char 13) (int-char 32) (int-char 9))) ;; '(#\Newline #\Space #\Tab)
       (cond
	 ((eq state :collecting) 
	  (setq state :finished)
	  (if junk-allowed
	      (progn (setq end i) (return))))
	 ((eq state :initial) nil)	; don't do anything
	 ((eq state :finished) nil)))
	
      ;; check for leading sign
      ((and (eq state :initial) (member c '(#\+ #\-)))
       (setq state :collecting)
       (if (char= c #\-)
	   (progn (setf sign -1) (incf start))))
	
      (t 
       (if (not junk-allowed)
	   (error "Invalid integer parsed: ~A" string) ;; string  
	   (progn (setq end i) (return))))))
    
  (if (eq state :initial)
      (setq result nil)
      (setq result (* result sign)))
  (values result end))