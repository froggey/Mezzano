(in-package :pdf)

;;; these patches permit operation without libgz
;;; [for example, on a PC/ms windows]


;;; --------------------------------------------------
;;; for PDF.lisp:

(defmethod write-object ((obj pdf-stream) &optional root-level)
  #+(or Lispworks Allegro)
  (when (and (> (length (content obj)) +min-size-for-compression+)
	     *libgz-loaded*)
    (setf (content obj) (compress-string (content obj)))
    (let ((filter (get-dict-value obj "/Filter")))
      (if filter
	(change-dict-value obj "/Filter" (vector "/FlateDecode" filter))
	(push (cons "/Filter" "/FlateDecode")(dict-values obj)))))
  (call-next-method)
  (write-line "stream" *pdf-stream*)
  (write-sequence (content obj) *pdf-stream*)
  (write-char #\Newline *pdf-stream*)
  (write-line "endstream" *pdf-stream*))

;;; --------------------------------------------------
;;; For cl-zlib-small.lisp

;;; This variable may remain nil, if the load fails

(defvar *libgz-loaded* nil)

(defun load-libgz ()
  (restart-case
      (handler-bind
	  ((error 
	    #'(lambda (cond)
		(format *debug-io* "The libz.so file can't be loaded from ~s~%" *libgz-path*)
		(format *debug-io* "Correct *libgz-path* and retry" *libgz-path*)
		(invoke-debugger cond))))
	(let* ((result (load *libgz-path*)))  ;; result of successful load is non-nil
	  (setq *libgz-loaded* (if result t))))
    (skip-loading ()
	:report "Skip using libz, pdfs will not be compressed"
      (return-from load-libgz nil))
    )
  t)

(eval-when (:load-toplevel)
  (unless *libgz-loaded*
    (load-libgz)))

;;; note: if the libz library is not available, this just returns the source string

(defun compress-string (source)
  "Compress the string SOURCE. Returns two values: the array of bytes
representing the compressed data and the number of compressed bytes."
  (if *libgz-loaded*
      (let* ((sourcelen (length source))
	     (destsize (+ 12 (ceiling (* sourcelen 1.05))))
	     (dest (make-array destsize :element-type '(unsigned-byte 8)
			       :initial-element 0))
	     (destlen (make-array 1 :element-type '(unsigned-byte 32)
				  :initial-element destsize)))
	(let ((res (gz-string dest destlen source sourcelen)))
	  (if (zerop res)
	      (values dest (aref destlen 0))
	    (error "zlib error, code ~d" res))))
    (values source (length source))))
