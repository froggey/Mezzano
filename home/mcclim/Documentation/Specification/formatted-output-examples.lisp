;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-

(defun table-test (count &optional (stream *standard-output*))
  (with-open-file (file-stream #P"sys:clim;rel-2;specs;table-example.ps"
			       :direction :output)
    (with-output-to-postscript-stream (stream file-stream)
      (fresh-line stream)
      (formatting-table (stream :inter-column-spacing '(3 :character))
	(dotimes (i count)
	  (formatting-row (stream)
	    (formatting-cell (stream :align-x :right)
	      (prin1 i stream))
	    (formatting-cell (stream :align-x :right)
	      (prin1 (* i i) stream))
	    (formatting-cell (stream :align-x :right)
	      (prin1 (* i i i) stream))))))))

(defun graph-test (&optional (stream *standard-output*) (orientation :horizontal)) 
  (with-open-file (file-stream #P"sys:clim;rel-2;specs;graph-example.ps"
			       :direction :output)
    (with-output-to-postscript-stream (stream file-stream)
      (fresh-line stream)
      (macrolet ((make-node (&key name children)
		   `(list* ,name ,children)))
	(flet ((node-name (node)
		 (car node))
	       (node-children (node)
		 (cdr node)))
	  (let* ((2a (make-node :name "2A"))
		 (2b (make-node :name "2B"))
		 (2c (make-node :name "2C"))
		 (1a (make-node :name "1A" :children (list 2a 2b)))
		 (1b (make-node :name "1B" :children (list 2b 2c)))
		 (root (make-node :name "0" :children (list 1a 1b))))
	    (format-graph-from-root
	      root
	      #'(lambda (node s)
		  (write-string (node-name node) s))
	      #'node-children
	      :orientation orientation
	      :stream stream)))))))

(defun border-test (&optional (stream *standard-output*))
  (with-open-file (file-stream #P"sys:clim;rel-2;specs;border-example.ps" 
			       :direction :output)
    (with-output-to-postscript-stream (stream file-stream)
      (fresh-line stream)
      (surrounding-output-with-border (stream :shape :rectangle)
	(format stream "This is some output with a rectangular border"))
      (terpri stream) (terpri stream)
      (surrounding-output-with-border (stream :shape :drop-shadow)
	(format stream "This has a drop-shadow under it"))
      (terpri stream) (terpri stream)
      (surrounding-output-with-border (stream :shape :underline)
	(format stream "And this output is underlined")))))
