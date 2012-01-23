;;;; Full versions of the sequence functions.

(in-package "SYSTEM.INTERNALS")

;;; FIXME: Should be a class.
(deftype sequence ()
  '(or vector list))

(defun length (sequence)
  (etypecase sequence
    (list (or (list-length sequence)
	      (error 'simple-type-error
		     :expected-type 'sequence
		     :datum sequence
		     :format-control "List ~S is circular."
		     :format-arguments (list sequence))))
    (vector (if (array-has-fill-pointer-p sequence)
		(fill-pointer sequence)
		(array-dimension sequence 0)))))

(defun elt (sequence index)
  (check-type sequence sequence)
  (if (listp sequence)
      ;; TODO: more error checking.
      (nth index sequence)
      (aref sequence index)))

(defun position (item sequence &key test key); test-not start end from-end
  (unless test (setf test #'eql))
  (unless key (setf key #'identity))
  (if (listp sequence)
      (do ((p 0 (1+ p))
	   (i sequence (cdr i)))
	  ((null i) nil)
	(when (funcall test item (funcall key (car i)))
	  (return p)))
      (dotimes (i (length sequence) nil)
	(when (funcall test item (funcall key (elt sequence i)))
	  (return i)))))

(defun count-if (predicate sequence &key key);from-end start end
  (unless key (setf key #'identity))
  (let ((n 0))
    (if (listp sequence)
	(dolist (e sequence)
	  (when (funcall predicate (funcall key e))
	    (incf n)))
	(dotimes (i (length sequence) nil)
	  (when (funcall predicate (funcall key (elt sequence i)))
	    (incf n))))
    n))

(defun count-if-not (predicate sequence &key key);from-end start end
  (count-if (complement predicate) sequence :key key))

(defun find-if (predicate sequence &key key); (start 0) end from-end
  (unless key (setf key #'identity))
  (if (listp sequence)
      (dolist (e sequence)
	(when (funcall predicate (funcall key e))
	  (return e)))
      (dotimes (i (length sequence) nil)
	(when (funcall predicate (funcall key (elt sequence i)))
	  (return (elt sequence i))))))

(defun find (item sequence &key key test test-not); (start 0) end from-end
  (when (and test test-not)
    (error "Both :test and :test-not specified"))
  (when test-not (setf test (complement test-not)))
  (unless test (setf test #'eql))
  (find-if (lambda (x) (funcall test item x)) sequence :key key))

(defun find-if-not (predicate sequence &key key); (start 0) end from-end
  (find-if (complement predicate) sequence :key key))

(defun remove-if (test sequence &key key); from-end (start 0) end count
  (unless key (setf key #'identity))
  (let* ((list (cons nil nil))
	 (tail list))
    (dolist (e sequence (cdr list))
      (when (not (funcall test (funcall key e)))
	(setf (cdr tail) (cons e nil)
	      tail (cdr tail))))))

(defun remove (item sequence &key key test test-not); from-end (start 0) end count
  (when (and test test-not)
    (error "Both :test and :test-not specified"))
  (when test-not (setf test (complement test-not)))
  (unless test (setf test #'eql))
  (remove-if (lambda (x) (funcall test item x)) sequence :key key))

(defun remove-if-not (test sequence &key key); from-end (start 0) end count
  (remove-if (complement test) sequence :key key))

(defun remove-duplicates (sequence); &key test test-not key from-end (start 0) end
  (do* ((result (cons nil nil))
	(tail result)
	(i sequence (cdr i)))
       ((null i)
	(cdr result))
    (unless (member (car i) (cdr result))
      (setf (cdr tail) (cons (car i) nil)
	    tail (cdr tail)))))

(defun subseq-list (sequence start end)
  ;; Seek in sequence
  (do () ((or (null sequence) (= 0 start)))
    (setf sequence (cdr sequence)
	  start (1- start))
    (when end (setf end (1- end))))
  ;; Extract the subsequence
  (do* ((list (cons nil nil))
	(tail list)
	(i sequence (cdr i)))
       ((or (null i)
	    (and end (= 0 end)))
	(cdr list))
    (setf (cdr tail) (cons (car i) nil)
	  tail (cdr tail))
    (when end (setf end (1- end)))))

(defun subseq-vector (sequence start end)
  (if end
      (when (> end (length sequence))
	(error "Invalid bounding index designators ~S ~S for ~S." start end sequence))
      (setf end (length sequence)))
  (when (or (> 0 start) (> start end))
    (error "Invalid bounding index designators ~S ~S for ~S." start end sequence))
  (let ((new-vector (make-array (- end start) :element-type (array-element-type sequence))))
    (dotimes (i (- end start) new-vector)
      (setf (aref new-vector i) (aref sequence (+ start i))))))

(defun subseq (sequence start &optional end)
  (if (listp sequence)
      (subseq-list sequence start end)
      (subseq-vector sequence start end)))

;; Selection sort!
(defun sort (sequence predicate &key key)
  (unless key (setf key #'identity))
  (when sequence
    (do* ((ipos sequence (cdr ipos))
	  (imin ipos ipos))
	 ((null ipos)
	  sequence)
      (do ((i (cdr ipos) (cdr i)))
	  ((null i))
	(when (funcall predicate (car i) (car imin))
	  (setf imin i)))
      (when (not (eq imin ipos))
	;; Swap
	(let ((old-ipos (car ipos))
	      (old-imin (car imin)))
	  (setf (car ipos) old-imin
		(car imin) old-ipos))))))

(defun concatenate (result-type &rest sequences)
  (declare (dynamic-extent sequences))
  ;; Compute total length.
  (let ((total-length (apply #'+ (mapcar #'length sequences))))
    (case result-type
      ((null) (if (= total-length 0)
		  nil
		  (error "Too many elements for result-type NULL.")))
      ((cons list)
       (let* ((result (cons nil nil))
	      (tail result))
	 (dolist (seq sequences)
	   (if (listp seq)
	       (dolist (elt seq)
		 (setf (cdr tail) (cons elt nil)
		       tail (cdr tail)))
	       (dotimes (i (length seq))
		 (setf (cdr tail) (cons (aref seq i) nil)
		       tail (cdr tail)))))
	 (cdr result)))
      (t (let ((result (make-array total-length
				   ;; TODO: Match type better
				   :element-type (ecase result-type
						   ((simple-base-string base-string)
						    'base-char)
						   ((simple-string string)
						    'character)
						   ((simple-vector vector)
						    't))
				   :fill-pointer 0)))
	   (dolist (seq sequences)
	     (if (listp seq)
		 (dolist (elt seq)
		   (vector-push elt result))
		 (dotimes (i (length seq))
		   (vector-push (aref seq i) result))))
	   ;; Grab the storage vector out of the array
	   ;; forcing it to a simple-array type.
	   (array-storage result))))))
