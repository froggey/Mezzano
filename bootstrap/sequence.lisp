;;;; Full versions of the sequence functions.

(in-package #:sys.int)

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

(defun (setf elt) (value sequence index)
  (check-type sequence sequence)
  (if (listp sequence)
      ;; TODO: more error checking.
      (setf (nth index sequence) value)
      (setf (aref sequence index) value)))

(declaim (inline position))
(defun position (item sequence &key test key from-end); test-not start end
  (unless test (setf test 'eql))
  (unless key (setf key 'identity))
  (cond ((listp sequence)
         (when from-end
           (setf sequence (reverse sequence)))
         (do ((p 0 (1+ p))
              (i sequence (cdr i)))
             ((null i) nil)
           (when (funcall test item (funcall key (car i)))
             (return p))))
        (t (if from-end
               (let ((len (length sequence)))
                 (dotimes (i len nil)
                   (when (funcall test item (funcall key (elt sequence (- len i 1))))
                     (return (- len i 1)))))
               (dotimes (i (length sequence) nil)
                 (when (funcall test item (funcall key (elt sequence i)))
                   (return i)))))))

;;; SBCL's compiler has trouble inlining position optimally.
;;; Give it a little help to speed Genesis up.
(define-compiler-macro position (&whole whole item sequence &key test key from-end)
  (when (or test key from-end)
    (return-from position whole))
  `(%position-eq ,item ,sequence))

(defun %position-eq (item sequence)
  (if (listp sequence)
      (do ((p 0 (1+ p))
	   (i sequence (cdr i)))
	  ((null i) nil)
	(when (eql item (car i))
	  (return p)))
      (dotimes (i (length sequence) nil)
	(when (eql item (elt sequence i))
	  (return i)))))

(defun count-if (predicate sequence &key key);from-end start end
  (unless key (setf key 'identity))
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

(declaim (inline find-if find find-if-not))
(defun find-if (predicate sequence &key key); (start 0) end from-end
  (unless key (setf key 'identity))
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
  (unless test (setf test 'eql))
  (find-if (lambda (x) (funcall test item x)) sequence :key key))

(defun find-if-not (predicate sequence &key key); (start 0) end from-end
  (find-if (complement predicate) sequence :key key))

(declaim (inline remove-if remove remove-if-not))
(defun remove-if (test sequence &key key); from-end (start 0) end count
  (unless key (setf key 'identity))
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
  (unless test (setf test 'eql))
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

(defun (setf subseq) (value sequence start &optional end)
  (let ((count (min (- (or end (length sequence)) start)
                    (length value))))
    (dotimes (i count)
      (setf (elt sequence (+ start i)) (elt value i)))
    value))

;; Selection sort!
(defun sort (sequence predicate &key key)
  (unless key (setf key 'identity))
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
	   result)))))

;;; FIXME: must work on sequences, not lists.
(defun every (predicate first-seq &rest sequences)
  (declare (dynamic-extent sequences))
  (if (listp first-seq)
      (do* ((lists (cons first-seq sequences)))
           (nil)
        (do* ((call-list (cons nil nil))
              (call-tail call-list (cdr call-tail))
              (itr lists (cdr itr)))
             ((null itr)
              (when (not (apply predicate (cdr call-list)))
                (return-from every nil)))
          (when (null (car itr))
            (return-from every t))
          (setf (cdr call-tail) (cons (caar itr) nil)
                (car itr) (cdar itr))))
      (progn
        (when sequences
          (error "TODO: Every with many non-list sequences."))
        (dotimes (i (length first-seq) t)
          (unless (funcall predicate (aref first-seq i))
            (return nil))))))

(defun some (predicate first-seq &rest sequences)
  (declare (dynamic-extent sequences))
  (do* ((lists (cons first-seq sequences)))
       (nil)
    (do* ((call-list (cons nil nil))
	  (call-tail call-list (cdr call-tail))
	  (itr lists (cdr itr)))
	 ((null itr)
	  (let ((result (apply predicate (cdr call-list))))
	    (when result
	      (return-from some result))))
      (when (null (car itr))
	(return-from some nil))
      (setf (cdr call-tail) (cons (caar itr) nil)
	    (car itr) (cdar itr)))))
