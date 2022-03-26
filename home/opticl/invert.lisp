
(in-package :opticl)

(defun identity-matrix (n &key (element-type 'double-float))
  (let ((the-matrix (make-array (list n n)
                                :element-type element-type
                                :initial-element (coerce 0 element-type))))
    (let ((one (coerce 1 element-type)))
      (dotimes (i n)
        (setf (aref the-matrix i i) one)))
    the-matrix))

(defun horizontal-concatentate-array (a b &key (element-type (array-element-type a)))
  (if (= (array-rank a) 2)
      (if (= (array-rank b) 2)
          (destructuring-bind (a-rows a-columns)
              (array-dimensions a)
            (destructuring-bind (b-rows b-columns)
                (array-dimensions b)
              (if (= a-rows b-rows)
                  (let ((c (make-array
                            (list a-rows (+ a-columns b-columns))
                            :element-type element-type)))
                    (dotimes (i a-rows)
                      (dotimes (j a-columns)
                        (setf  (aref c i j) (aref a i j))))
                    (dotimes (i a-rows)
                      (dotimes (j b-columns)
                        (setf (aref c i (+ j a-columns)) (aref b i j))))
                    c)
                  (error "Arrays dimension 0 must be equal"))))
          (error "Array not of rank 2"))
      (error "Array not of rank 2")))

(defun array-columns (a column-start column-end &key (element-type (array-element-type a)))
  (let ((a-rows (array-dimension a 0)))
    (let ((c (make-array
              (list a-rows (- column-end column-start))
              :element-type element-type)))
      (dotimes (i a-rows)
        (loop for j below (- column-end column-start)
           do (setf (aref c i j) (aref a i (+ column-start j)))))
      c)))

(defun get-first-non-zero-row-in-col (a column &optional (start 0))
  (let ((n (array-dimension a 0)))
    (do ((i start (+ i 1)))
        ((or (= i n)
             (not (zerop (aref a i column))))
         (unless (= i n) i))))) 

(defun swap-rows (a k l)
  (let* ((columns (array-dimension a 1)))
    (dotimes (j columns)
      (let ((temp (aref a k j)))
	(setf (aref a k j) (aref a l j))
	(setf (aref a l j) temp)))))

(defun invert-matrix (a)
  (let* ((columns (array-dimension a 1))
         (c (horizontal-concatentate-array a (identity-matrix columns))))
    (do*
     ((y 0 (+ y 1)))
     ((or (= y columns) (not c)))
      (let ((z (get-first-non-zero-row-in-col c y y)))
        (cond
          ((not z) (setf c nil))
          (t
           (when (> z y)
             (swap-rows c y z))
           ;; divide row y by diagonal entry y
           (loop for i below (ash columns 1)
              with denom = (aref c y y)
              do (setf (aref c y i) (/ (aref c y i) denom)))
           (do*
            ((i 0 (+ i 1)))
            ((= i columns))
             (unless (= i y)
               (let ((k (aref c i y)))
                 (dotimes (j (ash columns 1))
                   (setf (aref c i j) (+ (aref c i j)
                                         (* (- k) (aref c y j))))))))))))
    (if c
      (array-columns c columns (+ columns columns))
      (error "Matrix not invertible."))))


