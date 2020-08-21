;;;; Full versions of the sequence functions.

(in-package :mezzano.internals)

(declaim (inline check-test-test-not))
(defun check-test-test-not (test test-not)
  (when (and test test-not)
    (error "TEST and TEST-NOT specified")))

(defmacro with-test-test-not ((test test-not &key (default-test ''eql)) &body body)
  (check-type test symbol)
  (check-type test-not symbol)
  `(progn
     (check-test-test-not ,test ,test-not)
     (let ((,test (cond (,test)
                        (,test-not (complement ,test-not))
                        (t ,default-test))))
       ,@body)))

(defun length (sequence)
  (cond ((null sequence) 0)
        ((consp sequence)
         (or (list-length sequence)
             (error 'simple-type-error
                    :expected-type 'sequence
                    :datum sequence
                    :format-control "List ~S is circular."
                    :format-arguments (list sequence))))
        ((%simple-1d-array-p sequence)
         (%object-header-data sequence))
        ((and (complex-array-p sequence)
              (eql (%object-header-data sequence) 1)) ; rank 1
         (let ((fill-pointer (%object-ref-t sequence +complex-array-fill-pointer+)))
           (or fill-pointer
               (%object-ref-t sequence +complex-array-axis-0+))))
        (t
         (error 'type-error
                :expected-type 'sequence
                :datum sequence))))

(defun elt (sequence index)
  (check-type sequence sequence)
  (check-type index (integer 0))
  (if (listp sequence)
      (let ((the-cdr (nthcdr index sequence)))
        (when (null the-cdr)
          (error "Index ~D out of bounds for sequence ~S." index sequence))
        (car the-cdr))
      (aref sequence index)))

(defun (setf elt) (value sequence index)
  (check-type sequence sequence)
  (check-type index (integer 0))
  (if (listp sequence)
      (let ((the-cdr (nthcdr index sequence)))
        (when (null the-cdr)
          (error "Index ~D out of bounds for sequence ~S." index sequence))
        (setf (car the-cdr) value))
      (setf (aref sequence index) value)))

(defun (cas elt) (sequence index old new)
  (check-type sequence sequence)
  (check-type index (integer 0))
  (if (listp sequence)
      (let ((the-cdr (nthcdr index sequence)))
        (when (null the-cdr)
          (error "Index ~D out of bounds for sequence ~S." index sequence))
        (cas (car the-cdr) old new))
      (cas (aref sequence index) old new)))

(declaim (inline position-if))
(defun position-if (predicate sequence &key key from-end (start 0) end)
  (unless key (setf key 'identity))
  (etypecase sequence
    (list
     (cond ((and (eql start 0)
                 (eql end nil))
            (when from-end
              (setf sequence (reverse sequence)))
            (do ((p 0 (1+ p))
                 (i sequence (cdr i)))
                ((null i) nil)
              (when (funcall predicate (funcall key (car i)))
                (return (if from-end
                            (- (length sequence) p 1)
                            p)))))
           (t
            (let ((end (or end (length sequence))))
              (if from-end
                  (let ((len (- end start)))
                    (dotimes (i len nil)
                      (when (funcall predicate (funcall key (elt sequence (+ start (- len i 1)))))
                        (return (+ start (- len i 1))))))
                  (dotimes (i (- end start) nil)
                    (when (funcall predicate (funcall key (elt sequence (+ i start))))
                      (return (+ start i)))))))))
    (vector
     (assert (<= 0 start))
     (when end
       (assert (<= end (length sequence))))
     (let ((end (or end (length sequence))))
       (if from-end
           (loop
              for i fixnum from (1- end) downto start
              when (funcall predicate (funcall key (aref sequence i)))
              do (return i))
           (loop
              for i fixnum from start below end
              when (funcall predicate (funcall key (aref sequence i)))
              do (return i)))))))

(declaim (inline position))
(defun position (item sequence &key test test-not key from-end (start 0) end)
  (with-test-test-not (test test-not)
    (position-if (lambda (x) (funcall test item x)) sequence
                 :key key
                 :from-end from-end
                 :start start
                 :end end)))

(declaim (inline position-if-not))
(defun position-if-not (predicate sequence &key key from-end (start 0) end)
  (position-if (complement predicate) sequence
               :key key
               :from-end from-end
               :start start
               :end end))

(defun count-if (predicate sequence &key key from-end (start 0) end)
  (unless key (setf key #'identity))
  (when (or (not (zerop start))
            end)
   (setf sequence (subseq sequence start end)))
  (when from-end
    (setf sequence (reverse sequence)))
  (let ((n 0))
    (if (listp sequence)
        (dolist (e sequence)
          (when (funcall predicate (funcall key e))
            (incf n)))
        (dotimes (i (length sequence))
          (when (funcall predicate (funcall key (elt sequence i)))
            (incf n))))
    n))

(defun count (item sequence &key key from-end (start 0) end test test-not)
  (with-test-test-not (test test-not)
    (count-if #'(lambda (y) (funcall test item y)) sequence :key key :from-end from-end :start start :end end)))

(defun count-if-not (predicate sequence &key key from-end (start 0) end)
  (count-if (complement predicate) sequence :key key :from-end from-end :start start :end end))

(declaim (inline find-if find find-if-not))
(defun find-if (predicate sequence &key key (start 0) end from-end)
  (unless key (setf key 'identity))
  (etypecase sequence
    (list
     (when (or (not (zerop start))
               end)
       (setf sequence (subseq sequence start end)))
     (when from-end
       (setf sequence (reverse sequence)))
     (dolist (e sequence)
       (when (funcall predicate (funcall key e))
         (return e))))
    (vector
     (assert (<= 0 start (length sequence)))
     (when end
       (assert (<= start end (length sequence))))
     (let ((end (or end (length sequence))))
       (cond (from-end
              (loop
                 for i fixnum from (1- end) downto start
                 for val = (aref sequence i)
                 when (funcall predicate (funcall key val))
                 do (return val)))
             (t
              (loop
                 for i fixnum from start below end
                 for val = (aref sequence i)
                 when (funcall predicate (funcall key val))
                 do (return val))))))))

(defun find (item sequence &key key test test-not (start 0) end from-end)
  (with-test-test-not (test test-not)
    (find-if (lambda (x) (funcall test item x)) sequence :key key :start start :end end :from-end from-end)))

(defun find-if-not (predicate sequence &key key (start 0) end from-end)
  (find-if (complement predicate) sequence :key key :start start :end end :from-end from-end))

(declaim (inline remove-if remove remove-if-not))
(defun remove-if (test sequence &key from-end (start 0) end count key)
  (unless key (setf key 'identity))
  (when from-end
    (setf sequence (reverse sequence))
    (rotatef start end)
    (when (null start) (setf start 0))
    (if (= end 0)
        (setf end nil)
        (setf end (- (length sequence) end))))
  (when count
    (check-type count (or null integer)))
  (etypecase sequence
    (list
     (loop
        with remaining = count
        for idx from 0
        for e in sequence
        when (or (< idx start)
                 (and (numberp end)
                      (<= end idx))
                 (not (and (funcall test (funcall key e))
                           (or (null count)
                               (>= (decf remaining) 0)))))
        collect e into result
        finally (return (if from-end
                            (nreverse result)
                            result))))
    (vector
     (loop
        with remaining = count
        with result = (make-array (length sequence)
                                  :element-type (array-element-type sequence)
                                  :fill-pointer 0)
        for idx from 0
        for e across sequence
        when (or (< idx start)
                 (and (numberp end)
                      (<= end idx))
                 (not (and (funcall test (funcall key e))
                           (or (null count)
                               (>= (decf remaining) 0)))))
        do (vector-push e result)
        finally (return (if from-end
                            (nreverse result)
                            result))))))

(defun remove (item sequence &key from-end test test-not (start 0) end count key)
  (with-test-test-not (test test-not)
    (remove-if (lambda (x) (funcall test item x)) sequence
               :from-end from-end
               :start start
               :end end
               :count count
               :key key)))

(defun remove-if-not (test sequence &key from-end (start 0) end count key)
  (remove-if (complement test) sequence
             :from-end from-end
             :start start
             :end end
             :count count
             :key key))

(declaim (inline delete-if delete delete-if-not))
(defun delete-if (test sequence &key from-end (start 0) end count key)
  (remove-if test sequence :from-end from-end :start start :end end :count count :key key))

(defun delete (item sequence &key from-end test test-not (start 0) end count key)
  (with-test-test-not (test test-not)
    (delete-if (lambda (x) (funcall test item x)) sequence
               :from-end from-end
               :start start
               :end end
               :count count
               :key key)))

(defun delete-if-not (test sequence &key from-end (start 0) end count key)
  (delete-if (complement test) sequence
             :from-end from-end
             :start start
             :end end
             :count count
             :key key))

(defun remove-duplicates (sequence &key from-end test test-not key (start 0) end)
  (with-test-test-not (test test-not)
    (unless key (setf key 'identity))
    (when (or (not (eql start 0))
              end)
      (setf sequence (subseq sequence start end)))
    (etypecase sequence
      (list
       (if from-end
           (do* ((result (cons nil nil))
                 (tail result)
                 (i sequence (cdr i)))
                ((null i)
                 (cdr result))
             (declare (dynamic-extent result))
             (unless (member (funcall key (car i)) (cdr result) :test test :key key)
               (setf (cdr tail) (cons (car i) nil)
                     tail (cdr tail))))
           (do* ((result (cons nil nil))
                 (tail result)
                 (i sequence (cdr i)))
                ((null i)
                 (cdr result))
             (declare (dynamic-extent result))
             (unless (member (funcall key (car i)) (cdr i) :test test :key key)
               (setf (cdr tail) (cons (car i) nil)
                     tail (cdr tail))))))
      (vector
       (when from-end
         (setf sequence (reverse sequence)))
       (let ((result (make-array (length sequence)
                                 :element-type (array-element-type sequence)
                                 :fill-pointer 0)))
         (dotimes (i (length sequence))
           (unless (find (funcall key (aref sequence i))
                         result
                         :key key
                         :test test)
             (vector-push (aref sequence i) result)))
         (when from-end
           (setf result (nreverse result)))
         ;; Simplify result.
         (subseq result 0))))))

(defun delete-duplicates (sequence &key from-end test test-not (start 0) end key)
  (remove-duplicates sequence
                     :from-end from-end
                     :test test
                     :test-not test-not
                     :start start
                     :end end
                     :key key))

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
    (declare (dynamic-extent list))
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
    (replace new-vector sequence :start2 start :end2 end)
    new-vector))

(defun subseq (sequence start &optional end)
  (if (listp sequence)
      (subseq-list sequence start end)
      (subseq-vector sequence start end)))

(defun (setf subseq) (value sequence start &optional end)
  (replace sequence value :start1 start :end1 end)
  value)

(defparameter *sort-list-vector-crossover-length* 128
  "A list longer than this will be coerced to a vector, sorted, then coerced back to a list.")

;; Selection sort!
(defun sort-list (sequence predicate key)
  (let ((len (length sequence)))
    (cond ((> len *sort-list-vector-crossover-length*)
           (let ((vec (make-array len)))
             (loop
                for i from 0
                for elt in sequence
                do (setf (svref vec i) elt))
             (sort-vector vec predicate key)
             (loop
                for i from 0
                for cons on sequence
                do (setf (car cons) (svref vec i))))
           sequence)
          (t
           (cond ((endp sequence)
                  '())
                 (t
                  (do* ((ipos sequence (cdr ipos))
                        (imin ipos ipos))
                       ((null ipos)
                        sequence)
                    (do ((i (cdr ipos) (cdr i)))
                        ((null i))
                      (when (funcall predicate (funcall key (car i)) (funcall key (car imin)))
                        (setf imin i)))
                    (when (not (eq imin ipos))
                      ;; Swap
                      (let ((old-ipos (car ipos))
                            (old-imin (car imin)))
                        (setf (car ipos) old-imin
                              (car imin) old-ipos))))))))))

;; Heapsort implementation from https://en.wikipedia.org/wiki/Heapsort
(defun sort-vector (sequence predicate key)
  (labels ((i-parent (i)
             (truncate (1- i) 2))
           (i-left-child (i)
             (+ (* i 2) 1))
           (i-right-child (i)
             (+ (* i 2) 2))
           (heapify ()
             (let ((start (i-parent (1- (length sequence)))))
               (loop while (>= start 0) do
                    (sift-down start (1- (length sequence)))
                    (decf start))))
           (get-elt (index)
             (if (simple-vector-p sequence)
                 (svref sequence index)
                 (aref sequence index)))
           ((setf get-elt) (value index)
             (if (simple-vector-p sequence)
                 (setf (svref sequence index) value)
                 (setf (aref sequence index) value)))
           (cmp (a b)
             (funcall predicate
                      (funcall key (get-elt a))
                      (funcall key (get-elt b))))
           (sift-down (start end)
             (let ((root start))
               (loop while (<= (i-left-child root) end) do
                    (let ((child (i-left-child root))
                          (swap root))
                      (when (cmp swap child)
                        (setf swap child))
                      (when (and (<= (1+ child) end)
                                 (cmp swap (1+ child)))
                        (setf swap (1+ child)))
                      (cond ((eql swap root)
                             (return))
                            (t
                             (rotatef (get-elt root)
                                      (get-elt swap))
                             (setf root swap))))))))
    (heapify)
    (let ((end (1- (length sequence))))
      (loop while (> end 0) do
           (rotatef (get-elt end) (get-elt 0))
           (decf end)
           (sift-down 0 end)))
    sequence))

(defun sort (sequence predicate &key key)
  (unless key (setf key 'identity))
  (etypecase sequence
    (list
     (sort-list sequence predicate key))
    (vector
     (sort-vector sequence predicate key))))

(defun check-vector-sorted (vector predicate &key (key 'identity))
  (dotimes (i (1- (length vector)))
    (assert (funcall predicate
                     (funcall key (aref vector i))
                     (funcall key (aref vector (1+ i)))))))

(eval-when (:compile-toplevel :load-toplevel :execute) ; needed for x-compiler
;; FIXME: This should also extract the length and return it too.
(defun vector-type-element-type (type &optional environment)
  (let ((expanded-type (typeexpand type environment)))
    (cond ((and (listp expanded-type)
                (member (first expanded-type) '(vector simple-array array))
                (>= (length expanded-type) 2)
                (not (eql (second expanded-type) '*)))
           (second expanded-type))
          ((subtypep expanded-type 'base-string environment)
           'base-char)
          ((subtypep expanded-type 'string environment)
           'character)
          ((subtypep expanded-type 'bit-vector environment)
           'bit)
          (t 't))))
)

(define-compiler-macro concatenate (&whole whole &environment environment result-type &rest sequences)
  (flet ((bail () (return-from concatenate whole)))
    (when (not (and (consp result-type)
                    (eql (first result-type) 'quote)
                    (consp (rest result-type))
                    (null (cddr result-type))))
      (bail))
    (let ((type (second result-type)))
      (cond ((and (not (subtypep type 'null environment))
                  (subtypep type 'list environment))
             `(%concatenate-list ,@sequences))
            ((subtypep type 'vector environment)
             `(%concatenate-vector ',(upgraded-array-info
                                      (vector-type-element-type type environment))
                                   ,@sequences))
            (t (bail))))))

(defun %concatenate-vector (array-info &rest sequences)
  (declare (dynamic-extent sequences))
  ;; Compute total length.
  (let* ((total-length (loop for seq in sequences summing (length seq)))
         (result (if (specialized-array-definition-tag array-info)
                     (make-simple-array-1 total-length array-info nil)
                     (make-array total-length
                                 :element-type (specialized-array-definition-type array-info))))
         (position 0))
    (dolist (seq sequences)
      (setf (subseq result position) seq)
      (incf position (length seq)))
    result))

(defun %concatenate-list (&rest sequences)
  (declare (dynamic-extent sequences))
  (let* ((result (cons nil nil))
         (tail result))
    (declare (dynamic-extent result))
    (dolist (seq sequences)
      (if (listp seq)
          (dolist (elt seq)
            (setf (cdr tail) (cons elt nil)
                  tail (cdr tail)))
          (dotimes (i (length seq))
            (setf (cdr tail) (cons (aref seq i) nil)
                  tail (cdr tail)))))
    (cdr result)))

(defun concatenate (result-type &rest sequences)
  (declare (dynamic-extent sequences))
  (cond
    ((subtypep result-type 'null)
     (let ((total-length (loop for seq in sequences summing (length seq))))
       (when (not (eql total-length 0))
         (error "Too many elements for result-type NULL."))
       nil))
    ((subtypep result-type 'list)
     (apply #'%concatenate-list sequences))
    ((subtypep result-type 'vector)
     (apply #'%concatenate-vector
            (upgraded-array-info (vector-type-element-type result-type))
            sequences))
    (t (error "Don't understand result-type ~S." result-type))))

(define-compiler-macro every (predicate first-seq &rest more-sequences)
  (let* ((predicate-sym (gensym "PREDICATE"))
         (block (gensym "EVERY"))
         (n-sequences (1+ (length more-sequences)))
         (element-vars (loop
                          repeat n-sequences
                          collect (gensym))))
    `(let ((,predicate-sym ,predicate))
       (block ,block
         (map nil (lambda ,element-vars
                    (when (not (funcall ,predicate-sym ,@element-vars))
                      (return-from ,block nil)))
              ,first-seq ,@more-sequences)
         t))))

(defun every (predicate first-seq &rest more-sequences)
  (apply #'map nil (lambda (&rest seqs)
                     (when (not (apply predicate seqs))
                       (return-from every nil)))
         first-seq more-sequences)
  t)

(define-compiler-macro some (predicate first-seq &rest more-sequences)
  (let* ((predicate-sym (gensym "PREDICATE"))
         (block (gensym "SOME"))
         (n-sequences (1+ (length more-sequences)))
         (element-vars (loop
                          repeat n-sequences
                          collect (gensym)))
         (pred-result (gensym)))
    `(let ((,predicate-sym ,predicate))
       (block ,block
         (map nil (lambda ,element-vars
                    (let ((,pred-result (funcall ,predicate-sym ,@element-vars)))
                      (when ,pred-result
                        (return-from ,block ,pred-result))))
              ,first-seq ,@more-sequences)
         nil))))

(defun some (predicate first-seq &rest more-sequences)
  (apply #'map nil (lambda (&rest seqs)
                     (let ((pred-result (apply predicate seqs)))
                       (when pred-result
                         (return-from some pred-result))))
         first-seq more-sequences)
  nil)

(define-compiler-macro notany (predicate first-seq &rest more-sequences)
  `(not (some ,predicate ,first-seq ,@more-sequences)))

(defun notany (predicate first-sequence &rest more-sequences)
  (not (apply 'some predicate first-sequence more-sequences)))

(define-compiler-macro notevery (predicate first-seq &rest more-sequences)
  `(not (every ,predicate ,first-seq ,@more-sequences)))

(defun notevery (predicate first-sequence &rest more-sequences)
  (not (apply 'every predicate first-sequence more-sequences)))

(defun replace-known-args (sequence-1 sequence-2 start1 end1 start2 end2)
  (unless end1 (setf end1 (length sequence-1)))
  (unless end2 (setf end2 (length sequence-2)))
  (assert (<= 0 start1 end1 (length sequence-1)))
  (assert (<= 0 start2 end2 (length sequence-2)))
  ;; Trim both ends.
  (let* ((n1 (- end1 start1))
         (n2 (- end2 start2))
         (n (min n1 n2)))
    (setf end1 (+ start1 n)
          end2 (+ start2 n)))
  ;; Check for overlapping sequences.
  (let ((copy-backwards nil))
    (when (and (eql sequence-1 sequence-2)
               ;; Only matters if source is before dest.
               (<= start2 start1)
               (or (and (<= start1 start2) (< start2 end1))
                   (and (< start1 end2) (<= end2 end1))
                   (and (<= start2 start1) (< start1 end2))
                   (and (< start2 end1) (<= end1 end2))))
      (when (eql start1 start2)
        (return-from replace-known-args sequence-1))
      (setf copy-backwards t))
    (macrolet ((fast-vector (type)
                 `(if (and (typep sequence-1 '(array ,type (*)))
                           (not (array-displacement sequence-1))
                           (typep sequence-2 '(array ,type (*)))
                           (not (array-displacement sequence-2)))
                      (let ((simple-vector-1 (if (typep sequence-1 '(simple-array ,type (*)))
                                                 sequence-1
                                                 (sys.int::%complex-array-storage sequence-1)))
                            (simple-vector-2 (if (typep sequence-2 '(simple-array ,type (*)))
                                                 sequence-2
                                                 (sys.int::%complex-array-storage sequence-2))))
                        (declare (type (simple-array ,type (*)) simple-vector-1 simple-vector-2)
                                 (type fixnum start1 end1 start2 end2)
                                 (optimize speed (safety 0)))
                        (cond (copy-backwards
                               (loop
                                  with n = (min (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
                                  with origin1 fixnum = (1- (+ start1 n))
                                  with origin2 fixnum = (1- (+ start2 n))
                                  for i fixnum below n
                                  do
                                    (setf (aref simple-vector-1 (the fixnum (- origin1 i)))
                                          (aref simple-vector-2 (the fixnum (- origin2 i))))))
                              (t
                               (loop
                                  for i fixnum below (min (the fixnum (- end1 start1)) (the fixnum (- end2 start2)))
                                  do
                                    (setf (aref simple-vector-1 (the fixnum (+ start1 i)))
                                          (aref simple-vector-2 (the fixnum (+ start2 i)))))))
                        t)
                      nil)))
      (cond ((fast-vector (unsigned-byte 8)))
            ((fast-vector (unsigned-byte 16)))
            ((fast-vector (unsigned-byte 32)))
            ((fast-vector (unsigned-byte 64)))
            ((fast-vector (signed-byte 8)))
            ((fast-vector (signed-byte 16)))
            ((fast-vector (signed-byte 32)))
            ((fast-vector (signed-byte 64)))
            ((fast-vector t))
            ((fast-vector short-float))
            ((fast-vector single-float))
            ((fast-vector double-float))
            ((and (typep sequence-1 'string)
                  (not (array-displacement sequence-1))
                  (typep sequence-2 'string)
                  (not (array-displacement sequence-2)))
             ;; Make sure the destination is at least as wide as the source.
             (let ((storage-1 (sys.int::%complex-array-storage sequence-1))
                   (storage-2 (sys.int::%complex-array-storage sequence-2)))
               (when (not (typep storage-2 '(simple-array (unsigned-byte 8) (*))))
                 ;; Source may contain wide characters.
                 (cond ((typep storage-2 '(simple-array (unsigned-byte 16) (*)))
                        (when (typep storage-1 '(simple-array (unsigned-byte 8) (*)))
                          ;; Expand 8-bit dest to 16-bit
                          (setf (sys.int::%complex-array-storage sequence-1)
                                (make-array (length storage-1)
                                            :element-type '(unsigned-byte 16)
                                            :initial-contents storage-1))))
                       (t
                        ;; source must be 32-bit
                        (when (not (typep storage-1 '(simple-array (unsigned-byte 32) (*))))
                          ;; Expand non-32-bit dest
                          (setf (sys.int::%complex-array-storage sequence-1)
                                (make-array (length storage-1)
                                            :element-type '(unsigned-byte 32)
                                            :initial-contents storage-1)))))))
             (replace (sys.int::%complex-array-storage sequence-1)
                      (sys.int::%complex-array-storage sequence-2)
                      :start1 start1 :end1 end1
                      :start2 start2 :end2 end2))
            (t
             (let ((n (min (- end1 start1) (- end2 start2))))
               (cond (copy-backwards
                      (dotimes (i n)
                        (setf (elt sequence-1 (- (+ start1 n) i 1)) (elt sequence-2 (- (+ start2 n) i 1)))))
                     (t
                      (dotimes (i n)
                        (setf (elt sequence-1 (+ start1 i)) (elt sequence-2 (+ start2 i)))))))))))
  sequence-1)

(declaim (inline replace))
(defun replace (sequence-1 sequence-2 &key (start1 0) end1 (start2 0) end2)
  (replace-known-args sequence-1 sequence-2 start1 end1 start2 end2))

(defmacro object-type-dispatch (object &body body)
  "Fast CASE on (%OBJECT-TAG object)"
  (let ((object-tags (make-array 64 :initial-element nil))
        (targets '())
        (default-form nil)
        (default-target (gensym))
        (block-name (gensym)))
    (loop
       for (keys . forms) in body
       for sym = (gensym)
       do
         (cond ((eql keys t)
                (when default-form
                  (error "Duplicate default forms"))
                (setf default-form `(return-from ,block-name (progn ,@forms))))
               (t
                (when (not (listp keys))
                  (setf keys (list keys)))
                (dolist (key keys)
                  (assert (<= 0 key 64))
                  (when (aref object-tags key)
                    (error "Duplicate key ~S~%" key))
                  (setf (aref object-tags key) sym))
                (push sym targets)
                (push `(return-from ,block-name (progn ,@forms)) targets))))
    `(block ,block-name
       (tagbody
          (%jump-table (%object-tag ,object)
                       ,@(loop
                            for target across object-tags
                            collect `(go ,(or target default-target))))
          ,@(reverse targets)
          ,default-target
          ,default-form))))

(defun fill-known-args (sequence item start end)
  (check-type start (integer 0))
  (prog ((original-sequence sequence))
     (when (not (%value-has-tag-p sequence +tag-object+))
       (go NOT-OBJECT))
     RETRY-COMPLEX-ARRAY
     (macrolet ((fast-vector-fill (type)
                  `(progn
                     (check-type item ,type)
                     (locally
                         (declare (type (simple-array ,type (*)) sequence)
                                  (type ,type item)
                                  (optimize speed (safety 0) (debug 1)))
                       (cond (end
                              (assert (<= start end))
                              (assert (<= end (length sequence))))
                             (t
                              (assert (<= start (length sequence)))
                              (setf end (length sequence))))
                       (locally
                           (declare (type fixnum start end))
                         (loop
                            for i fixnum below (the fixnum (- end start))
                            do
                              (setf (aref sequence (the fixnum (+ start i))) item)))))))
       (object-type-dispatch sequence
         (#.+object-tag-array-t+ (fast-vector-fill t))
         (#.+object-tag-array-unsigned-byte-8+ (fast-vector-fill (unsigned-byte 8)))
         (#.+object-tag-array-unsigned-byte-16+ (fast-vector-fill (unsigned-byte 16)))
         (#.+object-tag-array-unsigned-byte-32+ (fast-vector-fill (unsigned-byte 32)))
         (#.+object-tag-array-unsigned-byte-64+ (fast-vector-fill (unsigned-byte 64)))
         (#.+object-tag-array-signed-byte-8+ (fast-vector-fill (signed-byte 8)))
         (#.+object-tag-array-signed-byte-16+ (fast-vector-fill (signed-byte 16)))
         (#.+object-tag-array-signed-byte-32+ (fast-vector-fill (signed-byte 32)))
         (#.+object-tag-array-signed-byte-64+ (fast-vector-fill (signed-byte 64)))
         (#.+object-tag-array-short-float+ (fast-vector-fill short-float))
         (#.+object-tag-array-single-float+ (fast-vector-fill single-float))
         (#.+object-tag-array-double-float+ (fast-vector-fill double-float))
         ((#.+object-tag-simple-array+ #.+object-tag-array+)
          (when (not (eql (array-rank sequence) 1))
            (error 'type-error :datum sequence :expected-type 'sequence))
          (when (array-displacement sequence)
            (go GENERIC))
          ;; 1D non-displaced array. Adjustable or has a fill-pointer.
          (when (array-has-fill-pointer-p sequence)
            (cond (end
                   (assert (<= end (fill-pointer sequence))))
                  (t
                   (setf end (fill-pointer sequence)))))
          (setf sequence (sys.int::%complex-array-storage sequence))
          (go RETRY-COMPLEX-ARRAY))
         ;; Strings. Check item is a character, expand underlying array as required, fill underlying array with char-as-int.
         ((#.+object-tag-simple-string+ #.+object-tag-string+)
          (when (not (eql (array-rank sequence) 1))
            (error 'type-error :datum sequence :expected-type 'sequence))
          (when (array-displacement sequence)
            (go GENERIC))
          ;; 1D non-displaced array. Adjustable or has a fill-pointer.
          (when (array-has-fill-pointer-p sequence)
            (cond (end
                   (assert (<= end (fill-pointer sequence))))
                  (t
                   (setf end (fill-pointer sequence)))))
          (check-type item character)
          (mezzano.runtime::ensure-string-wide-enough item sequence)
          (setf sequence (sys.int::%complex-array-storage sequence)
                item (char-int item))
          (go RETRY-COMPLEX-ARRAY))
         (t
          (go GENERIC))))
     (return original-sequence)
     NOT-OBJECT
     (when (not (consp sequence))
       (error 'type-error :datum sequence :expected-type 'sequence))
     GENERIC
     (when (not end)
       (setf end (length sequence)))
     (assert (<= 0 start end (length sequence)))
     (dotimes (i (- end start))
       (setf (elt sequence (+ i start)) item))
     (return original-sequence)))

;; Avoid keyword argument setup.
(declaim (inline fill))
(defun fill (sequence item &key (start 0) end)
  (fill-known-args sequence item start end))

(define-compiler-macro map (&whole whole result-type function first-sequence &rest more-sequences)
  (when (not (or (eql result-type 'nil)
                 (equal result-type ''nil)))
    (return-from map whole))
  (let* ((function-sym (gensym "FUNCTION"))
         (n-sequences (1+ (length more-sequences)))
         (seq-vars (loop
                      repeat n-sequences
                      collect (gensym)))
         (n-results (gensym))
         (iter (gensym)))
    `(let* ((,function-sym ,function)
            ,@(loop
                 for seq in (list* first-sequence more-sequences)
                 for var in seq-vars
                 collect (list var seq))
            (,n-results (min ,@(loop
                                  for var in seq-vars
                                  collect `(length ,var)))))
       (loop
          for ,iter below ,n-results
          do (funcall ,function-sym ,@(loop
                                         for var in seq-vars
                                         collect `(elt ,var ,iter)))))))

(defun map (result-type function first-sequence &rest more-sequences)
  (let* ((sequences (cons first-sequence more-sequences))
         (n-results (reduce 'min (mapcar 'length sequences))))
    (flet ((map-body (accum-fn)
             (dotimes (i n-results)
               (funcall accum-fn
                        (apply function
                               (mapcar (lambda (seq)
                                         (elt seq i))
                                       sequences))))))
      (cond ((null result-type)
             ;; No result is accumulated, NIL is returned.
             (map-body (lambda (value) (declare (ignore value)))))
            ((subtypep result-type 'list)
             ;; Generating a list.
             (let* ((head (cons nil nil))
                    (tail head))
               (declare (dynamic-extent head))
               (map-body (lambda (value)
                           (setf (cdr tail) (cons value nil)
                                 tail (cdr tail))))
               (cdr head)))
            ((subtypep result-type 'vector)
             (multiple-value-bind (element-type array-dimensions)
                 (cond ((typep result-type 'class)
                        (values '* '*))
                       ((subtypep result-type 'string)
                        (values 'character '*))
                       (t
                        (parse-array-type (typeexpand result-type))))
               (when (eql element-type '*) (setf element-type 't))
               (let* ((expected-length (cond ((eql array-dimensions '*) n-results)
                                             ((eql (first array-dimensions) '*) n-results)
                                             (t (first array-dimensions))))
                      (result-vector (make-array n-results :element-type (if (eql element-type '*) 't element-type)))
                      (position 0))
                 (unless (eql n-results expected-length)
                   (error 'simple-type-error
                          :expected-type `(eql ,n-results)
                          :datum expected-length
                          :format-control "Result-type restricted to ~D elements, but ~D elements provided"
                          :format-arguments (list expected-length n-results)))
                 (map-body (lambda (value)
                             (setf (aref result-vector position) value)
                             (incf position)))
                 result-vector)))
            (t (error "~S is not a subtype of SEQUENCE." result-type))))))

(defun substitute-if (newitem predicate sequence &key key (start 0) end count from-end)
  (check-type count (or null integer))
  (unless key (setf key 'identity))
  (cond ((and (listp sequence)
              (zerop start)
              (null end)
              (not from-end))
         (mapcar (lambda (x)
                   (if (and (funcall predicate (funcall key x))
                            (or (null count)
                                (>= (decf count) 0)))
                       newitem
                       x))
                 sequence))
        (t
         (unless end (setf end (length sequence)))
         (let ((new-sequence (if (listp sequence)
                                 (copy-list sequence)
                                 (make-array (length sequence)
                                             :element-type (array-element-type sequence)
                                             :initial-contents sequence))))
           (cond (from-end
                  (dotimes (i (- end start))
                    (when (and (funcall predicate (funcall key (elt new-sequence (- end i 1))))
                               (or (null count)
                                   (>= (decf count) 0)))
                      (setf (elt new-sequence (- end i 1)) newitem)))
                  )
                 (t
                  (dotimes (i (- end start))
                    (when (and (funcall predicate (funcall key (elt new-sequence (+ start i))))
                               (or (null count)
                                   (>= (decf count) 0)))
                      (setf (elt new-sequence (+ start i)) newitem)))))
           new-sequence))))

(defun substitute-if-not (newitem predicate sequence &key key (start 0) end count from-end)
  (substitute-if newitem (complement predicate) sequence
                 :key key
                 :start start
                 :end end
                 :count count
                 :from-end from-end))

(defun substitute (newitem olditem sequence &key test test-not key (start 0) end count from-end)
  (with-test-test-not (test test-not)
    (substitute-if newitem
                   (lambda (x) (funcall test olditem x))
                   sequence
                   :key key
                   :start start
                   :end end
                   :count count
                   :from-end from-end)))

(defun nsubstitute-if (newitem predicate sequence &key key (start 0) end from-end count)
  (unless key (setf key 'identity))
  (unless end (setf end (length sequence)))
  (check-type count (or null integer))
  (cond (from-end
         (dotimes (i (- end start))
           (when (and (funcall predicate (funcall key (elt sequence (- end i 1))))
                      (or (null count)
                          (>= (decf count) 0)))
             (setf (elt sequence (- end i 1)) newitem))))
        (t
         (dotimes (i (- end start))
           (when (and (funcall predicate (funcall key (elt sequence (+ start i))))
                      (or (null count)
                          (>= (decf count) 0)))
             (setf (elt sequence (+ start i)) newitem)))))
  sequence)

(defun nsubstitute-if-not (newitem predicate sequence &key key (start 0) end from-end count)
  (nsubstitute-if newitem (complement predicate) sequence
                  :key key
                  :start start
                  :end end
                  :count count
                  :from-end from-end
                  :count count))

(defun nsubstitute (newitem olditem sequence &key test test-not key (start 0) end from-end count)
  (with-test-test-not (test test-not)
    (nsubstitute-if newitem
                    (lambda (x) (funcall test olditem x))
                    sequence
                    :key key
                    :start start
                    :end end
                    :from-end from-end
                    :count count)))

(defun reduce (function sequence &key key (initial-value nil initial-valuep) from-end (start 0) end)
  (check-type key (or null symbol function))
  (unless key (setf key 'identity))
  (when (or (not (eql start 0))
            end)
    (setf sequence (subseq sequence start end)))
  (when from-end
    (setf sequence (reverse sequence)))
  (cond ((eql (length sequence) 0)
         (if initial-valuep
             initial-value
             (funcall function)))
        ((and (eql (length sequence) 1)
              (not initial-valuep))
         (funcall key (elt sequence 0)))
        (initial-valuep
         (let ((x (if from-end
                      (funcall function
                               (funcall key (elt sequence 0))
                               initial-value)
                      (funcall function
                               initial-value
                               (funcall key (elt sequence 0))))))
           (dotimes (i (1- (length sequence)))
             (setf x (if from-end
                         (funcall function
                                  (funcall key
                                           (elt sequence (1+ i)))
                                  x)
                         (funcall function
                                  x
                                  (funcall key
                                           (elt sequence (1+ i)))))))
           x))
        (t (let ((x (if from-end
                        (funcall function
                             (funcall key (elt sequence 1))
                             (funcall key (elt sequence 0)))
                        (funcall function
                             (funcall key (elt sequence 0))
                             (funcall key (elt sequence 1))))))
             (dotimes (i (- (length sequence) 2))
               (setf x (if from-end
                           (funcall function
                                (funcall key
                                         (elt sequence (+ i 2)))
                                x)
                           (funcall function
                                x
                                (funcall key
                                         (elt sequence (+ i 2)))))))
             x))))

(defun copy-seq (x) (subseq x 0))

;; I sure hope so...
(setf (fdefinition 'stable-sort) #'sort)

(defun make-sequence (result-type size &key (initial-element nil initial-element-p))
  (cond ((subtypep result-type 'list)
         (loop for i below size collect initial-element))
        ((subtypep result-type 'vector)
         (if initial-element-p
             (make-array size
                         :element-type (or (coerce-vector-element-type result-type) 't)
                         :initial-element initial-element)
             (make-array size
                         :element-type (or (coerce-vector-element-type result-type) 't))))
        (t (error "Type ~S is not a recognized sequence type." result-type))))

;;; Bastardized SEARCH from SBCL.
(defun search (sequence-1 sequence-2 &key from-end test test-not key (start1 0) (start2 0) end1 end2)
  (setf end1 (or end1 (length sequence-1)))
  (setf end2 (or end2 (length sequence-2)))
  (with-test-test-not (test test-not)
    (setf key (or key #'identity))
    (do ((index2 start2 (1+ index2))
         (terminus (- end2 (- end1 start1)))
         (last-match nil))
        ((> index2 terminus)
         last-match)
      (if (do ((index index2 (1+ index))
               (sub-index start1 (1+ sub-index)))
              ((= sub-index end1) t)
            (if (not (funcall test
                              (funcall key (elt sequence-1 sub-index))
                              (funcall key (elt sequence-2 index))))
                (return nil)))
          (if from-end
              (setf last-match index2)
              (return index2))))))

(defun mismatch (sequence-1 sequence-2 &key from-end test test-not key (start1 0) (start2 0) end1 end2)
  (with-test-test-not (test test-not)
    (setf key (or key #'identity))
    (when from-end
      (setf sequence-1 (reverse sequence-1)
            sequence-2 (reverse sequence-2)))
    (setf end1 (or end1 (length sequence-1)))
    (setf end2 (or end2 (length sequence-2)))
    (dotimes (position (min (- end1 start1)
                            (- end2 start2))
              (when (not (eql (- end1 start1) (- end2 start2)))
                (+ start1 position)))
      (when (not (funcall test
                          (funcall key (elt sequence-1 (+ start1 position)))
                          (funcall key (elt sequence-2 (+ start2 position)))))
        (return (+ start1 position))))))


;;;; MERGE, from SBCL.

;;; Destructively merge LIST-1 with LIST-2 (given that they're already
;;; sorted w.r.t. PRED-FUN on KEY-FUN, giving output sorted the same
;;; way). In the resulting list, elements of LIST-1 are guaranteed to
;;; come before equal elements of LIST-2.
;;;
;;; Enqueues the values in the right order in HEAD's cdr, and returns
;;; the merged list.
(defun merge-lists* (head list1 list2 test key &aux (tail head))
  (declare (type cons head list1 list2)
           (type function test key)
           (optimize speed))
  (let ((key1 (funcall key (car list1)))
        (key2 (funcall key (car list2))))
    (macrolet ((merge-one (l1 k1 l2)
                 `(progn
                    (setf (cdr tail) ,l1
                          tail       ,l1)
                    (let ((rest (cdr ,l1)))
                      (cond (rest
                             (setf ,l1 rest
                                   ,k1 (funcall key (first rest))))
                            (t
                             (setf (cdr ,l1) ,l2)
                             (return (cdr head))))))))
      (loop
       (if (funcall test key2           ; this way, equivalent
                         key1)          ; values are first popped
           (merge-one list2 key2 list1) ; from list1
           (merge-one list1 key1 list2))))))

;;; Convenience wrapper for CL:MERGE
(declaim (inline merge-lists))
(defun merge-lists (list1 list2 test key)
  (cond ((null list1)
         list2)
        ((null list2)
         list1)
        (t
         (let ((head (cons nil nil)))
           (declare (dynamic-extent head))
           (merge-lists* head list1 list2 test key)))))

(defmacro funcall2-using-key (pred key one two)
  `(if ,key
       (funcall ,pred (funcall ,key ,one)
                (funcall ,key  ,two))
       (funcall ,pred ,one ,two)))

;;; MERGE-VECTORS returns a new vector which contains an interleaving
;;; of the elements of VECTOR-1 and VECTOR-2. Elements from VECTOR-2
;;; are chosen only if they are strictly less than elements of
;;; VECTOR-1, (PRED ELT-2 ELT-1), as specified in the manual.
(defmacro merge-vectors (vector-1 length-1 vector-2 length-2
                               result-vector pred key access)
  (let ((result-i (gensym))
        (i (gensym))
        (j (gensym)))
    `(let* ((,result-i 0)
            (,i 0)
            (,j 0))
       (declare (type fixnum ,result-i ,i ,j))
       (loop
        (cond ((= ,i ,length-1)
               (loop (if (= ,j ,length-2) (return))
                     (setf (,access ,result-vector ,result-i)
                           (,access ,vector-2 ,j))
                     (incf ,result-i)
                     (incf ,j))
               (return ,result-vector))
              ((= ,j ,length-2)
               (loop (if (= ,i ,length-1) (return))
                     (setf (,access ,result-vector ,result-i)
                           (,access ,vector-1 ,i))
                     (incf ,result-i)
                     (incf ,i))
               (return ,result-vector))
              ((funcall2-using-key ,pred ,key
                                   (,access ,vector-2 ,j) (,access ,vector-1 ,i))
               (setf (,access ,result-vector ,result-i)
                     (,access ,vector-2 ,j))
               (incf ,j))
              (t (setf (,access ,result-vector ,result-i)
                       (,access ,vector-1 ,i))
                 (incf ,i)))
        (incf ,result-i)))))

(defun merge (result-type sequence1 sequence2 predicate &key key)
  "Merge the sequences SEQUENCE1 and SEQUENCE2 destructively into a
   sequence of type RESULT-TYPE using PREDICATE to order the elements."
  ;; FIXME: This implementation is remarkably inefficient in various
  ;; ways. In decreasing order of estimated user astonishment, I note:
  ;; full calls to SPECIFIER-TYPE at runtime; copying input vectors
  ;; to lists before doing MERGE-LISTS -- WHN 2003-01-05
  (cond
    ((subtypep result-type 'list)
     ;; the VECTOR clause, below, goes through MAKE-SEQUENCE, so
     ;; benefits from the error checking there. Short of
     ;; reimplementing everything, we can't do the same for the LIST
     ;; case, so do relevant length checking here:
     (let ((s1 (coerce sequence1 'list))
           (s2 (coerce sequence2 'list))
           (pred-fun (%coerce-to-callable predicate))
           (key-fun (%coerce-to-callable (or key #'identity))))
       (when (subtypep 'list result-type) ; result-type = 'list
         (return-from merge (merge-lists s1 s2 pred-fun key-fun)))
       (when (and (subtypep 'null result-type)
                  (subtypep result-type 'null))
         (if (and (null s1) (null s2))
             (return-from merge 'nil)
             ;; FIXME: This will break on circular lists (as,
             ;; indeed, will the whole MERGE function).
             (error "MERGE result type has too few elements.")))
       (error "Result type ~S looks a bit like 'LIST, but is too complicated!" result-type)))
    ((subtypep result-type 'vector)
     (let* ((vector-1 (coerce sequence1 'vector))
            (vector-2 (coerce sequence2 'vector))
            (length-1 (length vector-1))
            (length-2 (length vector-2))
            (result (make-sequence result-type (+ length-1 length-2))))
       (declare (type vector vector-1 vector-2)
                (type fixnum length-1 length-2))
       (if (and (simple-vector-p result)
                (simple-vector-p vector-1)
                (simple-vector-p vector-2))
           (merge-vectors vector-1 length-1 vector-2 length-2
                          result predicate key svref)
           (merge-vectors vector-1 length-1 vector-2 length-2
                          result predicate key aref))))
    (t (error "Unknown MERGE result-type ~S." result-type))))

(defun map-into (result-sequence function &rest sequences)
  (dotimes (i (reduce #'min (mapcar #'length sequences)
                      :initial-value (length result-sequence)))
    (setf (elt result-sequence i)
          (apply function
                 (mapcar (lambda (s) (elt s i))
                         sequences))))
  result-sequence)
