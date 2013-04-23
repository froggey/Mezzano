;;;; This file contains versions of the sequence functions that only
;;;; operate on lists. It is loaded during bootstrap and the functions
;;;; are replaced with the full versions later.

(defun some (predicate list)
  (dolist (i list)
    (let ((result (funcall predicate i)))
      (when result (return result)))))

(defun every (predicate list)
  (dolist (i list t)
    (unless (funcall predicate i)
      (return nil))))

(defun notevery (predicate list)
  (not (every predicate list)))

(defun notany (predicate list)
  (not (some predicate list)))

(defun length (list)
  (or (list-length list)
      (error "LIST is circular.")))

(defun set-difference (list-1 list-2)
  (let ((result '()))
    (dolist (e list-1)
      (when (not (member e list-2))
	(setq result (cons e result))))
    result))

(defun union (list-1 list-2)
  (let ((result (copy-list list-1)))
    (dolist (e list-2)
      (when (not (member e list-1))
	(setq result (cons e result))))
    result))

(defun intersection (list-1 list-2)
  (when list-1
    (if (member (first list-1) list-2)
        (cons (first list-1) (intersection (rest list-1) list-2))
        (intersection (rest list-1) list-2))))

(defun find (item list &key key test test-not)
  (when (and test test-not)
    (error "TEST and TEST-NOT specified."))
  (when test-not
    (setq test (complement test)))
  (unless test
    (setq test #'eql))
  (unless key
    (setq key #'identity))
  (dolist (i list)
    (when (funcall test item (funcall key i))
      (return i))))

(defun remove (item list &key key test test-not)
  (when (and test test-not)
    (error "TEST and TEST-NOT specified."))
  (when test-not
    (setq test (complement test)))
  (unless test
    (setq test #'eql))
  (unless key
    (setq key #'identity))
  (let* ((new-list (cons nil nil))
	 (tail new-list))
    (dolist (i list (cdr new-list))
      (unless (funcall test item (funcall key i))
	(funcall #'(setf cdr) (cons i nil) tail)
	(setq tail (cdr tail))))))

(defun remove-if (test sequence &key key)
  (unless key (setf key #'identity))
  (let* ((list (cons nil nil))
	 (tail list))
    (dolist (e sequence (cdr list))
      (when (not (funcall test (funcall key e)))
	(funcall #'(setf cdr) (cons e nil) tail)
	(setq tail (cdr tail))))))

(defun delete (item sequence &key key test test-not)
  (remove item sequence :key key :test test :test-not test-not))

(defun delete-if (test sequence &key key)
  (remove-if test sequence :key key))
