;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Basic cons-related functions.
;;;; This file is loaded at bootstrap time and once more after the compiler
;;;; is loaded, so it should only contain final versions of the functions.

(in-package :sys.int)

(declaim (inline caar cadr cdar cddr
		 (setf caar) (setf cadr)
		 (setf cdar) (setf cddr)))
(defun caar (x)
  (car (car x)))
(defun cadr (x)
  (car (cdr x)))
(defun cdar (x)
  (cdr (car x)))
(defun cddr (x)
  (cdr (cdr x)))
(defun (setf caar) (value x)
  (setf (car (car x)) value))
(defun (setf cadr) (value x)
  (setf (car (cdr x)) value))
(defun (setf cdar) (value x)
  (setf (cdr (car x)) value))
(defun (setf cddr) (value x)
  (setf (cdr (cdr x)) value))

(defun caaar (x)
  (car (car (car x))))
(defun caadr (x)
  (car (car (cdr x))))
(defun cadar (x)
  (car (cdr (car x))))
(defun caddr (x)
  (car (cdr (cdr x))))
(defun cdaar (x)
  (cdr (car (car x))))
(defun cdadr (x)
  (cdr (car (cdr x))))
(defun cddar (x)
  (cdr (cdr (car x))))
(defun cdddr (x)
  (cdr (cdr (cdr x))))
(defun (setf caaar) (value x)
  (setf (car (car (car x))) value))
(defun (setf caadr) (value x)
  (setf (car (car (cdr x))) value))
(defun (setf cadar) (value x)
  (setf (car (cdr (car x))) value))
(defun (setf caddr) (value x)
  (setf (car (cdr (cdr x))) value))
(defun (setf cdaar) (value x)
  (setf (cdr (car (car x))) value))
(defun (setf cdadr) (value x)
  (setf (cdr (car (cdr x))) value))
(defun (setf cddar) (value x)
  (setf (cdr (cdr (car x))) value))
(defun (setf cdddr) (value x)
  (setf (cdr (cdr (cdr x))) value))

(defun caaaar (x)
  (car (car (car (car x)))))
(defun caaadr (x)
  (car (car (car (cdr x)))))
(defun caadar (x)
  (car (car (cdr (car x)))))
(defun caaddr (x)
  (car (car (cdr (cdr x)))))
(defun cadaar (x)
  (car (cdr (car (car x)))))
(defun cadadr (x)
  (car (cdr (car (cdr x)))))
(defun caddar (x)
  (car (cdr (cdr (car x)))))
(defun cadddr (x)
  (car (cdr (cdr (cdr x)))))
(defun cdaaar (x)
  (cdr (car (car (car x)))))
(defun cdaadr (x)
  (cdr (car (car (cdr x)))))
(defun cdadar (x)
  (cdr (car (cdr (car x)))))
(defun cdaddr (x)
  (cdr (car (cdr (cdr x)))))
(defun cddaar (x)
  (cdr (cdr (car (car x)))))
(defun cddadr (x)
  (cdr (cdr (car (cdr x)))))
(defun cdddar (x)
  (cdr (cdr (cdr (car x)))))
(defun cddddr (x)
  (cdr (cdr (cdr (cdr x)))))
(defun (setf caaaar) (value x)
  (setf (car (car (car (car x)))) value))
(defun (setf caaadr) (value x)
  (setf (car (car (car (cdr x)))) value))
(defun (setf caadar) (value x)
  (setf (car (car (cdr (car x)))) value))
(defun (setf caaddr) (value x)
  (setf (car (car (cdr (cdr x)))) value))
(defun (setf cadaar) (value x)
  (setf (car (cdr (car (car x)))) value))
(defun (setf cadadr) (value x)
  (setf (car (cdr (car (cdr x)))) value))
(defun (setf caddar) (value x)
  (setf (car (cdr (cdr (car x)))) value))
(defun (setf cadddr) (value x)
  (setf (car (cdr (cdr (cdr x)))) value))
(defun (setf cdaaar) (value x)
  (setf (cdr (car (car (car x)))) value))
(defun (setf cdaadr) (value x)
  (setf (cdr (car (car (cdr x)))) value))
(defun (setf cdadar) (value x)
  (setf (cdr (car (cdr (car x)))) value))
(defun (setf cdaddr) (value x)
  (setf (cdr (car (cdr (cdr x)))) value))
(defun (setf cddaar) (value x)
  (setf (cdr (cdr (car (car x)))) value))
(defun (setf cddadr) (value x)
  (setf (cdr (cdr (car (cdr x)))) value))
(defun (setf cdddar) (value x)
  (setf (cdr (cdr (cdr (car x)))) value))
(defun (setf cddddr) (value x)
  (setf (cdr (cdr (cdr (cdr x)))) value))

(declaim (inline first second
		 (setf first)
		 (setf second)))
(defun first (x)
  (car x))
(defun second (x)
  (car (cdr x)))
(defun third (x)
  (car (cdr (cdr x))))
(defun fourth (x)
  (car (cdr (cdr (cdr x)))))
(defun fifth (x)
  (car (cdr (cdr (cdr (cdr x))))))
(defun sixth (x)
  (car (cdr (cdr (cdr (cdr (cdr x)))))))
(defun seventh (x)
  (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))
(defun eighth (x)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))
(defun ninth (x)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))
(defun tenth (x)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))
(defun (setf first) (value x)
  (setf (car x) value))
(defun (setf second) (value x)
  (setf (car (cdr x)) value))
(defun (setf third) (value x)
  (setf (car (cdr (cdr x))) value))
(defun (setf fourth) (value x)
  (setf (car (cdr (cdr (cdr x)))) value))
(defun (setf fifth) (value x)
  (setf (car (cdr (cdr (cdr (cdr x))))) value))
(defun (setf sixth) (value x)
  (setf (car (cdr (cdr (cdr (cdr (cdr x)))))) value))
(defun (setf seventh) (value x)
  (setf (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))) value))
(defun (setf eighth) (value x)
  (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))) value))
(defun (setf ninth) (value x)
  (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))) value))
(defun (setf tenth) (value x)
  (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))) value))

(declaim (inline rest (setf rest)))
(defun rest (list)
  (cdr list))
(defun (setf rest) (value list)
  (setf (cdr list) value))

(declaim (inline rplaca rplacd))
(defun rplaca (cons object)
  "Replace the car of CONS with OBJECT, returning CONS."
  (setf (car cons) object)
  cons)
(defun rplacd (cons object)
  "Replace the cdr of CONS with OBJECT, returning CONS."
  (setf (cdr cons) object)
  cons)

(declaim (inline atom))
(defun atom (object)
  "Returns true if OBJECT is of type atom; otherwise, returns false."
  (not (consp object)))

(declaim (inline listp))
(defun listp (object)
  "Returns true if OBJECT is of type (OR NULL CONS); otherwise, returns false."
  (or (null object) (consp object)))

(defun list-length (list)
  "Returns the length of LIST if list is a proper list. Returns NIL if LIST is a circular list."
  ;; Implementation from the HyperSpec
  (do ((n 0 (+ n 2))             ; Counter.
       (fast list (cddr fast))   ; Fast pointer: leaps by 2.
       (slow list (cdr slow)))   ; Slow pointer: leaps by 1.
      (nil)
    ;; If fast pointer hits the end, return the count.
    (when (endp fast) (return n))
    (when (endp (cdr fast)) (return (1+ n)))
    ;; If fast pointer eventually equals slow pointer,
    ;;  then we must be stuck in a circular list.
    (when (and (eq fast slow) (> n 0)) (return nil))))

(defun dotted-list-length (list)
  "Returns the length of LIST if list is a proper list. Returns NIL if LIST is a circular list."
  ;; Implementation from the HyperSpec
  (do ((n 0 (+ n 2))             ; Counter.
       (fast list (cddr fast))   ; Fast pointer: leaps by 2.
       (slow list (cdr slow)))   ; Slow pointer: leaps by 1.
      (nil)
    ;; If fast pointer hits the end, return the count.
    (when (atom fast) (return n))
    (when (atom (cdr fast)) (return (1+ n)))
    ;; If fast pointer eventually equals slow pointer,
    ;;  then we must be stuck in a circular list.
    (when (and (eq fast slow) (> n 0)) (return nil))))

(defun last (list)
  (do ((i list (cdr i)))
      ((null (cdr i))
       i)))

(defun butlast (list &optional (n 1))
  (do* ((result (cons nil nil))
	(tail result (cdr tail))
	(itr list (cdr itr)))
       ((<= (length itr) n)
	(cdr result))
    (setf (cdr tail) (cons (car itr) nil))))

(defun nthcdr (n list)
  (dotimes (i n list)
    (setf list (cdr list))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun (setf nth) (value n list)
  (setf (car (nthcdr n list)) value))

(defun append (&rest lists)
  (do* ((head (cons nil nil))
	(tail head)
	(i lists (cdr i)))
       ((null (cdr i))
        (setf (cdr tail) (car i))
	(cdr head))
    (dolist (elt (car i))
      (setf (cdr tail) (cons elt nil)
            tail (cdr tail)))))

(defun nconc (&rest lists)
  (let ((start (do ((x lists (cdr x)))
		   ((or (null x) (car x)) x))))
    (when start
      (do ((list (last (car start)) (last list))
	   (i (cdr start) (cdr i)))
	  ((null (cdr i))
	   (setf (cdr list) (car i))
	   (car start))
	(setf (cdr list) (car i))))))

(defun reverse (sequence)
  (if (listp sequence)
      (let ((result '()))
        (dolist (elt sequence result)
          (setf result (cons elt result))))
      (nreverse (make-array (length sequence)
                            :element-type (array-element-type sequence)
                            :initial-contents sequence))))

(defun nreverse (sequence)
  (if (vectorp sequence)
      (dotimes (i (truncate (length sequence) 2) sequence)
             (rotatef (aref sequence i) (aref sequence (- (length sequence) 1 i))))
      (reverse sequence)))

;; The following functional equivalences are true, although good implementations
;; will typically use a faster algorithm for achieving the same effect:
(defun revappend (list tail)
  (nconc (reverse list) tail))
(defun nreconc (list tail)
  (nconc (nreverse list) tail))

(defun single-mapcar (function list)
  (do* ((result (cons nil nil))
	(tail result (cdr tail))
	(itr list (cdr itr)))
       ((null itr)
	(cdr result))
    (setf (cdr tail) (cons (funcall function (car itr)) nil))))

(defun mapcar (function list &rest more-lists)
  (if more-lists
      (do* ((lists (cons list more-lists))
	    (result (cons nil nil))
	    (tail result (cdr tail)))
	   (nil)
	(do* ((call-list (cons nil nil))
	      (call-tail call-list (cdr call-tail))
	      (itr lists (cdr itr)))
	     ((null itr)
              (setf (cdr tail) (cons (apply function (cdr call-list)) nil)))
	  (when (null (car itr))
	    (return-from mapcar (cdr result)))
          (setf (cdr call-tail) (cons (caar itr) nil)
                (car itr) (cdar itr))))
      (single-mapcar function list)))

(defun mapc (function list &rest more-lists)
  (apply 'mapcar function list more-lists)
  list)

(defun maplist (function list &rest more-lists)
  (when (null list)
    (return-from maplist nil))
  (dolist (l more-lists)
    (when (null l)
      (return-from maplist nil)))
  (do* ((lists (cons list more-lists))
	(result (cons nil nil))
	(tail result (cdr tail)))
       (nil)
    (setf (cdr tail) (cons (apply function lists) nil))
    (do ((itr lists (cdr itr)))
	((null itr))
      (setf (car itr) (cdar itr))
      (when (null (car itr))
	(return-from maplist (cdr result))))))

(defun mapcan (function list &rest more-lists)
  (do* ((lists (cons list more-lists))
	(result (cons nil nil))
	(tail result (last tail)))
       (nil)
    (do* ((call-list (cons nil nil))
	  (call-tail call-list (cdr call-tail))
	  (itr lists (cdr itr)))
	 ((null itr)
	  (setf (cdr tail) (apply function (cdr call-list))))
      (when (null (car itr))
	(return-from mapcan (cdr result)))
      (setf (cdr call-tail) (cons (caar itr) nil))
      (setf (car itr) (cdar itr)))))

(defun getf (plist indicator &optional default)
  (do ((i plist (cddr i)))
      ((null i) default)
    (when (eq (car i) indicator)
      (return (cadr i)))))

(defun %putf (plist indicator value)
  (do ((i plist (cddr i)))
      ((null i)
       (list* indicator value plist))
    (when (eql (car i) indicator)
      (setf (cadr i) value)
      (return plist))))

(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (temps vals stores
                              store-form access-form)
      (get-setf-expansion place env);Get setf expansion for place.
    (let ((indicator-temp (gensym))
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores))) ;Temp var for int to store.
      (when (cdr stores) (error "Can't expand this."))
      ;; Return the setf expansion for LDB as five values.
      (values (list* indicator-temp temps)       ;Temporary variables.
              (list* indicator vals)     ;Value forms.
              (list store)             ;Store variables.
              `(let ((,stemp (%putf ,access-form ,indicator-temp ,store)))
                 ,default
                 ,store-form
                 ,store)               ;Storing form.
              `(getf ,access-form ,indicator-temp ,default))))) ;Accessing form.

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun (setf get) (value symbol indicator &optional default)
  (declare (ignore default))
  (setf (getf (symbol-plist symbol) indicator) value))

(declaim (inline assoc))
(defun assoc (item alist &key key test test-not)
  (when (and test test-not)
    (error "TEST and TEST-NOT specified."))
  (when test-not
    (setf test (complement test-not)))
  (unless test
    (setf test 'eql))
  (unless key
    (setf key 'identity))
  (dolist (i alist)
    (when (funcall test item (funcall key (car i)))
      (return i))))

(declaim (inline member))
(defun member (item list &key key test test-not)
  (when (and test test-not)
    (error "TEST and TEST-NOT specified."))
  (when test-not
    (setf test (complement test-not)))
  (unless test
    (setf test 'eql))
  (unless key
    (setf key 'identity))
  (do ((i list (cdr i)))
      ((endp i))
    (when (funcall test item (funcall key (car i)))
      (return i))))

(defun member-if (predicate list)
  (when list
    (if (funcall predicate (first list))
        list
        (member-if predicate (rest list)))))

(defun get-properties (plist indicator-list)
  (do ((i plist (cddr i)))
      ((null i)
       (values nil nil nil))
    (when (member (car i) indicator-list)
      (return (values (first i) (second i) i)))))

(defun list* (object &rest objects)
  (declare (dynamic-extent object))
  (if objects
      (do* ((i objects (cdr i))
	    (result (cons object nil))
	    (tail result))
	   ((null (cdr i))
	    (setf (cdr tail) (car i))
	    result)
	(setf (cdr tail) (cons (car i) nil)
	      tail (cdr tail)))
      object))

(defun make-list (size &key initial-element)
  (check-type size (integer 0))
  (let ((result '()))
    (dotimes (i size)
      (push initial-element result))
    result))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun sublis (alist tree &key) ; key test test-not
  (flet ((sublis-one (thing)
           (if (consp thing)
               (sublis alist thing)
               (let ((x (assoc thing alist)))
                 (if x (cdr x) thing)))))
    (cons (sublis-one (car tree))
          (sublis-one (cdr tree)))))

(defun pairlis (keys data &optional alist)
  (assert (or (and keys data)
              (and (not keys) (not data))))
  (if keys
      (cons (cons (car keys) (car data))
            (pairlis (cdr keys) (cdr data) alist))
      alist))

(defun ldiff (list object)
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
    (when (eql object list)
      (return (nreverse r)))))

(defun tailp (object list)
   (do ((list list (cdr list)))
       ((atom list) (eql list object))
      (if (eql object list)
          (return t))))

(defun adjoin (item list &key key test test-not)
  (if (member item list :key key :test test :test-not test-not)
      list
      (cons item list)))

(defun subst (old new tree &key (test #'eql))
  (cond ((funcall test old tree)
         new)
        ((atom tree) tree)
        (t (let ((a (subst old new (car tree) :test test))
                 (d (subst old new (cdr tree) :test test)))
             (if (and (funcall test a (car tree))
                      (funcall test d (cdr tree)))
                 tree
                 (cons a d))))))

(declaim (inline endp))
(defun endp (list)
  (cond ((null list) t)
        ((consp list) nil)
        (t (error 'type-error
                  :datum list
                  :expected-type 'list))))

(defun copy-tree (tree)
  (if (consp tree)
      (cons (copy-tree (car tree))
            (copy-tree (cdr tree)))
      tree))
