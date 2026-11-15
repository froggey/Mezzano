;;;; CONS and LIST related functions

(in-package :mezzano.internals)

(declaim (inline caar cadr cdar cddr
                 (setf caar) (setf cadr)
                 (setf cdar) (setf cddr)))
;; Define CAAR/CADR/etc to length 4
(macrolet ((def (length)
             `(progn
                ,@(loop
                     for i below (expt 2 length)
                     for perm = (loop
                                   for j below length
                                   collect (if (logbitp j i) #\D #\A))
                     collect (let ((name (intern (format nil "C~{~A~}R" (reverse perm)) :cl))
                                   (body (loop
                                            with result = 'list
                                            for ch in perm
                                            do (setf result (list (ecase ch (#\D 'cdr) (#\A 'car)) result))
                                            finally (return result))))
                               `(progn
                                  (defun* ,name (list) ,body)
                                  (defun* (setf ,name) (new-object list) (setf ,body new-object))
                                  (defun* (cas ,name) (old new list) (cas ,body old new))))))))
  (def 2)
  (def 3)
  (def 4))

(declaim (inline first second
                 (setf first)
                 (setf second)))
;; Define FIRST, SECOND, ..., TENTH.
(macrolet ((def (n)
             (let ((name (intern (format nil "~:@(~:R~)" n) :cl))
                   (forms (loop
                             with result = `list
                             repeat (1- n)
                             do (setf result `(cdr ,result))
                             finally (return `(car ,result)))))
               `(progn
                  (defun* ,name (list) ,forms)
                  (defun* (setf ,name) (new-object list) (setf ,forms new-object))
                  (defun* (cas ,name) (old new list) (cas ,forms old new)))))
           (def-all ()
             `(progn
                ,@(loop for i from 1 to 10 collect `(def ,i)))))
  (def-all))

(declaim (inline rest (setf rest) (cas rest)))
(defun rest (list)
  (cdr list))

(defun (setf rest) (new-tail list)
  (setf (cdr list) new-tail))

(defun (cas rest) (old new so fo list)
  (cas (cdr list) old new so fo))

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

(defun last (list &optional (n 1))
  (check-type n (integer 0))
  (do ((l list (cdr l))
       (r list)
       (i 0 (+ i 1)))
      ((atom l)
       r)
    (if (>= i n)
        (pop r))))

(defun butlast (list &optional (n 1))
  (do* ((result (cons nil nil))
        (tail result (cdr tail))
        (itr list (cdr itr)))
       ((<= (dotted-list-length itr) n)
        (cdr result))
    (declare (dynamic-extent result))
    (setf (cdr tail) (cons (car itr) nil))))

(defun nthcdr (n list)
  (check-type n (integer 0))
  (dotimes (i n list)
    (setf list (cdr list))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun (setf nth) (value n list)
  (setf (car (nthcdr n list)) value))

(defun (cas nth) (old new so fo n list)
  (cas (car (nthcdr n list)) old new so fo))

(defun append (&rest lists)
  (declare (dynamic-extent lists))
  (do* ((head (cons nil nil))
        (tail head)
        (i lists (cdr i)))
       ((null (cdr i))
        (setf (cdr tail) (car i))
        (cdr head))
    (declare (dynamic-extent head))
    (dolist (elt (car i))
      (setf (cdr tail) (cons elt nil)
            tail (cdr tail)))))

(defun nconc (&rest lists)
  (declare (dynamic-extent lists))
  (let ((start (do ((x lists (cdr x)))
                   ((or (null x) (car x)) x))))
    (when start
      (do ((list (last (car start)) (last list))
           (i (cdr start) (cdr i)))
          ((null (cdr i))
           (when (consp list)
             (setf (cdr list) (car i)))
           (car start))
        (setf (cdr list) (car i))))))

(defun reverse (sequence)
  (etypecase sequence
    (list
     (let ((result '()))
       (dolist (elt sequence result)
         (setf result (cons elt result)))))
    (vector
     (nreverse (make-array (length sequence)
                           :element-type (array-element-type sequence)
                           :initial-contents sequence)))))

(defun nreverse (sequence)
  (etypecase sequence
    (vector
     (dotimes (i (truncate (length sequence) 2) sequence)
       (rotatef (aref sequence i) (aref sequence (- (length sequence) 1 i)))))
    (list
     (loop
        with result = '()
        while (not (endp sequence))
        do (push (pop sequence) result)
        finally (return result)))))

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
    (declare (dynamic-extent result))
    (setf (cdr tail) (cons (funcall function (car itr)) nil))))

(define-compiler-macro mapcar (function list &rest more-lists)
  (let* ((fn-sym (gensym "FN"))
         (result-sym (gensym "RESULT"))
         (tail-sym (gensym "TAIL"))
         (tmp-sym (gensym "TMP"))
         (all-lists (list* list more-lists))
         (iterators (mapcar (lambda (x) (declare (ignore x)) (gensym))
                            all-lists)))
    `(do* ((,fn-sym ,function)
           (,result-sym (cons nil nil))
           (,tail-sym ,result-sym)
           ,@(mapcar (lambda (name form)
                       (list name form `(cdr ,name)))
                     iterators all-lists))
          ((or ,@(mapcar (lambda (name) `(null ,name))
                         iterators))
           (cdr ,result-sym))
       (declare (dynamic-extent ,result-sym))
       (let ((,tmp-sym (cons (funcall ,fn-sym ,@(mapcar (lambda (name) `(car ,name))
                                                        iterators))
                             nil)))
         (setf (cdr ,tail-sym) ,tmp-sym
               ,tail-sym ,tmp-sym)))))

(defun mapcar (function list &rest more-lists)
  (declare (dynamic-extent more-lists))
  (if more-lists
      (do* ((lists (cons list more-lists))
            (result (cons nil nil))
            (tail result (cdr tail)))
           (nil)
        (declare (dynamic-extent lists result))
        (do* ((call-list (cons nil nil))
              (call-tail call-list (cdr call-tail))
              (itr lists (cdr itr)))
             ((null itr)
              (setf (cdr tail) (cons (apply function (cdr call-list)) nil)))
          (declare (dynamic-extent call-list))
          (when (null (car itr))
            (return-from mapcar (cdr result)))
          (setf (cdr call-tail) (cons (caar itr) nil)
                (car itr) (cdar itr))))
      (single-mapcar function list)))

(define-compiler-macro mapc (function list &rest more-lists)
  (let* ((fn-sym (gensym "FN"))
         (result-sym (gensym "RESULT"))
         (all-lists (list* list more-lists))
         (iterators (mapcar (lambda (x) (declare (ignore x)) (gensym))
                            all-lists)))
    `(do* ((,fn-sym ,function)
           ,@(mapcar (lambda (name form)
                       (list name form `(cdr ,name)))
                     iterators all-lists)
           (,result-sym ,(first iterators)))
          ((or ,@(mapcar (lambda (name) `(null ,name))
                         iterators))
           ,result-sym)
       (funcall ,fn-sym ,@(mapcar (lambda (name) `(car ,name))
                                  iterators)))))

(defun mapc (function list &rest more-lists)
  (apply 'mapcar function list more-lists)
  list)

(define-compiler-macro maplist (function list &rest more-lists)
  (let* ((fn-sym (gensym "FN"))
         (result-sym (gensym "RESULT"))
         (tail-sym (gensym "TAIL"))
         (tmp-sym (gensym "TMP"))
         (all-lists (list* list more-lists))
         (iterators (mapcar (lambda (x) (declare (ignore x)) (gensym))
                            all-lists)))
    `(do* ((,fn-sym ,function)
           (,result-sym (cons nil nil))
           (,tail-sym ,result-sym)
           ,@(mapcar (lambda (name form)
                       (list name form `(cdr ,name)))
                     iterators all-lists))
          ((or ,@(mapcar (lambda (name) `(null ,name))
                         iterators))
           (cdr ,result-sym))
       (declare (dynamic-extent ,result-sym))
       (let ((,tmp-sym (cons (funcall ,fn-sym ,@iterators) nil)))
         (setf (cdr ,tail-sym) ,tmp-sym
               ,tail-sym ,tmp-sym)))))

(defun maplist (function list &rest more-lists)
  (declare (dynamic-extent more-lists))
  (when (null list)
    (return-from maplist nil))
  (dolist (l more-lists)
    (when (null l)
      (return-from maplist nil)))
  (do* ((lists (cons list more-lists))
        (result (cons nil nil))
        (tail result (cdr tail)))
       (nil)
    (declare (dynamic-extent lists result))
    (setf (cdr tail) (cons (apply function lists) nil))
    (do ((itr lists (cdr itr)))
        ((null itr))
      (setf (car itr) (cdar itr))
      (when (null (car itr))
        (return-from maplist (cdr result))))))

(defun mapl (function list &rest more-lists)
  (apply #'maplist function list more-lists)
  list)

(defun mapcan (function list &rest more-lists)
  (do* ((lists (cons list more-lists))
        (result (cons nil nil))
        (tail result (last tail)))
       (nil)
    (declare (dynamic-extent lists result))
    (do* ((call-list (cons nil nil))
          (call-tail call-list (cdr call-tail))
          (itr lists (cdr itr)))
         ((null itr)
          (setf (cdr tail) (apply function (cdr call-list))))
      (declare (dynamic-extent call-list))
      (when (null (car itr))
        (return-from mapcan (cdr result)))
      (setf (cdr call-tail) (cons (caar itr) nil))
      (setf (car itr) (cdar itr)))))

(defun mapcon (function list &rest more-lists)
  (apply #'nconc (apply #'maplist function list more-lists)))

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

(defun %remf (plist indicator)
  (cond ((endp plist)
         (values nil nil))
        ((eql (first plist) indicator)
         (values (cddr plist) t))
        (t
         ;; Not empty and not first entry.
         (do ((i plist (cddr i)))
             ((null (cddr i))
              (values plist nil))
           (when (eql (third i) indicator)
             (setf (cddr i) (cddddr i))
             (return (values plist t)))))))

(defmacro remf (place indicator &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion place env)
    (let ((result (gensym)))
      (when (cdr stores)
        (error "Can't expand this"))
      `(let* (,@(mapcar #'list temps vals))
         (multiple-value-bind (,(first stores) ,result)
             (%remf ,access-form ,indicator)
           ,store-form
           ,result)))))

(declaim (inline assoc-if assoc-if-not assoc))
(defun assoc-if (predicate alist &key key)
  (unless key
    (setf key 'identity))
  (dolist (i alist)
    (when (and i
               (funcall predicate (funcall key (car i))))
      (return i))))

(defun assoc-if-not (predicate alist &key key)
  (assoc-if (complement predicate) alist
            :key key))

(defun assoc (item alist &key key test test-not)
  (with-test-test-not (test test-not)
    (assoc-if (lambda (x) (funcall test item x))
              alist
              :key key)))

(declaim (inline member-if member-if-not member))
(defun member-if (predicate list &key key)
  (when (not key)
    (setf key 'identity))
  (do ((i list (cdr i)))
      ((endp i))
    (when (funcall predicate (funcall key (car i)))
      (return i))))

(defun member-if-not (predicate list &key key)
  (member-if (complement predicate) list
             :key key))

(defun member (item list &key key test test-not)
  (with-test-test-not (test test-not)
    (member-if (lambda (x) (funcall test item x))
               list
               :key key)))

(define-compiler-macro member (&whole whole item list &key key test test-not)
  (cond
    ((or (not (and (consp list)
                   (consp (cdr list))
                   (null (cddr list))
                   (eql (first list) 'quote)
                   (listp (second list))))
         ;; Bail out when keyword arguments are involved.
         ;; Tricky to keep order of evaluation correct.
         key
         test
         test-not)
     ;; Skip if the list isn't a quoted proper list.
     whole)
    ((eql (second list) '())
     ''nil)
    (t
     (let ((item-sym (gensym "ITEM")))
       `(let ((,item-sym ,item))
          (if (eql ,item-sym ',(first (second list)))
              ',(second list)
              (member ,item-sym ',(rest (second list)))))))))

(defun get-properties (plist indicator-list)
  (do ((i plist (cddr i)))
      ((null i)
       (values nil nil nil))
    (when (member (car i) indicator-list)
      (return (values (first i) (second i) i)))))

(defun list* (object &rest objects)
  (declare (dynamic-extent objects))
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

(defun sublis (alist tree &key key test test-not)
  (with-test-test-not (test test-not)
    (unless key
      (setf key 'identity))
    (labels ((sublis-one (thing)
               (let ((x (assoc (funcall key thing) alist :test test)))
                 (cond (x
                        (cdr x))
                       ((consp thing)
                        (cons (sublis-one (car thing))
                              (sublis-one (cdr thing))))
                       (t
                        thing)))))
      (sublis-one tree))))

(defun pairlis (keys data &optional alist)
  (assert (or (and keys data)
              (and (not keys) (not data))))
  (if keys
      (cons (cons (car keys) (car data))
            (pairlis (cdr keys) (cdr data) alist))
      alist))

(defun copy-alist (alist)
  (loop
     for entry in alist
     collect (if (consp entry)
                 (cons (car entry) (cdr entry))
                 entry)))

(defun ldiff (list object)
  (check-type list list)
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
    (when (eql object list)
      (return (nreverse r)))))

(defun tailp (object list)
  (check-type list list)
  (do ((list list (cdr list)))
       ((atom list) (eql list object))
      (if (eql object list)
          (return t))))

(defun adjoin (item list &key key test test-not)
  (setf key (or key #'identity))
  (if (member (funcall key item) list :key key :test test :test-not test-not)
      list
      (cons item list)))

(defun subst-if (new predicate tree &key key)
  (setf key (or key #'identity))
  (cond ((funcall predicate (funcall key tree))
         new)
        ((atom tree)
         tree)
        (t (let ((a (subst-if new predicate (car tree) :key key))
                 (d (subst-if new predicate (cdr tree) :key key)))
             (if (and (eql a (car tree))
                      (eql d (cdr tree)))
                 tree
                 (cons a d))))))

(defun subst-if-not (new predicate tree &key key)
  (subst-if new
            (complement predicate)
            tree
            :key key))

(defun subst (new old tree &key key test test-not)
  (with-test-test-not (test test-not)
    (subst-if new
              (lambda (x) (funcall test old x))
              tree
              :key key)))

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

(defun rassoc-if (predicate alist &key key)
  (unless key
    (setf key 'identity))
  (dolist (i alist)
    (when (and i
               (funcall predicate (funcall key (cdr i))))
      (return i))))

(defun rassoc-if-not (predicate alist &key key)
  (rassoc-if (complement predicate) alist
             :key key))

(defun rassoc (item alist &key key test test-not)
  (with-test-test-not (test test-not)
    (rassoc-if (lambda (x) (funcall test item x))
               alist
               :key key)))

(declaim (inline set-difference nset-difference
                 union nunion
                 intersection nintersection
                 set-exclusive-or nset-exclusive-or))

(defun set-difference (list-1 list-2 &key key test test-not)
  (when (not key)
    (setf key 'identity))
  (check-type list-1 list)
  (check-type list-2 list)
  (check-test-test-not test test-not)
  (cond (list-2
         (let ((result '()))
           (dolist (e list-1)
             (when (not (member (funcall key e) list-2 :key key :test test :test-not test-not))
               (push e result)))
           result))
        (t
         list-1)))

(defun nset-difference (list-1 list-2 &key key test test-not)
  (set-difference list-1 list-2 :key key :test test :test-not test-not))

(defun union (list-1 list-2 &key key test test-not)
  (when (not key)
    (setf key 'identity))
  (check-type list-1 list)
  (check-type list-2 list)
  (check-test-test-not test test-not)
  (cond ((and list-1 list-2)
         (let ((result list-2))
           (dolist (e list-1)
             (when (not (member (funcall key e) list-2 :key key :test test :test-not test-not))
               (push e result)))
           result))
        (list-1)
        (list-2)))

(defun nunion (list-1 list-2 &key key test test-not)
  (union list-1 list-2 :key key :test test :test-not test-not))

(defun intersection (list-1 list-2 &key key test test-not)
  (when (not key)
    (setf key 'identity))
  (check-type list-1 list)
  (check-type list-2 list)
  (check-test-test-not test test-not)
  (let ((result '()))
    (dolist (e list-1)
      (when (member (funcall key e) list-2 :key key :test test :test-not test-not)
        (push e result)))
    result))

(defun nintersection (list-1 list-2 &key key test test-not)
  (intersection list-1 list-2 :key key :test test :test-not test-not))

(defun set-exclusive-or (list-1 list-2 &key key test test-not)
  (when (not key)
    (setf key 'identity))
  (check-type list-1 list)
  (check-type list-2 list)
  (check-test-test-not test test-not)
  (let ((result '()))
    (dolist (e list-1)
      (when (not (member (funcall key e) list-2 :key key :test test :test-not test-not))
        (push e result)))
    (dolist (e list-2)
      (when (not (member (funcall key e) list-1 :key key :test test :test-not test-not))
        (push e result)))
    result))

(defun nset-exclusive-or (list-1 list-2 &key key test test-not)
  (set-exclusive-or list-1 list-2 :key key :test test :test-not test-not))

(defun subsetp (list-1 list-2 &key key test test-not)
  (when (not key)
    (setf key 'identity))
  (check-type list-1 list)
  (check-type list-2 list)
  (check-test-test-not test test-not)
  (every (lambda (e)
           (member (funcall key e) list-2 :key key :test test :test-not test-not))
         list-1))

(defun tree-equal (tree-1 tree-2 &key test test-not)
  (with-test-test-not (test test-not)
    (labels ((frob (lhs rhs)
               (or (and (atom lhs)
                        (atom rhs)
                        (funcall test lhs rhs))
                   (and (consp lhs)
                        (consp rhs)
                        (frob (car lhs) (car rhs))
                        (frob (cdr lhs) (cdr rhs))))))
      (frob tree-1 tree-2))))
