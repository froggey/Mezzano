;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; array.lisp

(in-package :sys.int)

(defconstant array-rank-limit (- (ash 1 +object-data-size+) +complex-array-axis-0+))
(defconstant array-dimension-limit (ash 1 +object-data-size+))
(defconstant array-total-size-limit (ash 1 +object-data-size+))

(deftype array-axis ()
  `(integer 0 (,array-rank-limit)))

(deftype simple-vector (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(simple-array t (,size)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-simple-vector-type (object type)
  (destructuring-bind (&optional (size '*)) (rest type)
    (check-type size (or non-negative-fixnum (eql *)))
    (if (eql size '*)
        `(simple-vector-p ,object)
        `(and (simple-vector-p ,object)
              (eq (%object-header-data ,object) ',size)))))
(%define-compound-type-optimizer 'simple-vector 'compile-simple-vector-type)
(%define-type-symbol 'simple-vector 'simple-vector-p)
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-array-type-1 (object base-predicate element-type dimensions)
  `(and (,base-predicate ,object)
        ,@(when (not (eql element-type '*))
            ;; Parse-array-type returns the upgraded element type.
            `((equal (array-element-type ,object) ',element-type)))
        ,@(when (not (eql dimensions '*))
            `((eql (array-rank ,object) ',(length dimensions))))
        ,@(when (not (eql dimensions '*))
            (loop
               for dim in dimensions
               for i from 0
               unless (eql dim '*)
               collect `(eql (array-dimension ,object ',i) ',dim)))))

(defun compile-array-type (object type)
  (multiple-value-bind (element-type dimensions)
      (parse-array-type type)
    `(or (typep ,object '(simple-array ,element-type ,dimensions))
         ,(compile-array-type-1 object 'arrayp element-type dimensions))))
(%define-compound-type-optimizer 'array 'compile-array-type)

(defun compile-simple-array-type (object type)
  (multiple-value-bind (element-type dimensions)
      (parse-array-type type)
    (when (or (eql element-type '*)
              (eql dimensions '*)
              (not (eql (length dimensions) 1)))
      (return-from compile-simple-array-type
        (compile-array-type-1 object 'simple-array-p element-type dimensions)))
    (let ((info (upgraded-array-info element-type)))
      (when (not (second info))
        (return-from compile-simple-array-type
          (compile-array-type-1 object 'simple-array-p element-type dimensions)))
      (cond ((eql (first dimensions) '*)
             `(sys.int::%object-of-type-p ,object ,(second info)))
            (t
             `(and (sys.int::%object-of-type-p ,object ,(second info))
                   (eq (%object-header-data ,object) ,(first dimensions))))))))
(%define-compound-type-optimizer 'simple-array 'compile-simple-array-type)
)

(deftype vector (&optional element-type size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(array ,element-type (,size)))

(deftype simple-bit-vector (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(simple-array bit (,size)))

(deftype bit-vector (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(vector bit ,size))

(deftype simple-base-string (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(simple-array base-char (,size)))

(deftype base-string (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(vector base-char ,size))

(deftype simple-string (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(or (simple-array base-char (,size))
       (simple-array character (,size))))

(deftype string (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(or (vector base-char ,size)
       (vector character ,size)))

(defun simple-array-p (object)
  (or (%simple-1d-array-p object)
      (%object-of-type-p object +object-tag-simple-string+)
      (%object-of-type-p object +object-tag-simple-array+)))

(defun simple-array-type-p (object type)
  (and (simple-array-p object)
       (array-type-p object type)))
(%define-compound-type 'simple-array 'simple-array-type-p)

(eval-when (:compile-toplevel :load-toplevel :execute)
(%define-type-symbol 'simple-array 'simple-array-p)
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-array-type (type)
  (destructuring-bind (&optional (element-type '*) (dimension-spec '*))
      (cdr type)
    (when (integerp dimension-spec)
      (assert (not (minusp dimension-spec)))
      (setf dimension-spec (make-list dimension-spec :initial-element '*)))
    (assert (or (and (listp dimension-spec)
                     (every (lambda (x)
                              (typep x '(or (eql *) non-negative-fixnum)))
                            dimension-spec))
                (eql dimension-spec '*))
            (dimension-spec))
    (values (if (eql element-type '*)
                '*
                (upgraded-array-element-type element-type))
            dimension-spec)))
)

(defun array-type-p (object type)
  (multiple-value-bind (element-type dimension-spec)
      (parse-array-type type)
    (let ((rank (if (listp dimension-spec)
                    (length dimension-spec)
                    '*)))
      (and (arrayp object)
           (if (eql element-type '*)
               t
               ;; Type equality.
               (and (subtypep element-type (array-element-type object))
                    (subtypep (array-element-type object) element-type)))
           (if (eql rank '*)
               t
               (eql (array-rank object) rank))
           (if (eql dimension-spec '*)
               t
               (every (lambda (x y)
                        (if (eql x '*)
                            t
                            (eql x y)))
                      dimension-spec (array-dimensions object)))))))
(%define-compound-type 'array 'array-type-p)
(eval-when (:compile-toplevel :load-toplevel :execute)
(%define-type-symbol 'array 'arrayp)
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun upgraded-array-info (typespec &optional environment)
  ;; Pick off a few obvious cases.
  (case typespec
    ((t) *array-t-info*)
    ((character base-char) *array-character-info*)
    ((bit) *array-bit-info*)
    (t (if (subtypep typespec 'nil environment)
           ;; NIL promotes to T.
           *array-t-info*
           (dolist (info *array-info* *array-t-info*)
             (let ((type (first info)))
               (when (subtypep typespec type environment)
                 (return info))))))))

(defun upgraded-array-element-type (typespec &optional environment)
  (first (upgraded-array-info typespec environment)))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(%define-type-symbol 'array 'arrayp)
)

(defun adjustable-array-p (array)
  (check-type array array)
  (not (%simple-1d-array-p array)))

(defun vectorp (object)
  (or (%simple-1d-array-p object)
      (and (arrayp object)
           (eql (array-rank object) 1))))
(eval-when (:compile-toplevel :load-toplevel :execute)
(%define-type-symbol 'vector 'vectorp)
)

(defun bit-array-p (object)
  (and (arrayp object)
       (eql (array-element-type object) 'bit)))

(defun simple-bit-vector-p (object)
  (and (%simple-1d-array-p object)
       (eql (array-element-type object) 'bit)))

(defun bit-vector-p (object)
  (and (vectorp object)
       (eql (array-element-type object) 'bit)))
(eval-when (:compile-toplevel :load-toplevel :execute)
(%define-type-symbol 'bit-vector 'bit-vector-p)
)

(defun make-simple-array (length &optional (element-type 't) area (initial-element nil initial-element-p))
  (let* ((info (upgraded-array-info element-type))
         (array (make-simple-array-1 length info area)))
    (when initial-element-p
      (unless (typep initial-element element-type)
        (error 'type-error :expected-type element-type :datum initial-element))
      (unless (eql initial-element (fifth info))
        (fill array initial-element)))
    array))

(defun initialize-from-initial-contents (array sequence)
  "Fill an array using a sequence."
  (case (array-rank array)
    (0 (setf (aref array) sequence))
    (1 (when (/= (array-dimension array 0) (length sequence))
         (error "Malformed :INITIAL-CONTENTS: Dimension of axis 0 is ~S but ~S is ~S long."
                (array-dimension array 0) sequence (length sequence)))
       (cond ((listp sequence)
              (let ((i 0))
                (dolist (e sequence)
                  (setf (aref array i) e)
                  (incf i))))
             (t (dotimes (i (array-dimension array 0))
                  (setf (aref array i) (aref sequence i))))))
    (t
     (labels ((frob (axis indices dims contents)
                (cond ((null dims)
                       (setf (apply #'aref array indices) contents))
                      ((/= (first dims) (length contents))
                       (error "Malformed :INITIAL-CONTENTS: Dimension of axis ~D is ~S but ~S is ~S long."
                              axis (first dims) sequence (length sequence)))
                      (t
                       (cond ((listp contents)
                              (let ((i 0))
                                (dolist (e contents)
                                  (frob (1+ axis) (append indices (list i)) (rest dims) e)
                                  (incf i))))
                             (t (dotimes (i (length contents))
                                  (frob (1+ axis) (append indices (list i)) (rest dims) (aref contents i)))))))))
       (frob 0 '() (array-dimensions array) sequence)))))

(define-compiler-macro aref (&whole whole array &rest subscripts)
  (case (length subscripts)
    (1 `(aref-1 ,array ,@subscripts))
    (2 `(aref-2 ,array ,@subscripts))
    (3 `(aref-3 ,array ,@subscripts))
    (t whole)))

(define-compiler-macro (setf aref) (&whole whole value array &rest subscripts)
  (case (length subscripts)
    (1 `(funcall #'(setf aref-1) ,value ,array ,@subscripts))
    (2 `(funcall #'(setf aref-2) ,value ,array ,@subscripts))
    (3 `(funcall #'(setf aref-3) ,value ,array ,@subscripts))
    (t whole)))

(defun %make-array-header (tag storage fill-pointer info dimensions area)
  (let* ((rank (length dimensions))
         (array (mezzano.runtime::%allocate-object tag
                                                   rank
                                                   (+ 3 rank)
                                                   area)))
    (setf (%complex-array-storage array) storage
          (%complex-array-fill-pointer array) fill-pointer
          (%complex-array-info array) info)
    (loop
       for rank from 0
       for d in dimensions
       do (setf (%complex-array-dimension array rank) d))
    array))

(define-compiler-macro make-array (&whole whole dimensions
                                          &key
                                          (element-type ''t)
                                          (initial-element nil initial-element-p)
                                          (initial-contents nil initial-contents-p)
                                          adjustable
                                          fill-pointer
                                          displaced-to displaced-index-offset
                                          area)
  (cond ((or (not (or (eql element-type 't)
                      (and (consp element-type)
                           (eql (first element-type) 'quote))))
             ;; One or the other.
             (and initial-element-p
                  initial-contents-p)
             adjustable
             fill-pointer
             displaced-to displaced-index-offset)
         whole)
        (t
         (let ((array-sym (gensym "ARRAY"))
               (info (upgraded-array-info (if (consp element-type)
                                              (second element-type)
                                              element-type))))
           (when (not (second info))
             (return-from make-array whole))
           `(let ((,array-sym (make-array-with-known-element-type
                               ,dimensions ,element-type ',info ,area
                               ,(if initial-element-p
                                    initial-element
                                    `',(fifth info)))))
              ,@(when initial-contents-p
                  `((initialize-from-initial-contents ,array-sym ,initial-contents)))
              ,array-sym)))))

(defun make-array-with-known-element-type (dimensions element-type info area initial-element)
  (when (and (consp dimensions)
             (integerp (first dimensions))
             (endp (rest dimensions)))
    (setf dimensions (first dimensions)))
  (cond ((integerp dimensions)
         (assert (<= 0 dimensions))
         (let ((array (make-simple-array-1 dimensions info area)))
           (when (not (eql initial-element (fifth info)))
             (fill array initial-element))
           array))
        (t
         (make-array dimensions
                     :element-type element-type
                     :initial-element initial-element
                     :area area))))

(defun make-array (dimensions &key
                                (element-type t)
                                (initial-element nil initial-element-p)
                                (initial-contents nil initial-contents-p)
                                adjustable
                                fill-pointer
                                displaced-to displaced-index-offset
                                area)
  ;; n => (n)
  (when (not (listp dimensions))
    (setf dimensions (list dimensions)))
  (let ((rank (length dimensions))
        (upgraded-element-type (upgraded-array-element-type element-type)))
    (when (>= rank array-rank-limit)
      (error "Array has too many dimensions."))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot supply :INITIAL-ELEMENT and :INITIAL-CONTENTS."))
    (when (and displaced-to (or initial-element-p initial-contents-p))
      (error "Cannot use :INITIAL-ELEMENT or :INITIAL-CONTENTS with a displaced array."))
    (when (and (not displaced-to) displaced-index-offset)
      (error "Non-NIL :DISPLACED-INDEX-OFFSET with NIL :DISPLACED-TO."))
    (when fill-pointer
      (unless (eql rank 1)
        (error ":FILL-POINTER is not valid on multidimensional arrays."))
      (when (eql fill-pointer 't)
        (setf fill-pointer (first dimensions)))
      (unless (integerp fill-pointer)
        (error "Invalid :FILL-POINTER ~S." fill-pointer))
      (unless (<= 0 fill-pointer (first dimensions))
        (error "Fill-pointer ~S out of vector bounds. Should be non-negative and <=~S." fill-pointer (first dimensions))))
    (dolist (dimension dimensions)
      (check-type dimension (integer 0)))
    (cond ((and (eql rank 1)
                (not adjustable)
                (not fill-pointer)
                (not displaced-to)
                ;; character arrays are special.
                (not (eql upgraded-element-type 'character)))
           ;; Create a simple 1D array.
           (let ((array (if initial-element-p
                            (make-simple-array (first dimensions) element-type area initial-element)
                            (make-simple-array (first dimensions) element-type area))))
             (when initial-contents-p
               (initialize-from-initial-contents array initial-contents))
             array))
          (displaced-to
           (unless displaced-index-offset
             (setf displaced-index-offset 0))
           (%make-array-header +object-tag-array+ displaced-to fill-pointer displaced-index-offset dimensions area))
          ((eql upgraded-element-type 'character)
           (let* ((total-size (apply #'* dimensions))
                  (backing-array (make-simple-array total-size '(unsigned-byte 8) area))
                  (array (%make-array-header (if (and (not adjustable)
                                                      (not fill-pointer))
                                                 +object-tag-simple-string+
                                                 +object-tag-string+)
                                             backing-array
                                             fill-pointer
                                             nil
                                             dimensions
                                             area)))
             (when initial-element-p
               (dotimes (i total-size)
                 (setf (%row-major-aref array i) initial-element)))
             (when initial-contents-p
               (initialize-from-initial-contents array initial-contents))
             array))
          (t (let* ((total-size (if (integerp dimensions)
                                    dimensions
                                    (apply #'* dimensions)))
                    (backing-array (if initial-element-p
                                       (make-simple-array total-size element-type area initial-element)
                                       (make-simple-array total-size element-type area)))
                    (array (%make-array-header (if (and (not fill-pointer)
                                                        (not adjustable))
                                                   +object-tag-simple-array+
                                                   +object-tag-array+)
                                               backing-array
                                               fill-pointer
                                               nil
                                               dimensions
                                               area)))
               (when initial-contents-p
                 (initialize-from-initial-contents array initial-contents))
               array)))))

(defun adjust-array (array new-dimensions &key
                     (element-type (array-element-type array))
                     (initial-element nil initial-element-p)
                     (initial-contents nil initial-contents-p)
                     fill-pointer
                     (area (object-allocation-area array)))
  (when (integerp new-dimensions)
    (setf new-dimensions (list new-dimensions)))
  (when (not (eql (array-rank array) (length new-dimensions)))
    (error "New dimensions do not match array's rank."))
  (unless (equal element-type (array-element-type array))
    (error "Cannot convert array ~S to different element-type ~S from ~S."
           array element-type (array-element-type array)))
  (when (and initial-element-p initial-contents-p)
    (error "Cannot supply :INITIAL-ELEMENT and :INITIAL-CONTENTS."))
  (when fill-pointer
    (unless (vectorp array)
      (error ":FILL-POINTER is only valid with vectors."))
    (when (eql fill-pointer 't)
      (setf fill-pointer (first new-dimensions)))
    (unless (integerp fill-pointer)
      (error "Invalid :FILL-POINTER ~S." fill-pointer))
    (unless (<= 0 fill-pointer (first new-dimensions))
      (error "Fill-pointer ~S out of vector bounds. Should non-negative and <=~S." fill-pointer (first new-dimensions))))
  (unless (vectorp array)
    (error "TODO: adjust-array on non-vectors."))
  (when (and (array-has-fill-pointer-p array)
             (not fill-pointer)
             (< (first new-dimensions) (fill-pointer array)))
    (error "Fill-pointer ~S on array ~S is larger than the new size ~S."
           (fill-pointer array) array new-dimensions))
  (cond ((%simple-1d-array-p array)
         (let ((new-array (if initial-element-p
                              (make-simple-array (first new-dimensions) element-type area initial-element)
                              (make-simple-array (first new-dimensions) element-type area))))
           (cond (initial-contents-p
                  (initialize-from-initial-contents new-array initial-contents))
                 (t
                  (dotimes (i (min (first new-dimensions) (array-dimension array 0)))
                    (setf (aref new-array i) (aref array i)))))
           new-array))
        ((and (null (%complex-array-info array))
              (character-array-p array))
         (setf (%complex-array-storage array) (if initial-element-p
                                                  (adjust-array (%complex-array-storage array) new-dimensions
                                                                :initial-element (char-int initial-element))
                                                  (adjust-array (%complex-array-storage array) new-dimensions))
               (%complex-array-dimension array 0) (first new-dimensions))
         (when initial-contents-p
           (initialize-from-initial-contents array initial-contents))
         (when fill-pointer
           (setf (fill-pointer array) fill-pointer))
         array)
        ((null (%complex-array-info array))
         (setf (%complex-array-dimension array 0) (first new-dimensions)
               (%complex-array-storage array) (if initial-element-p
                                                  (adjust-array (%complex-array-storage array) new-dimensions
                                                                :initial-element initial-element)
                                                  (adjust-array (%complex-array-storage array) new-dimensions)))
         (when fill-pointer
           (setf (fill-pointer array) fill-pointer))
         (when initial-contents-p
           (initialize-from-initial-contents array initial-contents))
         array)
        (t (error "TODO: Adjusting unusual array ~S." array))))

(defun array-rank (array)
  (check-type array array)
  (cond ((%simple-1d-array-p array)
         1)
        (t (%object-header-data array))))

(defun array-dimensions (array)
  (check-type array array)
  (loop for i below (array-rank array)
     collect (array-dimension array i)))

(defun array-dimension (array axis-number)
  (check-type array array)
  (check-type axis-number (integer 0) "a non-negative integer")
  (cond ((%simple-1d-array-p array)
         (unless (zerop axis-number)
           (error "Axis ~S exceeds array rank 1." axis-number))
         (%object-header-data array))
        (t
         (when (>= axis-number (array-rank array))
           (error "Axis ~S exceeds array rank ~D."
                  axis-number (array-rank array)))
         (%complex-array-dimension array axis-number))))

(defun array-total-size (array)
  (check-type array array)
  (apply #'* (array-dimensions array)))

(defun array-displacement (array)
  (check-type array array)
  (if (or (%simple-1d-array-p array)
          (not (fixnump (%complex-array-info array))))
      (values nil 0)
      (values (%complex-array-storage array)
              (%complex-array-info array))))

(defun array-element-type (array)
  (check-type array array)
  (cond ((%simple-1d-array-p array)
         (%simple-array-element-type array))
        ((character-array-p array)
         'character)
        ((%complex-array-info array)
         ;; Displaced arrays inherit the type of the array they displace on.
         (array-element-type (%complex-array-storage array)))
        (t ;; Normal arrays use the type of their storage simple array.
         (%simple-array-element-type (%complex-array-storage array)))))

(defun array-has-fill-pointer-p (array)
  (check-type array array)
  (and (vectorp array)
       (not (%simple-1d-array-p array))
       (%complex-array-fill-pointer array)))

(defun array-in-bounds-p (array &rest subscripts)
  (check-type array array)
  (and (not (some #'minusp subscripts))
       (every #'< subscripts (array-dimensions array))))

(defun array-row-major-index (array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (assert (eql (array-rank array) (length subscripts)))
  (apply #'+ (maplist (lambda (x y)
                        (unless (<= 0 (car x) (1- (car y)))
                          (error "Subscript ~S is invalid for axis, should be non-negative and less than ~S."
                                 (car x) (car y)))
                        (* (car x) (apply #'* (cdr y))))
                      subscripts
                      (array-dimensions array))))

(defun check-vector-has-fill-pointer (vector)
  (unless (array-has-fill-pointer-p vector)
    (error 'simple-type-error
           :expected-type 'vector
           :datum vector
           :format-control "VECTOR ~S is not a VECTOR with a fill-pointer."
           :format-arguments (list vector))))

(defun fill-pointer (vector)
  (check-vector-has-fill-pointer vector)
  (%complex-array-fill-pointer vector))

(defun (setf fill-pointer) (new-value vector)
  (check-type new-value (integer 0))
  ;; Not sure if this should allow adding a fill-pointer to a vector
  ;; that doesn't already have one.
  (check-vector-has-fill-pointer vector)
  (unless (<= 0 new-value (array-dimension vector 0))
    (error "New fill-pointer ~S exceeds vector bounds. Should be non-negative and <=~S."
           new-value (array-dimension vector 0)))
  (setf (%complex-array-fill-pointer vector) new-value))

(defun vector (&rest objects)
  (declare (dynamic-extent objects))
  (make-array (length objects)
              :element-type t
              :initial-contents objects))

(defun %row-major-aref (array index)
  "ROW-MAJOR-AREF with no bounds check."
  (cond ((%simple-1d-array-p array)
         (%simple-array-aref array index))
        ((%complex-array-info array)
         ;; A displaced array.
         (row-major-aref (%complex-array-storage array)
                         (+ index (%complex-array-info array))))
        ((character-array-p array)
         ;; Character array. Elements are characters, stored as integers.
         (%%assemble-value (ash (%simple-array-aref (%complex-array-storage array) index)
                                4)
                           +tag-character+))
        (t ;; Normal array, backed by a simple array.
         (%simple-array-aref (%complex-array-storage array) index))))

(defun (setf %row-major-aref) (value array index)
  "(SETF ROW-MAJOR-AREF) with no bounds check."
  (cond ((%simple-1d-array-p array)
         (setf (%simple-array-aref array index) value))
        ((%complex-array-info array)
         ;; A displaced array.
         (setf (row-major-aref (%complex-array-storage array)
                               (+ index (%complex-array-info array)))
               value))
        ((character-array-p array)
         ;; Character array. Elements are characters, stored as integers.
         (let ((min-len (cond ((<= (char-int value) #xFF)
                               +object-tag-array-unsigned-byte-8+)
                              ((<= (char-int value) #xFFFF)
                               +object-tag-array-unsigned-byte-16+)
                              (t +object-tag-array-unsigned-byte-32+)))
               (backing-type (%object-tag (%complex-array-storage array))))
           (when (< backing-type min-len)
             ;; Promote the storage array to fit the character.
             (setf (%complex-array-storage array) (make-array (length (%complex-array-storage array))
                                                              :element-type (ecase min-len
                                                                              (#.+object-tag-array-unsigned-byte-8+ '(unsigned-byte 8))
                                                                              (#.+object-tag-array-unsigned-byte-16+ '(unsigned-byte 16))
                                                                              (#.+object-tag-array-unsigned-byte-32+ '(unsigned-byte 32)))
                                                              :initial-contents (%complex-array-storage array))))
           (setf (%simple-array-aref (%complex-array-storage array) index)
                 (char-int value)))
         ;; Return VALUE, not the result of setting the storage array, which
         ;; is an integer.
         value)
        (t ;; Normal array, backed by a simple array.
         (setf (%simple-array-aref (%complex-array-storage array) index) value))))

(defun row-major-aref (array index)
  (check-type array array)
  (check-type index (integer 0) "a non-negative integer")
  (let ((total-size (array-total-size array)))
    (when (>= index total-size)
      (error "Row-major index ~S exceeds total size ~S of array ~S." index total-size array))
    (%row-major-aref array index)))

(defun (setf row-major-aref) (value array index)
  (check-type array array)
  (check-type index (integer 0) "a non-negative integer")
  (let ((total-size (array-total-size array)))
    (when (>= index total-size)
      (error "Row-major index ~S exceeds total size ~S of array ~S." index total-size array))
    (setf (%row-major-aref array index) value)))

(defun aref (array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (%row-major-aref array (apply #'array-row-major-index array subscripts)))

(defun (setf aref) (value array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (setf (%row-major-aref array (apply #'array-row-major-index array subscripts)) value))

(defun aref-1 (array index)
  (unless (= (array-rank array) 1)
    (error "Invalid number of indices to array ~S." array))
  (when (>= index (array-dimension array 0))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index (array-dimension array 0)))
  (%row-major-aref array index))

(defun (setf aref-1) (value array index)
  (unless (= (array-rank array) 1)
    (error "Invalid number of indices to array ~S." array))
  (when (>= index (array-dimension array 0))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index (array-dimension array 0)))
  (setf (%row-major-aref array index) value))

(defun aref-2 (array index1 index2)
  (unless (= (array-rank array) 2)
    (error "Invalid number of indices to array ~S." array))
  (when (>= index1 (array-dimension array 0))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index1 (array-dimension array 0)))
  (when (>= index2 (array-dimension array 1))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index2 (array-dimension array 1)))
  (let ((ofs (+ (* index1 (array-dimension array 1)) index2)))
    (%row-major-aref array ofs)))

(defun (setf aref-2) (value array index1 index2)
  (unless (= (array-rank array) 2)
    (error "Invalid number of indices to array ~S." array))
  (when (>= index1 (array-dimension array 0))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index1 (array-dimension array 0)))
  (when (>= index2 (array-dimension array 1))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index2 (array-dimension array 1)))
  (let ((ofs (+ (* index1 (array-dimension array 1)) index2)))
    (setf (%row-major-aref array ofs) value)))

(defun aref-3 (array index1 index2 index3)
  (unless (= (array-rank array) 3)
    (error "Invalid number of indices to array ~S." array))
  (let ((dim1 (array-dimension array 0))
        (dim2 (array-dimension array 1))
        (dim3 (array-dimension array 2)))
    (when (>= index1 dim1)
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index1 dim1))
    (when (>= index2 dim2)
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index2 dim2))
    (when (>= index3 dim3)
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index3 dim3))
    (let ((ofs (+ (* (+ (* index1 dim2) index2) dim3) index3)))
      (%row-major-aref array ofs))))

(defun (setf aref-3) (value array index1 index2 index3)
  (unless (= (array-rank array) 3)
    (error "Invalid number of indices to array ~S." array))
  (let ((dim1 (array-dimension array 0))
        (dim2 (array-dimension array 1))
        (dim3 (array-dimension array 2)))
    (when (>= index1 dim1)
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index1 dim1))
    (when (>= index2 dim2)
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index2 dim2))
    (when (>= index3 dim3)
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index3 dim3))
    (let ((ofs (+ (* (+ (* index1 dim2) index2) dim3) index3)))
      (setf (%row-major-aref array ofs) value))))

(defun bit (bit-array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (check-type bit-array (array bit))
  (apply #'aref bit-array subscripts))

(defun (setf bit) (value bit-array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (check-type bit-array (array bit))
  (apply #'(setf aref) value bit-array subscripts))

(defun sbit (bit-array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (check-type bit-array (simple-array bit))
  (apply #'aref bit-array subscripts))

(defun (setf sbit) (value bit-array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (check-type bit-array (simple-array bit))
  (apply #'(setf aref) value bit-array subscripts))

#+(or)(defun char (string index)
  (check-type string string)
  (aref string index))

(defun (setf char) (value string index)
  (check-type string string)
  (setf (aref string index) value))

(defun schar (string index)
  (check-type string string)
  (char string index))

(defun (setf schar) (value string index)
  (check-type string string)
  (setf (char string index) value))

(defun vector-pop (vector)
  (check-vector-has-fill-pointer vector)
  (when (zerop (fill-pointer vector))
    (error "Vector ~S is empty." vector))
  (aref vector (decf (fill-pointer vector))))

(defun vector-push (new-element vector)
  "Attempts to set the element of VECTOR designated by its fill-pointer
to NEW-ELEMENT and increments the fill-pointer by one. Returns NIL if the fill-pointer
is too large; otherwise the index of the new element is returned."
  (check-vector-has-fill-pointer vector)
  (let ((fp (fill-pointer vector)))
    (when (< fp (array-dimension vector 0))
      (setf (aref vector fp) new-element)
      (incf (fill-pointer vector))
      fp)))

(defun vector-push-extend (new-element vector &optional (extension (1+ (length vector))))
  "Sets the element of VECTOR designated by its fill-pointer to NEW-ELEMENT, increments
the fill-pointer by one and returns the index of the new element. VECTOR is extended by
at least MIN-EXTENSION if required."
  (check-type extension (integer 1) "a positive integer")
  (check-vector-has-fill-pointer vector)
  (when (>= (fill-pointer vector) (array-dimension vector 0))
    (adjust-array vector (+ (array-dimension vector 0) extension)))
  (vector-push new-element vector))

(defmacro define-bit-function (name operator)
  `(defun ,name (bit-array1 bit-array2 &optional opt-arg)
     (assert (equal (array-dimensions bit-array1) (array-dimensions bit-array2)))
     (let ((result-array (etypecase opt-arg
                           ((array bit) opt-arg)
                           ((eql t) bit-array1)
                           ((eql nil) (make-array (array-dimensions bit-array1)
                                                  :element-type 'bit)))))
       (dotimes (i (array-total-size bit-array1))
         (setf (row-major-aref result-array i) (logand (,operator
                                                        (row-major-aref bit-array1 i)
                                                        (row-major-aref bit-array2 i))
                                                       1)))
       result-array)))

(define-bit-function bit-and logand)
(define-bit-function bit-eqv logeqv)
(define-bit-function bit-ior logior)
(define-bit-function bit-xor logxor)
(define-bit-function bit-nand lognand)
(define-bit-function bit-nor lognor)
(define-bit-function bit-andc1 logandc1)
(define-bit-function bit-andc2 logandc2)
(define-bit-function bit-orc1 logorc1)
(define-bit-function bit-orc2 logorc2)

(defun bit-not (bit-array &optional opt-arg)
  (let ((result-array (etypecase opt-arg
                        ((array bit) opt-arg)
                        ((eql t) bit-array)
                        ((eql nil) (make-array (array-dimensions bit-array)
                                               :element-type 'bit)))))
    (dotimes (i (array-total-size bit-array))
      (setf (row-major-aref result-array i) (logand (lognot (row-major-aref bit-array i)) 1)))
    result-array))
