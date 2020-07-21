;;;; Array functions

(in-package :mezzano.internals)

(defconstant array-rank-limit (- (ash 1 +object-data-size+) +complex-array-axis-0+))
(defconstant array-dimension-limit (ash 1 +object-data-size+))
(defconstant array-total-size-limit (ash 1 +object-data-size+))

(deftype array-axis ()
  `(integer 0 (,array-rank-limit)))

(deftype array-index ()
  `(integer 0 (,array-dimension-limit)))

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
         ,(compile-array-type-1 object 'sys.int::complex-array-p element-type dimensions))))
(%define-compound-type-optimizer 'array 'compile-array-type)

(defun compile-simple-array-type (object type)
  (multiple-value-bind (element-type dimensions)
      (parse-array-type type)
    (when (or (eql element-type '*)
              (eql dimensions '*))
      (return-from compile-simple-array-type
        (compile-array-type-1 object 'simple-array-p element-type dimensions)))
    (let ((info (upgraded-array-info element-type))
          (rank (length dimensions)))
      (when (eql (specialized-array-definition-type info) 'character)
        ;; Strings.
        (return-from compile-simple-array-type
          `(and (simple-character-array-p ,object)
                (eq (sys.int::%object-header-data ,object) ',rank)
                ,@(loop
                     for dim in dimensions
                     for i from 0
                     unless (eql dim '*)
                     collect `(eq (sys.int::%object-ref-t ,object (+ sys.int::+complex-array-axis-0+ ,i)) ,dim)))))
      (when (not (specialized-array-definition-tag info))
        (return-from compile-simple-array-type
          (compile-array-type-1 object 'simple-array-p element-type dimensions)))
      (cond ((not (eql rank 1))
             `(and (sys.int::%object-of-type-p ,object sys.int::+object-tag-simple-array+)
                   (eq (sys.int::%object-header-data ,object) ',rank)
                   (sys.int::%object-of-type-p (sys.int::%object-ref-t ,object sys.int::+complex-array-storage+)
                                               ,(specialized-array-definition-tag info))
                   ,@(loop
                        for dim in dimensions
                        for i from 0
                        unless (eql dim '*)
                        collect `(eq (sys.int::%object-ref-t ,object (+ sys.int::+complex-array-axis-0+ ,i)) ,dim))))
            ((eql (first dimensions) '*)
             `(sys.int::%object-of-type-p ,object ,(specialized-array-definition-tag info)))
            (t
             `(and (sys.int::%object-of-type-p ,object ,(specialized-array-definition-tag info))
                   (eq (%object-header-data ,object) ,(first dimensions))))))))
(%define-compound-type-optimizer 'simple-array 'compile-simple-array-type)
)

(deftype simple-vector (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(simple-array t (,size)))

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
  `(simple-string ,size))

(deftype base-string (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(string ,size))

(deftype simple-string (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(simple-array character (,size)))

(deftype string (&optional size)
  (check-type size (or non-negative-fixnum (eql *)))
  `(vector character ,size))

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
             (let ((type (specialized-array-definition-type info)))
               (when (subtypep typespec type environment)
                 (return info))))))))

(defun upgraded-array-element-type (typespec &optional environment)
  (specialized-array-definition-type (upgraded-array-info typespec environment)))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(%define-type-symbol 'array 'arrayp)
)

(defun adjustable-array-p (array)
  (check-type array array)
  (not (%simple-1d-array-p array)))

(declaim (inline simple-vector-p vectorp bit-array-p simple-bit-vector-p bit-vector-p))

(defun simple-vector-p (object)
  (typep object 'simple-vector))

(defun vectorp (object)
  (typep object 'vector))

(defun bit-array-p (object)
  (typep object '(array bit *)))

(defun simple-bit-vector-p (object)
  (typep object '(simple-array bit (*))))

(defun bit-vector-p (object)
  (typep object '(array bit (*))))

(defun make-simple-array (length info area initial-element initial-element-p)
  (let* ((array (make-simple-array-1 length info area)))
    (when initial-element-p
      (unless (typep initial-element (specialized-array-definition-type info))
        (error 'type-error :expected-type (specialized-array-definition-type info) :datum initial-element))
      (unless (eql initial-element (specialized-array-definition-zero-element info))
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

(define-compiler-macro (cas aref) (&whole whole old new array &rest subscripts)
  (case (length subscripts)
    (1 `(funcall #'(cas aref-1) ,old ,new ,array ,@subscripts))
    (2 `(funcall #'(cas aref-2) ,old ,new ,array ,@subscripts))
    (3 `(funcall #'(cas aref-3) ,old ,new ,array ,@subscripts))
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
                                          memory physical-memory
                                          area)
  (cond ((or (not (or (eql element-type 't)
                      (and (consp element-type)
                           (eql (first element-type) 'quote))))
             ;; One or the other.
             (and initial-element-p
                  initial-contents-p)
             (not (or (eql fill-pointer nil)
                      (eql fill-pointer t)
                      (integerp fill-pointer)))
             (not (or (eql adjustable nil)
                      (eql adjustable t)))
             displaced-to displaced-index-offset
             memory physical-memory)
         whole)
        (t
         (let ((array-sym (gensym "ARRAY"))
               (area-sym (gensym "AREA"))
               (info (upgraded-array-info (if (consp element-type)
                                              (second element-type)
                                              element-type))))
           (when (not (specialized-array-definition-tag info))
             (return-from make-array whole))
           `(let* ((,area-sym ,area)
                   (,array-sym (make-array-with-known-element-type
                                ,dimensions ,element-type ',info ,area-sym
                                ,(if initial-element-p
                                     initial-element
                                     `',(specialized-array-definition-zero-element info))
                                ',adjustable
                                ',fill-pointer)))
              ,@(when initial-contents-p
                  `((initialize-from-initial-contents ,array-sym ,initial-contents)))
              ,array-sym)))))

(defun make-array-with-known-element-type (dimensions element-type info area initial-element adjustable fill-pointer)
  (when (and (consp dimensions)
             (integerp (first dimensions))
             (endp (rest dimensions)))
    (setf dimensions (first dimensions)))
  (cond ((integerp dimensions)
         (assert (<= 0 dimensions))
         (let ((array (make-simple-array-1 dimensions info area)))
           (when (not (eql initial-element (specialized-array-definition-zero-element info)))
             (fill array initial-element))
           (when (or adjustable fill-pointer)
             (when fill-pointer
               (when (eql fill-pointer t)
                 (setf fill-pointer dimensions))
               (unless (integerp fill-pointer)
                 (error "Invalid :FILL-POINTER ~S." fill-pointer))
               (unless (<= 0 fill-pointer dimensions)
                 (error "Fill-pointer ~S out of vector bounds. Should be non-negative and <= ~S." fill-pointer dimensions)))
             (setf array (%make-array-header +object-tag-array+
                                             array
                                             fill-pointer
                                             nil
                                             (if (listp dimensions)
                                                 dimensions
                                                 (list dimensions))
                                             area)))
           array))
        (t
         (make-array dimensions
                     :element-type element-type
                     :initial-element initial-element
                     :adjustable adjustable
                     :fill-pointer fill-pointer
                     :area area))))

(defun make-array (dimensions &key
                                (element-type t)
                                (initial-element nil initial-element-p)
                                (initial-contents nil initial-contents-p)
                                adjustable
                                fill-pointer
                                displaced-to displaced-index-offset
                                memory physical-memory
                                area)
  ;; n => (n)
  (when (not (listp dimensions))
    (setf dimensions (list dimensions)))
  (let* ((rank (length dimensions))
         (element-type-info (upgraded-array-info element-type))
         (upgraded-element-type (specialized-array-definition-type element-type-info)))
    (when (>= rank array-rank-limit)
      (error "Array has too many dimensions."))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot supply :INITIAL-ELEMENT and :INITIAL-CONTENTS."))
    (when (and (or displaced-to memory physical-memory)
               (or initial-element-p initial-contents-p))
      (error "Cannot use :INITIAL-ELEMENT or :INITIAL-CONTENTS with a displaced or memory array."))
    (when (and (not displaced-to) displaced-index-offset)
      (error "Non-NIL :DISPLACED-INDEX-OFFSET with NIL :DISPLACED-TO."))
    (when (and displaced-to (or memory physical-memory))
      (error "Cannot supply :DISPLACED-TO and :MEMORY or :PHYSICAL-MEMORY"))
    (when fill-pointer
      (unless (eql rank 1)
        (error ":FILL-POINTER is not valid on multidimensional arrays."))
      (when (eql fill-pointer 't)
        (setf fill-pointer (first dimensions)))
      (unless (integerp fill-pointer)
        (error "Invalid :FILL-POINTER ~S." fill-pointer))
      (unless (<= 0 fill-pointer (first dimensions))
        (error "Fill-pointer ~S out of vector bounds. Should be non-negative and <= ~S." fill-pointer (first dimensions))))
    (when (and (eql upgraded-element-type 'character)
               (or memory physical-memory))
      (error "Element type ~S (upgraded to ~S) not supported for memory arrays."
             element-type upgraded-element-type))
    (when (and memory physical-memory)
      (error "Cannot supply :MEMORY and :PHYSICAL-MEMORY"))
    (when physical-memory
      (check-type physical-memory fixnum)
      (setf memory (mezzano.supervisor::convert-to-pmap-address physical-memory)))
    (when memory
      (check-type memory (or mezzano.supervisor:dma-buffer fixnum)))
    (dolist (dimension dimensions)
      (check-type dimension (integer 0)))
    (cond ((and (eql rank 1)
                (not adjustable)
                (not fill-pointer)
                (not displaced-to)
                (not memory)
                ;; character arrays are special.
                (not (eql upgraded-element-type 'character)))
           ;; Create a simple 1D array.
           (let ((array (make-simple-array (first dimensions) element-type-info area initial-element initial-element-p)))
             (when initial-contents-p
               (initialize-from-initial-contents array initial-contents))
             array))
          (memory
           ;; storage = address or dma-buffer
           ;; info = element type as an object-tag
           (when (typep memory 'mezzano.supervisor:dma-buffer)
             (let ((total-size (reduce #'* dimensions)))
               (assert (<= total-size (mezzano.supervisor:dma-buffer-length memory)))))
           (%make-array-header +object-tag-array+
                               memory
                               fill-pointer
                               (specialized-array-definition-tag element-type-info)
                               dimensions
                               area))
          (displaced-to
           (unless displaced-index-offset
             (setf displaced-index-offset 0))
           (%make-array-header +object-tag-array+ displaced-to fill-pointer displaced-index-offset dimensions area))
          ((eql upgraded-element-type 'character)
           (let* ((total-size (apply #'* dimensions))
                  (backing-array (make-array total-size :element-type '(unsigned-byte 8) :area area))
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
                    (backing-array (make-simple-array total-size element-type-info area initial-element initial-element-p))
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
                     (element-type (array-element-type array) element-type-p)
                     (initial-element nil initial-element-p)
                     (initial-contents nil initial-contents-p)
                     fill-pointer
                     (area (object-allocation-area array)))
  (when (integerp new-dimensions)
    (setf new-dimensions (list new-dimensions)))
  (when (not (eql (array-rank array) (length new-dimensions)))
    (error "New dimensions do not match array's rank."))
  (when element-type-p
    (unless (equal element-type (array-element-type array))
      (error "Cannot convert array ~S to different element-type ~S from ~S."
             array element-type (array-element-type array))))
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
  (cond ((and (%simple-1d-array-p array)
              (or (not element-type-p)
                  (equal element-type (array-element-type array))))
         ;; Same element type.
         (let* ((array-info (%simple-array-info array))
                (new-array (make-simple-array-1 (first new-dimensions) array-info area)))
           (when (and initial-element-p
                      (not (eql (specialized-array-definition-zero-element array-info)
                                initial-element)))
             (fill new-array initial-element))
           (cond (initial-contents-p
                  (initialize-from-initial-contents new-array initial-contents))
                 (t
                  (replace new-array array)))
           new-array))
        ((%simple-1d-array-p array)
         (let ((new-array (make-simple-array (first new-dimensions)
                                             (if element-type-p
                                                 (upgraded-array-info element-type)
                                                 (array-element-info array))
                                             area
                                             initial-element
                                             initial-element-p)))
           (cond (initial-contents-p
                  (initialize-from-initial-contents new-array initial-contents))
                 (t
                  (replace new-array array)))
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
  (cond ((%simple-1d-array-p array)
         1)
        (t
         (check-type array array)
         (%object-header-data array))))

(defun array-dimensions (array)
  (check-type array array)
  (loop for i below (array-rank array)
     collect (array-dimension array i)))

(defun array-dimension (array axis-number)
  (check-type axis-number (integer 0) "a non-negative integer")
  (cond ((%simple-1d-array-p array)
         (unless (zerop axis-number)
           (error "Axis ~S exceeds array rank 1." axis-number))
         (%object-header-data array))
        (t
         (check-type array array)
         (when (>= axis-number (array-rank array))
           (error "Axis ~S exceeds array rank ~D."
                  axis-number (array-rank array)))
         (%complex-array-dimension array axis-number))))

(defun array-total-size (array)
  (check-type array array)
  (apply #'* (array-dimensions array)))

(defun array-displacement (array)
  (check-type array array)
  (cond ((or (%simple-1d-array-p array)
             (not (%complex-array-info array)))
         (values nil 0))
        ((or (fixnump (%complex-array-storage array))
             (typep (%complex-array-storage array) 'mezzano.supervisor:dma-buffer))
         ;; Memory array.
         (values (%complex-array-storage array) 0))
        (t
         ;; Normal displaced array.
         (values (%complex-array-storage array)
                 (%complex-array-info array)))))

(defun array-element-type (array)
  (check-type array array)
  (cond ((%simple-1d-array-p array)
         (%simple-array-element-type array))
        ((character-array-p array)
         'character)
        ((or (fixnump (%complex-array-storage array))
             (typep (%complex-array-storage array) 'mezzano.supervisor:dma-buffer))
         ;; Memory arrays have their type in the info field.
         (svref *array-types* (%complex-array-info array)))
        ((%complex-array-info array)
         ;; Displaced arrays inherit the type of the array they displace on.
         (array-element-type (%complex-array-storage array)))
        (t ;; Normal arrays use the type of their storage simple array.
         (%simple-array-element-type (%complex-array-storage array)))))

(defun array-element-info (array)
  (check-type array array)
  (cond ((%simple-1d-array-p array)
         (%simple-array-info array))
        ((character-array-p array)
         *array-character-info*)
        ((or (fixnump (%complex-array-storage array))
             (typep (%complex-array-storage array) 'mezzano.supervisor:dma-buffer))
         ;; Memory arrays have their type in the info field.
         (upgraded-array-info (svref *array-types* (%complex-array-info array))))
        ((%complex-array-info array)
         ;; Displaced arrays inherit the type of the array they displace on.
         (array-element-type (%complex-array-storage array)))
        (t ;; Normal arrays use the type of their storage simple array.
         (%simple-array-info (%complex-array-storage array)))))

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
  (loop
     for axis below (array-rank array)
     for subscript in subscripts
     for dimension = (array-dimension array axis)
     do
       (check-type subscript integer)
       (unless (<= 0 subscript (1- dimension))
         (raise-complex-bounds-error
          array subscript dimension nil))
     summing
       (* subscript (loop
                       with result = 1
                       for remaining-axis from (1+ axis) below (array-rank array)
                       do (setf result (* result (array-dimension array remaining-axis)))
                       finally (return result)))))

(defun check-vector-has-fill-pointer (vector)
  (unless (array-has-fill-pointer-p vector)
    (error 'simple-type-error
           :expected-type '(and vector (satisfies array-has-fill-pointer-p))
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
    (error "New fill-pointer ~S exceeds vector bounds. Should be non-negative and <= ~S."
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
         (let ((storage (%complex-array-storage array)))
           (cond ((or (fixnump storage)
                      (typep storage 'mezzano.supervisor:dma-buffer))
                  ;; A memory array.
                  (%memory-array-aref array index))
                 (t
                  ;; A displaced array.
                  (row-major-aref storage (+ index (%complex-array-info array)))))))
        ((character-array-p array)
         ;; Character array. Elements are characters, stored as integers.
         (%%assemble-value
          (logior (ash (%simple-array-aref (%complex-array-storage array) index)
                       (+ (byte-position sys.int::+immediate-tag+)
                          (byte-size sys.int::+immediate-tag+))))
          (dpb +immediate-tag-character+
               +immediate-tag+
               +tag-immediate+)))
        (t ;; Normal array, backed by a simple array.
         (%simple-array-aref (%complex-array-storage array) index))))

(defun (setf %row-major-aref) (value array index)
  "(SETF ROW-MAJOR-AREF) with no bounds check."
  (cond ((%simple-1d-array-p array)
         (setf (%simple-array-aref array index) value))
        ((%complex-array-info array)
         (let ((storage (%complex-array-storage array)))
           (cond ((or (fixnump storage)
                      (typep storage 'mezzano.supervisor:dma-buffer))
                  ;; A memory array.
                  (setf (%memory-array-aref array index) value))
                 (t
                  ;; A displaced array.
                  (setf (row-major-aref storage (+ index (%complex-array-info array)))
                        value)))))
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

(defun (cas %row-major-aref) (old new array index)
  "(CAS ROW-MAJOR-AREF) with no bounds check."
  (cond ((%simple-1d-array-p array)
         (cas (%simple-array-aref array index) old new))
        ((%complex-array-info array)
         (let ((storage (%complex-array-storage array)))
           (cond ((or (fixnump storage)
                      (typep storage 'mezzano.supervisor:dma-buffer))
                  ;; A memory array.
                  (cas (%memory-array-aref storage index) old new))
                 (t
                  ;; A displaced array.
                  (cas (row-major-aref storage
                                       (+ index (%complex-array-info array)))
                       old new)))))
        ((character-array-p array)
         ;; TODO. The promotion from narrow backing arrays to wide backing arrays complicates this...
         (error "CAS not supported on character arrays"))
        (t ;; Normal array, backed by a simple array.
         (cas (%simple-array-aref (%complex-array-storage array) index) old new))))

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

(defun (cas row-major-aref) (old new array index)
  (check-type array array)
  (check-type index (integer 0) "a non-negative integer")
  (let ((total-size (array-total-size array)))
    (when (>= index total-size)
      (error "Row-major index ~S exceeds total size ~S of array ~S." index total-size array))
    (cas (%row-major-aref array index) old new)))

(defun aref (array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (%row-major-aref array (apply #'array-row-major-index array subscripts)))

(defun (setf aref) (value array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (setf (%row-major-aref array (apply #'array-row-major-index array subscripts)) value))

(defun (cas aref) (old new array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (cas (%row-major-aref array (apply #'array-row-major-index array subscripts)) old new))

(defun aref-1 (array index)
  (check-type index integer)
  (unless (= (array-rank array) 1)
    (error "Invalid number of indices to array ~S." array))
  (when (or (< index 0)
            (>= index (array-dimension array 0)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index (array-dimension array 0)))
  (%row-major-aref array index))

(defun (setf aref-1) (value array index)
  (check-type index integer)
  (unless (= (array-rank array) 1)
    (error "Invalid number of indices to array ~S." array))
  (when (or (< index 0)
            (>= index (array-dimension array 0)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index (array-dimension array 0)))
  (setf (%row-major-aref array index) value))

(defun (cas aref-1) (old new array index)
  (check-type index integer)
  (unless (= (array-rank array) 1)
    (error "Invalid number of indices to array ~S." array))
  (when (or (< index 0)
            (>= index (array-dimension array 0)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index (array-dimension array 0)))
  (cas (%row-major-aref array index) old new))

(defun aref-2 (array index1 index2)
  (check-type index1 integer)
  (check-type index2 integer)
  (unless (= (array-rank array) 2)
    (error "Invalid number of indices to array ~S." array))
  (when (or (< index1 0)
            (>= index1 (array-dimension array 0)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index1 (array-dimension array 0)))
  (when (or (< index2 0)
            (>= index2 (array-dimension array 1)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index2 (array-dimension array 1)))
  (let ((ofs (+ (* index1 (array-dimension array 1)) index2)))
    (%row-major-aref array ofs)))

(defun (setf aref-2) (value array index1 index2)
  (check-type index1 integer)
  (check-type index2 integer)
  (unless (= (array-rank array) 2)
    (error "Invalid number of indices to array ~S." array))
  (when (or (< index1 0)
            (>= index1 (array-dimension array 0)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index1 (array-dimension array 0)))
  (when (or (< index2 0)
            (>= index2 (array-dimension array 1)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index2 (array-dimension array 1)))
  (let ((ofs (+ (* index1 (array-dimension array 1)) index2)))
    (setf (%row-major-aref array ofs) value)))

(defun (cas aref-2) (old new array index1 index2)
  (check-type index1 integer)
  (check-type index2 integer)
  (unless (= (array-rank array) 2)
    (error "Invalid number of indices to array ~S." array))
  (when (or (< index1 0)
            (>= index1 (array-dimension array 0)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index1 (array-dimension array 0)))
  (when (or (< index2 0)
            (>= index2 (array-dimension array 1)))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index2 (array-dimension array 1)))
  (let ((ofs (+ (* index1 (array-dimension array 1)) index2)))
    (cas (%row-major-aref array ofs) old new)))

(defun aref-3 (array index1 index2 index3)
  (check-type index1 integer)
  (check-type index2 integer)
  (check-type index3 integer)
  (unless (= (array-rank array) 3)
    (error "Invalid number of indices to array ~S." array))
  (let ((dim1 (array-dimension array 0))
        (dim2 (array-dimension array 1))
        (dim3 (array-dimension array 2)))
    (when (or (< index1 0)
              (>= index1 dim1))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index1 dim1))
    (when (or (< index2 0)
              (>= index2 dim2))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index2 dim2))
    (when (or (< index3 0)
              (>= index3 dim3))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index3 dim3))
    (let ((ofs (+ (* (+ (* index1 dim2) index2) dim3) index3)))
      (%row-major-aref array ofs))))

(defun (setf aref-3) (value array index1 index2 index3)
  (check-type index1 integer)
  (check-type index2 integer)
  (check-type index3 integer)
  (unless (= (array-rank array) 3)
    (error "Invalid number of indices to array ~S." array))
  (let ((dim1 (array-dimension array 0))
        (dim2 (array-dimension array 1))
        (dim3 (array-dimension array 2)))
    (when (or (< index1 0)
              (>= index1 dim1))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index1 dim1))
    (when (or (< index2 0)
              (>= index2 dim2))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index2 dim2))
    (when (or (< index3 0)
              (>= index3 dim3))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index3 dim3))
    (let ((ofs (+ (* (+ (* index1 dim2) index2) dim3) index3)))
      (setf (%row-major-aref array ofs) value))))

(defun (cas aref-3) (old new array index1 index2 index3)
  (check-type index1 integer)
  (check-type index2 integer)
  (check-type index3 integer)
  (unless (= (array-rank array) 3)
    (error "Invalid number of indices to array ~S." array))
  (let ((dim1 (array-dimension array 0))
        (dim2 (array-dimension array 1))
        (dim3 (array-dimension array 2)))
    (when (or (< index1 0)
              (>= index1 dim1))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index1 dim1))
    (when (or (< index2 0)
              (>= index2 dim2))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index2 dim2))
    (when (or (< index3 0)
              (>= index3 dim3))
      (error "Index ~S out of bounds. Must be 0 <= n < ~D~%" index3 dim3))
    (let ((ofs (+ (* (+ (* index1 dim2) index2) dim3) index3)))
      (cas (%row-major-aref array ofs) old new))))

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
