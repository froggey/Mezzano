;;;; type.lisp - Type management.

(in-package :mezzano.internals)

(defmacro deftype (name lambda-list &body body)
  (let ((whole (gensym "WHOLE"))
        (env (gensym "ENV")))
    (multiple-value-bind (new-lambda-list env-binding)
        (fix-lambda-list-environment lambda-list)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (%deftype ',name
                   (lambda (,whole ,env)
                     (declare (lambda-name (deftype ,name))
                              (ignorable ,whole ,env))
                     ,(expand-destructuring-lambda-list new-lambda-list name body
                                                        whole `(cdr ,whole)
                                                        (when env-binding
                                                          (list `(,env-binding ,env)))
                                                        :default-value ''*
                                                        :permit-docstring t))
                   ',(nth-value 2 (parse-declares body :permit-docstring t)))
         ',name))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct type-info
  docstring
  maybe-class
  type-symbol
  type-expander
  compound-type
  compound-type-optimizer
  numeric-supertype)

;; Initialized here for the cold generator and in cold-start for normal operation
(defvar *type-info* (make-hash-table))
(defvar *type-info-lock* nil)

(defun type-info-for (name &optional (create t))
  (check-type name symbol)
  (let ((entry (mezzano.supervisor:with-rw-lock-read (*type-info-lock*)
                 (gethash name *type-info*))))
    (when (and (not entry) create)
      (mezzano.supervisor:with-rw-lock-write (*type-info-lock*)
        (let ((new-entry (make-type-info)))
          (setf entry (or (cas (gethash name *type-info*) nil new-entry)
                          new-entry)))))
    entry))

(defun %deftype (name expander documentation)
  (let ((info (type-info-for name)))
    (setf (type-info-docstring info) documentation
          (type-info-maybe-class info) nil
          (type-info-type-expander info) expander)))
(defun %define-compound-type (name function)
  (let ((info (type-info-for name)))
    (setf (type-info-maybe-class info) nil
          (type-info-compound-type info) function)))
(defun %define-compound-type-optimizer (name function)
  (let ((info (type-info-for name)))
    (setf (type-info-maybe-class info) nil
          (type-info-compound-type-optimizer info) function)))
(defun %define-type-symbol (name function)
  (let ((info (type-info-for name)))
    (setf (type-info-maybe-class info) nil
          (type-info-type-symbol info) function)))

(defun %compiler-defclass (name)
  ;; If name exists as a type, do nothing.
  ;; Otherwise, mark it as a potential class.
  (let ((info (type-info-for name)))
    (unless (or (type-info-type-expander info)
                (type-info-compound-type info)
                (type-info-type-symbol info))
      (setf (type-info-maybe-class info) t)))
  name)

(defun type-specifier-p (name)
  (let ((info (type-info-for name nil)))
    (and info
         (or (type-info-type-expander info)
             (type-info-compound-type info)
             (type-info-type-symbol info)
             (type-info-maybe-class info)))))
)

(deftype bit ()
  '(integer 0 1))

(deftype unsigned-byte (&optional s)
  (cond ((eql s '*)
         `(integer 0))
        (t (unless (and (integerp s) (plusp s))
             (error 'type-error :expected-type '(integer 1) :datum s))
           `(integer 0 ,(1- (expt 2 s))))))

(deftype signed-byte (&optional s)
  (cond ((eql s '*)
         'integer)
        (t (unless (and (integerp s) (plusp s))
             (error 'type-error :expected-type '(integer 1) :datum s))
           `(integer ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s)))))))

(deftype mod (n)
  (unless (and (integerp n) (plusp n))
    (error 'type-error :expected-type '(integer 1) :datum n))
  `(integer 0 (,n)))

(deftype long-float (&optional min max)
  `(double-float ,min ,max))

(deftype fixnum ()
  `(integer ,most-negative-fixnum ,most-positive-fixnum))

(deftype bignum ()
  `(and integer (not fixnum)))

(deftype positive-fixnum ()
  `(integer 1 ,most-positive-fixnum))

(deftype non-negative-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(deftype negative-fixnum ()
  `(integer ,most-negative-fixnum -1))

(deftype non-positive-fixnum ()
  `(integer ,most-negative-fixnum 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun typeexpand-1 (type &optional environment)
  (when (not (or (symbolp type)
                 (listp type)))
    (return-from typeexpand-1 (values type nil)))
  (let ((expander (let ((info (type-info-for
                               (if (symbolp type)
                                   type
                                   (first type))
                               nil)))
                    (and info (type-info-type-expander info)))))
    (cond (expander
           (when (symbolp type)
             (setf type (list type)))
           (values (funcall expander type environment) t))
          ((and (consp type)
                (member (first type) '(array simple-array))
                (second type))
           (multiple-value-bind (expanded-inner expandedp)
               (typeexpand (second type) environment)
             (if expandedp
                 (values (list* (first type)
                                expanded-inner
                                (cddr type))
                         t)
                 (values type nil))))
          ((or (eql type 'complex)
               (and (consp type)
                    (eql (first type) 'complex)
                    (or (null (rest type))
                        (eql (second type) '*))))
           '(or (complex rational)
                (complex short-float)
                (complex single-float)
                (complex double-float)))
          (t (values type nil)))))

(defun typeexpand (type &optional environment)
  (do ((have-expanded nil)) (nil)
    (multiple-value-bind (expansion expanded-p)
        (typeexpand-1 type environment)
      (unless expanded-p
        (return (values expansion have-expanded)))
      (setf have-expanded t
            type expansion))))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun canonicalize-real-type (type name)
  (if (consp type)
      (destructuring-bind (&optional (min '*) (max '*))
          (cdr type)
        (when (consp min)
          (when (rest min)
            (error "Bad ~S type: ~S." name type))
          (setf min (1- (first min))))
        (unless (or (eql min '*) (typep min name))
          (error "Bad ~S type: ~S." name type))
        (when (consp max)
          (when (rest max)
            (error "Bad ~S type: ~S." name type))
          (setf max (1- (first max))))
        (unless (or (eql max '*) (typep max name))
          (error "Bad ~S type: ~S." name type))
        (values min max))
      (values '* '*)))
)

(defun satisfies-type-p (object type)
  (destructuring-bind (function) (cdr type)
    (funcall function object)))

(%define-compound-type 'satisfies 'satisfies-type-p)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-satisfies-type (object type)
  (destructuring-bind (predicate-name) (rest type)
    `(,predicate-name ,object)))
(%define-compound-type-optimizer 'satisfies 'compile-satisfies-type)
)

(defun integer-type-p (object type)
  (multiple-value-bind (min max)
      (canonicalize-real-type type 'integer)
    (and (integerp object)
         (or (eql min '*)
             (>= object min))
         (or (eql max '*)
             (<= object max)))))
(%define-compound-type 'integer 'integer-type-p)

(defun rational-type-p (object type)
  (multiple-value-bind (min max)
      (canonicalize-real-type type 'rational)
    (and (rationalp object)
         (or (eql min '*)
             (>= object min))
         (or (eql max '*)
             (<= object max)))))
(%define-compound-type 'rational 'rational-type-p)

(defun real-type-p (object type)
  (multiple-value-bind (min max)
      (canonicalize-real-type type 'real)
    (and (realp object)
         (or (eql min '*)
             (>= object min))
         (or (eql max '*)
             (<= object max)))))
(%define-compound-type 'real 'real-type-p)

(defun float-type-p (object type)
  (multiple-value-bind (min max)
      (canonicalize-real-type type 'float)
    (and (floatp object)
         (or (eql min '*)
             (>= object min))
         (or (eql max '*)
             (<= object max)))))
(%define-compound-type 'float 'float-type-p)

(defun short-float-type-p (object type)
  (multiple-value-bind (min max)
      (canonicalize-real-type type 'short-float)
    (and (short-float-p object)
         (or (eql min '*)
             (>= object min))
         (or (eql max '*)
             (<= object max)))))
(%define-compound-type 'short-float 'short-float-type-p)

(defun single-float-type-p (object type)
  (multiple-value-bind (min max)
      (canonicalize-real-type type 'single-float)
    (and (single-float-p object)
         (or (eql min '*)
             (>= object min))
         (or (eql max '*)
             (<= object max)))))
(%define-compound-type 'single-float 'single-float-type-p)

(defun double-float-type-p (object type)
  (multiple-value-bind (min max)
      (canonicalize-real-type type 'double-float)
    (and (double-float-p object)
         (or (eql min '*)
             (>= object min))
         (or (eql max '*)
             (<= object max)))))
(%define-compound-type 'double-float 'double-float-type-p)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-rational-type (object type)
  "Convert a type specifier with interval designators like INTEGER, REAL and RATIONAL."
  (cond ((symbolp type)
         `(typep ,object ',type))
        (t (destructuring-bind (base &optional (min '*) (max '*))
               type
             (when (and (eql base 'integer)
                        (eql min most-negative-fixnum)
                        (eql max most-positive-fixnum))
               (return-from compile-rational-type `(sys.int::fixnump ,object)))
             (when (and (eql base 'integer)
                        (eql min 0)
                        (eql max (1- (expt 2 64))))
               (return-from compile-rational-type `(sys.int::unsigned-byte-64-p ,object)))
             `(and (typep ,object ',base)
                   ,(cond ((eql min '*) 't)
                          ((consp min)
                           (unless (null (rest min))
                             (error "Bad type ~S." type))
                           (when (not (typep (first min) base))
                             (error "Bad type ~S (lower-limit is not of type ~S)."
                                    type base))
                           (if (and (eql base 'integer)
                                    (< (first min) most-negative-fixnum))
                               `(or (fixnump ,object)
                                    (> ,object ',(first min)))
                               `(> ,object ',(first min))))
                          (t
                           (when (not (typep min base))
                             (error "Bad type ~S (lower-limit is not of type ~S)."
                                    type base))
                           (if (and (eql base 'integer)
                                    (<= min most-negative-fixnum))
                               `(or (fixnump ,object)
                                    (>= ,object ',min))
                               `(>= ,object ',min))))
                   ,(cond ((eql max '*) 't)
                          ((consp max)
                           (unless (null (rest max))
                             (error "Bad type ~S." type))
                           (when (not (typep (first max) base))
                             (error "Bad type ~S (upper-limit is not of type ~S)."
                                    type base))
                           (if (and (eql base 'integer)
                                    (> (first max) most-positive-fixnum))
                               `(or (fixnump ,object)
                                    (< ,object ',(first max)))
                               `(< ,object ',(first max))))
                          (t
                           (when (not (typep max base))
                             (error "Bad type ~S (lower-limit is not of type ~S)."
                                    type base))
                           (if (and (eql base 'integer)
                                    (>= max most-positive-fixnum))
                               `(or (fixnump ,object)
                                    (<= ,object ',max))
                               `(<= ,object ',max)))))))))

(%define-compound-type-optimizer 'real 'compile-rational-type)
(%define-compound-type-optimizer 'rational 'compile-rational-type)
(%define-compound-type-optimizer 'integer 'compile-rational-type)
(%define-compound-type-optimizer 'rational 'compile-rational-type)
(%define-compound-type-optimizer 'float 'compile-rational-type)
)

(defun cons-type-p (object type)
  (destructuring-bind (&optional (car-type '*) (cdr-type '*))
      (cdr type)
    (when (eql car-type '*)
      (setf car-type 't))
    (when (eql cdr-type '*)
      (setf cdr-type 't))
    (and (consp object)
         (or (eql car-type 't)
             (typep (car object) car-type))
         (or (eql cdr-type 't)
             (typep (cdr object) cdr-type)))))
(%define-compound-type 'cons 'cons-type-p)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-cons-type (object type)
  (destructuring-bind (&optional (car-type '*) (cdr-type '*))
      (cdr type)
    (when (eql car-type '*)
      (setf car-type 't))
    (when (eql cdr-type '*)
      (setf cdr-type 't))
    `(and (consp ,object)
          ,@(when (not (eql car-type 't))
              `((typep (car ,object) ',car-type)))
          ,@(when (not (eql cdr-type 't))
              `((typep (cdr ,object) ',cdr-type))))))
(%define-compound-type-optimizer 'cons 'compile-cons-type)
)

(deftype null ()
  '(eql nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
(%define-type-symbol 'list 'listp)
(%define-type-symbol 'cons 'consp)
(%define-type-symbol 'symbol 'symbolp)
(%define-type-symbol 'number 'numberp)
(%define-type-symbol 'complex 'complexp)
(%define-type-symbol 'real 'realp)
(%define-type-symbol 'fixnum 'fixnump)
(%define-type-symbol 'bignum 'bignump)
(%define-type-symbol 'integer 'integerp)
(%define-type-symbol 'ratio 'mezzano.internals.numbers.ratio:ratiop)
(%define-type-symbol 'rational 'rationalp)
(%define-type-symbol 'short-float 'short-float-p)
(%define-type-symbol 'single-float 'single-float-p)
(%define-type-symbol 'double-float 'double-float-p)
(%define-type-symbol 'float 'floatp)
(%define-type-symbol 'character 'characterp)
(%define-type-symbol 'string 'stringp)
(%define-type-symbol 'function 'functionp)
(%define-type-symbol 'structure-object 'structure-object-p)
(%define-type-symbol 'keyword 'keywordp)
(%define-type-symbol 't 'yes-its-true)

;; Can't be done in instance.lisp. Too early, causes xcompiler issues.
(%define-type-symbol 'instance-header 'instance-header-p)
)

(defun yes-its-true (object)
  (declare (ignore object))
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun upgraded-complex-part-type (typespec &optional environment)
  (cond
    ((subtypep typespec 'nil environment)
     nil)
    ((subtypep typespec 'short-float environment)
     'short-float)
    ((subtypep typespec 'single-float environment)
     'single-float)
    ((subtypep typespec 'double-float environment)
     'double-float)
    ((subtypep typespec 'rational environment)
     'rational)
    ((subtypep typespec 'real environment)
     'real)
    (t
     (error "Type specifier ~S is not a subtype of REAL." typespec))))
)

(defun complex-type (object type)
  (destructuring-bind (&optional (typespec '*))
      (if (listp type)
          (rest type)
          '(*))
    (and (complexp object)
         (or (eql typespec '*)
             (typep (realpart object)
                    (upgraded-complex-part-type typespec))))))
(%define-compound-type 'complex 'complex-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-complex-type (object type)
  (destructuring-bind (&optional (typespec '*))
      (if (listp type)
          (rest type)
          '(*))
    (cond ((eql typespec '*)
           `(complexp ,object))
          (t
           (let ((upgraded (upgraded-complex-part-type typespec)))
             `(and (complexp ,object)
                   (typep (realpart ,object) ',upgraded)))))))
(%define-compound-type-optimizer 'complex 'compile-complex-type)
)

;; Not exactly correct. This is a class, SUBTYPEP has problems with that.
(deftype list ()
  `(or cons null))

(deftype atom ()
  `(not cons))

(defun or-type (object type)
  (dolist (elt (cdr type))
    (when (typep object elt)
      (return elt))))
(%define-compound-type 'or 'or-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-or-type (object type)
  `(or ,@(mapcar (lambda (x) `(typep ,object ',x)) (rest type))))
(%define-compound-type-optimizer 'or 'compile-or-type)
)

(defun and-type (object type)
  (dolist (elt (cdr type) t)
    (when (not (typep object elt))
      (return nil))))
(%define-compound-type 'and 'and-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-and-type (object type)
  `(and ,@(mapcar (lambda (x) `(typep ,object ',x)) (rest type))))
(%define-compound-type-optimizer 'and 'compile-and-type)
)

(defun not-type (object type)
  (destructuring-bind (_ inner-type) type
    (declare (ignore _))
    (not (typep object inner-type))))
(%define-compound-type 'not 'not-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-not-type (object type)
  (destructuring-bind (_ inner-type) type
    (declare (ignore _))
    `(not (typep ,object ',inner-type))))
(%define-compound-type-optimizer 'not 'compile-not-type)
)

(defun member-type (object type)
  (dolist (o (cdr type))
    (when (eql object o)
      (return t))))
(%define-compound-type 'member 'member-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-member-type (object type)
  `(member ,object ',(cdr type)))
(%define-compound-type-optimizer 'member 'compile-member-type)
)

(defun eql-type (object type)
  (destructuring-bind (other-object) (rest type)
    (eql object other-object)))
(%define-compound-type 'eql 'eql-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-eql-type (object type)
  `(eql ,object ',(second type)))
(%define-compound-type-optimizer 'eql 'compile-eql-type)
)

(deftype boolean ()
  '(member t nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun set-numeric-supertype (type supertype)
  "Set the supertype of a numeric type."
  (setf (type-info-numeric-supertype
         (type-info-for type))
        supertype))

(set-numeric-supertype 'fixnum 'integer)
(set-numeric-supertype 'bignum 'integer)
(set-numeric-supertype 'integer 'rational)
(set-numeric-supertype 'ratio 'rational)
(set-numeric-supertype 'rational 'real)
(set-numeric-supertype 'short-float 'float)
(set-numeric-supertype 'single-float 'float)
(set-numeric-supertype 'double-float 'float)
(set-numeric-supertype 'long-float 'float)
(set-numeric-supertype 'float 'real)
(set-numeric-supertype 'real 'number)
(set-numeric-supertype 'complex 'number)
(set-numeric-supertype 'number 't)

(defun numeric-supertype (type)
  (let ((info (type-info-for type nil)))
    (and info (type-info-numeric-supertype info))))

(defun numeric-subtypep (t1 t2)
  (and (symbolp t1) (symbolp t2)
       (numeric-supertype t1)
       (numeric-supertype t2)
       (or (eql t1 t2)
           (numeric-subtypep (numeric-supertype t1) t2))))

(defun real-subtype-p (type)
  (numeric-subtypep type 'real))
(defun number-subtype-p (type)
  (numeric-subtypep type 'number))
)

(deftype standard-char ()
  '(member
    #\Space #\Newline
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
    #\! #\$ #\" #\' #\( #\) #\, #\_ #\- #\. #\/ #\: #\;
    #\? #\+ #\< #\= #\> #\# #\% #\& #\* #\@ #\[ #\\ #\]
    #\{ #\| #\} #\` #\^ #\~))

(deftype base-char ()
  'character)

(deftype extended-char ()
  'nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun known-type-p (type &optional environment)
  (let ((type (typeexpand type environment)))
    (not (not
          (or (and (symbolp type)
                   (find-class type nil environment))
              ;; Figure 4-3. Standardized Compound Type Specifier Names
              ;; With types defined by deftype (such as string, vector, etc) removed.
              (typep type '(cons (member
                                  and
                                  array
                                  complex
                                  cons
                                  double-float
                                  eql
                                  float
                                  function
                                  integer
                                  long-float
                                  member
                                  not
                                  or
                                  rational
                                  real
                                  satisfies
                                  short-float
                                  simple-array
                                  single-float
                                  string
                                  values
                                  vector))))))))

(defun type-equal (type-1 type-2 &optional environment)
  (multiple-value-bind (ok-1 valid-1)
      (subtypep type-1 type-2 environment)
    (multiple-value-bind (ok-2 valid-2)
        (subtypep type-2 type-1 environment)
      (if (and valid-1 valid-2)
          (values (and ok-1 ok-2) t)
          (values nil nil)))))

;;; This is annoyingly incomplete and isn't particularly well integrated.
(defun subtypep (type-1 type-2 &optional environment)
  (declare (notinline typep)) ; ### Boostrap hack.
  (when (equal type-1 type-2)
    (return-from subtypep (values t t)))
  (when (typep type-2 'class)
    (return-from subtypep (subclassp (if (listp type-1)
                                         (first type-1)
                                         type-1)
                                     type-2)))
  (when (typep type-1 'class)
    (let ((other-class (when (symbolp type-2)
                         (find-class type-2 nil))))
      (when other-class
        (return-from subtypep (subclassp type-1 other-class)))))
  (let ((t1 (typeexpand type-1 environment))
        (t2 (typeexpand type-2 environment)))
    (cond ((equal t1 t2) (values t t))
          ((eql t1 'nil) (values t t))
          ((eql t2 'nil) (values nil t))
          ((and (or (real-subtype-p t2)
                    (and (consp t2)
                         (real-subtype-p (car t2))))
                (or (real-subtype-p t1)
                    (and (consp t1)
                         (real-subtype-p (car t1))))
                (numeric-subtypep (if (consp t1) (car t1) t1) (if (consp t2) (car t2) t2)))
           (multiple-value-bind (min-1 max-1)
               (canonicalize-real-type t1 (if (consp t1) (car t1) t1))
             (multiple-value-bind (min-2 max-2)
                 (canonicalize-real-type t2 (if (consp t2) (car t2) t2))
               (values (and (or (eql min-2 '*)
                                (and (not (eql min-1 '*)) (<= min-2 min-1)))
                            (or (eql max-2 '*)
                                (and (not (eql max-1 '*)) (>= max-2 max-1))))
                       t))))
          ((and (or (number-subtype-p t2)
                    (and (consp t2)
                         (number-subtype-p (first t2))))
                (or (number-subtype-p t1)
                    (and (consp t1)
                         (number-subtype-p (first t1))))
                (numeric-subtypep (if (consp t1) (first t1) t1) (if (consp t2) (first t2) t2)))
           (cond ((and (consp t1)
                       (eql (first t1) 'complex)
                       (consp t2)
                       (eql (first t2) 'complex))
                  (subtypep (second t1) (second t2)))
                 ((and (consp t1)
                       (eql (first t1) 'complex))
                  (subtypep 'number t2))
                 ((and (consp t2)
                       (eql (first t2) 'complex))
                  (values nil t))
                 (t
                  (values t t))))
          ((and (eql t2 'character)
                (or (eql t1 'standard-char)
                    ;; aka standard-char.
                    (equal t1 '(and character (satisfies standard-char-p)))
                    (eql t1 'character)))
           (values t t))
          ((eql t2 't)
           (values t t))
          ((and (or (and (consp t1) (member (first t1) '(array simple-array)))
                    (member t1 '(array simple-array)))
                (or (and (consp t2) (member (first t2) '(array simple-array)))
                    (member t2 '(array simple-array))))
           (destructuring-bind (t1-base &optional (t1-element-type '*) (t1-dimension-spec '*))
               (if (consp t1) t1 (list t1))
             (destructuring-bind (t2-base &optional (t2-element-type '*) (t2-dimension-spec '*))
                 (if (consp t2) t2 (list t2))
               (when (integerp t1-dimension-spec)
                 (setf t1-dimension-spec (make-list t1-dimension-spec :initial-element '*)))
               (when (integerp t2-dimension-spec)
                 (setf t2-dimension-spec (make-list t2-dimension-spec :initial-element '*)))
               (values (and (or (eql t2-base 'array)
                                (eql t1-base 'simple-array))
                            (cond ((eql t2-element-type '*) t)
                                  ((eql t1-element-type '*) nil)
                                  ;; U-A-E-T always returns consistent results,
                                  ;; so EQUAL is acceptable for determining
                                  ;; type equality here.
                                  (t (equal (upgraded-array-element-type t1-element-type)
                                            (upgraded-array-element-type t2-element-type))))
                            (cond ((eql t2-dimension-spec '*) t)
                                  ((eql t1-dimension-spec '*) nil)
                                  ((not (eql (length t1-dimension-spec)
                                             (length t2-dimension-spec)))
                                   nil)
                                  (t (every 'identity
                                            (mapcar (lambda (l r)
                                                      (cond ((eql r '*) t)
                                                            ((eql l '*) nil)
                                                            (t (<= l r))))
                                                    t1-dimension-spec
                                                    t2-dimension-spec)))))
                       t))))
          ((and (consp t1) (eql (first t1) 'eql))
           (handler-case
               (destructuring-bind (object) (rest t1)
                 (values (if (and (consp t2) (eql (first t2) 'function))
                             ;; Reduce complicated FUNCTION types down to the symbol
                             (typep object 'function)
                             (typep object t2))
                         t))
             (unknown-type-specifier-error ()
               (values nil nil))))
          ((and (consp t1) (eql (first t1) 'member))
           (subtypep `(or ,@(loop for object in (rest t1)
                               collect `(eql ,object)))
                     t2))
          ((and (consp t2)
                (eql (first t2) 'not))
           (cond ((not (subtypep t1 (second t2)))
                  (multiple-value-bind (subtype-p valid-p)
                      (subtypep (second t2) t1)
                    (if valid-p
                        (values (not subtype-p) t)
                        (values nil nil))))
                 (t
                  (values nil t))))
          ((and (consp t1)
                (eql (first t1) 'or))
           (dolist (type (rest t1) (values t t))
             (unless (subtypep type t2)
               (return (values nil t)))))
          ((and (consp t1)
                (eql (first t1) 'and))
           (cond ((endp (rest t1))
                  (subtypep 't t2 environment))
                 ((endp (rest (rest t1)))
                  (subtypep (second t1) t2 environment))
                 ((endp (rest (rest (rest t1))))
                  (let ((lhs (second t1))
                        (rhs (third t1)))
                    (cond ((subtypep lhs rhs environment)
                           (subtypep lhs t2 environment))
                          ((subtypep rhs lhs environment)
                           (subtypep rhs t2 environment))
                          (t
                           (subtypep t2 nil environment)))))
                 (t
                  (subtypep `(and (and (second t1) (third t1))
                                  ,@(rest (rest (rest t1))))
                            t2
                            environment))))
          ((and (consp t2)
                (eql (first t2) 'or))
           (dolist (type (rest t2) (values nil t))
             (when (subtypep t1 type)
               (return (values t t)))))
          ((and (consp t2)
                (eql (first t2) 'and))
           (dolist (type (rest t2) (values t t))
             (when (not (subtypep t1 type))
               (return (values nil t)))))
          ((and (consp t1)
                (eql (first t1) 'not))
           (values nil nil))
          ((and (symbolp t1)
                (find-class t1 nil)
                (symbolp t2)
                (find-class t2 nil))
           (subclassp (find-class t1 nil) (find-class t2 nil)))
          ((or (and (consp t1) (eql (first t1) 'satisfies))
               (and (consp t2) (eql (first t2) 'satisfies)))
           (values nil nil))
          (t
           (values nil (and (known-type-p t1 environment)
                            (known-type-p t2 environment)))))))

(defun subclassp (class-1 class-2)
  (declare (notinline typep)) ; ### Boostrap hack.
  (let ((c1 (if (typep class-1 'class)
                class-1
                (find-class class-1 nil))))
    (cond (c1 (values (member class-2 (mezzano.clos::safe-class-precedence-list c1)) t))
          (t (values nil nil)))))
)

(defun class-typep (object class)
  (let ((obj-class (class-of object)))
    (or (eq obj-class class)
        (member class (mezzano.clos::safe-class-precedence-list obj-class)
                :test #'eq))))

(defun typep (object type-specifier &optional environment)
  (declare (notinline find-class)) ; ### Boostrap hack.
  (when (or (eql type-specifier 'values)
            (and (consp type-specifier)
                 (member (first type-specifier) '(values function))))
    (error "Type specifier ~S cannot be used with TYPEP" type-specifier))
  (when (and (instance-p type-specifier)
             (subclassp (class-of type-specifier) (find-class 'mezzano.clos:class)))
    (return-from typep
      (class-typep object type-specifier)))
  (let ((type-symbol (cond ((symbolp type-specifier)
                            type-specifier)
                           ((and (consp type-specifier)
                                 (null (rest type-specifier)))
                            (first type-specifier)))))
    (when type-symbol
      (let ((test (let ((info (type-info-for type-symbol nil)))
                         (and info
                              (type-info-type-symbol info)))))
        (when test
          (return-from typep (funcall test object))))))
  (when (symbolp type-specifier)
    (let ((class (find-class type-specifier nil)))
      (when class
        (return-from typep
          (if (mezzano.runtime::structure-class-p class)
              (structure-type-p object class)
              (class-typep object class))))))
  (let ((compound-test (let ((info (type-info-for
                                    (if (symbolp type-specifier)
                                        type-specifier
                                        (first type-specifier))
                                    nil)))
                         (and info
                              (type-info-compound-type info)))))
    (when compound-test
      (return-from typep (funcall compound-test object type-specifier))))
  (when (eql type-specifier 'nil)
    (return-from typep nil))
  (multiple-value-bind (expansion expanded-p)
      (typeexpand-1 type-specifier environment)
    (if expanded-p
        (typep object expansion)
        (error 'unknown-type-specifier-error :type-specifier type-specifier))))

(defun check-type-error (place value typespec string)
  (restart-case (if string
                    (error 'simple-type-error
                           :expected-type typespec
                           :datum value
                           :format-control "The value of ~S is ~S, which is not ~A."
                           :format-arguments (list place value string))
                    (error 'simple-type-error
                           :expected-type typespec
                           :datum value
                           :format-control "The value of ~S is ~S, which is not of type ~S."
                           :format-arguments (list place value typespec)))
    (store-value (v)
      :interactive (lambda ()
                     (format t "Enter a new value (evaluated): ")
                     (list (eval (read))))
      :report (lambda (s) (format s "Input a new value for ~S." place))
      v)))

(defmacro check-type (&environment environment place typespec &optional string)
  (when (typep typespec '(cons (eql quote)))
    (error "CHECK-TYPE TYPESPEC ~S must not be quoted" typespec))
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    (let ((value (gensym))
          (original-value (gensym)))
      ;; This is written in a slightly convoluted way to allow the compiler
      ;; to optimize away CHECK-TYPE at low safety when the types are known.
      `(let* (,@(mapcar #'list vars vals)
              (,original-value ,reader-form))
         (when (not (typep ,original-value ',typespec))
           (do ((,value ,original-value ,reader-form))
               ((typep ,value ',typespec))
             (let ((,(first store-vars) (check-type-error ',place ,value ',typespec ,string)))
               ,writer-form)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-typep-expression (object type-specifier)
  (let ((type-symbol (cond ((symbolp type-specifier)
                            type-specifier)
                           ((and (consp type-specifier)
                                 (null (rest type-specifier))
                                 (symbolp (first type-specifier)))
                            (first type-specifier)))))
    (when type-symbol
      (cond ((eql type-symbol 't)
             (return-from compile-typep-expression `(progn ,object 't)))
            ((eql type-symbol 'nil)
             (return-from compile-typep-expression `(progn ,object 'nil)))
            (t
             (let ((test (let ((info (type-info-for type-symbol nil)))
                           (and info
                                (type-info-type-symbol info)))))
               (when test
                 (return-from compile-typep-expression
                   `(funcall ',test ,object))))))))
  (when (and (listp type-specifier)
             (symbolp (first type-specifier)))
    (let ((compiler (let ((info (type-info-for (first type-specifier) nil)))
                      (and info
                           (type-info-compound-type-optimizer info)))))
      (when compiler
        (let* ((sym (gensym))
               (code (funcall compiler sym type-specifier)))
          (when code
            (return-from compile-typep-expression
              `(let ((,sym ,object))
                 ,code)))))))
  (when (symbolp type-specifier)
    (let ((struct-type (get-structure-type type-specifier nil))
          (object-sym (gensym "OBJECT")))
      (when struct-type
        (return-from compile-typep-expression
          (if (mezzano.clos:class-sealed struct-type)
              `(let ((,object-sym ,object))
                 (and (%value-has-tag-p ,object-sym ,+tag-object+)
                      (%fast-instance-layout-eq-p
                       ,object-sym
                       ',(mezzano.runtime::%make-instance-header
                          (mezzano.clos:class-layout struct-type)))))
              `(let ((,object-sym ,object))
                 (or
                  (and (%value-has-tag-p ,object-sym ,+tag-object+)
                       (%fast-instance-layout-eq-p
                        ,object-sym
                        ',(mezzano.runtime::%make-instance-header
                           (mezzano.clos:class-layout struct-type))))
                  (structure-type-p ,object-sym ',struct-type))))))))
  (when (eql type-specifier 'sequence)
    ;; Open-code parts the sequence type check.
    ;; This is a performance hack that replaces the old DEFTYPE SEQUENCE,
    ;; which was just plain wrong.
    (return-from compile-typep-expression
      (let ((object-sym (gensym "OBJECT")))
        `(let ((,object-sym ,object))
           (or (listp ,object-sym)
               (vectorp ,object-sym))))))
  (when (and (symbolp type-specifier)
             (let ((info (type-info-for type-specifier nil)))
               (and info
                    (type-info-maybe-class info))))
    (return-from compile-typep-expression
      (let ((class (gensym "CLASS"))
            (object-sym (gensym "OBJECT")))
        `(let ((,object-sym ,object)
               (,class (mezzano.clos::find-class-in-reference
                        (load-time-value (mezzano.clos::class-reference ',type-specifier))
                        nil)))
           (if ,class
               (class-typep ,object-sym ,class)
               nil)))))
  nil)
)

(define-compiler-macro typep (&whole whole object type-specifier &optional environment)
  ;; Simple environments only.
  (when environment
    (return-from typep whole))
  ;; Only deal with quoted type specifiers.
  (unless (and (listp type-specifier)
               (= (list-length type-specifier) 2)
               (eql (first type-specifier) 'quote))
    (return-from typep whole))
  ;; Special-case COMPLEX here or typeexpand will turn it into a more
  ;; complicated OR type.
  (when (eql (second type-specifier) 'complex)
    (return-from typep `(complexp ,object)))
  (or (compile-typep-expression object
                                (typeexpand (second type-specifier) environment))
      whole))

(defun type-of (object)
  (cond ((fixnump object)
         (case object
           ((0 1) 'bit)
           (t 'fixnum)))
        ;; Non-fixnum immediates.
        ((characterp object)
         (if (standard-char-p object)
             'standard-char
             'character))
        ((single-float-p object)
         'single-float)
        ((mezzano.internals.numbers.logical::small-byte-p object)
         'byte)
        ;; Heap objects.
        ((consp object)
         'cons)
        ((%value-has-tag-p object +tag-object+)
         (case (%object-tag object)
           (#.+object-tag-array-t+
            `(simple-vector ,(array-dimension object 0)))
           (#.+object-tag-array-fixnum+
            `(simple-array fixnum (,(array-dimension object 0))))
           (#.+object-tag-array-bit+
            `(simple-bit-vector ,(array-dimension object 0)))
           (#.+object-tag-array-unsigned-byte-2+
            `(simple-array (unsigned-byte 2) (,(array-dimension object 0))))
           (#.+object-tag-array-unsigned-byte-4+
            `(simple-array (unsigned-byte 4) (,(array-dimension object 0))))
           (#.+object-tag-array-unsigned-byte-8+
            `(simple-array (unsigned-byte 8) (,(array-dimension object 0))))
           (#.+object-tag-array-unsigned-byte-16+
            `(simple-array (unsigned-byte 16) (,(array-dimension object 0))))
           (#.+object-tag-array-unsigned-byte-32+
            `(simple-array (unsigned-byte 32) (,(array-dimension object 0))))
           (#.+object-tag-array-unsigned-byte-64+
            `(simple-array (unsigned-byte 64) (,(array-dimension object 0))))
           (#.+object-tag-array-signed-byte-1+
            `(simple-array (signed-byte 1) (,(array-dimension object 0))))
           (#.+object-tag-array-signed-byte-2+
            `(simple-array (signed-byte 2) (,(array-dimension object 0))))
           (#.+object-tag-array-signed-byte-4+
            `(simple-array (signed-byte 4) (,(array-dimension object 0))))
           (#.+object-tag-array-signed-byte-8+
            `(simple-array (signed-byte 8) (,(array-dimension object 0))))
           (#.+object-tag-array-signed-byte-16+
            `(simple-array (signed-byte 16) (,(array-dimension object 0))))
           (#.+object-tag-array-signed-byte-32+
            `(simple-array (signed-byte 32) (,(array-dimension object 0))))
           (#.+object-tag-array-signed-byte-64+
            `(simple-array (signed-byte 64) (,(array-dimension object 0))))
           (#.+object-tag-array-single-float+
            `(simple-array single-float (,(array-dimension object 0))))
           (#.+object-tag-array-double-float+
            `(simple-array double-float (,(array-dimension object 0))))
           (#.+object-tag-array-short-float+
            `(simple-array short-float (,(array-dimension object 0))))
           (#.+object-tag-array-long-float+
            `(simple-array long-float (,(array-dimension object 0))))
           (#.+object-tag-array-complex-single-float+
            `(simple-array (complex single-float) (,(array-dimension object 0))))
           (#.+object-tag-array-complex-double-float+
            `(simple-array (complex double-float) (,(array-dimension object 0))))
           (#.+object-tag-array-complex-short-float+
            `(simple-array (complex short-float) (,(array-dimension object 0))))
           (#.+object-tag-array-complex-long-float+
            `(simple-array (complex long-float) (,(array-dimension object 0))))
           (#.+object-tag-simple-string+
            (if (eql (array-rank object) 1)
                `(simple-string ,(array-dimension object 0))
                `(simple-array character ,(array-dimensions object))))
           (#.+object-tag-string+
            (if (eql (array-rank object) 1)
                `(string ,(array-dimension object 0))
                `(array character ,(array-dimensions object))))
           (#.+object-tag-simple-array+
            `(simple-array ,(array-element-type object) ,(array-dimensions object)))
           (#.+object-tag-array+
            (if (eql (array-rank object) 1)
                `(vector ,(array-element-type object) ,(array-dimension object 0))
                `(array ,(array-element-type object) ,(array-dimensions object))))
           (#.+object-tag-bignum+
            'bignum)
           (#.+object-tag-ratio+
            'ratio)
           (#.+object-tag-double-float+
            'double-float)
           (#.+object-tag-short-float+
            'short-float)
           (#.+object-tag-long-float+
            'long-float)
           (#.+object-tag-complex-rational+
            '(complex rational))
           (#.+object-tag-complex-single-float+
            '(complex single-float))
           (#.+object-tag-complex-double-float+
            '(complex double-float))
           (#.+object-tag-complex-short-float+
            '(complex short-float))
           (#.+object-tag-complex-long-float+
            '(complex long-float))
           (#.+object-tag-symbol-value-cell+
            'mezzano.runtime::symbol-value-cell)
           (#.+object-tag-mmx-vector+
            'mezzano.simd:mmx-vector)
           (#.+object-tag-symbol+
            (cond ((eql object 'nil) 'null)
                  ((eql object 't) 'boolean)
                  ((keywordp object) 'keyword)
                  (t 'symbol)))
           (#.+object-tag-sse-vector+
            'mezzano.simd:sse-vector)
           ((#.+object-tag-instance+
             #.+object-tag-funcallable-instance+)
            (let* ((class (class-of object))
                   (name (class-name class)))
              (if (eql class (find-class name nil))
                  name
                  class)))
           (#.+object-tag-function-reference+
            'function-reference)
           (#.+object-tag-interrupt-frame+
            'interrupt-frame)
           (#.+object-tag-cons+
            'pinned-cons)
           (#.+object-tag-freelist-entry+
            'freelist-entry)
           (#.+object-tag-weak-pointer+
            'weak-pointer)
           (#.+object-tag-delimited-continuation+
            'mezzano.delimited-continuations:delimited-continuation)
           (#.+object-tag-function+
            'compiled-function)
           (#.+object-tag-closure+ 'closure)
           (t
            `(invalid-object ,(%object-tag object)))))
        ((%value-has-tag-p object +tag-instance-header+)
         'instance-header)
        ;; Invalid objects, these shouldn't be seen in the machine.
        ((%value-has-tag-p object +tag-dx-root-object+)
         'dx-root)
        ((%value-has-tag-p object +tag-gc-forward+)
         'gc-forwarding-pointer)
        (t
         `(invalid-value ,(%tag-field object)))))
