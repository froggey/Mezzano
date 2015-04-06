;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defconstant most-negative-short-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-negative-single-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-negative-double-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-negative-long-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-positive-short-float (%integer-as-single-float #x7F7FFFFF))
(defconstant most-positive-single-float (%integer-as-single-float #x7F7FFFFF))
(defconstant most-positive-double-float (%integer-as-single-float #x7F7FFFFF))
(defconstant most-positive-long-float (%integer-as-single-float #x7F7FFFFF))

(defmacro define-commutative-arithmetic-operator (name base identity)
  `(progn (defun ,name (&rest numbers)
            (declare (dynamic-extent numbers))
            (let ((result ,identity))
              (dolist (n numbers)
                (setf result (,base result n)))
              result))
          (define-compiler-macro ,name (&rest numbers)
            (declare (dynamic-extent numbers))
            (cond ((null numbers) ',identity)
                  ((null (rest numbers))
                   `(the number ,(first numbers)))
                  (t (let ((result (first numbers)))
                       (dolist (n (rest numbers))
                         (setf result (list ',base result n)))
                       result))))))

(define-commutative-arithmetic-operator + binary-+ 0)
(define-commutative-arithmetic-operator * binary-* 1)
(define-commutative-arithmetic-operator logand binary-logand -1)
(define-commutative-arithmetic-operator logior binary-logior 0)
(define-commutative-arithmetic-operator logxor binary-logxor 0)

;;; LOGEQV is funny, it doesn't have a compiler builtin.
(define-compiler-macro logeqv (&rest numbers)
  `(lognot (logxor ,@numbers)))

(defun logeqv (&rest numbers)
  (declare (dynamic-extent numbers))
  (lognot (apply #'logxor numbers)))

;;; - and / do not fit into the previous template, so have to be
;;; explicitly defined.

(defun - (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (cond (more-numbers
         (let ((result number))
           (dolist (n more-numbers)
             (setf result (binary-- result n)))
           result))
        (t (binary-- 0 number))))

(define-compiler-macro - (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (cond ((null more-numbers) `(binary-- 0 ,number))
        (t (let ((result number))
             (dolist (n more-numbers)
               (setf result `(binary-- ,result ,n)))
             result))))

(defun / (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (cond (more-numbers
         (let ((result number))
           (dolist (n more-numbers)
             (setf result (binary-/ result n)))
           result))
        (t (binary-/ 1 number))))

(define-compiler-macro / (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (cond ((null more-numbers) `(binary-/ 1 ,number))
        (t (let ((result number))
             (dolist (n more-numbers)
               (setf result `(binary-/ ,result ,n)))
             result))))

(declaim (inline truncate))
(defun truncate (number &optional (divisor 1))
  (%truncate number divisor))

;; Can't use DEFINE-COMMUTATIVE-ARITHMETIC-OPERATOR here because one-arg GCD is ABS.
;; Types are also wrong (integer vs number).
(defun gcd (&rest integers)
  (declare (dynamic-extent integers))
  (cond
    ((endp integers) 0)
    ((endp (rest integers))
     (check-type (first integers) integer)
     (abs (first integers)))
    (t (reduce #'two-arg-gcd integers))))

(define-compiler-macro gcd (&rest integers)
  (declare (dynamic-extent integers))
  (cond ((null integers) '0)
        ((null (rest integers))
         `(abs (the integer ,(first integers))))
        (t (let ((result (first integers)))
             (dolist (n (rest integers))
               (setf result (list 'two-arg-gcd result n)))
             result))))

(defmacro define-comparison-operator (name base type)
  `(progn (defun ,name (number &rest more-numbers)
            (declare (dynamic-extent more-numbers))
            (check-type number ,type)
            (dolist (n more-numbers t)
              (unless (,base number n)
                (return nil))
              (setf number n)))
          (define-compiler-macro ,name (number &rest more-numbers)
            (declare (dynamic-extent more-numbers))
            (let ((n-numbers (1+ (length more-numbers))))
              (case n-numbers
                (1 `(progn (check-type ,number ,',type) 't))
                (2 `(,',base ,number ,(first more-numbers)))
                (t (let* ((all-nums (list* number more-numbers))
                          (syms (loop for i below n-numbers
                                   collect (gensym))))
                     `(let ,(mapcar #'list syms all-nums)
                        (and ,@(let ((prev (first syms)))
                                    (mapcar (lambda (sym)
                                              (prog1 (list ',base prev sym)
                                                (setf prev sym)))
                                            (rest syms))))))))))))

(define-comparison-operator < binary-< real)
(define-comparison-operator <= binary-<= real)
(define-comparison-operator > binary-> real)
(define-comparison-operator >= binary->= real)
(define-comparison-operator = binary-= number)

(defun /= (number &rest more-numbers)
  "Returns true if no two numbers are the same in value; otherwise, returns false."
  (declare (dynamic-extent more-numbers))
  (check-type number number)
  (do ((lhs number (car n))
       (n more-numbers (cdr n)))
      ((endp n) t)
    (dolist (rhs n)
      (check-type rhs number)
      (when (= lhs rhs)
	(return-from /= nil)))))

(define-compiler-macro /= (&whole whole number &rest more-numbers)
  (case (length more-numbers)
    (0 `(the number ,number))
    (1 `(not (= ,number ,(first more-numbers))))
    (t whole)))

(defun min (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (check-type number number)
  (dolist (n more-numbers number)
    (when (< n number)
      (setf number n))))

(define-compiler-macro min (number &rest more-numbers)
  (cond
    ((null more-numbers)
     `(the number ,number))
    ((null (rest more-numbers))
     (let ((lhs (gensym))
           (rhs (gensym)))
       `(let ((,lhs ,number)
              (,rhs ,(first more-numbers)))
          (if (< ,lhs ,rhs)
              ,lhs
              ,rhs))))
    (t (let* ((n (gensym))
              (symbols (mapcar (lambda (x)
                                 (declare (ignore x))
                                 (gensym))
                               more-numbers)))
         `(let ,(cons (list n number)
                      (mapcar 'list symbols more-numbers))
            ,@(mapcar (lambda (sym)
                        `(when (< ,sym ,n)
                           (setf ,n ,sym)))
                      symbols)
            ,n)))))

(defun max (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (check-type number number)
  (dolist (n more-numbers number)
    (when (> n number)
      (setf number n))))

(define-compiler-macro max (number &rest more-numbers)
  (cond
    ((null more-numbers)
     `(the number ,number))
    ((null (rest more-numbers))
     (let ((lhs (gensym))
           (rhs (gensym)))
       `(let ((,lhs ,number)
              (,rhs ,(first more-numbers)))
          (if (> ,lhs ,rhs)
              ,lhs
              ,rhs))))
    (t (let* ((n (gensym))
              (symbols (mapcar (lambda (x)
                                 (declare (ignore x))
                                 (gensym))
                               more-numbers)))
         `(let ,(cons (list n number)
                      (mapcar 'list symbols more-numbers))
            ,@(mapcar (lambda (sym)
                        `(when (> ,sym ,n)
                           (setf ,n ,sym)))
                      symbols)
            ,n)))))

(declaim (inline 1+))
(defun 1+ (x)
  (+ x 1))

(declaim (inline 1-))
(defun 1- (x)
  (- x 1))

(declaim (inline plusp))
(defun plusp (number)
  (> number 0))

(declaim (inline minusp))
(defun minusp (number)
  (< number 0))

(declaim (inline zerop))
(defun zerop (number)
  (= number 0))

(declaim (inline evenp))
(defun evenp (integer)
  (check-type integer integer)
  (eql (logand integer 1) 0))

(declaim (inline oddp))
(defun oddp (integer)
  (check-type integer integer)
  (eql (logand integer 1) 1))

(declaim (inline logtest))
(defun logtest (integer-1 integer-2)
  (not (zerop (logand integer-1 integer-2))))

(define-setf-expander ldb (bytespec int &environment env)
  (multiple-value-bind (temps vals stores
                              store-form access-form)
      (get-setf-expansion int env);Get setf expansion for int.
    (let ((btemp (gensym))     ;Temp var for byte specifier.
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores)) ;Temp var for int to store.
          (bs-size (gensym))   ; Temp var for byte specifier size.
          (bs-position (gensym))) ; Temp var for byte specifier position.
      (when (cdr stores) (error "Can't expand this."))
      ;; Generate calls to %LDB and %DPB when the bytespec is
      ;; well-known.
      (if (and (listp bytespec)
               (eql (length bytespec) 3)
               (eql (first bytespec) 'byte))
          (values (list* bs-size bs-position temps)       ;Temporary variables.
                  (list* (second bytespec) (third bytespec) vals)     ;Value forms.
                  (list store)             ;Store variables.
                  `(let ((,stemp (%dpb ,store ,bs-size ,bs-position ,access-form)))
                     ,store-form
                     ,store)               ;Storing form.
                  `(%ldb ,bs-size ,bs-position ,access-form) ;Accessing form.
                  )
          ;; Return the setf expansion for LDB as five values.
          (values (cons btemp temps)       ;Temporary variables.
                  (cons bytespec vals)     ;Value forms.
                  (list store)             ;Store variables.
                  `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                     ,store-form
                     ,store)               ;Storing form.
                  `(ldb ,btemp ,access-form) ;Accessing form.
                  )))))

(define-setf-expander mask-field (bytespec integer &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion integer env);Get setf expansion for int.
    (let ((btemp (gensym))     ;Temp var for byte specifier.
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores)) ;Temp var for int to store.
          (bs-size (gensym))   ; Temp var for byte specifier size.
          (bs-position (gensym))) ; Temp var for byte specifier position.
      (when (cdr stores) (error "Can't expand this."))
      ;; Generate calls to %MASK-FIELD and %DEPOSIT-FIELD when the bytespec is
      ;; well-known.
      (if (and (listp bytespec)
               (eql (length bytespec) 3)
               (eql (first bytespec) 'byte))
          (values (list* bs-size bs-position temps)       ;Temporary variables.
                  (list* (second bytespec) (third bytespec) vals)     ;Value forms.
                  (list store)             ;Store variables.
                  `(let ((,stemp (%deposit-field ,store ,bs-size ,bs-position ,access-form)))
                     ,store-form
                     ,store)               ;Storing form.
                  `(%mask-field ,bs-size ,bs-position ,access-form) ;Accessing form.
                  )
          ;; Return the setf expansion for LDB as five values.
          (values (cons btemp temps)       ;Temporary variables.
                  (cons bytespec vals)     ;Value forms.
                  (list store)             ;Store variables.
                  `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
                     ,store-form
                     ,store)               ;Storing form.
                  `(mask-field ,btemp ,access-form) ;Accessing form.
                  )))))

(define-compiler-macro ldb (&whole whole bytespec integer)
  ;; Avoid creating a byte specifier.
  ;; (LDB (BYTE size position) integer)
  ;; => (%LDB size position integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte))
         `(%ldb ,(second bytespec)
                ,(third bytespec)
                ,integer))
        (t whole)))

(define-compiler-macro dpb (&whole whole newbyte bytespec integer)
  ;; Avoid creating a byte specifier.
  ;; (DPB newbyte (BYTE size position) integer)
  ;; => (%DPB newbyte size position integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte))
         `(%dpb ,newbyte
                ,(second bytespec)
                ,(third bytespec)
                ,integer))
        (t whole)))

(define-compiler-macro mask-field (&whole whole bytespec integer)
  ;; Avoid creating a byte specifier.
  ;; (MASK-FIELD (BYTE size position) integer)
  ;; => (%MASK-FIELD size position integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte))
         `(%mask-field ,(second bytespec)
                       ,(third bytespec)
                       ,integer))
        (t whole)))

(define-compiler-macro deposit-field (&whole whole newbyte bytespec integer)
  ;; Avoid creating a byte specifier.
  ;; (DEPOSIT-FIELD newbyte (BYTE size position) integer)
  ;; => (%DEPOSIT-FIELD newbyte size position integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte))
         `(%deposit-field ,newbyte
                          ,(second bytespec)
                          ,(third bytespec)
                          ,integer))
        (t whole)))

(define-compiler-macro ldb-test (&whole whole bytespec integer)
  ;; Avoid creating a byte specifier.
  ;; (LDB-TEST (BYTE size position) integer)
  ;; => (%LDB-TEST size position integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte))
         `(%ldb-test ,(second bytespec)
                     ,(third bytespec)
                     ,integer))
        (t whole)))

(defun parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  (setf end (or end (length string)))
  (let ((negativep nil)
        (n 0))
    ;; Eat leading/trailing whitespace.
    (do () ((or (>= start end)
                (and (not (member (char string start) '(#\Space #\Newline #\Tab))))))
      (incf start))
    (when (>= start end)
      (if junk-allowed
          (return-from parse-integer (values nil start))
          (error "No non-whitespace characters in ~S." string)))
    (cond ((eql (char string start) #\+)
           (incf start))
          ((eql (char string start) #\-)
           (setf negativep t)
           (incf start)))
    (do ((offset start (1+ offset)))
        ((or (>= offset end)
             (member (char string offset) '(#\Space #\Newline #\Tab)))
         (when negativep
           (setf n (- n)))
         (values n offset))
      (let ((weight (digit-char-p (char string offset) radix)))
        (when (not weight)
          (if junk-allowed
              (return-from parse-integer
                (values (if (eql offset start)
                            nil
                            n)
                        offset))
              (error "Not a parseable integer ~S." string)))
        (setf n (+ (* n radix) weight))))))
