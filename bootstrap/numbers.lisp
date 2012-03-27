(in-package "SYSTEM.INTERNALS")

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
(define-commutative-arithmetic-operator logeqv binary-logeqv -1)
(define-commutative-arithmetic-operator logior binary-logior 0)
(define-commutative-arithmetic-operator logxor binary-logxor 0)

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

(defmacro define-comparison-operator (name base)
  `(progn (defun ,name (number &rest more-numbers)
            (declare (dynamic-extent more-numbers))
            (check-type number number)
            (dolist (n more-numbers t)
              (unless (,base number n)
                (return nil))
              (setf number n)))
          (define-compiler-macro ,name (&whole whole number &rest more-numbers)
            (declare (dynamic-extent more-numbers))
            (cond ((null more-numbers) 't)
                  ((null (rest more-numbers))
                   `(,',base ,number ,(first more-numbers)))
                  (t whole)))))

(define-comparison-operator < binary-<)
(define-comparison-operator <= binary-<=)
(define-comparison-operator > binary->)
(define-comparison-operator >= binary->=)
(define-comparison-operator = binary-=)

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
          (stemp (first stores))) ;Temp var for int to store.
      (when (cdr stores) (error "Can't expand this."))
      ;; Generate (dpb ... (byte x y) ...) when the bytespec is
      ;; well-known.
      (if (and (listp bytespec)
               (eql (length bytespec) 3)
               (eql (first bytespec) 'byte)
               (integerp (second bytespec))
               (integerp (third bytespec)))
          (values temps       ;Temporary variables.
                  vals     ;Value forms.
                  (list store)             ;Store variables.
                  `(let ((,stemp (dpb ,store ,bytespec ,access-form)))
                     ,store-form
                     ,store)               ;Storing form.
                  `(ldb ,bytespec ,access-form) ;Accessing form.
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
