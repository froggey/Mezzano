;;;; Logical and Byte Operations on Integers

(in-package :mezzano.internals.numbers.logical)

;; FIXME: This should inherit from the built-in class byte
(defstruct (large-byte (:constructor make-large-byte (size position)))
  (size 0 :type (integer 0) :read-only t)
  (position 0 :type (integer 0) :read-only t))

;; Stuff size & position into the low 32-bits of small bytes
(defconstant +byte-size+ (byte 13 6))
(defconstant +byte-position+ (byte 13 19))

(deftype byte ()
  `(satisfies bytep))

(defun small-byte-p (object)
  (int::%value-has-immediate-tag-p object int::+immediate-tag-byte-specifier+))

(defun bytep (object)
  (or (small-byte-p object)
      (large-byte-p object)))

(defun fits-in-field-p (bytespec integer)
  "Test if INTEGER fits in the byte defined by BYTESPEC."
  (eql integer (logand integer
                       (1- (ash 1 (byte-size bytespec))))))

(defun byte (size position)
  (if (and (fits-in-field-p +byte-size+ size)
           (fits-in-field-p +byte-position+ position))
      (int::%%assemble-value
       (logior (ash size (byte-position +byte-size+))
               (ash position (byte-position +byte-position+))
               (dpb int::+immediate-tag-byte-specifier+
                    int::+immediate-tag+
                    0))
       int::+tag-immediate+)
      (make-large-byte size position)))

(defun byte-size (byte-specifier)
  (if (small-byte-p byte-specifier)
      (ldb +byte-size+ (int::lisp-object-address byte-specifier))
      (large-byte-size byte-specifier)))

(defun byte-position (byte-specifier)
  (if (small-byte-p byte-specifier)
      (ldb +byte-position+ (int::lisp-object-address byte-specifier))
      (large-byte-position byte-specifier)))

(declaim (inline ldb))
(defun ldb (bytespec integer)
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec)))))

(define-setf-expander ldb (bytespec int &environment env)
  (multiple-value-bind (temps vals stores
                              store-form access-form)
      (get-setf-expansion int env);Get setf expansion for int.
    (let ((btemp (gensym))     ;Temp var for byte specifier.
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores))) ;Temp var for int to store.
      (when (cdr stores) (error "Can't expand this."))
      ;; Return the setf expansion for LDB as five values.
      (values (cons btemp temps)       ;Temporary variables.
              (cons bytespec vals)     ;Value forms.
              (list store)             ;Store variables.
              `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                 ,store-form
                 ,store)               ;Storing form.
              `(ldb ,btemp ,access-form))))) ;Accessing form.

(declaim (inline dpb))
(defun dpb (newbyte bytespec integer)
  (let ((mask (1- (ash 1 (byte-size bytespec))))
        (position (byte-position bytespec)))
    (logior (ash (logand newbyte mask) position)
            (logand integer (lognot (ash mask position))))))

(declaim (inline ldb-test))
(defun ldb-test (bytespec integer)
  (not (eql 0 (ldb bytespec integer))))

(declaim (inline logbitp))
(defun logbitp (index integer)
  (ldb-test (byte 1 index) integer))

(declaim (inline mask-field))
(defun mask-field (bytespec integer)
  (logand integer (dpb -1 bytespec 0)))

(define-setf-expander mask-field (bytespec integer &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion integer env);Get setf expansion for int.
    (let ((btemp (gensym))     ;Temp var for byte specifier.
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores))) ;Temp var for int to store.
      (when (cdr stores) (error "Can't expand this."))
      (values (cons btemp temps)       ;Temporary variables.
              (cons bytespec vals)     ;Value forms.
              (list store)             ;Store variables.
              `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
                 ,store-form
                 ,store)               ;Storing form.
              `(mask-field ,btemp ,access-form))))) ;Accessing form.

(declaim (inline deposit-field))
(defun deposit-field (newbyte bytespec integer)
  (let ((mask (dpb -1 bytespec 0)))
    (logior (logand integer (lognot mask))
            (logand newbyte mask))))

;; TODO: The binary- and generic- variants should be exported
;; from this package too.
(macrolet ((def (name bignum-name)
             `(defun ,name (x y)
                (cond ((and (int::fixnump x)
                            (int::fixnump y))
                       (error "FIXNUM/FIXNUM case hit ~S." ',name))
                      ((and (int::fixnump x)
                            (int::bignump y))
                       (,bignum-name (int::%make-bignum-from-fixnum x) y))
                      ((and (int::bignump x)
                            (int::fixnump y))
                       (,bignum-name x (int::%make-bignum-from-fixnum y)))
                      ((and (int::bignump x)
                            (int::bignump y))
                       (,bignum-name x y))
                      (t (check-type x integer)
                         (check-type y integer)
                         (error "Argument combination not supported."))))))
  (def int::generic-logand int::%%bignum-logand)
  (def int::generic-logior int::%%bignum-logior)
  (def int::generic-logxor int::%%bignum-logxor))

(defun int::generic-lognot (integer)
  (logxor integer -1))

(int::define-commutative-arithmetic-operator logand int::binary-logand -1)
(int::define-commutative-arithmetic-operator logior int::binary-logior 0)
(int::define-commutative-arithmetic-operator logxor int::binary-logxor 0)

;;; LOGEQV is funny, it doesn't have a compiler builtin.
(define-compiler-macro logeqv (&rest numbers)
  `(lognot (logxor ,@numbers)))

(defun logeqv (&rest numbers)
  (declare (dynamic-extent numbers))
  (lognot (apply #'logxor numbers)))

(declaim (inline logtest))
(defun logtest (integer-1 integer-2)
  (not (zerop (logand integer-1 integer-2))))

(defun logcount (integer)
  (check-type integer integer)
  (when (minusp integer)
    (setf integer (lognot integer)))
  (do ((n 0))
      ((eql integer 0)
       n)
    (when (logtest integer 1)
      (incf n))
    (setf integer (ash integer -1))))

(declaim (inline logandc1))
(defun logandc1 (integer-1 integer-2)
  "AND complement of INTEGER-1 with INTEGER-2."
  (logand (lognot integer-1) integer-2))

(declaim (inline logandc2))
(defun logandc2 (integer-1 integer-2)
  "AND INTEGER-1 with complement of INTEGER-2."
  (logand integer-1 (lognot integer-2)))

(declaim (inline lognand))
(defun lognand (integer-1 integer-2)
  "Complement of INTEGER-1 AND INTEGER-2."
  (lognot (logand integer-1 integer-2)))

(declaim (inline lognor))
(defun lognor (integer-1 integer-2)
  "Complement of INTEGER-1 OR INTEGER-2."
  (lognot (logior integer-1 integer-2)))

(declaim (inline logorc1))
(defun logorc1 (integer-1 integer-2)
  "OR complement of INTEGER-1 with INTEGER-2."
  (logior (lognot integer-1) integer-2))

(declaim (inline logorc2))
(defun logorc2 (integer-1 integer-2)
  "OR INTEGER-1 with complement of INTEGER-2."
  (logior integer-1 (lognot integer-2)))

(defconstant boole-1 'boole-1 "integer-1")
(defconstant boole-2 'boole-2 "integer-2")
(defconstant boole-andc1 'boole-andc1 "and complement of integer-1 with integer-2")
(defconstant boole-andc2 'boole-andc2 "and integer-1 with complement of integer-2")
(defconstant boole-and 'boole-and "and")
(defconstant boole-c1 'boole-c1 "complement of integer-1")
(defconstant boole-c2 'boole-c2 "complement of integer-2")
(defconstant boole-clr 'boole-clr "always 0 (all zero bits)")
(defconstant boole-eqv 'boole-eqv "equivalence (exclusive nor)")
(defconstant boole-ior 'boole-ior "inclusive or")
(defconstant boole-nand 'boole-nand "not-and")
(defconstant boole-nor 'boole-nor "not-or")
(defconstant boole-orc1 'boole-orc1 "or complement of integer-1 with integer-2")
(defconstant boole-orc2 'boole-orc2 "or integer-1 with complement of integer-2")
(defconstant boole-set 'boole-set "always -1 (all one bits)")
(defconstant boole-xor 'boole-xor "exclusive or")

(defun boole (op integer-1 integer-2)
  "Perform bit-wise logical OP on INTEGER-1 and INTEGER-2."
  (check-type integer-1 integer)
  (check-type integer-2 integer)
  (ecase op
    (boole-1 integer-1)
    (boole-2 integer-2)
    (boole-andc1 (logandc1 integer-1 integer-2))
    (boole-andc2 (logandc2 integer-1 integer-2))
    (boole-and (logand integer-1 integer-2))
    (boole-c1 (lognot integer-1))
    (boole-c2 (lognot integer-2))
    (boole-clr 0)
    (boole-eqv (logeqv integer-1 integer-2))
    (boole-ior (logior integer-1 integer-2))
    (boole-nand (lognand integer-1 integer-2))
    (boole-nor (lognor integer-1 integer-2))
    (boole-orc1 (logorc1 integer-1 integer-2))
    (boole-orc2 (logorc2 integer-1 integer-2))
    (boole-set -1)
    (boole-xor (logxor integer-1 integer-2))))
