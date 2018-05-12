;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; A bunch of functions with no proper home.

(in-package :sys.int)

(defconstant call-arguments-limit 500)
(defconstant lambda-parameters-limit 500)
(defconstant multiple-values-limit (+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5))

(unless (boundp 'lambda-list-keywords)
  (defconstant lambda-list-keywords '(&allow-other-keys &aux &body &environment &key &optional &rest &whole &fref &closure &count)))

(declaim (inline null not))

(defun null (object)
  (if object
      'nil
      't))

(defun not (object)
  (if object
      'nil
      't))

(defun equal (x y)
  (cond
    ((eql x y))
    ((stringp x)
     (and (stringp y)
          (string= x y)))
    ((bit-vector-p x)
     (and (bit-vector-p y)
          (eql (length x) (length y))
          (dotimes (i (length x) t)
            (when (not (eql (bit x i) (bit y i)))
              (return nil)))))
    ((consp x)
     (loop
        (when (not (consp y))
          (return nil))
        (when (not (equal (car x) (car y)))
          (return nil))
        (setf x (cdr x)
              y (cdr y))
        (when (not (consp x))
          (return (equal x y)))))
    ((and (pathnamep x) (pathnamep y))
     (pathnames-equal x y))))

(define-compiler-macro equal (&whole whole x y)
  (declare (notinline typep)) ; ### Bootstrap hack.
  (when (or (not (or (symbolp x) (listp x)))
            (and (listp x)
                 (= (list-length x) 2)
                 (eql (first x) 'quote)))
    (rotatef x y))
  (cond
    ((or (not (or (symbolp y) (listp y)))
         (and (listp y)
              (= (list-length y) 2)
              (eql (first y) 'quote)))
     (let ((constant (if (not (or (symbolp y) (listp y)))
                         ;; Self-evaluating form.
                         y
                         ;; Quoted form.
                         (second y))))
       (typecase constant
         (symbol `(eq ,x ',constant))
         ((or number character)
          `(eql ,x ',constant))
         (cons
          (when (null (dotted-list-length constant))
            ;; Give up when faced with a circular list.
            (return-from equal whole))
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (consp ,sym)
                    (equal (car ,sym) ',(car constant))
                    (equal (cdr ,sym) ',(cdr constant))))))
         (string
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (stringp ,sym)
                    (string= ,sym ',constant)))))
         (bit-vector
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (bit-vector-p ,sym)
                    (eql (length ,sym) ,(length constant))
                    (every 'eql ,sym ',constant)))))
         (pathname
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (pathnamep ,sym)
                    (pathnames-equal ,sym ',constant)))))
         (t `(eq ,x ',constant)))))
    (t whole)))

(defun equalp (x y)
  (typecase x
    (character (and (characterp y)
                    (char-equal x y)))
    (number (and (numberp y)
                 (= x y)))
    (cons (and (consp y)
               (equalp (car x) (car y))
               (equalp (cdr x) (cdr y))))
    (vector (and (vectorp y)
                 (eql (length x) (length y))
                 (dotimes (i (length x) t)
                   (when (not (equalp (aref x i) (aref y i)))
                     (return nil)))))
    (array (and (arrayp y)
                (equalp (array-dimensions x) (array-dimensions y))
                (dotimes (i (array-total-size x) t)
                  (when (not (equalp (row-major-aref x i) (row-major-aref y i)))
                    (return nil)))))
    (hash-table
     (and (hash-table-p y)
          (eql (hash-table-count x)
               (hash-table-count y))
          (eql (hash-table-test x)
               (hash-table-test y))
          (block nil
            (maphash (lambda (k v)
                       (multiple-value-bind (other-v presentp)
                           (gethash k y)
                         (when (or (not presentp)
                                   (not (equalp v other-v)))
                           (return nil))))
                     x)
            t)))
    (structure-object
     (and (typep y 'structure-object)
          (eq (%struct-slot x 0) (%struct-slot y 0))
          (dotimes (slot (length (structure-slots (%struct-slot x 0)))
                    t)
            (when (not (equalp (%struct-slot x (1+ slot))
                               (%struct-slot y (1+ slot))))
              (return nil)))))
    (t (equal x y))))

(defun macroexpand-1 (form &optional env)
  (declare (notinline typep)) ; ### Bootstrap hack.
  (cond ((symbolp form)
         (let ((var (sys.c::lookup-variable-in-environment form env)))
           (cond ((typep var 'sys.c::symbol-macro)
                  (values (sys.c::symbol-macro-expansion var) t))
                 (t
                  (values form nil)))))
        ((consp form)
         (let ((fn (macro-function (first form) env)))
           (if fn
               (values (funcall *macroexpand-hook* fn form env) t)
               (values form nil))))
        (t (values form nil))))

(defun macroexpand (form &optional env)
  (let ((did-expand nil))
    (do () (nil)
       (multiple-value-bind (expansion expanded-p)
           (macroexpand-1 form env)
         (if expanded-p
             (setf form expansion
                   did-expand t)
             (return (values form did-expand)))))))

(declaim (inline identity))
(defun identity (thing)
  thing)

(declaim (inline complement))
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun set (symbol value)
  (setf (symbol-value symbol) value))

(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

(defun bsearch (item vector &key (start 0) end (stride 1) (key 'identity))
  "Locate ITEM using a binary search through VECTOR."
  ;; IMIN/IMAX are inclusive indicies.
  (do ((imin start)
       (imax (1- (truncate (or end (length vector)) stride))))
      ((< imax imin)
       nil)
    (let* ((imid (truncate (+ imin imax) 2))
           (elt (funcall key (aref vector (* imid stride)))))
      (cond ((< elt item) (setf imin (1+ imid)))
            ((> elt item) (setf imax (1- imid)))
            (t (return (* imid stride)))))))

;;; cl-nibbles-style accessors.

(defun ub16ref/be (vector index)
  (logior (ash (aref vector index) 8)
          (aref vector (1+ index))))
(defun (setf ub16ref/be) (value vector index)
  (setf (aref vector index) (ash value -8)
        (aref vector (1+ index)) (logand value #xFF))
  value)

(defun ub16ref/le (vector index)
  (logior (aref vector index)
          (ash (aref vector (1+ index)) 8)))
(defun (setf ub16ref/le) (value vector index)
  (setf (aref vector index) (logand value #xFF)
        (aref vector (1+ index)) (ash value -8))
  value)

(defun ub32ref/be (vector index)
  (logior (ash (aref vector index) 24)
          (ash (aref vector (+ index 1)) 16)
          (ash (aref vector (+ index 2)) 8)
          (aref vector (+ index 3))))
(defun (setf ub32ref/be) (value vector index)
  (setf (aref vector index) (ash value -24)
        (aref vector (+ index 1)) (logand (ash value -16) #xFF)
        (aref vector (+ index 2)) (logand (ash value -8) #xFF)
        (aref vector (+ index 3)) (logand value #xFF))
  value)

(defun ub32ref/le (vector index)
  (logior (aref vector index)
          (ash (aref vector (+ index 1)) 8)
          (ash (aref vector (+ index 2)) 16)
          (ash (aref vector (+ index 3)) 24)))
(defun (setf ub32ref/le) (value vector index)
  (setf (aref vector index) (logand value #xFF)
        (aref vector (+ index 1)) (logand (ash value -8) #xFF)
        (aref vector (+ index 2)) (logand (ash value -16) #xFF)
        (aref vector (+ index 3)) (ash value -24))
  value)

(defun ub64ref/be (vector index)
  (logior (ash (aref vector index) 56)
          (ash (aref vector (+ index 1)) 48)
          (ash (aref vector (+ index 2)) 40)
          (ash (aref vector (+ index 3)) 32)
          (ash (aref vector (+ index 4)) 24)
          (ash (aref vector (+ index 5)) 16)
          (ash (aref vector (+ index 6)) 8)
          (aref vector (+ index 7))))
(defun (setf ub64ref/be) (value vector index)
  (setf (aref vector index) (ldb (byte 8 56) value)
        (aref vector (+ index 1)) (ldb (byte 8 48) value)
        (aref vector (+ index 2)) (ldb (byte 8 40) value)
        (aref vector (+ index 3)) (ldb (byte 8 32) value)
        (aref vector (+ index 4)) (ldb (byte 8 24) value)
        (aref vector (+ index 5)) (ldb (byte 8 16) value)
        (aref vector (+ index 6)) (ldb (byte 8 8) value)
        (aref vector (+ index 7)) (ldb (byte 8 0) value))
  value)

(defun ub64ref/le (vector index)
  (logior (aref vector index)
          (ash (aref vector (+ index 1)) 8)
          (ash (aref vector (+ index 2)) 16)
          (ash (aref vector (+ index 3)) 24)
          (ash (aref vector (+ index 4)) 32)
          (ash (aref vector (+ index 5)) 40)
          (ash (aref vector (+ index 6)) 48)
          (ash (aref vector (+ index 7)) 56)))
(defun (setf ub64ref/le) (value vector index)
  (setf (aref vector index) (ldb (byte 8 0) value)
        (aref vector (+ index 1)) (ldb (byte 8 8) value)
        (aref vector (+ index 2)) (ldb (byte 8 16) value)
        (aref vector (+ index 3)) (ldb (byte 8 24) value)
        (aref vector (+ index 4)) (ldb (byte 8 32) value)
        (aref vector (+ index 5)) (ldb (byte 8 40) value)
        (aref vector (+ index 6)) (ldb (byte 8 48) value)
        (aref vector (+ index 7)) (ldb (byte 8 56) value))
  value)

;;;
;;; PRNG WELL512 based on the public domain algorithm by Chris Lomont
;;; as published in
;;; http://lomont.org/Math/Papers/2008/Lomont_PRNG_2008.pdf
;;;

(defstruct (random-state
             (:constructor %make-random-state (index bits)))
  index
  bits)

(defvar *random-state* (make-random-state t))

(defun make-random-state (&optional state)
  (case state
    ((t)
     (let ((state (%make-random-state 0 (make-array
                                         16
                                         :element-type '(unsigned-byte 32)
                                         :initial-element (get-universal-time)))))
       (dotimes (i 64)
         (%random state))
       state))
    ((nil)
     (%make-random-state (random-state-index *random-state*)
                         (copy-seq (random-state-bits *random-state*))))
    (otherwise
     (check-type state random-state)
     (%make-random-state (random-state-index state)
                         (copy-seq (random-state-bits state))))))

;;; Generate 32-bit random number
(defun %random (random-state)
  (let* ((idx (random-state-index random-state))
         (bits (random-state-bits random-state))
         (a (aref bits idx))
         (c (aref bits (ldb (byte 4 0) (+ idx 13))))
         (b (logxor a c (dpb a (byte 16 16) 0) (dpb c (byte 17 15) 0)))
         (d))
    (setf c (logxor c (ash c -11)))
    (setf a (setf (aref bits idx) (logxor b c)))
    (setf d  (logxor a (logand (dpb a (byte 27 5) 0) #xDA442D24)))
    (setf idx (ldb (byte 4 0) (+ idx 15)))
    (setf (random-state-index random-state) idx)
    (setf a (aref bits idx))
    (setf (aref bits idx) (logxor a b d
                                  (dpb a (byte 30 2) 0)
                                  (dpb b (byte 14 18) 0)
                                  (dpb c (byte 4 28) 0)))))

(defun random (limit &optional (random-state *random-state*))
  (unless (or (and (integerp limit) (> limit 0))
              (and (realp limit) (> limit 0.0)))
    (error 'type-error :expected-type '(or positive-fixnum positive-real)
                :datum limit))
  (let* ((r (%random random-state))
         (rd (/ (float r 1.0d0) (+ 1.0d0 (float #xFFFFFFFF 1.0d0)))))
    (etypecase limit
      ;; Using high bits of %random even though it takes more work
      ;; using floating point intermediaries
      (integer (values (truncate (* limit rd))))
      ;; 32-bits is too big for single precision so try again if rd
      ;; rounds up to 1.0f0
      (single-float
       (let ((rs (float rd 1.0f0)))
         (if (= rs 1.0f0)
             (random limit random-state)
             (* limit rs))))
      (double-float (* limit rd)))))
