;;;; Complex numbers

(in-package :mezzano.internals.numbers.complex)

(declaim (inline complexp))
(defun complexp (object)
  (int::%object-of-type-range-p
   object
   int::+first-complex-object-tag+
   int::+last-complex-object-tag+))

(defun complex (realpart &optional imagpart)
  (check-type realpart real)
  (check-type imagpart (or real null))
  (unless imagpart
    (setf imagpart (coerce 0 (type-of realpart))))
  (cond ((or (typep realpart 'double-float)
             (typep imagpart 'double-float))
         (let ((result (mezzano.runtime::%allocate-object int::+object-tag-complex-double-float+ 0 2 nil)))
           (setf (int::%object-ref-double-float result int::+complex-realpart+) (float realpart 0.0d0)
                 (int::%object-ref-double-float result int::+complex-imagpart+) (float imagpart 0.0d0))
           result))
        ((or (typep realpart 'single-float)
             (typep imagpart 'single-float))
         (let ((result (mezzano.runtime::%allocate-object int::+object-tag-complex-single-float+ 0 1 nil)))
           (setf (int::%object-ref-single-float result int::+complex-realpart+) (float realpart 0.0f0)
                 (int::%object-ref-single-float result int::+complex-imagpart+) (float imagpart 0.0f0))
           result))
        ((or (typep realpart 'short-float)
             (typep imagpart 'short-float))
         (let ((result (mezzano.runtime::%allocate-object int::+object-tag-complex-short-float+ 0 1 nil)))
           (setf (int::%object-ref-short-float result int::+complex-realpart+) (float realpart 0.0s0)
                 (int::%object-ref-short-float result int::+complex-imagpart+) (float imagpart 0.0s0))
           result))
        ((not (zerop imagpart))
         (let ((result (mezzano.runtime::%allocate-object int::+object-tag-complex-rational+ 0 2 nil)))
           (setf (int::%object-ref-t result int::+complex-realpart+) realpart
                 (int::%object-ref-t result int::+complex-imagpart+) imagpart)
           result))
        (t
         realpart)))

(defun realpart (number)
  (cond
    ((int::%object-of-type-p number int::+object-tag-complex-rational+)
     (int::%object-ref-t number int::+complex-realpart+))
    ((int::%object-of-type-p number int::+object-tag-complex-short-float+)
     (int::%object-ref-short-float number int::+complex-realpart+))
    ((int::%object-of-type-p number int::+object-tag-complex-single-float+)
     (int::%object-ref-single-float number int::+complex-realpart+))
    ((int::%object-of-type-p number int::+object-tag-complex-double-float+)
     (int::%object-ref-double-float number int::+complex-realpart+))
    (t
     (check-type number number)
     number)))

(defun imagpart (number)
  (cond
    ((int::%object-of-type-p number int::+object-tag-complex-rational+)
     (int::%object-ref-t number int::+complex-imagpart+))
    ((int::%object-of-type-p number int::+object-tag-complex-short-float+)
     (int::%object-ref-short-float number int::+complex-imagpart+))
    ((int::%object-of-type-p number int::+object-tag-complex-single-float+)
     (int::%object-ref-single-float number int::+complex-imagpart+))
    ((int::%object-of-type-p number int::+object-tag-complex-double-float+)
     (int::%object-ref-double-float number int::+complex-imagpart+))
    (t
     (check-type number number)
     (* 0 number))))

(declaim (inline complex-=))
(defun complex-= (x y)
  (and (= (realpart x) (realpart y))
       (= (imagpart x) (imagpart y))))

(defun complex-+ (x y)
  (complex (+ (realpart x) (realpart y))
           (+ (imagpart x) (imagpart y))))

(defun complex-- (x y)
  (complex (- (realpart x) (realpart y))
           (- (imagpart x) (imagpart y))))

(defun complex-* (x y)
  (complex (- (* (realpart x) (realpart y))
              (* (imagpart x) (imagpart y)))
           (+ (* (imagpart x) (realpart y))
              (* (realpart x) (imagpart y)))))

(defun complex-/ (x y)
  (complex (/ (+ (* (realpart x) (realpart y))
                 (* (imagpart x) (imagpart y)))
              (+ (expt (realpart y) 2)
                 (expt (imagpart y) 2)))
           (/ (- (* (imagpart x) (realpart y))
                 (* (realpart x) (imagpart y)))
              (+ (expt (realpart y) 2)
                 (expt (imagpart y) 2)))))

(defun complex-abs (number)
  (sqrt (+ (expt (realpart number) 2)
           (expt (imagpart number) 2))))

(defun complex-sqrt (number)
  (exp (/ (log number) 2)))

(defun complex-sin (x)
  (let ((real (realpart x))
        (imag (imagpart x)))
    (complex (* (sin real) (cosh imag))
             (* (cos real) (sinh imag)))))

(defun complex-cos (x)
  (let ((real (realpart x))
        (imag (imagpart x)))
    (complex (* (cos real) (cosh imag))
             (- (* (sin real) (sinh imag))))))

(defun complex-log-e (number)
  (complex (log (abs number)) (phase number)))

(defun conjugate (number)
  (if (complexp number)
      (complex (realpart number)
               (- (imagpart number)))
      number))
