(in-package :mezzano.internals.numbers.transcendental)

(defconstant pi 3.14159265358979323846264338327950288419716939937511d0)

(defun expt (base power)
  (etypecase power
    (integer
     (cond ((minusp power)
            (/ (expt base (- power))))
           (t (let ((accum 1))
                (dotimes (i power accum)
                  (setf accum (* accum base)))))))
    (float
     (cond ((eql (float (truncate power) power) power)
            ;; Moderately integer-like?
            (expt base (truncate power)))
           ((zerop base)
            (assert (not (minusp power)))
            (float 0.0 power))
           (t
            ;; Slower...
            (exp (* power (log base))))))
    (ratio (exp (* power (log base))))))

(defun phase (number)
  (atan (imagpart number) (realpart number)))

(defun signum (number)
  (if (zerop number)
      number
      (/ number (abs number))))

(defun abs (number)
  (check-type number number)
  (etypecase number
    (complex
     (mezzano.internals.numbers.complex:complex-abs number))
    (single-float
     (int::%integer-as-single-float
      (logand #x7FFFFFFF
              (int::%single-float-as-integer number))))
    (double-float
     (int::%integer-as-double-float
      (logand #x7FFFFFFFFFFFFFFF
              (int::%double-float-as-integer number))))
    (real
     (if (minusp number)
         (- number)
         number))))

(defun sqrt (number)
  (check-type number number)
  (etypecase number
    (double-float
     (int::%%double-float-sqrt number))
    (short-float
     (int::%%short-float-sqrt number))
    (real
     (int::%%single-float-sqrt (float number 0.0f0)))
    (complex
     (mezzano.internals.numbers.complex:complex-sqrt number))))

(defun isqrt (number)
  (values (floor (sqrt number))))

;;; Derived from SLEEF: https://github.com/shibatch/sleef

(defconstant +sleef-pi4-af+ 0.78515625f0)
(defconstant +sleef-pi4-bf+ 0.00024187564849853515625f0)
(defconstant +sleef-pi4-cf+ 3.7747668102383613586f-08)
(defconstant +sleef-pi4-df+ 1.2816720341285448015f-12)

(defun sleef-mulsignf (x y)
  (int::%integer-as-single-float
   (logxor
    (int::%single-float-as-integer x)
    (logand (int::%single-float-as-integer y)
            (ash 1 31)))))

(defun sleef-signf (d)
  (sleef-mulsignf 1.0f0 d))

(declaim (inline sleef-mlaf))
(defun sleef-mlaf (x y z)
  (declare (type single-float x y z))
  (+ (* x y) z))

(declaim (inline sleef-rintf))
(defun sleef-rintf (x)
  (declare (type single-float x))
  (if (< x 0.0f0)
      (the fixnum (truncate (- x 0.5f0)))
      (the fixnum (truncate (+ x 0.5f0)))))

(defconstant +sleef-pi4-a+ 0.78539816290140151978d0)
(defconstant +sleef-pi4-b+ 4.9604678871439933374d-10)
(defconstant +sleef-pi4-c+ 1.1258708853173288931d-18)
(defconstant +sleef-pi4-d+ 1.7607799325916000908d-27)

(declaim (inline sleef-mla))
(defun sleef-mla (x y z)
  (declare (type double-float x y z))
  (+ (* x y) z))

(declaim (inline sleef-rint))
(defun sleef-rint (x)
  (declare (type double-float x))
  (if (< x 0.0d0)
      (the fixnum (truncate (- x 0.5d0)))
      (the fixnum (truncate (+ x 0.5d0)))))

(declaim (inline finish-sincos-single-float))
(defun finish-sincos-single-float (s d)
  (declare (type single-float s d))
  (let ((u 2.6083159809786593541503f-06))
    (declare (type single-float u))
    (setf u (sleef-mlaf u s -0.0001981069071916863322258f0))
    (setf u (sleef-mlaf u s 0.00833307858556509017944336f0))
    (setf u (sleef-mlaf u s -0.166666597127914428710938f0))

    (setf u (sleef-mlaf s (* u d) d))

    (cond ((mezzano.extensions:float-infinity-p d)
           (/ 0.0f0 0.0f0))
          (t
           u))))

(defun sin-single-float (d)
  (declare (type single-float d)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((q (sleef-rintf (* d (/ (float pi 0.0f0)))))
         (q-float (float q 0.0f0)))
    (declare (type fixnum q)
             (type single-float q-float))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-af+ -4) d))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-bf+ -4) d))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-cf+ -4) d))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-df+ -4) d))
    (let ((s (* d d)))
      (declare (type single-float s))
      (when (logtest q 1)
        (setf d (- 0.0f0 d)))
      (finish-sincos-single-float s d))))

(defun cos-single-float (d)
  (declare (type single-float d)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((q (+ 1 (* 2 (sleef-rintf (- (* d (/ (float pi 0.0f0))) 0.5f0)))))
         (q-float (float q 0.0f0)))
    (declare (type fixnum q)
             (type single-float q-float))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-af+ -2) d))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-bf+ -2) d))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-cf+ -2) d))
    (setf d (sleef-mlaf q-float (* +sleef-pi4-df+ -2) d))
    (let ((s (* d d)))
      (declare (type single-float s))
      (when (not (logtest q 2))
        (setf d (- 0.0f0 d)))
      (finish-sincos-single-float s d))))

(declaim (inline finish-sincos-double-float))
(defun finish-sincos-double-float (s d)
  (declare (type double-float s d))
  (let ((u -7.97255955009037868891952d-18))
    (declare (type double-float u))
    (setf u (sleef-mla u s 2.81009972710863200091251d-15))
    (setf u (sleef-mla u s -7.64712219118158833288484d-13))
    (setf u (sleef-mla u s 1.60590430605664501629054d-10))
    (setf u (sleef-mla u s -2.50521083763502045810755d-08))
    (setf u (sleef-mla u s 2.75573192239198747630416d-06))
    (setf u (sleef-mla u s -0.000198412698412696162806809d0))
    (setf u (sleef-mla u s 0.00833333333333332974823815d0))
    (setf u (sleef-mla u s -0.166666666666666657414808d0))

    (sleef-mla s (* u d) d)))

(defun sin-double-float (d)
  (declare (type double-float d)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((q (sleef-rint (* d (/ (float pi 0.0d0)))))
         (q-float (float q 0.0d0)))
    (declare (type fixnum q)
             (type double-float q-float))
    (setf d (sleef-mla q-float (* +sleef-pi4-a+ -4) d))
    (setf d (sleef-mla q-float (* +sleef-pi4-b+ -4) d))
    (setf d (sleef-mla q-float (* +sleef-pi4-c+ -4) d))
    (setf d (sleef-mla q-float (* +sleef-pi4-d+ -4) d))
    (let ((s (* d d)))
      (declare (type double-float s))
      (when (logtest q 1)
        (setf d (- 0.0d0 d)))
      (finish-sincos-double-float s d))))

(defun cos-double-float (d)
  (declare (type double-float d)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((q (+ 1 (* 2 (sleef-rint (- (* d (/ (float pi 0.0d0))) 0.5d0)))))
         (q-float (float q 0.0d0)))
    (declare (type fixnum q)
             (type double-float q-float))
    (setf d (sleef-mla q-float (* +sleef-pi4-a+ -2) d))
    (setf d (sleef-mla q-float (* +sleef-pi4-b+ -2) d))
    (setf d (sleef-mla q-float (* +sleef-pi4-c+ -2) d))
    (setf d (sleef-mla q-float (* +sleef-pi4-d+ -2) d))
    (let ((s (* d d)))
      (declare (type double-float s))
      (when (not (logtest q 2))
        (setf d (- 0.0d0 d)))
      (finish-sincos-double-float s d))))

(defun sin (x)
  (etypecase x
    (complex
     (mezzano.internals.numbers.complex:complex-sin x))
    (double-float
     (sin-double-float x))
    (short-float
     (float (sin-single-float (float x 0.0f0)) 0.0s0))
    (real
     (sin-single-float (float x 0.0f0)))))

(defun cos (x)
  (etypecase x
    (complex
     (mezzano.internals.numbers.complex:complex-cos x))
    (double-float
     (cos-double-float x))
    (short-float
     (float (cos-single-float (float x 0.0f0)) 0.0s0))
    (real
     (cos-single-float (float x 0.0f0)))))

(defun tan-single-float (d)
  (declare (type single-float d))
  (let* ((q (sleef-rintf (* d 2 (/ (float pi 0.0f0)))))
         (q-float (float q 0.0f0))
         u s
         (x d))
    (declare (type fixnum q)
             (type single-float q-float x))
    (setf x (sleef-mlaf q-float (* +sleef-pi4-af+ -4 0.5f0) x))
    (setf x (sleef-mlaf q-float (* +sleef-pi4-bf+ -4 0.5f0) x))
    (setf x (sleef-mlaf q-float (* +sleef-pi4-cf+ -4 0.5f0) x))
    (setf x (sleef-mlaf q-float (* +sleef-pi4-df+ -4 0.5f0) x))

    (setf s (* x x))

    (when (not (eql (logand q 1) 0))
      (setf x (- x)))

    (setf u 0.00927245803177356719970703f0)
    (setf u (sleef-mlaf u s 0.00331984995864331722259521f0))
    (setf u (sleef-mlaf u s 0.0242998078465461730957031f0))
    (setf u (sleef-mlaf u s 0.0534495301544666290283203f0))
    (setf u (sleef-mlaf u s 0.133383005857467651367188f0))
    (setf u (sleef-mlaf u s 0.333331853151321411132812f0))

    (setf u (sleef-mlaf s (* u x) x))

    (when (not (eql (logand q 1) 0))
      (setf u (/ u)))

    (when (mezzano.extensions:float-infinity-p d)
      (setf u (/ 0.0f0 0.0f0)))

    (if (floatp d)
        (float u d)
        u)))

(defun tan (radians)
  (etypecase radians
    (double-float
     (float (tan-single-float (float radians 0.0f0)) 0.0d0))
    (short-float
     (float (tan-single-float (float radians 0.0f0)) 0.0s0))
    (real
     (tan-single-float (float radians 0.0f0)))))

(defconstant +sleef-r-ln2f+ 1.442695040888963407359924681001892137426645954152985934135449406931f0)
(defconstant +sleef-l2uf+ 0.693145751953125f0)
(defconstant +sleef-l2lf+ 1.428606765330187045f-06)

(defun sleef-ldexpkf (x q)
  (let (u m)
    (setf m (ash q -31))
    (setf m (ash (- (ash (+ m q) -6) m) 4))
    (setf q (- q (ash m 2)))
    (incf m 127)
    (setf m (if (< m 0) 0 m))
    (setf m (if (> m 255) 255 m))
    (setf u (int::%integer-as-single-float (ash m 23)))
    (setf x (* x u u u u))
    (setf u (int::%integer-as-single-float (ash (+ q #x7f) 23)))
    (* x u)))

(defun sleef-ilogbkf (d)
  (let (m q)
    (setf m (< d 5.421010862427522f-20))
    (setf d (if m
                (* 1.8446744073709552f19 d)
                d))
    (setf q (logand (ash (int::%single-float-as-integer d) -23) #xFF))
    (if m
        (- q (+ 64 #x7F))
        (- q #x7F))))

(defun exp-single-float (d)
  (declare (type single-float d))
  (let* ((q (sleef-rintf (* d +sleef-r-ln2f+)))
         (q-float (float q 0.0f0))
         (s d)
         u)
    (declare (type fixnum q)
             (type single-float q-float s))
    (setf s (sleef-mlaf q-float (- +sleef-l2uf+) s))
    (setf s (sleef-mlaf q-float (- +sleef-l2lf+) s))

    (setf u 0.000198527617612853646278381f0)
    (setf u (sleef-mlaf u s 0.00139304355252534151077271f0))
    (setf u (sleef-mlaf u s 0.00833336077630519866943359f0))
    (setf u (sleef-mlaf u s 0.0416664853692054748535156f0))
    (setf u (sleef-mlaf u s 0.166666671633720397949219f0))
    (setf u (sleef-mlaf u s 0.5f0))

    (setf u (+ (* s s u) s 1.0f0))
    (setf u (sleef-ldexpkf u q))

    (if (< d -104) (setf u 0))
    (if (> d  104) (setf u mezzano.extensions:single-float-positive-infinity))

    u))

(defun exp (number)
  (etypecase number
    (double-float
     (float (exp-single-float (float number 0.0f0)) 0.0d0))
    (real
     (exp-single-float (float number 0.0f0)))))

(defun log-e (number)
  (let ((d (float number 0.0f0))
        x x2 tt m e)
    (setf e (sleef-ilogbkf (* d (/ 0.75f0))))
    (setf m (sleef-ldexpkf d (- e)))

    (setf x (/ (- m 1.0f0) (+ m 1.0f0)))
    (setf x2 (* x x))

    (setf tt 0.2392828464508056640625f0)
    (setf tt (sleef-mlaf tt x2 0.28518211841583251953125f0))
    (setf tt (sleef-mlaf tt x2 0.400005877017974853515625f0))
    (setf tt (sleef-mlaf tt x2 0.666666686534881591796875f0))
    (setf tt (sleef-mlaf tt x2 2.0f0))

    (setf x (+ (* x tt) (* 0.693147180559945286226764f0 e)))

    (when (mezzano.extensions:float-infinity-p d)
      (setf x mezzano.extensions:single-float-positive-infinity))
    (when (< d 0)
      (setf x (/ 0.0f0 0.0f0)))
    (when (= d 0)
      (setf x mezzano.extensions:single-float-negative-infinity))

    (if (floatp number)
        (float x number)
        x)))

(defun log (number &optional base)
  (cond (base
         (/ (log number) (log base)))
        ((complexp number)
         (mezzano.internals.numbers.complex:complex-log-e number))
        (t
         (log-e number))))

(defun atan (number1 &optional number2)
  (if number2
      (atan2 number1 number2)
      (let ((s (float number1 0.0f0))
            (q 0)
            tt u)
        (when (= (sleef-signf s) -1)
          (setf s (- s))
          (setf q 2))
        (when (> s 1)
          (setf s (/ s))
          (setf q (logior q 1)))

        (setf tt (* s s))

        (setf u 0.00282363896258175373077393f0)
        (setf u (sleef-mlaf u tt -0.0159569028764963150024414f0))
        (setf u (sleef-mlaf u tt 0.0425049886107444763183594f0))
        (setf u (sleef-mlaf u tt -0.0748900920152664184570312f0))
        (setf u (sleef-mlaf u tt 0.106347933411598205566406f0))
        (setf u (sleef-mlaf u tt -0.142027363181114196777344f0))
        (setf u (sleef-mlaf u tt 0.199926957488059997558594f0))
        (setf u (sleef-mlaf u tt -0.333331018686294555664062f0))

        (setf tt (+ s (* s tt u)))

        (when (not (eql (logand q 1) 0))
          (setf tt (- 1.570796326794896557998982f0 tt)))
        (when (not (eql (logand q 2) 0))
          (setf tt (- tt)))
        (if (floatp number1)
            (float tt number1)
            tt))))

(defun atan2 (y x)
  (let ((n (cond ((> x 0) (atan (/ y x)))
                 ((and (>= y 0) (< x 0))
                  (+ (atan (/ y x)) pi))
                 ((and (< y 0) (< x 0))
                  (- (atan (/ y x)) pi))
                 ((and (> y 0) (zerop x))
                  (/ pi 2))
                 ((and (< y 0) (zerop x))
                  (- (/ pi 2)))
                 (t 0))))
    (cond ((and (floatp x) (floatp y))
           (float (float n x) y))
          ((floatp x)
           (float n x))
          ((floatp y)
           (float n y))
          (t n))))
