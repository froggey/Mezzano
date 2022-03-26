(cl:in-package #:clim-tests)

(def-suite* :mcclim.setf-star
  :in :mcclim)

(defclass my-point ()
  ((x :accessor my-point-x :initarg :x)
   (y :accessor my-point-y :initarg :y)))

(defgeneric my-point-position (point))

(defmethod my-point-position ((point my-point))
  (values (my-point-x point) (my-point-y point)))

(climi::defgeneric* (setf my-point-position)
    (nx ny point &optional return-polar &key coordinates))

(climi::defmethod* (setf my-point-position)
    (nx ny (point my-point) &optional return-polar &key (coordinates :cartesian))
  "Set X an Y positions of POINT. If COORDINATES is :CARTESIAN, NX and
NY are the new cartesian coordinates. If COORDINATES is :POLAR, NX is
the radial coordinate and NY is the angular coordinate. By default
cartesian coordinates are returned. When the optional argument
RETURN-POLAR is true then return the polar coordinates."
  (with-slots (x y) point
    (case coordinates
      (:polar (multiple-value-bind (nx ny)
                  (polar-to-caresian nx ny)
                (setf x nx y ny)))
      (:cartesian (setf x nx y ny))
      (otherwise
       (error "Invalid coordinates. Must be one of :cartesian or :polar.")))
    (if return-polar
        (cartesian-to-polar x y)
        (values x y))))

(defun polar-to-caresian (r a)
  (let ((x (* r (cos a)))
        (y (* r (sin a))))
    (values x y)))

(defun cartesian-to-polar (x y)
  (let ((r (sqrt (+ (expt x 2) (expt y 2))))
        (a (atan (/ y x))))
    (values r a)))

(test required-args
  (let ((point (make-instance 'my-point :x 1 :y 2)))
    (setf (my-point-position point) (values 3 4))
    (is (equal (my-point-position point) (values 3 4))
        "The new coordinates of the point are correct.")))

(test optional-args
  (let* ((point (make-instance 'my-point :x 1 :y 2))
         (nx 3)
         (ny 4)
         (polar-point-coordinates (cartesian-to-polar nx ny)))
    (is (equal (setf (my-point-position point t) (values nx ny))
               polar-point-coordinates)
        "Return polar coordinates are correct.")
    (is (equal (my-point-position point) (values nx ny))
        "The new coordinates of the point are correct.")))

(test key-args
  (let ((point (make-instance 'my-point :x 1 :y 2)))
    (setf (my-point-position point nil :coordinates :cartesian) (values 1 pi))
    (is (equal (my-point-position point) (values 1 pi))
        "New position values use cartesian coordinates when the keyword argument is passed")

    (setf (my-point-position point nil :coordinates :polar) (values 0 (/ pi 4)))
    (is (<= 0 (abs (- (my-point-x point) (my-point-y point))) 0.000001)
        "New position values use polar coordinates when the keyword argument is passed")

    ;; Error is signalled because :angular is not a valid coordinates
    ;; type
    (signals error
      (setf (my-point-position point nil :coordinates :angular) (values 1 pi)))))


(climi::defgeneric* (setf optional-arguments) (arg1 arg2
                                               &optional
                                               kaboom1
                                               kaboom2
                                               &key
                                               ((:duf uno))
                                               ((:qux duo))))

(climi::defmethod* (setf optional-arguments) (arg1 arg2
                                                   &optional
                                                   (kaboom1 nil kaboom1-p)
                                                   (kaboom2 6 kaboom2-p)
                                                   &key
                                                   ((:duf one) nil duf-p)
                                                   ((:qux two) 7 qux-p))
  (list arg1 arg2
        kaboom1 kaboom1-p kaboom2 kaboom2-p
        one duf-p two qux-p))

(test optional-arguments-test
  (macrolet ((test-case (arguments new-value expected)
               `(let ((result (setf (optional-arguments ,@arguments) ,new-value)))
                  (is (equal (list ,@expected) result)))))
    (test-case (:arg2)                   5 (5 :arg2 nil nil 6 nil nil nil 7 nil))
    (test-case (:arg2 1 2 :qux 4)        5 (5 :arg2 1   t   2 t   nil nil 4 t))
    (test-case (:arg2 1 2 :duf 3 :qux 4) 5 (5 :arg2 1   t   2 t   3   t   4 t))))
