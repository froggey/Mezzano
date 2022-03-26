(in-package "SPATIAL-TREES-IMPL")

;;; Because of the need to represent infinite dimensions, a bound is
;;; either cl:- (representing negative infinity), cl:+ (representing
;;; positive infinity) or a number (representing itself).  All bounds
;;; are inclusive at present; it's possible that this should change at
;;; some point, but solutions of the form "lower bound -> inclusive,
;;; upper -> exclusive" are undesireable for a number of reasons:
;;; chiefly introduction of an asymmetry into an otherwise symmetric
;;; space, and an inability to represent point data.

(defun bound= (x y)
  (case x
    ((- +) (eql y x))
    (t (case y
         ((- +) nil)
         (t (= x y))))))

(defun bound< (x y)
  (case x
    ((-) (not (eql y '-)))
    ((+) nil)
    (t (case y
         ((-) nil)
         ((+) t)
         (t (< x y))))))

(defun bound<= (x y)
  (case x
    ((-) t)
    ((+) (eql y '+))
    (t (case y
         ((-) nil)
         ((+) t)
         (t (<= x y))))))

(defun boundmax (x y)
  (if (bound< x y) y x))
(defun boundmin (x y)
  (if (bound< x y) x y))

#+(or)
(progn
  (defclass rectangle ()
    ((lows :initarg :lows :reader lows)
     (highs :initarg :highs :reader highs)))
  (defmethod initialize-instance :after ((o rectangle) &rest args)
    (declare (ignore args))
    #+slow
    (unless (every #'bound<= (lows o) (highs o))
      (error "Bad coordinates for rectangle: ~S ~S" (lows o) (highs o))))
  (defun make-rectangle (&key lows highs)
    (make-instance 'rectangle :lows lows :highs highs)))
#+(and)
(progn
  (defstruct (rectangle
               (:conc-name nil)
               (:constructor %make-rectangle (lows highs)))
    (lows nil :read-only t)
    (highs nil :read-only t))
  (defun make-rectangle (&key lows highs)
    #+slow
    (unless (every #'bound<= lows highs)
      (error "Bad coordinates for rectangle: ~S ~S" lows highs))
    (%make-rectangle lows highs)))
(defmethod print-object ((o rectangle) s)
  (print-unreadable-object (o s)
    (format s "(~{~D~^,~}) - (~{~D~^,~})" (lows o) (highs o))))

(define-condition rectangle-infinite (error)
  ((rectangle :initarg :rectangle :reader rectangle-infinite-rectangle))
  (:report
   (lambda (c s)
     (format s "The rectangle ~S is infinite in at least one dimension"
             (rectangle-infinite-rectangle c)))))

(defun %intersection/1d (l1 h1 l2 h2)
  (cond
    ((and (bound<= l1 l2) (bound<= l2 h1)) (cons l2 (boundmin h1 h2)))
    ((and (bound<= l2 l1) (bound<= l1 h2)) (cons l1 (boundmin h1 h2)))))

(declaim (inline %intersect/1d-p))
(defun %intersect/1d-p (l1 h1 l2 h2)
  (or (and (bound<= l1 l2) (bound<= l2 h1))
      (and (bound<= l2 l1) (bound<= l1 h2))))

(defgeneric intersectp (one two))
(defmethod intersectp ((r1 rectangle) (r2 rectangle))
  (loop for l1 in (lows r1)
        for h1 in (highs r1)
        for l2 in (lows r2)
        for h2 in (highs r2)
        always (%intersect/1d-p l1 h1 l2 h2)))

(defgeneric intersection (one two))
(defmethod intersection ((r1 rectangle) (r2 rectangle))
  (let ((intersections (mapcar #'%intersection/1d
                               (lows r1) (highs r1)
                               (lows r2) (highs r2))))
    (make-rectangle
     :lows (mapcar (lambda (x)
                     (when (null x)
                       (return-from intersection nil))
                     (car x))
                   intersections)
     :highs (mapcar #'cdr intersections))))

(defgeneric minimum-bound (one two))
(defmethod minimum-bound ((r1 rectangle) (r2 rectangle))
  (make-rectangle
   :lows #+slow (mapcar #'boundmin (lows r1) (lows r2))
   (loop for l1 in (lows r1) for l2 in (lows r2)
         collect (boundmin l1 l2))
   :highs #+slow (mapcar #'boundmax (highs r1) (highs r2))
   (loop for h1 in (highs r1) for h2 in (highs r2)
         collect (boundmax h1 h2))))

(defgeneric area (object))
(defmethod area ((r rectangle))
  #+slow ; unbearably slow and consy(!)
  (reduce #'* (mapcar #'- (highs r) (lows r)))
  (do* ((lows (lows r) (cdr lows))
        (low (car lows) (car lows))
        (highs (highs r) (cdr highs))
        (high (car highs) (car highs))
        (result 1))
       ((null lows) result)
    (when (or (symbolp high) (symbolp low))
      (error 'rectangle-infinite :rectangle r))
    (setf result (* result (- high low)))))
