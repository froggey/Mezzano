;;; Copyright (c) 2008 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :mcclim-render-internals)
;;; Adapted from Ben Deane's com.elbeno.curve 0.1 library, with
;;; permission. See http://www.elbeno.com/lisp/ for the original.

(defparameter +cubic-error-coeffs-0+
  (make-array '(2 4 4) :initial-contents
              '((( 3.85268  -21.229      -0.330434   0.0127842)
                 (-1.61486    0.706564    0.225945   0.263682)
                 (-0.910164   0.388383    0.00551445 0.00671814)
                 (-0.630184   0.192402    0.0098871  0.0102527))
                ((-0.162211   9.94329     0.13723    0.0124084)
                 (-0.253135   0.00187735  0.0230286  0.01264)
                 (-0.0695069 -0.0437594   0.0120636  0.0163087)
                 (-0.0328856 -0.00926032 -0.00173573 0.00527385)))))

(defparameter +cubic-error-coeffs-1+
  (make-array '(2 4 4) :initial-contents
              '((( 0.0899116 -19.2349    -4.11711     0.183362)
                 ( 0.138148   -1.45804    1.32044     1.38474)
                 ( 0.230903   -0.450262   0.219963    0.414038)
                 ( 0.0590565  -0.101062   0.0430592   0.0204699))
                (( 0.0164649   9.89394    0.0919496   0.00760802)
                 ( 0.0191603  -0.0322058  0.0134667  -0.0825018)
                 ( 0.0156192  -0.017535   0.00326508 -0.228157)
                 (-0.0236752   0.0405821 -0.0173086   0.176187)))))

;;; [elbeno:]
;;; compute the error of a cubic bezier
;;; that approximates an elliptical arc
;;; with radii a, b
;;; between angles eta1 and eta2

(defun calc-c-term (i b/a etasum arr)
  (loop
     for j from 0 to 3
     sum (* (/ (+ (* (aref arr i j 0) b/a b/a)
                  (* (aref arr i j 1) b/a)
                  (aref arr i j 2))
               (+ (aref arr i j 3) b/a))
            (cos (* j etasum)))))

(defun bezier-error (a b eta1 eta2)
  (let* ((b/a (/ b a))
         (etadiff (- eta2 eta1))
         (etasum (+ eta2 eta1))
         (arr (if (< b/a 0.25)
                  +cubic-error-coeffs-0+
                  +cubic-error-coeffs-1+)))
    (* (/ (+ (* 0.001 b/a b/a) (* 4.98 b/a) 0.207)
          (+ b/a 0.0067))
       a
       (exp (+ (calc-c-term 0 b/a etasum arr)
               (* (calc-c-term 1 b/a etasum arr) etadiff))))))

(defun ellipse-val (cx cy a b theta eta)
  (values
   (+ cx
      (* a (cos theta) (cos eta))
      (* (- b) (sin theta) (sin eta)))
   (+ cy
      (* a (sin theta) (cos eta))
      (* b (cos theta) (sin eta)))))

(defun ellipse-deriv-val (a b theta eta)
  (values
   (+ (* (- a) (cos theta) (sin eta))
      (* (- b) (sin theta) (cos eta)))
   (+ (* (- a) (sin theta) (sin eta))
      (* b (cos theta) (cos eta)))))

(defun curve-to (path cx1 cy1 cx2 cy2 x y)
  (%curve-to path cx1 cy1 cx2 cy2 x y))

(defun %curve-to (path cx1 cy1 cx2 cy2 x y)
  "Draw a cubic Bezier curve from the current point to (x,y)
through two control points."
  (let ((control-point-1 (paths:make-point cx1 cy1))
        (control-point-2 (paths:make-point cx2 cy2))
        (end-point (paths:make-point x y)))
    (paths:path-extend path
                       (paths:make-bezier-curve (list control-point-1
                                                      control-point-2))
                       end-point)))

(defun approximate-arc-single (path cx cy a b theta eta1 eta2)
  (let* ((etadiff (- eta2 eta1))
         (k (tan (/ etadiff 2)))
         (alpha (* (sin etadiff)
                   (/ (1- (sqrt (+ 4 (* 3 k k)))) 3)))
         px1 py1
         px2 py2
         qx1 qy1
         qx2 qy2
         sx1 sy1
         sx2 sy2)
    (setf (values px1 py1) (ellipse-val cx cy a b theta eta1)
          (values px2 py2) (ellipse-val cx cy a b theta eta2)
          (values sx1 sy1) (ellipse-deriv-val a b theta eta1)
          (values sx2 sy2) (ellipse-deriv-val a b theta eta2)
          qx1 (+ px1 (* alpha sx1))
          qy1 (+ py1 (* alpha sy1))
          qx2 (- px2 (* alpha sx2))
          qy2 (- py2 (* alpha sy2)))
    (curve-to path qx1 qy1 qx2 qy2 px2 py2)))

(defun approximate-arc (path cx cy a b theta eta1 eta2 err)
  (cond ((< eta2 eta1)
         (error "approximate-arc: eta2 must be bigger than eta1"))
        ((> eta2 (+ eta1 (/ pi 2) (* eta2 long-float-epsilon)))
         (let ((etamid (+ eta1 (/ pi 2) (* eta2 long-float-epsilon))))
           (approximate-arc path cx cy a b theta eta1 etamid err)
           (approximate-arc path cx cy a b theta etamid eta2 err)))
        (t (if (> err (bezier-error a b eta1 eta2))
               (approximate-arc-single path cx cy a b theta eta1 eta2)
               (let ((etamid (/ (+ eta1 eta2) 2)))
                 (approximate-arc path cx cy a b theta eta1 etamid err)
                 (approximate-arc path cx cy a b theta etamid eta2 err))))))

(defun approximate-elliptical-arc (path cx cy a b theta eta1 eta2
                                   &optional (err 0.5))
  "Approximate an elliptical arc with a cubic bezier spline into the path."
  (if (> b a)
      (approximate-arc path cx cy b a
                            (+ theta (/ pi 2))
                            (- eta1 (/ pi 2))
                            (- eta2 (/ pi 2)) err)
      (approximate-arc path cx cy a b theta eta1 eta2 err)))

(defun arc (cx cy r theta1 theta2)
  (loop while (< theta2 theta1) do (incf theta2 (* 2 pi)))
  (let ((path (paths:create-path :open-polyline)))
    (multiple-value-bind (startx starty) (ellipse-val cx cy r r 0 theta1)
      (paths:path-reset path (paths:make-point startx starty))
      (approximate-elliptical-arc path cx cy r r 0 theta1 theta2)
      path)))

(defun ellipse-arc (cx cy rx ry theta lambda1 lambda2)
  (let ((eta1 (atan (/ (sin lambda1) ry) (/ (cos lambda1) rx)))
        (eta2 (atan (/ (sin lambda2) ry) (/ (cos lambda2) rx)))
        (2pi (* 2 pi)))
    ;; make sure we have eta1 <= eta2 <= eta1 + 2 PI
    (decf eta2 (* 2pi (floor (- eta2 eta1) 2pi)))

    ;; the preceding correction fails if we have exactly et2 - eta1 = 2 PI
    ;; it reduces the interval to zero length
    (when (and (> (- lambda2 lambda1) pi) (< (- eta2 eta1) pi))
      (incf eta2 2pi))

    (let ((path (paths:create-path :open-polyline)))
      (multiple-value-bind (startx starty) (ellipse-val cx cy rx ry theta eta1)
        (paths:path-reset path (paths:make-point startx starty))
        (approximate-elliptical-arc path cx cy rx ry theta eta1 eta2)
        path))))
