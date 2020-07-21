;;;; Demo of the compositor's display post-processing effects.

(defpackage :colour-matrix-demo
  (:use :cl)
  (:local-nicknames (:gui :mezzano.gui)
                    (:comp :mezzano.gui.compositor)
                    (:simd :mezzano.simd))
  (:export #:demo))

(in-package :colour-matrix-demo)

(defconstant +red-luminance-weight+ 0.3086)
(defconstant +green-luminance-weight+ 0.6094)
(defconstant +blue-luminance-weight+ 0.0820)

(defun make-identity-colour-matrix ()
  (mezzano.gui:make-colour-matrix 1.0 0.0 0.0 0.0
                                  0.0 1.0 0.0 0.0
                                  0.0 0.0 1.0 0.0
                                  0.0 0.0 0.0 1.0))

(defun make-random-colour-matrix (&key (min 0.0) (max 1.0))
  (mezzano.gui:make-colour-matrix (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))
                                  (+ min (random (- max min)))))

(defun make-saturation-colour-matrix (saturation)
  (let* ((s saturation)
         (sr (* (- 1.0 s) +red-luminance-weight+))
         (sg (* (- 1.0 s) +green-luminance-weight+))
         (sb (* (- 1.0 s) +blue-luminance-weight+)))
    (gui:make-colour-matrix
     (+ sr s) sg       sb       0.0
     sr       (+ sg s) sb       0.0
     sr       sg       (+ sb s) 0.0
     0.0      0.0      0.0      1.0)))

(defun make-invert-colour-matrix ()
  (gui:make-colour-matrix
   -1.0  0.0  0.0  1.0
    0.0 -1.0  0.0  1.0
    0.0  0.0 -1.0  1.0
    0.0  0.0  0.0  1.0))

(defun make-tint-colour-matrix (r g b)
  (gui:make-colour-matrix
   (/ r 3) (/ r 3) (/ r 3) 0.0
   (/ g 3) (/ g 3) (/ g 3) 0.0
   (/ b 3) (/ b 3) (/ b 3) 0.0
   0.0     0.0     0.0     1.0))

(declaim (inline vec-set1))
(defun vec-set1 (value)
  (declare (type single-float value))
  (let ((v (simd::%single-float-to-sse-vector value)))
    (simd:shufps v v #4r0000)))

(defun %lerp-matrix (r x y a)
  (declare (type gui::matrix4 r x y)
           (type single-float a))
  (let* ((a-vec (vec-set1 a))
         (1-a-vec (simd:subps (vec-set1 1.0) a-vec))
         ;; Matrix columns
         (x-c0 (simd:sse-vector-single-float-4-ref x 0))
         (x-c1 (simd:sse-vector-single-float-4-ref x 4))
         (x-c2 (simd:sse-vector-single-float-4-ref x 8))
         (x-c3 (simd:sse-vector-single-float-4-ref x 12))
         (y-c0 (simd:sse-vector-single-float-4-ref y 0))
         (y-c1 (simd:sse-vector-single-float-4-ref y 4))
         (y-c2 (simd:sse-vector-single-float-4-ref y 8))
         (y-c3 (simd:sse-vector-single-float-4-ref y 12))
         ;; Lerped result columns
         (r-c0 (simd:addps (simd:mulps x-c0 1-a-vec) (simd:mulps y-c0 a-vec)))
         (r-c1 (simd:addps (simd:mulps x-c1 1-a-vec) (simd:mulps y-c1 a-vec)))
         (r-c2 (simd:addps (simd:mulps x-c2 1-a-vec) (simd:mulps y-c2 a-vec)))
         (r-c3 (simd:addps (simd:mulps x-c3 1-a-vec) (simd:mulps y-c3 a-vec))))
    ;; Store result columns.
    (setf (simd:sse-vector-single-float-4-ref r 0) r-c0
          (simd:sse-vector-single-float-4-ref r 4) r-c1
          (simd:sse-vector-single-float-4-ref r 8) r-c2
          (simd:sse-vector-single-float-4-ref r 12) r-c3)
    r))

(defun lerp-matrix (x y a)
  (check-type x gui:colour-matrix)
  (check-type y gui:colour-matrix)
  (check-type a single-float)
  (let ((r (make-array 16 :element-type 'single-float)))
    (%lerp-matrix r
                  (gui::colour-matrix-elements x)
                  (gui::colour-matrix-elements y)
                  a)
    (gui::%make-colour-matrix :elements r)))

(defun print-matrix (mat)
  (dotimes (i 4)
    (format t "~S ~15T ~S ~30T ~S ~45T ~S~%"
            (gui:colour-matrix-element mat 0 i)
            (gui:colour-matrix-element mat 1 i)
            (gui:colour-matrix-element mat 2 i)
            (gui:colour-matrix-element mat 3 i))))

(defun demo ()
  (unwind-protect
       (let ((current-matrix (make-identity-colour-matrix)))
         (multiple-value-bind (start-x start-y)
             (ignore-errors
               (multiple-value-prog1
                   (mezzano.internals::stream-cursor-pos *terminal-io*)
                 (print-matrix current-matrix)))
           (flet ((lerp-to (target-matrix &optional (time 2.0))
                    (loop
                       with step-size = (/ 30.0) ; target frame rate
                       for start-time = (/ (get-internal-real-time) internal-time-units-per-second)
                       for lerp-time from 0.0 by (* (/ time) step-size) to 1.0
                       do
                         (setf (comp:postprocess-matrix) (lerp-matrix current-matrix target-matrix lerp-time))
                         (ignore-errors
                           (multiple-value-bind (cur-x cur-y)
                               (mezzano.internals::stream-cursor-pos *terminal-io*)
                             (mezzano.internals::stream-clear-between *terminal-io*
                                                                      start-x start-y
                                                                      cur-x cur-y)
                             (mezzano.internals::stream-move-to *terminal-io*
                                                                start-x start-y))
                           (let ((*standard-output* *terminal-io*))
                             (print-matrix (comp:postprocess-matrix))))
                         (let* ((end-time (/ (get-internal-real-time) internal-time-units-per-second))
                                (run-time (- end-time start-time)))
                           (sleep (max 0.0 (- step-size run-time)))))
                    (setf current-matrix target-matrix)))
             (lerp-to (make-invert-colour-matrix))
             (lerp-to (gui:colour-matrix-matrix-multiply
                       (make-invert-colour-matrix)
                       (make-saturation-colour-matrix 0.0)))
             (loop repeat 5 do
                  (lerp-to (make-random-colour-matrix :min -0.75 :max 0.75)))
             (lerp-to (make-saturation-colour-matrix 10.0))
             (sleep 0.1)
             (loop repeat 5 do
                  (lerp-to (make-random-colour-matrix :min -1.5 :max 1.5)))
             (lerp-to (make-saturation-colour-matrix 0.0))
             (sleep 0.1)
             (loop repeat 10 do
                  (lerp-to (make-random-colour-matrix :min 0.0 :max 0.5) 1.0))
             (lerp-to (make-identity-colour-matrix))
             (lerp-to (make-tint-colour-matrix 0.5 1.0 1.0))
             (lerp-to (make-tint-colour-matrix 1.0 0.5 0.7))
             (lerp-to (make-saturation-colour-matrix 0.0))
             (lerp-to (make-tint-colour-matrix 1.0 0.5 0.7))
             (lerp-to (make-tint-colour-matrix 0.5 1.0 1.0))
             (lerp-to (make-identity-colour-matrix)))
           (ignore-errors
             (multiple-value-bind (cur-x cur-y)
                 (mezzano.internals::stream-cursor-pos *terminal-io*)
               (mezzano.internals::stream-clear-between *terminal-io*
                                                        start-x start-y
                                                        cur-x cur-y)
               (mezzano.internals::stream-move-to *terminal-io*
                                                  start-x start-y)))))
    (setf (comp:postprocess-matrix) nil))
  (values))
