;;;; Mandelbrot viewer written by Mike "scgtrp" Smith, IPhD.

(defpackage #:mandelbrot
  (:use #:cl))

(in-package #:mandelbrot)

(defclass mandelbrot-window (sys.graphics::window-with-chrome)
  ())

(defun m (cr ci iterations)
  (let ((zr 0) (zi 0))
    (dotimes (i iterations (sys.graphics::make-colour :black))
      (psetf zr (+ cr (- (* zr zr) (* zi zi)))
             zi (+ ci (* zi zr) (* zr zi)))
      (when (> (+ (* zr zr) (* zi zi)) 4)
        (return (sys.graphics::make-colour
                 (list (/ (rem i 7) 7) (/ (rem i 5) 5) (* 2 (/ i iterations)))))))))

(defmethod sys.graphics::window-redraw ((window mandelbrot-window))
  (let* ((fb (sys.graphics::window-backbuffer window))
         (dims (array-dimensions fb))
         (width (second dims))
         (height (first dims))
         (white (sys.graphics::make-colour :white))
         (scale (/ 4 width)))
    (multiple-value-bind (left right top bottom)
        (sys.graphics::compute-window-margins window)
      (setf width (- width left right))
      (setf height (- height top bottom))
      (dotimes (x width)
        (dotimes (y height)
          (let ((cr (- (* scale (- x (/ width 2))) 0.5))
                (ci (* scale (- y (/ height 2)))))
            (setf (aref fb (+ y top) (+ x bottom)) (m cr ci 100))))))))

(defun create-mandelbrot-window ()
  (sys.graphics::window-set-visibility (sys.graphics::make-window "Mandelbrot" 500 500 'mandelbrot-window) t))

(setf (gethash (name-char "F5") sys.graphics::*global-keybindings*) 'create-mandelbrot-window)
