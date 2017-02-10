;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mandelbrot
  (:use :cl)
  (:export #:spawn #:benchmark))

(in-package :mandelbrot)

(defun hue-to-rgb (h)
  (declare (optimize (speed 3) (safety 0))
           (type single-float h))
  (let* ((h* (/ (rem h 360.0f0) 60.0f0))
         (x (- 1 (abs (- (mod h* 2.0f0) 1.0f0)))))
    (declare (type single-float h* x))
    (cond
      ((< h* 1.0f0) (values 1.0f0 x     0.0f0))
      ((< h* 2.0f0) (values x     1.0f0 0.0f0))
      ((< h* 3.0f0) (values 0.0f0 1.0f0 x))
      ((< h* 4.0f0) (values 0.0f0 x     1.0f0))
      ((< h* 5.0f0) (values x     0.0f0 1.0f0))
      ((< h* 6.0f0) (values 1.0f0 0.0f0 x)))))

(defun m (cr ci iterations)
  (declare (optimize (speed 3) (safety 0))
           (type single-float cr ci)
           (type fixnum iterations))
  (let ((zr 0.0f0) (zi 0.0f0))
    (declare (type single-float zr zi))
    (dotimes (i iterations nil)
      (declare (type fixnum i))
      (let ((zr2 (* zr zr))
            (zi2 (* zi zi)))
        (declare (type single-float zr2 zi2))
        (psetf zr (+ cr (- zr2 zi2))
               zi (+ ci (* zi zr) (* zr zi)))
        (when (> (+ zr2 zi2) 4.0f0)
          (return (* (/ (float i 0.0f0) (float iterations 0.0f0)) 360.0f0)))))))

(defun render-mandelbrot (x y width height hue-offset)
  "Render one pixel."
  (declare (optimize (speed 3) (safety 0))
           (type single-float x y width height hue-offset))
  (let* ((scale (/ 4.0f0 width))
         (x^ (- x (/ width 2.0f0)))
         (y^ (- y (/ height 2.0f0)))
         (r 0.0f0) (g 0.0f0) (b 0.0f0))
    (declare (type single-float scale x^ y^ r g b))
    (flet ((frag (x y)
             (declare (type single-float x y))
             (let ((hue (m (- (* scale x) 0.5f0)
                           (* scale y)
                           25)))
               (when hue
                 (multiple-value-bind (r* g* b*)
                     (hue-to-rgb (+ hue-offset (the single-float hue)))
                   (declare (type single-float r* g* b*))
                   (incf r r*)
                   (incf g g*)
                   (incf b b*))))))
      (frag    x^           y^)
      (frag (+ x^ 0.5f0)    y^)
      (frag    x^        (+ y^ 0.5f0))
      (frag (+ x^ 0.5f0) (+ y^ 0.5f0))
      (mezzano.gui:make-colour (/ r 4.0f0) (/ g 4.0f0) (/ b 4.0f0)))))

(defgeneric dispatch-event (frame event)
  (:method (f e)))

(defmethod dispatch-event (frame (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep frame) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame frame))

(defmethod dispatch-event (frame (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event frame event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

(defmethod dispatch-event (frame (event mezzano.gui.compositor:window-close-event))
  (throw 'quit nil))

(defun benchmark (&optional (width 500) (height width))
  (let ((framebuffer (mezzano.gui:make-surface width height))
        (hue-offset (rem (get-universal-time) 360)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (mezzano.gui:surface-pixel framebuffer x y)
              (render-mandelbrot (float x) (float y) (float width) (float height) (float hue-offset)))))
    framebuffer))

(defun mandelbrot-main (width height)
  (with-simple-restart (abort "Close Mandelbrot")
    (catch 'quit
      (let ((fifo (mezzano.supervisor:make-fifo 50)))
        (mezzano.gui.compositor:with-window (window fifo width height)
          (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
                 (frame (make-instance 'mezzano.gui.widgets:frame
                                       :framebuffer framebuffer
                                       :title "Mandelbrot"
                                       :close-button-p t
                                       :damage-function (mezzano.gui.widgets:default-damage-function window))))
            (mezzano.gui.widgets:draw-frame frame)
            (mezzano.gui.compositor:damage-window window
                                                  0 0
                                                  (mezzano.gui.compositor:width window)
                                                  (mezzano.gui.compositor:height window))
            (multiple-value-bind (left right top bottom)
                (mezzano.gui.widgets:frame-size frame)
              (let ((width (- (mezzano.gui.compositor:width window) left right))
                    (height (- (mezzano.gui.compositor:height window) top bottom))
                    (pixel-count 0)
                    (hue-offset (rem (get-universal-time) 360)))
                ;; Render a line at a time, should do this in a seperate thread really...
                ;; More than one thread, even.
                (dotimes (y height)
                  (dotimes (x width)
                    (setf (mezzano.gui:surface-pixel framebuffer (+ left x) (+ top y))
                          (render-mandelbrot (float x) (float y) (float width) (float height) (float hue-offset))))
                  (mezzano.gui.compositor:damage-window window left (+ top y) width 1)
                  (loop
                     (let ((evt (mezzano.supervisor:fifo-pop fifo nil)))
                       (when (not evt) (return))
                       (dispatch-event frame evt)))))
              (loop
                 (dispatch-event frame (mezzano.supervisor:fifo-pop fifo))))))))))

(defun spawn (&optional (width 500) (height width))
  (mezzano.supervisor:make-thread (lambda () (mandelbrot-main width height))
                                  :name "Mandelbrot"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Mandelbrot console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
