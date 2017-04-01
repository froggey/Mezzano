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

(defun m (cr ci zr zi iterations)
  (declare (optimize (speed 3) (safety 0))
           (type single-float cr ci zr zi)
           (type fixnum iterations))
  (dotimes (i iterations nil)
    (declare (type fixnum i))
    (let ((zr2 (* zr zr))
          (zi2 (* zi zi)))
      (declare (type single-float zr2 zi2))
      (psetf zr (+ cr (- zr2 zi2))
             zi (+ ci (* zi zr) (* zr zi)))
      (when (> (+ zr2 zi2) 4.0f0)
        (return (* (/ (float i 0.0f0) (float iterations 0.0f0)) 360.0f0))))))

(defun render-mandelbrot (x y width height hue-offset julia)
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
             (let ((hue (if julia
                            (m -0.4f0 0.6f0
                               (* scale x) (* scale y)
                               250)
                            (m (- (* scale x) 0.5f0) (* scale y)
                               0.0f0 0.0f0
                               25))))
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

(defclass mandelbrot ()
  ((%frame :initarg :frame :accessor frame)
   (%window :initarg :window :accessor window)
   (%fifo :initarg :fifo :accessor fifo)
   (%juliap :initarg :juliap :accessor juliap)))

(defgeneric dispatch-event (app event)
  (:method (f e)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame app)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame app)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event (frame app) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:window-close-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event mezzano.gui.compositor:quit-event))
  (throw 'quit nil))

(define-condition must-redraw () ())

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-request-event))
  (let ((old-width (mezzano.gui.compositor:width (window app)))
        (old-height (mezzano.gui.compositor:height (window app)))
        (new-width (max 100 (mezzano.gui.compositor:width event)))
        (new-height (max 100 (mezzano.gui.compositor:height event))))
    (when (or (not (eql old-width new-width))
              (not (eql old-height new-height)))
      (let ((new-framebuffer (mezzano.gui:make-surface
                              new-width new-height)))
        (mezzano.gui.widgets:resize-frame (frame app) new-framebuffer)
        (mezzano.gui.compositor:resize-window
         (window app) new-framebuffer
         :origin (mezzano.gui.compositor:resize-origin event))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-event))
  (signal 'must-redraw))

(defmethod dispatch-event (app (event mezzano.gui.compositor:key-event))
  (when (mezzano.gui.compositor:key-releasep event)
    (case (mezzano.gui.compositor:key-key event)
      ((#\j #\J)
       (setf (juliap app) t))
      ((#\m #\M)
       (setf (juliap app) nil)))
    (signal 'must-redraw)))

(defun benchmark (&optional (width 500) (height width) julia)
  (let ((framebuffer (mezzano.gui:make-surface width height))
        (hue-offset (rem (get-universal-time) 360)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (mezzano.gui:surface-pixel framebuffer x y)
              (render-mandelbrot (float x 0.0f0) (float y 0.0f0)
                                 (float width 0.0f0) (float height 0.0f0)
                                 (float hue-offset 0.0f0)
                                 julia))))
    framebuffer))

(defun mandelbrot-main (width height)
  (with-simple-restart (abort "Close Mandelbrot")
    (catch 'quit
      (let ((fifo (mezzano.supervisor:make-fifo 50)))
        (mezzano.gui.compositor:with-window (window fifo width height)
          (let* ((frame (make-instance 'mezzano.gui.widgets:frame
                                       :framebuffer (mezzano.gui.compositor:window-buffer window)
                                       :title "Mandelbrot"
                                       :close-button-p t
                                       :resizablep t
                                       :damage-function (mezzano.gui.widgets:default-damage-function window)
                                       :set-cursor-function (mezzano.gui.widgets:default-cursor-function window)))
                 (app (make-instance 'mandelbrot
                                     :fifo fifo
                                     :window window
                                     :frame frame
                                     :juliap nil)))
            (mezzano.gui.widgets:draw-frame (frame app))
            (mezzano.gui.compositor:damage-window window
                                                  0 0
                                                  (mezzano.gui.compositor:width window)
                                                  (mezzano.gui.compositor:height window))
            (tagbody
             REDRAW
               (multiple-value-bind (left right top bottom)
                   (mezzano.gui.widgets:frame-size (frame app))
                 (let ((framebuffer (mezzano.gui.compositor:window-buffer window))
                       (width (- (mezzano.gui.compositor:width window) left right))
                       (height (- (mezzano.gui.compositor:height window) top bottom))
                       (pixel-count 0)
                       (hue-offset (rem (get-universal-time) 360)))
                   ;; Render a line at a time, should do this in a seperate thread really...
                   ;; More than one thread, even.
                   (dotimes (y height)
                     (dotimes (x width)
                       (setf (mezzano.gui:surface-pixel framebuffer (+ left x) (+ top y))
                             (render-mandelbrot (float x) (float y)
                                                (float width) (float height)
                                                (float hue-offset)
                                                (juliap app))))
                     (mezzano.gui.compositor:damage-window window left (+ top y) width 1)
                     (loop
                        (let ((evt (mezzano.supervisor:fifo-pop fifo nil)))
                          (when (not evt) (return))
                          (handler-case
                              (dispatch-event app evt)
                            (must-redraw ()
                              (go REDRAW))))))))
               (loop
                  (handler-case
                      (dispatch-event app (mezzano.supervisor:fifo-pop fifo))
                    (must-redraw ()
                      (go REDRAW)))))))))))

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
