;;;; Starfield screensaver

(defpackage :mezzano.gui.starfield
  (:use :cl)
  (:export #:spawn))

(in-package :mezzano.gui.starfield)

(defclass starfield ()
  ((%window :initarg :window :accessor starfield-window)
   (%backbuffer :initarg :backbuffer :accessor starfield-backbuffer)))

(defclass star ()
  ((%angle :initarg :angle :accessor star-angle)
   (%radius :initarg :radius :accessor star-radius)))

(defconstant +white+ (mezzano.gui:make-colour 1.0 1.0 1.0))
(defconstant +black+ (mezzano.gui:make-colour 0.0 0.0 0.0))

(defgeneric dispatch-event (app event)
  (:method (f e)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:window-activation-event))
  (when (not (mezzano.gui.compositor:state event))
    (throw 'quit nil)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:key-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event mezzano.gui.compositor:mouse-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event mezzano.gui.compositor:window-close-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event mezzano.gui.compositor:quit-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event mezzano.gui.compositor:screen-geometry-update))
  (let ((new-framebuffer (mezzano.gui:make-surface (mezzano.gui.compositor:width event)
                                                   (mezzano.gui.compositor:height event)))
        (new-backbuffer (mezzano.gui:make-surface (mezzano.gui.compositor:width event)
                                                  (mezzano.gui.compositor:height event))))
    (mezzano.gui.compositor:resize-window (starfield-window app) new-framebuffer)
    (setf (starfield-backbuffer app) new-backbuffer)))

(defun make-stars (n-stars)
  (let ((stars (make-array n-stars)))
    (dotimes (i n-stars)
      (setf (aref stars i) (make-instance 'star
                                          :angle (* (random 1.0f0) 2 pi)
                                          :radius (random 1.0f0))))
    stars))

(defun render-star (star framebuffer x y width height)
  (let ((star-x (truncate (+ x (* 1.5 (star-radius star) (cos (star-angle star)) (/ width 2)) (/ width 2))))
        (star-y (truncate (+ y (* 1.5 (star-radius star) (sin (star-angle star)) (/ height 2)) (/ height 2)))))
    (mezzano.gui:bitset :set 2 2
                        +white+
                        framebuffer
                        star-x
                        star-y)))

(defun render-starfield (stars framebuffer x y width height)
  (mezzano.gui:bitset :set width height
                      +black+
                      framebuffer
                      x y)
  (loop
     for star across stars
     do (render-star star framebuffer x y width height)))

(defparameter *star-speed* 1)

(defun update-starfield (stars dt)
  (loop
     for star across stars
     do
       (incf (star-radius star) (* (expt (star-radius star) 1.25) dt *star-speed*))
       (when (> (star-radius star) 1.0)
         (setf (star-angle star) (* (random 1.0f0) 2 pi)
               (star-radius star) (+ (random 0.2f0) (random 0.1f0))))))

(defun starfield-main ()
  (with-simple-restart (abort "Close Starfield")
    (catch 'quit
      (let ((fifo (mezzano.supervisor:make-fifo 50))
            (stars (make-stars 100))
            (starfield (make-instance 'starfield)))
        (mezzano.gui.compositor:with-window (window fifo 0 0 :initial-z-order :top :layer :top :name starfield :kind :screensaver)
          (mezzano.gui.compositor:subscribe-notification window :screen-geometry)
          (mezzano.gui.compositor:set-window-data window :cursor :none)
          (setf (starfield-window starfield) window
                (starfield-backbuffer starfield) (mezzano.gui:make-surface 0 0))
          (loop
             (let ((start-time (get-internal-run-time))
                   (framebuffer (mezzano.gui.compositor:window-buffer window))
                   (backbuffer (starfield-backbuffer starfield))
                   (width (mezzano.gui.compositor:width window))
                   (height (mezzano.gui.compositor:height window)))
               (render-starfield stars backbuffer 0 0 width height)
               (mezzano.gui:bitblt :set width height
                                   backbuffer 0 0
                                   framebuffer 0 0)
               (mezzano.gui.compositor:damage-window window 0 0 width height)
               (loop
                  (let ((evt (mezzano.supervisor:fifo-pop fifo nil)))
                    (when (not evt) (return))
                    (dispatch-event starfield evt)))
               (sleep (/ 1 30))
               (let ((dt (/ (- (get-internal-run-time) start-time)
                            (float internal-time-units-per-second 0.0d0))))
                 (update-starfield stars dt)))))))))

(defun spawn ()
  (mezzano.supervisor:make-thread (lambda () (starfield-main))
                                  :name "Starfield"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Mandelbrot console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
