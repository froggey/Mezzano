;;;; Fractal renderer.

(defpackage :mezzano.mandelbrot
  (:use :cl)
  (:export #:spawn #:benchmark))

(in-package :mezzano.mandelbrot)

(defun hue-to-rgb (h)
  (declare (optimize (speed 3) (safety 0))
           (type single-float h))
  (let* ((h* h)
         ;; Convert 1.0 to 0.0
         (h^ (* (the single-float
                     (if (>= h* 1.0f0)
                         (- h* 1.0f0)
                         h*))
                6.0f0))
         (index (truncate h^))
         (f (- h^ (float index 0.0f0)))
         (q (- 1.0f0 f)))
    (declare (type single-float h* h^)
             (type fixnum index))
    (case index
      (0 (values 1.0f0 f     0.0f0))
      (1 (values q     1.0f0 0.0f0))
      (2 (values 0.0f0 1.0f0 f))
      (3 (values 0.0f0 q     1.0f0))
      (4 (values f     0.0f0 1.0f0))
      (5 (values 1.0f0 0.0f0 q)))))

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
        (return (/ (float i 0.0f0) (float iterations 0.0f0)))))))

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
               ;; Hue and hue-offset are both in [0,1]
               (when hue
                 ;; Keep the final hue within the range [0,1]
                 (let ((final-hue (+ (the single-float hue) hue-offset)))
                   (declare (type single-float final-hue))
                   (when (>= final-hue 1.0f0)
                     (decf final-hue 1.0f0))
                   (multiple-value-bind (r* g* b*)
                       (hue-to-rgb final-hue)
                     (declare (type single-float r* g* b*))
                     (incf r r*)
                     (incf g g*)
                     (incf b b*)))))))
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

(defun benchmark (&key (width 500) (height width) julia (hue (get-universal-time)))
  (let ((framebuffer (mezzano.gui:make-surface width height))
        (hue-offset (/ (rem hue 360) 360.0)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (mezzano.gui:surface-pixel framebuffer x y)
              (render-mandelbrot (float x 0.0f0) (float y 0.0f0)
                                 (float width 0.0f0) (float height 0.0f0)
                                 hue-offset
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
            (setf (mezzano.gui.compositor:name window) app)
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
                       (hue-offset (/ (rem (get-universal-time) 360) 360.0)))
                   ;; Render a line at a time, should do this in a seperate thread really...
                   ;; More than one thread, even.
                   (dotimes (y height)
                     (dotimes (x width)
                       (setf (mezzano.gui:surface-pixel framebuffer (+ left x) (+ top y))
                             (render-mandelbrot (float x) (float y)
                                                (float width) (float height)
                                                hue-offset
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
