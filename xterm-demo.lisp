(cl:defpackage :xterm-demo
  (:use :cl)
  (:export #:do-xterm-demo)

(in-package :xterm-demo)

(defclass xterm-demo-window (sys.graphics::window-with-chrome)
  ((terminal :reader terminal)))

(defmethod initialize-instance :after ((window xterm-demo-window) &rest initargs)
  (multiple-value-bind (left right top bottom)
      (sys.graphics::compute-window-margins window)
    (let ((fb (sys.graphics::window-backbuffer window)))
      (setf (slot-value window 'terminal)
            (make-instance 'sys.xterm:xterm-terminal
                           :framebuffer fb
                           :x left
                           :y top
                           :width (- (array-dimension fb 1) left right)
                           :height (- (array-dimension fb 0) top bottom))))))

(defmethod sys.graphics::window-redraw ((window xterm-demo-window)))

(defmethod sys.graphics::window-close-event ((window xterm-demo-window))
  (sys.graphics::close-window window))

(defun do-xterm-demo (&rest args)
  (let ((process (make-process "xterm-demo")))
    (apply #'sys.int::process-preset process #'do-xterm-demo-1 args)
    (sys.int::process-enable process)))

(defun do-xterm-demo-1 (&key file (width 640) (height 400))
  (let* ((window (sys.graphics::make-window "Xterm test."
                                            width height
                                            'xterm-demo-window))
         (terminal (terminal window)))
    (sys.graphics::window-set-visibility window t)
    (with-open-file (s file :element-type '(unsigned-byte 8))
      (loop (let ((byte (read-byte s nil)))
              (when (null byte) (return))
              (write-byte byte terminal)
              (sys.graphics::update-window window))))))
