;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.gui.image-viewer
  (:use :cl)
  (:export #:spawn))

(in-package :mezzano.gui.image-viewer)

(defclass image-viewer ()
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%thread :initarg :thread :reader thread)
   (%font :initarg :font :reader font)
   (%frame :initarg :frame :reader frame)))

(defgeneric dispatch-event (viewer event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (declare (ignore window event))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  (declare (ignore window event)))

(defun compute-window-size (image)
  ;; Make a fake frame to get the frame size.
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (make-instance 'mezzano.gui.widgets:frame))
    (values (+ left (max 32 (array-dimension image 1)) right)
            (+ top (max 32 (array-dimension image 0)) bottom))))

(defun main (path)
  (mezzano.gui.font:with-font (font
                               mezzano.gui.font:*default-monospace-font*
                               mezzano.gui.font:*default-monospace-font-size*)
    (let ((fifo (mezzano.supervisor:make-fifo 50))
          (image (mezzano.gui.desktop::load-image path)))
      (multiple-value-bind (width height)
          (compute-window-size image)
        (mezzano.gui.compositor:with-window (window fifo width height)
          (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
                 (frame (make-instance 'mezzano.gui.widgets:frame
                                       :framebuffer framebuffer
                                       :title (namestring path)
                                       :close-button-p t
                                       :damage-function (mezzano.gui.widgets:default-damage-function window)))
                 (viewer (make-instance 'image-viewer
                                        :fifo fifo
                                        :window window
                                        :thread (mezzano.supervisor:current-thread)
                                        :font font
                                        :frame frame)))
            (multiple-value-bind (left right top bottom)
                (mezzano.gui.widgets:frame-size frame)
              (mezzano.gui:bitblt (array-dimension image 0) (array-dimension image 1)
                                  image 0 0
                                  framebuffer
                                  (+ top (- (truncate (- height top bottom) 2) (truncate (array-dimension image 0) 2)))
                                  (+ left (- (truncate (- width left right) 2) (truncate (array-dimension image 1) 2))))
              (mezzano.gui.widgets:draw-frame frame)
              (mezzano.gui.compositor:damage-window window
                                                    0 0
                                                    width height))
            (loop
               (handler-case
                   (dispatch-event viewer (mezzano.supervisor:fifo-pop fifo))
                 (error (c)
                   (ignore-errors
                     (format t "Error: ~A~%" c)))
                 ;; Exit when the close button is clicked.
                 (mezzano.gui.widgets:close-button-clicked ()
                   (return-from main))))))))))

(defun spawn (path)
  (setf path (merge-pathnames path))
  (mezzano.supervisor:make-thread (lambda () (main path))
                                  :name (format nil "Image Viewer - ~S" path)
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Image Viewer console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
