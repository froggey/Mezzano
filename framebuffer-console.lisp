(in-package :sys.int)

;; Take over the cold stream.
(defun fbcon-initialize ()
  (when (and (boundp '*cold-stream-screen*)
             (eql (first *cold-stream-screen*) :framebuffer))
    (let* ((fb (second *cold-stream-screen*))
           (stream (make-instance 'framebuffer-stream
                                  :framebuffer fb
                                  :x (car *screen-offset*)
                                  :y (cdr *screen-offset*))))
      (let ((buffer (make-array (array-dimensions fb)
                                :element-type '(unsigned-byte 32)))
            (width (second (array-dimensions fb)))
            (height (first (array-dimensions fb))))
        (sys.graphics::register-screen :framebuffer buffer buffer
                                       (lambda ()
                                         (%bitblt height width buffer 0 0 fb 0 0))))
      (setf *terminal-io* stream
            *cold-stream-screen* (list (lambda (c) (write-char c stream)))))))

(add-hook '*initialize-hook* 'fbcon-initialize)
