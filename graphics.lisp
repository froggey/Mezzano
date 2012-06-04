(in-package #:sys.int)

(defmacro with-saved-screen ((&optional framebuffer &rest options) &body body)
  (let ((fb-sym (or framebuffer (gensym))))
    `(%with-saved-screen (lambda (,fb-sym) (declare (ignorable ,fb-sym)) ,@body) ,@options)))

(defun framebuffer-from-stream (stream)
  (when (typep stream 'shadow-stream)
    (setf stream (shadow-stream-primary stream)))
  (when (typep stream 'framebuffer-stream)
    (slot-value stream 'framebuffer)))

(defun %with-saved-screen (fn)
  (let ((fb (framebuffer-from-stream *terminal-io*)))
    (if fb
        (let* ((dims (array-dimensions fb))
               (position (multiple-value-list (sys.int::stream-cursor-pos *terminal-io*)))
               (back-buffer (make-array (array-dimensions fb)
                                        :element-type (array-element-type fb))))
          (%bitblt (first dims) (second dims)
                            fb 0 0
                            back-buffer 0 0)
          (unwind-protect
               (funcall fn fb)
            (apply 'stream-move-to *terminal-io* position)
            (%bitblt (first dims) (second dims)
                              back-buffer 0 0
                              fb 0 0)))
        (funcall fn nil))))
