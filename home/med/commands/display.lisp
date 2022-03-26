(in-package :med)

(defun recenter-command ()
  (recenter (current-buffer *editor*)))

(defun redraw-screen-command ()
  (redraw-screen))

(defun scroll-up-command ()
  ;; Find the display line at the bottom of the screen and recenter on that.
  (let ((current-screen (editor-current-screen *editor*))
        (point (buffer-point (current-buffer *editor*))))
    (dotimes (i (length current-screen))
      (let ((line (aref current-screen (- (length current-screen) i 1))))
        (when line
          (setf (mark-line point) (display-line-line line)
                (mark-charpos point) (display-line-start line))
          (recenter (current-buffer *editor*))
          (return))))))

(defun scroll-down-command ()
  ;; Recenter on the topmost display line.
  (let* ((current-screen (editor-current-screen *editor*))
         (line (aref current-screen 0))
         (point (buffer-point (current-buffer *editor*))))
    (setf (mark-line point) (display-line-line line)
          (mark-charpos point) (display-line-start line))
    (recenter (current-buffer *editor*))))

