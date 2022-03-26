(in-package :med)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro save-excursion ((buffer) &body body)
    "Save the point & mark in buffer, execute body, then restore the saved point
and mark."
    `(call-with-save-excursion ,buffer (lambda () ,@body))))

(defun call-with-save-excursion (buffer fn)
  (let ((previous-point (copy-mark (buffer-point buffer) :right))
        (previous-mark (copy-mark (buffer-mark buffer) :left))
        (previous-mark-active (buffer-mark-active buffer)))
    (unwind-protect
         (funcall fn)
      (move-mark-to-mark (buffer-point buffer) previous-point)
      (move-mark-to-mark (buffer-mark buffer) previous-mark)
      (setf (buffer-mark-active buffer) previous-mark-active)
      (delete-mark previous-point)
      (delete-mark previous-mark))))

