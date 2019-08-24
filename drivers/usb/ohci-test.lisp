(in-package :mezzano.driver.usb.ohci)

(defvar *test-eds*)

(defun test-ohci ()
  (setf *test-eds* nil)
  (setf *ohci* (make-instance 'ohci))
  (ohci-init-memory *ohci*)
  (init-interrupt-table *ohci*))

(defun add-ed (num-bytes interval)
  (let ((ed (get-disabled-ed *ohci*)))
    (push ed *test-eds*)
    (add-interrupt-ed *ohci* ed num-bytes interval)))

(defun add-random-ed ()
  (add-ed (random 1000) (nth (random 6) '(:1ms :2ms :4ms :8ms :16ms :32ms))))
