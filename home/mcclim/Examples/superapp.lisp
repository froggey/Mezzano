
(defpackage #:clim-demo.app
  (:use #:clim #:clim-lisp)
  (:export #:app-main))

(in-package #:clim-demo.app)

(define-application-frame superapp ()
  ()
  (:panes
   (int :interactor)
   (lab1 :push-button-pane :label "dummy-1")
   (lab2 :label-pane :label "dummy-2" :background +blue-violet+))
  (:layouts
   (default (vertically () #|int|#
              int
              lab1 lab2))))

(defun app-main ()
  (let ((frame (make-application-frame 'superapp)))
    (values frame
            (clim-sys:make-process
             (lambda ()
               (run-frame-top-level frame))))))
