;;;; Additional support for VirtualBox's guest integration

(defpackage :mezzano.gui.virtualbox-helper
  (:use :cl))

(in-package :mezzano.gui.virtualbox-helper)

(defvar *virtualbox-helper* nil)

(defun virtualbox-helper-thread ()
  (loop
     (mezzano.internals::log-and-ignore-errors
      (multiple-value-bind (abs-x abs-y)
          (mezzano.supervisor.virtualbox:virtualbox-read-event)
        (cond ((integerp abs-x)
               (mezzano.gui.compositor:submit-mouse-absolute abs-x abs-y))
              ((eql abs-x :screen-geometry-changed)
               (mezzano.supervisor.virtualbox:virtualbox-graphics-update-framebuffer))
              (t
               (error "Unknown VirtualBox event ~S." abs-x)))))))

(when (not *virtualbox-helper*)
  (setf *virtualbox-helper*
        (mezzano.supervisor:make-thread 'virtualbox-helper-thread
                                        :name "VirtualBox Guest Helper")))
