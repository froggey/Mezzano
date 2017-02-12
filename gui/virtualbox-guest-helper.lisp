;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.gui.virtualbox-helper)

(defvar *virtualbox-helper* nil)

(defun virtualbox-helper-thread ()
  (loop
     (sys.int::log-and-ignore-errors
      (multiple-value-bind (abs-x abs-y)
          (mezzano.supervisor:virtualbox-read-event)
        (cond ((integerp abs-x)
               (mezzano.gui.compositor:submit-mouse-absolute abs-x abs-y))
              ((eql abs-x :screen-geometry-changed)
               (mezzano.supervisor:virtualbox-graphics-update-framebuffer))
              (t
               (error "Unknown VirtualBox event ~S." abs-x)))))))

(when (not *virtualbox-helper*)
  (setf *virtualbox-helper*
        (mezzano.supervisor:make-thread 'virtualbox-helper-thread
                                        :name "VirtualBox Guest Helper")))
