(cl:defpackage #:mcclim.text-editor-pane-test
  (:use
   #:cl
   #:clim)

  (:shadowing-import-from #:clim
   #:interactive-stream-p))

(cl:in-package #:mcclim.text-editor-pane-test)

(defclass text-editor-with-syntax-pane (text-editor-pane)
  ())

(defmethod initialize-instance :after ((instance text-editor-with-syntax-pane) &key)
  (let* ((view   (view (climi::substrate instance)))
         (buffer (drei:buffer view)))
    (setf (drei:syntax view)
          (make-instance 'drei-lisp-syntax:lisp-syntax :buffer buffer))))

(define-application-frame text-editor-pane-test ()
  ()
  (:panes
   (editor text-editor-with-syntax-pane :nlines 20)
   (drei   :drei :syntax (make-instance (drei-syntax:syntax-from-name :lisp)) :scroll-bars t))
  (:layouts
   (default
       (vertically ()
         editor
         drei))))

(let ((frame (make-application-frame 'text-editor-pane-test)))
  (run-frame-top-level frame))
