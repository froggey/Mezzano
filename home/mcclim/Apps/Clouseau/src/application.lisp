;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; Utilities

(defun inspector-name (object &key (length-limit 128))
  (let* ((string (with-output-to-string (stream)
                   (write-string "Inspector: " stream)
                   (handler-case
                       (with-safe-and-terse-printing (stream)
                         (prin1 object stream))
                     (error ()
                       (write-string "<error printing object>" stream)))))
         (length (length string))
         (end    (min length
                      (or (position #\Newline string) most-positive-fixnum)
                      (- length-limit 3))))
    (if (< end length)
        (concatenate 'string (subseq string 0 end) "...")
        string)))

;;; Application frame

(define-application-frame inspector ()
  ((%state          :accessor %state
                    :initform nil)
   (%change-handler :accessor %change-handler
                    :initform nil))
  (:panes
   (history    history-pane   :state (%state *application-frame*))
   (inspector  inspector-pane :state (%state *application-frame*))
   (interactor :interactor    :min-height 100))
  (:layouts
   (with-interactor
    (vertically ()
      (scrolling (:scroll-bars :horizontal :suggested-height 48)
        history)
      (:fill (scrolling (:width 920 :height 240) inspector))
      (make-pane 'clime:box-adjuster-gadget)
      (1/16 interactor :height 200)))
   (without-interactor
    (vertically ()
      (scrolling (:scroll-bars :horizontal :suggested-height 48)
        history)
      (:fill (scrolling (:width 920 :height 480) inspector)))))
  (:command-table (application :inherit-from (inspector-pane-command-table
                                              navigation-command-table)))
  (:command-definer nil)
  (:menu-bar nil)
  (:pointer-documentation t)
  (:default-initargs
   :object (error "~@<Missing required initarg ~S.~@:>" :object)))

(defmethod initialize-instance :after ((instance inspector)
                                       &key object
                                            (handle-errors t))
  (setf (%change-handler instance)
        (lambda (old-root-place new-root-place)
          (declare (ignore old-root-place))
          (setf (clim:frame-pretty-name instance)
                (inspector-name (value new-root-place))))
        (%state instance) (make-instance 'inspector-state
                                         :root-object   object
                                         :handle-errors handle-errors)))

(defmethod (setf %state) :around ((new-value t) (object inspector))
  (let ((old-value (%state object)))
    (prog1
        (call-next-method)
      (unless (eq new-value old-value)
        (let ((handler (%change-handler object)))
          (when old-value
            (removef (change-hook old-value) handler))
          (push handler (change-hook new-value)))))))

(defmethod root-place ((inspector-state inspector) &key run-hook-p)
  (declare (ignore run-hook-p))
  (root-place (%state inspector-state)))

(defmethod (setf root-place) ((new-value t) (inspector-state inspector)
                              &key run-hook-p)
  (setf (root-place (%state inspector-state) :run-hook-p run-hook-p)
        new-value))

(defmethod root-object ((inspector-state inspector) &key run-hook-p)
  (declare (ignore run-hook-p))
  (root-object (%state inspector-state)))

(defmethod (setf root-object) ((new-value t) (inspector-state inspector)
                              &key run-hook-p)
  (setf (root-object (%state inspector-state) :run-hook-p run-hook-p)
        new-value))

(defmethod frame-standard-output ((frame inspector))
  (or (find-pane-named frame 'interactor)
      (find-pane-named frame 'inspector)))

;;; Additional commands

(define-command (com-toggle-interactor :command-table application
                                       :name          t
                                       :keystroke     (#\i :control))
    ()
  (let ((frame *application-frame*))
    (setf (frame-current-layout frame)
          (case (frame-current-layout frame)
            (with-interactor    'without-interactor)
            (without-interactor 'with-interactor)))))

(define-command (com-quit :command-table application :name t)
    ()
  (frame-exit *application-frame*))

;;; Interface

(defun inspect (object &key (new-process nil) (handle-errors t))
  "Inspect OBJECT in a new inspector window.

Return two values: 1) OBJECT 2) the created inspector application
frame.

If NEW-PROCESS is false (the default), this function returns to the
caller after the inspector window has been closed. If NEW-PROCESS is
true, this function returns to the caller immediately and the
inspector frame executes in a separate thread.

HANDLE-ERRORS controls whether errors signaled when printing and
inspecting objects should be handled. HANDLE-ERRORS must be a valid
type specifier (T and NIL are legal values). Signaled errors of the
specified type will be handled by printing an error messages in place
of the inspected object. Other errors will not be handled and might
invoke the debugger in the usual way."
  (when (and (not new-process) (typep *application-frame* 'inspector))
    (restart-case
        (error "~@<Clouseau called from inside Clouseau, possibly ~
                infinite recursion~@:>")
      (continue ()
        :report "Continue by starting a new Clouseau instance")
      (abort-clouseau ()
        :report "Abort this call to Clouseau"
        (return-from inspect))))

  (let* ((name  (inspector-name object))
         (frame (make-application-frame 'inspector
                                        :object        object
                                        :handle-errors handle-errors
                                        :pretty-name   name)))
    (labels ((run ()
               (run-frame-top-level frame))
             (run-with-defaults (default-server-path)
               (let ((*default-server-path* default-server-path))
                 (run))))
      (if new-process
          ;; Install the current value *DEFAULT-SERVER-PATH* in the
          ;; new process so that the new inspector will usually use
          ;; the same port as the caller.
          (clim-sys:make-process
           (curry #'run-with-defaults *default-server-path*)
           :name name)
          (run))
      (values object frame))))
