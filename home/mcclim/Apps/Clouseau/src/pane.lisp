;;;; Copyright (C) 2018, 2019 Jan Moringen
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

;;; Helper for preserving scroll position

(defclass scroll-position-preserving-mixin ()
  ())

(defmethod redisplay-frame-pane
    :around ((frame application-frame)
             (pane  scroll-position-preserving-mixin)
             &key force-p)
  (declare (ignore force-p))
  (multiple-value-bind (x-displacement y-displacement)
      (transform-position (sheet-transformation pane) 0 0)
    (call-next-method)
    (when-let ((viewport (pane-viewport pane)))
      (scroll-extent pane
                     (min (- x-displacement)
                          (- (bounding-rectangle-width pane)
                             (bounding-rectangle-width viewport)))
                     (min (- y-displacement)
                          (- (bounding-rectangle-height pane)
                             (bounding-rectangle-height viewport)))))))

;;; Mixin for managing inspector presentations on a pane

(defclass inspector-pane-mixin ()
  ((%state          :reader   state
                    :writer   (setf %state)
                    :initform nil)
   (%change-handler :accessor %change-handler
                    :initform nil)))

(defmethod shared-initialize :before ((instance   inspector-pane-mixin)
                                      (slot-names t)
                                      &key (state nil state-supplied-p)
                                           (root  nil root-supplied-p))
  (declare (ignore state root))
  (when (and state-supplied-p root-supplied-p)
    (error "~@<The initargs ~S and ~S are mutually exclusive.~@:>"
           :state :root)))

(defmethod shared-initialize :after ((instance   inspector-pane-mixin)
                                     (slot-names t)
                                     &key (state nil state-supplied-p)
                                          (root  nil root-supplied-p))
  (unless (%change-handler instance)
    (setf (%change-handler instance)
          (lambda (old-root-place new-root-place)
            (declare (ignore old-root-place new-root-place))
            (queue-redisplay instance))))
  (cond (state-supplied-p
         (setf (%state instance) state))
        (root-supplied-p
         (setf (%state instance) (make-instance 'inspector-state
                                                :root-object root)))))

(defmethod initialize-instance :after ((instance inspector-pane-mixin)
                                        &key (state nil state-supplied-p)
                                             (root  nil root-supplied-p))
  (declare (ignore state root))
  (unless (or state-supplied-p root-supplied-p)
    (setf (%state instance) (make-instance 'inspector-state))))

(defmethod (setf %state) :around ((new-value t) (object inspector-pane-mixin))
  (let ((old-value (state object)))
    (prog1
        (call-next-method)
      (unless (eq new-value old-value)
        (let ((handler (%change-handler object)))
          (when old-value
            (removef (change-hook old-value) handler))
          (push handler (change-hook new-value)))))))

(defmethod root-place ((inspector-state inspector-pane-mixin)
                       &key run-hook-p)
  (declare (ignore run-hook-p))
  (root-place (state inspector-state)))

(defmethod (setf root-place) ((new-value       t)
                              (inspector-state inspector-pane-mixin)
                               &key run-hook-p)
  (setf (root-place (state inspector-state) :run-hook-p run-hook-p)
        new-value))

(defmethod root-object ((inspector-state inspector-pane-mixin)
                        &key run-hook-p)
  (declare (ignore run-hook-p))
  (root-object (state inspector-state)))

(defmethod (setf root-object) ((new-value       t)
                               (inspector-state inspector-pane-mixin)
                               &key run-hook-p)
  (setf (root-object (state inspector-state) :run-hook-p run-hook-p)
        new-value))

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane  inspector-pane-mixin)
                                 &key force-p)
  (declare (ignore force-p))
  (present-inspected-object-graph (state pane) pane)
  ;; Place the cursor on a fresh line so prompts work predictably when
  ;; the frame does not have an interactor pane and PANE is used for
  ;; command input/output.
  (fresh-line pane))

;;; Redisplay

(defclass redisplay-event (clim:window-event) ())

(defmethod queue-redisplay ((pane inspector-pane-mixin))
  (queue-event pane (make-instance 'redisplay-event :sheet pane)))

(defmethod handle-event ((client inspector-pane-mixin)
                         (event  redisplay-event))
  (redisplay-frame-pane (pane-frame client) client :force-p t))

;;; Commands

(define-command-table inspector-pane-command-table
  :inherit-from (inspector-command-table))

(defun inspector-state ()
  (map-over-sheets (lambda (sheet)
                     (when (typep sheet 'inspector-pane-mixin)
                       (return-from inspector-state sheet)))
                   (frame-panes *application-frame*)))

(define-command (com-eval-inspect :command-table inspector-pane-command-table
                                  :name          t)
    ((form clim:form))
  (let ((state (inspector-state)))
    (with-command-error-handling ("Error evaluating and inspecting")
        (let ((object (eval-with-bindings form :root-place (root-place state))))
          (setf (root-object state :run-hook-p t) object)))))

(define-command (com-eval :command-table inspector-pane-command-table
                          :name          t)
    ((form 'clim:form))
  (let ((state (inspector-state)))
    (with-command-error-handling ("Error evaluating form")
        (present (eval-with-bindings form :root-place (root-place state))
                 'clim:expression))))

;;; The actual inspector pane

(defclass inspector-pane (inspector-pane-mixin
                          scroll-position-preserving-mixin
                          application-pane)
  ())
