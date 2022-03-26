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

;;; Model

(defclass history ()
  ((%elements :initarg  :elements
              :accessor elements
              :initform '())))

(defmethod push-element ((element t) (history history))
  (unless (eq element (first (elements history)))
    (push element (elements history))))

(defmethod pop-element ((history history))
  (pop (elements history))
  (first (elements history)))

(defmethod clear ((history history))
  (setf (elements history) '()))

;;; Presentations

(define-presentation-type element (&key (selectedp nil)))
(define-presentation-type history (&key selected))

(flet ((print-it (object stream)
         (with-print-error-handling (stream)
           (with-safe-and-terse-printing (stream)
             (let ((string (prin1-to-string (value object))))
               (print-string-compactly string stream :delimitersp nil))))))

  (define-presentation-method present ((object t)
                                       (type   element)
                                       (stream t)
                                       (view   t)
                                       &key)
    (print-it object stream))

  (define-presentation-method present ((object t)
                                       (type   element)
                                       (stream extended-output-stream)
                                       (view   t)
                                       &key)
    (with-preserved-cursor-y (stream)
      (surrounding-output-with-border (stream :shape   :rounded
                                              :radius  3
                                              :padding 3
                                              :outline-ink +gray70+
                                              :background (if selectedp +gray90+ +background-ink+))
        (with-drawing-options (stream :text-face (if selectedp :bold nil)
                                      :text-size :smaller)
          (print-it object stream))))))

(define-presentation-method present ((object t)
                                     (type   history)
                                     (stream t)
                                     (view   t)
                                     &key)
  (loop :with firstp = t
        :for element :in (reverse (elements object))
        :for selectedp = (eq element selected)
        :do (if firstp
                (setf firstp nil)
                (write-string " » " stream))
            (present element `(element :selectedp ,selectedp)
                     :stream     stream
                     :single-box t)))

;;; Pane

(defclass history-pane (application-pane)
  (;; The inspector state this history should observe and manipulate.
   (%state          :reader   state
                    :writer   (setf %state)
                    :initform nil)
   (%history        :reader   history
                    :initform (make-instance 'history))
   (%change-handler :accessor %change-handler
                    :initform nil))
  (:default-initargs
   :end-of-line-action :allow
   :state              (error "Missing required initarg :state")))

(defmethod shared-initialize :after ((instance history-pane) (slot-names t)
                                     &key (state nil state-supplied-p))
  ;; Observe the inspector state, extending or at least redisplaying
  ;; the history when the root object changes.
  (unless (%change-handler instance)
    (let* ((history (history instance))
           (handler (lambda (old-root-place new-root-place)
                      (unless (or (eq old-root-place new-root-place)
                                  (find old-root-place (elements history) :test #'eq))
                        (push-element old-root-place history)
                        (redisplay-frame-pane (pane-frame instance) instance)))))
      (setf (%change-handler instance) handler)))
  (when state-supplied-p
    (setf (%state instance) state)))

(defmethod (setf %state) :around ((new-value t) (object history-pane))
  (let ((old-value (state object)))
    (prog1
        (call-next-method)
      (unless (eq new-value old-value)
        (let ((handler (%change-handler object)))
          (when old-value
            (removef (change-hook old-value) handler))
          (push handler (change-hook new-value)))))))

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane  history-pane)
                                 &key force-p)
  (declare (ignore force-p))
  (stream-increment-cursor-position pane 8 8)
  (let* ((history      (history pane))
         (selected     (root-place (state pane)))
         (presentation (present history `(history :selected ,selected)
                                :stream pane :single-box t)))
    (when-let ((viewport (pane-viewport pane))
               (record   (find-if (lambda (record)
                                    (and (presentationp record)
                                         (eq (presentation-object record)
                                             selected)))
                                  (output-record-children presentation))))
      (scroll-extent pane (bounding-rectangle-min-x record) 0))))

;;; Commands

(define-command-table navigation-command-table)

(defun history-pane ()
  (map-over-sheets (lambda (sheet)
                     (when (typep sheet 'history-pane)
                       (return-from history-pane sheet)))
                   (frame-panes *application-frame*)))

(macrolet ((with-history ((state-var &optional history-var) &body body)
             (with-unique-names (pane)
               `(let* ((,pane (history-pane))
                       ,@(when state-var
                           `((,state-var (state ,pane))))
                       ,@(when history-var
                           `((,history-var (history ,pane)))))
                  ,@body))))

  (define-command (com-select :command-table navigation-command-table
                              :name          "Inspect object")
      ((object inspected-object :gesture :describe))
    (with-history (state)
      (setf (root-object state :run-hook-p t) (object object)))) ; TODO getting the object can fail

  (define-command (com-visit :command-table navigation-command-table
                             :name          t)
      ((object element :gesture :select))
    (with-history (state)
      (setf (root-place state :run-hook-p t) object)))

  (define-command (com-back :command-table navigation-command-table
                            :name          t
                            :keystroke     (#\l :meta))
      ()
    (with-history (state history)
      (setf (root-place state :run-hook-p t) (pop-element history))))

  (define-command (com-clear :command-table navigation-command-table
                             :name          t)
      ()
    (with-history (nil history)
      (clear history))))
