;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2001 by
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2009, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; How to use the possibilities of the traffic-lights, you have two
;;; possibilites :
;;; 1 - Click on a toggle-button : the color of the light-pane
;;;     will change
;;; 2 - Click on the yellow or green toggle-button, then move your
;;;     mouse-pointer on the light-pane, and wait a few seconds.

(in-package :clim-demo)

;;; example gadget definition
(defclass light-pane (basic-gadget)
  ((light-color :initform +green+ :accessor light-color :initarg :light-color)))

(defmethod handle-repaint ((pane light-pane) region)
  (declare (ignore region))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (draw-rectangle* pane x1 y1 x2 y2 :ink (light-color pane) :filled t)))

;;; callback functions

(defun simulate-action (toggle-button)
  (setf (gadget-value toggle-button :invoke-callback t)
	(not (gadget-value toggle-button))))

(defmethod handle-event :after
    ((pane light-pane) (event pointer-event))
  (declare (ignorable event))
  (let ((label (gadget-label (radio-box-current-selection
                              (slot-value *application-frame* 'radio-box)))))
    (cond ((string= label "Y")
           (traffic-pause 2)
           (simulate-action (find-pane-named *application-frame* 'red)))
	  ((string= label "G")
           (traffic-pause 3)
           (simulate-action (find-pane-named *application-frame* 'yellow)))
	  (t nil))))

(defun traffic-pause (time)
  (let ((time-left-window (find-pane-named *application-frame* 'time-left)))
    (flet ((show-time (left)
             (setf (gadget-value time-left-window)
                   (format nil "~D" left))))
      (loop for left from time downto 1
         do (show-time left)
            (sleep 1))
      (show-time 0))))

(defun repaint-all-sheets ()
  (map-over-sheets (lambda (sheet) (repaint-sheet sheet +everywhere+))
                   (frame-top-level-sheet *application-frame*)))

(defun callback-red (gadget value)
  (declare (ignore gadget))
  (when value
    (let ((pane (find-pane-named *application-frame* 'light)))
      (setf (light-color pane) +red+)))
  (repaint-all-sheets))

(defun callback-yellow (gadget value)
  (declare (ignore gadget))
  (when value
      (let ((pane (find-pane-named *application-frame* 'light)))
        (setf (light-color pane) +gold1+)))
  (repaint-all-sheets))

(defun callback-green (gadget value)
  (declare (ignore gadget))
  (when value
      (let ((pane (find-pane-named *application-frame* 'light)))
        (setf (light-color pane) +green+)))
  (repaint-all-sheets))

(defun callback-time-left (gadget value)
  (declare (ignore value))
  (repaint-sheet gadget +everywhere+))

;;; test functions

(defun traffic-lights ()
  (run-frame-top-level (make-application-frame 'traffic-lights)))

(defmethod traffic-lights-frame-top-level ((frame application-frame))
  (setf (slot-value frame 'light) (find-pane-named frame 'light)
        (slot-value frame 'radio-box) (find-pane-named frame 'radio-box))
  (clim-extensions:simple-event-loop))

(defmacro make-color-chooser-toggle-button (name label callback)
  `(make-pane 'toggle-button
              :name ,name
              :label ,label
              :indicator-type :one-of
              :width 30
              :height 30
              :value-changed-callback ,callback))

(define-application-frame traffic-lights ()
  ((radio-box :initform nil)
   (light :initform nil))
  (:menu-bar nil)
  (:panes
   (light light-pane
	  :width 30)
   (radio-box (with-radio-box ()
                (radio-box-current-selection
                 (make-color-chooser-toggle-button 'red "R" 'callback-red))
                (make-color-chooser-toggle-button 'yellow "Y" 'callback-yellow)
                (make-color-chooser-toggle-button 'green "G" 'callback-green)))
   (time-left text-field
              :editable-p nil
              :value "0"
              :value-changed-callback 'callback-time-left))
  (:layouts
   (default (horizontally () (vertically (:spacing 10) radio-box time-left) light)))
  (:top-level (traffic-lights-frame-top-level . nil)))
