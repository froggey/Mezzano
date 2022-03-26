;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2001 by 
;;;           Tim Moore (moore@bricoworks.com)

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

(in-package :clim-demo)

(defclass echo-interactor-pane (interactor-pane)
  ())

(defvar *debug-echo* t)

(defmethod handle-event :after ((stream echo-interactor-pane)
				 (event key-press-event))
  (let* ((buffer (stream-input-buffer stream))
	 (fill (fill-pointer buffer)))
    (when (> fill 0)	;Should always be true
      (let ((gesture (aref buffer (1- fill))))
	(when (characterp gesture)
      (stream-write-char stream gesture))))))

(defmethod stream-read-gesture :around ((stream echo-interactor-pane)
				       &key &allow-other-keys)
  (let* ((results (multiple-value-list (call-next-method)))
	 (gesture (car results)))
    (when (and *debug-echo*
	       gesture)
      (print gesture *trace-output*))
    (values-list results)))

(define-application-frame stream-test ()
  ()
  (:menu-bar nil)
  (:panes
   (tester (make-clim-stream-pane :type 'echo-interactor-pane)))
  (:layouts
   (default (vertically () tester))))
