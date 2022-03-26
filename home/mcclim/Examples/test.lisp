;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(defun address-book ()
  (declare (special frame fm port pane medium graft))
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (setq frame (make-application-frame 'address-book))
;  (setq fm (frame-manager frame))
;  (setq port (port fm))
;  (setq pane (frame-standard-output frame))
;  (setq medium (sheet-medium pane))
;  (setq graft (graft frame))
  (run-frame-top-level frame))

(defun test-define-application-frame ()
  (macroexpand '(define-application-frame address-book ()
    ;; This application has two state variables, the currently displayed
    ;; address and the window from which user queries should be read.
    ((current-address :initform nil)
     (interaction-pane )
     (name-pane))
  (:panes
    (interactor :interactor)
    (address :application
	     :incremental-redisplay t
	     :display-function 'display-current-address)
    (names :application
	   :incremental-redisplay t
	   :display-function 'display-names))
  (:layouts
    (default
      (vertically ()
        (horizontally ()
	  address names)
	interactor))))))
