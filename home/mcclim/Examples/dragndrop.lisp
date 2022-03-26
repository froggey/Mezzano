;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2004 by 
;;;           Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2017 by
;;;           Nisar Ahmad (nisarahmad1324@gmail.com)

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

(define-application-frame dragndrop ()
  ()
  (:menu-bar nil)
  (:pointer-documentation t)
  (:panes
   (interactor :interactor)
   (scratchpad :application :display-time nil :height 600 :scroll-bars nil))
  (:layouts
   (default
    (vertically ()
      (scrolling (:height 300)
	scratchpad)
      interactor))))

(defclass circle ()
  ((center :accessor center :initarg :center)
   (radius :accessor radius :initarg :radius))
  (:default-initargs :radius 50 :center nil))

(defun get-pointer-position (pane)
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

(define-dragndrop-command (com-add-circle)
    ((center point :prompt "point")
     (radius real :prompt "radius"))
  (let ((pane (get-frame-pane *application-frame* 'scratchpad)))
    (with-output-as-presentation
	(pane (make-instance 'circle :center center
				     :radius radius)
	      'circle)
      (draw-circle pane center radius))))

(define-dragndrop-command (com-quit-dragndrop :name "Quit")
    ()
  (frame-exit *application-frame*))

(define-presentation-to-command-translator translator-draw-circle
    (blank-area com-add-circle dragndrop
                :documentation "Add a circle"
		:tester
		((object)
		 (let ((frame *application-frame*))
		   (eq (pointer-sheet (port-pointer (port frame)))
		       (get-frame-pane frame 'scratchpad)))))
    (object)
  (list (get-pointer-position
	    (get-frame-pane *application-frame* 'scratchpad))
	50))

(define-dragndrop-command (com-clone-circle)
    ((original circle))
  (let ((pane (get-frame-pane *application-frame* 'scratchpad)))
    (multiple-value-bind (x y)
	(dragging-output (pane :finish-on-release t)
	  (draw-circle pane (get-pointer-position pane)
		       (radius original)
		       :filled nil))
      (com-add-circle (make-point x y) (radius original)))))

(define-presentation-to-command-translator translator-clone-circle
    (circle com-clone-circle dragndrop)
    (object)
  (list object))

(defun drag-circles ()
  (run-frame-top-level (make-application-frame 'dragndrop)))
