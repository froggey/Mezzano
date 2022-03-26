;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2006 by 
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

(defparameter *colors* (list +black+ +white+ +red+ +green+ +blue+ +magenta+
			     +cyan+ +yellow+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *color-alist*  `(("black" . ,+black+)
				 ("white" . ,+white+)
				 ("red" . ,+red+)
				 ("green" . ,+green+)
				 ("blue" . ,+blue+)
				 ("magenta" . ,+magenta+)
				 ("cyan" . ,+cyan+)
				 ("yellow" . ,+yellow+))))

(define-presentation-type named-color ()
  :inherit-from `(completion ,*color-alist* :value-key cdr))

(defclass rect ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (color :accessor color :initarg :color))
  (:default-initargs :x 0 :y 0 :width 50 :height 50 :color +black+))

(defgeneric draw (stream thing))

(defmethod draw (stream (thing rect))
  (with-output-as-presentation (stream thing 'rect)
    (let ((x (x thing))
	  (y (y thing)))
      (draw-rectangle* stream x y (+ x (width thing)) (+ y (height thing))
		       :ink (color thing)))))


(define-application-frame drag-test ()
  ((shape1 :accessor shape1 :initform (make-instance 'rect :x 10 :y 10))
   (shape2 :accessor shape2 :initform (make-instance 'rect :x 100 :y 10)))
  (:pointer-documentation t)
  (:menu-bar nil)
  (:panes
   (interactor :interactor)
   (scribble :application :width 200 :display-function 'display-shapes))
  (:layouts
   (default
     (vertically ()
       scribble interactor))))

(defun display-shapes (frame stream)
  (draw stream (shape1 frame))
  (draw stream (shape2 frame)))

(define-drag-test-command (com-set-color :name t)
    ((shape rect) &key (color named-color :default +cyan+ ))
  (setf (color shape) color))

(define-drag-test-command (com-set-random-color :name t)
    ((shape rect))
  (let ((elt (random (length *color-alist*))))
    (setf (color shape) (cdr (nth elt *color-alist*)))))

(define-drag-and-drop-translator com-drop-color
    (rect command rect drag-test)
    (object destination-object)
  (if (eq object destination-object)
      `(com-set-random-color ,object)
      `(com-set-color ,destination-object :color ,(color object))))

#-(and)
(define-gesture-name :drag-and-drop :pointer-button (:control :left)
		     :unique t)


