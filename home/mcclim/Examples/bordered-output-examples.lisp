;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-DEMO; -*-

;;; Test surrounding-output-with-border with various shapes and keywords.

;;;  (c) Copyright 2007 by Andy Hefner (ahefner@gmail.com)

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

(define-presentation-type border-style ())

(define-application-frame bordered-output ()
  ((shapes :initform (append (reverse climi::*border-types*)
			     (list
			      (list :rectangle
                                    :ink +gray80+
                                    :padding 0
                                    :padding-left 24
                                    :line-thickness 4)
                              (list :rectangle
                                    :ink +gray50+
                                    :line-dashes t)
			      (list :oval
                                    :ink +white+
                                    :highlight-background +white+
                                    :line-thickness 3)
                              (list :oval
                                    :line-dashes t)
                              (list :oval
                                    :ink nil
                                    :background +white+)
			      (list :underline :ink +red+ :line-thickness 2)
                              (list :underline :ink +red+ :line-dashes t)
                              (list :crossout :ink +red+ :line-thickness 2)
                              (list :crossout :ink +red+ :line-dashes t)
                              (list :rectangle
                                    :ink +gray50+
                                    :background +white+
                                    :filled t)
			      (list :oval
                                    :ink +gray60+
                                    :background +gray85+
                                    :filled t)
                              (list :oval
                                    :ink (make-ihs-color 0.8 1.0 0.5)
                                    :line-thickness 3
                                    :background (make-ihs-color 1.0 0.0 0.5)
                                    ;; FIXME, breaks on ovals. =(
                                    ;:highlight-background (make-ihs-color 1.5 0.0 0.5)
                                    :shadow-offset 8
                                    :shadow +gray80+
                                    :filled t)
                              (list :drop-shadow
                                    :ink +black+
                                    :padding 10
                                    :padding-left 20
                                    :background +gray70+
                                    :shadow +gray80+
                                    :shadow-offset 8
                                    :filled t)

                              (list :rectangle :shadow +grey80+ :background +white+)
                              (list :rounded
                                    :padding-x 27
                                    :padding-top 17
                                    :padding-bottom 27)
                              (list :rounded
                                    :line-dashes t)
                              (list :rounded
                                    :padding 13
                                    :line-thickness 2
                                    :ink +gray70+)
                              (list :rounded
                                    :padding 13
                                    :line-thickness 2
                                    :shadow +gray80+
                                    :background +white+
                                    :ink +red+)
                              (list :ellipse
                                    :line-dashes t
                                    :circle t)
                              (list :ellipse
                                    :line-thickness 2
                                    :outline-ink +red+
                                    :background +white+)
                              (list :ellipse
                                    :shadow +gray80+
                                    :outline-ink +gray60+
                                    :background +white+)
                              
                              ;; These are just my tests that the literal corner cases of draw-rounded-rectangle*
                              ;; work correctly.
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-top 0 :outline-ink +red+
                                    :background +white+ :shadow +gray80+)
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-left 0 :outline-ink +red+
                                    :background +white+ :shadow +gray80+)
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-right 0 :outline-ink +red+
                                    :background +white+ :shadow +gray80+)
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-bottom 0 :outline-ink +red+
                                    :background +white+ :shadow +gray80+)
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-y 0 :outline-ink +red+
                                    :background +white+ :shadow +gray80+)
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-x 0 :outline-ink +red+
                                    :background +white+ :shadow +gray80+)
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-right 0 :radius-top 0
                                    :outline-ink +red+ :background +white+
                                    :shadow +gray80+)
                              (list :rounded :highlight-background +yellow+
                                    :radius 27 :radius-bottom 0 :radius-left 0
                                    :outline-ink +red+ :background +white+
                                    :shadow +gray80+)))
	   :reader shapes-of))
  (:menu-bar nil)
  (:pane
   (scrolling (:width 600 :height 700)
     (make-pane :application-pane
		:end-of-line-action :allow
                :end-of-page-action :allow ; Why isn't this working?
                :background +gray90+
                :name :border-examples
		:display-function
		(lambda (frame stream)
                  (format-items (shapes-of frame)
				:stream stream
                                :presentation-type 'border-style
                                :cell-align-x :center
                                :cell-align-y :center
                                :y-spacing 16
                                :x-spacing 16
				:printer
				(lambda (shape stream)
				  (let ((shape-name-style (make-text-style :sans-serif :bold  :normal))
					(keywords-style   (make-text-style :sans-serif :roman :small)))
				    (flet ((show (stream)
					     (with-text-style (stream shape-name-style)
					       (if (listp shape)
						   (progn 
						     (format stream "~A" (first shape))
						     (with-text-style (stream keywords-style)
						       (format stream "~{~%  ~W ~W~}" (rest shape))))
						   (princ shape stream)))))
				      (if (listp shape)
					  (apply #'climi::invoke-surrounding-output-with-border
						 stream #'show (cons :shape shape))
					  (surrounding-output-with-border (stream :shape shape)
					    (show stream)))))))
                  (terpri stream))))))

;;; Define a dummy command, just to get highlighting of the border styles. 
(define-bordered-output-command (com-do-nothing)
    ((style border-style :gesture :select))
  (declare (ignore style))
  #+NIL (clouseau:inspector (stream-output-history *standard-output*)))
