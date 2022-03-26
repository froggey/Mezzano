;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)
;;;  (c) 2018 Jan Moringen (jmoringe@techfak.uni-bielefeld.de)

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

(cl:in-package #:clim-demo)

;;; State

(defclass state ()
  ((%text        :initarg  :text
                 :accessor text)
   (%text-family :initarg  :text-family
                 :accessor text-family)
   (%text-face   :initarg  :text-face
                 :accessor text-face)
   (%text-size   :initarg  :text-size
                 :accessor text-size*)
   (%rectangle   :initarg  :rectangle
                 :type     (member :text-size :text-bounding-rectangle)
                 :accessor rectangle)
   (%hook        :initarg  :hook
                 :accessor hook
                 :initform nil)))

(defmethod maybe-run-hook ((state state))
  (alexandria:when-let ((hook (hook state)))
    (funcall hook state)))

(defmethod (setf text) :after (new-value (state state))
  (maybe-run-hook state))

(defmethod (setf text-family) :after (new-value (state state))
  (maybe-run-hook state))

(defmethod (setf text-face) :after (new-value (state state))
  (maybe-run-hook state))

(defmethod (setf text-size*) :after (new-value (state state))
  (maybe-run-hook state))

(defmethod (setf rectangle) :after (new-value (state state))
  (maybe-run-hook state))

(defmethod text-style ((state state))
  (make-text-style (text-family state) (text-face state) (text-size* state)))

;;; Canvas

(defclass canvas (basic-pane)
  ((%state :initarg :state
           :reader  state))
  (:default-initargs
   :background +white+))

(defmethod initialize-instance :after ((instance canvas) &key state)
  (setf (hook state) (lambda (state)
                       (declare (ignore state))
                       (repaint-sheet instance (sheet-region instance)))))

(defmethod resize-sheet :after ((sheet canvas) width height)
  (repaint-sheet sheet (sheet-region sheet)))

(defmethod handle-repaint ((sheet canvas) region)
  (draw-text-size-info sheet (state sheet)))

(defun draw-text-size-info (stream state)
  (let* ((medium (sheet-medium stream))
         (region (let ((region (sheet-region stream)))
                   (if (not (region-equal region +everywhere+))
                       region
                       (make-rectangle* 0 0 800 600))))
         (pane-width (rectangle-width region))
         (pane-height (rectangle-height region))

         (text      (text state))
         (style     (text-style state))
         (rectangle (rectangle state)))
    (draw-design stream region :ink (clime:background stream))
    (multiple-value-bind (width height final-x final-y baseline)
        (text-size stream text :text-style style)
      (let* ((x1 (/ (- pane-width width) 2))
             (y1 (/ (- pane-height height) 2))
             (ybase (+ y1 baseline))

             (inks (coerce (make-contrasting-inks 8) 'list))
             (legend-entries '())
             (legend-text-style (make-text-style :sans-serif :roman :small)))
        (labels ((draw-vdist (stream x y1 y2)
                   (draw-line* stream (- x 10) y1 (+ x 10) y1)
                   (draw-line* stream (- x 10) y2 (+ x 10) y2)
                   (draw-arrow* stream x y1 x y2))
                 (draw-hdist (stream y x1 x2)
                   (draw-line* stream x1 (- y 10) x1 (+ y 10))
                   (draw-line* stream x2 (- y 10) x2 (+ y 10))
                   (draw-arrow* stream x1 y x2 y))
                 (component (title thunk &rest args &key &allow-other-keys)
                   (let* ((ink             (pop inks))
                          (drawing-options (list* :ink ink args)))
                     (push (cons title drawing-options) legend-entries)
                     (apply #'invoke-with-drawing-options
                            stream thunk drawing-options))))
          (draw-text* stream
                      (format nil "fixed-width-p: ~(~A~)"
                              (handler-case
                                  (text-style-fixed-width-p style medium)
                                (error (c)
                                  c)))
                      2 pane-height :text-style legend-text-style)
          (component "Ascent"
                     (lambda (stream)
                       (let ((ascent (text-style-ascent style medium)))
                         (draw-vdist stream (- x1 20) ybase (- ybase ascent)))))
          (component "Descend"
                     (lambda (stream)
                       (let ((descent (text-style-descent style medium)))
                         (draw-vdist stream (- x1 20) ybase (+ ybase descent)))))
          (component "Height"
                     (lambda (stream)
                       (let ((height (text-style-height style medium)))
                         (draw-vdist stream (- x1 40) y1 (+ y1 height))))
                     :line-style (make-line-style :thickness 2))
          (component "Average character width"
                     (lambda (stream)
                       (let ((width (text-style-width style medium)))
                         (draw-hdist stream (- y1 20) x1 (+ x1 width)))))
          (component "Baseline"
                     (lambda (stream)
                       (draw-line* stream 0 ybase pane-width ybase)))
          (draw-text* stream text x1 ybase :text-style style)
          (ecase rectangle
            ((:text-size)
             (component "Text size (width/height)"
                        (lambda (stream)
                          (draw-rectangle*
                           stream x1 y1 (+ x1 width) (+ y1 height)
                           :filled nil)))
             (component "Text size (final x/y)"
                        (lambda (stream)
                          (draw-line*
                           stream 0 (+ y1 final-y) pane-width (+ y1 final-y))
                          (draw-line*
                           stream (+ x1 final-x) 0 (+ x1 final-x) pane-height))))
            ((:text-bounding-rectangle)
             (multiple-value-bind (left top right bottom)
                 (climi::text-bounding-rectangle* medium text :text-style style)
               (component "Bounding rectangle"
                          (lambda (stream)
                            (draw-rectangle* stream
                                             (+ x1 left) (+ y1 baseline top)
                                             (+ x1 right) (+ y1 baseline bottom)
                                             :filled nil)))))))

        ;; Draw a legend for all drawn components
        (loop with line-height = (nth-value 1 (text-size
                                               stream "dummy"
                                               :text-style legend-text-style))
              for (text . drawing-options) in (reverse legend-entries)
              for y from (+ 2 line-height) by line-height
              for y* = (+ 0.5 (round (- y (/ line-height 2))))
              do (apply #'draw-line* stream 2 y* 35 y* drawing-options)
                 (draw-text* stream text 40 y :text-style legend-text-style))))))
