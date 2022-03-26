;;; -*- Mode: Lisp; Package: CLIM-TRANSFORMATIONS-TEST; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2001 by 
;;;           Alexey Dejneka (adejneka@comail.ru)

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

(defpackage #:clim-demo.transformations-test
  (:use #:clim #:clim-lisp))

(in-package #:clim-demo.transformations-test)

(defparameter *transformations-test-file* #p"transformations-test.ps")


;;; --- Test 1: Painter (see SICP 2.2.4). Scaling and reflection.

;;; Protocol class
(defclass picture ()
  ())

(defgeneric draw (sheet picture)
   (:documentation "Draw PICTURE on SHEET in square (0,0)-(1,0)-(1,1)-(0,1)"))

;;; Basic picture

(defconstant +d+ 0.15)
(defconstant +eyes-y+ 0.80)
(defconstant +r-mouth+ +d+)

(defclass face (picture)
  ())

(defmethod draw (sheet (picture face))
  (draw-circle* sheet 0.5 0.5 0.45 :filled nil)
  (draw-points* sheet (list (- 0.5 +d+)
                            +eyes-y+
                            (+ 0.5 +d+)
                            +eyes-y+))
  (draw-line* sheet 0.5 +eyes-y+ 0.5 0.5)
  (draw-circle* sheet 0.5 0.5 +r-mouth+ :filled nil :start-angle (* 5/4 pi)))

;;;
(defclass vertical-picture (picture)
  ((picture1 :type picture :initarg :picture1)
   (picture2 :type picture :initarg :picture2)))

(defmethod draw (sheet (picture vertical-picture))
  (with-slots (picture1 picture2) picture
    (with-scaling (sheet 1 0.5)
      (draw sheet picture1)
      (with-translation (sheet 0 1)
        (draw sheet picture2)))))

(defclass horizontal-picture (picture)
  ((picture1 :type picture :initarg :picture1)
   (picture2 :type picture :initarg :picture2)))

(defmethod draw (sheet (picture horizontal-picture))
  (with-slots (picture1 picture2) picture
    (with-scaling (sheet 0.5 1)
      (draw sheet picture1)
      (with-translation (sheet 1 0)
        (draw sheet picture2)))))

(defun below (picture1 picture2)
  (make-instance 'vertical-picture :picture1 picture1 :picture2 picture2))

(defun beside (picture1 picture2)
  (make-instance 'horizontal-picture :picture1 picture1 :picture2 picture2))

;;; Transformers
(defclass picture-transformer (picture)
  ((original :type picture :initarg :picture :reader original)))

(defmethod draw (sheet (picture picture-transformer))
  (draw sheet (original picture)))

;;; Flipping
(defclass vertically-flipped-picture (picture-transformer)
  ())

(defmethod draw :around (sheet (picture vertically-flipped-picture))
  (with-drawing-options (sheet :transformation (make-reflection-transformation* 0 0.5 1 0.5))
    (call-next-method sheet picture)))

(defclass horizontally-flipped-picture (picture-transformer)
  ())

(defmethod draw :around (sheet (picture horizontally-flipped-picture))
  (with-drawing-options (sheet :transformation (make-reflection-transformation* 0.5 0 0.5 1))
    (call-next-method sheet picture)))

(defun flip-vert (picture)
  (make-instance 'vertically-flipped-picture :picture picture))

(defun flip-horiz (picture)
  (make-instance 'horizontally-flipped-picture :picture picture))

;;; ROTATED-PICTURE

(defclass rotated-picture (picture-transformer)
  ())

(defmethod draw :around (sheet (picture rotated-picture))
  (with-rotation (sheet (* pi 0.5) (make-point 0.5 0.5))
    (call-next-method sheet picture)))

(defun rotate (picture)
  (make-instance 'rotated-picture :picture picture))

;;; Recursive splitting

(defun right-split (picture times)
  (if (= times 0)
      picture
      (let ((smaller (right-split picture (1- times))))
        (beside picture (below smaller smaller)))))

(defun up-split (picture times)
  (if (= times 0)
      picture
    (let ((smaller (up-split picture (1- times))))
      (below picture (beside smaller smaller)))))

(defun corner-split (picture times)
  (if (= times 0)
      picture
      (let ((up (up-split picture (1- times)))
            (right (right-split picture (1- times))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split picture (1- times))))
          (beside (below picture top-left)
                  (below bottom-right corner))))))

(defun square-limit (picture n)
  (let* ((quarter (corner-split picture n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

;;; Test
(defvar *basic-picture* (make-instance 'face))
(defvar *my-picture* (square-limit *basic-picture* 4))

(defun test-painter (sheet)
  (with-scaling (sheet 300)
    (with-translation (sheet 0.5 1)
      (draw sheet *my-picture*))))


;;; --- Test 2: Rotation and slanting ---
(defun make-slanting-transformation (k)
  "Make transformation (x,y) -> (x, y+kx)"
  (make-3-point-transformation* 0 0 1 0 0 1
                                0 0 1 0 k 1))

(defmacro with-slanting ((medium k) &body body)
  `(with-drawing-options (,medium :transformation (make-slanting-transformation ,k))
     ,@body))

(defun draw-my-square (sheet)
  (draw-rectangle* sheet -0.5 -0.5 0.5 0.5 :filled nil)
  (draw-circle* sheet 0 0 0.5 :start-angle (* pi 0.5) :ink +red+))

(defconstant +slanting-full-angle+ (* pi 0.5))
(defconstant +slanting-full-slant+ 3)

(defun draw-slantings (sheet &key (nangles 10) (nslantings 10))
  (let ((da (/ +slanting-full-angle+ nangles))
        (ds (/ +slanting-full-slant+ nslantings)))
    (dotimes (iangle (1+ nangles))
      (let ((angle (* da iangle)))
        (dotimes (islant nslantings)
          (let ((slant (* ds islant)))
            (with-translation (sheet (* 1.5 iangle) (* 1.5 islant))
              (with-slanting (sheet slant)
                (with-rotation (sheet angle)
                  (draw-my-square sheet)))
              (draw-lines* sheet '(
                                   -0.5 0 0.5 0
                                   0 -0.5 0 0.5)
                           :ink +blue+))))))))

(defun test-slantings (sheet)
  (with-scaling (sheet 30)
    (with-translation (sheet 1 1)
      (draw-slantings sheet))))


;;; --- Test 3: Continuity ---
(defun draw-sectors  (sheet &key (nwidths 10) (nstarts 10))
  (let ((dsa (/ (* 2 pi) nstarts))
        (dw (/ (* 2 pi) nwidths)))
    (dotimes (iw (1+ nwidths))
      (dotimes (isa (1+ nstarts))
        (let* ((sa (* isa dsa))
               (w (* iw dw)))
          (with-translation (sheet (* 1.5 iw) (* 1.5 isa))
                            (with-rotation (sheet sa (make-point 0.5 0.5))
                                           (draw-circle* sheet 0.5 0.5 0.5
                                                         :start-angle 0
                                                         :end-angle w
                                                         :ink +blue+
                                                         :filled nil))
                            (draw-lines* sheet '(0.0 0.5 1.0 0.5
                                                     0.5 0.0 0.5 1.0)
                                         :ink +red+)))))))

(defun test-continuity (sheet)
  (with-scaling (sheet 30)
                (draw-sectors sheet)))
;;; Test

(defun transformations-test ()
  (with-open-file (file *transformations-test-file* :direction :output)
    (with-output-to-postscript-stream (stream file)
      (with-output-recording-options (stream :record nil)
        (test-painter stream)
        (new-page stream)

        (test-slantings stream)
        (new-page stream)

        (test-continuity stream)
        (new-page stream)

        (with-drawing-options (stream :transformation
                                      (make-reflection-transformation* 0 500 500 0))
          (test-continuity stream))))))
