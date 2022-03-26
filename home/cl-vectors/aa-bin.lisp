;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

;;;; See http://projects.tuxee.net/cl-vectors/

;;;; The name 'cl-aa-bin' is derived from 'cl-aa' which is the library
;;;; used to rasterize antialiased polygons. The '-bin' version
;;;; doesn't perform antialiasing (the alpha value is always a
;;;; multiple of 256), but support the same protocol (drop-in
;;;; replacement) hence the choice of the name.

;;;; The aa-bin algorithm is faster and more accurate than when using
;;;; the original 'cl-aa' algorithm as a non-antialiasing rasterizer.

;;;; The algorithm compute all the pixels whose "center" (assuming a
;;;; "pixel is a little square"..) are inside the polygon to
;;;; rasterize.

(defpackage #:net.tuxee.aa-bin
  (:use #:cl)
  (:nicknames #:aa-bin)
  (:export #:make-state
           #:line
           #:line-f
           #:cells-sweep))

(in-package #:net.tuxee.aa-bin)

(defconstant +cell-width+ 256
  "A cell represent a pixel square, and the width is the
fractional part of the fixed-point coordinate. A large value
increase precision. 256 should be enough though. Note that
smaller value should NOT increase performance.")

(defconstant +alpha-range+ 256
  "For non overlapping polygons, the alpha value will be in the
range (-limit,limit) where limit is +alpha-range+. The value is
negative or positive accordingly to the polygon
orientation (clockwise or counter-clockwise.)")

(defun map-line-intersections (function x1 y1 x2 y2)
  (declare (optimize speed (safety 0) (debug 0)))
  (when (/= y1 y2)
    (when (> y1 y2)
      (rotatef y1 y2)
      (rotatef x1 x2))
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      ;; FIXME: optimize the loop with the usual Bresenham integer
      ;; algorithm
      (loop for n from (* +cell-width+ (ceiling y1 +cell-width+))
                  upto (* +cell-width+ (floor (1- y2) +cell-width+))
                  by +cell-width+
         do (funcall function
                     (+ x1 (floor (* dx (- n y1)) dy))
                     n)))))

(defstruct cell
  x y (value 0))

(defstruct state
  "AA state. Hold all the cells generated when drawing lines."
  (cells nil))

(defun state-reset (state)
  "Reset the state, losing all accumulated cells. It can be
faster or less memory consuming to reset a state and reuse it,
rather than creating a new state."
  (setf (state-cells state) nil))

(declaim (inline set-current-cell))
(defun set-current-cell (state x y)
  (let ((cells (state-cells state)))
    (if (and cells
             (= (cell-x (first cells)) x)
             (= (cell-y (first cells)) y))
        (first cells)
        (let ((cell (make-cell :x x :y y)))
          (push cell (state-cells state))
          cell))))

(defun line (state x1 y1 x2 y2)
  (when (/= y1 y2)
    (map-line-intersections (lambda (x y)
                              (let ((x-m (ceiling x +cell-width+))
                                    (y-m (floor y +cell-width+)))
                                (incf (cell-value (set-current-cell state x-m y-m))
                                      (if (< y1 y2) 1 -1))))
                            (- x1 (floor +cell-width+ 2))
                            (- y1 (floor +cell-width+ 2))
                            (- x2 (floor +cell-width+ 2))
                            (- y2 (floor +cell-width+ 2)))))

(defun line-f (state x1 y1 x2 y2)
  "Draw a line, whose coordinates are translated to fixed-point
as expected by function LINE. This is a convenient function to
not depend on +CELL-WIDTH+."
  (labels ((float-to-fixed (n)
             (values (round (* +cell-width+ n)))))
    (line state
          (float-to-fixed x1) (float-to-fixed y1)
          (float-to-fixed x2) (float-to-fixed y2))))

(declaim (inline compare-cells))
(defun compare-cells (a b)
  "Compare coordinates between 2 cells. Used to sort cells by Y,
then by X."
  (or (< (cell-y a) (cell-y b))
      (and (= (cell-y a) (cell-y b))
           (< (cell-x a) (cell-x b)))))

(defun cells-sweep (state function &optional span-function)
  "Call FUNCTION for each pixel on the polygon path described by
previous call to LINE or LINE-F. The pixels are scanned in
increasing Y, then on increasing X. For optimization purpose, the
optional FUNCTION-SPAN, if provided, is called for a full span of
identical alpha pixel. If not provided, a call is made to
FUNCTION for each pixel in the span."
  (setf (state-cells state) (sort (state-cells state) #'compare-cells))
  (let (x y value)
    (flet ((call ()
             (unless (zerop value)
               (funcall function x y (* +alpha-range+ value)))))
      (dolist (cell (state-cells state))
        (cond
          ((null value)
           ;; first cell
           (setf x (cell-x cell)
                 y (cell-y cell)
                 value (cell-value cell)))
          ((/= (cell-y cell) y)
           ;; different y
           (call)
           (setf x (cell-x cell)
                 y (cell-y cell)
                 value (cell-value cell)))
          ((/= (cell-x cell) x)
           ;; same y, different x
           (call)
           (unless (zerop value)
             (let ((scaled-value (* +alpha-range+ value)))
               (if (and (> (- (cell-x cell) x) 1)
                        span-function)
                   (funcall span-function (1+ x) (cell-x cell) y scaled-value)
                   (loop for ix from (1+ x) below (cell-x cell)
                      do (funcall function ix y scaled-value)))))
           (setf x (cell-x cell))
           (incf value (cell-value cell)))
          (t
           ;; same cell, accumulate
           (incf value (cell-value cell)))))
      (when value
        (call)))))
