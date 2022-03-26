;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

;;;; This file implement the AA algorithm from the AntiGrain project
;;;; (http://antigrain.com/).
;;;;
;;;; Changelogs:
;;;;
;;;; 2007-03-11: Extended the protocol to provide a way to sweep only
;;;;             a rectangular zone of the resulting state. This was
;;;;             done with some new functions: FREEZE-STATE,
;;;;             SCANLINE-SWEEP, SCANLINE-Y and CELLS-SWEEP/RECTANGLE.
;;;;             The function CELLS-SWEEP is now based on them.
;;;;
;;;; 2007-02-25: Released under LLGPL this time. Future changes made
;;;;             in this file will be thus covered by this license.
;;;;
;;;; 2007-01-20: Minors updates to comments and code.
;;;;
;;;; 2007-01-11: I chose to release the code in this file in public
;;;;             domain. You can do whatever you want with the code.
;;;;
;;;; 2007-01-07: fixed 2 bugs related to cells reuse. The first bug
;;;;             was that the cell after the last reused one was kept
;;;;             in the list. The second bug occured when the latest
;;;;             cell (the current one) was empty. The code was
;;;;             failing to correctly eliminate unused cells in such
;;;;             case.
;;;;
;;;; 2007-01-05: +cell-width+ is no longer passed as parameter to let
;;;;             the CL compiler optimize various computation
;;;;             involving this value. Added docstrings and
;;;;             (hopefully) clarified some points.
;;;;
;;;; 2006-12-31: moved examples to a separate file
;;;;
;;;; 2006-12-30: added animated GIF (using Skippy) example
;;;;
;;;; 2006-12-30: cleaned the code, factorized, simplified
;;;;
;;;; 2006-12-30: map-grid-spans rewritten in term of map-line-spans
;;;;
;;;; 2006-12-30: first release
;;;;
;;;; About AntiGrain: "Anti-Grain Geometry (AGG) is an Open Source,
;;;; free of charge graphic library, written in industrially standard
;;;; C++." "A High Quality Rendering Engine for C++". Its main author
;;;; is Maxim Shemanarev. Project home page is at http://antigrain.com/
;;;;
;;;; How to use it:
;;;;
;;;; 1) create a state with MAKE-STATE, or reuse a previous state by
;;;;    calling STATE-RESET on it.
;;;;
;;;; 2) call LINE-F (or LINE) to draw each line of one or several
;;;;    closed polygons. It is very important to close them to get a
;;;;    coherent result. Note that nothing is really drawn at this
;;;;    stage (not until the call to CELLS-SWEEP.)
;;;;
;;;; 3) finally, call CELLS-SWEEP to let it call your own function for
;;;;    each pixels covered by the polygon(s), where the callback
;;;;    function take 3 arguments: x, y, alpha. Pixels are scanned on
;;;;    increasing y, then on increasing x. Optionnaly, CELLS-SWEEP
;;;;    can take another callback function as parameter. See its
;;;;    documentation for details.
;;;;
;;;;    The alpha value passed to the callback function can be used in
;;;;    various way. Usually you want:
;;;;
;;;;      (defun normalize-alpha (alpha)
;;;;        (min 255 (abs alpha)))
;;;;
;;;;    to get a normalized alpha value between 0 and 255. But you may
;;;;    also be interested by:
;;;;
;;;;      (defun even-odd-alpha (alpha)
;;;;        (let ((value (mod alpha 512)))
;;;;          (min 255 (if (< value 256) value (- 512 value)))))
;;;;
;;;;    to simulate "even/odd" fill. You can also use the alpha value
;;;;    to render polygons without anti-aliasing by using:
;;;;
;;;;      (defun bool-alpha (value)
;;;;        (if (>= (abs value) 128) 255 0))
;;;;
;;;;    or, for "even/odd" fill:
;;;;
;;;;      (defun bool-even-odd-alpha (value)
;;;;        (if (<= 128 (mod (abs value) 256) 384) 255 0))
;;;;
;;;; Note: Drawing direction (clockwise or counter-clockwise) is only
;;;; important if polygons overlap during a single
;;;; cells-state. Opposite directions produce hole at the intersection
;;;; (coverage is canceled), while identical directions does not
;;;; (coverage overflow.)
;;;;
;;;; The latest version can be downloaded from:
;;;;
;;;;     http://tuxee.net/cl-aa.lisp
;;;;     http://tuxee.net/cl-aa-sample.lisp
;;;;
;;;; See also:
;;;;
;;;;     http://projects.tuxee.net/cl-aa-path/
;;;;
;;;; See examples of output at:
;;;;
;;;;     http://tuxee.net/cl-aa-1.png
;;;;     http://tuxee.net/cl-aa-2.png (this one was a bug.)
;;;;     http://tuxee.net/cl-aa-3.png
;;;;     http://tuxee.net/cl-aa-4.png
;;;;     http://tuxee.net/cl-aa-5.png (when testing transparency, but looks bad.)
;;;;     http://tuxee.net/cl-aa-6.png
;;;;     http://tuxee.net/cl-aa-7.png
;;;;     http://tuxee.net/cl-aa-8.png
;;;;     http://tuxee.net/cl-aa-stroke-0.png (using stroke functions not provided here.)
;;;;     http://tuxee.net/cl-aa-stroke-1.png
;;;;     http://tuxee.net/cl-aa-stroke-2.png
;;;;     http://tuxee.net/cl-aa-skippy-1.gif (animated GIF, thanks to Skippy library)
;;;;     http://tuxee.net/cl-aa-skippy-2.gif
;;;;
;;;; The code is absolutely NOT optimized in any way. It was mainly to
;;;; figure how the algorithm was working. Also, I don't have tested
;;;; many corner cases. It is absolutely NOT for production use.
;;;;
;;;; About the example, note that the resulting image is exported as a
;;;; PNM file. Not great, but no need for any external lib. You can
;;;; use pnmtopng to convert it to PNG afterward.
;;;;
;;;; Inspiration come from agg/include/agg_rasterizer_cells_aa.h and
;;;; agg/include/agg_rasterizer_scanline_aa.h sources files from the
;;;; AntiGrain project (version 2.5 at this date.)
;;;;
;;;; For animated GIF, see Zach Beane's Skippy project at:
;;;;     http://www.cliki.net/Skippy

;;;; Naming convention:
;;;; foo-m for fixed-point mantissa,
;;;; foo-f for fixed-point fractional part.

#+nil(error "This file assume that #+NIL is never defined.")

(defpackage #:net.tuxee.aa
  (:use #:common-lisp)
  (:nicknames #:aa)
  (:export #:make-state
           #:state-reset
           #:line
           #:line-f
           #:freeze-state
           #:scanline-y
           #:scanline-sweep
           #:cells-sweep
           #:cells-sweep/rectangle))

(in-package #:net.tuxee.aa)

;;;--[ Utility function ]-----------------------------------------------------

(defconstant +cell-width+ 256
  "A cell represent a pixel square, and the width is the
fractional part of the fixed-point coordinate. A large value
increase precision. 256 should be enough though. Note that
smaller value should NOT increase performance.")

;;; This function is used to split a line at each pixel boundaries
;;; (when using sub-pixel coordinates.) Since the function only cut
;;; along one axis, it must be called twice (with the second call with
;;; coordinates swapped) to split along X and Y axis.
;;;
;;; In the comments below, by "main axis" I mean the X axis if A1 and
;;; A2 are the X coordinates, or the Y axis otherwise.
(declaim (inline map-line-spans))
(defun map-line-spans (function a1 b1 a2 b2)
  "Call FUNCTION for each segment of a line with integer
coordinates (A1,B1)-(A2,B2) cut by a grid of spacing
+CELL-WIDTH+."
  (multiple-value-bind (b1-m b1-f) (floor b1 +cell-width+)
    (multiple-value-bind (b2-m b2-f) (floor b2 +cell-width+)
      (cond
        ;; The line doesn't cross the grid in the main axis. We have a
        ;; single segment. Just call FUNCTION.
        ((= b1-m b2-m)
         (funcall function b1-m a1 b1-f a2 b2-f))
        ;; The line cross the grid in the main axis. We have at least
        ;; 2 segments.
        (t
         (let* ((b-m b1-m)
                (delta-a (- a2 a1))
                (delta-b (abs (- b2 b1)))
                (b-increment (signum (- b2 b1)))
                (from-boundary (if (< b1 b2) 0 +cell-width+)) 
                (to-boundary (if (< b1 b2) +cell-width+ 0)))
           (multiple-value-bind (a ma) (floor (+ (* delta-a (if (< b1 b2)
                                                                (- +cell-width+ b1-f)
                                                                b1-f))
                                                 ;; a littre change compared to
                                                 ;; AntiGrain AA algorithm. Used
                                                 ;; to round to the nearest integer
                                                 ;; instead of the "floor" one.
                                                 (floor delta-b 2))
                                              delta-b)
             (incf a a1)
             ;; The first segment (to reach the first grid boundary)
             (funcall function b1-m a1 b1-f a to-boundary)
             (incf b-m b-increment)
             (when (/= b-m b2-m)
               (multiple-value-bind (step mod) (floor (* +cell-width+ delta-a) delta-b)
                 (loop
                    do (let ((prev-a a))
                         (incf a step)
                         (incf ma mod)
                         (when (>= ma delta-b)
                           (incf a)
                           (decf ma delta-b))
                         ;; A segment from one grid boundary to the other.
                         (funcall function b-m prev-a from-boundary a to-boundary)
                         (incf b-m b-increment))
                    while (/= b-m b2-m))))
             ;; The last segment (from the latest grid boundary up to
             ;; the final coordinates.)
             (funcall function b-m a from-boundary a2 b2-f))))))))

(defun map-grid-spans (function x1 y1 x2 y2)
  "Call FUNCTION for each segments of the line from (X1,Y1)
to (X2,Y2) cut by a grid with spacing +CELL-WIDTH+."
  (check-type x1 integer)
  (check-type y1 integer)
  (check-type x2 integer)
  (check-type y2 integer)
  (flet ((hline (y-m x1 y1-f x2 y2-f)
           (declare (integer y-m x1 y1-f x2 y2-f))
           (flet ((pixel (x-m y1-f x1-f y2-f x2-f)
                    (declare (integer x-m y1-f x1-f y2-f x2-f))
                    (funcall function x-m y-m x1-f y1-f x2-f y2-f)))
             ;; further split along Y axis
             (map-line-spans #'pixel y1-f x1 y2-f x2))))
    ;; first split along X axis
    (map-line-spans #'hline x1 y1 x2 y2)))

;;;--[ cell ]-----------------------------------------------------------------

;;; Note that cover and area are unbound and could take any value
;;; while drawing polygons (even negative values), especially when
;;; drawing multiple overlapping polygons. However, for non
;;; overlapping polygons, cover is in the range (-width,width) and
;;; area in the range (-2*width*width,2*width*width), where width is
;;; +cell-width+ defined above.
(defstruct cell
  "A cell used to represent the partial area covered by a line
passing by a corresponding pixel. The cell alone doesn't hold all
the information to calculate the area."
  (x 0 :type integer)
  (y 0 :type integer)
  (cover 0 :type integer)
  (area 0 :type integer))

(declaim (inline cell-empty-p))
(defun cell-empty-p (cell)
  "Test if the cell is empty. A cell is empty when COVER and AREA
are both zero."
  (and (zerop (cell-cover cell))
       (zerop (cell-area cell))))

(declaim (inline cell-reset))
(defun cell-reset (cell)
  "Reset the cell such that CELL-EMPTY-P is true."
  (setf (cell-area cell) 0
        (cell-cover cell) 0))

(declaim (inline compare-cells))
(defun compare-cells (a b)
  "Compare coordinates between 2 cells. Used to sort cells by Y,
then by X."
  (or (< (cell-y a) (cell-y b))
      (and (= (cell-y a) (cell-y b))
           (< (cell-x a) (cell-x b)))))

(declaim (inline update-cell))
(defun update-cell (cell fx1 fy1 fx2 fy2)
  "Update COVER and AREA given a segment inside the corresponding
cell. FX1, FY1, FX2 and FY2 must be subpixel coordinates between
0 and +CELL-WIDTH+ included."
  (let ((delta (- fy2 fy1)))
    (incf (cell-cover cell) delta)
    ;; Note: increase by twice the area, for optimization
    ;; purpose. Will be divided by 2 in the final pass.
    (incf (cell-area cell) (* (+ fx1 fx2) delta))))

;;;-------------------------------------------------------------------------

(defconstant +alpha-range+ 256
  "For non overlapping polygons, the alpha value will be in the
range (-limit,limit) where limit is +alpha-range+. The value is
negative or positive accordingly to the polygon
orientation (clockwise or counter-clockwise.)")

(defconstant +alpha-divisor+ (floor (* 2 +cell-width+ +cell-width+)
                                    +alpha-range+)
  "Constant used to translate value computed by AREA and COVER to
an alpha value.")

(defstruct state
  "AA state. Hold all the cells generated when drawing lines."
  (current-cell (make-cell) :type cell)
  (cells nil)
  (scanlines nil)
  ;; these slots for reusing cells with state-reset
  (end-of-lines nil)
  (dropped-cells nil)
  (recycling-cells (cons nil nil)))

(defun state-reset (state)
  "Reset the state, losing all accumulated cells. It can be
faster or less memory consuming to reset a state and reuse it,
rather than creating a new state."
  (cell-reset (state-current-cell state))
  (when (state-end-of-lines state)
    ;; join back the scanlines to form a single list
    (loop for line in (rest (state-scanlines state))
       for eol in (state-end-of-lines state)
       do (setf (cdr eol) line)))
  (let ((cells (nconc (state-dropped-cells state)
                      (state-cells state))))
    (setf (state-recycling-cells state) (cons nil cells)
          (state-scanlines state) nil
          (state-end-of-lines state) nil
          (state-dropped-cells state) nil
          (state-cells state) cells)))

(declaim (inline state-push-current-cell))
(defun state-push-cell (state cell)
  "Store a copy of the current cell into the cells list. If the
state was reset, possibly reuse previous cells."
  (unless (cell-empty-p cell)
    (let ((recycling-cells (cdr (state-recycling-cells state))))
      (cond
        (recycling-cells
         (let ((target-cell (car recycling-cells)))
           (setf (cell-x target-cell) (cell-x cell)
                 (cell-y target-cell) (cell-y cell)
                 (cell-cover target-cell) (cell-cover cell)
                 (cell-area target-cell) (cell-area cell)))
         (setf (state-recycling-cells state) recycling-cells))
        (t
         (push (copy-cell cell) (state-cells state)))))))

(defun state-finalize (state)
  "Finalize the state."
  ;; Ensure that the current cell is stored with other cells and that
  ;; old cells (before the last reset) that were not reused are
  ;; correctly removed from the result.
  (let ((current-cell (state-current-cell state)))
    (unless (cell-empty-p current-cell)
      (state-push-cell state current-cell)
      (cell-reset current-cell))
    (when (cdr (state-recycling-cells state))
      (setf (cdr (state-recycling-cells state)) nil)
      (unless (car (state-recycling-cells state))
        (setf (state-cells state) nil)))))

(defun set-current-cell (state x y)
  "Ensure current cell is one at coordinate X and Y. If not,
the current cell is stored, then reset accordingly to new
coordinate.

Returns the current cell."
  (let ((current-cell (state-current-cell state)))
    (declare (cell current-cell))
    (when (or (/= x (cell-x current-cell))
              (/= y (cell-y current-cell)))
      ;; Store the current cell, then reset it.
      (state-push-cell state current-cell)
      (setf (cell-x current-cell) x
            (cell-y current-cell) y
            (cell-cover current-cell) 0
            (cell-area current-cell) 0))
    current-cell))

(defun state-sort-cells (state)
  "Sort the cells by Y, then by X."
  (setf (state-cells state)
        (sort (state-cells state) #'compare-cells)))

(defun line (state x1 y1 x2 y2)
  "Draw a line from (X1,Y1) to (X2,Y2). All coordinates are
integers with subpixel accuracy (a pixel width is given by
+CELL-WIDTH+.) The line must be part of a closed polygon."
  (declare (integer x1 y1 x2 y2))
  (map-grid-spans (lambda (x y fx1 fy1 fx2 fy2)
                         (update-cell (set-current-cell state x y)
                                      fx1 fy1 fx2 fy2))
                       x1 y1 x2 y2))

(defun line-f (state x1 y1 x2 y2)
  "Draw a line, whose coordinates are translated to fixed-point
as expected by function LINE. This is a convenient function to
not depend on +CELL-WIDTH+."
  (labels ((float-to-fixed (n)
             (values (round (* +cell-width+ n)))))
    (line state
          (float-to-fixed x1) (float-to-fixed y1)
          (float-to-fixed x2) (float-to-fixed y2))))

(declaim (inline compute-alpha))
(defun compute-alpha (cover area)
  "Compute the alpha value given the accumulated cover and the
actual area of a cell."
  (truncate (- (* 2 +cell-width+ cover) area)
            +alpha-divisor+))

(defun freeze-state (state)
  "Freeze the state and return a list of scanlines. A scanline is
an object which can be examined with SCANLINE-Y and processed
with SCANLINE-SWEEP."
  (unless (state-scanlines state)
    (state-finalize state)
    (state-sort-cells state)
    (let (lines
          end-of-lines
          dropped-cells
          (cells (state-cells state)))
      (when cells
        (push cells lines)
        (let ((previous-cell (first cells)))
          (loop
             (unless (rest cells)
               (return))
             (let ((cell (second cells))
                   (rest (cdr cells)))
               (cond
                 ((/= (cell-y previous-cell) (cell-y cell))
                  ;; different y, break the cells list, begin a new
                  ;; line.
                  (push cells end-of-lines)
                  (push rest lines)
                  (setf (cdr cells) nil
                        previous-cell cell)
                  (setf cells rest))
                 ((/= (cell-x previous-cell) (cell-x cell))
                  ;; same y, different x, do nothing special, move to
                  ;; the next cell.
                  (setf previous-cell cell)
                  (setf cells rest))
                 (t
                  ;; same coordinates, accumulate current cell into
                  ;; the previous, and remove current from the list.
                  (incf (cell-cover previous-cell) (cell-cover cell))
                  (incf (cell-area previous-cell) (cell-area cell))
                  (push cell dropped-cells)
                  (setf (cdr cells) (cdr rest))))))))
      (setf (state-scanlines state) (nreverse lines)
            (state-end-of-lines state) (nreverse end-of-lines)
            (state-dropped-cells state) dropped-cells)))
  (state-scanlines state))

(declaim (inline scanline-y))
(defun scanline-y (scanline)
  "Get the Y position of SCANLINE."
  (cell-y (first scanline)))

(defun scanline-sweep (scanline function function-span &key start end)
  "Call FUNCTION for each pixel on the polygon covered by
SCANLINE. The pixels are scanned in increasing X. The sweep can
be limited to a range by START (included) or/and END (excluded)."
  (declare (optimize speed (debug 0) (safety 0) (space 2)))
  (let ((cover 0)
        (y (scanline-y scanline))
        (cells scanline)
        (last-x nil))
    (when start
      ;; skip initial cells that are before START
      (loop while (and cells (< (cell-x (car cells)) start))
         do (incf cover (cell-cover (car cells)))
         (setf last-x (cell-x (car cells))
               cells (cdr cells))))
    (when cells
      (dolist (cell cells)
        (let ((x (cell-x cell)))
          (when (and last-x (> x (1+ last-x)))
            (let ((alpha (compute-alpha cover 0)))
              (unless (zerop alpha)
                (let ((start-x (if start (max start (1+ last-x)) (1+ last-x)))
                      (end-x (if end (min end x) x)))
                  (if function-span
                      (funcall function-span start-x end-x y alpha)
                      (loop for ix from start-x below end-x
                         do (funcall function ix y alpha)))))))
          (when (and end (>= x end))
            (return))
          (incf cover (cell-cover cell))
          (let ((alpha (compute-alpha cover (cell-area cell))))
            (unless (zerop alpha)
              (funcall function x y alpha)))
          (setf last-x x))))))

(defun cells-sweep/rectangle (state x1 y1 x2 y2 function &optional function-span)
  "Call FUNCTION for each pixel on the polygon described by
previous call to LINE or LINE-F. The pixels are scanned in
increasing Y, then on increasing X. This is limited to the
rectangle region specified with (X1,Y1)-(X2,Y2) (where X2 must be
greater than X1 and Y2 must be greater than Y1, to describe a
non-empty region.)

For optimization purpose, the optional FUNCTION-SPAN, if
provided, is called for a full span of identical alpha pixel. If
not provided, a call is made to FUNCTION for each pixel in the
span."
  (let ((scanlines (freeze-state state)))
    (dolist (scanline scanlines)
      (when (<= y1 (scanline-y scanline) (1- y2))
        (scanline-sweep scanline function function-span :start x1 :end x2))))
  (values))

(defun cells-sweep (state function &optional function-span)
  "Call FUNCTION for each pixel on the polygon described by
previous call to LINE or LINE-F. The pixels are scanned in
increasing Y, then on increasing X.

For optimization purpose, the optional FUNCTION-SPAN, if
provided, is called for a full span of identical alpha pixel. If
not provided, a call is made to FUNCTION for each pixel in the
span."
  (let ((scanlines (freeze-state state)))
    (dolist (scanline scanlines)
      (scanline-sweep scanline function function-span)))
  (values))
