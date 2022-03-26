;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:net.tuxee.paths-ttf
  (:use #:cl #:net.tuxee.paths #:zpb-ttf)
  (:nicknames #:paths-ttf)
  (:export #:paths-from-glyph
           #:paths-from-string
           #:make-string-path))

(in-package #:net.tuxee.paths-ttf)

(defun paths-from-glyph (glyph &key (offset (make-point 0 0))
                                    (scale-x 1.0) (scale-y 1.0)
                                    (auto-orient nil))
  "Extract paths from a glyph."
  (flet ((point (p) (p+ (make-point (* (x p) scale-x)
                                    (* (y p) scale-y))
                        offset)))
    (let (result)
      (do-contours (contour glyph)
        (let ((path (create-path :polygon))
              (last-point nil))
          (do-contour-segments (a b c) contour
            (let ((pa (point a))
                  (pb (when b (point b)))
                  (pc (point c)))
              (if last-point
                  (assert (and (= (point-x last-point) (point-x pa))
                               (= (point-y last-point) (point-y pa))))
                  (path-reset path pa))
              (path-extend path
                           (if b
                               (make-bezier-curve (list pb))
                               (make-straight-line))
                           pc)
              (setq last-point pc)))
          (when (minusp (* scale-x scale-y))
            (path-reverse path))
          (push path result)))
      (setq result (nreverse result))
      (when (and auto-orient result)
        (path-orient (car result) auto-orient (cdr result)))
      result)))

(defun paths-from-string (font-loader text &key (offset (make-point 0 0))
                                                (scale-x 1.0) (scale-y 1.0)
                                                (kerning t) (auto-orient nil))
  "Extract paths from a string."
  (let (result)
    (loop
       for previous-char = nil then char
       for char across text
       for previous-glyph = nil then glyph
       for glyph = (find-glyph char font-loader)
       do (when previous-char
            (setf offset
                  (p+ offset
                      (make-point (* scale-x
                                     (+ (advance-width previous-glyph)
                                        (if kerning
                                            (kerning-offset previous-char
                                                            char
                                                            font-loader)
                                            0)))
                                  0))))
       (let ((glyph-paths (paths-from-glyph glyph
                                            :offset offset :auto-orient auto-orient
                                            :scale-x scale-x :scale-y scale-y)))
         (push glyph-paths result)))
    (apply #'nconc (nreverse result))))

(defun make-string-path (font-loader text &key (position (make-point 0 0)) (size 12)
                                               (halign :left) (valign :baseline)
                                               (inverted t) (kerning t))
  (let* ((em (units/em font-loader))
         (scale (/ size em))
         (scale-x scale)
         (scale-y scale))
    (when inverted
      (setq scale-y (- scale-y)))
    (let ((bb (string-bounding-box text font-loader :kerning kerning)))
      (setq position (p- position
                         (p* (make-point
                              (ecase halign
                                (:none
                                 0)
                                (:left
                                 (aref bb 0))
                                (:right
                                 (aref bb 2))
                                (:center
                                 (/ (+ (aref bb 0) (aref bb 2)) 2.0)))
                              (ecase valign
                                (:baseline
                                 0)
                                (:top
                                 (aref bb 1))
                                (:bottom
                                 (aref bb 3))
                                (:center
                                 (/ (+ (aref bb 1) (aref bb 3)) 2.0))))
                             scale))))
    (paths-from-string font-loader text :offset position
                       :scale-x scale-x :scale-y scale-y
                       :kerning kerning
                       :auto-orient :cw)))
