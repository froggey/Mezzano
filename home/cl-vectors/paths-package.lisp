;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:net.tuxee.paths
  (:use #:cl)
  (:nicknames #:paths)
  (:export ;; 2D points (knot and control points)
           #:make-point
           #:point-x
           #:point-y
           #:p+
           #:p-
           #:p*
           #:point-rotate
           #:point-angle
           #:point-norm
           #:point-distance
           ;; Paths
           #:create-path
           #:path-clear
           #:path-reset
           #:path-extend
           #:path-concatenate
           #:path-replace
           #:path-size
           #:path-last-knot
           #:path-orient
           #:path-clone
           #:path-reverse
           #:path-reversed
           #:path-translate
           #:path-rotate
           #:path-scale
           #:path-transform-as-marker
           ;; Interpolators
           #:make-straight-line
           #:make-arc
           #:make-catmull-rom
           #:make-bezier-curve
           ;; Path iterators
           #:path-iterator-reset
           #:path-iterator-next
           #:path-iterator
           #:path-iterator-segmented
           #:filter-distinct
           ;; Misc
           #:make-discrete-path
           #:make-circle-path
           #:make-rectangle-path
           #:make-rectangle-path/center
           #:make-regular-polygon-path
           #:make-simple-path
           #:path-annotated
           #:make-simple-path
           ;; Transformations
           #:stroke-path
           #:dash-path
           #:clip-path
           #:clip-path/path
           ;; Variables
           #:*bezier-distance-tolerance*
           #:*bezier-angle-tolerance*
           #:*arc-length-tolerance*
           ))

(in-package #:net.tuxee.paths)

