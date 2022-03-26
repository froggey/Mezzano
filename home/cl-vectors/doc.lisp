;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:net.tuxee.vectors-doc
  (:use #:cl #:aa #:paths)
  (:export #:generate))

(in-package #:net.tuxee.vectors-doc)

(defvar *target* "/home/fred/Devel/cl-vectors/doc-pictures/")

;;;--[ Path annotation ]-----------------------------------------------------

(defun path-map-line (path function)
  "Iterate over all the line on the contour of the path."
  (loop with iterator = (path-iterator-segmented path)
     for previous-knot = nil then knot
     for (interpolation knot end-p) = (multiple-value-list (path-iterator-next iterator))
     while knot
     when previous-knot
     do (funcall function previous-knot knot)
     until end-p
     finally (when knot
               (funcall function knot (nth-value 1 (path-iterator-next iterator))))))

(defun rasterize-paths (paths image &optional (color #(0 0 0)) (opacity 1.0) (scale 1.0))
  (let ((state (make-state)))
    (flet ((do-line (p1 p2)
             (line-f state
                     (* scale (point-x p1)) (* scale (point-y p1))
                     (* scale (point-x p2)) (* scale (point-y p2)))))
      (loop for path in (flatten paths)
         do (path-map-line path #'do-line)))
    (cells-sweep state (aa-misc:image-put-pixel image color opacity))))

(defun flatten (path)
  (if (not (listp path))
      (list path)
      (loop for item in path nconc (flatten item))))

(defun paths-bounding-box (paths &optional (scale 1.0))
  (let ((state (make-state))
        min-x max-x
        min-y max-y)
    (flet ((do-line (p1 p2)
             (line-f state
                     (* scale (point-x p1)) (* scale (point-y p1))
                     (* scale (point-x p2)) (* scale (point-y p2))))
           (do-cell (x y alpha)
             (declare (ignore alpha))
             (cond
               (min-x
                (cond
                  ((< x min-x) (setf min-x x))
                  ((> x max-x) (setf max-x x)))
                (cond
                  ((< y min-y) (setf min-y y))
                  ((> y max-y) (setf max-y y))))
               (t
                (setf min-x x
                      max-x x
                      min-y y
                      max-y y)))))
      (loop for path in (flatten paths)
         do (path-map-line path #'do-line))
      (cells-sweep state #'do-cell (lambda (&rest args) (declare (ignore args)))))
    (when min-x
      (values min-x min-y (1+ max-x) (1+ max-y)))))

(defun show-paths (paths &key (color #(0 0 0)) (opacity 1.0) (width 800) (height 600)
                   (background #(255 255 255)))
  (let ((image (aa-misc:make-image width height background)))
    (rasterize-paths paths image color opacity)
    (aa-misc:show-image image)))

(defun create-graph (graph &key subgraphs (width 800) (height 600) (auto-size t) (scale 1.0)
                     (background #(255 255 255)))
  (when auto-size
    (let (min-x max-x
          min-y max-y)
      (flet ((update-limits (graph)
               (loop for (color . paths) in graph
                  do (multiple-value-bind (x1 y1 x2 y2) (paths-bounding-box paths scale)
                       (when x1
                         (when (or (null min-x) (< x1 min-x)) (setf min-x x1))
                         (when (or (null max-x) (> x2 max-x)) (setf max-x x2))
                         (when (or (null min-y) (< y1 min-y)) (setf min-y y1))
                         (when (or (null max-y) (> y2 max-y)) (setf max-y y2)))))))
        (when graph
          (update-limits graph))
        (when subgraphs
          (mapcar #'update-limits subgraphs)))
      (ecase auto-size
        (:border
         (setf width (max 1 (+ (max 0 min-x) max-x))
               height (max 1 (+ (max 0 min-y) max-y))))
        (t
         (setf width (max 1 max-x)
               height (max 1 max-y))))))
  (let ((image (aa-misc:make-image width height background)))
    (when graph
      (loop for (color . paths) in graph
         do (rasterize-paths paths image color 1.0 scale)))
    (dolist (subgraph subgraphs)
      (loop for (color . paths) in subgraph
         do (rasterize-paths paths image color 0.3 scale)))
    image))

(defun generate-annotated-path (path &rest args &key reference &allow-other-keys)
  (apply #'create-graph (when path (path-annotated path))
         :subgraphs (mapcar #'path-annotated (if (listp reference) reference (list reference)))
         :allow-other-keys t
         args))

(defun show-annotated-path (&rest args)
  (aa-misc:show-image (apply #'generate-annotated-path args)))

(defun show-graph (graph)
  (aa-misc:show-image (create-graph graph)))

(defun save-image* (filename image)
  (aa-misc:save-image (merge-pathnames filename *target*) image :pnm))

(defun save-graph (filename graph)
  (aa-misc:save-image (merge-pathnames filename *target*) (create-graph graph) :pnm))

(defun save-annotated-path (filename &rest args)
  (aa-misc:save-image (merge-pathnames filename *target*) (apply #'generate-annotated-path args) :pnm))

;;;--------------------------------------------------------------------------

(defun test ()
  (let ((path (create-path :polygon)))
    (path-reset path (make-point 25 15))
    (path-extend path (make-straight-line) (make-point 250 25))
    (path-extend path (make-bezier-curve (list (make-point 300 40)
                                               (make-point 400 150)
                                               (make-point 200 100)))
                 (make-point 250 250))
    (path-extend path (make-arc 100 200 :x-axis-rotation -0.8)
                 (make-point 25 250))
    (path-extend path (make-catmull-rom (make-point 10 270)
                                        (list (make-point 10 200)
                                              (make-point 40 160)
                                              (make-point 25 120)
                                              (make-point 60 90))
                                        (make-point 70 40))
                 (make-point 55 55))
    (show-annotated-path path)))

(defun generate ()
  ;;
  ;; Reference path
  ;;
  (let ((path (make-simple-path '((125 . 20)
                                  (75 . 105)
                                  (45 . 110)
                                  (25 . 25)
                                  (200 . 55)
                                  (75 . 155)
                                  (90 . 170)
                                  (240 . 120)
                                  (125 . 90)))))
    (save-annotated-path
     "pic-main.pnm"
     path
     :auto-size :border))
  ;;
  ;; Interpolations
  ;;
  (let ((path (create-path :polygon)))
    (path-reset path (make-point 25 15))
    (path-extend path (make-straight-line) (make-point 250 25))
    (path-extend path (make-bezier-curve (list (make-point 300 40)
                                               (make-point 400 150)
                                               (make-point 200 100)))
                 (make-point 250 250))
    (path-extend path (make-arc 100 200 :x-axis-rotation -0.8)
                 (make-point 25 250))
    (path-extend path (make-catmull-rom (make-point 10 270)
                                        (list (make-point 10 200)
                                              (make-point 40 160)
                                              (make-point 25 120)
                                              (make-point 60 90))
                                        (make-point 70 40))
                 (make-point 55 55))
    (save-annotated-path
     "pic-interpolations.pnm"
     path
     :auto-size :border))
  ;;
  ;; Discrete path - Before
  ;;
  (let ((path (make-simple-path '((80 . 80) (100 . 200) (250 . 80) (300 . 200)))))
    (save-annotated-path
     "pic-before-discrete.pnm"
     (paths:stroke-path path 100.0
      :caps :round
      :joint :round
      :inner-joint :miter)
     :reference path
     :auto-size :border))
  ;;
  ;; Discrete path - After
  ;;
  (let ((path (make-simple-path '((80 . 80) (100 . 200) (250 . 80) (300 . 200)))))
    (save-annotated-path
     "pic-after-discrete.pnm"
     (make-discrete-path (first (paths:stroke-path path 100.0
                                 :caps :round
                                 :joint :round
                                 :inner-joint :miter)))
     :reference path
     :auto-size :border))
  ;;
  ;; Stroke
  ;;
  (let* ((path (make-simple-path '((50 . 50) (70 . 170) (190 . 90) (270 . 170) (300 . 40))))
         (stroked (stroke-path path 40.0
                   :caps :square
                   :joint :round
                   :inner-joint :miter
                   :assume-type :open-polyline)))
    (save-annotated-path
     "pic-stroke-open.pnm"
     stroked
     :reference path
     :auto-size :border))
  (let* ((path (make-simple-path '((50 . 50) (70 . 170) (190 . 90) (270 . 170) (300 . 40))))
         (stroked (stroke-path path 40.0
                   :caps :square
                   :joint :round
                   :inner-joint :miter
                   :assume-type :closed-polyline)))
    (save-annotated-path
     "pic-stroke-closed.pnm"
     stroked
     :reference path
     :auto-size :border))
  (let* ((path (make-simple-path '((50 . 50) (70 . 170) (190 . 90) (270 . 170) (300 . 40))))
         (stroked (stroke-path path 40.0
                   :caps :square
                   :joint :round
                   :inner-joint :miter
                   :assume-type :polygon)))
    (save-annotated-path
     "pic-stroke-polygon.pnm"
     stroked
     :reference path
     :auto-size :border))
  ;;
  ;; Dash
  ;;
  (let ((path (create-path :open-polyline)))
    (path-reset path (make-point 30 30))
    (path-extend path (make-straight-line) (make-point 180 80))
    (path-extend path (make-arc 80 80 :large-arc-flag t :sweep-flag t) (make-point 150 150))
    (path-extend path (make-straight-line) (make-point 90 200))
    (save-annotated-path
     "pic-dash-1.pnm"
     (dash-path path #(80 50))
     :reference path
     :auto-size :border))
  ;;
  ;; Clipping
  ;;
  (let ((path (make-simple-path '((50 . 50) (70 . 170) (190 . 30) (270 . 170))))
        (clipping (make-rectangle-path/center 140 120 80 80)))
    (paths::path-rotate clipping 0.3 (make-point 140 120))
    (print (paths::clip-path/path path clipping))
    (save-annotated-path
     "pic-clipping.pnm"
     (paths::clip-path/path path clipping)
     :reference (list path clipping)
     :auto-size :border))
  ;;
  ;; Rotate
  ;;
  (let* ((paths (stroke-path (make-simple-path '((50 . 50) (70 . 170) (190 . 30) (270 . 170)))
                             40.0 :caps :round :inner-joint :miter :joint :round))
         (paths-copy (mapcar #'path-clone paths)))
    (dolist (path paths)
      (path-rotate path 0.4 (make-point 100 80)))
    (save-annotated-path
     "pic-rotate.pnm"
     paths
     :reference (list paths-copy)
     :auto-size :border))
  ;;
  ;; Circle example
  ;;
  (let ((path (make-circle-path 100 50 90 40 0.2)))
    (save-annotated-path
     "pic-circle.pnm"
     path
     :auto-size :border))
  ;;
  ;; Rectangle example
  ;;
  (let ((path (make-rectangle-path 10 10 300 100 :round-x 20 :round-y 30)))
    (save-annotated-path
     "pic-rectangle.pnm"
     path
     :auto-size :border))
  ;;
  ;; Arc example
  ;;
  (let ((path (create-path :open-polyline)))
    (path-reset path (make-point 20 300))
    (path-extend path (make-straight-line) (make-point 70 275))
    (path-extend path (make-arc 25 25 :x-axis-rotation -0.5 :sweep-flag t)
                 (make-point 120 250))
    (path-extend path (make-straight-line) (make-point 170 225))
    (path-extend path (make-arc 25 50 :x-axis-rotation -0.5 :sweep-flag t)
                 (make-point 220 200))
    (path-extend path (make-straight-line) (make-point 270 175))
    (path-extend path (make-arc 25 75 :x-axis-rotation -0.5 :sweep-flag t)
                 (make-point 320 150))
    (path-extend path (make-straight-line) (make-point 370 125))
    (path-extend path (make-arc 25 100 :x-axis-rotation -0.5 :sweep-flag t)
                 (make-point 420 100))
    (path-extend path (make-straight-line) (make-point 470 75))
    (paths::path-scale path 0.7 0.7)
    (save-annotated-path
     "pic-arc.pnm"
     path
     :auto-size :border))
  ;;
  ;; Catmull-Rom example
  ;;
  (let ((path (create-path :open-polyline)))
    (path-reset path (make-point 30 40))
    (path-extend path (make-catmull-rom (make-point 20 20)
                                        (list (make-point 80 20)
                                              (make-point 140 190)
                                              (make-point 200 140)
                                              (make-point 130 30))
                                        (make-point 300 90))
                 (make-point 270 40))
    (save-annotated-path
     "pic-catmull-rom.pnm"
     path
     :auto-size :border))
  ;;
  ;; Bezier example
  ;;
  (let ((path (create-path :open-polyline)))
    (path-reset path (make-point 10 100))
    (path-extend path (make-bezier-curve (list (make-point 80 10)
                                               (make-point 140 250)
                                               (make-point 200 200)
                                               (make-point 250 90)))
                 (make-point 300 100))
    (save-annotated-path
     "pic-bezier.pnm"
     path
     :auto-size :border))
  ;;
  ;; list marker
  ;;
  (let ((path (create-path :polygon)))
    (path-extend path (make-arc 50 50) (make-point 0 0))
    (path-extend path (make-arc 34 34) (make-point 20 20))
    (path-extend path (make-arc 34 34) (make-point 0 40))
    (path-reverse path)
    (save-graph "pic-list.pnm" (list (list #(120 120 120) (stroke-path path 2))
                                     (list #(0 0 0) path))))
  ;;
  ;; black triangle antialiased
  ;;
  (let ((state (aa:make-state)))
    (aa:line-f state 200 50 250 150)
    (aa:line-f state 250 150 50 100)
    (aa:line-f state 50 100 200 50)
    (let* ((image (aa-misc:make-image 300 200 #(255 255 255)))
           (put-pixel (aa-misc:image-put-pixel image #(0 0 0))))
      (aa:cells-sweep state put-pixel)
      (save-image* "pic-tut1.pnm" image)))
  ;; 2 overlapping triangles
  (let ((state (aa:make-state)))        ; create the state
    ;; the 1st triangle
    (aa:line-f state 200 50 250 150)    ; describe the 3 sides
    (aa:line-f state 250 150 50 100)    ; of the first triangle
    (aa:line-f state 50 100 200 50)
    ;; the 2nd triangle
    (aa:line-f state 75 25 10 75)       ; describe the 3 sides
    (aa:line-f state 10 75 175 100)     ; of the second triangle
    (aa:line-f state 175 100 75 25)
    (let* ((image (aa-misc:make-image 300 200 #(255 255 255)))
           (put-pixel (aa-misc:image-put-pixel image #(0 0 0))))
      (aa:cells-sweep state put-pixel)  ; render it
      (save-image* "pic-tut2.pnm" image)))
  ;; 2 overlapping triangles red/blue
  (let ((state1 (aa:make-state))
        (state2 (aa:make-state)))
    ;; the 1st triangle
    (aa:line-f state1 200 50 250 150)   ; describe the 3 sides
    (aa:line-f state1 250 150 50 100)   ; of the first triangle
    (aa:line-f state1 50 100 200 50)
    ;; the 2nd triangle
    (aa:line-f state2 75 25 10 75)      ; describe the 3 sides
    (aa:line-f state2 10 75 175 100)    ; of the second triangle
    (aa:line-f state2 175 100 75 25)
    (let ((image (aa-misc:make-image 300 200 #(255 255 255))))
      (aa:cells-sweep state1 (aa-misc:image-put-pixel image #(255 0 0)))
      (aa:cells-sweep state2 (aa-misc:image-put-pixel image #(0 0 255)))
      (save-image* "pic-tut3.pnm" image)))
  )

#|
(defun path-extend-with-waves (path knot width frequency)
  ;; generate a serie of arc to represent a wave up to knot
  )
|#
