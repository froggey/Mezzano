;;;;  Copyright (c) 2003       Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;;  Copyright (c) 2018       Daniel Kochmański <daniel@turtleware.eu>
;;;;
;;;;    License:  LGPL-2.1-or-later

(in-package #:clim-clx)

(defclass clx-render-medium (clx-medium
                             climb:multiline-text-medium-mixin
                             climb:font-rendering-medium-mixin)
  ((picture :initform nil)))

(defun clx-render-medium-picture (medium)
  (with-slots (picture) medium
    (alexandria:when-let* ((mirror (port-lookup-mirror (port medium) (medium-sheet medium)))
                           (format (xlib:find-window-picture-format (xlib:drawable-root mirror))))
      (cond
        ((null picture)
         (setf picture (xlib:render-create-picture mirror :format format)))
        ;; We need this comparison to mitigate a rogue mirror swaps with a
        ;; pixmap, i.e in WITH-TEMP-MIRROR%%% for double buffering.
        ((eq mirror (xlib:picture-drawable picture))
         picture)
        (T ;; mirror has been swapped!
         (xlib:render-free-picture picture)
         (setf picture (xlib:render-create-picture mirror :format format)))))))


(defmethod clim:medium-copy-area ((from-drawable clx-render-medium) from-x from-y
                                  width height
                                  (to-drawable clx-render-medium) to-x to-y)
  (call-next-method))

(defmethod clim:medium-copy-area ((from-drawable clx-render-medium) from-x from-y
                                  width height
                                  (to-drawable pixmap) to-x to-y)
  (call-next-method))

(defmethod clim:medium-copy-area ((from-drawable pixmap) from-x from-y
                                  width height
                                  (to-drawable clx-render-medium) to-x to-y)
  (call-next-method))


(defmethod clim:medium-draw-point* ((medium clx-render-medium) x y)
  (call-next-method))

(defmethod clim:medium-draw-points* ((medium clx-render-medium) coord-seq)
  (call-next-method))


;;; XXX: CLX decorates lines for us. When we do it ourself this should be
;;; adjusted. For details see CLIM 2, Part 4, Section 12.4.1. -- jd 2019-01-31

(defmethod clim:medium-draw-line* ((medium clx-render-medium) x1 y1 x2 y2)
  (call-next-method))

(defmethod clim:medium-draw-lines* ((medium clx-render-medium) coord-seq)
  (call-next-method))

;;; XXX: all MEDIUM-DRAW-FOO* methods with "filled" argument should be in fact
;;; dispatched somewhere else from the DRAW-FOO* for the path rendering
;;; (MEDIUM-DRAW-LINES* and MEDIUM-DRAW-ELLIPSE* vs MEDIUM-DRAW-RECTANGLE* and
;;; MEDIUM-DRAW-AREA*. This will require going through all defined backends and
;;; purging "extra" arguments. -- jd 2019-01-31


(defmethod clim:medium-draw-polygon* ((medium clx-render-medium) coord-seq closed filled)
  (call-next-method medium coord-seq closed filled))


(defun medium-draw-rectangle-xrender (medium x1 y1 x2 y2 filled)
  (declare (ignore filled))
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (multiple-value-bind (r g b a) (clime:color-rgba (medium-ink medium))
            ;; Hmm, XRender uses pre-multiplied alpha, how useful!
            (setf r (min #xffff (max 0 (round (* #xffff a r))))
                  g (min #xffff (max 0 (round (* #xffff a g))))
                  b (min #xffff (max 0 (round (* #xffff a b))))
                  a (min #xffff (max 0 (round (* #xffff a)))))
            ;; If there is no picture that means that sheet does not have a
            ;; registered mirror. Happens with DREI panes during the startup..
            (alexandria:when-let ((picture (clx-render-medium-picture medium)))
              (setf (xlib:picture-clip-mask picture) (clipping-region->rect-seq
                                                      (or (last-medium-device-region medium)
                                                          (medium-device-region medium))))
              (xlib:render-fill-rectangle picture :over (list r g b a)
                                          (max #x-8000 (min #x7FFF x1))
                                          (max #x-8000 (min #x7FFF y1))
                                          (max 0 (min #xFFFF (- x2 x1)))
                                          (max 0 (min #xFFFF (- y2 y1)))))))))))

(defmethod clim:medium-draw-rectangle* ((medium clx-render-medium) left top right bottom filled)
  #- (or)
  (call-next-method medium left top right bottom filled)
  #+ (or) ;; this causes regressions (i.e in draggable graph)
  (if filled
      (typecase (medium-ink medium)
        ((or climi::uniform-compositum clim:color clim:opacity)
         (medium-draw-rectangle-xrender medium left top right bottom filled))
        (otherwise
         (call-next-method)))
      (call-next-method)))

(defmethod clim:medium-draw-rectangles* ((medium clx-render-medium) position-seq filled)
  (call-next-method medium position-seq filled))


(defmethod clim:medium-draw-ellipse* ((medium clx-render-medium) center-x center-y
                                      radius-1-dx radius-1-dy
                                      radius-2-dx radius-2-dy
                                      start-angle end-angle filled)
  (call-next-method))

(defmethod clime:medium-draw-circle* ((medium clx-render-medium)
                                      center-x center-y radius start-angle end-angle
                                      filled)
  (call-next-method))


(defmethod climb:text-size ((medium clx-render-medium) string &key text-style (start 0) end
                            &aux
                              (string (string string))
                              (end (or end (length string))))
  (when (= start end)
    (return-from text-size (values 0 0 0 0 (text-style-ascent text-style medium))))
  (let* ((text-style (merge-text-styles text-style
                                        (medium-merged-text-style medium)))
         (font (text-style-mapping (port medium) text-style))
         (text (subseq (string string) start end))
         (ascent (climb:font-ascent font))
         (line-height (+ ascent (climb:font-descent font)))
         (leading (climb:font-leading font))
         (current-dx 0)
         (maximum-dx 0)
         (current-y 0))
    (dolines (text text)
      (loop
         with origin-x fixnum = 0
         for code across (climb:font-string-glyph-codes font text)
         do (incf origin-x (climb:font-glyph-dx font code))
         finally
           (alexandria:maxf maximum-dx origin-x)
           (setf current-dx origin-x)
           (incf current-y leading)))
    (values maximum-dx (+ current-y line-height (- leading))
            current-dx (- current-y leading)
            ascent)))

(defvar *draw-font-lock* (clim-sys:make-lock "draw-font"))
(defmethod clim:medium-draw-text* ((medium clx-render-medium) string x y
                                   start end
                                   align-x align-y
                                   toward-x toward-y transform-glyphs
                                   &aux (end (if (null end)
                                                 (length string)
                                                 (min end (length string)))))
  ;; Possible optimalzaions:
  ;;
  ;; * with-clx-graphics already creates appropriate pixmap for us (correct one!) and we have
  ;; medium picture in place - there is no need for gcontext-picture (see xrender-fonts)
  ;; * don't use (PICTURE-DRAWABLE (CLX-RENDER-MEDIUM-PICTURE MEDIUM)) - it is slow due to possible
  ;; mirror swaps, use (SHEET-XMIRROR (MEDIUM-SHEET MEDIUM)) instead. It might be a good idea to
  ;; wrap our own (CLX-RENDER-MEDIUM-MIRROR MEDIUM) function.
  (declare (ignore toward-x toward-y))
  (when (alexandria:emptyp string)
    (return-from clim:medium-draw-text*))
  (with-clx-graphics () medium
    (clim-sys:with-lock-held (*draw-font-lock*)
      (mcclim-font:draw-glyphs medium mirror gc x y string
                               :start start :end end
                               :align-x align-x :align-y align-y
                               :translate #'translate
                               :transformation (sheet-device-transformation (medium-sheet medium))
                               :transform-glyphs transform-glyphs))))



(defmethod clim:medium-buffering-output-p ((medium clx-render-medium))
  (call-next-method))

(defmethod (setf clim:medium-buffering-output-p) (buffer-p (medium clx-render-medium))
  (call-next-method))

(defmethod clim:medium-finish-output ((medium clx-render-medium))
  (call-next-method))

(defmethod clim:medium-force-output ((medium clx-render-medium))
  (call-next-method))

(defmethod clim:medium-clear-area ((medium clx-render-medium) left top right bottom)
  (call-next-method))

(defmethod clim:medium-beep ((medium clx-render-medium))
  (call-next-method))

(defmethod clime:medium-miter-limit ((medium clx-render-medium))
  (call-next-method))
