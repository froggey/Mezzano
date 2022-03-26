;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Glyph rendering via zpb-ttf and cl-vectors
;;;   Created: 2008-01-26 16:32
;;;    Author: Andy Hefner <ahefner@gmail.com>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2008 by Andy Hefner
;;;  (c) copyright 2016 by Daniel Kochmański
;;;
;;;    See toplevel file 'Copyright' for the copyright details.
;;;

(in-package :mcclim-truetype)

;;; TODO:
;;;  * Implement fixed-font-width-p for zpb-ttf.
;;;  * Implement text direction for font-text-extents

;;; Wish-list:
;;;  * Subpixel antialiasing. It would be straightforward to generate the
;;;    glyphs by tripling the width as passed to cl-vectors and compressing
;;;    triplets of pixels together ourselves. I'm not certain how to draw
;;;    the result through xrender. I've seen hints on Google that there is
;;;    subpixel AA support in xrender, which isn't obvious from CLX or the 
;;;    spec. Failing that, we could use a 24bpp mask with component-alpha. 
;;;    That might even be how you're supposed to do it. I'm skeptical as to 
;;;    whether this would be accelerated for most people.

;;;  * Subpixel positioning. Not hard in principle - render multiple versions
;;;    of each glyph, offset by fractions of a pixel. Horizontal positioning
;;;    is more important than vertical, so 1/4 pixel horizontal resolution
;;;    and 1 pixel vertical resolution should suffice. Given how ugly most
;;;    CLIM apps are, and the lack of WYSIWYG document editors crying out 
;;;    for perfect text spacing in small fonts, we don't really need this.


(defvar *zpb-font-lock* (clim-sys:make-lock "zpb-font"))
(defparameter *dpi* 72)

(defclass truetype-font-family (clim-extensions:font-family)
  ((all-faces :initform nil
              :accessor all-faces
              :reader clim-extensions:font-family-all-faces)))

(defclass truetype-face (clim-extensions:font-face)
  ((all-fonts :initform nil
              :accessor all-fonts)
   (font-loader :initarg :loader :reader zpb-ttf-font-loader)))

(defmethod initialize-instance :after ((face truetype-face) &key &allow-other-keys)
  (let ((family (clim-extensions:font-face-family face)))
    (pushnew face (all-faces family))))

(defclass truetype-font ()
  ((face          :initarg :face     :reader climb:font-face)
   (size          :initarg :size     :reader climb:font-size)
   (kerning-p     :initarg :kerning  :reader climb:font-kerning-p)
   (tracking      :initarg :tracking :reader climb:font-tracking)
   (leading       :initarg :leading  :reader climb:font-leading)
   (fixed-width   :initarg :fixed    :reader climb:font-fixed-width :type (or fixnum null))
   (ascent                           :reader climb:font-ascent)
   (descent                          :reader climb:font-descent)
   (units->pixels                    :reader zpb-ttf-font-units->pixels))
  ;; Parameters TRACKING and LEADING are specified in [em]. Internally we keep
  ;; them in [units].
  (:default-initargs :fixed nil :kerning t :tracking 0.0 :leading 1.2))

(defmethod initialize-instance :after ((font truetype-font) &key tracking leading &allow-other-keys)
  (with-slots (face size ascent descent font-loader) font
    (let* ((loader (zpb-ttf-font-loader face))
           (em->units (zpb-ttf:units/em loader))
           (units->pixels (/ (* size (/ *dpi* 72)) em->units)))
      (setf ascent  (+ (* units->pixels (zpb-ttf:ascender loader)))
            descent (- (* units->pixels (zpb-ttf:descender loader)))
            (slot-value font 'tracking) (* units->pixels (* em->units tracking))
            (slot-value font 'leading)  (* units->pixels (* em->units leading))
            (slot-value font 'units->pixels) units->pixels))
    (pushnew font (all-fonts face))))

(defmethod zpb-ttf:kerning-offset ((left character) (right character) (font truetype-font))
  (if (null (climb:font-kerning-p font))
      0
      (zpb-ttf:kerning-offset left right (zpb-ttf-font-loader (climb:font-face font)))))

(defmethod clim-extensions:font-face-all-sizes ((face truetype-face))
  (sort (mapcar #'climb:font-size (all-fonts face)) #'<))

(defmethod clim-extensions:font-face-text-style
    ((face truetype-face) &optional size)
  (make-text-style (clim-extensions:font-family-name
                    (clim-extensions:font-face-family face))
                   (clim-extensions:font-face-name face)
                   size))

(defmethod print-object ((object truetype-font) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots (size ascent descent units->pixels) object      
      (format stream " size=~A ascent=~A descent=~A units->pixels=~A"
              size ascent descent units->pixels))))

;;; Derived from CL-VECTORS library function PATHS-TTF:PATHS-FROM-GLYPH.
(defun paths-from-glyph* (glyph tr)
  "Extract paths from a glyph."
  (flet ((point (p) (multiple-value-call #'net.tuxee.paths:make-point
                      (transform-position tr (zpb-ttf:x p) (zpb-ttf:y p)))))
    (let (result)
      (zpb-ttf:do-contours (contour glyph)
        (let ((path (net.tuxee.paths:create-path :polygon))
              (last-point nil))
          (zpb-ttf:do-contour-segments (a b c) contour
            (let ((pa (point a))
                  (pb (when b (point b)))
                  (pc (point c)))
              (unless last-point
                (net.tuxee.paths:path-reset path pa))
              (net.tuxee.paths:path-extend path
                                           (if b
                                               (net.tuxee.paths:make-bezier-curve (list pb))
                                               (net.tuxee.paths:make-straight-line))
                                           pc)
              (setq last-point pc)))
          (push path result)))
      (setq result (nreverse result))
      result)))

(defun glyph-pixarray (font char next transformation)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array suitable
   as an alpha mask, and dimensions. This function returns seven values: alpha
   mask byte array, x-origin, y-origin (subtracted from position before
   rendering), glyph width and height, horizontal and vertical advances."
  (declare (optimize (debug 3)))
  (clim-sys:with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels size ascent descent) font
      (let* ((font-loader (zpb-ttf-font-loader (climb:font-face font)))
             (glyph (zpb-ttf:find-glyph char font-loader))
             ;; (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
             ;; (right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
             (udx (+ (* units->pixels (zpb-ttf:advance-width glyph))
                     (* units->pixels (zpb-ttf:kerning-offset char next font))
                     (climb:font-tracking font)))
             (udy 0)
             (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                                (zpb-ttf:bounding-box glyph)))
             (min-x (elt bounding-box 0))
             (min-y (elt bounding-box 1))
             (max-x (elt bounding-box 2))
             (max-y (elt bounding-box 3))
             ;; top-side-bearing is mostly useful for vertical
             ;; metrics. left-side-bearing is the same as xmin,
             ;; right-side-bearing may be inferred as well. bottom-side-bearing
             ;; is usually not mentioned in the literature due to its limited
             ;; purpose (I'm not aware of any vertical alphabet which direction
             ;; is bottom-to-top), but we could imagine it has a similar
             ;; relation as the right-side-bearing.
             ;;
             ;;   left-side-bearing = min-x
             ;;   right-side-bearing = advance-width - left-side-bearing - width
             ;;   top-side-bearing = baseline + max-y
             ;;   bottom-side-bearing = advance-height - top-side-bearing - height
             ;;
             ;; all these values may be inferred from other glyph properties so
             ;; we do not return them. -- jd 2018-10-14
             width height left top array)

        (with-bounding-rectangle* (x1 y1 x2 y2)
            (transform-region transformation (make-rectangle* min-x min-y max-x max-y))
          (setq width  (- (ceiling x2) (floor x1)))
          (setq height (- (ceiling y2) (floor y1)))
          (setq left (- (floor x1)))
          (setq top (ceiling y2))
          (setq array (make-array (list height width)
                                  :initial-element 0
                                  :element-type '(unsigned-byte 8))))
        (let* ((glyph-tr (compose-transformations
                          (compose-transformations
                           (make-translation-transformation left top)
                           (make-scaling-transformation units->pixels (- units->pixels)))
                          transformation))
               (paths (paths-from-glyph* glyph glyph-tr))
               (state (aa:make-state)))
          (dolist (path paths)
            (vectors:update-state state path))
          (aa:cells-sweep state
                          (lambda (x y alpha)
                            (when (array-in-bounds-p array y x)
                              (setf alpha (min 255 (abs alpha))
                                    (aref array y x) (climi::clamp
                                                      (floor (+ (* (- 256 alpha) (aref array y x))
                                                                (* alpha 255))
                                                             256)
                                                      0 255))))))
        #+ (or) ;; draw delicate border around each glyph (for testing)
        (progn
          (loop for j from 0 below height do (setf (aref array j 0)
                                                   (logior #x40 (aref array j 0))
                                                   (aref array j (1- width))
                                                   (logior #x40 (aref array j (1- width)))))
          (loop for i from 0 below width do (setf (aref array 0 i)
                                                  (logior #x40 (aref array 0 i))
                                                  (aref array (1- height) i)
                                                  (logior #x40 (aref array (1- height) i)))))
        (multiple-value-bind (dx dy)
            ;; Transformation is supplied in font coordinates for easy
            ;; composition with offset and scaling. advance values should be
            ;; returned in screen coordinates, so we transform it here.
            (transform-distance (compose-transformations
                                 #1=(make-scaling-transformation 1.0 -1.0)
                                 (compose-transformations transformation #1#))
                                udx udy)
          (values array (- left) top width height
                  ;; X uses horizontal/vertical advance between letters. That
                  ;; way glyph sequence may be rendered. This should not be
                  ;; confused with font width/height! -- jd 2018-09-28
                  (round dx)
                  (round dy)
                  ;; Transformed text is rendered glyph by glyph to mitigate
                  ;; accumulation of the rounding error. For that we need values
                  ;; without rounding nor transformation. -- jd 2018-10-04
                  udx
                  udy))))))


(deftype glyph-pixarray () '(simple-array (unsigned-byte 8) (* *)))

(defstruct (glyph-info (:constructor glyph-info (id pixarray width height
                                                 left right top bottom
                                                 advance-width advance-height
                                                 advance-width* advance-height*)))
  (id 0                :read-only t :type fixnum)
  (pixarray nil        :read-only t :type (or null glyph-pixarray))
  (width 0             :read-only t)
  (height 0            :read-only t)
  (left 0              :read-only t)
  (right 0             :read-only t)
  (top 0               :read-only t)
  (bottom 0            :read-only t)
  (advance-width 0     :read-only t)
  (advance-height 0    :read-only t)
  ;; untransformed values
  (advance-width* 0f0  :read-only t)
  (advance-height* 0f0 :read-only t))


(defclass cached-truetype-font (truetype-font)
  ((char->glyph-info  :initform (make-hash-table :size 512))))

(defun font-glyph-info (font code)
  (with-slots (char->glyph-info) font
    (ensure-gethash code char->glyph-info
                    (font-generate-glyph font code))))

(defgeneric font-generate-glyph (font code &optional tr)
  (:documentation "Truetype TTF renderer internal interface. Backend-specific."))

(defun font-glyph-id (font code)
  (glyph-info-id (font-glyph-info font code)))

(defmethod climb:font-glyph-width ((font cached-truetype-font) code)
  (glyph-info-width (font-glyph-info font code)))

(defmethod climb:font-glyph-height ((font cached-truetype-font) code)
  (glyph-info-height (font-glyph-info font code)))

(defmethod climb:font-glyph-dx ((font cached-truetype-font) code)
  (glyph-info-advance-width (font-glyph-info font code)))

(defmethod climb:font-glyph-dy ((font cached-truetype-font) code)
  (glyph-info-advance-height (font-glyph-info font code)))

(defmethod climb:font-glyph-left ((font cached-truetype-font) code)
  (glyph-info-left (font-glyph-info font code)))

(defmethod climb:font-glyph-right ((font cached-truetype-font) code)
  (glyph-info-right (font-glyph-info font code)))

(defmethod climb:font-glyph-top ((font cached-truetype-font) code)
  (glyph-info-top (font-glyph-info font code)))

(defmethod climb:font-glyph-bottom ((font cached-truetype-font) code)
  (glyph-info-bottom (font-glyph-info font code)))

(defmethod climb:font-string-glyph-codes ((font truetype-font) string
                                          &key (start 0) (end (length string)))
  (alexandria:minf end (length string))
  (when (>= start end)
    (return-from climb:font-string-glyph-codes #()))
  (loop
     with array = (make-array (- end start) :fill-pointer 0)
     as char = (char string start) then next-char
     for i fixnum from (1+ start) below end
     as next-char = (char string i)
     as code = (dpb (char-code next-char)
                    (byte #.(ceiling (log char-code-limit 2))
                          #.(ceiling (log char-code-limit 2)))
                    (char-code char))
     do
       (vector-push code array)
     finally
       (vector-push (char-code char) array)
       (return array)))

(defmethod climb:font-glyph-code-char ((font truetype-font) code)
  (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code)))
