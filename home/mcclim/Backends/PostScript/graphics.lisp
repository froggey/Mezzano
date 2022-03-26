;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)


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

;;; TODO:
;;;
;;; - clipping
;;; - - more regions to draw
;;; - (?) blending
;;; - MEDIUM-DRAW-TEXT*
;;; - - :towards-(x,y)
;;; - - landscape orientation
;;; - - (?) :transform-glyphs
;;; - POSTSCRIPT-ACTUALIZE-GRAPHICS-STATE: fix CLIPPING-REGION reusing logic
;;; - MEDIUM-DRAW-... should not duplicate code from POSTSCRIPT-ADD-PATH
;;; - structure this file
;;; - set miter limit?

(in-package :clim-postscript)

;;; Postscript output utilities
(defun write-number (stream number)
  (format stream "~,3F " (coerce number 'single-float)))

(defun write-angle (stream angle)
  (write-number stream (* angle (/ 180 pi))))

(defun write-coordinates (stream x y)
  (write-number stream x)
  (write-number stream y))

(defun write-transformation* (stream mxx mxy myx myy tx ty)
  (write-char #\[ stream)
  (dolist (c (list mxx myx mxy myy tx ty))
     (write-number stream c))
  (write-string "] " stream))

;;; Low level functions
(defstruct postscript-procedure
  (name nil :type (or symbol string))
  (body "" :type string))

(defvar *dictionary-name* "McCLIMDict")

(defvar *procedures* (make-hash-table))

(defvar *extra-entries* 0)

(defun dump-reencode (sink)
  (format sink "
% base-font-name new-font-name encoding-vector ReEncode -->
/ReEncode
{
  5 dict 
  begin
    /newencoding exch def
    /newfontname exch def
    /basefontname exch def

    /basefontdict basefontname findfont def
    /newfont basefontdict maxlength dict def
    
    basefontdict
    { 
      exch dup dup /FID ne exch /Encoding ne and 
      { exch newfont 3 1 roll put }
      { pop pop }
      ifelse
    } forall

    newfont /FontName newfontname put
    newfont /Encoding newencoding put
    newfontname newfont definefont pop
  end
} def

/R { 2 1 roll 0 rmoveto show } def

/ISOmapping 256 array def
")
  (format sink "ISOmapping~%")
  (dotimes (i 256)
    (format sink "  dup ~3D /~A put~%" i (or (aref *iso-latin-1-symbolic-names* i) ".notdef")))
  (format sink "pop~%~%")
  
  (dolist (k '("Times-Roman" "Times-Italic" "Times-Bold" "Times-BoldItalic"
	       "Helvetica" "Helvetica-Oblique" "Helvetica-Bold" "Helvetica-BoldOblique"
	       "Courier" "Courier-Oblique" "Courier-Bold" "Courier-BoldOblique"))
    (format sink "/~A /~A-iso ISOmapping ReEncode~%" k k)))

(defun write-postscript-dictionary (stream)
  ;;; FIXME: DSC
  (format stream "~&%%BeginProlog~%")
  (format stream "/~A ~D dict def ~2:*~A begin~%"
          *dictionary-name* (+ (hash-table-count *procedures*)
                               *extra-entries*))
  (loop for proc being each hash-value in *procedures*
     for name = (postscript-procedure-name proc)
     and body = (postscript-procedure-body proc)
     do (format stream "/~A { ~A } def~%" name body))
  (format stream "end~%")
  (dump-reencode stream)
  (format stream "%%EndProlog~%"))

(defmacro define-postscript-procedure
    ((name &key postscript-name postscript-body
           (extra-entries 0)) args
     &body body)
  (check-type name symbol)
  (check-type postscript-name (or symbol string))
  (check-type postscript-body string)
  (check-type extra-entries unsigned-byte)
  `(progn
     (setf (gethash ',name *procedures*)
           (make-postscript-procedure :name ,postscript-name
                                      :body ,postscript-body))
     (maxf *extra-entries* ,extra-entries)
     (defun ,name ,args ,@body)))

;;;
(define-postscript-procedure
    (moveto* :postscript-name "m"
             :postscript-body "moveto")
    (stream x y)
  (write-coordinates stream x y)
  (format stream "m~%"))

(define-postscript-procedure
    (lineto* :postscript-name "l"
             :postscript-body "lineto")
    (stream x y)
  (write-coordinates stream x y)
  (format stream "l~%"))

(define-postscript-procedure
    (put-rectangle* :postscript-name "pr"
                    :postscript-body
                    "/y2 exch def /x2 exch def /y1 exch def /x1 exch def
x1 y1 moveto x1 y2 lineto x2 y2 lineto x2 y1 lineto x1 y1 lineto"
                    :extra-entries 4)
    (stream x1 y1 x2 y2)
  (write-coordinates stream x1 y1)
  (write-coordinates stream x2 y2)
  (format stream "pr~%"))

(define-postscript-procedure (put-line* :postscript-name "pl"
                                        :postscript-body "moveto lineto")
    (stream x1 y1 x2 y2)
  (write-coordinates stream x2 y2)
  (write-coordinates stream x1 y1)
  (format stream "pl~%"))

(define-postscript-procedure
    (put-ellipse :postscript-name "pe"
                 :postscript-body
                 ;; filled end-angle start-angle trans
                 "matrix currentmatrix 5 1 roll
concat dup rotate sub
1 0 moveto
0 0 1 0 5 -1 roll arc
{ 0 0 lineto 1 0 lineto } {} ifelse
setmatrix")
    (stream ellipse filled)
  (multiple-value-bind (ndx1 ndy1 ndx2 ndy2) (ellipse-normal-radii* ellipse)
    (let* ((center (ellipse-center-point ellipse))
           (cx (point-x center))
           (cy (point-y center))
           (tr (make-transformation ndx2 ndx1 ndy2 ndy1 cx cy))
           (circle (untransform-region tr ellipse))
           ;; we need an extra minus sign because the rotation
           ;; convention for Postscript differs in chirality from the
           ;; abstract CLIM convention; we do a reflection
           ;; transformation to move the coordinates to the right
           ;; handedness, but then the sense of positive rotation is
           ;; backwards, so we need this reflection for angles.  --
           ;; CSR, 2005-08-01
           (start-angle (- (or (ellipse-end-angle circle) 0)))
           (end-angle (- (or (ellipse-start-angle circle) (* -2 pi)))))
      (write-string (if filled "true " "false ") stream)
      (write-angle stream (if (< end-angle start-angle)
                              (+ end-angle (* 2 pi))
                              end-angle))
      (write-angle stream start-angle)
      (write-transformation* stream ndx2 ndx1 ndy2 ndy1 cx cy)
      (format stream "pe~%"))))

;;;;
;;; Postscript output utilities
(defmacro with-graphics-state ((stream) &body body)
  `(invoke-with-graphics-state ,stream
    (lambda () ,@body)))

(defun postscript-save-graphics-state (stream)
  (push (copy-list (first (slot-value stream 'graphics-state-stack)))
        (slot-value stream 'graphics-state-stack))
  (when (stream-drawing-p stream)
    (format (sheet-mirror stream) "gsave~%")))

(defun postscript-restore-graphics-state (stream)
  (pop (slot-value stream 'graphics-state-stack))
  (when (stream-drawing-p stream)
    (format (sheet-mirror stream) "grestore~%")))

(defun invoke-with-graphics-state (stream continuation)
  (postscript-save-graphics-state stream)
  (funcall continuation)
  (postscript-restore-graphics-state stream))


;;; Postscript path functions

(defgeneric postscript-add-path (stream medium region)
  (:documentation
   "Adds REGION (if it is a path) or its boundary (if it is an area)
   to the current path of STREAM."))

(defmethod postscript-add-path (stream medium (region (eql +nowhere+)))
  (declare (ignore stream)))

(defmethod postscript-add-path (stream medium (region standard-region-union))
  (map-over-region-set-regions (lambda (region)
                                 (postscript-add-path stream medium region))
                               region))

(defmethod postscript-add-path (stream medium (region standard-region-intersection))
  (format stream "gsave~%")
  (loop for subregion in (region-set-regions region)
        do (format stream "newpath~%")
        (postscript-add-path stream medium subregion)
        (format stream "clip~%"))
  (format stream "clippath false upath~%")
  (format stream "grestore~%")
  (format stream "uappend~%"))

;;; Primitive paths
(defmethod postscript-add-path (stream medium (polygon polygon))
  (let ((points (polygon-points polygon))
        (tr (sheet-native-transformation (medium-sheet medium))))
    (let ((x0 (point-x (first points)))
          (y0 (point-y (first points))))
      (with-transformed-position (tr x0 y0)
        (moveto* stream x0 y0)))
    (loop for point in (rest points)
       do (let ((x1 (point-x point))
                (y1 (point-y point)))
            (with-transformed-position (tr x1 y1)
              (lineto* stream x1 y1))))
    (format stream "closepath~%")))

(defmethod postscript-add-path (stream medium (ellipse ellipse))
  (let* ((tr (sheet-native-transformation (medium-sheet medium)))
         (ellipse (transform-region tr ellipse)))
    (put-ellipse stream ellipse t)))

(defmethod postscript-add-path (stream medium (rs climi::standard-rectangle-set))
  (map-over-region-set-regions
   (lambda (r) (postscript-add-path stream medium r))
   rs))

;;; Graphics state

(defgeneric postscript-set-graphics-state (stream medium kind))

(defvar *postscript-graphics-states*
  '((:line-style . medium-line-style)
    (:color . medium-ink)
    (:clipping-region . medium-clipping-region)
    (:text-style . medium-text-style)))

(defun postscript-current-state (medium kind)
  (funcall (cdr (assoc kind *postscript-graphics-states*))
           medium))

(defmacro postscript-saved-state (medium kind)
  `(getf (postscript-medium-graphics-state ,medium) ,kind))

(defun postscript-actualize-graphics-state (stream medium &rest kinds)
  "Sets graphics parameters named in STATES."
  (loop for kind in (cons :clipping-region kinds)
        ;; every drawing function depends on clipping region
        ;;
        ;; KLUDGE: clipping-region MUST be actualized first due to its
        ;; dirty dealing with graphics state. -- APD, 2002-02-11
        unless (eql (postscript-current-state medium kind)
                    (postscript-saved-state medium kind))
        do (postscript-set-graphics-state stream medium kind)
           (setf (postscript-saved-state medium kind)
                 (postscript-current-state medium kind))))

;;; Line style
(defconstant +postscript-line-joints+ '(:miter 0
                                        :round 1
                                        :bevel 2
                                        :none 0))

(defconstant +postscript-line-caps+ '(:butt 0
                                      :round 1
                                      :square 2 ; extended butt caps
                                      :no-end-point 0))

(defconstant +postscript-default-line-dashes+ '(30 30))

(defconstant +normal-line-width+ (/ 2.0 3.0))

(defun line-style-scale (line-style medium)
  (let ((unit (line-style-unit line-style)))
    (ecase unit
      (:normal +normal-line-width+)
      (:point 1)
      (:coordinate (multiple-value-bind (x y)
                       (transform-distance (medium-transformation medium) 0.71 0.71)
                     (sqrt (+ (expt x 2) (expt y 2))))))))

(defmethod line-style-effective-thickness
    (line-style (medium postscript-medium))
  (* (line-style-thickness line-style)
     (line-style-scale line-style medium)))

(defun medium-line-thickness (medium)
  (line-style-effective-thickness (medium-line-style medium) medium))

(defmethod postscript-set-graphics-state (stream medium
                                          (kind (eql :line-style)))
  (let* ((line-style (medium-line-style medium))
         (scale (line-style-scale line-style medium)))
    (write-number stream (* scale (line-style-thickness line-style)))
    (format stream "setlinewidth ~A setlinejoin ~A setlinecap~%"
            (getf +postscript-line-joints+
                  (line-style-joint-shape line-style))
            (getf +postscript-line-caps+
                  (line-style-cap-shape line-style)))
    (let ((dashes (line-style-dashes line-style)))
      (format stream "[")
      (mapc (lambda (l) (write-number stream (* scale l)))
            (if (eq dashes 't)
                +postscript-default-line-dashes+
                dashes))
      (format stream "] 0 setdash~%"))))

;;; Color
(defgeneric medium-color-rgb (medium ink))

(defmethod medium-color-rgb (medium (ink clime:indirect-ink))
  ;; If foreground/background doesn't resolve properly it is a bug in core
  ;; system. We could have masked it with the following code. --jd 2018-09-27
  #+ (or)
  (alexandria:switch (ink)
    (+foreground-ink+ (medium-color-rgb (medium-foreground medium)))
    (+background-ink+ (medium-color-rgb (medium-background medium)))
    (otherwise (medium-color-rgb (clime:indirect-ink-ink ink))))
  (medium-color-rgb medium (clime:indirect-ink-ink ink)))

(defmethod medium-color-rgb (medium (ink color))
  (declare (ignore medium))
  (color-rgb ink))

(defmethod postscript-set-graphics-state (stream medium (kind (eql :color)))
  (multiple-value-bind (r g b)
      (medium-color-rgb medium (medium-ink medium))
    (write-number stream r)
    (write-number stream g)
    (write-number stream b)
    (format stream "setrgbcolor~%")))

;;; Clipping region
(defgeneric postscript-set-clipping-region (stream medium region))

(defmethod postscript-set-clipping-region (stream medium region)
  (format stream "newpath~%")
  (postscript-add-path stream medium region)
  (format stream "clip~%"))

(defmethod postscript-set-clipping-region (stream medium (region (eql +everywhere+)))
  (declare (ignore stream)))

(defmethod postscript-set-clipping-region (stream medium (region (eql +nowhere+)))
  (format stream "newpath 0 0 moveto closepath clip~%"))

(defmethod postscript-set-graphics-state (stream medium
                                          (kind (eql :clipping-region)))
  ;; FIXME: There is no way to enlarge clipping path. Current code
  ;; does only one level of saving graphics state, so we can restore
  ;; and save again GS to obtain an initial CP. It is ugly, but I see
  ;; no other way now. -- APD, 2002-02-11
  (postscript-restore-graphics-state (medium-sheet medium))
  (postscript-save-graphics-state (medium-sheet medium))
  (postscript-set-clipping-region stream
                                  medium
                                  (medium-clipping-region medium)))


;;; Medium drawing functions

;;; FIXME: the following methods should share code with POSTSCRIPT-ADD-PATH

(defmethod medium-draw-point* ((medium postscript-medium) x y)
  (let ((stream (medium-drawable medium))
        (tr (sheet-native-transformation (medium-sheet medium)))
        (radius (/ (medium-line-thickness medium) 2)))
    (with-graphics-state ((medium-sheet medium))
      (postscript-actualize-graphics-state stream medium :color)
      (format stream "newpath~%")
      (with-transformed-position (tr x y)
        (write-coordinates stream x y))
      (write-number stream radius)
      (format stream "0 360 arc~%")
      (format stream "fill~%"))))

(defmethod medium-draw-points* ((medium postscript-medium) coord-seq)
  (let ((stream (medium-drawable medium))
        (tr (sheet-native-transformation (medium-sheet medium)))
        (radius (/ (medium-line-thickness medium) 2)))
    (with-graphics-state ((medium-sheet medium))
      (postscript-actualize-graphics-state stream medium :color)
      (map-repeated-sequence 'nil 2
                             (lambda (x y)
                               (format stream "newpath~%")
                               (with-transformed-position (tr x y)
                                 (write-coordinates stream x y))
                               (write-number stream radius)
                               (format stream "0 360 arc~%")
                               (format stream "fill~%"))
                             coord-seq))))

(defmethod medium-draw-line* ((medium postscript-medium) x1 y1 x2 y2)
  (let ((stream (medium-drawable medium))
        (tr (sheet-native-transformation (medium-sheet medium))))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath ")
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (put-line* stream x1 y1 x2 y2)))
    (format stream "stroke~%")))

(defmethod medium-draw-lines* ((medium postscript-medium) coord-seq)
  (let ((stream (medium-drawable medium))
        (tr (sheet-native-transformation (medium-sheet medium))))
    (with-graphics-state ((medium-sheet medium))
      (postscript-actualize-graphics-state stream medium :line-style :color)
      (format stream "newpath~%")
      (map-repeated-sequence 'nil 4
                             (lambda (x1 y1 x2 y2)
                               (with-transformed-position (tr x1 y1)
                                 (with-transformed-position (tr x2 y2)
                                   (put-line* stream x1 y1 x2 y2))))
                             coord-seq)
      (format stream "stroke~%"))))

(defmethod medium-draw-polygon*
    ((medium postscript-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (let ((stream (medium-drawable medium))
        (tr (sheet-native-transformation (medium-sheet medium))))
    (with-graphics-state ((medium-sheet medium))
      (postscript-actualize-graphics-state stream medium :line-style :color)
      (format stream "newpath~%")
      (let ((command "moveto"))
        (map-repeated-sequence 'nil 2
                               (lambda (x y)
                                 (with-transformed-position (tr x y)
                                   (write-coordinates stream x y))
                                 (format stream "~A~%"
                                         command)
                                 (setq command "lineto"))
                               coord-seq))
      (when closed
        (format stream "closepath~%"))
      (format stream (if filled "fill~%" "stroke~%")))))

(defmethod medium-draw-rectangle*
    ((medium postscript-medium) x1 y1 x2 y2 filled)
  (let ((stream (medium-drawable medium))
        (tr (sheet-native-transformation (medium-sheet medium))))
    (with-graphics-state ((medium-sheet medium))
      (postscript-actualize-graphics-state stream medium :line-style :color)
      (format stream "newpath~%")
      (with-transformed-position (tr x1 y1)
        (with-transformed-position (tr x2 y2)
          (put-rectangle* stream x1 y1 x2 y2)))
      (format stream (if filled "fill~%" "stroke~%")))))

(defmethod medium-draw-rectangles*
    ((medium postscript-medium) position-seq filled)
  (assert (evenp (length position-seq)))
  (let ((stream (medium-drawable medium))
        (tr (sheet-native-transformation (medium-sheet medium))))
    (with-graphics-state ((medium-sheet medium))
      (postscript-actualize-graphics-state stream medium :line-style :color)
      (format stream "newpath~%")
      (map-repeated-sequence 'nil 4
                             (lambda (x1 y1 x2 y2)
                               (with-transformed-position (tr x1 y1)
                                 (with-transformed-position (tr x2 y2)
                                   (put-rectangle* stream x1 y1 x2 y2))))
                             position-seq)
      (format stream (if filled "fill~%" "stroke~%")))))

(defmethod medium-draw-ellipse* ((medium postscript-medium) center-x center-y
				 radius1-dx radius1-dy radius2-dx radius2-dy
				 start-angle end-angle filled)
  (let* ((stream (medium-drawable medium))
         (tr (sheet-native-transformation (medium-sheet medium)))
         (ellipse (transform-region
                   tr
                   (make-ellipse* center-x center-y
                                  radius1-dx radius1-dy radius2-dx radius2-dy
                                  :start-angle start-angle
                                  :end-angle end-angle))))
    (with-graphics-state ((medium-sheet medium))
      (postscript-actualize-graphics-state stream medium :line-style :color)
      (format stream "newpath~%")
      (put-ellipse stream ellipse filled)
      (format stream (if filled "fill~%" "stroke~%")))))

(defun medium-font (medium)
  (text-style-mapping (port medium) (medium-merged-text-style medium)))

(defun %font-name-postscript-name (font-name)
  (etypecase font-name
    (clim-postscript-font:postscript-device-font-name
     (let ((font-info (clim-postscript-font:get-font-info font-name)))
       (unless font-info
         (error "Unknown font: ~S" font-info))
       (clim-postscript-font:font-info-name font-info)))
    (clim-postscript-font:postscript-font-name
     (concatenate 'string (clim-postscript-font:font-name-name font-name) "-iso"))))

(defmethod postscript-set-graphics-state (stream medium
                                          (kind (eql :text-style)))
  (let* ((font-name (medium-font medium))
         (font (%font-name-postscript-name font-name))
         (size (clim-postscript-font:font-name-size font-name)))
    (pushnew font (slot-value (medium-sheet medium) 'document-fonts)
             :test #'string=)
    (format stream "/~A findfont ~D scalefont setfont~%"
	    font
	    size))) ;### evil hack.

(defun postscript-escape-char (char)
  (case char
    (#\Linefeed "\\n")
    (#\Return "\\r")
    (#\Tab "\\t")
    (#\Backspace "\\b")
    (#\Page "\\f")
    (#\\ "\\\\")
    (#\( "\\(")
    (#\) "\\)")
    (t (if (standard-char-p char)
           (string char)
           (format nil "\\~3,'0O" (char-code char))))))

(defun postscript-escape-string (string)
  (apply #'concatenate 'string
         (map 'list #'postscript-escape-char string)))

(defmethod medium-draw-text* ((medium postscript-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (setq string (if (characterp string)
                   (make-string 1 :initial-element string)
                   (subseq string start end)))
  (let ((sheet-transformation (sheet-native-transformation (medium-sheet medium)))
        (medium-transformation (medium-transformation medium)))
    (let ((file-stream (medium-drawable medium)))
      (postscript-actualize-graphics-state file-stream medium :color :text-style)
      (with-graphics-state ((medium-sheet medium))
        (multiple-value-bind (total-width total-height
                                          final-x final-y baseline)
            (let* ((font-name (medium-font medium))
                   (font (clim-postscript-font:font-name-metrics-key font-name))
                   (size (clim-postscript-font:font-name-size font-name)))
              (clim-postscript-font:text-size-in-font font size string 0 nil))
          (declare (ignore final-x final-y))
          ;; Only one line?
          (let ((x (ecase align-x
                     (:left x)
                     (:center (- x (/ total-width 2)))
                     (:right (- x total-width))))
                (y (ecase align-y
                     (:top (+ y baseline))
                     (:center (+ y baseline (- (/ total-height 2))))
                     (:baseline y)
                     (:bottom (+ y (- total-height baseline))))))
            (multiple-value-bind (mxx mxy myx myy tx ty)
                (climi::get-transformation (clim:compose-transformations
                                            sheet-transformation
                                            medium-transformation))
              (format file-stream "[~,3F ~,3F ~,3F ~,3F ~,3F ~,3F] concat~%"
                      mxx mxy myx myy tx ty))
            (moveto* file-stream x y)
            (format file-stream "[~,3F ~,3F ~,3F ~,3F ~,3F ~,3F] concat~%"
                    1 0 0 -1 0 0)
            (format file-stream "(~A) show~%" (postscript-escape-string string))))))))
