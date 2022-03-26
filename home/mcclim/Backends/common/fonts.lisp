(in-package #:climi)

(defclass font-rendering-medium-mixin ()
  ()
  (:documentation "Mixed in when the medium text-style-mapping returns
a font implementing the protocol defined below."))

(defgeneric open-font (port font-designator)
  (:documentation "Opens font for a port"))

(defgeneric climb:font-face (font))
(defgeneric climb:font-size (font))

(defgeneric climb:font-character-width (font character)
  (:method (font character)
    (let* ((codes (climb:font-string-glyph-codes font (string character)))
           (code (alexandria:first-elt codes)))
      (assert (alexandria:length= 1 codes))
      (climb:font-glyph-dx font code)))
  (:documentation "Returns width of the character. Character may be composed of
many codepoints, but argument must constitute exactly one character."))

(defgeneric climb:font-string-width (font string &key start end)
  (:method (font string &key start end)
    (alexandria:if-let ((character-width (climb:font-fixed-width font))
                        (glyph-sequence (climb:font-string-glyph-codes font string :start start :end end)))
      (* character-width (length glyph-sequence))
      (values (climb:font-text-extents font string :start start :end end))))
  (:documentation "Returns a width of the string."))

(defgeneric climb:font-string-glyph-codes (font string &key start end)
  (:method (font string &key (start 0) (end (length string)))
    (map 'vector #'char-code (subseq string start end)))
  (:documentation "Converts string to a sequence of glyph codes. Some characters
are composed of many codepoints – it is not guaranteed that length of the string
and the length of resulting sequence are equal."))

(defgeneric climb:font-glyph-code-char (font code)
  (:method (font code)
    (code-char code)))

(defun line-bbox (font glyph-codes align-x)
  (loop
     for code across glyph-codes
     with origin-x fixnum = 0
     with origin-y fixnum = 0
     with xmin = most-positive-fixnum
     with ymin = most-positive-fixnum
     with xmax = most-negative-fixnum
     with ymax = most-negative-fixnum
     as glyph-left fixnum =   (+ origin-x (climb:font-glyph-left font code))
     as glyph-top fixnum =    (+ origin-y (- (climb:font-glyph-top font code)))
     as glyph-right fixnum =  (+ origin-x (climb:font-glyph-right font code))
     as glyph-bottom fixnum = (+ origin-y (- (climb:font-glyph-bottom font code)))
     do
       (alexandria:minf xmin glyph-left)
       (alexandria:minf ymin glyph-top)
       (alexandria:maxf xmax glyph-right)
       (alexandria:maxf ymax glyph-bottom)
       (incf origin-x (climb:font-glyph-dx font code))
       (incf origin-y (climb:font-glyph-dy font code))
     finally
       (case align-x
         (:center
          (let ((width/2 (/ (- xmax xmin) 2)))
            (setf xmin (- width/2))
            (setf xmax (+ width/2))))
         (:right
          (let ((width (- xmax xmin)))
            (setf xmin (- width))
            (setf xmax 0))))
       (return (values xmin ymin xmax ymax origin-x origin-y))))

(defgeneric climb:font-text-extents (font string &key start end align-x align-y direction)
  (:method (font string &key start end align-x align-y direction)
    (declare (ignore direction))
    (when (alexandria:emptyp string)
      (values 0 0 0 0 0 0))
    (let* ((ascent (climb:font-ascent font))
           (descent (climb:font-descent font))
           (line-height (+ ascent descent))
           (xmin most-positive-fixnum)
           (ymin most-positive-fixnum)
           (xmax most-negative-fixnum)
           (ymax most-negative-fixnum)
           (dx 0)
           (dy 0)
           (current-y 0)
           (current-dx 0))
      (dolines (line (subseq string start end))
        (multiple-value-bind (xmin* ymin* xmax* ymax* dx* dy*)
            (if (alexandria:emptyp line)
                (values 0 0 0 0 0 0)
                (line-bbox font (climb:font-string-glyph-codes font line) align-x))
          (case align-y
            (:top
             (let ((height (- ymax* ymin*))
                   (ymin* (- ascent (abs ymin*))))
               (minf ymin (+ current-y ymin*))
               (maxf ymax (+ current-y (+ ymin* height)))))
            (:center
             (let ((height/2 (/ (+ current-y (- ymax* ymin*)) 2)))
               (minf ymin (- height/2))
               (maxf ymax (+ height/2))))
            (:bottom
             (let ((height (- ymax* ymin*))
                   (ymax* (- ymax* descent)))
               (minf ymin (- (- ymax* height) current-y))
               (maxf ymax ymax*)))
            (:baseline*
             (minf ymin (- ymin* current-y))
             (maxf ymax ymax*))
            (otherwise
             (minf ymin (+ current-y ymin*))
             (maxf ymax (+ current-y ymax*))))
          (minf xmin xmin*)
          (maxf xmax xmax*)
          (maxf dx dx*)
          (maxf dy (+ current-y dy*))
          (incf current-y (climb:font-leading font))
          (setf current-dx dx*)))
      (return-from climb:font-text-extents
        (values
         ;; text bounding box
         xmin ymin xmax ymax
         ;; text-bounding-rectangle
         0 #|x0|# (climb:font-ascent font) #|y0|# dx (+ dy line-height)
         ;; line properties (ascent, descent, line gap)
         (climb:font-ascent font)
         (climb:font-descent font)
         (- (climb:font-leading font)
            (+ (climb:font-ascent font)
               (climb:font-descent font)))
         ;; cursor-dx cursor-dy
         current-dx dy))))
  (:documentation "Function computes text extents as if it were drawn with a
specified font. It returns two distinct extents: first is an exact pixel-wise
bounding box. The second is a text bounding box with all its bearings. Text may
contain newlines, if it doesn't linegap should be nil. Cursor advance is
returned as the last two values.

Width and height are relative to the position [-top, left]. For right-to-left
direction left will be probably a negative number with the width being close to
its absolute value. All other values are relative to the postion
origin. Coordinate system is in the fourth quadrant (same as sheet coordinates).

Returned values:

xmin ymin xmax ymax
left top width height ascent descent linegap
cursor-dx cursor-dy"))

(defgeneric climb:font-ascent (font))
(defgeneric climb:font-descent (font))

(defgeneric climb:font-leading  (font)
  (:method (font) (* 1.2 (+ (climb:font-ascent font) (climb:font-descent font))))
  (:documentation "Font leading is a vertical space between baselines of a
consecutive lines."))

(defgeneric climb:font-tracking (font)
  (:method (font) 0)
  (:documentation "Font tracking is an additional horizontal space between
consecutive chracters also known as a letterspacing."))

(defgeneric climb:font-fixed-width (font)
  (:method (font) nil)
  (:documentation "Generalized boolean. If the font character width is fixed it
is returned, otherwise returns NIL."))

(defgeneric climb:font-kerning-p (font)
  (:method (font) nil)
  (:documentation "Kerning is a customized advance-width between different pairs
of letters specified in a separate kerning-table."))

(defgeneric climb:font-glyph-width (font code))
(defgeneric climb:font-glyph-height (font code))
(defgeneric climb:font-glyph-top (font code))
(defgeneric climb:font-glyph-left (font code))
(defgeneric climb:font-glyph-bottom (font code))
(defgeneric climb:font-glyph-right (font code))
(defgeneric climb:font-glyph-dx (font code))
(defgeneric climb:font-glyph-dy (font code))

(declaim (inline climb:normalize-font-size))
(defun climb:normalize-font-size (size)
  (cond ((null size)
         (let ((size* (text-style-size *default-text-style*)))
           (etypecase size*
             (number size*)
             (symbol (getf +font-sizes+ size* nil)))))
        ((eq size :smaller)
         (getf +font-sizes+ (text-style-size *default-text-style*) nil))
        ((eq size :larger)
         (getf +font-sizes+ (text-style-size *default-text-style*) nil))
        ((realp size)
         (round (max size 2)))
        ((getf +font-sizes+ size nil))
        (t
         (error "~s is not a valid text style size!" size))))

(defun parse-text-style* (style)
  "Returns complete text-style without NIL components and with numeric size."
  (flet ((new-text-style (family face size)
           (let ((default *default-text-style*))
             (make-text-style (or family (text-style-family default))
                              (or face   (text-style-face   default))
                              (climb:normalize-font-size size)))))
    (cond ((device-font-text-style-p style)
           style)
          ((text-style-p style)
           (multiple-value-bind (family face size)
               (text-style-components style)
             (if (and (realp size)
                      (text-style-components-fully-specified-p
                       family face size))
                 style
                 (new-text-style family face size))))
          ((null style)
           (parse-text-style* *default-text-style*))
          ((and (listp style) (alexandria:length= 3 style))
           (destructuring-bind (family face size) style
             (new-text-style family face size)))
          (t (error "Invalid text style specification ~S." style)))))

(defmethod text-style-ascent (text-style (medium font-rendering-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (climb:font-ascent font)))

(defmethod text-style-descent (text-style (medium font-rendering-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (climb:font-descent font)))

(defmethod text-style-height (text-style (medium font-rendering-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (+ (climb:font-ascent font) (climb:font-descent font))))

(defmethod text-style-character-width (text-style (medium font-rendering-medium-mixin) char)
  (climb:font-character-width (text-style-mapping (port medium) text-style) char))

(defmethod text-style-width (text-style medium)
  (text-style-character-width text-style medium #\M))

(defmethod text-style-fixed-width-p (text-style medium)
  (eql (text-style-family text-style) :fix))

(defgeneric climb:text-bounding-rectangle*
    (medium string &key text-style start end align-x align-y direction)
  (:documentation "Function returns a bounding box of the text for given
text-style, alignment and direction. Direction is derived from the toward-point
argument.

Argument types:
align-x   (member :left :center :right)
align-y   (member :top :baseline :center :baseline* :bottom)
direction (member :ltr :rtl)

Returned values:
xmin ymin xmax ymax."))

(defmethod climb:text-bounding-rectangle*
    ((medium font-rendering-medium-mixin) string
     &key
       text-style
       (start 0) end
       (align-x :left) (align-y :baseline) (direction :ltr)
     &aux (end (or end (length string))))
  (when (= start end)
    (return-from climb:text-bounding-rectangle* (values 0 0 0 0)))
  (let ((text (string string))
        (font (text-style-mapping (clim:port medium)
                                        (clim:merge-text-styles text-style
                                                                (clim:medium-merged-text-style medium)))))
    (multiple-value-bind (xmin ymin xmax ymax)
        (climb:font-text-extents font text :start start :end end
                                 :align-x align-x :align-y align-y :direction direction)
      (values xmin ymin xmax ymax))))

(defmethod climb:text-size
    ((medium font-rendering-medium-mixin) string
     &key text-style (start 0) end
     &aux
       (string (string string))
       (end (or end (length string)))
       (text-style (clim:merge-text-styles text-style
                                           (clim:medium-merged-text-style medium))))
  (when (= start end)
    (return-from climb:text-size (values 0 0 0 0 (clim:text-style-ascent text-style medium))))
  (let ((text (string string))
        (font (text-style-mapping (clim:port medium) text-style)))
    (multiple-value-bind (xmin ymin xmax ymax
                               left top width height
                               ascent descent linegap
                               cursor-dx cursor-dy)
        (climb:font-text-extents font text :start start :end end)
      (declare (ignore xmin ymin xmax ymax left top descent linegap))
      (values width height cursor-dx cursor-dy ascent))))
