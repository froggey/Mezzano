;;;; GUI surfaces.
;;;;
;;;; 2D arrays of pixel data.

(in-package :mezzano.gui)

(defstruct (surface
             (:constructor %make-surface)
             (:copier nil))
  (pixels (error "Pixel data not specified."))
  (format (error "Format not specified.")))

(defmethod print-object ((object surface) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~Dx~D ~S"
            (surface-width object) (surface-height object)
            (surface-format object))))

(defun make-surface (width height &key (format :argb32) (initial-colour 0))
  "Create a new surface of the specified WIDTH, HEIGHT and FORMAT.
The surface will be filled with INITIAL-COLOUR, which defaults to fully transparent."
  (check-type format (member :argb32 :a8 :a1 :argb64f))
  (%make-surface :pixels (if (eql format :argb64f)
                             (let ((pixels (make-array (list height width 4)
                                                       :element-type 'short-float))
                                   (a (float (colour-alpha initial-colour) 0.0s0))
                                   (r (float (colour-red initial-colour) 0.0s0))
                                   (g (float (colour-green initial-colour) 0.0s0))
                                   (b (float (colour-blue initial-colour) 0.0s0)))
                               (dotimes (y height)
                                 (dotimes (x width)
                                   (setf (aref pixels y x 0) b
                                         (aref pixels y x 1) g
                                         (aref pixels y x 2) r
                                         (aref pixels y x 3) a)))
                               pixels)
                             (make-array (list height width)
                                     :element-type (ecase format
                                                     (:argb32 '(unsigned-byte 32))
                                                     (:a8 '(unsigned-byte 8))
                                                     (:a1 'bit))
                                     :initial-element initial-colour))
                 :format format))

(defun make-surface-from-array (array &key (format :argb32) premultiplied)
  "Create a new surface with the same dimensions as ARRAY.
If the surface format contains colour components, they will be converted to
premultiplied alpha unless PREMULTIPLIED is true."
  (check-type format (member :argb32 :a8 :a1 :argb64f))
  (let ((pixels (ecase format
                  (:argb32
                   (check-type array (array (unsigned-byte 32) (* *)))
                   (let ((pixels (make-array (array-dimensions array)
                                             :element-type '(unsigned-byte 32))))
                     ;; Fill pixels with premultiplied colour.
                     (dotimes (y (array-dimension array 0))
                       (dotimes (x (array-dimension array 1))
                         (let* ((colour (aref array y x))
                                (alpha (ldb (byte 8 24) colour))
                                (red   (ldb (byte 8 16) colour))
                                (green (ldb (byte 8  8) colour))
                                (blue  (ldb (byte 8  0) colour)))
                           (setf (aref pixels y x) (make-colour-from-octets
                                                    red green blue alpha
                                                    premultiplied)))))
                     pixels))
                  (:a8
                   (check-type array (array (unsigned-byte 8) (* *)))
                   (let ((new (make-array (array-dimensions array)
                                          :element-type '(unsigned-byte 8))))
                     (dotimes (i (array-total-size array))
                       (setf (row-major-aref new i) (row-major-aref array i)))
                     new))
                  (:a1
                   (check-type array (array (unsigned-byte 1) (* *)))
                   (let ((new (make-array (array-dimensions array)
                                          :element-type '(unsigned-byte 1))))
                     (dotimes (i (array-total-size array))
                       (setf (row-major-aref new i) (row-major-aref array i)))
                     new))
                  (:argb64f
                   (check-type array (array short-float (* * 4)))
                   (let ((pixels (make-array (array-dimensions array)
                                             :element-type 'short-float)))
                     ;; Fill pixels with premultiplied colour.
                     (cond (premultiplied
                            (dotimes (i (array-total-size array))
                              (setf (row-major-aref pixels i) (row-major-aref array i))))
                           (t
                            (dotimes (y (array-dimension array 0))
                              (dotimes (x (array-dimension array 1))
                                (let* ((alpha (aref array y x 3))
                                       (red   (aref array y x 2))
                                       (green (aref array y x 1))
                                       (blue  (aref array y x 0)))
                                  (setf (aref pixels y x 3) alpha
                                        (aref pixels y x 2) (* red alpha)
                                        (aref pixels y x 1) (* green alpha)
                                        (aref pixels y x 0) (* blue alpha)))))))
                     pixels)))))
    (%make-surface :pixels pixels
                   :format format)))

(defun copy-surface (surface)
  (%make-surface :pixels (make-array (array-dimensions (surface-pixels surface))
                                     :element-type (array-element-type (surface-pixels surface))
                                     :initial-contents (surface-pixels surface))
                 :format (surface-format surface)))

(defun surface-width (surface)
  "Return the width of SURFACE in pixels."
  (array-dimension (surface-pixels surface) 1))

(defun surface-height (surface)
  "Return the height of SURFACE in pixels."
  (array-dimension (surface-pixels surface) 0))

(defun surface-pixel (surface x y)
  "Get the pixel in SURFACE at (X,Y)."
  (if (eql (surface-pixels surface) :argb64f)
      (make-colour (aref (surface-pixels surface) y x 3)
                   (aref (surface-pixels surface) y x 2)
                   (aref (surface-pixels surface) y x 1)
                   (aref (surface-pixels surface) y x 0)
                   t)
      (aref (surface-pixels surface) y x)))

(defun (setf surface-pixel) (value surface x y)
  "Set the pixel in SURFACE at (X,Y)."
  (cond ((eql (surface-pixels surface) :argb64f)
         (setf (aref (surface-pixels surface) y x 0) (float (colour-blue value) 0.0s0)
               (aref (surface-pixels surface) y x 1) (float (colour-green value) 0.0s0)
               (aref (surface-pixels surface) y x 2) (float (colour-red value) 0.0s0)
               (aref (surface-pixels surface) y x 3) (float (colour-alpha value) 0.0s0))
         value)
        (t
         (setf (aref (surface-pixels surface) y x) value))))

(defun bitblt (mode width height source-surface source-x source-y dest-surface dest-x dest-y)
  "Copy a WIDTHxHEIGHT rectangle of pixels from SOURCE-SURFACE to DEST-SURFACE.
SOURCE-X,SOURCE-Y specify the top-left pixel in the source rectangle,
DEST-X,DEST-Y specify the top-left pixel in the destination rectangle.
MODE can be :SET, :BLEND, :XOR, or a COLOUR-MATRIX.
:SET will replace destination pixels with source pixels.
:BLEND will alpha blend destination pixels with source pixels using the over operator.
:XOR will exclusive-or destination pixels with source pixels.
The :XOR blend mode may interact poorly with pixels that are not fully opaque.
A COLOUR-MATRIX is equivalent to :SET, but will transform the copied pixels
using the matrix.
The rectangle will be clipped so that it is fully inside SOURCE/DEST."
  (etypecase mode
    ((eql :set)
     (2d-array-bitblt
      height width
      (surface-pixels source-surface) source-y source-x
      (surface-pixels dest-surface) dest-y dest-x))
    ((eql :blend)
     (2d-array-bitblt-blend
      height width
      (surface-pixels source-surface) source-y source-x
      (surface-pixels dest-surface) dest-y dest-x))
    ((eql :xor)
     (2d-array-bitblt-xor
      height width
      (surface-pixels source-surface) source-y source-x
      (surface-pixels dest-surface) dest-y dest-x))
    (colour-matrix
     (2d-array-bitblt-matrix
      mode
      height width
      (surface-pixels source-surface) source-y source-x
      (surface-pixels dest-surface) dest-y dest-x))))

(defun bitset (mode width height colour dest-surface dest-x dest-y &optional mask mask-x mask-y)
  "Set a WIDTHxHEIGHT rectangle of pixels in DEST-SURFACE to COLOUR.
DEST-X,DEST-Y specify the top-left pixel in the destination rectangle.
MODE can be :SET, :BLEND, or :XOR.
:SET will replace destination pixels with the colour.
:BLEND will alpha blend destination pixels with source pixels using the over operator.
:XOR will exclusive-or destination pixels with source pixels.
The :XOR blend mode may interact poorly with pixels that are not fully opaque.
If MASK is non-nil then it must be an alpha surface.
When MASK is specified, COLOUR will be component-multiplied with it for each pixel.
The rectangle will be clipped so that it is fully inside MASK/DEST."
  (ecase mode
    (:set
     (cond
       ((not mask)
        (2d-array-bitset
         height width
         colour
         (surface-pixels dest-surface) dest-y dest-x))
       (t (ecase (surface-format mask)
            (:a1
             (2d-array-bitset-mask-1
              height width
              colour
              (surface-pixels mask) mask-y mask-x
              (surface-pixels dest-surface) dest-y dest-x))
            (:a8
             (2d-array-bitset-mask-8
              height width
              colour
              (surface-pixels mask) mask-y mask-x
              (surface-pixels dest-surface) dest-y dest-x))))))
    (:blend
     (cond
       ((not mask)
        (2d-array-bitset-blend
         height width
         colour
         (surface-pixels dest-surface) dest-y dest-x))
       (t (ecase (surface-format mask)
            (:a1
             (2d-array-bitset-blend-mask-1
              height width
              colour
              (surface-pixels mask) mask-y mask-x
              (surface-pixels dest-surface) dest-y dest-x))
            (:a8
             (2d-array-bitset-blend-mask-8
              height width
              colour
              (surface-pixels mask) mask-y mask-x
              (surface-pixels dest-surface) dest-y dest-x))))))
    (:xor
     (cond
       ((not mask)
        (2d-array-bitset-xor
         height width
         colour
         (surface-pixels dest-surface) dest-y dest-x))
       (t (ecase (surface-format mask)
            (:a1
             (2d-array-bitset-xor-mask-1
              height width
              colour
              (surface-pixels mask) mask-y mask-x
              (surface-pixels dest-surface) dest-y dest-x))
            (:a8
             (2d-array-bitset-xor-mask-8
              height width
              colour
              (surface-pixels mask) mask-y mask-x
              (surface-pixels dest-surface) dest-y dest-x))))))))
