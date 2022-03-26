(in-package :pngload)

(defun get-channel-count ()
  (ecase (color-type *png-object*)
    (:truecolour 3)
    (:truecolour-alpha 4)
    (:indexed-colour 1)
    (:greyscale-alpha 2)
    (:greyscale 1)))

(defun get-sample-bytes ()
  (max 1 (/ (bit-depth *png-object*) 8)))

(defun get-pixel-bytes ()
  (* (get-sample-bytes) (get-channel-count)))

(defun get-scanline-bytes (width)
  (ceiling (* (bit-depth *png-object*)
              (get-channel-count)
              width)
           8))

(defun (setf color-type) (color-type)
  (setf (slot-value *png-object* 'color-type)
        (ecase color-type
          (0 :greyscale)
          (2 :truecolour)
          (3 :indexed-colour)
          (4 :greyscale-alpha)
          (6 :truecolour-alpha))))

(defun (setf compression-method) (compression-method)
  (setf (slot-value *png-object* 'compression-method)
        (ecase compression-method
          (0 :zlib))))

(defun (setf interlace-method) (interlace-method)
  (setf (slot-value *png-object* 'interlace-method)
        (ecase interlace-method
          (0 :null)
          (1 :adam7))))

(defun (setf filter-method) (filter-method)
  (setf (slot-value *png-object* 'filter-method)
        (ecase filter-method
          (0 :standard))))

(defun (setf gamma) (gamma)
  (setf (slot-value *png-object* 'gamma)
        (float (/ gamma 100000))))

(defun (setf rendering-intent) (rendering-intent)
  (setf (slot-value *png-object* 'rendering-intent)
        (ecase rendering-intent
          (0 :perceptual)
          (1 :relative-colorimetric)
          (2 :saturation)
          (3 :absolute-colorimetric))))

(defun (setf pixel-size) (pixel-size)
  (destructuring-bind (x y unit) pixel-size
    (setf (slot-value *png-object* 'pixel-size)
          (list :x x :y y :unit (ecase unit (0 :unknown) (1 :meter))))))

(defun (setf last-modified) (time)
  (destructuring-bind (year month day hour minute second) time
    (setf (slot-value *png-object* 'last-modified)
          (encode-universal-time second minute hour day month year))))

(defun (setf text) (text)
  (destructuring-bind (keyword string &optional language translated-keyword) text
    (push
     (if (and language translated-keyword)
         (list language keyword translated-keyword string)
         (cons keyword string))
     (slot-value *png-object* 'text))))
