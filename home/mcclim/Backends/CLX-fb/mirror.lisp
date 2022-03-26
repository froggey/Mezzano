(in-package :clim-clx-fb)

(defclass clx-fb-mirror (image-mirror-mixin)
  ((width :initform 0)
   (height :initform 0)
   (xmirror :initform nil
	    :initarg :xmirror)
   (xlib-image :initform nil)
   (dirty-xr :initform +nowhere+)
   (clx-image :initform nil)
   (gcontext :initform nil)
   (skip-count :initform 0)))

;;; for port
(defmethod mcclim-render-internals::%create-mirror-image :after ((sheet clx-fb-mirror) w h)
  (with-slots (mcclim-render-internals::dirty-region) sheet
    (setf mcclim-render-internals::dirty-region nil))
  ;;(let ((data (climi::pattern-array (image-mirror-image sheet))))
    (with-slots (width height clx-image xlib-image) sheet
      (setf width w
	    height h)
      (setf xlib-image (make-array (list height width)
                                   :element-type '(unsigned-byte 32)
                                   :initial-element #x00FFFFFF))
      (setf clx-image
	    (xlib:create-image :bits-per-pixel 32
			       :data xlib-image
			       :depth 24
			       :width width
			       :height height
			       :format :z-pixmap))))

(defgeneric image-mirror-to-x (sheet))

(defmethod image-mirror-to-x ((sheet image-mirror-mixin))
  )

(defmethod image-mirror-to-x ((sheet xlib:window))
  )

(defun image-mirror-put (width height xmirror gcontext clx-image dirty-r)
  (map-over-region-set-regions
   #'(lambda (region)
       (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
           (region-intersection region
                                (make-rectangle* 0 0 width height))
         (let ((width (round (- max-x min-x)))
               (height (round (- max-y min-y))))
           (when (and xmirror clx-image)
             (xlib::put-image xmirror
                              gcontext
                              clx-image
                              :src-x (round (max min-x 0)) :src-y (round (max min-y 0))
                              :x (round (max min-x 0)) :y (round (max min-y 0))
                              :width  (max 0 (- width (min 0 (- min-x))))
                              :height (max 0 (- height (min 0 (- min-y)))))))))
   dirty-r))

(declaim (inline xlib-image-data-set-pixel))
(defun xlib-image-data-set-pixel (data x y red green blue)
  (setf (aref data y x)
	(dpb blue (byte 8 0)
	     (dpb green (byte 8 8)
		  (dpb red (byte 8 16) 0)))))

(defun image-mirror-pre-put (width height xmirror sheet clx-image xlib-image dirty-r)
  (let* ((pixels (climi::pattern-array (image-mirror-image sheet)))
         (fn (etypecase pixels
               ((simple-array (unsigned-byte 32) 2)
                #'(lambda (region)
                    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                        (region-intersection region (make-rectangle* 0 0 (1- width) (1- height)))
                      (when (and xmirror clx-image)
                        (do ((x min-x)
                             (y min-y (1+ y)))
                            ((> x max-x))
                          (setf (aref xlib-image y x) (ash (aref pixels y x) -8))
                          (when (= y max-y)
                            (incf x)
                            (setf y min-y))))))))))
    (map-over-region-set-regions fn dirty-r)))

(defmethod image-mirror-to-x ((sheet clx-fb-mirror))
  (declare (optimize speed))
  (with-slots (xmirror
               clx-image xlib-image
               mcclim-render-internals::image-lock gcontext
               mcclim-render-internals::dirty-region
               mcclim-render-internals::finished-output
               mcclim-render-internals::updating-p
               width height dirty-xr skip-count)
      sheet
    (when (not (region-equal dirty-xr +nowhere+))
      (let ((reg))
        (clim-sys:with-lock-held (mcclim-render-internals::image-lock)
          (setf reg dirty-xr)
          (setf dirty-xr +nowhere+))
        (image-mirror-put width height xmirror gcontext clx-image reg)))))

(defmethod clim-clx::port-set-mirror-region ((port clx-fb-port) (mirror clx-fb-mirror) mirror-region)
  (clim-clx::port-set-mirror-region port (slot-value mirror 'xmirror) mirror-region))

(defmethod clim-clx::port-set-mirror-transformation
    ((port clx-fb-port) (mirror clx-fb-mirror) mirror-transformation)
  (clim-clx::port-set-mirror-transformation port (slot-value mirror 'xmirror) mirror-transformation))

(defmethod mcclim-render-internals::%mirror-force-output ((mirror clx-fb-mirror))
  (with-slots (mcclim-render-internals::image-lock mcclim-render-internals::dirty-region dirty-xr width height clx-image
                          xlib-image xmirror)
      mirror
    (when mcclim-render-internals::dirty-region
      (clim-sys:with-lock-held (mcclim-render-internals::image-lock)
        (when mcclim-render-internals::dirty-region
          (setf dirty-xr (region-union dirty-xr mcclim-render-internals::dirty-region))
          (image-mirror-pre-put width height xmirror mirror clx-image xlib-image dirty-xr)
          (setf mcclim-render-internals::dirty-region nil))))))
