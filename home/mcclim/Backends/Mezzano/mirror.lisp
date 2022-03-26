(in-package :clim-mezzano)

;;;
;;; fwidth/fheight are width and height including frame
;;; width and height are the interior width and height available to mcclim
;;; dx/dy are the x and y offsets to the interior available to mcclim
;;;
(defclass mezzano-mirror (image-mirror-mixin)
  ((top-levelp :initform nil)
   (fwidth     :initform 0)
   (fheight    :initform 0)
   (width      :initform 0)
   (height     :initform 0)
   (dx         :initform 0)
   (dy         :initform 0)
   (last-abs-x :initform 0)
   (last-abs-y :initform 0)
   (mez-pixels :initform nil)
   (mez-window :initform nil)
   (mez-frame  :initform nil)
   (mez-dirty-region :initform +nowhere+)
   (skip-count :initform 0)))

(defun size-deltas (mez-mirror)
  (with-slots (fwidth fheight width height) mez-mirror
    (values (- fwidth width) (- fheight height))))

(defun resize-mirror (mirror new-width new-height)
  (setf new-width (max 5 new-width))
  (setf new-height (max 5 new-height))
  (with-slots (fwidth fheight width height mez-frame mez-window) mirror
    (when (or (/= width new-width) (/= height new-height))
      (setf fwidth (+ new-width (- fwidth width))
            fheight (+ new-height (- fheight height))
            width new-width
            height new-height)
      (let* ((surface (mos:make-surface fwidth fheight))
             (pixels (mos:surface-pixels surface)))
        (mos:resize-frame mez-frame surface)
        (mos:resize-window mez-window surface :origin :midpoint)
        (setf (slot-value mirror 'mez-pixels) pixels)
        (mos:draw-frame mez-frame)))))

(defmethod %create-mirror-image :after ((mirror mezzano-mirror) width height)
  (resize-mirror mirror width height))

(defgeneric image-mirror-to-mezzano (sheet))

(defmethod image-mirror-to-mezzano ((sheet image-mirror-mixin))
  )

(defun image-mirror-pre-put (mirror mez-pixels dx dy width height dirty-r)
  (declare (type fixnum dx dy))
  (when mez-pixels
    (let* ((pixels  (climi::pattern-array (image-mirror-image mirror)))
           (s-width (array-dimension pixels 1))
           (d-width (array-dimension mez-pixels 1))
           (clip    (make-rectangle* 0 0 (1- width) (1- height))))
      (declare (type (simple-array (unsigned-byte 32) 2) pixels)
               (type (simple-array (unsigned-byte 32) 2) mez-pixels)
               (optimize (speed 3) (safety 0) (debug 0)))
      (map-over-region-set-regions
       (lambda (region)
         (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
             (region-intersection region clip)
           (declare (type fixnum min-x min-y max-x max-y))
           (mcclim-render-internals::do-region-pixels ((s-width si :x1 min-x :x2 max-x
                                                                   :y1 min-y :y2 max-y)
                                                       (d-width di :x1 (+ dx min-x)
                                                                   :y1 (+ dy min-y)))
             (setf (row-major-aref mez-pixels di)
                   (logior #xff000000
                           (the (unsigned-byte 32) (row-major-aref pixels si)))))))
       dirty-r))))

(defmethod image-mirror-to-mezzano ((sheet mezzano-mirror))
  (declare (optimize speed))
  (with-slots (mcclim-render-internals::image-lock
               mcclim-render-internals::dirty-region
               dx dy
               width height
               mez-window
               mez-dirty-region skip-count) sheet
    (when (not (region-equal mez-dirty-region +nowhere+))
      (let ((reg))
        (climi::with-lock-held (mcclim-render-internals::image-lock)
          (setf reg mez-dirty-region)
          (setf mez-dirty-region +nowhere+))
	(when mez-window
	  ;; (debug-format "image-mirror-put ~S ~S ~S ~S ~S"
	  ;;               mez-window dx dy width height)
	  ;; (mos:damage-window
	  ;;  mez-window
	  ;;  dx
	  ;;  dy
	  ;;  width
	  ;;  height)
	  (let ((result +nowhere+))
	    (map-over-region-set-regions
	     (lambda (region)
	       (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
		   (region-intersection region (make-rectangle* 0 0 width height))
		 (let ((width (round (- max-x min-x)))
		       (height (round (- max-y min-y))))
		   (mos:damage-window
		    mez-window
		    (+ dx (round (max 0 min-x)))
		    (+ dy (round (max 0 min-y)))
		    width
		    height))))
	     reg)
	    ))))))

(defmethod climb:port-set-mirror-name ((port mezzano-port) (mirror mezzano-mirror) (name t))
  (setf (mos:frame-title (slot-value mirror 'mez-frame)) name))

(defmethod port-set-mirror-region
    ((port mezzano-port) (mirror mezzano-mirror) mirror-region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) mirror-region
    (with-port-lock (port)
      (resize-mirror mirror
                     (1+ (ceiling (- max-x min-x)))
                     (1+ (ceiling (- max-y min-y))))))
  (mos:draw-frame (slot-value mirror 'mez-frame)))

(defmethod port-set-mirror-transformation
    ((port mezzano-port) (mirror mezzano-mirror) mirror-transformation)
  (unless (slot-value mirror 'top-levelp)
    (multiple-value-bind (x y) (transform-position mirror-transformation 0 0)
      (mos:move-window (slot-value mirror 'mez-window) (floor x) (floor y)))))

(defmethod destroy-mirror ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-mirror sheet)))
    (when (typep mirror 'mezzano-mirror)
      (let ((mez-window (slot-value mirror 'mez-window)))
        (with-port-lock (port)
          (remhash mez-window (slot-value port 'mez-window->sheet))
          (remhash mez-window (slot-value port 'mez-window->mirror)))
        (mos:close-window mez-window)))
    (when (port-lookup-mirror port sheet)
      (port-unregister-mirror port sheet (sheet-mirror sheet)))))

(defmethod port-disable-sheet ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-mirror sheet)))
    (when (and (typep mirror 'mezzano-mirror)
               (eq sheet (port-lookup-sheet port mirror)))
      ;; disabling a top level sheet - close the window and delete mappings
      (let ((mez-window (slot-value mirror 'mez-window)))
        (with-port-lock (port)
          (remhash mez-window (slot-value port 'mez-window->sheet))
          (remhash mez-window (slot-value port 'mez-window->mirror)))
        (mos:close-window mez-window)))))

(defmethod mcclim-render-internals::%mirror-force-output ((mirror mezzano-mirror))
  (with-slots (mcclim-render-internals::image-lock
               mcclim-render-internals::dirty-region
               dx dy
               width height
               mez-pixels
               mez-dirty-region) mirror
    (when mcclim-render-internals::dirty-region
      (climi::with-lock-held (mcclim-render-internals::image-lock)
        (when mcclim-render-internals::dirty-region
          (setf mez-dirty-region
                (region-union mez-dirty-region
                              mcclim-render-internals::dirty-region))
;;          (debug-format "dirty regin ~S" mez-dirty-region)
          (image-mirror-pre-put mirror mez-pixels dx dy width height mez-dirty-region)
          (setf mcclim-render-internals::dirty-region nil))))))
