(defpackage #:clim-demo.image-transform-demo
  (:use #:clim-lisp)
  (:export #:image-transform-demo))

(in-package #:clim-demo.image-transform-demo)

(clim:define-application-frame image-transform-demo ()
  ((image :initarg :image
          :accessor image-transform-demo/image))
  (:menu-bar nil)
  (:panes (image-demo :application
                      :display-function 'display-image-demo
                      :scroll-bars :both
                      :incremental-redisplay t)
          (rot-slider :slider
                      :value 0
                      :show-value-p nil
                      :orientation :horizontal
                      :min-value 0
                      :max-value (* pi 2)
                      :value-changed-callback #'slider-updated-callback
                      :drag-callback #'slider-updated-callback)
          (x-slider :slider
                    :value 0
                    :show-value-p nil
                    :orientation :horizontal
                    :min-value 0
                    :max-value 200
                    :value-changed-callback #'slider-updated-callback
                    :drag-callback #'slider-updated-callback)
          (y-slider :slider
                    :value 0
                    :show-value-p nil
                    :orientation :horizontal
                    :min-value 0
                    :max-value 200
                    :value-changed-callback #'slider-updated-callback
                    :drag-callback #'slider-updated-callback)
          (scale-slider :slider
                        :value 1
                        :show-value-p nil
                        :orientation :horizontal
                        :min-value 0.1
                        :max-value 5
                        :value-changed-callback #'slider-updated-callback
                        :drag-callback #'slider-updated-callback)
          (x-skew-slider :slider
                         :value 0
                         :show-value-p nil
                         :orientation :horizontal
                         :min-value -1
                         :max-value 1
                         :value-changed-callback #'slider-updated-callback
                         :drag-callback #'slider-updated-callback)
          (y-skew-slider :slider
                         :value 0
                         :show-value-p nil
                         :orientation :horizontal
                         :min-value -1
                         :max-value 1
                         :value-changed-callback #'slider-updated-callback
                         :drag-callback #'slider-updated-callback))
  (:layouts (default (clim:vertically ()
                       (14/20 (clim:labelling (:label "Test Image"
					       :align-x :center
					       :label-alignment :top)
				image-demo))
		       (clim:horizontally ()
			 (clim:vertically ()
			   (1/20 (clim:labelling (:label "Rotate")
				   rot-slider))
			   (1/20 (clim:labelling (:label "Translate X")
				   x-slider)))
			 (clim:vertically ()
			   (1/20 (clim:labelling (:label "Translate Y")
				   y-slider))
			   (1/20 (clim:labelling (:label "Scale")
				   scale-slider)))
			 (clim:vertically ()
			   (1/20 (clim:labelling (:label "Skew X")
				   x-skew-slider))
			   (1/20 (clim:labelling (:label "Skew Y")
				   y-skew-slider))))))))

(defmethod initialize-instance :after ((obj image-transform-demo) &key)
  (setf (image-transform-demo/image obj)
        (clim:make-pattern-from-bitmap-file (merge-pathnames #p"images/kitten.jpg"
                                                             (asdf:system-source-directory :clim-examples)))))

(defun slider-updated-callback (gadget value)
  (declare (ignore gadget value))
  (let ((frame clim:*application-frame*))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'image-demo))))

(defun make-skew-transformation (x-skew y-skew)
  (clim:make-transformation 1 (tan x-skew)
                            (tan y-skew) 1
                            0 0))

(defun display-image-demo (frame stream)
  (clim:updating-output (stream)
    (let* ((image (image-transform-demo/image frame))
           (rotation (clim:gadget-value (clim:find-pane-named frame 'rot-slider)))
           (x-translation (clim:gadget-value (clim:find-pane-named frame 'x-slider)))
           (y-translation (clim:gadget-value (clim:find-pane-named frame 'y-slider)))
           (scale (clim:gadget-value (clim:find-pane-named frame 'scale-slider)))
           (x-skew (clim:gadget-value (clim:find-pane-named frame 'x-skew-slider)))
           (y-skew (clim:gadget-value (clim:find-pane-named frame 'y-skew-slider)))
           (tr (clim:compose-transformations
                (clim:make-translation-transformation x-translation y-translation)
                (clim:compose-transformations
                 (clim:make-scaling-transformation scale scale)
                 (clim:compose-transformations
                  (clim:make-rotation-transformation* rotation
                                                      (/ (clim:pattern-width image) 2)
                                                      (/ (clim:pattern-height image) 2))
                  (make-skew-transformation x-skew y-skew))))))
      (clim:with-drawing-options (stream :transformation tr)
        (clim:draw-design stream image)
        (clim:draw-rectangle* stream 0 0 (clim:pattern-width image) (clim:pattern-height image) :filled nil :ink clim:+blue+)
        ;; We don't display text here if using the Truetype font
        ;; renderer, since other font renderers doesn't support text
        ;; transform.
        #+mcclim-ffi-freetype
        (clim:with-text-size (stream 60)
          (clim:draw-text* stream "Foo abcdefgh" 100 100))))))

(defun image-transform-demo ()
  (let ((frame (clim:make-application-frame 'image-transform-demo)))
    (clim:run-frame-top-level frame)))
