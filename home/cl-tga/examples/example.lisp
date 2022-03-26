(defpackage #:cl-tga-example
  (:use :cl)
  (:export #:example))

(dolist (i '(cl-tga cl-glut cl-opengl cl-glu))
  (require i))

(in-package #:cl-tga-example)

(defclass cl-tga-example-window (glut:window)
  ((texture :accessor window-texture
	    :initform nil))
  (:default-initargs :width 400
		     :height 400
		     :title "cl-tga Example"
		     :mode '(:single :rgb)))

(defun example ()
  (glut:display-window (make-instance 'cl-tga-example-window)))

(defmethod glut:display-window :before ((window cl-tga-example-window))
  (let ((image (time (tga:read-tga (merge-pathnames "examples/trucks.tga"
						    (asdf:system-source-directory (asdf:find-system :cl-tga)))))))

    (setf (window-texture window) (car (gl:gen-textures 1)))
    (gl:bind-texture :texture-2d (window-texture window))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d :texture-2d 0 :rgba
		     (tga:image-width image) (tga:image-height image)
		     0 (ecase (tga:image-channels image)
			 (3 :bgr)
			 (4 :bgra))
		     :unsigned-byte (tga:image-data image)))

  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((window cl-tga-example-window))
  (gl:clear :color-buffer-bit)
;  (gl:color 1 1 1)
  (gl:with-pushed-matrix
    (gl:rotate 30 1 1 0)
    (when (window-texture window)
      (gl:enable :texture-2d)
      (gl:bind-texture :texture-2d (window-texture window))
      (gl:with-primitive :quads
	#|
		(gl:vertex -.5 -.5) (gl:tex-coord 1 1)
		(gl:vertex .5 -.5) (gl:tex-coord 1 0)
		(gl:vertex .5 .5) (gl:tex-coord 0 0)
		(gl:vertex -.5 .5) (gl:tex-coord 0 1) 
	|#
	(gl:tex-coord 0 0) (gl:vertex -1 -.5)
	(gl:tex-coord 1 0) (gl:vertex 1 -.5)
	(gl:tex-coord 1 1)(gl:vertex 1 .5) 
	(gl:tex-coord 0 1) (gl:vertex -1 .5)
	)))
  (gl:flush))

(defmethod glut:reshape ((window cl-tga-example-window) w h)
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1 20)
  (glu:look-at 0 0 2 0 0 0 0 1 0)
  (gl:matrix-mode :modelview))

(defmethod glut:close ((window cl-tga-example-window))
  (when (window-texture window)
    (gl:delete-textures (list (window-texture window)))))
