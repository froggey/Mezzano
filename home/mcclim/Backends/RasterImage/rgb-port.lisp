(in-package :mcclim-raster-image)

;;;
;;; port
;;;

(defclass rgb-image-port (raster-image-port)
  ())

(setf (get :rgb-image :port-type) 'rgb-image-port)
(setf (get :rgb-image :server-path-parser) 'parse-raster-image-server-path)

(defmethod realize-mirror ((port rgb-image-port) sheet)
  (setf (sheet-parent sheet) (graft port))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port sheet mirror)
    (%make-image mirror sheet)))

;;;
;;; Pixmap
;;;

(defclass rgb-image-pixmap (image-pixmap-mixin basic-pane)
  ((region :initform +nowhere+)))

(defmethod port-allocate-pixmap ((port rgb-image-port) sheet width height)
  (let ((pixmap (make-instance 'rgb-image-pixmap
                               :sheet sheet
                               :width width
                               :height height
                               :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port rgb-image-port) pixmap)
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))
