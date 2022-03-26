(in-package :mcclim-render-internals)

(defclass image-pixmap-mixin (image-sheet-mixin
                              mirrored-sheet-mixin
                              mirrored-pixmap
                              basic-sheet
                              design)
  ())

(defmethod sheet-medium ((pixmap image-pixmap-mixin))
  (pixmap-medium pixmap))

(defmethod sheet-direct-mirror ((pixmap image-pixmap-mixin))
  (climi::port-lookup-mirror (port pixmap) pixmap))
