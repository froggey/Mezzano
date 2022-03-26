(cl:in-package #:mcclim-raster-image)

(defclass raster-image-top-level-pane (;;sheet-mute-input-mixin
                                       sheet-mute-repainting-mixin
                                       image-sheet-mixin
                                       mirrored-sheet-mixin
                                       unmanaged-top-level-sheet-pane)
  ())
