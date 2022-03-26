(defpackage :mcclim-render
  (:nicknames #:clim-render)
  (:use)
  (:export
   ;; colors
   #:color->octets
   ;; image
   #:image
   #:draw-image*
   #:medium-draw-image*
   ;; image ops
   #:make-image
   #:clone-image
   #:copy-image
   #:blend-image
   #:crop-image
   #:fill-image
   ;; two dimensional array image
   #:two-dim-array-image
   #:rgba-image
   #:gray-image))

(defpackage :mcclim-render-extensions
  (:use)
  (:export
   ;; colors
   #:octet
   #:color-octet-xor
   #:octet-mult
   #:octet-blend-function
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:color-value->octet
   #:color-octet->value
   #:rgba->rgb
   #:rgba->gray
   #:rgba->gray-alpha
   #:rgba->alpha
   #:rgb->rgba
   #:rgb->gray
   #:rgb->alpha
   #:gray->rgba
   #:gray->rgb
   #:gray->alpha))

(defpackage :mcclim-render-internals
  (:use #:clim #:clim-lisp #:mcclim-render #:mcclim-render-extensions)
  (:import-from :clim-internals
                #:standard-color
                #:named-color
                #:standard-flipping-ink
                #:%transparent-ink
                #:standard-opacity
                #:opacity-value
                #:pattern
                #:indexed-pattern
                #:rectangular-tile
                #:rectangular-tile-design
                #:transformed-design
                #:transformed-design-design
                #:transformed-design-transformation
                #:with-transformed-position
                #:in-compositum
                #:out-compositum
                #:over-compositum
                #:compositum-ink
                #:compositum-mask
                #:compositum-foreground
                #:compositum-background
                #:def-grecording
                #:defmethod*
                #:output-record-position
                #:defrecord-predicate
                #:with-standard-rectangle*
                #:coordinate=
                #:if-supplied
                ;; backend
                #:destroy-mirror
                #:realize-mirror
                #:mirrored-pixmap
                #:port-register-mirror
                #:port-lookup-mirror
                #:port-lookup-sheet
                #:pixmap-mirror
                #:pixmap-medium)
  (:import-from :mcclim-truetype
                #:glyph-info
                #:font-glyph-info
                #:font-generate-glyph
                #:glyph-info-left
                #:glyph-info-top
                #:glyph-info-advance-width
                #:glyph-info-advance-height
                #:glyph-info-advance-width*
                #:glyph-info-advance-height*
                #:glyph-info-pixarray
                #:glyph-pixarray
                #:ensure-gethash
                #:invoke-with-truetype-path-restart
                #:*truetype-font-path*
                #:*family-names*
                #:zpb-ttf-font-loader
                #:*zpb-font-lock*
                #:*fontconfig-faces*
                #:*families/faces*
                #:truetype-device-font-name
                #:fontconfig-font-name
                #:make-truetype-device-font-name
                #:make-fontconfig-font-name
                #:truetype-font
                #:truetype-face
                #:zpb-ttf-font-units->pixels)
  (:import-from :clim-backend
                #:port-set-mirror-region
                #:port-set-mirror-transformation)
  (:export #:render-medium-mixin
           #:render-port-mixin
           #:image-mirror-image
           #:image-sheet-mixin
           #:image-pixmap-mixin
           #:image-pixels
           #:image-pixmap-mixin
           #:image-mirror-mixin
           #:opticl-rgb-image-pixels
           ))
