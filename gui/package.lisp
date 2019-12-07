;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.gui
  (:use :cl)
  (:export #:clamp
           #:rectangle
           #:make-rectangle
           #:rectangle-x
           #:rectangle-y
           #:rectangle-width
           #:rectangle-height
           #:bitblt
           #:bitset
           #:*default-foreground-colour*
           #:*default-background-colour*
           #:colour
           #:+colour-alpha-bits+
           #:+colour-red-bits+
           #:+colour-green-bits+
           #:+colour-blue-bits+
           #:make-colour
           #:make-colour-from-octets
           #:colour-equal
           #:colour-red
           #:colour-red-as-octet
           #:colour-green
           #:colour-green-as-octet
           #:colour-blue
           #:colour-blue-as-octet
           #:colour-alpha
           #:colour-alpha-as-octet
           #:surface
           #:surface-p
           #:make-surface
           #:make-surface-from-array
           #:surface-format
           #:surface-pixels
           #:surface-width
           #:surface-height
           #:surface-pixel
           #:colour-matrix
           #:colour-matrix-p
           #:make-colour-matrix
           #:colour-matrix-element
           #:simd-coloud
           #:make-simd-colour
           #:simd-colour-elements
           #:colour-lerp
           #:colour-matrix-matrix-multiply
           #:colour-matrix-multiply))

(defpackage :mezzano.gui.compositor
  (:use :cl :mezzano.gui)
  (:export #:window
           #:window-buffer
           #:x
           #:y
           #:width
           #:height
           #:event
           #:key-event
           #:key-scancode
           #:key-releasep
           #:key-key
           #:key-modifier-state
           #:submit-key
           #:mouse-event
           #:mouse-button-state
           #:mouse-button-change
           #:mouse-x-position
           #:mouse-y-position
           #:mouse-x-motion
           #:mouse-y-motion
           #:submit-mouse
           #:submit-mouse-absolute
           #:global-mouse-state
           #:make-window
           #:with-window
           #:window-close-event
           #:close-window
           #:window-activation-event
           #:state
           #:damage-window
           #:begin-window-drag
           #:resize-request-event
           #:resize-event
           #:resize-origin
           #:resize-window
           #:move-event
           #:move-window
           #:set-window-data
           #:grab-cursor
           #:make-mouse-cursor
           #:register-mouse-cursor
           #:quit-event
           #:subscribe-notification
           #:unsubscribe-notification
           #:get-window-by-kind
           #:screen-geometry-update
           #:screen-update
           #:force-redisplay
           #:window-x
           #:window-y
           #:*screensaver-spawn-function*
           #:*screensaver-time*
           #:postprocess-matrix
           #:*enable-live-resize*))

(defpackage :mezzano.gui.input-drivers
  (:use :cl))
