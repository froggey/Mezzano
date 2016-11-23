;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
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
           #:make-surface
           #:make-surface-from-array
           #:surface-width
           #:surface-height
           #:surface-pixel))

(defpackage :mezzano.gui.compositor
  (:use :cl :mezzano.gui)
  (:export #:window
           #:window-buffer
           #:width
           #:height
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
           #:global-mouse-state
           #:make-window
           #:with-window
           #:window-close-event
           #:close-window
           #:window-activation-event
           #:state
           #:damage-window
           #:subscribe-notification
           #:unsubscribe-notification
           #:get-window-by-kind
           #:screen-geometry-update
           #:force-redisplay))

(defpackage :mezzano.gui.input-drivers
  (:use :cl))

(defpackage :mezzano.gui.basic-repl
  (:use :cl)
  (:export #:spawn))
