;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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
           #:bitset
           #:bitset-argb-xrgb-mask-8
           #:bitset-argb-xrgb-mask-1
           #:bitset-argb-xrgb
           #:bitblt
           #:bitblt-argb-xrgb
           #:bitxor
           #:*default-foreground-colour*
           #:*default-background-colour*))

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
           #:screen-geometry-update))

(defpackage :mezzano.gui.input-drivers
  (:use :cl))

(defpackage :mezzano.gui.basic-repl
  (:use :cl)
  (:export #:spawn))
