(defpackage :mezzanine.gui
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
           #:bitblt
           #:bitblt-argb-xrgb))

(defpackage :mezzanine.gui.compositor
  (:use :cl :mezzanine.gui)
  (:export #:window-buffer
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
           #:make-window
           #:damage-window
           #:close-window
           #:global-mouse-state
           #:window-activation-event
           #:state))

(defpackage :mezzanine.gui.input-drivers
  (:use :cl))

(defpackage :mezzanine.gui.basic-repl
  (:use :cl)
  (:export #:spawn))
