(defpackage :mezzanine.gui
  (:use :cl)
  (:export #:clamp
           #:rectangle
           #:rectangle-x
           #:rectangle-y
           #:rectangle-width
           #:rectangle-height
           #:bitset-argb-xrgb-mask-8
           #:bitset-argb-xrgb-mask-1
           #:bitblt-argb-xrgb))

(defpackage :mezzanine.gui.compositor
  (:use :cl :mezzanine.gui)
  (:export #:key-event
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
           #:global-mouse-state))

(defpackage :mezzanine.gui.input-drivers
  (:use :cl))
