;;;; GUI-related packages

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
           #:colour-over
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
  (:export #:name
           #:window
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
           #:window-create-event
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

(defpackage :mezzano.gui.theme
  (:use :cl)
  (:local-nicknames (:gui :mezzano.gui))
  (:export #:*foreground*
           #:*background*
           #:*active-frame*
           #:*active-frame-top*
           #:*inactive-frame*
           #:*inactive-frame-top*
           #:*frame-title*
           #:*filer-lisp-source-code*
           #:*filer-compiled-lisp-code*
           #:*filer-text*
           #:*filer-font*
           #:*filer-media*
           #:*memory-monitor-not-present*
           #:*memory-monitor-free*
           #:*memory-monitor-wired*
           #:*memory-monitor-wired-backing*
           #:*memory-monitor-active*
           #:*memory-monitor-active-writeback*
           #:*memory-monitor-inactive-writeback*
           #:*memory-monitor-page-table*
           #:*memory-monitor-other*
           #:*memory-monitor-mixed*
           #:*memory-monitor-graph-background*
           #:*memory-monitor-graph-tracker*
           #:*memory-monitor-general-area-usage*
           #:*memory-monitor-general-area-alloc*
           #:*memory-monitor-general-area-commit*
           #:*memory-monitor-cons-area-usage*
           #:*memory-monitor-cons-area-alloc*
           #:*memory-monitor-cons-area-commit*
           #:*memory-monitor-pinned-area-usage*
           #:*memory-monitor-wired-area-usage*
           #:*memory-monitor-function-area-usage*
           #:*memory-monitor-wired-function-area-usage*
           #:*desktop-text*
           #:*xterm-background*
           #:set-desktop-background-image
           #:set-desktop-background-colour
           #:set-desktop-text-colour))

(defpackage :mezzano.gui.font
  (:use :cl)
  (:local-nicknames (:sys.int :mezzano.internals))
  (:export #:open-font
           #:name
           #:size
           #:line-height
           #:em-square-width
           #:ascender
           #:glyph-character
           #:glyph-mask
           #:glyph-yoff
           #:glyph-xoff
           #:glyph-advance
           #:character-to-glyph
           #:*default-font*
           #:*default-font-size*
           #:*default-bold-font*
           #:*default-bold-font-size*
           #:*default-monospace-font*
           #:*default-monospace-font-size*
           #:*default-monospace-bold-font*
           #:*default-monospace-bold-font-size*
           #:string-display-width
           #:draw-string
           #:draw-stroked-string))

(defpackage :mezzano.gui.image
  (:use :cl)
  (:export #:load-image
           #:flush-image-cache
           #:transcode-cl-jpeg-buffer))

(defpackage :mezzano.gui.desktop
  (:use :cl)
  (:export #:spawn)
  (:local-nicknames (:gui :mezzano.gui)
                    (:comp :mezzano.gui.compositor)
                    (:theme :mezzano.gui.theme)
                    (:font :mezzano.gui.font))
  (:import-from :mezzano.gui.image
                #:load-image))

(defpackage :mezzano.gui.input-drivers
  (:use :cl))
