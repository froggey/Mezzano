;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(defpackage #:clim-mezzano
    (:use #:clim-lisp #:clim #:clim-backend #:mcclim-render-extensions)
  (:import-from #:climi
                #:port-lookup-mirror
                #:port-register-mirror
                #:port-event-process
                #:port-grafts
                #:%%sheet-native-transformation
                #:%%set-sheet-native-transformation
                ;;
                #:get-environment-variable
                #:pixmap-sheet
                #:port-lookup-sheet
                #:port-unregister-mirror
                #:port-pointer-sheet
                #:pixmap-mirror
                #:with-double-buffering
                #:with-transformed-position
                #:with-transformed-positions
                #:with-medium-options
                ;;
                #:border-pane
                #:pixmap
                #:top-level-sheet-pane
                #:unmanaged-top-level-sheet-pane
                #:menu-frame
                ;;
                #:frame-managers        ;used as slot
                #:top-level-sheet       ;used as slot
                #:medium-device-region
                #:draw-image
                #:coordinate=
                #:get-transformation
                ;;
                ; #:invoke-with-special-choices
                #:medium-miter-limit
                ;; classes:
                #:mirrored-pixmap
                #:window-destroy-event
                ; #:pointer-ungrab-event
                ; #:pointer-motion-hint-event
                #:device-font-text-style
                ;;
                #:make-medium)

  (:import-from #:mcclim-render-internals
                #:%create-mirror-image
                #:render-medium-mixin
                #:render-port-mixin
                #:image-mirror-image
                #:image-sheet-mixin
                #:image-pixmap-mixin
                #:image-pixmap-mixin
                #:image-mirror-mixin)

  (:import-from #:clim-internals
                #:standard-port))

;; Mezzano OS interface package. All of the Mezzano symbols used by
;; the mezzano backend are in this package and all the symbols are
;; exported. So, every reference to a mezzano symbol will use
;; mos:<symbol>. This keeps the symbols separate from any imported
;; clim/mcclim symbols and makes it easy to identify mezzano symbols
;; when reading the code.
(defpackage #:clim-mezzano-os
  (:nicknames #:mos)
  (:use #:mezzano.supervisor
        #:mezzano.gui
        #:mezzano.gui.compositor
        #:mezzano.gui.widgets
        #:mezzano.sync)

  ;; from mezzano.gui
  (:export #:rectangle
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
           #:surface-pixel)

  ;; from mezzano.gui.compositor
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
           #:force-redisplay
           #:window-x
           #:window-y)

  ;; from mezzano.gui.widgets
  (:export #:default-damage-function
           #:default-cursor-function
           #:frame
           #:frame-title
           #:close-button-p
           #:close-button-hover
           #:activep
           #:frame-mouse-event
           #:close-button-clicked
           #:draw-frame
           #:frame-size
           #:resize-frame
           #:text-widget
           #:resize-text-widget
           #:reset
           #:cursor-visible
           #:in-frame-header-p
           #:in-frame-border-p
           #:set-cursor-function)

  ;; select exports from mezzano.supervisor
  (:export #:make-thread

           #:debug-print-line

           #:panic

           #:current-framebuffer
           #:framebuffer-blit
           #:framebuffer-width
           #:framebuffer-height)

  ;; select exports from mezzano.sync
  (:export #:mailbox
           #:make-mailbox
           #:mailbox-send
           #:mailbox-recieve
           #:mailbox-flush))
