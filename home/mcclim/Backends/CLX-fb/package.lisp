;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-clx-fb
    (:use :clim :clim-lisp :clim-backend :clim-clx :mcclim-render-extensions)
  (:import-from :climi
                #:+alt-key+
                ;;
                #:port-text-style-mappings
                #:port-lookup-mirror
                #:port-register-mirror
                #:port-event-process
                #:port-grafts
                #:%%sheet-native-transformation
                #:%%set-sheet-native-transformation
                ;;
                #:clamp
                #:get-environment-variable
                #:pixmap-sheet
                #:port-lookup-sheet
                #:port-unregister-mirror
                #:port-pointer-sheet
                #:map-repeated-sequence
                #:pixmap-mirror
                #:do-sequence
                #:with-double-buffering
                #:with-transformed-position
                #:with-transformed-positions
                #:with-medium-options
                ;;
                #:pixmap
                #:top-level-sheet-mixin
                #:unmanaged-sheet-mixin
                #:top-level-sheet-pane
                #:unmanaged-top-level-sheet-pane
                #:menu-frame
                ;;
                #:frame-managers        ;used as slot
                #:top-level-sheet       ;used as slot
                #:medium-device-region
                #:draw-image
                #:height                ;this seems bogus
                #:width                 ;dito
                #:coordinate=
                #:get-transformation
                ;;
                #:medium-miter-limit
                ;; classes:
                #:mirrored-pixmap
                #:window-destroy-event
                #:device-font-text-style
                ;;
                #:make-medium)
   (:import-from :mcclim-render-internals
                  #:render-medium-mixin
                  #:render-port-mixin
                  #:image-mirror-image
                  #:image-sheet-mixin
                  #:image-pixmap-mixin
                  #:image-pixmap-mixin
                  #:image-mirror-mixin)
   (:import-from :clim-clx
                 #:CLX-PORT-DISPLAY
                 #:clx-medium
                 #:initialize-clx
                 #:clx-port-screen
                #:clx-graft
                #:clx-port-window
                #:sheet-xmirror
                #:sheet-direct-xmirror
                )
  (:import-from :climi #:standard-port))
