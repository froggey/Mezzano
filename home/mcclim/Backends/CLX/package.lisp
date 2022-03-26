;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-xcommon
  (:use :clim :clim-lisp)
  (:export #:keysym-port-mixin
           #:keysym-to-keysym-name
           #:modifier-mapping
           #:keysym-name-to-keysym
           #:x-event-state-modifiers
           #:x-keysym-to-clim-modifiers))

(defpackage :clim-clx
  (:use :clim :clim-lisp :clim-backend)
  (:import-from :alexandria
                #:when-let*)
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
                #:device-transformation
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
                ;; classes
                #:mirrored-pixmap
                #:window-destroy-event
                #:pointer-grab-enter-event
                #:pointer-grab-leave-event
                #:pointer-ungrab-leave-event
                #:pointer-ungrab-enter-event
                #:device-font-text-style
                ;; utils
                #:dolines
                #:maybe-funcall
                #:when-let
                #:if-let)
  (:import-from #:climi
                #:event-listen-or-wait
                #:%sheet-mirror-region
                #:%sheet-mirror-transformation
                #:standard-port)
  (:export
   #:clx-port
   #:clx-render-port
   #:clx-port-display
   #:clx-medium
   #:clx-render-medium
   #:initialize-clx
   #:clx-port-screen
   #:clx-graft
   #:clx-port-window
   #:sheet-xmirror
   #:sheet-direct-xmirror
   #:port-find-all-font-families))
