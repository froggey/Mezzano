;;; -*- Syntax: Common-lisp; Package: User -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :clim-2 *features*))

(defpackage dwim
  (:use #:cl)
  (:import-from #:clim
                #:present-to-string
                #:presentation-type
                #:present
                #:port
                #:boolean
                #:frame-manager
                #:find-frame-manager
                #:suggest
                #:expression
                #:accept)
  (:shadow #:interactive-stream-p

           #:menu-choose
           #:presentation-under-pointer
           #:presentation-p
           #:presentation-object
           #:presentation-subtypep
           #:presentation-type-p
           #:describe-presentation-type
           #:bounding-rectangle*
           #:redisplay
           #:redisplayable-format
           #:accepting-values
           #:accept-values
           #:accept-variable-values
           #:menu-choose
           #:read-token
           #:input-position
           #:insertion-pointer
           #:input-not-of-required-type
           #:catching-parser-failures
           #:validate-object
           #:with-accept-activation-chars
           #:accept-activation-p
           #:with-accept-blip-chars
           #:accept-blip-p
           #:with-activation-characters
           #:with-blip-characters
           #:completing-from-suggestions
           #:complete-from-sequence
           #:with-presentation-input-context
           #:with-input-context
           #:sheet
           #:accept-values-choose-from-sequence
           #:alist-subset
           #:invisible-object

           #:color-stream-p
           #:with-clipping-from-output
           #:with-underlining
           #:surrounding-output-with-border
           #:%flip
           #:%draw
           #:%erase
           #:%alu
           #:draw-point
           #:draw-line
           #:draw-string
           #:draw-string-image
           #:draw-polygon
           #:draw-triangle
           #:draw-circle
           #:draw-rectangle

           #:window-under-mouse
           #:change-size
           #:stream-line-height
           #:stream-character-width
           #:stream-cursor-position*
           #:stream-set-cursor-position*
           #:stream-viewport
           #:stream-viewport-size

           #:stream-pointer-position*
           #:make-application-frame
           #:window-set-viewport-position*
           #:launch-frame

           #:printing-random-object
           #:with-stack-list
           #:define-command
           #:install-command
           #:define-presentation-to-command-translator
           #:define-presentation-translator
           #:define-presentation-action
           #:define-presentation-type
           #:with-output-as-presentation
           #:with-output-truncation
           #:with-output-recording-enabled
           #:with-output-recording-disabled
           #:with-redisplayable-output
           #:with-character-face
           #:with-text-face
           #:with-character-style
           #:with-character-size
           #:with-character-family
           #:with-text-style

           #:alist-member
           #:command

           #:status-pane
           #:status-line
           #:set-status-line
           #:mouse-documentation-pane
           #:*include-machine-name-in-status-line-p*
           #:*frame-for-status-line*
           #:*time-type*
           #:initialize-status-line
           #:make-status-line
           #:refresh-status-line
           #:noting-progress
           #:note-progress)
  (:export #:present
           #:present-to-string
           #:presentation-type
           #:menu-choose

           #:presentation-under-pointer
           #:presentation-p
           #:presentation-object
           #:presentation-subtypep
           #:presentation-type-p
           #:present-to-string
           #:describe-presentation-type
           #:bounding-rectangle*
           #:redisplay
           #:redisplayable-format
           #:accepting-values
           #:accept-values
           #:accept-variable-values
           #:menu-choose
           #:read-token
           #:input-position
           #:insertion-pointer
           #:input-not-of-required-type
           #:catching-parser-failures
           #:validate-object
           #:with-accept-activation-chars
           #:accept-activation-p
           #:with-accept-blip-chars
           #:accept-blip-p
           #:with-activation-characters
           #:with-blip-characters
           #:completing-from-suggestions
           #:suggest
           #:complete-from-sequence
           #:with-presentation-input-context
           #:with-input-context
           #:sheet
           #:accept-values-choose-from-sequence
           #:alist-subset
           #:invisible-object

           #:color-stream-p
           #:with-clipping-from-output
           #:with-underlining
           #:surrounding-output-with-border
           #:%flip
           #:%draw
           #:%erase
           #:%alu
           #:draw-point
           #:draw-line
           #:draw-string
           #:draw-string-image
           #:draw-polygon
           #:draw-triangle
           #:draw-circle
           #:draw-rectangle

           #:window-under-mouse
           #:change-size
           #:stream-line-height
           #:stream-character-width
           #:stream-cursor-position*
           #:stream-set-cursor-position*
           #:stream-viewport
           #:stream-viewport-size

           #:stream-pointer-position*
           #:stream-set-pointer-position*
           #:make-application-frame
           #:window-set-viewport-position*
           #:launch-frame

           #:printing-random-object
           #:with-stack-list
           #:define-command
           #:install-command
           #:define-presentation-to-command-translator
           #:define-presentation-translator
           #:define-presentation-action
           #:define-presentation-type
           #:with-output-as-presentation
           #:with-output-truncation
           #:with-output-recording-enabled
           #:with-output-recording-disabled
           #:with-redisplayable-output
           #:with-character-face
           #:with-text-face
           #:with-character-style
           #:with-character-size
           #:with-character-family
           #:with-text-style

           #:alist-member
           #:command
           #:expression

           #:status-pane
           #:status-line
           #:set-status-line
           #:mouse-documentation-pane
           #:*include-machine-name-in-status-line-p*
           #:*frame-for-status-line*
           #:*time-type*
           #:initialize-status-line
           #:make-status-line
           #:refresh-status-line
           #:noting-progress
           #:note-progress))

