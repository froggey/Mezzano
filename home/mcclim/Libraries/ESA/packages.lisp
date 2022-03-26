;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004-2006 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2006-2008 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Package definitions for ESA.

(defpackage :esa-utils
  (:use :clim-lisp :c2mop :clim)
  (:shadowing-import-from :clim-lisp #:describe-object)
  (:shadowing-import-from :c2mop
                          #:defclass
                          #:defgeneric
                          #:defmethod
                          #:standard-generic-function
                          #:standard-method
                          #:standard-class)
  (:import-from #:alexandria
		#:once-only
		#:with-gensyms)
  (:export #:with-gensyms
	   #:once-only
           #:unlisted
           #:fully-unlisted
           #:listed
           #:list-aref
           #:letf
           #:display-string
           #:object-equal
           #:object=
           #:no-upper-p
           #:case-relevant-test
           #:with-keywords-removed
           #:invoke-with-dynamic-bindings-1
           #:invoke-with-dynamic-bindings
           #:maptree
           #:subtype-compatible-p
           #:capitalize
           #:ensure-array-size
           #:values-max-min
           #:retaining-value
           #:format-sym
           #:build-menu #:define-menu-table
           #:observable-mixin
           #:add-observer #:remove-observer
           #:observer-notified #:notify-observers
           #:name-mixin #:name
           #:subscriptable-name-mixin #:subscripted-name #:subscript #:subscript-generator
           #:mode #:modual-class
           #:available-modes
           #:mode-directly-applicable-p #:mode-applicable-p
           #:mode-enabled-p
           #:enabled-modes
           #:nonapplicable-mode
           #:change-class-for-enabled-mode
           #:change-class-for-disabled-mode
           #:enable-mode #:disable-mode
           #:add-default-modes #:remove-default-modes))

(defpackage :esa
  (:use :clim-lisp :clim :esa-utils :clim-extensions)
  (:export #:*esa-instance*
           #:buffers #:esa-current-buffer #:current-buffer
           #:windows #:esa-current-window #:current-window
           #:*previous-command*
           #:*minibuffer* #:minibuffer #:minibuffer-pane #:display-message
           #:with-minibuffer-stream
           #:esa-pane-mixin #:previous-command
           #:info-pane #:master-pane
           #:esa-frame-mixin #:recordingp #:executingp
           #:*esa-abort-gestures* #:*current-gesture* #:*command-processor*
           #:unbound-gesture-sequence #:gestures
           #:command-processor #:instant-macro-execution-mixin
           #:asynchronous-command-processor #:command-loop-command-processor
           #:dead-key-merging-command-processor
           #:overriding-handler #:directly-processing-p #:process-gesture #:process-gestures-or-command
           #:command-for-unbound-gestures
           #:*extended-command-prompt*
           #:define-esa-top-level #:esa-top-level #:simple-command-loop
           #:convert-to-gesture #:gesture-name
	   #:invoke-with-help-stream #:with-help-stream
           #:set-key
           #:find-applicable-command-table
           #:esa-command-parser
           #:esa-partial-command-parser
           #:esa-command-table

           #:gesture-matches-gesture-name-p #:meta-digit
           #:proper-gesture-p
           #:universal-argument #:meta-minus

           ;; General commands
           #:global-esa-table
           #:com-quit #:com-extended-command

           ;; Help commands
           #:help-table #:help-menu-table
           #:com-describe-key-briefly #:com-where-is
           #:com-describe-bindings
           #:com-describe-key #:com-describe-command
           #:com-apropos-command

           ;; Keyboard macro commands
           #:keyboard-macro-table #:keyboard-macro-menu-table
           #:com-start-macro #:com-end-macro
           #:com-call-last-macro))

(defpackage :esa-buffer
  (:use :clim-lisp :clim :esa :esa-utils)
  (:export #:frame-make-buffer-from-stream #:make-buffer-from-stream
           #:frame-save-buffer-to-stream #:save-buffer-to-stream
           #:filepath #:name #:needs-saving #:file-write-time #:file-saved-p
           #:esa-buffer-mixin
           #:frame-make-new-buffer #:make-new-buffer
           #:read-only-p))

(defpackage :esa-io
  (:use :clim-lisp :clim :esa :esa-buffer :esa-utils)
  (:export #:frame-find-file #:find-file
           #:frame-find-file-read-only #:find-file-read-only
           #:frame-set-visited-file-name #:set-visited-filename
           #:check-buffer-writability
           #:frame-save-buffer #:save-buffer
           #:frame-write-buffer #:write-buffer
           #:buffer-writing-error #:buffer #:filepath
           #:filepath-is-directory
           #:esa-io-table #:esa-io-menu-table
           #:com-find-file #:com-find-file-read-only
           #:com-read-only #:com-set-visited-file-name
           #:com-save-buffer #:com-write-buffer))
