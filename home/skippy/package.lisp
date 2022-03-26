;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;; $Id: package.lisp,v 1.15 2007/01/05 14:51:36 xach Exp $

(defpackage #:skippy
  (:use #:cl)
  (:export
   ;; data-stream
   #:make-data-stream
   #:height
   #:width
   #:color-table
   #:loopingp
   #:comment
   #:images
   ;; image
   #:*default-delay-time*
   #:make-image
   #:image-data
   ;;data-stream
   ;;width
   ;;height
   #:top-position
   #:left-position
   #:disposal-method
   #:delay-time
   #:transparency-index
   #:make-image-data
   ;; color tables
   #:make-color-table
   #:add-color
   #:find-color
   #:ensure-color
   #:rgb-color
   #:color-rgb
   #:color-table-size
   #:color-table-entry
   #:copy-color-table
   ;; canvas
   #:make-canvas
   #:composite
   #:fill-area
   #:write-canvas
   #:read-canvas
   #:save-canvas
   #:load-canvas
   #:fill-canvas
   #:clip-canvas
   #:clone
   #:flip-horizontal
   #:flip-vertical
   #:rotate-180
   #:scale
   #:pixel-ref
   ;; util
   #:canvas-image
   #:last-image
   #:add-delay
   #:add-image
   #:write-data-stream
   #:output-data-stream
   #:read-data-stream
   #:load-data-stream
   ;; warnings & conditions
   #:skippy-warning
   #:skippy-error
   #:lzw-error
   #:unexpected-value
   #:missing-color-table
   #:color-table-full
   #:signature-error
   ))
