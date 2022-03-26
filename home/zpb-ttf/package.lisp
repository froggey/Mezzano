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
;;; $Id: package.lisp,v 1.22 2006/03/27 21:57:37 xach Exp $

(defpackage #:zpb-ttf
  (:use #:cl)
  (:export
   ;; font loader
   #:open-font-loader
   #:close-font-loader
   #:with-font-loader
   #:glyph-count
   #:name-entry-value
   #:find-name-entry
   #:value
   ;; font typographic
   #:italic-angle
   #:underline-thickness
   #:underline-position
   #:fixed-pitch-p
   #:units/em
   #:ascender
   #:descender
   #:line-gap
   ;; other font attributes
   #:postscript-name
   #:full-name
   #:family-name
   #:subfamily-name
   #:all-kerning-pairs
   #:glyph-exists-p
   #:index-glyph
   #:find-glyph
   ;; shared between font-loader and glyph
   #:bounding-box
   #:xmin
   #:ymin
   #:xmax
   #:ymax
   ;; control points
   #:x
   #:y
   #:on-curve-p
   ;; glyph contours
   #:contour-count
   #:contour
   #:contours
   #:do-contours
   #:explicit-contour-points
   #:do-contour-segments
   #:do-contour-segments*
   ;; glyph other
   #:code-point
   #:font-index
   ;; glyph typographic
   #:advance-width
   #:left-side-bearing
   #:right-side-bearing
   #:kerning-offset
   #:string-bounding-box))

