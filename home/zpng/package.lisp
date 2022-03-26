;;;
;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
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

(defpackage #:zpng
  (:use #:cl #:salza2)
  (:export
   ;; generic
   #:width
   #:height
   #:rowstride
   #:color-type
   #:samples-per-pixel
   ;; in-memory pngs
   #:png
   #:copy-png
   #:png=
   #:image-data
   #:data-array
   #:write-png
   #:write-png-stream
   ;; row-at-a-time pngs
   #:streamed-png
   #:row-data
   #:start-png
   #:row-data
   #:write-row
   #:rows-written
   #:rows-left
   #:finish-png
   ;; pixel-at-a-time pngs
   #:pixel-streamed-png
   #:write-pixel
   #:pixels-left-in-row
   ;; conditions
   #:zpng-error
   #:invalid-size
   #:invalid-size-width
   #:invalid-size-height
   #:invalid-row-length
   #:insufficient-rows
   #:incomplete-row
   #:too-many-rows
   #:color-type-mismatch))
