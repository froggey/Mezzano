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
;;;; $Id: example.lisp,v 1.7 2006/12/11 15:49:17 xach Exp $

(defpackage #:skippy-example
  (:use #:cl #:skippy)
  (:export #:example1 #:example2 #:example3))

(in-package #:skippy-example)

(defun example1 ()
  (let* ((height 100)
         (width 100)
         (data-stream (make-data-stream :height height
                                        :width width
                                        :color-table t))
         (image (make-image :height height :width width))
         (red (ensure-color (rgb-color #xFF #x00 #x00)
                            (color-table data-stream)))
         (white (ensure-color (rgb-color #xFF #xFF #xFF)
                              (color-table data-stream))))
    (add-image image data-stream)
    (fill (image-data image) white)
    (dotimes (i (truncate height 2))
      (let* ((start (* i width 2))
             (end (+ start width)))
        (fill (image-data image) red :start start :end end)))
    (output-data-stream data-stream #p"example1.gif")))

(defun example2 ()
  (let* ((height 9)
         (width 99)
         (color-table (make-color-table))
         (data-stream (make-data-stream :height height
                                        :width width
                                        :color-table color-table))
         (gray (ensure-color #xCCCCCC color-table))
         (white (ensure-color #xFFFFFF color-table))
         (black (ensure-color #x000000 color-table))
         (bg (make-image :data-stream data-stream
                         :width width :height height
                         :image-data (make-image-data height width
                                                      :initial-element gray)))
         (sprite-data (make-image-data 3 3)))
    (flet ((hatch-data (data a b)
             (dotimes (i (length data))
               (setf (aref data i) (if (zerop (mod i 2)) a b)))))
      (hatch-data sprite-data white black)
      (hatch-data (image-data bg) white gray)
      (dotimes (i 128)
        (add-color (random #xFFFFF) color-table))
      (dotimes (i 96)
        (let ((image (make-image :height 3
                                 :width 3
                                 :image-data sprite-data
                                 :top-position 3
                                 :delay-time 10
                                 :disposal-method :restore-previous
                                 :transparency-index white
                                 :left-position i)))
          (add-image image data-stream)))
      (setf (loopingp data-stream) t)
      (output-data-stream data-stream #p"example2.gif"))))

(defun example3 ()
  (let* ((height 100)
         (width 100)
         (color-count 256)
         (color-table (make-color-table))
         (data-stream (make-data-stream :color-table color-table
                                        :loopingp t
                                        :height height
                                        :width width)))
    (dotimes (i color-count)
      (add-color (rgb-color (random 256) (random 256) (random 256))
                 color-table))
    (dotimes (i color-count)
      (let* ((top (random height))
             (left (random width))
             (h (1+ (random (- height top))))
             (w (1+ (random (- width left))))
             (image (make-image :height h
                                :width w
                                :data-stream data-stream
                                :top-position top
                                :left-position left
                                :image-data (make-image-data w h
                                                             :initial-element (random color-count))
                                :delay-time 5)))
        (add-image image data-stream)))
    (output-data-stream data-stream #p"example3.gif")))
