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
;;;; $Id: image.lisp,v 1.8 2008/01/31 20:25:40 xach Exp $

(in-package #:skippy)

(defvar *default-delay-time* 100)

(defun check-image-dimensions (width height)
  (unless (and (typep width 'image-dimension)
               (typep height 'image-dimension))
    (error 'invalid-image-dimensions
           :width width
           :height height)))

(defclass image (canvas)
  ((data-stream
    :initarg :data-stream
    :accessor data-stream
    :documentation "The data stream in which this image occurs.")
   (height
    :initarg :height
    :accessor height)
   (width
    :initarg :width
    :accessor width)
   (image-data
    :initarg :image-data
    :accessor image-data)
   (top-position
    :initarg :top-position
    :accessor top-position
    :documentation
    "The position of the image relative to the top of the logical screen")
   (left-position
    :initarg :left-position
    :accessor left-position
    :documentation
    "The position of the image relative to the left of the logical screen")
   (color-table
    :initarg :color-table
    :accessor color-table
    :documentation "The local color table of the image, if any.")
   (interlacedp
    :initarg :interlacedp
    :accessor interlacedp
    :documentation "Is the image interlaced?")
   (disposal-method
    :initarg :disposal-method
    :accessor disposal-method)
   (delay-time
    :initarg :delay-time
    :accessor delay-time
    :documentation "The time, in hundredths of a second, to wait after
this image before displaying the next image")
   (transparency-index
    :initarg :transparency-index
    :accessor transparency-index
    :documentation "The color table index of the transparent color for
this image. If null, the image has no transparent color."))
  (:default-initargs
   :top-position 0
   :left-position 0
   :color-table nil
   :interlacedp nil
   :height nil
   :width nil
   :disposal-method :unspecified
   :delay-time *default-delay-time*
   :transparency-index nil)
  (:documentation
   "An IMAGE instance represents a graphic within a GIF data
stream. There may be multiple images in the data stream."))

(defmethod print-object ((object image) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "geometry ~Dx~D+~D+~D"
            (width object)
            (height object)
            (left-position object)
            (top-position object))))

(defmethod initialize-instance :after ((image image)
                                       &key data-stream
                                       height width
                                       image-data
                                       color-table
                                       &allow-other-keys)
  (when (eql color-table t)
    (setf (color-table image) (make-color-table)))
  (unless height
    (setf (height image) (height data-stream)
          height (height data-stream)))
  (unless width
    (setf (width image) (width data-stream)
          width (width data-stream)))
  (cond (image-data
         (let ((required-type `(array (unsigned-byte 8)
                                (,(* height width)))))
           (unless (typep image-data required-type)
             (error "Supplied ~S is not of the required type ~A"
                    :image-data required-type))))
        (t
         (setf (image-data image) (make-image-data height width))))
  (when data-stream
    (vector-push-extend image (images data-stream))))
    

(defmethod (setf data-stream) :after (image (data-stream data-stream))
  (unless (slot-boundp image 'height)
    (setf (height image) (height data-stream)))
  (unless (slot-boundp image 'width)
    (setf (width image) (width data-stream)))
  (vector-push-extend image (images data-stream)))

(defgeneric transparentp (image)
  (:method (image)
    (not (null (transparency-index image)))))

(defun canvas-image (canvas)
  (make-instance 'image
                 :height (height canvas)
                 :width (width canvas)
                 :image-data (image-data canvas)))

(defun make-image (&key height width image-data data-stream
                   (top-position 0) (left-position 0)
                   color-table
                   interlacedp
                   (delay-time *default-delay-time*)
                   transparency-index
                   (disposal-method :unspecified))
  (check-image-dimensions width height)
  (make-instance 'image
                 :height height
                 :width width
                 :image-data image-data
                 :data-stream data-stream
                 :top-position top-position
                 :left-position left-position
                 :color-table color-table
                 :interlacedp interlacedp
                 :delay-time delay-time
                 :transparency-index transparency-index
                 :disposal-method disposal-method))
                   
(defmethod clone ((image image))
  (make-instance 'image
                 :height (height image)
                 :width (width image)
                 :image-data (copy-seq (image-data image))
                 :data-stream (data-stream image)
                 :top-position (top-position image)
                 :left-position (left-position image)
                 :color-table (copy-color-table (color-table image))
                 :delay-time (delay-time image)
                 :transparency-index (transparency-index image)
                 :disposal-method (disposal-method image)))

(defmethod wrap-image (image)
  "Return a data stream of the appropriate size that contains IMAGE."
  (make-data-stream :height (height image)
                    :width (width image)
                    :initial-images (list image)))
