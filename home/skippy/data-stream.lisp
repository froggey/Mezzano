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
;;;; $Id: data-stream.lisp,v 1.11 2007/01/03 22:01:59 xach Exp $

(in-package #:skippy)

(defclass data-stream ()
  ((height
    :initarg :height
    :reader height
    :documentation "The height of the logical screen")
   (width
    :initarg :width
    :reader width
    :documentation "The width of the logical screen")
   (color-table
    :initarg :color-table
    :accessor color-table
    :documentation "The global color table for the data stream (optional)")
   (loopingp
    :initarg :loopingp
    :accessor loopingp)
   (comment
    :initarg :comment
    :accessor comment)
   (images
    :initarg :images
    :reader images
    :documentation "A vector of the images in the data stream"))
  (:default-initargs
   :height (error "~A initarg is required" :height)
   :width (error "~A initarg is required" :width)
   :color-table nil
   :loopingp nil
   :comment nil
   :images (make-array 10 :adjustable t :fill-pointer 0))
  (:documentation
   "A DATA-STREAM instance represents a container for GIF image
data. It defines the logical dimensions of the overall image. It may
contain a color table, which is used if an individual image does not
provide its own color table."))

(defmethod initialize-instance :after ((data-stream data-stream) 
                                       &key color-table
                                       &allow-other-keys)
  (when (eql color-table t)
    (setf (color-table data-stream) (make-color-table))))

(defmethod print-object ((object data-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "geometry ~Dx~D, ~D image~:*~P"
            (width object)
            (height object)
            (length (images object)))))

(defun last-image (data-stream)
  (let* ((images (images data-stream))
         (i (fill-pointer images)))
    (unless (zerop i)
      (aref images (1- i)))))

(defun add-delay (delay data-stream)
  (let ((image (last-image data-stream)))
    (when image
      (incf (delay-time image) delay))))

(defun check-dimensions (data-stream image)
  (when (or (< (height data-stream) (height image))
            (< (width data-stream) (width image)))
    (skippy-warn "Image ~A is larger than its containing data stream ~A, ~
                  output may not display properly"
                  image data-stream)))

(defun add-image (image data-stream)
  (setf (data-stream image) data-stream)
  (check-dimensions data-stream image)
  (vector-push-extend image (images data-stream)))


(defun make-data-stream (&key height width color-table loopingp comment
                         initial-images)
  (let ((data-stream (make-instance 'data-stream
                                    :height height
                                    :width width
                                    :color-table color-table
                                    :loopingp loopingp
                                    :comment comment)))
    (dolist (image initial-images data-stream)
      (add-image image data-stream))))
