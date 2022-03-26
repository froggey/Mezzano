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
;;;; $Id: canvas.lisp,v 1.15 2006/12/28 16:35:01 xach Exp $

(in-package #:skippy)

(deftype canvas-data ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype canvas-index ()
  `(mod ,most-positive-fixnum))

(deftype palette-index ()
  '(unsigned-byte 8))

(defclass canvas ()
  ((height
    :initarg :height
    :reader height)
   (width
    :initarg :width
    :reader width)
   (image-data
    :initarg :image-data
    :accessor image-data))
  (:default-initargs
   :height (error "~S required" :height)
   :width (error "~S required" :width)))

(defun make-image-data (width height &key
                        (initial-element 0)
                        initial-contents)
  (if initial-contents
      (make-array (* width height)
                  :element-type 'octet
                  :initial-contents initial-contents)
      (make-array (* width height)
                  :element-type 'octet
                  :initial-element initial-element)))

(defun make-canvas (&key width height
                    image-data (initial-element 0) initial-contents)
  (unless (and height width)
    (error "~S and ~S required" :height :width))
  (unless image-data
    (setf image-data (make-image-data width height
                                      :initial-element initial-element
                                      :initial-contents initial-contents)))
  (make-instance 'canvas
                 :height height
                 :width width
                 :image-data image-data))
                 

(defmethod print-object ((canvas canvas) stream)
  (print-unreadable-object (canvas stream :type t :identity t)
    (format stream "geometry ~Dx~D" (width canvas) (height canvas))))

(defmethod initialize-instance :after ((canvas canvas) &key height width)
  (unless (slot-boundp canvas 'image-data)
    (setf (image-data canvas) (make-array (* height width)
                                          :initial-element 0
                                          :element-type 'octet))))

(defun clip (xmin0 ymin0 xmax0 ymax0
             xmin1 ymin1 xmax1 ymax1)
  (flet ((clamp (min val max)
           (cond ((< val min) min)
                 ((> val max) max)
                 (t val))))
    (values (clamp xmin0 xmin1 xmax0)
            (clamp ymin0 ymin1 ymax0)
            (clamp xmin0 xmax1 xmax0)
            (clamp ymin0 ymax1 ymax0))))

(defun clip-canvas (source dest &key (sx 0) (sy 0) (dx 0) (dy 0)
                   (width (width source)) (height (height source)))
  "Return new dx,dy and sx,sy and width,height values to use when
clipping SOURCE to fit within the bounds of DEST."
  (let* ( ;; destination
         (xmin0 0)
         (ymin0 0)
         (xmax0 (width dest))
         (ymax0 (height dest))
         ;; source
         (xmin1 (- dx sx))
         (ymin1 (- dy sy))
         (xmax1 (+ xmin1 (width source)))
         (ymax1 (+ ymin1 (height source)))
         ;; source offset
         (xmin2 dx)
         (ymin2 dy)
         (xmax2 (+ xmin2 width))
         (ymax2 (+ ymin2 height)))
    ;; clip source offset to source
    (multiple-value-bind (xmin3 ymin3 xmax3 ymax3)
        (clip xmin1 ymin1 xmax1 ymax1
              xmin2 ymin2 xmax2 ymax2)
      ;; clip that against dest
      (multiple-value-bind (xmin4 ymin4 xmax4 ymax4)
          (clip xmin0 ymin0 xmax0 ymax0
                xmin3 ymin3 xmax3 ymax3)
        (values xmin4 ymin4
                (- xmin4 xmin1)
                (- ymin4 ymin1)
                (- xmax4 xmin4)
                (- ymax4 ymin4))))))
         
(defun composite (source dest
                  &key (sx 0) (sy 0)
                  (dx 0) (dy 0)
                  (width (width source)) (height (height source)))
  (multiple-value-bind (dx* dy* sx* sy* width* height*)
      (clip-canvas source dest
                   :sx sx :sy sy
                   :dx dx :dy dy
                   :width width :height height)
    (when (or (zerop width*)
              (zerop height*))
      (return-from composite))
    (let ((source-data (image-data source))
          (source-width (width source))
          (dest-data (image-data dest))
          (dest-width (width dest)))
      (declare (type canvas-data source-data dest-data)
               (type canvas-index source-width dest-width))
      (loop repeat height*
            for source-start from (+ (* source-width sy*) sx*) by source-width
            for dest-start   from (+ (* dest-width   dy*) dx*) by dest-width
            for source-end   from (+ source-start width*) by source-width
            do (replace dest-data source-data :start1 dest-start
                        :start2 source-start :end2 source-end))
      dest)))





;;; Save and load canvases

(defvar *canvas-magic*
  (make-array 3 :element-type '(unsigned-byte 8)
              :initial-contents (list #x89 #xAD #x17)))

(defvar *file-format-version* 1)

(defun write-u32 (i stream)
  (write-byte (logand #xFF (ash i -24)) stream)
  (write-byte (logand #xFF (ash i -16)) stream)
  (write-byte (logand #xFF (ash i  -8)) stream)
  (write-byte (logand #xFF (ash i   0)) stream))

(defun read-u32 (stream)
  (logand #xFFFFFFFF
          (+ (ash (read-byte stream) 24)
             (ash (read-byte stream) 16)
             (ash (read-byte stream)  8)
             (ash (read-byte stream)  0))))

(defun write-canvas (canvas stream)
  (write-sequence *canvas-magic* stream)
  (write-byte *file-format-version* stream)
  (write-u32 (width canvas) stream)
  (write-u32 (height canvas) stream)
  (write-sequence (image-data canvas) stream)
  t)

(defun read-canvas (stream)
  (dotimes (i (length *canvas-magic*))
    (let ((byte (read-byte stream)))
      (when (/= byte (aref *canvas-magic* i))
        (error "Bad magic in stream"))))
  (let ((version (read-byte stream)))
    (when (/= version *file-format-version*)
      (error "Unsupported version in stream -- expected ~D, read ~D"
             *file-format-version* version)))
  (let ((width (read-u32 stream))
        (height (read-u32 stream)))
    (when (>= (* width height) array-total-size-limit)
      (error "Canvas dimensions (~Dx~D) too large to load"
             width height))
    (let ((canvas (make-instance 'canvas :height height :width width)))
      (read-sequence (image-data canvas) stream)
      canvas)))

(defun save-canvas (canvas file &key (if-exists :supersede))
  (with-open-file (stream file :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists if-exists)
    (write-canvas canvas stream))
  (probe-file file))

(defun load-canvas (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8)
                          :direction :input)
    (read-canvas stream)))


;;; Useful operations

(defun fill-canvas (canvas palette-index)
  (declare (type palette-index palette-index)
           (optimize (speed 3)))
  (let ((data (image-data canvas)))
    (declare (type canvas-data data))
    (fill data palette-index)
    (values)))

(defmethod clone ((canvas canvas))
  (make-instance 'canvas
                 :height (height canvas)
                 :width (width canvas)
                 :image-data (copy-seq (image-data canvas))))

(defmethod flip-horizontal (canvas)
  "Horizontally mirror the image data of CANVAS."
  (loop repeat (height canvas)
        with data = (image-data canvas)
        with width = (width canvas)
        for i = 0 then (+ i width)
        for j = (1- width) then (+ j width)
        do (loop for m from i
                 for n downfrom j
                 while (< m n) do
                 (rotatef (aref data m) (aref data n))))
  canvas)

(defmethod rotate-180 (canvas)
  "Does a 180-degree rotation of the image data of CANVAS."
  (setf (image-data canvas) (nreverse (image-data canvas)))
  canvas)

(defmethod flip-vertical (canvas)
  "Vertically mirror the image data of CANVAS."
  (rotate-180 canvas)
  (flip-horizontal canvas))

(defmethod scale ((canvas canvas) factor)
  "Integer scale CANVAS and return it as a new canvas." 
  (let* ((width (* (width canvas) factor))
         (height (* (height canvas) factor))
         (new (make-instance 'canvas :width width :height height)))
    (dotimes (y (height canvas) new)
      (dotimes (x (width canvas))
        (let ((p (pixel-ref canvas x y))
              (xf (* x factor))
              (yf (* y factor)))
          (dotimes (i factor)
            (dotimes (j factor)
              (setf (pixel-ref new (+ xf i) (+ yf j)) p))))))))

(defmethod fill-area (canvas palette-index &key
                      (x 0)
                      (y 0)
                      (width (width canvas))
                      (height (height canvas)))
  (let ((xmin0 x)
        (ymin0 y)
        (xmax0 (+ x width))
        (ymax0 (+ y height))
        (xmin1 0)
        (ymin1 0)
        (xmax1 (width canvas))
        (ymax1 (height canvas)))
    (multiple-value-bind (xmin2 ymin2 xmax2 ymax2)
        (clip xmin0 ymin0 xmax0 ymax0
              xmin1 ymin1 xmax1 ymax1)
      (let ((w (- xmax2 xmin2))
            (h (- ymax2 ymin2)))
        (when (and (plusp w) (plusp h))
          (loop with dest-width = (width canvas)
                with data = (image-data canvas)
                with start = (+ xmin2 (* ymin2 dest-width))
                for i = start then (+ i dest-width)
                for j = (+ start w) then (+ j dest-width)
                repeat h
                do (fill data palette-index :start i :end j)))))))

(defmethod pixel-ref (canvas x y)
  (aref (image-data canvas) (+ (* y (width canvas)) x)))

(defmethod (setf pixel-ref) (new-value canvas x y)
  (setf (aref (image-data canvas) (+ (* y (width canvas)) x)) new-value))

(defun deinterlaced-image-data (canvas)
  (let* ((source (image-data canvas))
         (dest (copy-seq source))
         (width (width canvas))
         (height (height canvas)))
    (declare (type (simple-array octet (*)) source dest)
             (type fixnum width))
    (flet ((copy-row (i j)
             (let ((s1 (* i width))
                   (s2 (* j width)))
               (replace dest source
                        :start1 s2 :end1 (+ s2 width)
                        :start2 s1))))
      (let ((j -1))
        (macrolet ((pass (start step)
                     `(loop for i from ,start below height by ,step
                       do (copy-row (incf j) i))))
          (pass 0 8)
          (pass 4 8)
          (pass 2 4)
          (pass 1 2))
        dest))))

(defun interlaced-image-data (canvas)
  (let* ((source (image-data canvas))
         (dest (copy-seq source))
         (width (width canvas))
         (height (height canvas)))
    (declare (type (simple-array octet (*)) source dest)
             (type fixnum width))
    (flet ((copy-row (i j)
             (let ((s1 (* i width))
                   (s2 (* j width)))
               (replace dest source
                        :start1 s2 :end1 (+ s2 width)
                        :start2 s1))))
      (let ((j -1))
        (macrolet ((pass (start step)
                     `(loop for i from ,start below height by ,step
                       do (copy-row i (incf j)))))
          (pass 0 8)
          (pass 4 8)
          (pass 2 4)
          (pass 1 2))
        dest))))
