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
;;;; $Id: gif89a.lisp,v 1.19 2007/06/26 20:54:24 xach Exp $

(in-package #:skippy)

(defvar *gif-signature*
  (make-array 6
              :element-type '(unsigned-byte 8)
              :initial-contents '(71 73 70 56 57 97))
  "The ASCII codes for the characters of the string \"GIF89a\".")

(defvar *netscape-signature*
  (make-array 11
              :element-type '(unsigned-byte 8)
              :initial-contents '(78 69 84 83 67 65 80 69 50 46 48))
  "The ASCII codes for the characters of the string \"NETSCAPE2.0\".")

(defvar *disposal-methods*
  '((:unspecified . 0)
    (:none . 1)
    (:restore-background . 2)
    (:restore-previous . 3)))

(defconstant +pixel-aspect-ratio+ 0
  "Pixel aspect ratios are not set.")

(defconstant +image-separator-code+ #x2C)

(defconstant +gif-trailer-code+ #x3B
  "The end-of-GIF marker.")


(defun write-uint16 (number stream)
  (write-byte (logand #xFF number) stream)
  (write-byte (ash number -8) stream))

(defun write-block-terminator (stream)
  (write-byte 0 stream))

(defun boolean-bit (value)
  (if value 1 0))

;;; Spec from http://members.aol.com/royalef/gifabout.htm
(defun write-netscape-looping-block (stream)
  (write-byte #x21 stream)
  (write-byte #xFF stream)
  (write-byte (length *netscape-signature*) stream)
  (write-sequence *netscape-signature* stream)
  (write-byte 3 stream)
  (write-byte 1 stream)
  (write-uint16 #xFFFF stream)
  (write-byte 0 stream))

(defun write-comment (comment stream)
  "Write COMMENT to the GIF. Since the characters must be ASCII,
replace any out-of-range character codes with #\\Space."
  ;;; Comments must be at least one character long
  (when (zerop (length comment))
    (return-from write-comment))
  (when (< 255 (length comment))
    (skippy-warn "Truncating comment from ~D to 255 characters"
                 (length comment))
    (setf comment (subseq comment 255)))
  (flet ((cleaned-char-code (char)
           (let ((code (char-code char)))
             (if (> code 127) 32 code))))
    (write-byte #x21 stream)
    (write-byte #xFE stream)
    (write-byte (length comment) stream)
    (loop for char across comment do
          (write-byte (cleaned-char-code char) stream))
    (write-block-terminator stream)))

(defun disposal-method-value (keyword)
  (let ((method (assoc keyword *disposal-methods*)))
    (cond (method (cdr method))
          (t
           (skippy-warn "Unknown disposal method ~S ~
                         (expected one of ~{~S~^ ~}), using ~S instead"
                 keyword
                 (mapcar #'car *disposal-methods*)
                 :unspecified)
           0))))

(defun write-graphic-control-block (image stream)
  (let ((extension-introducer #x21)
        (graphic-control-label #xF9)
        (block-size 4))
    (write-byte extension-introducer stream)
    (write-byte graphic-control-label stream)
    (write-byte block-size stream)
    ;; packed field: RRRDDDUT
    ;; RRR = reserved (left as zero)
    ;; DDD = disposal method
    ;; U = user input (ignored, left as zero),
    ;; T = transparent color flag
    (let ((flags 
           (logior 
            (dpb (disposal-method-value (disposal-method image))
                 (byte 3 2)
                 0)
            (dpb (boolean-bit (transparentp image))
                 (byte 1 0)
                 0))))
      (write-byte flags stream))
    (write-uint16 (delay-time image) stream)
    (write-byte (or (transparency-index image) 0) stream)
    (write-block-terminator stream)))

(defun write-color-table (table stream)
  (let ((count (expt 2 (color-table-code-size table))))
    (loop for color across (entries table)
          do (multiple-value-bind (r g b)
                 (color-rgb color)
               (write-byte r stream)
               (write-byte g stream)
               (write-byte b stream))
          (decf count))
    (dotimes (i (* count 3))
      (write-byte 0 stream))))

(defun effective-color-table (image)
  "Return the color table in effect when writing out IMAGE, or signal
an error if no color table is available."
  (let ((color-table (color-table image)))
    (cond (color-table)
          ((or (not (slot-boundp image 'data-stream))
               (not (data-stream image)))
           (error 'missing-color-table :image image))
          ((color-table (data-stream image)))
          (t
           (error 'missing-color-table :image image)))))

(defun compression-code-size (image)
  "Return the number of bits needed to represent the largest index in
the effective color table of INDEX."
  (color-table-code-size (effective-color-table image)))

(defun write-image (image context stream)
  (let* ((color-table (color-table image))
         (code-size (compression-code-size image))
         (width (width image))
         (height (height image)))
    (check-image-dimensions width height)
    (write-graphic-control-block image stream)
    (write-byte +image-separator-code+ stream)
    (write-uint16 (left-position image) stream)
    (write-uint16 (top-position image) stream)
    (write-uint16 (width image) stream)
    (write-uint16 (height image) stream)
    ;; packed byte: CISRRSSS
    ;; C = local color table flag
    ;; I = interlaced flag (left as zero)
    ;; S = sort flag (left as zero)
    ;; RR = reserved (left as zero)
    ;; SSS = size (bit depth) of color table, minus one
    (let ((flags
           (logior
            (dpb (boolean-bit color-table)         (byte 1 7) 0)
            (dpb (boolean-bit (interlacedp image)) (byte 1 6) 0)
            (dpb (1- code-size)                    (byte 3 0) 0))))
      (write-byte flags stream))
    (when color-table
      (write-color-table color-table stream))
    (write-byte code-size stream)
    (let ((data (if (interlacedp image)
                    (interlaced-image-data image)
                    (image-data image))))
      (lzw-compress data code-size context stream))
    (write-block-terminator stream)))
  
(defun write-data-stream-header (data-stream stream)
  (let* ((color-table (color-table data-stream))
         (code-size (color-table-code-size color-table)))
    (write-sequence *gif-signature* stream)
    (write-uint16 (width data-stream) stream)
    (write-uint16 (height data-stream) stream)
    ;; packed byte: GRRRSTTT
    ;; G = global color table flag, RRR = color resolution, S = sort flag,
    ;; TTT = global color table size
    (write-byte (logior (ash (boolean-bit (color-table data-stream)) 7)
                        (1- code-size))
                stream)
    ;; background color index
    (write-byte 0 stream)
    (write-byte +pixel-aspect-ratio+ stream)
    (when color-table
      (write-color-table color-table stream))
    (when (comment data-stream)
      (write-comment (comment data-stream) stream))
    (when (loopingp data-stream)
      (write-netscape-looping-block stream))))

(defun write-end-code (data-stream stream)
  (declare (ignore data-stream))
  (write-byte +gif-trailer-code+ stream))

(defun write-data-stream (data-stream stream)
  (write-data-stream-header data-stream stream)
  (when (zerop (length (images data-stream)))
    (skippy-warn "No images in ~A" data-stream))
  (loop with context = (make-instance 'compression-context)
        for image across (images data-stream) do
        (check-dimensions data-stream image)
        (write-image image context stream))
  (write-end-code data-stream stream)
  (values))

(defun output-data-stream (data-stream file &key (if-exists :supersede))
  (with-open-file (stream file
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
    (write-data-stream data-stream stream)
    (probe-file file)))
