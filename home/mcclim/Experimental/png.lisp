;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PNG; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Reading .png Files
;;;   Created: 1997-04-24
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2001 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; XXX this one is borken, we have a more current one in the closure
;;; XXX browser!

(require :unzip)

(defpackage :png
    (:use :clim-lisp))

(in-package :PNG)

;;; Image formats to support:
;;;
;;;   Color    Allowed    Interpretation
;;;   Type    Bit Depths
;;;
;;;   0       1,2,4,8,16  Each pixel is a grayscale sample.
;;;   2       8,16        Each pixel is an R,G,B triple.
;;;   3       1,2,4,8     Each pixel is a palette index; a PLTE chunk must appear.
;;;   4       8,16        Each pixel is a grayscale sample, followed by an alpha sample.
;;;   6       8,16        Each pixel is an R,G,B triple, followed by an alpha sample.
;;;

(defconstant *png-magic* '#(137 80 78 71 13 10 26 10)
  "The first eight bytes of a png file.")

(defstruct png-image 
  ihdr
  idat
  plte)

(defstruct ihdr
  width height bit-depth color-type compression-method filter-method interlace-method)

;; CODE DUPLICATION ALERT! killed+yanked from images.lisp
(defun full-read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (unless (<= end start)
    (do ((i 0 n)
         (n (read-sequence sequence input :start 0)
            (read-sequence sequence input :start n)))
        ((or (= i n)
             (>= n end))
         (when (= i n)
           (error "EOF during ~S." 'full-read-byte-sequence))))))

(defun read-png-signature-p (source)
  "Checks for PNG signature."
  ;; Returns non-NIL if the first eight bytes read from 'source' is the valid PNG header, NIL otherwise.
  ;; If eof occurs while reading from source NIL is returned
  (dotimes (i (length *png-magic*) t)
    (when (not (eql (read-byte source nil 256) (aref *png-magic* i)))
      (return nil))))

(defun read-chunk (source)
 "Read a PNG chunk from 'source' and return the chunk type, a four
  character string, and a vector containing the data bytes. The CRC is
  not included into the data bytes.
  If eof occurs return NIL."
  ;;TODO: check for CRC errors
  (let ((length (bu:read-unsigned-byte-32 source nil nil)))
    (cond (length
           (let* ((type (bu:read-fixed-ascii-string source 4))
                 (data (make-array length :element-type '(unsigned-byte 8) :initial-element 0)))
             (full-read-byte-sequence data source)
             (bu:read-unsigned-byte-32 source);the crc
             (values type data) ))
          (t
           nil) )))

(defun decode-ihdr (data)
 "Decode an IHDR chunk from data."
  (declare (type (vector (unsigned-byte 8)) data))
  (make-ihdr :width              (bu:decode-unsigned-byte-32 data 0)
             :height             (bu:decode-unsigned-byte-32 data 4)
             :bit-depth          (aref data 8)
             :color-type         (aref data 9)
             :compression-method (aref data 10)
             :filter-method      (aref data 11)
             :interlace-method   (aref data 12)))

(defun decode-plte (data)
 "Decode a PLTE chunk from the byte vector 'data'."
  (declare (type (vector (unsigned-byte 8)) data))
  (assert (zerop (mod (length data) 3)))
  (let* ((len (floor (length data) 3))
         (palette (make-array len)))
    (loop
        for i from 0 to (1- len)
        do
          (setf (aref palette i) 
            (vector (aref data (+ (* i 3) 0))
                    (aref data (+ (* i 3) 1))
                    (aref data (+ (* i 3) 2))
                    255)) )
    palette))

(defun decode-trns (palette data)
  (dotimes (i (length data))
    (setf (svref (aref palette i) 3) (aref data i))))

(defun read-png-image (input)
  (unless (read-png-signature-p input)
    (error "~A is probably no PNG file." input))
  (let ((idat '#())
        (plte nil)
        (ihdr nil))
    (do ((x (multiple-value-list (read-chunk input))
            (multiple-value-list (read-chunk input))))
        ((or (null (car x)) (string= (car x) "IEND"))
         (cond ((null (car x))
                (error "png file lacks an IEND chunk"))))
      (let ((data (cadr x))
            (type (car x)))
        (let ((*print-array* nil))
          (cond ((string= type "IHDR")
                 (setq ihdr (decode-ihdr data)) )
                ((string= type "PLTE")
                 (setq plte (decode-plte data)) )
                ((and plte (string= type "tRNS"))
                 (decode-trns plte data) )
                ((string= type "tEXt")
                 (let ((p (position 0 data)))
                   (format nil "~%;Text: `~A' = `~A'."
                           (map 'string #'code-char (subseq data 0 p))
                           (map 'string #'code-char (subseq data (+ p 1))))))
                #||                     ;
                ((string= type "zTXt")
                (let ((p (position 0 data)))
                (format nil "~%;zText: `~A' = `~A'."
                (map 'string #'code-char (subseq data 0 p)) 
                (map 'string #'code-char 
                (png::rfc1951-uncompress-octects (subseq data (+ p 4))) ))))
                ||#
                ((string= type "IDAT")
                 (setf idat (concatenate '(simple-array (unsigned-byte 8)) idat data)))
                (t
                 ) ))))
    ;;
    ;; XXX this is sub-optimal
    ;;
    (make-png-image :plte plte
                    :idat (let ((i (unzip:make-inflating-stream (unzip:make-octet-input-stream idat)
                                                                :format :zlib
                                                                :element-type '(unsigned-byte 8))))
                            (unzip:with-output-to-octet-vector (o)
                              (do ((x (read-byte i nil :eof) (read-byte i nil :eof)))
                                  ((eq x :eof))
                                (write-byte x o))))
                    :ihdr ihdr) ))


(defun png-image-row-length (im)
  (let ((width (ihdr-width (png-image-ihdr im)))
        (bit-depth (ihdr-bit-depth (png-image-ihdr im)))
        (color-type (ihdr-color-type (png-image-ihdr im))))
    (+ 1 
       (ceiling
        (* width (ecase color-type
                   (0 bit-depth)
                   (2 (* 3 bit-depth))
                   (3 bit-depth)
                   (4 (* 2 bit-depth))
                   (6 (* 4 bit-depth))))
        8)) ))

(defun paeth-predictor (a b c)
  (let* ((p  (- (+ a b) c))             ;initial estimate
         (pa (abs (- p a)))             ;distances to a, b, c
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    ;; return nearest of a,b,c,
    ;; breaking ties in order a,b,c.
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c) ) ))

(defun apply-png-filter (filter data j j0 len bpp)
  (dotimes (x len)
    (let ((raw (aref data (+ j x)))
          (above (if j0 (aref data (+ j0 x)) 0))
          (left  (if (>= (- x bpp) 0) (aref data (+ j x (- bpp))) 0))
          (left-above (if (and j0 (>= (- x bpp) 0)) (aref data (+ j0 x (- bpp))) 0)))
      (setf (aref data (+ j x))
        (ecase filter
          (0 raw)
          (1 (logand #xFF (+ raw left)))
          (2 (logand #xFF (+ raw above)))
          (3 (logand #xFF (+ raw (floor (+ left above) 2))))
          (4 (logand #xFF (+ raw (paeth-predictor left above left-above)) )))))))

(defun png-image-bits-per-pixel (im)
  (let ((bit-depth (ihdr-bit-depth (png-image-ihdr im)))
        (color-type (ihdr-color-type (png-image-ihdr im))))
    (ecase color-type
      (0 bit-depth)
      (2 (* 3 bit-depth))
      (3 bit-depth)
      (4 (* 2 bit-depth))
      (6 (* 4 bit-depth)))))

(defun png-image-bytes-per-pixel (im)
  (ceiling (png-image-bits-per-pixel im) 8))

(defsubst get-sample (data i j bit-depth)
  (ecase bit-depth
    (1 (ldb (byte 1 (- 7 (mod i 8))) (aref data (+ (floor i 8) j))))
    (2 (ldb (byte 2 (* 2 (- 3 (mod i 4)))) (aref data (+ (floor i 4) j))))
    (4 (ldb (byte 4 (* 4 (- 1 (mod i 2)))) (aref data (+ (floor i 2) j))))
    (8 (aref data (+ i j)))
    (16 (logior (ash (aref data (+ (* 2 i) j)) 8)
                (aref data (+ (* 2 i) 1 j)))) ))

(defsubst get-sample* (data i j bit-depth)
  (ecase bit-depth
    (1 (* 255 (get-sample data i j bit-depth)))
    (2 (* 85 (get-sample data i j bit-depth)))
    (4 (* 17 (get-sample data i j bit-depth)))
    (8 (get-sample data i j bit-depth))
    (16 (ldb (byte 8 8) (get-sample data i j bit-depth))) ))


(defun render-filtered-row (im bit-depth color-type data j y x0 dx width pw ph put-pixel)
  (cu:declared-type-variants (bit-depth (member 1) (member 2) (member 4) (member 8) (member 16))
    (cu:declared-type-variants (color-type (member 0) (member 2) (member 3) (member 4) (member 6))
      (do ((x x0 (+ x dx))
           (i 0 (+ i 1)))
          ((>= x width))
        (ecase color-type
          (0
           (let ((v (get-sample* data i (+ j 1) bit-depth)))
             (funcall put-pixel x y v v v 255 pw ph)))
          (2
           (let ((r (get-sample* data (+ 0 (* 3 i)) (+ j 1) bit-depth))
                 (g (get-sample* data (+ 1 (* 3 i)) (+ j 1) bit-depth))
                 (b (get-sample* data (+ 2 (* 3 i)) (+ j 1) bit-depth)))
             (funcall put-pixel x y r g b 255 pw ph)))
          (3
           (let* ((i (get-sample data i (+ j 1) bit-depth))
                  (p (aref (png-image-plte im) i)))
             (funcall put-pixel x y (aref p 0) (aref p 1) (aref p 2) (aref p 3) pw ph)))
          (4
           (let ((v (get-sample* data (+ 0 (* i 2)) (+ j 1) bit-depth))
                 (a (get-sample* data (+ 1 (* i 2)) (+ j 1) bit-depth)))
             (funcall put-pixel x y v v v a pw ph)))
          (6
           (let ((r (get-sample* data (+ 0 (* 4 i)) (+ j 1) bit-depth))
                 (g (get-sample* data (+ 1 (* 4 i)) (+ j 1) bit-depth))
                 (b (get-sample* data (+ 2 (* 4 i)) (+ j 1) bit-depth))
                 (a (get-sample* data (+ 3 (* 4 i)) (+ j 1) bit-depth)))
             (funcall put-pixel x y r g b a pw ph))) ) ))))

(defun render-png-image-to-aimage (im)
  (let* ((bpp (png-image-bytes-per-pixel im))
         (data (png-image-idat im))
         (bit-depth (ihdr-bit-depth (png-image-ihdr im)))
         (width (ihdr-width (png-image-ihdr im)))
         (height (ihdr-height (png-image-ihdr im)))
         (color-type (ihdr-color-type (png-image-ihdr im)))
         (res (make-array (list height width 4) :element-type '(unsigned-byte 8))))
    (labels ((put-pixel (x y r g b a pw ph)
               pw ph a
               (setf (aref res y x 0) r
                     (aref res y x 1) g
                     (aref res y x 2) b
                     (aref res y x 3) a)))
      
      (case (ihdr-interlace-method (png-image-ihdr im))
        (0
         (let ((row-len (png-image-row-length im)))
           (do ((y 0 (+ y 1))
                (j 0 (+ j row-len))
                (j0 nil j))
               ((>= j (length data)))
             (apply-png-filter (aref data j) data (+ j 1) (if j0 (+ j0 1) nil) (1- row-len) bpp)
             (render-filtered-row im bit-depth color-type data j y 0 1 width 1 1 #'put-pixel))))
        (1
         (let (j0 (j 0))
           (do ((pass 7 (- pass 1)))
               ((< pass 1))
             (let* ((y0 (aref '#(0 1 0 2 0 4 0 0) pass))
                    (x0 (aref '#(0 0 1 0 2 0 4 0) pass))
                    (dy (aref '#(1 2 2 4 4 8 8 8) pass))
                    (ph (aref '#(1 1 2 2 4 4 8 8) pass))
                    (dx (aref '#(1 1 2 2 4 4 8 8) pass)) 
                    (pw (aref '#(1 1 1 2 2 4 4 8) pass)) )
               (let ((row-len (+ 1 (ceiling (* (png-image-bits-per-pixel im) (ceiling (- width x0) dx))
                                            8))))
                 (setf j0 nil)
                 (when (> row-len 1)
                   (do ((y y0 (+ y dy)))
                       ((>= y height))
                     (apply-png-filter (aref data j) data (+ j 1) (if j0 (+ j0 1) nil) (1- row-len) bpp)
                     (render-filtered-row im bit-depth color-type data j y x0 dx width pw ph #'put-pixel)
                     (psetf j (+ j row-len) j0 j))))))
           (assert (= j (length data))) ))
        (t
         (error "Unknown interlace method: ~D." (ihdr-interlace-method (png-image-ihdr im)))) ))
    res))

(defun png-stream-to-aimage (stream)
  (render-png-image-to-aimage (read-png-image stream)))
