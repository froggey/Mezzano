;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: Pixel translation
;;;   Created: 2001-07-14
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;;       $Id: pixel-format.lisp,v 1.2 2002/02/21 03:38:24 gilbert Exp $
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

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

;;;; Changes

;; $Log:$

;;;;

(in-package :clim-internals)

(defparameter *code-optimization*
    '(optimize (safety 0) (space 0) (speed 3) (debug 0))
    "Code optimization level within pixel translation code.")

;; A pixel format is the representation of such an easy concept as an
;; RGBA quadruple. Such an representation is called a »sample« here. 
;; Things are made harder by various formats for this information and
;; by the possible need to dither colors on some devices.

;; We define a new [protocol] class named PIXEL-FORMAT and their
;; subclasses INPUT-PIXEL-FORMAT, which denotes a pixel format
;; suitable to converting a sample to an RGBA quadruple and
;; PIXEL-OUTPUT-FORMAT suitable for converting an RGBA quadruple to a
;; sample. IO-PIXEL-FORMAT then is the union of both.

;; Some pixel formats may dither and thus [output] conversion may
;; depend on the x/y coördinate pair of the pixel. Others are
;; »uniform«, that is they do not depend on the coördinate of the
;; pixel.

;; What we finally want to achieve is to have some function:

;; CONVERT-PIXELS input-array input-format output-array output-format

;; Which converts pixels stored in some format to pixels stored in
;; some other format. And: This functions should be as fast as
;; possible. We achieve the speed by not calling some generic function
;; on the pixel-format for each pixel to do the conversion, but by
;; compiling the actual conversion code. Compilation takes place in
;; run-time, which allows us to write rather general code and still
;; adopt to all those weird X11 visual classes. 

;; So we have:

;; PIXEL-FORMAT                                                 [class]
;; INPUT-PIXEL-FORMAT                                           [class]
;; OUTPUT-PIXEL-FORMAT                                          [class]
;; IO-PIXEL-FORMAT                                              [class]

(define-protocol-class pixel-format ()
  ((composer :initform nil)))

(define-protocol-class input-pixel-format (pixel-format) 
  ())

(define-protocol-class output-pixel-format (pixel-format)
  ())

(define-protocol-class io-pixel-format (input-pixel-format output-pixel-format) ())

(define-protocol-class uniform-pixel-format () ()
  ;; Mixin class for uniform pixel formats
  )

(define-protocol-class non-uniform-pixel-format () ()
  ;; Mixin class for non-uniform pixel formats
  )

;; and their predicates

;; The protocol

(defgeneric pixel-decomposing-code (pixel-format)
  ;; Convert a sample to an RGBA quadruple.
  ;;
  ;; Returns code, which represents F.
  ;; F: context sample -> r g b a
  )

(defgeneric pixel-composing-code (pixel-format)
  ;; Convert an RGBA quadruple to a sample.
  ;;
  ;; Returns code, which represents F.
  ;; F: context R G B A x y -> sample
  )

(defgeneric pixel-format-element-type (pixel-format)
  ;; Returns *the* element-type used for arrays containing images made
  ;; out of pixel in this particular format.
  )

(defgeneric pixel-format-maximum-component-values (pixel-format)
  ;; Returns four values
  ;;  - maximum value of red component
  ;;  - maximum value of green component
  ;;  - maximum value of blue component
  ;;  - maximum value of alpha component
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Some Utilities
;;;;

(defun pixel-format-red-max (pixel-format)
  (nth-value 0 (pixel-format-maximum-component-values pixel-format)))

(defun pixel-format-green-max (pixel-format)
  (nth-value 1 (pixel-format-maximum-component-values pixel-format)))

(defun pixel-format-blue-max (pixel-format)
  (nth-value 2 (pixel-format-maximum-component-values pixel-format)))

(defun pixel-format-alpha-max (pixel-format)
  (nth-value 3 (pixel-format-maximum-component-values pixel-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Direct Pixel Formats
;;;;

;;;; True Color

(defvar *make-pixel-format-instance/unique-table*
  (make-hash-table :test #'equal))

(defclass true-color-pixel-format (io-pixel-format uniform-pixel-format)
  ((red-byte     :reader true-color-pixel-format-red-byte   :initarg :red-byte)
   (green-byte   :reader true-color-pixel-format-green-byte :initarg :green-byte)
   (blue-byte    :reader true-color-pixel-format-blue-byte  :initarg :blue-byte)
   (alpha-byte   :reader true-color-pixel-format-alpha-byte :initarg :alpha-byte)
   (red-max                                                 :initarg :red-max)
   (green-max                                               :initarg :green-max)
   (blue-max                                                :initarg :blue-max)
   (alpha-max                                               :initarg :alpha-max)
   (element-type :reader pixel-format-element-type          :initarg :element-type)))

(defmethod pixel-format-maximum-component-values ((pixel-format true-color-pixel-format))
  (with-slots (red-max green-max blue-max alpha-max) pixel-format
    (values red-max green-max blue-max alpha-max)))

(defun make-true-color-pixel-format
  (&key (red-byte   (error "~S is a required argument" :red-byte))
        (green-byte (error "~S is a required argument" :green-byte))
        (blue-byte  (error "~S is a required argument" :blue-byte))
        (alpha-byte (byte 0 0))
        (red-max    (1- (expt 2 (byte-size red-byte))))
        (green-max  (1- (expt 2 (byte-size green-byte))))
        (blue-max   (1- (expt 2 (byte-size blue-byte))))
        (alpha-max  (1- (expt 2 (byte-size alpha-byte))))
        (element-type `(unsigned-byte
                        ,(max (+ (byte-size red-byte) (byte-position red-byte))
                              (+ (byte-size green-byte) (byte-position green-byte))
                              (+ (byte-size blue-byte) (byte-position blue-byte))
                              (+ (byte-size alpha-byte) (byte-position alpha-byte))))))
  (make-pixel-format-instance 'true-color-pixel-format
                              :red-byte red-byte
                              :green-byte green-byte
                              :blue-byte blue-byte
                              :alpha-byte alpha-byte
                              :red-max red-max
                              :green-max green-max
                              :blue-max blue-max
                              :alpha-max alpha-max
                              :element-type element-type))

(defun make-pixel-format-instance (&rest args)
  (or (gethash args *make-pixel-format-instance/unique-table*)
      (setf (gethash args *make-pixel-format-instance/unique-table*)
            (apply #'make-instance args))))

(defmethod pixel-decomposing-code ((pf true-color-pixel-format))
  (with-slots (red-byte green-byte blue-byte alpha-byte
                        red-max green-max blue-max alpha-max
                        element-type)
      pf
    `(lambda (context sample)
       (declare (ignore context)
                (type ,element-type sample))
       (values (ldb (byte ,(byte-size red-byte)   ,(byte-position red-byte))   sample)
               (ldb (byte ,(byte-size green-byte) ,(byte-position green-byte)) sample)
               (ldb (byte ,(byte-size blue-byte)  ,(byte-position blue-byte))  sample)
               ,(if (= 0 (byte-size alpha-byte))
                    0
                  `(ldb (byte ,(byte-size alpha-byte) ,(byte-position alpha-byte)) sample)))) ))

(defmethod pixel-composing-code ((pf true-color-pixel-format))
  (with-slots ((red-max   red-max)
               (green-max green-max)
               (blue-max  blue-max)
               (alpha-max alpha-max)
               red-byte green-byte blue-byte alpha-byte element-type) pf
    `(lambda (context r g b a x y)
       (declare (ignore x y)
                (ignore context)
                (ignorable a)
                (type (integer 0 ,red-max)   r)
                (type (integer 0 ,green-max) g)
                (type (integer 0 ,blue-max)  b)
                (type (integer 0 ,alpha-max) a))
       (the ,element-type
         (dpb r (byte ,(byte-size red-byte) ,(byte-position red-byte))
              (dpb g (byte ,(byte-size green-byte) ,(byte-position green-byte))
                   (dpb b (byte ,(byte-size blue-byte) ,(byte-position blue-byte))
                        ,(if (= (byte-size alpha-byte) 0)
                             ;; no alpha here
                             0
                           ;; else also deposit the alpha value
                           `(dpb a
                                 (byte ,(byte-size alpha-byte) ,(byte-position alpha-byte))
                                 0))))) ))))

;;;; Gray Scale

(defclass gray-scale-pixel-format (io-pixel-format uniform-pixel-format)
  ((gray-byte    :reader gray-scale-pixel-format-gray-byte      :initarg :gray-byte)
   (alpha-byte   :reader gray-scale-pixel-format-alpha-byte     :initarg :alpha-byte)
   (gray-max     :reader gray-scale-pixel-format-gray-max       :initarg :gray-max)
   (alpha-max                                                   :initarg :alpha-max)
   (element-type :reader pixel-format-element-type              :initarg :element-type) ))

(defmethod pixel-format-maximum-component-values ((pixel-format gray-scale-pixel-format))
  (values (gray-scale-pixel-format-gray-max pixel-format)
          (gray-scale-pixel-format-gray-max pixel-format)
          (gray-scale-pixel-format-gray-max pixel-format)
          0))

(defun make-gray-scale-pixel-format
  (&key (gray-byte   (error "~S is a required argument" :gray-byte))
        (alpha-byte  (byte 0 0))
        (gray-max    (1- (expt 2 (byte-size gray-byte))))
        (alpha-max   (1- (expt 2 (byte-size alpha-byte))))
        (element-type `(unsigned-byte
                        ,(max (+ (byte-size gray-byte) (byte-position gray-byte))
                              (+ (byte-size alpha-byte) (byte-position alpha-byte))))))
  (make-pixel-format-instance 'gray-scale-pixel-format
                              :gray-byte    gray-byte
                              :alpha-byte   alpha-byte
                              :gray-max     gray-max
                              :alpha-max    alpha-max
                              :element-type element-type))

(defmethod pixel-decomposing-code ((pf gray-scale-pixel-format))
  (with-slots (alpha-byte gray-byte) pf
    `(lambda (context sample)
       (declare (ignore context)
                (type ,(pixel-format-element-type pf) sample))
       ,(cond ((= 0 (byte-size alpha-byte))
               `(values sample sample sample 0))
              (t
               `(let ((s (ldb (byte ,(byte-size gray-byte) ,(byte-position gray-byte))
                              sample)))
                  (values s s s
                          (ldb (byte ,(byte-size alpha-byte) ,(byte-position alpha-byte))
                               sample))))))))

(defmethod pixel-composing-code ((pf gray-scale-pixel-format))
  (with-slots (gray-byte gray-max alpha-byte alpha-max) pf   
    (let ((max gray-max))
      (assert (= max
                 (pixel-format-red-max pf)
                 (pixel-format-green-max pf)
                 (pixel-format-blue-max pf)))
      `(lambda (context r g b a x y)
         (declare (ignore x y context)
                  (ignorable a)
                  (type (integer 0 ,max) r g b)
                  (type (integer 0 ,alpha-max) a))
         (let ((lum (floor (the (integer 0 ,(* 1024 max))
                             (+ (* 307 r) (* 599 g) (* 118 b)))
                           1024)))
           (declare (type (integer 0 ,max) lum))
           (logior (ash lum ,(byte-position gray-byte))
                   ,(if (> (byte-size alpha-byte) 0)
                        `(ash a ,(byte-position alpha-byte))
                      0)))))))

;;;; CLIM:COLOR

(defmethod pixel-format-maximum-component-values ((pixel-format (eql 'clim:color)))
  (values #xFFFF #xFFFF #xFFFF 0))

(defmethod pixel-decomposing-code ((pixel-format (eql 'clim:color)))
  `(lambda (context sample)
     (declare (ignore context))
     (multiple-value-bind (r g b) (color-rgb sample)
       (values (floor (* ,(pixel-format-red-max pixel-format) r))
               (floor (* ,(pixel-format-green-max pixel-format) g))
               (floor (* ,(pixel-format-blue-max pixel-format) b))
               0))))

(defmethod pixel-composing-code ((pixel-format (eql 'clim:color)))
  (multiple-value-bind (rmax gmax bmax) (pixel-format-maximum-component-values pixel-format)
    `(lambda (context r g b a x y)
       (declare (ignore context a x y)
                (type (integer 0 ,rmax) r)
                (type (integer 0 ,gmax) g)
                (type (integer 0 ,bmax) b))
       (make-rgb-color (/ r ,(coerce rmax 'short-float))
                       (/ g ,(coerce gmax 'short-float))
                       (/ b ,(coerce bmax 'short-float))))))

(defmethod pixel-format-element-type ((pixel-format (eql 'clim:color)))
  'clim:color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Dithers
;;;;

(defclass color-dithering-pixel-format (output-pixel-format non-uniform-pixel-format)
  ((output-format :initarg :output-format)
   (red-max       :initarg :red-max             :initform 255)
   (green-max     :initarg :green-max           :initform 255)
   (blue-max      :initarg :blue-max            :initform 255)
   (alpha-max     :initarg :alpha-max           :initform 0)))

(defmethod pixel-format-maximum-component-values ((pf color-dithering-pixel-format))
  (with-slots (red-max green-max blue-max alpha-max) pf
    (values red-max green-max blue-max alpha-max)))

(defconstant +dither-map+
  '#2A(( 0 48 12 60  3 51 15 63)
       (32 16 44 28 35 19 47 31)
       ( 8 56  4 52 11 59  7 55)
       (40 24 36 20 43 27 39 23)
       ( 2 50 14 62  1 49 13 61)
       (34 18 46 30 33 17 45 29)
       (10 58  6 54  9 57  5 53)
       (42 26 38 22 41 25 37 21)))

(defmethod pixel-composing-code ((pf color-dithering-pixel-format))
  (with-slots (red-max green-max blue-max alpha-max output-format) pf
    (multiple-value-bind (ormax ogmax obmax oamax)
        (pixel-format-maximum-component-values output-format)
      `(lambda (context r g b a x y)
         (declare (ignorable a)
                  (type (integer 0 ,red-max) r)
                  (type (integer 0 ,green-max) g)
                  (type (integer 0 ,blue-max) b)
                  (type (integer 0 ,alpha-max) a))
         (,(pixel-composing-code output-format)
          context
          (,(generic-ditherer (1+ ormax) red-max)   x y r)
          (,(generic-ditherer (1+ ogmax) green-max) x y g)
          (,(generic-ditherer (1+ obmax) blue-max)  x y b)
          ,(if (= 0 oamax)
               0
             `(,(generic-ditherer (1+ oamax) alpha-max) x y a))
          x y)))))

(defmethod pixel-format-element-type ((pf color-dithering-pixel-format))
  (with-slots (output-format) pf
    (pixel-format-element-type output-format)))

(defun generic-ditherer (m maxval)
  (let ((table-size (1+ maxval)))
    (let ((table (make-array (list 8 8 table-size))))
      (dotimes (x 8)
        (dotimes (y 8)
          (dotimes (s table-size)
            (multiple-value-bind (c0 delta) (floor (* s (1- m)) maxval)
              (setf (aref table x y s)
                    (min (1- m)
                         (if ;; (<= (/ delta maxval) (/ (aref +dither-map+ x y) 64))
                             (<= (* 64 delta) (* maxval (aref +dither-map+ x y)))
                             c0
                            (+ c0 1))))))))
      `(lambda (x y s)
         (the fixnum
           (aref (the (simple-array t (8 8 ,table-size)) ',table)
                 (the fixnum (logand #x7 x))
                 (the fixnum (logand #x7 y))
                 (the fixnum s) ))))))

;;; gray ditherer

(defclass gray-dithering-pixel-format (output-pixel-format non-uniform-pixel-format)
  ((output-format :initarg :output-format)
   (gray-max      :initarg :gray-max            :initform 255)
   (alpha-max     :initarg :alpha-max           :initform 0)))

(defmethod pixel-format-maximum-component-values ((pf gray-dithering-pixel-format))
  (with-slots (gray-max alpha-max) pf
    (values gray-max gray-max gray-max alpha-max)))

(defmethod pixel-composing-code ((pf gray-dithering-pixel-format))
  (with-slots (gray-max alpha-max output-format) pf
    (multiple-value-bind (ormax ogmax obmax oamax)
        (pixel-format-maximum-component-values output-format)
      `(lambda (context r g b a x y)
         (declare (ignorable a)
                  (type (integer 0 ,gray-max) r g b)
                  (type (integer 0 ,alpha-max) a))
         (let ((gray (floor (the (integer 0 ,(* 1024 gray-max))
                              (+ (* 307 r) (* 599 g) (* 118 b)))
                            1024)))
           (,(pixel-composing-code output-format)
            context
            (,(generic-ditherer (1+ ormax) gray-max) x y gray)
            (,(generic-ditherer (1+ ogmax) gray-max) x y gray)
            (,(generic-ditherer (1+ obmax) gray-max) x y gray)
            ,(if (= 0 oamax)
                 0
               `(,(generic-ditherer (1+ oamax) alpha-max) x y a))
            x y))))))
           
(defmethod pixel-format-element-type ((pf gray-dithering-pixel-format))
  (with-slots (output-format) pf
    (pixel-format-element-type output-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Helper for RGB-Cubes
;;;;

(defclass rgb-cube-output-pixel-format (output-pixel-format uniform-pixel-format)
  ((cube :initarg :cube)
   (element-type :initarg :element-type :reader pixel-format-element-type)))

(defmethod pixel-format-maximum-component-values ((pf rgb-cube-output-pixel-format))
  (with-slots (cube) pf
    (values (1- (array-dimension cube 0))
            (1- (array-dimension cube 1))
            (1- (array-dimension cube 2))
            0)))

(defmethod pixel-composing-code ((pf rgb-cube-output-pixel-format))
  (with-slots (cube) pf
    `(lambda (context r g b a x y)
       (declare (ignore context a x y)
                (type (integer 0 ,(array-dimension cube 0)) r)
                (type (integer 0 ,(array-dimension cube 1)) g)
                (type (integer 0 ,(array-dimension cube 2)) b))
       (aref (the ,(type-of cube) ',cube) r g b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Generic Pixel Translator
;;;;

(defmethod pixel-translating-code (input-pixel-format output-pixel-format)
  ;; generic pixel translator
  (multiple-value-bind (irmax igmax ibmax iamax)
      (pixel-format-maximum-component-values input-pixel-format)
    (multiple-value-bind (ormax ogmax obmax oamax)
        (pixel-format-maximum-component-values output-pixel-format)
      `(lambda (input-context output-context sample x y)
         (multiple-value-bind (r g b a)
             (,(pixel-decomposing-code input-pixel-format) input-context sample)
           (,(pixel-composing-code output-pixel-format)
            output-context
            (,(rescaling-code irmax ormax) r)
            (,(rescaling-code igmax ogmax) g)
            (,(rescaling-code ibmax obmax) b)
            (,(rescaling-code iamax oamax) a)
            x y))))))

(defun power-of-two-p (x)
  (= (logcount x) 1))

(defparameter *table-threshold* 256)

(defun rescaling-code (input-max output-max)
  `(lambda (x)
     ,(cond ((= input-max output-max)
             'x)
            ((and (< output-max input-max)
                  (power-of-two-p (1+ output-max))
                  (power-of-two-p (1+ input-max)))
             ;; This already is an approximation?
             `(ash x ,(- (integer-length output-max) (integer-length input-max))))
            ;;
            ((= input-max 0)
             ;; This prevents a divide by zero below and takes care of
             ;; missing alpha-channels.
             `',output-max)
            ;; table approach
            ((< input-max *table-threshold*)
             (let ((table (make-array (1+ input-max) :element-type `(integer 0 ,output-max))))
               (loop for i from 0 to input-max do
                 (setf (aref table i) (round (* output-max i) input-max)))
               `(aref (the ,(type-of table) ',table) x)))
            ;;
            (t
             (let ((q (/ output-max input-max)))
               `(floor (* ,(numerator q) x) ,(denominator q)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Common Formats
;;;;

(defconstant +RGB-8-format+
  (make-true-color-pixel-format
   :red-byte   (byte 8 16)
   :green-byte (byte 8 8)
   :blue-byte  (byte 8 0)))

(defconstant +BGR-8-format+
  (make-true-color-pixel-format
   :red-byte   (byte 8 0)
   :green-byte (byte 8 8)
   :blue-byte  (byte 8 16)))

(defconstant +gray-8-format+
  (make-gray-scale-pixel-format :gray-byte (byte 8 0)))

(defconstant +bitarray-format+
  (make-gray-scale-pixel-format :gray-byte (byte 1 0)))

(defmethod print-object ((x (eql +rgb-8-format+)) sink)
  (declare (ignorable x))
  (format sink "#.~S" '+rgb-8-format+))

(defmethod print-object ((x (eql +bgr-8-format+)) sink)
  (declare (ignorable x))
  (format sink "#.~S" '+bgr-8-format+))

(defmethod print-object ((x (eql +gray-8-format+)) sink)
  (declare (ignorable x))
  (format sink "#.~S" '+gray-8-format+))

(defmethod print-object ((x (eql +bitarray-format+)) sink)
  (declare (ignorable x))
  (format sink "#.~S" '+bitarray-format+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; TODO

;;; - A pixel format needs some notion of a context. E.g. They might
;;; be indexed pixel formats, where the pixel translating code needs
;;; some access to the palette.

;;; - A pixel format also needs something like a base type.

;;; - The (type (unsigned-byte 16) x y) is considered harmful.

;;; - For more speed still, we want [sub]array translators

;;; - There are still assumptions that components are 8-bit wide.

;;; - There still is the question, if we should allocate the colormap
;;;   entries, wenn we generate the pixel translator.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
(defmethod pixel-format-composer ((pf output-pixel-format))
  (with-slots (composer) pf
    (or composer
        (setf composer
              (compile nil
                       (multiple-value-bind (rmax gmax bmax amax)
                           (pixel-format-maximum-component-values pf)
                         `(lambda (context r g b a x y)
                            (declare (type (unsigned-byte 8) r g b a)
                                     (type (unsigned-byte 16) x y)
                                     ,*code-optimization*)
                            (,(pixel-composing-code pf)
                             context
                             (,(rescaling-code 255 rmax) r)
                             (,(rescaling-code 255 gmax) g)
                             (,(rescaling-code 255 bmax) b)
                             (,(rescaling-code 255 amax) a)
                             x y))))))))

(defmethod compose-standard-pixel ((pf output-pixel-format) r g b a x y)
  (with-slots (composer) pf
    (funcall (or composer (pixel-format-composer pf))
             nil r g b a x y)))

(let ((clim-color-composer nil))
  (defmethod compose-standard-pixel ((pf (eql 'clim:color)) r g b a x y)
    (funcall (or clim-color-composer
                 (setf clim-color-composer
                       (compile nil
                                (multiple-value-bind (rmax gmax bmax amax)
                                    (pixel-format-maximum-component-values pf)
                                  `(lambda (context r g b a x y)
                                     (declare (type (unsigned-byte 8) r g b a)
                                              (type (unsigned-byte 16) x y)
                                              ,*code-optimization*)
                                     (,(pixel-composing-code pf)
                                      context
                                      (,(rescaling-code 255 rmax) r)
                                      (,(rescaling-code 255 gmax) g)
                                      (,(rescaling-code 255 bmax) b)
                                      (,(rescaling-code 255 amax) a)
                                      x y))))))
             nil r g b a x y)))

||#

;;; XXX (unsigned-byte 16) considered harmful.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Pixel Format Cache
;;;;

;; Since compilation for pixel code is considered expensive, we cache
;; binary code. We cache the following functions:

;; - compose-pixel
;; - decompose-pixel
;; - translate-pixel
;; - translate-pixels

;; The user then has the following API:

;; TRANSLATE-PIXEL input-pixel-format output-pixel-format sample -> sample'
;; TRANSLATE-PIXELS input-pixel-format output-pixel-format source-array destination-array

;; and:

;; PIXEL-TRANSLATOR input-pixel-format output-pixel-format -> (sample -> sample)
;; PIXELS-TRANSLATOR input-pixel-format output-pixel-format -> (source-array destination-array ->)

;;(defclass spreaded-pixel-format ())
;; A special pixel format.
;; Input samples are spreaded R,G,B,A tuples
;; Output samples are multiple values.

(defvar *pixel-translator-cache*
  (make-hash-table :test #'equal))

(defvar *pixels-translator-cache*
  (make-hash-table :test #'equal))

(defmethod pixel-translator (input-pixel-format output-pixel-format)
  (or (gethash (list input-pixel-format output-pixel-format) *pixel-translator-cache*)
      (setf (gethash (list input-pixel-format output-pixel-format) *pixel-translator-cache*)
            (compile nil `(lambda (sample x y)
                           (declare ,*code-optimization*)
                           (declare (type fixnum x y)) ;xxx
                           (,(pixel-translating-code input-pixel-format output-pixel-format)
                            nil nil sample x y)) ))))

(defmethod pixels-translator (input-pixel-format output-pixel-format)
  (or (gethash (list input-pixel-format output-pixel-format) *pixels-translator-cache*)
      (setf (gethash (list input-pixel-format output-pixel-format) *pixels-translator-cache*)
            (compile
             nil
             `(lambda (source-array source-start source-skip
                       dest-array dest-start dest-skip
                       width height
                       x0 y0)
                (let ((source-ptr source-start)
                      (dest-ptr   dest-start))
                  (loop for y from y0 below (+ y0 height) do
                    (loop for i from 0 below width do
                      (setf (row-major-aref dest-array (+ dest-ptr i))
                            (,(pixel-translating-code input-pixel-format output-pixel-format)
                             nil nil
                             (row-major-aref source-array (+ source-ptr i))
                             (+ x0 i) y)))
                    (incf source-ptr source-skip)
                    (incf dest-ptr dest-skip)))) ))))

(defun translate-pixel (input-pixel-format output-pixel-format sample &optional (x 0) (y 0))
  (funcall (pixel-translator input-pixel-format output-pixel-format) sample x y))

(defun translate-pixels (input-pixel-format input-array output-pixel-format output-array
                         &optional (x0 0) (y0 0)) 
  (funcall (pixels-translator input-pixel-format output-pixel-format)
           input-array 0 (array-dimension input-array 1)
           output-array 0 (array-dimension output-array 1)
           (array-dimension input-array 1)
           (array-dimension input-array 0)
           x0 y0))
  
;;;;;

(defmethod compose-pixel ((pixel-format pixel-format) red green blue opacity x y)
  (funcall (or (slot-value pixel-format 'composer)
               (setf (slot-value pixel-format 'composer)
                 (compile nil (pixel-composing-code pixel-format))))
           nil red green blue opacity x y))