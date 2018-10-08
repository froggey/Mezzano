;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.gui)

(deftype colour ()
  '(unsigned-byte 32))

(defconstant +colour-alpha-bits+ (byte 8 24))
(defconstant +colour-red-bits+   (byte 8 16))
(defconstant +colour-green-bits+ (byte 8 8))
(defconstant +colour-blue-bits+  (byte 8 0))

(defun make-colour (&optional (red 0.0) (green 0.0) (blue 0.0) (alpha 1.0) premultiplied)
  "Return a new colour object with the specified components.
Components should be reals in the range [0,1].
If PREMULTIPLIED is false, then the components will be treated as premultiplied alpha and used as-is,
otherwise they will be treated as straight alpha and converted to premultiplied alpha."
  (check-type red   (real 0 1))
  (check-type green (real 0 1))
  (check-type blue  (real 0 1))
  (check-type alpha (real 0 1))
  (setf red   (float red   0.0f0))
  (setf green (float green 0.0f0))
  (setf blue  (float blue  0.0f0))
  (setf alpha (float alpha 0.0f0))
  (when (not premultiplied)
    ;; Convert to premultiplied alpha.
    (setf red   (* red alpha)
          green (* green alpha)
          blue  (* blue alpha)))
  ;; Convert to 32-bit pixel.
  (logior (ash (truncate (* alpha 255)) 24)
          (ash (truncate (* red   255)) 16)
          (ash (truncate (* green 255)) 8)
               (truncate (* blue  255))))

(defun make-colour-from-octets (&optional (red 0) (green 0) (blue 0) (alpha 255) premultiplied)
  "Return a new colour object with the specified components.
Components should be octets.
If PREMULTIPLIED is false, then the components will be treated as premultiplied alpha and used as-is,
otherwise they will be treated as straight alpha and converted to premultiplied alpha."
  (check-type red   (unsigned-byte 8))
  (check-type green (unsigned-byte 8))
  (check-type blue  (unsigned-byte 8))
  (check-type alpha (unsigned-byte 8))
  (make-colour (/ red   255.0)
               (/ green 255.0)
               (/ blue  255.0)
               (/ alpha 255.0)
               premultiplied))

(defun colour-alpha (colour)
  "Return the alpha component of COLOUR as a single float in [0,1]."
  (/ (colour-alpha-as-octet colour) 255.0f0))

(defun colour-alpha-as-octet (colour)
  "Return the alpha component of COLOUR as an octet."
  (check-type colour colour)
  (ldb +colour-alpha-bits+ colour))

(defun colour-red (colour)
  "Return the red component of COLOUR as a single float in [0,1]."
  (/ (colour-red-as-octet colour) 255.0f0))

(defun colour-red-as-octet (colour)
  "Return the red component of COLOUR as an octet."
  (check-type colour colour)
  (ldb +colour-red-bits+ colour))

(defun colour-green (colour)
  "Return the green component of COLOUR as a single float in [0,1]."
  (/ (colour-green-as-octet colour) 255.0f0))

(defun colour-green-as-octet (colour)
  "Return the green component of COLOUR as an octet."
  (check-type colour colour)
  (ldb +colour-green-bits+ colour))

(defun colour-blue (colour)
  "Return the blue component of COLOUR as a single float in [0,1]."
  (/ (colour-blue-as-octet colour) 255.0f0))

(defun colour-blue-as-octet (colour)
  "Return the blue component of COLOUR as an octet."
  (check-type colour colour)
  (ldb +colour-blue-bits+ colour))
