;;;; Architecture-independent surface blitting and filling functions.

(in-package :mezzano.gui)

;; Line traversal functions.

(defun %bitblt-line (blender to to-offset ncols from from-offset)
  (declare (type (simple-array mezzano.gui:colour (*)) to from)
           (type fixnum ncols to-offset from-offset)
           (type function blender)
           (optimize speed (safety 0)))
  (loop
     for i fixnum below ncols
     for to-ofs fixnum from to-offset
     for from-ofs fixnum from from-offset
     do (funcall blender (aref from from-ofs) to to-ofs)))

(defun %bitset-line (blender to to-offset ncols colour)
  (declare (type (simple-array mezzano.gui:colour (*)) to)
           (type mezzano.gui:colour colour)
           (type fixnum ncols to-offset)
           (type function blender)
           (optimize speed (safety 0)))
  (loop
     for i fixnum below ncols
     for to-ofs fixnum from to-offset
     do (funcall blender colour to to-ofs)))

(defun %bitset-mask-1-line (blender to to-offset ncols mask mask-offset colour)
  (declare (type (simple-array mezzano.gui:colour (*)) to)
           (type (simple-array bit (*)) mask)
           (type mezzano.gui:colour colour)
           (type fixnum ncols to-offset mask-offset)
           (type function blender)
           (optimize speed (safety 0)))
  (loop
     for i fixnum below ncols
     for to-ofs fixnum from to-offset
     for mask-ofs fixnum from mask-offset
     do (let ((bit (aref mask mask-ofs)))
          (when (eql bit 1)
            (funcall blender colour to to-ofs)))))

(declaim (inline component-*))
(defun component-* (colour alpha-octet)
  (declare (type mezzano.gui:colour colour)
           (type (unsigned-byte 8) alpha-octet)
           (optimize speed (safety 0)))
  (cond ((eql alpha-octet 0)
         0)
        ((eql alpha-octet 255)
         colour)
        (t
         (let ((alpha (/ alpha-octet 255.0f0)))
           (make-colour (* (colour-red colour) alpha)
                        (* (colour-green colour) alpha)
                        (* (colour-blue colour) alpha)
                        (* (colour-alpha colour) alpha)
                        t)))))

(declaim (inline component-+))
(defun component-+ (lhs rhs)
  (make-colour (+ (colour-red lhs) (colour-red rhs))
               (+ (colour-green lhs) (colour-green rhs))
               (+ (colour-blue lhs) (colour-blue rhs))
               (+ (colour-alpha lhs) (colour-alpha rhs))
               t))

(defun %bitset-mask-8-line (blender to to-offset ncols mask mask-offset colour)
  (declare (type (simple-array mezzano.gui:colour (*)) to)
           (type (simple-array (unsigned-byte 8) (*)) mask)
           (type mezzano.gui:colour colour)
           (type fixnum ncols to-offset mask-offset)
           (type function blender)
           (optimize speed (safety 0)))
  (loop
     for i fixnum below ncols
     for to-ofs fixnum from to-offset
     for mask-ofs fixnum from mask-offset
     do (funcall blender
                   (component-* colour (aref mask mask-ofs))
                   to to-ofs)))

;;; Final blending functions.

(defun %%set-one-argb8888-argb8888 (colour to to-offset)
  (declare (type (simple-array mezzano.gui:colour (*)) to)
           (type mezzano.gui:colour colour)
           (type fixnum to-offset)
           (optimize speed (safety 0)))
  (setf (aref to to-offset) colour))

(defun %%xor-one-argb8888-argb8888 (colour to to-offset)
  (declare (type (simple-array mezzano.gui:colour (*)) to)
           (type mezzano.gui:colour colour)
           (type fixnum to-offset)
           (optimize speed (safety 0)))
  (setf (aref to to-offset) (logxor (aref to to-offset) colour)))

;; Alpha-blend PIXEL into DEST.
;; GL_FUNC_ADD
;; src = GL_ONE
;; dst = GL_ONE_MINUS_SRC_ALPHA
(defun %%alpha-blend-one-argb8888-argb8888 (colour to to-offset)
  (declare (type (simple-array mezzano.gui:colour (*)) to)
           (type mezzano.gui:colour colour)
           (type fixnum to-offset)
           (optimize speed (safety 0)))
  (let* ((source-alpha (colour-alpha-as-octet colour)))
    (cond ((eql source-alpha 0)
           ;; Fully transparent, do nothing.
           nil)
          ((eql source-alpha 255)
           ;; Fully opaque, just write.
           (setf (aref to to-offset) colour))
          (t
           (let* ((inverse-alpha (- 255 source-alpha))
                  (dest (aref to to-offset))
                  (dest-blended (component-* dest inverse-alpha))
                  (result (component-+ dest-blended colour)))
             (setf (aref to to-offset) result))))))
