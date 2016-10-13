;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.gui)

;; Line traversal functions.

(defun %bitblt-line (blender to to-offset ncols from from-offset)
  (dotimes (i ncols)
    (funcall blender (sys.int::%simple-array-aref from from-offset) to to-offset)
    (incf to-offset)
    (incf from-offset)))

(defun %bitset-line (blender to to-offset ncols colour)
  (dotimes (i ncols)
    (funcall blender colour to to-offset)
    (incf to-offset)))

(defun %bitset-mask-1-line (blender to to-offset ncols mask mask-offset colour)
  (dotimes (i ncols)
    (let ((bit (sys.int::%simple-array-aref mask mask-offset)))
      (when (eq bit 1)
        (funcall blender colour to to-offset)))
    (incf to-offset)
    (incf mask-offset)))

(defun component-* (colour alpha)
  (make-colour (* (colour-red colour) alpha)
               (* (colour-green colour) alpha)
               (* (colour-blue colour) alpha)
               (* (colour-alpha colour) alpha)
               t))

(defun component-+ (lhs rhs)
  (make-colour (+ (colour-red lhs) (colour-red rhs))
               (+ (colour-green lhs) (colour-green rhs))
               (+ (colour-blue lhs) (colour-blue rhs))
               (+ (colour-alpha lhs) (colour-alpha rhs))
               t))

(defun %bitset-mask-8-line (blender to to-offset ncols mask mask-offset colour)
  (dotimes (i ncols)
    (let ((mask-value (/ (sys.int::%simple-array-aref mask mask-offset) 255.0s0)))
      (funcall blender (component-* colour mask-value) to to-offset))
    (incf to-offset)
    (incf mask-offset)))

;;; Final blending functions.

(defun %%set-one-argb8888-argb8888 (colour to to-offset)
  (setf (sys.int::%simple-array-aref to to-offset) colour))

(defun %%xor-one-argb8888-argb8888 (colour to to-offset)
  (setf (sys.int::%simple-array-aref to to-offset) (logxor (sys.int::%simple-array-aref to to-offset)
                                                           colour)))

;; Alpha-blend PIXEL into DEST.
;; GL_FUNC_ADD
;; src = GL_ONE
;; dst = GL_ONE_MINUS_SRC_ALPHA
(defun %%alpha-blend-one-argb8888-argb8888 (colour to to-offset)
  (let* ((source-alpha (colour-alpha colour)))
    (cond ((eq source-alpha 0.0s0)
           ;; Fully transparent, do nothing.
           nil)
          ((eq source-alpha 1.0s0)
           ;; Fully opaque, just write.
           (setf (sys.int::%simple-array-aref to to-offset) colour))
          (t
           (let* ((inverse-alpha (- 1.0s0 source-alpha))
                  (dest (sys.int::%simple-array-aref to to-offset))
                  (dest-blended (component-* dest inverse-alpha))
                  (result (component-+ dest-blended colour)))
             (setf (sys.int::%simple-array-aref to to-offset) result))))))
