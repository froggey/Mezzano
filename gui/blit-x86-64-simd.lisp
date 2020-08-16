;;;; SIMD-accelerated blitting functions.

(in-package :mezzano.gui)

;; Line traversal functions.

(defun %bitblt-line (blender to to-offset ncols from from-offset)
  (declare (optimize speed (safety 0) (debug 1))
           (type function blender)
           (type fixnum to-offset ncols from-offset)
           (type (simple-array (unsigned-byte 32) (*)) to from))
  (loop for i fixnum below ncols do
    (funcall blender (aref from from-offset) to to-offset)
    (incf to-offset)
    (incf from-offset)))

(defun %bitset-line (blender to to-offset ncols colour)
  (declare (optimize speed (safety 0) (debug 1))
           (type function blender)
           (type fixnum to-offset ncols)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type (unsigned-byte 32) colour))
  (loop for i fixnum below ncols do
    (funcall blender colour to to-offset)
    (incf to-offset)))

(defun %bitset-mask-1-line (blender to to-offset ncols mask mask-offset colour)
  (declare (optimize speed (safety 0) (debug 1))
           (type function blender)
           (type fixnum to-offset ncols mask-offset)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type (simple-array bit (*)) mask)
           (type (unsigned-byte 32) colour))
  (loop for i fixnum below ncols do
    (when (not (eql (aref mask mask-offset) 0))
      (funcall blender colour to to-offset))
    (incf to-offset)
    (incf mask-offset)))

(defun %bitset-mask-8-line (blender to to-offset ncols mask mask-offset colour)
  (declare (optimize speed (safety 0) (debug 1))
           (type function blender)
           (type fixnum to-offset ncols mask-offset)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type (simple-array (unsigned-byte 8) (*)) mask)
           (type (unsigned-byte 32) colour))
  (loop for i fixnum below ncols do
    (let ((mask-byte (aref mask mask-offset)))
      (cond ((eql mask-byte 0)
             ;; Do nothing.
             nil)
            ((eql mask-byte #xFF)
             ;; Don't need to adjust colour.
             (funcall blender colour to to-offset))
            (t
             ;; Component multiply colour with mask.
             (let* ((vec-colour (mezzano.simd:make-mmx-vector colour)) ; (0000ARGB)
                    (vec-mask (mezzano.simd:make-mmx-vector mask-byte)) ; (0000000M)
                    (vec-zero (mezzano.simd:make-mmx-vector 0))
                    ;; Mask (0M0M0M0M)
                    (unpacked-mask (mezzano.simd:punpcklbw
                                    (mezzano.simd:pmuludq vec-mask
                                                          (mezzano.simd:make-mmx-vector #x01010101))
                                    vec-zero))
                    ;; Colour (0A0R0G0B)
                    (unpacked-colour (mezzano.simd:punpcklbw vec-colour vec-zero))
                    ;; Multiply colour by mask, rounding correctly.
                    (adjusted-colour (mezzano.simd:pmulhuw
                                      (mezzano.simd:paddusw
                                       (mezzano.simd:pmullw unpacked-colour unpacked-mask)
                                       (mezzano.simd:make-mmx-vector #x0080008000800080))
                                      (mezzano.simd:make-mmx-vector #x0101010101010101)))
                    (final-colour (ldb (byte 32 0) (mezzano.simd:mmx-vector-value (mezzano.simd:packuswb adjusted-colour vec-zero)))))
               (funcall blender final-colour to to-offset)))))
    (incf to-offset)
    (incf mask-offset)))

;;; Final blending functions.

(defun %%set-one-argb8888-argb8888 (source to to-offset)
  (declare (optimize speed (safety 0) (debug 1))
           (type (unsigned-byte 32) source)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type fixnum to-offset))
  (setf (aref to to-offset) source))

(defun %%xor-one-argb8888-argb8888 (source to to-offset)
  (declare (optimize speed (safety 0) (debug 1))
           (type (unsigned-byte 32) source)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type fixnum to-offset))
  (setf (aref to to-offset) (logxor source (aref to to-offset))))

;; Alpha-blend PIXEL into DEST.
;; GL_FUNC_ADD
;; src = GL_ONE
;; dst = GL_ONE_MINUS_SRC_ALPHA
(defun %%alpha-blend-one-argb8888-argb8888 (source to to-offset)
  (declare (optimize speed (safety 0) (debug 1))
           (type (unsigned-byte 32) source)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type fixnum to-offset))
  (let ((source-alpha (logand source #xFF000000)))
    (cond ((eql source-alpha #x00000000)
           ;; Fully transparent, do nothing.
           nil)
          ((eql source-alpha #xFF000000)
           ;; Fully opaque, just write.
           (setf (aref to to-offset) source))
          (t
           ;; Alpha blending.
           (let* ((vec-source (mezzano.simd:make-mmx-vector source))
                  (vec-alpha (mezzano.simd:make-mmx-vector (the (unsigned-byte 8) (ash source-alpha -24))))
                  (vec-dest (mezzano.simd:make-mmx-vector (aref to to-offset)))
                  (vec-zero (mezzano.simd:make-mmx-vector 0))
                  ;; source alpha (0A0A0A0A)
                  (exploded-alpha (mezzano.simd:punpcklbw (mezzano.simd:pmuludq vec-alpha
                                                                  (mezzano.simd:make-mmx-vector #x01010101))
                                                   vec-zero))
                  ;; inverse source alpha (0A0A0A0A)
                  (inverse-alpha (mezzano.simd:psubb (mezzano.simd:make-mmx-vector #x00FF00FF00FF00FF)
                                              exploded-alpha))
                  ;; Expand source/dest to 0A0R0G0B.
                  (unpacked-source (mezzano.simd:punpcklbw vec-source vec-zero))
                  (unpacked-dest (mezzano.simd:punpcklbw vec-dest vec-zero))
                  ;; Multiply dest by inverse alpha, rounding correctly.
                  (adjusted-dest (mezzano.simd:pmulhuw
                                  (mezzano.simd:paddusw
                                   (mezzano.simd:pmullw unpacked-dest inverse-alpha)
                                   (mezzano.simd:make-mmx-vector #x0080008000800080))
                                  (mezzano.simd:make-mmx-vector #x0101010101010101)))
                  (blended (mezzano.simd:paddusw unpacked-source adjusted-dest))
                  (final (ldb (byte 32 0) (mezzano.simd:mmx-vector-value (mezzano.simd:packuswb blended vec-zero)))))
             (setf (aref to to-offset) final))))))
