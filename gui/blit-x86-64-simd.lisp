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

;; SIMD-optimized %bitset-mask-8-line.
;; Always performs the SIMD computation, matching the arm64 version.
(defun %bitset-mask-8-line (blender to to-offset ncols mask mask-offset colour)
  (declare (optimize speed (safety 0) (debug 1))
           (type function blender)
           (type fixnum to-offset ncols mask-offset)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type (simple-array (unsigned-byte 8) (*)) mask)
           (type (unsigned-byte 32) colour))
  (loop for i fixnum below ncols do
    (let ((mask-byte (aref mask mask-offset)))
      (let* ((vec-colour (mezzano.simd.x86-64:make-sse-vector colour))
             (vec-mask (mezzano.simd.x86-64:make-sse-vector
                        (* mask-byte #x01010101010101010101010101010101)))
             (vec-zero (mezzano.simd.x86-64:make-sse-vector 0))
             (colour-words (mezzano.simd.x86-64:punpcklbw vec-colour vec-zero))
             (mask-words (mezzano.simd.x86-64:punpcklbw vec-mask vec-zero))
             (product (mezzano.simd.x86-64:pmullw colour-words mask-words))
             (rounded (mezzano.simd.x86-64:paddw product
                       (mezzano.simd.x86-64:make-sse-vector #x00800080008000800080008000800080)))
             (rescaled (mezzano.simd.x86-64:pmulhuw rounded
                        (mezzano.simd.x86-64:make-sse-vector #x01010101010101010101010101010101)))
             (packed (mezzano.simd.x86-64:packuswb rescaled vec-zero))
             (result (ldb (byte 32 0)
                          (mezzano.simd.x86-64:sse-vector-value packed))))
        (funcall blender result to to-offset)))
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
  (let ((source-alpha (ldb (byte 8 24) source)))
    (cond ((= source-alpha #x00)
           nil)
          ((= source-alpha #xFF)
           (setf (aref to to-offset) source))
          (t
           (let* ((vec-src (mezzano.simd.x86-64:make-sse-vector source))
                  (vec-dst (mezzano.simd.x86-64:make-sse-vector (aref to to-offset)))
                  (vec-zero (mezzano.simd.x86-64:make-sse-vector 0))
                  (src-words (mezzano.simd.x86-64:punpcklbw vec-src vec-zero))
                  (dst-words (mezzano.simd.x86-64:punpcklbw vec-dst vec-zero))
                   (alpha-splat (mezzano.simd.x86-64:pshuflw src-words src-words #xFF))
                   (alpha-words (mezzano.simd.x86-64:pshufhw alpha-splat alpha-splat #xFF))
                  (inv-alpha-words (mezzano.simd.x86-64:psubw
                                    (mezzano.simd.x86-64:make-sse-vector #x00FF00FF00FF00FF00FF00FF00FF00FF)
                                    alpha-words))
                  (product (mezzano.simd.x86-64:pmullw dst-words inv-alpha-words))
                  (rounded (mezzano.simd.x86-64:paddw product
                            (mezzano.simd.x86-64:make-sse-vector #x00800080008000800080008000800080)))
                  (rescaled (mezzano.simd.x86-64:pmulhuw rounded
                             (mezzano.simd.x86-64:make-sse-vector #x01010101010101010101010101010101)))
                  (packed (mezzano.simd.x86-64:packuswb rescaled vec-zero))
                  (blended (mezzano.simd.x86-64:paddusb packed vec-src))
                  (result (ldb (byte 32 0)
                               (mezzano.simd.x86-64:sse-vector-value blended))))
             (setf (aref to to-offset) result))))))

(defun alpha-blend-quad (source source-offset to to-offset)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) (*)) source to)
           (type fixnum source-offset to-offset))
  (let* ((src (mezzano.simd.x86-64:u32.4-aref source source-offset))
         (dst (mezzano.simd.x86-64:u32.4-aref to to-offset))
         (vec-zero (mezzano.simd.x86-64:make-sse-vector 0))
         (src-lo (mezzano.simd.x86-64:punpcklbw src vec-zero))
         (src-hi (mezzano.simd.x86-64:punpckhbw src vec-zero))
         (dst-lo (mezzano.simd.x86-64:punpcklbw dst vec-zero))
         (dst-hi (mezzano.simd.x86-64:punpckhbw dst vec-zero))
         (alpha-lo-splat (mezzano.simd.x86-64:pshuflw src-lo src-lo #xFF))
         (alpha-lo (mezzano.simd.x86-64:pshufhw alpha-lo-splat alpha-lo-splat #xFF))
         (alpha-hi-splat (mezzano.simd.x86-64:pshuflw src-hi src-hi #xFF))
         (alpha-hi (mezzano.simd.x86-64:pshufhw alpha-hi-splat alpha-hi-splat #xFF))
         (all-255 (mezzano.simd.x86-64:make-sse-vector #x00FF00FF00FF00FF00FF00FF00FF00FF))
         (inv-alpha-lo (mezzano.simd.x86-64:psubw all-255 alpha-lo))
         (inv-alpha-hi (mezzano.simd.x86-64:psubw all-255 alpha-hi))
         (prod-lo (mezzano.simd.x86-64:pmullw dst-lo inv-alpha-lo))
         (prod-hi (mezzano.simd.x86-64:pmullw dst-hi inv-alpha-hi))
         (rounding (mezzano.simd.x86-64:make-sse-vector #x00800080008000800080008000800080))
         (rounded-lo (mezzano.simd.x86-64:paddw prod-lo rounding))
         (rounded-hi (mezzano.simd.x86-64:paddw prod-hi rounding))
         (scale (mezzano.simd.x86-64:make-sse-vector #x01010101010101010101010101010101))
         (rescaled-lo (mezzano.simd.x86-64:pmulhuw rounded-lo scale))
         (rescaled-hi (mezzano.simd.x86-64:pmulhuw rounded-hi scale))
         (packed (mezzano.simd.x86-64:packuswb rescaled-lo rescaled-hi))
         (blended (mezzano.simd.x86-64:paddusb packed src)))
    (setf (mezzano.simd.x86-64:u32.4-aref to to-offset) blended))
  nil)

(defun alpha-blend-interleaved (source source-offset to to-offset)
  "Blend 16 pixels at once."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) (*)) source to)
           (type fixnum source-offset to-offset))
  (alpha-blend-quad source source-offset to to-offset)
  (alpha-blend-quad source (+ source-offset 4) to (+ to-offset 4))
  (alpha-blend-quad source (+ source-offset 8) to (+ to-offset 8))
  (alpha-blend-quad source (+ source-offset 12) to (+ to-offset 12))
  nil)

(defun %bitblt-blend-line (to to-offset ncols from from-offset)
  (declare (type (simple-array (unsigned-byte 32) (*)) to from)
           (type fixnum ncols to-offset from-offset)
           (optimize speed (safety 0) (debug 0)))
  (loop
    while (>= ncols 16)
    do (alpha-blend-interleaved from from-offset to to-offset)
       (decf ncols 16)
       (incf to-offset 16)
       (incf from-offset 16))
  (loop
    while (>= ncols 4)
    do (alpha-blend-quad from from-offset to to-offset)
       (decf ncols 4)
       (incf to-offset 4)
       (incf from-offset 4))
  (loop
     for i fixnum below ncols
     for to-ofs fixnum from to-offset
     for from-ofs fixnum from from-offset
     do (%%alpha-blend-one-argb8888-argb8888 (aref from from-ofs) to to-ofs)))
