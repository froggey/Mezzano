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

#+(or)
(progn
(deftype m128 () 'mezzano.simd:sse-vector)
(deftype m128i () 'mezzano.simd:sse-vector)
(deftype m128d () 'mezzano.simd:sse-vector)

(declaim (inline mm-set1-epi32))
(defun mm-set1-epi32 (value)
  (declare (type (signed-byte 32) value))
  (let ((x (mezzano.simd:make-sse-vector (ldb (byte 32 0) value))))
    ;; FIXME: Should use pshufd
    (mezzano.simd:shufps x x #4r0000)))

(declaim (inline mm-set1-ps))
(defun mm-set1-ps (value)
  (declare (type single-float value))
  (let ((x (mezzano.simd:make-sse-vector-single-float value)))
    (mezzano.simd:shufps x x #4r0000)))

(declaim (inline mm-castsi128-ps))
(defun mm-castsi128-ps (value)
  (declare (type m128i value))
  (the m128 value))

(declaim (inline mm-castps-si128))
(defun mm-castps-si128 (value)
  (declare (type m128 value))
  (the m128i value))

(declaim (inline mm-add-ps))
(defun mm-add-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:addps a b)))

(declaim (inline mm-add-epi32))
(defun mm-add-epi32 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:paddd a b)))

(declaim (inline mm-sub-ps))
(defun mm-sub-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:subps a b)))

(declaim (inline mm-sub-epi32))
(defun mm-sub-epi32 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:psubd a b)))

(declaim (inline mm-mul-ps))
(defun mm-mul-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:mulps a b)))

(declaim (inline mm-div-ps))
(defun mm-div-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:divps a b)))

(declaim (inline mm-and-ps))
(defun mm-and-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:andps a b)))

(declaim (inline mm-and-si128))
(defun mm-and-si128 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:pand a b)))

(declaim (inline mm-andnot-ps))
(defun mm-andnot-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:andnps a b)))

(declaim (inline mm-andnot-si128))
(defun mm-andnot-si128 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:pandn a b)))

(declaim (inline mm-or-ps))
(defun mm-or-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:orps a b)))

(declaim (inline mm-or-si128))
(defun mm-or-si128 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:por a b)))

(declaim (inline mm-xor-ps))
(defun mm-xor-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:xorps a b)))

(declaim (inline mm-xor-si128))
(defun mm-xor-si128 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:pxor a b)))

(declaim (inline mm-slli-epi32))
(defun mm-slli-epi32 (a imm8)
  (declare (type m128i a)
           (type (unsigned-byte 8) imm8))
  (the m128i (mezzano.simd:pslld a imm8)))

(declaim (inline mm-srai-epi32))
(defun mm-srai-epi32 (a imm8)
  (declare (type m128i a)
           (type (unsigned-byte 8) imm8))
  (the m128i (mezzano.simd:psrad a imm8)))

(declaim (inline mm-srli-epi32))
(defun mm-srli-epi32 (a imm8)
  (declare (type m128i a)
           (type (unsigned-byte 8) imm8))
  (the m128i (mezzano.simd:psrld a imm8)))

(declaim (inline mm-cmpunord-ps))
(defun mm-cmpunord-ps (a b)
  (declare (type m128 a b))
  (the m128 (mezzano.simd:cmpunordps a b)))

(declaim (inline mm-cmpeq-epi32))
(defun mm-cmpeq-epi32 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:pcmpeqd a b)))

(declaim (inline mm-cmpgt-epi32))
(defun mm-cmpgt-epi32 (a b)
  (declare (type m128i a b))
  (the m128i (mezzano.simd:pcmpgtd a b)))

(declaim (inline packed-single-to-short))
(defun packed-single-to-short (f)
  "Convert 4xf32 to 4xf16"
  (declare (type mezzano.simd:sse-vector f)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((mask-sign (mm-set1-epi32 #x80000000))
         (c-f16max (mm-set1-epi32 (ash (+ 127 16) 23))) ; all FP32 values >=this round to +inf
         (c-nanbit (mm-set1-epi32 #x200))
         (c-infty-as-fp16 (mm-set1-epi32 #x7c00))
         (c-min-normal (mm-set1-epi32 (ash (- 127 14) 23))) ; smallest FP32 that yields a normalized FP16
         (c-subnorm-magic (mm-set1-epi32 (ash (+ (- 127 15) (- 23 10) 1) 23)))
         (c-normal-bias (mm-set1-epi32 (- #xfff (ash (- 127 15) 23)))) ; adjust exponent and add mantissa rounding

         (msign (mm-castsi128-ps mask-sign))
         (justsign (mm-and-ps msign f))
         (absf (mm-xor-ps f justsign))
         (absf-int (mm-castps-si128 absf)) ; the cast is "free" (extra bypass latency, but no thruput hit)
         (f16max c-f16max)
         (b-isnan (mm-cmpunord-ps absf absf)) ; is this a NaN?
         (b-isregular (mm-cmpgt-epi32 f16max absf-int)) ; (sub)normalized or special?
         (nanbit (mm-and-si128 (mm-castps-si128 b-isnan) c-nanbit))
         (inf-or-nan (mm-or-si128 nanbit c-infty-as-fp16)) ; output for specials

         (min-normal c-min-normal)
         (b-issub (mm-cmpgt-epi32 min-normal absf-int))

         ;; "result is subnormal" path
         (subnorm1 (mm-add-ps absf (mm-castsi128-ps c-subnorm-magic))) ; magic value to round output mantissa
         (subnorm2 (mm-sub-epi32 (mm-castps-si128 subnorm1) c-subnorm-magic)) ; subtract out bias

         ;; "result is normal" path
         (mantoddbit (mm-slli-epi32 absf-int (- 31 13))) ; shift bit 13 (mantissa LSB) to sign
         (mantodd (mm-srai-epi32 mantoddbit 31)) ; -1 if FP16 mantissa odd, else 0

         (round1 (mm-add-epi32 absf-int c-normal-bias))
         (round2 (mm-sub-epi32 round1 mantodd)) ; if mantissa LSB odd, bias towards rounding up (RTNE)
         (normal (mm-srli-epi32 round2 13)) ; rounded result

         ;; combine the two non-specials
         (nonspecial (mm-or-si128 (mm-and-si128 subnorm2 b-issub) (mm-andnot-si128 b-issub normal)))

         ;; merge in specials as well
         (joined (mm-or-si128 (mm-and-si128 nonspecial b-isregular) (mm-andnot-si128 b-isregular inf-or-nan)))

         (sign-shift (mm-srli-epi32 (mm-castps-si128 justsign) 16))
         (final (mm-or-si128 joined sign-shift)))

    ;; ~28 SSE2 ops
    final))

(declaim (inline packed-short-to-single))
(defun packed-short-to-single (f)
  "Convert 4xf16 to 4xf32"
  (declare (type mezzano.simd:sse-vector f)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((magic (mm-castsi128-ps (mm-set1-epi32 (ash 113 23))))
         (zero (mm-set1-epi32 0))
         (shifted-exp (mm-set1-epi32 (ash #x7C00 13))) ; exponent mask after shift
         (mask-sign (mm-set1-epi32 #x8000))
         (mask-abs (mm-set1-epi32 #x7FFF))
         (justsign (mm-and-si128 f mask-sign))
         (abs-int (mm-and-si128 f mask-abs))
         (unadjusted-exp/sig (mm-slli-epi32 abs-int 13)) ; exponent/mantissa bits
         (exp (mm-and-si128 unadjusted-exp/sig shifted-exp)) ; just the exponent
         (exp-adjust (mm-set1-epi32 (ash (- 127 15) 23)))
         (adjusted-exp/sig (mm-add-epi32 unadjusted-exp/sig exp-adjust))
         ;; Inf/NaN?
         (inf/nan-p (mm-cmpeq-epi32 exp shifted-exp))
         (inf/nan-adjust (mm-set1-epi32 (ash (- 128 16) 23))) ; extra exp adjust
         (inf/nan-result (mm-add-epi32 adjusted-exp/sig inf/nan-adjust))
         ;; Zero/Denormal?
         (zero/denormal-p (mm-cmpeq-epi32 exp zero))
         (zero/denormal-adjusted (mm-add-epi32 adjusted-exp/sig
                                               (mm-set1-epi32 (ash 1 23))))
         (zero/denormal-result (mm-sub-ps zero/denormal-adjusted magic)) ; renormalize
         ;; Combine result.
         (join1 (mm-or-si128 (mm-and-si128 inf/nan-result inf/nan-p) (mm-andnot-si128 inf/nan-p adjusted-exp/sig)))
         (join2 (mm-or-si128 (mm-and-si128 zero/denormal-result zero/denormal-p) (mm-andnot-si128 zero/denormal-p join1)))
         ;; Mash sign bit in.
         (shifted-sign (mm-slli-epi32 justsign 16))
         (result (mm-or-si128 shifted-sign join2)))
    result))

(declaim (inline unpack-colour))
(defun unpack-colour (colour)
  "Convert a packed colour object to an unpacked single-float SSE vector."
  (declare (type colour colour))
  (let* ((icolour (mezzano.simd:make-sse-vector colour))
         (zero (mezzano.simd:make-sse-vector 0))
         (unpacked-colours (mezzano.simd:punpcklwd
                            (mezzano.simd:punpcklbw icolour zero)
                            zero))
         ;; Convert to float4.
         (float-colours (mezzano.simd:cvtdq2ps unpacked-colours))
         ;; Return to 0-1 range.
         (colours01 (mm-div-ps float-colours (mm-set1-ps 255.0))))
    colours01))

(declaim (inline short-float-colour (setf short-float-colour)))
(defun short-float-colour (array offset)
  (declare (type (simple-array short-float (*)) array)
           (type fixnum offset))
  (let* ((packed (mezzano.simd:sse-vector-short-float-4-ref array offset))
         (unpacked (mezzano.simd:punpcklwd packed (mezzano.simd:make-sse-vector 0))))
    (packed-short-to-single unpacked)))

(defun (setf short-float-colour) (value array offset)
  (declare (type (simple-array short-float (*)) array)
           (type fixnum offset)
           (type m128 value))
  (let* ((unpacked (packed-single-to-short value))
         (packed (mezzano.simd:packssdw unpacked unpacked)))
    (setf (mezzano.simd:sse-vector-short-float-4-ref array offset) packed)
    value))

(declaim (inline u32-colour (setf u32-colour)))
(defun u32-colour (array offset)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) (*)) array)
           (type fixnum offset))
  (let* ((colour-vec (mezzano.simd:make-sse-vector (aref array offset)))
         ;; Unpack bytes to doublewords.
         (unpacked-colours (mezzano.simd:punpcklwd
                            (mezzano.simd:punpcklbw colour-vec
                                            (mezzano.simd:make-sse-vector 0))
                            (mezzano.simd:make-sse-vector 0)))
         ;; Convert to float4.
         (float-colours (mezzano.simd:cvtdq2ps unpacked-colours))
         ;; Return to 0-1 range.
         (colours01 (mezzano.simd:divps float-colours (mm-set1-ps 255.0))))
    colours01))

(defun (setf u32-colour) (value array offset)
  (declare (type (simple-array (unsigned-byte 32) (*)) array)
           (type fixnum offset)
           (type m128 value))
  ;; Clamp to 0/1
  (let* ((argb01 (mezzano.simd:maxps
                  (mezzano.simd:minps value (mezzano.simd:make-sse-vector-single-float 1.0 1.0 1.0 1.0))
                  (mezzano.simd:make-sse-vector-single-float 0.0 0.0 0.0 0.0)))
         ;; Rescale to 0-255
         (argb255 (mezzano.simd:mulps
                   argb01
                   (mezzano.simd:make-sse-vector-single-float 255.0 255.0 255.0 255.0)))
         (argb8-unpacked (mezzano.simd:cvttps2dq argb255))
         (tmp (mezzano.simd:packssdw argb8-unpacked argb8-unpacked)) ; half-pack.
         (argb8 (mezzano.simd:packuswb tmp tmp))) ; fully packed
    (setf (aref array offset)
          (ldb (byte 32 0) (mezzano.simd:sse-vector-value argb8)))
    value))

(declaim (inline alpha-blend))
(defun alpha-blend (source dest)
  (declare (type m128 source dest))
  (let* (;; source alpha (0A0A0A0A)
         (exploded-alpha (mezzano.simd:shufps source source #4r3333))
         ;; inverse source alpha (0A0A0A0A)
         (inverse-alpha (mezzano.simd:subps (mezzano.simd:make-sse-vector-single-float 1.0 1.0 1.0 1.0)
                                            exploded-alpha))
         ;; Multiply dest by inverse alpha.
         (adjusted-dest (mezzano.simd:mulps dest inverse-alpha))
         ;; Blend.
         (blended (mezzano.simd:addps source adjusted-dest)))
    blended))

(declaim (inline source-only-blend))
(defun source-only-blend (source dest)
  (declare (type m128 source dest)
           (ignore dest))
  source)

(declaim (inline xor-blend))
(defun xor-blend (source dest)
  (declare (type m128 source dest))
  (mezzano.simd:xorps source dest))

(declaim (inline bitset-body))
(defun bitset-body (nrows ncols colour to to-offset to-stride to-elts to-read-fn to-write-fn blend-fn)
  (declare (type fixnum nrows ncols to-offset to-stride)
           (type colour colour))
  (when (not (eql to-elts 1))
    (setf to-offset (the fixnum (* to-offset to-elts))
          to-stride (the fixnum (* to-stride to-elts))))
  (when (and (> ncols 0)
             (> nrows 0))
    (loop
       with colour-vec of-type m128 = (unpack-colour colour)
       for y fixnum below nrows
       do
         (loop
            with to-offset* fixnum = to-offset
            for x fixnum below ncols
            do
              (funcall to-write-fn
                       (funcall blend-fn
                                colour-vec
                                (funcall to-read-fn to to-offset*))
                       to to-offset*)
              (incf to-offset* to-elts))
         (incf to-offset to-stride))))

(defun fast-short-float-array-bitset-blend (nrows ncols colour to to-offset to-stride)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum nrows ncols to-offset to-stride)
           (type colour colour)
           (type (simple-array short-float (*)) to))
  (bitset-body nrows ncols
               colour
               to to-offset to-stride 4 #'short-float-colour #'(setf short-float-colour)
               #'alpha-blend))

(defun short-float-array-bitset (nrows ncols colour to-array to-row to-col)
  (check-type to-array (array short-float (* * 4)))
  (check-type colour colour)
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest nrows ncols to-array to-row to-col)
    (check-type to (simple-array short-float (*)))
    (fast-short-float-array-bitset-blend
     nrows ncols colour to to-offset to-stride)))

(declaim (inline bitblt-body))
(defun bitblt-body (nrows ncols from from-offset from-stride from-elts from-read-fn from-write-fn to to-offset to-stride to-elts to-read-fn to-write-fn blend-fn)
  (declare (ignore from-write-fn)
           (type fixnum nrows ncols from-offset from-stride to-offset to-stride))
  (when (not (eql from-elts 1))
    (setf from-offset (the fixnum (* from-offset from-elts))
          from-stride (the fixnum (* from-stride from-elts))))
  (when (not (eql to-elts 1))
    (setf to-offset (the fixnum (* to-offset to-elts))
          to-stride (the fixnum (* to-stride to-elts))))
  (when (and (> ncols 0)
             (> nrows 0))
    (loop
       for y fixnum below nrows
       do
         (loop
            with to-offset* fixnum = to-offset
            with from-offset* fixnum = from-offset
            for x fixnum below ncols
            do
              (funcall to-write-fn
                       (funcall blend-fn
                                (funcall from-read-fn from from-offset*)
                                (funcall to-read-fn to to-offset*))
                       to to-offset*)
              (incf from-offset* from-elts)
              (incf to-offset* to-elts))
         (incf from-offset from-stride)
         (incf to-offset to-stride))))

(defun fast-short-float/octet-colour-array-bitblt (nrows ncols from from-offset from-stride to to-offset to-stride)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum nrows ncols from-offset from-stride to-offset to-stride)
           (type (simple-array short-float (*)) from)
           (type (simple-array (unsigned-byte 32) (*)) to))
  (bitblt-body nrows ncols
               from from-offset from-stride 4 #'short-float-colour #'(setf short-float-colour)
               to to-offset to-stride 1 #'u32-colour #'(setf u32-colour)
               #'alpha-blend))

(defun short-float/octet-colour-array-bitblt (nrows ncols from-array from-row from-col to-array to-row to-col)
  (check-type from-array (array short-float (* * 4)))
  (check-type to-array (array colour (* *)))
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (check-type from (simple-array short-float (*)))
    (assert (simple-ub32-vector-p to))
    (fast-short-float/octet-colour-array-bitblt
     nrows ncols from from-offset from-stride to to-offset to-stride)))
)
