;;;; Colours

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

(defun colour-over (ca cb)
  (let* ((aa (colour-alpha ca))
         (aa^ (- 1 aa))
         (ba (colour-alpha cb)))
    (make-colour (+ (colour-red ca) (* (colour-red cb) aa^))
                 (+ (colour-green ca) (* (colour-green cb) aa^))
                 (+ (colour-blue ca) (* (colour-blue cb) aa^))
                 (+ aa (* ba aa^)))))

(defstruct (colour-matrix
             (:constructor %make-colour-matrix))
  (elements (error "Elements not supplied") :type matrix4 :read-only t))

(defun make-colour-matrix (a b c d e f g h i j k l m n o p)
  (let ((elts (make-array 16 :element-type 'single-float)))
    (setf (aref elts  0) a
          (aref elts  1) e
          (aref elts  2) i
          (aref elts  3) m
          (aref elts  4) b
          (aref elts  5) f
          (aref elts  6) j
          (aref elts  7) n
          (aref elts  8) c
          (aref elts  9) g
          (aref elts 10) k
          (aref elts 11) o
          (aref elts 12) d
          (aref elts 13) h
          (aref elts 14) l
          (aref elts 15) p)
    (%make-colour-matrix :elements elts)))

(defun colour-matrix-element (colour-matrix row col)
  (aref (colour-matrix-elements colour-matrix) (+ (* row 4) col)))

#+x86-64
(progn
(deftype simd-colour ()
  ;; A 4-element ARGB single-float SIMD vector containing non-premultiplied colour.
  ;; The alpha channel is stored in lane 0, red in lane 1, green in lane 2, and
  ;; blue in lane 3.
  'mezzano.simd:sse-vector)

(declaim (inline make-simd-colour))
(defun make-simd-colour (&optional (red 0.0) (green 0.0) (blue 0.0) (alpha 1.0))
  (declare (type single-float red green blue alpha))
  (mezzano.simd:make-sse-vector-single-float
   alpha red green blue))

(declaim (inline simd-colour-elements))
(defun simd-colour-elements (simd-colour)
  "Return the red, green, blue, and alpha channels of SIMD-COLOUR as values."
  (declare (type simd-colour-vector simd-colour))
  (values (mezzano.simd:sse-vector-single-float-element simd-colour 1)
          (mezzano.simd:sse-vector-single-float-element simd-colour 2)
          (mezzano.simd:sse-vector-single-float-element simd-colour 3)
          (mezzano.simd:sse-vector-single-float-element simd-colour 0)))

(declaim (inline simd-colour-to-floats))
(defun simd-colour-to-floats (colour)
  "Unpack 4 bytes in a UB32 to 4 floats."
  (declare (type colour colour))
  (let* ((colour-vec (mezzano.simd:make-sse-vector colour))
         (zero-vec (mezzano.simd:make-sse-vector 0))
         ;; Unpack bytes to doublewords.
         (unpacked-colour (mezzano.simd:punpcklwd
                            (mezzano.simd:punpcklbw colour-vec zero-vec)
                            zero-vec))
         ;; Convert to single-float.
         (float-colour (mezzano.simd:cvtdq2ps unpacked-colour)))
    float-colour))

(declaim (inline simd-floats-to-colour))
(defun simd-floats-to-colour (vec)
  "Pack 4 floats to 4 bytes in a UB32."
  (declare (type mezzano.simd:sse-vector vec))
  (let* ((unpacked (mezzano.simd:cvtps2dq vec))
         (half-pack (mezzano.simd:packssdw unpacked unpacked))
         (result (mezzano.simd:packuswb half-pack half-pack)))
    (ldb (byte 32 0) (mezzano.simd:sse-vector-value result))))

(declaim (inline simd-unpack-colour))
(defun simd-unpack-colour (colour)
  "Convert a COLOUR to a SIMD-COLOUR."
  (declare (type colour colour))
  (let* ((colour-vec (simd-colour-to-floats colour))
         ;; Return to 0-1 range.
         (colours01 (mezzano.simd:mulps
                     colour-vec
                     (mezzano.simd:make-sse-vector-single-float
                      (/ 255.0) (/ 255.0) (/ 255.0) (/ 255.0))))
         ;; Convert back to straight colour.
         ;; Make sure alpha is non-zero. If alpha really is zero then
         ;; all the other elements must also be zero.
         (alpha (mezzano.simd:maxps
                 (mezzano.simd:shufps colours01 colours01 #4r3333)
                 (mezzano.simd:make-sse-vector-single-float
                  single-float-epsilon
                  single-float-epsilon
                  single-float-epsilon
                  ;; Leave alpha unchanged when converting.
                  1.0)))
         (bgra (mezzano.simd:divps colours01 alpha))
         ;; Fix order.
         (result (mezzano.simd:shufps bgra bgra #4r0123)))
    result))

(declaim (inline simd-pack-colour))
(defun simd-pack-colour (simd-colour)
  "Convert SIMD-COLOUR to a COLOUR. Channel values are clamped to 0-1."
  (declare (type simd-colour simd-colour))
  (let* ((argb01 (mezzano.simd:maxps
                  (mezzano.simd:minps simd-colour (mezzano.simd:make-sse-vector-single-float 1.0 1.0 1.0 1.0))
                  (mezzano.simd:make-sse-vector-single-float 0.0 0.0 0.0 0.0)))
         ;; Multiple colour channels with alpha.
         (alpha-scale (mezzano.simd:movss
                       (mezzano.simd:shufps argb01 argb01 #4r0000)
                       (mezzano.simd:make-sse-vector-single-float 1.0)))
         (argb01-premult (mezzano.simd:mulps argb01 alpha-scale))
         ;; Scale to 0-255
         (argb-scaled (mezzano.simd:mulps
                       argb01-premult
                       (mezzano.simd:make-sse-vector-single-float 255.0 255.0 255.0 255.0)))
         ;; Fix the order.
         (bgra-scaled (mezzano.simd:shufps argb-scaled argb-scaled #4r0123)))
    (simd-floats-to-colour bgra-scaled)))

(defun %colour-lerp (c1 c2 a)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type colour c1 c2)
           (type single-float a))
  (let* ((c1-vec (simd-colour-to-floats c1))
         (c2-vec (simd-colour-to-floats c2))
         (a-tmp (mezzano.simd:make-sse-vector-single-float a))
         (a-vec (mezzano.simd:shufps
                 a-tmp a-tmp
                 #4r0000))
         (1-a-vec (mezzano.simd:subps
                   (mezzano.simd:make-sse-vector-single-float 1.0 1.0 1.0 1.0)
                   a-vec))
         (c (mezzano.simd:addps
             (mezzano.simd:mulps 1-a-vec c1-vec)
             (mezzano.simd:mulps a-vec c2-vec))))
    (simd-floats-to-colour c)))

(defun colour-lerp (c1 c2 a)
  "Lerp between C1 and C2 based on A"
  (assert (typep a 'single-float))
  (assert (typep c1 'colour))
  (assert (typep c2 'colour))
  (%colour-lerp c1 c2 a))

(deftype matrix4 ()
  '(simple-array single-float (16)))

(declaim (inline matrix4-column (setf matrix4-column)))
(defun matrix4-column (mat col)
  (mezzano.simd:sse-vector-single-float-ref mat 4 (* col 4)))
(defun (setf matrix4-column) (vec mat col)
  (setf (mezzano.simd:sse-vector-single-float-ref mat 4 (* col 4)) vec))

(declaim (inline matrix4-vector4-multiply))
(defun matrix4-vector4-multiply (mat vec)
  (declare (type mezzano.simd:sse-vector vec)
           (type matrix4 mat))
  (let* ((c0 (matrix4-column mat 0))
         (c1 (matrix4-column mat 1))
         (c2 (matrix4-column mat 2))
         (c3 (matrix4-column mat 3))
         (xv (mezzano.simd:shufps vec vec #4r0000))
         (yv (mezzano.simd:shufps vec vec #4r1111))
         (zv (mezzano.simd:shufps vec vec #4r2222))
         (wv (mezzano.simd:shufps vec vec #4r3333))
         (t0 (mezzano.simd:mulps c0 xv)) ; ax ex ix mx
         (t1 (mezzano.simd:mulps c1 yv)) ; by fy jy ny
         (t2 (mezzano.simd:mulps c2 zv)) ; cz gz kz oz
         (t3 (mezzano.simd:mulps c3 wv))) ; dw hw lw pw
    (mezzano.simd:addps (mezzano.simd:addps t0 t1) (mezzano.simd:addps t2 t3))))

(defun matrix4-multiply (result ma mb)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type matrix4 result ma mb))
  (let* ((mac0 (matrix4-column ma 0)) ; a e i m
         (mac1 (matrix4-column ma 1)) ; b f j n
         (mac2 (matrix4-column ma 2)) ; c g k o
         (mac3 (matrix4-column ma 3)) ; d h l p
         (mbc0 (matrix4-column mb 0)) ; A E I M
         (mbc1 (matrix4-column mb 1)) ; B F J N
         (mbc2 (matrix4-column mb 2)) ; C G K O
         (mbc3 (matrix4-column mb 3))); D H L P
    (flet ((frob (b-column)
             (mezzano.simd:addps
              (mezzano.simd:addps
               (mezzano.simd:mulps mac0 (mezzano.simd:shufps b-column b-column #4r0000))    ; a * R0, e * R0, i * R0, m * R0
               (mezzano.simd:mulps mac1 (mezzano.simd:shufps b-column b-column #4r1111)))   ; b * R1, f * R1, j * R1, n * R1
              (mezzano.simd:addps
               (mezzano.simd:mulps mac2 (mezzano.simd:shufps b-column b-column #4r2222))    ; c * R2, g * R2, k * R2, o * R2
               (mezzano.simd:mulps mac3 (mezzano.simd:shufps b-column b-column #4r3333)))))); d * R3, h * R3, l * R3, p * R3
      (setf (matrix4-column result 0) (frob mbc0)
            (matrix4-column result 1) (frob mbc1)
            (matrix4-column result 2) (frob mbc2)
            (matrix4-column result 3) (frob mbc3))))
  result)

(defun colour-matrix-matrix-multiply (ma mb)
  "Multiply two colour matrices together, returning the result as a new matrix."
  (assert (colour-matrix-p ma))
  (assert (colour-matrix-p mb))
  (let ((mr (make-array 16 :element-type 'single-float)))
    (matrix4-multiply mr
                      (colour-matrix-elements ma)
                      (colour-matrix-elements mb))
    (%make-colour-matrix :elements mr)))

(declaim (inline %colour-matrix-multiply))
(defun %colour-matrix-multiply (matrix colour)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type matrix4 matrix)
           (type colour colour))
  (let* ((simd-colour (simd-unpack-colour colour))
         ;; Fetch the RGB channels only, leaving 1.0 in the alpha channel.
         (original-rgb (mezzano.simd:movss
                        simd-colour
                        (mezzano.simd:make-sse-vector-single-float 1.0)))
         ;; Mangle & unmangle the vector ordering so the colour matrix
         ;; layout makes more sense. This could be fixed up by rearranging
         ;; the matrix layout, but that's a bit beyond me.
         (new-rgb (matrix4-vector4-multiply
                   matrix
                   (mezzano.simd:shufps original-rgb original-rgb
                                        #4r0321)))
         ;; Restore alpha.
         (new-colour (mezzano.simd:movss (mezzano.simd:shufps new-rgb new-rgb
                                                              #4r2103)
                                         simd-colour)))
    (simd-pack-colour new-colour)))

(defun colour-matrix-multiply (colour-matrix colour)
  "Multiply the RGB elements of COLOUR with COLOUR-MATRIX."
  (declare (notinline %colour-matrix-multiply))
  (assert (colour-matrix-p colour-matrix))
  (assert (typep colour 'colour))
  (%colour-matrix-multiply (colour-matrix-elements colour-matrix) colour))
)
