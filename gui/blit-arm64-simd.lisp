;;;; SIMD-accelerated blitting functions.

(in-package :mezzano.gui)

(declaim (inline u16.4*-upper))
(defun u16.4*-upper (x y)
  "Multiply x & y, returning the unsigned upper half of the full 32-bit result."
  (mezzano.simd.arm64:u16.8-from-u32.4
   (mezzano.simd.arm64:u32.4-shiftr (mezzano.simd.arm64:u16.8*-long x y) 16)))

(declaim (inline u8.16-from-u16.8-pair))
(defun u8.16-from-u16.8-pair (lo hi)
  "Truncate a pair of 16.8 values into a 8.16 value."
  (mezzano.simd.arm64:u8.16-from-u16.8-hi
   (mezzano.simd.arm64:u8.16-from-u16.8 lo)
   hi))

(declaim (inline u16.8-from-u32.4-pair))
(defun u16.8-from-u32.4-pair (lo hi)
  "Truncate a pair of 32.4 values into a 16.4 value."
  (mezzano.simd.arm64:u16.8-from-u32.4-hi
   (mezzano.simd.arm64:u16.8-from-u32.4 lo)
   hi))

(declaim (inline u16.8*-upper))
(defun u16.8*-upper (x y)
  "Multiply x & y, returning the unsigned upper half of the full 32-bit result."
  (let* ((lo (mezzano.simd.arm64:u16.8*-long x y))
         (hi (mezzano.simd.arm64:u16.8*-long-hi x y))
         (lo16 (mezzano.simd.arm64:u32.4-shiftr lo 16))
         (hi16 (mezzano.simd.arm64:u32.4-shiftr hi 16)))
    (u16.8-from-u32.4-pair lo16 hi16)))

;; Line traversal functions.

(defun %bitset-mask-8-line (blender to to-offset ncols mask mask-offset colour)
  (declare (optimize speed (safety 0) (debug 1))
           (type function blender)
           (type fixnum to-offset ncols mask-offset)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type (simple-array (unsigned-byte 8) (*)) mask)
           (type (unsigned-byte 32) colour))
  (loop for i fixnum below ncols do
    (let ((mask-byte (aref mask mask-offset)))
      ;; Component multiply colour with mask.
      (let* ((vec-colour (mezzano.simd.arm64:u8.16! (mezzano.simd.arm64:u32.4! colour))) ; (0000ARGB)
             (colour*mask (mezzano.simd.arm64:u8.16*-long vec-colour mask-byte))
             ;; Rescale back to 0-255, rounding correctly.
             (colour*mask-16.8 (u16.4*-upper
                                (mezzano.simd.arm64:u16.8+ colour*mask #x0080)
                                #x0101))
             (final-colour (mezzano.simd.arm64:u8.16-from-u16.8 colour*mask-16.8))
             (result (mezzano.simd.arm64:u32.4-lane-extract
                      (mezzano.simd.arm64:u32.4! final-colour)
                      0)))
        (funcall blender result to to-offset)))
    (incf to-offset)
    (incf mask-offset)))

;;; Final blending functions.

(defun %%alpha-blend-one-argb8888-argb8888 (source to to-offset)
  (declare (optimize speed (safety 0) (debug 0))
           (type (unsigned-byte 32) source)
           (type (simple-array (unsigned-byte 32) (*)) to)
           (type fixnum to-offset))
  ;; Double bitwise cast here because we start off with u32 values.
  (let* ((src (mezzano.simd.arm64:u8.16! (mezzano.simd.arm64:u32.4! source)))
         (dst (mezzano.simd.arm64:u8.16! (mezzano.simd.arm64:u32.4! (aref to to-offset))))
         ;; Extract source alpha.
         (sa (mezzano.simd.arm64:u8.16-dup src 3))
         ;; Inverse alpha, this is 1-as. NOT gives us that trivially, thanks
         ;; to wraparound behaviour.
         (ia (mezzano.simd.arm64:u8.16-not sa))
         ;; Multiply dest by inverse alpha.
         (dst*ia (mezzano.simd.arm64:u8.16*-long dst ia))
         ;; Rescale back to 0-255, rounding correctly.
         (dst*ia^-16.8 (u16.4*-upper (mezzano.simd.arm64:u16.8+ dst*ia #x0080) #x0101))
         ;; Repack.
         (dst*ia^ (mezzano.simd.arm64:u8.16-from-u16.8 dst*ia^-16.8))
         ;; Finally, do the actual blend.
         (blended (mezzano.simd.arm64:u8.16+-saturating dst*ia^ src))
         ;; Store back.
         (result (mezzano.simd.arm64:u32.4-lane-extract
                  (mezzano.simd.arm64:u32.4! blended)
                  0)))
    (setf (aref to to-offset) result)))

(defun alpha-blend-quad (source source-offset to to-offset)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) (*)) source to)
           (type fixnum source-offset to-offset))
  (let* ((src (mezzano.simd.arm64:u8.16!
               (mezzano.simd.arm64:u32.4-aref source source-offset)))
         (dst (mezzano.simd.arm64:u8.16!
               (mezzano.simd.arm64:u32.4-aref to to-offset)))
         (source-alpha (mezzano.simd.arm64:u32.4-and
                        (mezzano.simd.arm64:u32.4-shiftr (mezzano.simd.arm64:u32.4! src) 24)
                        #xFF))
         (source-alpha-splat (mezzano.simd.arm64:u32.4* source-alpha #x01010101))
         (inverse-alpha (mezzano.simd.arm64:u8.16-not (mezzano.simd.arm64:u8.16! source-alpha-splat)))
         ;; Multiply dest by inverse alpha.
         (mult-lo (mezzano.simd.arm64:u8.16*-long dst inverse-alpha))
         (mult-hi (mezzano.simd.arm64:u8.16*-long-hi dst inverse-alpha))
         ;; Rescale back to 0-255, rounding correctly.
         (adjusted-dest-lo (u16.8*-upper (mezzano.simd.arm64:u16.8+ mult-lo #x0080) #x0101))
         (adjusted-dest-hi (u16.8*-upper (mezzano.simd.arm64:u16.8+ mult-hi #x0080) #x0101))
         ;; Repack.
         (adjusted-dest (u8.16-from-u16.8-pair adjusted-dest-lo adjusted-dest-hi))
         ;; Finally, do the actual blend.
         (blended (mezzano.simd.arm64:u32.4!
                   (mezzano.simd.arm64:u8.16+-saturating adjusted-dest src))))
    (setf (mezzano.simd.arm64:u32.4-aref to to-offset) blended))
  nil)

(defun alpha-blend-interleaved (source source-offset to to-offset)
  "Blend 16 pixels at once."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) (*)) source to)
           (type fixnum source-offset to-offset))
  (multiple-value-bind (sb sg sr sa)
      (mezzano.simd.arm64:u8.16-aref-4-interleaved-in-u32 source source-offset)
    (multiple-value-bind (db dg dr da)
        (mezzano.simd.arm64:u8.16-aref-4-interleaved-in-u32 to to-offset)
      (let* ((inverse-alpha (mezzano.simd.arm64:u8.16-not sa))
             ;; Multiply dest by inverse alpha.
             (da*ia-lo (mezzano.simd.arm64:u8.16*-long    da inverse-alpha))
             (da*ia-hi (mezzano.simd.arm64:u8.16*-long-hi da inverse-alpha))
             (dr*ia-lo (mezzano.simd.arm64:u8.16*-long    dr inverse-alpha))
             (dr*ia-hi (mezzano.simd.arm64:u8.16*-long-hi dr inverse-alpha))
             (dg*ia-lo (mezzano.simd.arm64:u8.16*-long    dg inverse-alpha))
             (dg*ia-hi (mezzano.simd.arm64:u8.16*-long-hi dg inverse-alpha))
             (db*ia-lo (mezzano.simd.arm64:u8.16*-long    db inverse-alpha))
             (db*ia-hi (mezzano.simd.arm64:u8.16*-long-hi db inverse-alpha))
             ;; Rescale back to 0-255, rounding correctly.
             (da*ia^-lo (u16.8*-upper (mezzano.simd.arm64:u16.8+ da*ia-lo #x0080) #x0101))
             (da*ia^-hi (u16.8*-upper (mezzano.simd.arm64:u16.8+ da*ia-hi #x0080) #x0101))
             (dr*ia^-lo (u16.8*-upper (mezzano.simd.arm64:u16.8+ dr*ia-lo #x0080) #x0101))
             (dr*ia^-hi (u16.8*-upper (mezzano.simd.arm64:u16.8+ dr*ia-hi #x0080) #x0101))
             (dg*ia^-lo (u16.8*-upper (mezzano.simd.arm64:u16.8+ dg*ia-lo #x0080) #x0101))
             (dg*ia^-hi (u16.8*-upper (mezzano.simd.arm64:u16.8+ dg*ia-hi #x0080) #x0101))
             (db*ia^-lo (u16.8*-upper (mezzano.simd.arm64:u16.8+ db*ia-lo #x0080) #x0101))
             (db*ia^-hi (u16.8*-upper (mezzano.simd.arm64:u16.8+ db*ia-hi #x0080) #x0101))
             ;; Repack.
             (da*ia^ (u8.16-from-u16.8-pair da*ia^-lo da*ia^-hi))
             (dr*ia^ (u8.16-from-u16.8-pair dr*ia^-lo dr*ia^-hi))
             (dg*ia^ (u8.16-from-u16.8-pair dg*ia^-lo dg*ia^-hi))
             (db*ia^ (u8.16-from-u16.8-pair db*ia^-lo db*ia^-hi))
             ;; Finally, do the actual blend.
             (ba (mezzano.simd.arm64:u8.16+-saturating da*ia^ sa))
             (br (mezzano.simd.arm64:u8.16+-saturating dr*ia^ sr))
             (bg (mezzano.simd.arm64:u8.16+-saturating dg*ia^ sg))
             (bb (mezzano.simd.arm64:u8.16+-saturating db*ia^ sb)))
        (setf (mezzano.simd.arm64:u8.16-aref-4-interleaved-in-u32 to to-offset)
              (values bb bg br ba)))))
  nil)
