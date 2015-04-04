;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Optimized pluggable blitter functions.
;;;; The low-level pixel blending functions use a custom calling
;;;; convetion and must not be called directly from lisp.
;;;; RAX contains the source pixel.
;;;; RDI contains the address of the destination pixel.
;;;; RAX, RBX, RDX and MMX/SSE registers are caller save.
;;;; All other registers are callee save.
;;;; Higher-level line blenders must be called with the GC deferred.

(in-package :mezzano.gui)

(defun compute-blit-info-dest-src (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Clamp parameters to array boundaries, return the stride of both arrays and their undisplaced base arrays."
  (let ((from-width (array-dimension from-array 1))
        (from-height (array-dimension from-array 0))
        (from-offset 0)
        (to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0))
        (to-offset 0))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (decf from-row to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (decf from-col to-col)
      (setf to-col 0))
    ;; Clamp from row/column.
    (when (< from-row 0)
      (incf nrows from-row)
      (decf to-row from-row)
      (setf from-row 0))
    (when (< from-col 0)
      (incf ncols from-col)
      (decf to-col from-col)
      (setf from-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (max (min nrows (- to-height to-row) (- from-height from-row)) 0))
    (setf ncols (max (min ncols (- to-width to-col) (- from-width from-col)) 0))
    ;; Undisplace displaced arrays
    (multiple-value-bind (from-displaced-to from-displaced-offset)
        (array-displacement from-array)
      (when from-displaced-to
        (setf from-array from-displaced-to
              from-offset from-displaced-offset)))
    (multiple-value-bind (to-displaced-to to-displaced-offset)
        (array-displacement to-array)
      (when to-displaced-to
        (setf to-array to-displaced-to
              to-offset to-displaced-offset)))
    (incf from-offset (+ (* from-row from-width) from-col))
    (incf to-offset (+ (* to-row to-width) to-col))
    (values nrows ncols
            from-array from-offset from-width
            to-array to-offset to-width)))

(defun bitblt (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Like %BITBLT, but clamps to the array dimensions."
  (let ((from-width (array-dimension from-array 1))
        (from-height (array-dimension from-array 0))
        (to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0)))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (decf from-row to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (decf from-col to-col)
      (setf to-col 0))
    ;; Clamp from row/column.
    (when (< from-row 0)
      (incf nrows from-row)
      (decf to-row from-row)
      (setf from-row 0))
    (when (< from-col 0)
      (incf ncols from-col)
      (decf to-col from-col)
      (setf from-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (min nrows (- to-height to-row) (- from-height from-row)))
    (setf ncols (min ncols (- to-width to-col) (- from-width from-col)))
    (when (and (> nrows 0)
               (> ncols 0))
      (sys.int::%bitblt nrows ncols from-array from-row from-col to-array to-row to-col))))

(defun bitset (nrows ncols val to-array to-row to-col)
  "Like %BITSET, but clamps to the array dimensions."
  (let ((to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0)))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (setf to-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (min nrows (- to-height to-row)))
    (setf ncols (min ncols (- to-width to-col)))
    (when (and (> nrows 0)
               (> ncols 0))
      (sys.int::%bitset nrows ncols val to-array to-row to-col))))

(defun %simple-array-data-pointer (array)
  "Find the address of ARRAY's first data element.
ARRAY must not be a displaced array."
  (when (not (sys.int::%simple-1d-array-p array))
    ;; Complex array, convert to storage.
    (setf array (sys.int::%complex-array-storage array)))
  (assert (sys.int::%simple-1d-array-p array))
  ;; Array to address.
  (+ (sys.int::lisp-object-address array) (- sys.int::+tag-object+) 8))

;;; SETTER(:r8) NCOLS(:r9) COLOUR(:r10) MASK(:r11) MASK-OFFSET(:r12) TO(+8) TO-OFFSET(+16)
;;; TO and TO-OFFSET are fixnums, no GC info required.
(sys.int::define-lap-function %bitset-mask-8-line ()
  (sys.lap-x86:mov64 :rsi :r11)
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+) ; RSI = MASK address (byte address)
  (sys.lap-x86:mov64 :rdi (:rsp 8))
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; RDI = TO address (byte address)
  (sys.lap-x86:sar64 (:rsp 16) #.sys.int::+n-fixnum-bits+) ; TO-OFFSET (raw)
  (sys.lap-x86:shl64 (:rsp 16) 2) ; TO-OFFSET * 4(raw)
  (sys.lap-x86:add64 :rdi (:rsp 16)) ; RDI = TO + TO-OFFSET, first pixel on the line.
  (sys.lap-x86:sar64 :r10 #.sys.int::+n-fixnum-bits+) ; R10 = colour (raw)
  (sys.lap-x86:sar64 :r12 #.sys.int::+n-fixnum-bits+) ; R12 = MASK-OFFEST (raw)
  (sys.lap-x86:add64 :rsi :r12) ; RSI = MASK + MASK-OFFSET, first pixel in the mask line.
  (sys.lap-x86:test64 :r9 :r9) ; R9(NCOLS) == 0?
  (sys.lap-x86:jz out) ; true, goto out.
  head
  (sys.lap-x86:mov8 :al (:rsi)) ; AL = MASK-PIXEL
  (sys.lap-x86:test8 :al :al) ; MASK-CURRENT == 0?
  (sys.lap-x86:jz next) ; true, skip pixel.
  (sys.lap-x86:cmp8 :al #xFF) ; MASK-CURRENT == #xFF?
  (sys.lap-x86:jne blend-alpha) ; true, do full blend.
  (sys.lap-x86:mov32 :eax :r10d) ; EAX = COLOUR.
  invoke-setter
  (sys.lap-x86:call (:r8 #.(+ (- sys.int::+tag-object+) 8))) ; Call SETTER.
  next
  (sys.lap-x86:add64 :rdi 4) ; Advance TO address.
  (sys.lap-x86:add64 :rsi 1) ; Advance MASK address.
  (sys.lap-x86:sub64 :r9 #.(ash 1 sys.int::+n-fixnum-bits+)) ; Decrement NCOLS, set ZF when zero.
  (sys.lap-x86:jnz head) ; loop when stuff left to do.
  out
  ;; Clear the two data registers that got smashed.
  (sys.lap-x86:xor32 :r10d :r10d)
  (sys.lap-x86:xor32 :r12d :r12d)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:ret)
  ;; Multiply colour's alpha channel by the mask.
  blend-alpha
  (sys.lap-x86:and32 :eax #x000000FF) ; eax = alpha (000A)
  (sys.lap-x86:mov32 :edx :r10d) ; edx = colour (ARGB)
  (sys.lap-x86:shr32 :edx 24) ; edx = colour's alpha (000A)
  (sys.lap-x86:mul8 :dl) ; ax = mask * colour alpha (00Ax)
  (sys.lap-x86:add16 :ax 255) ; round to nearest (00Ax)
  (sys.lap-x86:shl32 :eax 16) ; (Ax00)
  (sys.lap-x86:and32 :eax #xFF000000) ; (A000)
  (sys.lap-x86:mov32 :edx :r10d) ; edx = colour (ARGB)
  (sys.lap-x86:and32 :edx #x00FFFFFF) ; (0RGB)
  (sys.lap-x86:or32 :eax :edx) ; create final colour with proper alpha (ARGB)
  (sys.lap-x86:jmp invoke-setter))

(defun bitset-argb-xrgb-mask-8 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  "Fill a rectangle with COLOUR using an 8-bit mask."
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (equal (array-element-type mask) '(unsigned-byte 8)))
    (assert (equal (array-element-type to) '(unsigned-byte 32)))
    ;; Stop early for 100% transparent colours.
    (unless (zerop (ldb (byte 8 24) colour))
      (mezzano.supervisor:with-pseudo-atomic
        (setf to (%simple-array-data-pointer to))
        (setf mask (%simple-array-data-pointer mask))
        (dotimes (y nrows)
          (%bitset-mask-8-line #'%%alpha-blend-one-argb8888-xrgb8888 ncols colour mask mask-offset to to-offset)
          (incf mask-offset mask-stride)
          (incf to-offset to-stride))))))

;;; SETTER NCOLS COLOUR MASK MASK-OFFSET TO TO-OFFSET
(sys.int::define-lap-function %bitset-mask-1-line ()
  (sys.lap-x86:mov64 :rsi :r11)
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+) ; RSI = MASK address (byte address)
  (sys.lap-x86:mov64 :rdi (:rsp 8))
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; RSI = TO address (byte address)
  (sys.lap-x86:sar64 (:rsp 16) #.sys.int::+n-fixnum-bits+) ; TO-OFFSET (raw)
  (sys.lap-x86:shl64 (:rsp 16) 2) ; TO-OFFSET * 4(raw)
  (sys.lap-x86:add64 :rdi (:rsp 16)) ; RDI = TO + TO-OFFSET, first pixel on the line.
  (sys.lap-x86:sar64 :r10 #.sys.int::+n-fixnum-bits+) ; R10 = colour (raw)
  (sys.lap-x86:sar64 :r12 #.sys.int::+n-fixnum-bits+) ; R12 = MASK-OFFEST (raw)
  (sys.lap-x86:mov64 :rcx :r12)
  (sys.lap-x86:and8 :cl #b111111) ; CL = current bit in mask.
  (sys.lap-x86:mov32 :eax 1)
  (sys.lap-x86:shl64 :rax :cl)
  (sys.lap-x86:mov64 :rcx :rax) ; RAX = mask mask.
  (sys.lap-x86:sar64 :r12 6) ; R12 = offset to mask word.
  (sys.lap-x86:lea64 :rsi (:rsi (:r12 8))) ; (RSI) = current mask word.
  (sys.lap-x86:test64 :r9 :r9)
  (sys.lap-x86:jmp test)
  head
  (sys.lap-x86:test64 (:rsi) :rcx)
  (sys.lap-x86:jz next)
  (sys.lap-x86:mov32 :eax :r10d)
  (sys.lap-x86:call (:r8 #.(+ (- sys.int::+tag-object+) 8)))
  next
  (sys.lap-x86:add64 :rdi 4)
  (sys.lap-x86:add64 :rcx :rcx)
  (sys.lap-x86:jnc no-carry)
  (sys.lap-x86:mov32 :ecx 1)
  (sys.lap-x86:add64 :rsi 8)
  no-carry
  (sys.lap-x86:sub64 :r9 #.(ash 1 sys.int::+n-fixnum-bits+))
  test
  (sys.lap-x86:jnz head)
  ;; Clear the two data registers that got smashed.
  (sys.lap-x86:xor32 :r10d :r10d)
  (sys.lap-x86:xor32 :r12d :r12d)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:ret))

(declaim (inline %bitset-mask-1-whole))
(defun %bitset-mask-1-whole (setter nrows ncols colour mask mask-offset mask-stride to to-offset to-stride)
  (mezzano.supervisor:with-pseudo-atomic
    (setf to (%simple-array-data-pointer to))
    (setf mask (%simple-array-data-pointer mask))
    (dotimes (y nrows)
      (%bitset-mask-1-line setter ncols colour mask mask-offset to to-offset)
      (incf mask-offset mask-stride)
      (incf to-offset to-stride))))

(defun bitset-argb-xrgb-mask-1 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  "Fill a rectangle with COLOUR using a 1-bit mask."
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (equal (array-element-type mask) 'bit))
    (assert (equal (array-element-type to) '(unsigned-byte 32)))
    (case (ldb (byte 8 24) colour)
      ;; Stop early for 100% transparent colours.
      (0)
      ;; Don't blend 100% opaque colours.
      (#xFF (%bitset-mask-1-whole #'%%set-one-xrgb8888-xrgb8888 nrows ncols colour mask mask-offset mask-stride to to-offset to-stride))
      ;; Fall back on blend function.
      (t (%bitset-mask-1-whole #'%%alpha-blend-one-argb8888-xrgb8888 nrows ncols colour mask mask-offset mask-stride to to-offset to-stride)))))

;;; SETTER NCOLS COLOUR TO TO-OFFSET
(sys.int::define-lap-function %bitset-line ()
  (sys.lap-x86:mov64 :rdi :r11)
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; RSI = TO address (byte address)
  (sys.lap-x86:sar64 :r12 #.sys.int::+n-fixnum-bits+) ; TO-OFFSET (raw)
  (sys.lap-x86:shl64 :r12 2) ; TO-OFFSET * 4(raw)
  (sys.lap-x86:add64 :rdi :r12) ; RDI = TO + TO-OFFSET, first pixel on the line.
  (sys.lap-x86:sar64 :r10 #.sys.int::+n-fixnum-bits+) ; R10 = colour (raw)
  (sys.lap-x86:test64 :r9 :r9)
  (sys.lap-x86:jz exit)
  head
  (sys.lap-x86:mov32 :eax :r10d)
  (sys.lap-x86:call (:r8 #.(+ (- sys.int::+tag-object+) 8)))
  (sys.lap-x86:add64 :rdi 4)
  (sys.lap-x86:sub64 :r9 #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jnz head)
  exit
  ;; Clear the two data registers that got smashed.
  (sys.lap-x86:xor32 :r10d :r10d)
  (sys.lap-x86:xor32 :r12d :r12d)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:ret))

(declaim (inline %bitset-whole))
(defun %bitset-whole (setter nrows ncols colour to to-offset to-stride)
  (mezzano.supervisor:with-pseudo-atomic
    (setf to (%simple-array-data-pointer to))
    (dotimes (y nrows)
      (%bitset-line setter ncols colour to to-offset)
      (incf to-offset to-stride))))

(defun bitset-argb-xrgb (nrows ncols colour to-array to-row to-col)
  "Fill a rectangle with COLOUR."
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols to-array to-row to-col to-array to-row to-col)
    (assert (equal (array-element-type to) '(unsigned-byte 32)))
    (case (ldb (byte 8 24) colour)
      ;; Stop early for 100% transparent colours.
      (0)
      ;; Don't blend 100% opaque colours.
      (#xFF (%bitset-whole #'%%set-one-xrgb8888-xrgb8888 nrows ncols colour to to-offset to-stride))
      ;; Fall back on blend function.
      (t (%bitset-whole #'%%alpha-blend-one-argb8888-xrgb8888 nrows ncols colour to to-offset to-stride)))))

;;; EAX = XRGB8888 pixel.
;;; RDI = XRGB8888 destination.
(sys.int::define-lap-function %%set-one-xrgb8888-xrgb8888 ()
  (sys.lap-x86:mov32 (:rdi) :eax)
  (sys.lap-x86:ret))

;;; EAX = ARGB8888 pixel.
;;; RDI = XRGB8888 destination.
;;; Alpha-blend PIXEL into DEST.
;;; GL_FUNC_ADD
;;; srcRGB = GL_SRC_ALPHA
;;; dstRGB = GL_ONE_MINUS_SRC_ALPHA
;;; srcAlpha = GL_ZERO
;;; dstAlpha = GL_ONE
;;; Blend the colour channels, while preserving the destination alpha channel
(sys.int::define-lap-function %%alpha-blend-one-argb8888-xrgb8888 ()
  (sys.lap-x86:mov32 :ecx :eax) ; ecx = pixel (ARGB)
  (sys.lap-x86:and32 :ecx #xFF000000) ; ecx = pixel-alpha (A000)
  (sys.lap-x86:jz out) ; Fully transparent, bail out.
  ;; Read destination.
  (sys.lap-x86:mov32 :edx (:rdi)) ; rdx = pixel (XRGB)
  ;; Check for fully opaque source.
  (sys.lap-x86:cmp32 :ecx #xFF000000)
  (sys.lap-x86:je fully-opaque)
  ;; MMX pixel blend.
  ;; RAX = Source pixel.
  ;; RCX = Alpha channel of source pixel at (byte 8 24).
  ;; RDX = Dest pixel.
  (sys.lap-x86:shr32 :ecx 24) ; ECX = alpha (000A)
  (sys.lap-x86:movd :mm0 :eax) ; MM0 = source (0000ARGB)
  (sys.lap-x86:movd :mm1 :ecx) ; MM1 = source alpha (0000000A)
  (sys.lap-x86:movd :mm2 :edx) ; MM2 = dest (0000ARGB)
  (sys.lap-x86:pxor :mm3 :mm3) ; MM3 = 0
  ;; Swizzle alpha.
  (sys.lap-x86:pmuludq :mm1 (:rip alpha-shuffle)) ; MM1 = source alpha (00000AAA)
  (sys.lap-x86:punpcklbw :mm1 :mm3) ; MM1 = source alpha (000A0A0A)
  ;; Compute inverse alpha.
  (sys.lap-x86:movq :mm4 (:rip inverse-alpha))
  (sys.lap-x86:psubb :mm4 :mm1) ; MM4 = (- 255 alpha)
  ;; Unpack pixels.
  (sys.lap-x86:punpcklbw :mm0 :mm3) ; MM0 = source (0A0R0G0B)
  (sys.lap-x86:punpcklbw :mm2 :mm3) ; MM2 = dest (0A0R0G0B)
  ;; Multiply by alpha.
  (sys.lap-x86:pmullw :mm0 :mm1) ; MM0 = source * alpha
  (sys.lap-x86:pmullw :mm2 :mm4) ; MM2 = dest * inverse-alpha
  (sys.lap-x86:paddusw :mm0 (:rip round-thingy))
  (sys.lap-x86:paddusw :mm2 (:rip round-thingy))
  (sys.lap-x86:pmulhuw :mm0 (:rip mul-thingy))
  (sys.lap-x86:pmulhuw :mm2 (:rip mul-thingy))
  ;; Blend.
  (sys.lap-x86:paddusw :mm0 :mm2) ; MM0 = final pixel (0A0R0G0B)
  ;; Renormalize.
  (sys.lap-x86:packuswb :mm0 :mm3) ; MM0 = final pixel (0000ARGB)
  (sys.lap-x86:movd :eax :mm0)
  ;; EAX = Final pixel.
  set-result
  (sys.lap-x86:mov32 (:rdi) :eax) ; rdx = pixel (XRGB)
  out
  (sys.lap-x86:ret)
  fully-opaque
  (sys.lap-x86:and32 :edx #xFF000000)
  (sys.lap-x86:and32 :eax #x00FFFFFF)
  (sys.lap-x86:or32 :eax :edx)
  (sys.lap-x86:jmp set-result)
  ;(:align 4) ; 16 byte alignment for XMM. (TODO)
  alpha-shuffle
  (:d64/le #x0000000000010101)
  inverse-alpha
  (:d64/le #x00FF00FF00FF00FF)
  round-thingy
  (:d64/le #x0080008000800080)
  mul-thingy
  (:d64/le #x0101010101010101))

;;; NCOLS FROM FROM-OFFSET TO TO-OFFSET
(sys.int::define-lap-function %bitblt-argb-xrgb-line ()
  (sys.lap-x86:mov64 :rsi :r9) ; rsi = FROM.
  (sys.lap-x86:shr64 :rsi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 :rcx :r10) ; rcx = FROM-OFFSET (fixnum).
  (sys.lap-x86:sar64 :rcx #.sys.int::+n-fixnum-bits+) ; FROM-OFFSET (raw)
  (sys.lap-x86:shl64 :rcx 2) ; FROM-OFFSET * 4(raw)
  (sys.lap-x86:add64 :rsi :rcx) ; rsi = FROM + FROM-OFFSET.
  (sys.lap-x86:mov64 :rdi :r11) ; rdi = TO.
  (sys.lap-x86:shr64 :rdi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 :rdx :r12) ; rcx = TO-OFFSET (fixnum).
  (sys.lap-x86:sar64 :rdx #.sys.int::+n-fixnum-bits+) ; TO-OFFSET (raw)
  (sys.lap-x86:shl64 :rdx 2) ; TO-OFFSET * 4(raw)
  (sys.lap-x86:add64 :rdi :rdx) ; rdi = TO + TO-OFFSET.
  ;; R8 = NCOLS.
  ;; RSI = Source.
  ;; RDI = Destination.
  (sys.lap-x86:jmp test)
  head
  ;; Read source.
  (sys.lap-x86:mov32 :eax (:rsi))
  (sys.lap-x86:mov32 :ecx :eax) ; ecx = pixel (ARGB)
  (sys.lap-x86:and32 :ecx #xFF000000) ; ecx = pixel-alpha (A000)
  (sys.lap-x86:jz out) ; Fully transparent, bail out.
  (sys.lap-x86:cmp32 :ecx #xFF000000)
  (sys.lap-x86:je set-result) ; Fully opaque, just set it.
  ;; Read destination.
  (sys.lap-x86:mov32 :edx (:rdi)) ; rdx = pixel (XRGB)
  ;; MMX pixel blend.
  ;; RAX = Source pixel.
  ;; RCX = Alpha channel of source pixel at (byte 8 24).
  ;; RDX = Dest pixel.
  (sys.lap-x86:movd :mm0 :eax) ; MM0 = source (0000ARGB)
  (sys.lap-x86:movd :mm1 :ecx) ; MM1 = source alpha (0000A000)
  (sys.lap-x86:movd :mm2 :edx) ; MM2 = dest (0000XRGB)
  (sys.lap-x86:pxor :mm3 :mm3) ; MM3 = 0
  ;; Swizzle alpha.
  (sys.lap-x86:psrld :mm1 24) ; MM1 = source alpha (0000000A)
  (sys.lap-x86:pmuludq :mm1 (:rip alpha-shuffle)) ; MM1 = source alpha (0000AAAA)
  (sys.lap-x86:punpcklbw :mm1 :mm3) ; MM1 = source alpha (0A0A0A0A)
  (sys.lap-x86:movq :mm4 (:rip inverse-alpha))
  ;; Compute inverse alpha.
  (sys.lap-x86:psubb :mm4 :mm1) ; MM4 = (- 255 alpha)
  ;; Unpack pixels.
  (sys.lap-x86:punpcklbw :mm0 :mm3) ; MM0 = source (0A0R0G0B)
  (sys.lap-x86:punpcklbw :mm2 :mm3) ; MM2 = dest (0A0R0G0B)
  ;; Multiply by alpha.
  (sys.lap-x86:pmullw :mm0 :mm1) ; MM0 = source * alpha
  (sys.lap-x86:pmullw :mm2 :mm4) ; MM0 = dest * inverse-alpha
  ;; Blend.
  (sys.lap-x86:paddw :mm0 :mm2) ; MM0 = final pixel (A0R0G0B0)
  ;; Renormalize.
  (sys.lap-x86:psrlw :mm0 8) ; MM0 = final pixel (0A0R0G0B)
  (sys.lap-x86:packuswb :mm0 :mm3) ; MM0 = final pixel (0000ARGB)
  (sys.lap-x86:movd :eax :mm0)
  ;; EAX = Final pixel.
  set-result
  (sys.lap-x86:mov32 (:rdi) :eax) ; rdx = pixel (XRGB)
  out
  (sys.lap-x86:sub64 :r8 #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:add64 :rsi 4)
  (sys.lap-x86:add64 :rdi 4)
  test
  (sys.lap-x86:test64 :r8 :r8)
  (sys.lap-x86:jnz head)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  ;(:align 4) ; 16 byte alignment for XMM. (TODO)
  alpha-shuffle
  (:d64/le #x0000000000010101)
  inverse-alpha
  (:d64/le #x00FF00FF00FF00FF))

(defun bitblt-argb-xrgb (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Alpha-blend FROM onto TO."
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (equal (array-element-type from) '(unsigned-byte 32)))
    (assert (equal (array-element-type to) '(unsigned-byte 32)))
    (mezzano.supervisor:with-pseudo-atomic
      (setf from (%simple-array-data-pointer from))
      (setf to (%simple-array-data-pointer to))
      (dotimes (y nrows)
        (%bitblt-argb-xrgb-line ncols from from-offset to to-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))

;;; NCOLS COLOUR TO TO-OFFSET
(sys.int::define-lap-function %bitxor-line ()
  (sys.lap-x86:mov64 :rdi :r10)
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; RSI = TO address (byte address)
  (sys.lap-x86:mov64 :rsi :r11)
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+) ; TO-OFFSET (raw)
  (sys.lap-x86:shl64 :rsi 2) ; TO-OFFSET * 4(raw)
  (sys.lap-x86:add64 :rdi :rsi) ; RDI = TO + TO-OFFSET, first pixel on the line.
  (sys.lap-x86:sar64 :r9 #.sys.int::+n-fixnum-bits+) ; R9 = colour (raw)
  (sys.lap-x86:test64 :r8 :r8)
  (sys.lap-x86:jmp test)
  head
  (sys.lap-x86:xor32 (:rdi) :r9d)
  (sys.lap-x86:add64 :rdi 4)
  (sys.lap-x86:sub64 :r8 #.(ash 1 sys.int::+n-fixnum-bits+))
  test
  (sys.lap-x86:jnz head)
  (sys.lap-x86:xor32 :r9d :r9d)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:ret))

(defun bitxor (nrows ncols colour to-array to-row to-col)
  "Exclusive OR a rectangle with COLOUR."
  (let ((to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0))
        (to-offset 0))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (setf to-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (min nrows (- to-height to-row)))
    (setf ncols (min ncols (- to-width to-col)))
    (multiple-value-bind (to-displaced-to to-displaced-offset)
        (array-displacement to-array)
      (when to-displaced-to
        (setf to-array to-displaced-to
              to-offset to-displaced-offset)))
    (assert (equal (array-element-type to-array) '(unsigned-byte 32)))
    (incf to-offset (+ (* to-row to-width) to-col))
    (when (and (> nrows 0)
               (> ncols 0))
      (mezzano.supervisor:with-pseudo-atomic ()
        (let ((to (%simple-array-data-pointer to-array)))
          (dotimes (y nrows)
            (%bitxor-line ncols colour to to-offset)
            (incf to-offset to-width)))))))
