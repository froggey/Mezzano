;;;; Optimized pluggable blitter functions.
;;;; The low-level pixel blending functions use a custom calling
;;;; convetion and must not be called directly from lisp.
;;;; RAX contains the source pixel.
;;;; RDI contains the address of the destination pixel.
;;;; RAX, RBX, RDX and MMX/SSE registers are caller save.
;;;; All other registers are callee save.
;;;; Higher-level line blenders must be called with the GC deferred.

(in-package #:sys.graphics)

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
    (setf nrows (min nrows (- to-height to-row) (- from-height from-row)))
    (setf ncols (min ncols (- to-width to-col) (- from-width from-col)))
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

;;; SETTER NCOLS COLOUR MASK MASK-OFFSET TO TO-OFFSET
(sys.int::define-lap-function %bitset-mask-1-line ()
  (sys.lap-x86:mov64 :rsi :r11)
  (sys.lap-x86:sar64 :rsi 3) ; RSI = MASK address (byte address)
  (sys.lap-x86:mov64 :rdi (:lsp 0))
  (sys.lap-x86:sar64 :rdi 3) ; RSI = TO address (byte address)
  (sys.lap-x86:sar64 (:lsp 8) 1) ; (:lsp 8) = TO-OFFSET (*4)
  (sys.lap-x86:add64 :rdi (:lsp 8)) ; RDI = TO + TO-OFFSET, first pixel on the line.
  (sys.lap-x86:sar64 :r10 3) ; R10 = colour (raw)
  (sys.lap-x86:sar64 :r12 3) ; R12 = MASK-OFFEST (raw)
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
  (sys.lap-x86:call :r8)
  next
  (sys.lap-x86:add64 :rdi 4)
  (sys.lap-x86:add64 :rcx :rcx)
  (sys.lap-x86:jnc no-carry)
  (sys.lap-x86:mov32 :ecx 1)
  (sys.lap-x86:add64 :rsi 8)
  no-carry
  (sys.lap-x86:sub64 :r9 8)
  test
  (sys.lap-x86:jnz head)
  ;; Clear the two data registers that got smashed.
  (sys.lap-x86:xor32 :r10d :r10d)
  (sys.lap-x86:xor32 :r12d :r12d)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:lea64 :rbx (:lsp 16))
  (sys.lap-x86:ret))

(declaim (inline bitset-mask-1-whole))
(defun %bitset-mask-1-whole (setter nrows ncols colour mask mask-offset mask-stride to to-offset to-stride)
  (sys.int::with-deferred-gc ()
    (when (sys.int::%array-header-p to)
      (setf to (sys.int::%array-header-storage to)))
    (unless (integerp to)
      (assert (typep to 'simple-array))
      (setf to (+ (sys.int::lisp-object-address to) (- sys.int::+tag-array-like+) 8)))
    (when (sys.int::%array-header-p mask)
      (setf mask (sys.int::%array-header-storage mask)))
    (unless (integerp mask)
      (assert (typep mask 'simple-array))
      (setf mask (+ (sys.int::lisp-object-address mask) (- sys.int::+tag-array-like+) 8)))
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

;;; EAX = XRGB8888 pixel.
;;; RDI = XRGB8888 destination.
(sys.int::define-lap-function %%set-one-xrgb8888-xrgb8888 ()
  (sys.lap-x86:mov32 (:rdi) :eax)
  (sys.lap-x86:ret))

;;; EAX = XRGB8888 pixel.
;;; RDI = XRGB8888 destination.
;;; Alpha-blend PIXEL into DEST.
;;; Uses OpenGL-style Src-alpha/One-minus-src-alpha blending.
(sys.int::define-lap-function %%alpha-blend-one-argb8888-xrgb8888 ()
  (sys.lap-x86:mov32 :ebx :eax) ; ebx = pixel (ARGB)
  (sys.lap-x86:and32 :ebx #xFF000000) ; ebx = pixel-alpha (A000)
  (sys.lap-x86:jz out) ; Fully transparent, bail out.
  (sys.lap-x86:cmp32 :ebx #xFF000000)
  (sys.lap-x86:je set-result) ; Fully opaque, just set it.
  ;; Read destination.
  (sys.lap-x86:mov32 :edx (:rdi)) ; rdx = pixel (XRGB)
  ;; MMX pixel blend.
  ;; RAX = Source pixel.
  ;; RCX = Alpha channel of source pixel at (byte 8 24).
  ;; RDX = Dest pixel.
  (sys.lap-x86:movd :mm0 :eax) ; MM0 = source (0000ARGB)
  (sys.lap-x86:movd :mm1 :ebx) ; MM1 = source alpha (0000A000)
  (sys.lap-x86:movd :mm2 :edx) ; MM2 = dest (0000XRGB)
  (sys.lap-x86:pxor :mm3 :mm3) ; MM3 = 0
  ;; Swizzle alpha.
  ;; need ssse3 (sys.lap-x86:pshufb :mm1 (:rip alpha-shuffle)) ; MM1 = source alpha (0A0A0A0A)
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
  (sys.lap-x86:ret)
  ;(:align 4) ; 16 byte alignment for XMM. (TODO)
  alpha-shuffle
  (:d64/le #x0000000001010101)
  ;(:d64/le #x8003800380038003)
  inverse-alpha
  (:d64/le #x00FF00FF00FF00FF))

;;; NCOLS FROM FROM-OFFSET TO TO-OFFSET
(sys.int::define-lap-function %bitblt-argb-xrgb-line ()
  (sys.lap-x86:mov64 :rsi :r9) ; rsi = FROM.
  (sys.lap-x86:shr64 :rsi 3)
  (sys.lap-x86:mov64 :rcx :r10) ; rcx = FROM-OFFSET (fixnum).
  (sys.lap-x86:shr64 :rcx 1) ; FROM-OFFSET (* 4).
  (sys.lap-x86:add64 :rsi :rcx) ; rsi = FROM + FROM-OFFSET.
  (sys.lap-x86:mov64 :rdi :r11) ; rdi = TO.
  (sys.lap-x86:shr64 :rdi 3)
  (sys.lap-x86:mov64 :rdx :r12) ; rcx = TO-OFFSET (fixnum).
  (sys.lap-x86:shr64 :rdx 1) ; TO-OFFSET (* 4).
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
  (sys.lap-x86:sub64 :r8 8)
  (sys.lap-x86:add64 :rsi 4)
  (sys.lap-x86:add64 :rdi 4)
  test
  (sys.lap-x86:test64 :r8 :r8)
  (sys.lap-x86:jnz head)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  ;(:align 4) ; 16 byte alignment for XMM. (TODO)
  alpha-shuffle
  (:d64/le #x0000000001010101)
  inverse-alpha
  (:d64/le #x00FF00FF00FF00FF))

(defun bitblt-argb-xrgb (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Alpha-blend FROM onto TO."
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (equal (array-element-type from) '(unsigned-byte 32)))
    (assert (equal (array-element-type to) '(unsigned-byte 32)))
    (sys.int::with-deferred-gc ()
      (when (sys.int::%array-header-p from)
        (setf from (sys.int::%array-header-storage from)))
      (unless (integerp from)
        (assert (typep from 'simple-array))
        (setf from (+ (sys.int::lisp-object-address from) (- sys.int::+tag-array-like+) 8)))
      (when (sys.int::%array-header-p to)
        (setf to (sys.int::%array-header-storage to)))
      (unless (integerp to)
        (assert (typep to 'simple-array))
        (setf to (+ (sys.int::lisp-object-address to) (- sys.int::+tag-array-like+) 8)))
      (dotimes (y nrows)
        (%bitblt-argb-xrgb-line ncols from from-offset to to-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))
