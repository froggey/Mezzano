;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Optimized pluggable blitter functions.
;;;; The low-level pixel blending functions use a custom calling
;;;; convetion and must not be called directly from lisp.
;;;; RAX contains the source pixel.
;;;; R9 contains the destination simple UB32 vector.
;;;; RDI contains the index of the destination pixel.
;;;; RAX, RCX, RDX and MMX/SSE registers are caller save.
;;;; All other registers are callee save.

(in-package :mezzano.gui)

(defun compute-blit-info-dest-src (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Clamp parameters to array boundaries, return the stride of both arrays and their undisplaced, non-complex base arrays."
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
            (if (sys.int::%simple-1d-array-p from-array)
                from-array
                (sys.int::%complex-array-storage from-array))
            from-offset from-width
            (if (sys.int::%simple-1d-array-p to-array)
                to-array
                (sys.int::%complex-array-storage to-array))
            to-offset to-width)))

(defun compute-blit-info-dest (nrows ncols to-array to-row to-col)
  "Clamp parameters to array boundaries, return the stride of the array and its undisplaced, non-complex base array."
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
    ;; Undisplace displaced arrays
    (multiple-value-bind (to-displaced-to to-displaced-offset)
        (array-displacement to-array)
      (when to-displaced-to
        (setf to-array to-displaced-to
              to-offset to-displaced-offset)))
    (incf to-offset (+ (* to-row to-width) to-col))
    (values nrows ncols
            (if (sys.int::%simple-1d-array-p to-array)
                to-array
                (sys.int::%complex-array-storage to-array))
            to-offset to-width)))

(declaim (inline simple-ub32-vector-p simple-ub8-vector-p simple-ub1-vector-p))

(defun simple-ub32-vector-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-array-unsigned-byte-32+))

(defun simple-ub8-vector-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-array-unsigned-byte-8+))

(defun simple-ub1-vector-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-array-bit+))

;;; High-level functions.

(defun 2d-array-bitblt (nrows ncols from-array from-row from-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (simple-ub32-vector-p from))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitblt-line #'%%set-one-argb8888-argb8888
                      to to-offset
                      ncols
                      from from-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitblt-blend (nrows ncols from-array from-row from-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (simple-ub32-vector-p from))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitblt-line #'%%alpha-blend-one-argb8888-argb8888
                      to to-offset
                      ncols
                      from from-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitblt-xor (nrows ncols from-array from-row from-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (simple-ub32-vector-p from))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitblt-line #'%%xor-one-argb8888-argb8888
                      to to-offset
                      ncols
                      from from-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset (nrows ncols colour to-array to-row to-col)
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest nrows ncols to-array to-row to-col)
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-line #'%%set-one-argb8888-argb8888
                      to to-offset
                      ncols
                      colour)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-blend (nrows ncols colour to-array to-row to-col)
  (when (eql (ldb (byte 8 24) colour) #xFF)
    ;; Fully opaque, just use BITSET.
    (return-from 2d-array-bitset-blend (2d-array-bitset nrows ncols colour to-array to-row to-col)))
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest nrows ncols to-array to-row to-col)
    (assert (simple-ub32-vector-p to))
    (when (and (not (eql (ldb (byte 8 24) colour) 0))
               (> ncols 0))
      (dotimes (y nrows)
        (%bitset-line #'%%alpha-blend-one-argb8888-argb8888
                      to to-offset
                      ncols
                      colour)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-xor (nrows ncols colour to-array to-row to-col)
  "Exclusive OR a rectangle with COLOUR."
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest nrows ncols to-array to-row to-col)
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-line #'%%xor-one-argb8888-argb8888
                      to to-offset
                      ncols
                      colour)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-mask-1 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub1-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-1-line #'%%set-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-blend-mask-1 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub1-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (and (not (eql (ldb (byte 8 24) colour) 0))
               (> ncols 0))
      (dotimes (y nrows)
        (%bitset-mask-1-line #'%%alpha-blend-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-xor-mask-1 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub1-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-1-line #'%%xor-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-mask-8 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub8-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-8-line #'%%set-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-blend-mask-8 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub8-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (and (not (eql (ldb (byte 8 24) colour) 0))
               (> ncols 0))
      (dotimes (y nrows)
        (%bitset-mask-8-line #'%%alpha-blend-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-xor-mask-8 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub8-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-8-line #'%%xor-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

;; Line traversal functions.

;; (BLENDER[:R8] TO[:R9] TO-OFFSET[:R10] NCOLS[:R11] FROM[:R12] FROM-OFFSET[(:RSP 8)])
;; Assumes NCOLS is positive.
;;(defun %bitblt-line (blender to to-offset ncols from from-offset)
;;  (dotimes (i ncols)
;;    (funcall blender (row-major-aref from from-offset) to to-offset)
;;    (incf to-offset)
;;    (incf from-offset)))
(sys.int::define-lap-function %bitblt-line ()
  (:gc :no-frame)
  (sys.lap-x86:mov64 :rdi :r10) ; TO-OFFSET
  (sys.lap-x86:mov64 :rsi (:rsp 8)) ; FROM-OFFSET
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+) ; unbox fixnum
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; unbox fixnum
  LOOP-HEAD
  (sys.lap-x86:mov32 :eax (:object :r12 0 :rsi 4)) ; Read FROM
  (sys.lap-x86:call (:object :r8 0)) ; Call BLENDER.
  ;; Advance FROM-OFFSET/TO-OFFSET.
  (sys.lap-x86:add64 :rsi 1)
  (sys.lap-x86:add64 :rdi 1)
  ;; Decrement NCOLS and stop when zero.
  (sys.lap-x86:sub64 :r11 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jnz loop-head)
  (sys.lap-x86:ret))

;; (BLENDER[:R8] TO[:R9] TO-OFFSET[:R10] NCOLS[:R11] COLOUR[:R12])
;; Assumes NCOLS is positive.
;;(defun %bitset-line (blender to to-offset ncols colour)
;;  (dotimes (i ncols)
;;    (funcall blender colour to to-offset)
;;    (incf to-offset)))
(sys.int::define-lap-function %bitset-line ()
  (:gc :no-frame)
  (sys.lap-x86:mov64 :rdi :r10) ; TO-OFFSET
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; unbox fixnum
  LOOP-HEAD
  (sys.lap-x86:mov64 :rax :r12)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+) ; unbox fixnum
  (sys.lap-x86:call (:object :r8 0)) ; Call BLENDER.
  ;; Advance TO-OFFSET.
  (sys.lap-x86:add64 :rdi 1)
  ;; Decrement NCOLS and stop when zero.
  (sys.lap-x86:sub64 :r11 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jnz loop-head)
  (sys.lap-x86:ret))

;; (BLENDER[:R8] TO[:R9] TO-OFFSET[:R10] NCOLS[:R11] MASK[:R12] MASK-OFFSET[(:RSP 8)] COLOUR[(:RSP 16)]
;; Assumes NCOLS is positive.
;;(defun %bitset-mask-1-line (blender to to-offset ncols mask mask-offset colour)
;;  (dotimes (i ncols)
;;    (funcall blender (component-* colour (bit mask mask-offset)) to to-offset)
;;    (incf to-offset)
;;    (incf mask-offset)))
(sys.int::define-lap-function %bitset-mask-1-line ()
  (:gc :no-frame)
  (sys.lap-x86:mov64 :rdi :r10) ; RDI = TO-OFFSET.
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; unbox fixnum.
  (sys.lap-x86:mov64 :r10 (:rsp 16)) ; R10 = COLOUR.
  ;; RBX = [free]
  ;; R8 = blender
  ;; R9 = to
  ;; R10 = colour
  ;; R11 = ncols
  ;; R12 = mask
  ;; R13 = mask word offset (premultiplied)
  ;; RSI = mask bit
  ;; RDI = to-offset
  (sys.lap-x86:mov64 :r13 (:rsp 8)) ; R13 = MASK-OFFSET.
  ;; Set CL to the bit offset within the current mask word.
  (sys.lap-x86:mov64 :rcx :r13)
  (sys.lap-x86:shr32 :ecx  #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:and8 :cl #b111111) ; Addressing mask as a 64-bit quantity, so 6 low bits.
  ;; Set R13 to the word offset within mask.
  ;; This has the low bits clear, so is GC-safe.
  (sys.lap-x86:and64 :r13 #.(ash (lognot #b111111) #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:sar64 :r13 #.(+ sys.int::+n-fixnum-bits+ (- 6 3))) ; Bit index to word index, and unbox fixnum.
  ;; Convert the bit offset in CL to a bit mask in RSI.
  (sys.lap-x86:mov32 :esi 1)
  (sys.lap-x86:shl64 :rsi :cl)
  HEAD
  ;; Test current mask bit.
  (sys.lap-x86:xor32 :eax :eax)
  (sys.lap-x86:test64 (:object :r12 0 :r13 1) :rsi)
  (sys.lap-x86:jz WRITE)
  ;; Blend.
  (sys.lap-x86:mov64 :rax :r10)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  WRITE
  (sys.lap-x86:call (:object :r8 0))
  NEXT
  ;; Advance to next dest pixel.
  (sys.lap-x86:add64 :rdi 1)
  ;; Advance to next bit, resetting if it falls off the edge.
  (sys.lap-x86:shl64 :rsi 1) ; bit mask
  (sys.lap-x86:jnc no-carry)
  (sys.lap-x86:mov32 :esi 1)
  (sys.lap-x86:add64 :r13 8) ; mask word
  NO-CARRY
  ;; Decrement NCOLS and stop when zero.
  (sys.lap-x86:sub64 :r11 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jnz head)
  (sys.lap-x86:ret))

;; (BLENDER[:R8] TO[:R9] TO-OFFSET[:R10] NCOLS[:R11] MASK[:R12] MASK-OFFSET[(:RSP 8)] COLOUR[(:RSP 16)]
;; Assumes NCOLS is positive.
;; (defun %bitset-mask-8-line (blender to to-offset ncols mask mask-offset colour)
;;   (dotimes (i ncols)
;;     (funcall blender (component-* colour (aref mask mask-offset)) to to-offset)
;;     (incf to-offset)
;;     (incf mask-offset)))
;; (defun component-* (colour alpha)
;;   (make-colour-premultiplied (* (red-component colour) alpha)
;;                              (* (green-component colour) alpha)
;;                              (* (blue-component colour) alpha)
;;                              (* (alpha-component colour) alpha)))
;; (defun red-component (colour)
;;   (/ (ldb (byte 8 16) colour) 255.0))
(sys.int::define-lap-function %bitset-mask-8-line ()
  (:gc :no-frame)
  ;; Fetch & unbox MASK-OFFSET.
  (sys.lap-x86:mov64 :rsi (:rsp 8))
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+)
  ;; Fetch & unbox TO-OFFSET.
  (sys.lap-x86:mov64 :rdi :r10)
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+)
  ;; Fetch colour.
  (sys.lap-x86:mov64 :r10 (:rsp 16))
  HEAD
  ;; Read current mask pixel.
  (sys.lap-x86:mov8 :cl (:object :r12 0 :rsi 1))
  (sys.lap-x86:test8 :cl :cl) ; MASK-CURRENT == 0?
  (sys.lap-x86:jz next) ; true, skip pixel.
  ;; Unbox colour in EAX.
  (sys.lap-x86:mov64 :rax :r10)
  (sys.lap-x86:sar64 :rax 1)
  (sys.lap-x86:cmp8 :cl #xFF) ; MASK-CURRENT == #xFF?
  (sys.lap-x86:je invoke-blender) ; true, don't need to adjust colour.
  ;; Component multiply colour with mask.
  (sys.lap-x86:movzx8 :ecx :cl) ; Clear bits 8-32 of ECX.
  (sys.lap-x86:movd :mm0 :eax) ; MM0 = colour (0000ARGB)
  (sys.lap-x86:movd :mm1 :ecx) ; MM1 = mask (0000000M)
  (sys.lap-x86:pxor :mm3 :mm3) ; MM3 = 0
  ;; Spread mask into each element.
  (sys.lap-x86:pmuludq :mm1 (:rip mask-shuffle)) ; MM1 = mask (0000MMMM)
  (sys.lap-x86:punpcklbw :mm1 :mm3) ; MM1 = source alpha (0M0M0M0M)
  ;; Unpack colour.
  (sys.lap-x86:punpcklbw :mm0 :mm3) ; MM0 = colour (0A0R0G0B)
  ;; Multiply colour by mask.
  (sys.lap-x86:pmullw :mm0 :mm1) ; MM0 = colour * mask
  (sys.lap-x86:paddusw :mm0 (:rip round-thingy))
  (sys.lap-x86:pmulhuw :mm0 (:rip mul-thingy))
  ;; Renormalize.
  (sys.lap-x86:packuswb :mm0 :mm3) ; MM0 = final pixel (0000ARGB)
  (sys.lap-x86:movd :eax :mm0)
  ;; EAX = result
  INVOKE-BLENDER
  (sys.lap-x86:call (:object :r8 0))
  NEXT
  (sys.lap-x86:add64 :rdi 1) ; Advance TO address.
  (sys.lap-x86:add64 :rsi 1) ; Advance MASK address.
  (sys.lap-x86:sub64 :r11 #.(ash 1 sys.int::+n-fixnum-bits+)) ; Decrement NCOLS, set ZF when zero.
  (sys.lap-x86:jnz head) ; loop when stuff left to do.
  (sys.lap-x86:ret)
  mask-shuffle
  (:d64/le #x0000000001010101)
  round-thingy
  (:d64/le #x0080008000800080)
  mul-thingy
  (:d64/le #x0101010101010101))

;;; Final blending functions.

;;; EAX = ARGB8888 source pixel.
;;; R9 + RDI = ARGB8888 destination.
(sys.int::define-lap-function %%set-one-argb8888-argb8888 ()
  (:gc :no-frame)
  (sys.lap-x86:mov32 (:object :r9 0 :rdi 4) :eax)
  (sys.lap-x86:ret))

;;; EAX = ARGB8888 source pixel.
;;; R9 + RDI = ARGB8888 destination.
(sys.int::define-lap-function %%xor-one-argb8888-argb8888 ()
  (:gc :no-frame)
  (sys.lap-x86:xor32 (:object :r9 0 :rdi 4) :eax)
  (sys.lap-x86:ret))

;; EAX = ARGB8888 source pixel.
;; R9 + RDI = ARGB8888 destination.
;; Alpha-blend PIXEL into DEST.
;; GL_FUNC_ADD
;; src = GL_ONE
;; dst = GL_ONE_MINUS_SRC_ALPHA
(sys.int::define-lap-function %%alpha-blend-one-argb8888-argb8888 ()
  (:gc :no-frame)
  (sys.lap-x86:mov32 :ecx :eax) ; ecx = source (ARGB)
  (sys.lap-x86:and32 :ecx #xFF000000) ; ecx = source alpha (A000)
  (sys.lap-x86:jz out) ; Fully transparent, bail out.
  (sys.lap-x86:cmp32 :ecx #xFF000000)
  (sys.lap-x86:je fully-opaque) ; Fully opaque, just write.
  ;; Set up for blending.
  (sys.lap-x86:shr32 :ecx 24) ; ECX = source alpha (000A)
  (sys.lap-x86:movd :mm0 :eax) ; MM0 = source (0000ARGB)
  (sys.lap-x86:movd :mm1 :ecx) ; MM1 = source alpha (0000000A)
  (sys.lap-x86:movd :mm2 (:object :r9 0 :rdi 4)) ; MM2 = dest (0000ARGB)
  (sys.lap-x86:pxor :mm3 :mm3) ; MM3 = 0
  ;; Swizzle alpha.
  (sys.lap-x86:pmuludq :mm1 (:rip alpha-shuffle)) ; MM1 = source alpha (0000AAAA)
  (sys.lap-x86:punpcklbw :mm1 :mm3) ; MM1 = source alpha (0A0A0A0A)
  ;; Compute inverse alpha.
  (sys.lap-x86:movq :mm4 (:rip inverse-alpha))
  (sys.lap-x86:psubb :mm4 :mm1) ; MM4 = inverse source alpha (0I0I0I0I)
  ;; Unpack pixels.
  (sys.lap-x86:punpcklbw :mm0 :mm3) ; MM0 = source (0A0R0G0B)
  (sys.lap-x86:punpcklbw :mm2 :mm3) ; MM2 = dest (0A0R0G0B)
  ;; Multiply by alpha.
  (sys.lap-x86:pmullw :mm2 :mm4) ; MM2 = dest * inverse-alpha
  (sys.lap-x86:paddusw :mm2 (:rip round-thingy))
  (sys.lap-x86:pmulhuw :mm2 (:rip mul-thingy))
  ;; Blend.
  (sys.lap-x86:paddusw :mm0 :mm2) ; MM0 = final pixel (0A0R0G0B)
  ;; Renormalize.
  (sys.lap-x86:packuswb :mm0 :mm3) ; MM0 = final pixel (0000ARGB)
  ;; Write back to dest
  (sys.lap-x86:movd (:object :r9 0 :rdi 4) :mm0)
  OUT
  (sys.lap-x86:ret)
  FULLY-OPAQUE
  (sys.lap-x86:mov32 (:object :r9 0 :rdi 4) :eax)
  (sys.lap-x86:ret)
  ;(:align 4) ; 16 byte alignment for XMM. (TODO)
  alpha-shuffle
  (:d64/le #x0000000001010101)
  inverse-alpha
  (:d64/le #x00FF00FF00FF00FF)
  round-thingy
  (:d64/le #x0080008000800080)
  mul-thingy
  (:d64/le #x0101010101010101))
