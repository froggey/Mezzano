;;;; Low-level definitions for data types

(in-package :mezzano.internals)

(defconstant +n-fixnum-bits+ 1)
(defconstant +fixnum-tag-mask+ (1- (ash 1 +n-fixnum-bits+)))

;;; Fields in the object header.
(defconstant +object-type-shift+ 2)
(defconstant +object-type-size+ 6)
(defconstant +object-data-shift+ 8)
(defconstant +object-data-size+ 56)

;;; Low 4 bits of a value are tag bits
(defconstant +tag-fixnum-000+       #b0000)
;; +TAG-CONS+ and +TAG-OBJECT+ have been carefully chosen so they have
;; exactly one bit different. This allows ordinary object pointers to
;; be detected trivially: (eql (logand val #b111) 1)
(defconstant +tag-cons+             #b0001)
(defconstant +tag-fixnum-001+       #b0010)
;;#b0011
(defconstant +tag-fixnum-010+       #b0100)
(defconstant +tag-immediate+        #b0101)
(defconstant +tag-fixnum-011+       #b0110)
;; Low two bits of this one must be set, high two bits must match low
;; two bits of +object-tag-instance+.
;; See %FAST-INSTANCE-LAYOUT-EQ-P.
(defconstant +tag-instance-header+  #b0111) ; Low two bits must be set.
(defconstant +tag-fixnum-100+       #b1000)
(defconstant +tag-object+           #b1001)
(defconstant +tag-fixnum-101+       #b1010)
;;#b1011
(defconstant +tag-fixnum-110+       #b1100)
(defconstant +tag-dx-root-object+   #b1101)
(defconstant +tag-fixnum-111+       #b1110)
(defconstant +tag-gc-forward+       #b1111)

(defconstant +tag-field+ (byte 4 0))

(defconstant +immediate-tag+ (byte 2 4))
(defconstant +immediate-tag-character+ #b00)
(defconstant +immediate-tag-single-float+ #b01)
(defconstant +immediate-tag-byte-specifier+ #b10)
;;#b11

;;; Simple 1D arrays.
;; Array type T == simple vector.
;; Is zero to allow for faster type checking.
(defconstant +object-tag-array-t+                    #b000000)
(defconstant +object-tag-array-fixnum+               #b000001)
(defconstant +object-tag-array-bit+                  #b000010)
(defconstant +object-tag-array-unsigned-byte-2+      #b000011)
(defconstant +object-tag-array-unsigned-byte-4+      #b000100)
(defconstant +object-tag-array-unsigned-byte-8+      #b000101)
(defconstant +object-tag-array-unsigned-byte-16+     #b000110)
(defconstant +object-tag-array-unsigned-byte-32+     #b000111)
(defconstant +object-tag-array-unsigned-byte-64+     #b001000)
(defconstant +object-tag-array-signed-byte-1+        #b001001)
(defconstant +object-tag-array-signed-byte-2+        #b001010)
(defconstant +object-tag-array-signed-byte-4+        #b001011)
(defconstant +object-tag-array-signed-byte-8+        #b001100)
(defconstant +object-tag-array-signed-byte-16+       #b001101)
(defconstant +object-tag-array-signed-byte-32+       #b001110)
(defconstant +object-tag-array-signed-byte-64+       #b001111)
(defconstant +object-tag-array-single-float+         #b010000)
(defconstant +object-tag-array-double-float+         #b010001)
(defconstant +object-tag-array-short-float+          #b010010)
(defconstant +object-tag-array-long-float+           #b010011)
(defconstant +object-tag-array-complex-single-float+ #b010100)
(defconstant +object-tag-array-complex-double-float+ #b010101)
(defconstant +object-tag-array-complex-short-float+  #b010110)
(defconstant +object-tag-array-complex-long-float+   #b010111)
(defconstant +first-simple-1d-array-object-tag+ +object-tag-array-t+)
(defconstant +last-simple-1d-array-object-tag+ +object-tag-array-complex-long-float+)
;;#b011000
;;#b011001
;;#b011010
;;#b011011
;; Strings. Simple strings are the same as normal strings, except marked as simple.
;; These are actually character arrays, they're only string when rank = 1.
(defconstant +object-tag-simple-string+           #b011100)
(defconstant +object-tag-string+                  #b011101)
;; Other arrays.
(defconstant +object-tag-simple-array+            #b011110)
(defconstant +object-tag-array+                   #b011111)
(defconstant +first-complex-array-object-tag+ +object-tag-simple-string+)
(defconstant +last-complex-array-object-tag+ +object-tag-array+)
(defconstant +first-array-object-tag+ +object-tag-array-t+)
(defconstant +last-array-object-tag+ +object-tag-array+)

;; When set, the array or string is not simple.
;; Only valid on object with +object-tag(-simple)-{string/array}+ tags.
(defconstant +array-type-simple-bit+ #b000001)

;;; All these object tags, along with immediate fixnums and single-floats are numbers.
(defconstant +object-tag-bignum+                  #b100000)
(defconstant +object-tag-ratio+                   #b100001)
(defconstant +object-tag-double-float+            #b100010)
(defconstant +object-tag-short-float+             #b100011)
(defconstant +object-tag-long-float+              #b100100)
(defconstant +object-tag-complex-rational+        #b100101)
(defconstant +object-tag-complex-single-float+    #b100110)
(defconstant +object-tag-complex-double-float+    #b100111)
(defconstant +object-tag-complex-short-float+     #b101000)
(defconstant +object-tag-complex-long-float+      #b101001)
(defconstant +first-rational-object-tag+ +object-tag-bignum+) ; plus fixnum
(defconstant +last-rational-object-tag+ +object-tag-ratio+)
(defconstant +first-float-object-tag+ +object-tag-double-float+) ; plus single-float
(defconstant +last-float-object-tag+ +object-tag-long-float+)
(defconstant +first-real-object-tag+ +object-tag-bignum+) ; plus fixnum & single-float
(defconstant +last-real-object-tag+ +object-tag-long-float+)
(defconstant +first-complex-object-tag+ +object-tag-complex-rational+)
(defconstant +last-complex-object-tag+ +object-tag-complex-long-float+)
(defconstant +first-numeric-object-tag+ +object-tag-bignum+) ; plus fixnum & single-float
(defconstant +last-numeric-object-tag+ +object-tag-complex-long-float+)
;;#b101010
;;#b101011
;;#b101100
;;#b101101
(defconstant +object-tag-symbol-value-cell+       #b101110)
(defconstant +object-tag-mmx-vector+              #b101111)
(defconstant +object-tag-symbol+                  #b110000)
;;#b110001
;;#b110010
(defconstant +object-tag-sse-vector+              #b110011)
;;#b110100
;; Low two bits must match high two bits of +tag-instance-header+.
;; Must be one bit different from +object-tag-funcallable-instance+.
(defconstant +object-tag-instance+                #b110101)
(defconstant +object-tag-function-reference+      #b110110)
(defconstant +object-tag-interrupt-frame+         #b110111)
;; Conses get an object header when allocated in a non-cons area, purely
;; to allow heap walking. The header is two words long, with the length
;; field containing 0 and the second header word containing 0.
;; Cons values always point to the pair of pointers, never to the header.
(defconstant +object-tag-cons+                    #b111000)
(defconstant +object-tag-freelist-entry+          #b111001)
(defconstant +object-tag-weak-pointer+            #b111010)
;; Function objects.
(defconstant +object-tag-delimited-continuation+  #b111011)
(defconstant +object-tag-function+                #b111100)
;; Low two bits must match high two bits of +tag-instance-header+.
;; Must be one bit different from +object-tag-instance+.
(defconstant +object-tag-funcallable-instance+    #b111101)
(defconstant +object-tag-closure+                 #b111110)
(defconstant +first-function-object-tag+ +object-tag-delimited-continuation+)
(defconstant +last-function-object-tag+ +object-tag-closure+)
;;#b111111

;;; Layout of symbols.
(defconstant +symbol-name+ 0)
(defconstant +symbol-package+ 1)
(defconstant +symbol-value+ 2) ; actually the global symbol-value-cell
(defconstant +symbol-function+ 3) ; actually an fref
(defconstant +symbol-type+ 4)

(defconstant +symbol-value-cell-symbol+ 1)
(defconstant +symbol-value-cell-value+ 2)

(defconstant +symbol-header-mode+ (byte 3 0))

(defconstant +symbol-mode-nil+ 0)
(defconstant +symbol-mode-special+ 1)
(defconstant +symbol-mode-constant+ 2)
(defconstant +symbol-mode-symbol-macro+ 3)
(defconstant +symbol-mode-global+ 4)

;;; Layout of a function's header.
;;; Only applies to compiled functions.
;; Machine code size is measured in paragraphs (16 byte units) and starts
;; at the beginning of the object, including the header.
(defconstant +function-header-code-size+ (byte 16 8))
(defconstant +function-header-pool-size+ (byte 16 24))
(defconstant +function-header-metadata-size+ (byte 16 40))

;;; Layout of functions.
;;; Common to all functions.

;; Entry point of the function, used by function call machinery.
(defconstant +function-entry-point+ 0)

;;; Closures.
;;; Only the position of the function is specified. The compiler may arrange
;;; closure environments however it wants, including inlining them into the
;;; closure object.
(defconstant +closure-function+ 1)

;;; Funcallable instances.
;; Layout is important. Update (setf funcallable-std-instance-function) if
;; it changes.
(defconstant +funcallable-instance-function+ 1)

;;; Delimited continuations.
(defconstant +delimited-continuation-stack+ 1)
(defconstant +delimited-continuation-stack-pointer+ 2)
(defconstant +delimited-continuation-state+ 3)
(defconstant +delimited-continuation-prompt+ 4)

;;; Layout of function-references.
(defconstant +fref-undefined-entry-point+ 0)
(defconstant +fref-name+ 1)
(defconstant +fref-function+ 2)
(defconstant +fref-code+ 3)

;;; Layout of complex arrays.
(defconstant +complex-array-storage+ 0)
(defconstant +complex-array-fill-pointer+ 1)
(defconstant +complex-array-info+ 2)
(defconstant +complex-array-axis-0+ 3)

;;; Layout of weak pointers.
(defconstant +weak-pointer-header-livep+ 0
  "Set by the GC when the value is live.")
(defconstant +weak-pointer-header-weakness+ (byte 2 1))
(defconstant +weak-pointer-weakness-key+ 0
  "The weak pointer is live as long as the key is reachable.")
(defconstant +weak-pointer-weakness-value+ 1
  "The weak pointer is live as long as the value is reachable.")
(defconstant +weak-pointer-weakness-and+ 2
  "The weak pointer is live as long as both the key and the value are reachable.")
(defconstant +weak-pointer-weakness-or+ 3
  "The weak pointer is live as long as either the key or the value are reachable.")
(defconstant +weak-pointer-link+ 0)
(defconstant +weak-pointer-key+ 1)
(defconstant +weak-pointer-value+ 2)
(defconstant +weak-pointer-finalizer+ 3)
(defconstant +weak-pointer-finalizer-link+ 4)

;;; Layout of ratios.
(defconstant +ratio-numerator+ 0)
(defconstant +ratio-denominator+ 1)

;;; Layout of complex numbers.
(defconstant +complex-realpart+ 0)
(defconstant +complex-imagpart+ 1)

;; Some bits are stored in the high(ish) bits of the address.
;; These are used to support the GC.

(defconstant +address-tag-shift+ 45)
(defconstant +address-tag-size+ 3)
(defconstant +address-tag+ (byte +address-tag-size+ +address-tag-shift+))

(defconstant +address-old-generation+ (expt 2 43)
  "This bit distingushes the young and old generations.")
(defconstant +address-semispace+ (expt 2 44)
  "This bit defines the semispaces for the young and old generations.")

;; Pinned must be zero, a number of critical objects are pinned & wired and stored
;; below 2GB to permit fast access to them.
(defconstant +address-tag-pinned+       #b000)
(defconstant +address-tag-stack+        #b001)
(defconstant +address-tag-general+      #b010)
(defconstant +address-tag-cons+         #b011)

(defconstant +card-size+ #x1000) ; Match page size for now.
(defconstant +card-table-entry-size+ 4)

(defconstant +card-table-entry-offset+ (byte 16 0)
  "A negative 16-bit offset from the start of the card to the start
of the first object in the card. Measured in 16-byte units.
An offset of all ones (1- (expt 2 16)) indicates that the start of the
object is further away than what can be encoded and the the system
should continue looking backwards.")
(defconstant +card-table-entry-dirty-gen+ (byte 2 16))
;; Bits 31-18 available.

;; Cover the whole address space.
(defconstant +card-table-size+ (* (/ (expt 2 47) +card-size+)
                                  +card-table-entry-size+))
(defconstant +card-table-base+ #x4000000000) ; 256GB, mostly arbitrary but in the wired area
;; VM regions must meet this allocation requirement so that the card table
;; entries associated with an allocation cover an exact number of pages.
;; This allows the pager to map/unmap regions in the card table without worrying
;; about partial page coverage.
;; NOTE: Stacks don't have card table entries and aren't subject to this
;; alignment constraint. They must still be page-aligned.
;; This must also be chosen so that (>= (/ +a-m-a+ 16 8) page-size), to support
;; the mark bit region.
(defconstant +allocation-minimum-alignment+ (* (/ #x1000 +card-table-entry-size+)
                                               +card-size+))

;; GC mark bit region.
(defconstant +octets-per-mark-bit+ 16)
(defconstant +mark-bit-region-base+ #x0000100000000000)
;; 48 address bits, every 2 words/16 bytes needs a mark bit, 8 bits per byte.
(defconstant +mark-bit-region-size+ (/ (expt 2 48) +octets-per-mark-bit+ 8))

(defconstant +block-map-present+ #x01
  "Entry is present. This entry may still have a block associated with it, even if it is not present.")
;; FIXME: This isn't really respected properly.
(defconstant +block-map-writable+ #x02
  "Entry is writable.")
(defconstant +block-map-zero-fill+ #x04
  "Entry should be zero-filled.")
(defconstant +block-map-committed+ #x08
  "This block is owned by the currently running system, not by a previous snapshot and can be written to safely.
Internal to the pager, should not be used by other code.")
(defconstant +block-map-wired+ #x10
  "Entry should be wired in memory.")
(defconstant +block-map-track-dirty+ #x20
  "Dirty bit tracking is enabled for this entry.
When the page is written to, the corresponding dirty bit in the card table will be set and this flag will be cleared.")
(defconstant +block-map-transient+ #x40
  "Entry is transient and won't be saved by snapshots.
Accesses to stale memory will signal an error.
Internal to sg-vec, should not be used by other code.")
(defconstant +block-map-flag-mask+ #xFF)
(defconstant +block-map-id-shift+ 8)
(defconstant +block-map-id-size+ 54) ; keep it a few bits smaller than 56 to avoid bignums.
(defconstant +block-map-id-lazy+ (1- (ash 1 +block-map-id-size+))
  "When stored in the ID field, this value indicates that space has been
reserved on the disk, but no specific block has been allocated.")
(defconstant +block-map-id-not-allocated+ 0)

(defparameter *llf-version* 36)

(defconstant +llf-arch-x86-64+ 1)
(defconstant +llf-arch-arm64+ 2)

(defconstant +llf-end-of-load+               #xFF)
(defconstant +llf-backlink+                  #x01)
(defconstant +llf-function+                  #x02)
(defconstant +llf-cons+                      #x03)
(defconstant +llf-symbol+                    #x04)
(defconstant +llf-uninterned-symbol+         #x05)
(defconstant +llf-string+                    #x07)
(defconstant +llf-integer+                   #x09)
(defconstant +llf-simple-vector+             #x0C)
(defconstant +llf-character+                 #x0D)
(defconstant +llf-structure-definition+      #x0E)
(defconstant +llf-single-float+              #x10)
(defconstant +llf-proper-list+               #x11)
(defconstant +llf-package+                   #x12)
;; A vector consisting entirely of integers.
(defconstant +llf-integer-vector+            #x13)
(defconstant +llf-add-backlink+              #x14)
(defconstant +llf-ratio+                     #x15)
(defconstant +llf-array+                     #x16)
(defconstant +llf-bit-vector+                #x18)
(defconstant +llf-function-reference+        #x19)
(defconstant +llf-character-with-bits+       #x1A)
(defconstant +llf-structure-slot-definition+ #x1B)
(defconstant +llf-byte+                      #x1C)
(defconstant +llf-double-float+              #x1D)
(defconstant +llf-typed-array+               #x1E)
;; Call a function with N arguments, push the result.
(defconstant +llf-funcall-n+                 #x1F)
;; Discard the top of stack value.
(defconstant +llf-drop+                      #x20)
(defconstant +llf-complex-rational+          #x21)
(defconstant +llf-complex-single-float+      #x22)
(defconstant +llf-complex-double-float+      #x23)
(defconstant +llf-instance-header+           #x24)
(defconstant +llf-symbol-global-value-cell+  #x25)
(defconstant +llf-if+                        #x26)
(defconstant +llf-else+                      #x27)
(defconstant +llf-fi+                        #x28)
(defconstant +llf-layout+                    #x29)
(defconstant +llf-initialize-array+          #x2A)
(defconstant +llf-short-float+               #x2B)
(defconstant +llf-complex-short-float+       #x2C)

;;; Fields in the Unicode info tables.

(defconstant +unicode-info-name-offset+ (byte 20 0)
  "Offset of the name in the name table.")
(defconstant +unicode-info-name-length+ (byte 6 20)
  "Length of the name in the name table.")
(defconstant +unicode-info-othercase-code+ (byte 21 26)
  "Character code for the alternate case character.")
(defconstant +unicode-info-general-category+ (byte 5 47)
  "General category code.")

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun unicode-general-category-encode (category)
  (ecase category
    (:uppercase-letter 0)
    (:lowercase-letter 1)
    (:titlecase-letter 2)
    (:modifier-letter 3)
    (:other-letter 4)
    (:nonspacing-mark 5)
    (:spacing-mark 6)
    (:enclosing-mark 7)
    (:decimal-number 8)
    (:letter-number 9)
    (:other-number 10)
    (:connector-punctuation 11)
    (:dash-punctuation 12)
    (:open-punctuation 13)
    (:close-punctuation 14)
    (:initial-punctuation 15)
    (:final-punctuation 16)
    (:other-punctuation 17)
    (:math-symbol 18)
    (:currency-symbol 19)
    (:modifier-symbol 20)
    (:other-symbol 21)
    (:space-separator 22)
    (:line-separator 23)
    (:paragraph-separator 24)
    (:control 25)
    (:format 26)
    (:surrogate 27)
    (:private-use 28)
    (:unassigned 29)))

(defun unicode-general-category-decode (code)
  (ecase code
    (0 :uppercase-letter)
    (1 :lowercase-letter)
    (2 :titlecase-letter)
    (3 :modifier-letter)
    (4 :other-letter)
    (5 :nonspacing-mark)
    (6 :spacing-mark)
    (7 :enclosing-mark)
    (8 :decimal-number)
    (9 :letter-number)
    (10 :other-number)
    (11 :connector-punctuation)
    (12 :dash-punctuation)
    (13 :open-punctuation)
    (14 :close-punctuation)
    (15 :initial-punctuation)
    (16 :final-punctuation)
    (17 :other-punctuation)
    (18 :math-symbol)
    (19 :currency-symbol)
    (20 :modifier-symbol)
    (21 :other-symbol)
    (22 :space-separator)
    (23 :line-separator)
    (24 :paragraph-separator)
    (25 :control)
    (26 :format)
    (27 :surrogate)
    (28 :private-use)
    (29 :unassigned)))
)

(defconstant +debug-end-entry-op+ #x00)
(defconstant +debug-add-var-op+ #x10)
(defconstant +debug-add-hidden-var-op+ #x11)
(defconstant +debug-drop-n-op+ #x40)
(defconstant +debug-drop-op+ #x50)
(defconstant +debug-update-n-op+ #x60)
(defconstant +debug-update-op+ #x70)

(defconstant +debug-repr-value+ 0)
(defconstant +debug-repr-single-float+ 1)
(defconstant +debug-repr-double-float+ 2)
(defconstant +debug-repr-mmx-vector+ 3)
(defconstant +debug-repr-sse-vector+ 4)
(defconstant +debug-repr-fixnum+ 5)
(defconstant +debug-repr-unsigned-byte-64+ 6)
(defconstant +debug-repr-signed-byte-64+ 7)
(defconstant +debug-repr-short-float+ 8)

(defvar *debug-x86-64-register-encodings* #(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
                                            :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                            :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                            :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
                                            :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defvar *debug-arm64-register-encodings* #(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                                           :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
                                           :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
                                           :x24 :x25 :x26 :x27 :x28 :x29 :x30 :xzr
                                           :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
                                           :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
                                           :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
                                           :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defconstant +gcmd-flag0-frame+ 0)
(defconstant +gcmd-flag0-interrupt+ 1)
(defconstant +gcmd-flag0-block-or-tagbody-thunk+ 2)
(defconstant +gcmd-flag0-incoming-arguments+ 3)
(defconstant +gcmd-flag0-pushed-values-register+ 4)
(defconstant +gcmd-flag0-extra-registers+ (byte 2 5))
(defconstant +gcmd-flag0-restart+ 7)

(defconstant +gcmd-flag1-multiple-values+ (byte 4 0))
(defconstant +gcmd-flag1-incoming-arguments-location+ (byte 4 4))
