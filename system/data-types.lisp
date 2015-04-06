;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defconstant +n-fixnum-bits+ 1)
(defconstant +fixnum-tag-mask+ (1- (ash 1 +n-fixnum-bits+)))

;;; Fields in the object header.
(defconstant +object-type-shift+ 2)
(defconstant +object-type-size+ 6)
(defconstant +object-data-shift+ 8)
(defconstant +object-data-size+ 56)
(defconstant +pinned-object-mark-bit+ #b10)

;;; Low 4 bits of a value are tag bits:
(defconstant +tag-fixnum-000+     #b0000)
(defconstant +tag-dx-root-object+ #b0001)
(defconstant +tag-fixnum-001+     #b0010)
(defconstant +tag-cons+           #b0011)
(defconstant +tag-fixnum-010+     #b0100)
;;#b0101
(defconstant +tag-fixnum-011+     #b0110)
;;#b0111
(defconstant +tag-fixnum-100+     #b1000)
(defconstant +tag-object+         #b1001)
(defconstant +tag-fixnum-101+     #b1010)
(defconstant +tag-character+      #b1011)
(defconstant +tag-fixnum-110+     #b1100)
(defconstant +tag-single-float+   #b1101)
(defconstant +tag-fixnum-111+     #b1110)
(defconstant +tag-gc-forward+     #b1111)

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
(defconstant +object-tag-array-xmm-vector+           #b011000)
(defconstant +last-simple-1d-array-object-tag+ +object-tag-array-xmm-vector+)
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

;; When set, the array or string is not simple.
;; Only valid on object with +object-tag(-simple)-{string/array}+ tags.
(defconstant +array-type-simple-bit+ #b000001)

;;; All these object tags, along with immediate fixnums and single-floats are numbers.
(defconstant +object-tag-bignum+                  #b100000)
(defconstant +object-tag-double-float+            #b100001)
(defconstant +object-tag-short-float+             #b100010)
(defconstant +object-tag-long-float+              #b100011)
(defconstant +object-tag-complex-rational+        #b100100)
(defconstant +object-tag-complex-single-float+    #b100101)
(defconstant +object-tag-complex-double-float+    #b100110)
(defconstant +object-tag-complex-short-float+     #b100111)
(defconstant +object-tag-complex-long-float+      #b101000)
(defconstant +object-tag-ratio+                   #b101001)
(defconstant +first-numeric-object-tag+ +object-tag-bignum+)
(defconstant +last-numeric-object-tag+ +object-tag-ratio+)
;;#b101010
;;#b101011
;;#b101100
;;#b101101
;;#b101110
;;#b101111
(defconstant +object-tag-symbol+                  #b110000)
(defconstant +object-tag-structure-object+        #b110001)
(defconstant +object-tag-std-instance+            #b110010)
(defconstant +object-tag-xmm-vector+              #b110011)
(defconstant +object-tag-thread+                  #b110100)
(defconstant +object-tag-unbound-value+           #b110101)
(defconstant +object-tag-function-reference+      #b110110)
(defconstant +object-tag-interrupt-frame+         #b110111)
;; Conses get an object header when allocated in a non-cons area, purely
;; to allow heap walking. The header is two words long, with the length
;; field containing 0 and the second header word containing 0.
;; Cons values always point to the pair of pointers, never to the header.
(defconstant +object-tag-cons+                    #b111000)
(defconstant +object-tag-freelist-entry+          #b111001)
(defconstant +first-misc-object-tag+ +object-tag-symbol+)
(defconstant +last-misc-object-tag+ +object-tag-freelist-entry+)
;;#b111000
;;#b111001
;;#b111010
;;#b111011
(defconstant +object-tag-function+                #b111100)
(defconstant +object-tag-closure+                 #b111101)
(defconstant +object-tag-funcallable-instance+    #b111110)
(defconstant +first-function-object-tag+ +object-tag-function+)
(defconstant +last-function-object-tag+ +object-tag-funcallable-instance+)
;;#b111111

;;; Layout of symbols.
(defconstant +symbol-name+ 0)
(defconstant +symbol-package+ 1)
(defconstant +symbol-value+ 2)
(defconstant +symbol-function+ 3)
(defconstant +symbol-plist+ 4)

(defconstant +symbol-header-tls-size+ 16)
(defconstant +symbol-header-tls-position+ 2)

(defconstant +symbol-header-mode-size+ 2)
(defconstant +symbol-header-mode-position+ 0)

(defconstant +symbol-mode-nil+ 0)
(defconstant +symbol-mode-special+ 1)
(defconstant +symbol-mode-constant+ 2)
(defconstant +symbol-mode-symbol-macro+ 3)

;;; Layout of a function's header.
;;; Currently applies to all 3 function types.

;; Machine code size is measured in paragraphs (16 byte units) and starts
;; at the beginning of the object, including the header.
(defconstant +function-machine-code-size+ 16)
(defconstant +function-machine-code-position+ 8)
;; Number of entries in the constant pool.
(defconstant +function-constant-pool-size+ 16)
(defconstant +function-constant-pool-position+ 24)
;; Size of the GC metadata in bytes.
(defconstant +function-gc-metadata-size+ 16)
(defconstant +function-gc-metadata-position+ 40)

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
;; Funcallable instances store the entry point of their function to avoid an
;; additional indirection when invoked. This is seperate from the function
;; entry point slot.
;; Layout is important. Update (setf funcallable-std-instance-function) if
;; it changes.
(defconstant +funcallable-instance-entry-point+ 3)
(defconstant +funcallable-instance-function+ 4)
(defconstant +funcallable-instance-class+ 5)
(defconstant +funcallable-instance-slots+ 6)

;;; Layout of function-references.

(defconstant +fref-name+ 0)
;; Layout of these two slots is important, update (SETF FUNCTION-REFERENCE-FUNCTION) if it changes.
(defconstant +fref-function+ 1)
(defconstant +fref-entry-point+ 2)

;;; Layout of complex arrays.

(defconstant +complex-array-storage+ 0)
(defconstant +complex-array-fill-pointer+ 1)
(defconstant +complex-array-info+ 2)
(defconstant +complex-array-axis-0+ 3)

;; Some bits are stored in the high(ish) bits of the address.
;; These are used to support the GC.

(defconstant +address-tag-shift+ 45)
(defconstant +address-tag-size+ 3)

(defconstant +address-newspace/oldspace-bit+ 44)

;; Pinned must be zero, a number of critical objects are pinned & wired and stored
;; below 2GB to permit fast access to them.
(defconstant +address-tag-pinned+       #b000)
(defconstant +address-tag-stack+        #b001)
(defconstant +address-tag-general+      #b010)
(defconstant +address-tag-cons+         #b011)

(defconstant +block-map-present+ 1 "Entry is present. This entry may still have a block associated with it, even if it is not present.")
(defconstant +block-map-writable+ 2 "Entry is writable.")
(defconstant +block-map-zero-fill+ 4 "Entry should be zero-filled.")
(defconstant +block-map-flag-mask+ #xFF)
(defconstant +block-map-id-shift+ 8)
(defconstant +block-map-id-size+ 56)

(defparameter *llf-version* 5)

(defconstant +llf-end-of-load+ #xFF)
(defconstant +llf-backlink+ #x01)
(defconstant +llf-function+ #x02)
(defconstant +llf-cons+ #x03)
(defconstant +llf-symbol+ #x04)
(defconstant +llf-uninterned-symbol+ #x05)
(defconstant +llf-unbound+ #x06)
(defconstant +llf-string+ #x07)
(defconstant +llf-setf-symbol+ #x08)
(defconstant +llf-integer+ #x09)
;; Call a function, ignore the result.
(defconstant +llf-invoke+ #x0A)
(defconstant +llf-setf-fdefinition+ #x0B)
(defconstant +llf-simple-vector+ #x0C)
(defconstant +llf-character+ #x0D)
(defconstant +llf-structure-definition+ #x0E)
(defconstant +llf-single-float+ #x10)
(defconstant +llf-proper-list+ #x11)
(defconstant +llf-package+ #x12)
;; A vector consisting entirely of integers.
(defconstant +llf-integer-vector+ #x13)
(defconstant +llf-add-backlink+ #x14)
(defconstant +llf-ratio+ #x15)
(defconstant +llf-array+ #x16)
;; Call a function, push the result.
(defconstant +llf-funcall+ #x17)
(defconstant +llf-bit-vector+ #x18)
(defconstant +llf-function-reference+ #x19)
(defconstant +llf-character-with-bits+ #x1A)
