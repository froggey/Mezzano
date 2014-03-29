(in-package :sys.int)

(defconstant +n-fixnum-bits+ 1)
(defconstant +fixnum-tag-mask+ (1- (ash 1 +n-fixnum-bits+)))
(defconstant +array-type-shift+ 2)
(defconstant +array-type-size+ 6)
(defconstant +array-length-shift+ 8)
(defconstant +array-length-size+ 56)

(defconstant +tag-fixnum-000+    #b0000)
;;#b0001
(defconstant +tag-fixnum-001+    #b0010)
(defconstant +tag-cons+          #b0011)
(defconstant +tag-fixnum-010+    #b0100)
(defconstant +tag-unbound-value+ #b0101)
(defconstant +tag-fixnum-011+    #b0110)
;;#b0111
(defconstant +tag-fixnum-100+    #b1000)
;; WARNING! This must be >= 8 for backcompat.
;; Functions are currently invoked directly through the pointer!
(defconstant +tag-object+        #b1001)
(defconstant +tag-fixnum-101+    #b1010)
(defconstant +tag-character+     #b1011)
(defconstant +tag-fixnum-110+    #b1100)
(defconstant +tag-single-float+  #b1101)
(defconstant +tag-fixnum-111+    #b1110)
(defconstant +tag-gc-forward+    #b1111)

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
;; Arrays that point to somewhere special in memory (for MMIO, etc).
(defconstant +object-tag-memory-array+            #b011011)
;; Strings. Simple strings are the same as normal strings, except marked as simple.
;; These are actually character arrays, they're only string when rank = 1.
(defconstant +object-tag-simple-string+           #b011100)
(defconstant +object-tag-string+                  #b011101)
;; Other arrays.
(defconstant +object-tag-simple-array+            #b011110)
(defconstant +object-tag-array+                   #b011111)
(defconstant +first-complex-array-object-tag+ +object-tag-memory-array+)
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
(defconstant +object-tag-stack-group+             #b110100)
(defconstant +first-misc-object-tag+ +object-tag-symbol+)
(defconstant +last-misc-object-tag+ +object-tag-stack-group+)
;;#b110101
;;#b110110
;;#b110111
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

(defconstant +symbol-mode-nil+ 0)
(defconstant +symbol-mode-special+ 1)
(defconstant +symbol-mode-constant+ 2)
(defconstant +symbol-mode-symbol-macro+ 3)
