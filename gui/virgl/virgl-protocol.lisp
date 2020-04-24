(in-package :mezzano.gui.virgl)

#|/*
 * Copyright 2014, 2015 Red Hat.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */|#

;;; Texture formats.
(defconstant +virgl-format-b8g8r8a8-unorm+           1)
(defconstant +virgl-format-b8g8r8x8-unorm+           2)
(defconstant +virgl-format-a8r8g8b8-unorm+           3)
(defconstant +virgl-format-x8r8g8b8-unorm+           4)
(defconstant +virgl-format-b5g5r5a1-unorm+           5)
(defconstant +virgl-format-b4g4r4a4-unorm+           6)
(defconstant +virgl-format-b5g6r5-unorm+             7)
(defconstant +virgl-format-r10g10b10a2-unorm+        8)
(defconstant +virgl-format-l8-unorm+                 9)    ; ubyte luminance
(defconstant +virgl-format-a8-unorm+                 10)   ; ubyte alpha
(defconstant +virgl-format-l8a8-unorm+               12)   ; ubyte alpha, luminance
(defconstant +virgl-format-l16-unorm+                13)   ; ushort luminance

(defconstant +virgl-format-z16-unorm+                16)
(defconstant +virgl-format-z32-unorm+                17)
(defconstant +virgl-format-z32-float+                18)
(defconstant +virgl-format-z24-unorm-s8-uint+        19)
(defconstant +virgl-format-s8-uint-z24-unorm+        20)
(defconstant +virgl-format-z24x8-unorm+              21)
(defconstant +virgl-format-s8-uint+                  23)   ; ubyte stencil

(defconstant +virgl-format-r32-float+                28)
(defconstant +virgl-format-r32g32-float+             29)
(defconstant +virgl-format-r32g32b32-float+          30)
(defconstant +virgl-format-r32g32b32a32-float+       31)

(defconstant +virgl-format-r16-unorm+                48)
(defconstant +virgl-format-r16g16-unorm+             49)

(defconstant +virgl-format-r16g16b16a16-unorm+       51)

(defconstant +virgl-format-r16-snorm+                56)
(defconstant +virgl-format-r16g16-snorm+             57)
(defconstant +virgl-format-r16g16b16a16-snorm+       59)

(defconstant +virgl-format-r8-unorm+                 64)
(defconstant +virgl-format-r8g8-unorm+               65)

(defconstant +virgl-format-r8g8b8a8-unorm+           67)

(defconstant +virgl-format-r8-snorm+                 74)
(defconstant +virgl-format-r8g8-snorm+               75)
(defconstant +virgl-format-r8g8b8-snorm+             76)
(defconstant +virgl-format-r8g8b8a8-snorm+           77)

(defconstant +virgl-format-r16-float+                91)
(defconstant +virgl-format-r16g16-float+             92)
(defconstant +virgl-format-r16g16b16-float+          93)
(defconstant +virgl-format-r16g16b16a16-float+       94)

(defconstant +virgl-format-l8-srgb+                  95)
(defconstant +virgl-format-l8a8-srgb+                96)
(defconstant +virgl-format-b8g8r8a8-srgb+            100)
(defconstant +virgl-format-b8g8r8x8-srgb+            101)
(defconstant +virgl-format-r8g8b8a8-srgb+            104)

;; compressed formats
(defconstant +virgl-format-dxt1-rgb+                 105)
(defconstant +virgl-format-dxt1-rgba+                106)
(defconstant +virgl-format-dxt3-rgba+                107)
(defconstant +virgl-format-dxt5-rgba+                108)

;; sRGB, compressed
(defconstant +virgl-format-dxt1-srgb+                109)
(defconstant +virgl-format-dxt1-srgba+               110)
(defconstant +virgl-format-dxt3-srgba+               111)
(defconstant +virgl-format-dxt5-srgba+               112)

;; rgtc compressed
(defconstant +virgl-format-rgtc1-unorm+              113)
(defconstant +virgl-format-rgtc1-snorm+              114)
(defconstant +virgl-format-rgtc2-unorm+              115)
(defconstant +virgl-format-rgtc2-snorm+              116)

(defconstant +virgl-format-a8b8g8r8-unorm+           121)
(defconstant +virgl-format-b5g5r5x1-unorm+           122)
(defconstant +virgl-format-r11g11b10-float+          124)
(defconstant +virgl-format-r9g9b9e5-float+           125)
(defconstant +virgl-format-z32-float-s8x24-uint+     126)

(defconstant +virgl-format-b10g10r10a2-unorm+        131)
(defconstant +virgl-format-r8g8b8x8-unorm+           134)
(defconstant +virgl-format-b4g4r4x4-unorm+           135)
(defconstant +virgl-format-x24s8-uint+               136)
(defconstant +virgl-format-s8x24-uint+               137)
(defconstant +virgl-format-b2g3r3-unorm+             139)

(defconstant +virgl-format-l16a16-unorm+             140)
(defconstant +virgl-format-a16-unorm+                141)

(defconstant +virgl-format-a8-snorm+                 147)
(defconstant +virgl-format-l8-snorm+                 148)
(defconstant +virgl-format-l8a8-snorm+               149)

(defconstant +virgl-format-a16-snorm+                151)
(defconstant +virgl-format-l16-snorm+                152)
(defconstant +virgl-format-l16a16-snorm+             153)

(defconstant +virgl-format-a16-float+                155)
(defconstant +virgl-format-l16-float+                156)
(defconstant +virgl-format-l16a16-float+             157)

(defconstant +virgl-format-a32-float+                159)
(defconstant +virgl-format-l32-float+                160)
(defconstant +virgl-format-l32a32-float+             161)

(defconstant +virgl-format-r8-uint+                  177)
(defconstant +virgl-format-r8g8-uint+                178)
(defconstant +virgl-format-r8g8b8-uint+              179)
(defconstant +virgl-format-r8g8b8a8-uint+            180)

(defconstant +virgl-format-r8-sint+                  181)
(defconstant +virgl-format-r8g8-sint+                182)
(defconstant +virgl-format-r8g8b8-sint+              183)
(defconstant +virgl-format-r8g8b8a8-sint+            184)

(defconstant +virgl-format-r16-uint+                 185)
(defconstant +virgl-format-r16g16-uint+              186)
(defconstant +virgl-format-r16g16b16-uint+           187)
(defconstant +virgl-format-r16g16b16a16-uint+        188)

(defconstant +virgl-format-r16-sint+                 189)
(defconstant +virgl-format-r16g16-sint+              190)
(defconstant +virgl-format-r16g16b16-sint+           191)
(defconstant +virgl-format-r16g16b16a16-sint+        192)
(defconstant +virgl-format-r32-uint+                 193)
(defconstant +virgl-format-r32g32-uint+              194)
(defconstant +virgl-format-r32g32b32-uint+           195)
(defconstant +virgl-format-r32g32b32a32-uint+        196)

(defconstant +virgl-format-r32-sint+                 197)
(defconstant +virgl-format-r32g32-sint+              198)
(defconstant +virgl-format-r32g32b32-sint+           199)
(defconstant +virgl-format-r32g32b32a32-sint+        200)

(defconstant +virgl-format-a8-uint+                  201)
(defconstant +virgl-format-l8-uint+                  203)
(defconstant +virgl-format-l8a8-uint+                204)

(defconstant +virgl-format-a8-sint+                  205)
(defconstant +virgl-format-l8-sint+                  207)
(defconstant +virgl-format-l8a8-sint+                208)

(defconstant +virgl-format-a16-uint+                 209)
(defconstant +virgl-format-l16-uint+                 211)
(defconstant +virgl-format-l16a16-uint+              212)

(defconstant +virgl-format-a16-sint+                 213)
(defconstant +virgl-format-l16-sint+                 215)
(defconstant +virgl-format-l16a16-sint+              216)

(defconstant +virgl-format-a32-uint+                 217)
(defconstant +virgl-format-l32-uint+                 219)
(defconstant +virgl-format-l32a32-uint+              220)

(defconstant +virgl-format-a32-sint+                 221)
(defconstant +virgl-format-l32-sint+                 223)
(defconstant +virgl-format-l32a32-sint+              224)

(defconstant +virgl-format-b10g10r10a2-uint+         225)
(defconstant +virgl-format-r8g8b8x8-snorm+           229)

(defconstant +virgl-format-r8g8b8x8-srgb+            230)

(defconstant +virgl-format-r8g8b8x8-uint+            231)
(defconstant +virgl-format-r8g8b8x8-sint+            232)
(defconstant +virgl-format-b10g10r10x2-unorm+        233)
(defconstant +virgl-format-r16g16b16x16-unorm+       234)
(defconstant +virgl-format-r16g16b16x16-snorm+       235)
(defconstant +virgl-format-r16g16b16x16-float+       236)
(defconstant +virgl-format-r16g16b16x16-uint+        237)
(defconstant +virgl-format-r16g16b16x16-sint+        238)

(defconstant +virgl-format-r10g10b10a2-uint+         253)

(defconstant +virgl-format-bptc-rgba-unorm+          255)
(defconstant +virgl-format-bptc-srgba+               256)
(defconstant +virgl-format-bptc-rgb-float+           257)
(defconstant +virgl-format-bptc-rgb-ufloat+          258)

(defconstant +virgl-format-r10g10b10x2-unorm+        308)
(defconstant +virgl-format-a4b4g4r4-unorm+           311)

;;; These are used by the capability_bits field in virgl_caps_v2.
(defconstant +virgl-cap-none+ 0)
(defconstant +virgl-cap-tgsi-invariant+       (ash 1 0))
(defconstant +virgl-cap-texture-view+         (ash 1 1))
(defconstant +virgl-cap-set-min-samples+      (ash 1 2))
(defconstant +virgl-cap-copy-image+           (ash 1 3))
(defconstant +virgl-cap-tgsi-precise+         (ash 1 4))
(defconstant +virgl-cap-txqs+                 (ash 1 5))
(defconstant +virgl-cap-memory-barrier+       (ash 1 6))
(defconstant +virgl-cap-compute-shader+       (ash 1 7))
(defconstant +virgl-cap-fb-no-attach+         (ash 1 8))
(defconstant +virgl-cap-robust-buffer-access+ (ash 1 9))
(defconstant +virgl-cap-tgsi-fbfetch+         (ash 1 10))
(defconstant +virgl-cap-shader-clock+         (ash 1 11))
(defconstant +virgl-cap-texture-barrier+      (ash 1 12))
(defconstant +virgl-cap-tgsi-components+      (ash 1 13))

;;; virgl bind flags - these are compatible with mesa 10.5 gallium.
;;; but are fixed, no other should be passed to virgl either.
(defconstant +virgl-bind-depth-stencil+ (ash 1 0))
(defconstant +virgl-bind-render-target+ (ash 1 1))
(defconstant +virgl-bind-sampler-view+  (ash 1 3))
(defconstant +virgl-bind-vertex-buffer+ (ash 1 4))
(defconstant +virgl-bind-index-buffer+  (ash 1 5))
(defconstant +virgl-bind-constant-buffer+ (ash 1 6))
(defconstant +virgl-bind-display-target+ (ash 1 7))
(defconstant +virgl-bind-stream-output+ (ash 1 11))
(defconstant +virgl-bind-shader-buffer+ (ash 1 14))
(defconstant +virgl-bind-cursor+        (ash 1 16))
(defconstant +virgl-bind-custom+        (ash 1 17))
(defconstant +virgl-bind-scanout+       (ash 1 18))

#|/*
 * Copyright 2014, 2015 Red Hat.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */|#

;;; Object types.
(defconstant +virgl-object-null+                       0)
(defconstant +virgl-object-blend+                      1)
(defconstant +virgl-object-rasterizer+                 2)
(defconstant +virgl-object-dsa+                        3)
(defconstant +virgl-object-shader+                     4)
(defconstant +virgl-object-vertex-elements+            5)
(defconstant +virgl-object-sampler-view+               6)
(defconstant +virgl-object-sampler-state+              7)
(defconstant +virgl-object-surface+                    8)
(defconstant +virgl-object-query+                      9)
(defconstant +virgl-object-streamout-target+          10)

;;; Context commands.
(defconstant +virgl-ccmd-nop+                          0)
(defconstant +virgl-ccmd-create-object+                1)
(defconstant +virgl-ccmd-bind-object+                  2)
(defconstant +virgl-ccmd-destroy-object+               3)
(defconstant +virgl-ccmd-set-viewport-state+           4)
(defconstant +virgl-ccmd-set-framebuffer-state+        5)
(defconstant +virgl-ccmd-set-vertex-buffers+           6)
(defconstant +virgl-ccmd-clear+                        7)
(defconstant +virgl-ccmd-draw-vbo+                     8)
(defconstant +virgl-ccmd-resource-inline-write+        9)
(defconstant +virgl-ccmd-set-sampler-views+           10)
(defconstant +virgl-ccmd-set-index-buffer+            11)
(defconstant +virgl-ccmd-set-constant-buffer+         12)
(defconstant +virgl-ccmd-set-stencil-ref+             13)
(defconstant +virgl-ccmd-set-blend-color+             14)
(defconstant +virgl-ccmd-set-scissor-state+           15)
(defconstant +virgl-ccmd-blit+                        16)
(defconstant +virgl-ccmd-resource-copy-region+        17)
(defconstant +virgl-ccmd-bind-sampler-states+         18)
(defconstant +virgl-ccmd-begin-query+                 19)
(defconstant +virgl-ccmd-end-query+                   20)
(defconstant +virgl-ccmd-get-query-result+            21)
(defconstant +virgl-ccmd-set-polygon-stipple+         22)
(defconstant +virgl-ccmd-set-clip-state+              23)
(defconstant +virgl-ccmd-set-sample-mask+             24)
(defconstant +virgl-ccmd-set-streamout-targets+       25)
(defconstant +virgl-ccmd-set-render-condition+        26)
(defconstant +virgl-ccmd-set-uniform-buffer+          27)

(defconstant +virgl-ccmd-set-sub-ctx+                 28)
(defconstant +virgl-ccmd-create-sub-ctx+              29)
(defconstant +virgl-ccmd-destroy-sub-ctx+             30)
(defconstant +virgl-ccmd-bind-shader+                 31)
(defconstant +virgl-ccmd-set-tess-state+              32)
(defconstant +virgl-ccmd-set-min-samples+             33)
(defconstant +virgl-ccmd-set-shader-buffers+          34)
(defconstant +virgl-ccmd-set-shader-images+           35)
(defconstant +virgl-ccmd-memory-barrier+              36)
(defconstant +virgl-ccmd-launch-grid+                 37)
(defconstant +virgl-ccmd-set-framebuffer-state-no-attach+ 38)
(defconstant +virgl-ccmd-texture-barrier+             39)

(defconstant +virgl-max-color-bufs+ 8)
(defconstant +virgl-max-clip-planes+ 8)


#|
/**************************************************************************
 *
 * Copyright 2007 VMware, Inc.
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sub license, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
 * IN NO EVENT SHALL VMWARE AND/OR ITS SUPPLIERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 **************************************************************************/
|#

(defconstant +pipe-blendfactor-one+                 #x1)
(defconstant +pipe-blendfactor-src-color+           #x2)
(defconstant +pipe-blendfactor-src-alpha+           #x3)
(defconstant +pipe-blendfactor-dst-alpha+           #x4)
(defconstant +pipe-blendfactor-dst-color+           #x5)
(defconstant +pipe-blendfactor-src-alpha-saturate+  #x6)
(defconstant +pipe-blendfactor-const-color+         #x7)
(defconstant +pipe-blendfactor-const-alpha+         #x8)
(defconstant +pipe-blendfactor-src1-color+          #x9)
(defconstant +pipe-blendfactor-src1-alpha+          #x0A)
(defconstant +pipe-blendfactor-zero+                #x11)
(defconstant +pipe-blendfactor-inv-src-color+       #x12)
(defconstant +pipe-blendfactor-inv-src-alpha+       #x13)
(defconstant +pipe-blendfactor-inv-dst-alpha+       #x14)
(defconstant +pipe-blendfactor-inv-dst-color+       #x15)
(defconstant +pipe-blendfactor-inv-const-color+     #x17)
(defconstant +pipe-blendfactor-inv-const-alpha+     #x18)
(defconstant +pipe-blendfactor-inv-src1-color+      #x19)
(defconstant +pipe-blendfactor-inv-src1-alpha+      #x1A)

(defconstant +pipe-blend-add+               0)
(defconstant +pipe-blend-subtract+          1)
(defconstant +pipe-blend-reverse-subtract+  2)
(defconstant +pipe-blend-min+               3)
(defconstant +pipe-blend-max+               4)

(defconstant +pipe-logicop-clear+            0)
(defconstant +pipe-logicop-nor+              1)
(defconstant +pipe-logicop-and-inverted+     2)
(defconstant +pipe-logicop-copy-inverted+    3)
(defconstant +pipe-logicop-and-reverse+      4)
(defconstant +pipe-logicop-invert+           5)
(defconstant +pipe-logicop-xor+              6)
(defconstant +pipe-logicop-nand+             7)
(defconstant +pipe-logicop-and+              8)
(defconstant +pipe-logicop-equiv+            9)
(defconstant +pipe-logicop-noop+             10)
(defconstant +pipe-logicop-or-inverted+      11)
(defconstant +pipe-logicop-copy+             12)
(defconstant +pipe-logicop-or-reverse+       13)
(defconstant +pipe-logicop-or+               14)
(defconstant +pipe-logicop-set+              15)

(defconstant +pipe-mask-r+  #x1)
(defconstant +pipe-mask-g+  #x2)
(defconstant +pipe-mask-b+  #x4)
(defconstant +pipe-mask-a+  #x8)
(defconstant +pipe-mask-rgba+ #xf)
(defconstant +pipe-mask-z+  #x10)
(defconstant +pipe-mask-s+  #x20)
(defconstant +pipe-mask-zs+ #x30)
(defconstant +pipe-mask-rgbazs+ (logior +pipe-mask-rgba+ +pipe-mask-zs+))


;;; Inequality functions.  Used for depth test, stencil compare, alpha
;;; test, shadow compare, etc.
(defconstant +pipe-func-never+    0)
(defconstant +pipe-func-less+     1)
(defconstant +pipe-func-equal+    2)
(defconstant +pipe-func-lequal+   3)
(defconstant +pipe-func-greater+  4)
(defconstant +pipe-func-notequal+ 5)
(defconstant +pipe-func-gequal+   6)
(defconstant +pipe-func-always+   7)

;;; Polygon fill mode
(defconstant +pipe-polygon-mode-fill+  0)
(defconstant +pipe-polygon-mode-line+  1)
(defconstant +pipe-polygon-mode-point+ 2)

;;; Polygon face specification, eg for culling
(defconstant +pipe-face-none+           0)
(defconstant +pipe-face-front+          1)
(defconstant +pipe-face-back+           2)
(defconstant +pipe-face-front-and-back+ (logior +pipe-face-front+ +pipe-face-back+))

;;; Stencil ops
(defconstant +pipe-stencil-op-keep+       0)
(defconstant +pipe-stencil-op-zero+       1)
(defconstant +pipe-stencil-op-replace+    2)
(defconstant +pipe-stencil-op-incr+       3)
(defconstant +pipe-stencil-op-decr+       4)
(defconstant +pipe-stencil-op-incr-wrap+  5)
(defconstant +pipe-stencil-op-decr-wrap+  6)
(defconstant +pipe-stencil-op-invert+     7)

;;; Texture types.
;;; See the documentation for info on PIPE-TEXTURE-RECT vs PIPE-TEXTURE-2D
;;; [what documentation?]
(defconstant +pipe-buffer+           0)
(defconstant +pipe-texture-1d+       1)
(defconstant +pipe-texture-2d+       2)
(defconstant +pipe-texture-3d+       3)
(defconstant +pipe-texture-cube+     4)
(defconstant +pipe-texture-rect+     5)
(defconstant +pipe-texture-1d-array+ 6)
(defconstant +pipe-texture-2d-array+ 7)
(defconstant +pipe-texture-cube-array+ 8)

(defconstant +pipe-tex-face-pos-x+ 0)
(defconstant +pipe-tex-face-neg-x+ 1)
(defconstant +pipe-tex-face-pos-y+ 2)
(defconstant +pipe-tex-face-neg-y+ 3)
(defconstant +pipe-tex-face-pos-z+ 4)
(defconstant +pipe-tex-face-neg-z+ 5)
(defconstant +pipe-tex-face-max+   6)

(defconstant +pipe-tex-wrap-repeat+                   0)
(defconstant +pipe-tex-wrap-clamp+                    1)
(defconstant +pipe-tex-wrap-clamp-to-edge+            2)
(defconstant +pipe-tex-wrap-clamp-to-border+          3)
(defconstant +pipe-tex-wrap-mirror-repeat+            4)
(defconstant +pipe-tex-wrap-mirror-clamp+             5)
(defconstant +pipe-tex-wrap-mirror-clamp-to-edge+     6)
(defconstant +pipe-tex-wrap-mirror-clamp-to-border+   7)

;;; Between mipmaps, ie mipfilter
(defconstant +pipe-tex-mipfilter-nearest+  0)
(defconstant +pipe-tex-mipfilter-linear+   1)
(defconstant +pipe-tex-mipfilter-none+     2)

;;; Within a mipmap, ie min/mag filter
(defconstant +pipe-tex-filter-nearest+      0)
(defconstant +pipe-tex-filter-linear+       1)

(defconstant +pipe-tex-compare-none+          0)
(defconstant +pipe-tex-compare-r-to-texture+  1)

;;; Clear buffer bits
(defconstant +pipe-clear-depth+        (ash 1 0))
(defconstant +pipe-clear-stencil+      (ash 1 1))
(defconstant +pipe-clear-color0+       (ash 1 2))
(defconstant +pipe-clear-color1+       (ash 1 3))
(defconstant +pipe-clear-color2+       (ash 1 4))
(defconstant +pipe-clear-color3+       (ash 1 5))
(defconstant +pipe-clear-color4+       (ash 1 6))
(defconstant +pipe-clear-color5+       (ash 1 7))
(defconstant +pipe-clear-color6+       (ash 1 8))
(defconstant +pipe-clear-color7+       (ash 1 9))
;; Combined flags
;; All color buffers currently bound
(defconstant +pipe-clear-color+        (logior +pipe-clear-color0+ +pipe-clear-color1+
                                               +pipe-clear-color2+ +pipe-clear-color3+
                                               +pipe-clear-color4+ +pipe-clear-color5+
                                               +pipe-clear-color6+ +pipe-clear-color7+))
(defconstant +pipe-clear-depthstencil+ (logior +pipe-clear-depth+ +pipe-clear-stencil+))

;;; Transfer object usage flags
(defconstant +pipe-transfer-read+ (ash 1 0)
  "Resource contents read back (or accessed directly) at transfer create time.")
(defconstant +pipe-transfer-write+ (ash 1 1)
  "Resource contents will be written back at transfer-unmap time (or modified as a result of being accessed directly).")
(defconstant +pipe-transfer-read-write+ (logior +pipe-transfer-read+ +pipe-transfer-write+)
  "Read/modify/write")
(defconstant +pipe-transfer-map-directly+ (ash 1 2)
  " The transfer should map the texture storage directly. The driver may
return NULL if that isn't possible, and the state tracker needs to cope
with that and use an alternative path without this flag.

E.g. the state tracker could have a simpler path which maps textures and
does read/modify/write cycles on them directly, and a more complicated
path which uses minimal read and write transfers.")
(defconstant +pipe-transfer-discard-range+ (ash 1 8)
  "Discards the memory within the mapped region.

It should not be used with PIPE-TRANSFER-READ.

See also:
 - OpenGL's ARB-map-buffer-range extension, MAP-INVALIDATE-RANGE-BIT flag.")
(defconstant +pipe-transfer-dontblock+ (ash 1 9)
  "Fail if the resource cannot be mapped immediately.

See also:
 - Direct3D's D3DLOCK-DONOTWAIT flag.
 - Mesa3D's MESA-MAP-NOWAIT-BIT flag.
 - WDDM's D3DDDICB-LOCKFLAGS.DonotWait flag.")
(defconstant +pipe-transfer-unsynchronized+ (ash 1 10)
   "Do not attempt to synchronize pending operations on the resource when mapping.

It should not be used with PIPE-TRANSFER-READ.

See also:
 - OpenGL's ARB-map-buffer-range extension, MAP-UNSYNCHRONIZED-BIT flag.
 - Direct3D's D3DLOCK-NOOVERWRITE flag.
 - WDDM's D3DDDICB-LOCKFLAGS.IgnoreSync flag.")
(defconstant +pipe-transfer-flush-explicit+ (ash 1 11)
  "Written ranges will be notified later with pipe-context::transfer-flush-region.

It should not be used with PIPE-TRANSFER-READ.

See also:
 - pipe-context::transfer-flush-region
 - OpenGL's ARB-map-buffer-range extension, MAP-FLUSH-EXPLICIT-BIT flag.")
(defconstant +pipe-transfer-discard-whole-resource+ (ash 1 12)
  "Discards all memory backing the resource.

It should not be used with PIPE-TRANSFER-READ.

This is equivalent to:
 - OpenGL's ARB-map-buffer-range extension, MAP-INVALIDATE-BUFFER-BIT
 - BufferData(NULL) on a GL buffer
 - Direct3D's D3DLOCK-DISCARD flag.
 - WDDM's D3DDDICB-LOCKFLAGS.Discard flag.
 - D3D10 DDI's D3D10-DDI-MAP-WRITE-DISCARD flag
 - D3D10's D3D10-MAP-WRITE-DISCARD flag.")
(defconstant +pipe-transfer-persistent+ (ash 1 13)
  "Allows the resource to be used for rendering while mapped.

PIPE-RESOURCE-FLAG-MAP-PERSISTENT must be set when creating the resource.

If COHERENT is not set, memory-barrier(PIPE-BARRIER-MAPPED-BUFFER)
must be called to ensure the device can see what the CPU has written.")
(defconstant +pipe-transfer-coherent+ (ash 1 14)
"If PERSISTENT is set, this ensures any writes done by the device are
immediately visible to the CPU and vice versa.

PIPE-RESOURCE-FLAG-MAP-COHERENT must be set when creating the resource.")

;;; Flags for the flush function.
(defconstant +pipe-flush-end-of-frame+ (ash 1 0))

;;; Flags for pipe-context::memory-barrier.
(defconstant +pipe-barrier-mapped-buffer+     (ash 1 0))
(defconstant +pipe-barrier-shader-buffer+     (ash 1 1))
(defconstant +pipe-barrier-query-buffer+      (ash 1 2))
(defconstant +pipe-barrier-vertex-buffer+     (ash 1 3))
(defconstant +pipe-barrier-index-buffer+      (ash 1 4))
(defconstant +pipe-barrier-constant-buffer+   (ash 1 5))
(defconstant +pipe-barrier-indirect-buffer+   (ash 1 6))
(defconstant +pipe-barrier-texture+           (ash 1 7))
(defconstant +pipe-barrier-image+             (ash 1 8))
(defconstant +pipe-barrier-framebuffer+       (ash 1 9))
(defconstant +pipe-barrier-streamout-buffer+  (ash 1 10))
(defconstant +pipe-barrier-global-buffer+     (ash 1 11))
(defconstant +pipe-barrier-all+               (1- (ash 1 12)))

;;; Flags for pipe-context::texture-barrier.
(defconstant +pipe-texture-barrier-sampler+      (ash 1 0))
(defconstant +pipe-texture-barrier-framebuffer+  (ash 1 1))

;;; Resource binding flags -- state tracker must specify in advance all
;;; the ways a resource might be used.
(defconstant +pipe-bind-depth-stencil+        (ash 1 0)) ; create-surface
(defconstant +pipe-bind-render-target+        (ash 1 1)) ; create-surface
(defconstant +pipe-bind-blendable+            (ash 1 2)) ; create-surface
(defconstant +pipe-bind-sampler-view+         (ash 1 3)) ; create-sampler-view
(defconstant +pipe-bind-vertex-buffer+        (ash 1 4)) ; set-vertex-buffers
(defconstant +pipe-bind-index-buffer+         (ash 1 5)) ; draw-elements
(defconstant +pipe-bind-constant-buffer+      (ash 1 6)) ; set-constant-buffer
(defconstant +pipe-bind-display-target+       (ash 1 8)) ; flush-front-buffer
(defconstant +pipe-bind-transfer-write+       (ash 1 9)) ; transfer-map
(defconstant +pipe-bind-transfer-read+        (ash 1 10)) ; transfer-map
(defconstant +pipe-bind-stream-output+        (ash 1 11)) ; set-stream-output-buffers
(defconstant +pipe-bind-cursor+               (ash 1 16)) ; mouse cursor
(defconstant +pipe-bind-custom+               (ash 1 17)) ; state-tracker/winsys usages
(defconstant +pipe-bind-global+               (ash 1 18)) ; set-global-binding
(defconstant +pipe-bind-shader-resource+      (ash 1 19)) ; set-shader-resources
(defconstant +pipe-bind-compute-resource+     (ash 1 20)) ; set-compute-resources
(defconstant +pipe-bind-command-args-buffer+  (ash 1 21)) ; pipe-draw-info.indirect

#|
/* The first two flags above were previously part of the amorphous
 * TEXTURE-USAGE, most of which are now descriptions of the ways a
 * particular texture can be bound to the gallium pipeline.  The two flags
 * below do not fit within that and probably need to be migrated to some
 * other place.
 *
 * It seems like scanout is used by the Xorg state tracker to ask for
 * a texture suitable for actual scanout (hence the name), which
 * implies extra layout constraints on some hardware.  It may also
 * have some special meaning regarding mouse cursor images.
 *
 * The shared flag is quite underspecified, but certainly isn't a
 * binding flag - it seems more like a message to the winsys to create
 * a shareable allocation.
 *
 * The third flag has been added to be able to force textures to be created
 * in linear mode (no tiling).
 */
|#
(defconstant +pipe-bind-scanout+     (ash 1 14))
(defconstant +pipe-bind-shared+      (ash 1 15)) ; get-texture-handle ???
(defconstant +pipe-bind-linear+      (ash 1 21))

;;; Flags for the driver about resource behaviour:
(defconstant +pipe-resource-flag-map-persistent+ (ash 1 0))
(defconstant +pipe-resource-flag-map-coherent+   (ash 1 1))
(defconstant +pipe-resource-flag-drv-priv+    (ash 1 16)) ; driver/winsys private
(defconstant +pipe-resource-flag-st-priv+     (ash 1 24)) ; state-tracker/winsys private

;;; Hint about the expected lifecycle of a resource.
;;; Sorted according to GPU vs CPU access.
(defconstant +pipe-usage-default+        0) ;; fast GPU access
(defconstant +pipe-usage-immutable+      1) ;; fast GPU access, immutable
(defconstant +pipe-usage-dynamic+        2) ;; uploaded data is used multiple times
(defconstant +pipe-usage-stream+         3) ;; uploaded data is used once
(defconstant +pipe-usage-staging+        4) ;; fast CPU access

;;; Shaders
(defconstant +pipe-shader-vertex+   0)
(defconstant +pipe-shader-fragment+ 1)
(defconstant +pipe-shader-geometry+ 2)
(defconstant +pipe-shader-tess-ctrl+ 3)
(defconstant +pipe-shader-tess-eval+ 4)
(defconstant +pipe-shader-compute+  5)
(defconstant +pipe-shader-types+    6)

;;; Primitive types:
(defconstant +pipe-prim-points+                    0)
(defconstant +pipe-prim-lines+                     1)
(defconstant +pipe-prim-line-loop+                 2)
(defconstant +pipe-prim-line-strip+                3)
(defconstant +pipe-prim-triangles+                 4)
(defconstant +pipe-prim-triangle-strip+            5)
(defconstant +pipe-prim-triangle-fan+              6)
(defconstant +pipe-prim-quads+                     7)
(defconstant +pipe-prim-quad-strip+                8)
(defconstant +pipe-prim-polygon+                   9)
(defconstant +pipe-prim-lines-adjacency+          10)
(defconstant +pipe-prim-line-strip-adjacency+     11)
(defconstant +pipe-prim-triangles-adjacency+      12)
(defconstant +pipe-prim-triangle-strip-adjacency+ 13)
(defconstant +pipe-prim-patches+                  14)
(defconstant +pipe-prim-max+                      15)

;;; Tessellator spacing types
(defconstant +pipe-tess-spacing-fractional-odd+    0)
(defconstant +pipe-tess-spacing-fractional-even+   1)
(defconstant +pipe-tess-spacing-equal+             2)

;;; Query object types
(defconstant +pipe-query-occlusion-counter+     0)
(defconstant +pipe-query-occlusion-predicate+   1)
(defconstant +pipe-query-timestamp+             2)
(defconstant +pipe-query-timestamp-disjoint+    3)
(defconstant +pipe-query-time-elapsed+          4)
(defconstant +pipe-query-primitives-generated+  5)
(defconstant +pipe-query-primitives-emitted+    6)
(defconstant +pipe-query-so-statistics+         7)
(defconstant +pipe-query-so-overflow-predicate+ 8)
(defconstant +pipe-query-gpu-finished+          9)
(defconstant +pipe-query-pipeline-statistics+  10)
(defconstant +pipe-query-occlusion-predicate-conservative+ 11)
(defconstant +pipe-query-so-overflow-any-predicate+ 12)
(defconstant +pipe-query-types+                13)

;;; start of driver queries,
;;; see pipe-screen::get-driver-query-info
(defconstant +pipe-query-driver-specific+     256)

;;; Conditional rendering modes
(defconstant +pipe-render-cond-wait+              0)
(defconstant +pipe-render-cond-no-wait+           1)
(defconstant +pipe-render-cond-by-region-wait+    2)
(defconstant +pipe-render-cond-by-region-no-wait+ 3)

;;; Point sprite coord modes
(defconstant +pipe-sprite-coord-upper-left+ 0)
(defconstant +pipe-sprite-coord-lower-left+ 1)

;;; Texture swizzles
(defconstant +pipe-swizzle-red+   0)
(defconstant +pipe-swizzle-green+ 1)
(defconstant +pipe-swizzle-blue+  2)
(defconstant +pipe-swizzle-alpha+ 3)
(defconstant +pipe-swizzle-zero+  4)
(defconstant +pipe-swizzle-one+   5)

(defconstant +pipe-timeout-infinite+ #xffffffffffffffff)

;;; pipe-image-view access flags.
(defconstant +pipe-image-access-read+       (ash 1 0))
(defconstant +pipe-image-access-write+      (ash 1 1))
(defconstant +pipe-image-access-read-write+ (logior +pipe-image-access-read+
                                                    +pipe-image-access-write+))

;;; Implementation capabilities/limits which are queried through
;;; pipe-screen::get-param()
(defconstant +pipe-cap-npot-textures+ 1)
(defconstant +pipe-cap-two-sided-stencil+ 2)
(defconstant +pipe-cap-max-dual-source-render-targets+ 4)
(defconstant +pipe-cap-anisotropic-filter+ 5)
(defconstant +pipe-cap-point-sprite+ 6)
(defconstant +pipe-cap-max-render-targets+ 7)
(defconstant +pipe-cap-occlusion-query+ 8)
(defconstant +pipe-cap-query-time-elapsed+ 9)
(defconstant +pipe-cap-texture-shadow-map+ 10)
(defconstant +pipe-cap-texture-swizzle+ 11)
(defconstant +pipe-cap-max-texture-2d-levels+ 12)
(defconstant +pipe-cap-max-texture-3d-levels+ 13)
(defconstant +pipe-cap-max-texture-cube-levels+ 14)
(defconstant +pipe-cap-texture-mirror-clamp+ 25)
(defconstant +pipe-cap-blend-equation-separate+ 28)
(defconstant +pipe-cap-sm3+ 29 "Shader Model, supported")
(defconstant +pipe-cap-max-stream-output-buffers+ 30)
(defconstant +pipe-cap-primitive-restart+ 31)
(defconstant +pipe-cap-indep-blend-enable+ 33
  "blend enables and write masks per rendertarget")
(defconstant +pipe-cap-indep-blend-func+ 34
  "different blend funcs per rendertarget")
(defconstant +pipe-cap-max-texture-array-layers+ 36)
(defconstant +pipe-cap-tgsi-fs-coord-origin-upper-left+ 37)
(defconstant +pipe-cap-tgsi-fs-coord-origin-lower-left+ 38)
(defconstant +pipe-cap-tgsi-fs-coord-pixel-center-half-integer+ 39)
(defconstant +pipe-cap-tgsi-fs-coord-pixel-center-integer+ 40)
(defconstant +pipe-cap-depth-clip-disable+ 41)
(defconstant +pipe-cap-shader-stencil-export+ 42)
(defconstant +pipe-cap-tgsi-instanceid+ 43)
(defconstant +pipe-cap-vertex-element-instance-divisor+ 44)
(defconstant +pipe-cap-fragment-color-clamped+ 45)
(defconstant +pipe-cap-mixed-colorbuffer-formats+ 46)
(defconstant +pipe-cap-seamless-cube-map+ 47)
(defconstant +pipe-cap-seamless-cube-map-per-texture+ 48)
(defconstant +pipe-cap-min-texel-offset+ 50)
(defconstant +pipe-cap-max-texel-offset+ 51)
(defconstant +pipe-cap-conditional-render+ 52)
(defconstant +pipe-cap-texture-barrier+ 53)
(defconstant +pipe-cap-max-stream-output-separate-components+ 55)
(defconstant +pipe-cap-max-stream-output-interleaved-components+ 56)
(defconstant +pipe-cap-stream-output-pause-resume+ 57)
(defconstant +pipe-cap-tgsi-can-compact-constants+ 59) ;; temporary
(defconstant +pipe-cap-vertex-color-unclamped+ 60)
(defconstant +pipe-cap-vertex-color-clamped+ 61)
(defconstant +pipe-cap-glsl-feature-level+ 62)
(defconstant +pipe-cap-quads-follow-provoking-vertex-convention+ 63)
(defconstant +pipe-cap-user-vertex-buffers+ 64)
(defconstant +pipe-cap-vertex-buffer-offset-4byte-aligned-only+ 65)
(defconstant +pipe-cap-vertex-buffer-stride-4byte-aligned-only+ 66)
(defconstant +pipe-cap-vertex-element-src-offset-4byte-aligned-only+ 67)
(defconstant +pipe-cap-compute+ 68)
(defconstant +pipe-cap-user-index-buffers+ 69)
(defconstant +pipe-cap-user-constant-buffers+ 70)
(defconstant +pipe-cap-constant-buffer-offset-alignment+ 71)
(defconstant +pipe-cap-start-instance+ 72)
(defconstant +pipe-cap-query-timestamp+ 73)
(defconstant +pipe-cap-texture-multisample+ 74)
(defconstant +pipe-cap-min-map-buffer-alignment+ 75)
(defconstant +pipe-cap-cube-map-array+ 76)
(defconstant +pipe-cap-texture-buffer-objects+ 77)
(defconstant +pipe-cap-texture-buffer-offset-alignment+ 78)
(defconstant +pipe-cap-tgsi-texcoord+ 79)
(defconstant +pipe-cap-prefer-blit-based-texture-transfer+ 80)
(defconstant +pipe-cap-query-pipeline-statistics+ 81)
(defconstant +pipe-cap-texture-border-color-quirk+ 82)
(defconstant +pipe-cap-max-texture-buffer-size+ 83)
(defconstant +pipe-cap-max-viewports+ 84)
(defconstant +pipe-cap-endianness+ 85)
(defconstant +pipe-cap-mixed-framebuffer-sizes+ 86)
(defconstant +pipe-cap-tgsi-vs-layer-viewport+ 87)
(defconstant +pipe-cap-max-geometry-output-vertices+ 88)
(defconstant +pipe-cap-max-geometry-total-output-components+ 89)
(defconstant +pipe-cap-max-texture-gather-components+ 90)
(defconstant +pipe-cap-texture-gather-sm5+ 91)
(defconstant +pipe-cap-buffer-map-persistent-coherent+ 92)
(defconstant +pipe-cap-fake-sw-msaa+ 93)
(defconstant +pipe-cap-texture-query-lod+ 94)
(defconstant +pipe-cap-min-texture-gather-offset+ 95)
(defconstant +pipe-cap-max-texture-gather-offset+ 96)
(defconstant +pipe-cap-sample-shading+ 97)
(defconstant +pipe-cap-texture-gather-offsets+ 98)
(defconstant +pipe-cap-tgsi-vs-window-space-position+ 99)
(defconstant +pipe-cap-max-vertex-streams+ 100)
(defconstant +pipe-cap-draw-indirect+ 101)
(defconstant +pipe-cap-tgsi-fs-fine-derivative+ 102)
(defconstant +pipe-cap-vendor-id+ 103)
(defconstant +pipe-cap-device-id+ 104)
(defconstant +pipe-cap-accelerated+ 105)
(defconstant +pipe-cap-video-memory+ 106)
(defconstant +pipe-cap-uma+ 107)
(defconstant +pipe-cap-conditional-render-inverted+ 108)
(defconstant +pipe-cap-max-vertex-attrib-stride+ 109)
(defconstant +pipe-cap-sampler-view-target+ 110)
(defconstant +pipe-cap-clip-halfz+ 111)
(defconstant +pipe-cap-vertexid-nobase+ 112)
(defconstant +pipe-cap-polygon-offset-clamp+ 113)

(defconstant +pipe-quirk-texture-border-color-swizzle-nv50+ (ash 1 0))
(defconstant +pipe-quirk-texture-border-color-swizzle-r600+ (ash 1 1))

(defconstant +pipe-endian-little+ 0)
(defconstant +pipe-endian-big+ 1)

;;; Implementation limits which are queried through
;;; pipe-screen::get-paramf()
(defconstant +pipe-capf-max-line-width+ 0)
(defconstant +pipe-capf-max-line-width-aa+ 1)
(defconstant +pipe-capf-max-point-width+ 2)
(defconstant +pipe-capf-max-point-width-aa+ 3)
(defconstant +pipe-capf-max-texture-anisotropy+ 4)
(defconstant +pipe-capf-max-texture-lod-bias+ 5)
(defconstant +pipe-capf-guard-band-left+ 6)
(defconstant +pipe-capf-guard-band-top+ 7)
(defconstant +pipe-capf-guard-band-right+ 8)
(defconstant +pipe-capf-guard-band-bottom+ 9)

;;; Shader caps not specific to any single stage
(defconstant +pipe-shader-cap-max-instructions+ 0) ; if 0, it means the stage is unsupported
(defconstant +pipe-shader-cap-max-alu-instructions+ 1)
(defconstant +pipe-shader-cap-max-tex-instructions+ 2)
(defconstant +pipe-shader-cap-max-tex-indirections+ 3)
(defconstant +pipe-shader-cap-max-control-flow-depth+ 4)
(defconstant +pipe-shader-cap-max-inputs+ 5)
(defconstant +pipe-shader-cap-max-outputs+ 6)
(defconstant +pipe-shader-cap-max-const-buffer-size+ 7)
(defconstant +pipe-shader-cap-max-const-buffers+ 8)
(defconstant +pipe-shader-cap-max-temps+ 9)
(defconstant +pipe-shader-cap-max-preds+ 10)
;; boolean caps
(defconstant +pipe-shader-cap-tgsi-cont-supported+ 11)
(defconstant +pipe-shader-cap-indirect-input-addr+ 12)
(defconstant +pipe-shader-cap-indirect-output-addr+ 13)
(defconstant +pipe-shader-cap-indirect-temp-addr+ 14)
(defconstant +pipe-shader-cap-indirect-const-addr+ 15)
(defconstant +pipe-shader-cap-subroutines+ 16) ; BGNSUB, ENDSUB, CAL, RET
(defconstant +pipe-shader-cap-integers+ 17)
(defconstant +pipe-shader-cap-max-texture-samplers+ 18)
(defconstant +pipe-shader-cap-preferred-ir+ 19)
(defconstant +pipe-shader-cap-tgsi-sqrt-supported+ 20)
(defconstant +pipe-shader-cap-max-sampler-views+ 21)
(defconstant +pipe-shader-cap-doubles+ 22)

;;; Shader intermediate representation.
(defconstant +pipe-shader-ir-tgsi+ 0)
(defconstant +pipe-shader-ir-llvm+ 1)
(defconstant +pipe-shader-ir-native+ 2)

;;; Compute-specific implementation capability.  They can be queried
;;; using pipe-screen::get-compute-param.
(defconstant +pipe-compute-cap-ir-target+ 0)
(defconstant +pipe-compute-cap-grid-dimension+ 1)
(defconstant +pipe-compute-cap-max-grid-size+ 2)
(defconstant +pipe-compute-cap-max-block-size+ 3)
(defconstant +pipe-compute-cap-max-threads-per-block+ 4)
(defconstant +pipe-compute-cap-max-global-size+ 5)
(defconstant +pipe-compute-cap-max-local-size+ 6)
(defconstant +pipe-compute-cap-max-private-size+ 7)
(defconstant +pipe-compute-cap-max-input-size+ 8)
(defconstant +pipe-compute-cap-max-mem-alloc-size+ 9)
(defconstant +pipe-compute-cap-max-clock-frequency+ 10)
(defconstant +pipe-compute-cap-max-compute-units+ 11)
(defconstant +pipe-compute-cap-images-supported+ 12)

(defconstant +virgl-resource-y-0-top+ 1)
