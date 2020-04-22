;;;; Test rendering.

(in-package :mezzano.gui.virgl.test)

(defun test-clear (&optional (red 0.25) (green 0.33) (blue 0.66))
  "Clear the scanout."
  (virgl:with-context (context :name "Test context")
    ;; Create a surface object backed by the scanout buffer
    (let* ((scanout (virgl:virgl-scanout (virgl:virgl context)))
           (scanout-surface (virgl:make-surface context scanout))
           (cmd-buf (virgl:make-command-buffer context)))
      ;; Attach scanout surface object to the framebuffer's color0 channel.
      ;; No depth/stencil surface.
      (virgl:add-command-set-framebuffer-state cmd-buf nil scanout-surface)
      ;; Clear.
      (virgl:add-command-clear
       cmd-buf :color red green blue 1.0 0.0d0 0)
      (virgl:command-buffer-finalize cmd-buf)
      (virgl:command-buffer-submit cmd-buf)
      (virgl:scanout-flush scanout 0 0 (virgl:width scanout) (virgl:height scanout)))))

(defun test-triangle ()
  "Clear the scanout and draw a simple triangle."
  (virgl:with-context (context :name "Test context")
    (let ((test-data '(0.0 0.0 0.0 1.0   ; Position 0
                       1.0 0.0 0.0 1.0   ; Colour 0
                       0.5 0.0 0.0 1.0   ; Position 1
                       0.0 1.0 0.0 1.0   ; Colour 1
                       0.5 0.5 0.0 1.0   ; Position 2
                       0.0 0.0 1.0 1.0))); Colour 2
      (virgl:with-resource (vertex-buffer (virgl:make-vertex-buffer context (* (length test-data) 4) :name "Test vertex buffer"))
        ;; Upload test data
        (let* ((dma-buf (virgl:resource-dma-buffer vertex-buffer))
               (vertex-buf (make-array (sup:dma-buffer-length dma-buf)
                                       :element-type '(unsigned-byte 8)
                                       :fill-pointer 0
                                       :memory dma-buf)))
          (dolist (val test-data)
            (virgl::vector-push-extend-single/le val vertex-buf))
          (virgl:transfer-to-gpu vertex-buffer))
        (let* ((vertex-shader (virgl:make-shader context
                                                 :vertex
                                                 '((tgsi:dcl (:in 0))
                                                   (tgsi:dcl (:in 1))
                                                   (tgsi:dcl (:out 0) :position)
                                                   (tgsi:dcl (:out 1) :color)
                                                   ;; Pass vertex position (input 0) and colour (input 1) through unchanged.
                                                   1 (tgsi:mov (:out 0) (:in 0))
                                                   2 (tgsi:mov (:out 1) (:in 1))
                                                   3 (tgsi:end))))
               (fragment-shader (virgl:make-shader context
                                                   :fragment
                                                   ;; Color semantic and color interpolation
                                                   '((tgsi:dcl (:in 0) :color :color)
                                                     (tgsi:dcl (:out 0) :color)
                                                     (tgsi:imm :flt32 (1.0 1.0 1.0 1.0))
                                                     ;; Invert the colour channel.
                                                     1 (tgsi:sub (:out 0) (:imm 0) (:in 0))
                                                     2 (tgsi:end))))
               ;; Create a surface object backed by the scanout buffer
               (scanout (virgl:virgl-scanout (virgl:virgl context)))
               (scanout-surface (virgl:make-surface context scanout))
               ;; Create vertex elements buffer.
               (vertex-elements (virgl:make-vertex-elements
                                 context nil
                                 ;; First element: Positions.
                                 '(0 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)
                                 ;; Second element: Colours.
                                 '(16 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)))
               (blend (virgl:make-blend context :colormask :rgba))
               (cmd-buf (virgl:make-command-buffer context)))
          ;; Attach scanout surface object to the framebuffer's color0 channel.
          ;; No depth/stencil surface.
          (virgl:add-command-set-framebuffer-state cmd-buf nil scanout-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5
           (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5)
          ;; Clear.
          (virgl:add-command-clear
           cmd-buf :color 0.25 0.33 0.66 0.75 0.0d0 0)
          ;; Configure vertex buffer
          (virgl:add-command-set-vertex-buffers
           cmd-buf
           `(32 ; stride, 2*float4
             0 ; offset
             ,vertex-buffer)) ; vertex buffer resource handle
          (virgl:add-command-bind-vertex-elements cmd-buf vertex-elements)
          (virgl:add-command-bind-shader cmd-buf vertex-shader)
          (virgl:add-command-bind-shader cmd-buf fragment-shader)
          ;; Enable writes to the color channels
          (virgl:add-command-bind-blend cmd-buf blend)
          (virgl:add-command-draw-vbo cmd-buf 0 3 :triangles)
          (virgl:command-buffer-finalize cmd-buf)
          (virgl:command-buffer-submit cmd-buf)
          (virgl:scanout-flush scanout 0 0 (virgl:width scanout) (virgl:height scanout)))))))

(defun test-spin ()
  "Clear the scanout and draw a spinning triangle."
  (virgl:with-context (context :name "Test context")
    (let ((test-data '(0.0 0.0 0.0 1.0   ; 0 Position A0
                       1.0 0.0 0.0 1.0   ;   Colour A0
                       0.5 0.0 0.0 1.0   ; 1 Position A1
                       0.0 1.0 0.0 1.0   ;   Colour A1
                       0.5 0.5 0.0 1.0   ; 2 Position A2
                       0.0 0.0 1.0 1.0   ;   Colour A2
                       0.0 0.5 0.0 1.0   ; 3 Position B3
                       1.0 1.0 0.0 1.0)) ;   Colour B3
          (test-indices '(0 1 2 0 3 2)))
      (virgl:with-resources ((vertex-buffer (virgl:make-vertex-buffer context (* (length test-data) 4) :name "Test vertex buffer"))
                             (index-buffer (virgl:make-index-buffer context (* (length test-indices) 2) :name "Test index buffer")))
        ;; Upload test data
        (let* ((vertex-dma-buf (virgl:resource-dma-buffer vertex-buffer))
               (vertex-buf (make-array (sup:dma-buffer-length vertex-dma-buf)
                                       :element-type '(unsigned-byte 8)
                                       :fill-pointer 0
                                       :memory vertex-dma-buf)))
          (dolist (val test-data)
            (virgl::vector-push-extend-single/le val vertex-buf))
          (virgl:transfer-to-gpu vertex-buffer))
        (let* ((index-dma-buf (virgl:resource-dma-buffer index-buffer))
               (index-buf (make-array (sup:dma-buffer-length index-dma-buf)
                                       :element-type '(unsigned-byte 8)
                                       :fill-pointer 0
                                       :memory index-dma-buf)))
          (dolist (val test-indices)
            (virgl::vector-push-extend-ub16/le val index-buf))
          (virgl:transfer-to-gpu index-buffer))
        (let* ((vertex-shader (virgl:make-shader context
                                                 :vertex
                                                 '((tgsi:dcl (:in 0))
                                                   (tgsi:dcl (:in 1))
                                                   (tgsi:dcl (:out 0) :position)
                                                   (tgsi:dcl (:out 1) :color)
                                                   (tgsi:dcl (:const 0))
                                                   (tgsi:dcl (:temp 0))
                                                   (tgsi:imm :flt32 (0.0 0.0 0.0 1.0))
                                                   ;; Apply the 2D transform matrix (const 0) to the vertex position (input 0).
                                                   (tgsi:mov (:out 0) (:imm 0)) ; Clear the output bits, get .w set properly
                                                   (tgsi:mul (:temp 0) (:const 0) (:in 0 :xyxy))
                                                   (tgsi:add (:out 0 :xy) (:temp 0 :xzxx) (:temp 0 :ywxx))
                                                   ;; Pass colour (input 1) through unchanged.
                                                   (tgsi:mov (:out 1) (:in 1))
                                                   (tgsi:end))))
               (fragment-shader (virgl:make-shader context
                                                   :fragment
                                                   ;; Color semantic and color interpolation
                                                   '((tgsi:dcl (:in 0) :color :color)
                                                     (tgsi:dcl (:out 0) :color)
                                                     (tgsi:imm :flt32 (1.0 1.0 1.0 1.0))
                                                     ;; Invert the colour channel.
                                                     1 (tgsi:sub (:out 0) (:imm 0) (:in 0))
                                                     2 (tgsi:end))))
               ;; Create a surface object backed by the scanout buffer
               (scanout (virgl:virgl-scanout (virgl:virgl context)))
               (scanout-surface (virgl:make-surface context scanout))
               ;; Create vertex elements buffer.
               (vertex-elements (virgl:make-vertex-elements
                                 context nil
                                 ;; First element: Positions.
                                 '(0 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)
                                 ;; Second element: Colours.
                                 '(16 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)))
               (blend (virgl:make-blend context :colormask :rgba))
               (setup-cmd-buf (virgl:make-command-buffer context)))
          ;; Attach scanout surface object to the framebuffer's color0 channel.
          ;; No depth/stencil surface.
          (virgl:add-command-set-framebuffer-state setup-cmd-buf nil scanout-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           setup-cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5
           (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5)
          ;; Configure vertex buffer
          (virgl:add-command-set-vertex-buffers
           setup-cmd-buf
           `(32 ; stride, 2*float4
             0 ; offset
             ,vertex-buffer)) ; vertex buffer resource handle
          (virgl:add-command-bind-vertex-elements setup-cmd-buf vertex-elements)
          (virgl:add-command-bind-shader setup-cmd-buf vertex-shader)
          (virgl:add-command-bind-shader setup-cmd-buf fragment-shader)
          ;; Enable writes to the color channels
          (virgl:add-command-bind-blend setup-cmd-buf blend)
          (virgl:add-command-set-index-buffer setup-cmd-buf index-buffer 2 0)
          (virgl:command-buffer-finalize setup-cmd-buf)
          (virgl:command-buffer-submit setup-cmd-buf)
          (loop
             with cmd-buf = (virgl:make-command-buffer context)
             with n = 1000
             for i below n
             ;with i = 0
             do
               (virgl:command-buffer-reset cmd-buf)
             ;; Clear framebuffer.
               (virgl:add-command-clear
                cmd-buf :color 0.25 0.33 0.66 0.75 0.0d0 0)
             ;; Set constants
             ;; This is a matrix that looks like:
             ;; [cos i, -sin i,
             ;;  sin i,  cos i]
               (let ((s (sin (/ i 100.0))) (c (cos (/ i 100.0))))
                 (virgl:add-command-set-constant-buffer
                  cmd-buf :vertex
                  c (- s) s c))
             ;; Draw
               (virgl:add-command-draw-vbo cmd-buf 0 6 :triangles :indexed t)
             ;; Do it!
               (virgl:command-buffer-finalize cmd-buf)
               (virgl:command-buffer-submit cmd-buf)
               (virgl:scanout-flush scanout 0 0 (virgl:width scanout) (virgl:height scanout))
               (sleep 1/60)))))))

(defun degrees-to-radians (degrees)
  (* degrees (/ (float pi 0.0f0) 180.0)))

(defun radians-to-degrees (radians)
  (* radians (/ 180.0 (float pi 0.0f0))))

(defun make-matrix (a b c d e f g h i j k l m n o p)
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
    elts))

(defun print-matrix (matrix)
  (format t "┌ ~S ~15T ~S ~30T ~S ~45T ~S ┐~%"
          (aref matrix 0) (aref matrix 4) (aref matrix 8) (aref matrix 12))
  (format t "│ ~S ~15T ~S ~30T ~S ~45T ~S │~%"
          (aref matrix 1) (aref matrix 5) (aref matrix 9) (aref matrix 13))
  (format t "│ ~S ~15T ~S ~30T ~S ~45T ~S │~%"
          (aref matrix 2) (aref matrix 6) (aref matrix 10) (aref matrix 14))
  (format t "└ ~S ~15T ~S ~30T ~S ~45T ~S ┘~%"
          (aref matrix 3) (aref matrix 7) (aref matrix 11) (aref matrix 15)))

(defun make-identity-matrix ()
  (make-matrix
   1.0 0.0 0.0 0.0
   0.0 1.0 0.0 0.0
   0.0 0.0 1.0 0.0
   0.0 0.0 0.0 1.0))

(defun make-scale-matrix (x y z)
  (make-matrix
   x   0.0 0.0 0.0
   0.0 y   0.0 0.0
   0.0 0.0 z   0.0
   0.0 0.0 0.0 1.0))

(defun make-translate-matrix (x y z)
  (make-matrix
   1.0 0.0 0.0 x
   0.0 1.0 0.0 y
   0.0 0.0 1.0 z
   0.0 0.0 0.0 1.0))

(defun make-rotate-matrix (x y z angle)
  (let* ((s (sin angle)) (c (cos angle)) (c* (- 1.0 c))
         (l (max (sqrt (+ (* x x) (* y y) (* z z))) single-float-epsilon))
         (nx (/ x l)) (ny (/ y l)) (nz (/ z l)))
    (make-matrix
     (+ (* nx nx c*) c)        (- (* nx ny c*) (* nz s)) (+ (* nx nz c*) (* ny s)) 0.0
     (+ (* ny nx c*) (* nz s)) (+ (* ny ny c*) c)        (- (* ny nz c*) (* nx s)) 0.0
     (- (* nz nx c*) (* ny s)) (+ (* nz ny c*) (* nx s)) (+ (* nz nz c*) c)        0.0
     0.0                       0.0                       0.0                       1.0)))

(defun make-rotate-x-matrix (angle)
  (let* ((s (sin angle)) (c (cos angle)))
    (make-matrix
     1.0   0.0   0.0   0.0
     0.0   c     (- s) 0.0
     0.0   s     c     0.0
     0.0   0.0   0.0   1.0)))

(defun make-rotate-y-matrix (angle)
  (let* ((s (sin angle)) (c (cos angle)))
    (make-matrix
     c     0.0   s     0.0
     0.0   1.0   0.0   0.0
     (- s) 0.0   c     0.0
     0.0   0.0   0.0   1.0)))

(defun make-rotate-z-matrix (angle)
  (let* ((s (sin angle)) (c (cos angle)))
    (make-matrix
     c     (- s) 0.0   0.0
     s     c     0.0   0.0
     0.0   0.0   1.0   0.0
     0.0   0.0   0.0   1.0)))

(defun make-perspective-matrix (fov aspect near far)
  (let ((tan-half-fov (tan (/ fov 2.0))))
    (make-matrix
     (/ (* aspect tan-half-fov)) 0.0              0.0                               0.0
     0.0                         (/ tan-half-fov) 0.0                               0.0
     0.0                         0.0              (- (/ (+ far near) (- far near))) (- (/ (* 2 far near) (- far near)))
     0.0                         0.0              -1.0                              1.0)))

(defun matrix-multiply (a b)
  (let ((result (make-array 16 :element-type 'single-float)))
    (gui::matrix4-multiply result a b)
    result))

(defun test-cube ()
  "Draw a cube."
  (virgl:with-context (context :name "Test context")
    ;; A cube is made of 8 vertices, formed into 6 faces, made of 2 tris each.
    (let ((test-data '(+0.5 +0.5 +0.5 1.0   ; 0 Position RUF (right upper front)
                        1.0  0.0  0.0 1.0   ;   Colour
                       +0.5 +0.5 -0.5 1.0   ; 1 Position RUB (right upper back)
                        0.0  1.0  0.0 1.0   ;   Colour
                       +0.5 -0.5 +0.5 1.0   ; 2 Position RDF (right down front)
                        0.0  0.0  1.0 1.0   ;   Colour
                       +0.5 -0.5 -0.5 1.0   ; 3 Position RDB (right down back)
                        1.0  1.0  0.0 1.0   ;   Colour
                       -0.5 +0.5 +0.5 1.0   ; 4 Position LUF (left upper front)
                        1.0  0.0  1.0 1.0   ;   Colour
                       -0.5 +0.5 -0.5 1.0   ; 5 Position LUB (left upper back)
                        0.0  1.0  1.0 1.0   ;   Colour
                       -0.5 -0.5 +0.5 1.0   ; 6 Position LDF (left down front)
                        0.5  0.5  0.0 1.0   ;   Colour
                       -0.5 -0.5 -0.5 1.0   ; 7 Position LDB (left down back)
                        0.5  0.0  0.5 1.0)) ;   Colour
          (test-indices '(6 2 0 0 4 6    ; Front face
                          7 5 1 1 3 7    ; Back face
                          3 1 0 0 2 3    ; Right face
                          7 6 4 4 5 7    ; Left face
                          5 4 0 0 1 5    ; Up face
                          7 3 2 2 6 7))) ; Down face
      (virgl:with-resources ((vertex-buffer (virgl:make-vertex-buffer context (* (length test-data) 4) :name "Test vertex buffer"))
                             (index-buffer (virgl:make-index-buffer context (* (length test-indices) 2) :name "Test index buffer")))
        ;; Upload test data
        (let* ((vertex-dma-buf (virgl:resource-dma-buffer vertex-buffer))
               (vertex-buf (make-array (sup:dma-buffer-length vertex-dma-buf)
                                       :element-type '(unsigned-byte 8)
                                       :fill-pointer 0
                                       :memory vertex-dma-buf)))
          (dolist (val test-data)
            (virgl::vector-push-extend-single/le val vertex-buf))
          (virgl:transfer-to-gpu vertex-buffer))
        (let* ((index-dma-buf (virgl:resource-dma-buffer index-buffer))
               (index-buf (make-array (sup:dma-buffer-length index-dma-buf)
                                       :element-type '(unsigned-byte 8)
                                       :fill-pointer 0
                                       :memory index-dma-buf)))
          (dolist (val test-indices)
            (virgl::vector-push-extend-ub16/le val index-buf))
          (virgl:transfer-to-gpu index-buffer))
        (let* ((vertex-shader (virgl:make-shader context
                                                 :vertex
                                                 '((tgsi:dcl (:in 0))
                                                   (tgsi:dcl (:in 1))
                                                   (tgsi:dcl (:out 0) :position)
                                                   (tgsi:dcl (:out 1) :color)
                                                   (tgsi:dcl (:const 0 3))
                                                   (tgsi:dcl (:temp 0 3))
                                                   ;; Apply the 4D transform matrix (const 0..3) to the vertex position (input 0).
                                                   (tgsi:mul (:temp 0) (:const 0) (:in 0 :xxxx))
                                                   (tgsi:mul (:temp 1) (:const 1) (:in 0 :yyyy))
                                                   (tgsi:mul (:temp 2) (:const 2) (:in 0 :zzzz))
                                                   (tgsi:mul (:temp 3) (:const 3) (:in 0 :wwww))
                                                   (tgsi:add (:out 0) (:temp 0) (:temp 1))
                                                   (tgsi:add (:out 0) (:out 0) (:temp 2))
                                                   (tgsi:add (:out 0) (:out 0) (:temp 3))
                                                   ;; Pass colour (input 1) through unchanged.
                                                   (tgsi:mov (:out 1) (:in 1))
                                                   (tgsi:end))))
               (fragment-shader (virgl:make-shader context
                                                   :fragment
                                                   ;; Color semantic and color interpolation
                                                   '((tgsi:dcl (:in 0) :color :color)
                                                     (tgsi:dcl (:out 0) :color)
                                                     (tgsi:imm :flt32 (1.0 1.0 1.0 1.0))
                                                     ;; Invert the colour channel.
                                                     1 (tgsi:sub (:out 0) (:imm 0) (:in 0))
                                                     2 (tgsi:end))))
               ;; Create a surface object backed by the scanout buffer
               (scanout (virgl:virgl-scanout (virgl:virgl context)))
               (scanout-surface (virgl:make-surface context scanout))
               ;; Create vertex elements buffer.
               (vertex-elements (virgl:make-vertex-elements
                                 context nil
                                 ;; First element: Positions.
                                 '(0 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)
                                 ;; Second element: Colours.
                                 '(16 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)))
               (blend (virgl:make-blend context :colormask :rgba))
               (rasterizer (virgl:make-rasterizer
                            context
                            :front-ccw t
                            :cull-face :back))
               (setup-cmd-buf (virgl:make-command-buffer context)))
          ;; Attach scanout surface object to the framebuffer's color0 channel.
          ;; No depth/stencil surface.
          (virgl:add-command-set-framebuffer-state setup-cmd-buf nil scanout-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           setup-cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5
           (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5)
          ;; Configure vertex buffer
          (virgl:add-command-set-vertex-buffers
           setup-cmd-buf
           `(32 ; stride, 2*float4
             0 ; offset
             ,vertex-buffer)) ; vertex buffer resource handle
          (virgl:add-command-bind-vertex-elements setup-cmd-buf vertex-elements)
          (virgl:add-command-bind-shader setup-cmd-buf vertex-shader)
          (virgl:add-command-bind-shader setup-cmd-buf fragment-shader)
          ;; Enable writes to the color channels
          (virgl:add-command-bind-blend setup-cmd-buf blend)
          (virgl:add-command-bind-rasterizer setup-cmd-buf rasterizer)
          (virgl:add-command-set-index-buffer setup-cmd-buf index-buffer 2 0)
          (virgl:command-buffer-finalize setup-cmd-buf)
          (virgl:command-buffer-submit setup-cmd-buf)
          (loop
             with cmd-buf = (virgl:make-command-buffer context)
             with perspective = (make-perspective-matrix
                                 (degrees-to-radians 90)
                                 (float (/ (virgl:width scanout) (virgl:height scanout)))
                                 0.01 100.0)
             with n = 1000
             for i below n
             ;with i = 0
             do
               (virgl:command-buffer-reset cmd-buf)
             ;; Clear framebuffer.
               (virgl:add-command-clear
                cmd-buf :color 0.25 0.33 0.66 0.75 0.0d0 0)
             ;; Set constants.
             ;; An identity matrix. Watch out - this is transposed!
               (virgl:add-command-set-constant-buffer
                cmd-buf :vertex
                (matrix-multiply
                 perspective
                 (matrix-multiply
                  (make-translate-matrix 0.0 0.0 -1.0)
                  (make-rotate-matrix 1.5 0.5 1.0 (degrees-to-radians i)))))
             ;; Draw
               (virgl:add-command-draw-vbo cmd-buf 0 (length test-indices)#+(or)(/ (length test-indices) 3) :triangles :indexed t)
             ;; Do it!
               (virgl:command-buffer-finalize cmd-buf)
               (virgl:command-buffer-submit cmd-buf)
               (virgl:scanout-flush scanout 0 0 (virgl:width scanout) (virgl:height scanout))
               (sleep 1/60)))))))
