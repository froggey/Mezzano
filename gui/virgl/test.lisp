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
                                                   ;; Apply the transform matrix (const 0) to the vertex position (input 0).
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
             ;; Testing clear.
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
