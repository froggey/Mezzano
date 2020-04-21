;;;; Test rendering.

(in-package :mezzano.gui.virgl.test)

(defvar *test-context* (virgl:make-context :name "Test context"))
;; Push vertex data to GPU.
(defvar *test-vertex-buffer* (virgl:make-vertex-buffer *test-context* 96 :name "Test vertex buffer"))

(defun upload-test-data ()
  (let* ((dma-buf (virgl:resource-dma-buffer *test-vertex-buffer*))
         (vertex-buf (make-array (sup:dma-buffer-length dma-buf)
                                 :element-type '(unsigned-byte 8)
                                 :fill-pointer 0
                                 :memory dma-buf)))
    ;; Position 0
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    ;; Colour 0
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    ;; Position 1
    (virgl::vector-push-extend-single/le 0.5 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    ;; Colour 1
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    ;; Position 2
    (virgl::vector-push-extend-single/le 0.5 vertex-buf)
    (virgl::vector-push-extend-single/le 0.5 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    ;; Colour 2
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 0.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    (virgl::vector-push-extend-single/le 1.0 vertex-buf)
    ;; Upload the data.
    (virgl:transfer-to-gpu *test-vertex-buffer*)))

(defun test ()
  (upload-test-data)
  (let* ((vertex-shader (virgl:make-shader *test-context*
                                           :vertex
                                           '((tgsi:dcl (:in 0))
                                             (tgsi:dcl (:in 1))
                                             (tgsi:dcl (:out 0) :position)
                                             (tgsi:dcl (:out 1) :color)
                                             ;; Pass vertex position (input 0) and colour (input 1) through unchanged.
                                             1 (tgsi:mov (:out 0) (:in 0))
                                             2 (tgsi:mov (:out 1) (:in 1))
                                             3 (tgsi:end))))

         (fragment-shader (virgl:make-shader *test-context*
                                             :fragment
                                             ;; Color semantic and color interpolation
                                             '((tgsi:dcl (:in 0) :color :color)
                                               (tgsi:dcl (:out 0) :color)
                                               (tgsi:imm :flt32 (1.0 1.0 1.0 1.0))
                                               ;; Invert the colour channel.
                                               1 (tgsi:sub (:out 0) (:imm 0) (:in 0))
                                               2 (tgsi:end))))
         ;; Create a surface object backed by the scanout buffer
         (scanout (virgl:virgl-scanout (virgl:virgl *test-context*)))
         (scanout-surface (virgl:make-surface *test-context* scanout))
         ;; Create vertex elements buffer.
         (vertex-elements (virgl:make-vertex-elements
                           *test-context* nil
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
         (blend (virgl:make-blend *test-context* :colormask :rgba))
         (cmd-buf (virgl:make-command-buffer *test-context*)))
    ;; Attach scanout surface object to the framebuffer's color0 channel.
    ;; No depth/stencil surface.
    (virgl:add-command-set-framebuffer-state cmd-buf nil scanout-surface)
    ;; Viewport.
    (virgl:add-command-set-viewport-state
     cmd-buf
     ;; near-depth = 0, far-depth = 1
     (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5
     (/ (virgl:width scanout) 2.0) (/ (virgl:height scanout) 2.0) 0.5)
    ;; Testing clear.
    (virgl:add-command-clear
     cmd-buf :color 0.25 0.33 0.66 0.75 0.0d0 0)
    ;; Configure vertex buffer
    (virgl:add-command-set-vertex-buffers
     cmd-buf
     `(32 ; stride, 2*float4
       0 ; offset
       ,*test-vertex-buffer*)) ; vertex buffer resource handle
    (virgl:add-command-bind-vertex-elements cmd-buf vertex-elements)
    (virgl:add-command-bind-shader cmd-buf vertex-shader)
    (virgl:add-command-bind-shader cmd-buf fragment-shader)
    ;; Enable writes to the color channels
    (virgl:add-command-bind-blend cmd-buf blend)
    (virgl:add-command-draw-vbo cmd-buf 0 3 :triangles)
    (virgl:command-buffer-finalize cmd-buf)
    (virgl:command-buffer-submit cmd-buf)
    (virgl:scanout-flush scanout 0 0 (virgl:width scanout) (virgl:height scanout))
    ))
