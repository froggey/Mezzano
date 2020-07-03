;;;; Test rendering.

(defpackage :mezzano.gui.virgl.test
  (:use :cl)
  (:local-nicknames (:gui :mezzano.gui)
                    (:comp :mezzano.gui.compositor)
                    (:widgets :mezzano.gui.widgets)
                    (:sync :mezzano.sync)
                    (:virgl :mezzano.gui.virgl)
                    (:tgsi :mezzano.gui.virgl.tgsi)
                    (:gpu :mezzano.supervisor.virtio-gpu)
                    (:ext :mezzano.extensions)
                    (:sup :mezzano.supervisor))
  (:export #:test-clear
           #:test-triangle
           #:test-spin
           #:test-cube
           #:test-texture
           #:demo))

(in-package :mezzano.gui.virgl.test)

;;; Demo framework.
(defmacro with-demo-gui ((context window-texture &key title (width 800) (height 600)) &body body)
  (let ((width-sym (gensym "WIDTH"))
        (height-sym (gensym "HEIGHT"))
        (window (gensym "WINDOW"))
        (mailbox (gensym "MAILBOX"))
        (frame (gensym "FRAME"))
        (frame-left (gensym "FRAME-LEFT"))
        (frame-right (gensym "FRAME-RIGHT"))
        (frame-top (gensym "FRAME-TOP"))
        (frame-bottom (gensym "FRAME-BOTTOM")))
    `(let ((,mailbox (sync:make-mailbox :capacity 50))
           (,width-sym ,width)
           (,height-sym ,height))
       (multiple-value-bind (,frame-left ,frame-right ,frame-top ,frame-bottom)
           (widgets:frame-size (make-instance 'widgets:frame))
         (comp:with-window (,window ,mailbox (+ ,width-sym ,frame-left ,frame-right) (+ ,height-sym ,frame-top ,frame-bottom) :name "Virgl demo")
           (let ((,frame (make-instance 'widgets:frame
                                        :framebuffer (comp:window-buffer ,window)
                                        :title ',title
                                        :close-button-p t
                                        :damage-function (widgets:default-damage-function ,window)
                                        :set-cursor-function (widgets:default-cursor-function ,window))))
             (widgets:draw-frame ,frame)
             (virgl:with-context (,context :name ',title)
               (virgl:with-resources ((,window-texture (virgl:make-texture ,context
                                                                           :b8g8r8a8-unorm
                                                                           (list ,width-sym ,height-sym)
                                                                           :name "Window display surface"
                                                                           :render-target t)))
                 (macrolet ((window-loop (&body body)
                              `(loop
                                  (progn ,@body)
                                  (window-loop-process-events ,',mailbox ,',frame)
                                  ;; Update the window surface.
                                  (virgl:transfer-from-gpu ,',window-texture)
                                  (virgl:copy-texture-2d-to-gui-surface
                                   ,',width-sym ,',height-sym
                                   ,',window-texture 0 0
                                   (comp:window-buffer ,',window) ,',frame-left ,',frame-top)
                                  (comp:damage-window ,',window ,',frame-left ,',frame-top ,',width-sym ,',height-sym)
                                  (sleep 1/60))))
                   (catch 'quit
                     ,@body))))))))))

(defmacro window-loop (&body body)
  (declare (ignore body))
  (error "WINDOW-LOOP must only be used within WITH-DEMO-GUI"))

(defun window-loop-process-events (mailbox frame)
  (loop
     (let ((evt (sync:mailbox-receive mailbox :wait-p nil)))
       (when (not evt) (return))
       (typecase evt
         ((or comp:quit-event comp:window-close-event)
          (throw 'quit nil))
         (comp:window-activation-event
          (setf (widgets:activep frame) (comp:state evt))
          (widgets:draw-frame frame))
         (comp:mouse-event
          (handler-case
              (widgets:frame-mouse-event frame evt)
            (widgets:close-button-clicked ()
              (throw 'quit nil))))))))

;;; The tests.

(defun test-clear (&optional (red 0.25) (green 0.33) (blue 0.66))
  "Clear the window."
  (with-demo-gui (context window-texture :title "Clear test")
    ;; Create a surface object backed by the window texture.
    (let* ((window-surface (virgl:make-surface context window-texture))
           (cmd-buf (virgl:make-command-buffer context)))
      ;; Attach scanout surface object to the framebuffer's color0 channel.
      ;; No depth/stencil surface.
      (virgl:add-command-set-framebuffer-state cmd-buf nil window-surface)
      ;; Clear.
      (virgl:add-command-clear
       cmd-buf :color red green blue 1.0 0.0d0 0)
      (virgl:command-buffer-finalize cmd-buf)
      (virgl:command-buffer-submit cmd-buf)
      (window-loop))))

(defun test-triangle ()
  "Draw a simple triangle."
  (with-demo-gui (context window-texture :title "Triangle test")
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
                                                     ;; Invert the colour channel, leave alpha alone.
                                                     1 (tgsi:mov (:out 0) (:in 0))
                                                     2 (tgsi:sub (:out 0 :xyz) (:imm 0) (:out 0))
                                                     3 (tgsi:end))))
               ;; Create a surface object backed by the window texture.
               (window-surface (virgl:make-surface context window-texture))
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
          (virgl:add-command-set-framebuffer-state cmd-buf nil window-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5)
          ;; Clear.
          (virgl:add-command-clear
           cmd-buf :color 0.25 0.33 0.66 1.0 0.0d0 0)
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
          (virgl:add-command-draw-vbo cmd-buf 0 (length test-data) :triangles)
          (virgl:command-buffer-finalize cmd-buf)
          (virgl:command-buffer-submit cmd-buf)
          (window-loop))))))

(defun test-spin ()
  "Draw a spinning square."
  (with-demo-gui (context window-texture :title "Uniform test")
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
                                                     ;; Invert the colour channel, leave alpha alone.
                                                     1 (tgsi:mov (:out 0) (:in 0))
                                                     2 (tgsi:sub (:out 0 :xyz) (:imm 0) (:out 0))
                                                     3 (tgsi:end))))
               ;; Create a surface object backed by the window texture.r
               (window-surface (virgl:make-surface context window-texture))
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
          ;; Attach window surface object to the framebuffer's color0 channel.
          ;; No depth/stencil surface.
          (virgl:add-command-set-framebuffer-state setup-cmd-buf nil window-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           setup-cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5)
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
          (let ((cmd-buf (virgl:make-command-buffer context))
                (i 0))
            (window-loop
               (virgl:command-buffer-reset cmd-buf)
             ;; Clear framebuffer.
               (virgl:add-command-clear
                cmd-buf :color 0.25 0.33 0.66 1.0 0.0d0 0)
             ;; Set constants
             ;; This is a matrix that looks like:
             ;; [cos i, -sin i,
             ;;  sin i,  cos i]
               (let ((s (sin (/ i 100.0))) (c (cos (/ i 100.0))))
                 (virgl:add-command-set-constant-buffer
                  cmd-buf :vertex
                  c (- s) s c))
             ;; Draw
               (virgl:add-command-draw-vbo cmd-buf 0 (length test-indices) :triangles :indexed t)
             ;; Do it!
               (virgl:command-buffer-finalize cmd-buf)
               (virgl:command-buffer-submit cmd-buf)
               (incf i))))))))

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
  "Draw a cube, with a depth buffer and backface culling."
  (with-demo-gui (context window-texture :title "Test cube")
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
                             (index-buffer (virgl:make-index-buffer context (* (length test-indices) 2) :name "Test index buffer"))
                             (depth-texture (virgl:make-texture context
                                                                :s8-uint-z24-unorm
                                                                (list (virgl:width window-texture) (virgl:height window-texture))
                                                                :name "Test depth/stencil texture"
                                                                :depth/stencil t
                                                                :host-only t)))
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
                                                     ;; Invert the colour channel, leave alpha alone.
                                                     1 (tgsi:mov (:out 0) (:in 0))
                                                     2 (tgsi:sub (:out 0 :xyz) (:imm 0) (:out 0))
                                                     3 (tgsi:end))))
               ;; Create a surface object backed by the window texture.
               (window-surface (virgl:make-surface context window-texture))
               ;; And the depth texture
               (depth-surface (virgl:make-surface context depth-texture))
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
                            :front-ccw t ; match opengl
                            :cull-face :back))
               (dsa (virgl:make-dsa
                     context
                     :depth-enabled t
                     :depth-writemask t
                     :depth-func :less))
               (setup-cmd-buf (virgl:make-command-buffer context)))
          ;; Attach window surface object to the framebuffer's color0 channel.
          (virgl:add-command-set-framebuffer-state setup-cmd-buf depth-surface window-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           setup-cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5)
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
          (virgl:add-command-bind-dsa setup-cmd-buf dsa)
          (virgl:add-command-set-index-buffer setup-cmd-buf index-buffer 2 0)
          (virgl:command-buffer-finalize setup-cmd-buf)
          (virgl:command-buffer-submit setup-cmd-buf)
          (let ((cmd-buf (virgl:make-command-buffer context))
                (perspective (make-perspective-matrix
                              (degrees-to-radians 90)
                              (float (/ (virgl:width window-texture) (virgl:height window-texture)))
                              0.01 100.0))
                (i 0))
            (window-loop
              (virgl:command-buffer-reset cmd-buf)
              ;; Clear framebuffer.
              (virgl:add-command-clear
               cmd-buf '(:color :depth) 0.25 0.33 0.66 1.0 1.0d0 0)
              ;; Set constants.
              (virgl:add-command-set-constant-buffer
               cmd-buf :vertex
               (matrix-multiply
                perspective
                (matrix-multiply
                 (make-translate-matrix 0.0 0.0 -1.0)
                 (make-rotate-matrix 1.5 0.5 1.0 (degrees-to-radians i)))))
              ;; Draw
              (virgl:add-command-draw-vbo cmd-buf 0 (length test-indices) :triangles :indexed t)
              ;; Do it!
              (virgl:command-buffer-finalize cmd-buf)
              (virgl:command-buffer-submit cmd-buf)
              (incf i))))))))

(defun load-test-image (name)
  (mezzano.gui.image:load-image
   (merge-pathnames name
                    (translate-logical-pathname
                     (or *load-pathname*
                         *default-pathname-defaults*)))))

(defvar *alien-logo* (load-test-image "lisplogo_alien_256.png"))
(defvar *made-with-lisp* (load-test-image "made-with-lisp_256.png"))

(defun test-texture ()
  "Draw a textured square."
  (with-demo-gui (context window-texture :title "Test texture")
    (let ((test-data '(0.0 0.0 0.0 1.0   ; 0 Position UR
                       0.0 1.0 0.0 0.0   ;   Texcoord
                       0.5 0.0 0.0 1.0   ; 1 Position DR
                       1.0 1.0 0.0 0.0   ;   Texcoord
                       0.5 0.5 0.0 1.0   ; 2 Position DL
                       1.0 0.0 0.0 0.0   ;   Texcoord
                       0.0 0.5 0.0 1.0   ; 3 Position UL
                       0.0 0.0 0.0 0.0)) ;   Texcoord
          (test-indices '(2 1 0 0 3 2)))
      (virgl:with-resources ((vertex-buffer (virgl:make-vertex-buffer context (* (length test-data) 4) :name "Test vertex buffer"))
                             (index-buffer (virgl:make-index-buffer context (* (length test-indices) 2) :name "Test index buffer"))
                             (texture (virgl:make-texture-2d-from-gui-surface context *alien-logo* :name "Test texture")))
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
                                                 '((tgsi:dcl (:in 0)) ; vertex position
                                                   (tgsi:dcl (:in 1)) ; vertex texcoord
                                                   (tgsi:dcl (:out 0) :position)
                                                   (tgsi:dcl (:out 1) :texcoord)
                                                   ;; Pass inputs through unchanged.
                                                   (tgsi:mov (:out 0) (:in 0))
                                                   (tgsi:mov (:out 1) (:in 1))
                                                   (tgsi:end))))
               (fragment-shader (virgl:make-shader context
                                                   :fragment
                                                   '((tgsi:dcl (:in 1) :texcoord) ; index must match the index in the vertex shader.
                                                     (tgsi:dcl (:out 0) :color)
                                                     (tgsi:dcl (:samp 0))
                                                     (tgsi:dcl (:temp 0))
                                                     (tgsi:imm :flt32 (0.0 0.0 0.0 0.5)) ; 0 alpha threshold
                                                     (tgsi:tex (:out 0) (:in 1) (:samp 0) :2d)
                                                     ;; Alpha test.
                                                     (tgsi:sub (:temp 0 :w) (:out 0) (:imm 0))
                                                     (tgsi:kill-if (:temp 0 :wwww))
                                                     (tgsi:end))))
               ;; Needs to have both a view and a state bound.
               (sampler-view (virgl:make-sampler-view context texture))
               (sampler-state (virgl:make-sampler-state context))
               ;; Create a surface object backed by the window texture.
               (window-surface (virgl:make-surface context window-texture))
               ;; Create vertex elements buffer.
               (vertex-elements (virgl:make-vertex-elements
                                 context nil
                                 ;; First element: Positions.
                                 '(0 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)
                                 ;; Second element: Texcoord.
                                 '(16 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)))
               (blend (virgl:make-blend context
                                        :colormask :rgba

                                        ))
               (cmd-buf (virgl:make-command-buffer context)))
          ;; Attach window surface object to the framebuffer's color0 channel.
          ;; No depth/stencil surface.
          (virgl:add-command-set-framebuffer-state cmd-buf nil window-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5)
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
          (virgl:add-command-set-index-buffer cmd-buf index-buffer 2 0)
          (virgl:add-command-bind-sampler-states cmd-buf :fragment 0 sampler-state)
          (virgl:add-command-set-sampler-views cmd-buf :fragment 0 sampler-view)
          ;; Clear framebuffer.
          (virgl:add-command-clear
           cmd-buf :color 0.25 0.33 0.66 1.0 0.0d0 0)
          ;; Draw
          (virgl:add-command-draw-vbo cmd-buf 0 (length test-indices) :triangles :indexed t)
          ;; Do it!
          (virgl:command-buffer-finalize cmd-buf)
          (virgl:command-buffer-submit cmd-buf)
          (window-loop))))))

(defun demo ()
  "A fun demo!"
  (with-demo-gui (context window-texture :title "Demo")
    ;; A cube is made of 8 vertices, formed into 6 faces, made of 2 tris each.
    (let ((test-data '(+0.5 +0.5 +0.5 1.0   ; 0 Position RUF (right upper front)
                        1.0  0.0  0.0 1.0   ;   Colour
                        1.0  0.0  0.0 0.0   ;   Texcoord
                       +0.5 +0.5 -0.5 1.0   ; 1 Position RUB (right upper back)
                        1.0  0.0  0.0 1.0   ;   Colour
                        1.0  1.0  0.0 0.0   ;   Texcoord
                       +0.5 -0.5 +0.5 1.0   ; 2 Position RDF (right down front)
                        0.0  0.0  1.0 1.0   ;   Colour
                        1.0  1.0  0.0 0.0   ;   Texcoord
                       +0.5 -0.5 -0.5 1.0   ; 3 Position RDB (right down back)
                        1.0  1.0  0.0 1.0   ;   Colour
                        1.0  0.0  0.0 0.0   ;   Texcoord
                       -0.5 +0.5 +0.5 1.0   ; 4 Position LUF (left upper front)
                        1.0  0.0  1.0 1.0   ;   Colour
                        0.0  0.0  0.0 0.0   ;   Texcoord
                       -0.5 +0.5 -0.5 1.0   ; 5 Position LUB (left upper back)
                        0.0  1.0  1.0 1.0   ;   Colour
                        0.0  1.0  0.0 0.0   ;   Texcoord
                       -0.5 -0.5 +0.5 1.0   ; 6 Position LDF (left down front)
                        0.5  0.5  0.0 1.0   ;   Colour
                        0.0  1.0  0.0 0.0   ;   Texcoord
                       -0.5 -0.5 -0.5 1.0   ; 7 Position LDB (left down back)
                        0.5  0.0  0.5 1.0   ;   Colour
                        0.0  0.0  0.0 0.0   ;   Texcoord
                       ;; Side coordinates - different texcoords.
                       +0.5 +0.5 +0.5 1.0   ; 8 Position RUF (right upper front)
                        1.0  0.0  0.0 1.0   ;   Colour
                        0.0  0.0  0.0 0.0   ;   Texcoord
                       +0.5 +0.5 -0.5 1.0   ; 9 Position RUB (right upper back)
                        1.0  0.0  0.0 1.0   ;   Colour
                        0.0  1.0  0.0 0.0   ;   Texcoord
                       +0.5 -0.5 +0.5 1.0   ;10 Position RDF (right down front)
                        0.0  0.0  1.0 1.0   ;   Colour
                        1.0  0.0  0.0 0.0   ;   Texcoord
                       +0.5 -0.5 -0.5 1.0   ;11 Position RDB (right down back)
                        1.0  1.0  0.0 1.0   ;   Colour
                        1.0  1.0  0.0 0.0   ;   Texcoord
                       -0.5 +0.5 +0.5 1.0   ;12 Position LUF (left upper front)
                        1.0  0.0  1.0 1.0   ;   Colour
                        1.0  0.0  0.0 0.0   ;   Texcoord
                       -0.5 +0.5 -0.5 1.0   ;13 Position LUB (left upper back)
                        0.0  1.0  1.0 1.0   ;   Colour
                        0.0  0.0  0.0 0.0   ;   Texcoord
                       -0.5 -0.5 +0.5 1.0   ;14 Position LDF (left down front)
                        0.5  0.5  0.0 1.0   ;   Colour
                        1.0  1.0  0.0 0.0   ;   Texcoord
                       -0.5 -0.5 -0.5 1.0   ;15 Position LDB (left down back)
                        0.5  0.0  0.5 1.0   ;   Colour
                        0.0  1.0  0.0 0.0)) ;   Texcoord
          (test-indices '( 6  2  0  0  4  6   ; Cube front face
                           7  5  1  1  3  7   ; Cube back face
                          11  9  8  8 10 11   ; Cube right face
                          15 14 12 12 13 15   ; Cube left face
                           5  4  0  0  1  5   ; Cube up face
                           7  3  2  2  6  7   ; Cube down face
                           6 2 0 0 4 6))) ; Background lines.
      (virgl:with-resources ((vertex-buffer (virgl:make-vertex-buffer context (* (length test-data) 4) :name "Test vertex buffer"))
                             (index-buffer (virgl:make-index-buffer context (* (length test-indices) 2) :name "Test index buffer"))
                             (depth-texture (virgl:make-texture context
                                                                :s8-uint-z24-unorm
                                                                (list (virgl:width window-texture) (virgl:height window-texture))
                                                                :name "Test depth/stencil texture"
                                                                :depth/stencil t
                                                                ;; Host only as it is never directly read from.
                                                                :host-only t))
                             (alien-texture (virgl:make-texture-2d-from-gui-surface context *alien-logo* :name "Lisp Alien"))
                             (mwl-texture (virgl:make-texture-2d-from-gui-surface context *made-with-lisp* :name "Made with Lisp")))
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
                                                 '((tgsi:dcl (:in 0)) ; vertex position
                                                   (tgsi:dcl (:in 1)) ; vertex colour
                                                   (tgsi:dcl (:in 2)) ; vertex texcoord
                                                   (tgsi:dcl (:out 0) :position)
                                                   (tgsi:dcl (:out 1) :color)
                                                   (tgsi:dcl (:out 2) :texcoord)
                                                   (tgsi:dcl (:const 0 3)) ; projection/view matrix
                                                   (tgsi:dcl (:const 4)) ; texcoord adjust (xy = translate, zw = scale)
                                                   (tgsi:dcl (:const 5)) ; vertex colour multiplier
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
                                                   (tgsi:mul (:out 1) (:in 1) (:const 5))
                                                   ;; Scale the texcoord and add the offset.
                                                   (tgsi:mul (:out 2 :xy) (:in 2) (:const 4 :zwww))
                                                   (tgsi:add (:out 2 :xy) (:out 2) (:const 4))
                                                   (tgsi:end))))
               (fragment-shader (virgl:make-shader context
                                                   :fragment
                                                   ;; Color semantic and color interpolation
                                                   '((tgsi:dcl (:in 0) :color :color)
                                                     (tgsi:dcl (:in 2) :texcoord)
                                                     (tgsi:dcl (:out 0) :color)
                                                     (tgsi:dcl (:samp 0))
                                                     (tgsi:dcl (:temp 0))
                                                     (tgsi:imm :flt32 (1.0 1.0 1.0 1.0)) ; inverse alpha
                                                     (tgsi:imm :flt32 (0.0 0.0 0.0 0.5)) ; alpha threshold
                                                     ;; Read texture.
                                                     (tgsi:tex (:out 0) (:in 2) (:samp 0) :2d)
                                                     (tgsi:mul (:out 0 :xyz) (:out 0) (:out 0 :wwww)) ; multiply by alpha
                                                     ;; Multiply vertex colour by inverse alpha.
                                                     (tgsi:sub (:temp 0) (:imm 0) (:out 0))
                                                     (tgsi:mul (:temp 0) (:in 0) (:temp 0 :wwww))
                                                     ;; Alpha blend the two together.
                                                     (tgsi:add (:out 0) (:out 0) (:temp 0))
                                                     ;; Alpha test.
                                                     (tgsi:sub (:temp 0 :w) (:out 0) (:imm 1))
                                                     (tgsi:kill-if (:temp 0 :wwww))
                                                     (tgsi:end))))
               ;; Needs to have both a view and a state bound.
               (sampler-state (virgl:make-sampler-state context))
               (alien-view (virgl:make-sampler-view context alien-texture))
               (mwl-view (virgl:make-sampler-view context mwl-texture))
               ;; Create a surface object backed by the window buffer.
               (window-surface (virgl:make-surface context window-texture))
               ;; And the depth texture
               (depth-surface (virgl:make-surface context depth-texture))
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
                                   :r32g32b32a32-float)
                                 ;; Third element: Texcoord.
                                 '(32 ; src-offset
                                   0 ; instance-divisor
                                   0 ; vertex buffer index
                                   :r32g32b32a32-float)))
               (blend (virgl:make-blend context :colormask :rgba))
               (rasterizer (virgl:make-rasterizer
                            context
                            :front-ccw t ; match opengl
                            :cull-face :back))
               (dsa (virgl:make-dsa
                     context
                     :depth-enabled t
                     :depth-writemask t
                     :depth-func :less))
               (setup-cmd-buf (virgl:make-command-buffer context)))
          ;; Attach window surface object to the framebuffer's color0 channel.
          (virgl:add-command-set-framebuffer-state setup-cmd-buf depth-surface window-surface)
          ;; Viewport.
          (virgl:add-command-set-viewport-state
           setup-cmd-buf
           ;; near-depth = 0, far-depth = 1
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5
           (/ (virgl:width window-texture) 2.0) (/ (virgl:height window-texture) 2.0) 0.5)
          ;; Configure vertex buffer
          (virgl:add-command-set-vertex-buffers
           setup-cmd-buf
           `(48 ; stride, 3*float4
             0 ; offset
             ,vertex-buffer)) ; vertex buffer resource handle
          (virgl:add-command-bind-vertex-elements setup-cmd-buf vertex-elements)
          (virgl:add-command-bind-shader setup-cmd-buf vertex-shader)
          (virgl:add-command-bind-shader setup-cmd-buf fragment-shader)
          ;; Enable writes to the color channels
          (virgl:add-command-bind-blend setup-cmd-buf blend)
          (virgl:add-command-bind-rasterizer setup-cmd-buf rasterizer)
          (virgl:add-command-bind-dsa setup-cmd-buf dsa)
          (virgl:add-command-set-index-buffer setup-cmd-buf index-buffer 2 0)
          (virgl:add-command-bind-sampler-states setup-cmd-buf :fragment 0 sampler-state)
          (virgl:command-buffer-finalize setup-cmd-buf)
          (virgl:command-buffer-submit setup-cmd-buf)
          (let ((cmd-buf (virgl:make-command-buffer context))
                (perspective (make-perspective-matrix
                              (degrees-to-radians 90)
                              (/ (float (virgl:width window-texture)) (virgl:height window-texture))
                              0.01 100.0))
                (i 0))
            (window-loop
              (virgl:command-buffer-reset cmd-buf)
              ;; Clear framebuffer.
              (virgl:add-command-clear
               cmd-buf '(:color :depth) 0.25 0.33 0.66 1.0 1.0d0 0)
              ;; Drawing background lines
              ;; Bind line texture
              (virgl:add-command-set-sampler-views cmd-buf :fragment 0 mwl-view)
              ;; Set constants.
              (virgl:add-command-set-constant-buffer
               cmd-buf :vertex
               ;; Transform matrix (:const 0 3)
               (matrix-multiply
                perspective
                (matrix-multiply
                 (make-translate-matrix 0.0 2.0 -5.0)
                 (make-scale-matrix 40.0 2.0 2.0)))
               ;; Texture offset and scale (:const 4)
               (* (float i 0.0f0) -0.01) 0.0 20.0 1.0
               ;; Vertex colour multiplier (:const 5)
               0.0 0.0 0.0 0.0)
              ;; Draw line indices.
              (virgl:add-command-draw-vbo cmd-buf 36 6 :triangles :indexed t)
              ;; Set constants.
              (virgl:add-command-set-constant-buffer
               cmd-buf :vertex
               ;; Transform matrix (:const 0 3)
               (matrix-multiply
                perspective
                (matrix-multiply
                 (make-translate-matrix 0.0 -2.0 -5.0)
                 (make-scale-matrix 40.0 2.0 2.0)))
               ;; Texture offset and scale (:const 4)
               (* (float i 0.0f0) 0.01) 0.0 20.0 1.0
               ;; Vertex colour multiplier (:const 5)
               0.0 0.0 0.0 0.0)
              ;; Draw line indices.
              (virgl:add-command-draw-vbo cmd-buf 36 6 :triangles :indexed t)
              ;; Drawing cube
              ;; Bind cube face texture
              (virgl:add-command-set-sampler-views cmd-buf :fragment 0 alien-view)
              ;; Set constants.
              (virgl:add-command-set-constant-buffer
               cmd-buf :vertex
               ;; Transform matrix (:const 0 3)
               (matrix-multiply
                perspective
                (matrix-multiply
                 (make-translate-matrix 0.0 0.0 -1.0)
                 (make-rotate-matrix 1.0 2.0 0.0 (degrees-to-radians (* i 2)))))
               ;; Texture offset and scale (:const 4)
               0.0 0.0 1.0 1.0
               ;; Vertex colour multiplier (:const 5)
               1.0 1.0 1.0 1.0)
              ;; Draw cube indices.
              (virgl:add-command-draw-vbo cmd-buf 0 36 :triangles :indexed t)
              ;; Do it!
              (virgl:command-buffer-finalize cmd-buf)
              (virgl:command-buffer-submit cmd-buf)
              (incf i))))))))
