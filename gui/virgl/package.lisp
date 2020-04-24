(defpackage :mezzano.gui.virgl
  (:use :cl)
  (:local-nicknames (:gpu :mezzano.supervisor.virtio-gpu)
                    (:ext :mezzano.extensions)
                    (:sup :mezzano.supervisor)
                    (:gui :mezzano.gui))
  (:export #:get-virgl
           #:virgl
           #:virgl-scanout
           #:scanout-flush
           #:destroy
           #:width
           #:height
           #:depth

           #:make-context
           #:with-context

           #:make-vertex-buffer
           #:make-index-buffer
           #:make-texture
           #:make-texture-from-gui-surface
           #:resource-dma-buffer
           #:transfer-to-gpu
           #:with-resource
           #:with-resources

           #:make-shader
           #:make-surface
           #:make-vertex-elements
           #:make-blend
           #:make-rasterizer
           #:make-dsa
           #:make-sampler-state
           #:make-sampler-view
           #:with-object

           #:make-command-buffer
           #:command-buffer-finalize
           #:command-buffer-submit
           #:command-buffer-reset
           #:add-command-set-framebuffer-state
           #:add-command-set-viewport-state
           #:add-command-clear
           #:add-command-set-vertex-buffers
           #:add-command-set-sampler-views
           #:add-command-bind-vertex-elements
           #:add-command-bind-shader
           #:add-command-bind-blend
           #:add-command-bind-rasterizer
           #:add-command-bind-dsa
           #:add-command-bind-sampler-states
           #:add-command-draw-vbo
           #:add-command-set-constant-buffer
           #:add-command-set-index-buffer
           #:add-command-clear-index-buffer))

(defpackage :mezzano.gui.virgl.tgsi
  (:use :cl)
  (:export #:assemble
           #:dcl #:imm
           #:end #:mov #:add #:sub #:mul
           #:tex))

(defpackage :mezzano.gui.virgl.test
  (:use :cl)
  (:local-nicknames (:gui :mezzano.gui)
                    (:virgl :mezzano.gui.virgl)
                    (:tgsi :mezzano.gui.virgl.tgsi)
                    (:gpu :mezzano.supervisor.virtio-gpu)
                    (:ext :mezzano.extensions)
                    (:sup :mezzano.supervisor)))
