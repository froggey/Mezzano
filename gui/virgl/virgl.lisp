(in-package :mezzano.gui.virgl)

;;; Renderer capabilities.

(defstruct virgl-caps data)

(defmacro define-caps-struct-slots (&rest slots)
  `(progn
     ,@(loop
          for (version name offset accessor) in slots
          collect (etypecase accessor
                    (integer ; bitfield
                     `(defun ,name (caps index)
                        (assert (<= 0 index ,(1- accessor)))
                        (multiple-value-bind (leaf bit)
                            (truncate index 8)
                          (logbitp bit (aref (virgl-caps-data caps) (+ ,offset leaf))))))
                    ((cons integer null) ; bit
                     (multiple-value-bind (leaf bit)
                         (truncate (first accessor) 8)
                       `(defun ,name (caps)
                          (logbitp ,bit (aref (virgl-caps-data caps) ,(+ offset leaf))))))
                    ((cons symbol (cons integer (cons integer null))) ; array
                     `(defun ,name (caps index)
                        (assert (<= 0 index ,(1- (second accessor))))
                        (,(first accessor) (virgl-caps-data caps) (+ ,offset (* index ,(third accessor))))))
                    (symbol ; regular slot
                     `(defun ,name (caps)
                        (,accessor (virgl-caps-data caps) ,offset)))))
     (defmethod describe-object ((object virgl-caps) stream)
       (format stream "~S is a ~S.~%" object (type-of object))
       ,@(loop
            for (version name offset accessor) in slots
            collect `(when (<= ,version (caps-max-version object))
                       ,(etypecase accessor
                          (integer ; bitfield
                           `(let ((vec (make-array ,accessor :element-type 'bit)))
                              (dotimes (i ,accessor)
                                (setf (aref vec i) (if (,name object i) 1 0)))
                              (format stream "  ~:(~S~): ~S~%" ',name vec)))
                          ((cons integer null) ; bit
                           `(format stream "  ~:(~S~): ~S~%" ',name (,name object)))
                          ((cons symbol (cons integer (cons integer null))) ; array
                           `(let ((vec (make-array ,(second accessor))))
                              (dotimes (i ,(second accessor))
                                (setf (aref vec i) (,name object i)))
                              (format stream "  ~:(~S~): ~S~%" ',name vec)))
                          (symbol ; regular slot
                           `(format stream "  ~:(~S~): ~S~%" ',name (,name object)))))))))

(defun ieee-single-ref/le (vector index)
  (ext:single-float-to-ieee-binary32 (ext:ub32ref/le vector index)))

(defun (setf ieee-single-ref/le) (value vector index)
  (setf (ext:ub32ref/le vector index) (ext:ieee-binary32-to-single-float value))
  value)

(define-caps-struct-slots
  (0 caps-max-version                         0 ext:ub32ref/le)
  ;; v1 caps
  (1 caps-sampler                             4 512)
  (1 caps-render                             68 512)
  (1 caps-depthstencil                      132 512)
  (1 caps-vertexbuffer                      196 512)
  (1 caps-indep-blend-enable                260 (0))
  (1 caps-indep-blend-func                  260 (1))
  (1 caps-cube-map-array                    260 (2))
  (1 caps-shader-stencil-export             260 (3))
  (1 caps-conditional-render                260 (4))
  (1 caps-start-instance                    260 (5))
  (1 caps-primitive-restart                 260 (6))
  (1 caps-blend-eq-sep                      260 (7))
  (1 caps-instanceid                        260 (8))
  (1 caps-vertex-element-instance-divisor   260 (9))
  (1 caps-seamless-cube-map                 260 (10))
  (1 caps-occlusion-query                   260 (11))
  (1 caps-timer-query                       260 (12))
  (1 caps-streamout-pause-resume            260 (13))
  (1 caps-texture-multisample               260 (14))
  (1 caps-fragment-coord-conventions        260 (15))
  (1 caps-depth-clip-disable                260 (16))
  (1 caps-seamless-cube-map-per-texture     260 (17))
  (1 caps-ubo                               260 (18))
  (1 caps-color-clamping                    260 (19)) ; not in GL 3.1 core profile
  (1 caps-poly-stipple                      260 (20)) ; not in GL 3.1 core profile
  (1 caps-mirror-clamp                      260 (21))
  (1 caps-texture-query-lod                 260 (22))
  (1 caps-has-fp64                          260 (23))
  (1 caps-has-tessellation-shaders          260 (24))
  (1 caps-has-indirect-draw                 260 (25))
  (1 caps-has-sample-shading                260 (26))
  (1 caps-has-cull                          260 (27))
  (1 caps-conditional-render-inverted       260 (28))
  (1 caps-derivative-control                260 (29))
  (1 caps-polygon-offset-clamp              260 (30))
  (1 caps-transform-feedback-overflow-query 260 (31))
  (1 caps-glsl-level                        264 ext:ub32ref/le)
  (1 caps-max-texture-array-layers          268 ext:ub32ref/le)
  (1 caps-max-streamout-buffers             272 ext:ub32ref/le)
  (1 caps-max-dual-source-render-targets    276 ext:ub32ref/le)
  (1 caps-max-render-targets                280 ext:ub32ref/le)
  (1 caps-max-samples                       284 ext:ub32ref/le)
  (1 caps-prim-mask                         288 ext:ub32ref/le)
  (1 caps-max-tbo-size                      292 ext:ub32ref/le)
  (1 caps-max-uniform-blocks                296 ext:ub32ref/le)
  (1 caps-max-viewports                     300 ext:ub32ref/le)
  (1 caps-max-texture-gather-components     304 ext:ub32ref/le)
  ;; v2 caps
  (2 caps-min-aliased-point-size            308 ieee-single-ref/le)
  (2 caps-max-aliased-point-size            312 ieee-single-ref/le)
  (2 caps-min-smooth-point-size             316 ieee-single-ref/le)
  (2 caps-max-smooth-point-size             320 ieee-single-ref/le)
  (2 caps-min-aliased-line-width            324 ieee-single-ref/le)
  (2 caps-max-aliased-line-width            328 ieee-single-ref/le)
  (2 caps-min-smooth-line-width             332 ieee-single-ref/le)
  (2 caps-max-smooth-line-width             336 ieee-single-ref/le)
  (2 caps-max-texture-lod-bias              340 ieee-single-ref/le)
  (2 caps-max-geom-output-vertices          344 ext:ub32ref/le)
  (2 caps-max-geom-total-output-components  348 ext:ub32ref/le)
  (2 caps-max-vertex-outputs                352 ext:ub32ref/le)
  (2 caps-max-vertex-attribs                356 ext:ub32ref/le)
  (2 caps-max-shader-patch-varyings         360 ext:ub32ref/le)
  (2 caps-min-texel-offset                  364 ext:sb32ref/le)
  (2 caps-max-texel-offset                  368 ext:sb32ref/le)
  (2 caps-min-texture-gather-offset         372 ext:sb32ref/le)
  (2 caps-max-texture-gather-offset         376 ext:sb32ref/le)
  (2 caps-texture-buffer-offset-alignment   380 ext:ub32ref/le)
  (2 caps-uniform-buffer-offset-alignment   384 ext:ub32ref/le)
  (2 caps-shader-buffer-offset-alignment    388 ext:ub32ref/le)
  (2 caps-capability-bits                   392 ext:ub32ref/le)
  (2 caps-sample-locations                  396 (ext:ub32ref/le 8 4))
  (2 caps-max-vertex-attrib-stride          428 ext:ub32ref/le)
  (2 caps-max-shader-buffer-frag-compute    432 ext:ub32ref/le)
  (2 caps-max-shader-buffer-other-stages    436 ext:ub32ref/le)
  (2 caps-max-shader-image-frag-compute     440 ext:ub32ref/le)
  (2 caps-max-shader-image-other-stages     444 ext:ub32ref/le)
  (2 caps-max-image-samples                 448 ext:ub32ref/le)
  (2 caps-max-compute-work-group-invocations 452 ext:ub32ref/le)
  (2 caps-max-compute-shared-memory-size    456 ext:ub32ref/le)
  (2 caps-max-compute-grid-size             460 (ext:ub32ref/le 3 4))
  (2 caps-max-compute-block-size            472 (ext:ub32ref/le 3 4))
  (2 caps-max-texture-2d-size               484 ext:ub32ref/le)
  (2 caps-max-texture-3d-size               488 ext:ub32ref/le)
  (2 caps-max-texture-cube-size             492 ext:ub32ref/le))

(defun pack-command (cmd obj-type len)
  (check-type cmd (unsigned-byte 8))
  (check-type obj-type (unsigned-byte 8))
  (check-type len (unsigned-byte 16))
  (logior cmd
          (ash obj-type 8)
          (ash len 16)))

(defun vector-push-extend-ub32/le (new-element vector)
  (let ((here (vector-push-extend 0 vector)))
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (setf (ext:ub32ref/le vector here) new-element)
    here))

(defun vector-push-extend-sb32/le (new-element vector)
  (let ((here (vector-push-extend 0 vector)))
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (setf (ext:sb32ref/le vector here) new-element)
    here))

(defun vector-push-extend-single/le (new-element vector)
  (check-type new-element single-float)
  (vector-push-extend-ub32/le
   (ext:single-float-to-ieee-binary32 new-element)
   vector))

(defun vector-push-extend-ub64/le (new-element vector)
  (let ((here (vector-push-extend 0 vector)))
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (vector-push-extend 0 vector)
    (setf (ext:ub64ref/le vector here) new-element)
    here))

(defun vector-push-extend-double/le (new-element vector)
  (check-type new-element double-float)
  (vector-push-extend-ub64/le
   (ext:double-float-to-ieee-binary64 new-element)
   vector))

(defun encode-clear (cmd-buf
                     buffers ; +pipe-clear-*+
                     color0 color1 color2 color3
                     depth ; double
                     stencil)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-clear+ +virgl-object-null+ 8) cmd-buf)
  (vector-push-extend-ub32/le buffers cmd-buf)
  (vector-push-extend-single/le color0 cmd-buf)
  (vector-push-extend-single/le color1 cmd-buf)
  (vector-push-extend-single/le color2 cmd-buf)
  (vector-push-extend-single/le color3 cmd-buf)
  (vector-push-extend-double/le depth cmd-buf)
  (vector-push-extend-ub32/le stencil cmd-buf))

(defun encode-blit (cmd-buf
                    mask ; bitmask of PIPE_MASK_R/G/B/A/Z/S
                    filter ; PIPE_TEX_FILTER_*
                    scissor-enable
                    render-condition-enable
                    alpha-blend ; dst.rgb = src.rgb * src.a + dst.rgb * (1 - src.a)
                    scissor-minx scissor-miny scissor-maxx scissor-maxy
                    dst-resource dst-level dst-format
                    dst-x dst-y dst-z dst-w dst-h dst-d
                    src-resource src-level src-format
                    src-x src-y src-z src-w src-h src-d)
  (check-type mask (unsigned-byte 8))
  (check-type filter (unsigned-byte 2))
  (check-type scissor-minx (unsigned-byte 16))
  (check-type scissor-miny (unsigned-byte 16))
  (check-type scissor-maxx (unsigned-byte 16))
  (check-type scissor-maxy (unsigned-byte 16))
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-blit+ +virgl-object-null+ 21) cmd-buf)
  (vector-push-extend-ub32/le (logior mask
                                      (ash filter 8)
                                      (ash (if scissor-enable 1 0) 10)
                                      (ash (if render-condition-enable 1 0) 11)
                                      (ash (if alpha-blend 1 0) 12))
                              cmd-buf)
  (vector-push-extend-ub32/le (logior scissor-minx (ash scissor-miny 16)) cmd-buf)
  (vector-push-extend-ub32/le (logior scissor-maxx (ash scissor-maxy 16)) cmd-buf)
  (vector-push-extend-ub32/le dst-resource cmd-buf)
  (vector-push-extend-ub32/le dst-level cmd-buf)
  (vector-push-extend-ub32/le dst-format cmd-buf)
  (vector-push-extend-ub32/le dst-x cmd-buf)
  (vector-push-extend-ub32/le dst-y cmd-buf)
  (vector-push-extend-ub32/le dst-z cmd-buf)
  (vector-push-extend-ub32/le dst-w cmd-buf)
  (vector-push-extend-ub32/le dst-h cmd-buf)
  (vector-push-extend-ub32/le dst-d cmd-buf)
  (vector-push-extend-ub32/le src-resource cmd-buf)
  (vector-push-extend-ub32/le src-level cmd-buf)
  (vector-push-extend-ub32/le src-format cmd-buf)
  (vector-push-extend-ub32/le src-x cmd-buf)
  (vector-push-extend-ub32/le src-y cmd-buf)
  (vector-push-extend-ub32/le src-z cmd-buf)
  (vector-push-extend-ub32/le src-w cmd-buf)
  (vector-push-extend-ub32/le src-h cmd-buf)
  (vector-push-extend-ub32/le src-d cmd-buf))

(defun encode-resource-copy-region (cmd-buf
                                    dst-resource dst-level
                                    dst-x dst-y dst-z
                                    src-resource src-level
                                    src-x src-y src-z
                                    w h d)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-resource-copy-region+ +virgl-object-null+ 13) cmd-buf)
  (vector-push-extend-ub32/le dst-resource cmd-buf)
  (vector-push-extend-ub32/le dst-level cmd-buf)
  (vector-push-extend-ub32/le dst-x cmd-buf)
  (vector-push-extend-ub32/le dst-y cmd-buf)
  (vector-push-extend-ub32/le dst-z cmd-buf)
  (vector-push-extend-ub32/le src-resource cmd-buf)
  (vector-push-extend-ub32/le src-level cmd-buf)
  (vector-push-extend-ub32/le src-x cmd-buf)
  (vector-push-extend-ub32/le src-y cmd-buf)
  (vector-push-extend-ub32/le src-z cmd-buf)
  (vector-push-extend-ub32/le w cmd-buf)
  (vector-push-extend-ub32/le h cmd-buf)
  (vector-push-extend-ub32/le d cmd-buf))

(defun encode-create-shader (handle
                             type ; +pipe-shader-*+
                             tgsi-text
                             num-tokens
                             ;; Only for stream-output
                             so-stride so-outputs)
  (let* ((tgsi-text-bytes (mezzano.internals::encode-utf-8-string
                           tgsi-text
                           :eol-style :lf
                           :nul-terminate t))
         (text-words (truncate (+ (length tgsi-text-bytes) 3) 4))
         (cmd-buf (make-array 100
                              :element-type '(unsigned-byte 8)
                              :adjustable t
                              :fill-pointer 0)))
    (when (> (length tgsi-text-bytes) 1500)
      ;; There's a send limit, but the create shader call supports sending
      ;; the text over in multiple commands.
      (error "Shader too large"))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                              +virgl-object-shader+
                                              (+ (1- 6) ; header word is implicit
                                                 (if so-outputs
                                                     (+ 4 ; stride
                                                        (* (length so-outputs) 2))
                                                     0)
                                                 text-words))
                                cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf)
  (vector-push-extend-ub32/le type cmd-buf)
  ;; Also used as continuation offset, must have high bit set in that case.
  (vector-push-extend-ub32/le (length tgsi-text-bytes) cmd-buf)
  (vector-push-extend-ub32/le num-tokens cmd-buf)
  (cond ((null so-outputs)
         (vector-push-extend-ub32/le 0 cmd-buf)) ; no stream-outputs
        (t
         (vector-push-extend-ub32/le (length so-outputs) cmd-buf)
         (vector-push-extend-ub32/le (elt so-stride 0) cmd-buf)
         (vector-push-extend-ub32/le (elt so-stride 1) cmd-buf)
         (vector-push-extend-ub32/le (elt so-stride 2) cmd-buf)
         (vector-push-extend-ub32/le (elt so-stride 3) cmd-buf)
         (loop
            for (register-index start-component num-components buffer dst-offset stream) in so-outputs
            do
              (check-type register-index (unsigned-byte 8))
              (check-type start-component (unsigned-byte 2))
              (check-type num-components (unsigned-byte 3))
              (check-type buffer (unsigned-byte 3))
              (check-type dst-offset (unsigned-byte 16))
              (check-type stream (unsigned-byte 2))
              (vector-push-extend-ub32/le (logior register-index
                                                  (ash start-component 8)
                                                  (ash num-components 10)
                                                  (ash buffer 13)
                                                  (ash dst-offset 16))
                                          cmd-buf)
              (vector-push-extend-ub32/le stream cmd-buf))))
  ;; Copy shader text into the buffer.
  (let ((current (length cmd-buf)))
    (adjust-array cmd-buf (+ current (* text-words 4)) :fill-pointer t)
    (replace cmd-buf tgsi-text-bytes :start1 current))
  cmd-buf))

;; For texture surfaces, not buffer surfaces.
;; Not sure what a buffer surface is, but they seem to exist.
(defun encode-create-surface (cmd-buf
                              handle ; object handle
                              res-handle ; resource handle
                              format
                              first-layer
                              last-layer
                              level)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                            +virgl-object-surface+
                                            5)
                              cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf)
  (vector-push-extend-ub32/le res-handle cmd-buf)
  (vector-push-extend-ub32/le format cmd-buf)
  (vector-push-extend-ub32/le level cmd-buf)
  (check-type first-layer (unsigned-byte 16))
  (check-type last-layer (unsigned-byte 16))
  (vector-push-extend-ub32/le (logior first-layer
                                      (ash last-layer 16))
                              cmd-buf))

(defun encode-destroy-object (cmd-buf handle &optional (type +virgl-object-null+))
  ;; Object type doesn't actually seem to matter
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-destroy-object+ type 1)
                              cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf))

(defun encode-set-framebuffer-state (cmd-buf zsurf-handle &rest surf-handles)
  (let ((nr-cbufs (length surf-handles)))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-set-framebuffer-state+
                                              +virgl-object-null+
                                              (+ 2 nr-cbufs))
                                cmd-buf)
    (vector-push-extend-ub32/le nr-cbufs cmd-buf)
    (vector-push-extend-ub32/le zsurf-handle cmd-buf)
    (loop
       for surf-handle in surf-handles
       do (vector-push-extend-ub32/le surf-handle cmd-buf))))

;; type is +PIPE-SHADER-foo+
(defun encode-bind-shader (cmd-buf handle type)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-bind-shader+
                                            +virgl-object-null+
                                            2)
                              cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf)
  (vector-push-extend-ub32/le type cmd-buf))

(defun encode-set-vertex-buffers (cmd-buf &rest buffers)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-set-vertex-buffers+
                                            +virgl-object-null+
                                            (* (length buffers) 3))
                              cmd-buf)
  (loop
     for (stride offset res-handle) in buffers
     do
       (vector-push-extend-ub32/le stride cmd-buf)
       (vector-push-extend-ub32/le offset cmd-buf)
       (vector-push-extend-ub32/le res-handle cmd-buf)))

(defun encode-create-vertex-elements (cmd-buf handle &rest elements)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                            +virgl-object-vertex-elements+
                                            (+ 1 (* (length elements) 4)))
                              cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf)
  (loop
     for (src-offset instance-divisor vertex-buffer-index src-format) in elements
     do
       (vector-push-extend-ub32/le src-offset cmd-buf)
       (vector-push-extend-ub32/le instance-divisor cmd-buf)
       (vector-push-extend-ub32/le vertex-buffer-index cmd-buf)
       (vector-push-extend-ub32/le src-format cmd-buf)))

(defun encode-bind-vertex-elements (cmd-buf handle)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-bind-object+
                                            +virgl-object-vertex-elements+
                                            1)
                              cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf))

(defun encode-create-blend (cmd-buf handle
                            logicop-enable
                            dither
                            alpha-to-coverage
                            alpha-to-one
                            logicop-func ; +pipe-logicop-*+
                            blend-enable
                            rgb-func ; +pipe-blend-*+
                            rgb-src-factor rgb-dst-factor ; +pipe-blendfactor-*+
                            alpha-func alpha-src-factor alpha-dst-factor
                            colormask) ; +pipe-mask-*+
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                            +virgl-object-blend+
                                            (+ 3 +virgl-max-color-bufs+))
                              cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf)
  (vector-push-extend-ub32/le (logior ;(if independent-blend-enable 1 0)
                                      (ash (if logicop-enable 1 0) 1)
                                      (ash (if dither 1 0) 2)
                                      (ash (if alpha-to-coverage 1 0) 3)
                                      (ash (if alpha-to-one 1 0) 4))
                              cmd-buf)
  (check-type logicop-func (unsigned-byte 4))
  (vector-push-extend-ub32/le logicop-func cmd-buf)
  (check-type rgb-func (unsigned-byte 3))
  (check-type rgb-src-factor (unsigned-byte 3))
  (check-type rgb-dst-factor (unsigned-byte 5))
  (check-type alpha-func (unsigned-byte 3))
  (check-type alpha-src-factor (unsigned-byte 3))
  (check-type alpha-dst-factor (unsigned-byte 5))
  (check-type colormask (unsigned-byte 4))
  (vector-push-extend-ub32/le (logior (if blend-enable 1 0)
                                      (ash rgb-func 1)
                                      (ash rgb-src-factor 4)
                                      (ash rgb-dst-factor 9)
                                      (ash alpha-func 14)
                                      (ash alpha-src-factor 17)
                                      (ash alpha-dst-factor 22)
                                      (ash colormask 27))
                              cmd-buf)
  ;; Not using independent blend.
  (dotimes (i (1- +virgl-max-color-bufs+))
    (vector-push-extend-ub32/le 0 cmd-buf)))

(defun encode-bind-blend (cmd-buf handle)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-bind-object+
                                            +virgl-object-blend+
                                            1)
                              cmd-buf)
  (vector-push-extend-ub32/le handle cmd-buf))

(defun encode-draw-vbo (cmd-buf
                        start ; index of the first vertex
                        count ; number of vertices
                        mode ; mode of the primitive. +PIPE-PRIM-foo+
                        indexed ; use index buffer
                        instance-count ; number of instances
                        start-instance ; first instance id
                        ;; Primitive restart enable/index
                        ;; (only applies to indexed drawing)
                        primitive-restart
                        restart-index
                        ;; For indexed drawing, these fields apply
                        ;; after index lookup.
                        index-bias ; a bias to be added to each index
                        min-index ; the min index (see glDrawRangeElements)
                        max-index ; the max index (see glDrawRangeElements)
                        ;; Seems to override start(set to 0)/count(set to this).
                        count-from-so)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-draw-vbo+
                                            +virgl-object-null+
                                            12)
                              cmd-buf)
  (vector-push-extend-ub32/le start cmd-buf)
  (vector-push-extend-ub32/le count cmd-buf)
  (vector-push-extend-ub32/le mode cmd-buf)
  (vector-push-extend-ub32/le (if indexed 1 0) cmd-buf)
  (vector-push-extend-ub32/le instance-count cmd-buf)
  (vector-push-extend-sb32/le index-bias cmd-buf)
  (vector-push-extend-ub32/le start-instance cmd-buf)
  (vector-push-extend-ub32/le (if primitive-restart 1 0) cmd-buf)
  (vector-push-extend-ub32/le restart-index cmd-buf)
  (vector-push-extend-ub32/le min-index cmd-buf)
  (vector-push-extend-ub32/le max-index cmd-buf)
  (vector-push-extend-ub32/le count-from-so cmd-buf))

(defun encode-set-viewport-state (cmd-buf scale0 scale1 scale2 translate0 translate1 translate2)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-set-viewport-state+
                                            +virgl-object-null+
                                            7)
                              cmd-buf)
  (vector-push-extend-ub32/le 0 cmd-buf) ; start-slot
  (vector-push-extend-single/le scale0 cmd-buf)
  (vector-push-extend-single/le scale1 cmd-buf)
  (vector-push-extend-single/le scale2 cmd-buf)
  (vector-push-extend-single/le translate0 cmd-buf)
  (vector-push-extend-single/le translate1 cmd-buf)
  (vector-push-extend-single/le translate2 cmd-buf))

(defvar *gpu* (sup:framebuffer-device (mezzano.supervisor:current-framebuffer)))
;; Push vertex data to GPU.
(defvar *test-vertex-dma-buf* (sup:make-dma-buffer 96 :name "Virgl vertex data"))
(defun upload-test-data ()
  (let* ((vertex-dma-buf *test-vertex-dma-buf*)
         (vertex-buf-raw-vec (sup:dma-buffer-array vertex-dma-buf))
         (vertex-buf (make-array (length vertex-buf-raw-vec)
                                 :fill-pointer 0
                                 :displaced-to vertex-buf-raw-vec)))
    ;; Position 0
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    ;; Colour 0
    (vector-push-extend-single/le 1.0 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    ;; Position 1
    (vector-push-extend-single/le 0.5 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    ;; Colour 1
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    ;; Position 2
    (vector-push-extend-single/le 0.5 vertex-buf)
    (vector-push-extend-single/le 0.5 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    ;; Colour 2
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 0.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    (vector-push-extend-single/le 1.0 vertex-buf)
    ;; Create buffer.
    (gpu:virtio-gpu-resource-create-3d
     *gpu* 1000 +pipe-buffer+ 0 +virgl-bind-vertex-buffer+
     (length vertex-buf) 1 1 1 0 0 0)
    ;; Upload data.
    (gpu:virtio-gpu-resource-attach-backing *gpu* 1000 1 (sup:dma-buffer-physical-address vertex-dma-buf) (length vertex-buf))
    ;; Use context 0, this context has all resources attached for the
    ;; purposes of upload.
    (gpu:virtio-gpu-transfer-to-host-3d *gpu* 0 0 0 (length vertex-buf) 1 1 0 1000 0 0 0 :context 0)))
(upload-test-data)

;; Create new context for testing and attach resources to it.
(gpu:virtio-gpu-ctx-create *gpu* 0 :context 1)
(gpu:virtio-gpu-attach-resource *gpu* 123 :context 1) ; scanout
(gpu:virtio-gpu-attach-resource *gpu* 1000 :context 1) ; vertex-buf

#|
(tgsi:assemble-shader :vertex
 '((tgsi:dcl (:in 0))
   (tgsi:dcl (:in 1))
   (tgsi:dcl (:out 0) :position)
   (tgsi:dcl (:out 1) :color) ; screaming.
   (tgsi:mov (:out 0) (:in 0))
   (tgsi:mov (:out 1) (:in 1))
   (tgsi:end)))

(tgsi:assemble-shader :fragment
 '((tgsi:dcl (:in 0) :color :color) ; more screaming.
   (tgsi:dcl (:out 0) :color) ; ahhhhhh.
   (tgsi:imm :flt32 (1.0 1.0 1.0 1.0))
   (tgsi:sub (:out 0) (:imm 0) (:in 0))
   (tgsi:end)))
|#

(defun test ()
  ;; Pass position & colour through unchanged.
  (let ((cmd-buf (encode-create-shader 42
                                       +pipe-shader-vertex+
                                       "VERT
DCL IN[0]
DCL IN[1]
DCL OUT[0], POSITION
DCL OUT[1], COLOR
MOV OUT[0], IN[0]
MOV OUT[1], IN[1]
END"
                                       (+ 2 ; header 3?
                                          (* 4 2) ; declaration decl(1)+range(1)?
                                          0 ; immediate (immediate + 4 words)
                                          1 ; mov
                                          1 ; mov
                                          1 ; end
                                          )
                                       ;; No stream output
                                       nil nil)))
    (gpu:virtio-gpu-submit-3d *gpu* cmd-buf :context 1))
  ;; Fancy inverted colour fragment shader.
  (let ((cmd-buf (encode-create-shader 43
                                       +pipe-shader-fragment+
                                       "FRAG
DCL IN[0], COLOR, COLOR
DCL OUT[0], COLOR
IMM FLT32 { 1.0, 1.0, 1.0, 1.0 }
SUB OUT[0], IMM[0], IN[0]
END"
                                       (+ 2 ; header 3?
                                          (* 2 2) ; declaration decl(1)+range(1)?
                                          5 ; immediate (immediate + 4 words)
                                          1 ; mov
                                          1 ; end
                                          )
                                       ;; No stream output
                                       nil nil)))
    (gpu:virtio-gpu-submit-3d *gpu* cmd-buf :context 1))
  (let ((cmd-buf (make-array 100 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    ;; Create a surface object backed by the scanout buffer
    (encode-create-surface
     cmd-buf 123 123 +virgl-format-b8g8r8a8-unorm+ 0 0 0)
    ;; Attach scanout surface object to the framebuffer's color0 channel.
    ;; No depth/stencil surface.
    (encode-set-framebuffer-state
     cmd-buf 0 123)
    ;; Viewport.
    (encode-set-viewport-state
     cmd-buf
     ;; near-depth = 0, far-depth = 1
     (/ 1024.0 2) (/ 768.0 2) 0.5
     (/ 1024.0 2) (/ 768.0 2) 0.5)
    ;; Testing clear.
    (encode-clear
     cmd-buf +pipe-clear-color0+ 0.25 0.33 0.66 0.75 0.0d0 42)
    ;; Configure vertex buffer
    (encode-set-vertex-buffers
     cmd-buf
     '(32 ; stride, 2*float4
       0 ; offset
       1000) ; vertex buffer resource handle
     )
    (encode-create-vertex-elements
     cmd-buf
     1001 ; vertex-elements object handle
     ;; First element: Positions.
     '(0 ; src-offset
       0 ; instance-divisor
       0 ; vertex buffer index
       #.+virgl-format-r32g32b32a32-float+)
     ;; Second element: Colours.
     '(16 ; src-offset
       0 ; instance-divisor
       0 ; vertex buffer index
       #.+virgl-format-r32g32b32a32-float+))
    (encode-bind-vertex-elements cmd-buf 1001)
    (encode-bind-shader cmd-buf 42 +pipe-shader-vertex+)
    (encode-bind-shader cmd-buf 43 +pipe-shader-fragment+)
    ;; Enable writes to the color channels
    (encode-create-blend cmd-buf 44
                         nil
                         nil
                         nil
                         nil
                         0
                         nil 0 0 0 0 0 0
                         +pipe-mask-rgba+)
    (encode-bind-blend cmd-buf 44)
    (encode-draw-vbo cmd-buf 0 3 +pipe-prim-triangles+ nil 0 0 nil 0 0 0 #xFFFFFFFF 0)
    (gpu:virtio-gpu-submit-3d *gpu* cmd-buf :context 1)
    (gpu:virtio-gpu-resource-flush *gpu* 0 0 1024 768 123))
)
