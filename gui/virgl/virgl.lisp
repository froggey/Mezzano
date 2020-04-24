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
  (ext:ieee-binary32-to-single-float (ext:ub32ref/le vector index)))

(defun (setf ieee-single-ref/le) (value vector index)
  (setf (ext:ub32ref/le vector index) (ext:single-float-to-ieee-binary32 value))
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

(defconstant +capset-virgl+ 1)
(defconstant +capset-virgl2+ 2)

(defun read-virgl-capset (gpu)
  (let ((n-caps (gpu:virtio-gpu-n-capsets gpu))
        (best-id nil)
        (best-version nil)
        (best nil))
    ;; Prioritize v2 with the highest version, then v1.
    (loop
       for i below n-caps
       do (multiple-value-bind (id max-version)
              (gpu:virtio-gpu-get-capset-info gpu i)
            (cond ((eql id +capset-virgl+)
                   (when (or (not best-id)
                             (and (eql best-id +capset-virgl+)
                                  (< best-version max-version)))
                     (setf best i
                           best-id id
                           best-version max-version)))
                  ((eql id +capset-virgl2+)
                   (when (or (not best-id)
                             (< best-version max-version))
                     (setf best i
                           best-id id
                           best-version max-version))))))
    (when (not best)
      (return-from read-virgl-capset nil))
    (multiple-value-bind (id max-version max-size)
        (gpu:virtio-gpu-get-capset-info gpu best)
      (let ((data (make-array max-size :element-type '(unsigned-byte 8))))
        (gpu:virtio-gpu-get-capset gpu id max-version data)
        ;; Add in formats that're always supported.
        (let ((depthstencil-offset 132))
          ;; These are defined in virglrenderer's base_depth_formats.
          (dolist (format '(:z16-unorm
                            :z32-unorm
                            ;; This is the usual format.
                            :s8-uint-z24-unorm
                            :z24x8-unorm
                            :z32-float
                            :z32-float-s8x24-uint
                            :x24s8-uint))
            (multiple-value-bind (index bit)
                (truncate (encode-texture-format format) 8)
              (setf (aref data (+ depthstencil-offset index))
                    (logior (ash 1 bit)
                            (aref data (+ depthstencil-offset index)))))))
        (make-virgl-caps :data data)))))

(defun caps-sampler-supported-p (caps texture-format)
  (caps-sampler caps (encode-texture-format texture-format)))

(defun caps-render-supported-p (caps texture-format)
  (caps-render caps (encode-texture-format texture-format)))

(defun caps-depthstencil-supported-p (caps texture-format)
  (caps-depthstencil caps (encode-texture-format texture-format)))

(defun caps-vertexbuffer-supported-p (caps texture-format)
  (caps-vertexbuffer caps (encode-texture-format texture-format)))

(defun pack-command (cmd obj-type len)
  (check-type cmd (unsigned-byte 8))
  (check-type obj-type (unsigned-byte 8))
  (check-type len (unsigned-byte 16))
  (logior cmd
          (ash obj-type 8)
          (ash len 16)))

(defun vector-push-extend-ub16/le (new-element vector)
  (let ((here (vector-push-extend 0 vector)))
    (vector-push-extend 0 vector)
    (setf (ext:ub16ref/le vector here) new-element)
    here))

(defun vector-push-extend-sb16/le (new-element vector)
  (let ((here (vector-push-extend 0 vector)))
    (vector-push-extend 0 vector)
    (setf (ext:sb16ref/le vector here) new-element)
    here))

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
  (dolist (element elements)
    (destructuring-bind (src-offset instance-divisor vertex-buffer-index src-format)
        element
       (vector-push-extend-ub32/le src-offset cmd-buf)
       (vector-push-extend-ub32/le instance-divisor cmd-buf)
       (vector-push-extend-ub32/le vertex-buffer-index cmd-buf)
       (vector-push-extend-ub32/le (encode-texture-format src-format) cmd-buf))))

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

(defun encode-set-sub-ctx (cmd-buf ctx-sub-id)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-set-sub-ctx+
                                            +virgl-object-null+
                                            1)
                              cmd-buf)
  (vector-push-extend-ub32/le ctx-sub-id cmd-buf))

(defun encode-create-sub-ctx (cmd-buf ctx-sub-id)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-sub-ctx+
                                            +virgl-object-null+
                                            1)
                              cmd-buf)
  (vector-push-extend-ub32/le ctx-sub-id cmd-buf))

(defun encode-destroy-sub-ctx (cmd-buf ctx-sub-id)
  (vector-push-extend-ub32/le (pack-command +virgl-ccmd-destroy-sub-ctx+
                                            +virgl-object-null+
                                            1)
                              cmd-buf)
  (vector-push-extend-ub32/le ctx-sub-id cmd-buf))

(defconstant +virgl-gpu-context+ 1
  "The context ID that the virgl renderer uses.
Avoid using context 0 because that's what the compositor and 2D rendering uses.")

;; Actually closer to 2^32, but lets not get that close.
(defconstant +virgl-max-object-id+ (1- (expt 2 31)))

;; Actually closer to 2^32, but lets not get that close.
(defconstant +virgl-max-resource-id+ (1- (expt 2 31)))

;; Actually closer to 2^32, but lets not get that close.
(defconstant +virgl-max-sub-contexts+ (1- (expt 2 31)))

(defclass virgl ()
  ((%gpu :initarg :gpu :reader virgl-gpu)
   (%caps :initarg :caps :reader virgl-caps)
   (%lock :reader virgl-lock)
   (%error-state :initform nil :accessor virgl-error-state)
   (%scanout :reader virgl-%scanout)
   ;; Actually sub-contexts on context +virgl-gpu-context+.
   (%contexts :initform (make-hash-table) :reader virgl-contexts)
   (%next-context-id :initform 0 :accessor virgl-next-context-id)
   (%resources :initform (make-hash-table) :reader virgl-resources)
   (%next-resource-id :initform gpu:+virtio-gpu-internal-resource-max+ :accessor virgl-next-resource-id)))

(defmethod initialize-instance :after ((instance virgl) &key)
  (setf (slot-value instance '%lock) (sup:make-mutex instance)))

(define-condition virgl-unsupported-error (error)
  ((gpu :initarg :gpu :reader virgl-unsupported-error-gpu))
  (:report (lambda (condition stream)
             (format stream "Virgl not supported on GPU ~S"
                     (virgl-unsupported-error-gpu condition)))))

(define-condition virgl-error (error)
  ((virgl :initarg :virgl :reader virgl-error-virgl)
   (context :initarg :context :initform nil :reader virgl-error-context)))

(define-condition simple-virgl-error (virgl-error simple-error)
  ())

(defun simple-virgl-error (virgl context format-control &rest format-arguments)
  (error 'simple-virgl-error
         :virgl virgl
         :context context
         :format-control format-control
         :format-arguments format-arguments))

(defun get-virgl (&key flush-existing from)
  "Get the virgl object for the compositor's current display."
  (when (typep from 'virgl)
    (return-from get-virgl from))
  (let ((gpu (etypecase from
               (gpu:virtio-gpu from)
               (sup:framebuffer (sup:framebuffer-device from))
               (null (sup:framebuffer-device mezzano.gui.compositor::*main-screen*)))))
    (when (or (not (typep gpu 'gpu:virtio-gpu))
              (not (gpu:virtio-gpu-virgl-p gpu)))
      (error 'virgl-unsupported-error :gpu gpu))
    (let ((virgl (gpu:virtio-gpu-virgl-data gpu)))
      (cond ((and (not flush-existing) virgl))
            (t (let ((new (make-instance 'virgl :gpu gpu)))
                 ;; Take the lock early so that we can do initialization
                 ;; work before anything else is aware of this object.
                 (sup:with-mutex ((virgl-lock new))
                   (let ((existing (ext:cas (gpu:virtio-gpu-virgl-data gpu)
                                            virgl
                                            new)))
                     (cond ((not (eql existing virgl)))
                           (t
                            ;; Our virgl was actually installed,
                            ;; do further inititialization work.
                            (virgl-reset-1 new)
                            new))))))))))

(defun virgl-reset-1 (virgl)
  (assert (sup:mutex-held-p (virgl-lock virgl)))
  (let ((gpu (virgl-gpu virgl)))
    (gpu:virtio-gpu-ctx-destroy gpu :context +virgl-gpu-context+)
    ;; Invalidate contexts.
    (loop
       for context being the hash-keys of (virgl-contexts virgl)
       do (setf (slot-value context '%id) nil))
    (clrhash (virgl-contexts virgl))
    ;; Create the primary virgl context.
    (multiple-value-bind (successp error)
        (gpu:virtio-gpu-ctx-create
         gpu "virgl gpu context"
         :context +virgl-gpu-context+)
      (when (not successp)
        (setf (virgl-error-state virgl) error)
        (simple-virgl-error virgl nil "Unable to create primary context: ~D" error)))
    ;; Attach the scanout to it.
    (multiple-value-bind (successp error)
        (gpu:virtio-gpu-attach-resource
         gpu gpu:+virtio-gpu-framebuffer-resource-id+
         :context +virgl-gpu-context+)
      (when (not successp)
        (setf (virgl-error-state virgl) error)
        (simple-virgl-error virgl nil "Unable to attach scanout to primary context: ~D" error)))
    (setf (slot-value virgl '%caps) (read-virgl-capset gpu))
    ;; TODO: What to do when the scanout changes size?
    (setf (slot-value virgl '%scanout)
          (make-instance 'scanout
                         :virgl virgl :context nil
                         :name `(scanout 0)
                         :id gpu:+virtio-gpu-framebuffer-resource-id+
                         :dma-buffer (gpu:virtio-gpu-framebuffer gpu)
                         :format (gpu:virtio-gpu-framebuffer-format gpu)
                         :render-target t
                         :width (gpu:virtio-gpu-width gpu)
                         :height (gpu:virtio-gpu-height gpu)))
    (setf (virgl-error-state virgl) nil))
  (values))

(defun virgl-reset (&key virgl)
  (setf virgl (get-virgl :from virgl))
  (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
    (virgl-reset-1 virgl)))

(define-condition context-ids-exhausted-error (virgl-error)
  ())

(defclass context ()
  ((%virgl :initarg :virgl :reader virgl)
   (%name :initarg :name :accessor name)
   (%id :initarg :id :reader context-id)
   (%objects :initform (make-hash-table) :reader context-objects)
   (%next-object-id :initform 1 :accessor context-next-object-id)))

(defun allocate-context-id (virgl)
  (when (> (hash-table-count (virgl-contexts virgl))
           (1- +virgl-max-sub-contexts+))
    (error 'context-ids-exhausted-error :virgl virgl))
  (loop
     ;; Avoid id 0!
     (let ((id (incf (virgl-next-context-id virgl))))
       (when (>= id +virgl-max-sub-contexts+)
         (setf id (setf (virgl-next-context-id virgl) 0)))
       (when (not (gethash id (virgl-contexts virgl)))
         (return id)))))

(defun virgl-submit-simple-command-buffer-1 (virgl cmd-buf)
  (assert (sup:mutex-held-p (virgl-lock virgl)))
  (multiple-value-bind (successp error)
      (gpu:virtio-gpu-submit-3d (virgl-gpu virgl)
                                cmd-buf
                                :context +virgl-gpu-context+)
    (when (not successp)
      (simple-virgl-error virgl nil "Command buffer submission failed: ~D" error))))

(defun make-context (&key virgl name)
  (setf virgl (get-virgl :from virgl))
  (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
    (let ((id (allocate-context-id virgl))
          (cmd-buf (make-array 100
                               :element-type '(unsigned-byte 8)
                               :adjustable t
                               :fill-pointer 0)))
      ;; Issue a create sub-context command.
      (encode-create-sub-ctx cmd-buf id)
      (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
      (let ((ctx (make-instance 'context
                                :virgl virgl
                                :name name
                                :id id)))
        (setf (gethash id (virgl-contexts virgl)) ctx)
        ctx))))

(defmacro with-context ((context &key virgl name) &body body)
  (let ((context-sym (gensym "CONTEXT")))
    `(let ((,context-sym (make-context :virgl ,virgl :name ,name)))
     (unwind-protect
          (let ((,context ,context-sym)) ,@body)
       (destroy ,context-sym)))))

(defgeneric destroy (object)
  (:documentation "Destroy an object & release all resources associated with it."))

(defmethod destroy ((context context))
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (when (context-id context)
        (let ((cmd-buf (make-array 100
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t
                                   :fill-pointer 0)))
          ;; Issue a destroy sub-context command.
          (encode-destroy-sub-ctx cmd-buf (context-id context))
          (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
          (remhash (context-id context) (virgl-contexts virgl))
          (setf (slot-value context '%id) nil)
          (values))))))

(define-condition resource-ids-exhausted-error (virgl-error)
  ())

(defun allocate-resource-id (virgl)
  (when (> (hash-table-count (virgl-resources virgl))
           (- +virgl-max-resource-id+ gpu:+virtio-gpu-internal-resource-max+))
    (error 'resource-ids-exhausted-error :virgl virgl))
  (loop
     ;; Avoid ids below gpu:+virtio-gpu-internal-resource-max+!
     (let ((id (virgl-next-resource-id virgl)))
       (cond ((>= id +virgl-max-resource-id+)
              (setf id (setf (virgl-next-resource-id virgl)
                             gpu:+virtio-gpu-internal-resource-max+)))
             (t
              (incf (virgl-next-resource-id virgl))))
       (when (not (gethash id (virgl-resources virgl)))
         (return id)))))

(defclass resource ()
  ((%virgl :initarg :virgl :reader virgl)
   (%context :initarg :context :reader context)
   (%name :initarg :name :accessor name)
   (%id :initarg :id :reader resource-id)
   (%dma-buffer :initarg :dma-buffer :reader resource-dma-buffer))
  (:default-initargs :name nil))

(defmethod destroy ((resource resource))
  (let ((virgl (virgl resource)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (when (resource-id resource)
        ;; Unbind it from the virgl context and release the reference.
        (gpu:virtio-gpu-detach-resource
         (virgl-gpu virgl) (resource-id resource)
         :context +virgl-gpu-context+)
        (gpu:virtio-gpu-resource-unref
         (virgl-gpu virgl) (resource-id resource))
        ;; Release the dma buffer.
        (let ((dma-buffer (resource-dma-buffer resource)))
          (when dma-buffer
            (sup:release-dma-buffer dma-buffer)))
        (remhash (resource-id resource) (virgl-resources virgl))
        (setf (slot-value resource '%id) nil)))))

(deftype pipe-target ()
  '(member
    :buffer
    :texture-1d :texture-2d :texture-3d :texture-cube
    :texture-rect
    :texture-1d-array :texture-2d-array :texture-cube-array))

(defun encode-pipe-target (pipe-target)
  (ecase pipe-target
    (:buffer +pipe-buffer+)
    (:texture-1d +pipe-texture-1d+)
    (:texture-2d +pipe-texture-2d+)
    (:texture-3d +pipe-texture-3d+)
    (:texture-cube +pipe-texture-cube+)
    (:texture-rect +pipe-texture-rect+)
    (:texture-1d-array +pipe-texture-1d-array+)
    (:texture-2d-array +pipe-texture-2d-array+)
    (:texture-cube-array +pipe-texture-cube-array+)))

(defparameter *texture-formats*
  `((:b8g8r8a8-unorm ,+virgl-format-b8g8r8a8-unorm+ 4)
    (:b8g8r8x8-unorm ,+virgl-format-b8g8r8x8-unorm+ 4)
    (:a8r8g8b8-unorm ,+virgl-format-a8r8g8b8-unorm+ 4)
    (:x8r8g8b8-unorm ,+virgl-format-x8r8g8b8-unorm+ 4)
    (:b5g5r5a1-unorm ,+virgl-format-b5g5r5a1-unorm+ 2)
    (:b4g4r4a4-unorm ,+virgl-format-b4g4r4a4-unorm+ 2)
    (:b5g6r5-unorm ,+virgl-format-b5g6r5-unorm+ 2)
    (:r10g10b10a2-unorm ,+virgl-format-r10g10b10a2-unorm+ 4)
    (:l8-unorm ,+virgl-format-l8-unorm+ 1)    ; ubyte luminance
    (:a8-unorm ,+virgl-format-a8-unorm+ 1)   ; ubyte alpha
    (:l8a8-unorm ,+virgl-format-l8a8-unorm+ 2)   ; ubyte alpha, luminance

    (:l16-unorm ,+virgl-format-l16-unorm+ 2)   ; ushort luminance

    (:z16-unorm ,+virgl-format-z16-unorm+ 2)
    (:z32-unorm ,+virgl-format-z32-unorm+ 4)
    (:z32-float ,+virgl-format-z32-float+ 4)
    (:z24-unorm-s8-uint ,+virgl-format-z24-unorm-s8-uint+ 4)
    (:s8-uint-z24-unorm ,+virgl-format-s8-uint-z24-unorm+ 4)
    (:z24x8-unorm ,+virgl-format-z24x8-unorm+ 4)
    (:s8-uint ,+virgl-format-s8-uint+ 1)   ; ubyte stencil

    (:r32-float ,+virgl-format-r32-float+ 4)
    (:r32g32-float ,+virgl-format-r32g32-float+ 8)
    (:r32g32b32-float ,+virgl-format-r32g32b32-float+ 12)
    (:r32g32b32a32-float ,+virgl-format-r32g32b32a32-float+ 16)

    (:r16-unorm ,+virgl-format-r16-unorm+ 2)
    (:r16g16-unorm ,+virgl-format-r16g16-unorm+ 2)

    (:r16g16b16a16-unorm ,+virgl-format-r16g16b16a16-unorm+ 8)

    (:r16-snorm ,+virgl-format-r16-snorm+ 2)
    (:r16g16-snorm ,+virgl-format-r16g16-snorm+ 4)
    (:r16g16b16a16-snorm ,+virgl-format-r16g16b16a16-snorm+ 8)

    (:r8-unorm ,+virgl-format-r8-unorm+ 1)
    (:r8g8-unorm ,+virgl-format-r8g8-unorm+ 2)

    (:r8g8b8a8-unorm ,+virgl-format-r8g8b8a8-unorm+ 4)

    (:r8-snorm ,+virgl-format-r8-snorm+ 1)
    (:r8g8-snorm ,+virgl-format-r8g8-snorm+ 2)
    (:r8g8b8-snorm ,+virgl-format-r8g8b8-snorm+ 3)
    (:r8g8b8a8-snorm ,+virgl-format-r8g8b8a8-snorm+ 4)

    (:r16-float ,+virgl-format-r16-float+ 2)
    (:r16g16-float ,+virgl-format-r16g16-float+ 4)
    (:r16g16b16-float ,+virgl-format-r16g16b16-float+ 6)
    (:r16g16b16a16-float ,+virgl-format-r16g16b16a16-float+ 8)

    (:l8-srgb ,+virgl-format-l8-srgb+ 1)
    (:l8a8-srgb ,+virgl-format-l8a8-srgb+ 2)
    (:b8g8r8a8-srgb ,+virgl-format-b8g8r8a8-srgb+ 4)
    (:b8g8r8x8-srgb ,+virgl-format-b8g8r8x8-srgb+ 4)
    (:r8g8b8a8-srgb ,+virgl-format-r8g8b8a8-srgb+ 4)

    ;; compressed formats
    (:dxt1-rgb ,+virgl-format-dxt1-rgb+ nil)
    (:dxt1-rgba ,+virgl-format-dxt1-rgba+ nil)
    (:dxt3-rgba ,+virgl-format-dxt3-rgba+ nil)
    (:dxt5-rgba ,+virgl-format-dxt5-rgba+ nil)

    ;; sRGB, compressed
    (:dxt1-srgb ,+virgl-format-dxt1-srgb+ nil)
    (:dxt1-srgba ,+virgl-format-dxt1-srgba+ nil)
    (:dxt3-srgba ,+virgl-format-dxt3-srgba+ nil)
    (:dxt5-srgba ,+virgl-format-dxt5-srgba+ nil)

    ;; rgtc compressed
    (:rgtc1-unorm ,+virgl-format-rgtc1-unorm+ nil)
    (:rgtc1-snorm ,+virgl-format-rgtc1-snorm+ nil)
    (:rgtc2-unorm ,+virgl-format-rgtc2-unorm+ nil)
    (:rgtc2-snorm ,+virgl-format-rgtc2-snorm+ nil)

    (:a8b8g8r8-unorm ,+virgl-format-a8b8g8r8-unorm+ 4)
    (:b5g5r5x1-unorm ,+virgl-format-b5g5r5x1-unorm+ 2)
    (:r11g11b10-float ,+virgl-format-r11g11b10-float+ 4)
    (:r9g9b9e5-float ,+virgl-format-r9g9b9e5-float+ 4)
    (:z32-float-s8x24-uint ,+virgl-format-z32-float-s8x24-uint+ 8)

    (:b10g10r10a2-unorm ,+virgl-format-b10g10r10a2-unorm+ 4)
    (:r8g8b8x8-unorm ,+virgl-format-r8g8b8x8-unorm+ 4)
    (:b4g4r4x4-unorm ,+virgl-format-b4g4r4x4-unorm+ 2)
    (:x24s8-uint ,+virgl-format-x24s8-uint+ 4)
    (:s8x24-uint ,+virgl-format-s8x24-uint+ 4)
    (:b2g3r3-unorm ,+virgl-format-b2g3r3-unorm+ 8)

    (:l16a16-unorm ,+virgl-format-l16a16-unorm+ 4)
    (:a16-unorm ,+virgl-format-a16-unorm+ 2)

    (:a8-snorm ,+virgl-format-a8-snorm+ 1)
    (:l8-snorm ,+virgl-format-l8-snorm+ 1)
    (:l8a8-snorm ,+virgl-format-l8a8-snorm+ 2)

    (:a16-snorm ,+virgl-format-a16-snorm+ 2)
    (:l16-snorm ,+virgl-format-l16-snorm+ 2)
    (:l16a16-snorm ,+virgl-format-l16a16-snorm+ 4)

    (:a16-float ,+virgl-format-a16-float+ 2)
    (:l16-float ,+virgl-format-l16-float+ 2)
    (:l16a16-float ,+virgl-format-l16a16-float+ 4)

    (:a32-float ,+virgl-format-a32-float+ 4)
    (:l32-float ,+virgl-format-l32-float+ 4)
    (:l32a32-float ,+virgl-format-l32a32-float+ 8)

    (:r8-uint ,+virgl-format-r8-uint+ 1)
    (:r8g8-uint ,+virgl-format-r8g8-uint+ 2)
    (:r8g8b8-uint ,+virgl-format-r8g8b8-uint+ 3)
    (:r8g8b8a8-uint ,+virgl-format-r8g8b8a8-uint+ 4)

    (:r8-sint ,+virgl-format-r8-sint+ 1)
    (:r8g8-sint ,+virgl-format-r8g8-sint+ 2)
    (:r8g8b8-sint ,+virgl-format-r8g8b8-sint+ 3)
    (:r8g8b8a8-sint ,+virgl-format-r8g8b8a8-sint+ 4)

    (:r16-uint ,+virgl-format-r16-uint+ 2)
    (:r16g16-uint ,+virgl-format-r16g16-uint+ 4)
    (:r16g16b16-uint ,+virgl-format-r16g16b16-uint+ 6)
    (:r16g16b16a16-uint ,+virgl-format-r16g16b16a16-uint+ 8)

    (:r16-sint ,+virgl-format-r16-sint+ 2)
    (:r16g16-sint ,+virgl-format-r16g16-sint+ 4)
    (:r16g16b16-sint ,+virgl-format-r16g16b16-sint+ 6)
    (:r16g16b16a16-sint ,+virgl-format-r16g16b16a16-sint+ 8)
    (:r32-uint ,+virgl-format-r32-uint+ 2)
    (:r32g32-uint ,+virgl-format-r32g32-uint+ 4)
    (:r32g32b32-uint ,+virgl-format-r32g32b32-uint+ 6)
    (:r32g32b32a32-uint ,+virgl-format-r32g32b32a32-uint+ 8)

    (:r32-sint ,+virgl-format-r32-sint+ 4)
    (:r32g32-sint ,+virgl-format-r32g32-sint+ 8)
    (:r32g32b32-sint ,+virgl-format-r32g32b32-sint+ 12)
    (:r32g32b32a32-sint ,+virgl-format-r32g32b32a32-sint+ 16)

    (:a8-uint ,+virgl-format-a8-uint+ 1)
    (:l8-uint ,+virgl-format-l8-uint+ 1)
    (:l8a8-uint ,+virgl-format-l8a8-uint+ 2)

    (:a8-sint ,+virgl-format-a8-sint+ 1)
    (:l8-sint ,+virgl-format-l8-sint+ 1)
    (:l8a8-sint ,+virgl-format-l8a8-sint+ 2)

    (:a16-uint ,+virgl-format-a16-uint+ 2)
    (:l16-uint ,+virgl-format-l16-uint+ 2)
    (:l16a16-uint ,+virgl-format-l16a16-uint+ 4)

    (:a16-sint ,+virgl-format-a16-sint+ 2)
    (:l16-sint ,+virgl-format-l16-sint+ 2)
    (:l16a16-sint ,+virgl-format-l16a16-sint+ 4)

    (:a32-uint ,+virgl-format-a32-uint+ 4)
    (:l32-uint ,+virgl-format-l32-uint+ 4)
    (:l32a32-uint ,+virgl-format-l32a32-uint+ 8)

    (:a32-sint ,+virgl-format-a32-sint+ 4)
    (:l32-sint ,+virgl-format-l32-sint+ 4)
    (:l32a32-sint ,+virgl-format-l32a32-sint+ 8)

    (:b10g10r10a2-uint ,+virgl-format-b10g10r10a2-uint+ 4)
    (:r8g8b8x8-snorm ,+virgl-format-r8g8b8x8-snorm+ 4)

    (:r8g8b8x8-srgb ,+virgl-format-r8g8b8x8-srgb+ 4)

    (:r8g8b8x8-uint ,+virgl-format-r8g8b8x8-uint+ 4)
    (:r8g8b8x8-sint ,+virgl-format-r8g8b8x8-sint+ 4)
    (:b10g10r10x2-unorm ,+virgl-format-b10g10r10x2-unorm+ 4)
    (:r16g16b16x16-unorm ,+virgl-format-r16g16b16x16-unorm+ 8)
    (:r16g16b16x16-snorm ,+virgl-format-r16g16b16x16-snorm+ 8)
    (:r16g16b16x16-float ,+virgl-format-r16g16b16x16-float+ 8)
    (:r16g16b16x16-uint ,+virgl-format-r16g16b16x16-uint+ 8)
    (:r16g16b16x16-sint ,+virgl-format-r16g16b16x16-sint+ 8)

    (:r10g10b10a2-uint ,+virgl-format-r10g10b10a2-uint+ 4)

    (:bptc-rgba-unorm ,+virgl-format-bptc-rgba-unorm+ nil)
    (:bptc-srgba ,+virgl-format-bptc-srgba+ nil)
    (:bptc-rgb-float ,+virgl-format-bptc-rgb-float+ nil)
    (:bptc-rgb-ufloat ,+virgl-format-bptc-rgb-ufloat+ nil)

    (:r10g10b10x2-unorm ,+virgl-format-r10g10b10x2-unorm+ 4)
    (:a4b4g4r4-unorm ,+virgl-format-a4b4g4r4-unorm+ 2)))

(defun supported-texture-formats (caps accessor)
  (loop
     for (name encoding) in *texture-formats*
     when (funcall accessor caps encoding)
     collect name))

(defun supported-sampler-formats (caps)
  (supported-texture-formats caps #'caps-sampler))

(defun supported-render-texture-formats (caps)
  (supported-texture-formats caps #'caps-render))

(defun supported-depthstencil-formats (caps)
  (supported-texture-formats caps #'caps-depthstencil))

(defun supported-vertexbuffer-formats (caps)
  (supported-texture-formats caps #'caps-vertexbuffer))

(defun texture-format-width (texture-format)
  (loop
     for (name encoding width) in *texture-formats*
     when (eql name texture-format)
     do
       (when (not width)
         (error "Texture format ~S has unknown width. Compressed format?" texture-format))
       (return width)
     finally (error "Unknown texture-format ~S" texture-format)))

(defun encode-texture-format (texture-format)
  (loop
     for (name encoding) in *texture-formats*
     when (eql name texture-format)
     do (return encoding)
     finally (error "Unknown texture-format ~S" texture-format)))

(defun encode-pipe-bind (bind)
  (ecase bind
    (:depth/stencil +pipe-bind-depth-stencil+) ; create-surface
    (:render-target +pipe-bind-render-target+) ; create-surface
    (:blendable +pipe-bind-blendable+) ; create-surface
    (:sampler-view +pipe-bind-sampler-view+) ; create-sampler-view
    (:vertex-buffer +pipe-bind-vertex-buffer+) ; set-vertex-buffers
    (:index-buffer +pipe-bind-index-buffer+) ; draw-elements
    (:constant-buffer +pipe-bind-constant-buffer+) ; set-constant-buffer
    (:display-target +pipe-bind-display-target+) ; flush-front-buffer
    (:transfer-write +pipe-bind-transfer-write+) ; transfer-map
    (:transfer-read +pipe-bind-transfer-read+) ; transfer-map
    (:stream-output +pipe-bind-stream-output+) ; set-stream-output-buffers
    (:cursor +pipe-bind-cursor+) ; mouse cursor
    (:custom +pipe-bind-custom+) ; state-tracker/winsys usages
    (:global +pipe-bind-global+) ; set-global-binding
    (:shader-resource +pipe-bind-shader-resource+) ; set-shader-resources
    (:compute-resource +pipe-bind-compute-resource+) ; set-compute-resources
    (:command-args-buffer +pipe-bind-command-args-buffer+) ; pipe-draw-info.indirect
    (:scanout +pipe-bind-scanout+)
    (:shared +pipe-bind-shared+) ; get-texture-handle ???
    (:linear +pipe-bind-linear+)))

(defun encode-pipe-binds (binds)
  (when (not (listp binds))
    (setf binds (list binds)))
  (let ((result 0))
    (dolist (bind binds result)
      (setf result (logior (encode-pipe-bind bind) result)))))

(defun create-resource (virgl context id pipe-target texture-format pipe-binds width height depth array-size last-level nr-samples flags)
  (multiple-value-bind (successp error)
      (gpu:virtio-gpu-resource-create-3d
       (virgl-gpu virgl)
       id
       (encode-pipe-target pipe-target)
       (if texture-format
           (encode-texture-format texture-format)
           0)
       (encode-pipe-binds pipe-binds)
       width height depth
       array-size last-level nr-samples
       flags)
    (when (not successp)
      (simple-virgl-error virgl context "Resource creation failed: ~D" error))
    (values)))

(defclass buffer (resource) ())
(defclass vertex-buffer (buffer) ())
(defclass index-buffer (buffer) ())

(defun make-buffer-1 (context class length bind initargs)
  (let* ((virgl (virgl context))
         (buffer (apply 'make-instance class
                        :virgl virgl
                        :context context
                        initargs)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let ((id (allocate-resource-id virgl)))
        (setf (slot-value buffer '%id) id)
        (create-resource virgl context id :buffer nil bind
                         length 1 1 1 0 0 0)
        (setf (gethash id (virgl-resources virgl)) buffer)
        ;; Create a dma-buffer to back this resource.
        (let ((dma-buffer (sup:make-dma-buffer length :name buffer)))
          (setf (slot-value buffer '%dma-buffer) dma-buffer)
          ;; Attach it to the resource.
          (multiple-value-bind (successp error)
              (gpu:virtio-gpu-resource-attach-backing
               (virgl-gpu virgl) id
               1
               (sup:dma-buffer-physical-address dma-buffer)
               length)
            (when (not successp)
              (simple-virgl-error
               virgl context
               "Unable to attach backing memory to resource: ~D" error))))
        ;; Associate the resource with the virgl context.
        (multiple-value-bind (successp error)
            (gpu:virtio-gpu-attach-resource
             (virgl-gpu virgl) id
             :context +virgl-gpu-context+)
          (when (not successp)
            (simple-virgl-error
             virgl context
             "Unable to attach resource to virgl context: ~D" error)))))
    buffer))

(defmacro with-resource ((variable resource) &body body)
  (let ((resource-sym (gensym "RESOURCE")))
    `(let ((,resource-sym ,resource))
       (unwind-protect
            (let ((,variable ,resource-sym)) ,@body)
         (destroy ,resource-sym)))))

(defmacro with-resources (resource-bindings &body body)
  ;; Ensure that bindings are covered by U-P as soon as they
  ;; are created, so errors in other init-forms will not cause
  ;; them to leak.
  (let ((resource-syms (loop for (var val) in resource-bindings
                          collect (gensym "RESOURCE"))))
    (labels ((frob (bindings syms)
               (cond ((endp bindings)
                      `(let ,(loop
                                for (var val) in resource-bindings
                                for sym in resource-syms
                                collect (list var sym))
                         ,@body))
                     (t
                      `(let ((,(first syms) ,(second (first bindings))))
                         (unwind-protect
                              ,(frob (rest bindings) (rest syms))
                           (destroy ,(first syms))))))))
      (frob resource-bindings resource-syms))))

(defun make-vertex-buffer (context length &rest initargs &key name)
  (declare (ignore name))
  (make-buffer-1 context 'vertex-buffer length :vertex-buffer initargs))

(defun make-index-buffer (context length &rest initargs &key name)
  (declare (ignore name))
  (make-buffer-1 context 'index-buffer length :index-buffer initargs))

(defgeneric transfer-to-gpu (resource &key))
(defgeneric transfer-from-gpu (resource &key))

(defmethod transfer-to-gpu ((buffer buffer) &key)
  (let ((virgl (virgl buffer)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (multiple-value-bind (successp error)
          (gpu:virtio-gpu-transfer-to-host-3d
           (virgl-gpu virgl)
           0 0 0
           (sup:dma-buffer-length (resource-dma-buffer buffer)) 1 1
           0
           (resource-id buffer)
           0 0 0
           :context +virgl-gpu-context+)
        (when (not successp)
          (simple-virgl-error
           virgl (context buffer)
           "Unable to transfer resource data to GPU: ~D" error))))))

(defclass texture (resource)
  ((%format :initarg :format :reader texture-format)
   (%render-target :initform nil :initarg :render-target :reader texture-render-target)
   (%depth/stencil :initform nil :initarg :depth/stencil :reader texture-depth/stencil)))

(defgeneric width (texture))
(defgeneric height (texture)
  (:method ((texture texture)) 1))
(defgeneric depth (texture)
  (:method ((texture texture)) 1))

(defclass texture-1d (texture)
  ((%width :initarg :width :initform 0 :reader width)))

(defmethod print-object ((instance texture-1d) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A ~D" (texture-format instance)
            (width instance))))

(defclass texture-2d (texture)
  ((%width :initarg :width :initform 0 :reader width)
   (%height :initarg :height :initform 0 :reader height)))

(defmethod print-object ((instance texture-2d) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A ~Dx~D" (texture-format instance)
            (width instance) (height instance))))

(defclass texture-3d (texture)
  ((%width :initarg :width :initform 0 :reader width)
   (%height :initarg :height :initform 0 :reader height)
   (%depth :initarg :depth :initform 0 :reader depth)))

(defmethod print-object ((instance texture-3d) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A ~Dx~Dx~D" (texture-format instance)
            (width instance) (height instance) (depth instance))))

(defclass scanout (texture-2d) ())

(defun scanout-flush (scanout x y w h)
  (let ((virgl (virgl scanout)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (multiple-value-bind (successp error)
          (gpu:virtio-gpu-resource-flush
           (virgl-gpu virgl)
           x y w h
           (resource-id scanout))
        (when (not successp)
          (simple-virgl-error
           virgl (context scanout)
           "Unable to flush scanout: ~D" error))))))

(defun virgl-scanout (virgl &key (index 0))
  "Return the scanout texture associated with the specified scanout."
  (ecase index
    (0 (virgl-%scanout virgl))))

(defun make-texture (context format dimensions &key name render-target depth/stencil host-only)
  (encode-texture-format format) ; check format is valid
  (let* ((rank (length dimensions))
         (virgl (virgl context))
         (total-size (reduce #'* dimensions))
         (texture (ecase rank
                    (1 (make-instance 'texture-1d :virgl virgl :context context :name name
                                      :format format
                                      :render-target render-target
                                      :depth/stencil depth/stencil
                                      :width (first dimensions)))
                    (2 (make-instance 'texture-2d :virgl virgl :context context :name name
                                      :format format
                                      :render-target render-target
                                      :depth/stencil depth/stencil
                                      :width (first dimensions)
                                      :height (second dimensions)))
                    (3 (make-instance 'texture-3d :virgl virgl :context context :name name
                                      :format format
                                      :render-target render-target
                                      :depth/stencil depth/stencil
                                      :width (first dimensions)
                                      :height (second dimensions)
                                      :depth (third dimensions))))))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let ((id (allocate-resource-id virgl)))
        (setf (slot-value texture '%id) id)
        (create-resource virgl context id
                         (ecase rank
                           (1 :texture-1d)
                           (2 :texture-2d)
                           (3 :texture-3d))
                         format
                         (list*
                          :sampler-view
                          (append (if depth/stencil (list :depth/stencil) nil)
                                  (if render-target (list :render-target) nil)))
                         (width texture) (height texture) (depth texture)
                         1 0 0
                         ;; Without this everything gets rendered inside out???
                         +virgl-resource-y-0-top+)
        (setf (gethash id (virgl-resources virgl)) texture)
        ;; Create a dma-buffer to back this resource, if requested.
        (cond (host-only
               (setf (slot-value texture '%dma-buffer) nil))
              (t
               ;; TODO: Discontiguous buffers.
               (let ((dma-buffer (sup:make-dma-buffer
                                  (* total-size (texture-format-width format))
                                  :name texture :contiguous t)))
                 (setf (slot-value texture '%dma-buffer) dma-buffer)
                 ;; Attach it to the resource.
                 (multiple-value-bind (successp error)
                     (gpu:virtio-gpu-resource-attach-backing
                      (virgl-gpu virgl) id
                      1
                      (sup:dma-buffer-physical-address dma-buffer)
                      (sup:dma-buffer-length dma-buffer))
                   (when (not successp)
                     (simple-virgl-error
                      virgl context
                      "Unable to attach backing memory to resource: ~D" error))))))
        ;; Associate the resource with the virgl context.
        (multiple-value-bind (successp error)
            (gpu:virtio-gpu-attach-resource
             (virgl-gpu virgl) id
             :context +virgl-gpu-context+)
          (when (not successp)
            (simple-virgl-error
             virgl context
             "Unable to attach resource to virgl context: ~D" error)))))
    texture))

(defmethod transfer-to-gpu ((texture texture) &key)
  (let ((virgl (virgl texture)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (multiple-value-bind (successp error)
          (gpu:virtio-gpu-transfer-to-host-3d
           (virgl-gpu virgl)
           0 0 0
           (width texture) (height texture) (depth texture)
           0
           (resource-id texture)
           0 0 0
           :context +virgl-gpu-context+)
        (when (not successp)
          (simple-virgl-error
           virgl (context texture)
           "Unable to transfer resource data to GPU: ~D" error))))))

(defmethod transfer-from-gpu ((texture texture) &key)
  (let ((virgl (virgl texture)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (multiple-value-bind (successp error)
          (gpu:virtio-gpu-transfer-from-host-3d
           (virgl-gpu virgl)
           0 0 0
           (width texture) (height texture) (depth texture)
           0
           (resource-id texture)
           0 0 0
           :context +virgl-gpu-context+)
        (when (not successp)
          (simple-virgl-error
           virgl (context texture)
           "Unable to transfer resource data from GPU: ~D" error))))))

(defun make-texture-2d-from-gui-surface (context surface &key name render-target)
  (let* ((texture (make-texture context
                               (ecase (gui:surface-format surface)
                                 (:argb32 :b8g8r8a8-unorm))
                               (list (gui:surface-width surface)
                                     (gui:surface-height surface))
                               :name name
                               :render-target render-target))
         (surface-row-major-pixels (make-array (* (gui:surface-height surface)
                                                  (gui:surface-width surface))
                                               :displaced-to (gui:surface-pixels surface)))
         (dma-buf (resource-dma-buffer texture))
         (texture-data (make-array (* (gui:surface-height surface)
                                      (gui:surface-width surface))
                                  :element-type '(unsigned-byte 32)
                                  :memory dma-buf)))
    (replace texture-data surface-row-major-pixels)
    (transfer-to-gpu texture)
    texture))

(defun compute-blit-info-dest-src (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Clamp parameters to array boundaries, return the stride of both arrays and their undisplaced, non-complex base arrays."
  (let ((from-width (width from-array))
        (from-height (height from-array))
        (from-offset 0)
        (to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0))
        (to-offset 0))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (decf from-row to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (decf from-col to-col)
      (setf to-col 0))
    ;; Clamp from row/column.
    (when (< from-row 0)
      (incf nrows from-row)
      (decf to-row from-row)
      (setf from-row 0))
    (when (< from-col 0)
      (incf ncols from-col)
      (decf to-col from-col)
      (setf from-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (max (min nrows (- to-height to-row) (- from-height from-row)) 0))
    (setf ncols (max (min ncols (- to-width to-col) (- from-width from-col)) 0))
    ;; Undisplace displaced arrays
    (multiple-value-bind (to-displaced-to to-displaced-offset)
        (array-displacement to-array)
      (when to-displaced-to
        (when (integerp to-displaced-to)
          (error "Memory arrays not supported"))
        (setf to-array to-displaced-to
              to-offset to-displaced-offset)))
    (incf from-offset (+ (* from-row from-width) from-col))
    (incf to-offset (+ (* to-row to-width) to-col))
    (values nrows ncols
            from-offset from-width
            (if (mezzano.internals::%simple-1d-array-p to-array)
                to-array
                (mezzano.internals::%complex-array-storage to-array))
            to-offset to-width)))

(defun %bitblt-line (to to-offset ncols from-address from-offset)
  (declare (optimize speed (safety 0) (debug 1))
           (type fixnum to-offset ncols from-offset)
           (type (simple-array (unsigned-byte 32) (*)) to))
  (loop for i fixnum below ncols do
       (setf (aref to to-offset) (mezzano.internals::memref-unsigned-byte-32 from-address from-offset))
       (incf to-offset)
       (incf from-offset)))

(defun copy-texture-2d-to-gui-surface (width height source source-x source-y dest dest-x dest-y)
  (check-type source texture-2d)
  (check-type dest gui:surface)
  (multiple-value-bind (nrows ncols from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src height width
                                  source source-y source-x
                                  (gui:surface-pixels dest) dest-y dest-x)
    (assert (typep to '(simple-array (unsigned-byte 32) (*))))
    (let ((from (sup:dma-buffer-virtual-address
                 (resource-dma-buffer source))))
      (when (> ncols 0)
        ;; Everything is upside down because of the +virgl-resource-y-0-top+
        ;; flag. Correct for this.
        (setf from-offset (* from-stride nrows))
        (dotimes (y nrows)
          (decf from-offset from-stride)
          (%bitblt-line to to-offset
                        ncols
                        from from-offset)
          (incf to-offset to-stride))))))

(defclass object ()
  ((%context :initarg :context :reader context)
   (%name :initarg :name :accessor name)
   (%id :initarg :id :reader object-id))
  (:default-initargs :name nil))

(define-condition object-ids-exhausted-error (virgl-error)
  ())

(defun allocate-object-id (context)
  (when (> (hash-table-count (context-objects context))
           +virgl-max-object-id+)
    (error 'object-ids-exhausted-error
           :virgl (virgl context) :context context))
  (loop
     (let ((id (context-next-object-id context)))
       (cond ((>= id +virgl-max-object-id+)
              (setf id (setf (context-next-object-id context) 1)))
             (t
              (incf (context-next-object-id context))))
       (when (not (gethash id (context-objects context)))
         (return id)))))

(defmacro with-object ((variable object) &body body)
  (let ((object-sym (gensym "OBJECT")))
    `(let ((,object-sym ,object))
       (unwind-protect
            (let ((,variable ,object-sym)) ,@body)
         (destroy ,object-sym)))))

(defclass shader (object)
  ((%source :initarg :source :reader shader-source)))

(defclass vertex-shader (shader) ())
(defclass fragment-shader (shader) ())

(defun encode-shader-type (shader-type)
  (ecase shader-type
    (:vertex +pipe-shader-vertex+)
    (:fragment +pipe-shader-fragment+)
    (:geometry +pipe-shader-geometry+)
    (:tess-ctrl +pipe-shader-tess-ctrl+)
    (:tess-eval +pipe-shader-tess-eval+)
    (:compute +pipe-shader-compute+)))

(defun make-shader (context processor source &key name)
  (multiple-value-bind (tgsi-text n-tokens)
      (mezzano.gui.virgl.tgsi:assemble processor source)
    (let ((virgl (virgl context)))
      (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
        (let* ((id (allocate-object-id context))
               (cmd-buf (encode-create-shader
                         id
                         (encode-shader-type processor)
                         tgsi-text n-tokens
                         nil nil)))
          (encode-set-sub-ctx cmd-buf (context-id context))
          (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
          (setf (gethash id (context-objects context))
                (make-instance (ecase processor
                                 (:vertex 'vertex-shader)
                                 (:fragment 'fragment-shader))
                               :context context
                               :name name
                               :id id
                               :source source)))))))

(defclass surface (object)
  ((%texture :initarg :texture :reader surface-texture)))

(defun make-surface (context texture &key name (format (texture-format texture)) (first-layer 0) (last-layer 0) (level 0))
  (check-type texture texture)
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let* ((id (allocate-object-id context))
             (cmd-buf (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
        (encode-set-sub-ctx cmd-buf (context-id context))
        (encode-create-surface
         cmd-buf id (resource-id texture)
         (encode-texture-format format)
         first-layer last-layer level)
        (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
        (setf (gethash id (context-objects context))
              (make-instance 'surface
                             :context context
                             :name name
                             :id id
                             :texture texture))))))

(defclass vertex-elements (object)
  ((%elements :initarg :elements :reader vertex-elements-elements)))

(defun make-vertex-elements (context name &rest elements)
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let* ((id (allocate-object-id context))
             (cmd-buf (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
        (encode-set-sub-ctx cmd-buf (context-id context))
        (apply #'encode-create-vertex-elements
         cmd-buf id elements)
        (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
        (setf (gethash id (context-objects context))
              (make-instance 'vertex-elements
                             :context context
                             :name name
                             :id id
                             :elements elements))))))

(defclass blend (object)
  ((%logic-op :initarg :logic-op :reader blend-logic-op)
   (%dither :initarg :dither :reader blend-dither)
   (%alpha-to-coverage :initarg :alpha-to-coverage :reader blend-alpha-to-coverage)
   (%alpha-to-one :initarg :alpha-to-one :reader blend-alpha-to-one)
   (%blend-enable :initarg :blend-enable :reader blend-blend-enable)
   (%rgb-func :initarg :rgb-func :reader blend-rgb-func)
   (%rgb-src-factor :initarg :rgb-src-factor :reader blend-rgb-src-factor)
   (%rgb-dst-factor :initarg :rgb-dst-factor :reader blend-rgb-dst-factor)
   (%alpha-func :initarg :alpha-func :reader blend-alpha-func)
   (%alpha-src-factor :initarg :alpha-src-factor :reader blend-alpha-src-factor)
   (%alpha-dst-factor :initarg :alpha-dst-factor :reader blend-alpha-dst-factor)
   (%colormask :initarg :colormask :reader blend-colormask)))

(defun encode-logic-op (logic-op)
  (ecase logic-op
    (:clear +pipe-logicop-clear+)
    (:nor +pipe-logicop-nor+)
    (:and-inverted +pipe-logicop-and-inverted+)
    (:copy-inverted +pipe-logicop-copy-inverted+)
    (:and-reverse +pipe-logicop-and-reverse+)
    (:invert +pipe-logicop-invert+)
    (:xor +pipe-logicop-xor+)
    (:nand +pipe-logicop-nand+)
    (:and +pipe-logicop-and+)
    (:equiv +pipe-logicop-equiv+)
    (:noop +pipe-logicop-noop+)
    (:or-inverted +pipe-logicop-or-inverted+)
    (:copy +pipe-logicop-copy+)
    (:or-reverse +pipe-logicop-or-reverse+)
    (:or +pipe-logicop-or+)
    (:set +pipe-logicop-set+)))

(defun encode-colormask (colormask)
  (case colormask
    (:rgba +pipe-mask-rgba+)
    (t
     (when (not (listp colormask))
       (setf colormask (list colormask)))
     (let ((mask 0))
       (dolist (entry colormask)
         (setf mask (logior mask
                            (ecase entry
                              (:r +pipe-mask-r+)
                              (:g +pipe-mask-g+)
                              (:b +pipe-mask-b+)
                              (:a +pipe-mask-a+)))))
       mask))))

(defun encode-blend-func (blend-func)
  (ecase blend-func
    (:add +pipe-blend-add+)
    (:subtract +pipe-blend-subtract+)
    (:reverse-subtract +pipe-blend-reverse-subtract+)
    (:min +pipe-blend-min+)
    (:max +pipe-blend-max+)))

(defun encode-blend-factor (blend-factor)
  (ecase blend-factor
    (:one +pipe-blendfactor-one+)
    (:src-color +pipe-blendfactor-src-color+)
    (:src-alpha +pipe-blendfactor-src-alpha+)
    (:dst-alpha +pipe-blendfactor-dst-alpha+)
    (:dst-color +pipe-blendfactor-dst-color+)
    (:src-alpha-saturate +pipe-blendfactor-src-alpha-saturate+)
    (:const-color +pipe-blendfactor-const-color+)
    (:const-alpha +pipe-blendfactor-const-alpha+)
    (:src1-color +pipe-blendfactor-src1-color+)
    (:src1-alpha +pipe-blendfactor-src1-alpha+)
    (:zero +pipe-blendfactor-zero+)
    (:inv-src-color +pipe-blendfactor-inv-src-color+)
    (:inv-src-alpha +pipe-blendfactor-inv-src-alpha+)
    (:inv-dst-alpha +pipe-blendfactor-inv-dst-alpha+)
    (:inv-dst-color +pipe-blendfactor-inv-dst-color+)
    (:inv-const-color +pipe-blendfactor-inv-const-color+)
    (:inv-const-alpha +pipe-blendfactor-inv-const-alpha+)
    (:inv-src1-color +pipe-blendfactor-inv-src1-color+)
    (:inv-src1-alpha +pipe-blendfactor-inv-src1-alpha+)))

(defun make-blend (context &key name logic-op dither alpha-to-coverage alpha-to-one blend-enable rgb-func rgb-src-factor rgb-dst-factor alpha-func alpha-src-factor alpha-dst-factor colormask)
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let* ((id (allocate-object-id context))
             (cmd-buf (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
        (encode-set-sub-ctx cmd-buf (context-id context))
        (encode-create-blend
         cmd-buf id
         logic-op dither
         alpha-to-coverage alpha-to-one
         (if logic-op (encode-logic-op logic-op) 0)
         blend-enable
         (if rgb-func (encode-blend-func rgb-func) 0)
         (if rgb-src-factor (encode-blend-factor rgb-src-factor) 0)
         (if rgb-dst-factor (encode-blend-factor rgb-dst-factor) 0)
         (if alpha-func (encode-blend-func alpha-func) 0)
         (if alpha-src-factor (encode-blend-factor alpha-src-factor) 0)
         (if alpha-dst-factor (encode-blend-factor alpha-dst-factor) 0)
         (encode-colormask colormask))
        (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
        (setf (gethash id (context-objects context))
              (make-instance 'blend
                             :context context
                             :name name
                             :id id
                             :logic-op logic-op
                             :dither dither
                             :alpha-to-coverage alpha-to-coverage
                             :alpha-to-one alpha-to-one
                             :blend-enable blend-enable
                             :rgb-func rgb-func
                             :rgb-src-factor rgb-src-factor
                             :rgb-dst-factor rgb-dst-factor
                             :alpha-func alpha-func
                             :alpha-src-factor alpha-src-factor
                             :alpha-dst-factor alpha-dst-factor
                             :colormask colormask))))))

(deftype polygon-mode ()
  `(member :fill :line :point))

(defun encode-polygon-mode (polygon-mode)
  (ecase polygon-mode
    (:fill +pipe-polygon-mode-fill+)
    (:line +pipe-polygon-mode-line+)
    (:point +pipe-polygon-mode-point+)))

(deftype polygon-face ()
  `(member :none :front :back :front-and-back))

(defun encode-polygon-face (polygon-face)
  (ecase polygon-face
    (:none +pipe-face-none+)
    (:front +pipe-face-front+)
    (:back +pipe-face-back+)
    (:front-and-back +pipe-face-front-and-back+)))

(deftype sprite-coord ()
  `(member :upper-left :lower-left))

(defun encode-sprite-coord (sprite-coord)
  (ecase sprite-coord
    (:upper-left +pipe-sprite-coord-upper-left+)
    (:lower-left +pipe-sprite-coord-lower-left+)))

(defclass rasterizer (object)
  ((%flatshade :initform nil :initarg :flatshade :reader rasterizer-flatshade)
   (%light-twoside :initform nil :initarg :light-twoside :reader rasterizer-light-twoside)
   (%clamp-vertex-color :initform nil :initarg :clamp-vertex-color :reader rasterizer-clamp-vertex-color)
   (%clamp-fragment-color :initform nil :initarg :clamp-fragment-color :reader rasterizer-clamp-fragment-color)
   (%front-ccw :initform nil :initarg :front-ccw :reader rasterizer-front-ccw)
   (%cull-face :initform :none :initarg :cull-face :reader rasterizer-cull-face :type polygon-face)
   (%fill-front :initform :fill :initarg :fill-front :reader rasterizer-fill-front :type polygon-mode)
   (%fill-back :initform :fill :initarg :fill-back :reader rasterizer-fill-back :type polygon-mode)
   (%offset-point :initform nil :initarg :offset-point :reader rasterizer-offset-point)
   (%offset-line :initform nil :initarg :offset-line :reader rasterizer-offset-line)
   (%offset-tri :initform nil :initarg :offset-tri :reader rasterizer-offset-tri)
   (%scissor :initform nil :initarg :scissor :reader rasterizer-scissor)
   (%poly-smooth :initform nil :initarg :poly-smooth :reader rasterizer-poly-smooth)
   (%poly-stipple-enable :initform nil :initarg :poly-stipple-enable :reader rasterizer-poly-stipple-enable)
   (%point-smooth :initform nil :initarg :point-smooth :reader rasterizer-point-smooth)
   (%sprite-coord-mode :initform :upper-left :initarg :sprite-coord-mode :reader rasterizer-sprite-coord-mode :type sprite-coord)
   (%point-quad-rasterization :initform nil :initarg :point-quad-rasterization :reader rasterizer-point-quad-rasterization)
   (%point-size-per-vertex :initform nil :initarg :point-size-per-vertex :reader rasterizer-point-size-per-vertex)
   (%multisample :initform nil :initarg :multisample :reader rasterizer-multisample)
   (%force-persample-interp :initform nil :initarg :force-persample-interp :reader rasterizer-force-persample-interp)
   (%line-smooth :initform nil :initarg :line-smooth :reader rasterizer-line-smooth)
   (%line-stipple-enable :initform nil :initarg :line-stipple-enable :reader rasterizer-line-stipple-enable)
   (%line-last-pixel :initform nil :initarg :line-last-pixel :reader rasterizer-line-last-pixel)
   (%flatshade-first :initform nil :initarg :flatshade-first :reader rasterizer-flatshade-first)
   (%half-pixel-center :initform nil :initarg :half-pixel-center :reader rasterizer-half-pixel-center)
   (%bottom-edge-rule :initform nil :initarg :bottom-edge-rule :reader rasterizer-bottom-edge-rule)
   (%rasterizer-discard :initform nil :initarg :rasterizer-discard :reader rasterizer-rasterizer-discard)
   (%depth-clip :initform nil :initarg :depth-clip :reader rasterizer-depth-clip)
   (%clip-halfz :initform nil :initarg :clip-halfz :reader rasterizer-clip-halfz)
   (%clip-plane-enable :initform 0 :initarg :clip-plane-enable :reader rasterizer-clip-plane-enable :type (unsigned-byte 8))
   (%line-stipple-factor :initform 0 :initarg :line-stipple-factor :reader rasterizer-line-stipple-factor :type (unsigned-byte 8))
   (%line-stipple-pattern :initform 0 :initarg :line-stipple-pattern :reader rasterizer-line-stipple-pattern :type (unsigned-byte 16))
   (%sprite-coord-enable :initform 0 :initarg :sprite-coord-enable :reader rasterizer-sprite-coord-enable :type (unsigned-byte 32))
   (%line-width :initform 1.0 :initarg :line-width :reader rasterizer-line-width  :type single-float)
   (%point-size :initform 1.0 :initarg :point-size :reader rasterizer-point-size :type single-float)
   (%offset-units :initform 0.0 :initarg :offset-units :reader rasterizer-offset-units :type single-float)
   (%offset-scale :initform 0.0 :initarg :offset-scale :reader rasterizer-offset-scale :type single-float)
   (%offset-clamp :initform 0.0 :initarg :offset-clamp :reader rasterizer-offset-clamp :type single-float)))

(defun make-rasterizer (context &rest initargs &key name &allow-other-keys)
  (declare (ignore name))
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let* ((id (allocate-object-id context))
             (cmd-buf (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
             (rasterizer (apply #'make-instance 'rasterizer
                                :context context
                                :id id
                                initargs)))
        (encode-set-sub-ctx cmd-buf (context-id context))
        (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                                  +virgl-object-rasterizer+
                                                  9)
                                    cmd-buf)
        (vector-push-extend-ub32/le id cmd-buf)
        (vector-push-extend-ub32/le
         (logior (ash (if (rasterizer-flatshade rasterizer) 1 0) 0)
                 (ash (if (rasterizer-depth-clip rasterizer) 1 0) 1)
                 (ash (if (rasterizer-clip-halfz rasterizer) 1 0) 2)
                 (ash (if (rasterizer-rasterizer-discard rasterizer) 1 0) 3)
                 (ash (if (rasterizer-flatshade-first rasterizer) 1 0) 4)
                 (ash (if (rasterizer-light-twoside rasterizer) 1 0) 5)
                 (ash (encode-sprite-coord (rasterizer-sprite-coord-mode rasterizer)) 6)
                 (ash (if (rasterizer-point-quad-rasterization rasterizer) 1 0) 7)
                 (ash (encode-polygon-face (rasterizer-cull-face rasterizer)) 8)
                 (ash (encode-polygon-mode (rasterizer-fill-front rasterizer)) 10)
                 (ash (encode-polygon-mode (rasterizer-fill-back rasterizer)) 12)
                 (ash (if (rasterizer-scissor rasterizer) 1 0) 14)
                 (ash (if (rasterizer-front-ccw rasterizer) 1 0) 15)
                 (ash (if (rasterizer-clamp-vertex-color rasterizer) 1 0) 16)
                 (ash (if (rasterizer-clamp-fragment-color rasterizer) 1 0) 17)
                 (ash (if (rasterizer-offset-line rasterizer) 1 0) 18)
                 (ash (if (rasterizer-offset-point rasterizer) 1 0) 19)
                 (ash (if (rasterizer-offset-tri rasterizer) 1 0) 20)
                 (ash (if (rasterizer-poly-smooth rasterizer) 1 0) 21)
                 (ash (if (rasterizer-poly-stipple-enable rasterizer) 1 0) 22)
                 (ash (if (rasterizer-point-smooth rasterizer) 1 0) 23)
                 (ash (if (rasterizer-point-size-per-vertex rasterizer) 1 0) 24)
                 (ash (if (rasterizer-multisample rasterizer) 1 0) 25)
                 (ash (if (rasterizer-line-smooth rasterizer) 1 0) 26)
                 (ash (if (rasterizer-line-stipple-enable rasterizer) 1 0) 27)
                 (ash (if (rasterizer-line-last-pixel rasterizer) 1 0) 28)
                 (ash (if (rasterizer-half-pixel-center rasterizer) 1 0) 29)
                 (ash (if (rasterizer-bottom-edge-rule rasterizer) 1 0) 30)
                 (ash (if (rasterizer-force-persample-interp rasterizer) 1 0) 31))
         cmd-buf)
        (vector-push-extend-single/le (rasterizer-point-size rasterizer) cmd-buf)
        (vector-push-extend-ub32/le (rasterizer-sprite-coord-enable rasterizer) cmd-buf)
        (vector-push-extend-ub32/le (logior (rasterizer-line-stipple-pattern rasterizer)
                                            (ash (rasterizer-line-stipple-factor rasterizer) 16)
                                            (ash (rasterizer-clip-plane-enable rasterizer) 24))
                                    cmd-buf)
        (vector-push-extend-single/le (rasterizer-line-width rasterizer) cmd-buf)
        (vector-push-extend-single/le (rasterizer-offset-units rasterizer) cmd-buf)
        (vector-push-extend-single/le (rasterizer-offset-scale rasterizer) cmd-buf)
        (vector-push-extend-single/le (rasterizer-offset-clamp rasterizer) cmd-buf)
        (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
        (setf (gethash id (context-objects context)) rasterizer)))))

(deftype pipe-function ()
  '(member :never :less :equal :lequal :greater :notequal :gequal :always))

(defun encode-pipe-function (pipe-function)
  (ecase pipe-function
    (:never +pipe-func-never+)
    (:less +pipe-func-less+)
    (:equal +pipe-func-equal+)
    (:lequal +pipe-func-lequal+)
    (:greater +pipe-func-greater+)
    (:notequal +pipe-func-notequal+)
    (:gequal +pipe-func-gequal+)
    (:always +pipe-func-always+)))

(deftype stencil-op ()
  '(member :keep :zero :replace :incr :decr :incr-wrap :decr-wrap :invert))

(defun encode-stencil-op (stencil-op)
  (ecase stencil-op
    (:keep +pipe-stencil-op-keep+)
    (:zero +pipe-stencil-op-zero+)
    (:replace +pipe-stencil-op-replace+)
    (:incr +pipe-stencil-op-incr+)
    (:decr +pipe-stencil-op-decr+)
    (:incr-wrap +pipe-stencil-op-incr-wrap+)
    (:decr-wrap +pipe-stencil-op-decr-wrap+)
    (:invert +pipe-stencil-op-invert+)))

(defclass dsa (object)
  ((%depth-enabled :initform nil :initarg :depth-enabled :reader dsa-depth-enabled)
   (%depth-writemask :initform nil :initarg :depth-writemask :reader dsa-depth-writemask)
   (%depth-func :initform :never :initarg :depth-func :reader dsa-depth-func :type pipe-function)
   (%alpha-enabled :initform nil :initarg :alpha-enabled :reader dsa-alpha-enabled)
   (%alpha-func :initform :never :initarg :alpha-func :reader dsa-alpha-func :type pipe-function)
   (%alpha-ref :initform 0.0 :initarg :alpha-ref :reader dsa-alpha-ref :type single-float)
   (%stencil-front-enabled :initform nil :initarg :stencil-front-enabled :reader dsa-stencil-front-enabled)
   (%stencil-front-func :initform :never :initarg :stencil-front-func :reader dsa-stencil-front-func :type pipe-function)
   (%stencil-front-fail-op :initform :keep :initarg :stencil-front-fail-op :reader dsa-stencil-front-fail-op :type stencil-op)
   (%stencil-front-zpass-op :initform :keep :initarg :stencil-front-zpass-op :reader dsa-stencil-front-zpass-op :type stencil-op)
   (%stencil-front-zfail-op :initform :keep :initarg :stencil-front-zfail-op :reader dsa-stencil-front-zfail-op :type stencil-op)
   (%stencil-front-valuemask :initform 0 :initarg :stencil-front-valuemask :reader dsa-stencil-front-valuemask :type (unsigned-byte 8))
   (%stencil-front-writemask :initform 0 :initarg :stencil-front-writemask :reader dsa-stencil-front-writemask :type (unsigned-byte 8))
   (%stencil-back-enabled :initform nil :initarg :stencil-back-enabled :reader dsa-stencil-back-enabled)
   (%stencil-back-func :initform :never :initarg :stencil-back-func :reader dsa-stencil-back-func :type pipe-function)
   (%stencil-back-fail-op :initform :keep :initarg :stencil-back-fail-op :reader dsa-stencil-back-fail-op :type stencil-op)
   (%stencil-back-zpass-op :initform :keep :initarg :stencil-back-zpass-op :reader dsa-stencil-back-zpass-op :type stencil-op)
   (%stencil-back-zfail-op :initform :keep :initarg :stencil-back-zfail-op :reader dsa-stencil-back-zfail-op :type stencil-op)
   (%stencil-back-valuemask :initform 0 :initarg :stencil-back-valuemask :reader dsa-stencil-back-valuemask :type (unsigned-byte 8))
   (%stencil-back-writemask :initform 0 :initarg :stencil-back-writemask :reader dsa-stencil-back-writemask :type (unsigned-byte 8))))

(defun encode-stencil-state (enabled func fail-op zpass-op zfail-op valuemask writemask)
  (logior (ash (if enabled 1 0) 0)
          (ash (encode-pipe-function func) 1)
          (ash (encode-stencil-op fail-op) 4)
          (ash (encode-stencil-op zpass-op) 7)
          (ash (encode-stencil-op zfail-op) 10)
          (ash valuemask 13)
          (ash writemask 21)))

(defun make-dsa (context &rest initargs &key name &allow-other-keys)
  (declare (ignore name))
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let* ((id (allocate-object-id context))
             (cmd-buf (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
             (dsa (apply #'make-instance 'dsa
                         :context context
                         :id id
                         initargs)))
        (encode-set-sub-ctx cmd-buf (context-id context))
        (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                                  +virgl-object-dsa+
                                                  5)
                                    cmd-buf)
        (vector-push-extend-ub32/le id cmd-buf)
        (vector-push-extend-ub32/le
         (logior (ash (if (dsa-depth-enabled dsa) 1 0) 0)
                 (ash (if (dsa-depth-writemask dsa) 1 0) 1)
                 (ash (encode-pipe-function (dsa-depth-func dsa)) 2)
                 (ash (if (dsa-alpha-enabled dsa) 1 0) 8)
                 (ash (encode-pipe-function (dsa-alpha-func dsa)) 9))
         cmd-buf)
        (vector-push-extend-ub32/le
         (encode-stencil-state (dsa-stencil-front-enabled dsa)
                               (dsa-stencil-front-func dsa)
                               (dsa-stencil-front-fail-op dsa)
                               (dsa-stencil-front-zpass-op dsa)
                               (dsa-stencil-front-zfail-op dsa)
                               (dsa-stencil-front-valuemask dsa)
                               (dsa-stencil-front-writemask dsa))
         cmd-buf)
        (vector-push-extend-ub32/le
         (encode-stencil-state (dsa-stencil-back-enabled dsa)
                               (dsa-stencil-back-func dsa)
                               (dsa-stencil-back-fail-op dsa)
                               (dsa-stencil-back-zpass-op dsa)
                               (dsa-stencil-back-zfail-op dsa)
                               (dsa-stencil-back-valuemask dsa)
                               (dsa-stencil-back-writemask dsa))
         cmd-buf)
        (vector-push-extend-single/le (dsa-alpha-ref dsa) cmd-buf)
        (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
        (setf (gethash id (context-objects context)) dsa)))))

(deftype tex-wrap ()
  '(member :repeat :clamp :clamp-to-edge :clamp-to-border :mirror-repeat :mirror-clamp :mirror-clamp-to-edge :mirror-clamp-to-border))

(defun encode-tex-wrap (tex-wrap)
  (ecase tex-wrap
    (:repeat +pipe-tex-wrap-repeat+)
    (:clamp +pipe-tex-wrap-clamp+)
    (:clamp-to-edge +pipe-tex-wrap-clamp-to-edge+)
    (:clamp-to-border +pipe-tex-wrap-clamp-to-border+)
    (:mirror-repeat +pipe-tex-wrap-mirror-repeat+)
    (:mirror-clamp +pipe-tex-wrap-mirror-clamp+)
    (:mirror-clamp-to-edge +pipe-tex-wrap-mirror-clamp-to-edge+)
    (:mirror-clamp-to-border +pipe-tex-wrap-mirror-clamp-to-border+)))

(deftype tex-filter ()
  '(member :nearest :linear))

(defun encode-tex-filter (tex-filter)
  (ecase tex-filter
    (:nearest +pipe-tex-filter-nearest+)
    (:linear +pipe-tex-filter-linear+)))

(deftype tex-mipfilter ()
  '(member :nearest :linear :none))

(defun encode-tex-mipfilter (tex-mipfilter)
  (ecase tex-mipfilter
    (:nearest +pipe-tex-mipfilter-nearest+)
    (:linear +pipe-tex-mipfilter-linear+)
    (:none +pipe-tex-mipfilter-none+)))

(deftype tex-compare ()
  '(member :none :r-to-texture))

(defun encode-tex-compare (tex-compare)
  (ecase tex-compare
    (:none +pipe-tex-compare-none+)
    (:r-to-texture +pipe-tex-compare-r-to-texture+)))

(defclass sampler-state (object)
  ((%wrap-s :initform :repeat :initarg :wrap-s :reader sampler-state-wrap-s :type tex-wrap)
   (%wrap-t :initform :repeat :initarg :wrap-t :reader sampler-state-wrap-t :type tex-wrap)
   (%wrap-r :initform :repeat :initarg :wrap-r :reader sampler-state-wrap-r :type tex-wrap)
   (%min-img-filter :initform :nearest :initarg :min-img-filter :reader sampler-state-min-img-filter :type tex-filter)
   (%min-mip-filter :initform :nearest :initarg :min-mip-filter :reader sampler-state-min-mip-filter :type tex-mipfilter)
   (%mag-img-filter :initform :nearest :initarg :mag-img-filter :reader sampler-state-mag-img-filter :type tex-filter)
   (%compare-mode :initform :none :initarg :compare-mode :reader sampler-state-compare-mode :type tex-compare)
   (%compare-func :initform :never :initarg :compare-func :reader sampler-state-compare-func :type pipe-function)
   (%seamless-cube-map :initform nil :initarg :seamless-cube-map :reader sampler-state-seamless-cube-map)
   (%lod-bias :initform 0.0 :initarg :lod-bias :reader sampler-state-lod-bias :type single-float)
   (%min-lod :initform 0.0 :initarg :min-lod :reader sampler-state-min-lod :type single-float)
   (%max-lod :initform 0.0 :initarg :max-lod :reader sampler-state-max-lod :type single-float)))

(defun make-sampler-state (context &rest initargs &key name &allow-other-keys)
  (declare (ignore name))
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let* ((id (allocate-object-id context))
             (cmd-buf (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
             (sampler-state (apply #'make-instance 'sampler-state
                                   :context context
                                   :id id
                                   initargs)))
        (encode-set-sub-ctx cmd-buf (context-id context))
        (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                                  +virgl-object-sampler-state+
                                                  9)
                                    cmd-buf)
        (vector-push-extend-ub32/le id cmd-buf)
        (vector-push-extend-ub32/le
         (logior (ash (encode-tex-wrap (sampler-state-wrap-s sampler-state)) 0)
                 (ash (encode-tex-wrap (sampler-state-wrap-t sampler-state)) 3)
                 (ash (encode-tex-wrap (sampler-state-wrap-r sampler-state)) 6)
                 (ash (encode-tex-filter (sampler-state-min-img-filter sampler-state)) 9)
                 (ash (encode-tex-mipfilter (sampler-state-min-mip-filter sampler-state)) 11)
                 (ash (encode-tex-filter (sampler-state-mag-img-filter sampler-state)) 13)
                 (ash (encode-tex-compare (sampler-state-compare-mode sampler-state)) 15)
                 (ash (encode-pipe-function (sampler-state-compare-func sampler-state)) 16)
                 (ash (if (sampler-state-seamless-cube-map sampler-state) 1 0) 19))
         cmd-buf)
        (vector-push-extend-single/le (sampler-state-lod-bias sampler-state) cmd-buf)
        (vector-push-extend-single/le (sampler-state-min-lod sampler-state) cmd-buf)
        (vector-push-extend-single/le (sampler-state-max-lod sampler-state) cmd-buf)
        ;; These are the border colour fields, but I can't figure out how
        ;; they're supposed to be encoded.
        (vector-push-extend-ub32/le 0 cmd-buf)
        (vector-push-extend-ub32/le 0 cmd-buf)
        (vector-push-extend-ub32/le 0 cmd-buf)
        (vector-push-extend-ub32/le 0 cmd-buf)
        (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
        (setf (gethash id (context-objects context)) sampler-state)))))

(defun encode-swizzle (swizzle)
  (assert (keywordp swizzle))
  (let ((encoded 0)
        (text (string swizzle)))
    (assert (eql (length text) 4))
    (loop
       for i from 0 by 3
       for ch across text
       do
         (setf encoded (logior encoded
                               (ash (ecase ch
                                      (#\R +pipe-swizzle-red+)
                                      (#\G +pipe-swizzle-green+)
                                      (#\B +pipe-swizzle-blue+)
                                      (#\A +pipe-swizzle-alpha+)
                                      (#\1 +pipe-swizzle-one+)
                                      (#\0 +pipe-swizzle-zero+))
                                    i))))
    encoded))

(defclass sampler-view (object)
  ((%texture :initarg :texture :reader sampler-view-texture)
   (%swizzle :initform :xyzw :initarg :swizzle :reader sampler-view-swizzle :type swizzle)))

(defun make-sampler-view (context texture &key name (swizzle :rgba))
  (check-type texture texture)
  (assert (encode-swizzle swizzle))
  (let ((virgl (virgl context)))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let* ((id (allocate-object-id context))
             (cmd-buf (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
             (sampler-view (make-instance 'sampler-view
                                          :context context
                                          :id id
                                          :name name
                                          :texture texture
                                          :swizzle swizzle)))
        (encode-set-sub-ctx cmd-buf (context-id context))
        (vector-push-extend-ub32/le (pack-command +virgl-ccmd-create-object+
                                                  +virgl-object-sampler-view+
                                                  6)
                                    cmd-buf)
        (vector-push-extend-ub32/le id cmd-buf)
        (vector-push-extend-ub32/le (resource-id texture) cmd-buf)
        (vector-push-extend-ub32/le (encode-texture-format (texture-format texture)) cmd-buf)
        ;; Not sure what these are for... Texture layer/level.
        (vector-push-extend-ub32/le #xFFFF0000 cmd-buf)
        (vector-push-extend-ub32/le #xFFFF0000 cmd-buf)
        (vector-push-extend-ub32/le (encode-swizzle swizzle) cmd-buf)
        (virgl-submit-simple-command-buffer-1 virgl cmd-buf)
        (setf (gethash id (context-objects context)) sampler-view)))))

(defclass command-buffer ()
  ((%context :initarg :context :reader context)
   (%name :initarg :name :reader name)
   (%finalized :initform nil :reader command-buffer-finalized)
   (%dma-buffer :initform nil :reader command-buffer-dma-buffer)
   (%data-array :initform (make-array 1024
                                      :element-type '(unsigned-byte 8)
                                      :fill-pointer 0
                                      :adjustable t)
                :reader command-buffer-data-array)))

(defun make-command-buffer (context &key name)
  (let* ((cbuf (make-instance 'command-buffer
                              :context context
                              :name name))
         (data (command-buffer-data-array cbuf)))
    ;; Include a set subcontext command as the first thing.
    (encode-set-sub-ctx data (context-id context))
    cbuf))

(defun check-command-buffer-not-finalized (command-buffer)
  (assert (not (command-buffer-finalized command-buffer))
          (command-buffer)
          "Command buffer must not be finalized!"))

(defun command-buffer-finalize (command-buffer &key optimize)
  "Finalize COMMAND-BUFFER.
Once the command buffer has been finalized, no further commands can
be added. If OPTIMIZE is true, then the buffer will be copied to
dedicated memory where it will be available for faster resubmission.
This should be set for command buffers that are likely to be used
multiple times."
  (when (command-buffer-finalized command-buffer)
    (error "Command buffer ~D already finalized!" command-buffer))
  (let ((data (command-buffer-data-array command-buffer)))
    (when (or optimize
              ;; Limit of VIRTIO-GPU-SUBMIT-3D's internal command buffer.
              (> (length data) 1024))
      ;; Add extra for the GPU header and command size.
      ;; TODO: Figure out how to do SG with virtio.
      ;; FIXME: Reuse any existing dma buffer if it has the right size.
      (let* ((dma-buffer (sup:make-dma-buffer (+ 24 4 (length data))
                                              :name command-buffer
                                              :contiguous t))
             (buf-vec (make-array (sup:dma-buffer-length dma-buffer)
                                  :element-type '(unsigned-byte 8)
                                  :memory dma-buffer)))
        ;; Configure the header.
        (setf (ext:ub32ref/le buf-vec gpu:+virtio-gpu-ctrl-hdr-type+)
              gpu:+virtio-gpu-cmd-submit-3d+)
        (setf (ext:ub32ref/le buf-vec gpu:+virtio-gpu-ctrl-hdr-flags+) 0)
        (setf (ext:ub64ref/le buf-vec gpu:+virtio-gpu-ctrl-hdr-fence-id+) 0)
        (setf (ext:ub32ref/le buf-vec gpu:+virtio-gpu-ctrl-hdr-ctx-id+)
              +virgl-gpu-context+)
        ;; Command size.
        (setf (ext:ub32ref/le data 28) (length data))
        (replace buf-vec data :start1 32)
        (setf (slot-value command-buffer '%dma-buffer) dma-buffer))))
  (setf (slot-value command-buffer '%finalized) t)
  (values))

(defun command-buffer-submit (command-buffer)
  (assert (command-buffer-finalized command-buffer))
  (let ((virgl (virgl (context command-buffer))))
    (sup:with-mutex ((virgl-lock virgl) :resignal-errors virgl-error)
      (let ((dma-buf (command-buffer-dma-buffer command-buffer)))
        (cond (dma-buf
               (error "TODO"))
              (t
               (virgl-submit-simple-command-buffer-1 virgl (command-buffer-data-array command-buffer)))))))
  (values))

(defun command-buffer-reset (command-buffer)
  "Unfinalize COMMAND-BUFFER and remove any commands.
This does not free associated memory, so the command buffer can be
reused without consing."
  ;; Leave the set sub ctx command intact.
  (setf (fill-pointer (command-buffer-data-array command-buffer)) 8
        (slot-value command-buffer '%finalized) nil)
  (values))

(defmethod destroy ((command-buffer command-buffer))
  (when (command-buffer-dma-buffer command-buffer)
    (sup:release-dma-buffer (command-buffer-dma-buffer command-buffer))))

(defconstant +max-framebuffer-color-buffers+ 8)

(defun add-command-set-framebuffer-state (command-buffer depth/stencil-surface &rest color-surfaces)
  (check-command-buffer-not-finalized command-buffer)
  (let ((nr-cbufs (length color-surfaces)))
    (assert (<= nr-cbufs +max-framebuffer-color-buffers+))
    (check-type depth/stencil-surface (or null surface))
    (apply #'encode-set-framebuffer-state
           (command-buffer-data-array command-buffer)
           (if depth/stencil-surface
               (object-id depth/stencil-surface)
               0)
           (loop
              for surf in color-surfaces
              collect
                (cond (surf
                       (check-type surf surface)
                       (object-id surf))
                      (t 0)))))
  (values))

(defun add-command-set-viewport-state (command-buffer
                                       scale0 scale1 scale2
                                       translate0 translate1 translate2)
  (check-command-buffer-not-finalized command-buffer)
  (encode-set-viewport-state
   (command-buffer-data-array command-buffer)
   scale0 scale1 scale2
   translate0 translate1 translate2)
  (values))

(defun add-command-clear (command-buffer buffers color0 color1 color2 color3 depth stencil)
  (check-command-buffer-not-finalized command-buffer)
  (let ((encoded-buffers 0))
    (when (not (listp buffers))
      (setf buffers (list buffers)))
    (dolist (buffer buffers)
      (setf encoded-buffers (logior encoded-buffers
                                    (ecase buffer
                                      (:color +pipe-clear-color+)
                                      (:color-0 +pipe-clear-color0+)
                                      (:color-1 +pipe-clear-color1+)
                                      (:color-2 +pipe-clear-color2+)
                                      (:color-3 +pipe-clear-color3+)
                                      (:color-4 +pipe-clear-color4+)
                                      (:color-5 +pipe-clear-color5+)
                                      (:color-6 +pipe-clear-color6+)
                                      (:color-7 +pipe-clear-color7+)
                                      (:depth +pipe-clear-depth+)
                                      (:stencil +pipe-clear-stencil+)))))
    (encode-clear (command-buffer-data-array command-buffer)
                  encoded-buffers
                  color0 color1 color2 color3
                  (or depth 0.0d0)
                  (or stencil 0)))
  (values))

(defun add-command-set-vertex-buffers (command-buffer &rest vertex-buffers)
  (check-command-buffer-not-finalized command-buffer)
  (apply #'encode-set-vertex-buffers
         (command-buffer-data-array command-buffer)
         (loop
            for (stride offset vertex-buffer) in vertex-buffers
            do
              (check-type vertex-buffer vertex-buffer)
            collect
              (list stride offset (resource-id vertex-buffer))))
  (values))

(defun add-command-bind-vertex-elements (command-buffer vertex-elements)
  (check-command-buffer-not-finalized command-buffer)
  (check-type vertex-elements vertex-elements)
  (encode-bind-vertex-elements
   (command-buffer-data-array command-buffer)
   (object-id vertex-elements))
  (values))

(defun add-command-bind-shader (command-buffer shader)
  (check-command-buffer-not-finalized command-buffer)
  (check-type shader shader)
  (encode-bind-shader
   (command-buffer-data-array command-buffer)
   (object-id shader)
   (etypecase shader
     (vertex-shader +pipe-shader-vertex+)
     (fragment-shader +pipe-shader-fragment+)))
  (values))

(defun add-command-bind-blend (command-buffer blend)
  (check-command-buffer-not-finalized command-buffer)
  (check-type blend blend)
  (encode-bind-blend
   (command-buffer-data-array command-buffer)
   (object-id blend))
  (values))

(defun add-command-bind-rasterizer (command-buffer rasterizer)
  (check-command-buffer-not-finalized command-buffer)
  (check-type rasterizer rasterizer)
  (let ((cmd-buf (command-buffer-data-array command-buffer)))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-bind-object+
                                              +virgl-object-rasterizer+
                                              1)
                                cmd-buf)
    (vector-push-extend-ub32/le (object-id rasterizer) cmd-buf))
  (values))

(defun add-command-bind-dsa (command-buffer dsa)
  (check-command-buffer-not-finalized command-buffer)
  (check-type dsa dsa)
  (let ((cmd-buf (command-buffer-data-array command-buffer)))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-bind-object+
                                              +virgl-object-dsa+
                                              1)
                                cmd-buf)
    (vector-push-extend-ub32/le (object-id dsa) cmd-buf))
  (values))

(defun add-command-bind-sampler-states (command-buffer shader-type start-slot &rest states)
  (check-command-buffer-not-finalized command-buffer)
  (let ((cmd-buf (command-buffer-data-array command-buffer)))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-bind-sampler-states+
                                              +virgl-object-null+
                                              (+ 2 (length states)))
                                cmd-buf)
    (vector-push-extend-ub32/le (encode-shader-type shader-type) cmd-buf)
    (vector-push-extend-ub32/le start-slot cmd-buf)
    (dolist (sampler-state states)
      (check-type sampler-state sampler-state)
      (vector-push-extend-ub32/le (object-id sampler-state) cmd-buf)))
  (values))

(defun add-command-set-sampler-views (command-buffer shader-type start-slot &rest views)
  (check-command-buffer-not-finalized command-buffer)
  (let ((cmd-buf (command-buffer-data-array command-buffer)))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-set-sampler-views+
                                              +virgl-object-null+
                                              (+ 2 (length views)))
                                cmd-buf)
    (vector-push-extend-ub32/le (encode-shader-type shader-type) cmd-buf)
    (vector-push-extend-ub32/le start-slot cmd-buf)
    (dolist (sampler-view views)
      (check-type sampler-view sampler-view)
      (vector-push-extend-ub32/le (object-id sampler-view) cmd-buf)))
  (values))

(defun encode-primitive-mode (mode)
  (ecase mode
    (:points +pipe-prim-points+)
    (:lines +pipe-prim-lines+)
    (:line-loop +pipe-prim-line-loop+)
    (:line-strip +pipe-prim-line-strip+)
    (:triangles +pipe-prim-triangles+)
    (:triangle-strip +pipe-prim-triangle-strip+)
    (:triangle-fan +pipe-prim-triangle-fan+)
    (:quads +pipe-prim-quads+)
    (:quad-strip +pipe-prim-quad-strip+)
    (:polygon +pipe-prim-polygon+)
    (:lines-adjacency +pipe-prim-lines-adjacency+)
    (:line-strip-adjacency +pipe-prim-line-strip-adjacency+)
    (:triangles-adjacency +pipe-prim-triangles-adjacency+)
    (:triangle-strip-adjacency +pipe-prim-triangle-strip-adjacency+)
    (:patches +pipe-prim-patches+)))

(defun add-command-draw-vbo (command-buffer
                             start ; index of the first vertex
                             count ; number of vertices
                             mode ; mode of the primitive
                             &key
                               indexed ; use index buffer
                               (instance-count 0) ; number of instances
                               (start-instance 0) ; first instance-id
                               primitive-restart-index
                               (index-bias 0)
                               (min-index 0)
                               (max-index #xFFFFFFFF)
                               (count-from-so 0))
  (check-command-buffer-not-finalized command-buffer)
  (encode-draw-vbo
   (command-buffer-data-array command-buffer)
   start count (encode-primitive-mode mode)
   indexed
   instance-count start-instance
   primitive-restart-index
   (or primitive-restart-index 0)
   index-bias
   min-index
   max-index
   count-from-so)
  (values))

(defun add-command-set-constant-buffer (command-buffer shader &rest constants)
  (check-command-buffer-not-finalized command-buffer)
  (let* ((buf (command-buffer-data-array command-buffer))
         (header-pos (length buf))
         (n-constants 0))
    (vector-push-extend-ub32/le 0 buf) ; header, filled in once we know how many constants there are.
    (vector-push-extend-ub32/le (encode-shader-type shader) buf)
    (vector-push-extend-ub32/le 0 buf) ; index, not actually used.
    (dolist (constant constants)
      (etypecase constant
        ((simple-array single-float (*))
         (dotimes (i (length constant))
           (incf n-constants)
           (vector-push-extend-single/le (aref constant i) buf)))
        (single-float
         (incf n-constants)
         (vector-push-extend-single/le constant buf))))
    (setf (ext:ub32ref/le buf header-pos)
          (pack-command +virgl-ccmd-set-constant-buffer+
                        +virgl-object-null+
                        (+ 2 n-constants))))
  (values))

(defun add-command-set-index-buffer (command-buffer index-buffer element-width offset)
  (check-type index-buffer index-buffer)
  (check-type element-width (member 1 2 4))
  (check-command-buffer-not-finalized command-buffer)
  (let ((buf (command-buffer-data-array command-buffer)))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-set-index-buffer+
                                              +virgl-object-null+
                                              3)
                                buf)
    (vector-push-extend-ub32/le (resource-id index-buffer) buf)
    (vector-push-extend-ub32/le element-width buf)
    (vector-push-extend-ub32/le offset buf))
  (values))

(defun add-command-clear-index-buffer (command-buffer)
  (check-command-buffer-not-finalized command-buffer)
  (let ((buf (command-buffer-data-array command-buffer)))
    (vector-push-extend-ub32/le (pack-command +virgl-ccmd-set-index-buffer+
                                              +virgl-object-null+
                                              1)
                                buf)
    (vector-push-extend-ub32/le 0 buf))
  (values))
