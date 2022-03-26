(in-package :mcclim-harfbuzz)

(cffi:define-foreign-library libharfbuzz
  (:darwin "libharfbuzz.dylib")                             
  (:unix "libharfbuzz.so"))

(cffi:use-foreign-library libharfbuzz)

(cffi:defcfun ("hb_buffer_create" hb-buffer-create) :pointer)
(cffi:defcfun ("hb_buffer_destroy" hb-buffer-destroy) :void
  (buffer :pointer))
(cffi:defcfun ("hb_buffer_set_direction" hb-buffer-set-direction) :void
  (buffer :pointer)
  (direction hb-direction-t))
(cffi:defcfun ("hb_buffer_set_script" hb-buffer-set-script) :void
  (buffer :pointer)
  (script hb-script-t))
(cffi:defcfun ("hb_buffer_add_utf8" hb-buffer-add-utf8) :void
  (buffer :pointer)
  (text (:pointer :char))
  (text-length :int)
  (item-offset :unsigned-int)
  (item-length :int))
(cffi:defcfun ("hb_buffer_get_glyph_infos" hb-buffer-get-glyph-infos) (:pointer (:struct hb-glyph-info-t))
  (buffer :pointer)
  (length (:pointer :unsigned-int)))
(cffi:defcfun ("hb_buffer_get_glyph_positions" hb-buffer-get-glyph-positions) (:pointer (:struct hb-glyph-position-t))
  (buffer :pointer)
  (length (:pointer :unsigned-int)))
(cffi:defcfun ("hb_buffer_guess_segment_properties" hb-buffer-guess-segment-properties) :void
  (buffer :pointer))
(cffi:defcfun ("hb_buffer_set_cluster_level" hb-buffer-set-cluster-level) :void
  (buffer :pointer)
  (cluster-level hb-buffer-cluster-level-t))
(cffi:defcfun ("hb_buffer_get_cluster_level" hb-buffer-get-cluster-level) hb-buffer-cluster-level-t
  (buffer :pointer))

(cffi:defcfun ("hb_ft_font_create" hb-ft-font-create) :pointer
  (face :pointer)
  (destroy-func :pointer))
(cffi:defcfun ("hb_ft_font_set_load_flags" hb-ft-font-set-load-flags) :void
  (font :pointer)
  (load-flags :int))
(cffi:defcfun ("hb_ft_font_get_load_flags" hb-ft-font-get-load-flags) :int
  (font :pointer))

(cffi:defcfun ("hb_shape" hb-shape) :void
  (font :pointer)
  (buffer :pointer)
  (features (:pointer (:struct hb-feature-t)))
  (num-features :unsigned-int))
