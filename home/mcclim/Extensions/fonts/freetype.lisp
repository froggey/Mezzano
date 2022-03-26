(defpackage :clim-freetype
  (:use :cl)
  (:export #:*enable-autohint*
           #:make-font-replacement-text-style))

(in-package :clim-freetype)

(defclass clx-freetype-port (clim-clx:clx-render-port) ())
(defclass clx-freetype-medium (clim-clx:clx-render-medium) ())

(setf (get :clx-ff :port-type) 'clx-freetype-port)
(setf (get :clx-ff :server-path-parser) 'clim-clx::parse-clx-server-path)

(defmethod clim:make-medium ((port clx-freetype-port) sheet)
  (make-instance 'clx-freetype-medium :sheet sheet))

(defparameter *freetype-font-scale* 26.6)

(defvar *enable-autohint* t)

(defvar *lock* (bordeaux-threads:make-recursive-lock))

#+autofitter-warp-available
(progn
  (defvar *enable-autofitter-warp* t)

  ;; The freetype2 library doesn't expose this function, so we add it here.
  (cffi:defcfun ("FT_Property_Set" ft-property-set) freetype2-types:ft-error
    (library freetype2-types:ft-library)
    (mod-name :string)
    (prop-name :string)
    (value :pointer)))

(cffi:defcfun ("FT_Library_SetLcdFilter" ft-library-set-lcd-filter) freetype2-types:ft-error
  (library freetype2-types:ft-library)
  (filter :int))

(cffi:defcfun ("FT_Library_SetLcdFilterWeights" ft-library-set-lcd-filter-weights) freetype2-types:ft-error
  (library freetype2-types:ft-library)
  (filter (:pointer :unsigned-char)))

;; Workaround for https://github.com/rpav/cl-freetype2/issues/11
(defun freetype-make-vector (x y)
  "Make an `FT-VECTOR` given `X` and `Y`.  This may be passed directly
to `SET-TRANSFORM`, and may be more efficient than converting from native
forms."
  (let ((vector (freetype2::make-collected-foreign 'freetype2-types:ft-vector
                                                   '(:struct freetype2-types:foreign-ft-vector))))
    (setf (freetype2-types:ft-vector-x vector) x)
    (setf (freetype2-types:ft-vector-y vector) y)
    vector))

#+ (or) ;; there is no such class hierarchy anymore
(defmethod initialize-instance :after ((obj freetype-font-renderer) &key)
  #+autofitter-warp-available
  (when *enable-autofitter-warp*
    (cffi:with-foreign-objects ((v :int))
      (setf (cffi:mem-ref v :int) 1)
      (ft-property-set freetype2:*library* "autofitter" "warping" v))))

(defun ensure-string-value (v)
  (etypecase v
    (string v)
    (character (string v))))

(defclass freetype-font-family (clim-extensions:font-family)
  ((faces :initform (make-hash-table :test 'equal)
          :reader freetype-font-family/faces)))

(defun find-font-family (port name)
  (let ((family (find name (clim-clx::font-families port) :key #'clim-extensions:font-family-name :test #'equal)))
    (or family
        (let ((v (make-instance 'freetype-font-family :port port :name name)))
          (push v (clim-clx::font-families port))
          v))))

(defclass cached-picture ()
  ((glyphset :initform nil
             :accessor cached-picture/glyphset)))

(defclass freetype-font-face (clim-extensions:font-face)
  ((file    :initarg :file
            :reader freetype-font-face/file)
   (charset :initarg :charset
            :initform nil
            :reader freetype-font-face/charset)
   (face    :initarg :face
            :initform nil
            :accessor freetype-font-face/face)))

(defun find-or-load-face (font)
  (check-type font freetype-font)
  (let ((f (freetype-font/face font)))
    (or (freetype-font-face/face f)
        (setf (freetype-font-face/face f) (freetype2:new-face (freetype-font-face/file f))))))

(defstruct glyph-attributes x-origin y-origin width height)

(defclass freetype-font ()
  ((face           :initarg :face
                   :reader freetype-font/face)
   (size           :initarg :size
                   :reader freetype-font/size)
   (cached-glyphs  :initform (make-hash-table :test 'eql)
                   :reader freetype-font/cached-glyphs)
   (cached-picture :type cached-picture
                   :reader freetype-font/cached-picture)
   (hb-font        :initarg :hb-font
                   :accessor freetype-font/hb-font)
   (port           :initarg :port
                   :reader freetype-font/port)
   (font-replace   :initarg :font-replace
                   :reader freetype-font-replace))
  (:default-initargs :size 10 :hb-font nil :font-replace nil))

(defmethod initialize-instance :after ((obj freetype-font) &key)
  (let ((cached (make-instance 'cached-picture)))
    (setf (slot-value obj 'cached-picture) cached)
    (trivial-garbage:finalize obj (lambda ()
                                    (alexandria:when-let ((glyphset (cached-picture/glyphset cached)))
                                      (xlib:render-free-glyph-set glyphset))))))

(defmethod print-object ((obj freetype-font) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "FACE ~s SIZE ~s" (freetype-font/face obj) (freetype-font/size obj))))

(defmacro with-size-face ((sym face size) &body body)
  (alexandria:once-only (face size)
    `(progn
       (freetype2:set-char-size ,face (round (* ,size 64)) 0 72 72)
       (let ((,sym ,face))
         ,@body))))

(defmacro with-face-from-font ((sym font) &body body)
  (alexandria:once-only (font)
    `(bordeaux-threads:with-recursive-lock-held (*lock*)
       (with-size-face (,sym (find-or-load-face ,font) (freetype-font/size ,font))
         ,@body))))

(defmethod clim-extensions:font-face-all-sizes ((face freetype-font-face))
  '(8 12 18 20 24 36 48 72))

(defmethod clim-extensions:font-face-text-style ((face freetype-font-face) &optional size)
  (clim:make-text-style (clim-extensions:font-face-family face)
                        (clim-extensions:font-face-name face)
                        size))

(defun find-rgba-format (display)
  (or (getf (xlib:display-plist display) 'rgba-format)
      (let* ((formats (xlib::render-query-picture-formats display))
             (format (find-if (lambda (v)
                                (and (= (byte-size (xlib:picture-format-red-byte v)) 8)
                                     (= (byte-size (xlib:picture-format-green-byte v)) 8)
                                     (= (byte-size (xlib:picture-format-blue-byte v)) 8)
                                     (= (byte-size (xlib:picture-format-alpha-byte v)) 8)))
                              formats)))
        (unless format
          (error "Can't find 8-bit RGBA format"))
        (setf (getf (xlib:display-plist display) 'rgba-format) format))))


(defun bitmap->array (bitmap)
  (let* ((width (/ (freetype2-types:ft-bitmap-width bitmap) 3))
         (height (freetype2-types:ft-bitmap-rows bitmap)))
    (assert (typep width 'integer))

    (if (or (zerop width) (zerop height))
        ;; Zero-sized glyphs causes the renderer to hang
        (make-array '(1 1) :element-type '(unsigned-byte 32) :initial-element 0)
        ;; Format the glyph array in a way that xrender accepts
        (let ((array (make-array (list height width) :element-type '(unsigned-byte 32) :initial-element 0)))
          (loop
            with buffer = (freetype2-types:ft-bitmap-buffer bitmap)
            with pitch = (freetype2-types:ft-bitmap-pitch bitmap)
            for y from 0 below height
            for ptr = (cffi:inc-pointer buffer (* y pitch))
            do (loop
                 for x from 0 below width
                 for v = (logior (ash (cffi:mem-ref ptr :unsigned-char (* x 3)) 16)
                                 (ash (cffi:mem-ref ptr :unsigned-char (+ (* x 3) 1)) 8)
                                 (cffi:mem-ref ptr :unsigned-char (+ (* x 3) 2))
                                 #xff000000)
                 do (setf (aref array y x) v)))
          array))))

(defun find-or-create-hb-font (font)
  (check-type font freetype-font)
  (or (freetype-font/hb-font font)
      (setf (freetype-font/hb-font font)
            (with-face-from-font (face font)
              (let ((font (mcclim-harfbuzz:hb-ft-font-create (freetype2::p* (freetype2::fw-ptr face)) (cffi:null-pointer))))
                (mcclim-harfbuzz:hb-ft-font-set-load-flags font (if *enable-autohint* 32 0))
                font)))))

(defun render-char-to-glyphset (glyphset face glyph-index)
  (ft-library-set-lcd-filter freetype2:*library* 1)
  (freetype2:load-glyph face glyph-index (if *enable-autohint* '(:force-autohint) nil))
  (let* ((glyph (freetype2-types:ft-face-glyph face))
         (bitmap (freetype2-types:ft-glyphslot-bitmap (freetype2:render-glyph glyph :lcd)))
         (x-origin (- (freetype2-types:ft-glyphslot-bitmap-left glyph)))
         (y-origin (freetype2-types:ft-glyphslot-bitmap-top glyph))
         (bitmap-array (bitmap->array bitmap)))
    (xlib:render-add-glyph glyphset glyph-index
                           :x-origin x-origin
                           :y-origin y-origin
                           :x-advance 0 ; (/ (freetype2-types:ft-vector-x advance) 64)
                           :y-advance 0 ; (/ (freetype2-types:ft-vector-y advance) 64)
                           :data bitmap-array)
    (make-glyph-attributes :x-origin x-origin
                           :y-origin y-origin
                           :width (/ (freetype2-types:ft-bitmap-width bitmap) 3)
                           :height (freetype2-types:ft-bitmap-rows bitmap))))

(defun create-glyphset (font)
  (let ((format (find-rgba-format (clim-clx::clx-port-display (freetype-font/port font)))))
    (xlib:render-create-glyph-set format)))

(defun find-or-create-cached-glyphset (font)
  (let ((cached (freetype-font/cached-picture font)))
    (or (cached-picture/glyphset cached)
        (setf (cached-picture/glyphset cached) (create-glyphset font)))))

(defun free-glyphset (glyphset)
  (xlib:render-free-glyph-set glyphset))

(defun ensure-glyphs-loaded (font index-list glyphset cached-glyphs transform-matrix)
  "Render all glyphs in INDEX-LIST into the given glyphset.
CACHED-GLYPHS is a hash table containing information about the glyphs
that have already been rendered, and will be filled in with any new
glyphs that were added to the glyphset. If non-NIL, TRANSFORM-MATRIX
is a 2x2 matrix that will be used to transform the glyphs prior to
rendering, otherwise the identity matrix will be used instead."
  (with-face-from-font (face font)
    (if transform-matrix
        (freetype2:set-transform face (freetype2-types:make-matrix
                                       (truncate (* #x10000 (aref transform-matrix 0 0)))
                                       (truncate (* #x10000 (aref transform-matrix 0 1)))
                                       (truncate (* #x10000 (aref transform-matrix 1 0)))
                                       (truncate (* #x10000 (aref transform-matrix 1 1))))
                                 (freetype-make-vector 0 0))
        (freetype2-ffi:ft-set-transform face (cffi:null-pointer) (cffi:null-pointer)))
    (loop
      for glyph-index in index-list
      unless (gethash glyph-index cached-glyphs)
        do (let ((attrs (render-char-to-glyphset glyphset face glyph-index)))
             (setf (gethash glyph-index cached-glyphs) attrs))))
  glyphset)

(defun load-cached-glyphset (font index-list)
  (let ((glyphset (find-or-create-cached-glyphset font))
        (cached-glyphs (freetype-font/cached-glyphs font)))
    (ensure-glyphs-loaded font index-list glyphset cached-glyphs nil)))

(defun load-standalone-glyphset (font index-list transform-matrix)
  (let ((glyphset (create-glyphset font))
        (cached-glyphs (make-hash-table :test 'eql)))
    (ensure-glyphs-loaded font index-list glyphset cached-glyphs transform-matrix)))

(defun create-picture-from-drawable (drawable)
  (xlib:render-create-picture drawable
                              :format (xlib:find-window-picture-format (xlib:drawable-root drawable))
                              :poly-edge :smooth
                              :poly-mode :precise))

(defun create-dest-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) 'cached-picture)
      (setf (getf (xlib:drawable-plist drawable) 'cached-picture)
            (create-picture-from-drawable drawable))))

(defun create-pen (drawable gc)
  (let* ((fg (xlib::gcontext-foreground gc))
         (cached-pen (getf (xlib:gcontext-plist gc) 'cached-pen)))
    (cond ((and cached-pen (equal (second cached-pen) fg))
           (first cached-pen))
          (t
           (when cached-pen
             (xlib:render-free-picture (first cached-pen)))
           (let* ((pixmap (xlib:create-pixmap :drawable (xlib:drawable-root drawable) :width 1 :height 1 :depth 32))
                  (picture (xlib:render-create-picture pixmap :format (find-rgba-format (xlib::drawable-display drawable)) :repeat :on))
                  (colour (list (ash (ldb (byte 8 16) fg) 8)
                                (ash (ldb (byte 8 8) fg) 8)
                                (ash (ldb (byte 8 0) fg) 8)
                                #xFFFF)))
             (xlib:render-fill-rectangle picture :over colour 0 0 1 1)
             (xlib:free-pixmap pixmap)
             (setf (getf (xlib:gcontext-plist gc) 'cached-pen) (list picture fg))
             picture)))))

(defstruct glyph-entry codepoint x-advance y-advance x-offset y-offset)

(defun make-glyph-list (font string direction)
  (mcclim-harfbuzz:with-buffer (buf)
    (let ((hb-font (find-or-create-hb-font font)))
      (mcclim-harfbuzz:hb-buffer-set-direction buf (ecase direction
                                                     (:ltr :hb-direction-ltr)
                                                     (:rtl :hb-direction-rtl)))
      (mcclim-harfbuzz:buffer-add-string buf string)
      (mcclim-harfbuzz:hb-buffer-guess-segment-properties buf)
      (mcclim-harfbuzz:hb-shape hb-font buf (cffi:null-pointer) 0)
      (cffi:with-foreign-objects ((num-glyphs :int))
        (let* ((glyph-info (mcclim-harfbuzz:hb-buffer-get-glyph-infos buf num-glyphs))
               (glyph-info-element-size (cffi:foreign-type-size '(:struct mcclim-harfbuzz::hb-glyph-info-t)))
               (glyph-pos (mcclim-harfbuzz:hb-buffer-get-glyph-positions buf num-glyphs))
               (glyph-pos-element-size (cffi:foreign-type-size '(:struct mcclim-harfbuzz::hb-glyph-position-t))))
          (loop
            for i from 0 below (cffi:mem-ref num-glyphs :int)
            for codepoint = (cffi:foreign-slot-value (cffi:inc-pointer glyph-info (* glyph-info-element-size i))
                                                     '(:struct mcclim-harfbuzz::hb-glyph-info-t)
                                                     'mcclim-harfbuzz::codepoint)
            for pos = (cffi:inc-pointer glyph-pos (* glyph-pos-element-size i))
            collect (make-glyph-entry :codepoint codepoint
                                      :x-advance (cffi:foreign-slot-value pos '(:struct mcclim-harfbuzz::hb-glyph-position-t)
                                                                          'mcclim-harfbuzz::x-advance)
                                      :y-advance (cffi:foreign-slot-value pos '(:struct mcclim-harfbuzz::hb-glyph-position-t)
                                                                          'mcclim-harfbuzz::y-advance)
                                      :x-offset (cffi:foreign-slot-value pos '(:struct mcclim-harfbuzz::hb-glyph-position-t)
                                                                          'mcclim-harfbuzz::x-offset)
                                      :y-offset (cffi:foreign-slot-value pos '(:struct mcclim-harfbuzz::hb-glyph-position-t)
                                                                          'mcclim-harfbuzz::y-offset))))))))

(defun convert-transformation-to-matrix (transformation)
  "Given a CLIM transformation object, return the appropriate matrix,
or NIL if the current transformation is the identity transformation."
  (if (eq transformation 'clim:+identity-transformation+)
      nil
      (multiple-value-bind (rxx rxy ryx ryy)
          (climi::get-transformation transformation)
        (if (and (= rxx 1)
                 (= rxy 0)
                 (= ryx 0)
                 (= ryy 1))
            nil
            (make-array '(2 2) :initial-contents (list (list rxx (- rxy)) (list (- ryx) ryy)))))))

(defvar *draw-font-lock* (clim-sys:make-lock "draw-font"))
(defmethod clim:medium-draw-text* ((medium clx-freetype-medium) string x y
                                   start end
                                   align-x align-y
                                   toward-x toward-y transform-glyphs
                                   &aux (end (if (null end)
                                                 (length string)
                                                 (min end (length string)))))
  ;; Possible optimalzaions:
  ;;
  ;; * with-clx-graphics already creates appropriate pixmap for us (correct one!) and we have
  ;; medium picture in place - there is no need for gcontext-picture (see xrender-fonts)
  ;; * don't use (PICTURE-DRAWABLE (CLX-RENDER-MEDIUM-PICTURE MEDIUM)) - it is slow due to possible
  ;; mirror swaps, use (SHEET-XMIRROR (MEDIUM-SHEET MEDIUM)) instead. It might be a good idea to
  ;; wrap our own (CLX-RENDER-MEDIUM-MIRROR MEDIUM) function.
  (declare (ignore toward-x toward-y))
  (let ((string (ensure-string-value string)))
    (when (alexandria:emptyp string)
      (return-from clim:medium-draw-text*))
    (clim-clx::with-clx-graphics () medium
      (unless (eq align-y :baseline)
        (let* ((font (clim:text-style-mapping (clim:port medium) (clim:medium-text-style medium)))
               (ascent (climb:font-ascent font))
               (descent (climb:font-descent font))
               (text-height (+ ascent descent)))
          (setq y (ecase align-y
                    (:top (+ y ascent))                              ; OK
                    #+ (or) (:baseline y)                            ; OK
                    (:center (+ y ascent (- (/ text-height 2.0))))   ; See :around for multiline
                    (:baseline* y)                                   ; See :around for multiline
                    (:bottom (- y descent))))))                      ; See :around for multiline
      (unless (eq align-x :left)
        ;; This is the worst case - we need to compute whole text width what
        ;; requires walking all lines char-by char.
        (let ((text-width (clim:text-size medium string :start start :end end)))
          (setq x (- (- x 0.5) (ecase align-x
                                 ;;(:left 0)
                                 (:center (/ text-width 2.0))
                                 (:right text-width))))))
      (clim-sys:with-lock-held (*draw-font-lock*)
        (freetype-draw-glyphs medium clim-clx::mirror clim-clx::gc x y string
                              :start start :end end
                              :transformation (clim:sheet-device-transformation
                                               (clim:medium-sheet medium))
                              :transform-glyphs transform-glyphs)))))

;;; We only cache glyphsets that does not have a transformation
;;; applied. The assumption is that applying transformation on text is
;;; rare enough that the lower performance will be acceptable.

(defun freetype-draw-glyphs (medium mirror gc x y string
                    &key (start 0) (end (length string))
                      (direction :ltr)
                      transformation transform-glyphs
                    &aux
                      (font (clim:text-style-mapping (clim:port medium) (clim:medium-text-style medium))))
  (unless (or transform-glyphs (clim:translation-transformation-p transformation))
    (multiple-value-setq (x y) (clim:transform-position transformation x y))
    (setq transformation clim:+identity-transformation+))
  (when (null (freetype-font-replace font))
    (return-from freetype-draw-glyphs
      (%freetype-draw-glyphs font mirror gc x y string
                             :start start :end end :direction direction
                             :transformation transformation)))
  ;; font replacement logic
  (let* ((text-style (clim:medium-text-style medium))
         (string (subseq string start end))
         (blocks (find-replacement-fonts (clim:port medium) text-style string))
         (size (clim:text-style-size text-style)))
    (loop
       with curr-x = x
       for (string family style) in blocks
       for new-text-style = (if family (clim:make-text-style family style size) text-style)
       do (let ((font (clim:text-style-mapping (clim:port medium) new-text-style)))
            (%freetype-draw-glyphs font mirror gc curr-x y string
                                   :direction direction
                                   :transformation transformation)
            (incf curr-x (clim:text-size medium string :text-style new-text-style)))
       finally (return curr-x))))

(defun %freetype-draw-glyphs (font mirror gc x y string
                             &key (start 0) (end (length string))
                               (direction :ltr) transformation)
  (with-face-from-font (face font)
    (let ((transform-matrix (convert-transformation-to-matrix transformation)))
      (freetype2-ffi:ft-set-transform face (cffi:null-pointer) (cffi:null-pointer))
      (let* ((index-list (make-glyph-list font (subseq string start end) direction))
             (codepoints (mapcar #'glyph-entry-codepoint index-list))
             (glyphset (if transform-matrix
                           ;; We have a transformation matrix, so let's load the glyphset without caching
                           (load-standalone-glyphset font codepoints transform-matrix)
                           ;; ELSE: No transformation, make sure the glyphs are cached
                           (load-cached-glyphset font codepoints))))
        (unwind-protect
             (let ((source (create-pen mirror gc))
                   (dest (create-dest-picture mirror))
                   (vec (make-array 1 :element-type 'integer :initial-element 0)))
               (unless  (eq (xlib:picture-clip-mask dest)
                            (xlib:gcontext-clip-mask gc))
                 (setf (xlib:picture-clip-mask dest)
                       (xlib:gcontext-clip-mask gc)))
               (loop
                 with rx = 0
                 with ry = 0
                 for current-index in index-list
                 do (let ((x-pos (+ x rx #+(or) (glyph-entry-x-offset current-index)))
                          (y-pos (+ y ry #+(or) (glyph-entry-y-offset current-index))))
                      (setf (aref vec 0) (glyph-entry-codepoint current-index))
                      (multiple-value-bind (transformed-x transformed-y)
                          (clim:transform-position transformation x-pos y-pos)
                        (let ((sx (truncate (+ transformed-x 0.5)))
                              (sy (truncate (+ transformed-y 0.5))))
                          (unless (or (> sx #x7fff)
                                      (> sy #x7fff))
                            (xlib:render-composite-glyphs dest glyphset source sx sy vec))))
                      (incf rx (/ (glyph-entry-x-advance current-index) 64))
                      (incf ry (/ (glyph-entry-y-advance current-index) 64)))))
          (when transform-matrix
            (free-glyphset glyphset)))))))

(defmethod climb:font-text-extents ((font freetype-font) string
                                    &key align-x align-y (start 0) (end (length string)) (direction :ltr))
  (declare (ignore align-x align-y))
  ;; Values to return:
  ;;-> xmin ymin xmax ymax left top width height ascent descent linegap cursor-dx cursor-dy
  (with-face-from-font (face font)
    (freetype2-ffi:ft-set-transform face (cffi:null-pointer) (cffi:null-pointer))
    (flet ((text-extents (font string start end)
             (let* ((index-list (make-glyph-list font (subseq string start end) direction)))
               (load-cached-glyphset font (mapcar #'glyph-entry-codepoint index-list))
               (let ((cached-glyphs (freetype-font/cached-glyphs font)))
                 (multiple-value-bind (width ascender descender)
                     (loop
                        with rx = 0
                        with ascender = 0
                        with descender = 0
                        for current-index on index-list
                        for e = (car current-index)
                        for attrs = (gethash (glyph-entry-codepoint e) cached-glyphs)
                        do (progn
                             (when (cdr current-index)
                               (incf rx (/ (glyph-entry-x-advance e) 64)))
                             (setf descender (max descender (- (glyph-attributes-height attrs)
                                                               (glyph-attributes-y-origin attrs))))
                             (setf ascender (max ascender (glyph-attributes-y-origin attrs))))
                        finally (return (values rx ascender descender)))
                   (let* ((e (car (last index-list)))
                          (glyph-attrs (gethash (glyph-entry-codepoint e) cached-glyphs)))
                     (values
                      ;; bounding box
                      (- (glyph-attributes-x-origin
                          (gethash (glyph-entry-codepoint (first index-list)) cached-glyphs)))
                      (- ascender)
                      (- (+ width (glyph-attributes-width glyph-attrs))
                         (glyph-attributes-x-origin glyph-attrs))
                      descender
                      ;; text size
                      0 0
                      (+ width (/ (glyph-entry-x-advance e) 64))
                      (+ (freetype2:face-ascender-pixels face)
                         (freetype2:face-descender-pixels face))
                      ;; line properties
                      (freetype2:face-ascender-pixels face)
                      (freetype2:face-descender-pixels face)
                      (- (climb:font-leading font)
                         (+ (freetype2:face-ascender-pixels face)
                            (freetype2:face-descender-pixels face)))
                      ;; cursor motion
                      (+ width (/ (glyph-entry-x-advance e) 64)) 0)))))))
      (cond ((= start end)
             (values 0 0 0 0 0 (freetype2:face-ascender-pixels face) (freetype2:face-descender-pixels face) 0 end))
            ((freetype-font-replace font)
             (let* ((text-style (freetype-font-replace font))
                    (port (freetype-font/port font))
                    (string (subseq string start end))
                    (blocks (find-replacement-fonts port text-style string))
                    (size (clim:text-style-size text-style)))
               (loop
                  with curr-x = 0
                  with sizes = nil
                  for (string family style) in blocks
                  for new-text-style = (if family (clim:make-text-style family style size) text-style)
                  do (let ((font (clim:text-style-mapping port new-text-style)))
                       (setf sizes (multiple-value-list (text-extents font string 0 (length string))))
                       (incf curr-x (car sizes)))
                  finally
                    (setf (car sizes) curr-x)
                    (return (apply #'values sizes)))))
            (t
             (text-extents font string start end))))))

(defmethod climb:font-glyph-dx ((font freetype-font) character)
  (nth-value 2 (climb:font-text-extents font (format nil "~c" (code-char character)))))

(defmethod climb:text-bounding-rectangle* ((medium clx-freetype-medium) string
                                           &key
                                             text-style
                                             (start 0) end
                                             (align-x :left) (align-y :baseline) (direction :ltr)
                                           &aux (end (or end (length string))))
  (when (= start end)
    (return-from climb:text-bounding-rectangle* (values 0 0 0 0)))
  (let ((text (string string))
        (font (clim:text-style-mapping (clim:port medium)
                                        (clim:merge-text-styles text-style
                                                                (clim:medium-merged-text-style medium)))))
    (multiple-value-bind (xmin ymin xmax ymax)
        (climb:font-text-extents font text :start start :end end
                                 :align-x align-x :align-y align-y :direction direction)
      (values xmin ymin xmax ymax))))

(defmethod climb:text-size ((medium clx-freetype-medium) string &key text-style (start 0) end)
  (let* ((string (ensure-string-value string))
         (end (or end (length string)))
         (text-style (clim:merge-text-styles text-style
                                             (clim:medium-merged-text-style medium))))
    (when (= start end)
      (return-from climb:text-size (values 0 0 0 0 (clim:text-style-ascent text-style medium))))
    (let ((text (string string))
          (font (clim:text-style-mapping (clim:port medium) text-style)))
      (multiple-value-bind (xmin ymin xmax ymax
                            left top width height
                            ascent descent linegap
                            cursor-dx cursor-dy)
          (climb:font-text-extents font text :start start :end end)
        (declare (ignore xmin ymin xmax ymax left top descent linegap))
        (values width height cursor-dx cursor-dy ascent)))))

(defmethod climb:font-ascent ((font freetype-font))
  (with-face-from-font (face font)
    (freetype2:face-ascender-pixels face)))

(defmethod climb:font-descent ((font freetype-font))
  (with-face-from-font (face font)
    (freetype2:face-descender-pixels face)))

(defun make-family-pattern (family)
  (list (cond
          ((typep family 'freetype-font-family) `(:family . ,(clim-extensions:font-family-name family)))
          ((stringp family) `(:family . ,family))
          ((eq family :fix) '(:spacing . 100))
          ((eq family :sans-serif) '(:family . "DejaVu Sans"))
          ((eq family :serif) '(:family . "DejaVu Serif"))
          (t '(:family . "DejaVu Sans")))))

(defun make-face-pattern (face)
  (loop
    for f in (if (listp face) face (list face))
    append (cond
             ((typep f 'freetype-font-face) `(("style" . ,(clim-extensions:font-face-name face))))
             ((stringp face) `((:style . ,face)))
             ((eq f :roman) '((:weight . 80) (:slant . 0)))
             ((eq f :bold) '((:weight . 200)))
             ((eq f :italic) '((:slant . 100)))
             (t nil))))

(defparameter *main-filter* '((:scalable . :true)))

(defun find-best-match (family face)
  (let ((result (mcclim-fontconfig:match-font (append *main-filter*
                                                      (make-family-pattern family)
                                                      (make-face-pattern face))
                                              '(:family :style :file :charset)
                                              :kind :match-font)))
    (list (cdr (assoc :family result))
          (cdr (assoc :style result))
          (cdr (assoc :file result))
          (cdr (assoc :charset result)))))

(defun find-freetype-font (port text-style)
  (multiple-value-bind (family face size)
      (clim:text-style-components text-style)
    (destructuring-bind (found-family found-style found-file found-charset)
        (find-best-match family face)
      (let* ((family-obj (find-font-family port found-family))
             (face-obj (alexandria:ensure-gethash found-style (freetype-font-family/faces family-obj)
                                                  (make-instance 'freetype-font-face
                                                                 :family family-obj
                                                                 :name found-style
                                                                 :file found-file
                                                                 :charset found-charset))))
        (make-instance 'freetype-font
                       :port port
                       :face face-obj
                       :size (climb:normalize-font-size size)
                       :font-replace (typecase text-style
                                       (font-replacement-text-style text-style)
                                       (otherwise nil)))))))

(defmethod clim:text-style-mapping ((port clx-freetype-port) (text-style clim:standard-text-style) &optional character-set)
  (declare (ignore character-set))
  (find-freetype-font port text-style))

;;;
;;;  List fonts
;;;

(defmethod clime:port-all-font-families ((port clx-freetype-port) &key invalidate-cache)
  (let* ((h (make-hash-table :test 'equal))
         (existing-families (make-hash-table :test 'equal))
         (prev-families nil))
    (unless invalidate-cache
      (loop
        for fam in (clim-clx::font-families port)
        do (setf (gethash (clim-extensions:font-family-name fam) existing-families) t))
      (setf prev-families (clim-clx::font-families port)))
    (loop
      for font in (mcclim-fontconfig:font-list *main-filter* '(:family :style :file))
      for family = (cdr (assoc :family font))
      for style = (cdr (assoc :style font))
      for file = (cdr (assoc :file font))
      for m = (alexandria:ensure-gethash family h
                                         (make-hash-table :test 'equal))
      do (setf (gethash style m) file))
    (let ((new-families (loop
                          for family being each hash-key using (hash-value style-hash) in h
                          unless (gethash family existing-families)
                            collect (let ((f (make-instance 'freetype-font-family :name family :port port)))
                                      (loop
                                        with font-family-styles = (freetype-font-family/faces f)
                                        for style being each hash-key using (hash-value file) in style-hash
                                        unless (gethash style font-family-styles)
                                          do (setf (gethash style font-family-styles)
                                                   (make-instance 'freetype-font-face :name style :family f :file file)))
                                      f))))
      (setf (clim-clx::font-families port)
            (sort (append prev-families new-families)
                  #'string<
                  :key #'clim-extensions:font-family-name))))
  (clim-clx::font-families port))

(defmethod clim-extensions:font-family-all-faces ((family freetype-font-family))
  (loop
    for face being each hash-value in (freetype-font-family/faces family)
    collect face))

;;;
;;;  Character info
;;;

(macrolet ((define-glyph-info-method (name reader-function)
             `(defmethod ,name ((font freetype-font) code)
                (with-face-from-font (face font)
                  (freetype2-ffi:ft-set-transform face (cffi:null-pointer) (cffi:null-pointer))
                  ;; This ensures that the glyph in question has been cached
                  (load-cached-glyphset font (list code))
                  (let ((info (gethash code (freetype-font/cached-glyphs font))))
                    (unless info
                      (error "Character with code ~s was not found in font ~s after loading cached glyphs" code font))
                    ,reader-function)))))
  (define-glyph-info-method climb:font-glyph-top (glyph-attributes-y-origin info))
  (define-glyph-info-method climb:font-glyph-left (- (glyph-attributes-x-origin info)))
  (define-glyph-info-method climb:font-glyph-bottom (- (glyph-attributes-height info)
                                                       (glyph-attributes-y-origin info)))
  (define-glyph-info-method climb:font-glyph-right (- (glyph-attributes-width info)
                                                      (glyph-attributes-x-origin info)))
  (define-glyph-info-method climb:font-glyph-width (glyph-attributes-width info))
  (define-glyph-info-method climb:font-glyph-height (glyph-attributes-height info)))

(defmethod climb:font-string-glyph-codes ((font freetype-font) string &key (start 0) (end (length string)))
  (let ((index-list (make-glyph-list font (subseq string start end) :ltr)))
    (map 'vector #'glyph-entry-codepoint index-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font replacement code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass font-replacement-text-style (clim:standard-text-style)
  ((fallback-fonts :initarg :replacement
                   :initform :all
                   :reader font-replacement-text-style/replacement)))

(defgeneric text-style-fallback-fonts (text-style)
  (:method ((text-style clim:standard-text-style))
    nil)
  (:method ((text-style font-replacement-text-style))
    (font-replacement-text-style/replacement text-style)))

(defmethod clim:merge-text-styles ((style1 font-replacement-text-style) (style2 clim:standard-text-style))
  (let ((merged-standard (call-next-method)))
    (make-font-replacement-text-style (clim:text-style-family merged-standard)
                                      (clim:text-style-face merged-standard)
                                      (clim:text-style-size merged-standard)
                                      (font-replacement-text-style/replacement style1))))

(defmethod clim:merge-text-styles ((style1 clim:standard-text-style) (style2 font-replacement-text-style))
  (let ((merged-standard (call-next-method)))
    (make-font-replacement-text-style (clim:text-style-family merged-standard)
                                      (clim:text-style-face merged-standard)
                                      (clim:text-style-size merged-standard)
                                      (font-replacement-text-style/replacement style2))))

;;; The text styles needs to be cached because the implementation of
;;; CLIM-CLX::LOOKUP-TEXT-STYLE-TO-X-FONT calls
;;; CLIM:TEXT-STYLE-MAPPING in order to avoid doing a full font
;;; lookup. The implementation of TEXT-STYLE-MAPPING ends up doing a
;;; hash lookup on the text-style instance, which for CLOS instances
;;; uses object identity.
;;;
;;; Having this cache effectively interns instances of
;;; FONT-REPLACEMENT-TEXT-STYLE, allowing the TEXT-STYLE-MAPPING cache
;;; to work properly.
(defvar *text-styles* (make-hash-table :test 'equal))

(defun make-font-replacement-text-style (family face size replacement)
  (alexandria:ensure-gethash (list family face size replacement)
                             *text-styles*
                             (make-instance 'font-replacement-text-style
                                            :text-family family
                                            :text-face face
                                            :text-size size
                                            :replacement replacement)))

(defun find-best-font (ch)
  (let* ((match (mcclim-fontconfig:match-font `((:charset . (:charset ,ch))) '(:family :style) :kind :match-font)))
    (let ((family (cdr (assoc :family match)))
          (style (cdr (assoc :style match))))
      (cond ((and family style)
             (list family style))
            (t
             '(nil nil))))))

(defun text-style-contains-char-p (port text-style ch)
  (let* ((font (clim:text-style-mapping port text-style))
         (charset (clim-freetype::freetype-font-face/charset (clim-freetype::freetype-font/face font))))
    (mcclim-fontconfig:charset-contains-char-p charset ch)))

(defvar *replacement-font-cache* (make-hash-table :test 'equal))

(defun find-best-font-for-fallback-internal (port text-style ch)
  (loop
    for fallback in (text-style-fallback-fonts text-style)
    for fallback-family = (first fallback)
    for fallback-style = (second fallback)
    when (text-style-contains-char-p port (clim:make-text-style fallback-family fallback-style 10) ch)
      return fallback
    finally (return (find-best-font ch))))

(defun find-best-font-for-fallback (port text-style ch)
  (alexandria:ensure-gethash (list (clim:text-style-family text-style)
                                   (clim:text-style-face text-style)
                                   ch)
                             *replacement-font-cache*
                             (find-best-font-for-fallback-internal port text-style ch)))

(defun find-replacement-fonts (port text-style string)
  (let* ((default-font (clim:text-style-mapping port text-style))
         (default-charset (clim-freetype::freetype-font-face/charset (clim-freetype::freetype-font/face default-font)))
         (result nil)
         (current-string (make-string-output-stream))
         (current-text-style nil))
    (labels ((push-string ()
               (let ((s (get-output-stream-string current-string)))
                 (when (plusp (length s))
                   (push (cons s current-text-style) result))))
             (collect-result (ch s)
               (unless (equal current-text-style s)
                 (push-string)
                 (setq current-text-style s))
               (write-char ch current-string)))
      (loop
        for ch across string
        do (collect-result ch (if (mcclim-fontconfig:charset-contains-char-p default-charset ch)
                                  '(nil nil)
                                  (find-best-font-for-fallback port text-style ch))))
      (push-string)
      (reverse result))))
