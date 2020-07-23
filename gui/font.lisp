;;;; Font support

(in-package :mezzano.gui.font)

(defvar *default-font* "DejaVuSans")
(defvar *default-font-size* 12)
(defvar *default-bold-font* "DejaVuSans-Bold")
(defvar *default-bold-font-size* 12)
(defvar *default-monospace-font* "DejaVuSansMono")
(defvar *default-monospace-font-size* 12)
(defvar *default-monospace-bold-font* "DejaVuSansMono-Bold")
(defvar *default-monospace-bold-font-size* 12)

(defclass memory-file-stream (mezzano.gray:fundamental-binary-input-stream file-stream)
  ((%vector :initarg :vector :reader memory-file-stream-vector)
   (%fpos :initform 0)))

(defmethod initialize-instance :after ((stream memory-file-stream) &key vector)
  (check-type vector (array (unsigned-byte 8))))

(defmethod mezzano.gray:stream-element-type ((stream memory-file-stream))
  '(unsigned-byte 8))

(defmethod mezzano.gray:stream-file-position ((stream memory-file-stream) &optional (position-spec nil position-specp))
  (with-slots (%fpos) stream
    (cond (position-specp
           (setf %fpos (case position-spec
                         (:start 0)
                         (:end (length (memory-file-stream-vector stream)))
                         (t position-spec))))
          (t %fpos))))

(defmethod mezzano.gray:stream-file-length ((stream memory-file-stream))
  (length (memory-file-stream-vector stream)))

(defmethod mezzano.gray:stream-read-byte ((stream memory-file-stream))
  (with-slots (%fpos) stream
    (cond ((>= %fpos (length (memory-file-stream-vector stream)))
           :eof)
          (t (prog1
                 (aref (memory-file-stream-vector stream) %fpos)
               (incf %fpos))))))

(defclass typeface ()
  ((%font-loader :initarg :font-loader :reader font-loader)
   (%name :initarg :name :reader name)
   (%lock :reader typeface-lock)))

(defmethod initialize-instance :after ((instance typeface) &key)
  (setf (slot-value instance '%lock) (mezzano.supervisor:make-mutex (format nil "Typeface ~A lock" (name instance)))))

(defclass font ()
  ((%typeface :initarg :typeface :reader typeface)
   (%font-size :initarg :size :reader size)
   (%font-scale :reader font-scale)
   (%line-height :reader line-height)
   (%em-square-width :reader em-square-width)
   (%font-ascender :reader ascender)
   (%glyph-cache-lock :reader glyph-cache-lock)
   (%glyph-cache :reader glyph-cache)))

(defmethod print-object ((typeface typeface) stream)
  (print-unreadable-object (typeface stream :type t :identity t)
    (format stream "~S" (name typeface))))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t :identity t)
    (format stream "~S ~S" (name font) (size font))))

(defmethod name ((font font))
  (name (typeface font)))

(defmethod font-loader ((font font))
  (font-loader (typeface font)))

(defstruct glyph
  ;; Lisp character this glyph represents.
  character
  ;; 8-bit alpha mask for this glyph.
  mask
  ;; Y offset from baseline.
  yoff
  ;; X offset from pen position.
  xoff
  ;; Total width of this character.
  advance)

(defvar *font-lock* (mezzano.supervisor:make-mutex "Font lock")
  "Lock protecting the typeface and font caches.")

;; font-name (lowercase) -> typeface
(defvar *typeface-cache* (make-hash-table :test 'equal :weakness :value))
;; (lowercase font name . single-float size) -> font
(defvar *font-cache* (make-hash-table :test 'equal :weakness :value))

(defun path-map-line (path function)
  "Iterate over all the line on the contour of the path."
  (loop with iterator = (paths:path-iterator-segmented path)
     for previous-knot = nil then knot
     for (interpolation knot end-p) = (multiple-value-list (paths:path-iterator-next iterator))
     while knot
     when previous-knot
     do (funcall function previous-knot knot)
     until end-p
     finally (when knot
               (funcall function knot (nth-value 1 (paths:path-iterator-next iterator))))))

(defun rasterize-paths (paths sweep-function)
  (let ((state (aa:make-state)))
    (flet ((do-line (p1 p2)
             (aa:line-f state
                        (paths:point-x p1) (paths:point-y p1)
                        (paths:point-x p2) (paths:point-y p2))))
      (loop for path in paths
         do (path-map-line path #'do-line)))
    (aa:cells-sweep state sweep-function)))

(defun normalize-alpha (alpha)
  (min 255 (abs alpha)))

(defun scale-bb (bb scale)
  (vector (floor (* (zpb-ttf:xmin bb) scale)) (floor (* (zpb-ttf:ymin bb) scale))
          (ceiling (* (zpb-ttf:xmax bb) scale)) (ceiling (* (zpb-ttf:ymax bb) scale))))

(defun rasterize-glyph (glyph scale)
  (let* ((bb (scale-bb (zpb-ttf:bounding-box glyph) scale))
         (width (- (zpb-ttf:xmax bb) (zpb-ttf:xmin bb)))
         (height (- (zpb-ttf:ymax bb) (zpb-ttf:ymin bb)))
         (array (mezzano.gui:make-surface width height :format :a8))
         (paths (paths-ttf:paths-from-glyph glyph
                                            :scale-x scale
                                            :scale-y (- scale)
                                            :offset (paths:make-point 0 (zpb-ttf:ymax bb))
                                            :auto-orient :cw)))
    (rasterize-paths paths (lambda (x y alpha)
                             (setf (mezzano.gui:surface-pixel array (- x (zpb-ttf:xmin bb)) y)
                                   (normalize-alpha alpha))))
    array))

(defgeneric character-to-glyph (font character))

(defmethod character-to-glyph ((font font) character)
  ;; TODO: char-bits
  (let* ((code (char-code character))
         (plane (ash code -16))
         (cell (logand code #xFFFF))
         (main-cache (glyph-cache font))
         (cell-cache (aref main-cache plane)))
    (when (not cell-cache)
      (setf cell-cache (make-array (expt 2 16) :initial-element nil)
            (aref main-cache plane) cell-cache))
    (let ((glyph (aref cell-cache cell)))
      (when (not glyph)
        ;; Glyph does not exist in the cache, rasterize it.
        (mezzano.supervisor:with-mutex ((glyph-cache-lock font))
          (mezzano.supervisor:with-mutex ((typeface-lock (typeface font)))
            (cond ((and (zpb-ttf:glyph-exists-p code (font-loader font))
                        (not (zerop (zpb-ttf:code-point (zpb-ttf:find-glyph code (font-loader font))))))
                   (let* ((ttf-glyph (zpb-ttf:find-glyph code (font-loader font)))
                          (scale (font-scale font))
                          (bb (scale-bb (zpb-ttf:bounding-box ttf-glyph) scale))
                          (advance (round (* (zpb-ttf:advance-width ttf-glyph) scale))))
                     (setf glyph (make-glyph :character (code-char code)
                                             :mask (rasterize-glyph ttf-glyph scale)
                                             :yoff (zpb-ttf:ymax bb)
                                             :xoff (zpb-ttf:xmin bb)
                                             :advance advance)
                           (aref cell-cache cell) glyph)))
                  (t ;; Use Unifont fallback.
                   (let ((mask (or (sys.int::map-unifont-2d (code-char code))
                                   (sys.int::map-unifont-2d #\WHITE_VERTICAL_RECTANGLE))))
                     (setf glyph (make-glyph :character (code-char code)
                                             :mask (mezzano.gui:make-surface-from-array
                                                    mask :format :a1)
                                             :yoff 12
                                             :xoff 0
                                             :advance (array-dimension mask 1))
                           (aref cell-cache cell) glyph)))))))
      glyph)))

(defmethod initialize-instance :after ((font font) &key typeface size)
  (let ((loader (font-loader typeface)))
    (setf (slot-value font '%font-scale) (/ size (float (zpb-ttf:units/em loader)))
          (slot-value font '%line-height) (round (* (+ (zpb-ttf:ascender loader)
                                                       (- (zpb-ttf:descender loader))
                                                       (zpb-ttf:line-gap loader))
                                                    (font-scale font)))
          (slot-value font '%em-square-width) (round (* (+ (zpb-ttf:xmax (zpb-ttf:bounding-box loader))
                                                           (- (zpb-ttf:xmin (zpb-ttf:bounding-box loader))))
                                                        (font-scale font)))
          (slot-value font '%font-ascender) (round (* (zpb-ttf:ascender loader)
                                                      (font-scale font)))
          (slot-value font '%glyph-cache-lock) (mezzano.supervisor:make-mutex (format nil "~S ~S lock" (name typeface) size))
          (slot-value font '%glyph-cache) (make-array 17 :initial-element nil))))

(defun find-font (name &optional (errorp t))
  (with-open-file (s (make-pathname :name name :type "ttf" :defaults "LOCAL:>Fonts>" #+(or)"SYS:FONTS;")
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist (if errorp
                                            :error
                                            nil))
    (when s
      (let ((font-data (make-array (file-length s) :element-type '(unsigned-byte 8))))
        (read-sequence font-data s)
        (make-instance 'memory-file-stream :vector font-data)))))

(defun open-font (name size)
  (check-type name (or string symbol))
  (check-type size real)
  (let* ((typeface-key (string-downcase name))
         (font-key (cons typeface-key (float size))))
    (mezzano.supervisor:with-mutex (*font-lock*)
      (let ((font (gethash font-key *font-cache*)))
        (when font
          (return-from open-font font))
        ;; No font object, create a new one.
        (let ((typeface (gethash typeface-key *typeface-cache*)))
          (when typeface
            (setf font (make-instance 'font
                                      :typeface typeface
                                      :size (float size))
                  (gethash font-key *font-cache*) font)
            #+(or)(format t "Creating new font ~S with typeface ~S.~%" font typeface)
            (return-from open-font font)))))
    ;; Neither font nor typeface in cache. Open the TTF outside the lock
    ;; to avoid signalling with the lock held.
    (let ((loader (zpb-ttf:open-font-loader (find-font name))))
      (mezzano.supervisor:with-mutex (*font-lock*)
        ;; Repeat font test, another thread may have created the font while
        ;; the lock was dropped.
        (let ((font (gethash font-key *font-cache*)))
          (when font
            (return-from open-font font)))
        (let ((typeface (gethash typeface-key *typeface-cache*)))
          (cond (typeface
                 ;; A typeface was created for this font while the lock
                 ;; was dropped. Forget our font loader and use this one.
                 (zpb-ttf:close-font-loader loader))
                (t
                 (setf typeface (make-instance 'typeface :name (format nil "~:(~A~)" name) :font-loader loader))
                 (setf (gethash typeface-key *typeface-cache*) typeface)
                 (sys.int::make-weak-pointer
                  typeface
                  :finalizer (lambda ()
                               (zpb-ttf:close-font-loader loader)))
                 #+(or)(format t "Creating new typeface ~S.~%" typeface)))
          (let ((font (make-instance 'font
                                     :typeface typeface
                                     :size (float size))))
            #+(or)(format t "Creating new font ~S with typeface ~S.~%" font typeface)
            (setf (gethash font-key *font-cache*) font)
            font))))))

(defun string-display-width (string font)
  (loop
     for ch across string
     for glyph = (mezzano.gui.font:character-to-glyph font ch)
     summing (mezzano.gui.font:glyph-advance glyph)))

(defun draw-string (string font surface x y colour)
  (loop
     with pen = x
     for ch across string
     for glyph = (mezzano.gui.font:character-to-glyph font ch)
     for mask = (mezzano.gui.font:glyph-mask glyph)
     do
       (mezzano.gui:bitset :blend
                           (mezzano.gui:surface-width mask) (mezzano.gui:surface-height mask)
                           colour
                           surface
                           (+ pen (mezzano.gui.font:glyph-xoff glyph))
                           (- y (mezzano.gui.font:glyph-yoff glyph))
                           mask 0 0)
       (incf pen (mezzano.gui.font:glyph-advance glyph))
     finally (return pen)))

(defun character-to-raw-glyph (font ch)
  (let ((code (char-code ch)))
    (mezzano.supervisor:with-mutex ((glyph-cache-lock font))
      (mezzano.supervisor:with-mutex ((typeface-lock (typeface font)))
        (when (and (zpb-ttf:glyph-exists-p code (font-loader font))
                   (not (zerop (zpb-ttf:code-point (zpb-ttf:find-glyph code (font-loader font))))))
          (let* ((glyph (zpb-ttf:find-glyph code (font-loader font)))
                 (scale (font-scale font))
                 (bb (scale-bb (zpb-ttf:bounding-box glyph) scale))
                 (advance (round (* (zpb-ttf:advance-width glyph) scale))))
            (values (paths-ttf:paths-from-glyph glyph
                                                :scale-x scale
                                                :scale-y (- scale)
                                                :offset (paths:make-point 0 (zpb-ttf:ymax bb))
                                                :auto-orient :cw)
                    advance
                    (zpb-ttf:ymax bb)
                    (zpb-ttf:xmin bb))))))))

(defun draw-stroked-string (string font surface x y colour stroke-colour stroke-width)
  (loop
     with pen = x
     for ch across string
     do
       (multiple-value-bind (paths advance yoff xoff)
           (character-to-raw-glyph font ch)
         (when advance
           (rasterize-paths paths
                            (lambda (px py alpha)
                              (setf (mezzano.gui:surface-pixel surface (+ pen px) (+ (- y yoff) py))
                                    (mezzano.gui:make-colour
                                     (mezzano.gui:colour-red colour)
                                     (mezzano.gui:colour-green colour)
                                     (mezzano.gui:colour-blue colour)
                                     (/ (normalize-alpha alpha) 255.0)))))
           (rasterize-paths (paths:stroke-path paths stroke-width :assume-type :closed-polyline)
                            (lambda (px py alpha)
                              (setf (mezzano.gui:surface-pixel surface (+ pen px) (+ (- y yoff) py))
                                    (mezzano.gui:colour-over
                                     (mezzano.gui:surface-pixel surface (+ pen px) (+ (- y yoff) py))
                                     (mezzano.gui:make-colour
                                      (mezzano.gui:colour-red stroke-colour)
                                      (mezzano.gui:colour-green stroke-colour)
                                      (mezzano.gui:colour-blue stroke-colour)
                                      (/ (normalize-alpha alpha) 255.0))))))
           (incf pen advance)))))
