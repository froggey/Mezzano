(in-package :sys.int)

(declaim (special *unifont-bmp* *unifont-bmp-data*))

(defparameter *unifont-glyph-cache* (make-array 256 :initial-element nil))
(defparameter *unifont-2d-glyph-cache* (make-array 256 :initial-element nil))

(defun map-unifont (c)
  "Locate information for character C in Unifont.
Returns the character position in *UNIFONT-BMP-DATA* and the character pixel width (8 or 16) or NIL if the character is not present."
  (let ((code (char-code c)))
    ;; Unifont only covers plane 0, no fancy stuff either.
    (when (and (<= code #xFFFF)
               (zerop (char-bits c)))
      (let ((row (aref *unifont-bmp* (ldb (byte 8 8) code))))
	(when row
          (let ((data (aref row (ldb (byte 8 0) code))))
            (when (logtest data #x80000000)
              (values (ldb (byte 30 0) data)
                      (if (logtest data #x40000000) 16 8)))))))))

(defun unifont-glyph-width (character)
  (multiple-value-bind (offset width)
      (map-unifont character)
    (declare (ignore offset))
    (or width
        (* (length (char-name character)) 8))))

(defun render-unifont-glyph (c)
  "Produce an array containing the glyph for C that can be blitted directly to the screen."
  (multiple-value-bind (glyph-offset width)
      (map-unifont c)
    (cond (glyph-offset
           (let* ((screen-glyph (make-array (list 16 width)
                                            :element-type '(unsigned-byte 32))))
             (dotimes (y 16)
               (dotimes (x width)
                 (setf (aref screen-glyph y x)
                       (if (zerop (aref *unifont-bmp-data* (+ glyph-offset x (* y width))))
                           #xFF000000
                           #xFFB2B2B2))))
             screen-glyph))
          (t ;; Render the character name surounded by a box.
           ;; ### Assumes that names only contain 8-wide characters!
           (let* ((name (char-name c))
                  (screen-glyph (make-array (list 16 (* (length name) 8))
                                            :element-type '(unsigned-byte 32))))
             (dotimes (i (length name))
               (let ((foo (get-unifont-glyph (char name i))))
                 (%bitblt 16 8 foo 0 0
                          screen-glyph 0 (* i 8))))
             ;; Draw box.
             (%bitset 1 (* (length name) 8) #xFFB2B2B2 screen-glyph 0 0)
             (%bitset 1 (* (length name) 8) #xFFB2B2B2 screen-glyph 15 0)
             (dotimes (i 16)
               (setf (aref screen-glyph i 0) #xFFB2B2B2
                     (aref screen-glyph i (1- (* (length name) 8))) #xFFB2B2B2))
             screen-glyph)))))

(defun map-unifont-2d (c)
  ;; TODO: Generate missing characters here.
  (let* ((code (char-code c))
         (row (ldb (byte 8 8) code))
         (cell (ldb (byte 8 0) code))
         (cache-row nil)
         (glyph nil))
    ;; Unifont only covers plane 0
    (when (and (<= code #xffff)
               (zerop (char-bits c)))
      (setf cache-row (svref *unifont-2d-glyph-cache* row))
      (unless cache-row
        (setf cache-row (make-array 256 :initial-element nil)
              (svref *unifont-2d-glyph-cache* row) cache-row))
      (setf glyph (svref cache-row cell))
      (unless glyph
        (multiple-value-bind (glyph-offset width)
            (map-unifont c)
          (when (not glyph-offset) (return-from map-unifont-2d nil))
          (setf glyph (make-array (list 16 width)
                                  :displaced-to *unifont-bmp-data*
                                  :displaced-index-offset glyph-offset)
                (svref cache-row cell) glyph)))
      glyph)))

(defun get-unifont-glyph (c)
  ;; Only use the cache for characters in the BMP.
  (if (and (<= (char-code c) #xFFFF)
           (zerop (char-bits c)))
      (let* ((row (ash (char-code c) -8))
	     (cell (logand (char-code c) #xFF))
	     (cache-row (svref *unifont-glyph-cache* row)))
	(unless cache-row
	  (setf cache-row (make-array 256 :initial-element nil)
		(svref *unifont-glyph-cache* row) cache-row))
	(let ((glyph (svref cache-row cell)))
	  (unless glyph
	    (setf glyph (render-unifont-glyph c)
		  (svref cache-row cell) glyph))
	  glyph))
      (render-unifont-glyph c)))

(defun render-char-at (c framebuffer x y)
  (let ((glyph (get-unifont-glyph c)))
    (%bitblt 16 (array-dimension glyph 1) glyph 0 0
             framebuffer y x)))
