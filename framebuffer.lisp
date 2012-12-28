(in-package #:sys.int)

(declaim (special *unifont-bmp*))

(defparameter *unifont-glyph-cache* (make-array 256 :initial-element nil))
(defparameter *unifont-2d-glyph-cache* (make-array 256 :initial-element nil))

(defun map-unifont (c)
  (let ((code (char-code c)))
    ;; Unifont only covers plane 0
    (when (and (<= code #xffff)
               (zerop (char-bits c)))
      (let ((row (aref *unifont-bmp* (ash code -8))))
	(when row
	  (aref row (logand code #xff)))))))

(defun unifont-glyph-width (character)
  (let ((glyph (map-unifont character)))
    (cond ((null glyph)
           (* (length (char-name character)) 8))
          (t (truncate (length glyph) 16)))))

(defun render-unifont-glyph (c)
  "Produce an array containing the glyph for C that can be blitted directly to the screen."
  (let ((uniglyph (map-unifont c)))
    (cond (uniglyph
           (let* ((width (truncate (length uniglyph) 16))
                  (screen-glyph (make-array (list 16 width)
                                            :element-type '(unsigned-byte 32))))
             (dotimes (y 16)
               (dotimes (x width)
                 (setf (aref screen-glyph y x)
                       (if (= 0 (aref uniglyph (+ x (* y width))))
                           #xFF000000
                           #xFFB2B2B2))))
             screen-glyph))
          (t ;; Render the character name surounded by a box.
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
         (glyph nil)
         (1d-glyph nil))
    ;; Unifont only covers plane 0
    (when (and (<= code #xffff)
               (zerop (char-bits c)))
      (setf cache-row (svref *unifont-2d-glyph-cache* row))
      (unless cache-row
        (setf cache-row (make-array 256 :initial-element nil)
              (svref *unifont-2d-glyph-cache* row) cache-row))
      (setf glyph (svref cache-row cell))
      (unless glyph
        (setf 1d-glyph (map-unifont c))
        (when (not 1d-glyph) (return-from map-unifont-2d nil))
        (setf glyph (make-array (list 16 (truncate (length 1d-glyph) 16)) :displaced-to 1d-glyph)
              (svref cache-row cell) glyph))
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
