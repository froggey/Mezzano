(in-package #:sys.int)

(declaim (special *unifont-bmp*))

(defvar *framebuffer* nil)
(defparameter *unifont-glyph-cache* (make-array 256 :initial-element nil))

(defun map-unifont (c)
  (let ((code (char-code c)))
    ;; Unifont only covers plane 0
    (when (<= code #xffff)
      (let ((row (aref *unifont-bmp* (ash code -8))))
	(when row
	  (aref row (logand code #xff)))))))

(defun unifont-glyph-width (character)
  (let ((glyph (map-unifont character)))
    (if (and glyph (eql (array-dimension glyph 0) 32))
	16
	8)))

(defun render-unifont-glyph (c)
  "Produce an array containing the glyph for C that can be blitted directly to the screen."
  (let* ((uniglyph (or (map-unifont c)
		       ;; Fall back on the non-printing glyph for characters that
		       ;; Unifont does not cover.
		       #(#x00 #x54 #x2A #x54 #x2A #x54 #x2A #x54
			 #x2A #x54 #x2A #x54 #x2A #x54 #x2A #x00)))
	 (is-fullwidth (= (length uniglyph) 32))
	 (screen-glyph (make-array (list 16 (if is-fullwidth 16 8))
				   :element-type '(unsigned-byte 32))))
    (if is-fullwidth
	;; Fullwidth.
	(dotimes (i 16)
	  (dotimes (j 8)
	    (setf (aref screen-glyph i j)
		  (if (= 0 (logand (ash 1 (- 7 j)) (aref uniglyph (* i 2))))
		      #xFF000000
		      #xFFB2B2B2)
		  (aref screen-glyph i (+ j 8))
		  (if (= 0 (logand (ash 1 (- 7 j)) (aref uniglyph (1+ (* i 2)))))
		      #xFF000000
		      #xFFB2B2B2))))
	;; Halfwidth.
	(dotimes (i 16)
	  (dotimes (j 8)
	    (setf (aref screen-glyph i j)
		  (if (= 0 (logand (ash 1 (- 7 j)) (aref uniglyph i)))
		      #xFF000000
		      #xFFB2B2B2)))))
    screen-glyph))

(defun get-unifont-glyph (c)
  ;; Only use the cache for characters in the BMP.
  (if (<= (char-code c) #xFFFF)
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
    (if (eql (array-dimension glyph 1) 16)
	(%bitblt 16 16 glyph 0 0
		 framebuffer y x)
	(%bitblt 16 8 glyph 0 0
		 framebuffer y x))))

(defvar *framebuffer-x* 0)
(defvar *framebuffer-y* 0)

(defun framebuffer-write (character)
  (cond
    ((eql character #\Newline)
     ;; Clear the next line.
     (setf *framebuffer-x* 0
           *framebuffer-y* (if (>= (+ y 16) (array-dimension *framebuffer* 0))
                               0
                               (+ y 16)))
     (%bitset 16 (array-dimension *framebuffer* 1) #xFF000000 *framebuffer* *framebuffer-y* 0))
    (t (let ((width (if (eql character #\Space) 8 (unifont-glyph-width character))))
         (when (> (+ *framebuffer-x* width) (array-dimension *framebuffer* 1))
           ;; Advance to the next line.
           ;; Maybe should clear the end of the current line?
           (setf *framebuffer-x* 0
                 *framebuffer-y* (if (>= (+ *framebuffer-y* 16) (array-dimension *framebuffer* 0))
                                     0
                                     (+ *framebuffer-y* 16)))
           (%bitset 16 (array-dimension *framebuffer* 1) #xFF000000 *framebuffer* *framebuffer-y* 0))
         (if (eql character #\Space)
             (%bitset 16 8 #xFF000000 *framebuffer* *framebuffer-y* *framebuffer-x*)
             (render-char-at character *framebuffer* *framebuffer-x* *framebuffer-y*))
         (incf *framebuffer-x* width)))))


