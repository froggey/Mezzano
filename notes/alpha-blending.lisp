;; (mod 256) integer arithmetic.
(declaim (inline +! -! *! /! %!))
(defun +! (x y) (logand (+ x y) #xFF))
(defun -! (x y) (logand (- x y) #xFF))
(defun *! (x y) (logand (* x y) #xFF))
(defun /! (x y) (logand (truncate x y) #xFF))
(defun %! (x y) (logand (rem x y) #xFF))

(defun alpha-blend-one (dest dest-offset colour)
  "Alpha-blend COLOUR into DEST.
DEST must be of type (simple-array (unsigned-byte 32) (* *)).
DEST-OFFSET must be a row-major index into DEST.
COLOUR must be a 32-bit ARGB pixel."
  (declare (type (unsigned-byte 32) colour)
           (type (simple-array (unsigned-byte 32) (* *)) dest)
           (optimize speed))
  (let ((ca (ldb (byte 8 24) colour))
        (cr (ldb (byte 8 16) colour))
        (cg (ldb (byte 8  8) colour))
        (cb (ldb (byte 8  0) colour)))
    (when (zerop ca)
      ;; That was easy. COLOUR is totally transparent.
      (return-from alpha-blend-one))
    ;; Do complicated alpha blending.
    (let* ((d (row-major-aref dest dest-offset))
           (da (ldb (byte 8 24) d))
           (dr (ldb (byte 8 16) d))
           (dg (ldb (byte 8  8) d))
           (db (ldb (byte 8  0) d))
           (fa (+! ca (* da (- 255 ca))))
           (fr (/! (+ (* cr ca)
                      (* dr da (- 255 ca)))
                   fa))
           (fg (/! (+ (* cg ca)
                      (* dg da (- 255 ca)))
                   fa))
           (fb (/! (+ (* cb ca)
                      (* db da (- 255 ca)))
                   fa)))
      (setf (row-major-aref dest dest-offset)
            (logior (ash da 24) ; leave the old alpha in place.
                    (ash fr 16)
                    (ash fg  8)
                    fb)))))

(defun alpha-blend-one-sdl (dest dest-offset colour)
  "Alpha-blend COLOUR into DEST.
DEST must be of type (simple-array (unsigned-byte 32) (* *)).
DEST-OFFSET must be a row-major index into DEST.
COLOUR must be a 32-bit ARGB pixel."
  (declare (type (unsigned-byte 32) colour)
           (type (simple-array (unsigned-byte 32) (* *)) dest)
           (optimize speed))
  (let ((ca (ldb (byte 8 24) colour))
        (cr (ldb (byte 8 16) colour))
        (cg (ldb (byte 8  8) colour))
        (cb (ldb (byte 8  0) colour)))
    (when (zerop ca)
      ;; That was easy. COLOUR is totally transparent.
      (return-from alpha-blend-one-sdl))
    (flet ((blend (s d a)
             (ldb (byte 8 0)
                  (+ (ash (* (- s d) a) -8) d))))
      ;; Do complicated alpha blending.
      (let* ((d (row-major-aref dest dest-offset))
             (da (ldb (byte 8 24) d))
             (dr (ldb (byte 8 16) d))
             (dg (ldb (byte 8  8) d))
             (db (ldb (byte 8  0) d))
             #+nil(fa (+! ca (* da (- 255 ca))))
             (fr (blend cr dr ca))
             (fg (blend cg dg ca))
             (fb (blend cb db ca)))
        (setf (row-major-aref dest dest-offset)
              (logior (ash da 24) ; leave the old alpha in place.
                      (ash fr 16)
                      (ash fg  8)
                      fb))))))

(defun alpha-blend-one-gl-src-alpha-one-minus-src-alpha-float (dest dest-offset colour)
  "Alpha-blend COLOUR into DEST.
DEST must be of type (simple-array (unsigned-byte 32) (* *)).
DEST-OFFSET must be a row-major index into DEST.
COLOUR must be a 32-bit ARGB pixel."
  (declare (type (unsigned-byte 32) colour)
           (type (simple-array (unsigned-byte 32) (* *)) dest)
           (optimize speed))
  (let ((ca (ldb (byte 8 24) colour))
        (cr (ldb (byte 8 16) colour))
        (cg (ldb (byte 8  8) colour))
        (cb (ldb (byte 8  0) colour)))
    (when (zerop ca)
      ;; That was easy. COLOUR is totally transparent.
      (return-from alpha-blend-one-gl-src-alpha-one-minus-src-alpha-float))
    (flet ((blend (s d sa)
             (logand #xFF (min (truncate (+ (* s (/ sa 255.0))
                                       (* d (- 1 (/ sa 255.0)))))
                               255))))
      ;; Do complicated alpha blending.
      (let* ((d (row-major-aref dest dest-offset))
             (da (ldb (byte 8 24) d))
             (dr (ldb (byte 8 16) d))
             (dg (ldb (byte 8  8) d))
             (db (ldb (byte 8  0) d))
             (fa (blend ca da ca))
             (fr (blend cr dr ca))
             (fg (blend cg dg ca))
             (fb (blend cb db ca)))
        (setf (row-major-aref dest dest-offset)
              (logior (ash fa 24)
                      (ash fr 16)
                      (ash fg  8)
                      fb))))))

(defun alpha-blend-one-gl-src-alpha-one-minus-src-alpha (dest dest-offset colour)
  "Alpha-blend COLOUR into DEST.
DEST must be of type (simple-array (unsigned-byte 32) (* *)).
DEST-OFFSET must be a row-major index into DEST.
COLOUR must be a 32-bit ARGB pixel."
  (declare (type (unsigned-byte 32) colour)
           (type (simple-array (unsigned-byte 32) (* *)) dest)
           (optimize speed))
  (let ((ca (ldb (byte 8 24) colour))
        (cr (ldb (byte 8 16) colour))
        (cg (ldb (byte 8  8) colour))
        (cb (ldb (byte 8  0) colour)))
    (when (zerop ca)
      ;; That was easy. COLOUR is totally transparent.
      (return-from alpha-blend-one-gl-src-alpha-one-minus-src-alpha))
    (flet ((blend (s d sa)
             (min (+ (truncate (* s sa) 255)
                     (truncate (* d (- 255 sa)) 255))
                  255)))
      ;; Do complicated alpha blending.
      (let* ((d (row-major-aref dest dest-offset))
             (da (ldb (byte 8 24) d))
             (dr (ldb (byte 8 16) d))
             (dg (ldb (byte 8  8) d))
             (db (ldb (byte 8  0) d))
             (fa (blend ca da ca))
             (fr (blend cr dr ca))
             (fg (blend cg dg ca))
             (fb (blend cb db ca)))
        (setf (row-major-aref dest dest-offset)
              (logior (ash fa 24)
                      (ash fr 16)
                      (ash fg  8)
                      fb))))))
