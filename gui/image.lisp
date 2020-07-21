;;;; Image support

(in-package :mezzano.gui.image)

;; FIXME: This should probably be weak.
;; ### Normal weakness seems excessive. Soft references?
(defvar *image-cache* (make-hash-table :test 'equal :synchronized t))

(defun flush-image-cache ()
  (clrhash *image-cache*)
  (values))

(defun load-jpeg (path)
  (ignore-errors
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (multiple-value-bind (data height width channels)
          (jpeg:decode-stream stream)
        (transcode-cl-jpeg-buffer (mezzano.gui:make-surface width height)
                                  0 0
                                  data width height channels)))))

(defun transcode-cl-jpeg-buffer (destination-surface x-offset y-offset data width height channels)
  (when (not (eql channels 3))
    (error "Unsupported JPEG image, too many or too few channels."))
  (when (not (eql (mezzano.gui:surface-format destination-surface) :argb32))
    (error "Unsupported destination surface format ~S."
           (mezzano.gui:surface-format destination-surface)))
  (let* ((dest-array (mezzano.gui:surface-pixels destination-surface))
         (dest-width (mezzano.gui:surface-width destination-surface))
         (dest-height (mezzano.gui:surface-height destination-surface)))
    (check-type dest-array (simple-array (unsigned-byte 32) (* *)))
    (check-type data (simple-array (unsigned-byte 8) (*)))
    (assert (<= 0 x-offset (+ x-offset width) dest-width))
    (assert (<= 0 y-offset (+ y-offset height) dest-height))
    (locally
      (declare (type fixnum dest-width dest-height)
               (type (simple-array (unsigned-byte 32) (* *)) dest-array)
               (type (simple-array (unsigned-byte 8) (*)) data)
               (optimize speed (safety 0)))
      (loop
         for y fixnum below height do
           (loop
              for x fixnum below width
              for src-idx fixnum = (* (the fixnum (+ (the fixnum (* y width)) x)) 3)
              for dst-idx fixnum = (+ (the fixnum (* (the fixnum (+ y-offset y)) dest-width))
                                      (the fixnum (+ x-offset x)))
              do (setf (row-major-aref dest-array dst-idx)
                       (the fixnum
                            (logior #xFF000000
                                    (the fixnum (ash (aref data (the fixnum (+ src-idx 2))) 16))
                                    (the fixnum (ash (aref data (the fixnum (+ src-idx 1))) 8))
                                    (aref data src-idx)))))))
    destination-surface))

(defun load-png (path)
  (ignore-errors
    (let* ((png (png-read:read-png-file path))
           (data (png-read:image-data png))
           (width (png-read:width png))
           (height (png-read:height png))
           (array (make-array (list height width) :element-type '(unsigned-byte 32))))
      (ecase (png-read:colour-type png)
        (:truecolor-alpha
         (dotimes (y height)
           (dotimes (x width)
             (setf (aref array y x) (logior (ash (aref data x y 0) 16)
                                            (ash (aref data x y 1) 8)
                                            (aref data x y 2)
                                            (ash (aref data x y 3) 24))))))
        (:truecolor
         (dotimes (y height)
           (dotimes (x width)
             (setf (aref array y x) (logior (ash (aref data x y 0) 16)
                                            (ash (aref data x y 1) 8)
                                            (aref data x y 2)
                                            (ash #xFF 24)))))))
      (mezzano.gui:make-surface-from-array array))))

(defun load-image (path)
  (let* ((truename (truename path))
         (image (gethash truename *image-cache*)))
    (unless image
      (setf image (or (load-jpeg truename)
                      (load-png truename)
                      (error "Unable to load ~S." path)))
      (setf (gethash truename *image-cache*) image))
    image))
