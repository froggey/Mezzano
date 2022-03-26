;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:net.tuxee.aa-misc
  (:use #:common-lisp)
  (:nicknames #:aa-misc)
  (:export ;; minimal image support (for testing purpose!)
           #:make-image
           #:image-width
           #:image-height
           ;; Rendering functions.
           #:image-put-pixel
           #:image-put-span
           ;; Loading, saving and displaying image.
           #:load-image
           #:save-image
           #:show-image
           #:*external-viewer*
           ))

(in-package #:net.tuxee.aa-misc)

(defvar *external-viewer* "xv"
  "Default program to run to display a PNM image.")

(deftype octet () '(unsigned-byte 8))

(defun make-image (width height &optional default-color)
  "Create a new image.

width -- width of the image
height -- height of the image
default-color -- if not NIL, then the image is filled with the
specified color. If unspecified, then the contents of the image
is also unspecified.

Return the newly created image."
  (let ((image (make-array (list height width 3)
                           :element-type 'octet)))
    (when default-color
      (loop for y below height
           do (loop for x below width
                   do (loop for rgb below 3
                         do (setf (aref image y x rgb) (aref default-color rgb))))))
    image))

(defun image-width (image)
  (array-dimension image 1))

(defun image-height (image)
  (array-dimension image 0))

;;;--[ Rendering ]-----------------------------------------------------------

(declaim (inline blend-value))
(defun blend-value (a b alpha)
  (max 0 (min 255 (floor (+ (* (- 256 alpha) a)
                            (* alpha b))
                         256))))

(defun alpha/normalized (alpha)
  (min 255 (abs alpha)))

(defun alpha/even-odd (alpha)
  (min 255 (- 256 (abs (- 256 (mod (abs alpha) 512))))))

(defun image-put-pixel (image &optional (color #(0 0 0)) (opacity 1.0) (alpha-function :normalized))
  (check-type image (array octet (* * 3)))
  (let ((width (image-width image))
        (height (image-height image)))
    (case alpha-function
      (:normalized
       (setf alpha-function #'alpha/normalized))
      (:even-odd
       (setf alpha-function #'alpha/even-odd)))
    (if (/= opacity 1.0)
        (lambda (x y alpha)
          (declare (optimize speed (safety 0) (debug 0)))
          (when (and (<= 0 x (1- width))
                     (<= 0 y (1- height)))
            (loop for rgb below 3
               do (setf (aref image y x rgb)
                        (blend-value (aref image y x rgb)
                                     (aref color rgb)
                                     (floor (* opacity (funcall alpha-function alpha))))))))
        (lambda (x y alpha)
          (declare (optimize speed (safety 0) (debug 0)))
          (when (and (<= 0 x (1- width))
                     (<= 0 y (1- height)))
            (loop for rgb below 3
               do (setf (aref image y x rgb)
                        (blend-value (aref image y x rgb)
                                     (aref color rgb)
                                     (funcall alpha-function alpha)))))))))

(defun image-put-span (image &optional (color #(0 0 0)) (opacity 1.0) (alpha-function :normalized))
  (check-type image (array octet (* * 3)))
  (let ((width (image-width image))
        (height (image-height image)))
    (case alpha-function
      (:normalized
       (setf alpha-function #'alpha/normalized))
      (:even-odd
       (setf alpha-function #'alpha/even-odd)))
    (if (/= opacity 1.0)
        (lambda (x1 x2 y alpha)
          (declare (optimize speed (safety 0) (debug 0)))
          (when (and (< x1 width)
                     (> x2 0)
                     (<= 0 y (1- height)))
            (setf alpha (funcall alpha-function alpha))
            (loop for x from (max 0 x1) below (min x2 width)
               do (loop for rgb below 3
                     do (setf (aref image y x rgb)
                              (blend-value (aref image y x rgb)
                                           (aref color rgb)
                                           (floor (* opacity alpha))))))))
        (lambda (x1 x2 y alpha)
          (declare (optimize speed (safety 0) (debug 0)))
          (when (and (< x1 width)
                     (> x2 0)
                     (<= 0 y (1- height)))
            (setf alpha (funcall alpha-function alpha))
            (loop for x from (max 0 x1) below (min x2 width)
               do (loop for rgb below 3
                     do (setf (aref image y x rgb)
                              (blend-value (aref image y x rgb)
                                           (aref color rgb)
                                           alpha)))))))))

;;;--[ load/save/display ]---------------------------------------------------

(defun %load-image/pnm (filename)
  (with-open-file (file filename :element-type 'octet)
    (flet ((read-word (&optional limit)
             "Read the next \"word\" (a sequence of non-space
             characters) skipping initial blanks. The first blank
             character after the word is also consumed."
             (declare (ignore limit))   ; FIXME
             (let ((result (make-array 0
                                       :element-type 'octet
                                       :fill-pointer 0
                                       :adjustable t)))
               ;; skip blanks, extract the word, consume the following
               ;; blank.
               (loop for byte = (read-byte file)
                  while (member byte '(9 10 13 32))
                  finally (vector-push-extend byte result))
               (loop for byte = (read-byte file)
                  until (member byte '(9 10 13 32))
                  do (vector-push-extend byte result))
               result))
           (parse-ascii-integer (seq)
             "Parse an integer represented by the ASCII charset
             in the array SEQ."
             (let ((result 0))
               (loop for digit in (coerce seq 'list)
                  unless (<= 48 digit 57)
                  do (error "Invalid ASCII integer")
                  do (setf result (+ (* 10 result) (- digit 48))))
               result)))
      (let ((format (read-word 3)))
        (unless (equalp format #(80 54))
          (error "Expected P6 image format (got ASCII sequence ~S)" (subseq format 0 16)))
        (let ((width (parse-ascii-integer (read-word 10)))
              (height (parse-ascii-integer (read-word 10)))
              (maxval (parse-ascii-integer (read-word 10))))
          (when (/= maxval 255)
            (error "Expected 24 bits color image"))
          (unless (and (<= 1 width 4096)
                       (<= 1 height 4096))
            (error "Cowardly refusing to read an image of size ~Dx~D" width height))
          (let* ((image (make-array (list height width 3) :element-type 'octet))
                 (index 0)
                 (end-index (apply #'* (array-dimensions image))))
            ;; skip blanks to find the first byte of data.
            (loop for byte = (read-byte file)
               while (member byte '(9 10 13 32))
               finally (setf (row-major-aref image index) byte))
            (incf index)
            ;; read the rest of the data.
            (loop while (< index end-index)
                 for byte = (read-byte file)
                 do (setf (row-major-aref image index) byte)
                 (incf index))
            image))))))

(defun load-image (filename format)
  (ecase format
    (:pnm
     (%load-image/pnm filename))))

(defun make-array-flat-displaced (array &optional (start 0))
  (make-array (apply #'* (array-dimensions array))
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset start))

(defun save-image/pnm (filename image)
  "Save image with PNM format into the file with filename
FILENAME. IMAGE must be an (UNSIGNED-BYTE 8) array of
dimension (* * 3). Last axis represent the RGB component in that
order."
  (with-open-file (file filename
                        :element-type 'octet
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :overwrite)
    (labels ((write-ascii-integer (n stream)
               (when (minusp n)
                 (write-byte 45 stream) ; #\-
                 (setf n (- n)))
               (write-sequence (loop with digits = ()
                                  for digit = (mod n 10)
                                  do (push (+ 48 digit) digits)
                                  (setf n (floor n 10))
                                  while (plusp n)
                                  finally (return digits))
                               stream)))
      ;; "P6" <width> <height> <maxval>
      (write-sequence #(80 54) file)
      (write-byte 32 file)
      (write-ascii-integer (array-dimension image 1) file)
      (write-byte 32 file)
      (write-ascii-integer (array-dimension image 0) file)
      (write-byte 32 file)
      (write-ascii-integer 255 file)
      (write-byte 10 file)
      (write-sequence (make-array-flat-displaced image) file))))

(defun save-image (filename image format)
  (ecase format
    ((:pnm :ppm)
     (save-image/pnm filename image)))
  (values))

;;; WARNING: Run external program.
(defun show-image (image &optional (external-viewer *external-viewer*))
  "Display IMAGE using the specified external viewver."
  (let ((temp-filename "/tmp/.cl-aa-tmp.pnm"))
    (save-image temp-filename image :pnm)
    (asdf:run-shell-command "~S ~S" external-viewer temp-filename)
    (delete-file temp-filename)))
