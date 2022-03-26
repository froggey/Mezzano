;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package #:skippy)

(defvar *effective-graphic-control* nil
  "The graphic control extension in effect for the current image.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +extension-introducer+ #x21)
  (defconstant +graphic-control-label+ #xF9)
  (defconstant +comment-label+ #xFE)
  (defconstant +application-label+ #xFF) 
  (defconstant +plain-text-label+ #x01))

(defclass graphic-control-extension ()
  ((delay-time
    :initarg :delay-time
    :reader delay-time)
   (disposal-method
    :initarg :disposal-method
    :reader disposal-method)
   (transparency-index
    :initarg :transparency-index
    :reader transparency-index)))

(defmacro bind-bits ((integer size) bindings &body body)
  (let ((value (gensym))
        (names (mapcar #'first bindings))
        (sizes (mapcar #'second bindings)))
    (let ((total-size (apply #'+ sizes)))
      (when (> total-size size)
        (error "Bitfield total size (~D) is larger than provided integer size (~D)"
               total-size size))
      `(let* ((,value ,integer)
              ,@(loop for offset = size then (- offset field-size)
                      for name in names
                      for field-size in sizes
                      when name
                      collect (list name
                                    `(ldb (byte ,field-size ,(- offset field-size)) ,value))))
         ,@body))))

(defun read-uint16 (stream)
  (logand #xFFFF (+ (ash (read-byte stream) 0)
                    (ash (read-byte stream) 8))))

(defun read-color (stream)
  (logand #xFFFFFF (+ (ash (read-byte stream) 16)
                      (ash (read-byte stream)  8)
                      (ash (read-byte stream)  0))))

(defun read-color-table (count stream)
  (let ((color-table (make-color-table)))
    (dotimes (i count color-table)
      (add-color (read-color stream) color-table))))

(defun stream-position (stream &key (offset 0))
  "FILE-POSITION may return NIL or may signal an error \(for e.g. Gray
streams); wrap it."
  (let ((pos (ignore-errors (file-position stream))))
    (when pos
      (+ pos offset))))

(defun advance-stream-position (stream count)
  "Skip past COUNT bytes of input in STREAM."
  (let ((pos (stream-position stream :offset count)))
    (if pos
        (file-position stream pos)
        (dotimes (i count)
          (read-byte stream)))))

(defun merge-graphic-control (image)
  (when *effective-graphic-control*
    (setf (delay-time image)
          (delay-time *effective-graphic-control*)
          (disposal-method image)
          (disposal-method *effective-graphic-control*)
          (transparency-index image)
          (transparency-index *effective-graphic-control*)
          *effective-graphic-control* nil)))

(defun read-image (context stream)
  (let ((left-position (read-uint16 stream))
        (top-position (read-uint16 stream))
        (width (read-uint16 stream))
        (height (read-uint16 stream))
        (flags (read-byte stream))
        (color-table nil))
    (bind-bits (flags 8)
        ((local-color-table-flag 1)
         (interlaced-flag        1)
         (sort-flag              1)
         (reserved               2)
         (color-table-size       3))
      (declare (ignore sort-flag reserved))
      (when (plusp local-color-table-flag)
        (let ((color-table-entry-count (expt 2 (1+ color-table-size))))
          (setf color-table (read-color-table color-table-entry-count
                                              stream))))
      (let* ((code-size (read-byte stream))
             (image-data (make-image-data width height)))
        (lzw-decompress image-data code-size context stream)
        (let ((image 
               (make-image :left-position left-position
                           :top-position top-position
                           :width width
                           :height height
                           :image-data image-data
                           :color-table color-table
                           :interlacedp (plusp interlaced-flag))))
          (when (plusp interlaced-flag)
            (replace image-data (deinterlaced-image-data image)))
          (merge-graphic-control image)
          image)))))

(defun disposal-method-keyword (method)
  (or (car (rassoc method *disposal-methods*))
      :unspecified))

(defun read-graphic-control-extension (stream)
  ;; STREAM is positioned just after the Graphic Control Label
  (let ((block-size (read-byte stream)))
    (when (/= block-size 4)
      (error 'unexpected-value
             :description "block-size"
             :expected-value 4
             :actual-value block-size
             :source stream
             :source-position (stream-position stream :offset -1)))
    (let ((fields (read-byte stream))
          (delay-time (read-uint16 stream))
          (transparency-index (read-byte stream))
          (block-terminator (read-byte stream)))
      (when (/= block-terminator 0)
        (error 'unexpected-value
               :description "block-terminator"
               :actual-value block-terminator
               :expected-value 0
               :source stream
               :source-position (stream-position stream :offset -1)))
      (bind-bits (fields 8)
          ((reserved               3)
           (disposal-method        3)
           (user-input-flag        1)
           (transparent-color-flag 1))
        (declare (ignore reserved user-input-flag))
        (when (zerop transparent-color-flag)
          (setf transparency-index nil))
        (make-instance 'graphic-control-extension
                       :delay-time delay-time
                       :disposal-method (disposal-method-keyword disposal-method)
                       :transparency-index transparency-index)))))



(defun skip-data-blocks (stream)
  ;; Data blocks take the form of a series of (<size octet> <vector of
  ;; <size octet>s of data>) sequences. A size octet of zero
  ;; terminates a data block.
  (loop
   (let ((size (read-byte stream)))
     (when (zerop size)
       (return))
     (advance-stream-position stream size))))

(defun read-application-extension (stream data-stream)
  (let ((block-size (read-byte stream)))
    (let ((block (make-array block-size :element-type 'octet)))
      (read-sequence block stream)
      ;;; XXX If skippy ever supports more application extensions, it
      ;;; would make sense to put them in a table instead of
      ;;; hardcoding specific extension identifiers here.
      (when (equalp block *netscape-signature*)
        (setf (loopingp data-stream) t)))
    (skip-data-blocks stream)))

(defun read-comment-extension (stream)
  (flet ((ascii-char (code)
           ;;; FIXME: This assumes ASCII code-char mapping; could keep a table
           ;;; instead.
           (code-char (min code 127))))
    (with-output-to-string (output)
      (let ((block (make-array 255 :element-type 'octet)))
        (loop
         (let ((count (read-byte stream)))
           (when (zerop count)
             (return))
           (read-sequence block stream :end count)
           (loop for i below count
                 for octet across block
                 do (write-char (ascii-char octet) output))))))))

(defun read-extension-object (stream data-stream)
  (let ((label (read-byte stream)))
    (case label
      (#.+plain-text-label+
       (skip-data-blocks stream))
      (#.+graphic-control-label+
       (setf *effective-graphic-control*
             (read-graphic-control-extension stream)))
      (#.+application-label+
       (read-application-extension stream data-stream))
      (#.+comment-label+
       (when (comment data-stream)
         (skippy-warn "Multiple comments found; only the final comment ~
                       will be loaded"))
       (setf (comment data-stream) (read-comment-extension stream)))
      (t
       (skippy-warn "Skipping unrecognized extension with label #x~2,'0X" label)
       (skip-data-blocks stream)))))

(defun process-objects (data-stream stream)
  (let ((context (make-instance 'decompression-context)))
    (loop
     (let ((tag (read-byte stream nil)))
       (case tag
         ((nil)
          (return))
         (#.+gif-trailer-code+
          (return))
         (#.+image-separator-code+
          (add-image (read-image context stream) data-stream))
         (#.+extension-introducer+
          (read-extension-object stream data-stream))
         (t
          (skippy-warn "Unknown tag ~D in ~A~:[~; at position ~:*~D~]"
                       tag stream (stream-position stream :offset -1))))))))

(defvar *gif87a-signature*
  ;; The ASCII for string "GIF87a"
  (make-array 6 :element-type 'octet
              :initial-contents #(71 73 70 56 55 97)))

(defvar *gif89a-signature*
  ;; The ASCII for string "GIF89a"
  (make-array 6 :element-type 'octet
              :initial-contents #(71 73 70 56 57 97)))

(defun check-gif-signature (stream)
  "Check that STREAM starts with the ASCII string \"GIF89a\" or \"GIF87a\"."
  (let* ((pos (stream-position stream))
         (signature (make-array 6 :element-type 'octet))
         (count (read-sequence signature stream)))
    (when (/= count 6)
      (error 'short-signature
             :source stream
             :position pos))
    (when (and (mismatch signature *gif89a-signature*)
               (mismatch signature *gif87a-signature*))
      (error 'signature-mismatch
             :source stream
             :position pos))))

(defun read-data-stream (stream)
  (check-gif-signature stream)
  (let ((width (read-uint16 stream))
        (height (read-uint16 stream))
        (flags (read-byte stream))
        (background-color-index (read-byte stream))
        (pixel-aspect-ratio (read-byte stream))
        (color-table nil)
        (*effective-graphic-control* nil))
    (declare (ignore background-color-index pixel-aspect-ratio))
    (bind-bits (flags 8)
        ((global-color-table-flag 1)
         (color-resolution        3)
         (sorted-flag             1)
         (global-color-table-size 3))
      (declare (ignore color-resolution sorted-flag))
      (when (plusp global-color-table-flag)
        (let ((color-table-entry-count (expt 2 (1+ global-color-table-size))))
          (setf color-table (read-color-table color-table-entry-count
                                              stream))))
      (let ((data-stream (make-data-stream :height height
                                           :width width
                                           :color-table color-table)))
        (process-objects data-stream stream)
        data-stream))))
            
(defun load-data-stream (file)
  (with-open-file (stream file :direction :input :element-type 'octet)
    (read-data-stream stream)))
