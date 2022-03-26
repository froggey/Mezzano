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
;;;; $Id: lzw.lisp,v 1.11 2007/01/03 22:01:10 xach Exp $

(in-package #:skippy)

(defclass compression-context ()
  ((table
    :initform (make-hash-table)
    :reader table))
  (:documentation
   "Store data structures that may be re-used when writing out
multiple images in a GIF animation."))

;;;
;;; The basic LZW compression algorithm is:
;;;
;;; prefix <- first character
;;; while pending data:
;;;     char <- next character
;;;     if prefix . char in table:
;;;         prefix <- prefix . char
;;;     else:
;;;         output code for prefix
;;;         add prefix . char to table
;;;         prefix <- char
;;; output code for prefix
;;;

(defun lzw-compress (vector code-size context stream)
  (declare (type (simple-array octet (*)) vector)
           (type (mod 13) code-size))
  (let ((iv 0)
        (data-stream (make-bitstream stream)))
    (declare (fixnum iv))
    (flet ((next-input ()
             (when (< iv (length vector))
               (prog1
                   (aref vector iv)
                 (incf iv)))))
      (let* ((string-table (table context))
             (clear-code (expt 2 code-size))
             (end-of-input-code (1+ clear-code))
             (index (+ 2 clear-code))
             (compression-size (1+ code-size))
             (max-index (1- (expt 2 compression-size)))
             (prefix (next-input))
             (next-char nil))
        (clrhash string-table)
        (flet ((output-code (code)
                 (write-bits code compression-size data-stream)))
          (output-code clear-code)
          (loop
           (setf next-char (next-input))
           (when (null next-char)
             (output-code prefix)
             (output-code end-of-input-code)
             (reset-stream data-stream)
             (return))
           (let* ((key (logior (ash prefix 8) next-char))
                  (entry (gethash key string-table)))
             (cond (entry
                    (setf prefix entry))
                   (t
                    (output-code prefix)
                    (setf (gethash key string-table) index)
                    (when (> index max-index)
                      (setf max-index (1- (expt 2 (incf compression-size)))))
                    (incf index)
                    (setf prefix next-char))))
           (when (= index #xFFF)
             ;; The index isn't allowed to be this big, so the string
             ;; table must be cleared out and restarted
             (output-code clear-code)
             (setf compression-size (1+ code-size))
             (setf max-index (1- (expt 2 compression-size)))
             (clrhash string-table)
             (setf index (+ 2 clear-code)))))))))


(deftype string-table-vector ()
  '(simple-array (signed-byte 16) (4096)))

(deftype string-table-entry ()
  '(signed-byte 16))

(defclass decompression-context ()
  ((entries
    :initform (make-array 4096
                          :element-type 'string-table-entry
                          :initial-element -1)
    :reader entries)
   (preds
    :initform (make-array 4096
                          :element-type 'string-table-entry
                          :initial-element -1)
    :reader preds))
  (:documentation
   "A decompression context is used to hold data structures that may
be re-used for repeated calls to lzw-decompress, so they don't have to
be allocated fresh each time."))

(defun lzw-decompress (vector code-size context stream)
  "Decompress the GIF LZW data from STREAM into VECTOR."
  (declare (type (simple-array octet (*)) vector)
           (type (mod 9) code-size)
           (type stream stream)
           (optimize speed))
  (let* ((entries (entries context))
         (preds (preds context))
         (clear-code (expt 2 code-size))
         (end-of-input (+ clear-code 1))
         (next-entry-index (+ clear-code 2))
         (compression-size (1+ code-size))
         (compression-threshold (* clear-code 2))
         (last-code -1)
         (pos 0)
         (bitstream (make-input-bitstream stream)))
    (declare (type string-table-vector entries preds)
             (type fixnum clear-code end-of-input next-entry-index
                   compression-size compression-threshold
                   last-code pos)
             (type bitstream bitstream))
    (fill entries -1)
    (fill preds -1)
    (dotimes (i clear-code)
      (setf (aref entries i) i))
    (labels ((reset-table ()
               (fill preds -1)
               (fill entries -1 :start clear-code)
               (setf last-code -1
                     next-entry-index (+ clear-code 2)
                     compression-size (1+ code-size)
                     compression-threshold (* clear-code 2)))
             (root-value (code)
               (loop
                (let ((pred (aref preds code)))
                  (when (minusp pred)
                    (return (aref entries code)))
                  (setf code pred))))
             (increase-compression-size ()
               (setf compression-size (min 12 (+ compression-size 1))
                     compression-threshold (* compression-threshold 2)))
             (add-entry (entry pred)
               (when (>= pred next-entry-index)
                 (error 'lzw-error
                        :description "Corrupt data in LZW stream"))
               (let ((result
                      (setf (aref preds next-entry-index) pred
                            (aref entries next-entry-index) entry
                            next-entry-index (1+ next-entry-index))))
                 (when (>= result compression-threshold)
                   (increase-compression-size))
                 (1- result)))
             (code-depth (code)
               (let ((depth 0))
                 (declare (fixnum depth))
                 (loop
                  (let ((pred (aref preds code)))
                    (when (minusp pred)
                      (return depth))
                    (setf depth (1+ depth)
                          code pred)))))
             (output-code-string (code)
               (let ((i (+ pos (code-depth code)))
                     (j pos))
                 (setf pos (1+ i))
                 (when (>= i (length vector))
                   (skippy-warn "Too much input data for image, ~
                                 ignoring extra")
                   (finish-input bitstream)
                   (return-from lzw-decompress))
                 (loop
                  (setf (aref vector i) (aref entries code)
                        code (aref preds code)
                        i (- i 1))
                  (when (< i j)
                    (return))))))
      (loop
       (let ((code (read-bits compression-size bitstream)))
         (declare (type fixnum code))
         (cond ((= code clear-code)
                (reset-table))
               ((= code end-of-input)
                (finish-input bitstream)
                (return-from lzw-decompress))
               ((= last-code -1)
                (output-code-string code)
                (setf last-code code))
               (t
                (let ((entry (aref entries code)))
                  (if (minusp entry)
                      (let ((root (root-value last-code)))
                        (output-code-string (add-entry root last-code))
                        (setf last-code code))
                      (let ((root (root-value code)))
                        (add-entry root last-code)
                        (setf last-code code)
                        (output-code-string code)))))))))))
