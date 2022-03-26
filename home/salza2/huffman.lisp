;;;
;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
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

(in-package #:salza2)

(deftype code-vector ()
  '(simple-array (unsigned-byte 32) (*)))

(deftype size-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(defclass huffman-codes ()
  ((codes
    :initarg :codes
    :accessor codes)
   (sizes
    :initarg :sizes
    :accessor sizes)))
                        
(defun code-vector (length)
  (make-array length :element-type '(unsigned-byte 32)))

(defun size-vector (length)
  (make-array length :element-type '(unsigned-byte 8)))

;;;
;;; Generate the fixed code/size vectors
;;;

(defun reverse-bits (word n)
  (let ((j 0))
    (dotimes (i n j)
      (setf j (logior (ash j 1) (logand #x1 word)))
      (setf word (ash word -1)))))

(defun fixed-huffman-codes ()
  "Generate the fixed Huffman codes specified by RFC1951."
  (let ((codes (code-vector 288))
        (sizes (size-vector 288))
        (i 0))
    (flet ((fill-range (length start end)
             (loop for j from start to end do
                   (setf (aref codes i) (reverse-bits j length)
                         (aref sizes i) length)
                   (incf i))))
      (fill-range 8 #b00110000  #b10111111)
      (fill-range 9 #b110010000 #b111111111)
      (fill-range 7 #b0000000   #b0010111)
      (fill-range 8 #b11000000  #b11000111)
      (make-instance 'huffman-codes :codes codes :sizes sizes))))

(defun length-codes (huffman-codes)
  "Compute a table of the (Huffman + extra bits) values for all
possible lengths for the given HUFFMAN-TABLE."
  (let ((codes (code-vector 259))
        (sizes (size-vector 259))
        (code 257)
        (length 3)
        (extra-bit-counts '(0 0 0 0 0 0 0 0
                            1 1 1 1
                            2 2 2 2
                            3 3 3 3
                            4 4 4 4
                            5 5 5 5
                            0)))
    (labels ((save-pair (i code size)
               (setf (aref codes i) code
                     (aref sizes i) size))
             (save-value (extra-bit-count extra-value)
               (let ((huffman-value (aref (codes huffman-codes) code))
                     (huffman-count (aref (sizes huffman-codes) code)))
                 (save-pair length 
                            (logior huffman-value
                                    (ash extra-value huffman-count))
                            (+ huffman-count extra-bit-count)))))
      (dolist (count extra-bit-counts)
        (dotimes (i (expt 2 count))
          (when (< length 258)
            (save-value count i)
            (incf length)))
        (incf code))
      (setf code 285)
      (save-value 0 0))
    (make-instance 'huffman-codes :codes codes :sizes sizes)))

(defun distance-codes ()
  "Compute a table of the (code + extra bits) values for all possible
distances as specified by RFC1951."
  (let ((codes (code-vector 32769))
        (sizes (size-vector 32769))
        (code 0)
        (distance 1)
        (extra-bit-counts '(0 0 0 0
                            1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9
                            10 10 11 11 12 12 13 13)))
    (flet ((save-value (extra-bit-count extra-value)
             (setf (aref codes distance)
                   (logior (ash extra-value 5) (reverse-bits code 5))
                   (aref sizes distance)
                   (+ 5 extra-bit-count))))
      (dolist (count extra-bit-counts)
        (dotimes (i (expt 2 count))
          (save-value count i)
          (incf distance))
        (incf code)))
    (make-instance 'huffman-codes :codes codes :sizes sizes)))

(defvar *fixed-huffman-codes* (fixed-huffman-codes))
(defvar *length-codes* (length-codes *fixed-huffman-codes*))
(defvar *distance-codes* (distance-codes))
