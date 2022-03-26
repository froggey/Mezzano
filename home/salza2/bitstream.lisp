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

(defun bitstream-callback-missing (&rest args)
  (declare (ignore args))
  (error "No callback set in bitstream"))

(defun merge-bits (code size buffer bits callback)
  (declare (type (unsigned-byte 32) code)
           (type (integer 0 32) size)
           (type bitstream-buffer-bit-count bits)
           (type bitstream-buffer buffer)
           (type function callback)
           (optimize speed))
  ;; BITS represents how many bits have been added to BUFFER so far,
  ;; so the FLOOR of it by 8 will give both the buffer byte index and
  ;; the bit index within that byte to where new bits should be
  ;; merged
  (let ((buffer-index (ash bits -3))
        (bit (logand #b111 bits)))
    ;; The first byte to which new bits are merged might have some
    ;; bits in it already, so pull it out for merging back in the
    ;; loop. This only has to be done for the first byte, since
    ;; subsequent bytes in the buffer will consist solely of bits from
    ;; CODE.
    ;;
    ;; The check (PLUSP BIT) is done to make sure that no garbage bits
    ;; from a previous write are re-used; if (PLUSP BIT) is zero, all
    ;; bits in the first output byte come from CODE.
    (let ((merge-byte (if (plusp bit) (aref buffer buffer-index) 0))
          (end #.+bitstream-buffer-size+)
          (result (+ bits size)))
      ;; (ceiling (+ bit size) 8) is the total number of bytes touched
      ;; in the buffer
      (dotimes (i (ceiling (+ bit size) 8))
        (let ((shift (+ bit (* i -8)))
              (j (+ buffer-index i)))
          ;; Buffer filled up in the middle of CODE
          (when (= j end)
            (funcall callback buffer j))
          ;; Merge part of CODE into the buffer
          (setf (aref buffer (logand #.+bitstream-buffer-mask+ j))
                (logior (logand #xFF (ash code shift)) merge-byte))
          (setf merge-byte 0)))
      ;; Writing is done, and the buffer is full, so call the callback
      (when (= result #.+bitstream-buffer-bits+)
        (funcall callback buffer #.+bitstream-buffer-size+))
      ;; Return only the low bits of the sum
      (logand #.+bitstream-buffer-bitmask+ result))))

(defun merge-octet (octet buffer bits callback)
  (declare (type octet octet)
           (type bitstream-buffer buffer)
           (type bitstream-buffer-bit-count bits)
           (type function callback)
           (optimize speed))
  (let ((offset (ceiling bits 8)))
    ;; End of the buffer beforehand
    (when (= offset #.+bitstream-buffer-size+)
      (funcall callback buffer #.+bitstream-buffer-size+)
      (setf offset 0
            bits 0))
    (setf (aref buffer offset) octet
          bits (+ bits 8))
    (when (= (1+ offset) #.+bitstream-buffer-size+)
      (funcall callback buffer #.+bitstream-buffer-size+)
      (setf bits 0))
    bits))

;;; Protocol

(defclass bitstream ()
  ((buffer
    :initarg :buffer
    :accessor buffer
    :documentation "Holds accumulated bits packed into octets.")
   (bits
    :initarg :bits
    :accessor bits
    :documentation "The number of bits written to the buffer so far.")
   (callback
    :initarg :callback
    :accessor callback
    :documentation "A function of two arguments, BUFFER and END,
    that should write out all the data in BUFFER up to END."))
   (:default-initargs
    :buffer (make-array +bitstream-buffer-size+ :element-type 'octet)
    :bits 0
    :callback #'bitstream-callback-missing))

(defgeneric write-bits (code size bitstream))
(defgeneric write-octet (octet bitstream))
(defgeneric write-octet-vector (vector bitstream &key start end))
(defgeneric flush (bitstream))

(defmethod write-bits (code size (bitstream bitstream))
  (setf (bits bitstream)
        (merge-bits code size
                    (buffer bitstream)
                    (bits bitstream)
                    (callback bitstream))))

(defmethod write-octet (octet (bitstream bitstream))
  (setf (bits bitstream)
        (merge-octet octet
                     (buffer bitstream)
                     (bits bitstream)
                     (callback bitstream))))

(defmethod write-octet-vector (vector (bitstream bitstream) &key (start 0) end)
  ;;; Not efficient in the slightest, but not actually used internally.
  (let ((end (or end (length vector))))
    (loop for i from start below end
          do (write-octet (aref vector i) bitstream))))

(defmethod flush ((bitstream bitstream))
  (let ((end (ceiling (bits bitstream) 8)))
    (funcall (callback bitstream) (buffer bitstream) end)
    (setf (bits bitstream) 0)))

(defmethod reset ((bitstream bitstream))
  (fill (buffer bitstream) 0)
  (setf (bits bitstream) 0))
