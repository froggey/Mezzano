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

(defun make-input ()
  (make-array 65536 :element-type 'octet))

(defun make-chains ()
  (make-array 65536
              :element-type '(unsigned-byte 16)
              :initial-element 0))

(defun make-hashes ()
  (make-array +hashes-size+
              :element-type '(unsigned-byte 16)
              :initial-element 0))

(defun error-missing-callback (&rest args)
  (declare (ignore args))
  (error "No callback given for compression"))

;;; FIXME: MERGE-INPUT is pretty ugly. It's the product of incremental
;;; evolution and experimentation. It should be cleaned up.
;;;
;;; Its basic purpose is to use octets from INPUT to fill up 32k-octet
;;; halves of the 64k-octet OUTPUT buffer. Whenever a half fills up,
;;; the COMPRESS-FUN is invoked to compress that half. At the end, a
;;; partial half may remain uncompressed to be either filled by a
;;; future call to MERGE-INPUT or to get flushed out by a call to
;;; FINAL-COMPRESS.

(defun merge-input (input start count output offset compress-fun)
  "Merge COUNT octets from START of INPUT into OUTPUT at OFFSET;
on reaching 32k boundaries within OUTPUT, call the COMPRESS-FUN
with OUTPUT, a starting offset, and the count of pending data."
  (declare (type octet-vector input output))
  (let ((i start)
        (j (+ start (min count (- +input-limit+ (mod offset +input-limit+)))))
        (result (logand +buffer-size-mask+ (+ offset count))))
    (dotimes (k (ceiling (+ (logand offset +input-limit-mask+) count)
                         +input-limit+))
      (when (plusp k)
        (funcall compress-fun
                 output
                 (logxor offset #x8000)
                 +input-limit+))
      (replace output input :start1 offset :start2 i :end2 j)
      (setf offset (logand +input-limit+ (+ offset +input-limit+)))
      (setf i j
            j (min (+ start count) (+ j +input-limit+))))
    (when (zerop (logand result +input-limit-mask+))
      (funcall compress-fun output (logxor offset #x8000) +input-limit+))
    result))

(defun reinitialize-bitstream-funs (compressor bitstream)
  (setf (literal-fun compressor)
        (make-huffman-writer *fixed-huffman-codes* bitstream)
        (length-fun compressor)
        (make-huffman-writer *length-codes* bitstream)
        (distance-fun compressor)
        (make-huffman-writer *distance-codes* bitstream)
        (compress-fun compressor)
        (make-compress-fun compressor)))


;;; Class & protocol

(defclass deflate-compressor ()
  ((input
    :initarg :input
    :accessor input)
   (chains
    :initarg :chains
    :accessor chains)
   (hashes
    :initarg :hashes
    :accessor hashes)
   (start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   (counter
    :initarg :counter
    :accessor counter)
   (octet-buffer
    :initarg :octet-buffer
    :accessor octet-buffer)
   (bitstream
    :initarg :bitstream
    :accessor bitstream)
   (literal-fun
    :initarg :literal-fun
    :accessor literal-fun)
   (length-fun
    :initarg :length-fun
    :accessor length-fun)
   (distance-fun
    :initarg :distance-fun
    :accessor distance-fun)
   (byte-fun
    :initarg :byte-fun
    :accessor byte-fun)
   (compress-fun
    :initarg :compress-fun
    :accessor compress-fun))
  (:default-initargs
   :input (make-input)
   :chains (make-chains)
   :hashes (make-hashes)
   :start 0
   :end 0
   :counter 0
   :bitstream (make-instance 'bitstream)
   :octet-buffer (make-octet-vector 1)))

;;; Public protocol GFs

(defgeneric start-data-format (compressor)
  (:documentation "Add any needed prologue data to the output bitstream."))

(defgeneric compress-octet (octet compressor)
  (:documentation "Add OCTET to the compressed data of COMPRESSOR."))

(defgeneric compress-octet-vector (vector compressor &key start end)
  (:documentation "Add the octets of VECTOR to the compressed
  data of COMPRESSOR."))

(defgeneric process-input (compressor input start count)
  (:documentation "Map over pending octets in INPUT and perform
  any needed processing. Called before the data is compressed. A
  subclass might use this to compute a checksum of all input
  data."))

(defgeneric finish-data-format (compressor)
  (:documentation "Add any needed epilogue data to the output bitstream."))

(defgeneric finish-compression (compressor)
  (:documentation "Finish the data format and flush all pending
  data in the bitstream."))

;;; Internal GFs

(defgeneric final-compress (compressor)
  (:documentation "Perform the final compression on pending input
  data in COMPRESSOR."))

(defgeneric make-compress-fun (compressor)
  (:documentation "Create a callback suitable for passing to
  MERGE-INPUT for performing incremental compression of the next
  32k octets of input."))

;;; Methods

(defmethod initialize-instance :after ((compressor deflate-compressor)
                                       &rest initargs
                                       &key
                                       literal-fun length-fun distance-fun
                                       compress-fun
                                       callback)
  (declare (ignore initargs))
  (let ((bitstream (bitstream compressor)))
    (setf (callback bitstream)
          (or callback #'error-missing-callback))
    (setf (literal-fun compressor)
          (or literal-fun (make-huffman-writer *fixed-huffman-codes*
                                               bitstream)))
    (setf (length-fun compressor)
          (or length-fun (make-huffman-writer *length-codes*
                                              bitstream)))
    (setf (distance-fun compressor)
          (or distance-fun (make-huffman-writer *distance-codes*
                                                bitstream)))
    (setf (compress-fun compressor)
          (or compress-fun (make-compress-fun compressor)))
    (start-data-format compressor)))

;;; A few methods defer to the bitstream

(defmethod (setf callback) (new-fun (compressor deflate-compressor))
  (let ((bitstream (bitstream compressor)))
    (prog1
        (setf (callback bitstream) new-fun)
      (reinitialize-bitstream-funs compressor bitstream))))

(defmethod write-bits (code size (compressor deflate-compressor))
  (write-bits code size (bitstream compressor)))

(defmethod write-octet (octet (compressor deflate-compressor))
  (write-octet octet (bitstream compressor)))

(defmethod write-octet-vector (vector (compressor deflate-compressor)
                               &key (start 0) end)
  (write-octet-vector vector (bitstream compressor)
                      :start start
                      :end end))
                               

(defmethod start-data-format ((compressor deflate-compressor))
  (let ((bitstream (bitstream compressor)))
    (write-bits +final-block+ 1 bitstream)
    (write-bits +fixed-tables+ 2 bitstream)))

(defmethod compress-octet (octet compressor)
  (let ((vector (octet-buffer compressor)))
    (setf (aref vector 0) octet)
    (compress-octet-vector vector compressor)))

(defmethod compress-octet-vector (vector compressor &key (start 0) end)
  (let* ((closure (compress-fun compressor))
         (end (or end (length vector)))
         (count (- end start)))
    (let ((end
           (merge-input vector start count
                        (input compressor)
                        (end compressor)
                        closure)))
      (setf (end compressor) end
            (start compressor) (logand #x8000 end)
            (counter compressor) (logand #x7FFF end)))))

(defmethod process-input ((compressor deflate-compressor) input start count)
  (update-chains input (hashes compressor) (chains compressor) start count))

(defmethod finish-data-format ((compressor deflate-compressor))
  (funcall (literal-fun compressor) 256))

(defmethod finish-compression ((compressor deflate-compressor))
  (final-compress compressor)
  (finish-data-format compressor)
  (flush (bitstream compressor)))

(defmethod final-compress ((compressor deflate-compressor))
  (let ((input (input compressor))
        (chains (chains compressor))
        (start (start compressor))
        (end (end compressor))
        (counter (counter compressor))
        (literal-fun (literal-fun compressor))
        (length-fun (length-fun compressor))
        (distance-fun (distance-fun compressor)))
    (process-input compressor input start counter)
    (compress input chains start end
              literal-fun
              length-fun
              distance-fun)))

(defmethod make-compress-fun ((compressor deflate-compressor))
  (let ((literal-fun (literal-fun compressor))
        (length-fun (length-fun compressor))
        (distance-fun (distance-fun compressor)))
    (lambda (input start count)
      (process-input compressor input start count)
      (let ((end (+ start count)))
        (compress input (chains compressor) start (logand #xFFFF end)
                  literal-fun
                  length-fun
                  distance-fun)))))

(defmethod reset ((compressor deflate-compressor))
  (fill (chains compressor) 0)
  (fill (input compressor) 0)
  (fill (hashes compressor) 0)
  (setf (start compressor) 0
        (end compressor) 0
        (counter compressor) 0)
  (reset (bitstream compressor))
  (start-data-format compressor))


(defmacro with-compressor ((var class
                                &rest initargs
                                &key &allow-other-keys)
                           &body body)
  `(let ((,var (make-instance ,class ,@initargs)))
     (multiple-value-prog1 
         (progn ,@body)
       (finish-compression ,var))))
