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
;;;; $Id: bitstream.lisp,v 1.6 2007/01/03 22:02:37 xach Exp $

(in-package #:skippy)

(declaim (inline bitstream-buffer))
(declaim (inline bitstream-offset))
(declaim (inline bitstream-octet))
(declaim (inline bitstream-count))
(declaim (inline bitstream-bits-left))
(declaim (inline bitstream-stream))

(defstruct (bitstream
             (:constructor
              %make-bitstream (buffer offset count octet bits-left stream)))
  (buffer (make-array 255 :element-type 'octet)
          :type bitstream-buffer)
  (offset 0 :type octet)
  (count 0 :type octet)
  (octet 0 :type octet)
  (bits-left 8 :type (mod 9))
  stream)

(defun make-bitstream (stream)
  (%make-bitstream (make-array 255 :element-type 'octet)
                   0
                   0
                   0
                   8
                   stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bitstream-slot-attributes*
    '((buffer
       :reader bitstream-buffer
       :type bitstream-buffer
       :save nil)
      (offset
       :reader bitstream-offset
       :type octet)
      (count
       :reader bitstream-count
       :type octet)
      (octet
       :reader bitstream-octet
       :type octet)
      (bits-left
       :reader bitstream-bits-left
       :type (mod 9))
      (stream
       :reader bitstream-stream
       :type cl:stream
       :save nil))))

(defmacro with-bitstream-slots (name-bindings bitstream &body body)
  (labels ((binding-var (binding)
             (if (consp binding) (first binding) binding))
           (binding-slot (binding)
             (if (consp binding) (second binding) binding)))
    (let ((type-declarations '())
          (binding-forms '())
          (save-forms '())
          (bitstream-var (gensym)))
      (dolist (binding name-bindings)
        (let* ((var (binding-var binding))
               (slot (binding-slot binding))
               (attributes (cdr (assoc slot *bitstream-slot-attributes*))))
          (unless attributes
            (error "Unknown bitstream slot -- ~S" slot))
          (destructuring-bind (&key reader type (save t))
              attributes
            (push `(,var (,reader ,bitstream-var)) binding-forms)
            (push `(type ,type ,var) type-declarations)
            (when save
              (push `(setf (,reader ,bitstream-var) ,var) save-forms)))))
      `(let ((,bitstream-var ,bitstream))
        (let ,binding-forms
          (declare ,@type-declarations)
          ,@body
          ,@save-forms)))))

(defun reset-stream (bitstream)
  (declare (optimize speed)
           (type bitstream bitstream))
  (with-bitstream-slots (stream buffer offset octet bits-left)
      bitstream
    (when (plusp bits-left)
      (setf (aref buffer offset) octet
            offset (1+ offset)))
    (write-byte offset stream)
    (write-sequence buffer stream :end offset)
    (fill buffer 0)))

(defun write-bits (code length bitstream)
  (declare (type (mod 13) length)
           (type fixnum code)
           (type bitstream bitstream)
           (optimize speed))
  (with-bitstream-slots (stream buffer offset octet bits-left)
      bitstream
    (flet ((merge-bits (len)
             (declare (type (mod 13) len))
             (setf octet (logand #xFF
                                 (logior (ash (ldb (byte len 0) code)
                                              (- 8 bits-left))
                                         octet))
                   bits-left (- bits-left len)
                   code (ash code (- len))
                   length (- length len))))
      (declare (inline merge-bits))
      (loop
       (when (< length bits-left)
         (return))
       (merge-bits bits-left)
       (setf bits-left 8
             (aref buffer offset) octet
             offset (1+ offset)
             octet 0)
       (when (= offset 255)
         (write-byte 255 stream)
         (write-sequence buffer stream)
         (fill buffer 0)
         (setf offset 0)))
      (when (plusp length)
        (merge-bits length)))))

(defun make-input-bitstream (stream)
  (let ((count (read-byte stream))
        (offset 0)
        (buffer (make-array 255 :element-type 'octet))
        (bits-left 0))
    (read-sequence buffer stream :end count)
    (%make-bitstream buffer offset count 0 bits-left stream)))

;;;
;;; When entering and leaving read-bits, OFFSET is always <255 and points
;;; at the NEXT input offset. It is 0 at the start of the process.
;;;
;;; BITS-LEFT may be zero when entering.
;;;

(defun read-bits (length bitstream)
  (declare (type (mod 13) length)
           (type bitstream bitstream)
           (optimize speed))
  (let ((result 0)
        (result-offset 0))
    (declare (type (unsigned-byte 12) result)
             (type (mod 13) result-offset))
    (with-bitstream-slots (stream offset count octet buffer bits-left)
        bitstream
      (loop
       (cond ((< length bits-left)
              (setf result (logior result
                                   (ash (ldb (byte length 0) octet)
                                        result-offset))
                    octet (ash octet (- length))
                    bits-left (- bits-left length))
              (return))
             (t
              (when (= offset count)
                (setf count (read-byte stream)
                      offset 0)
                (read-sequence buffer stream :end count))
                (setf result (logior result (ash octet result-offset))
                      result-offset (+ bits-left result-offset)
                      length (- length bits-left)
                      octet (aref buffer offset)
                      offset (+ offset 1)
                      bits-left 8)))))
    result))

(defun finish-input (bitstream)
  (when (plusp (bitstream-count bitstream))
    (let ((final-block (read-byte (bitstream-stream bitstream))))
      (unless (zerop final-block)
        (skippy-warn "Unexpected final block value in stream ~
                      (expected ~D, got ~D)"
                     0 final-block)))))

