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

(defun make-stream-output-callback (stream)
  "Return a function suitable for use as a compressor callback that
writes all compressed data to STREAM."
  (lambda (buffer end)
    (write-sequence buffer stream :end end)))

(defun gzip-stream (input output)
  (let ((callback (make-stream-output-callback output))
        (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (with-compressor (compressor 'gzip-compressor
                                 :callback callback)
      (loop
       (let ((end (read-sequence buffer input)))
         (when (zerop end)
           (return))
         (compress-octet-vector buffer compressor :end end))))))

(defun gzip-file (input output &key (if-exists :supersede))
  (with-open-file (istream input :element-type '(unsigned-byte 8))
    (with-open-file (ostream output
                             :element-type '(unsigned-byte 8)
                             :direction :output
                             :if-exists if-exists)
      (gzip-stream istream ostream)))
  (probe-file output))


(defun compressor-designator-compressor (designator initargs)
  (etypecase designator
    (symbol (apply #'make-instance designator initargs))
    (deflate-compressor designator)))

(defun compress-data (data compressor-designator &rest initargs)
  (let ((chunks '())
        (size 0)
        (compressor (compressor-designator-compressor compressor-designator
                                                      initargs)))
    (setf (callback compressor)
          (lambda (buffer end)
            (incf size end)
            (push (subseq buffer 0 end)
                  chunks)))
    (compress-octet-vector data compressor)
    (finish-compression compressor)
    (let ((compressed (make-array size :element-type '(unsigned-byte 8)))
          (start 0))
      (dolist (chunk (nreverse chunks))
        (replace compressed chunk :start1 start)
        (incf start (length chunk)))
      compressed)))
