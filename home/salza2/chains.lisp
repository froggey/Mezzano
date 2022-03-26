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

(defun hash-value (input position)
  (+ (* #.+rmax+ (aref input position))
     (* #.+radix+ (aref input (logand #.+input-mask+ (+ position 1))))
     (aref input (logand #.+input-mask+ (+ position 2)))))

(declaim (inline mod8191))
(defun mod8191 (z)
  (declare (type (integer 0 3057705) z))
  (let ((zz (+ (ash z -13) (logand #x1FFF z))))
    (if (< zz #x1FFF)
        zz
        (- zz #x1FFF))))

(defun update-chains (input hashes chains start count)
  (declare (type input-buffer input)
           (type hashes-buffer hashes)
           (type chains-buffer chains)
           (type input-index start)
           (type (integer 0 32768) count)
           (optimize speed))
  (when (< count 3)
    (return-from update-chains))
  (let* ((hash (hash-value input start))
         (p0 start)
         (p1 (logand (+ start 2) #xFFFF)))
    (declare (type (integer 0 3057705) hash))
    (loop
     (let ((hash-index (mod8191 hash)))
       ;; Stuff the old hash index into chains at p0
       (setf (aref chains p0) (aref hashes hash-index))
       ;; Stuff p0 into the hashes
       (setf (aref hashes hash-index) p0)
       ;; Tentatively advance; if we hit the end, don't do the rest of
       ;; the hash update
       (setf p1 (logand (1+ p1) #xFFFF))
       (decf count)
       (when (= count 2)
         (return))
       ;; We're not at the end, so lop off the high, shift left, and
       ;; add the low to form a new hash value
       (setf hash (- hash (* (aref input p0) 11881)))
       (setf hash (* hash 109))
       (setf p0 (logand (1+ p0) #xFFFF))
       (setf hash (+ hash (aref input p1)))))))
