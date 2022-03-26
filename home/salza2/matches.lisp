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

(defconstant +maximum-match-length+ 258
  "The maximum match length allowed.")

(defconstant +maximum-match-distance+ 32768
  "The maximum distance for a match.")

(declaim (inline match-length))
(defun match-length (p1 p2 input end)
  "Returns the length of the match between positions p1 and p2 in
INPUT; END is a sentinel position that ends the match length
check if reached."
  (declare (type input-index p1 p2 end)
           (type input-buffer input)
           (optimize speed))
  (let ((length 0))
    (loop
     (when (or (/= (aref input p1) (aref input p2))
               (= length +maximum-match-length+)
               (= p1 end))
       (return length))
     (setf p1 (logand (1+ p1) #xFFFF)
           p2 (logand (1+ p2) #xFFFF)
           length (logand #xFFF (1+ length))))))

(defun longest-match (p1 input chains end max-tests)
  (declare (type input-index p1 end)
           (type input-buffer input)
           (type chains-buffer chains)
           (type (integer 0 32) max-tests)
           (optimize speed))
  (let ((match-length 0)
        (p2 (aref chains p1))
        (test-count 0)
        (distance 0))
    (declare (type (integer 0 258) match-length)
             (type (integer 0 32) test-count))
    (loop
     (when (or (= match-length +maximum-match-length+)
               (= test-count max-tests)
               (= p2 p1)
               (= p2 (aref chains p2)))
       (return (values match-length distance)))
     (let ((step (logand (- p1 p2) #xFFFF)))
       (when (< +maximum-match-distance+ step)
         (return (values match-length distance)))
       (let ((possible-length (match-length p1 p2 input end)))
         (when (and (< 2 possible-length)
                    (< match-length possible-length))
           (setf distance step
                 match-length possible-length))
         (setf p2 (aref chains p2)))
       (incf test-count)))))

