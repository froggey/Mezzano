;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/length.lisp,v 1.6 2008/05/29 10:25:14 edi Exp $

;;; Copyright (c) 2005-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

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

(in-package :flexi-streams)

(defgeneric encoding-factor (format)
  (:documentation "Given an external format FORMAT, returns a factor
which denotes the octets to characters ratio to expect when
encoding/decoding.  If the returned value is an integer, the factor is
assumed to be exact.  If it is a \(double) float, the factor is
supposed to be based on heuristics and usually not exact.

This factor is used in string.lisp.")
  (declare #.*standard-optimize-settings*))

(defmethod encoding-factor ((format flexi-8-bit-format))
  (declare #.*standard-optimize-settings*)
  ;; 8-bit encodings map octets to characters in an exact one-to-one
  ;; fashion
  1)

(defmethod encoding-factor ((format flexi-utf-8-format))
  (declare #.*standard-optimize-settings*)
  ;; UTF-8 characters can be anything from one to six octets, but we
  ;; assume that the "overhead" is only about 5 percent - this
  ;; estimate is obviously very much dependant on the content
  1.05d0)

(defmethod encoding-factor ((format flexi-utf-16-format))
  (declare #.*standard-optimize-settings*)
  ;; usually one character maps to two octets, but characters with
  ;; code points above #x10000 map to four octets - we assume that we
  ;; usually don't see these characters but of course have to return a
  ;; float
  2.0d0)

(defmethod encoding-factor ((format flexi-utf-32-format))
  (declare #.*standard-optimize-settings*)
  ;; UTF-32 always matches every character to four octets
  4)

(defmethod encoding-factor ((format flexi-crlf-mixin))
  (declare #.*standard-optimize-settings*)
  ;; if the sequence #\Return #\Linefeed is the line-end marker, this
  ;; obviously makes encodings potentially longer and definitely makes
  ;; the estimate unexact
  (* 1.02d0 (call-next-method)))

(defgeneric check-end (format start end i)
  (declare #.*fixnum-optimize-settings*)
  (:documentation "Helper function used below to determine if we tried
to read past the end of the sequence.")
  (:method (format start end i)
   (declare #.*fixnum-optimize-settings*)
   (declare (ignore start))
   (declare (fixnum end i))
   (when (> i end)
     (signal-encoding-error format "This sequence can't be decoded ~
using ~A as it is too short.  ~A octet~:P missing at the end."
                            (external-format-name format)
                            (- i end))))
  (:method ((format flexi-utf-16-format) start end i)
   (declare #.*fixnum-optimize-settings*)
   (declare (fixnum start end i))
   (declare (ignore i))
   ;; don't warn twice
   (when (evenp (- end start))
     (call-next-method))))

(defgeneric compute-number-of-chars (format sequence start end)
  (declare #.*standard-optimize-settings*)
  (:documentation "Computes the exact number of characters required to
decode the sequence of octets in SEQUENCE from START to END using the
external format FORMAT."))

(defmethod compute-number-of-chars :around (format (list list) start end)
  (declare #.*standard-optimize-settings*)
  (call-next-method format (coerce list 'vector) start end))

(defmethod compute-number-of-chars ((format flexi-8-bit-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end))
  (declare (ignore sequence))
  (- end start))

(defmethod compute-number-of-chars ((format flexi-crlf-mixin) sequence start end)
  ;; this method only applies to the 8-bit formats as all other
  ;; formats with CRLF line endings have their own specialized methods
  ;; below
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((i start)
        (length (- end start)))
    (declare (fixnum i length))
    (loop
     (when (>= i end)
       (return))
     (let ((position (search #.(vector +cr+ +lf+) sequence :start2 i :end2 end :test #'=)))
       (unless position
         (return))
       (setq i (1+ position))
       (decf length)))
    length))

(defmethod compute-number-of-chars ((format flexi-utf-8-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (loop
     (when (>= i end)
       (return))
     (let* ((octet (aref sequence i))
            ;; note that there are no validity checks here
            (length (cond ((not (logbitp 7 octet)) 1)
                          ((= #b11000000 (logand* octet #b11100000)) 2)
                          ((= #b11100000 (logand* octet #b11110000)) 3)
                          (t 4))))
       (declare (fixnum length) (type octet octet))
       (incf sum)
       (incf i length)))
    (check-end format start end i)
    sum))

(defmethod compute-number-of-chars ((format flexi-crlf-utf-8-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((sum 0)
        (i start)
        (last-octet 0))
    (declare (fixnum i sum) (type octet last-octet))
    (loop
     (when (>= i end)
       (return))
     (let* ((octet (aref sequence i))
            ;; note that there are no validity checks here
            (length (cond ((not (logbitp 7 octet)) 1)
                          ((= #b11000000 (logand* octet #b11100000)) 2)
                          ((= #b11100000 (logand* octet #b11110000)) 3)
                          (t 4))))
       (declare (fixnum length) (type octet octet))
       (unless (and (= octet +lf+) (= last-octet +cr+))
         (incf sum))
       (incf i length)
       (setq last-octet octet)))
    (check-end format start end i)
    sum))

(defmethod compute-number-of-chars :before ((format flexi-utf-16-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (declare (ignore sequence))
  (when (oddp (- end start))
    (signal-encoding-error format "~A octet~:P cannot be decoded ~
using UTF-16 as ~:*~A is not even."
                           (- end start))))
  
(defmethod compute-number-of-chars ((format flexi-utf-16-le-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (decf end 2)
    (loop
     (when (> i end)
       (return))
     (let* ((high-octet (aref sequence (1+ i)))
            (length (cond ((<= #xd8 high-octet #xdf) 4)
                          (t 2))))
       (declare (fixnum length) (type octet high-octet))
       (incf sum)
       (incf i length)))
    (check-end format start (+ end 2) i)
    sum))

(defmethod compute-number-of-chars ((format flexi-utf-16-be-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (decf end 2)
    (loop
     (when (> i end)
       (return))
     (let* ((high-octet (aref sequence i))
            (length (cond ((<= #xd8 high-octet #xdf) 4)
                          (t 2))))
       (declare (fixnum length) (type octet high-octet))
       (incf sum)
       (incf i length)))
    (check-end format start (+ end 2) i)
    sum))

(defmethod compute-number-of-chars ((format flexi-crlf-utf-16-le-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((sum 0)
        (i start)
        (last-octet 0))
    (declare (fixnum i sum) (type octet last-octet))
    (decf end 2)
    (loop
     (when (> i end)
       (return))
     (let* ((high-octet (aref sequence (1+ i)))
            (length (cond ((<= #xd8 high-octet #xdf) 4)
                          (t 2))))
       (declare (fixnum length) (type octet high-octet))
       (unless (and (zerop high-octet)
                    (= (the octet (aref sequence i)) +lf+)
                    (= last-octet +cr+))         
         (incf sum))
       (setq last-octet (if (zerop high-octet)
                          (aref sequence i)
                          0))
       (incf i length)))
    (check-end format start (+ end 2) i)
    sum))

(defmethod compute-number-of-chars ((format flexi-crlf-utf-16-be-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((sum 0)
        (i start)
        (last-octet 0))
    (declare (fixnum i sum) (type octet last-octet))
    (decf end 2)
    (loop
     (when (> i end)
       (return))
     (let* ((high-octet (aref sequence i))
            (length (cond ((<= #xd8 high-octet #xdf) 4)
                          (t 2))))
       (declare (fixnum length) (type octet high-octet))
       (unless (and (zerop high-octet)
                    (= (the octet (aref sequence (1+ i))) +lf+)
                    (= last-octet +cr+))
         (incf sum))
       (setq last-octet (if (zerop high-octet)
                          (aref sequence (1+ i))
                          0))
       (incf i length)))
    (check-end format start (+ end 2) i)
    sum))

(defmethod compute-number-of-chars :before ((format flexi-utf-32-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end))
  (declare (ignore sequence))
  (let ((length (- end start)))
    (when (plusp (mod length 4))
      (signal-encoding-error format "~A octet~:P cannot be decoded ~
using UTF-32 as ~:*~A is not a multiple-value of four."
                             length))))

(defmethod compute-number-of-chars ((format flexi-utf-32-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end))
  (declare (ignore sequence))
  (ceiling (- end start) 4))

(defmethod compute-number-of-chars ((format flexi-crlf-utf-32-le-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((i start)
        (length (ceiling (- end start) 4)))
    (decf end 8)
    (loop
     (when (> i end)
       (return))
     (cond ((loop for j of-type fixnum from i
                  for octet across #.(vector +cr+ 0 0 0 +lf+ 0 0 0)
                  always (= octet (aref sequence j)))
            (decf length)
            (incf i 8))
           (t (incf i 4))))
    length))

(defmethod compute-number-of-chars ((format flexi-crlf-utf-32-be-format) sequence start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (vector sequence))
  (let ((i start)
        (length (ceiling (- end start) 4)))
    (decf end 8)
    (loop
     (when (> i end)
       (return))
     (cond ((loop for j of-type fixnum from i
                  for octet across #.(vector 0 0 0 +cr+ 0 0 0 +lf+)
                  always (= octet (aref sequence j)))
            (decf length)
            (incf i 8))
           (t (incf i 4))))
    length))

(defgeneric compute-number-of-octets (format sequence start end)
  (declare #.*standard-optimize-settings*)
  (:documentation "Computes the exact number of octets required to
encode the sequence of characters in SEQUENCE from START to END using
the external format FORMAT."))

(defmethod compute-number-of-octets :around (format (list list) start end)
  (declare #.*standard-optimize-settings*)
  (call-next-method format (coerce list 'string*) start end))

(defmethod compute-number-of-octets ((format flexi-8-bit-format) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end))
  (declare (ignore string))  
  (- end start))

(defmethod compute-number-of-octets ((format flexi-utf-8-format) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (string string))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (loop
     (when (>= i end)
       (return))
     (let* ((char-code (char-code (char string i)))
            (char-length (cond ((< char-code #x80) 1)
                               ((< char-code #x800) 2)
                               ((< char-code #x10000) 3)
                               (t 4))))
       (declare (fixnum char-length) (type char-code-integer char-code))
       (incf sum char-length)
       (incf i)))
    sum))

(defmethod compute-number-of-octets ((format flexi-crlf-utf-8-format) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (string string))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (loop
     (when (>= i end)
       (return))
     (let* ((char-code (char-code (char string i)))
            (char-length (cond ((= char-code #.(char-code #\Newline)) 2)
                               ((< char-code #x80) 1)
                               ((< char-code #x800) 2)
                               ((< char-code #x10000) 3)
                               (t 4))))
       (declare (fixnum char-length) (type char-code-integer char-code))
       (incf sum char-length)
       (incf i)))
    sum))

(defmethod compute-number-of-octets ((format flexi-utf-16-format) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (string string))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (loop
     (when (>= i end)
       (return))
     (let* ((char-code (char-code (char string i)))
            (char-length (cond ((< char-code #x10000) 2)
                               (t 4))))
       (declare (fixnum char-length) (type char-code-integer char-code))
       (incf sum char-length)
       (incf i)))
    sum))

(defmethod compute-number-of-octets ((format flexi-crlf-utf-16-le-format) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (string string))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (loop
     (when (>= i end)
       (return))
     (let* ((char-code (char-code (char string i)))
            (char-length (cond ((= char-code #.(char-code #\Newline)) 4)
                               ((< char-code #x10000) 2)
                               (t 4))))
       (declare (fixnum char-length) (type char-code-integer char-code))
       (incf sum char-length)
       (incf i)))
    sum))

(defmethod compute-number-of-octets ((format flexi-crlf-utf-16-be-format) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (string string))
  (let ((sum 0)
        (i start))
    (declare (fixnum i sum))
    (loop
     (when (>= i end)
       (return))
     (let* ((char-code (char-code (char string i)))
            (char-length (cond ((= char-code #.(char-code #\Newline)) 4)
                               ((< char-code #x10000) 2)
                               (t 4))))
       (declare (fixnum char-length) (type char-code-integer char-code))
       (incf sum char-length)
       (incf i)))
    sum))

(defmethod compute-number-of-octets ((format flexi-utf-32-format) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end))
  (declare (ignore string))
  (* 4 (- end start)))

(defmethod compute-number-of-octets ((format flexi-crlf-mixin) string start end)
  (declare #.*fixnum-optimize-settings*)
  (declare (fixnum start end) (string string))
  (+ (call-next-method)
     (* (case (external-format-name format)
          (:utf-32 4)
          (otherwise 1))
        (count #\Newline string :start start :end end :test #'char=))))

(defgeneric character-length (format char)
  (declare #.*fixnum-optimize-settings*)
  (:documentation "Returns the number of octets needed to encode the
single character CHAR.")
  (:method (format char)
   (compute-number-of-octets format (string char) 0 1)))

(defmethod character-length :around ((format flexi-crlf-mixin) (char (eql #\Newline)))
  (declare #.*fixnum-optimize-settings*)
  (+ (call-next-method format +cr+)
     (call-next-method format +lf+)))

(defmethod character-length ((format flexi-8-bit-format) char)
  (declare #.*fixnum-optimize-settings*)
  (declare (ignore char))
  1)

(defmethod character-length ((format flexi-utf-32-format) char)
  (declare #.*fixnum-optimize-settings*)
  (declare (ignore char))
  4)