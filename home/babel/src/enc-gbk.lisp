;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-gbk.lisp --- GBK encodings.
;;;
;;; Copyright (C) 2011, Li Wenpeng  <levin108@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:babel-encodings)

;; populated in gbk-map.lisp
(defvar *gbk-unicode-mapping*)

(defconstant +gbk2-offset+ 0)
(defconstant +gbk3-offset+ 6763)
(defconstant +gbk4-offset+ (+ 6763 6080))
(defconstant +gbk1-offset+ 20902)
(defconstant +gbk5-offset+ (+ 20902 846))

(define-character-encoding :gbk
    "GBK is an extension of the GB2312 character set for simplified
Chinese characters, used in the People's Republic of China."
  :max-units-per-char 4
  :literal-char-code-limit #x80)

(define-condition invalid-gbk-byte (character-decoding-error)
  ()
  (:documentation "Signalled when an invalid GBK byte is found."))

(define-condition invalid-gbk-character (character-encoding-error)
  ()
  (:documentation "Signalled when an invalid GBK character is found."))

(define-octet-counter :gbk (getter type)
  `(lambda (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (let ((noctets 0))
       (loop for i from start below end
             for u1 of-type code-point = (,getter seq i)
             do (cond ((< u1 #x80) (incf noctets))
                      (t (incf noctets 2)))
                (when (and (plusp max) (= noctets max))
                  (return (values noctets i)))
             finally (return (values noctets i))))))

(define-code-point-counter :gbk (getter type)
  `(lambda (seq start end max)
     (declare (type ,type seq))
     (let (u1 (noctets 0))
       (loop with i = start
             while (< i end)
             do (setf u1 (,getter seq i))
                (cond
                  ((eq 0 (logand u1 #x80)) (incf i))
                  (t (incf i 2)))
                (incf noctets)
                (when (and (plusp max) (= noctets max))
                  (return (values noctets i)))
             finally (return (values noctets i))))))

(define-encoder :gbk (getter src-type setter dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (macrolet
         ((do-encoding (index)
            `(let ((u1 0) (u2 0))
               (cond
                 ((<= +gbk2-offset+ ,index (- +gbk3-offset+ 1)) ; gbk/2
                  (setf u1 (+ #xB0 (truncate (/ ,index 94))))
                  (setf u2 (+ #xA1 (mod ,index 94))))
                 ((<= +gbk3-offset+ ,index (- +gbk4-offset+ 1)) ; gbk/3
                  (setf index (- ,index +gbk3-offset+))
                  (setf u1 (+ #x81 (truncate (/ ,index 190))))
                  (setf u2 (+ #x40 (mod ,index 190)))
                  (if (>= u2 #x7F) (incf u2)))
                 ((<= +gbk4-offset+ ,index (- +gbk1-offset+ 1)) ; gbk/4
                  (setf index (- ,index +gbk4-offset+))
                  (setf u1 (+ #xAA (truncate (/ ,index 96))))
                  (setf u2 (+ #x40 (mod ,index 96)))
                  (if (>= u2 #x7F) (incf u2)))
                 ((<= +gbk1-offset+ ,index (- +gbk5-offset+ 1)) ; gbk/1
                  (setf index (- ,index +gbk1-offset+))
                  (setf u1 (+ #xA1 (truncate (/ ,index 94))))
                  (setf u2 (+ #xA1 (mod ,index 94))))
                 ((<= +gbk5-offset+ ,index (length *gbk-unicode-mapping*)) ; gbk/5
                  (setf index (- ,index +gbk5-offset+))
                  (setf u1 (+ #xA8 (truncate (/ ,index 96))))
                  (setf u2 (+ #x40 (mod ,index 96)))
                  (if (>= u2 #x7F) (incf u2))))
               (values u1 u2))))
       (let ((c 0) index (noctets 0))
         (loop for i from start below end
               for code of-type code-point = (,getter src i)
               do (macrolet
                      ((handle-error (&optional (c 'character-encoding-error))
                         `(encoding-error code :gbk src i +repl+ ',c)))
                    (setf c (code-char code))
                    (cond
                      ((< code #x80)    ; ascii
                       (,setter code dest noctets)
                       (incf noctets))
                      (t                ; gbk
                       (setf index
                             (position c *gbk-unicode-mapping*))

                       (if (not index)
                           (handle-error invalid-gbk-character))
                       (multiple-value-bind (uh ul) (do-encoding index)
                         (,setter uh dest noctets)
                         (,setter ul dest (+ 1 noctets))
                         (incf noctets 2)))))
               finally (return (the fixnum (- noctets d-start))))))))

(define-decoder :gbk (getter src-type setter dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest))
     (let ((u1 0) (u2 0) (index 0) (tmp 0) (noctets 0))
       (loop with i = start
             while (< i end)
             do (macrolet
                    ((handle-error (&optional (c 'character-decoding-error))
                       `(decoding-error #(u1 u2) :gbk src i +repl+ ',c)))
                  (setf u1 (,getter src i))
                  (incf i)
                  (cond
                    ((eq 0 (logand u1 #x80))
                     (,setter u1 dest noctets))
                    (t
                     (setf u2 (,getter src i))
                     (incf i)
                     (setf index
                           (block setter-block
                             (cond
                               ((and (<= #xB0 u1 #xF7) (<= #xA1 u2 #xFE))
                                (+ +gbk2-offset+ (+ (* 94 (- u1 #xB0)) (- u2 #xA1))))

                               ((and (<= #x81 u1 #xA0) (<= #x40 u2 #xFE))
                                (cond ((> u2 #x7F) (setf tmp 1))
                                      (t (setf tmp 0)))
                                (+ +gbk3-offset+ (* 190 (- u1 #x81)) (- u2 #x40 tmp)))

                               ((and (<= #xAA u1 #xFE) (<= #x40 #xA0))
                                (cond ((> u2 #x7F) (setf tmp 1))
                                      (t (setf tmp 0)))
                                (+ +gbk4-offset+ (* 96 (- u1 #xAA)) (- u2 #x40 tmp)))

                               ((and (<= #xA1 u1 #xA9) (<= #xA1 u2 #xFE))
                                (+ +gbk1-offset+ (* 94 (- u1 #xA1)) (- u2 #xA1)))

                               ((and (<= #xA8 u1 #xA9) (<= #x40 #xA0))
                                (cond ((> u2 #x7F) (setf tmp 1))
                                      (t (setf tmp 0)))
                                (+ +gbk5-offset+ (* 96 (- u1 #xA8)) (- u2 #x40 tmp)))
                               (t
                                (handle-error invalid-gbk-byte)))))

                     (when (>= index (length *gbk-unicode-mapping*))
                       (handle-error invalid-gbk-byte))
                     (,setter (char-code
                               (elt *gbk-unicode-mapping* index))
                              dest noctets)))
                  (incf noctets))
             finally (return (the fixnum (- noctets d-start)))))))
