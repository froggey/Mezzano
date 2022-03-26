;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: UNZIP; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: DEFLATE Compression Method
;;;   Created: 2000-11-14
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2000,2001 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

;;(defpackage :UNZIP
;;  (:use :common-lisp) )

(in-package :UNZIP)

#+excl
(defmacro defsubst (fun args &body body)
  (if (and (consp fun) (eq (car fun) 'setf))
      (let ((fnam (intern (concatenate 'string
                            "(SETF " (symbol-name (cadr fun)) ")")
                          (symbol-package (cadr fun)))))
        `(progn
           (defsetf ,(cadr fun) (&rest ap) (new-value)
             (list* ',fnam new-value ap))
           (defsubst ,fnam ,args .,body)))
    `(progn
       (defun ,fun ,args .,body)
       (define-compiler-macro ,fun (&rest .args.)
         (cons '(lambda ,args .,body)
               .args.)))))

#+(or cmu clisp)
(defmacro defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))

(declaim (optimize (speed 3) (safety 3) (space 0)))

(deftype octet ()
  '(unsigned-byte 8))

(deftype simple-octet-vector (&optional (size '*))
  `(simple-array (unsigned-byte 8) (,size)))

;;; Some parameters of the DEFLATE compression method

(defconstant *length-encoding*
    ;; Maps length token to (<number of additional bits> <length-offset>)
  '#((0    3)     (0    4)     (0    5)     (0    6)
     (0    7)     (0    8)     (0    9)     (0   10)
     (1   11)     (1   13)     (1   15)     (1   17)
     (2   19)     (2   23)     (2   27)     (2   31)
     (3   35)     (3   43)     (3   51)     (3   59)
     (4   67)     (4   83)     (4   99)     (4  115)
     (5  131)     (5  163)     (5  195)     (5  227)
     (0  258) ))

(defconstant *dist-encoding*
    ;; Maps distance token to (<number of additional bits> <distance-offset>)
  '#( (0     1)      (0     2)      (0     3)      (0     4)
      (1     5)      (1     7)      (2     9)      (2    13)
      (3    17)      (3    25)      (4    33)      (4    49)
      (5    65)      (5    97)      (6   129)      (6   193)
      (7   257)      (7   385)      (8   513)      (8   769)
      (9  1025)      (9  1537)     (10  2049)     (10  3073)
     (11  4097)     (11  6145)     (12  8193)     (12 12289)
     (13 16385)     (13 24577) ))

(defconstant *fixed-huffman-code-lengths*
    ;; Code length for the fixed huffman code
    (let ((res (make-array 288)))
      (loop for i from   0 to 143 do (setf (aref res i) 8))
      (loop for i from 144 to 255 do (setf (aref res i) 9))
      (loop for i from 256 to 279 do (setf (aref res i) 7))
      (loop for i from 280 to 287 do (setf (aref res i) 8))
      res))

(defconstant *code-length-code-lengths-order*
    '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
  "Order in which code lengths for the code length alphabet are written.")

;; Layout for various kinds of huffman trees

(eval-when (compile eval load)
  (defconstant *code-len-ht-layout* '(4 2 2 2))
  (defconstant *literal-ht-layout*  '(9 2 2 2))
  (defconstant *dist-ht-layout*     '(9 2 2 2)) )

;;; fixnum arithmetric

(defmacro %incf (place)
  `(incf (the fixnum ,place)))

(defmacro %decf (place)
  `(decf (the fixnum ,place)))

(defmacro %= (x y)
  `(= (the fixnum ,x) (the fixnum ,y)))

(defmacro %+ (&rest xs)
  `(the fixnum (+ ,@(mapcar (lambda (x) `(the fixnum ,x)) xs))))

(defmacro %- (&rest xs)
  `(the fixnum (- ,@(mapcar (lambda (x) `(the fixnum ,x)) xs))))

(defmacro %logand (&rest xs)
  `(the fixnum (logand ,@(mapcar (lambda (x) `(the fixnum ,x)) xs))))

(defmacro %dotimes ((var n) &body body)
  (let ((g (gensym)))
    `(let ((,g ,n))
       (declare (type fixnum ,g))
       (do ((,var 0 (%+ ,var 1)))
           ((%= ,var ,g))
         (declare (type fixnum ,var))
         ,@body))))

(defconstant +null-octet-vector+
    (make-array 0 :element-type '(unsigned-byte 8)))

(defsubst reverse-byte (n x)
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 16) x)
           (type (integer 0 16) n))
  (setf x (logior (ash (logand x #b1010101010101010) -1)
                  (ash (logand x #b0101010101010101) +1)))
  (setf x (logior (ash (logand x #b1100110011001100) -2)
                  (ash (logand x #b0011001100110011) +2)))
  (setf x (logior (ash (logand x #b1111000011110000) -4)
                  (ash (logand x #b0000111100001111) +4)))
  (setf x (logior (ash (logand x #b1111111100000000) -8)
                  (ash (logand x #b0000000011111111) +8)))
  (ash x (- n 16)))
  
(defun build-huffman-tree (lengthen layout)
  ;; Build up a huffman tree given a vector of code lengthen as
  ;; described in RFC1951.
  (declare (type simple-vector lengthen))
  (let* ((max-bits   (loop for x across lengthen maximize (the fixnum x)))
         (max-symbol (1- (length lengthen)))
         (bl-count   (make-array (+ 1 max-bits) :initial-element 0))
         (next-code  (make-array (+ 1 max-bits) :initial-element 0))
         (ht         (make-array (expt 2 (car layout)) :initial-element nil)))
    (declare (type simple-vector bl-count next-code))
    ;;
    (loop for x fixnum across lengthen do
          (unless (zerop (the fixnum x))
            (incf (the (unsigned-byte 16) (aref bl-count x)))))
    ;;
    (let ((code 0))
      (loop for bits fixnum from 1 to max-bits do
            (setf code (the fixnum (ash (the fixnum (+ (the fixnum code) 
                                                       (the fixnum (aref bl-count (the fixnum (1- (the fixnum bits)))))))
                                        1)))
            (setf (aref next-code bits) code)))
    ;;
    (loop for sym from 0 to max-symbol do
          (let ((len (aref lengthen sym)))
            (declare (type fixnum len))
            (unless (zerop len)
              (let ((code (reverse-byte len (aref next-code len))))
                (declare (type fixnum code))
                (insert-into-tree ht len code sym layout)))
            (incf (the (unsigned-byte 16) (aref next-code len)))))
    ht ))

(defun insert-into-tree (table codelen code sym layout)
  (declare (type simple-vector table)
           (type (integer 0 15) codelen)
           (type (unsigned-byte 15) code)
           (type (unsigned-byte 10) sym))
  (let ((m (car layout)))
    (declare (type (integer 0 15) m))
    (cond ((> codelen m)
           (when (null (cdr layout))
             (error "Code too long"))
           (let ((upper (ldb (byte m 0) code))
                 (lower (ldb (byte 15 #|(- codelen m)|# m) code)))
             (unless (aref table upper)
               (setf (aref table upper)
                 (make-array (expt 2 (cadr layout)) :initial-element nil)))
             (insert-into-tree (aref table upper) (- codelen m) lower sym (cdr layout))))
          ((<= codelen m)
           (let ((lacking-bits (- m codelen)))
             (%dotimes (j (expt 2 lacking-bits))
               (declare (type (unsigned-byte 15) j))
               (let ((k (logior code (ash j codelen))))
                 #-(and)
                 (unless (null (aref table k))
                   (error "Code clash!"))
                 (setf (aref table k)
                   (cons codelen sym)))))))))

;;;

(defmacro bs/read-symbol (ht ms)
  ;;(assert (constantp ms));;xxx cmu barf on this
  (setf ms (eval ms))
  (cond 
   ((null ms)
    '(error "No such code")
    )
   (t
    ;; For an "interactive" unzipper, we only read new octets as
    ;; needed. We loop, until we have enough bits to make a
    ;; sensible decision.
    `((lambda (ht)
        (declare (type (simple-array t (*)) ht))
        (let ((m ',(car ms)) (b 0) x)
          (declare (type (unsigned-byte 24) b))
          ;; (bs/ensure-n-bits 24)
          (loop
            (setf b (logand bs/q (- (ash 1 m) 1)))
            (setf x (svref ht b))
            (cond ((null x) (error "invalid code"))
                  ((and (consp x) (<= (car x) bs/qs))
                   ;; we had enough bits
                   (bs/skip-byte* (car x))
                   (return (cdr x)))
                  ((and (not (consp x)) (<= m bs/qs))
                   ;; enough bits for recursion
                   (bs/skip-byte* m)
                   (return (bs/read-symbol x ',(cdr ms))))
                  (t
                   ;; we had not enough bits, read a further octet and
                   ;; loop.
                   (bs/ensure-n-bits (+ bs/qs 8)) )))))
      ,ht))))

(defmacro read-code-lengths (huffman-tree n)
  `((lambda (huffman-tree n)
      (declare (type (integer 0 1000) n))
      (let ((res (make-array n :initial-element 0)))
        (declare (type (simple-array t (*)) res))
        (do ((i 0 i))
            ((>= i n))
          (declare (type (integer 0 1000) i))
          (macrolet ((put-element (x)
                       `(progn (setf (aref res i) ,x)
                               (incf i)))
                     (put-n-elements (n x)
                       `(let ((nn ,n) (xx ,x))
                          (%dotimes (kk nn) (put-element xx)))))
            (let ((c (bs/read-symbol huffman-tree *code-len-ht-layout*)))
              (declare (type fixnum c))
              (case c
                (16 (put-n-elements (+ 3 (bs/read-byte 2)) (aref res (- i 1))))
                (17 (put-n-elements (+ 3 (bs/read-byte 3)) 0))
                (18 (put-n-elements (+ 11 (bs/read-byte 7)) 0))
                (t  (put-element c))))))
        res))
    ,huffman-tree ,n))

(defmacro read-length/dist-pair (length-code dist-ht)
  `((lambda (length-code dist-ht)
      (declare (type fixnum length-code))
      (values
       ;; length
       (let ((meaning (svref *length-encoding* (- length-code 257))))
         (+ (the (integer 0 258) (second meaning))
            (bs/read-byte (the (integer 0 24)
                            (first meaning)))))
       ;; distance
       (let ((dist-sym (if dist-ht
                           (bs/read-symbol dist-ht *dist-ht-layout*)
                         (bs/read-reversed-byte 5))))
         (let ((meaning (svref *dist-encoding* dist-sym)))
           (+ (the (integer 0 24577) (second meaning))
              (bs/read-byte (the (integer 0 24);xxx? right?
                              (first meaning)))))) ))
    ,length-code ,dist-ht))

(defvar *fixed-huffman-tree*
    nil)

(defun fixed-huffman-tree ()
  (or *fixed-huffman-tree*
      (build-huffman-tree *fixed-huffman-code-lengths* *literal-ht-layout*)))

(defstruct inflator-state
  (len          0       :type fixnum)   ;length of stored data
  (cfinalp      0       :type bit)      ;final flag
  dist-ht                               ;distance huffman tree
  literal-ht                            ;literal huffman tree
  
  ;;; Output buffer
  (buffer       (make-array #x10000 :element-type '(unsigned-byte 8))
                :type simple-octet-vector)
  (bptr         0       :type fixnum)   ;output buffer pointer

  (ip           'initial)               ;'instruction pointer'

  ;;; Bitstream
  
  ;;  bit buffer:
  (bs/q         0       :type (unsigned-byte 24))
  (bs/qs        0       :type (integer 0 24))

  length dist iii

  input
  )

(defun continue-inflator (inflator)
  (step-inflator inflator nil))

(defmacro bs/ensure-n-bits (n)
  ;; Ensure at least 'size' bits in 'q'
  (let ((nn (gensym)))
    `(let ((,nn ,n))
       (do () ((>= bs/qs ,nn))
         (setf bs/q (logior bs/q
                            (ash (the (unsigned-byte 8) (bs/next-octet))
                                 (the (integer 0 16) bs/qs)))
               bs/qs (+ bs/qs 8))))))

(defmacro bs/peek (n)
  `(logand bs/q (- (the (unsigned-byte 24) (ash 1 (the (integer 0 16) ,n))) 1)))

(defmacro bs/skip (n)
  `(setf bs/q (ash bs/q (- ,n))
         bs/qs (- bs/qs ,n)))

(defmacro bs/read-byte (size)
  (let ((gsize (gensym)))
    `(let ((,gsize ,size))
       (bs/ensure-n-bits ,gsize)
       (prog1
           (bs/peek ,gsize)
         (bs/skip ,gsize)))))

(defmacro bs/peek-byte (size)
  (let ((gsize (gensym)))
    `(let ((,gsize ,size))
       (declare (type (integer 0 16) ,gsize))
       (bs/ensure-n-bits ,gsize)
       (bs/peek ,gsize))))

(defmacro bs/skip-byte (size)
  (let ((gsize (gensym)))
    `(let ((,gsize ,size))
       (declare (type (integer 0 16) ,gsize))
       (bs/ensure-n-bits ,gsize)
       (bs/skip ,gsize))))

(defmacro bs/skip-byte* (size)
  (let ((gsize (gensym)))
    `(let ((,gsize ,size))
       (declare (type (integer 0 16) ,gsize))
       (bs/skip ,gsize))))

(defmacro bs/skip-to-byte-boundary ()
  `(bs/skip-byte (mod bs/qs 8)))

(defmacro bs/read-reversed-byte (size)
  (let ((gsize (gensym)))
    `(let ((,gsize ,size))
       (reverse-byte ,gsize (bs/read-byte ,gsize)))))

(defmacro ilabels (funs &body body)
  `(macrolet ,(mapcar (lambda (fun)
                        (destructuring-bind (name args &rest body) fun
                          `(,name ,args
                                  (list (list* 'lambda ',args ',body)
                                        ,@args))))
                      funs)
     ,@body))

(defmacro putbyte (by)
  `(progn
     (setf (aref buffer (%logand #xFFFF bptr)) 
       (the (unsigned-byte 8) ,by))
     (setf bptr (logand #xFFFF (%+ bptr 1)))))

(defmacro locdefs (&body body)
  (cond ((and (consp (car body))
              (eq (caar body) 'defun))
         `(labels (,(cdar body)) 
            (locdefs ,@(cdr body))))
        ((and (consp (car body))
              (eq (caar body) 'defsubst))
         `(ilabels (,(cdar body)) 
                   (locdefs ,@(cdr body))))
        ((and (consp (car body))
              (eq (caar body) 'defmacro))
         `(macrolet (,(cdar body)) 
            (locdefs ,@(cdr body))))
        (t
         `(locally ,@body))))

(defun step-inflator (state)
  (declare (type inflator-state state))
  (let* ((len        (inflator-state-len state))
         (cfinalp    (inflator-state-cfinalp state))
         (dist-ht    (inflator-state-dist-ht state))
         (literal-ht (inflator-state-literal-ht state))
         (buffer     (inflator-state-buffer state))
         (bptr       (inflator-state-bptr state))
         (ip         (inflator-state-ip state))
         (bs/q       (inflator-state-bs/q state))
         (bs/qs      (inflator-state-bs/qs state))
         (iii 0) (c 0)
         (length 0) (dist 0)
         )
    (declare
     (type fixnum length dist)
     (type fixnum iii c)
     (type fixnum len cfinalp bptr)
     (type (integer 0 24) bs/qs)
     (type (unsigned-byte 24) bs/q))
    (locdefs

     (defsubst bs/next-octet ()
       (read-byte (inflator-state-input state)))

     (multiple-value-prog1
         (block bar
           (tagbody
             (case ip
               (uncompress-loop (go uncompress-loop))
               (initial         (go initial))
               (stored-initial  (go stored-initial))
               (stored-loop     (go stored-loop))
               (dynamic-initial (go dynamic-initial))
               (L1-reentry      (go L1-reentry))
               (L3-reentry      (go L3-reentry))
               (done            (return-from bar nil)) )
    
            initial
             (when (= 1 cfinalp)
               (setf ip 'done)
               (return-from bar (values buffer 0 bptr t)))
             (setf cfinalp (bs/read-byte 1))
    
             (let ((btype (bs/read-byte 2)))
               (if (= btype 2) (go dynamic-initial))
               (if (= btype 0) (go stored-initial))
               (if (= btype 1) (go fixed-initial))
               (if (= btype 3) (error "Bad BTYPE=~D" btype)))

            uncompress-loop
             (setf iii 100)
            L1
             (when (%= iii 0)
               (go uncompress-loop))
             (%decf iii)
             
             (setf c (bs/read-symbol literal-ht *literal-ht-layout*))

             (unless (<= c 255) (go L2))
             (putbyte c)
             (when (= bptr 0)
               (setf (inflator-state-iii state) iii
                     ip 'L1-reentry)
               (return-from bar (values buffer 0 #x10000)))
             (go L1)
             
            L2
             (when (= c 256) (go initial))
             
             (multiple-value-setq (length dist)
               (read-length/dist-pair c dist-ht))

            L3
             (when (%= 0 length) (go L1))
             (setf length (%- length 1))
             (setf c (aref buffer (%logand (%- bptr dist) #xffff)))
             (putbyte c)
             (when (= bptr 0)
               (setf (inflator-state-length state) length
                     (inflator-state-dist state) dist
                     (inflator-state-iii state) iii
                     ip 'L3-reentry)
               (return-from bar (values buffer 0 #x10000)))
             (go L3)
             
            L3-reentry
             (setf length (inflator-state-length state)
                   dist (inflator-state-dist state)
                   iii  (inflator-state-iii state))
             (go L3)
             
            L1-reentry
             (setf iii  (inflator-state-iii state))
             (go L1)


            dynamic-initial
             (let (n-hlit n-hdist n-hclen hclens hlit-lens hdist-lens code-len-ht)
               (setf n-hlit  (+ 257 (bs/read-byte 5))
                     n-hdist (+ 1 (bs/read-byte 5))
                     n-hclen (+ 4 (bs/read-byte 4))
                     hclens  (make-array 19 :initial-element 0))
               (locally
                   (declare (type (simple-array t (*)) hclens)
                            (type (unsigned-byte 6) n-hdist)
                            (type (unsigned-byte 5) n-hclen))
                 (loop
                     for i fixnum from 1 to n-hclen
                     for j fixnum in *code-length-code-lengths-order*
                     do (setf (aref hclens j) (bs/read-byte 3)))
                 ;;
                 (setf code-len-ht (build-huffman-tree hclens *code-len-ht-layout*))

                 ;; slurp the huffman trees for literals and distances    
                 (setf hlit-lens (read-code-lengths code-len-ht n-hlit))
                 (setf hdist-lens (read-code-lengths code-len-ht n-hdist))
    
                 (setf literal-ht (build-huffman-tree hlit-lens *literal-ht-layout*))
                 (setf dist-ht (build-huffman-tree hdist-lens *dist-ht-layout*))))

             (go uncompress-loop)

            stored-initial
             (bs/skip-to-byte-boundary)
             (let ((nlen 0))
               (setf len  (bs/read-byte 16))
               (setf nlen (bs/read-byte 16))
               (unless (= (logxor #xFFFF nlen) len)
                 (error "NLEN/LEN mismatch: ~S versus ~S."
                        (logxor #xffff nlen) len)))
    
            stored-loop
             (when (= len 0)
               (go initial))
             (setf len (%- len 1))
             (putbyte (bs/read-byte 8))
             ;;
             (when (= bptr 0)
               (setf ip 'stored-loop)
               (return-from bar (values buffer 0 #x10000)))
             ;;;
             (go stored-loop)
             
            fixed-initial
             (setf literal-ht (fixed-huffman-tree))
             (setf dist-ht nil)
             (go uncompress-loop) ))
       ;;
       (setf (inflator-state-len state) len)
       (setf (inflator-state-cfinalp state) cfinalp)
       (setf (inflator-state-dist-ht state) dist-ht)
       (setf (inflator-state-literal-ht state) literal-ht)
       (setf (inflator-state-buffer state) buffer)
       (setf (inflator-state-bptr state) bptr)
       (setf (inflator-state-ip state) ip)
       (setf (inflator-state-bs/q state) bs/q)
       (setf (inflator-state-bs/qs state) bs/qs) ))))

;; make-inflator &key input
;;                    interactive-input
;;                    interactive-output
;;
;; step-inflator i -> buffer start end
;; inflator-unconsumed-input i -> octet-vector
;; inflator-get-crc i -> crc
;; inflator-get-uncompressed-length i ->
;; 
;; make-inflating-stream input
;;                       &key element-type
;;                            interactive-input
;;                            interactive-output
;; 