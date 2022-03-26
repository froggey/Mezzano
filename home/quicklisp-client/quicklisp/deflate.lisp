;;;; Deflate --- RFC 1951 Deflate Decompression
;;;;
;;;; Copyright (C) 2000-2009 PMSF IT Consulting Pierre R. Mai.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;;
;;;; Except as contained in this notice, the name of the author shall
;;;; not be used in advertising or otherwise to promote the sale, use or
;;;; other dealings in this Software without prior written authorization
;;;; from the author.
;;;; 
;;;; $Id: 377d3a33e9db5a3b54c850619183ee555a41b894 $

(cl:in-package #:ql-gunzipper)

;;;; %File Description:
;;;; 
;;;; This file contains routines implementing the RFC 1951 Deflate
;;;; Compression and/or Decompression method, as used by e.g. gzip and
;;;; other compression and archiving tools and protocols.  It also
;;;; implements handling routines for zlib-style (RFC 1950) and
;;;; gzip-style (RFC 1952) wrappers around raw Deflate streams.
;;;; 
;;;; The main entry points are the functions inflate-stream, and its
;;;; cousins inflate-zlib-stream and inflate-gzip-stream, which take
;;;; an input-stream and an output-stream as their arguments, and
;;;; inflate the RFC 1951, RFC 1950 or RFC 1952-style deflate formats
;;;; from the input-stream to the output-stream.
;;;;

;;;
;;; Conditions
;;;

(define-condition decompression-error (simple-error)
  ())

(define-condition deflate-decompression-error (decompression-error)
  ()
  (:report 
   (lambda (c s)
     (with-standard-io-syntax
       (let ((*print-readably* nil))
         (format s 
                 "Error detected during deflate decompression: ~?"
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c)))))))

(define-condition zlib-decompression-error (decompression-error)
  ()
  (:report 
   (lambda (c s)
     (with-standard-io-syntax
       (let ((*print-readably* nil))
         (format s 
                 "Error detected during zlib decompression: ~?"
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c)))))))

(define-condition gzip-decompression-error (decompression-error)
  ()
  (:report 
   (lambda (c s)
     (with-standard-io-syntax
       (let ((*print-readably* nil))
         (format s 
                 "Error detected during zlib decompression: ~?"
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c)))))))

;;;
;;; Adler-32 Checksums
;;;

(defconstant +adler-32-start-value+ 1
  "Start value for Adler-32 checksums as per RFC 1950.")

(defconstant +adler-32-base+ 65521
  "Base value for Adler-32 checksums as per RFC 1950.")

(declaim (ftype 
          (function ((unsigned-byte 32) (simple-array (unsigned-byte 8) (*)) fixnum)
                    (unsigned-byte 32))
          update-adler32-checksum))
(defun update-adler32-checksum (crc buffer end)
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum end)
           (optimize (speed 3) (debug 0) (space 0) (safety 0))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((s1 (ldb (byte 16 0) crc))
        (s2 (ldb (byte 16 16) crc)))
    (declare (type (unsigned-byte 32) s1 s2))
    (dotimes (i end)
      (declare (type fixnum i))
      (setq s1 (mod (+ s1 (aref buffer i)) +adler-32-base+)
            s2 (mod (+ s2 s1) +adler-32-base+)))
    (dpb s2 (byte 16 16) s1)))

;;;
;;; CRC-32 Checksums
;;;

(defconstant +crc-32-start-value+ 0
  "Start value for CRC-32 checksums as per RFC 1952.")

(defconstant +crc-32-polynomial+ #xedb88320
  "CRC-32 Polynomial as per RFC 1952.")

(declaim (ftype #-lispworks (function () (simple-array (unsigned-byte 32) (256)))
                #+lispworks (function () (sys:simple-int32-vector 256))
                generate-crc32-table))
(defun generate-crc32-table ()
  (let ((result #-lispworks (make-array 256 :element-type '(unsigned-byte 32))
                #+lispworks (sys:make-simple-int32-vector 256)))
    (dotimes (i #-lispworks (length result) #+lispworks 256 result)
      (let ((cur i))
        (dotimes (k 8)
          (setq cur (if (= 1 (logand cur 1))
                        (logxor (ash cur -1) +crc-32-polynomial+)
                        (ash cur -1))))
        #-lispworks (setf (aref result i) cur)
        #+lispworks (setf (sys:int32-aref result i)
                          (sys:integer-to-int32
                           (dpb (ldb (byte 32 0) cur) (byte 32 0) 
                                (if (logbitp 31 cur) -1 0))))))))

(declaim (ftype 
          (function ((unsigned-byte 32) (simple-array (unsigned-byte 8) (*)) fixnum)
                    (unsigned-byte 32))
          update-crc32-checksum))
#-lispworks
(defun update-crc32-checksum (crc buffer end)
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum end)
           (optimize (speed 3) (debug 0) (space 0) (safety 0))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((table (load-time-value (generate-crc32-table)))
        (cur (logxor crc #xffffffff)))
    (declare (type (simple-array (unsigned-byte 32) (256)) table)
             (type (unsigned-byte 32) cur))
    (dotimes (i end)
      (declare (type fixnum i))
      (let ((index (logand #xff (logxor cur (aref buffer i)))))
        (declare (type (unsigned-byte 8) index))
        (setq cur (logxor (aref table index) (ash cur -8)))))
    (logxor cur #xffffffff)))

#+lispworks
(defun update-crc32-checksum (crc buffer end)
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum end)
           (optimize (speed 3) (debug 0) (space 0) (safety 0) (float 0)))
  (let ((table (load-time-value (generate-crc32-table)))
        (cur (sys:int32-lognot (sys:integer-to-int32 
                                (dpb (ldb (byte 32 0) crc) (byte 32 0) 
                                     (if (logbitp 31 crc) -1 0))))))
    (declare (type (sys:simple-int32-vector 256) table)
             (type sys:int32 cur))
    (dotimes (i end)
      (declare (type fixnum i))
      (let ((index (sys:int32-to-integer
                    (sys:int32-logand #xff (sys:int32-logxor cur (aref buffer i))))))
        (declare (type fixnum index))
        (setq cur (sys:int32-logxor (sys:int32-aref table index)
                                    (sys:int32-logand #x00ffffff 
                                                      (sys:int32>> cur 8))))))
    (ldb (byte 32 0) (sys:int32-to-integer (sys:int32-lognot cur)))))

;;;
;;; Helper Data Structures: Sliding Window Stream
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +sliding-window-size+ 32768
    "Size of sliding window for RFC 1951 Deflate compression scheme."))

(defstruct sliding-window-stream
  (stream nil :type stream :read-only t)
  (buffer (make-array +sliding-window-size+ :element-type '(unsigned-byte 8)) 
   :type (simple-array (unsigned-byte 8) (#.+sliding-window-size+)) :read-only t)
  (buffer-end 0 :type fixnum)
  (checksum nil :type symbol :read-only t)
  (checksum-value 0 :type (unsigned-byte 32)))

(declaim (inline sliding-window-stream-write-byte))
(defun sliding-window-stream-write-byte (stream byte)
  (declare (type sliding-window-stream stream) (type (unsigned-byte 8) byte)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  "Write a single byte to the sliding-window-stream."
  (let ((end (sliding-window-stream-buffer-end stream)))
    (declare (type fixnum end))
    (unless (< end +sliding-window-size+)
      (write-sequence (sliding-window-stream-buffer stream)
                      (sliding-window-stream-stream stream))
      (case (sliding-window-stream-checksum stream)
        (:adler-32 (setf (sliding-window-stream-checksum-value stream)
                         (update-adler32-checksum
                          (sliding-window-stream-checksum-value stream)
                          (sliding-window-stream-buffer stream)
                          +sliding-window-size+)))
        (:crc-32 (setf (sliding-window-stream-checksum-value stream)
                       (update-crc32-checksum
                        (sliding-window-stream-checksum-value stream)
                        (sliding-window-stream-buffer stream)
                        +sliding-window-size+))))
      (setq end 0))
    (setf (aref (sliding-window-stream-buffer stream) end) byte
          (sliding-window-stream-buffer-end stream) (1+ end))))

(defun sliding-window-stream-flush (stream)
  (declare (type sliding-window-stream stream))
  "Flush any remaining buffered bytes from the stream."
  (let ((end (sliding-window-stream-buffer-end stream)))
    (declare (type fixnum end))
    (unless (zerop end)
      (case (sliding-window-stream-checksum stream)
        (:adler-32 (setf (sliding-window-stream-checksum-value stream)
                         (update-adler32-checksum
                          (sliding-window-stream-checksum-value stream)
                          (sliding-window-stream-buffer stream)
                          end)))
        (:crc-32 (setf (sliding-window-stream-checksum-value stream)
                       (update-crc32-checksum
                        (sliding-window-stream-checksum-value stream)
                        (sliding-window-stream-buffer stream)
                        end))))
      (write-sequence (sliding-window-stream-buffer stream)
                      (sliding-window-stream-stream stream)
                      :end end))))

(defun sliding-window-stream-copy-bytes (stream distance length)
  (declare (type sliding-window-stream stream) (type fixnum distance length))
  "Copy a number of bytes from the current sliding window."
  (let* ((end (sliding-window-stream-buffer-end stream))
         (start (mod (- end distance) +sliding-window-size+))
         (buffer (sliding-window-stream-buffer stream)))
    (declare (type fixnum end start)
             (type (simple-array (unsigned-byte 8) (#.+sliding-window-size+)) buffer))
    (dotimes (i length)
      (sliding-window-stream-write-byte
       stream
       (aref buffer (mod (+ start i) +sliding-window-size+))))))

;;;
;;; Helper Data Structures: Bit-wise Input Stream
;;;

(defstruct bit-stream
  (stream nil :type stream :read-only t)
  (next-byte 0 :type fixnum)
  (bits 0 :type (unsigned-byte 29))
  (bit-count 0 :type (unsigned-byte 8)))

(declaim (inline bit-stream-get-byte))
(defun bit-stream-get-byte (stream)
  (declare (type bit-stream stream))
  "Read another byte from the underlying stream."
  (the (unsigned-byte 8) (read-byte (bit-stream-stream stream))))

(declaim (inline bit-stream-read-bits))
(defun bit-stream-read-bits (stream bits)
  (declare (type bit-stream stream)
           ;; [quicklisp-added]
           ;; FIXME: This might be fixed soon in ECL.
           ;; http://article.gmane.org/gmane.lisp.ecl.general/7659
           #-ecl
           (type (unsigned-byte 8) bits))
  "Read single or multiple bits from the given bit-stream."
  (loop while (< (bit-stream-bit-count stream) bits)
        do
     ;; Fill bits
     (setf (bit-stream-bits stream)
           (logior (bit-stream-bits stream)
                   (the (unsigned-byte 29)
                     (ash (bit-stream-get-byte stream)
                          (bit-stream-bit-count stream))))
           (bit-stream-bit-count stream) (+ (bit-stream-bit-count stream) 8)))
  ;; Return properly masked bits
  (if (= (bit-stream-bit-count stream) bits)
      (prog1 (bit-stream-bits stream)
        (setf (bit-stream-bits stream) 0
              (bit-stream-bit-count stream) 0))
      (prog1 (ldb (byte bits 0) (bit-stream-bits stream))
        (setf (bit-stream-bits stream) (ash (bit-stream-bits stream) (- bits))
              (bit-stream-bit-count stream) (- (bit-stream-bit-count stream) bits)))))

(declaim (inline bit-stream-copy-block))
(defun bit-stream-copy-block (stream out-stream)
  (declare (type bit-stream stream) (type sliding-window-stream out-stream)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  "Copy a given block of bytes directly from the underlying stream."
  ;; Skip any remaining unprocessed bits
  (setf (bit-stream-bits stream) 0
        (bit-stream-bit-count stream) 0)
  ;; Get LEN/NLEN and copy bytes
  (let* ((len (logior (bit-stream-get-byte stream)
                      (ash (bit-stream-get-byte stream) 8)))
         (nlen (ldb (byte 16 0) 
                    (lognot (logior (bit-stream-get-byte stream)
                                    (ash (bit-stream-get-byte stream) 8))))))
    (unless (= len nlen)
      (error 'deflate-decompression-error
             :format-control 
             "Block length mismatch for stored block: LEN(~D) vs. NLEN(~D)!"
             :format-arguments (list len nlen)))
    (dotimes (i len)
      (sliding-window-stream-write-byte out-stream (bit-stream-get-byte stream)))))

;;;
;;; Huffman Coding
;;;

;;; A decode-tree struct contains all information necessary to decode
;;; the given canonical huffman code.  Note that length-count contains
;;; the number of codes with a given length for each length, whereas
;;; the code-symbols array contains the symbols corresponding to the
;;; codes in canoical order of the codes.
;;;
;;; Decoding then uses this information and the principles underlying
;;; canonical huffman codes to determine whether the currently
;;; collected word falls between the first code and the last code for
;;; the current length, and if so, uses the offset to determine the
;;; code's symbol.  Otherwise more bits are needed.

(defstruct decode-tree
  (length-count (make-array 16 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)) :read-only t)
  (code-symbols (make-array 16 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*))))

(defun make-huffman-decode-tree (code-lengths)
  "Construct a huffman decode-tree for the canonical huffman code with
the code lengths of each symbol given in the input array."
  (let* ((max-length (reduce #'max code-lengths :initial-value 0))
         (next-code (make-array (1+ max-length) :element-type 'fixnum
                                :initial-element 0))
         (code-symbols (make-array (length code-lengths) :element-type 'fixnum
                                   :initial-element 0))
         (length-count (make-array (1+ max-length) :element-type 'fixnum
                                   :initial-element 0)))
    ;; Count length occurences and calculate offsets of smallest codes
    (loop for index from 1 to max-length
          for code = 0 then (+ code (aref length-count (1- index)))
          do
       (setf (aref next-code index) code)
          initially
       ;; Count length occurences
       (loop for length across code-lengths
             do
          (incf (aref length-count length))
             finally
          (setf (aref length-count 0) 0)))
    ;; Construct code symbols mapping
    (loop for length across code-lengths
          for index upfrom 0
          unless (zerop length)
            do
         (setf (aref code-symbols (aref next-code length)) index)
         (incf (aref next-code length)))
    ;; Return result
    (make-decode-tree :length-count length-count :code-symbols code-symbols)))

(declaim (inline read-huffman-code))
(defun read-huffman-code (bit-stream decode-tree)
  (declare (type bit-stream bit-stream) (type decode-tree decode-tree)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  "Read the next huffman code word from the given bit-stream and
return its decoded symbol, for the huffman code given by decode-tree."
  (loop with length-count of-type (simple-array fixnum (*)) 
          = (decode-tree-length-count decode-tree)
        with code-symbols of-type (simple-array fixnum (*))
          = (decode-tree-code-symbols decode-tree)
        for code of-type fixnum = (bit-stream-read-bits bit-stream 1)
          then (+ (* code 2) (bit-stream-read-bits bit-stream 1))
        for index of-type fixnum = 0 then (+ index count)
        for first of-type fixnum = 0 then (* (+ first count) 2)
        for length of-type fixnum upfrom 1 below (length length-count)
        for count = (aref length-count length)
        thereis (when (< code (the fixnum (+ first count)))
                  (aref code-symbols (+ index (- code first))))
        finally
     (error 'deflate-decompression-error
            :format-control 
            "Corrupted Data detected during decompression: ~
             Incorrect huffman code (~X) in huffman decode!"
            :format-arguments (list code))))

;;;
;;; Standard Huffman Tables
;;;

(defparameter *std-lit-decode-tree* 
  (make-huffman-decode-tree 
   (concatenate 'vector
                (make-sequence 'vector 144 :initial-element 8)
                (make-sequence 'vector 112 :initial-element 9)
                (make-sequence 'vector 24 :initial-element 7)
                (make-sequence 'vector 8 :initial-element 8))))
                
(defparameter *std-dist-decode-tree* 
  (make-huffman-decode-tree
   (make-sequence 'vector 32 :initial-element 5)))

;;;
;;; Dynamic Huffman Table Handling
;;;

(defparameter *code-length-entry-order*
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
  "Order of Code Length Tree Code Lengths.")

(defun decode-code-length-entries (bit-stream count decode-tree)
  "Decode the given number of code length entries from the bit-stream 
using the given decode-tree, and return a corresponding array of code 
lengths for further processing."
  (do ((result (make-array count :element-type 'fixnum :initial-element 0))
       (index 0))
      ((>= index count) result)
    (let ((code (read-huffman-code bit-stream decode-tree)))
      (ecase code
        ((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
           (setf (aref result index) code)
           (incf index))
        (16
           (let ((length (+ 3 (bit-stream-read-bits bit-stream 2))))
             (dotimes (i length)
               (setf (aref result (+ index i)) (aref result (1- index))))
             (incf index length)))
        (17
           (let ((length (+ 3 (bit-stream-read-bits bit-stream 3))))
             (dotimes (i length)
               (setf (aref result (+ index i)) 0))
             (incf index length)))
        (18
           (let ((length (+ 11 (bit-stream-read-bits bit-stream 7))))
             (dotimes (i length)
               (setf (aref result (+ index i)) 0))
             (incf index length)))))))

(defun decode-huffman-tables (bit-stream)
  "Decode the stored huffman tables from the given bit-stream, returning
the corresponding decode-trees for literals/length and distance codes."
  (let* ((hlit (bit-stream-read-bits bit-stream 5))
         (hdist (bit-stream-read-bits bit-stream 5))
         (hclen (bit-stream-read-bits bit-stream 4)))
    ;; Construct Code Length Decode Tree
    (let ((cl-decode-tree
           (loop with code-lengths = (make-array 19 :element-type '(unsigned-byte 8) 
                                          :initial-element 0)
                 for index from 0 below (+ hclen 4)
                 for code-length = (bit-stream-read-bits bit-stream 3)
                 for code-index = (aref *code-length-entry-order* index)
                 do
              (setf (aref code-lengths code-index) code-length)
                 finally
              (return (make-huffman-decode-tree code-lengths)))))
      ;; Decode Code Length Table and generate separate huffman trees
      (let ((entries (decode-code-length-entries bit-stream 
                                                 (+ hlit 257 hdist 1)
                                                 cl-decode-tree)))
        (values 
          (make-huffman-decode-tree (subseq entries 0 (+ hlit 257)))
          (make-huffman-decode-tree (subseq entries (+ hlit 257))))))))

;;;
;;; Compressed Block Handling
;;;

(declaim (inline decode-length-entry))
(defun decode-length-entry (symbol bit-stream)
  "Decode the given length symbol into a proper length specification."
  (cond
    ((<= symbol 264) (- symbol 254))
    ((<= symbol 268) (+ 11 (* (- symbol 265) 2) (bit-stream-read-bits bit-stream 1)))
    ((<= symbol 272) (+ 19 (* (- symbol 269) 4) (bit-stream-read-bits bit-stream 2)))
    ((<= symbol 276) (+ 35 (* (- symbol 273) 8) (bit-stream-read-bits bit-stream 3)))
    ((<= symbol 280) (+ 67 (* (- symbol 277) 16) (bit-stream-read-bits bit-stream 4)))
    ((<= symbol 284)
     (+ 131 (* (- symbol 281) 32) (bit-stream-read-bits bit-stream 5)))
    ((= symbol 285) 258)
    (t 
     (error 'deflate-decompression-error 
            :format-control "Strange Length Code in bitstream: ~D" 
            :format-arguments (list symbol)))))

(declaim (inline decode-distance-entry))
(defun decode-distance-entry (symbol bit-stream)
  "Decode the given distance symbol into a proper distance specification."
  (cond
    ((<= symbol 3) (1+ symbol))
    (t
     (multiple-value-bind (order offset) (truncate symbol 2)
       (let* ((extra-bits (1- order))
              (factor (ash 1 extra-bits)))
         (+ (1+ (ash 1 order))
            (* offset factor)
            (bit-stream-read-bits bit-stream extra-bits)))))))

(defun decode-huffman-block (bit-stream window-stream 
                             lit-decode-tree dist-decode-tree)
  "Decode the huffman code block using the huffman codes given by 
lit-decode-tree and dist-decode-tree."
  (do ((symbol (read-huffman-code bit-stream lit-decode-tree)
               (read-huffman-code bit-stream lit-decode-tree)))
      ((= symbol 256))
    (cond
      ((<= symbol 255)
       (sliding-window-stream-write-byte window-stream symbol))
      (t
       (let ((length (decode-length-entry symbol bit-stream))
             (distance (decode-distance-entry 
                        (read-huffman-code bit-stream dist-decode-tree) bit-stream)))
         (sliding-window-stream-copy-bytes window-stream distance length))))))

;;;
;;; Block Handling Code
;;;

(defun decode-block (bit-stream window-stream)
  "Decompress a block read from bit-stream into window-stream."
  (let* ((finalp (not (zerop (bit-stream-read-bits bit-stream 1))))
         (type (bit-stream-read-bits bit-stream 2)))
    (ecase type
      (#b00 (bit-stream-copy-block bit-stream window-stream))
      (#b01 
         (decode-huffman-block bit-stream window-stream 
                               *std-lit-decode-tree* 
                               *std-dist-decode-tree*))
      (#b10 
         (multiple-value-bind (lit-decode-tree dist-decode-tree)
             (decode-huffman-tables bit-stream)
           (decode-huffman-block bit-stream window-stream 
                                 lit-decode-tree dist-decode-tree)))
      (#b11 
         (error 'deflate-decompression-error 
                :format-control "Encountered Reserved Block Type ~D!"
                :format-arguments (list type))))
    (not finalp)))

;;;
;;; ZLIB - RFC 1950 handling
;;;

(defun parse-zlib-header (input-stream)
  "Parse a ZLIB-style header as per RFC 1950 from the input-stream and
return the compression-method, compression-level dictionary-id and flags
fields of the header as return values.  Checks the header for corruption
and signals a zlib-decompression-error in case of corruption."
  (let ((compression-method (read-byte input-stream))
        (flags (read-byte input-stream)))
    (unless (zerop (mod (+ (* compression-method 256) flags) 31))
      (error 'zlib-decompression-error
             :format-control "Corrupted Header ~2,'0X,~2,'0X!"
             :format-arguments (list compression-method flags)))
    (let ((dict (unless (zerop (ldb (byte 1 5) flags))
                  (parse-zlib-checksum input-stream))))
      (values (ldb (byte 4 0) compression-method)
              (ldb (byte 4 4) compression-method)
              dict
              (ldb (byte 2 6) flags)))))

(defun parse-zlib-checksum (input-stream)
  (+ (* (read-byte input-stream) 256 256 256)
     (* (read-byte input-stream) 256 256)
     (* (read-byte input-stream) 256)
     (read-byte input-stream)))

(defun parse-zlib-footer (input-stream)
  "Parse the ZLIB-style footer as per RFC 1950 from the input-stream and
return the Adler-32 checksum contained in the footer as its return value."
  (parse-zlib-checksum input-stream))

;;;
;;; GZIP - RFC 1952 handling
;;;

(defconstant +gzip-header-id1+ 31
  "GZIP Header Magic Value ID1 as per RFC 1952.")

(defconstant +gzip-header-id2+ 139
  "GZIP Header Magic Value ID2 as per RFC 1952.")

(defun parse-gzip-header (input-stream)
  "Parse a GZIP-style header as per RFC 1952 from the input-stream and
return the compression-method, text-flag, modification time, XFLAGS,
OS, FEXTRA flags, filename, comment and CRC16 fields of the header as
return values (or nil if any given field is not present).  Checks the
header for magic values and correct flags settings and signals a
gzip-decompression-error in case of incorrect or unsupported magic
values or flags."
  (let ((id1 (read-byte input-stream))
        (id2 (read-byte input-stream))
        (compression-method (read-byte input-stream))
        (flags (read-byte input-stream)))
    (unless (and (= id1 +gzip-header-id1+) (= id2 +gzip-header-id2+))
      (error 'gzip-decompression-error
             :format-control
             "Header missing magic values ~2,'0X,~2,'0X (got ~2,'0X,~2,'0X instead)!"
             :format-arguments (list +gzip-header-id1+ +gzip-header-id2+ id1 id2)))
    (unless (= compression-method 8)
      (error 'gzip-decompression-error
             :format-control "Unknown compression-method in Header ~2,'0X!"
             :format-arguments (list compression-method)))
    (unless (zerop (ldb (byte 3 5) flags))
      (error 'gzip-decompression-error
             :format-control "Unknown flags in Header ~2,'0X!"
             :format-arguments (list flags)))
    (values compression-method
            ;; FTEXT
            (= 1 (ldb (byte 1 0) flags))
            ;; MTIME
            (parse-gzip-mtime input-stream)
            ;; XFLAGS
            (read-byte input-stream)
            ;; OS
            (read-byte input-stream)
            ;; FEXTRA
            (unless (zerop (ldb (byte 1 2) flags))
              (parse-gzip-extra input-stream))
            ;; FNAME
            (unless (zerop (ldb (byte 1 3) flags))
              (parse-gzip-string input-stream))
            ;; FCOMMENT
            (unless (zerop (ldb (byte 1 4) flags))
              (parse-gzip-string input-stream))
            ;; CRC16
            (unless (zerop (ldb (byte 1 1) flags))
              (+ (read-byte input-stream)
                 (* (read-byte input-stream 256)))))))

(defun parse-gzip-mtime (input-stream)
  (let ((time (+ (read-byte input-stream)
                 (* (read-byte input-stream) 256)
                 (* (read-byte input-stream) 256 256)
                 (* (read-byte input-stream) 256 256 256))))
    (if (zerop time)
        nil
        (+ time 2208988800))))

(defun parse-gzip-extra (input-stream)
  (let* ((length (+ (read-byte input-stream) (* (read-byte input-stream) 256)))
         (result (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence result input-stream)
    result))

(defun parse-gzip-string (input-stream)
  (with-output-to-string (string)
    (loop for value = (read-byte input-stream)
          until (zerop value)
          do (write-char (code-char value) string))))
          
(defun parse-gzip-checksum (input-stream)
  (+ (read-byte input-stream)
     (* (read-byte input-stream) 256)
     (* (read-byte input-stream) 256 256)
     (* (read-byte input-stream) 256 256 256)))

(defun parse-gzip-footer (input-stream)
  "Parse the GZIP-style footer as per RFC 1952 from the input-stream and
return the CRC-32 checksum and ISIZE fields contained in the footer as
its return values."
  (values (parse-gzip-checksum input-stream)
          ;; ISIZE
          (+ (read-byte input-stream)
             (* (read-byte input-stream) 256)
             (* (read-byte input-stream) 256 256)
             (* (read-byte input-stream) 256 256 256))))

;;;
;;; Main Entry Points
;;;

(defun inflate-stream (input-stream output-stream &key checksum)
  "Inflate the RFC 1951 data from the given input stream into the
given output stream, which are required to have an element-type
of (unsigned-byte 8).  If checksum is given, it indicates the
checksumming algorithm to employ in calculating a checksum of
the expanded content, which is then returned from this function.
Valid values are :adler-32 for Adler-32 checksum (see RFC 1950),
or :crc-32 for CRC-32 as per ISO 3309 (see RFC 1952, ZIP)."
  (loop with window-stream = (make-sliding-window-stream :stream output-stream
                                  :checksum checksum
                                  :checksum-value
                                  (ecase checksum
                                    ((nil) 0)
                                    (:crc-32 +crc-32-start-value+)
                                    (:adler-32 +adler-32-start-value+)))
        with bit-stream = (make-bit-stream :stream input-stream)
        while (decode-block bit-stream window-stream)
        finally (sliding-window-stream-flush window-stream)
                (when checksum 
                  (return (sliding-window-stream-checksum-value window-stream)))))

(defun inflate-zlib-stream (input-stream output-stream &key check-checksum)
  "Inflate the RFC 1950 zlib data from the given input stream into
the given output stream, which are required to have an element-type
of (unsigned-byte 8).  This returns the Adler-32 checksum of the
file as its first return value, with the compression level as its
second return value.  Note that it is the responsibility of the 
caller to check whether the expanded data matches the Adler-32
checksum, unless the check-checksum keyword argument is set to
true, in which case the checksum is checked internally and a
zlib-decompression-error is signalled if they don't match."
  (multiple-value-bind (cm cinfo dictid flevel) (parse-zlib-header input-stream)
    (unless (= cm 8)
      (error 'zlib-decompression-error
             :format-control "Unknown compression method ~D!"
             :format-arguments (list cm)))
    (unless (<= cinfo 7)
      (error 'zlib-decompression-error
             :format-control "Unsupported sliding window size 2^~D = ~D!"
             :format-arguments (list (+ 8 cinfo) (expt 2 (+ 8 cinfo)))))
    (unless (null dictid)
      (error 'zlib-decompression-error
             :format-control "Unknown preset dictionary id ~8,'0X!"
             :format-arguments (list dictid)))
    (let ((checksum-new (inflate-stream input-stream output-stream 
                                        :checksum (when check-checksum :adler-32)))
          (checksum-old (parse-zlib-footer input-stream)))
      (when (and check-checksum (not (= checksum-old checksum-new)))
        (error 'zlib-decompression-error
               :format-control
               "Checksum mismatch for decompressed stream: ~8,'0X != ~8,'0X!"
               :format-arguments (list checksum-old checksum-new)))
      (values checksum-old flevel))))

(defun inflate-gzip-stream (input-stream output-stream &key check-checksum)
  "Inflate the RFC 1952 gzip data from the given input stream into
the given output stream, which are required to have an element-type
of (unsigned-byte 8).  This returns the CRC-32 checksum of the
file as its first return value, with any filename, modification time,
and comment fields as further return values or nil if not present.
Note that it is the responsibility of the caller to check whether the
expanded data matches the CRC-32 checksum, unless the check-checksum
keyword argument is set to true, in which case the checksum is checked
internally and a gzip-decompression-error is signalled if they don't
match."
  (multiple-value-bind (cm ftext mtime xfl os fextra fname fcomment)
      (parse-gzip-header input-stream)
    (declare (ignore ftext xfl os fextra))
    (unless (= cm 8)
      (error 'gzip-decompression-error
             :format-control "Unknown compression method ~D!"
             :format-arguments (list cm)))
    (let ((checksum-new (inflate-stream input-stream output-stream 
                                        :checksum (when check-checksum :crc-32)))
          (checksum-old (parse-gzip-footer input-stream)))
      ;; Handle Checksums
      (when (and check-checksum (not (= checksum-old checksum-new)))
        (error 'gzip-decompression-error
               :format-control
               "Checksum mismatch for decompressed stream: ~8,'0X != ~8,'0X!"
               :format-arguments (list checksum-old checksum-new)))
      (values checksum-old fname mtime fcomment))))


(defun gunzip (input-file output-file)
  (with-open-file (input input-file
                         :element-type '(unsigned-byte 8))
    (with-open-file (output output-file
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (inflate-gzip-stream input output)))
  (probe-file output-file))
