;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-unicode.lisp --- Unicode encodings.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
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

;;; This implementation is largely based on OpenMCL's l1-unicode.lisp
;;;   Copyright (C) 2006 Clozure Associates and contributors.

(in-package #:babel-encodings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +repl+ #xfffd "Unicode replacement character code point.")
  (defconstant +byte-order-mark-code+ #xfeff)
  (defconstant +swapped-byte-order-mark-code+ #xfffe)
  (defconstant +swapped-byte-order-mark-code-32+ #xfffe0000))

;;; Some convenience macros adding FIXNUM declarations.
(defmacro f-ash (integer count) `(the fixnum (ash ,integer ,count)))
(defmacro f-logior (&rest integers) `(the fixnum (logior ,@integers)))
(defmacro f-logand (&rest integers) `(the fixnum (logand ,@integers)))
(defmacro f-logxor (&rest integers) `(the fixnum (logxor ,@integers)))

;;;; UTF-8

(define-character-encoding :utf-8
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 to 4 bytes."
  :max-units-per-char 4
  :literal-char-code-limit #x80
  :bom-encoding #(#xef #xbb #xbf)
  :default-replacement #xfffd)

(define-condition invalid-utf8-starter-byte (character-decoding-error)
  ()
  (:documentation "Signalled when an invalid UTF-8 starter byte is found."))

(define-condition invalid-utf8-continuation-byte (character-decoding-error)
  ()
  (:documentation
   "Signalled when an invalid UTF-8 continuation byte is found."))

(define-condition overlong-utf8-sequence (character-decoding-error)
  ()
  (:documentation "Signalled upon overlong UTF-8 sequences."))

(define-octet-counter :utf-8 (getter type)
  `(named-lambda utf-8-octet-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with noctets fixnum = 0
           for i fixnum from start below end
           for code of-type code-point = (,getter seq i) do
           (let ((new (+ (cond ((< code #x80) 1)
                               ((< code #x800) 2)
                               ((< code #x10000) 3)
                               (t 4))
                         noctets)))
             (if (and (plusp max) (> new max))
                 (loop-finish)
                 (setq noctets new)))
           finally (return (values noctets i)))))

(define-code-point-counter :utf-8 (getter type)
  `(named-lambda utf-8-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with nchars fixnum = 0
           with i fixnum = start
           while (< i end) do
           ;; check for invalid continuation bytes
           (macrolet ((invalid-cb-p (n)
                        `(and (< (+ i ,n) end)
                              (not (< #x7f (,',getter seq (+ i ,n)) #xc0)))))
             ;; wrote this code with LET instead of FOR because CLISP's
             ;; LOOP doesn't like WHILE clauses before FOR clauses.
             (let* ((octet (,getter seq i))
                    (next-i (+ i (cond ((or (< octet #xc0) (invalid-cb-p 1)) 1)
                                       ((or (< octet #xe0) (invalid-cb-p 2)) 2)
                                       ((or (< octet #xf0) (invalid-cb-p 3)) 3)
                                       ((or (< octet #xf8) (invalid-cb-p 4)) 4)
                                       ((or (< octet #xfc) (invalid-cb-p 5)) 5)
                                       (t 6)))))
               (declare (type ub8 octet) (fixnum next-i))
               (cond
                 ((> next-i end)
                  ;; Should we add restarts to this error, we'll have
                  ;; to figure out a way to communicate with the
                  ;; decoder since we probably want to do something
                  ;; about it right here when we have a chance to
                  ;; change the count or something.  (Like an
                  ;; alternative replacement character or perhaps the
                  ;; existence of this error so that the decoder
                  ;; doesn't have to check for it on every iteration
                  ;; like we do.)
                  ;;
                  ;; FIXME: The data for this error is not right.
                  (decoding-error (vector octet) :utf-8 seq i
                                  nil 'end-of-input-in-character)
                  (return (values (1+ nchars) end)))
                 (t
                  (setq nchars (1+ nchars)
                        i next-i)
                  (when (and (plusp max) (= nchars max))
                    (return (values nchars i)))))))
           finally (progn
                     (assert (= i end))
                     (return (values nchars i))))))

(define-encoder :utf-8 (getter src-type setter dest-type)
  `(named-lambda utf-8-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
           for i fixnum from start below end
           for code of-type code-point = (,getter src i) do
           (macrolet ((set-octet (offset value)
                        `(,',setter ,value dest (the fixnum (+ di ,offset)))))
             (cond
               ;; 1 octet
               ((< code #x80)
                (set-octet 0 code)
                (incf di))
               ;; 2 octets
               ((< code #x800)
                (set-octet 0 (logior #xc0 (f-ash code -6)))
                (set-octet 1 (logior #x80 (f-logand code #x3f)))
                (incf di 2))
               ;; 3 octets
               ((< code #x10000)
                (set-octet 0 (logior #xe0 (f-ash code -12)))
                (set-octet 1 (logior #x80 (f-logand #x3f (f-ash code -6))))
                (set-octet 2 (logior #x80 (f-logand code #x3f)))
                (incf di 3))
               ;; 4 octets
               (t
                (set-octet 0 (logior #xf0 (f-logand #x07 (f-ash code -18))))
                (set-octet 1 (logior #x80 (f-logand #x3f (f-ash code -12))))
                (set-octet 2 (logior #x80 (f-logand #x3f (f-ash code -6))))
                (set-octet 3 (logior #x80 (logand code #x3f)))
                (incf di 4))))
           finally (return (the fixnum (- di d-start))))))

(define-decoder :utf-8 (getter src-type setter dest-type)
  `(named-lambda utf-8-decoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((u2 0) (u3 0) (u4 0) (u5 0) (u6 0))
       (declare (type ub8 u2 u3 u4 u5 u6))
       (loop for di fixnum from d-start
             for i fixnum from start below end
             for u1 of-type ub8 = (,getter src i) do
             ;; Note: CONSUME-OCTET doesn't check if I is being
             ;; incremented past END.  We're assuming that END has
             ;; been calculated with the CODE-POINT-POINTER above that
             ;; checks this.
             (macrolet
                 ((consume-octet ()
                    `(let ((next-i (incf i)))
                       (if (= next-i end)
                           ;; FIXME: data for this error is incomplete.
                           ;; and signalling this error twice
                           (return-from setter-block
                             (decoding-error nil :utf-8 src i +repl+
                                             'end-of-input-in-character))
                           (,',getter src next-i))))
                  (handle-error (n &optional (c 'character-decoding-error))
                    `(decoding-error
                      (vector ,@(subseq '(u1 u2 u3 u4 u5 u6) 0 n))
                      :utf-8 src (1+ (- i ,n)) +repl+ ',c))
                  (handle-error-if-icb (var n)
                    `(when (not (< #x7f ,var #xc0))
                       (decf i)
                       (return-from setter-block
                         (handle-error ,n invalid-utf8-continuation-byte)))))
               (,setter
                (block setter-block
                  (cond
                    ((< u1 #x80) u1)    ; 1 octet
                    ((< u1 #xc0)
                     (handle-error 1 invalid-utf8-starter-byte))
                    (t
                     (setq u2 (consume-octet))
                     (handle-error-if-icb u2 1)
                     (cond
                       ((< u1 #xc2)
                        (handle-error 2 overlong-utf8-sequence))
                       ((< u1 #xe0)     ; 2 octets
                        (logior (f-ash (f-logand #x1f u1) 6)
                                (f-logxor u2 #x80)))
                       (t
                        (setq u3 (consume-octet))
                        (handle-error-if-icb u3 2)
                        (cond
                          ((and (= u1 #xe0) (< u2 #xa0))
                           (handle-error 3 overlong-utf8-sequence))
                          ((< u1 #xf0)  ; 3 octets
                           (let ((start (f-logior (f-ash (f-logand u1 #x0f) 12)
                                                  (f-ash (f-logand u2 #x3f) 6))))
                             (if (<= #xd800 start #xdfc0)
                                 (handle-error 3 character-out-of-range)
                                 (logior start (f-logand u3 #x3f)))))
                          (t            ; 4 octets
                           (setq u4 (consume-octet))
                           (handle-error-if-icb u4 3)
                           (cond
                             ((and (= u1 #xf0) (< u2 #x90))
                              (handle-error 4 overlong-utf8-sequence))
                             ((< u1 #xf8)
                              (if (or (> u1 #xf4) (and (= u1 #xf4) (> u2 #x8f)))
                                  (handle-error 4 character-out-of-range)
                                  (f-logior (f-ash (f-logand u1 7) 18)
                                            (f-ash (f-logxor u2 #x80) 12)
                                            (f-ash (f-logxor u3 #x80) 6)
                                            (f-logxor u4 #x80))))
                             ;; from here on we'll be getting either
                             ;; invalid continuation bytes or overlong
                             ;; 5-byte or 6-byte sequences.
                             (t
                              (setq u5 (consume-octet))
                              (handle-error-if-icb u5 4)
                              (cond
                                ((and (= u1 #xf8) (< u2 #x88))
                                 (handle-error 5 overlong-utf8-sequence))
                                ((< u1 #xfc)
                                 (handle-error 5 character-out-of-range))
                                (t
                                 (setq u6 (consume-octet))
                                 (handle-error-if-icb u6 5)
                                 (cond
                                   ((and (= u1 #xfc) (< u2 #x84))
                                    (handle-error 6 overlong-utf8-sequence))
                                   (t
                                    (handle-error 6 character-out-of-range)
                                    )))))))))))))
                dest di))
             finally (return (the fixnum (- di d-start)))))))

;;;; UTF-8B

;;; The following excerpt from a linux-utf8 message by Markus Kuhn is
;;; the closest thing to a UTF-8B specification:
;;;
;;; <http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html>
;;;
;;; "D) Emit a malformed UTF-16 sequence for every byte in a malformed
;;;     UTF-8 sequence
;;;
;;;  All the previous options for converting malformed UTF-8 sequences
;;;  to UTF-16 destroy information. This can be highly undesirable in
;;;  applications such as text file editors, where guaranteed binary
;;;  transparency is a desireable feature. (E.g., I frequently edit
;;;  executable code or graphic files with the Emacs text editor and I
;;;  hate the idea that my editor might automatically make U+FFFD
;;;  substitutions at locations that I haven't even edited when I save
;;;  the file again.)
;;;
;;;  I therefore suggested 1999-11-02 on the unicode@xxxxxxxxxxx
;;;  mailing list the following approach. Instead of using U+FFFD,
;;;  simply encode malformed UTF-8 sequences as malformed UTF-16
;;;  sequences. Malformed UTF-8 sequences consist excludively of the
;;;  bytes 0x80 - 0xff, and each of these bytes can be represented
;;;  using a 16-bit value from the UTF-16 low-half surrogate zone
;;;  U+DC80 to U+DCFF. Thus, the overlong "K" (U+004B) 0xc1 0x8b from
;;;  the above example would be represented in UTF-16 as U+DCC1
;;;  U+DC8B. If we simply make sure that every UTF-8 encoded surrogate
;;;  character is also treated like a malformed sequence, then there
;;;  is no way that a single high-half surrogate could precede the
;;;  encoded malformed sequence and cause a valid UTF-16 sequence to
;;;  emerge.
;;;
;;;  This way 100% binary transparent UTF-8 -> UTF-16 -> UTF-8
;;;  round-trip compatibility can be achieved quite easily.
;;;
;;;  On an output device, a lonely low-half surrogate character should
;;;  be treated just like a character outside the adopted subset of
;;;  representable characters, that is for the end user, the display
;;;  would look exactly like with semantics B), i.e. one symbol per
;;;  byte of a malformed sequence. However in contrast to semantics
;;;  B), no information is thrown away, and a cut&paste in an editor
;;;  or terminal emulator will be guaranteed to reconstruct the
;;;  original byte sequence. This should greatly reduce the incidence
;;;  of accidental corruption of binary data by UTF-8 -> UTF-16 ->
;;;  UTF-8 conversion round trips."

(define-character-encoding :utf-8b
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 to 4 bytes.  Invalid UTF-8 sequences are encoded with #xDCXX
code points for each invalid byte."
  :max-units-per-char 4
  :literal-char-code-limit #x80
  :bom-encoding #(#xef #xbb #xbf)
  :default-replacement nil)

;;; TODO: reuse the :UTF-8 octet counter through a simple macro.
(define-octet-counter :utf-8b (getter type)
  `(named-lambda utf-8b-octet-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with noctets fixnum = 0
           for i fixnum from start below end
           for code of-type code-point = (,getter seq i) do
           (let ((new (+ (cond ((< code #x80) 1)
                               ((< code #x800) 2)
                               ((<= #xdc80 code #xdcff) 1)
                               ((< code #x10000) 3)
                               (t 4))
                         noctets)))
             (if (and (plusp max) (> new max))
                 (loop-finish)
                 (setq noctets new)))
           finally (return (values noctets i)))))

(define-code-point-counter :utf-8b (getter type)
  `(named-lambda utf-8b-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with nchars fixnum = 0
           with i fixnum = start
           while (< i end) do
           ;; wrote this code with LET instead of FOR because CLISP's
           ;; LOOP doesn't like WHILE clauses before FOR clauses.
           (let* ((octet (,getter seq i))
                  (noctets (cond ((< octet #x80) 1)
                                 ((< octet #xe0) 2)
                                 ((< octet #xf0) 3)
                                 (t 4))))
             (declare (type ub8 octet) (fixnum noctets))
             (cond
               ((> (+ i noctets) end)
                ;; If this error is suppressed these last few bytes
                ;; will be encoded as raw bytes later.
                (decoding-error (vector octet) :utf-8 seq i
                                nil 'end-of-input-in-character)
                (return (values (+ nchars (- end i)) end)))
               (t
                ;; FIXME: clean this mess up.
                (let* ((u1 octet)
                       (u2 (if (>= noctets 2) (,getter seq (1+ i)) 0))
                       (u3 (if (>= noctets 3) (,getter seq (+ i 2)) 0))
                       (u4 (if (= noctets 4) (,getter seq (+ i 3)) 0))
                       (inc (or (and (> noctets 1)
                                     (< u1 #xc2))
                                (and (= noctets 2)
                                     (not (logior u2 #x40)))
                                (and (= noctets 3)
                                     (not (and (< (f-logxor u2 #x80) #x40)
                                               (< (f-logxor u3 #x80) #x40)
                                               (or (>= u1 #xe1) (>= u2 #xa0))
                                               (or (/= u1 #xed) (< u2 #xa0) (> u2 #xbf)))))
                                (and (= noctets 4)
                                     (not
                                      (and (< (f-logxor u2 #x80) #x40)
                                           (< (f-logxor u3 #x80) #x40)
                                           (< (f-logxor u4 #x80) #x40)
                                           (or (>= u1 #xf1) (>= u2 #x90))))))))
                  (let ((new-nchars (if inc (+ nchars noctets) (1+ nchars))))
                    (when (and (plusp max) (> new-nchars max))
                      (return (values nchars i)))
                    (incf i noctets)
                    (setq nchars new-nchars))))))
           finally (progn
                     (assert (= i end))
                     (return (values nchars i))))))

;;; TODO: reuse the :UTF-8 encoder with through a simple macro.
(define-encoder :utf-8b (getter src-type setter dest-type)
  `(named-lambda utf-8b-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
           for i fixnum from start below end
           for code of-type code-point = (,getter src i) do
           (macrolet ((set-octet (offset value)
                        `(,',setter ,value dest (the fixnum (+ di ,offset)))))
             (cond
               ;; 1 octet
               ((< code #x80)
                (set-octet 0 code)
                (incf di))
               ;; 2 octets
               ((< code #x800)
                (set-octet 0 (logior #xc0 (f-ash code -6)))
                (set-octet 1 (logior #x80 (f-logand code #x3f)))
                (incf di 2))
               ;; 1 octet (invalid octet)
               ((<= #xdc80 code #xdcff)
                (set-octet 0 (f-logand code #xff))
                (incf di))
               ;; 3 octets
               ((< code #x10000)
                (set-octet 0 (logior #xe0 (f-ash code -12)))
                (set-octet 1 (logior #x80 (f-logand #x3f (f-ash code -6))))
                (set-octet 2 (logior #x80 (f-logand code #x3f)))
                (incf di 3))
               ;; 4 octets
               (t
                (set-octet 0 (logior #xf0 (f-logand #x07 (f-ash code -18))))
                (set-octet 1 (logior #x80 (f-logand #x3f (f-ash code -12))))
                (set-octet 2 (logior #x80 (f-logand #x3f (f-ash code -6))))
                (set-octet 3 (logand #x3f code))
                (incf di 4))))
           finally (return (the fixnum (- di d-start))))))

(define-decoder :utf-8b (getter src-type setter dest-type)
  `(named-lambda utf-8b-decoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((u2 0) (u3 0) (u4 0))
       (declare (type ub8 u2 u3 u4))
       (loop for di fixnum from d-start
             for i fixnum from start below end
             for u1 of-type ub8 = (,getter src i) do
             ;; Unlike the UTF-8 version, this version of
             ;; CONSUME-OCTET needs to check if I is being incremented
             ;; past END because we might have trailing binary
             ;; garbage.
             (macrolet
                 ((consume-octet (n)
                    `(if (= i (1- end))
                         (encode-raw-octets ,n)
                         (,',getter src (incf i))))
                  (encode-raw-octets (n)
                    `(progn
                       ,@(loop for i below n and var in '(u1 u2 u3 u4)
                               collect `(,',setter (logior #xdc00 ,var) dest di)
                               unless (= i (1- n))
                               collect '(incf di))
                       (return-from set-body))))
               (block set-body
                 (,setter (cond
                            ((< u1 #x80) ; 1 octet
                             u1)
                            ((>= u1 #xc2)
                             (setq u2 (consume-octet 1))
                             (cond
                               ((< u1 #xe0) ; 2 octets
                                (if (< (f-logxor u2 #x80) #x40)
                                    (logior (f-ash (f-logand #x1f u1) 6)
                                            (f-logxor u2 #x80))
                                    (encode-raw-octets 2)))
                               (t
                                (setq u3 (consume-octet 2))
                                (cond
                                  ((< u1 #xf0) ; 3 octets
                                   (if (and (< (f-logxor u2 #x80) #x40)
                                            (< (f-logxor u3 #x80) #x40)
                                            (or (>= u1 #xe1) (>= u2 #xa0)))
                                       (let ((start (f-logior (f-ash (f-logand u1 #x0f) 12)
                                                              (f-ash (f-logand u2 #x3f) 6))))
                                         (if (<= #xd800 start #xdfc0)
                                             (encode-raw-octets 3)
                                             (logior start (f-logand u3 #x3f))))
                                       (encode-raw-octets 3)))
                                  (t    ; 4 octets
                                   (setq u4 (consume-octet 3))
                                   (if (and (< (f-logxor u2 #x80) #x40)
                                            (< (f-logxor u3 #x80) #x40)
                                            (< (f-logxor u4 #x80) #x40)
                                            (or (>= u1 #xf1) (>= u2 #x90)))
                                       (logior
                                        (f-logior (f-ash (f-logand u1 7) 18)
                                                  (f-ash (f-logxor u2 #x80) 12))
                                        (f-logior (f-ash (f-logxor u3 #x80) 6)
                                                  (f-logxor u4 #x80)))
                                       (encode-raw-octets 4)))))))
                            (t (encode-raw-octets 1)))
                          dest di)))
             finally (return (the fixnum (- di d-start)))))))

;;;; UTF-16

;;; TODO: add a way to pass some info at compile-time telling us that,
;;; for example, the maximum code-point will always be < #x10000 in
;;; which case we could simply return (* 2 (- end start)).
(defmacro utf16-octet-counter (getter type)
  `(named-lambda utf-16-octet-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with noctets fixnum = 0
           for i fixnum from start below end
           for code of-type code-point = (,getter seq i)
           do (let ((new (the fixnum (+ (if (< code #x10000) 2 4) noctets))))
                (if (and (plusp max) (> new max))
                    (loop-finish)
                    (setq noctets new)))
           finally (return (values noctets i)))))

(defmacro utf-16-combine-surrogate-pairs (u1 u2)
  `(the (unsigned-byte 21)
     (+ #x10000
        (the (unsigned-byte 20)
          (logior
           (the (unsigned-byte 20)
             (ash (the (unsigned-byte 10) (- ,u1 #xd800)) 10))
           (the (unsigned-byte 10)
             (- ,u2 #xdc00)))))))

(defmacro define-utf-16 (name &optional endianness)
  (check-type endianness (or null (eql :be) (eql :le)))
  (check-type name keyword)
  (let ((swap-var (gensym "SWAP"))
        (code-point-counter-name
          (format-symbol t '#:~a-code-point-counter (string name)))
        (encoder-name (format-symbol t '#:~a-encoder (string name)))
        (decoder-name (format-symbol t '#:~a-decoder (string name))))
    (labels ((make-bom-check-form (end start getter seq)
               (if (null endianness)
                 ``((,',swap-var
                      (when (> ,,end ,,start)
                        (case (,,getter ,,seq ,,start 2 :ne)
                          (#.+byte-order-mark-code+ (incf ,,start 2) nil)
                          (#.+swapped-byte-order-mark-code+ (incf ,,start 2) t)
                          (t #+little-endian t)))))
                 '()))
             (make-getter-form (getter src i)
               (case endianness
                 (:le ``(,,getter ,,src ,,i 2 :le))
                 (:be ``(,,getter ,,src ,,i 2 :be))
                 (t ``(if ,',swap-var
                        (,,getter ,,src ,,i 2 :re)
                        (,,getter ,,src ,,i 2 :ne)))))
             (make-setter-form (setter code dest di)
               (case endianness
                 (:be ``(,,setter ,,code ,,dest ,,di 2 :be))
                 (:le ``(,,setter ,,code ,,dest ,,di 2 :le))
                 (t ``(,,setter ,,code ,,dest ,,di 2 :ne)))))
      `(progn
         (define-octet-counter ,name (getter type)
           `(utf16-octet-counter ,getter ,type))
         (define-code-point-counter ,name (getter type)
           `(named-lambda ,',code-point-counter-name (seq start end max)
              (declare (type ,type seq) (fixnum start end max))
              (let* ,,(make-bom-check-form ''end ''start 'getter ''seq)
                (loop with count fixnum = 0
                  with i fixnum = start
                  while (<= i (- end 2)) do
                  (let* ((code ,,(make-getter-form 'getter ''seq ''i))
                         (next-i (+ i (if (or (< code #xd800) (>= code #xdc00))
                                        2
                                        4))))
                    (declare (type (unsigned-byte 16) code) (fixnum next-i))
                    (cond
                      ((> next-i end)
                       (decoding-error
                         (vector (,getter seq i) (,getter seq (1+ i)))
                         ,',name seq i nil 'end-of-input-in-character)
                       (return (values count i)))
                      (t
                        (setq i next-i
                              count (1+ count))
                        (when (and (plusp max) (= count max))
                          (return (values count i))))))
                  finally (progn
                            (assert (= i end))
                            (return (values count i)))))))
         (define-encoder ,name (getter src-type setter dest-type)
           `(named-lambda ,',encoder-name (src start end dest d-start)
              (declare (type ,src-type src)
                       (type ,dest-type dest)
                       (fixnum start end d-start))
              (loop with di fixnum = d-start
                for i fixnum from start below end
                for code of-type code-point = (,getter src i)
                for high-bits fixnum = (- code #x10000) do
                (cond ((< high-bits 0)
                       ,,(make-setter-form 'setter ''code ''dest ''di)
                       (incf di 2))
                      (t
                        ,,(make-setter-form
                              'setter ''(logior #xd800 (f-ash high-bits -10))
                            ''dest ''di)
                        ,,(make-setter-form
                              'setter ''(logior #xdc00 (f-logand high-bits #x3ff))
                            ''dest ''(+ di 2))
                        (incf di 4)))
                finally (return (the fixnum (- di d-start))))))
         (define-decoder ,name (getter src-type setter dest-type)
           `(named-lambda ,',decoder-name (src start end dest d-start)
              (declare (type ,src-type src)
                       (type ,dest-type dest)
                       (fixnum start end d-start))
              (let ,,(make-bom-check-form ''end ''start 'getter ''src)
                (loop with i fixnum = start
                  for di fixnum from d-start
                  until (= i end) do
                  (let ((u1 ,,(make-getter-form 'getter ''src ''i)))
                    (declare (type (unsigned-byte 16) u1))
                    (incf i 2)
                    (,setter (cond
                               ((or (< u1 #xd800) (>= u1 #xe000)) ; 2 octets
                                u1)
                               ((< u1 #xdc00) ; 4 octets
                                (let ((u2 ,,(make-getter-form 'getter ''src ''i)))
                                  (declare (type (unsigned-byte 16) u2))
                                  (incf i 2)
                                  (if (and (>= u2 #xdc00) (< u2 #xe000))
                                    (utf-16-combine-surrogate-pairs u1 u2)
                                    (decoding-error
                                      (vector (,getter src (- i 4))
                                              (,getter src (- i 3))
                                              (,getter src (- i 2))
                                              (,getter src (- i 1)))
                                      ,',name src i +repl+))))
                               (t
                                 (decoding-error (vector (,getter src (- i 2))
                                                         (,getter src (- i 1)))
                                                 ,',name src i +repl+)))
                             dest di))
                  finally (return (the fixnum (- di d-start)))))))
         ',name))))

(define-character-encoding :utf-16
    "A 16-bit, variable-length encoding in which characters with
code points less than #x10000 can be encoded in a single 16-bit
word and characters with larger codes can be encoded in a pair of
16-bit words.  The endianness of the encoded data is indicated by
the endianness of a byte-order-mark character (#\u+feff)
prepended to the data; in the absence of such a character on
input, the data is assumed to be in big-endian order.  Output is
written in native byte-order with a leading byte-order mark."
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness t            ; not necessarily true when decoding
  :decode-literal-code-unit-limit #xd800
  :encode-literal-code-unit-limit #x10000
  :use-bom #+big-endian :utf-16be #+little-endian :utf-16le
  :bom-encoding #+big-endian #(#xfe #xff) #+little-endian #(#xff #xfe)
  :nul-encoding #(0 0)
  :default-replacement #xfffd
  :ambiguous #+little-endian t #+big-endian nil)

(define-utf-16 :utf-16)

(define-character-encoding :utf-16le
    "A 16-bit, variable-length encoding in which characters with
code points less than #x10000 can be encoded in a single 16-bit
word and characters with larger codes can be encoded in a pair of
16-bit words.  The data is assumed to be in little-endian order.  Output is
written in little-endian byte-order without a leading byte-order mark."
  :aliases '(:utf-16/le)
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness #+little-endian t #+big-endian nil
  :decode-literal-code-unit-limit #xd800
  :encode-literal-code-unit-limit #x10000
  :nul-encoding #(0 0)
  :default-replacement #xfffd)

(define-utf-16 :utf-16le :le)

(define-character-encoding :utf-16be
    "A 16-bit, variable-length encoding in which characters with
code points less than #x10000 can be encoded in a single 16-bit
word and characters with larger codes can be encoded in a pair of
16-bit words.  The data is assumed to be in big-endian order.  Output is
written in big-endian byte-order without a leading byte-order mark."
  :aliases '(:utf-16/be)
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness #+little-endian nil #+big-endian t
  :decode-literal-code-unit-limit #xd800
  :encode-literal-code-unit-limit #x10000
  :nul-encoding #(0 0)
  :default-replacement #xfffd)

(define-utf-16 :utf-16be :be)

(defmacro define-ucs (name bytes &optional endianness (limit #x110000))
  (check-type name keyword)
  (check-type bytes (or (eql 2) (eql 4)))
  (check-type endianness (or null (eql :le) (eql :be)))
  (let ((swap-var (gensym "SWAP"))
        (code-point-counter-name
          (format-symbol t '#:~a-code-point-counter (string name)))
        (encoder-name
          (format-symbol t '#:~a-encoder (string name)))
        (decoder-name
          (format-symbol t '#:~a-decoder (string name))))
    (labels ((make-bom-check-form (end start getter src)
               (if (null endianness)
                   ``(when (not (zerop (- ,,end ,,start)))
                       (case (,,getter ,,src 0 ,',bytes :ne)
                         (#.+byte-order-mark-code+
                          (incf ,,start ,',bytes) nil)
                         (#.+swapped-byte-order-mark-code-32+
                          (incf ,,start ,',bytes) t)
                         (t #+little-endian t)))
                   '()))
             (make-setter-form (setter code dest di)
               ``(,,setter ,,code ,,dest ,,di ,',bytes
                           ,',(or endianness :ne)))
             (make-getter-form (getter src i)
               (if (null endianness)
                   ``(if ,',swap-var
                         (,,getter ,,src ,,i ,',bytes :re)
                         (,,getter ,,src ,,i ,',bytes :ne))
                   ``(,,getter ,,src ,,i ,',bytes ,',endianness))))
      `(progn
         (define-code-point-counter ,name (getter type)
           `(named-lambda ,',code-point-counter-name (seq start end max)
              (declare (type ,type seq) (fixnum start end max))
              ;; check for bom
              ,,(make-bom-check-form ''end ''start 'getter ''seq)
              (multiple-value-bind (count rem)
                  (floor (- end start) ,',bytes)
                (cond
                  ((and (plusp max) (> count max))
                   (values max (the fixnum (+ start (* ,',bytes max)))))
                  (t
                   ;; check for incomplete last character
                   (unless (zerop rem)
                     (let ((vector (make-array ,',bytes :fill-pointer 0)))
                       (dotimes (i rem)
                         (vector-push (,getter seq (+ i (- end rem))) vector))
                       (decoding-error vector ,',name seq (the fixnum (- end rem)) nil
                                       'end-of-input-in-character)
                       (decf end rem)))
                   (values count end))))))
         (define-encoder ,name (getter src-type setter dest-type)
           `(named-lambda ,',encoder-name (src start end dest d-start)
              (declare (type ,src-type src)
                       (type ,dest-type dest)
                       (fixnum start end d-start))
              (loop for i fixnum from start below end
                    and di fixnum from d-start by ,',bytes
                    for code of-type code-point = (,getter src i)
                    do (if (>= code ,',limit)
                           (encoding-error code ,',name src i +repl+)
                           ,,(make-setter-form 'setter ''code ''dest ''di))
                    finally (return (the fixnum (- di d-start))))))
         (define-decoder ,name (getter src-type setter dest-type)
           `(named-lambda ,',decoder-name (src start end dest d-start)
              (declare (type ,src-type src)
                       (type ,dest-type dest)
                       (fixnum start end d-start))
              (let ((,',swap-var ,,(make-bom-check-form ''end ''start 'getter ''src)))
                (declare (ignorable ,',swap-var))
                (loop for i fixnum from start below end by ,',bytes
                      and di from d-start
                      do (,setter (let ((unit ,,(make-getter-form 'getter ''src ''i)))
                                    (if (>= unit ,',limit)
                                        (decoding-error
                                         (vector (,getter src i)
                                                 (,getter src (+ i 1))
                                                 ,@,(if (= bytes 4)
                                                        ``((,getter src (+ i 2))
                                                           (,getter src (+ i 3)))))
                                         ,',name src i +repl+
                                         'character-out-of-range)
                                        unit))
                                  dest di)
                      finally (return (the fixnum (- di d-start)))))))
         ',name))))

;;;; UTF-32

(define-character-encoding :utf-32
   "A 32-bit, fixed-length encoding in which all Unicode
characters can be encoded in a single 32-bit word.  The
endianness of the encoded data is indicated by the endianness of
a byte-order-mark character (#\u+feff) prepended to the data; in
the absence of such a character on input, input data is assumed
to be in big-endian order.  Output is written in native byte
order with a leading byte-order mark."
  :aliases '(:ucs-4)
  :max-units-per-char 1
  :code-unit-size 32
  :native-endianness t ; not necessarily true when decoding
  :literal-char-code-limit #x110000
  :use-bom #+little-endian :utf-32le #+big-endian :utf-32be
  :bom-encoding
  #+big-endian #(#x00 #x00 #xfe #xff)
  #+little-endian #(#xff #xfe #x00 #x00)
  :nul-encoding #(0 0 0 0)
  :ambiguous #+little-endian t #+big-endian nil)

(define-ucs :utf-32 4)

(define-character-encoding :utf-32le
   "A 32-bit, fixed-length encoding in which all Unicode
characters can be encoded in a single 32-bit word. Input data is assumed
to be in little-endian order.  Output is also written in little-endian byte
order without a leading byte-order mark."
  :max-units-per-char 1
  :code-unit-size 32
  :aliases '(:utf-32/le :ucs-4le :ucs-4/le)
  :native-endianness #+little-endian t #+big-endian nil
  :literal-char-code-limit #x110000
  :nul-encoding #(0 0 0 0))

(define-ucs :utf-32le 4 :le)

(define-character-encoding :utf-32be
   "A 32-bit, fixed-length encoding in which all Unicode
characters can be encoded in a single 32-bit word. Input data is assumed
to be in big-endian order.  Output is also written in big-endian byte
order without a leading byte-order mark."
  :max-units-per-char 1
  :code-unit-size 32
  :aliases '(:utf-32/be :ucs-4be :ucs-4/be)
  :native-endianness #+little-endian nil #+big-endian t
  :literal-char-code-limit #x110000
  :nul-encoding #(0 0 0 0))

(define-ucs :utf-32be 4 :be)

;; UCS-2

(define-character-encoding :ucs-2
   "A 16-bit, fixed-length encoding in which all Unicode
characters can be encoded in a single 16-bit word.  The
endianness of the encoded data is indicated by the endianness of
a byte-order-mark character (#\u+feff) prepended to the data; in
the absence of such a character on input, input data is assumed
to be in big-endian order.  Output is written in native byte
order with a leading byte-order mark."
  :aliases '(:ucs-2)
  :max-units-per-char 1
  :code-unit-size 16
  :native-endianness t ; not necessarily true when decoding
  :literal-char-code-limit #x10000
  :use-bom #+little-endian :ucs-2le #+big-endian :ucs-2be
  :bom-encoding
  #+big-endian #(#xfe #xff)
  #+little-endian #(#xff #xfe)
  :nul-encoding #(0 0)
  :ambiguous #+little-endian t #+big-endian nil)

(define-ucs :ucs-2 2 nil #x10000)

(define-character-encoding :ucs-2le
   "A 16-bit, fixed-length encoding in which all Unicode
characters can be encoded in a single 16-bit word. Input data is assumed
to be in little-endian order.  Output is also written in little-endian byte
order without a leading byte-order mark."
  :max-units-per-char 1
  :code-unit-size 16
  :aliases '(:ucs-2/le)
  :native-endianness #+little-endian t #+big-endian nil
  :literal-char-code-limit #x10000
  :nul-encoding #(0 0))

(define-ucs :ucs-2le 2 :le #x10000)

(define-character-encoding :ucs-2be
   "A 16-bit, fixed-length encoding in which all Unicode
characters can be encoded in a single 16-bit word. Input data is assumed
to be in big-endian order.  Output is also written in big-endian byte
order without a leading byte-order mark."
  :max-units-per-char 1
  :code-unit-size 16
  :aliases '(:ucs-2/be)
  :native-endianness #+little-endian nil #+big-endian t
  :literal-char-code-limit #x10000
  :nul-encoding #(0 0))

(define-ucs :ucs-2be 2 :be #x10000)
