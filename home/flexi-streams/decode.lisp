;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/decode.lisp,v 1.35 2008/08/26 10:59:22 edi Exp $

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

(defun recover-from-encoding-error (external-format format-control &rest format-args)
  "Helper function used by OCTETS-TO-CHAR-CODE below to deal with
encoding errors.  Checks if *SUBSTITUTION-CHAR* is not NIL and returns
its character code in this case.  Otherwise signals an
EXTERNAL-FORMAT-ENCODING-ERROR as determined by the arguments to this
function and provides a corresponding USE-VALUE restart."
  (when *substitution-char*
    (return-from recover-from-encoding-error (char-code *substitution-char*)))
  (restart-case
      (apply #'signal-encoding-error external-format format-control format-args)
    (use-value (char)
      :report "Specify a character to be used instead."
      :interactive (lambda ()
                     (loop
                      (format *query-io* "Type a character: ")
                      (let ((line (read-line *query-io*)))
                        (when (= 1 (length line))
                          (return (list (char line 0)))))))
      (char-code char))))

(defgeneric octets-to-char-code (format reader)
  (declare #.*standard-optimize-settings*)
  (:documentation "Converts a sequence of octets to a character code
\(which is returned, or NIL in case of EOF) using the external format
FORMAT.  The sequence is obtained by calling the function \(which must
be a functional object) READER with no arguments which should return
one octet per call.  In the case of EOF, READER should return NIL.

The special variable *CURRENT-UNREADER* must be bound correctly
whenever this function is called."))

(defgeneric octets-to-string* (format sequence start end)
  (declare #.*standard-optimize-settings*)
  (:documentation "A generic function which dispatches on the external
format and does the real work for OCTETS-TO-STRING."))

(defmethod octets-to-string* :around (format (list list) start end)
  (declare #.*standard-optimize-settings*)
  (octets-to-string* format (coerce list 'vector) start end))

(defmacro define-sequence-readers ((format-class) &body body)
  "Non-hygienic utility macro which defines methods for READ-SEQUENCE*
and OCTETS-TO-STRING* for the class FORMAT-CLASS.  BODY is described
in the docstring of DEFINE-CHAR-ENCODERS but can additionally contain
a form \(UNGET <form>) which has to be replaced by the correct code to
`unread' the octets for the character designated by <form>."
  (let* ((body `((block char-decoder
                   (locally
                     (declare #.*fixnum-optimize-settings*)
                     ,@body)))))
    `(progn
       (defmethod read-sequence* ((format ,format-class) flexi-input-stream sequence start end)
         (with-accessors ((position flexi-stream-position)
                          (bound flexi-stream-bound)
                          (octet-stack flexi-stream-octet-stack)
                          (last-octet flexi-stream-last-octet)
                          (last-char-code flexi-stream-last-char-code)
                          (stream flexi-stream-stream))
             flexi-input-stream
           (let* (buffer
                  (buffer-pos 0)
                  (buffer-end 0)
                  (index start)
                  donep
                  ;; whether we will later be able to rewind the stream if
                  ;; needed (to get rid of unused octets in the buffer)
                  (can-rewind-p (maybe-rewind stream 0))
                  (factor (encoding-factor format))
                  (integer-factor (floor factor))
                  ;; it's an interesting question whether it makes sense
                  ;; performance-wise to make RESERVE significantly bigger
                  ;; (and thus put potentially a lot more octets into
                  ;; OCTET-STACK), especially for UTF-8
                  (reserve (cond ((or (not (floatp factor))
                                      (not can-rewind-p)) 0)
                                 (t (ceiling (* (- factor integer-factor) (- end start)))))))
             (declare (fixnum buffer-pos buffer-end index integer-factor reserve)
                      (boolean can-rewind-p))
             (flet ((compute-fill-amount ()
                      "Computes the amount of octets we can savely read into
the buffer without violating the stream's bound \(if there is one) and
without potentially reading much more than we need \(unless we can
rewind afterwards)."
                      (let ((minimum (min (the fixnum (+ (the fixnum (* integer-factor
                                                                        (the fixnum (- end index))))
                                                         reserve))
                                          +buffer-size+)))
                        (cond (bound (min minimum (- bound position)))
                              (t minimum))))
                    (fill-buffer (end)
                      "Tries to fill the buffer from BUFFER-POS to END and
returns NIL if the buffer doesn't contain any new data."
                      (when donep
                        (return-from fill-buffer nil))
                      ;; put data from octet stack into buffer if there is any
                      (loop
                       (when (>= buffer-pos end)
                         (return))
                       (let ((next-octet (pop octet-stack)))
                         (cond (next-octet
                                (setf (aref (the (array octet *) buffer) buffer-pos) (the octet next-octet))
                                (incf buffer-pos))
                               (t (return)))))
                      (setq buffer-end (read-sequence buffer stream
                                                      :start buffer-pos
                                                      :end end))
                      ;; we reached EOF, so we remember this
                      (when (< buffer-end end)
                        (setq donep t))
                      ;; BUFFER-POS is only greater than zero if the buffer
                      ;; already contains unread data from the octet stack
                      ;; (see below), so we test for ZEROP here and do /not/
                      ;; compare with BUFFER-POS
                      (unless (zerop buffer-end)
                        (incf position buffer-end))))
               (let ((minimum (compute-fill-amount)))
                 (declare (fixnum minimum))
                 (setq buffer (make-octet-buffer minimum))
                 ;; fill buffer for the first time or return immediately if
                 ;; we don't succeed
                 (unless (fill-buffer minimum)
                   (return-from read-sequence* start)))
               (setq buffer-pos 0)
               (macrolet ((iterate (set-place)
                            "A very unhygienic macro to implement the
actual iteration through the sequence including housekeeping for the
flexi stream.  SET-PLACE is the place \(using the index INDEX) used to
access the sequence."
                            `(flet ((leave ()
                                      "This is the function used to
abort the LOOP iteration below."
                                      (when (> index start)
                                        (setq last-octet nil
                                              last-char-code ,(sublis '((index . (1- index))) set-place)))
                                      (return-from read-sequence* index)))
                               (loop
                                (when (>= index end)
                                  ;; check if there are octets in the
                                  ;; buffer we didn't use - see
                                  ;; COMPUTE-FILL-AMOUNT above
                                  (let ((rest (- buffer-end buffer-pos)))
                                    (when (plusp rest)
                                      (or (and can-rewind-p
                                               (maybe-rewind stream rest))
                                          (loop
                                           (when (>= buffer-pos buffer-end)
                                             (return))
                                           (decf buffer-end)
                                           (push (aref (the (array octet *) buffer) buffer-end)
                                                 octet-stack)))))
                                  (leave))
                                (let ((next-char-code
                                       (progn (symbol-macrolet
                                                  ((octet-getter
                                                    ;; this is the code to retrieve the next octet (or
                                                    ;; NIL) and to fill the buffer if needed
                                                    (block next-octet
                                                      (when (>= buffer-pos buffer-end)
                                                        (setq buffer-pos 0)
                                                        (unless (fill-buffer (compute-fill-amount))
                                                          (return-from next-octet)))
                                                      (prog1
                                                          (aref (the (array octet *) buffer) buffer-pos)
                                                        (incf buffer-pos)))))
                                                (macrolet ((unget (form)
                                                             `(unread-char% ,form flexi-input-stream)))
                                                  ,',@body)))))
                                  (unless next-char-code
                                    (leave))
                                  (setf ,set-place (code-char next-char-code))
                                  (incf index))))))
                 (etypecase sequence
                   (string (iterate (char sequence index)))
                   (array (iterate (aref sequence index)))
                   (list (iterate (nth index sequence)))))))))
       (defmethod octets-to-string* ((format ,format-class) sequence start end)
         (declare #.*standard-optimize-settings*)
         (declare (fixnum start end))
         (let* ((i start)
                (string-length (compute-number-of-chars format sequence start end))
                (string (make-array string-length :element-type 'char*)))
           (declare (fixnum i string-length))
           (loop for j of-type fixnum from 0 below string-length
                 do (setf (schar string j)
                          (code-char (macrolet ((unget (form)
                                                  `(decf i (character-length format ,form))))
                                       ;; we don't need to test for
                                       ;; the end of SEQUENCE as the
                                       ;; computation has been done
                                       ;; for us already
                                       (symbol-macrolet ((octet-getter (prog1
                                                                           (aref sequence i)
                                                                         (incf i))))
                                         ,@body))))
                 finally (return string)))))))

(defmacro define-char-decoders ((lf-format-class cr-format-class crlf-format-class) &body body)
  "Non-hygienic utility macro which defines several decoding-related
methods for the classes LF-FORMAT-CLASS, CR-FORMAT-CLASS, and
CRLF-FORMAT-CLASS where it is assumed that CR-FORMAT-CLASS is the same
encoding as LF-FORMAT-CLASS but with CR instead of LF line endings and
similar for CRLF-FORMAT-CLASS, i.e. LF-FORMAT-CLASS is the base class.
BODY is a code template for the code to read octets and return one
character code.  BODY must contain a symbol OCTET-GETTER representing
the form which is used to obtain the next octet."
  (let* ((body (with-unique-names (char-code)
                 `((let ((,char-code (progn ,@body)))
                     (when (and ,char-code
                                (or (<= #xd8 (logand* #x00ff (ash* ,char-code -8)) #xdf)
                                    (> ,char-code #x10ffff)))
                       (recover-from-encoding-error format "Illegal code point ~A \(#x~:*~X)." ,char-code))
                     ,char-code)))))
    `(progn
       (defmethod octets-to-char-code ((format ,lf-format-class) reader)
         (declare #.*fixnum-optimize-settings*)
         (declare (function reader))
         (symbol-macrolet ((octet-getter (funcall reader)))
           ,@(sublis '((char-decoder . octets-to-char-code))
                     body)))
       (define-sequence-readers (,lf-format-class) ,@body)
       (define-sequence-readers (,cr-format-class)
         ,(with-unique-names (char-code)
            `(let ((,char-code (progn ,@body)))
               (case ,char-code
                 (#.+cr+ #.(char-code #\Newline))
                 (otherwise ,char-code)))))
       (define-sequence-readers  (,crlf-format-class)
         ,(with-unique-names (char-code next-char-code get-char-code)
            `(flet ((,get-char-code () ,@body))
               (let ((,char-code (,get-char-code)))
                 (case ,char-code
                   (#.+cr+
                    (let ((,next-char-code (,get-char-code)))
                      (case ,next-char-code
                        (#.+lf+ #.(char-code #\Newline))
                        ;; we saw a CR but no LF afterwards, but then the data
                        ;; ended, so we just return #\Return
                        ((nil) +cr+)
                        ;; if the character we peeked at wasn't a
                        ;; linefeed character we unread its constituents
                        (otherwise (unget (code-char ,next-char-code))
                                   ,char-code))))
                   (otherwise ,char-code)))))))))

(define-char-decoders (flexi-latin-1-format flexi-cr-latin-1-format flexi-crlf-latin-1-format)
  octet-getter)

(define-char-decoders (flexi-ascii-format flexi-cr-ascii-format flexi-crlf-ascii-format)
  (when-let (octet octet-getter)
    (if (> (the octet octet) 127)
      (recover-from-encoding-error format
                                   "No character which corresponds to octet #x~X." octet)
      octet)))

(define-char-decoders (flexi-8-bit-format flexi-cr-8-bit-format flexi-crlf-8-bit-format)
  (with-accessors ((decoding-table external-format-decoding-table))
      format
    (when-let (octet octet-getter)
      (let ((char-code (aref (the (simple-array char-code-integer *) decoding-table)
                             (the octet octet))))
        (if (or (null char-code)
                (= (the char-code-integer char-code) 65533))
          (recover-from-encoding-error format
                                       "No character which corresponds to octet #x~X." octet)
          char-code)))))

(define-char-decoders (flexi-utf-8-format flexi-cr-utf-8-format flexi-crlf-utf-8-format)
  (let (first-octet-seen)
    (declare (boolean first-octet-seen))
    (macrolet ((read-next-byte ()
                 '(prog1
                      (or octet-getter
                          (cond (first-octet-seen
                                 (return-from char-decoder
                                   (recover-from-encoding-error format
                                                                "End of data while in UTF-8 sequence.")))
                                (t (return-from char-decoder nil))))
                    (setq first-octet-seen t))))
      (flet ((recover-from-overlong-sequence (value)
               (restart-case
                   (recover-from-encoding-error format "`Overlong' UTF-8 sequence for code point #x~X."
                                                value)                 
                 (accept-overlong-sequence ()
                   :report "Accept the code point and continue."
                   value))))
        (let ((octet (read-next-byte)))
          (declare (type octet octet))
          (block utf-8-sequence
            (multiple-value-bind (start count)
                (cond ((not (logbitp 7 octet))
                       ;; avoid the overlong checks below
                       (return-from utf-8-sequence octet))
                      ((= #b11000000 (logand* octet #b11100000))
                       (values (logand* octet #b00011111) 1))
                      ((= #b11100000 (logand* octet #b11110000))
                       (values (logand* octet #b00001111) 2))
                      ((= #b11110000 (logand* octet #b11111000))
                       (values (logand* octet #b00000111) 3))
                      (t (return-from char-decoder
                           (recover-from-encoding-error format
                                                        "Unexpected value #x~X at start of UTF-8 sequence."
                                                        octet))))
              (declare (fixnum count))
              (loop for result of-type code-point
                    = start then (+ (ash* result 6)
                                    (logand* octet #b111111))
                    repeat count
                    for octet of-type octet = (read-next-byte)
                    unless (= #b10000000 (logand* octet #b11000000))
                    do (return-from char-decoder
                         (recover-from-encoding-error format
                                                      "Unexpected value #x~X in UTF-8 sequence." octet))
                    finally (return (cond ((< result (ecase count
                                                       (1 #x00080)
                                                       (2 #x00800)
                                                       (3 #x10000)))
                                           (recover-from-overlong-sequence result))
                                          (t result)))))))))))

(define-char-decoders (flexi-utf-16-le-format flexi-cr-utf-16-le-format flexi-crlf-utf-16-le-format)
  (let (first-octet-seen)
    (declare (boolean first-octet-seen))
    (macrolet ((read-next-byte ()
                 '(prog1
                      (or octet-getter
                          (cond (first-octet-seen
                                 (return-from char-decoder
                                   (recover-from-encoding-error format
                                                                "End of data while in UTF-16 sequence.")))
                                (t (return-from char-decoder nil))))
                    (setq first-octet-seen t))))
      (flet ((read-next-word ()
               (+ (the octet (read-next-byte))
                  (ash* (the octet (read-next-byte)) 8))))
        (declare (inline read-next-word))
        (let ((word (read-next-word)))
          (declare (type (unsigned-byte 16) word))
          (cond ((<= #xd800 word #xdfff)
                 (let ((next-word (read-next-word)))
                   (declare (type (unsigned-byte 16) next-word))
                   (unless (<= #xdc00 next-word #xdfff)
                     (return-from char-decoder
                       (recover-from-encoding-error format
                                                    "Unexpected UTF-16 word #x~X following #x~X."
                                                    next-word word)))
                   (+ (ash* (logand* #b1111111111 word) 10)
                      (logand* #b1111111111 next-word)
                      #x10000)))
                (t word)))))))

(define-char-decoders (flexi-utf-16-be-format flexi-cr-utf-16-be-format flexi-crlf-utf-16-be-format)
  (let (first-octet-seen)
    (declare (boolean first-octet-seen))
    (macrolet ((read-next-byte ()
                 '(prog1
                      (or octet-getter
                          (cond (first-octet-seen
                                 (return-from char-decoder
                                   (recover-from-encoding-error format
                                                                "End of data while in UTF-16 sequence.")))
                                (t (return-from char-decoder nil))))
                    (setq first-octet-seen t))))
      (flet ((read-next-word ()
               (+ (ash* (the octet (read-next-byte)) 8)
                  (the octet (read-next-byte)))))
        (declare (inline read-next-word))
        (let ((word (read-next-word)))
          (declare (type (unsigned-byte 16) word))
          (cond ((<= #xd800 word #xdfff)
                 (let ((next-word (read-next-word)))
                   (declare (type (unsigned-byte 16) next-word))
                   (unless (<= #xdc00 next-word #xdfff)
                     (return-from char-decoder
                       (recover-from-encoding-error format
                                                    "Unexpected UTF-16 word #x~X following #x~X."
                                                    next-word word)))
                   (+ (ash* (logand* #b1111111111 word) 10)
                      (logand* #b1111111111 next-word)
                      #x10000)))
                (t word)))))))

(define-char-decoders (flexi-utf-32-le-format flexi-cr-utf-32-le-format flexi-crlf-utf-32-le-format)
  (let (first-octet-seen)
    (declare (boolean first-octet-seen))
    (macrolet ((read-next-byte ()
                 '(prog1
                      (or octet-getter
                          (cond (first-octet-seen
                                 (return-from char-decoder
                                   (recover-from-encoding-error format
                                                                "End of data while in UTF-32 sequence.")))
                                (t (return-from char-decoder nil))))
                    (setq first-octet-seen t))))
      (loop for count of-type fixnum from 0 to 24 by 8
            for octet of-type octet = (read-next-byte)
            sum (ash* octet count)))))

(define-char-decoders (flexi-utf-32-be-format flexi-cr-utf-32-be-format flexi-crlf-utf-32-be-format)
  (let (first-octet-seen)
    (declare (boolean first-octet-seen))
    (macrolet ((read-next-byte ()
                 '(prog1
                      (or octet-getter
                          (cond (first-octet-seen
                                 (return-from char-decoder
                                   (recover-from-encoding-error format
                                                                "End of data while in UTF-32 sequence.")))
                                (t (return-from char-decoder nil))))
                    (setq first-octet-seen t))))
      (loop for count of-type fixnum from 24 downto 0 by 8
            for octet of-type octet = (read-next-byte)
            sum (ash* octet count)))))

(defmethod octets-to-char-code ((format flexi-cr-mixin) reader)
  (declare #.*fixnum-optimize-settings*)
  (declare (ignore reader))
  (let ((char-code (call-next-method)))
    (case char-code
      (#.+cr+ #.(char-code #\Newline))
      (otherwise char-code))))

(defmethod octets-to-char-code ((format flexi-crlf-mixin) reader)
  (declare #.*fixnum-optimize-settings*)
  (declare (function *current-unreader*))
  (declare (ignore reader))
  (let ((char-code (call-next-method)))
    (case char-code
      (#.+cr+
       (let ((next-char-code (call-next-method)))
         (case next-char-code
           (#.+lf+ #.(char-code #\Newline))
           ;; we saw a CR but no LF afterwards, but then the data
           ;; ended, so we just return #\Return
           ((nil) +cr+)
           ;; if the character we peeked at wasn't a
           ;; linefeed character we unread its constituents
           (otherwise (funcall *current-unreader* (code-char next-char-code))
                      char-code))))
      (otherwise char-code))))

