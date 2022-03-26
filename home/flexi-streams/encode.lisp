;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/encode.lisp,v 1.26 2008/05/26 10:55:08 edi Exp $

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

(defgeneric char-to-octets (format char writer)
  (declare #.*standard-optimize-settings*)
  (:documentation "Converts the character CHAR to a sequence of octets
using the external format FORMAT.  The conversion is performed by
calling the unary function \(which must be a functional object) WRITER
repeatedly each octet.  The return value of this function is
unspecified."))

(defgeneric write-sequence* (format stream sequence start end)
  (declare #.*standard-optimize-settings*)
  (:documentation "A generic function which dispatches on the external
format and does the real work for STREAM-WRITE-SEQUENCE."))

(defgeneric string-to-octets* (format string start end)
  (declare #.*standard-optimize-settings*)
  (:documentation "A generic function which dispatches on the external
format and does the real work for STRING-TO-OCTETS."))

(defmethod string-to-octets* :around (format (list list) start end)
  (declare #.*standard-optimize-settings*)
  (string-to-octets* format (coerce list 'string*) start end))

(defmacro define-sequence-writers ((format-class) &body body)
  "Non-hygienic utility macro which defines methods for
WRITE-SEQUENCE* and STRING-TO-OCTETS* for the class FORMAT-CLASS.  For
BODY see the docstring of DEFINE-CHAR-ENCODERS."
  (let ((body `((locally
                  (declare #.*fixnum-optimize-settings*)
                  ,@body))))
    `(progn
       (defmethod string-to-octets* ((format ,format-class) string start end)
         (declare #.*standard-optimize-settings*)
         (declare (fixnum start end) (string string))
         (let ((octets (make-array (compute-number-of-octets format string start end)
                                   :element-type 'octet))
               (j 0))
           (declare (fixnum j))
           (loop for i of-type fixnum from start below end do
                 (macrolet ((octet-writer (form)
                              `(progn
                                 (setf (aref (the (array octet *) octets) j) ,form)
                                 (incf j))))
                   (symbol-macrolet ((char-getter (char string i)))
                     (progn ,@body))))
           octets))
       (defmethod write-sequence* ((format ,format-class) stream sequence start end)
         (declare #.*standard-optimize-settings*)
         (declare (fixnum start end))
         (with-accessors ((column flexi-stream-column))
             stream
           (let* ((octet-seen-p nil)
                  (buffer-pos 0)
                  ;; estimate should be good enough...
                  (factor (encoding-factor format))
                  ;; we don't want arbitrarily large buffer, do we?
                  (buffer-size (min +buffer-size+ (ceiling (* factor (- end start)))))
                  (buffer (make-octet-buffer buffer-size))
                  (underlying-stream (flexi-stream-stream stream)))
             (declare (fixnum buffer-pos buffer-size)
                      (boolean octet-seen-p)
                      (type (array octet *) buffer))
             (macrolet ((octet-writer (form)
                          `(write-octet ,form)))
               (labels ((flush-buffer ()
                          "Sends all octets in BUFFER to the underlying stream."
                          (write-sequence buffer underlying-stream :end buffer-pos)
                          (setq buffer-pos 0))
                        (write-octet (octet)
                          "Adds one octet to the buffer and flushes it if necessary."
                          (declare (type octet octet))
                          (when (>= buffer-pos buffer-size)
                            (flush-buffer))
                          (setf (aref buffer buffer-pos) octet)
                          (incf buffer-pos))
                        (write-object (object)
                          "Dispatches to WRITE-OCTET or WRITE-CHARACTER
depending on the type of OBJECT."
                          (etypecase object
                            (octet (setq octet-seen-p t)
                                   (write-octet object))
                            (character (symbol-macrolet ((char-getter object))
                                         ,@body)))))
                 (macrolet ((iterate (&body output-forms)
                              "An unhygienic macro to implement the actual
iteration through SEQUENCE.  OUTPUT-FORM is the form to retrieve one
sequence element and put its octet representation into the buffer."
                              `(loop for index of-type fixnum from start below end
                                     do (progn ,@output-forms)
                                     finally (when (plusp buffer-pos)
                                               (flush-buffer)))))
                   (etypecase sequence
                     (string (iterate
                              (symbol-macrolet ((char-getter (char sequence index)))
                                ,@body)))
                     (array (iterate
                             (symbol-macrolet ((char-getter (aref sequence index)))
                               ,@body)))
                     (list  (iterate (write-object (nth index sequence))))))
                 ;; update the column slot, setting it to NIL if we sent
                 ;; octets
                 (setq column
                       (cond (octet-seen-p nil)
                             (t (let ((last-newline-pos (position #\Newline sequence
                                                                  :test #'char=
                                                                  :start start
                                                                  :end end
                                                                  :from-end t)))
                                  (cond (last-newline-pos (- end last-newline-pos 1))
                                        (column (+ column (- end start))))))))))))))))

(defmacro define-char-encoders ((lf-format-class cr-format-class crlf-format-class) &body body)
  "Non-hygienic utility macro which defines several encoding-related
methods for the classes LF-FORMAT-CLASS, CR-FORMAT-CLASS, and
CRLF-FORMAT-CLASS where it is assumed that CR-FORMAT-CLASS is the same
encoding as LF-FORMAT-CLASS but with CR instead of LF line endings and
similar for CRLF-FORMAT-CLASS, i.e. LF-FORMAT-CLASS is the base class.
BODY is a code template for the code to convert one character to
octets.  BODY must contain a symbol CHAR-GETTER representing the form
which is used to obtain the character and a forms like \(OCTET-WRITE
<thing>) to write the octet <thing>.  The CHAR-GETTER form might be
called more than once."
  `(progn
     (defmethod char-to-octets ((format ,lf-format-class) char writer)
       (declare #.*fixnum-optimize-settings*)
       (declare (character char) (function writer))
       (symbol-macrolet ((char-getter char))
         (macrolet ((octet-writer (form)
                      `(funcall writer ,form)))
           ,@body)))
     (define-sequence-writers (,lf-format-class) ,@body)
     (define-sequence-writers (,cr-format-class)
       ;; modify the body so that the getter replaces a #\Newline
       ;; with a #\Return
       ,@(sublis `((char-getter . ,(with-unique-names (char)
                                     `(let ((,char char-getter))
                                        (declare (character ,char))
                                        (if (char= ,char #\Newline)
                                          #\Return
                                          ,char)))))
                 body))
     (define-sequence-writers (,crlf-format-class)
       ;; modify the body so that we potentially write octets for
       ;; two characters (#\Return and #\Linefeed) - the original
       ;; body is wrapped with the WRITE-CHAR local function
       ,(with-unique-names (char write-char)
          `(flet ((,write-char (,char)
                    ,@(sublis `((char-getter . ,char)) body)))
             (let ((,char char-getter))
               (declare (character ,char))
               (cond ((char= ,char #\Newline)
                      (,write-char #\Return)
                      (,write-char #\Linefeed))
                     (t (,write-char ,char)))))))))

(define-char-encoders (flexi-latin-1-format flexi-cr-latin-1-format  flexi-crlf-latin-1-format)
  (let ((octet (char-code char-getter)))
    (when (> octet 255)
      (signal-encoding-error format "~S (code ~A) is not a LATIN-1 character." char-getter octet))
    (octet-writer octet)))

(define-char-encoders (flexi-ascii-format flexi-cr-ascii-format flexi-crlf-ascii-format)
  (let ((octet (char-code char-getter)))
    (when (> octet 127)
      (signal-encoding-error format "~S (code ~A) is not an ASCII character." char-getter octet))
    (octet-writer octet)))

(define-char-encoders (flexi-8-bit-format flexi-cr-8-bit-format flexi-crlf-8-bit-format)
  (with-accessors ((encoding-hash external-format-encoding-hash))
      format
    (let ((octet (gethash (char-code char-getter) encoding-hash)))
      (unless octet
        (signal-encoding-error format "~S (code ~A) is not in this encoding." char-getter octet))
      (octet-writer octet))))

(define-char-encoders (flexi-utf-8-format flexi-cr-utf-8-format flexi-crlf-utf-8-format)
  ;; the old version using LDB was more elegant, but some Lisps had
  ;; trouble optimizing it
  (let ((char-code (char-code char-getter)))
    (tagbody
     (cond ((< char-code #x80)
            (octet-writer char-code)
            (go zero))
           ((< char-code #x800)
            (octet-writer (logior* #b11000000 (ash* char-code -6)))
            (go one))
           ((< char-code #x10000)
            (octet-writer (logior* #b11100000 (ash* char-code -12)))
            (go two))
           (t
            (octet-writer (logior* #b11110000 (ash* char-code -18)))))
     (octet-writer (logior* #b10000000 (logand* #b00111111 (ash* char-code -12))))
     two
     (octet-writer (logior* #b10000000 (logand* #b00111111 (ash* char-code -6))))
     one
     (octet-writer (logior* #b10000000 (logand* #b00111111 char-code)))
     zero)))

(define-char-encoders (flexi-utf-16-le-format flexi-cr-utf-16-le-format flexi-crlf-utf-16-le-format)
  (flet ((write-word (word)
           (octet-writer (logand* #x00ff word))
           (octet-writer (ash* (logand* #xff00 word) -8))))
    (declare (inline write-word))
    (let ((char-code (char-code char-getter)))
      (declare (type char-code-integer char-code))
      (cond ((< char-code #x10000)
             (write-word char-code))
            (t (decf char-code #x10000)
               (write-word (logior* #xd800 (ash* char-code -10)))
               (write-word (logior* #xdc00 (logand* #x03ff char-code))))))))

(define-char-encoders (flexi-utf-16-be-format flexi-cr-utf-16-be-format flexi-crlf-utf-16-be-format)
  (flet ((write-word (word)
           (octet-writer (ash* (logand* #xff00 word) -8))
           (octet-writer (logand* #x00ff word))))
    (declare (inline write-word))
    (let ((char-code (char-code char-getter)))
      (declare (type char-code-integer char-code))
      (cond ((< char-code #x10000)
             (write-word char-code))
            (t (decf char-code #x10000)
               (write-word (logior* #xd800 (ash* char-code -10)))
               (write-word (logior* #xdc00 (logand* #x03ff char-code))))))))

(define-char-encoders (flexi-utf-32-le-format flexi-cr-utf-32-le-format flexi-crlf-utf-32-le-format)
  (let ((char-code (char-code char-getter)))
    (octet-writer (logand* #x00ff char-code))
    (octet-writer (logand* #x00ff (ash* char-code -8)))
    (octet-writer (logand* #x00ff (ash* char-code -16)))
    (octet-writer (logand* #x00ff (ash* char-code -24)))))

(define-char-encoders (flexi-utf-32-be-format flexi-cr-utf-32-be-format flexi-crlf-utf-32-be-format)
  (let ((char-code (char-code char-getter)))
    (octet-writer (logand* #x00ff (ash* char-code -24)))
    (octet-writer (logand* #x00ff (ash* char-code -16)))
    (octet-writer (logand* #x00ff (ash* char-code -8)))
    (octet-writer (logand* #x00ff char-code))))

(defmethod char-to-octets ((format flexi-cr-mixin) char writer)
  (declare #.*fixnum-optimize-settings*)
  (declare (character char))
  (if (char= char #\Newline)
    (call-next-method format #\Return writer)
    (call-next-method)))

(defmethod char-to-octets ((format flexi-crlf-mixin) char writer)
  (declare #.*fixnum-optimize-settings*)
  (declare (character char))
  (cond ((char= char #\Newline)
         (call-next-method format #\Return writer)
         (call-next-method format #\Linefeed writer))
        (t (call-next-method))))
