;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; sharp-backslash.lisp --- Alternative #\ dispatch code.
;;;
;;; Copyright (C) 2007-2009, Luis Oliveira  <loliveira@common-lisp.net>
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

(in-package #:babel)

#-allegro
(defun sharp-backslash-reader (original-reader stream char numarg)
  (let ((1st-char (read-char stream)))
    (if (and (char-equal 1st-char #\u)
             ;; because #\z is not a digit char...
             (digit-char-p (peek-char nil stream nil #\z) 16))
        ;; something better than READ would be nice here
        (let ((token (let ((*read-base* 16)) (read stream))))
          (if (typep token 'babel-encodings::code-point)
              (code-char token)
              (if *read-suppress*
                  nil
                  (simple-reader-error
                   stream "Unrecognized character name: u~A" token))))
        (funcall original-reader
                 (make-concatenated-stream (make-string-input-stream
                                            (string 1st-char))
                                           stream)
                 char
                 numarg))))

;;; Allegro's PEEK-CHAR seems broken in some situations, and the code
;;; above would generate an error about too many calls to UNREAD-CHAR.
;;; Then Allegro's original SHARP-BACKSLASH wants to UNREAD-CHAR
;;; twice, very weird.  This is the best workaround I could think of.
;;; It sucks.
#+allegro
(defun sharp-backslash-reader (original-reader stream char numarg)
  (let* ((1st-char (read-char stream))
         (rest (ignore-errors (excl::read-extended-token stream)))
         (code (when (and rest (char-equal 1st-char #\u))
                 (ignore-errors (parse-integer rest :radix 16)))))
    (if code
        (code-char code)
        (with-input-from-string
            (s (concatenate 'string "#\\" (string 1st-char) rest))
          (read-char s)
          (read-char s)
          (funcall original-reader s char numarg)))))

(defun make-sharp-backslash-reader ()
  (let ((original-sharp-backslash (get-dispatch-macro-character #\# #\\)))
    (lambda (stream char numarg)
      (sharp-backslash-reader original-sharp-backslash stream char numarg))))

(defmacro enable-sharp-backslash-syntax ()
  `(eval-when (:compile-toplevel :execute)
     (setf *readtable* (copy-readtable *readtable*))
     (set-sharp-backslash-syntax-in-readtable)
     (values)))

(defun set-sharp-backslash-syntax-in-readtable ()
  (set-dispatch-macro-character #\# #\\ (make-sharp-backslash-reader))
  (values))
