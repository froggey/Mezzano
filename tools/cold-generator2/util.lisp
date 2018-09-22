;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.cold-generator.util
  (:use :cl)
  (:export #:load-binary-file
           #:utf-8-sequence-length
           #:align-up
           #:git-revision
           #:generate-uuid
           #:format-uuid
           #:parse-uuid))

(in-package :mezzano.cold-generator.util)

(defun load-binary-file (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

(defun utf-8-sequence-length (byte)
  (cond
    ((eql (logand byte #x80) #x00)
     (values 1 byte))
    ((eql (logand byte #xE0) #xC0)
     (values 2 (logand byte #x1F)))
    ((eql (logand byte #xF0) #xE0)
     (values 3 (logand byte #x0F)))
    ((eql (logand byte #xF8) #xF0)
     (values 4 (logand byte #x07)))
    (t (error "Invalid UTF-8 lead byte ~S." byte))))

(defun align-up (value boundary)
  (incf value (1- boundary))
  (- value (rem value boundary)))

(defun git-revision ()
  "Return the current git hash as a string or NIL if it can't be determined."
  (ignore-errors
    (values (uiop/run-program:run-program '("git" "rev-parse" "HEAD")
                                          :output '(:string :stripped t)))))

(defun generate-uuid ()
  (let ((uuid (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref uuid i) (case i
                            (9 (logior #x40 (random 16)))
                            (7 (logior (random 64) #x80))
                            (t (random 256)))))
    uuid))

(defun format-uuid (stream object &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  ;; Printed UUIDs are super weird.
  (format stream "~2,'0X~2,'0X~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X"
          ;; Byteswapped.
          (aref object 3) (aref object 2) (aref object 1) (aref object 0)
          (aref object 5) (aref object 4)
          (aref object 7) (aref object 6)
          ;; Not byteswapped.
          (aref object 8) (aref object 9)
          (aref object 10) (aref object 11) (aref object 12) (aref object 13) (aref object 14) (aref object 15)))

(defun parse-uuid (string)
  (assert (eql (length string) 36))
  (assert (eql (char string 8) #\-))
  (assert (eql (char string 13) #\-))
  (assert (eql (char string 18) #\-))
  (assert (eql (char string 23) #\-))
  (flet ((b (start)
           (parse-integer string :radix 16 :start start :end (+ start 2))))
    (vector
     ;; First group. Byteswapped.
     (b 6) (b 4) (b 2) (b 0)
     ;; Second group. Byteswapped.
     (b 11) (b 9)
     ;; Third group. Byteswapped.
     (b 16) (b 14)
     ;; Fourth group. Not byteswapped.
     (b 19) (b 21)
     ;; Fifth group. Not byteswapped.
     (b 24) (b 26) (b 28) (b 30) (b 32) (b 34))))
