(defpackage :mezzano.cold-generator.util
  (:use :cl)
  (:export #:load-binary-file
           #:utf-8-sequence-length
           #:align-up
           #:git-revision))

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
