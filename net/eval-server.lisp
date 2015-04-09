;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.net)

(define-condition close-eval-server () ())

(defclass eval-server-stream (sys.gray:fundamental-character-input-stream
                              sys.gray:fundamental-character-output-stream
                              sys.gray:unread-char-mixin)
  ((remote-stream :initarg :remote-stream :reader remote-stream)
   (unread-char :initform nil)))

(defmethod sys.gray:stream-write-char ((stream eval-server-stream) char)
  (write-char char (remote-stream stream)))

(defmethod sys.gray:stream-read-char ((stream eval-server-stream))
  (let ((c (read-char (remote-stream stream) nil)))
    (or c
        (signal (make-condition 'close-eval-server)))))

(defun eval-server (stream)
  (with-open-stream (stream stream)
    (with-simple-restart (abort "Give up")
      (handler-case
          (let ((*terminal-io* (make-instance 'eval-server-stream :remote-stream stream))
                (*standard-output* (make-synonym-stream '*terminal-io*))
                (*standard-input* (make-synonym-stream '*terminal-io*))
                (*debug-io* (make-synonym-stream '*terminal-io*))
                (*query-io* (make-synonym-stream '*terminal-io*))
                (*error-output* (make-synonym-stream '*terminal-io*)))
            (sys.int::repl))
        (close-eval-server ())))))

(defun open-eval-server (connection)
  (mezzano.supervisor:make-thread (lambda () (eval-server (make-instance 'tcp-stream :connection connection)))
                                  :name "Remote Lisp server"))

(push '(1138 open-eval-server) mezzano.network.tcp::*server-alist*)
