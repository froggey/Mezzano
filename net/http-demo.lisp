;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :http-demo
  (:use :cl)
  (:export #:start-http-server))

(in-package :http-demo)

(defun parse-http-request (line)
  (format t "Parsing request ~S.~%" line)
  (let ((request nil)
        (path '())
        (get-parameters nil)
        (extra nil)
        (offset 0))
    (dotimes (i (length line))
      (when (eql (char line i) #\Space)
        (return))
      (incf offset))
    (setf request (subseq line 0 offset))
    ;; Eat spaces.
    (do () ((or (>= offset (length line))
                (not (eql (char line offset) #\Space))))
      (incf offset))
    ;; Path must start with /
    (unless (eql (char line offset) #\/)
      (return-from parse-http-request (values nil nil nil nil)))
    (incf offset)
    ;; Break request apart with / as the seperator, stopping
    ;; at space, ? or eol.
    (do ((start offset))
        ((or (>= offset (length line))
             (member (char line offset) '(#\Space #\?)))
         (unless (eql start offset)
           (push (subseq line start offset) path)))
      (when (eql (char line offset) #\/)
        (push (subseq line start offset) path)
        (setf start (1+ offset)))
      (incf offset))
    (when (and (< offset (length line))
               (eql (char line offset) #\?))
      (let ((parameter-start (1+ offset)))
        (do () ((or (>= offset (length line))
                    (eql (char line offset) #\Space)))
          (incf offset))
        (setf get-parameters (subseq line parameter-start offset))))
    ;; Eat spaces.
    (do () ((or (>= offset (length line))
                (not (eql (char line offset) #\Space))))
      (incf offset))
    (values request (nreverse path) get-parameters (subseq line offset))))

(defun convert-newlines (string)
  (let ((doing-indentation t))
    (dotimes (i (length string))
      (case (char string i)
        (#\Newline
         (setf doing-indentation t)
         (write-string "<BR>")
         (terpri))
        (#\Space
         (if doing-indentation
             (write-string "&nbsp;")
             (write-char #\Space)))
        (t (setf doing-indentation nil)
           (write-char (char string i)))))))

(defun demo-file (stream)
  (let ((*standard-output* stream))
    (format t "HTTP/1.0 200 OK~%")
    (format t "Content-Type: text/html; charset=utf-8~%~%")
    (format t "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"~%")
    (format t "\"http://www.w3.org/TR/html4/loose.dtd\">~%")
    (format t "<HTML><HEAD><TITLE>(LISP OS)</TITLE></HEAD>~%")
    (format t "<BODY><H1>Hello, World!</H1><BR>~%")
    (convert-newlines
     (with-output-to-string (*standard-output*)
       (room)))
    (format t "<BR>~%")
    (format t "<BR>~%Running on ~A ~S<BR>~%"
            (lisp-implementation-type)
            (lisp-implementation-version))
    (format t "<IMG SRC=\"/logo300x100.jpg\" ALT=\"Made with Lisp.\">~%")
    (format t "</BODY></HTML>~%")))

(defun serve-request (stream)
  (with-simple-restart (abort "Give up")
    (with-open-stream (stream stream)
      (let ((line (read-line stream nil)))
        (when (null line)
          (return-from serve-request))
        (multiple-value-bind (request path get-parameters)
            (parse-http-request line)
          (format t "HTTP: ~S ~S ~S~%" request path get-parameters)
          (cond ((string-equal "GET" request)
                 (cond ((eql path '())
                        ;; Fully buffered output.
                        (with-output-to-string (s)
                          (demo-file s)
                          (write-sequence (get-output-stream-string s) stream)))
                       ((equal path '("char-by-char"))
                        ;; Character-by-character output.
                        (demo-file stream))
                       ((equalp path '("logo300x100.jpg"))
                        (with-open-file (s "logo300x100.jpg"
                                           :element-type '(unsigned-byte 8)
                                           :if-does-not-exist nil)
                          (cond (s (sys.net:buffered-format stream "HTTP/1.0 200 OK~%~%")
                                   (let* ((file-size (file-length s))
                                          (buf (make-array file-size :element-type '(unsigned-byte 8))))
                                     (read-sequence buf s)
                                     (write-sequence buf stream)))
                                (t (sys.net:buffered-format stream "HTTP/1.0 404 Not Found~%~%")))))
                       (t (sys.net:buffered-format stream "HTTP/1.0 404 Not Found~%~%"))))
                (t (sys.net:buffered-format stream "HTTP/1.0 400 Bad Request~%~%"))))))))

(defun http-server (connection-queue)
  (loop
     (let ((connection (mezzano.supervisor:fifo-pop connection-queue)))
       (ignore-errors (serve-request connection)))))

(defun start-http-server (&optional (port 80))
  (let* ((connection-queue (mezzano.supervisor:make-fifo 50))
         (server-thread (mezzano.supervisor:make-thread (lambda () (http-server connection-queue))
                                                        :name "HTTP server"))
         (listen-function (lambda (connection)
                            (when (not (mezzano.supervisor:fifo-push
                                        (make-instance 'sys.net::tcp-stream :connection connection)
                                        connection-queue
                                        nil))
                              ;; Drop connections when they can't be handled.
                              (close connection)))))
    (setf mezzano.network.tcp::*server-alist* (remove port mezzano.network.tcp::*server-alist*
                                                      :key #'first))
    (push (list port listen-function) mezzano.network.tcp::*server-alist*)
    (values server-thread listen-function connection-queue)))

(defvar *http-server-thread* nil)
(defvar *http-server-listen-function* nil)
(defvar *http-server-connection-queue* nil)

(when (not *http-server-thread*)
  (multiple-value-setq (*http-server-thread* *http-server-function* *http-server-connection-queue*)
    (start-http-server)))
