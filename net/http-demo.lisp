;;;; Basic HTTP server

(defpackage :mezzano.http-demo
  (:use :cl)
  (:export #:start-server #:stop-server))

(in-package :mezzano.http-demo)

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
                          (cond (s (mezzano.network:buffered-format stream "HTTP/1.0 200 OK~%~%")
                                   (let* ((file-size (file-length s))
                                          (buf (make-array file-size :element-type '(unsigned-byte 8))))
                                     (read-sequence buf s)
                                     (write-sequence buf stream)))
                                (t (mezzano.network:buffered-format stream "HTTP/1.0 404 Not Found~%~%")))))
                       (t (mezzano.network:buffered-format stream "HTTP/1.0 404 Not Found~%~%"))))
                (t (mezzano.network:buffered-format stream "HTTP/1.0 400 Bad Request~%~%"))))))))

(defclass http-server ()
  ((%context :initarg :context :reader http-server-context)
   (%listener-source :reader http-server-listener-source)
   (%shutdown-state :initarg :shutdown-state :reader http-server-shutdown-state)))

(defun %start-server (server port)
  (let* ((listener (mezzano.network.tcp:tcp-listen
                    (mezzano.network.ip:make-ipv4-address "0.0.0.0")
                    port))
         (source (mezzano.sync.dispatch:make-source
                  listener
                  (lambda ()
                    (let ((connection (mezzano.network.tcp:tcp-accept listener)))
                      ;; Dispatch the new connection in the shutdown group so
                      ;; that all connections can finished up when the server
                      ;; is stopped.
                      (mezzano.sync.dispatch:dispatch-async
                       (lambda () (ignore-errors (serve-request connection)))
                       (mezzano.sync.dispatch:global-queue)
                       :group (http-server-shutdown-state server))))
                  :cancellation-handler (lambda ()
                                          (mezzano.network.tcp:close-tcp-listener listener)
                                          (mezzano.sync.dispatch:group-leave (http-server-shutdown-state server))))))
    (mezzano.sync.dispatch:group-enter (http-server-shutdown-state server))
    (setf (slot-value server '%listener-source) source)))

(defun start-server (&key (port 80))
  (let* ((context (mezzano.sync.dispatch:make-dispatch-context
                   :name (format nil "HTTP server port ~D" port)))
         (server (make-instance 'http-server
                                :context context
                                :shutdown-state (mezzano.sync.dispatch:make-group :context context))))
    (mezzano.sync.dispatch:dispatch-sync
     (lambda () (%start-server server port))
     (mezzano.sync.dispatch:global-queue :context context))
    server))

(defun stop-server (server)
  ;; Cancel the listener source to stop further connections.
  ;; TODO: Make pending connections cancellable.
  (mezzano.sync.dispatch:cancel (http-server-listener-source server))
  ;; Wait for shutdown to complete.
  (mezzano.sync.dispatch:wait (http-server-shutdown-state server))
  ;; Shutdown the rest of the dispatch system.
  (mezzano.sync.dispatch:dispatch-shutdown :context (http-server-context server)))

(defvar *demo-server* (start-server))
