;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(require :babel)

(defpackage :mezzano.file-system.http
  (:export #:http-host)
  (:use #:cl #:mezzano.file-system))

(in-package :mezzano.file-system.http)

(defvar *permit-redirects* t
  "If true, then redirects are followed. Otherwise 3xx status codes are treated as not-found.")

(defclass http-host () ())

(defmethod host-name ((host http-host))
  (declare (ignore host))
  "HTTP")

(defmethod host-default-device ((host http-host))
  (declare (ignore host))
  ())

(defclass http-binary-stream (sys.gray:fundamental-binary-input-stream
                              sys.gray:fundamental-binary-output-stream
                              file-stream)
  ((path :initarg :path :reader path)
   (position :initarg :position :accessor stream-position)
   (buffer :initarg :buffer :reader stream-buffer)))

(defclass http-character-stream (sys.gray:fundamental-character-input-stream
                                 sys.gray:fundamental-character-output-stream
                                 file-stream)
  ((path :initarg :path :reader path)
   (position :initarg :position :accessor stream-position)
   (buffer :initarg :buffer :reader stream-buffer)))

(defmethod print-object ((object http-binary-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (path object))))

(defmethod parse-namestring-using-host ((host http-host) namestring junk-allowed)
  (when junk-allowed
    (error "TODO: Junk-allowed"))
  ;; A namestring looks like:
  ;; url: ["//"] host [port] [path]
  ;; host: [a-zA-Z0-9_.-]+
  ;; port: ":" [0-9]+
  ;; path: "/" .*
  (let ((current 0)
        (domain (make-array 30 :adjustable t :fill-pointer 0 :element-type 'character))
        (port (make-array 30 :adjustable t :fill-pointer 0 :element-type 'character))
        (port-number 80)
        (path nil))
    (flet ((peek ()
             (when (< current (length namestring))
               (char namestring current)))
           (consume ()
             (prog1
                 (char namestring current)
               (incf current))))
      (when (and (>= (length namestring) 2)
                 (eql (char namestring 0) #\/)
                 (eql (char namestring 1) #\/))
        ;; Skip leading "//"
        (setf current 2))
      ;; Read host portion.
      (loop
         (when (not (find (peek) "abcdefghijklmnopqrstuvwxyz0123456789-_."))
           (return))
         (vector-push-extend (consume) domain))
      ;; Possible port.
      (when (eql (peek) #\:)
        (consume)
        (loop
           (when (not (find (peek) "abcdefghijklmnopqrstuvwxyz0123456789-_."))
             (setf port-number (parse-integer port))
             (return))
           (vector-push-extend (consume) port)))
      ;; Finally, the path.
      (cond
        ((eql (peek) #\/)
         (setf path (subseq namestring current)))
        ((eql (peek) nil)
         (setf path "/"))
        (t (error "Syntax error in path, expected / and or nothing after host/port.")))
      (make-pathname :host host
                     :device (cons domain port-number)
                     :directory '(:absolute)
                     :name path))))

(defun unparse-http-path (path)
  (format nil "~A:~D~A"
          (car (pathname-device path))
          (cdr (pathname-device path))
          (pathname-name path)))

(defmethod unparse-pathname (path (host http-host))
  (declare (ignore host))
  (unparse-http-path path))

(defmethod unparse-pathname-file (pathname (host http-host))
  (declare (ignore host))
  (unparse-http-path pathname))

(defmethod unparse-pathname-directory (pathname (host http-host))
  (declare (ignore host))
  (unparse-http-path pathname))

(defun match-header (header line)
  (and (< (length header) (length line))
       (string-equal header line :end2 (length header))
       (eql (char line (length header)) #\:)))

(defun url-encode (string)
  "Convert STRING into a string of URL-safe ASCII characters."
  (let ((encoded (babel:string-to-octets string :encoding :utf-8 :use-bom nil))
        (result (make-array (length string)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
        (hex-digit "0123456789ABCDEF"))
    (loop
       for byte across encoded
       do (cond ((find byte #.(map 'simple-vector #'char-code
                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()?/=&"))
                 ;; URL-safe (mostly).
                 (vector-push-extend (code-char byte) result))
                (t ;; URL encode.
                 (vector-push-extend #\% result)
                 (vector-push-extend (char hex-digit (ldb (byte 4 4) byte)) result)
                 (vector-push-extend (char hex-digit (ldb (byte 4 0) byte)) result))))
    result))

(defun type-equal (x y)
  (and (subtypep x y)
       (subtypep y x)))

(defun decode-buffer (buffer external-format)
  ;; Babel doesn't seem to understand external-formats properly, and ignores the EOL style.
  (let ((decoded (babel:octets-to-string buffer
                                         :encoding (if (eql external-format :default)
                                                       :utf-8
                                                       external-format)
                                         :errorp nil)))
    (if (eql external-format :default)
        (remove #\Carriage-Return decoded)
        decoded)))

(defun decode-status-line (line)
  (let* ((version-end (position #\Space line))
         (code-end (position #\Space line :start (1+ version-end)))
         (version (subseq line 0 version-end))
         (code (parse-integer line
                              :start (1+ version-end)
                              :end code-end))
         (reason (subseq line (1+ code-end))))
    (values version
            code
            reason)))

(defun decode-http-header-line (line)
  (let* ((seperator-position (position #\: line :test #'char-equal))
         (name (subseq line 0 seperator-position))
         (value (subseq line (1+ seperator-position))))
    (cons (string-trim " " name)
          (string-trim " " value))))

(defun header-name (header)
  (car header))

(defun header-value (header)
  (cdr header))

(defun read-chunked-body (stream)
  (let ((result (make-array 0
                            :element-type '(unsigned-byte 8)
                            :adjustable t)))
    (loop
       ;; Read chunks.
       (let ((chunk-size (parse-integer (read-line stream) :radix 16))
             (insertion-point (length result)))
         (when (eql chunk-size 0)
           (read-line stream)
           (return))
         (adjust-array result (+ (length result) chunk-size))
         (read-sequence result stream :start insertion-point :end (length result))
         (read-line stream)))
    result))

(defun read-content-length-body (stream content-length-header)
  (let* ((content-length (parse-integer (header-value content-length-header)))
         (result (make-array content-length
                             :element-type '(unsigned-byte 8))))
    (read-sequence result stream)
    result))

(defun read-http-response (stream)
  (multiple-value-bind (version status-code reason-phrase)
      (decode-status-line (read-line stream))
    (let* ((headers (loop
                       for line = (read-line stream)
                       until (eql (length line) 0)
                       collect (decode-http-header-line line)))
           (body nil)
           (content-length-header (find "Content-Length" headers :test #'string-equal :key #'header-name))
           (transfer-encoding-header (find "Transfer-Encoding" headers :test #'string-equal :key #'header-name)))
      (when (and transfer-encoding-header
                 (not (string-equal (header-value transfer-encoding-header) "chunked")))
        (error "Unsupported transfer encoding ~S." (header-value transfer-encoding-header)))
      (when (and transfer-encoding-header
                 content-length-header)
        (error "Transfer-Encoding and Content-Length headers are mutually exclusive?"))
      (cond
        (transfer-encoding-header
         (setf body (read-chunked-body stream)))
        (content-length-header
         (setf body (read-content-length-body stream content-length-header))))
      (values version status-code reason-phrase headers body))))

(defun http-request (host port path)
  ;; FIXME: Should catch unknown host & do something with that.
  (sys.net::with-open-network-stream (con host port)
    (sys.net::buffered-format con "GET ~A HTTP/1.1~%Host: ~A~%~%" path host)
    (read-http-response con)))

(defun make-http-stream (pathname body element-type external-format)
  (if (type-equal element-type 'character)
      (make-instance 'http-character-stream
                     :path pathname
                     :position 0
                     :buffer (decode-buffer body external-format))
      (make-instance 'http-binary-stream
                     :path pathname
                     :position 0
                     :buffer body)))

(defun decode-location (location)
  ;; This should parse an absoluteURI from rfc2396.
  ;; protocol "://" host [path]
  (let* ((protocol-end (position #\: location))
         (protocol (subseq location 0 protocol-end))
         (host-end (position #\/ location :start (+ protocol-end 3)))
         (host (subseq location (+ protocol-end 3) host-end)))
    (when (string-equal protocol "http")
      (values host 80 (if host-end
                          (subseq location host-end)
                          "/")))))

(defmethod open-using-host ((host http-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (declare (ignore host if-exists))
  (check-type direction (member :input :probe))
  (assert (not (eql if-does-not-exist :create)) (if-does-not-exist)
          ":IF-DOES-NOT-EXIST :CREATE not supported.")
  (when (and (not (type-equal element-type '(unsigned-byte 8)))
             (not (type-equal element-type 'character)))
    (error "Only (UNSIGNED-BYTE 8) and CHARACTER element types supported."))
  (let (version status-code reason-phrase headers body
        (redirect-count 0))
    ;; Initial request.
    (setf (values version status-code reason-phrase headers body) (http-request (car (pathname-device pathname))
                                                                                (cdr (pathname-device pathname))
                                                                                (url-encode (pathname-name pathname))))
    (tagbody
     REDIRECTED
       (when (and *permit-redirects*
                  (member status-code '(301 302 303 307)))
         (let ((location (find "Location" headers :test #'string-equal :key #'header-name)))
           (when location
             (multiple-value-bind (host port path)
                 (decode-location (header-value location))
               (when host
                 (setf (values version status-code reason-phrase headers body) (http-request host port path))
                 (incf redirect-count)
                 ;; Avoid redirect loops.
                 (when (< redirect-count 10)
                   (go REDIRECTED))))))))
    (cond
      ((eql status-code 200) ;; 200 OK
       (make-http-stream pathname (or body #()) element-type external-format))
      (t ;; Not found or other error.
       (when (eql if-does-not-exist :error)
         (error 'simple-file-error
                :pathname pathname
                :format-control "HTTP ~D ~A"
                :format-arguments (list status-code reason-phrase)))))))

(defmethod sys.gray:stream-element-type ((stream http-binary-stream))
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod sys.gray:stream-read-byte ((stream http-binary-stream))
  (cond ((>= (stream-position stream)
             (length (stream-buffer stream)))
         :eof)
        (t (prog1
               (aref (stream-buffer stream) (stream-position stream))
             (incf (stream-position stream))))))

(defmethod sys.gray:stream-file-position ((stream http-binary-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (setf (stream-position stream)
               (if (eql position-spec :end)
                   (length (stream-buffer stream))
                   position-spec)))
        (t (stream-position stream))))

(defmethod sys.gray:stream-file-length ((stream http-binary-stream))
  (length (stream-buffer stream)))

(defmethod sys.gray:stream-element-type ((stream http-character-stream))
  (declare (ignore stream))
  'character)

(defmethod sys.gray:stream-read-char ((stream http-character-stream))
  (cond ((>= (stream-position stream)
             (length (stream-buffer stream)))
         :eof)
        (t (prog1
               (aref (stream-buffer stream) (stream-position stream))
             (incf (stream-position stream))))))

(defmethod sys.gray:stream-file-position ((stream http-character-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (setf (stream-position stream)
               (if (eql position-spec :end)
                   (length (stream-buffer stream))
                   position-spec)))
        (t (stream-position stream))))

(defmethod sys.gray:stream-file-length ((stream http-character-stream))
  (length (stream-buffer stream)))

(defmethod directory-using-host ((host http-host) pathname &key)
  (declare (ignore host pathname))
  '())

(setf (find-host :http) (make-instance 'http-host))
