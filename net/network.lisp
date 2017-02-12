;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.net)

(deftype octet ()
  '(unsigned-byte 8))

(defun packet-length (packet)
  (reduce '+ (mapcar 'length packet)))

(defun copy-packet (buffer packet &optional (buffer-start 0))
  (dolist (p packet)
    (dotimes (i (length p))
      (setf (aref buffer buffer-start) (aref p i))
      (incf buffer-start))))

(defun buffered-format (stream control-string &rest arguments)
  "Buffered FORMAT."
  (declare (dynamic-extent argument))
  (write-sequence (apply 'format nil control-string arguments) stream))

(defun utf-8-decode-leader (leader)
  "Break a UTF-8 leader byte apart into sequence length (minus one) and code-point so far."
  (cond ((eql (logand leader #b10000000) 0)
         (values 0 leader))
        ((eql (logand leader #b11100000) #b11000000)
         (values 1 (logand leader #b00011111)))
        ((eql (logand leader #b11110000) #b11100000)
         (values 2 (logand leader #b00001111)))
        ((eql (logand leader #b11111000) #b11110000)
         (values 3 (logand leader #b00000111)))))

(defun encode-utf-8-string (sequence &optional (start 0) end (eol-style :crlf))
  (setf end (or end (length sequence)))
  (let ((bytes (make-array (- end start)
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (dotimes (i (- end start))
      (let ((c (char sequence (+ start i))))
        (cond
          ((eql c #\Newline)
           (ecase eol-style
             (:crlf
              (vector-push-extend #x0D bytes)
              (vector-push-extend #x0A bytes))
             (:lfcr
              (vector-push-extend #x0A bytes)
              (vector-push-extend #x0D bytes))
             (:lf
              (vector-push-extend #x0A bytes))
             (:cr
              (vector-push-extend #x0D bytes))))
          (t (let ((code (char-code c)))
               (unless (zerop (sys.int::char-bits c))
                 (setf code (char-code #\REPLACEMENT_CHARACTER)))
               (unless (and (<= 0 code #x1FFFFF)
                            (not (<= #xD800 code #xDFFF)))
                 (setf code (char-code #\REPLACEMENT_CHARACTER)))
               (cond ((<= code #x7F)
                      (vector-push-extend code bytes))
                     ((<= #x80 code #x7FF)
                      (vector-push-extend (logior (ash (logand code #x7C0) -6) #xC0) bytes)
                      (vector-push-extend (logior (logand code #x3F) #x80) bytes))
                     ((or (<= #x800 code #xD7FF)
                          (<= #xE000 code #xFFFF))
                      (vector-push-extend (logior (ash (logand code #xF000) -12) #xE0) bytes)
                      (vector-push-extend (logior (ash (logand code #xFC0) -6) #x80) bytes)
                      (vector-push-extend (logior (logand code #x3F) #x80) bytes))
                     ((<= #x10000 code #x10FFFF)
                      (vector-push-extend (logior (ash (logand code #x1C0000) -18) #xF0) bytes)
                      (vector-push-extend (logior (ash (logand code #x3F000) -12) #x80) bytes)
                      (vector-push-extend (logior (ash (logand code #xFC0) -6) #x80) bytes)
                      (vector-push-extend (logior (logand code #x3F) #x80) bytes))))))))
    bytes))

(defmacro with-open-network-stream ((var address port) &body body)
  `(with-open-stream (,var (mezzano.network.tcp:tcp-stream-connect ,address ,port))
     ,@body))

;;; Generics for working with packet-oriented connections.

(defgeneric send (sequence connection &optional (start 0) end))
(defgeneric receive (connection &optional timeout))
(defgeneric disconnect (connection))

;;; High-level address resolution.

(defvar *hosts* '())

(defun resolve-address (address &optional (errorp t))
  (cond ((listp address)
         (mezzano.network.ip:make-ipv4-address address))
        ((stringp address)
         (or
          ;; 1. Try to parse it as an IP address.
          (ignore-errors
            (mezzano.network.ip:make-ipv4-address address))
          ;; 2. Look in the hosts table.
          (second (assoc address *hosts* :test 'string-equal))
          ;; 3. Finally do a DNS lookup.
          (ignore-errors
            (mezzano.network.dns:resolve-address address))
          (when errorp
            (error "Unknown host ~S." address))))
        (t address)))

;;; Loopback adapter.

(defclass loopback-interface () ())
