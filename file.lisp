;; (defhost :host :simple-file "192.168.1.13") -> :HOST
;; (open "host:/Users/henry/.lispos.lisp") -> #<simple-file-stream "host:/Users/henry/.lispos.lisp" 1234>

(defpackage #:simple-file-client
  (:use #:cl))

(in-package #:simple-file-client)

(defvar *default-simple-file-port* 2599)

(defclass simple-file-host ()
  ((name :initarg :name :reader host-name)
   (address :initarg :address :reader host-address)
   (port :initarg :port :reader host-port))
  (:default-initargs :port *default-simple-file-port*))

(defmethod print-object ((object simple-file-host) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S:~S"
            (host-name object)
            (host-address object)
            (host-port object))))

(defclass simple-file-stream (sys.int::stream-object)
  ((path :initarg :path :reader path)
   (host :initarg :host :reader host)
   (position :initarg :position :accessor sf-position)
   (direction :initarg :direction :reader direction)
   ;; Buffer itself.
   (read-buffer :initform nil :accessor read-buffer)
   ;; File position where the buffer data starts.
   (read-buffer-position :accessor read-buffer-position)
   ;; Current offset into the buffer.
   (read-buffer-offset :accessor read-buffer-offset))
  (:default-initargs :position 0))

(defmethod print-object ((object simple-file-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~A"
            (host-name (host object))
            (path object))))

(defvar *host-alist* '())

(defun add-simple-file-host (name address &key (port *default-simple-file-port*))
  (push (list name (make-instance 'simple-file-host
                                  :name name
                                  :address address
                                  :port port))
        *host-alist*))

(defstruct pathname
  host device directory name type version)

(defun unparse-simple-file-path (pathname)
  (let ((dir (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname)))
    (with-output-to-string (s)
      (when (eql (first dir) :absolute)
        (write-char #\/ s))
      (dolist (d (rest dir))
        (cond
          ((stringp d) (write-string d s))
          ((eql d :up) (write-string ".." s))
          (t (error "Invalid directory component ~S." d)))
        (write-char #\/ s))
      (write-string name s)
      (when type
        (write-char #\. s)
        (write-string type s)))))

(defgeneric unparse-pathname (path host))

(defmethod unparse-pathname (path (host simple-file-host))
  (unparse-simple-file-path path))

(defmethod print-object ((object pathname) stream)
  (cond ((pathname-host object)
         (format stream "#P~S" (concatenate 'string
                                            (string (host-name (pathname-host object)))
                                            ":"
                                            (unparse-pathname object (pathname-host object)))))
        (t (print-unreadable-object (object stream :type t)
             (format stream ":HOST ~S :DEVICE ~S :DIRECTORY ~S :NAME ~S :TYPE ~S :VERSION ~S"
                     (pathname-host object) (pathname-device object)
                     (pathname-directory object) (pathname-name object)
                     (pathname-type object) (pathname-version object))))))

(defun pathname (pathname)
  (check-type pathname pathname)
  pathname)

(defun open-simple-file (pathspec direction)
  (let* ((p (pathname pathspec)))
    ;; Should do a test open here...
    (make-instance 'simple-file-stream
                   :path (unparse-simple-file-path p)
                   :host (pathname-host p)
                   :direction direction)))

(defmacro with-connection ((var host) &body body)
  `(sys.net::with-open-network-stream (,var (host-address ,host) (host-port ,host))
     ,@body))

(defmethod sys.int::stream-read-byte ((stream simple-file-stream))
  (when (and (read-buffer stream)
             (<= (read-buffer-position stream)
                 (sf-position stream)
                 (+ (read-buffer-position stream)
                    (length (read-buffer stream))
                    -1)))
    (return-from sys.int::stream-read-byte
      (prog1 (aref (read-buffer stream) (read-buffer-offset stream))
        (incf (read-buffer-offset stream))
        (incf (sf-position stream)))))
  (with-connection (con (host stream))
    (format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
    (let ((id (read-preserving-whitespace con)))
      (unless (integerp id)
        (error "Read error! ~S" id))
      (format con "(:READ ~D ~D ~D)~%" id (sf-position stream) (* 32 1024))
      (let ((count (read-preserving-whitespace con)))
        (unless (integerp count)
          (error "Read error! ~S" count))
        (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
          (read-line con)
          (read-sequence buffer con)
          (setf (read-buffer stream) buffer
                (read-buffer-position stream) (sf-position stream)
                (read-buffer-offset stream) 1)
          (incf (sf-position stream))
          (aref buffer 0))))))

(defmethod sys.int::stream-element-type* ((stream simple-file-stream))
  '(unsigned-byte 8))

(add-simple-file-host :host '(192 168 1 13))

(defun test* ()
  (open-simple-file
   (make-pathname :host (second (first *host-alist*))
                  :directory '(:absolute "Users" "henry" "Documents" "LispOS")
                  :name "file"
                  :type "lisp")
   :input))