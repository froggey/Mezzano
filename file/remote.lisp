;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Simple remote file protocol client.

(defpackage :mezzano.file-system.remote
  (:export #:add-simple-file-host
           #:test-host-connectivity)
  (:use #:cl #:mezzano.file-system))

(in-package :mezzano.file-system.remote)

(defvar *default-simple-file-port* 2599)
(defvar *read-cache-size* (* 128 1024))
(defvar *write-cache-size* (* 128 1024))

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

(defmethod host-default-device ((host simple-file-host))
  nil)

(defclass simple-file-stream (sys.gray:fundamental-binary-input-stream
                              sys.gray:fundamental-binary-output-stream
                              file-stream)
  ((path :initarg :path :reader path)
   (pathname :initarg :pathname :reader file-stream-pathname)
   (host :initarg :host :reader host)
   (position :initarg :position :accessor sf-position)
   (direction :initarg :direction :reader direction)
   ;; Buffer itself.
   (read-buffer :initform nil :accessor read-buffer)
   ;; File position where the buffer data starts.
   (read-buffer-position :accessor read-buffer-position)
   ;; Current offset into the buffer.
   (read-buffer-offset :accessor read-buffer-offset)
   ;; Write buffer.
   (write-buffer :initform nil :accessor write-buffer)
   (write-buffer-position :accessor write-buffer-position)
   (write-buffer-offset :accessor write-buffer-offset)
   (abort-action :initarg :abort-action :accessor abort-action))
  (:default-initargs :position 0))

(defclass simple-file-character-stream (sys.gray:fundamental-character-input-stream
                                        sys.gray:fundamental-character-output-stream
                                        simple-file-stream
                                        sys.gray:unread-char-mixin)
  ())

(defmethod print-object ((object simple-file-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~A"
            (host-name (host object))
            (path object))))

(defun add-simple-file-host (name address &key (port *default-simple-file-port*))
  (setf (find-host name)
        (make-instance 'simple-file-host
                       :name (string-upcase name)
                       :address address
                       :port port)))

(defun explode (character string &optional (start 0) end)
  (setf end (or end (length string)))
  (do ((elements '())
       (i start (1+ i))
       (elt-start start))
      ((>= i end)
       (push (subseq string elt-start i) elements)
       (nreverse elements))
    (when (eql (char string i) character)
      (push (subseq string elt-start i) elements)
      (setf elt-start (1+ i)))))

(defun parse-simple-file-path (host namestring)
  (let ((start 0)
        (end (length namestring))
        (directory '())
        (name nil)
        (type nil)
        (version nil))
    (when (eql start end)
      (return-from parse-simple-file-path (make-pathname :host host)))
    (cond ((eql (char namestring start) #\/)
           (push :absolute directory)
           (incf start))
          (t (push :relative directory)))
    ;; Last element is the name.
    (do* ((x (explode #\/ namestring start end) (cdr x)))
         ((null (cdr x))
          (let* ((name-element (car x))
                 (end (length name-element)))
            (unless (zerop (length name-element))
              ;; Check for a trailing ~ indicating a backup.
              (when (and (eql (char name-element (1- end)) #\~)
                         (not (eql (length name-element) 1)))
                (decf end)
                (setf version :previous))
              ;; Find the last dot.
              (let ((dot-position (position #\. name-element :from-end t)))
                (cond ((and dot-position (not (zerop dot-position)))
                       (setf type (subseq name-element (1+ dot-position) end))
                       (setf name (subseq name-element 0 dot-position)))
                      (t (setf name (subseq name-element 0 end))))))))
      (let ((dir (car x)))
        (cond ((or (string= "" dir)
                   (string= "." dir)))
              ((string= ".." dir)
               (push :up directory))
              ((string= "*" dir)
               (push :wild directory))
              ((string= "**" dir)
               (push :wild-inferiors directory))
              (t (push dir directory)))))
    (when (string= name "*") (setf name :wild))
    (when (string= type "*") (setf type :wild))
    (when (string= version "*") (setf version :wild))
    (make-pathname :host host
                   :directory (nreverse directory)
                   :name name
                   :type type
                   :version version)))

(defmethod parse-namestring-using-host ((host simple-file-host) namestring junk-allowed)
  (when junk-allowed
    (error "TODO: Junk-allowed"))
  (parse-simple-file-path host namestring))

(defun unparse-simple-file-path (pathname)
  (let ((dir (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname))
        (version (pathname-version pathname)))
    (with-output-to-string (s)
      (when (eql (first dir) :absolute)
        (write-char #\/ s))
      (dolist (d (rest dir))
        (cond
          ((stringp d) (write-string d s))
          ((eql d :up) (write-string ".." s))
          ((eql d :wild) (write-char #\* s))
          ((eql d :wild-inferiors) (write-string "**" s))
          (t (error "Invalid directory component ~S." d)))
        (write-char #\/ s))
      (if (eql name :wild)
          (write-char #\* s)
          (write-string name s))
      (when type
        (write-char #\. s)
        (if (eql type :wild)
            (write-char #\* s)
            (write-string type s)))
      (case version
        ((nil :newest))
        (:previous
         (write-char #\~ s))
        (t (format s ".~~~D~~" version))))))

(defmethod unparse-pathname (path (host simple-file-host))
  (unparse-simple-file-path path))

(defmethod unparse-pathname-file (pathname (host simple-file-host))
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname))
        (version (pathname-version pathname)))
    (when name
      (with-output-to-string (s)
        (write-string name s)
        (when type
          (write-char #\. s)
          (write-string type s))
        (when (eql version :previous)
          (write-char #\~ s))))))

(defmethod unparse-pathname-directory (pathname (host simple-file-host))
  (let ((dir (pathname-directory pathname)))
    (with-output-to-string (s)
      (when (eql (first dir) :absolute)
        (write-char #\/ s))
      (dolist (d (rest dir))
        (cond
          ((stringp d) (write-string d s))
          ((eql d :up) (write-string ".." s))
          ((eql d :wild) (write-char #\* s))
          ((eql d :wild-inferiors) (write-string "**" s))
          (t (error "Invalid directory component ~S." d)))
        (write-char #\/ s)))))

(defmacro with-connection ((var host) &body body)
  `(sys.net::with-open-network-stream (,var (host-address ,host) (host-port ,host))
     (with-standard-io-syntax
       ,@body)))

(defmethod open-using-host ((host simple-file-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (let ((path (unparse-simple-file-path pathname))
        (x nil)
        (created-file nil)
        (abort-action nil))
    (with-connection (con host)
      (sys.net:buffered-format con "(:PROBE ~S)~%" path)
      (setf x (read-preserving-whitespace con))
      (when (listp x)
        (ecase if-does-not-exist
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A does not exist. ~S"
                         :format-arguments (list pathname x)))
          (:create
           (setf created-file t
                 abort-action :delete)
           (sys.net:buffered-format con "(:CREATE ~S)~%" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error "Cannot create ~A. ~S" pathname x)))
          ((nil) (return-from open-using-host nil))))
      (when (and (not created-file) (member direction '(:output :io)))
        (ecase if-exists
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A exists."
                         :format-arguments (list pathname)))
          ((:new-version
            :rename
            :rename-and-delete)
           (sys.net:buffered-format con "(:BACKUP ~S)" path)
           (setf abort-action :restore)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not rename ~S."
                    :format-arguments (list pathname)))
           (sys.net:buffered-format con "(:DELETE ~S)" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not supersede ~S."
                    :format-arguments (list pathname)))
           (sys.net:buffered-format con "(:CREATE ~S)~%" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error "Cannot create ~A. ~S" pathname x)))
          (:supersede
           (setf abort-action :delete)
           (sys.net:buffered-format con "(:DELETE ~S)" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not supersede ~S."
                    :format-arguments (list pathname)))
           (sys.net:buffered-format con "(:CREATE ~S)~%" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error "Cannot create ~A. ~S" pathname x)))
          ((:overwrite :append))
          ((nil) (return-from open-using-host nil))))
      (let ((stream (cond ((or (eql element-type :default)
                               (subtypep element-type 'character))
                           (assert (member external-format '(:default :utf-8))
                                   (external-format))
                           (make-instance 'simple-file-character-stream
                                          :path path
                                          :pathname pathname
                                          :host host
                                          :direction direction
                                          :abort-action abort-action))
                          ((and (subtypep element-type '(unsigned-byte 8))
                                (subtypep '(unsigned-byte 8) element-type))
                           (assert (eql external-format :default) (external-format))
                           (make-instance 'simple-file-stream
                                          :path path
                                          :pathname pathname
                                          :host host
                                          :direction direction
                                          :abort-action abort-action))
                          (t (error "Unsupported element-type ~S." element-type)))))
        (when (and (member direction '(:output :io))
                   (eql if-exists :append))
          (file-position stream :end))
        stream))))

(defmethod sys.gray:stream-element-type ((stream simple-file-stream))
  '(unsigned-byte 8))

(defmethod sys.gray:stream-element-type ((stream simple-file-character-stream))
  'character)

(defmethod sys.gray:stream-write-byte ((stream simple-file-stream) byte)
  (assert (member (direction stream) '(:io :output)))
  (setf (read-buffer stream) nil)
  (when (and (write-buffer stream)
             (>= (write-buffer-offset stream) (length (write-buffer stream))))
    (flush-write-buffer stream))
  (unless (write-buffer stream)
    (setf (write-buffer stream) (make-array *write-cache-size* :element-type '(unsigned-byte 8))
          (write-buffer-position stream) (sf-position stream)
          (write-buffer-offset stream) 0))
  (setf (aref (write-buffer stream) (write-buffer-offset stream)) byte)
  (incf (write-buffer-offset stream))
  (incf (sf-position stream)))

(defmethod close ((stream simple-file-stream) &key abort)
  (cond ((not abort)
         (flush-write-buffer stream))
        (t (when (abort-action stream)
             (with-simple-restart (continue "Ignore failure")
               (with-connection (con (host stream))
                 (sys.net:buffered-format con "(~S ~S)" (abort-action stream) (path stream))
                 (let ((x (read-preserving-whitespace con)))
                   (unless (eql x :ok)
                     (error "Error: ~A ~S." (path stream) x))
                   x))))))
  t)

(defun flush-write-buffer (stream)
  (when (and (write-buffer stream)
             (not (zerop (write-buffer-offset stream))))
    (with-connection (con (host stream))
      (sys.net:buffered-format con "(:OPEN ~S :DIRECTION :OUTPUT :IF-DOES-NOT-EXIST :ERROR :IF-EXISTS :OVERWRITE)~%" (path stream))
      (let ((id (read-preserving-whitespace con)))
        (unless (integerp id)
          (error "Write error! ~S" id))
        (sys.net:buffered-format con "(:WRITE ~D ~D ~D)~%" id (write-buffer-position stream) (write-buffer-offset stream))
        (write-sequence (write-buffer stream) con :end (write-buffer-offset stream))
        (let ((x (read-preserving-whitespace con)))
          (when (listp x)
            (error "Write error! ~S" x))
          (setf (write-buffer stream) nil))))))

(defun refill-read-buffer (stream)
  "Ensure bytes are available in the stream read buffer, returning false at end-of-file."
  (when (and (read-buffer stream)
             (<= (read-buffer-position stream)
                 (sf-position stream)
                 (+ (read-buffer-position stream)
                    (length (read-buffer stream))
                    -1)))
    ;; At least one byte is available.
    (return-from refill-read-buffer t))
  ;; Refill buffer.
  (with-connection (con (host stream))
    (sys.net:buffered-format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
    (let ((id (read-preserving-whitespace con)))
      (unless (integerp id)
        (error "Read error! ~S" id))
      (sys.net:buffered-format con "(:READ ~D ~D ~D)~%" id (sf-position stream) *read-cache-size*)
      (let ((count (read-preserving-whitespace con)))
        (unless (integerp count)
          (error "Read error! ~S" count))
        (when (eql count 0)
          ;; Nothing to read, end of file.
          (return-from refill-read-buffer nil))
        (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
          (read-line con)
          (read-sequence buffer con)
          (setf (read-buffer stream) buffer
                (read-buffer-position stream) (sf-position stream)
                (read-buffer-offset stream) 0)
          t)))))

(defun remote-read-byte (stream)
  (if (refill-read-buffer stream)
      ;; Data available
      (prog1 (aref (read-buffer stream) (read-buffer-offset stream))
        (incf (read-buffer-offset stream))
        (incf (sf-position stream)))
      ;; End of file
      :eof))

(defmethod sys.gray:stream-read-byte ((stream simple-file-stream))
  (assert (member (direction stream) '(:input :io)))
  (remote-read-byte stream))

(defmethod sys.gray:stream-read-sequence ((stream simple-file-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((bytes-read 0)
        (offset start)
        (bytes-to-go (- end start)))
    (loop
       (when (<= bytes-to-go 0)
         (return))
       (when (not (refill-read-buffer stream))
         (return))
       (let ((bytes-to-read (min (- (length (read-buffer stream))
                                    (read-buffer-offset stream))
                                 bytes-to-go)))
         (replace sequence (read-buffer stream)
                  :start1 offset
                  :start2 (read-buffer-offset stream)
                  :end2 (+ (read-buffer-offset stream) bytes-to-read))
         (incf (read-buffer-offset stream) bytes-to-read)
         (incf (sf-position stream) bytes-to-read)
         (incf bytes-read bytes-to-read)
         (incf offset bytes-to-read)
         (decf bytes-to-go bytes-to-read)))
    (+ start bytes-read)))

(defmethod sys.gray:stream-write-char ((stream simple-file-character-stream) char)
  (let ((encoded (sys.net::encode-utf-8-string (string char) 0 nil :lf)))
    (loop
       for byte across encoded
       do (sys.gray:stream-write-byte stream byte))))

(defun read-and-decode-char (stream)
  (let ((leader (remote-read-byte stream)))
    (when (eql leader :eof)
      (return-from read-and-decode-char :eof))
    (when (eql leader #x0D)
      ;; Munch CR characters.
      (return-from read-and-decode-char
        (read-and-decode-char stream)))
    (multiple-value-bind (length code-point)
        (sys.net::utf-8-decode-leader leader)
      (when (null length)
        (return-from read-and-decode-char
          #\REPLACEMENT_CHARACTER))
      (dotimes (i length)
        (let ((byte (read-byte stream nil)))
          (when (or (null byte)
                    (/= (ldb (byte 2 6) byte) #b10))
            (return-from read-and-decode-char
              #\REPLACEMENT_CHARACTER))
          (setf code-point (logior (ash code-point 6)
                                   (ldb (byte 6 0) byte)))))
      (or (and (< code-point char-code-limit)
               (code-char code-point))
          #\REPLACEMENT_CHARACTER))))

(defmethod sys.gray:stream-read-sequence ((stream simple-file-character-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (cond ((stringp sequence)
         ;; This is slightly faster than going through method dispatch, but it's not great.
         (unless end (setf end (length sequence)))
         (dotimes (i (- end start)
                   end)
           (let ((ch (read-and-decode-char stream)))
             (when (eql ch :eof)
               (return (+ start i)))
             (setf (char sequence (+ start i)) ch))))
        (t (call-next-method))))

(defmethod sys.gray:stream-read-char ((stream simple-file-character-stream))
  (assert (member (direction stream) '(:input :io)))
  (read-and-decode-char stream))

(defmethod sys.gray:stream-file-position ((stream simple-file-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (flush-write-buffer stream)
         (when (eql position-spec :end)
           (with-connection (con (host stream))
             (sys.net:buffered-format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
             (let ((id (read-preserving-whitespace con)))
               (unless (integerp id)
                 (error "Read error! ~S" id))
               (sys.net:buffered-format con "(:SIZE ~D)~%" id)
               (let ((file-size (read-preserving-whitespace con)))
                 (unless (integerp file-size)
                   (error "Read error! ~S" file-size))
                 (setf position-spec file-size)))))
         (setf (read-buffer stream) nil)
         (setf (sf-position stream) position-spec))
        (t (sf-position stream))))

(defmethod sys.gray:stream-file-length ((stream simple-file-stream))
  (with-connection (con (host stream))
    (sys.net:buffered-format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
    (let ((id (read-preserving-whitespace con)))
      (unless (integerp id)
        (error "Read error! ~S" id))
      (sys.net:buffered-format con "(:SIZE ~D)~%" id)
      (let ((file-size (read-preserving-whitespace con)))
        (unless (integerp file-size)
          (error "Read error! ~S" file-size))
        file-size))))

(defmethod directory-using-host ((host simple-file-host) pathname &key)
  (let ((path (unparse-simple-file-path pathname))
        (x nil))
    (with-connection (con host)
      (sys.net:buffered-format con "(:DIRECTORY ~S)~%" path)
      (setf x (read-preserving-whitespace con))
      (unless (and (listp x) (eql (first x) :ok))
        (error 'simple-file-error
               :pathname pathname
               :format-control "Directory ~A does not exist. ~S"
               :format-arguments (list pathname x)))
      (mapcar 'pathname (rest x)))))

(defmethod ensure-directories-exist-using-host ((host simple-file-host) pathname &key verbose)
  (let ((dirs (pathname-directory pathname))
        (created-one nil))
    (assert (eql (first dirs) :absolute) (pathname) "Absoute pathname required.")
    (with-connection (con host)
      (dotimes (i (length dirs))
        (let* ((dir-path (make-pathname :directory (subseq dirs 0 (1+ i))
                                        :name nil :type nil :version nil
                                        :defaults pathname))
               (namestring (unparse-simple-file-path dir-path))
               x)
          (sys.net:buffered-format con "(:DIRECTORY ~S)~%" namestring)
          (setf x (read-preserving-whitespace con))
          (when (and (listp x) (= (length x) 1) (eql (first x) :ok))
            (when verbose (format t "Creating directory ~A~%" dir-path))
            (sys.net:buffered-format con "(:CREATE-DIRECTORY ~S)~%" namestring)
            (setf x (read-preserving-whitespace con))
            (unless (member x '(:ok :exists))
              (error "Cannot create ~A. ~S" namestring x))))))
    created-one))

(defmethod rename-file-using-host ((host simple-file-host) source dest)
  (assert (eql (first (pathname-directory source)) :absolute) (source) "Absoute pathname required.")
  (assert (eql (first (pathname-directory dest)) :absolute) (dest) "Absoute pathname required.")
  (with-connection (con host)
    (sys.net:buffered-format con "(:RENAME-FILE ~S ~S)~%"
            (unparse-simple-file-path source)
            (unparse-simple-file-path dest))
    (let ((x (read-preserving-whitespace con)))
      (unless (eql x :ok)
        (error "Could not rename ~A to ~A: ~S~%" source dest x)))))

(defmethod file-write-date-using-host ((host simple-file-host) path)
  (assert (eql (first (pathname-directory path)) :absolute) (path) "Absoute pathname required.")
  (with-connection (con host)
    (sys.net:buffered-format con "(:FILE-WRITE-DATE ~S)~%" (unparse-simple-file-path path))
    (let ((x (read-preserving-whitespace con)))
      (unless (or (integerp x) (null x))
        (error 'simple-file-error
               :pathname path
               :format-control "Error: ~A ~S."
               :format-arguments (list path x)))
      x)))

(defmethod delete-file-using-host ((host simple-file-host) path &key)
  (assert (eql (first (pathname-directory path)) :absolute) (path) "Absoute pathname required.")
  (with-connection (con host)
    (sys.net:buffered-format con "(:DELETE ~S)~%" (unparse-simple-file-path path))
    (let ((x (read-preserving-whitespace con)))
      (unless (eql x :ok)
        (error 'simple-file-error
               :pathname path
               :format-control "Error: ~A ~S."
               :format-arguments (list path x)))
      x)))

(defmethod expunge-directory-using-host ((host simple-file-host) path &key)
  (declare (ignore host path))
  t)

(defmethod stream-truename ((stream simple-file-stream))
  (file-stream-pathname stream))

(defun test-host-connectivity (host)
  (handler-case
      (with-connection (con host)
        (sys.net:buffered-format con "(:PING)~%")
        (let ((x (read-preserving-whitespace con nil :end-of-file)))
          (unless (eql x :pong)
            (error "Invalid ping response ~S received from remote file server." x)))
        t)
    (mezzano.network.tcp:connection-error (c)
      (error "Unable to connect to file server: ~S." c))))
