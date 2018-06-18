;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Simple remote file protocol client.

(defpackage :mezzano.file-system.remote
  (:export #:add-remote-file-host
           #:test-host-connectivity)
  (:use #:cl #:mezzano.file-system)
  (:local-nicknames (:gray :mezzano.gray))
  (:import-from :sys.int
                #:explode))

(in-package :mezzano.file-system.remote)

(defvar *default-remote-file-port* 2599)
(defvar *cache-size* (* 512 1024))

(defclass remote-file-host ()
  ((%name :initarg :name :reader host-name)
   (%address :initarg :address :reader host-address)
   (%port :initarg :port :reader host-port))
  (:default-initargs :port *default-remote-file-port*))

(defmethod print-object ((object remote-file-host) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S:~S"
            (host-name object)
            (host-address object)
            (host-port object))))

(defmethod host-default-device ((host remote-file-host))
  nil)

(defclass remote-file-stream (gray:fundamental-binary-input-stream
                              gray:fundamental-binary-output-stream
                              file-stream)
  ((%path :initarg :path :reader path)
   (%pathname :initarg :pathname :reader file-stream-pathname)
   (%host :initarg :host :reader host)
   (%position :initarg :position :accessor file-position*)
   (%length :initarg :length :accessor file-length*)
   (%direction :initarg :direction :reader direction)
   ;; The buffer.
   (%buffer :initform nil :accessor buffer)
   ;; File position where the buffer data starts.
   (%buffer-position :accessor buffer-position)
   ;; True when the buffer has been written to.
   (%buffer-dirty-b :initform nil :accessor buffer-dirty-p)
   (%abort-action :initarg :abort-action :accessor abort-action))
  (:default-initargs :position 0))

(defclass remote-file-character-stream (gray:fundamental-character-input-stream
                                        gray:fundamental-character-output-stream
                                        remote-file-stream
                                        gray:unread-char-mixin)
  ())

(defmethod print-object ((object remote-file-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~A"
            (host-name (host object))
            (path object))))

(defun add-remote-file-host (name address &key (port *default-remote-file-port*))
  (setf (find-host name)
        (make-instance 'remote-file-host
                       :name (string-upcase name)
                       :address address
                       :port port)))

(defun parse-remote-file-path (host namestring)
  (let ((start 0)
        (end (length namestring))
        (directory '())
        (name nil)
        (type nil)
        (version nil))
    (when (eql start end)
      (return-from parse-remote-file-path (make-pathname :host host)))
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

(defmethod parse-namestring-using-host ((host remote-file-host) namestring junk-allowed)
  (when junk-allowed
    (error "TODO: Junk-allowed"))
  (parse-remote-file-path host namestring))

(defun unparse-remote-file-path (pathname)
  (when (pathname-device pathname)
    (error 'no-namestring-error
           :pathname pathname
           :format-control "Pathname has a device component"))
  (let ((dir (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname))
        (version (pathname-version pathname)))
    (with-output-to-string (s)
      (when (eql dir :wild)
        (setf dir '(:absolute :wild-inferiors)))
      (when (stringp dir)
        (setf dir `(:absolute ,dir)))
      (when (eql dir :unspecific)
        (setf dir nil))
      (when (eql (first dir) :absolute)
        (write-char #\/ s))
      (dolist (d (rest dir))
        (cond
          ((stringp d) (write-string d s))
          ((eql d :up) (write-string ".." s))
          ((eql d :wild) (write-char #\* s))
          ((eql d :wild-inferiors) (write-string "**" s))
          (t
           (error 'no-namestring-error
                  :pathname pathname
                  :format-control "Invalid directory component ~S."
                  :format-arguments (list d))))
        (write-char #\/ s))
      (cond ((eql name :wild)
             (write-char #\* s))
            (name
             (write-string name s)))
      (when type
        (write-char #\. s)
        (if (eql type :wild)
            (write-char #\* s)
            (write-string type s)))
      (case version
        ((nil :newest :wild))
        (:previous
         (write-char #\~ s))
        (t (format s ".~~~D~~" version))))))

(defmethod namestring-using-host ((host remote-file-host) path)
  (unparse-remote-file-path path))

(defmacro with-connection ((var host) &body body)
  `(sys.net::with-open-network-stream (,var (host-address ,host) (host-port ,host))
     ,@body))

(defmethod open-using-host ((host remote-file-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (let ((path (unparse-remote-file-path pathname))
        (x nil)
        (created-file nil)
        (abort-action nil)
        (size nil))
    (with-connection (con host)
      (when (not (eql (ignore-errors (command pathname con `(:probe ,path))) :ok))
        (ecase if-does-not-exist
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A does not exist. ~S"
                         :format-arguments (list pathname x)))
          (:create
           (setf created-file t
                 abort-action :delete
                 size 0)
           (command pathname con `(:create ,path)))
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
           (command pathname con `(:backup ,path))
           (setf abort-action :restore)
           (command pathname con `(:delete ,path))
           (command pathname con `(:create ,path))
           (setf size 0))
          (:supersede
           (setf abort-action :delete)
           (command pathname con `(:delete ,path))
           (command pathname con `(:create ,path))
           (setf size 0))
          ((:overwrite :append))
          ((nil) (return-from open-using-host nil))))
      (when (not size)
        (let ((id (command pathname con `(:open ,path :direction :input))))
          (setf size (command pathname con `(:size ,id))))))
    (let ((stream (cond ((or (eql element-type :default)
                             (subtypep element-type 'character))
                         (assert (member external-format '(:default :utf-8))
                                 (external-format))
                         (make-instance 'remote-file-character-stream
                                        :path path
                                        :pathname pathname
                                        :length size
                                        :host host
                                        :direction direction
                                        :abort-action abort-action))
                        ((and (subtypep element-type '(unsigned-byte 8))
                              (subtypep '(unsigned-byte 8) element-type))
                         (assert (eql external-format :default) (external-format))
                         (make-instance 'remote-file-stream
                                        :path path
                                        :pathname pathname
                                        :length size
                                        :host host
                                        :direction direction
                                        :abort-action abort-action))
                        (t (error "Unsupported element-type ~S." element-type)))))
      (when (and (member direction '(:output :io))
                 (eql if-exists :append))
        (file-position stream :end))
      stream)))

(defmethod probe-using-host ((host remote-file-host) pathname)
  (let ((path (unparse-remote-file-path pathname)))
    (with-connection (con host)
      (when (eql (ignore-errors (command pathname con `(:probe ,path))) :ok)
        pathname))))

(defmethod gray:stream-element-type ((stream remote-file-stream))
  '(unsigned-byte 8))

(defmethod gray:stream-element-type ((stream remote-file-character-stream))
  'character)

(defmethod gray:stream-external-format ((stream remote-file-stream))
  :default)

(defmethod gray:stream-external-format ((stream remote-file-character-stream))
  :utf-8)

(defmethod input-stream-p ((stream remote-file-stream))
  (member (direction stream) '(:input :io)))

(defmethod output-stream-p ((stream remote-file-stream))
  (member (direction stream) '(:output :io)))

(defun command (pathname connection command &optional payload (start 0) end)
  (with-standard-io-syntax
    (sys.net:buffered-format connection "~S~%" command))
  (when payload
    (write-sequence payload connection :start start :end end))
  (let ((result (with-standard-io-syntax
                  (read-preserving-whitespace connection))))
    (when (and (listp result)
               (eql (first result) :error))
      (error 'simple-file-error
             :pathname (pathname pathname)
             :format-control "Remote error: ~S. Command: ~S~%"
             :format-arguments (list result command)))
    result))

(defun flush-buffer (stream)
  (when (and (buffer stream)
             (buffer-dirty-p stream))
    ;; Write data back.
    (with-connection (con (host stream))
      (let ((id (command stream con
                         `(:open ,(path stream) :direction :output :if-does-not-exist :error :if-exists :overwrite))))
        (command stream con
                 `(:write ,id ,(buffer-position stream) ,(length (buffer stream)))
                 (buffer stream)))))
  (setf (buffer-dirty-p stream) nil
        (buffer stream) nil)
  nil)

(defmethod gray:stream-clear-input ((stream remote-file-stream))
  (flush-buffer stream))

(defmethod gray:stream-clear-output ((stream remote-file-stream))
  ;; Not sure if this is sensible.
  (setf (buffer-dirty-p stream) nil)
  (flush-buffer stream))

(defmethod gray:stream-force-output ((stream remote-file-stream))
  (flush-buffer stream))

(defmethod gray:stream-finish-output ((stream remote-file-stream))
  (flush-buffer stream))

(defun maybe-write-byte (stream byte)
  (cond ((not (buffer stream))
         nil)
        (t
         (let ((write-position (- (file-position* stream) (buffer-position stream))))
           (cond ((eql write-position (length (buffer stream)))
                  (vector-push byte (buffer stream)))
                 ((and (<= 0 write-position)
                       (< write-position (length (buffer stream))))
                  (setf (aref (buffer stream) write-position) byte)
                  t)
                 (t nil))))))

(defmethod gray:stream-write-byte ((stream remote-file-stream) byte)
  (assert (member (direction stream) '(:io :output)))
  (when (not (maybe-write-byte stream byte))
    ;; No buffer, buffer full or buffer in the wrong place. Flush and retry.
    (flush-buffer stream)
    (setf (buffer-position stream) (file-position* stream))
    (setf (buffer stream) (make-array *cache-size* :element-type '(unsigned-byte 8) :fill-pointer 1))
    (setf (aref (buffer stream) 0) byte))
  (setf (buffer-dirty-p stream) t)
  (incf (file-position* stream))
  (setf (file-length* stream) (max (file-position* stream)
                                   (file-length* stream)))
  byte)

(defmethod close ((stream remote-file-stream) &key abort)
  (call-next-method)
  (cond ((not abort)
         (flush-buffer stream))
        (t
         (when (abort-action stream)
           (with-connection (con (host stream))
             (command stream con (list (abort-action stream) (path stream)))))))
  t)

(defun refill-buffer (stream)
  "Ensure the buffer is positioned at the current file position.
The file position must be less than the file length."
  (assert (< (file-position* stream) (file-length* stream)))
  (when (and (buffer stream)
             (<= (buffer-position stream) (file-position* stream))
             (< (file-position* stream) (+ (buffer-position stream) (length (buffer stream)))))
    ;; File position is within the buffer.
    (return-from refill-buffer t))
  (flush-buffer stream)
  (let* ((bytes-to-read (min (- (file-length* stream) (file-position* stream))
                             *cache-size*))
         (buffer (make-array *cache-size* :element-type '(unsigned-byte 8) :fill-pointer bytes-to-read)))
    (with-connection (con (host stream))
      (let* ((id (command stream con
                         `(:open ,(path stream) :direction :input)))
             (count (command stream con
                             `(:read ,id ,(file-position* stream) ,bytes-to-read))))
        (assert (eql count bytes-to-read))
        (read-sequence buffer con)))
    (setf (buffer-position stream) (file-position* stream))
    (setf (buffer stream) buffer)))

(defmethod gray:stream-read-byte ((stream remote-file-stream))
  (assert (member (direction stream) '(:input :io)))
  (cond ((>= (file-position* stream) (file-length* stream))
         :eof)
        (t
         (refill-buffer stream)
         (prog1 (aref (buffer stream) (- (file-position* stream) (buffer-position stream)))
           (incf (file-position* stream))))))

(defmethod gray:stream-read-sequence ((stream remote-file-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((bytes-read 0)
        (offset start)
        (bytes-to-go (- end start)))
    (loop
       (when (<= bytes-to-go 0)
         (return))
       (when (>= (file-position* stream) (file-length* stream))
         (return))
       (refill-buffer stream)
       (let* ((buffer-offset (- (file-position* stream) (buffer-position stream)))
              (bytes-to-read (min (- (length (buffer stream)) buffer-offset)
                                 bytes-to-go)))
         (replace sequence (buffer stream)
                  :start1 offset
                  :start2 buffer-offset
                  :end2 (+ buffer-offset bytes-to-read))
         (incf (file-position* stream) bytes-to-read)
         (incf bytes-read bytes-to-read)
         (incf offset bytes-to-read)
         (decf bytes-to-go bytes-to-read)))
    (+ start bytes-read)))

(defmethod gray:stream-write-char ((stream remote-file-character-stream) char)
  (let ((encoded (sys.net::encode-utf-8-string (string char) 0 nil :lf)))
    (loop
       for byte across encoded
       do (gray:stream-write-byte stream byte))))

(defun read-and-decode-char (stream)
  (let ((leader (read-byte stream nil :eof)))
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

(defmethod gray:stream-read-sequence ((stream remote-file-character-stream) sequence &optional (start 0) end)
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

(defmethod gray:stream-read-char ((stream remote-file-character-stream))
  (assert (member (direction stream) '(:input :io)))
  (read-and-decode-char stream))

(defmethod gray:stream-file-position ((stream remote-file-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (when (eql position-spec :start)
           (setf position-spec 0))
         (when (eql position-spec :end)
           (setf position-spec (file-length* stream)))
         (assert (<= 0 position-spec))
         (when (eql (direction stream) :input)
           (assert (<= position-spec (file-length* stream))))
         (setf (file-position* stream) position-spec))
        (t
         (file-position* stream))))

(defmethod gray:stream-file-length ((stream remote-file-stream))
  (file-length* stream))

(defmethod directory-using-host ((host remote-file-host) pathname &key)
  (when (eql (pathname-device pathname) :wild)
    (setf pathname (make-pathname :device nil
                                  :defaults pathname)))
  (let ((path (unparse-remote-file-path pathname)))
    (with-connection (con host)
      (loop
         for p in (rest (command pathname con `(:directory ,path)))
         collect (parse-namestring p host)))))

(defmethod ensure-directories-exist-using-host ((host remote-file-host) pathname &key verbose)
  (let ((dirs (pathname-directory pathname))
        (created-one nil))
    (assert (eql (first dirs) :absolute) (pathname) "Absoute pathname required.")
    (with-connection (con host)
      (dotimes (i (length dirs))
        (let* ((dir-path (make-pathname :directory (subseq dirs 0 (1+ i))
                                        :name nil :type nil :version nil
                                        :defaults pathname))
               (namestring (unparse-remote-file-path dir-path)))
          (when (endp (rest (command pathname con `(:directory ,namestring))))
            (when verbose (format t "Creating directory ~A~%" dir-path))
            (setf created-one t)
            (command pathname con `(:create-directory ,namestring))))))
    created-one))

(defmethod rename-file-using-host ((host remote-file-host) source dest)
  (assert (eql (first (pathname-directory source)) :absolute) (source) "Absoute pathname required.")
  (assert (eql (first (pathname-directory dest)) :absolute) (dest) "Absoute pathname required.")
  (with-connection (con host)
    (command source con `(:rename-file ,(unparse-remote-file-path source)
                                       ,(unparse-remote-file-path dest)))))

(defmethod file-write-date-using-host ((host remote-file-host) path)
  (assert (eql (first (pathname-directory path)) :absolute) (path) "Absoute pathname required.")
  (with-connection (con host)
    (command path con `(:file-write-date ,(unparse-remote-file-path path)))))

(defmethod delete-file-using-host ((host remote-file-host) path &key)
  (assert (eql (first (pathname-directory path)) :absolute) (path) "Absoute pathname required.")
  (with-connection (con host)
    (command path con `(:delete ,(unparse-remote-file-path path)))))

(defmethod expunge-directory-using-host ((host remote-file-host) path &key)
  (declare (ignore host path))
  t)

(defmethod stream-truename ((stream remote-file-stream))
  (file-stream-pathname stream))

(defun test-host-connectivity (host)
  (handler-case
      (with-connection (con host)
        (with-standard-io-syntax
          (sys.net:buffered-format con "(:PING)~%"))
        (let ((x (with-standard-io-syntax
                   (read-preserving-whitespace con nil :end-of-file))))
          (unless (eql x :pong)
            (error "Invalid ping response ~S received from remote file server." x)))
        t)
    (mezzano.network.tcp:connection-error (c)
      (error "Unable to connect to file server: ~S." c))))
