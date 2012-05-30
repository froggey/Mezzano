;;; Simple remote file protocol client.

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

(defclass simple-file-stream (sys.int::stream-object file-stream)
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

(defclass character-stream-input-mixin () ())
(defclass character-stream-output-mixin () ())

(defclass simple-file-character-stream (character-stream-input-mixin
                                        character-stream-output-mixin
                                        simple-file-stream)
  ())

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

(defstruct (pathname (:predicate pathnamep))
  host device directory name type version)

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

(defun parse-simple-file-path (host namestring &optional (start 0) end)
  (setf end (or end (length namestring)))
  (when (eql start end)
    (return-from parse-simple-file-path (make-pathname :host host)))
  (let ((directory '())
        (name nil)
        (type nil)
        (version nil))
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
                (setf version :backup))
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
              (t (push dir directory)))))
    (make-pathname :host host
                   :directory (nreverse directory)
                   :name name
                   :type type
                   :version version)))

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
          (t (error "Invalid directory component ~S." d)))
        (write-char #\/ s))
      (write-string name s)
      (when type
        (write-char #\. s)
        (write-string type s))
      (when (eql version :backup)
        (write-char #\~ s)))))

(defgeneric unparse-pathname (path host))

(defmethod unparse-pathname (path (host simple-file-host))
  (unparse-simple-file-path path))

(defgeneric unparse-pathname-file (pathname host))

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
        (when (eql version :backup)
          (write-char #\~ s))))))

(defun file-namestring (pathname)
  (unparse-pathname-file pathname (pathname-host pathname)))

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
  (cond ((pathnamep pathname)
         pathname)
        (t (parse-simple-file-path (pathname-host *default-pathname-defaults*) pathname))))

(defmacro with-connection ((var host) &body body)
  `(sys.net::with-open-network-stream (,var (host-address ,host) (host-port ,host))
     ,@body))

(defgeneric open-file (host pathname &key direction element-type if-exists if-does-not-exist external-format))

(define-condition simple-file-error (file-error simple-error) ())

(defmethod open-file ((host simple-file-host) pathname
                      &key direction element-type if-exists if-does-not-exist external-format)
  (let ((path (unparse-simple-file-path pathname))
        (x nil)
        (created-file nil))
    (with-connection (con host)
      (format con "(:PROBE ~S)~%" path)
      (setf x (read-preserving-whitespace con))
      (when (listp x)
        (ecase if-does-not-exist
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A does not exist. ~S"
                         :format-arguments (list pathname x)))
          (:create
           (setf created-file t)
           (format con "(:CREATE ~S)~%" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error "Cannot create ~A. ~S" pathname x)))
          ((nil) (return-from open-file nil))))
      (when (and (not created-file) (member direction '(:output :io)))
        (ecase if-exists
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A exists."
                         :format-arguments (list pathname)))
          ((:new-version
            :rename
            :rename-and-delete)
           (format con "(:BACKUP ~S)" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not rename ~S."
                    :format-arguments (list pathname))))
          (:supersede
           (format con "(:DELETE ~S)" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not supersede ~S."
                    :format-arguments (list pathname))))
          ((:overwrite :append))
          ((nil) (return-from open-file nil))))
      (let ((stream (cond ((subtypep element-type 'character)
                           (assert (eql external-format :default) (external-format))
                           (make-instance 'simple-file-character-stream
                                          :path path
                                          :host host
                                          :direction direction))
                          ((and (subtypep element-type '(unsigned-byte 8))
                                (subtypep '(unsigned-byte 8) element-type))
                           (assert (eql external-format :default) (external-format))
                           (make-instance 'simple-file-stream
                                          :path path
                                          :host host
                                          :direction direction))
                          (t (error "Unsupported element-type ~S." element-type)))))
        (when (and (member direction '(:output :io))
                   (eql if-exists :append))
          (file-position stream :end))
        stream))))

(defmethod sys.int::stream-write-byte (byte (stream simple-file-stream))
  (assert (member (direction stream) '(:io :output)))
  (with-connection (con (host stream))
    (format con "(:OPEN ~S :DIRECTION :OUTPUT :IF-DOES-NOT-EXIST :ERROR :IF-EXISTS :OVERWRITE)~%" (path stream))
    (let ((id (read-preserving-whitespace con)))
      (unless (integerp id)
        (error "Write error! ~S" id))
      (format con "(:WRITE ~D ~D ~D)~%" id (sf-position stream) 1)
      (write-byte byte con)
      (let ((x (read-preserving-whitespace con)))
        (when (listp x)
          (error "Write error! ~S" x))
        (setf (read-buffer stream) nil)
        (incf (sf-position stream))))))

(defmethod sys.int::stream-read-byte ((stream simple-file-stream))
  (assert (member (direction stream) '(:input :io)))
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
  ;; Refill buffer.
  (with-connection (con (host stream))
    (format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
    (let ((id (read-preserving-whitespace con)))
      (unless (integerp id)
        (error "Read error! ~S" id))
      (format con "(:READ ~D ~D ~D)~%" id (sf-position stream) (* 32 1024))
      (let ((count (read-preserving-whitespace con)))
        (unless (integerp count)
          (error "Read error! ~S" count))
        (when (eql count 0)
          ;; Nothing to read, end of file.
          (return-from sys.int::stream-read-byte nil))
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

(defmethod sys.int::stream-write-char (char (stream simple-file-character-stream))
  (sys.int::stream-write-byte (char-code char) stream))

(defmethod sys.int::stream-read-char ((stream simple-file-character-stream))
  (let ((x (sys.int::stream-read-byte stream)))
    (when x
      (code-char x))))

(defmethod sys.int::stream-file-position ((stream simple-file-stream))
  (sf-position stream))

(defmethod sys.int::stream-set-file-position ((stream simple-file-stream) new-position)
  (when (eql new-position :end)
    (with-connection (con (host stream))
      (format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
      (let ((id (read-preserving-whitespace con)))
        (unless (integerp id)
          (error "Read error! ~S" id))
        (format con "(:SIZE ~D)~%" id)
        (let ((file-size (read-preserving-whitespace con)))
          (unless (integerp file-size)
            (error "Read error! ~S" file-size))
          (setf new-position file-size)))))
  (setf (sf-position stream) new-position))

(defmethod sys.int::stream-element-type* ((stream simple-file-character-stream))
  'character)

;; This should really have a host associated with it...
(defvar *default-pathname-defaults* (make-pathname))

(defun merge-pathnames (pathname &optional
                        (default-pathname *default-pathname-defaults*)
                        (default-version :newest))
  (let* ((pathname (let ((*default-pathname-defaults* default-pathname))
                     (pathname pathname)))
         (host (or (pathname-host pathname) (pathname-host default-pathname)))
         (device (pathname-device pathname))
         (directory (pathname-directory pathname))
         (name (or (pathname-name pathname) (pathname-name default-pathname)))
         (type (or (pathname-type pathname) (pathname-type default-pathname)))
         (version (or (pathname-version pathname)
                      (if (pathname-name pathname)
                          default-version
                          (pathname-version default-pathname)))))
    (when (and (not device)
               (pathname-host pathname)
               (not (pathname-device pathname)))
      (if (and (eql (pathname-host pathname) (pathname-host default-pathname)))
          (setf device (pathname-device default-pathname))
          (setf device (host-default-device host))))
    (when (and (pathname-directory default-pathname)
               (eql (first directory) :relative))
      (setf directory (append (pathname-directory default-pathname)
                              (rest directory))))
    (make-pathname :host host
                   :device device
                   :directory directory
                   :name name
                   :type type
                   :version version)))

(defun open (filespec &key
             (direction :input)
             (element-type 'character)
             (if-exists nil if-exists-p)
             (if-does-not-exist nil if-does-not-exist-p)
             (external-format :default))
  (check-type direction (member :input :output :io :probe))
  (check-type if-exists (member :error :new-version :rename :rename-and-delete :overwrite :append :supersede nil))
  (check-type if-does-not-exist (member :error :create nil))
  (let* ((path (merge-pathnames filespec))
         (host (pathname-host path)))
    (unless if-exists-p
      (setf if-exists (if (eql (pathname-version path) :newest)
                          :new-version
                          :error)))
    (unless if-does-not-exist-p
      (cond ((or (eql direction :input)
                 (eql if-exists :overwrite)
                 (eql if-exists :append))
             (setf if-does-not-exist :error))
            ((or (eql direction :output)
                 (eql direction :io))
             (setf if-does-not-exist :create))
            ((eql direction :probe)
             (setf if-does-not-exist nil))))
    (open-file host path
               :direction direction
               :element-type element-type
               :if-exists if-exists
               :if-does-not-exist if-does-not-exist
               :external-format external-format)))

(add-simple-file-host :that-mac-thing '(192 168 1 13))
(setf *default-pathname-defaults* (make-pathname :host (second (first *host-alist*))
                                                 :directory '(:absolute "Users" "henry" "Documents" "LispOS")))
