;;; Simple remote file protocol client.

(defpackage :simple-file-client
  (:use :cl))

(in-package :simple-file-client)

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

(defclass simple-file-stream (sys.gray:fundamental-binary-input-stream
                              sys.gray:fundamental-binary-output-stream
                              file-stream)
  ((path :initarg :path :reader path)
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
   (write-buffer-offset :accessor write-buffer-offset))
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

(defvar *host-alist* '())

(defun add-simple-file-host (name address &key (port *default-simple-file-port*))
  (push (list name (make-instance 'simple-file-host
                                  :name name
                                  :address address
                                  :port port))
        *host-alist*))

(defstruct (pathname (:predicate pathnamep) (:constructor %make-pathname))
  %host %device %directory %name %type %version)

;; This should really have a host associated with it...
(defvar *default-pathname-defaults* (%make-pathname))

(defun make-pathname (&key host
                        (device nil devicep)
                        (directory nil directoryp)
                        (name nil namep)
                        (type nil typep)
                        (version nil versionp)
                        defaults)
  (if defaults
      (setf defaults (pathname defaults))
      (setf defaults (%make-pathname :%host (pathname-host *default-pathname-defaults*))))
  (%make-pathname :%host (or host (pathname-host defaults))
                  :%device (if devicep device (pathname-device defaults))
                  :%directory (if directoryp directory (pathname-directory defaults))
                  :%name (if namep name (pathname-name defaults))
                  :%type (if typep type (pathname-type defaults))
                  :%version (if versionp version (pathname-version defaults))))

(defun pathname-host (pathname &key (case :local))
  (pathname-%host (pathname pathname)))
(defun pathname-device (pathname &key (case :local))
  (pathname-%device (pathname pathname)))
(defun pathname-directory (pathname &key (case :local))
  (pathname-%directory (pathname pathname)))
(defun pathname-name (pathname &key (case :local))
  (pathname-%name (pathname pathname)))
(defun pathname-type (pathname &key (case :local))
  (pathname-%type (pathname pathname)))
(defun pathname-version (pathname &key (case :local))
  (pathname-%version (pathname pathname)))

(defun sys.int::pathnames-equal (x y)
  (and (equal (pathname-host x) (pathname-host y))
       (equal (pathname-device x) (pathname-device y))
       (equal (pathname-directory x) (pathname-directory y))
       (equal (pathname-name x) (pathname-name y))
       (equal (pathname-type x) (pathname-type y))
       (equal (pathname-version x) (pathname-version y))))

(defun pathname-match-directory (p w)
  (let ((p-dir (pathname-directory p))
        (w-dir (pathname-directory w)))
    (labels ((match (p w)
               (cond
                 ((and (null p) (null w)) t)
                 ((or (null p) (null w)) nil)
                 ((eql (first w) :wild)
                  (match (rest p) (rest w)))
                 ((eql (first w) :wild-inferiors)
                  (error "TODO: wild-inferiors"))
                 (t (and (string= (first p) (first w))
                         (match (rest p) (rest w)))))))
      (and (eql (first p-dir) (first w-dir))
           (match (rest p-dir) (rest w-dir))))))

(defun pathname-match-p (pathname wildcard)
  (let ((p (pathname pathname))
        (w (pathname wildcard)))
    (and (or (member (pathname-host w) '(nil :wild))
             (eql (pathname-host p) (pathname-host w)))
         (or (member (pathname-device w) '(nil :wild))
             (equal (pathname-device p) (pathname-device w)))
         (pathname-match-directory p w)
         (or (member (pathname-name w) '(nil :wild))
             (equal (pathname-name p) (pathname-name w)))
         (or (member (pathname-type w) '(nil :wild))
             (equal (pathname-type p) (pathname-type w)))
         (or (member (pathname-version w) '(nil :wild))
             (equal (pathname-version p) (pathname-version w))))))

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

(defgeneric unparse-pathname-directory (pathname host))

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

(defun file-namestring (pathname)
  (unparse-pathname-file pathname (pathname-host pathname)))

(defun directory-namestring (pathname)
  (unparse-pathname-directory pathname (pathname-host pathname)))

(defun namestring (pathname)
  (unparse-pathname pathname (pathname-host pathname)))

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
        ((typep pathname 'file-stream)
         (path pathname))
        (t (parse-simple-file-path (pathname-host *default-pathname-defaults*) pathname))))

(defun truename (pathname)
  (pathname pathname))

(defun buffered-format (stream control-stream &rest args)
  (write-sequence (apply 'format nil control-stream args) stream))

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
      (buffered-format con "(:PROBE ~S)~%" path)
      (setf x (read-preserving-whitespace con))
      (when (listp x)
        (ecase if-does-not-exist
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A does not exist. ~S"
                         :format-arguments (list pathname x)))
          (:create
           (setf created-file t)
           (buffered-format con "(:CREATE ~S)~%" path)
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
           (buffered-format con "(:BACKUP ~S)" path)
           (setf x (read-preserving-whitespace con))
           (when (listp x)
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "Could not rename ~S."
                    :format-arguments (list pathname))))
          (:supersede
           (buffered-format con "(:DELETE ~S)" path)
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
    (setf (write-buffer stream) (make-array 2040 :element-type '(unsigned-byte 8))
          (write-buffer-position stream) (sf-position stream)
          (write-buffer-offset stream) 0))
  (setf (aref (write-buffer stream) (write-buffer-offset stream)) byte)
  (incf (write-buffer-offset stream))
  (incf (sf-position stream)))

(defmethod close ((stream simple-file-stream) &key abort)
  (when (not abort)
    (flush-write-buffer stream))
  t)

(defun flush-write-buffer (stream)
  (when (and (write-buffer stream)
             (not (zerop (write-buffer-offset stream))))
    (with-connection (con (host stream))
      (buffered-format con "(:OPEN ~S :DIRECTION :OUTPUT :IF-DOES-NOT-EXIST :ERROR :IF-EXISTS :OVERWRITE)~%" (path stream))
      (let ((id (read-preserving-whitespace con)))
        (unless (integerp id)
          (error "Write error! ~S" id))
        (buffered-format con "(:WRITE ~D ~D ~D)~%" id (write-buffer-position stream) (write-buffer-offset stream))
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
    (buffered-format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
    (let ((id (read-preserving-whitespace con)))
      (unless (integerp id)
        (error "Read error! ~S" id))
      (buffered-format con "(:READ ~D ~D ~D)~%" id (sf-position stream) (* 32 1024))
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

(defmethod sys.gray:stream-read-byte ((stream simple-file-stream))
  (assert (member (direction stream) '(:input :io)))
  (if (refill-read-buffer stream)
      ;; Data available
      (prog1 (aref (read-buffer stream) (read-buffer-offset stream))
        (incf (read-buffer-offset stream))
        (incf (sf-position stream)))
      ;; End of file
      :eof))

(defmethod sys.gray:stream-read-sequence ((stream simple-file-stream) sequence &optional (start 0) end)
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
  (sys.gray:stream-write-byte stream (char-code char)))

;;; Explicitly fall back on the generic function so the byte read-sequence function
;;; doesn't get called.
(defmethod sys.gray:stream-read-sequence ((stream simple-file-character-stream) sequence start end)
  (if (stringp sequence)
      (sys.int::generic-read-sequence sequence stream start end)
      (call-next-method)))

(defmethod sys.gray:stream-read-char ((stream simple-file-character-stream))
  (let ((leader (read-byte stream nil)))
    (unless leader
      (return-from sys.gray:stream-read-char :eof))
    (when (eql leader #x0D)
      (read-byte stream nil)
      (setf leader #x0A))
    (multiple-value-bind (length code-point)
        (sys.net::utf-8-decode-leader leader)
      (when (null length)
        (return-from sys.gray:stream-read-char
          (code-char #xFFFE)))
      (dotimes (i length)
        (let ((byte (read-byte stream nil)))
          (when (or (null byte)
                    (/= (ldb (byte 2 6) byte) #b10))
            (return-from sys.gray:stream-read-char
              (code-char #xFFFE)))
          (setf code-point (logior (ash code-point 6)
                                   (ldb (byte 6 0) byte)))))
      (if (or (> code-point #x0010FFFF)
              (<= #xD800 code-point #xDFFF))
          (code-char #xFFFE)
          (code-char code-point)))))


(defmethod sys.gray:stream-file-position ((stream simple-file-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (when (eql position-spec :end)
           (with-connection (con (host stream))
             (buffered-format con "(:OPEN ~S :DIRECTION :INPUT)~%" (path stream))
             (let ((id (read-preserving-whitespace con)))
               (unless (integerp id)
                 (error "Read error! ~S" id))
               (buffered-format con "(:SIZE ~D)~%" id)
               (let ((file-size (read-preserving-whitespace con)))
                 (unless (integerp file-size)
                   (error "Read error! ~S" file-size))
                 (setf position-spec file-size)))))
         (setf (read-buffer stream) nil)
         (setf (sf-position stream) position-spec))
        (t (sf-position stream))))

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
    (cond ((and (pathname-directory default-pathname)
                (eql (first directory) :relative))
           (setf directory (append (pathname-directory default-pathname)
                                   (rest directory))))
          ((null directory)
           (setf directory (pathname-directory default-pathname))))
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

(defun probe-file (pathspec)
  (let ((stream (open pathspec :direction :probe)))
    (when stream
      (close stream)
      pathspec)))

(defun wild-pathname-p (pathname &optional field-key)
  (ecase field-key
    ((nil) (or (eql (pathname-host pathname) :wild)
               (eql (pathname-device pathname) :wild)
               (eql (pathname-directory pathname) :wild)
               (and (listp (pathname-directory pathname))
                    (or (find :wild (cdr (pathname-directory pathname)))
                        (find :wild-inferiors (cdr (pathname-directory pathname)))))
               (eql (pathname-name pathname) :wild)
               (eql (pathname-type pathname) :wild)
               (eql (pathname-version pathname) :wild)))
    (:host (eql (pathname-host pathname) :wild))
    (:device (eql (pathname-device pathname) :wild))
    (:directory (or (eql (pathname-directory pathname) :wild)
                    (and (listp (pathname-directory pathname))
                         (or (find :wild (cdr (pathname-directory pathname)))
                             (find :wild-inferiors (cdr (pathname-directory pathname)))))))
    (:name (eql (pathname-name pathname) :wild))
    (:type (eql (pathname-type pathname) :wild))
    (:version (eql (pathname-version pathname) :wild))))

(defun parse-namestring (thing
                         &optional
                           host
                           (default-pathname *default-pathname-defaults*)
                         &key
                           (start 0) end
                           junk-allowed)
  (when (or host
            (not (eql default-pathname *default-pathname-defaults*))
            (not (eql start 0))
            end
            junk-allowed)
    (error "TODO: host/default-pathname/start/end/junk-allowed."))
  (pathname thing))

(defun directory (pathspec &key)
  (let ((path (pathname pathspec)))
    (directory* (pathname-host path) path)))

(defgeneric directory* (host path))

(defmethod directory* ((host simple-file-host) pathname)
  (let ((path (unparse-simple-file-path pathname))
        (x nil))
    (with-connection (con host)
      (buffered-format con "(:DIRECTORY ~S)~%" path)
      (setf x (read-preserving-whitespace con))
      (unless (and (listp x) (eql (first x) :ok))
        (error 'simple-file-error
               :pathname pathname
               :format-control "Directory ~A does not exist. ~S"
               :format-arguments (list pathname x)))
      (mapcar 'pathname (rest x)))))

(defun translate-logical-pathname (pathname &key)
  (pathname pathname))

(defun translate-one (source from to what)
  (cond ((member (funcall what to) '(nil :wild))
         (funcall what source))
        ((or (eql (funcall what source) (funcall what from))
             (eql (funcall what from) :wild))
         (funcall what source))
        (t (error "Source and from ~S don't match." what))))

(defun translate-directory (source from-wildcard to-wildcard)
  (let* ((s-d (pathname-directory source))
         (f-d (pathname-directory from-wildcard))
         (t-d (pathname-directory to-wildcard))
         (new-path (list (first t-d))))
    (when (null f-d)
      (return-from translate-directory source))
    (loop ;; Match leading parts of source/from.
       (cond ((eql (first f-d) :wild)
              (error ":WILD elements in from-wildcard directory not yet supported..."))
             ((eql (first f-d) :wild-inferiors)
              (assert (null (rest f-d)) (source from-wildcard to-wildcard)
                      ":WILD-INFERIORS must be the last directory entry... (FIXME)")
              (return))
             ((and (null s-d) (null f-d))
              (return))
             ((or (null s-d) (null f-d)
                  (not (equal (first s-d) (first f-d))))
              (error "Directory entry mismatch. ~S ~S ~S ~S ~S~%"
                     (first s-d) (first f-d)
                     source from-wildcard to-wildcard)))
       (setf s-d (rest s-d)
             f-d (rest f-d)))
    ;; Merge SOURCE and TO. First component was done above.
    (do ((d (rest t-d) (cdr d)))
        ((eql (first d) :wild-inferiors)
         (assert (null (rest d))
                 (source from-wildcard to-wildcard)
                 ":WILD-INFERIORS must be the last directory entry... (FIXME)")
         (nconc (nreverse new-path) (copy-list s-d)))
      (push (first d) new-path))))

(defun translate-pathname (source from-wildcard to-wildcard &key)
  (assert (and (eql (pathname-host source) (pathname-host from-wildcard))
               (eql (pathname-host source) (pathname-host to-wildcard)))
          (source from-wildcard to-wildcard)
          "Translating between hosts not yet supported...")
  (make-pathname :host (pathname-host source)
                 :device (translate-one source from-wildcard to-wildcard 'pathname-device)
                 :name (translate-one source from-wildcard to-wildcard 'pathname-name)
                 :type (translate-one source from-wildcard to-wildcard 'pathname-type)
                 :version (translate-one source from-wildcard to-wildcard 'pathname-version)
                 :directory (translate-directory source from-wildcard to-wildcard)))

(defgeneric ensure-directories-exist* (host pathname &key verbose))

(defmethod ensure-directories-exist* ((host simple-file-host) pathname &key verbose)
  (declare (ignore host))
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
          (buffered-format con "(:DIRECTORY ~S)~%" namestring)
          (setf x (read-preserving-whitespace con))
          (when (and (listp x) (= (length x) 1) (eql (first x) :ok))
            (when verbose (format t "Creating directory ~A~%" dir-path))
            (buffered-format con "(:CREATE-DIRECTORY ~S)~%" namestring)
            (setf x (read-preserving-whitespace con))
            (unless (member x '(:ok :exists))
              (error "Cannot create ~A. ~S" namestring x))))))
    created-one))

(defun ensure-directories-exist (pathspec &rest keys &key verbose &allow-other-keys)
  (values pathspec (apply 'ensure-directories-exist* (pathname-host pathspec) (merge-pathnames pathspec) keys)))

(defgeneric rename-file* (host source dest))
(defmethod rename-file* ((host simple-file-host) source dest)
  (assert (eql (first (pathname-directory source)) :absolute) (source) "Absoute pathname required.")
  (assert (eql (first (pathname-directory dest)) :absolute) (dest) "Absoute pathname required.")
  (with-connection (con host)
    (buffered-format con "(:RENAME-FILE ~S ~S)~%"
            (unparse-simple-file-path source)
            (unparse-simple-file-path dest))
    (let ((x (read-preserving-whitespace con)))
      (unless (eql x :ok)
        (error "Could not rename ~A to ~A: ~S~%" source dest x)))))

(defun rename-file (filespec new-name)
  (let* ((source (merge-pathnames filespec))
         (dest (merge-pathnames new-name source)))
    (assert (eql (pathname-host source) (pathname-host dest))
            (filespec new-name) "Cannot rename across hosts yet.")
    (rename-file* (pathname-host source) source dest)
    (values dest source dest)))

(defgeneric file-write-date* (host path))
(defmethod file-write-date* ((host simple-file-host) path)
  (declare (ignore host))
  (assert (eql (first (pathname-directory path)) :absolute) (path) "Absoute pathname required.")
  (with-connection (con host)
    (buffered-format con "(:FILE-WRITE-DATE ~S)~%" (unparse-simple-file-path path))
    (let ((x (read-preserving-whitespace con)))
      (unless (or (integerp x) (null x))
        (error "Error: ~A ~S." path x))
      x)))

(defun file-write-date (pathspec)
  (let ((path (merge-pathnames pathspec)))
    (assert (not (wild-pathname-p path)))
    (file-write-date* (pathname-host path) path)))

(defgeneric delete-file* (host path))
(defmethod delete-file* ((host simple-file-host) path)
  (declare (ignore host))
  (assert (eql (first (pathname-directory path)) :absolute) (path) "Absoute pathname required.")
  (with-connection (con host)
    (buffered-format con "(:DELETE ~S)~%" (unparse-simple-file-path path))
    (let ((x (read-preserving-whitespace con)))
      (unless (eql x :ok)
        (error "Error: ~A ~S." path x))
      x)))

(defun delete-file (filespec)
  (let ((path (merge-pathnames filespec)))
    (assert (not (wild-pathname-p path)))
    (delete-file* (pathname-host path) path)))

(defvar *home-directory* nil)

(defun user-homedir-pathname (&optional host)
  (if host
      nil
      *home-directory*))

(add-simple-file-host :that-mac-thing '(192 168 1 13))
(setf *default-pathname-defaults* (make-pathname :host (second (first *host-alist*))
                                                 :directory '(:absolute "Users" "henry" "Documents" "LispOS")))
(setf *home-directory* (make-pathname :directory '(:absolute "Users" "henry" "Documents" "LispOS-home")))
