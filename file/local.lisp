;;;; Simple local file system, based on the automatic persistence system.

(defpackage :mezzano.file-system.local
  (:export #:add-local-file-host)
  (:use #:cl #:mezzano.file-system))

(in-package :mezzano.file-system.local)

(defvar *illegal-characters* ".<>\\/")

(defclass local-file-host (file-system-host)
  ((%name :initarg :name :reader host-name)
   (%root :reader local-host-root)
   (%lock :initarg :lock :reader local-host-lock))
  (:default-initargs :lock (mezzano.supervisor:make-mutex "Local File Host lock")))

(defmethod initialize-instance :after ((instance local-file-host) &key)
  (let* ((time (get-universal-time))
         (file (make-instance 'local-file
                              :truename (make-pathname :host instance
                                                       :directory '(:absolute)
                                                       :name nil
                                                       :type nil
                                                       :version nil)
                              :storage (make-array 1 :initial-element (make-hash-table :test 'equalp))
                              :plist (list :creation-date time
                                           :write-date time))))
    (setf (slot-value instance '%root) file)))

(defclass local-file ()
  ((%truename :initarg :truename :accessor file-truename)
   (%storage :initarg :storage :accessor file-storage)
   (%plist :initarg :plist :accessor file-plist)
   (%lock :initarg :lock :reader file-lock))
  (:default-initargs :plist '()
                     :lock (mezzano.supervisor:make-mutex "Local File lock")))

(defmethod print-object ((object local-file) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (file-truename object))))

(defclass local-stream (mezzano.gray:fundamental-stream file-stream)
  ((%file :initarg :file :reader local-stream-file)
   (%pathname :initarg :pathname :reader file-stream-pathname)
   (%position :initarg :position :accessor stream-position)
   (%direction :initarg :direction :reader direction)
   (%superseded-file :initarg :superseded-file :reader superseded-file))
  (:default-initargs :superseded-file nil))

(defclass local-character-stream (local-stream
                                  mezzano.gray:fundamental-character-input-stream
                                  mezzano.gray:fundamental-character-output-stream
                                  mezzano.gray:unread-char-mixin)
  ())

(defclass local-binary-stream (local-stream
                               mezzano.gray:fundamental-binary-input-stream
                               mezzano.gray:fundamental-binary-output-stream)
  ())

(defclass translating-stream (mezzano.internals::external-format-mixin
                              file-stream
                              mezzano.gray:fundamental-character-stream)
  ((%underlying-stream :initarg :underlying-stream :reader translating-underlying-stream)))

(defclass translating-input-stream (translating-stream
                                    mezzano.gray:fundamental-character-input-stream
                                    mezzano.gray:unread-char-mixin)
  ())

(defclass translating-output-stream (translating-stream
                                     mezzano.gray:fundamental-character-output-stream)
  ())

(defmacro with-host-locked ((host) &body body)
  `(mezzano.supervisor:with-mutex ((local-host-lock ,host))
     ,@body))

(defmethod print-object ((object local-file-host) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (host-name object))))

(defmethod host-default-device ((host local-file-host))
  nil)

(defun add-local-file-host (name)
  (setf (find-host name)
        (make-instance 'local-file-host
                       :name (string-upcase name))))

(defmethod parse-namestring-using-host ((host local-file-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (let ((start 0)
        (end (length namestring))
        (directory '())
        (name nil)
        (type nil)
        (version nil))
    (when (eql start end)
      (return-from parse-namestring-using-host (make-pathname :host host)))
    (cond ((eql (char namestring start) #\>)
           (push :absolute directory)
           (incf start))
          (t (push :relative directory)))
    ;; Last element is the name/type/version.
    (do* ((x (mezzano.internals::explode #\> namestring start end) (cdr x)))
         ((null (cdr x))
          (destructuring-bind (&optional name* type* version*)
              (mezzano.internals::explode #\. (car x))
            (when (zerop (length name*))
              (setf name* nil))
            (when (zerop (length type*))
              (setf type* nil))
            (when (zerop (length version*))
              (setf version* nil))
            (setf name name*
                  type type*
                  version version*)))
      (let ((dir (car x)))
        (cond ((string= "*" dir)
               (push :wild directory))
              ((string= "**" dir)
               (push :wild-inferiors directory))
              ((or (zerop (length dir))
                   (find-if (lambda (x) (find x *illegal-characters*)) dir))
               (error "Invalid directory name ~S." dir))
              (t (push dir directory)))))
    (when (string= name "*") (setf name :wild))
    (setf type (cond ((string= type "*") :wild)
                     ((string= type "") nil)
                     (t type)))
    (setf version (cond ((string-equal version "NEWEST")
                         :newest)
                        ((string= version "*")
                         :wild)
                        ((string-equal version "PREVIOUS")
                         :previous)
                        ((string-equal version "OLDEST")
                         :oldest)
                        (version
                         (parse-integer version))))
    (make-pathname :host host
                   :directory (nreverse directory)
                   :name name
                   :type type
                   :version version)))

(defmethod namestring-using-host ((host local-file-host) pathname)
  (when (pathname-device pathname)
    (error 'no-namestring-error
           :pathname pathname
           :format-control "Pathname has a device component"))
  (let ((dir (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname))
        (version (pathname-version pathname)))
    (with-output-to-string (s)
      (when (eql (first dir) :absolute)
        (write-char #\> s))
      (dolist (d (rest dir))
        (cond
          ((stringp d) (write-string d s))
          ((eql d :wild) (write-char #\* s))
          ((eql d :wild-inferiors) (write-string "**" s))
          (t
           (error 'no-namestring-error
                  :pathname pathname
                  :format-control "Invalid directory component ~S."
                  :format-arguments (list d))))
        (write-char #\> s))
      (when name
        (if (eql name :wild)
            (write-char #\* s)
            (write-string name s))
        (cond (type
               (write-char #\. s)
               (if (eql type :wild)
                   (write-char #\* s)
                   (write-string type s)))
              ((not (member version '(nil :newest)))
               (write-char #\. s)))
        (case version
          ((nil :newest))
          (:oldest
           (write-string ".oldest" s))
          (:previous
           (write-string ".previous" s))
          (:wild
           (write-string ".*" s))
          (t (format s ".~D" version)))))))

(defun file-container-key (f)
  (pathname-version (file-truename f)))

(defun read-directory-entry (dir file type &optional version)
  (let* ((name-table (aref (file-storage dir) 0))
         (container (gethash (cons file type) name-table))
         (index (version-position version container)))
    (when index
      (aref container index))))

(defmethod truename-using-host ((host local-file-host) pathname)
  (cond ((and (pathname-directory pathname)
              (not (pathname-name pathname))
              (not (pathname-type pathname)))
         ;; Special-case directory pathnames. The default implementation
         ;; uses PROBE-FILE, which tries to open the file. It's not possible
         ;; to open a directory using a #p"foo>bar>" style path.
         (cond ((or (equal '(:absolute) (pathname-directory pathname))
                    (equal '(:relative) (pathname-directory pathname)))
                pathname)
               (t
                (call-next-method host (make-pathname :directory (butlast (pathname-directory pathname))
                                                      :name (first (last (pathname-directory pathname)))
                                                      :type "directory"
                                                      :defaults pathname)))))
        (t
         (call-next-method))))

(defun next-version (name-table key)
  (let ((container (gethash key name-table)))
    (cond (container
           (1+ (pathname-version (file-truename (aref container (1- (length container)))))))
          (t
           1))))

(defun canonicalize-new-file-path (dir name type version)
  (let ((dir-truename (file-truename dir)))
    (cond ((pathname-name dir-truename)
           (make-pathname :defaults dir-truename
                          :directory (append (pathname-directory dir-truename)
                                             (list (pathname-name dir-truename)))
                          :name name
                          :type type
                          :version version))
          (t
           ;; Root dir does not have a name. Poor root directory.
           (make-pathname :defaults dir-truename
                          :directory '(:absolute)
                          :name name
                          :type type
                          :version version)))))

(defun make-file (dir truename element-type)
  (let* ((time (get-universal-time))
         (name-table (aref (file-storage dir) 0))
         (key (directory-key truename))
         (container (gethash key name-table)))
    (setf truename (canonicalize-new-file-path dir
                                               (pathname-name truename)
                                               (pathname-type truename)
                                               (if (member (pathname-version truename) '(nil :newest))
                                                   (next-version name-table key)
                                                   (pathname-version truename))))
    (let ((key (directory-key truename))
          (file (make-instance 'local-file
                               :truename truename
                               :storage (make-array 0
                                                    :element-type element-type
                                                    :adjustable t
                                                    :fill-pointer 0)
                               :plist (list :creation-date time
                                            :write-date time))))
      (cond (container
             ;; Other versions of this file exist, merge with container.
             ;; Assumes that no file with the same version exists!
             (setf (gethash key name-table) (merge 'vector
                                                   container (list file)
                                                   '<
                                                   :key 'file-container-key)))
            (t
             ;; No versions of the file exist, create a fresh container.
             (setf (gethash key name-table) (vector file))))
      file)))

(defmethod open-using-host ((host local-file-host) pathname
                            &key direction element-type if-exists if-does-not-exist external-format)
  (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Non-absolute pathname."))
  (when (eql element-type :default)
    (setf element-type 'character))
  (when (not (or (and (eql element-type 'character)
                      (eql external-format :utf-8))
                 (eql external-format :default)))
    (error "Unsupported external format ~S." external-format))
  (when (not (pathname-name pathname))
    (error 'simple-file-error
           :pathname pathname
           :format-control "I've been through the desert on a file with no name."))
  (when (and (string-equal (pathname-type pathname) "directory")
             (member direction '(:io :output)))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Cannot open directories for output."))
  (with-host-locked (host)
    (let* ((dir (do ((element (rest (pathname-directory pathname)) (cdr element))
                     (dir (local-host-root host)))
                    ((endp element)
                     dir)
                  (let ((next (read-directory-entry dir (car element) "directory")))
                    (when (not next)
                      (ecase if-does-not-exist
                        ((:error :create)
                         (error 'simple-file-error
                                :pathname pathname
                                :format-control "Subdirectory ~S does not exist."
                                :format-arguments (list element)))
                        ((nil)
                         (return-from open-using-host nil))))
                    (setf dir next))))
           (file (read-directory-entry dir (pathname-name pathname) (pathname-type pathname) (pathname-version pathname)))
           (createdp nil)
           (superseded-file nil))
      (when (not file)
        (ecase if-does-not-exist
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A does not exist."
                         :format-arguments (list pathname)))
          (:create
           (setf file (make-file dir pathname element-type)
                 createdp t))
          ((nil) (return-from open-using-host nil))))
      (when (and (not createdp)
                 (member direction '(:output :io)))
        (ecase if-exists
          (:error (error 'simple-file-error
                         :pathname pathname
                         :format-control "File ~A exists."
                         :format-arguments (list pathname)))
          (:new-version
           (setf file (make-file dir
                                 (make-pathname :version :newest
                                                :defaults pathname)
                                 element-type)))
          (:rename
           ???)
          (:rename-and-delete
           ???)
          (:supersede
           (setf superseded-file file
                 file (make-instance 'local-file
                                     :truename (file-truename file)
                                     :storage (make-array 0
                                                          :element-type element-type
                                                          :adjustable t
                                                          :fill-pointer 0))))
          ((:overwrite :append)
           ???)
          ((nil) (return-from open-using-host nil))))
      (cond ((and (not (eql direction :probe))
                  (not (equal (upgraded-array-element-type element-type)
                              (array-element-type (file-storage file)))))
             (make-translating-stream
              (make-instance (cond ((subtypep (array-element-type (file-storage file)) 'character)
                                   'local-character-stream)
                                  ((subtypep (array-element-type (file-storage file)) 'integer)
                                   'local-binary-stream)
                                  (t
                                   'local-stream))
                             :file file
                             :pathname pathname
                             :position (if (and (member direction '(:output :io))
                                                (eql if-exists :append))
                                           (length (file-storage file))
                                           0)
                             :direction direction
                             :superseded-file superseded-file)
              direction
              element-type
              external-format))
            (t
             (make-instance (cond ((subtypep element-type 'character)
                                   'local-character-stream)
                                  ((subtypep element-type 'integer)
                                   'local-binary-stream)
                                  (t
                                   'local-stream))
                            :file file
                            :pathname pathname
                            :position (if (and (member direction '(:output :io))
                                               (eql if-exists :append))
                                          (length (file-storage file))
                                          0)
                            :direction direction
                            :superseded-file superseded-file))))))

(defmethod probe-using-host ((host local-file-host) pathname)
  (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Non-absolute pathname."))
  (with-host-locked (host)
    (do ((element (rest (pathname-directory pathname)) (cdr element))
         (dir (local-host-root host)))
        ((endp element)
         (when (or (null (pathname-name pathname))
                   (and (null (pathname-type pathname))
                        (read-directory-entry
                         dir (pathname-name pathname) "directory"))
                   (read-directory-entry
                    dir
                    (pathname-name pathname)
                    (pathname-type pathname)
                    (pathname-version pathname)))
           (cond ((string= (pathname-type pathname) "directory")
                  (make-pathname :directory (append (pathname-directory pathname)
                                                    (list (pathname-name pathname)))
                                 :name nil
                                 :type nil
                                 :version :newest
                                 :defaults pathname))
                 (t
                  pathname))))
      (let ((next (read-directory-entry dir (car element) "directory")))
        (when (not next)
          ;; directory doesn't exist, return nil
          (return-from probe-using-host nil))
        (setf dir next)))))

(defun resolve-path (host pathname)
  "Return the file associated with PATH on HOST. HOST must be locked."
  (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Non-absolute pathname."))
  (when (and (not (pathname-name pathname))
             (not (pathname-type pathname))
             (rest (pathname-directory pathname)))
    (setf pathname (make-pathname :directory (butlast (pathname-directory pathname))
                                  :name (first (last (pathname-directory pathname)))
                                  :type "directory"
                                  :version :newest)))
  (do ((element (rest (pathname-directory pathname)) (cdr element))
       (dir (local-host-root host)))
      ((endp element)
       (let ((file (read-directory-entry dir (pathname-name pathname) (pathname-type pathname) (pathname-version pathname))))
         (when (not file)
           (error 'simple-file-error
                  :pathname pathname
                  :format-control "File does not exist."))
         file))
    (let ((next-dir (read-directory-entry dir (car element) "directory")))
      (when (not next-dir)
        (error 'simple-file-error
               :pathname pathname
               :format-control "File does not exist."))
      (setf dir next-dir))))

(defun get-file-properties (file)
  (mezzano.supervisor:with-mutex ((file-lock file))
    (list* :length (length (file-storage file))
           (file-plist file))))

(defun set-file-properties (file properties)
  (mezzano.supervisor:with-mutex ((file-lock file))
    (loop
      with a-o-k = (getf properties :allow-other-keys)
      for (key val) on properties by #'cddr
      unless (eql key :allow-other-keys)
      do
         (when (eql key :length)
           (unless (not a-o-k) ; ignore when a-o-k is set.
             (error "Can't set property :LENGTH")))
         (setf (getf (file-plist file) key) value))))

(defmethod file-properties-using-host ((host local-file-host) pathname)
  (with-host-locked (host)
    (get-file-properties (resolve-path host pathname))))

(defmethod set-file-properties-using-host ((host local-file-host) pathname &rest properties &key &allow-other-keys)
  (with-host-locked (host)
    (set-file-properties (resolve-path host pathname) properties)))

(defmethod file-properties-using-stream ((stream local-stream))
  (get-file-properties (local-stream-file stream)))

(defmethod set-file-properties-using-stream ((stream local-stream) &rest properties &key &allow-other-keys)
  (set-file-properties (local-stream-file stream) properties))

(defun match-version (container version)
  (let ((result '()))
    (flet ((accumulate (file exact-version)
             (let ((truename (file-truename file)))
               (cond ((string-equal (pathname-type truename) "directory")
                      ;; Add this as a directory, not a file entry.
                      (let ((truename (file-truename (aref container (1- (length container))))))
                        (push (make-pathname :directory (append (pathname-directory truename)
                                                                (list (pathname-name truename)))
                                             :name nil :type nil :version :newest
                                             :defaults truename)
                              result)))
                     (exact-version
                      (push truename result))
                     (t (push (make-pathname :version :newest
                                             :defaults truename)
                              result))))))
      (cond ((eql version :wild)
             ;; All of them.
             (dotimes (i (length container))
               (accumulate (aref container i) t)))
            (t
             (let ((index (version-position version container)))
               (when index
                 (accumulate (aref container index)
                             (not (member version '(nil :newest :unspecific)))))))))
    result))

(defun match-in-directory (directory rest-of-dir-path pathname)
  (let ((dir (aref (file-storage directory) 0)))
    (cond ((null rest-of-dir-path)
           (when (and (not (pathname-name pathname))
                      (not (pathname-type pathname)))
             (return-from match-in-directory (list pathname)))
           ;; Match name and type.
           (let ((result '()))
             (maphash (lambda (name-and-type container)
                        (when (and (or (eql (pathname-name pathname) :wild)
                                       (equalp (pathname-name pathname) (car name-and-type)))
                                   (or (eql (pathname-type pathname) :wild)
                                       (equalp (pathname-type pathname) (cdr name-and-type))))
                          (setf result (append (match-version container (pathname-version pathname))
                                               result))))
                      dir)
             result))
          ((eql (car rest-of-dir-path) :wild)
           ;; Match all subdirectories.
           (cond ((and (not (pathname-name pathname))
                       (or (not (pathname-type pathname))
                           (string-equal (pathname-type pathname) :directory)))
                  (match-in-directory directory '() (make-pathname :name :wild :type "directory" :defaults pathname)))
                 (t (let ((result '()))
                      (maphash (lambda (name-and-type container)
                                 (when (string-equal (cdr name-and-type) "directory")
                                   (setf result (append (match-in-directory (aref container (1- (length container)))
                                                                            (rest rest-of-dir-path)
                                                                            pathname)
                                                        result))))
                               dir)
                      result))))
          ((eql (car rest-of-dir-path) :wild-inferiors)
           ;; WOOO! All subdirectories!!
           (let ((result (match-in-directory directory '() pathname)))
             (maphash (lambda (name-and-type container)
                        (when (string-equal (cdr name-and-type) "directory")
                          (setf result (append (match-in-directory (aref container (1- (length container)))
                                                                   rest-of-dir-path
                                                                   pathname)
                                               result))))
                      dir)
             result))
          (t ;; Exact match (TODO: Wild strings).
           (let ((subdir (gethash (cons (first rest-of-dir-path) "directory") dir)))
             (when subdir
               (match-in-directory (aref subdir (1- (length subdir))) (rest rest-of-dir-path) pathname)))))))

(defmethod directory-using-host ((host local-file-host) pathname &key)
  (let ((dir (pathname-directory pathname)))
    (when (eql dir :wild)
      (setf dir '(:absolute :wild-inferiors)))
    (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
      (error 'simple-file-error
             :pathname pathname
             :format-control "Non-absolute pathname."))
    (when (eql (pathname-device pathname) :wild)
      (setf pathname (make-pathname :device nil
                                    :defaults pathname)))
    (let ((winf (member :wild-inferiors dir)))
      (when (cdr winf)
        ;; Implmentation limitation. FIXME...
        (error 'simple-file-error
               :pathname pathname
               :format-control ":WILD-INFERIORS must be the final directory element.")))
    (with-host-locked (host)
      (remove-duplicates (match-in-directory (local-host-root host) (cdr dir) pathname)
                         :test #'equal))))

(defmethod ensure-directories-exist-using-host ((host local-file-host) pathname &key verbose)
  (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Non-absolute pathname."))
  (dolist (entry (rest (pathname-directory pathname)))
    (when (not (stringp entry))
      (error 'simple-file-error
             :pathname pathname
             :format-control "Bad directory name ~S." entry)))
  (with-host-locked (host)
    (do ((element (rest (pathname-directory pathname)) (cdr element))
         (dir-path (make-pathname :host host :directory '(:absolute))
                   (make-pathname :directory (append (pathname-directory dir-path)
                                                     (list (car element)))
                                  :defaults dir-path))
         (dir (local-host-root host))
         (createdp nil))
        ((endp element)
         createdp)
      (let* ((name (car element))
             (existing (read-directory-entry dir name "directory")))
        (when (not existing)
          (setf createdp t)
          (let ((path (make-pathname :name name
                                     :type "directory"
                                     :version 1
                                     :defaults dir-path)))
            (setf existing (make-file dir path 't))
            (setf (file-storage existing) (make-array 1 :initial-element (make-hash-table :test 'equalp)))
            (when verbose
              (format t "Created directory ~S~%" path))))
        (setf dir existing)))))

(defun walk-directory (host pathname &key (errorp t))
  "Return the directory specified by PATHNAME.
If ERRORP is true, then a file error will be signalled if any components are missing."
  (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Non-absolute pathname."))
  (loop
     with dir = (local-host-root host)
     for element in (rest (pathname-directory pathname))
     for next-dir = (read-directory-entry dir element "directory")
     do
       (when (not next-dir)
         (if errorp
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "File does not exist.")
             (return nil)))
       (setf dir next-dir)
     finally
       (return dir)))

(defun canonicalize-directory-file-pathname (pathname)
  "Convert a pathname of the form \">foo>bar>\" to \">foo>bar.directory\""
  (if (and (not (pathname-name pathname))
           (not (pathname-type pathname))
           (rest (pathname-directory pathname)))
      (make-pathname :directory (butlast (pathname-directory pathname))
                     :name (first (last (pathname-directory pathname)))
                     :type "directory"
                     :version :newest)
      pathname))

(defun directory-key (pathname)
  (cons (pathname-name pathname) (pathname-type pathname)))

(defun version-position (version container)
  (when (or (eql version :wild)
            (not container))
    (return-from version-position nil))
  (case version
    ((:newest :unspecific nil)
     (1- (length container)))
    (:oldest
     0)
    (:previous
     (when (>= (length container) 2)
       (- (length container) 2)))
    (t
     (mezzano.internals::bsearch version container :key 'file-container-key))))

(defun remove-specific-file (name-table key container version-index)
  (cond ((eql (length container) 1)
         ;; Deleting the last version, remove the file
         ;; entirely from the directory.
         (remhash key name-table))
        (t
         (let ((new (make-array (1- (length container)))))
           (replace new container :end1 version-index)
           (replace new container :start1 version-index :start2 (1+ version-index))
           (setf (gethash key name-table) new)))))

(defmethod rename-file-using-host ((host local-file-host) source dest)
  (with-host-locked (host)
    (setf source (canonicalize-directory-file-pathname source))
    (setf dest (canonicalize-directory-file-pathname dest))
    (let* ((source-dir (walk-directory host source))
           (source-name-table (aref (file-storage source-dir) 0))
           (source-key (directory-key source))
           (source-container (gethash source-key source-name-table))
           (source-index (version-position (pathname-version source)
                                           source-container))
           (dest-dir (walk-directory host dest))
           (dest-name-table (aref (file-storage dest-dir) 0))
           (dest-key (directory-key dest)))
      ;; ###: Not sure if a specific version should be allowed here...
      (assert (member (pathname-version dest) '(:newest :unspecific nil)))
      (when (gethash dest-key dest-name-table)
        ;; ###: This should probably do something different based on the version.
        (error 'simple-file-error
               :pathname dest
               :format-control "Destination file exists."))
      (when (not source-index)
        (error 'simple-file-error
               :pathname source
               :format-control "File does not exist."))
      (let* ((file (aref source-container source-index))
             (old-truename (file-truename file))
             ;; As the destination file must not exist, the version will always be 1.
             (new-truename (canonicalize-new-file-path dest-dir (pathname-name dest) (pathname-type dest) 1)))
        ;; Remove the file from the old directory.
        (remove-specific-file source-name-table source-key source-container source-index)
        ;; Insert it into the new directory.
        (setf (gethash dest-key dest-name-table) (vector file))
        ;; Update name.
        (setf (file-truename file) new-truename)
        (values old-truename new-truename)))))

;; FIXME: Mark files deleted, let expunge actually delete them.
(defmethod delete-file-using-host ((host local-file-host) pathname &key)
  (with-host-locked (host)
    (setf pathname (canonicalize-directory-file-pathname pathname))
    (let* ((dir (walk-directory host pathname))
           (name-table (aref (file-storage dir) 0))
           (version (pathname-version pathname))
           (key (directory-key pathname))
           (container (gethash key name-table)))
      (cond ((and container
                  (eql version :wild))
             ;; Delete everything.
             (remhash key name-table))
            (t
             (let ((index (version-position version container)))
               (when (not index)
                 (error 'simple-file-error
                        :pathname pathname
                        :format-control "File does not exist."))
               (remove-specific-file name-table key container index)))))))

#|
           #:expunge-directory-using-host
|#

(defmethod mezzano.gray:stream-write-char ((stream local-stream) character)
  (check-type (direction stream) (member :io :output))
  (let ((file (local-stream-file stream)))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (when (> (stream-position stream)
               (length (file-storage file)))
        ;; Expand file.
        (adjust-array (file-storage file) (stream-position stream)))
      (cond ((eql (length (file-storage file))
                  (stream-position stream))
             (vector-push-extend character (file-storage file)))
            (t
             (setf (aref (file-storage file) (stream-position stream)) character)))
      (incf (stream-position stream))
      (setf (getf (file-plist file) :write-date) (get-universal-time)))))

(defmethod mezzano.gray:stream-read-char ((stream local-stream))
  (check-type (direction stream) (member :io :input))
  (let ((file (local-stream-file stream)))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (cond ((< (stream-position stream)
                (length (file-storage file)))
             (prog1 (aref (file-storage file) (stream-position stream))
               (incf (stream-position stream))))
            (t :eof)))))

(defmethod mezzano.gray:stream-write-byte ((stream local-stream) byte)
  (check-type (direction stream) (member :io :output))
  (let ((file (local-stream-file stream)))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (when (> (stream-position stream)
               (length (file-storage file)))
        ;; Expand file.
        (adjust-array (file-storage file) (stream-position stream)))
      (cond ((eql (length (file-storage file))
                  (stream-position stream))
             (vector-push-extend byte (file-storage file)))
            (t
             (setf (aref (file-storage file) (stream-position stream)) byte)))
      (incf (stream-position stream))
      (setf (getf (file-plist file) :write-date) (get-universal-time)))))

(defmethod mezzano.gray:stream-read-byte ((stream local-stream))
  (check-type (direction stream) (member :io :input))
  (let ((file (local-stream-file stream)))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (cond ((< (stream-position stream)
                (length (file-storage file)))
             (prog1 (aref (file-storage file) (stream-position stream))
               (incf (stream-position stream))))
            (t :eof)))))

(defmethod mezzano.gray:stream-write-sequence ((stream local-stream) sequence &optional (start 0) end)
  (check-type (direction stream) (member :io :output))
  (let ((file (local-stream-file stream))
        (end (or end (length sequence))))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (when (> (+ (stream-position stream)
                  (- end start))
               (length (file-storage file)))
        ;; Expand file.
        (adjust-array (file-storage file)
                      (+ (stream-position stream)
                         (- end start))
                      :fill-pointer t))
      (replace (file-storage file) sequence
               :start1 (stream-position stream)
               :end1 (+ (stream-position stream) (- end start))
               :start2 start
               :end2 end)
      (incf (stream-position stream) (- end start))
      (setf (getf (file-plist file) :write-date) (get-universal-time)))))

(defmethod mezzano.gray:stream-read-sequence ((stream local-stream) sequence &optional (start 0) end)
  (check-type (direction stream) (member :io :input))
  (let ((file (local-stream-file stream))
        (end (or end (length sequence))))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (let* ((pos (stream-position stream))
             (storage (file-storage file))
             (file-size (length storage))
             (requested-n-bytes (- end start))
             (n-bytes (max 0 (min requested-n-bytes (- file-size pos)))))
        (replace sequence storage
                 :start1 start
                 :start2 pos
                 :end2 (+ pos n-bytes))
        (incf (stream-position stream) n-bytes)
        (+ start n-bytes)))))

(defmethod mezzano.gray:stream-element-type ((stream local-stream))
  (array-element-type (file-storage (local-stream-file stream))))

(defmethod mezzano.gray:stream-external-format ((stream local-stream))
  :default)

(defmethod input-stream-p ((stream local-stream))
  (member (direction stream) '(:input :io)))

(defmethod output-stream-p ((stream local-stream))
  (member (direction stream) '(:output :io)))

(defmethod mezzano.gray:stream-file-length ((stream local-stream))
  (length (file-storage (local-stream-file stream))))

(defmethod mezzano.gray:stream-file-position ((stream local-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (setf (stream-position stream) (case position-spec
                                          (:start 0)
                                          (:end (length (file-storage (local-stream-file stream))))
                                          (t position-spec))))
        (t (stream-position stream))))

(defmethod mezzano.gray:stream-line-column ((stream local-stream))
  nil)

(defmethod mezzano.gray:stream-line-length ((stream local-stream))
  nil)

(defmethod mezzano.gray:stream-clear-output ((stream local-stream))
  nil)

(defmethod mezzano.gray:stream-finish-output ((stream local-stream))
  nil)

(defmethod mezzano.gray:stream-force-output ((stream local-stream))
  nil)

(defmethod close ((stream local-stream) &key abort &allow-other-keys)
  (call-next-method)
  (when (and (not abort)
             (superseded-file stream))
    ;; Replace the superseded file's contents.
    (let ((file (superseded-file stream))
          (time (get-universal-time)))
      (mezzano.supervisor:with-mutex ((file-lock file))
        (setf (file-storage file) (file-storage (local-stream-file stream)))
        ;; FIXME: This needs to merge the two plists together.
        (setf (getf (file-plist file) :creation-date) time
              (getf (file-plist file) :write-date) time))))
  t)

(defmethod stream-truename ((stream local-stream))
  (let ((truename (file-truename (local-stream-file stream))))
    (cond ((string= (pathname-type truename) "directory")
           (make-pathname :directory (append (pathname-directory truename)
                                             (list (pathname-name truename)))
                          :name nil
                          :type nil
                          :defaults truename))
          (t
           truename))))

(defun make-translating-stream (underlying-stream direction element-type external-format)
  (when (eql direction :io)
    (error "Element translation not supported with :IO direction"))
  (when (not (equal (stream-element-type underlying-stream) '(unsigned-byte 8)))
    (error "Underlying stream must have octet type, not type ~S"
           (stream-element-type underlying-stream)))
  (when (not (and (subtypep element-type 'character)
                  (subtypep 'character element-type)))
    (error "Translation element-type must be CHARACTER, not ~S" element-type))
  (make-instance (ecase direction
                   (:input 'translating-input-stream)
                   (:output 'translating-output-stream))
                 :underlying-stream underlying-stream
                 :external-format (mezzano.internals::make-external-format 'character external-format)))

(defmethod print-object ((instance translating-stream) stream)
  (print-unreadable-object (instance stream :type t)
    (format stream "for ~S" (translating-underlying-stream instance))))

(defmethod close ((stream translating-stream) &rest keys)
  (apply #'close (translating-underlying-stream stream) keys))

(defmethod mezzano.gray:stream-file-position ((stream translating-stream) &optional (position-spec nil position-spec-p))
  (if position-spec-p
      (file-position (translating-underlying-stream stream) position-spec)
      (file-position (translating-underlying-stream stream))))

(defmethod mezzano.gray:stream-file-length ((stream translating-stream))
  (file-length (translating-underlying-stream stream)))

(defmethod stream-truename ((stream translating-stream))
  (stream-truename (translating-underlying-stream stream)))

(defmethod file-stream-pathname ((stream translating-stream))
  (file-stream-pathname (translating-underlying-stream stream)))

(defmethod mezzano.gray:stream-clear-input ((stream translating-input-stream))
  (clear-input (translating-underlying-stream stream)))

(defmethod mezzano.gray:stream-read-byte ((stream translating-input-stream))
  (read-byte (translating-underlying-stream stream) nil :eof))
