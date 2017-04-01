;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Simple local file system, based on the automatic persistence system.

(defpackage :mezzano.file-system.local
  (:export #:add-local-file-host)
  (:use #:cl #:mezzano.file-system))

(in-package :mezzano.file-system.local)

(defvar *illegal-characters* ".<>\\/")

(defclass local-file-host ()
  ((%name :initarg :name :reader host-name)
   (%root :reader local-host-root)
   (%lock :initarg :lock :reader local-host-lock))
  (:default-initargs :lock (mezzano.supervisor:make-mutex "Local File Host lock")))

(defmethod initialize-instance :after ((instance local-file-host) &key &allow-other-keys)
  (let* ((time (get-universal-time))
         (file (make-instance 'local-file
                              :truename (make-pathname :host instance
                                                       :directory '(:absolute)
                                                       :name nil
                                                       :type nil
                                                       :version nil)
                              :storage (make-array 1 :initial-element (make-hash-table :test 'equalp))
                              :plist (list :creation-time time
                                           :write-time time))))
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

(defclass local-stream (file-stream sys.gray:fundamental-stream sys.gray:unread-char-mixin)
  ((%file :initarg :file :reader local-stream-file)
   (%pathname :initarg :pathname :reader file-stream-pathname)
   (%position :initarg :position :accessor stream-position)
   (%direction :initarg :direction :reader direction)
   (%superseded-file :initarg :superseded-file :reader superseded-file))
  (:default-initargs :superseded-file nil))

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
    (do* ((x (explode #\> namestring start end) (cdr x)))
         ((null (cdr x))
          (destructuring-bind (&optional name* type* version*)
              (explode #\. (car x))
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
    (setf version (cond ((or (not version)
                             (string-equal version "NEWEST"))
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

(defmethod unparse-pathname (pathname (host local-file-host))
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
          (t (error "Invalid directory component ~S." d)))
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

(defmethod unparse-pathname-file (pathname (host local-file-host))
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname))
        (version (pathname-version pathname)))
    (when name
      (with-output-to-string (s)
        (when name
          (write-string name s)
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
            (t (format s ".~D" version))))))))

(defmethod unparse-pathname-directory (pathname (host local-file-host))
  (let ((dir (pathname-directory pathname)))
    (with-output-to-string (s)
      (when (eql (first dir) :absolute)
        (write-char #\> s))
      (dolist (d (rest dir))
        (cond
          ((stringp d) (write-string d s))
          ((eql d :wild) (write-char #\* s))
          ((eql d :wild-inferiors) (write-string "**" s))
          (t (error "Invalid directory component ~S." d)))
        (write-char #\> s)))))

(defun file-container-key (f)
  (pathname-version (file-truename f)))

(defun read-directory-entry (dir file type &optional version)
  (let* ((name-table (aref (file-storage dir) 0))
         (container (gethash (cons file type) name-table)))
    (when (and container
               (not (zerop (length container))))
      (case version
        ((:newest nil)
         (aref container (1- (length container))))
        (:oldest
         (aref container 0))
        (:previous
         (when (>= (length container) 2)
           (aref container (- (length container) 2))))
        (t (let ((position (sys.int::bsearch version container
                                             :key 'file-container-key)))
             (when position
               (aref container position))))))))

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

(defun make-file (dir truename element-type)
  (let* ((time (get-universal-time))
         (name-table (aref (file-storage dir) 0))
         (container (gethash (cons (pathname-name truename) (pathname-type truename))
                             name-table)))
    ;; Figure out the file version.
    (when (member (pathname-version truename) '(nil :newest))
      (cond (container
             (setf truename (make-pathname :version (1+ (pathname-version (file-truename (aref container (1- (length container))))))
                                           :defaults truename)))
            (t
             (setf truename (make-pathname :version 1
                                           :defaults truename)))))
    ;; Canonicalize case in the directory part.
    (let ((dir-truename (file-truename dir)))
      ;; Root dir does not have a name. Poor root directory.
      (when (pathname-name dir-truename)
        (setf truename (make-pathname :directory (append (pathname-directory dir-truename)
                                                         (list (pathname-name dir-truename)))
                                      :defaults truename))))
    (format t "Creating file ~S.~%" truename)
    (let ((key (cons (pathname-name truename) (pathname-type truename)))
          (file (make-instance 'local-file
                               :truename truename
                               :storage (make-array 0
                                                    :element-type element-type
                                                    :adjustable t
                                                    :fill-pointer 0
                                                    :area :pinned)
                               :plist (list :creation-time time
                                            :write-time time))))
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
                                                          :fill-pointer 0
                                                          :area :pinned))))
          ((:overwrite :append)
           ???)
          ((nil) (return-from open-using-host nil))))
      (when (and (not (eql direction :probe))
                 (not (equal (upgraded-array-element-type element-type)
                             (array-element-type (file-storage file)))))
        (error "Incompatible ELEMENT-TYPE. File is of type ~S."
               (array-element-type (file-storage file))))
      (make-instance 'local-stream
                     :file file
                     :pathname pathname
                     :position (if (and (member direction '(:output :io))
                                        (eql if-exists :append))
                                   (length (file-storage file))
                                   0)
                     :direction direction
                     :superseded-file superseded-file))))

(defmethod file-write-date-using-host ((host local-file-host) pathname)
  (when (not (typep (pathname-directory pathname) '(cons (eql :absolute))))
    (error 'simple-file-error
           :pathname pathname
           :format-control "Non-absolute pathname."))
  (with-host-locked (host)
    (do ((element (rest (pathname-directory pathname)) (cdr element))
         (dir (local-host-root host)))
        ((endp element)
         (let ((file (read-directory-entry dir (pathname-name pathname) (pathname-type pathname) (pathname-version pathname))))
           (when (not file)
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "File does not exist."))
           (getf (file-plist file) :write-time)))
      (let ((next-dir (read-directory-entry dir (car element) "directory")))
        (when (not next-dir)
          (error 'simple-file-error
                 :pathname pathname
                 :format-control "File does not exist."))
        (setf dir next-dir)))))

(defun match-version (container version)
  (let ((result '()))
    (flet ((accumulate (file exact-version)
             (let ((truename (file-truename file)))
               (cond ((string-equal (pathname-type truename) "directory")
                      ;; Add this as a directory, not a file entry.
                      (let ((truename (file-truename (aref container (1- (length container))))))
                        (push (make-pathname :directory (append (pathname-directory truename)
                                                                (list (pathname-name truename)))
                                             :name nil :type nil :version nil
                                             :defaults truename)
                              result)))
                     (exact-version
                      (push truename result))
                     (t (push (make-pathname :version :newest
                                             :defaults truename)
                              result))))))
      (cond ((integerp version)
             ;; Exact match.
             (let ((position (sys.int::bsearch version container
                                               :key 'file-container-key)))
               (when position
                 (accumulate (aref container position) t))))
            ((eql version :wild)
             ;; All of them.
             (dotimes (i (length container))
               (accumulate (aref container i) t)))
            ((eql version :previous)
             ;; Latest but one (assuming at least two).
             (when (>= (length container) 2)
               (accumulate (aref container (- (length container) 2)) t)))
            ((member version '(nil :newest))
             ;; Latest one.
             (accumulate (aref container (1- (length container))) nil))))
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
          (setf existing (make-file dir
                                    (make-pathname :name name
                                                   :type "directory"
                                                   :version 1
                                                   :defaults dir-path)
                                    't))
          (setf (file-storage existing) (make-array 1 :initial-element (make-hash-table :test 'equalp))))
        (setf dir existing)))))

#|
           #:rename-file-using-host
           #:delete-file-using-host
           #:expunge-directory-using-host
|#

(defmethod sys.gray:stream-write-char ((stream local-stream) character)
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
      (setf (getf (file-plist file) :write-time) (get-universal-time)))))

(defmethod sys.gray:stream-read-char ((stream local-stream))
  (check-type (direction stream) (member :io :input))
  (let ((file (local-stream-file stream)))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (cond ((< (stream-position stream)
                (length (file-storage file)))
             (prog1 (aref (file-storage file) (stream-position stream))
               (incf (stream-position stream))))
            (t :eof)))))

(defmethod sys.gray:stream-write-byte ((stream local-stream) byte)
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
      (setf (getf (file-plist file) :write-time) (get-universal-time)))))

(defmethod sys.gray:stream-read-byte ((stream local-stream))
  (check-type (direction stream) (member :io :input))
  (let ((file (local-stream-file stream)))
    (mezzano.supervisor:with-mutex ((file-lock file))
      (cond ((< (stream-position stream)
                (length (file-storage file)))
             (prog1 (aref (file-storage file) (stream-position stream))
               (incf (stream-position stream))))
            (t :eof)))))

(defmethod sys.gray:stream-write-sequence ((stream local-stream) sequence &optional (start 0) end)
  (check-type (direction stream) (member :io :output))
  (let ((file (local-stream-file stream)))
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
      (setf (getf (file-plist file) :write-time) (get-universal-time)))))

(defmethod sys.gray:stream-read-sequence ((stream local-stream) sequence &optional (start 0) end)
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

(defmethod sys.gray:stream-element-type ((stream local-stream))
  (array-element-type (file-storage (local-stream-file stream))))

(defmethod sys.gray:stream-file-length ((stream local-stream))
  (length (file-storage (local-stream-file stream))))

(defmethod sys.gray:stream-file-position ((stream local-stream) &optional (position-spec nil position-specp))
  (cond (position-specp
         (setf (stream-position stream) (if (eql position-spec :end)
                                            (length (file-storage (local-stream-file stream)))
                                            position-spec)))
        (t (stream-position stream))))

(defmethod sys.gray:stream-line-column ((stream local-stream))
  nil)

(defmethod sys.gray:stream-line-length ((stream local-stream))
  nil)

(defmethod sys.gray:stream-clear-output ((stream local-stream))
  nil)

(defmethod sys.gray:stream-finish-output ((stream local-stream))
  nil)

(defmethod sys.gray:stream-force-output ((stream local-stream))
  nil)

(defmethod close ((stream local-stream) &key abort &allow-other-keys)
  (when (and (not abort)
             (superseded-file stream))
    ;; Replace the superseded file's contents.
    (let ((file (superseded-file stream))
          (time (get-universal-time)))
      (mezzano.supervisor:with-mutex ((file-lock file))
        (setf (file-storage file) (file-storage (local-stream-file stream)))
      (setf (getf (file-plist file) :creation-time) time
            (getf (file-plist file) :write-time) time))))
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
