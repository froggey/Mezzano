;;;; client-info.lisp

(in-package #:quicklisp-client)

(defparameter *client-base-url* "http://beta.quicklisp.org/")

(defgeneric info-equal (info1 info2)
  (:documentation "Return TRUE if INFO1 and INFO2 are 'equal' in some
  important sense."))

;;; Information for checking the validity of files fetched for
;;; installing/updating the client code.

(defclass client-file-info ()
  ((plist-key
    :initarg :plist-key
    :reader plist-key)
   (file-url
    :initarg :url
    :reader file-url)
   (name
    :reader name
    :initarg :name)
   (size
    :initarg :size
    :reader size)
   (md5
    :reader md5
    :initarg :md5)
   (sha256
    :reader sha256
    :initarg :sha256)
   (plist
    :reader plist
    :initarg :plist)))

(defmethod print-object ((info client-file-info) stream)
  (print-unreadable-object (info stream :type t)
    (format stream "~S ~D ~S"
            (name info)
            (size info)
            (md5 info))))

(defmethod info-equal ((info1 client-file-info) (info2 client-file-info))
  (and (eql (size info1) (size info2))
       (equal (name info1) (name info2))
       (equal (md5 info1) (md5 info2))))

(defclass asdf-file-info (client-file-info)
  ()
  (:default-initargs
   :plist-key :asdf
   :name "asdf.lisp"))

(defclass setup-file-info (client-file-info)
  ()
  (:default-initargs
   :plist-key :setup
   :name "setup.lisp"))

(defclass client-tar-file-info (client-file-info)
  ()
  (:default-initargs
   :plist-key :client-tar
   :name "quicklisp.tar"))

(define-condition invalid-client-file (error)
  ((file
    :initarg :file
    :reader invalid-client-file-file)))

(define-condition badly-sized-client-file (invalid-client-file)
  ((expected-size
    :initarg :expected-size
    :reader badly-sized-client-file-expected-size)
   (actual-size
    :initarg :actual-size
    :reader badly-sized-client-file-actual-size))
  (:report (lambda (condition stream)
             (format stream "Unexpected file size for ~A ~
                             - expected ~A but got ~A"
                     (invalid-client-file-file condition)
                     (badly-sized-client-file-expected-size condition)
                     (badly-sized-client-file-actual-size condition)))))

(defun check-client-file-size (file expected-size)
  (let ((actual-size (file-size file)))
    (unless (eql expected-size actual-size)
      (error 'badly-sized-client-file
             :file file
             :expected-size expected-size
             :actual-size actual-size))))

;;; TODO: check cryptographic digests too.

(defgeneric check-client-file (file client-file-info)
  (:documentation
   "Signal an INVALID-CLIENT-FILE error if FILE does not match the
   metadata in CLIENT-FILE-INFO.")
  (:method (file client-file-info)
    (check-client-file-size file (size client-file-info))
    client-file-info))

;;; Structuring and loading information about the Quicklisp client
;;; code

(defclass client-info ()
  ((setup-info
    :reader setup-info
    :initarg :setup-info)
   (asdf-info
    :reader asdf-info
    :initarg :asdf-info)
   (client-tar-info
    :reader client-tar-info
    :initarg :client-tar-info)
   (canonical-client-info-url
    :reader canonical-client-info-url
    :initarg :canonical-client-info-url)
   (version
    :reader version
    :initarg :version)
   (subscription-url
    :reader subscription-url
    :initarg :subscription-url)
   (plist
    :reader plist
    :initarg :plist)
   (source-file
    :reader source-file
    :initarg :source-file)))

(defmethod print-object ((client-info client-info) stream)
  (print-unreadable-object (client-info stream :type t)
    (prin1 (version client-info) stream)))

(defmethod available-versions-url ((info client-info))
  (make-versions-url (subscription-url info)))

(defgeneric extract-client-file-info (file-info-class plist)
  (:method (file-info-class plist)
    (let* ((instance (make-instance file-info-class))
           (key (plist-key instance))
           (file-info-plist (getf plist key)))
      (unless file-info-plist
        (error "Missing client-info data for ~S" key))
      (destructuring-bind (&key url size md5 sha256 &allow-other-keys)
          file-info-plist
        (unless (and url size md5 sha256)
          (error "Missing client-info data for ~S" key))
        (reinitialize-instance instance
                               :plist file-info-plist
                               :url url
                               :size size
                               :md5 md5
                               :sha256 sha256)))))

(defun format-client-url (path &rest format-arguments)
  (if format-arguments
      (format nil "~A~{~}" *client-base-url* path format-arguments)
      (format nil "~A~A" *client-base-url* path)))

(defun client-info-url-from-version (version)
  (format-client-url "client/~A/client-info.sexp" version))

(define-condition invalid-client-info (error)
  ((plist
    :initarg plist
    :reader invalid-client-info-plist)))

(defun load-client-info (file)
  (let ((plist (safely-read-file file)))
    (destructuring-bind (&key subscription-url
                              version
                              canonical-client-info-url
                              &allow-other-keys)
        plist
      (make-instance 'client-info
                     :setup-info (extract-client-file-info 'setup-file-info
                                                           plist)
                     :asdf-info (extract-client-file-info 'asdf-file-info
                                                          plist)
                     :client-tar-info
                     (extract-client-file-info 'client-tar-file-info
                                               plist)
                     :canonical-client-info-url canonical-client-info-url
                     :version version
                     :subscription-url subscription-url
                     :plist plist
                     :source-file (probe-file file)))))

(defun mock-client-info ()
  (flet ((mock-client-file-info (class)
           (make-instance class
                          :size 0
                          :url ""
                          :md5 ""
                          :sha256 ""
                          :plist nil)))
    (make-instance 'client-info
                   :version ql-info:*version*
                   :subscription-url
                   (format-client-url "client/quicklisp.sexp")
                   :setup-info (mock-client-file-info 'setup-file-info)
                   :asdf-info (mock-client-file-info 'asdf-file-info)
                   :client-tar-info (mock-client-file-info
                                     'client-tar-file-info))))

(defun fetch-client-info (url)
  (let ((info-file (qmerge "tmp/client-info.sexp")))
    (delete-file-if-exists info-file)
    (fetch url info-file :quietly t)
    (handler-case
        (load-client-info info-file)
      ;; FIXME: So many other things could go wrong here; I think it
      ;; would be nice to catch and report them clearly as bogus URLs
      (invalid-client-info ()
        (error "Invalid client info URL -- ~A" url)))))

(defun local-client-info ()
  (let ((info-file (qmerge "client-info.sexp")))
    (if (probe-file info-file)
        (load-client-info info-file)
        (progn
          (warn "Missing client-info.sexp, using mock info")
          (mock-client-info)))))

(defun newest-client-info (&optional (info (local-client-info)))
  (let ((latest (subscription-url info)))
    (when latest
      (fetch-client-info latest))))

(defun client-version-lessp (client-info-1 client-info-2)
  (string-lessp (version client-info-1)
                (version client-info-2)))

(defun client-version ()
  "Return the version for the current local client installation. May
or may not be suitable for passing as the :VERSION argument to
INSTALL-CLIENT, depending on if it's a standard Quicklisp-provided
client."
  (version (local-client-info)))

(defun client-url ()
  "Return an URL suitable for passing as the :URL argument to
INSTALL-CLIENT for the current local client installation."
  (canonical-client-info-url (local-client-info)))

(defun available-client-versions ()
  (let ((url (available-versions-url (local-client-info)))
        (temp-file (qmerge "tmp/client-versions.sexp")))
    (when url
      (handler-case
          (progn
            (maybe-fetch-gzipped url temp-file)
            (prog1
                (with-open-file (stream temp-file)
                  (safely-read stream))
              (delete-file-if-exists temp-file)))
        (unexpected-http-status (condition)
          (unless (url-not-suitable-error-p condition)
            (error condition)))))))
