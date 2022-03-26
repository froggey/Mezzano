;;;; client-update.lisp

(in-package #:quicklisp-client)

(defun fetch-client-file-info (client-file-info output-file)
  (maybe-fetch-gzipped (file-url client-file-info) output-file)
  (check-client-file output-file client-file-info)
  (probe-file output-file))

(defun retirement-directory (base)
  (let ((suffix 0))
    (loop
      (incf suffix)
      (let* ((try (format nil "~A-~D" base suffix))
             (dir (qmerge (make-pathname :defaults *quicklisp-home*
                                         :directory
                                         (list :relative "retired" try)))))
        (unless (probe-directory dir)
          (return dir))))))

(defun retire (directory base)
  (let ((retirement-home (qmerge "retired/"))
        (from (truename directory)))
    (ensure-directories-exist retirement-home)
    (let* ((*default-pathname-defaults* retirement-home)
           (to (retirement-directory base)))
      (rename-directory from to)
      to)))

(defun client-update-scratch-directory (client-info)
  (qmerge (make-pathname :defaults *quicklisp-home*
                         :directory
                         (list :relative
                               "tmp"
                               "client-update"
                               (version client-info)))))

(defun %install-client (new-info local-info)
  (let* ((work-directory (client-update-scratch-directory new-info))
         (current-quicklisp-directory (qmerge "quicklisp/"))
         (new-quicklisp-directory
          (merge-pathnames "quicklisp/" work-directory))
         (local-temp-tar (merge-pathnames "quicklisp.tar" work-directory))
         (local-setup (merge-pathnames "setup.lisp" work-directory))
         (local-asdf (merge-pathnames "asdf.lisp" work-directory))
         (new-client-tar-p (not (info-equal (client-tar-info new-info)
                                            (client-tar-info local-info))))
         (new-setup-p (not (info-equal (setup-info new-info)
                                       (setup-info local-info))))
         (new-asdf-p (not (info-equal (asdf-info new-info)
                                      (asdf-info local-info)))))
    (ensure-directories-exist work-directory)
    ;; Fetch and unpack quicklisp.tar if needed
    (when new-client-tar-p
      (fetch-client-file-info (client-tar-info new-info) local-temp-tar)
      (unpack-tarball local-temp-tar :directory work-directory))
    ;; Fetch setup.lisp if needed
    (when new-setup-p
      (fetch-client-file-info (setup-info new-info) local-setup))
    ;; Fetch asdf.lisp if needed
    (when new-asdf-p
      (fetch-client-file-info (asdf-info new-info) local-asdf))
    ;; Everything fetched, so move the old stuff away and move the new
    ;; stuff in
    (when new-client-tar-p
      (retire (qmerge "quicklisp/")
              (format nil "quicklisp-~A"
                      (version local-info)))
      (rename-directory new-quicklisp-directory current-quicklisp-directory))
    (when new-setup-p
      (replace-file local-setup (qmerge "setup.lisp")))
    (when new-asdf-p
      (replace-file local-asdf (qmerge "asdf.lisp")))
    ;; But unconditionally move the new client-info into place
    (replace-file (source-file new-info) (qmerge "client-info.sexp"))
    new-info))

(defun update-client (&key (prompt t))
  (let* ((local-info (local-client-info))
         (newest-info (newest-client-info local-info)))
    (cond ((null newest-info)
           (format t "No client update available.~%"))
          ((client-version-lessp local-info newest-info)
           (format t "Updating client from version ~A to version ~A.~%"
                   (version local-info)
                   (version newest-info))
           (when (or (not prompt)
                     (press-enter-to-continue))
             (%install-client newest-info local-info)
             (format t "~&New Quicklisp client installed. ~
                          It will take effect on restart.~%")))
          (t
           (format t "The most up-to-date client, version ~A, ~
                      is already installed.~%"
                   (version local-info)))))
  t)


(defun install-client (&key url version)
  (unless (or url version)
    (error "One of ~S or ~S is required" :url :version))
  (when (and url version)
    (error "Only one of ~S or ~S is allowed" :url :version))
  (when version
    (setf url (client-info-url-from-version version)))
  (let ((local-info (local-client-info))
        (new-info (fetch-client-info url)))
    (%install-client new-info local-info)))
