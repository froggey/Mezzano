;;;; fetch-gzipped.lisp

(in-package #:quicklisp-client)

(defun gzipped-url (url)
  (check-type url string)
  (concatenate 'string url ".gz"))

(defun fetch-gzipped-version (url file &key quietly)
  (let ((gzipped (gzipped-url url))
        (gzipped-temp (merge-pathnames "gzipped.tmp" file)))
    (fetch gzipped gzipped-temp :quietly quietly)
    (gunzip gzipped-temp file)
    (delete-file-if-exists gzipped-temp)
    (probe-file file)))

(defun url-not-suitable-error-p (condition)
  (<= 400 (unexpected-http-status-code condition) 499))

(defun maybe-fetch-gzipped (url file &key quietly)
  (handler-case
      (fetch-gzipped-version url file :quietly quietly)
    (unexpected-http-status (condition)
      (cond ((url-not-suitable-error-p condition)
             (fetch url file :quietly quietly)
             (probe-file file))
            (t
             (error condition))))))

