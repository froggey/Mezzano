#!/usr/bin/cl -sp asdf -E main
#|
Usage:
    ~/common-lisp/asdf/tools/cl-source-registry-cache.lisp ~/common-lisp
will compute a cache of the .asd files under ~/common-lisp//
vastly speeding the future initialization of the source-registry.
To update the cache, run the same command again.
To invalidate the cache, remove the cache file in the same directory:
    rm -f ~/common-lisp/.cl-source-registry.cache
|#

(in-package :asdf)

(defun collect-asd (table asd)
  (multiple-value-bind (previous foundp)
      (gethash (pathname-name asd) table)
    (when (or (not foundp) (> (length (pathname-directory previous))
                              (length (pathname-directory asd))))
      (setf (gethash (pathname-name asd) table) asd))))

(defun update-cache (directory &key (exclude *default-source-registry-exclusions*)
                               (recurse-beyond-asds *recurse-beyond-asds*))
  (let* ((dir (ensure-pathname directory
                               :namestring :native
                               :ensure-absolute t :want-non-wild t :ensure-directory t
                               :want-existing t))
         (table (make-hash-table :test 'equal))
         (collector #'(lambda (asd) (collect-asd table asd))))
    (collect-sub*directories
     dir
     #'(lambda (d)
         (if-let (cache (unless (equal d dir)
                          (probe-file* (subpathname d ".cl-source-registry.cache"))))
           (destructuring-bind (tag &rest entries) (read-file-form cache)
             (assert (eq tag :source-registry-cache))
             (dolist (asd entries) (funcall collector (subpathname d asd)))
             nil)
           (let ((asds (collect-asds-in-directory d collector)))
             (or recurse-beyond-asds (not asds)))))
     #'(lambda (sub)
         (not (member (car (last (pathname-directory sub))) exclude :test #'equal)))
     (constantly nil))
    (with-output-file (s (subpathname dir ".cl-source-registry.cache")
                         :if-exists :rename-and-delete :if-does-not-exist :create)
      (format s "(:source-registry-cache~{~% ~S~})~%"
              (sort (loop :for p :being :the :hash-values :of table
                          :collect (unix-namestring (enough-pathname p dir)))
                    'string<)))))

(defun main (argv)
  (map () 'update-cache argv))
