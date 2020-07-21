(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cl-fad)
  (require :external-program))

(defpackage :mkcd
  (:use :cl :cl-fad)
  (:export :build-kboot-stub))

(in-package :mkcd)

(defvar *staging-path* #p"iso_stage/")
(defvar *cdkboot* #p"tools/kboot/cdkboot.img")

(defun pathname-file-part (pathname)
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))

(defun build-kboot-stub (name
                         &key
                           confirm
                           (verbose t)
                           (base-path *default-pathname-defaults*)
                           (kboot-options '(("video_mode" . "lfb:1024x768")))
                           (disk "hd0"))
  (let* ((stage (merge-pathnames *staging-path*))
         (image-path (merge-pathnames (make-pathname :name name :type "iso")))
         (*default-pathname-defaults* base-path))
    (when (or confirm verbose)
      (format t "Staging path: ~A~%" stage)
      (format t "Image path: ~A~%" image-path)
      (format t "Base path: ~A~%" base-path))
    (when confirm
      (when (not (y-or-n-p "Ok?"))
        (return-from build-kboot-stub nil)))
    (delete-directory-and-files stage :if-does-not-exist :ignore)
    (unwind-protect
         (progn
           (ensure-directories-exist stage :verbose verbose)
           (when verbose
             (format t "KBoot source: ~A~%" (merge-pathnames *cdkboot* base-path))
             (format t "KBoot loader: ~A~%" (merge-pathnames (pathname-file-part *cdkboot*) stage)))
           (copy-file (merge-pathnames *cdkboot* base-path)
                      (merge-pathnames (pathname-file-part *cdkboot*) stage))
           (with-open-file (s (merge-pathnames "kboot.cfg" stage)
                              :direction :output
                              :if-does-not-exist :create)
             (format s "set \"timeout\" 1~%")
             (format s "entry \"Mezzano\" {~%")
             (loop for (name . value) in kboot-options
                do (format s "  set ~S ~S~%" name value))
             (format s "  mezzano ~S~%" disk)
             (format s "}~%"))
           (external-program:run "mkisofs"
                                 (list "-R" "-J" "-b" "cdkboot.img" "-no-emul-boot" "-boot-load-size" "4" "-boot-info-table" "-o"
                                       (format nil "~A" image-path)
                                       (format nil "~A" stage))
                                 :output *standard-output*
                                 :error :output))
      (delete-directory-and-files stage :if-does-not-exist :ignore))))
