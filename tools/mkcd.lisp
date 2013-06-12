(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cl-fad)
  (require :external-program))

(defpackage :mkcd
  (:use :cl :cl-fad)
  (:export :build-image
           :build-warm-image
           :build-raw-cold-image))

(in-package :mkcd)

(defvar *staging-path* #p"iso_stage/")
(defvar *grub-eltorito* #p"tools/stage2_eltorito")
(defvar *cdkboot* #p"tools/kboot/cdboot")

(defun pathname-file-part (pathname)
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))

(defun build-image (image-name kernel modules &key (bootloader :grub) confirm (verbose t) (base-path *default-pathname-defaults*))
  (check-type bootloader (member :grub :kboot))
  (let* ((stage (merge-pathnames *staging-path*))
         (image-path (merge-pathnames (make-pathname :name image-name :type "iso")))
         (*default-pathname-defaults* base-path)
         (bootloader-name nil))
    (when (or confirm verbose)
      (format t "Staging path: ~A~%" stage)
      (format t "Image path: ~A~%" image-path)
      (format t "Base path: ~A~%" base-path)
      (format t "Kernel: ~A~%" kernel)
      (format t "Modules: ~S~%" modules))
    (when confirm
      (when (not (y-or-n-p "Ok?"))
        (return-from build-image nil)))
    (delete-directory-and-files stage :if-does-not-exist :ignore)
    (unwind-protect
         (progn
           (ensure-directories-exist stage :verbose verbose)
           (ecase bootloader
             (:grub (let ((grub-dir (merge-pathnames "grub/" stage)))
                      (when verbose
                        (format t "Grub dir: ~A~%" grub-dir)
                        (format t "Grub source: ~A~%" (merge-pathnames *grub-eltorito* base-path))
                        (format t "Grub loader: ~A~%" (merge-pathnames (pathname-file-part *grub-eltorito*) grub-dir)))
                      (ensure-directories-exist grub-dir :verbose verbose)
                      (copy-file (merge-pathnames *grub-eltorito* base-path)
                                 (merge-pathnames (pathname-file-part *grub-eltorito*) grub-dir))
                      (with-open-file (s (merge-pathnames "menu.lst" grub-dir)
                                         :direction :output
                                         :if-does-not-exist :create)
                        (format s "default 0~%")
                        (format s "timeout 0~%")
                        (format s "title LispOS - ~A~%" image-name)
                        (format s "kernel /~A~%" (pathname-file-part kernel))
                        (dolist (f modules)
                          (format s "module /~A~%" (pathname-file-part f))))
                      (setf bootloader-name "grub/stage2_eltorito")))
             #+nil(:kboot (copy-file *cdkboot* (merge-pathnames *cdkboot* stage))))
           (copy-file kernel (merge-pathnames (pathname-file-part kernel) stage))
           (dolist (f modules)
             (copy-file f (merge-pathnames (pathname-file-part f) stage)))
           (external-program:run "mkisofs"
                                 (list "-R" "-b" bootloader-name "-no-emul-boot" "-boot-load-size" "4" "-boot-info-table" "-o"
                                       (format nil "~A.iso" image-name)
                                       (format nil "~A" stage))
                                 :output *standard-output*
                                 :error :output))
      (delete-directory-and-files stage :if-does-not-exist :ignore))))

(defun build-warm-image (&rest args)
  (apply #'build-image "lispos" "lispos-warm.image"
         '("init.llf" "system/file-compiler.llf")
         args))

(defun build-raw-cold-image (&rest args)
  (apply #'build-image "lispos-cold" "lispos.image"
         '()
         args))
