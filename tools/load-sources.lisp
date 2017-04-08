;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defun relativize-dir (path base)
  (labels ((frob (x y)
             (cond ((or (endp x)
                        (endp y)
                        (not (string-equal (first x) (first y))))
                    (list* :relative x))
                   (t
                    (frob (rest x) (rest y))))))
    (frob (pathname-directory path)
          (pathname-directory base))))

(defun guess-element-type-from-extension (type-name)
  (if (member type-name '("jpeg" "jpg" "png" "gif" "pdf" "ttf" "gz" "avi" "wav") :test #'string-equal)
      '(unsigned-byte 8)
      'character))

(defun copy-many (files base-path dest-path &optional filter)
  (dolist (f (directory files))
    (when (and (pathname-name f)
               (or (not filter)
                   (funcall filter f)))
      (let* ((dir (relativize-dir f base-path))
             (real-path (merge-pathnames
                         (make-pathname :directory dir
                                        :name (pathname-name f)
                                        :type (pathname-type f)
                                        :defaults dest-path)
                         dest-path)))
        (ensure-directories-exist real-path)
        (copy-file f real-path (guess-element-type-from-extension (pathname-type f)))))))

;; Copy Mezz source.
(copy-many (merge-pathnames "**/*.*" *default-pathname-defaults*)
           *default-pathname-defaults*
           #p"LOCAL:>Mezzano>"
           (lambda (path)
             (not (or (member ".git" (pathname-directory path) :test #'string-equal)
                      (member "notes" (pathname-directory path) :test #'string-equal)
                      (eql (pathname-version path) :previous)
                      (string-equal (pathname-name path) ".git")
                      (string-equal (pathname-type path) "llf")
                      (string-equal (pathname-type path) "image")
                      (string-equal (pathname-type path) "vmdk")
                      (string-equal (pathname-type path) "map")
                      (string-equal (pathname-type path) "iso")
                      (string-equal (pathname-type path) "pcap")
                      (string-equal (pathname-name path) "vboxserio")))))

;; And everything else.
(copy-many (merge-pathnames "*/**/*.*" (user-homedir-pathname))
           (user-homedir-pathname)
           #p"LOCAL:>Source>"
           (lambda (path)
             (not (or (member ".git" (pathname-directory path) :test #'string-equal)
                      (member "Fonts" (pathname-directory path) :test #'string-equal)
                      (eql (pathname-version path) :previous)
                      (string-equal (pathname-name path) ".git")
                      (string-equal (pathname-type path) "llf")
                      (string-equal (pathname-type path) "avi")))))

(copy-many (merge-pathnames "*.avi" (user-homedir-pathname))
           (user-homedir-pathname)
           #p"LOCAL:>Videos>")
