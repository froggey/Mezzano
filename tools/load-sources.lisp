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
  (if (member type-name '("jpeg" "jpg" "png" "gif" "pdf" "ttf") :test #'string-equal)
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
(ensure-directories-exist #p"LOCAL:>Mezzano>")
(copy-many #p"**/*.lisp" *default-pathname-defaults*
           #p"LOCAL:>Mezzano>")
(copy-many #p"**/*.asd" *default-pathname-defaults*
           #p"LOCAL:>Mezzano>")
(copy-file "LICENCE" #p"LOCAL:>Mezzano>LICENCE.text")
(copy-file "README" #p"LOCAL:>Mezzano>README.text")
(copy-file "tools/cl-symbols.lisp-expr" #p"LOCAL:>Mezzano>tools>cl-symbols.lisp-expr")
(copy-file "tools/disk_header" #p"LOCAL:>Mezzano>tools>disk_header.bin")
(copy-file "tools/font8x8" #p"LOCAL:>Mezzano>tools>font8x8.lisp-expr")
(copy-file "tools/pci.ids" #p"LOCAL:>Mezzano>tools>pci-ids.text")
(copy-file "tools/UnicodeData.txt" #p"LOCAL:>Mezzano>tools>UnicodeData.text")
(copy-file "tools/unifont-5.1.20080820.hex" (make-pathname :directory '(:absolute "Mezzano") :name "unifont-5.1.20080820" :type "text" :defaults #p"LOCAL:>"))

;; And everything else.
(copy-many (merge-pathnames "**/*.*" (user-homedir-pathname))
           (user-homedir-pathname)
           #p"LOCAL:>Source>"
           (lambda (x)
             (not (or (member ".git" (pathname-directory x) :test #'string-equal)
                      (string-equal (pathname-name x) ".git")
                      (string-equal (pathname-type x) "llf")))))
