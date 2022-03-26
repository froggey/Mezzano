;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-FAD-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/test.lisp,v 1.12 2009/09/30 14:23:10 edi Exp $

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-fad-test)

(defparameter *tmp-dir*
              #+(or :win32 :mswindows :windows) "c:\\tmp\\"
              #-(or :win32 :mswindows :windows) "/tmp/")

(defvar *test-counter* 0)

(defmacro assert* (form)
  `(progn
     (format t "Trying to assert ~A~%" ',form)
     (assert ,form)
     (format t "Test ~A passed.~%" (incf *test-counter*))))

(defun test ()
  (setq *test-counter* 0)

  (assert* (path:= (path:catdir) #P""))
  (assert* (path:= (path:catdir #P"/") #P"/"))
  (assert* (path:= (path:catdir #P"a/" #P"b/") #P"a/b/"))
  (assert* (path:= (path:catdir #P"/a/" #P"/b/" #P"c/" #P"./d/" #P"e" #P"f/") #P"/b/c/./d/f/"))

  (assert* (path:= (path:catfile) #P""))
  (assert* (path:= (path:catfile #P"R.txt") #P"R.txt"))
  (assert* (path:= (path:catfile #P"a/" #P"/b/" #P"R.txt") #P"/b/R.txt"))

  
  (let ((fad-dir (merge-pathnames (pathname-as-directory "fad-test")
                                  *tmp-dir*)))
    (delete-directory-and-files fad-dir :if-does-not-exist :ignore)
    (assert* (directory-pathname-p fad-dir))
    (assert* (directory-pathname-p (pathname *tmp-dir*)))
    (let ((foo-file (merge-pathnames "foo.lisp"
                                     fad-dir)))
      (assert* (not (directory-pathname-p foo-file)))
      (assert* (not (file-exists-p foo-file)))
      (assert* (not (file-exists-p fad-dir)))
      (with-open-file (out (ensure-directories-exist foo-file)
                           :direction :output
                           :if-does-not-exist :create)
        (write-string "NIL" out))
      (assert* (file-exists-p foo-file))
      (assert* (not (directory-exists-p foo-file)))
      (assert* (file-exists-p fad-dir))
      (assert* (directory-exists-p fad-dir))
      (assert* (equal fad-dir
                      (pathname-as-directory fad-dir)))
      (assert* (equal foo-file
                      (pathname-as-file foo-file)))
      (assert* (not (equal fad-dir
                           (pathname-as-file fad-dir))))
      (assert* (not (equal foo-file
                           (pathname-as-directory foo-file))))
      (dolist (name '("bar" "baz"))
        (let ((dir (merge-pathnames (pathname-as-directory name)
                                    fad-dir)))
          (dolist (name '("foo.text" "bar.lisp"))
            (let ((file (merge-pathnames name dir)))
              (with-open-file (out (ensure-directories-exist file)
                                   :direction :output
                                   :if-does-not-exist :create)
                (write-string "NIL" out))))))
      ;; /tmp/fad-test/foo.lisp
      ;; /tmp/fad-test/bar/bar.lisp
      ;; /tmp/fad-test/bar/foo.text
      ;; /tmp/fad-test/baz/bar.lisp
      ;; /tmp/fad-test/baz/foo.text
      ;; files : 5
      ;; dirs : 3
      (let ((file-counter 0)
            (file-and-dir-counter 0)
            (bar-counter 0))
        (walk-directory fad-dir
                        (lambda (file)
                          (declare (ignore file))
                          (incf file-counter)))
        ;; file-counter => 5
        (walk-directory fad-dir
                        (lambda (file)
                          (declare (ignore file))
                          (incf file-and-dir-counter))
                        :directories t)
        ;; file-and-dir-counter => 5 + 3
        (walk-directory fad-dir
                        (lambda (file)
                          (declare (ignore file))
                          (incf bar-counter))
                        :test (lambda (file)
                                (string= (pathname-name file)
                                         "bar"))
                        :directories t)
        ;; do not traverse the baz directory
        (walk-directory fad-dir
                        (lambda (file)
                          (declare (ignore file))
                          (incf file-and-dir-counter))
                        :test (lambda (file)
                                (not (and (directory-pathname-p file)
                                          (string= (first (last (pathname-directory file)))
                                                   "baz"))))
                        :directories :breadth-first)
        ;; file-and-dir-counter => 5 + 3 + 2 dirs + 3 files
        (assert* (= 5 file-counter))
        (assert* (= 13 file-and-dir-counter))
        (assert* (= 2 bar-counter)))
      (let ((bar-file (merge-pathnames "bar.lisp" fad-dir)))
        (copy-file foo-file bar-file)
        (assert* (file-exists-p bar-file))
        (with-open-file (foo-stream foo-file :element-type '(unsigned-byte 8))
          (with-open-file (bar-stream bar-file :element-type '(unsigned-byte 8))
            (assert* (= (file-length foo-stream)
                        (file-length bar-stream)))
            (loop for foo-byte = (read-byte foo-stream nil nil)
                  for bar-byte = (read-byte bar-stream nil nil)
                  while (and foo-byte bar-byte)
                  do (assert* (eql foo-byte bar-byte))))))
      (let ((baz-dir (merge-pathnames (pathname-as-directory "baz")
                                      fad-dir))
            (list (mapcar #'namestring (list-directory fad-dir))))
        (assert* (find (namestring (truename foo-file)) list :test #'string=))
        (assert* (find (namestring (truename baz-dir)) list :test #'string=))
        (assert* (not (find (namestring (pathname-as-file baz-dir))
                            list
                            :test #'string=)))))
    (delete-directory-and-files fad-dir :if-does-not-exist :error)
    (assert* (not (file-exists-p fad-dir)))
    (assert* (not (directory-exists-p fad-dir))))
  (format t "All tests passed.~%"))
