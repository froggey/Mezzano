;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               install-dependencies.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Get and installs the dependencies.  Should be run on the file server host.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-02 <PJB> 
;;;;BUGS
;;;;
;;;;    - there's no handling of errors in external programs
;;;;      (uiop:run-program breaks into the debugger).
;;;;
;;;;    - there's no provision for re-running this script (eg. when it
;;;;      failed halfway previously).
;;;;
;;;;    - the fonts are not installed yet.
;;;;
;;;;LEGAL
;;;;
;;;;    MIT
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
;;;;
;;;;    Permission is hereby granted, free of charge, to any person
;;;;    obtaining a copy of this software and associated documentation
;;;;    files (the "Software"), to deal in the Software without
;;;;    restriction, including without limitation the rights to use,
;;;;    copy, modify, merge, publish, distribute, sublicense, and/or
;;;;    sell copies of the Software, and to permit persons to whom the
;;;;    Software is furnished to do so, subject to the following
;;;;    conditions:
;;;;
;;;;    The above copyright notice and this permission notice shall be
;;;;    included in all copies or substantial portions of the
;;;;    Software.
;;;;
;;;;    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
;;;;    KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;;;;    WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;;;    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;;    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;;    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;;;    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;;**************************************************************************
(in-package :cl-user)
(load "configuration.lisp")
(load "ipl-configuration.lisp")

(ensure-directories-exist (make-pathname :name "DUMMY" :type "DUM" :defaults *file-server-home-directory*))

(uiop:with-current-directory (*file-server-home-directory*)
  (dolist (repo '("git@github.com:froggey/asdf.git"
                  "git@github.com:froggey/trivial-features.git"
                  "git@github.com:froggey/zpb-ttf.git"
                  "git@github.com:froggey/cl-jpeg.git"
                  "git@github.com:froggey/chipz.git"
                  "git://common-lisp.net/projects/alexandria/alexandria.git"
                  "git@github.com:cl-babel/babel.git"
                  "git@github.com:Ramarren/png-read.git"))
    (format t "cloning ~A~%" repo) (finish-output)
    (uiop:run-program (list "git" "clone" repo)))
  (dolist (resource '("http://upload.wikimedia.org/wikipedia/commons/7/73/Mandarin_Pair.jpg"))
    (format t "getting ~A~%" resource) (finish-output)
    (uiop:run-program (list "wget" resource)))
  (dolist (tarball '("http://common-lisp.net/project/iterate/releases/iterate-1.4.3.tar.gz"
                     "http://projects.tuxee.net/cl-vectors/files/cl-vectors-0.1.5.tar.gz"))
    (format t "getting ~A~%" tarball) (finish-output)
    (uiop:run-program (list "wget" tarball))
    (uiop:run-program (list "tar" "zxf" (subseq tarball (1+ (position #\/ tarball :from-end t)))))))

(let ((path (merge-pathnames ".config/common-lisp/source-registry.conf"
                             *file-server-home-directory*)))
  (format t "creating ~A~%" path) (finish-output)
  (ensure-directories-exist path)
  (with-open-file (conf path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (print `(:source-registry
             (:tree ,(namestring *file-server-home-directory*))
             :inherit-configuration)
           conf)
    (terpri)))

(format t "done~%") (finish-output)
;;;; THE END ;;;;
