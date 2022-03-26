;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/corman.lisp,v 1.5 2009/09/30 14:23:09 edi Exp $

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

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
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

(in-package :cl)

(defun wild-pathname-p (pathspec &optional field)
  (unless (pathnamep pathspec)
    (setq pathspec (pathname pathspec)))
  (labels ((name-wild-p (name)
             (or (eq :wild name)
                 (and (stringp name)
                      (string= "*" name))))
           (dir-wild-p (dir)
             (or (find :wild dir)
                 (find :wild-inferiors dir)
                 (find "*" dir :test #'string=))))
    (case field
      ((:name)
       (name-wild-p (pathname-name pathspec)))
      ((:type)
       (name-wild-p (pathname-type pathspec)))
      ((:directory)
       (dir-wild-p (pathname-directory pathspec)))
      ((nil)
       (or (name-wild-p (pathname-name pathspec))
           (name-wild-p (pathname-type pathspec))
           (dir-wild-p (pathname-directory pathspec))))
      (t nil))))

(defun file-namestring (pathspec)
  (flet ((string-list-for-component (component)
           (cond ((eq component :wild)
                  (list "*"))
                 (component
                  (list component))
                 (t nil))))
    (let* ((pathname (pathname pathspec))
           (name (pathnames::pathname-internal-name pathname))
           (type (pathnames::pathname-internal-type pathname)))
      (format nil "~{~A~}~{.~A~}"
              (string-list-for-component name)
              (string-list-for-component type)))))

(in-package :win32)

(defwinapi RemoveDirectory
    ((lpPathName LPCSTR))
  :return-type BOOL
  :library-name "Kernel32"
  :entry-name "RemoveDirectoryA"
  :linkage-type :pascal)

(defun delete-directory (pathspec)
  "Deletes the empty directory denoted by the pathname designator
PATHSPEC.  Returns true if successful, NIL otherwise."
  (win:RemoveDirectory
   (ct:lisp-string-to-c-string
    (namestring (pathname pathspec)))))

(export 'delete-directory)
