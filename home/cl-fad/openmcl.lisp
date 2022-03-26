;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CCL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/openmcl.lisp,v 1.6 2009/09/30 14:23:10 edi Exp $

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

(in-package :cl-fad)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((ccl-function-feature (symbol-name feature)
           (let ((symbol (find-symbol symbol-name :ccl)))
             (when (and symbol (fboundp symbol))
               (pushnew feature *features*)))))
    (ccl-function-feature "%RMDIR" :ccl-has-%rmdir)
    (ccl-function-feature "DELETE-DIRECTORY" :ccl-has-delete-directory)))

(defpackage :cl-fad-ccl
  (:use :cl)
  (:export delete-directory)
  (:import-from :ccl
                :%realpath
                :signal-file-error
                :native-translated-namestring
                :with-cstrs)
  #+ccl-has-%rmdir
  (:import-from :ccl :%rmdir)
  #+ccl-has-delete-directory
  (:import-from :ccl :delete-directory))

(in-package :cl-fad-ccl)

#-ccl-has-%rmdir
(defun %rmdir (name)
  (with-cstrs ((n name))
    (#_rmdir n)))

;;; ClozureCL 1.6 introduced ccl:delete-directory with semantics that
;;; are acceptably similar to this "legacy" definition.
;;;
;;; Except this legacy definition is not recursive, hence this function is
;;; used only if there is no :CCL-HAS-DELETE-DIRECTORY feature.

#-ccl-has-delete-directory
(defun delete-directory (path)
  (let* ((namestring (native-translated-namestring path)))
    (when (%realpath namestring)
      (let* ((err (%rmdir namestring)))
        (or (eql 0 err) (signal-file-error err path))))))

