(in-package :clim-listener)

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.


;; Icons for various things
;; Needs to have gilberth's XPM loader loaded first!

;; Some day, revamp icons so that ICON-OF gives you some wrapper that can
;; represent a set of icons, from which you can request one of a particular
;; size. For now, everything I use is 16x16.

;; This is a particularly silly idea, but ICON-OUTPUT-RECORD could be useful
;; for aligning images relative to the text baseline.


;; Ooops, icons.lisp gets left in the pathname, but that gets overridden anyway..
;(defparameter *icon-path* (merge-pathnames #P"icons/" #.*compile-file-truename*))

(defmacro deficon (var pathname)
  `(eval-when (:load-toplevel :execute)
     (defparameter ,var (make-pattern-from-bitmap-file
                         ,(merge-pathnames
                           (uiop:parse-unix-namestring
                            pathname
                            :defaults *icon-path*)
                           *icon-path*)
                         :format :xpm))))

(defvar *icon-cache* (make-hash-table  :test #'equal))

(defun standard-icon (filename)
  "Loads an icon from the *icon-path*, caching it by name in *icon-cache*"
  (or (gethash filename *icon-cache*)
      (setf (gethash filename *icon-cache*)
            (make-pattern-from-bitmap-file
             (merge-pathnames (uiop:parse-unix-namestring
                               filename
                               :defaults *icon-path*)
                              *icon-path*)
             :format :xpm))))

;; Don't particularly need these any more..
(deficon *folder-icon*   "folder.xpm")
(deficon *document-icon* "document.xpm")
(deficon *object-icon*   "simple-object.xpm")

;; Icon functions

(defmethod icon-of ((object t))
  *object-icon*)

(defun draw-icon (stream pattern &key (extra-spacing 0) )
  (let ((stream (if (eq stream t) *standard-output* stream)))
    (multiple-value-bind (x y) (stream-cursor-position stream)
      (draw-pattern* stream pattern x y)
      (stream-increment-cursor-position stream (+ (pattern-width pattern) extra-spacing) 0))))

(defun precache-icons ()
  (let ((pathnames (remove-if #'cl-fad:directory-pathname-p
                              (cl-fad:list-directory
                               (cl-fad:pathname-directory-pathname *icon-path*)))))
    (dolist (pn pathnames)
      (standard-icon (format nil "~A.~A" (pathname-name pn) (pathname-type pn))))))

#+(or)
(eval-when (:load-toplevel :execute)
  (precache-icons))
