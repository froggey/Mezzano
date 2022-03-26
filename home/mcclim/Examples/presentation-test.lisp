;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2001 by 
;;;           Tim Moore (moore@bricoworks.com)

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

(in-package :clim-demo)

(define-application-frame summation ()
  ((total :accessor summation-total :initarg :total :initform 0))
  (:menu-bar nil)
  (:panes
   (tester :interactor))
  (:layouts
   (default (vertically () tester)))
  (:top-level (summation-top-level)))

(defun summation-top-level (frame &key
			    command-parser command-unparser 
			    partial-command-parser prompt)
  (declare (ignore command-parser command-unparser
                   partial-command-parser prompt))
  (let ((*standard-output* (frame-standard-output frame))
	(*standard-input* (frame-standard-input frame))
	(*print-pretty* nil))
    (setf (cursor-visibility (stream-text-cursor *standard-input*)) t)
    (present "Hallo" 'string)
    (loop
     (climi::catch-abort-gestures ("Return to ~A command level"
				   (frame-pretty-name frame))
       (present (summation-total frame) 'real)
       (fresh-line)
       (let ((new-val (accept 'real
			      :default (summation-total frame)
			      :default-type 'real)))
	 (fresh-line)
	 (incf (summation-total frame) new-val))))))
