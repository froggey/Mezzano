;;; -*- Mode: Lisp; Package: CLIM-USER -*-

;;;  (c) copyright 2004 by Tim Moore (moore@bricoworks.com)
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

(in-package :clim-user)

;;; Keep track of how we're doing.

(defun print-unimplemented (&optional (packages '(:clim)))
  (let ((packages (mapcar #'find-package packages)))
    (loop
       for package in packages
       do (loop
	     for sym being the external-symbols of package
	     do (unless (or (boundp sym)
			    (fboundp sym)
			    (find-class sym nil)
			    (find-presentation-type-class sym nil)
			    (gethash sym
				     climi::*presentation-type-abbreviations*)
			    (gethash sym
				     climi::*presentation-gf-table*))
		  (print sym))))))
