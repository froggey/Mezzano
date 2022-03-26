;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)

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

(cl:in-package #:clim-postscript)

(defclass postscript-destination (climb:file-destination)
  ())

(defmethod climb:invoke-with-standard-output
    (continuation (destination postscript-destination))
  (call-next-method (lambda ()
                      (with-output-to-postscript-stream
                          (*standard-output* *standard-output*)
                        (funcall continuation)))
                    destination))

(climb:register-output-destination-type
 "Postscript File" 'postscript-destination)
