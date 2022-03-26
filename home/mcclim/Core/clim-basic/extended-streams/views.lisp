;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)
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

(in-package :clim-internals)

;;; Views are defined here and not in presentations.lisp so that they
;;; can be referenced in the streams code.

(defclass textual-view (view)
  ())

(defclass textual-menu-view (textual-view)
  ())

(defclass textual-dialog-view (textual-view)
  ())

(defclass gadget-view (view)
  ((foreground :initarg :foreground :reader gadget-view-foreground)
   (background :initarg :background :reader gadget-view-background)
   (text-style :initarg :text-style :reader gadget-view-text-style)))

(defclass gadget-menu-view (gadget-view)
  ())

(defclass gadget-dialog-view (gadget-view)
  ())

(defclass pointer-documentation-view (textual-view)
  ())


(defparameter +textual-view+ (make-instance 'textual-view))

(defparameter +textual-menu-view+ (make-instance 'textual-menu-view))

(defparameter +textual-dialog-view+ (make-instance 'textual-dialog-view))

(defparameter +gadget-view+ (make-instance 'gadget-view))

(defparameter +gadget-menu-view+ (make-instance 'gadget-menu-view))

(defparameter +gadget-dialog-view+ (make-instance 'gadget-dialog-view))

(defparameter +pointer-documentation-view+ (make-instance 'pointer-documentation-view))


(defmethod stream-default-view (stream)
  (declare (ignore stream))
  +textual-view+)

