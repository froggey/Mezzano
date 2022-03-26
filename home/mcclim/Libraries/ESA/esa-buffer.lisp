;;; -*- Mode: Lisp; Package: ESA-IO -*-

;;;  (c) copyright 2006 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2007 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

(in-package :esa-buffer)

(defgeneric frame-make-buffer-from-stream (application-frame stream)
  (:documentation "Create a fresh buffer by reading the external
representation from STREAM"))

(defun make-buffer-from-stream (stream)
  "Create a fresh buffer by reading the external representation
from STREAM"
  (frame-make-buffer-from-stream *application-frame* stream))

(defgeneric frame-make-new-buffer (application-frame &key &allow-other-keys)
  (:documentation "Create a empty buffer for the application frame."))

(defun make-new-buffer (&rest args &key &allow-other-keys)
  "Create a empty buffer for the current frame."
  (apply #'frame-make-new-buffer *application-frame* args))

(defgeneric frame-save-buffer-to-stream (application-frame buffer stream)
  (:documentation "Save the entire BUFFER to STREAM in the appropriate
external representation"))

(defun save-buffer-to-stream (buffer stream)
  "Save the entire BUFFER to STREAM in the appropriate external
representation"
  (frame-save-buffer-to-stream *application-frame* buffer stream))

(defclass esa-buffer-mixin (name-mixin)
  ((%filepath :initform nil :accessor filepath)
   (%needs-saving :initform nil :accessor needs-saving)
   (%file-write-time :initform nil :accessor file-write-time)
   (%file-saved-p :initform nil :accessor file-saved-p)
   (%read-only-p :initform nil :accessor read-only-p))
  (:default-initargs :name "*scratch*"))
