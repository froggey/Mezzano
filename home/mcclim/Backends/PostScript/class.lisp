;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)

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

;;; TODO:

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-POSTSCRIPT-STREAM should offer a :PAPER-SIZE option.
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;;
;;;--GB

(in-package :clim-postscript)

;;;; Medium

(defclass postscript-medium (postscript-font-medium) ())

(defmacro postscript-medium-graphics-state (medium)
  `(first (slot-value (medium-sheet ,medium) 'graphics-state-stack)))

;;;; Stream
(defvar *default-postscript-title* "")

(defvar *default-postscript-for*
  #+unix (or (get-environment-variable "USER")
             "Unknown")
  #-unix "")

(defclass postscript-stream (sheet-leaf-mixin
                             sheet-parent-mixin
                             sheet-transformation-mixin
                             sheet-mute-input-mixin
                             sheet-mute-repainting-mixin
                             climi::updating-output-stream-mixin
                             basic-sheet
                             standard-extended-output-stream
                             permanent-medium-sheet-output-mixin
                             standard-output-recording-stream)
  ((port :initform nil :initarg :port :accessor port)
   (title :initarg :title)
   (for :initarg :for)
   (current-page :initform 0)
   (document-fonts :initform '())
   (graphics-state-stack :initform '())
   (pages  :initform nil :accessor postscript-pages)))

(defun make-postscript-stream (port device-type
                               multi-page scale-to-fit
                               orientation header-comments)
  (declare (ignore multi-page scale-to-fit))
  (unless device-type (setq device-type :a4))
  (let ((title (or (getf header-comments :title)
                   *default-postscript-title*))
        (for (or (getf header-comments :for)
                 *default-postscript-for*))
        (region (paper-region device-type orientation)))
    (make-instance 'postscript-stream
                   :port port
                   :title title :for for
                   :region region)))


;;;; Port

(defclass postscript-port (postscript-font-port)
  ((stream :initform nil
           :accessor postscript-port-stream)
   (device-type :initform :a4 :initarg :device-type
                :accessor device-type
                :type keyword)
   (page-orientation :initform :portrait :initarg :page-orientation
                     :accessor page-orientation
                     :type (member :landscape :portrait))))

(defmethod climb:make-graft
    ((port postscript-port) &key (orientation :default) (units :device))
  (make-instance 'postscript-graft
                 :port port
                 :mirror (postscript-port-stream port)
                 :orientation orientation
                 :units units))

(defmethod initialize-instance :after ((port postscript-port) &key)
  (let* ((options (cdr (port-server-path port)))
         (stream (getf options :stream))
         (device-type (getf options :device-type :a4))
         (page-orientation (getf options :page-orientation :portrait)))
    (setf (postscript-port-stream port) stream
          (device-type port) device-type
          (page-orientation port) page-orientation))
  (climb:make-graft port))
