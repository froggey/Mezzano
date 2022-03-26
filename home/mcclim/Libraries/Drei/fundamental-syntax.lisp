;;; -*- Mode: Lisp; Package: DREI-FUNDAMENTAL-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;
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

;;; Syntax for unknown buffer contents.  Parse contents into lines.

(in-package :drei-fundamental-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Every syntax must have a command table.

(define-syntax-command-table fundamental-table
    :errorp nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The syntax object and misc stuff.

(define-syntax fundamental-syntax (syntax)
  ()
  (:command-table fundamental-table)
  (:name "Fundamental"))

(setf *default-syntax* 'fundamental-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defmethod update-syntax values-max-min ((syntax fundamental-syntax) prefix-size suffix-size
                                         &optional begin end)
  (declare (ignore begin end))
  ;; We do nothing. Technically, Fundamental syntax always parses the
  ;; entire buffer, though.
  (values 0 (size (buffer syntax))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Redisplay
;;;
;;; Just uses the default buffer-view redisplay behavior.

(defmethod pump-state-for-offset-with-syntax ((view textual-drei-syntax-view)
                                              (syntax fundamental-syntax) (offset integer))
  (buffer-view-pump-state-for-offset view offset))

(defmethod stroke-pump-with-syntax ((view textual-drei-syntax-view)
                                    (syntax fundamental-syntax) stroke
                                    pump-state)
  (buffer-view-stroke-pump view stroke pump-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse

;; do this better
(defmethod syntax-line-indentation ((syntax fundamental-syntax) mark tab-width)
  0)
