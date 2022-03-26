;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
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
;;;
;;;; $Id: color-table.lisp,v 1.7 2007/01/05 15:04:11 xach Exp $

(in-package #:skippy)

(deftype color-table-entry ()
  '(unsigned-byte 24))

(defconstant +max-color-table-size+
  256
  "Color tables are restricted by the GIF89a specification to 256 entries.")

(defun rgb-color (r g b)
  (logand #xFFFFFF
          (logior (ash (logand #xFF r) 16)
                  (ash (logand #xFF g)  8)
                  (ash (logand #xFF b)  0))))

(defun color-rgb (color)
  (values (ldb (byte 8 16) color)
          (ldb (byte 8  8) color)
          (ldb (byte 8  0) color)))

(defclass color-table ()
  ((entries
    :initform (make-array 4
                          :adjustable t
                          :element-type 'color-table-entry
                          :initial-element 0
                          :fill-pointer 0)
    :reader entries)))

(defmethod print-object ((object color-table) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "with ~D entries" (length (entries object)))))

(defun add-color (color table)
  (let ((entries (entries table)))
    (if (= (length entries) +max-color-table-size+)
        (error 'color-table-full
               :color-table table)
        (vector-push-extend color entries))))

(defun find-color (color table)
  (position color (entries table)))

(defun ensure-color (color table)
  (or (find-color color table)
      (add-color color table)))

(defun make-color-table (&key initial-contents)
  (let ((table (make-instance 'color-table)))
    (dolist (color initial-contents table)
      (add-color color table))))

(defun color-table-size (table)
  (length (entries table)))

(defun color-table-entry (table index)
  (aref (entries table) index))

(defun (setf color-table-entry) (new-color table index)
  (setf (aref (entries table) index) new-color))

(defun color-table-code-size (table)
  "The number of bits needed to store the largest index in the color
table. The spec-imposed minimum is 2."
  (if table
      (max 2 (integer-length (1- (length (entries table)))))
      2))

(defun copy-color-table (table)
  (let ((new-table (make-color-table)))
    (loop for color across (entries table)
          do (add-color color new-table))
    new-table))
