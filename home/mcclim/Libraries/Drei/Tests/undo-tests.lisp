;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2006 by
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

(cl:in-package :drei-tests)

(def-suite undo-tests :description "The test suite for tests
related to Drei's undo system." :in drei-tests)

(in-suite undo-tests)

(defclass test-undo-record (standard-undo-record)
  ())

(defmethod flip-undo-record ((record test-undo-record)))

(test add-undo
  (let ((tree (make-instance 'standard-undo-tree)))
    (finishes (add-undo (make-instance 'test-undo-record) tree))
    (finishes (add-undo (make-instance 'test-undo-record) tree))))

(test undo
  (let ((tree (make-instance 'standard-undo-tree)))
    (add-undo (make-instance 'test-undo-record) tree)
    (add-undo (make-instance 'test-undo-record) tree)
    (finishes (undo tree 2))
    (signals no-more-undo
      (undo tree 1))))

(test redo
  (let ((tree (make-instance 'standard-undo-tree)))
    (add-undo (make-instance 'test-undo-record) tree)
    (undo tree 1)
    (redo tree 1)
    (finishes (undo tree 1))))
