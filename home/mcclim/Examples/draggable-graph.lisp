;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2005 by
;;;           Andy Hefner (ahefner@gmail.com)

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

;;; Demo of draggable graph nodes

(define-application-frame draggable-graph-demo () ()
  (:menu-bar nil)
  (:pane (make-pane 'application-pane
                    :width :compute
                    :height :compute
                    :display-function 'generate-graph
                    :display-time t)))

(defun generate-graph (frame pane)
  (declare (ignore frame))
  (format-graph-from-roots
   (list (find-class 'number))
   (lambda (object stream)
     (present (class-name object) (presentation-type-of object) :stream stream))
   #'c2mop:class-direct-subclasses
   :stream pane))

(defun find-graph-node (record)
  "Searches upward until a graph node parent of the supplied output record is found."
  (loop for current = record then (output-record-parent current)
        while current
        when (graph-node-output-record-p current)
          do (return current)))

(defun node-edges (node)
  (append (alexandria:hash-table-values (slot-value node 'climi::edges-from))
          (alexandria:hash-table-values (slot-value node 'climi::edges-to))))

(defun node-and-edges-region (node edges)
  (stupid-copy-rectangle (reduce #'region-union edges :initial-value node)))

(defun redisplay-edges (graph edges)
  (dolist (edge edges)
    (climi::layout-edge-1 graph (climi::from-node edge) (climi::to-node edge))))

;;; (AH) McCLIM bug of the day:
;;;
;;; (I haven't looked in detail at the spec or McCLIM to confirm my
;;; assumptions here, but as I understand things..)  CLIM regions are
;;; immutable. Output records ARE mutable. A McCLIM output record can
;;; be used as a rectangular region corresponding to its bounding
;;; rectangle.  But this bounding rectangle is not immutable! So,
;;; region operations such as region-union may build a rectangle-set
;;; capturing the mutable output-record object, violating the
;;; immutability of regions and causing widespread panic and
;;; confusion.

(defun stupid-copy-rectangle (region)
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    (make-rectangle* x0 y0 x1 y1)))

(define-draggable-graph-demo-command (com-drag-node)
    ((record t) (offset-x real :default 0) (offset-y real :default 0))
  (let* ((stream *standard-output*)
         (node-record (find-graph-node record))
         (edge-records (node-edges node-record))
         (graph-record (output-record-parent node-record))
         (erase-region))
    (assert (typep graph-record 'graph-output-record))
    (drag-output-record stream node-record
                        :feedback (lambda (record stream old-x old-y x y mode)
                                    (declare (ignore old-x old-y))
                                    (ecase mode
                                      (:erase
                                       (erase-output-record record stream)
                                       (map nil #'clear-output-record edge-records)
                                       (setf erase-region
                                             (node-and-edges-region record edge-records)))
                                      (:draw
                                       (setf (output-record-position record)
                                             (values (- x offset-x) (- y offset-y)))
                                       (add-output-record record graph-record)
                                       (redisplay-edges graph-record edge-records)
                                       (repaint-sheet
                                        stream (region-union (or erase-region +nowhere+)
                                                             (node-and-edges-region record edge-records))))))
                        :finish-on-release t :multiple-window nil)))

(define-presentation-to-command-translator record-dragging-translator
    (t com-drag-node draggable-graph-demo
     :tester ((object presentation)
              (find-graph-node presentation)))
    (object presentation x y)
  (multiple-value-bind (old-x old-y) (output-record-position presentation)
    (list presentation (- x old-x) (- y old-y))))
