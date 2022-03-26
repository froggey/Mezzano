;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: puzzle.lisp,v 1.23 1993/07/27 01:46:05 colin Exp $

(in-package :clim-demo)

"Copyright (c) 1989, 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

(define-application-frame puzzle ()
    ((puzzle :initform (make-array '(4 4))
	     :accessor puzzle-puzzle))
  (:panes
    (display :application
	     :display-function 'draw-puzzle
	     :text-style (make-text-style :fix :bold :very-large)
	     :incremental-redisplay t
	     :text-cursor nil
	     :width :compute :height :compute
	     :end-of-page-action :allow
	     :end-of-line-action :allow))
  (:layouts
    (:default display)))

(defmethod frame-standard-input ((puzzle puzzle))
  (get-frame-pane puzzle 'display))

(defmethod frame-standard-output ((puzzle puzzle))
  (get-frame-pane puzzle 'display))

(defmethod run-frame-top-level :before ((puzzle puzzle) &key)
  (initialize-puzzle puzzle))

(defmethod read-frame-command ((puzzle puzzle) &key (stream *standard-input*))
  (let ((abort-chars #+genera '(#\Abort #\End)
		     #-genera nil))
    (let ((command (read-command-using-keystrokes
		     (frame-command-table puzzle) abort-chars
		     :stream stream)))
      (if (characterp command)
	  (frame-exit puzzle)
	  command))))

(define-presentation-type puzzle-cell ()
  :inherit-from '(integer 1 15))

(define-presentation-method highlight-presentation ((type puzzle-cell) record stream state)
  (with-bounding-rectangle* (left top right bottom) record
    (draw-rectangle* stream
		     left top right bottom
		     :ink +flipping-ink+)))

(defun encode-puzzle-cell (row column)
  (+ (* row 4) column))

(defun decode-puzzle-cell (encoding)
  (floor encoding 4))

(defmethod initialize-puzzle ((puzzle puzzle))
  (let ((puzzle-array (puzzle-puzzle puzzle)))
    (dotimes (row 4)
      (dotimes (column 4)
	(setf (aref puzzle-array row column) (mod (1+ (encode-puzzle-cell row column)) 16))))))

(defmethod draw-puzzle ((puzzle puzzle) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((puzzle-array (puzzle-puzzle puzzle)))
    (formatting-table (stream)
      (dotimes (row 4)
	(formatting-row (stream)
	  (dotimes (column 4)
	    (let* ((value (aref puzzle-array row column))
		   (cell-id (encode-puzzle-cell row column)))
	      (updating-output (stream :unique-id cell-id 
				       :cache-value value)
		(formatting-cell (stream :align-x :right)
		  (unless (zerop value)
		    (with-output-as-presentation
			(stream cell-id 'puzzle-cell)
		      (format stream "~2D" value))))))))))))

(defun find-open-cell (puzzle)
  (dotimes (row 4)
    (dotimes (column 4)
      (when (zerop (aref puzzle row column))
	(return (encode-puzzle-cell row column))))))

(defun cell-adjacent-to-open-cell (puzzle r c)
  ;; check row
  (or
    (dotimes (column 4)
      (when (and (/= column c) (zerop (aref puzzle r column)))
	(return (encode-puzzle-cell r column))))
    (dotimes (row 4)
      (when (and (/= row r) (zerop (aref puzzle row c)))
	(return (encode-puzzle-cell row c))))))

(define-puzzle-command com-move-cell
    ((cell puzzle-cell))
  (with-slots (puzzle) *application-frame*
    (multiple-value-bind (this-row this-column) (decode-puzzle-cell cell)
      (let ((open-cell (cell-adjacent-to-open-cell puzzle this-row this-column)))
	(multiple-value-bind (open-row open-column) (decode-puzzle-cell open-cell)
	  (cond ((= open-row this-row)
		 (cond ((> open-column this-column)
			(do ((c open-column (1- c)))
			    ((= c this-column))
			  (setf (aref puzzle this-row c)
				(aref puzzle this-row (1- c)))))
		       (t (do ((c open-column (1+ c)))
			      ((= c this-column))
			    (setf (aref puzzle this-row c)
				  (aref puzzle this-row (1+ c)))))))
		((= open-column this-column)
		 (cond ((> open-row this-row)
			(do ((r open-row (1- r)))
			    ((= r this-row))
			  (setf (aref puzzle r this-column)
				(aref puzzle (1- r) this-column))))
		       (t (do ((r open-row (1+ r)))
			      ((= r this-row))
			    (setf (aref puzzle r this-column)
				  (aref puzzle (1+ r) this-column)))))))))
      (setf (aref puzzle this-row this-column) 0))))

(define-presentation-to-command-translator move-cell
    (puzzle-cell com-move-cell puzzle
     :documentation "Move cell"
     :tester ((object) (cell-moveable-p object)))
    (object)
  (list object))

(defun cell-moveable-p (object)
  (multiple-value-bind (r c)
      (decode-puzzle-cell object)
    (cell-adjacent-to-open-cell (puzzle-puzzle *application-frame*) r c)))

(define-puzzle-command (com-scramble :menu t)
    ()
  (let ((ordering (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
	(puzzle-array (puzzle-puzzle *application-frame*)))
    (flet ((random-predicate (x y)
	     (declare (ignore x y))
	     (zerop (random 2))))
      (declare (dynamic-extent #'random-predicate))
      (setq ordering (sort ordering #'random-predicate)))
    (flet ((ordering-parity (ordering)
	     (do* ((ordering2 (copy-list ordering))
		   (total-parity t)
		   (start (position-if #'identity ordering2)
			  (position-if #'identity ordering2)))
		  ((null start) total-parity)
	       (let ((cycle-parity (do* ((evenp t (not evenp))
					 (item (nth start ordering) (nth item ordering)))
					((= item start)
					 (setf (nth start ordering2) nil)
					 evenp)
				     (setf (nth item ordering2) nil))))
		 (when (null cycle-parity)
		   (setq total-parity (not total-parity)))))))
      (unless (ordering-parity ordering)
	(rotatef (first ordering) (second ordering))))
    (dotimes (row 4)
      (dotimes (column 4)
	(setf (aref puzzle-array row column) (if ordering (+ 1 (pop ordering)) 0))))))

(define-puzzle-command (com-exit-puzzle :menu "Exit")
    ()
  (frame-exit *application-frame*))

