;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)

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

;;; A persistent buffer uses a persistent data structure for its
;;; contents, provides cursors into contents, and contains cursors
;;; into the current contents.

(in-package :drei-buffer)

;;; For now, pos contains just an integer, while it might contain a cons
;;; of two adjacent buffer elements for higher performance (with the help
;;; of buffer implementation, especially the rebalancing part).
(defclass persistent-cursor ()
  ((buffer :reader buffer :initarg :buffer) ; TODO: fix overlap with mark?
   (pos :accessor cursor-pos))
  (:documentation "The (non-persistent) cursor into PERSISTENT-BUFFER."))

(defclass left-sticky-persistent-cursor (persistent-cursor) ())

(defclass right-sticky-persistent-cursor (persistent-cursor) ())

(defclass line-cursor-mixin () ()
  (:documentation "Support for line-oriented buffers."))

(defclass left-sticky-line-persistent-cursor
    (left-sticky-persistent-cursor line-cursor-mixin) ())

(defclass right-sticky-line-persistent-cursor
    (right-sticky-persistent-cursor line-cursor-mixin) ())

(defmethod cursor-pos ((cursor left-sticky-persistent-cursor))
  (1+ (slot-value cursor 'pos)))

(defmethod (setf cursor-pos) (position (cursor left-sticky-persistent-cursor))
  (assert (<= 0 position (size (buffer cursor))) ()
	  "Cursor position out of bounds: ~S, ~S" cursor position)
  (setf (slot-value cursor 'pos) (1- position)))

(defmethod cursor-pos ((cursor right-sticky-persistent-cursor))
  (slot-value cursor 'pos))

(defmethod (setf cursor-pos) (position (cursor right-sticky-persistent-cursor))
  (assert (<= 0 position (size (buffer cursor))) ()
	  "Cursor position out of bounds: ~S, ~S" cursor position)
  (setf (slot-value cursor 'pos) position))

(defclass persistent-buffer (buffer)
  ((cursors :accessor cursors :initform nil))
  (:documentation "The Climacs persistent buffer base class
\(non-instantiable)."))

(defmethod initialize-instance :after ((cursor left-sticky-persistent-cursor)
				       &rest initargs &key (position 0))
  (declare (ignorable initargs))
  (with-slots (buffer pos) cursor
    (setf pos (1- position))
    (with-slots (cursors) buffer
      (push (flexichain::make-weak-pointer cursor) cursors))))

(defmethod initialize-instance :after ((cursor right-sticky-persistent-cursor)
				       &rest initargs &key (position 0))
  (declare (ignorable initargs))
  (with-slots (buffer pos) cursor
    (setf pos position)
    (with-slots (cursors) buffer
      (push (flexichain::make-weak-pointer cursor) cursors))))

(defclass binseq-buffer (persistent-buffer)
  ((contents :initform (list-binseq nil)))
  (:documentation "An instantiable subclass of PERSISTENT-BUFFER that
uses a binary sequence for the CONTENTS slot."))

(defclass obinseq-buffer (persistent-buffer)
  ((contents :initform (list-obinseq nil)))
  (:documentation "An instantiable subclass of PERSISTENT-BUFFER that
uses an optimized binary sequence (only non-nil atoms are allowed as
elements) for the CONTENTS slot."))

(defclass binseq2-buffer (persistent-buffer)
  ((contents :initform (list-binseq2 nil)))
  (:documentation "An instantiable subclass of PERSISTENT-BUFFER that
uses a binary sequence for lines and optimized binary sequences for
line contents, all kept in the CONTENTS slot."))

(defclass p-mark-mixin ()
  ((buffer :initarg :buffer :reader buffer)
   (cursor :reader cursor))
  (:documentation "A mixin class used in the initialization of a mark
that is used in a PERSISTENT-BUFFER."))

(defclass p-line-mark-mixin (p-mark-mixin) ()
  (:documentation "A persistent mark mixin class that works with
cursors that can efficiently work with lines."))

(defmethod backward-object ((mark p-mark-mixin) &optional (count 1))
  (decf (offset mark) count))

(defmethod forward-object ((mark p-mark-mixin) &optional (count 1))
  (incf (offset mark) count))

(defmethod offset ((mark p-mark-mixin))
  (cursor-pos (cursor mark)))

(defmethod (setf offset) (new-offset (mark p-mark-mixin))
  (assert (<= 0 new-offset) ()
	  (make-condition 'motion-before-beginning :offset new-offset))
  (assert (<= new-offset (size (buffer mark))) ()
	  (make-condition 'motion-after-end :offset new-offset))
  (setf (cursor-pos (cursor mark)) new-offset))

(defclass persistent-left-sticky-mark (left-sticky-mark p-mark-mixin) ()
  (:documentation "A LEFT-STICKY-MARK subclass suitable for use in a
PERSISTENT-BUFFER."))

(defclass persistent-right-sticky-mark (right-sticky-mark p-mark-mixin) ()
  (:documentation "A RIGHT-STICKY-MARK subclass suitable for use in a
PERSISTENT-BUFFER."))

(defclass persistent-left-sticky-line-mark (left-sticky-mark p-line-mark-mixin) ()
  (:documentation "A LEFT-STICKY-MARK subclass with line support,
suitable for use in a PERSISTENT-BUFFER."))

(defclass persistent-right-sticky-line-mark (right-sticky-mark p-line-mark-mixin) ()
  (:documentation "A RIGHT-STICKY-MARK subclass with line support,
suitable for use in a PERSISTENT-BUFFER."))

(defmethod initialize-instance :after ((mark persistent-left-sticky-mark)
				       &rest args &key (offset 0))
  "Associates a created mark with the buffer for which it was created."
  (declare (ignorable args))
  (assert (<= 0 offset) ()
	  (make-condition 'motion-before-beginning :offset offset))
  (assert (<= offset (size (buffer mark))) ()
	  (make-condition 'motion-after-end :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'left-sticky-persistent-cursor
		       :buffer (buffer mark)
		       :position offset)))

(defmethod initialize-instance :after ((mark persistent-right-sticky-mark)
				       &rest args &key (offset 0))
  "Associates a created mark with the buffer for which it was created."
  (declare (ignorable args))
  (assert (<= 0 offset) ()
	  (make-condition 'motion-before-beginning :offset offset))
  (assert (<= offset (size (buffer mark))) ()
	  (make-condition 'motion-after-end :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'right-sticky-persistent-cursor
		       :buffer (buffer mark)
		       :position offset)))

(defmethod initialize-instance :after ((mark persistent-left-sticky-line-mark)
				       &rest args &key (offset 0))
  "Associates a created mark with the buffer for which it was created."
  (declare (ignorable args))
  (assert (<= 0 offset) ()
	  (make-condition 'motion-before-beginning :offset offset))
  (assert (<= offset (size (buffer mark))) ()
	  (make-condition 'motion-after-end :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'left-sticky-line-persistent-cursor
		       :buffer (buffer mark)
		       :position offset)))

(defmethod initialize-instance :after ((mark persistent-right-sticky-line-mark)
				       &rest args &key (offset 0))
  "Associates a created mark with the buffer for which it was created."
  (declare (ignorable args))
  (assert (<= 0 offset) ()
	  (make-condition 'motion-before-beginning :offset offset))
  (assert (<= offset (size (buffer mark))) ()
	  (make-condition 'motion-after-end :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'right-sticky-line-persistent-cursor
		       :buffer (buffer mark)
		       :position offset)))

(defmethod clone-mark ((mark persistent-left-sticky-mark) &optional stick-to)
  (cond
    ((or (null stick-to) (eq stick-to :left))
     (make-instance 'persistent-left-sticky-mark
		    :buffer (buffer mark) :offset (offset mark)))
    ((eq stick-to :right)
     (make-instance 'persistent-right-sticky-mark
		    :buffer (buffer mark) :offset (offset mark)))
    (t (error "invalid value for stick-to"))))

(defmethod clone-mark ((mark persistent-right-sticky-mark) &optional stick-to)
  (cond
    ((or (null stick-to) (eq stick-to :right))
     (make-instance 'persistent-right-sticky-mark
		    :buffer (buffer mark) :offset (offset mark)))
    ((eq stick-to :left)
     (make-instance 'persistent-left-sticky-mark
		    :buffer (buffer mark) :offset (offset mark)))
    (t (error "invalid value for stick-to"))))

(defmethod clone-mark ((mark persistent-left-sticky-line-mark)
		       &optional stick-to)
  (cond
    ((or (null stick-to) (eq stick-to :left))
     (make-instance 'persistent-left-sticky-line-mark
		    :buffer (buffer mark) :offset (offset mark)))
    ((eq stick-to :right)
     (make-instance 'persistent-right-sticky-line-mark
		    :buffer (buffer mark) :offset (offset mark)))
    (t (error "invalid value for stick-to"))))

(defmethod clone-mark ((mark persistent-right-sticky-line-mark)
		       &optional stick-to)
  (cond
    ((or (null stick-to) (eq stick-to :right))
     (make-instance 'persistent-right-sticky-line-mark
		    :buffer (buffer mark) :offset (offset mark)))
    ((eq stick-to :left)
     (make-instance 'persistent-left-sticky-line-mark
		    :buffer (buffer mark) :offset (offset mark)))
    (t (error "invalid value for stick-to"))))

(defmethod size ((buffer binseq-buffer))
  (binseq-length (slot-value buffer 'contents)))

(defmethod size ((buffer obinseq-buffer))
  (obinseq-length (slot-value buffer 'contents)))

(defmethod size ((buffer binseq2-buffer))
  (binseq2-size (slot-value buffer 'contents)))

(defmethod number-of-lines ((buffer persistent-buffer))
  (loop for offset from 0 below (size buffer)
     count (eql (buffer-object buffer offset) #\Newline)))

(defmethod number-of-lines ((buffer binseq2-buffer))
  (let ((len (binseq2-length (slot-value buffer 'contents)))
	(size (size buffer)))
    (if (or (eql 0 size)
	    (eq (buffer-object buffer (1- size)) #\Newline))
	len
	(max 0 (1- len))))) ; weird?

(defmethod mark< ((mark1 p-mark-mixin) (mark2 p-mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (< (offset mark1) (offset mark2)))

(defmethod mark< ((mark1 p-mark-mixin) (mark2 integer))
  (< (offset mark1) mark2))

(defmethod mark< ((mark1 integer) (mark2 p-mark-mixin))
  (< mark1 (offset mark2)))

(defmethod mark<= ((mark1 p-mark-mixin) (mark2 p-mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (<= (offset mark1) (offset mark2)))

(defmethod mark<= ((mark1 p-mark-mixin) (mark2 integer))
  (<= (offset mark1) mark2))

(defmethod mark<= ((mark1 integer) (mark2 p-mark-mixin))
  (<= mark1 (offset mark2)))

(defmethod mark= ((mark1 p-mark-mixin) (mark2 p-mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (= (offset mark1) (offset mark2)))

(defmethod mark= ((mark1 p-mark-mixin) (mark2 integer))
  (= (offset mark1) mark2))

(defmethod mark= ((mark1 integer) (mark2 p-mark-mixin))
  (= mark1 (offset mark2)))

(defmethod mark> ((mark1 p-mark-mixin) (mark2 p-mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (> (offset mark1) (offset mark2)))

(defmethod mark> ((mark1 p-mark-mixin) (mark2 integer))
  (> (offset mark1) mark2))

(defmethod mark> ((mark1 integer) (mark2 p-mark-mixin))
  (> mark1 (offset mark2)))

(defmethod mark>= ((mark1 p-mark-mixin) (mark2 p-mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (>= (offset mark1) (offset mark2)))

(defmethod mark>= ((mark1 p-mark-mixin) (mark2 integer))
  (>= (offset mark1) mark2))

(defmethod mark>= ((mark1 integer) (mark2 p-mark-mixin))
  (>= mark1 (offset mark2)))

(defmethod beginning-of-buffer ((mark p-mark-mixin))
  (setf (offset mark) 0))

(defmethod end-of-buffer ((mark p-mark-mixin))
  (setf (offset mark) (size (buffer mark))))

(defmethod beginning-of-buffer-p ((mark p-mark-mixin))
  (zerop (offset mark)))

(defmethod end-of-buffer-p ((mark p-mark-mixin))
  (= (offset mark) (size (buffer mark))))

(defmethod beginning-of-line-p ((mark p-mark-mixin))
  (or (beginning-of-buffer-p mark)
      (eql (object-before mark) #\Newline)))

(defmethod end-of-line-p ((mark p-mark-mixin))
  (or (end-of-buffer-p mark)
      (eql (object-after mark) #\Newline)))

(defmethod beginning-of-line ((mark p-mark-mixin))
  (loop until (beginning-of-line-p mark)
	do (decf (offset mark))))

(defmethod beginning-of-line ((mark p-line-mark-mixin))
  (setf (offset mark)
	(binseq2-offset
	 (slot-value (buffer mark) 'contents) (line-number mark))))

(defmethod end-of-line ((mark p-mark-mixin))
  (let* ((offset (offset mark))
	 (buffer (buffer mark))
	 (size (size buffer)))
    (loop until (or (= offset size)
		    (eql (buffer-object buffer offset) #\Newline))
	  do (incf offset))
    (setf (offset mark) offset)))

(defmethod end-of-line ((mark p-line-mark-mixin))
  (let* ((offset (offset mark))
	 (buffer (buffer mark))
	 (size (size buffer))
	 (contents (slot-value buffer 'contents))
	 (next-line-offset
	  (binseq2-offset contents (1+ (binseq2-line2 contents offset)))))
    (setf (offset mark)
	  (cond
	    ((> next-line-offset offset) (1- next-line-offset))
	    ((and (> size 0) (eql (binseq2-get2 contents (1- size)) #\Newline))
	     (1- size))
	    (t size)))))

(defmethod buffer-line-number ((buffer persistent-buffer) (offset integer))
  (loop for i from 0 below offset
     count (eql (buffer-object buffer i) #\Newline)))

(defmethod buffer-line-number ((buffer binseq2-buffer) (offset integer))
  (binseq2-line2 (slot-value buffer 'contents) offset))

(defmethod line-number ((mark p-mark-mixin))
  (buffer-line-number (buffer mark) (offset mark)))

(defgeneric buffer-line-offset (buffer line-number))

(defmethod buffer-line-offset ((buffer binseq2-buffer) (line-no integer))
  (binseq2-offset (slot-value buffer 'contents) line-no))

(defmethod buffer-column-number ((buffer persistent-buffer) (offset integer))
  (loop for i downfrom offset
     while (> i 0)
     until (eql (buffer-object buffer (1- i)) #\Newline)
     count t))

(defmethod buffer-column-number ((buffer binseq2-buffer) (offset integer))
  (- offset
     (binseq2-offset
      (slot-value buffer 'contents) (buffer-line-number buffer offset))))

(defmethod column-number ((mark p-mark-mixin))
  (buffer-column-number (buffer mark) (offset mark)))

;;; the old value of the CONTENTS slot is dropped upon modification
;;; it can be saved for UNDO purposes in a history tree, by an UNDOABLE-BUFFER

(defmethod insert-buffer-object ((buffer binseq-buffer) offset object)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (setf (slot-value buffer 'contents)
	(binseq-insert (slot-value buffer 'contents) offset object)))

(defmethod insert-buffer-object ((buffer obinseq-buffer) offset object)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (setf (slot-value buffer 'contents)
	(obinseq-insert (slot-value buffer 'contents) offset object)))

(defmethod insert-buffer-object ((buffer binseq2-buffer) offset object)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
          (make-condition 'offset-after-end :offset offset))
  (setf (slot-value buffer 'contents)
	(binseq2-insert2 (slot-value buffer 'contents) offset object)))

(defmethod insert-object ((mark p-mark-mixin) object)
  (insert-buffer-object (buffer mark) (offset mark) object))

(defmethod insert-buffer-sequence ((buffer binseq-buffer) offset sequence)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (let ((binseq (vector-binseq sequence)))
    (setf (slot-value buffer 'contents)
	  (binseq-insert* (slot-value buffer 'contents) offset binseq))))

(defmethod insert-buffer-sequence ((buffer obinseq-buffer) offset sequence)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (let ((obinseq (vector-obinseq sequence)))
    (setf (slot-value buffer 'contents)
	  (obinseq-insert* (slot-value buffer 'contents) offset obinseq))))

(defmethod insert-buffer-sequence ((buffer binseq2-buffer) offset sequence)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (let ((binseq2 (vector-binseq2 sequence)))
    (setf (slot-value buffer 'contents)
	  (binseq2-insert*2 (slot-value buffer 'contents) offset binseq2))))

(defmethod insert-sequence ((mark p-mark-mixin) sequence)
  (insert-buffer-sequence (buffer mark) (offset mark) sequence))

(defmethod delete-buffer-range ((buffer binseq-buffer) offset n)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (assert (<= (+ offset n) (size buffer)) ()
          (make-condition 'offset-after-end :offset (+ offset n)))
  (setf (slot-value buffer 'contents)
	(binseq-remove* (slot-value buffer 'contents) offset n)))

(defmethod delete-buffer-range ((buffer obinseq-buffer) offset n)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (assert (<= (+ offset n) (size buffer)) ()
          (make-condition 'offset-after-end :offset (+ offset n)))
  (setf (slot-value buffer 'contents)
	(obinseq-remove* (slot-value buffer 'contents) offset n)))

(defmethod delete-buffer-range ((buffer binseq2-buffer) offset n)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset))
  (assert (<= (+ offset n) (size buffer)) ()
          (make-condition 'offset-after-end :offset (+ offset n)))
  (setf (slot-value buffer 'contents)
	(binseq2-remove*2 (slot-value buffer 'contents) offset n)))

(defmethod delete-range ((mark p-mark-mixin) &optional (n 1))
  (cond
    ((plusp n) (delete-buffer-range (buffer mark) (offset mark) n))
    ((minusp n) (delete-buffer-range (buffer mark) (+ (offset mark) n) (- n)))
    (t nil)))

(defmethod delete-region ((mark1 p-mark-mixin) (mark2 p-mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
        (offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (delete-buffer-range (buffer mark1) offset1 (- offset2 offset1))))

(defmethod delete-region ((mark1 p-mark-mixin) offset2)
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (delete-buffer-range (buffer mark1) offset1 (- offset2 offset1))))

(defmethod delete-region (offset1 (mark2 p-mark-mixin))
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (delete-buffer-range (buffer mark2) offset1 (- offset2 offset1))))

(defmethod buffer-object ((buffer binseq-buffer) offset)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (1- (size buffer))) ()
	  (make-condition 'offset-after-end :offset offset))
  (binseq-get (slot-value buffer 'contents) offset))

(defmethod (setf buffer-object) (object (buffer binseq-buffer) offset)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (1- (size buffer))) ()
	  (make-condition 'offset-after-end :offset offset))
  (setf (slot-value buffer 'contents)
	(binseq-set (slot-value buffer 'contents) offset object)))

(defmethod buffer-object ((buffer obinseq-buffer) offset)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (1- (size buffer))) ()
	  (make-condition 'offset-after-end :offset offset))
  (obinseq-get (slot-value buffer 'contents) offset))

(defmethod (setf buffer-object) (object (buffer obinseq-buffer) offset)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (1- (size buffer))) ()
	  (make-condition 'offset-after-end :offset offset))
  (setf (slot-value buffer 'contents)
	(obinseq-set (slot-value buffer 'contents) offset object)))

(defmethod buffer-object ((buffer binseq2-buffer) offset)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (1- (size buffer))) ()
	  (make-condition 'offset-after-end :offset offset))
  (binseq2-get2 (slot-value buffer 'contents) offset))

(defmethod (setf buffer-object) (object (buffer binseq2-buffer) offset)
  (assert (<= 0 offset) ()
	  (make-condition 'offset-before-beginning :offset offset))
  (assert (<= offset (1- (size buffer))) ()
	  (make-condition 'offset-after-end :offset offset))
  (setf (slot-value buffer 'contents)
	(binseq2-set2 (slot-value buffer 'contents) offset object)))

(defmethod buffer-sequence ((buffer binseq-buffer) offset1 offset2)
  (assert (<= 0 offset1) ()
	  (make-condition 'offset-before-beginning :offset offset1))
  (assert (<= offset1 (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset1))
  (assert (<= 0 offset2) ()
	  (make-condition 'offset-before-beginning :offset offset2))
  (assert (<= offset2 (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset2))
  (let ((len (- offset2 offset1)))
    (if (> len 0)
	(binseq-vector
	 (binseq-sub (slot-value buffer 'contents) offset1 len))
	(make-array 0))))

(defmethod buffer-sequence ((buffer obinseq-buffer) offset1 offset2)
  (assert (<= 0 offset1) ()
	  (make-condition 'offset-before-beginning :offset offset1))
  (assert (<= offset1 (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset1))
  (assert (<= 0 offset2) ()
	  (make-condition 'offset-before-beginning :offset offset2))
  (assert (<= offset2 (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset2))
  (let ((len (- offset2 offset1)))
    (if (> len 0)
	(obinseq-vector
	 (obinseq-sub (slot-value buffer 'contents) offset1 len))
	(make-array 0))))

(defmethod buffer-sequence ((buffer binseq2-buffer) offset1 offset2)
  (assert (<= 0 offset1) ()
	  (make-condition 'offset-before-beginning :offset offset1))
  (assert (<= offset1 (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset1))
  (assert (<= 0 offset2) ()
	  (make-condition 'offset-before-beginning :offset offset2))
  (assert (<= offset2 (size buffer)) ()
	  (make-condition 'offset-after-end :offset offset2))
  (let ((len (- offset2 offset1)))
    (if (> len 0)
	(binseq2-vector
	 (binseq2-sub2 (slot-value buffer 'contents) offset1 len))
	(make-array 0))))

(defmethod object-before ((mark p-mark-mixin))
  (buffer-object (buffer mark) (1- (offset mark))))

(defmethod object-after ((mark p-mark-mixin))
  (buffer-object (buffer mark) (offset mark)))

(defmethod region-to-sequence ((mark1 p-mark-mixin) (mark2 p-mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (buffer-sequence (buffer mark1) offset1 offset2)))

(defmethod region-to-sequence ((offset1 integer) (mark2 p-mark-mixin))
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (buffer-sequence (buffer mark2) offset1 offset2)))

(defmethod region-to-sequence ((mark1 p-mark-mixin) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (buffer-sequence (buffer mark1) offset1 offset2)))

(defmacro filter-and-update (l filter-fn update-fn)
  (let ((prev (gensym))
	(curr (gensym))
	(kept (gensym)))
    `(loop
	with ,prev = nil
	and ,curr = ,l
	and ,kept = nil
	do (cond
	     ((null ,curr) (return))
	     ((setf ,kept (funcall ,filter-fn (car ,curr)))
	      (funcall ,update-fn ,kept)
	      (setf ,prev ,curr
		    ,curr (cdr ,curr)))
	     (t (if ,prev
		    (setf (cdr ,prev) (cdr ,curr))
		    (setf ,l (cdr ,l)))
		(setf ,curr (cdr ,curr)))))))

(defun adjust-cursors-on-insert (buffer start &optional (increment 1))
  (filter-and-update
   (cursors buffer)
   #'(lambda (c) (flexichain::weak-pointer-value c))
   #'(lambda (wpc)
       (when (<= start (slot-value wpc 'pos))
	 (incf (slot-value wpc 'pos) increment)))))

(defun adjust-cursors-on-delete (buffer start n)
  (let ((end (+ start n)))
    (filter-and-update
     (cursors buffer)
     #'(lambda (c) (flexichain::weak-pointer-value c))
     #'(lambda (wpc)
	 (cond
	   ((<= (cursor-pos wpc) start))
	   ((< start (cursor-pos wpc) end)
	    (setf (cursor-pos wpc) start))
	   (t (decf (cursor-pos wpc) n)))))))

(defmethod insert-buffer-object
    :after ((buffer persistent-buffer) offset object)
  (adjust-cursors-on-insert buffer offset))

(defmethod insert-buffer-sequence
    :after ((buffer persistent-buffer) offset sequence)
  (adjust-cursors-on-insert buffer offset (length sequence)))

(defmethod delete-buffer-range
    :after ((buffer persistent-buffer) offset n)
  (adjust-cursors-on-delete buffer offset n))

(defmethod make-buffer-mark ((buffer persistent-buffer)
                             &optional (offset 0) (stick-to :left))
  (make-instance (ecase stick-to
                   (:left 'persistent-left-sticky-mark)
                   (:right 'persistent-right-sticky-mark))
   :offset offset :buffer buffer))
