;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2002-2004 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)

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

#|
Incremental Redisplay Theory of Operation

Incremental redisplay compares the tree of output records before and after
calling REDISPLAY and updates those parts of the screen that are different.
UPDATING-OUTPUT forms in the code create UPDATING-OUTPUT-RECORDs in the record
tree. These records hold the before and after snapshots of the tree.  When the
display code is first run, the bodies of all the UPDATING-OUTPUT forms are
captured as closures.  Usually only the closure in the top-level output record
will ever get called, but the programmer can call REDISPLAY on any
updating output record, so we have to be prepared for that.

Redisplay proceeds thus:

All the updating output records are visited.  Their state is changed to
:UPDATING and the OLD-CHILDREN slot is set to the current children.

The closure of the root updating output record is called.  None of the
closures  the child updating output records are called because any free
variables captured in the UPDATING-OUTPUT forms need to see the fresh bindings
from this run of the code.As UPDATING-OUTPUT forms are encountered, several
things can happen:

* The cache value of the form compares to the value stored in the record.  The
record, and all the updating output records below it, are marked :clean.  The
body of UPDATING-OUTPUT isn't run.

* The cache value doesn't compare.  The record is marked :UPDATED, and the body
is run.

* There isn't an existing UPDATING-OUTPUT-RECORD for this UPDATING-OUTPUT
form.  One is created in state :UPDATED.  The body is run.

Before the top level UPDATING-OUTPUT closure returns, various output records
in the history tree might be mutated e.g., be moved. The most common case of
this is in table layout, where the records for each cell are first created and
then assigned a final location based on the dimensions of the table. But these
nodes may be children of an updating output record that has been marked
:CLEAN. Therefore, they have to be treated specially so that the rest of
incremental redisplay will consider them and not leave the screen in a trashed
state. An around method on (SETF OUTPUT-RECORD-POSITION) for display records
checks if incremental redisplay is in progress; if so, it stores the mutated
record in its closest parent UPDATING-OUTPUT record (if any). If that parent
is :CLEAN then it and any other clean parent updating output records are
marked as :UPDATED.


Next, COMPUTE-DIFFERENCE-SET compares the old and new trees.  New output
records that aren't in the old tree need to be drawn.  Old records not in the
new tree need to be erased. Display records that were moved need are erased
and redrawn. COMPUTE-DIFFERENCE-SET only compares display output records that
are direct descendents (i.e., no intervening UPDATING-OUTPUT record) of an
updating output record; Compute-difference-set is called recursively on any
children updating output records.

As an optimization, COMPUTE-DIFFERENCE-SET ignores records that are outside of
the pane's visible regin.

Finally, the old tree is walked.  All updating output records in state
:UPDATING were not visited at all and thus are deleted from their parent
caches.


Problems / Future work

The complete traversals of the output history tree could be avoided by keeping
a generation number in the updating output record and updating that everytime
the node is visited.

The search for equal display nodes is expensive in part because we have no
spatially organized data structure.

|#

;;; The map from unique values to output records. Unfortunately the :ID-TEST
;;; is specified in the child updating output records, not in the record that
;;; holds the cache! So, the map lookup code jumps through some hoops to use a
;;; hash table if the child id tests allow that and if there enough records in
;;; the map to make that worthwhile.

(defclass updating-output-map-mixin ()
  ((id-map :accessor id-map :initform nil)
   (id-counter :accessor id-counter
	       :documentation "The counter used to assign unique ids to
		updating output records without one.")
   (tester-function :accessor tester-function :initform 'none
		    :documentation "The function used to lookup
  updating output records in this map if unique; otherwise, :mismatch.")
   (element-count :accessor element-count :initform 0)))

;;; Complete guess...
(defparameter *updating-map-threshold* 10
  "The limit at which the id map in an updating output record switches to a
  hash table.")

;;; ((eq map-test-func :mismatch)
;;;   nil)
(defun function-matches-p (map func)
  (let ((map-test-func (tester-function map)))
    (cond ((eq map-test-func func)
	   t)
	  ((and (symbolp map-test-func) (symbolp func)) ; not eq
	   nil)
	  ((and (symbolp map-test-func) (fboundp map-test-func))
	   (eq (symbol-function map-test-func) func))
	  ((and (symbolp func) (fboundp func))
	   (eq map-test-func (symbol-function func)))
	  (t nil))))

(defun ensure-test (map test)
  (unless (function-matches-p map test)
    (explode-map-hash map)
    (setf (tester-function map) :mismatch)))

(defgeneric clear-map (map))

(defmethod clear-map ((map updating-output-map-mixin))
  (setf (id-map map) nil)
  (setf (id-counter map) 0)
  (setf (element-count map) 0))

;;; Perhaps these should be generic functions, but in the name of premature
;;; optimization they're not :)
(defun get-from-map (map value test)
  (when (eq (tester-function map) 'none)
    (return-from get-from-map nil))
  (ensure-test map test)
  (let ((map (id-map map)))
    (if (hash-table-p map)
	(gethash value map)
	(cdr (assoc value map :test test)))))


(defun maybe-convert-to-hash (map)
  (let ((test (tester-function map)))
    (when (and (not (eq test :mismatch))
	       (> (element-count map) *updating-map-threshold*)
	       (or (case test
		     ((eq eql equal equalp) t))
		   (eq test #'eq)
		   (eq test #'eql)
		   (eq test #'equal)
		   (eq test #'equalp)))
      (let ((new-map (make-hash-table :test test)))
	(loop
	   for (key . value) in (id-map map)
	   do (setf (gethash key new-map) value))
	(setf (id-map map) new-map)))))

(defun explode-map-hash (map)
  (let ((hash-map (id-map map)))
    (when (hash-table-p hash-map)
      (loop
	 for key being each hash-key of hash-map using (hash-value record)
	 collect (cons key record) into alist
	 finally (setf (id-map map) alist)))))

(defun add-to-map (map record value test replace)
  (if (eq (tester-function map) 'none)
      (setf (tester-function map) test)
      (ensure-test map test))
  (let ((val-map (id-map map)))
    (if (hash-table-p val-map)
	(multiple-value-bind (existing-value in-table)
	    (if replace
		(gethash value val-map)
		(values nil nil))
	  (declare (ignore existing-value))
	  (setf (gethash value val-map) record)
	  (unless in-table
	    (incf (element-count map))))
	(let ((val-cons (if replace
			    (assoc value val-map :test test)
			    nil)))
	  (if val-cons
	      (setf (cdr val-cons) record)
	      (progn
		(setf (id-map map) (acons value record val-map))
		(incf (element-count map))
		(maybe-convert-to-hash map)))))))

(defun delete-from-map (map value test)
  (ensure-test map test)
  (let ((val-map (id-map map))
	(deleted nil))
    (if (hash-table-p val-map)
	(setf deleted (remhash value val-map))
	(setf (values (id-map map) deleted)
	      (delete-1 value val-map :test test :key #'car)))
    (when deleted
      (decf (element-count map)))))

;;; Reset the ID counter so that updating output records without explicit IDs
;;; can be assigned one during a run of the code. I'm not sure about using
;;; reinitialize-instance for this...
(defmethod shared-initialize :after ((obj updating-output-map-mixin) slot-names
				     &key)
  (declare (ignore slot-names))
  (setf (id-counter obj) 0))

;;; Should this have a more complete CPL, to pull in the fact that it needs a
;;; medium for graphics state?
(defclass updating-output-stream-mixin (updating-output-map-mixin
					extended-output-stream)
  ((redisplaying-p :reader stream-redisplaying-p :initform nil)
   (do-note-output-record :accessor do-note-output-record :initform t)
   (incremental-redisplay :initform nil
			  :initarg :incremental-redisplay
			  :accessor pane-incremental-redisplay)
   (updating-record :accessor updating-record
		    :initarg :updating-record :initform nil
		    :documentation "For incremental output, holds the
   top level updating-output-record.")))

(defgeneric redisplayable-stream-p (stream))

(defmethod redisplayable-stream-p ((stream t))
  nil)

(defmethod redisplayable-stream-p ((stream updating-output-stream-mixin))
  t)

(defmethod pane-needs-redisplay :around ((pane updating-output-stream-mixin))
  (let ((redisplayp (call-next-method)))
    (values redisplayp (and (not (eq redisplayp :no-clear))
			    (not (pane-incremental-redisplay pane))))))

(defmethod window-clear :after ((pane updating-output-stream-mixin))
  "Get rid of any updating output records stored in the stream; they're gone
  from the screen."
  (clear-map pane))

;;; INCREMENTAL-DISPLAY takes as input the difference set computed by
;;; COMPUTE-DIFFERENCE-SET and updates the screen. The 5 kinds of updates are
;;; not very well defined in the spec. I understand their semantics thus:
;;;
;;; Erases, moves, and draws refer to records that don't overlap *with other
;;; records that survive in the current rendering*. In other words, they don't
;;; overlap with records that were not considered by COMPUTE-DIFFRENCE-SET,
;;; either because they are children of a clean updating output node or they
;;; are in another part of the output history that is not being
;;; redisplayed. Also, moves and draws can not overlap each other. It is fine
;;; for erases and draws to overlap. Another way to think about erases, moves
;;; and draws is in terms of a possible implementation: they could be handled
;;; using only operations on the screen itself. First all the erase regions
;;; would be erased, the moves would be blitted, and then the individual draws
;;; records would be redisplayed.
;;;
;;; Records in erase-overlapping and move-overlapping might overlap with any
;;; other record. They need to be implemented by erasing their region on the
;;; screen and then replaying the output history for that region. Thus, any
;;; ordering issues implied by overlapping records is handled correctly. Note
;;; that draw records that overlap are included in erase-overlapping; the draw
;;; operation itself occurs when the screen is refreshed from the output
;;; history. -- moore

(defgeneric incremental-redisplay
    (stream position erases moves draws erase-overlapping move-overlapping))

(defmethod incremental-redisplay ((stream updating-output-stream-mixin) position
                                  erases moves draws erase-overlapping move-overlapping)
  (declare (ignore position))
  (let ((history (stream-output-history stream)))
    (with-output-recording-options (stream :record nil :draw t)
      (loop
          for (nil br) in erases
          do (erase-rectangle stream br))
      (loop
          for (nil old-bounding) in moves
          do (erase-rectangle stream old-bounding))
      (loop
          for (nil br) in erase-overlapping
          do (erase-rectangle stream br))
      (loop
          for (nil old-bounding) in move-overlapping
          do (erase-rectangle stream old-bounding)))
    (loop
        for (r) in moves
        do (replay r stream))
    (loop
        for (r) in draws
        do (replay r stream))
    (let ((res +nowhere+))
      (loop for (r) in erase-overlapping do (setf res (region-union res r)))
      (loop for (r) in move-overlapping do (setf res (region-union res r)))
      (replay history stream res))))

;;; FIXME: although this inherits from COMPLETE-MEDIUM-STATE, in fact
;;; it needn't, as we only ever call SET-MEDIUM-CURSOR-POSITION on it.
;;; Until 2006-05-28, we did also use the various medium attributes,
;;; but with the reworking of REPLAY-OUTPUT-RECORD
;;; (STANDARD-DISPLAYED-OUTPUT-RECORD) to use around methods and
;;; WITH-DRAWING-OPTIONS, they are no longer necessary.
(defclass updating-stream-state (complete-medium-state)
  ((cursor-x :accessor cursor-x :initarg :cursor-x :initform 0)
   (cursor-y :accessor cursor-y :initarg :cursor-y :initform 0)))

(defmethod initialize-instance :after ((obj updating-stream-state)
				       &key (stream nil))
  (when stream
    (setf (values (slot-value obj 'cursor-x) (slot-value obj 'cursor-y))
	  (stream-cursor-position stream))))

(defmethod match-output-records-1 and ((state updating-stream-state)
				       &key (cursor-x 0 x-supplied-p)
				       (cursor-y 0 y-supplied-p))
  (and (or (not x-supplied-p)
	   (coordinate= (slot-value state 'cursor-x) cursor-x))
       (or (not y-supplied-p)
	   (coordinate= (slot-value state 'cursor-y) cursor-y))))

(defgeneric set-medium-cursor-position (state stream))

(defmethod set-medium-cursor-position
    ((state updating-stream-state) (stream updating-output-stream-mixin))
  (setf (stream-cursor-position stream)
	(values (cursor-x state) (cursor-y state))))

(defmethod medium-graphics-state ((stream updating-output-stream-mixin)
				  &optional state)
  (if (and state (subtypep state 'updating-stream-state))
      (reinitialize-instance state :stream stream)
      (make-instance 'updating-stream-state :stream stream)))

;;; XXX Add to values we test, obviously.
;;;
;;;Well, maybe not.  The goal is to support output records that have moved
;;;but that are otherwise clean. I.e., some previous part of the output has
;;;changed (lines added or deleted, for example). If the stream cursor
;;;position is different, I'm not sure now that the code for the updating
;;;output record needs to be rerun; I think we could use only the difference
;;;in cursor position to move the record. Any other graphics state change --
;;;like a different foreground color -- should probably be handled by the
;;;programmer forcing all new output.

(defun state-matches-stream-p (state stream)
  (multiple-value-bind (cx cy) (stream-cursor-position stream)
    (declare (ignore cy))
    (with-sheet-medium (medium stream)
      ;; Note: We don't match the y coordinate.
      (match-output-records state :cursor-x cx))))

(defclass updating-output-record-mixin (updating-output-map-mixin
					standard-sequence-output-record)
  ((unique-id :reader output-record-unique-id :initarg :unique-id)
   (id-test :reader output-record-id-test :initarg :id-test
	    :initform #'eql)
   (cache-value :reader output-record-cache-value :initarg :cache-value)
   (cache-test :reader output-record-cache-test :initarg :cache-test
	       :initform #'eql)
   (fixed-position :reader output-record-fixed-position
		   :initarg :fixed-position :initform nil)
   (displayer :reader output-record-displayer :initarg :displayer)
   ;; Start and end cursor
   (start-graphics-state :accessor start-graphics-state
			 :initarg :start-graphics-state
			 :documentation "Graphics state needed to
   render record")
   (end-graphics-state :accessor end-graphics-state
		       :initarg :end-graphics-state
		       :documentation "Graphics state after rendering
   record; used to render non updating-output-records that follow")
   (old-children :accessor old-children
		 :documentation "Contains the output record tree for the
  current display.")
   (output-record-dirty :accessor output-record-dirty :initform :updating
	  :documentation
	  ":updating
           :updated
           :clean")
   (parent-cache :accessor parent-cache :initarg :parent-cache
		 :documentation "The parent cache in which this updating output
record is stored.")
   (stream :accessor updating-output-stream :initarg :stream :initform nil
	   :documentation "Capture the screen in order to restrict update to
					visible records")
   (parent-updating-output :accessor parent-updating-output
			   :initarg :parent-updating-output :initform nil
			   :documentation "A backlink to the
updating-output-parent above this one in the tree.")
   ;; Results of (setf output-record-position) while updating
   (explicit-moves :accessor explicit-moves)
   (old-bounds :accessor old-bounds
	       :initform (make-bounding-rectangle 0.0d0 0.0d0 0.0d0 0.0d0)
	       :documentation "Holds the old bounds of an updating output
 record if that can no longer be determined from the old-children.")
   ;; on-screen state?
   ))

(defgeneric sub-record (record)
  (:method ((record updating-output-record-mixin))
    (let ((children (output-record-children record)))
      (if (zerop (length children))
	  nil
	  (aref children 0)))))

(defmethod shared-initialize :after
    ((obj updating-output-record-mixin) slot-names
     &key (x-position 0.0d0) (y-position 0.0d0))
  (declare (ignore x-position y-position))
  (declare (ignore slot-names))
  (setf (explicit-moves obj) nil))

(defmethod output-record-start-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))

(defmethod output-record-end-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (end-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record updating-output-record-mixin))
  (let ((state (end-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))

;;; Prevent deleted output records from coming back from the dead.
(defmethod delete-output-record :after ((child updating-output-record-mixin)
					record
					&optional errorp)
  (declare (ignore record errorp))
  (let ((pcache (parent-cache child)))
    (delete-from-map pcache
		     (output-record-unique-id child)
		     (output-record-id-test child))))


(defclass standard-updating-output-record (updating-output-record-mixin
					   updating-output-record)
  ())

(defmethod print-object ((obj standard-updating-output-record) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-standard-rectangle (x1 y1 x2 y2)
	obj
      (format stream "X ~S:~S Y ~S:~S " x1 x2 y1 y2))
    (when (slot-boundp obj 'unique-id)
      (let ((*print-length* 10)
	    (*print-level* 3))
	(format stream " ~S" (output-record-unique-id obj))))))

;;; Helper function for visiting updating-output records in a tree

(defgeneric map-over-updating-output (function root use-old-records))

(defmethod map-over-updating-output (function
				     (record standard-updating-output-record)
				     use-old-records)
  (funcall function record)
  (let ((children (cond (use-old-records
                         (when (slot-boundp record 'old-children)
                           (old-children record)))
                        (t (sub-record record)))))
    (when children
      (map-over-updating-output function children use-old-records))))


(defmethod map-over-updating-output
    (function (record compound-output-record) use-old-records)
  (flet ((mapper (r)
	   (map-over-updating-output function r use-old-records)))
    (declare (dynamic-extent #'mapper))
    (map-over-output-records #'mapper record)))

(defmethod map-over-updating-output (function record use-old-records)
  (declare (ignore function record use-old-records))
  nil)
;;;
(defvar *current-updating-output* nil)

(defgeneric compute-new-output-records (record stream))

(defgeneric compute-new-output-records-1 (record stream displayer)
  (:documentation "Like compute-new-output-records with an explicit
  displayer function."))

(defmethod compute-new-output-records ((record standard-updating-output-record)
				       stream)
  (with-output-recording-options (stream :record t :draw nil)
    (map-over-updating-output
     #'(lambda (r)
         (let ((sub-record (sub-record r)))
           (when sub-record
             (setf (old-children r) sub-record)
             (setf (output-record-dirty r) :updating)
             (setf (rectangle-edges* (old-bounds r))
                   (rectangle-edges* sub-record)))))
     record
     nil)
    (force-output stream)
    ;; Why is this binding here? We need the "environment" in this call that
    ;; computes the new records of an outer updating output record to resemble
    ;; that when a record's contents are computed in invoke-updating-output.
    (letf (((stream-current-output-record stream)
	    (output-record-parent record)))
      (compute-new-output-records-1 record
				    stream
				    (output-record-displayer record)))))

;;; Create the sub-record that holds the new contents of the updating output
;;; record.
(defun %invoke-updating (record stream displayer)
  (letf (((stream-current-output-record stream) record))
    (with-new-output-record (stream)
      (funcall displayer stream))))

(defmethod compute-new-output-records-1
    ((record standard-updating-output-record) stream displayer)
  (multiple-value-bind (x y)
      (output-record-position record)
    (let ((sub-record (sub-record record)))
      (when sub-record
        (delete-output-record sub-record record)))
    ;; Don't add this record repeatedly to a parent updating-output-record.
    (unless (eq (output-record-parent record)
		(stream-current-output-record stream))
      (setf (output-record-parent record) nil)
      (add-output-record record (stream-current-output-record stream)))
    (reinitialize-instance record :x-position x :y-position y))
  (%invoke-updating record stream displayer)
  (setf (output-record-dirty record) :updated))

(defgeneric find-child-output-record (record use-old-elements record-type
				      &rest initargs
				      &key unique-id unique-id-test))

(defgeneric map-over-displayed-output-records
    (function root use-old-elements clean clip-region)
  (:documentation "Call function on all displayed-output-records in ROOT's
 tree. If USE-OLD-ELEMENTS is true, descend the old branch of
updating output records. If CLEAN is true, descend into clean updating output
records. "))

(defmethod map-over-displayed-output-records :around
    (function root use-old-elements clean (clip-rectangle bounding-rectangle))
  (declare (ignore function use-old-elements clean))
  (when (region-intersects-region-p root clip-rectangle)
    (call-next-method)))

(defmethod map-over-displayed-output-records (function
					      (root standard-updating-output-record)
					      use-old-elements
					      clean
					      clip-rectangle)
  (cond ((and (not clean) (eq (output-record-dirty root) :clean))
	 nil)
	((and use-old-elements (slot-boundp root 'old-children))
	 (map-over-displayed-output-records function
					    (old-children root)
					    use-old-elements
					    clean
					    clip-rectangle))
	((not use-old-elements)
	 (map-over-displayed-output-records function
					    (sub-record root)
					    use-old-elements
					    clean
					    clip-rectangle))
	(t nil)))

(defmethod map-over-displayed-output-records (function
					      (root compound-output-record)
					      use-old-elements
					      clean
					      clip-rectangle)
  (flet ((mapper (record)
	   (map-over-displayed-output-records function
					      record
					      use-old-elements
					      clean
					      clip-rectangle)))
    (declare (dynamic-extent #'mapper))
    (map-over-output-records #'mapper root)))

(defmethod map-over-displayed-output-records (function
					      (root displayed-output-record)
					      use-old-elements
					      clean
					      clip-rectangle)
  (declare (ignore clean use-old-elements clip-rectangle))
  (funcall function root))

(defgeneric compute-difference-set (record &optional check-overlapping
					   offset-x offset-y
					   old-offset-x old-offset-y))

;;; Helper functions for visiting only the highest level updating
;;; output records in a tree and only those display records that are
;;; not under updating output records. Do not pass these the parent
;;; updating output record; pass sub-record or old-children

(defgeneric map-over-child-updating-output (function record clip-rectangle)
  (:documentation "Apply FUNCTION to updating-output records that are
  children of record, but don't recurse into them.")
  (:method (function (record standard-updating-output-record) clip-rectangle)
    (declare (ignore clip-rectangle))
    (funcall function record))
  (:method (function (record compound-output-record) clip-rectangle)
    (flet ((mapper (r)
	     (map-over-child-updating-output function r clip-rectangle)))
      (declare (dynamic-extent #'mapper))
      (map-over-output-records #'mapper record)))
  (:method (function record clip-rectangle)
    (declare (ignore function record clip-rectangle))
    nil)
  (:method :around (function record (clip-rectangle bounding-rectangle))
    (declare (ignore function))
    (when (region-intersects-region-p record clip-rectangle)
      (call-next-method))))

(defgeneric map-over-child-display (function record clip-rectangle)
  (:documentation "Apply function to display records in RECORD's tree that are
  not under updating-output records")
  (:method (function (record displayed-output-record) clip-rectangle)
    (declare (ignore clip-rectangle))
    (funcall function record))
  (:method (function (record compound-output-record) clip-rectangle)
    (flet ((mapper (r)
	     (map-over-child-display function r clip-rectangle)))
      (declare (dynamic-extent #'mapper))
      (map-over-output-records #'mapper record)))
  (:method (function (record standard-updating-output-record) clip-rectangle)
    (declare (ignore function record clip-rectangle))
    nil)
  (:method (function record clip-rectangle)
    (declare (ignore function record clip-rectangle))
    nil)
  (:method :around (function record (clip-rectangle bounding-rectangle))
    (declare (ignore function))
    (when (region-intersects-region-p record clip-rectangle)
      (call-next-method))))

;;; Variation on a theme. Refactor, refactor...

(defgeneric map-over-obsolete-display (function record clip-rectangle)
  (:method (function (record displayed-output-record) clip-rectangle)
    (declare (ignore clip-rectangle))
    (funcall function record))
  (:method (function (record compound-output-record) clip-rectangle)
    (flet ((mapper (r)
	     (map-over-obsolete-display function r clip-rectangle)))
      (declare (dynamic-extent #'mapper))
      (map-over-output-records #'mapper record)))
  (:method (function (record standard-updating-output-record) clip-rectangle)
    (when (eq (output-record-dirty record) :updating)
      (map-over-obsolete-display function (sub-record record) clip-rectangle)))
  (:method (function record clip-rectangle)
    (declare (ignore function record clip-rectangle))
    nil)
  (:method :around (function record (clip-rectangle bounding-rectangle))
    (declare (ignore function))
    (when (region-intersects-region-p record clip-rectangle)
      (call-next-method))))

(defun find-existing-record (display-record root visible-region)
  "Returns a display record that is output-record-equal to display-record
  within visible-region and not under an updating-output record"
  (map-over-child-display #'(lambda (r)
			      (when (output-record-equal display-record r)
				(return-from find-existing-record r)))
			  root
			  visible-region)
  nil)

(defun copy-bounding-rectange (rect)
  (with-bounding-rectangle* (min-x min-y max-x max-y)
      rect
    (make-bounding-rectangle min-x min-y max-x max-y)))

;;; work in progress
(defvar *existing-output-records* nil)

;;;
(defgeneric output-record-hash (record)
  (:documentation "Produce a value that can be used to hash the output record
in an equalp hash table"))

(defmethod output-record-hash ((record standard-bounding-rectangle))
  (slot-value record 'coordinates))

(defconstant +fixnum-bits+ (integer-length most-positive-fixnum))

(declaim (inline hash-coords))
(defun hash-coords (x1 y1 x2 y2)
  (declare (type coordinate x1 y1 x2 y2))
  (let ((hash-val 0))
      (declare (type fixnum hash-val))
      (labels ((rot4 (val)
		 (dpb (ldb (byte 4 0) val)
		      (byte 4 (- +fixnum-bits+ 4 1))
		      (ash val -4)))
	       (mix-it-in (val)
	       (let ((xval (sxhash val)))
		 (declare (type fixnum xval))
		 (when (minusp val)
		   (setq xval (rot4 xval)))
		 (setq hash-val (logxor (rot4 hash-val) xval)))))
	(declare (inline rot4 mix-it-in))
	(mix-it-in x1)
	(mix-it-in y1)
	(mix-it-in x2)
	(mix-it-in y2)
	hash-val)))

(defmethod output-record-hash ((record output-record))
  (with-bounding-rectangle* (x1 y1 x2 y2)
      record
    (hash-coords x1 y1 x2 y2)))

(defmethod compute-difference-set ((record standard-updating-output-record)
				   &optional (check-overlapping t)
                                   offset-x offset-y
                                   old-offset-x old-offset-y)
  (declare (ignore offset-x offset-y old-offset-x old-offset-y))
  ;; (declare (values erases moves draws erase-overlapping move-overlapping))
  (let (was
        is
        stay
        come
        (everywhere (or +everywhere+
                        (pane-viewport-region (updating-output-stream record))))
        (was-table (make-hash-table :test #'equalp))
        (is-table (make-hash-table :test #'equalp)))

    (labels ((collect-1-was (record)
               (push record was)
               (push record (gethash (output-record-hash record) was-table)))
             (collect-1-is (record)
               (push record is)
               (push record (gethash (output-record-hash record) is-table))
               ;; come = is \ was
               ;; stay = is ^ was
               (cond ((updating-output-record-p record)
                      (if (eq :clean (output-record-dirty record))
                          (push record stay)
                          (push record come)))
                     (t
                      (let ((q (gethash (output-record-hash record) was-table)))
                        (if (some #'(lambda (x) (output-record-equal record x)) q)
                            (push record stay)
                            (push record come)))))))
      ;; Collect what was there
      (labels ((gather-was (record)
                 (cond ((displayed-output-record-p record)
                        (collect-1-was record))
                       ((updating-output-record-p record)
                        (cond ((eq :clean (output-record-dirty record))
                               (collect-1-was record))
                              ((eq :moved (output-record-dirty record))
                               (collect-1-was (slot-value record 'old-bounds)))
                              (t
                               (map-over-output-records-overlapping-region #'gather-was
                                                                           (old-children record)
                                                                           everywhere))))
                       (t
                        (map-over-output-records-overlapping-region #'gather-was record everywhere)))))
        (gather-was record))
      ;; Collect what still is there
      (labels ((gather-is (record)
                 (cond ((displayed-output-record-p record)
                        (collect-1-is record))
                       ((updating-output-record-p record)
                        (cond ((eq :clean (output-record-dirty record))
                               (collect-1-is record))
                              ((eq :moved (output-record-dirty record))
                               (collect-1-is record))
                              (t
                               (map-over-output-records-overlapping-region #'gather-is
                                                                           (sub-record record)
                                                                           everywhere))))
                       (t
                        (map-over-output-records-overlapping-region #'gather-is record everywhere) ))))
        (gather-is record)))
    ;;
    (let (gone)
      ;; gone = was \ is
      (loop for w in was do
        (cond ((updating-output-record-p w)
               (unless (eq :clean (output-record-dirty w))
                 (push (old-children w) gone)))
              (t
               (let ((q (gethash (output-record-hash w) is-table)))
                 (unless (some #'(lambda (x) (output-record-equal w x)) q)
                   (push w gone))))))
      ;; Now we essentially want 'gone', 'stay', 'come'
      (let ((gone-overlap nil)
            (come-overlap nil))
        (when check-overlapping
          (setf (values gone gone-overlap)
                (loop for k in gone
                      if (some (lambda (x) (region-intersects-region-p k x))
                               stay)
                        collect (list k k) into gone-overlap*
                      else collect (list k k) into gone*
                      finally (return (values gone* gone-overlap*))))
          (setf (values come come-overlap)
                (loop for k in come
                      if (some (lambda (x) (region-intersects-region-p k x))
                               stay)
                        collect (list k k) into come-overlap*
                      else collect (list k k) into come*
                      finally (return (values come* come-overlap*)))))
        ;; Hmm, we somehow miss come-overlap ...
        (values
         ;; erases
         gone
         ;; moves
         nil
         ;; draws
         (nreverse come)
         ;; erase overlapping
         (append gone-overlap come-overlap)
         ;; move overlapping
         nil)))))

(defvar *trace-updating-output* nil)

(defvar *no-unique-id* (cons nil nil))

(defun move-output-record (record dx dy)
  (multiple-value-bind (x y) (output-record-position record)
    (setf (output-record-position record)
          (values (+ x dx) (+ y dy))))
  ;; Cursor positions are only guaranteed to be non-nil for text
  ;; output records (16.2.1 The Basic Output Record Protocol)
  (multiple-value-bind (x y) (output-record-start-cursor-position record)
    (when (and x y)
      (setf (output-record-start-cursor-position record)
            (values (+ x dx) (+ y dy)))))
  (multiple-value-bind (x y) (output-record-end-cursor-position record)
    (when (and x y)
      (setf (output-record-end-cursor-position record)
            (values (+ x dx) (+ y dy))))))

(defmethod invoke-updating-output ((stream updating-output-stream-mixin)
				   continuation
				   record-type
				   unique-id id-test cache-value cache-test
				   &key (fixed-position nil) (all-new nil)
				   (parent-cache nil))
  (force-output stream)
  (let ((parent-cache (or parent-cache *current-updating-output* stream)))
    (when (eq unique-id *no-unique-id*)
      (setq unique-id (incf (id-counter parent-cache))))
    (let* ((record (get-from-map parent-cache unique-id id-test))
	   ;; For debugging
	   state-mismatch)
      (cond ((or all-new (null record))
	     ;; This case covers the outermost updating-output too.
	     (with-new-output-record (stream
				      record-type
				      *current-updating-output*
				      :unique-id unique-id
				      :id-test id-test
				      :cache-value cache-value
				      :cache-test cache-test
				      :fixed-position fixed-position
				      :displayer continuation
				      :parent-cache parent-cache
				      :stream stream
				      :parent-updating-output
				      *current-updating-output*)
	       (setq record *current-updating-output*)
	       (when *trace-updating-output*
		 (format *trace-output* "Creating ~S~%" record))
	       (setf (start-graphics-state record)
		     (medium-graphics-state stream))
	       (%invoke-updating record stream continuation)
	       (setf (end-graphics-state record)
		     (medium-graphics-state stream))
	       (add-to-map parent-cache record  unique-id id-test all-new)))
	    ((or (setq state-mismatch (not (state-matches-stream-p (start-graphics-state record) stream)))
		 (not (funcall cache-test cache-value (output-record-cache-value record))))
	     (when *trace-updating-output*
	       (format *trace-output* "~:[cache test~;stream state~] ~S~%" state-mismatch record))
	     (let ((*current-updating-output* record))
	       (setf (start-graphics-state record)
		     (medium-graphics-state stream))
	       (compute-new-output-records-1 record stream continuation)
	       (setf (slot-value record 'cache-value) cache-value)
	       (setf (end-graphics-state record)
		     (medium-graphics-state stream))
	       (setf (parent-cache record) parent-cache)))
	    (t
	     ;; It doesn't need to be updated, but it does go into the
	     ;; parent's sequence of records
             ;;
             (multiple-value-bind (cx cy) (stream-cursor-position stream)
               (multiple-value-bind (sx sy) (output-record-start-cursor-position record)
                 (let ((dx (- cx sx))
                       (dy (- cy sy)))
                   (unless (zerop dy)
                     (move-output-record record dx dy) )
                   (let ((tag (cond
				((= dx dy 0)
				 (when *trace-updating-output*
				   (format *trace-output* "clean ~S~%" record))
				 :clean)
				(t
				 (when *trace-updating-output*
				   (format *trace-output* "moved ~S~%" record))
				 :moved))))
                     (setf (output-record-dirty record) tag)
                     (setf (output-record-parent record) nil)
                     (map-over-updating-output #'(lambda (r)
                                                   (unless (eq r record)
                                                     (incf (slot-value (start-graphics-state r) 'cursor-x) dx)
                                                     (incf (slot-value (start-graphics-state r) 'cursor-y) dy)
                                                     (incf (slot-value (end-graphics-state r) 'cursor-x) dx)
                                                     (incf (slot-value (end-graphics-state r) 'cursor-y) dy))
                                                   (setf (output-record-dirty r) tag))
                                               record
                                               nil)
                     (add-output-record record (stream-current-output-record stream))
                     (set-medium-cursor-position (end-graphics-state record) stream)
                     (setf (parent-cache record) parent-cache) )) ))))
      record)))

;;; The Franz user guide says that updating-output does
;;; &allow-other-keys, and some code I've encountered does mention
;;; other magical arguments, so we'll do the same. -- moore
(defun force-update-cache-test (a b)
  (declare (ignore a b))
  nil)

(defmacro updating-output
    ((stream
      &key (unique-id '*no-unique-id*) (id-test '#'eql)
      (cache-value ''no-cache-value cache-value-supplied-p)
      (cache-test '#'eql)
      (fixed-position nil fixed-position-p)
      (all-new nil all-new-p)
      (parent-cache nil parent-cache-p)
      (record-type ''standard-updating-output-record)
      &allow-other-keys)
     &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (unless cache-value-supplied-p
    (setq cache-test '#'force-update-cache-test))
  (let ((func (gensym "UPDATING-OUTPUT-CONTINUATION")))
    `(flet ((,func (,stream)
	      (declare (ignorable ,stream))
	      ,@body))
       (invoke-updating-output ,stream #',func ,record-type ,unique-id
			       ,id-test ,cache-value ,cache-test
	                       ,@ (and fixed-position-p
				       `(:fixed-position ,fixed-position))
			       ,@(and all-new-p `(:all-new ,all-new))
			       ,@(and parent-cache-p
				      `(:parent-cache ,parent-cache))))))

(defun redisplay (record stream &key (check-overlapping t))
  (redisplay-output-record record stream check-overlapping))

;;; Take the spec at its word that the x/y and parent-x/parent-y arguments are
;;; "entirely bogus."

(defvar *dump-updating-output* nil)

(defgeneric redisplay-output-record (record stream
				     &optional check-overlapping))

(defmethod redisplay-output-record ((record updating-output-record)
				    (stream updating-output-stream-mixin)
				    &optional (check-overlapping t))
  (letf (((slot-value stream 'redisplaying-p) t))
    (let ((*current-updating-output* record)
	  (current-graphics-state (medium-graphics-state stream)))
      (unwind-protect
	   (progn
	     (letf (((do-note-output-record stream) nil))
	       (set-medium-cursor-position (start-graphics-state record) stream)
	       (compute-new-output-records record stream)
	       (when *dump-updating-output*
		 (dump-updating record :both *trace-output*)))
	     (multiple-value-bind
		   (erases moves draws erase-overlapping move-overlapping)
		 (compute-difference-set record check-overlapping)
	       (when *trace-updating-output*
		 (let ((*print-pretty* t))
		   (format *trace-output*
			   "erases: ~S~%moves: ~S~%draws: ~S~%erase ~
				    overlapping: ~S~%move overlapping: ~S~%"
			   erases moves draws
			   erase-overlapping move-overlapping)))
	       (incremental-redisplay stream nil erases moves draws
				      erase-overlapping move-overlapping))
	     (delete-stale-updating-output record))
	(set-medium-cursor-position current-graphics-state stream)))))

(defun erase-rectangle (stream bounding)
  (with-bounding-rectangle* (x1 y1 x2 y2)
      bounding
    (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+)))

(defun clear-moved-record (stream new-bounding old-bounding)
  (with-bounding-rectangle* (x1 y1 x2 y2)
      new-bounding
    (draw-rectangle* stream x1 y1 x2 y2
		     :ink +background-ink+))
  (with-bounding-rectangle* (x1 y1 x2 y2)
      old-bounding
    (draw-rectangle* stream x1 y1 x2 y2
		     :ink +background-ink+)))

;;; Suppress the got-sheet/lost-sheet notices during redisplay.

(defmethod note-output-record-lost-sheet :around
    (record (sheet updating-output-stream-mixin))
  (declare (ignore record))
  (when (do-note-output-record sheet)
    (call-next-method)))

(defmethod note-output-record-got-sheet :around
    (record (sheet updating-output-stream-mixin))
  (declare (ignore record))
  (when (do-note-output-record sheet)
    (call-next-method)))

(defun delete-stale-updating-output (record)
  (map-over-updating-output
   #'(lambda (r)
       (when (eq (output-record-dirty r) :updating)
	 (delete-from-map (parent-cache r)
			  (output-record-unique-id r)
			  (output-record-id-test r))))
   record
   t))


;;; Support for explicitly changing output records

(defun mark-updating-output-changed (record)
  (let ((state (output-record-dirty record)))
    (cond ((or (eq record *current-updating-output*)
	       (eq state :updated)
	       (eq state :updating))
	   nil)
	  ((eq state :clean)
	   (setf (output-record-dirty record) :updated)
	   (let ((parent (parent-updating-output record)))
	     (if (null parent)
		 (error "parent of ~S null" record)
		 (mark-updating-output-changed parent))))
	  (t nil))))

(defgeneric propagate-to-updating-output
    (record child mode old-bounding-rectangle)
  (:method
      ((record updating-output-record-mixin) child mode old-bounding-rectangle)
    (when (eq (output-record-dirty record) :clean)
      (case mode
	(:move
	 (push (list child old-bounding-rectangle nil) (explicit-moves record))
	 (mark-updating-output-changed record)))))
  (:method
      ((record output-record) child mode old-bounding-rectangle)
    (let ((parent (output-record-parent record)))
      (when parent
	(propagate-to-updating-output
	 parent child mode old-bounding-rectangle)))))

(defgeneric note-output-record-child-changed
    (record child mode old-position old-bounding-rectangle stream
     &optional erases moves draws erase-overlapping move-overlapping
     &key check-overlapping))

;;; The default - do nothing

(defmethod note-output-record-child-changed
    (record child mode old-position old-bounding-rectangle stream
     &optional erases moves draws erase-overlapping move-overlapping
     &key check-overlapping)
  (declare (ignore record child mode old-position old-bounding-rectangle stream
                   erases moves draws erase-overlapping move-overlapping
                   check-overlapping))
  nil)

(defmethod note-output-record-child-changed
    (record (child displayed-output-record) (mode (eql :move))
     old-position old-bounding-rectangle
     (stream updating-output-stream-mixin)
     &optional erases moves draws erase-overlapping move-overlapping
     &key (check-overlapping t))
  (declare (ignore old-position erases moves draws erase-overlapping
                   move-overlapping
                   check-overlapping))
  (when (stream-redisplaying-p stream)
    (propagate-to-updating-output record child  mode old-bounding-rectangle)))

(defmethod* (setf output-record-position) :around
    (nx ny (record displayed-output-record))
  (with-bounding-rectangle* (x y max-x max-y)
      record
    (multiple-value-prog1
	(call-next-method)
      ;; coordinate= here instead?
      (unless (and (= x nx) (= y ny))
	(let ((stream (and (slot-exists-p record 'stream)
			   (slot-value  record 'stream)))
	      (parent (output-record-parent record)))
	  (when (and stream parent)
	    (note-output-record-child-changed
	     parent record :move
	     (make-point x y) (make-bounding-rectangle x y max-x max-y)
	     stream)))))))

;;; Debugging hacks
(defun dump-updating (record old-records &optional (stream *standard-output*))
  (let ((*print-circle* t)
	(*print-pretty* t))
    (fresh-line stream)
    (dump-updating-aux record old-records stream)))

(defgeneric dump-updating-aux (record old-records stream))

(defmethod dump-updating-aux ((record standard-updating-output-record)
			      old-records
			      stream)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (record stream :type t)
      (let ((old-printed nil))
	(format stream "~S " (output-record-dirty record))
	(pprint-indent :block 2 stream)
	(pprint-newline :linear stream)
	(when (and (or (eq old-records :old)
		       (eq old-records :both))
		   (slot-boundp record 'old-children))
	  (format stream ":old ~@_")
	  (dump-updating-aux (old-children record) old-records stream)
	  (setq old-printed t))
	(when (or (eq old-records :new)
		  (eq old-records :both)
		  (not old-records))
	  (when old-printed
	    (pprint-newline :linear stream))
	  (format stream ":new ~@_")
	  (dump-updating-aux (sub-record record) old-records stream))))))


(defmethod dump-updating-aux ((record compound-output-record)
			      old-records
			      stream)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (record stream :type t)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (pprint-indent :block 2 stream)
      (pprint-logical-block (stream nil :prefix "#(" :suffix ")")
	(loop with children = (output-record-children record)
	      for i from 1 below (length children)
	      for child across children
	      do (progn
		   (pprint-pop)
		   (dump-updating-aux child old-records stream)
		   (write-char #\Space stream)
		   (pprint-newline :fill stream))
	      finally (when (> (length children) 0)
			(pprint-pop)
			(dump-updating-aux (elt children (1- i))
					   old-records
					   stream)))))))

(defmethod dump-updating-aux (record old-records stream)
  (declare (ignore old-records))
  (write record :stream stream))

(defmethod redisplay-frame-pane
    ((frame application-frame) (pane updating-output-stream-mixin) &key force-p)
  (setf (id-counter pane) 0)
  (let ((incremental-redisplay (pane-incremental-redisplay pane)))
    (cond ((not incremental-redisplay)
	   (call-next-method))
	  ((or (null (updating-record pane))
	       force-p)
	   (setf (updating-record pane)
		 (updating-output (pane :unique-id 'top-level)
		   (call-next-method frame pane :force-p force-p))))
	  ;; Implements the extension to the :incremental-redisplay
	  ;; pane argument found in the Franz User Guide.
	  (t (let ((record (updating-record pane)))
	       (if (consp incremental-redisplay)
		   (apply #'redisplay record pane incremental-redisplay)
		   (redisplay record pane))) ))))
