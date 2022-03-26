;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014, 2016 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2001, 2002 by Alexey Dejneka (adejneka@comail.ru)
;;;  (c) copyright 2003 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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
;;;
;;; - Scrolling does not work correctly. Region is given in "window"
;;; coordinates, without bounding-rectangle-position transformation.
;;; (Is it still valid?)
;;;
;;; - Redo setf*-output-record-position, extent recomputation for
;;; compound records
;;;
;;; - When DRAWING-P is NIL, should stream cursor move?
;;;
;;; - :{X,Y}-OFFSET.
;;;
;;; - (SETF OUTPUT-RECORD-START-CURSOR-POSITION) does not affect the
;;; bounding rectangle. What does it affect?
;;;
;;; - How should (SETF OUTPUT-RECORD-POSITION) affect the bounding
;;; rectangle of the parent? Now its bounding rectangle is accurately
;;; recomputed, but it is very inefficient for table formatting. It
;;; seems that CLIM is supposed to keep a "large enougn" rectangle and
;;; to shrink it to the correct size only when the layout is complete
;;; by calling TREE-RECOMPUTE-EXTENT.
;;;
;;; - Computation of the bounding rectangle of lines/polygons ignores
;;; LINE-STYLE-CAP-SHAPE.
;;;
;;; - Rounding of coordinates.
;;;
;;; - Document carefully the interface of
;;; STANDARD-OUTPUT-RECORDING-STREAM.
;;;
;;; - COORD-SEQ is a sequence, not a list.

;;; Troubles

;;; DC
;;;
;;; Some GFs are defined to have "a default method on CLIM's standard
;;; output record class". What does it mean? What is "CLIM's standard
;;; output record class"? Is it OUTPUT-RECORD or BASIC-OUTPUT-RECORD?
;;; Now they are defined on OUTPUT-RECORD.

(in-package :clim-internals)

;;; 16.2.1. The Basic Output Record Protocol (extras)

(defgeneric (setf output-record-parent) (parent record)
  (:documentation "Additional protocol generic function. PARENT may be
an output record or NIL."))

;;; 16.2.2. Output Record "Database" Protocol (extras)
;;; From the Franz CLIM user's guide but not in the spec... clearly necessary.

(defgeneric map-over-output-records-1
    (continuation record continuation-args))

(defun map-over-output-records
    (function record &optional (x-offset 0) (y-offset 0) &rest function-args)
  "Maps over all of the children of the RECORD, calling FUNCTION on
each one. It is a function of one or more arguments and called with
all of FUNCTION-ARGS as APPLY arguments."
  (declare (ignore x-offset y-offset))
  (map-over-output-records-1 function record function-args))

;;; Forward definition
(defclass stream-output-history-mixin ()
  ((stream :initarg :stream :reader output-history-stream)))

;;; 21.3 Incremental Redisplay Protocol.  These generic functions need
;;; to be implemented for all the basic displayed-output-records, so
;;; they are defined in this file.
;;;
;;; MATCH-OUTPUT-RECORDS and FIND-CHILD-OUTPUT-RECORD, as defined in
;;; the CLIM spec, are pretty silly.  How does incremental redisplay
;;; know what keyword arguments to supply to FIND-CHILD-OUTPUT-RECORD?
;;; Through a gf specialized on the type of the record it needs to
;;; match... why not define the search function and the predicate on
;;; two records then!
;;;
;;; We'll implement MATCH-OUTPUT-RECORDS and FIND-CHILD-OUTPUT-RECORD,
;;; but we won't actually use them.  Instead, output-record-equal will
;;; match two records, and find-child-record-equal will search for the
;;; equivalent record.

(defgeneric match-output-records (record &rest args))

;;; These gf's use :MOST-SPECIFIC-LAST because one of the least
;;; specific methods will check the bounding boxes of the records,
;;; which should cause an early out most of the time.

(defgeneric match-output-records-1 (record &key)
  (:method-combination and :most-specific-last))

(defgeneric output-record-equal (record1 record2)
  (:method-combination and :most-specific-last))

(defmethod output-record-equal :around (record1 record2)
  (cond ((eq record1 record2)
         ;; Some unusual record -- like a Goatee screen line -- might
         ;; exist in two trees at once
         t)
        ((eq (class-of record1) (class-of record2))
         (let ((result (call-next-method)))
           (if (eq result 'maybe)
               nil
               result)))
        (t nil)))

;;; A fallback method so that something's always applicable.

(defmethod output-record-equal and (record1 record2)
  (declare (ignore record1 record2))
  'maybe)

;;; The code for MATCH-OUTPUT-RECORDS-1 and OUTPUT-RECORD-EQUAL
;;; methods are very similar, hence this macro.  In order to exploit
;;; the similarities, it's necessary to treat the slots of the second
;;; record like variables, so for convenience the macro will use
;;; SLOT-VALUE on both records.

(defmacro defrecord-predicate (record-type slots &body body)
  "Each element of SLOTS is either a symbol or (:initarg-name slot-name)."
  (let* ((slot-names (mapcar #'(lambda (slot-spec)
                                 (if (consp slot-spec)
                                     (cadr slot-spec)
                                     slot-spec))
                             slots))
         (supplied-vars (mapcar #'(lambda (slot)
                                    (gensym (symbol-name
                                             (symbol-concat slot '#:-p))))
                                slot-names))
         (key-args (mapcar #'(lambda (slot-spec supplied)
                               `(,slot-spec nil ,supplied))
                           slots supplied-vars))
         (key-arg-alist (mapcar #'cons slot-names supplied-vars)))
    `(progn
       (defmethod output-record-equal and ((record ,record-type)
                                           (record2 ,record-type))
         (macrolet ((if-supplied ((var &optional (type t)) &body supplied-body)
                      (declare (ignore var type))
                      `(progn ,@supplied-body)))
           (with-slots ,slot-names record2
             ,@body)))
       (defmethod match-output-records-1 and ((record ,record-type)
                                              &key ,@key-args)
         (macrolet ((if-supplied ((var &optional (type t)) &body supplied-body)
                      (let ((supplied-var (cdr (assoc var ',key-arg-alist))))
                        (unless supplied-var
                          (error "Unknown slot ~S" var))
                        `(or (null ,supplied-var)
                             ,@(if (eq type t)
                                   `((progn ,@supplied-body))
                                   `((if (typep ,var ',type)
                                         (progn ,@supplied-body)
                                         (error 'type-error
                                                :datum ,var
                                                :expected-type ',type))))))))
           ,@body)))))

(defmacro with-output-recording-options ((stream
                                          &key (record nil record-supplied-p)
                                               (draw nil draw-supplied-p))
                                         &body body)
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-gensyms (continuation)
    `(flet ((,continuation  (,stream)
              ,(declare-ignorable-form* stream)
              ,@body))
       (declare (dynamic-extent #',continuation))
       (with-drawing-options (,stream)
         (invoke-with-output-recording-options
          ,stream #',continuation
          ,(if record-supplied-p record `(stream-recording-p ,stream))
          ,(if draw-supplied-p draw `(stream-drawing-p ,stream)))))))

;;; Macro masturbation...

(defmacro define-invoke-with (macro-name func-name record-type doc-string)
  `(defmacro ,macro-name ((stream
                           &optional
                           (record-type '',record-type)
                           (record (gensym))
                           &rest initargs)
                          &body body)
     ,doc-string
     (setq stream (stream-designator-symbol stream '*standard-output*))
     (with-gensyms (continuation)
       (multiple-value-bind (bindings m-i-args)
           (rebind-arguments initargs)
         `(let ,bindings
            (flet ((,continuation (,stream ,record)
                     ,(declare-ignorable-form* stream record)
                     ,@body))
              (declare (dynamic-extent #',continuation))
              (,',func-name ,stream #',continuation ,record-type ,@m-i-args)))))))

(define-invoke-with with-new-output-record invoke-with-new-output-record
  standard-sequence-output-record
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record, and inserts the new
record into the current \"open\" output record assotiated with STREAM.
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used.")

(define-invoke-with with-output-to-output-record
    invoke-with-output-to-output-record
  standard-sequence-output-record
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record. The cursor position of
STREAM is initially bound to (0,0)
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used.")


;;;; Implementation

(defclass basic-output-record (standard-bounding-rectangle output-record)
  ((parent :initarg :parent ; XXX
           :initform nil
           :accessor output-record-parent)) ; XXX
  (:documentation "Implementation class for the Basic Output Record Protocol."))

(defmethod initialize-instance :after ((record basic-output-record)
                                       &key (x-position 0.0d0)
                                            (y-position 0.0d0))
  (setf (rectangle-edges* record)
        (values x-position y-position x-position y-position)))

;;; We need to remember initial record position (hence x,y slots) in case when
;;; we add children expanding record in top-left direction and then call
;;; clear-output-record. We want to reposition output record then at its initial
;;; position. That's why this is not redundant with the bounding-rectangle.
(defclass compound-output-record (basic-output-record)
  ((x :initarg :x-position
      :initform 0.0d0
      :documentation "X-position of the empty record.")
   (y :initarg :y-position
      :initform 0.0d0
      :documentation "Y-position of the empty record.")
   (in-moving-p :initform nil
                :documentation "Is set while changing the position."))
  (:documentation "Implementation class for output records with children."))

;;; 16.2.1. The Basic Output Record Protocol
(defmethod output-record-position ((record basic-output-record))
  (bounding-rectangle-position record))

(defmethod* (setf output-record-position) (nx ny (record basic-output-record))
  (with-standard-rectangle (x1 y1 x2 y2)
     record
    (let ((dx (- nx x1))
          (dy (- ny y1)))
      (setf (rectangle-edges* record)
            (values nx ny (+ x2 dx) (+ y2 dy)))))
  (values nx ny))

(defmethod* (setf output-record-position) :around
            (nx ny (record basic-output-record))
  (with-bounding-rectangle* (min-x min-y max-x max-y) record
    (call-next-method)
    (let ((parent (output-record-parent record)))
      (when (and parent (not (and (typep parent 'compound-output-record)
                                  (slot-value parent 'in-moving-p)))) ; XXX
        (recompute-extent-for-changed-child parent record
                                            min-x min-y max-x max-y)))
    (values nx ny)))

(defmethod* (setf output-record-position)
  :before (nx ny (record compound-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      record
    (letf (((slot-value record 'in-moving-p) t))
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (map-over-output-records
         (lambda (child)
           (multiple-value-bind (x y) (output-record-position child)
             (setf (output-record-position child)
                   (values (+ x dx) (+ y dy)))))
         record)))))

(defmethod output-record-start-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record basic-output-record))
  (values x y))

(defmethod output-record-end-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record basic-output-record))
  (values x y))

(defun replay (record stream &optional (region (or (pane-viewport-region stream)
                                                   (sheet-region stream))))
  (when (typep stream 'encapsulating-stream)
    (return-from replay (replay record (encapsulating-stream-stream stream) region)))
  (stream-close-text-output-record stream)
  (when (stream-drawing-p stream)
    (with-output-recording-options (stream :record nil)
      (with-sheet-medium (medium stream)
        (letf (((cursor-visibility (stream-text-cursor stream)) nil) ;; FIXME?
               ((stream-cursor-position stream) (values 0 0))
               ;; Is there a better value to bind to baseline?
               ((slot-value stream 'baseline) (slot-value stream 'baseline))
               ((medium-transformation medium) +identity-transformation+))
          (replay-output-record record stream region))))))

(defmethod replay-output-record ((record compound-output-record) stream
                                 &optional region (x-offset 0) (y-offset 0))
  (when (null region)
    (setq region (or (pane-viewport-region stream) +everywhere+)))
  (with-drawing-options (stream :clipping-region region)
    (map-over-output-records-overlapping-region
     #'replay-output-record record region x-offset y-offset
     stream region x-offset y-offset)))

(defmethod output-record-hit-detection-rectangle* ((record output-record))
  ;; XXX DC
  (bounding-rectangle* record))

(defmethod output-record-refined-position-test
    ((record basic-output-record) x y)
  (declare (ignore x y))
  t)

(defmethod output-record-refined-position-test
    ((record gs-clip-mixin) x y)
  (region-contains-position-p (graphics-state-clip record) x y))

(defun highlight-output-record-rectangle (record stream state)
  (with-identity-transformation (stream)
    (ecase state
      (:highlight
       ;; We can't "just" draw-rectangle :filled nil because the path lines
       ;; rounding may get outside the bounding rectangle. -- jd 2019-02-01
       (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
         (draw-design (sheet-medium stream)
                      (if (or (> (1+ x1) (1- x2))
                              (> (1+ y1) (1- y2)))
                          (bounding-rectangle record)
                          (region-difference (bounding-rectangle record)
                                             (make-rectangle* (1+ x1) (1+ y1) (1- x2) (1- y2))))
                      :ink +foreground-ink+)))
      (:unhighlight
       (repaint-sheet stream (bounding-rectangle record))
       ;; Using queue-repaint should be faster in apps (such as clouseau) that
       ;; highlight/unhighlight many bounding rectangles at once. The event code
       ;; should merge these into a single larger repaint. Unfortunately, since
       ;; an enqueued repaint does not occur immediately, and highlight
       ;; rectangles are not recorded, newer highlighting gets wiped out shortly
       ;; after being drawn. So, we aren't ready for this yet.  ..Actually, it
       ;; isn't necessarily faster. Depends on the app.
       #+ (or)
       (queue-repaint stream (make-instance 'window-repaint-event
                                            :sheet stream
                                            :region (bounding-rectangle record)))))))

;;; XXX Should this only be defined on recording streams?
(defmethod highlight-output-record ((record output-record) stream state)
  ;; XXX DC
  ;; XXX Disable recording?
  (highlight-output-record-rectangle record stream state))

;;; 16.2.2. The Output Record "Database" Protocol

;;; These two aren't in the spec, but are needed to make indirect
;;; adding/deleting of GADGET-OUTPUT-RECORDs work:

(defgeneric note-output-record-lost-sheet (record sheet))
(defgeneric note-output-record-got-sheet  (record sheet))

(defmethod note-output-record-lost-sheet ((record output-record) sheet)
  (declare (ignore record sheet))
  (values))

(defmethod note-output-record-lost-sheet :after
    ((record compound-output-record) sheet)
  (map-over-output-records #'note-output-record-lost-sheet record 0 0 sheet))

(defmethod note-output-record-got-sheet ((record output-record) sheet)
  (declare (ignore record sheet))
  (values))

(defmethod note-output-record-got-sheet :after
    ((record compound-output-record) sheet)
  (map-over-output-records #'note-output-record-got-sheet record 0 0 sheet))

(defun find-output-record-sheet (record)
  "Walks up the parents of RECORD, searching for an output history from which
the associated sheet can be determined."
  (typecase record
    (stream-output-history-mixin
     (output-history-stream record))
    (basic-output-record
     (find-output-record-sheet (output-record-parent record)))))

(defmethod output-record-children ((record basic-output-record))
  nil)

(defmethod add-output-record (child (record basic-output-record))
  (declare (ignore child))
  (error "Cannot add a child to ~S." record))

(defmethod add-output-record :before (child (record compound-output-record))
  (let ((parent (output-record-parent child)))
    (cond (parent
           (restart-case
               (error "~S already has a parent ~S." child parent)
             (delete ()
               :report "Delete from the old parent."
               (delete-output-record child parent))))
          ((eq record child)
           (error "~S is being added to itself." record))
          ((eq (output-record-parent record) child)
           (error "child ~S is being added to its own child ~S."
                  child record)))))

(defmethod add-output-record :after (child (record compound-output-record))
  (recompute-extent-for-new-child record child)
  (when (eq record (output-record-parent child))
    (let ((sheet (find-output-record-sheet record)))
      (when sheet (note-output-record-got-sheet child sheet)))))

(defmethod delete-output-record :before (child (record basic-output-record)
                                         &optional (errorp t))
  (declare (ignore errorp))
  (let ((sheet (find-output-record-sheet record)))
    (when sheet
      (note-output-record-lost-sheet child sheet))))

(defmethod delete-output-record (child (record basic-output-record)
                                 &optional (errorp t))
  (declare (ignore child))
  (when errorp (error "Cannot delete a child from ~S." record)))

(defmethod delete-output-record :after (child (record compound-output-record)
                                        &optional (errorp t))
  (declare (ignore errorp))
  (with-bounding-rectangle* (x1 y1 x2 y2) child
    (recompute-extent-for-changed-child record child x1 y1 x2 y2)))

(defmethod clear-output-record ((record basic-output-record))
  (error "Cannot clear ~S." record))

(defmethod clear-output-record :before ((record compound-output-record))
  (let ((sheet (find-output-record-sheet record)))
    (when sheet
      (map-over-output-records #'note-output-record-lost-sheet record 0 0 sheet))))

(defmethod clear-output-record :around ((record compound-output-record))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
    (call-next-method)
    (assert (null-bounding-rectangle-p record))
    (when (output-record-parent record)
      (recompute-extent-for-changed-child
       (output-record-parent record) record x1 y1 x2 y2))))

(defmethod clear-output-record :after ((record compound-output-record))
  (with-slots (x y) record
    (setf (rectangle-edges* record) (values x y x y))))

(defmethod output-record-count ((record displayed-output-record))
  0)

(defmethod map-over-output-records-1
    (function (record displayed-output-record) function-args)
  (declare (ignore function function-args))
  nil)

;;; This needs to work in "most recently added last" order. Is this
;;; implementation right? -- APD, 2002-06-13
#+nil
(defmethod map-over-output-records
    (function (record compound-output-record)
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (map nil (lambda (child) (apply function child function-args))
       (output-record-children record)))

(defmethod map-over-output-records-containing-position
    (function (record displayed-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore function x y x-offset y-offset function-args))
  nil)

;;; This needs to work in "most recently added first" order. Is this
;;; implementation right? -- APD, 2002-06-13
#+nil
(defmethod map-over-output-records-containing-position
    (function (record compound-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (map nil
       (lambda (child)
         (when (and (multiple-value-bind (min-x min-y max-x max-y)
                        (output-record-hit-detection-rectangle* child)
                      (and (<= min-x x max-x) (<= min-y y max-y)))
                    (output-record-refined-position-test child x y))
           (apply function child function-args)))
       (output-record-children record)))

(defmethod map-over-output-records-overlapping-region
    (function (record displayed-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore function region x-offset y-offset function-args))
  nil)

;;; This needs to work in "most recently added last" order. Is this
;;; implementation right? -- APD, 2002-06-13
#+nil
(defmethod map-over-output-records-overlapping-region
    (function (record compound-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (map nil
       (lambda (child) (when (region-intersects-region-p region child)
                         (apply function child function-args)))
       (output-record-children record)))

;;; XXX Dunno about this definition... -- moore
;;; Your apprehension is justified, but we lack a better means by which
;;; to distinguish "empty" compound records (roots of trees of compound
;;; records, containing no non-compound records). Such subtrees should
;;; not affect bounding rectangles.  -- Hefner
(defun null-bounding-rectangle-p (bbox)
  (with-bounding-rectangle* (x1 y1 x2 y2) bbox
    (and (= x1 x2)
         (= y1 y2)
         t)))

;;; 16.2.3. Output Record Change Notification Protocol
(defmethod recompute-extent-for-new-child
    ((record compound-output-record) child)
  (unless (null-bounding-rectangle-p child)
    (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) record
      (cond
        ((null-bounding-rectangle-p record)
         (setf (rectangle-edges* record) (bounding-rectangle* child)))
        ((not (null-bounding-rectangle-p child))
         (assert (not (null-bounding-rectangle-p record))) ; important.
         (with-bounding-rectangle* (x1-child y1-child x2-child y2-child)
             child
           (setf (rectangle-edges* record)
                 (values (min old-x1 x1-child) (min old-y1 y1-child)
                         (max old-x2 x2-child) (max old-y2 y2-child))))))
      (let ((parent (output-record-parent record)))
        (when parent
          (recompute-extent-for-changed-child
           parent record old-x1 old-y1 old-x2 old-y2)))))
  record)

(defun %tree-recompute-extent* (record)
  (check-type record compound-output-record)
  ;; Internal helper function
  (if (zerop (output-record-count record)) ; no children
      (with-slots (x y) record
        (values x y x y))
      (let ((new-x1 0)
            (new-y1 0)
            (new-x2 0)
            (new-y2 0)
            (first-time t))
        (flet ((do-child (child)
                 (cond ((null-bounding-rectangle-p child))
                       (first-time
                        (multiple-value-setq (new-x1 new-y1 new-x2 new-y2)
                          (bounding-rectangle* child))
                        (setq first-time nil))
                       (t
                        (with-bounding-rectangle* (cx1 cy1 cx2 cy2) child
                          (minf new-x1 cx1)
                          (minf new-y1 cy1)
                          (maxf new-x2 cx2)
                          (maxf new-y2 cy2))))))
          (declare (dynamic-extent #'do-child))
          (map-over-output-records #'do-child record))
        (values new-x1 new-y1 new-x2 new-y2))))

(defmethod recompute-extent-for-changed-child
    ((record compound-output-record) changed-child
     old-min-x old-min-y old-max-x old-max-y)
  (with-bounding-rectangle* (ox1 oy1 ox2 oy2)  record
    (with-bounding-rectangle* (cx1 cy1 cx2 cy2) changed-child
      ;; If record is currently empty, use the child's bbox
      ;; directly. Else..  Does the new rectangle of the child contain
      ;; the original rectangle?  If so, we can use min/max to grow
      ;; record's current rectangle.  If not, the child has shrunk,
      ;; and we need to fully recompute.
      (multiple-value-bind (nx1 ny1 nx2 ny2)
          (cond
            ;; The child has been deleted; who knows what the
            ;; new bounding box might be.
            ;; This case shouldn't be really necessary.
            ((not (output-record-parent changed-child))
             (%tree-recompute-extent* record))
            ;; Only one child of record, and we already have the bounds.
            ((eql (output-record-count record) 1)
             ;; See output-record-children for why this assert breaks:
             ;; (assert (eq changed-child (elt (output-record-children
             ;; record) 0)))
             (values cx1 cy1 cx2 cy2))
            ;; If our record occupied no space (had no children, or
            ;; had only children similarly occupying no space,
            ;; hackishly determined by null-bounding-rectangle-p),
            ;; recompute the extent now, otherwise the next COND
            ;; clause would, as an optimization, attempt to extend our
            ;; current bounding rectangle, which is invalid.
            ((null-bounding-rectangle-p record)
             (%tree-recompute-extent* record))
            ;; In the following cases, we can grow the new bounding
            ;; rectangle from its previous state:
            ((or
              ;; If the child was originally empty, it could not have
              ;; affected previous computation of our bounding
              ;; rectangle.  This is hackish for reasons similar to
              ;; the above.
              (and (= old-min-x old-max-x) (= old-min-y old-max-y))
              ;; For each edge of the original child bounds, if it was
              ;; within its respective edge of the old parent bounding
              ;; rectangle, or if it has not changed:
              (and (or (> old-min-x ox1) (= old-min-x cx1))
                   (or (> old-min-y oy1) (= old-min-y cy1))
                   (or (< old-max-x ox2) (= old-max-x cx2))
                   (or (< old-max-y oy2) (= old-max-y cy2)))
              ;; New child bounds contain old child bounds, so use
              ;; min/max to extend the already-calculated rectangle.
              (and (<= cx1 old-min-x) (<= cy1 old-min-y)
                   (>= cx2 old-max-x) (>= cy2 old-max-y)))
             (values (min cx1 ox1) (min cy1 oy1)
                     (max cx2 ox2) (max cy2 oy2)))
            ;; No shortcuts - we must compute a new bounding box from
            ;; those of all our children. We want to avoid this - in
            ;; worst cases, such as a toplevel output history, large
            ;; graph, or table, there may exist thousands of
            ;; children. Without the above optimizations, construction
            ;; becomes O(N^2) due to bounding rectangle calculation.
            (t (%tree-recompute-extent* record)))
        (with-slots (x y) record
          (setf x nx1 y ny1)
          (setf (rectangle-edges* record) (values  nx1 ny1 nx2 ny2))
          (let ((parent (output-record-parent record)))
            (unless (or (null parent)
                        (and (= nx1 ox1) (= ny1 oy1)
                             (= nx2 ox2) (= nx2 oy2)))
              (recompute-extent-for-changed-child parent record
                                                  ox1 oy1 ox2 oy2)))))))
  record)

(defun tree-recompute-extent-aux (record &aux new-x1 new-y1 new-x2 new-y2 changedp)
  (when (or (null (typep record 'compound-output-record))
            (zerop (output-record-count record)))
    (return-from tree-recompute-extent-aux
      (bounding-rectangle* record)))
  (flet ((do-child (child)
           (if (null changedp)
               (progn
                 (multiple-value-setq (new-x1 new-y1 new-x2 new-y2)
                   (tree-recompute-extent-aux child))
                 (setq changedp t))
               (multiple-value-bind (cx1 cy1 cx2 cy2)
                   (tree-recompute-extent-aux child)
                 (minf new-x1 cx1) (minf new-y1 cy1)
                 (maxf new-x2 cx2) (maxf new-y2 cy2)))))
    (declare (dynamic-extent #'do-child))
    (map-over-output-records #'do-child record))
  (with-slots (x y) record
    (setf x new-x1 y new-y1)
    (setf (rectangle-edges* record)
          (values new-x1 new-y1 new-x2 new-y2))))

(defmethod tree-recompute-extent ((record compound-output-record))
  (tree-recompute-extent-aux record)
  record)

(defmethod tree-recompute-extent :around ((record compound-output-record))
  (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) record
    (call-next-method)
    (with-bounding-rectangle* (x1 y1 x2 y2) record
      (when-let ((parent (output-record-parent record)))
        (when (not (and (= old-x1 x1)
                        (= old-y1 y1)
                        (= old-x2 x2)
                        (= old-y2 y2)))
          (recompute-extent-for-changed-child parent record
                                              old-x1 old-y1
                                              old-x2 old-y2)))))
  record)

;;; 16.3.1. Standard output record classes

(defclass standard-sequence-output-record (compound-output-record)
  ((children :initform (make-array 8 :adjustable t :fill-pointer 0)
             :reader output-record-children)))

(defmethod add-output-record (child (record standard-sequence-output-record))
  (vector-push-extend child (output-record-children record))
  (setf (output-record-parent child) record))

(defmethod delete-output-record (child (record standard-sequence-output-record)
                                 &optional (errorp t))
  (with-slots (children) record
    (let ((pos (position child children :test #'eq)))
      (if (null pos)
          (when errorp
            (error "~S is not a child of ~S" child record))
          (progn
            (setq children (replace children children
                                    :start1 pos
                                    :start2 (1+ pos)))
            (decf (fill-pointer children))
            (setf (output-record-parent child) nil))))))

(defmethod clear-output-record ((record standard-sequence-output-record))
  (let ((children (output-record-children record)))
    (map 'nil (lambda (child) (setf (output-record-parent child) nil))
         children)
    (fill children nil)
    (setf (fill-pointer children) 0)))

(defmethod output-record-count ((record standard-sequence-output-record))
  (length (output-record-children record)))

(defmethod map-over-output-records-1
    (function (record standard-sequence-output-record) function-args)
  "Applies FUNCTION to all children in the order they were added."
  (let ((function (alexandria:ensure-function function)))
    (if function-args
        (loop for child across (output-record-children record)
              do (apply function child function-args))
        (loop for child across (output-record-children record)
              do (funcall function child)))))

(defmethod map-over-output-records-containing-position
    (function (record standard-sequence-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  "Applies FUNCTION to children, containing (X,Y), in the reversed
order they were added."
  (declare (ignore x-offset y-offset))
  (let ((function (alexandria:ensure-function function)))
    (loop with children = (output-record-children record)
          for i from (1- (length children)) downto 0
          for child = (aref children i)
          when (and (multiple-value-bind (min-x min-y max-x max-y)
                        (output-record-hit-detection-rectangle* child)
                      (and (<= min-x x max-x) (<= min-y y max-y)))
                    (output-record-refined-position-test child x y))
          do (apply function child function-args))))

(defmethod map-over-output-records-overlapping-region
    (function (record standard-sequence-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  "Applies FUNCTION to children, overlapping REGION, in the order they
were added."
  (declare (ignore x-offset y-offset))
  (let ((function (alexandria:ensure-function function)))
    (loop with children = (output-record-children record)
          for child across children
          when (region-intersects-region-p region child)
          do (apply function child function-args))))

;;; tree output recording

(defclass tree-output-record-entry ()
     ((record :initarg :record :reader tree-output-record-entry-record)
      (cached-rectangle :initform nil
                        :accessor tree-output-record-entry-cached-rectangle)
      (inserted-nr :initarg :inserted-nr
                   :accessor tree-output-record-entry-inserted-nr)))

(defun make-tree-output-record-entry (record inserted-nr)
  (make-instance 'tree-output-record-entry
    :record record
    :inserted-nr inserted-nr))

(defun %record-to-spatial-tree-rectangle (r)
  (rectangles:make-rectangle
   :lows `(,(bounding-rectangle-min-x r)
            ,(bounding-rectangle-min-y r))
   :highs `(,(bounding-rectangle-max-x r)
             ,(bounding-rectangle-max-y r))))

(defun %output-record-entry-to-spatial-tree-rectangle (r)
  (when (null (tree-output-record-entry-cached-rectangle r))
    (let* ((record (tree-output-record-entry-record r)))
      (setf (tree-output-record-entry-cached-rectangle r)
            (%record-to-spatial-tree-rectangle record))))
  (tree-output-record-entry-cached-rectangle r))

(defun %make-tree-output-record-tree ()
  (spatial-trees:make-spatial-tree :r
                        :rectfun #'%output-record-entry-to-spatial-tree-rectangle))

(defclass standard-tree-output-record (compound-output-record)
  ((children :initform (%make-tree-output-record-tree)
             :accessor %tree-record-children)
   (children-hash :initform (make-hash-table :test #'eql)
                  :reader %tree-record-children-cache)
   (child-count :initform 0)
   (last-insertion-nr :initform 0 :accessor last-insertion-nr)))

(defun %entry-in-children-cache (record entry)
  (gethash entry (%tree-record-children-cache record)))

(defun (setf %entry-in-children-cache) (new-val record entry)
  (setf (gethash entry (%tree-record-children-cache record)) new-val))

(defun %remove-entry-from-children-cache (record entry)
  (remhash entry (%tree-record-children-cache record)))

(defmethod output-record-children ((record standard-tree-output-record))
  (with-bounding-rectangle* (min-x min-y max-x max-y) record
    (map 'list
         #'tree-output-record-entry-record
         (spatial-trees:search
          ;; Originally, (%record-to-spatial-tree-rectangle record).
          ;; The form below intends to fix output-record-children not
          ;; reporting empty children, which may lie outside the
          ;; reported bounding rectangle of their parent.
          ;; Assumption: null bounding records are always at the
          ;; origin.  I've never noticed this violated, but it's out
          ;; of line with what null-bounding-rectangle-p checks, and
          ;; setf of output-record-position may invalidate it. Seems
          ;; to work, but fix that and try again later.  Note that max
          ;; x or y may be less than zero..
          (rectangles:make-rectangle
           :lows  (list (min 0 min-x) (min 0 min-y))
           :highs (list (max 0 max-x) (max 0 max-y)))
          (%tree-record-children record)))))

(defmethod add-output-record (child (record standard-tree-output-record))
  (let ((entry (make-tree-output-record-entry
                child (incf (last-insertion-nr record)))))
    (spatial-trees:insert entry (%tree-record-children record))
    (setf (output-record-parent child) record)
    (setf (%entry-in-children-cache record child) entry))
  (incf (slot-value record 'child-count))
  (values))

(defmethod delete-output-record
    (child (record standard-tree-output-record) &optional (errorp t))
  (let ((entry (find child (spatial-trees:search
                            (%entry-in-children-cache record child)
                            (%tree-record-children record))
                     :key #'tree-output-record-entry-record)))
    (decf (slot-value record 'child-count))
    (cond
      ((not (null entry))
       (spatial-trees:delete entry (%tree-record-children record))
       (%remove-entry-from-children-cache record child)
       (setf (output-record-parent child) nil))
      (errorp (error "~S is not a child of ~S" child record)))))

(defmethod clear-output-record ((record standard-tree-output-record))
  (map nil (lambda (child)
             (setf (output-record-parent child) nil)
             (%remove-entry-from-children-cache record child))
       (output-record-children record))
  (setf (slot-value record 'child-count) 0)
  (setf (%tree-record-children record) (%make-tree-output-record-tree)))

(defmethod output-record-count ((record standard-tree-output-record))
  (slot-value record 'child-count))

(defun map-over-tree-output-records
    (function record rectangle sort-order function-args)
  (dolist (child (sort (spatial-trees:search rectangle
                                             (%tree-record-children record))
                       (ecase sort-order
                         (:most-recent-first #'>)
                         (:most-recent-last #'<))
                       :key #'tree-output-record-entry-inserted-nr))
    (apply function (tree-output-record-entry-record child) function-args)))

(defmethod map-over-output-records-1 (function (record standard-tree-output-record) function-args)
  (map-over-tree-output-records function record
    (%record-to-spatial-tree-rectangle record) :most-recent-last
                                function-args))

(defmethod map-over-output-records-containing-position
    (function (record standard-tree-output-record) x y
     &optional x-offset y-offset &rest function-args)
  (declare (ignore x-offset y-offset))
  (map-over-tree-output-records function record
    (rectangles:make-rectangle :lows `(,x ,y) :highs `(,x ,y)) :most-recent-first
                                function-args))

(defmethod map-over-output-records-overlapping-region
    (function (record standard-tree-output-record) region
     &optional x-offset y-offset &rest function-args)
  (declare (ignore x-offset y-offset))
  (typecase region
    (everywhere-region (map-over-output-records-1 function record function-args))
    (nowhere-region nil)
    (otherwise (map-over-tree-output-records
                (lambda (child)
                  (when (region-intersects-region-p
                         (multiple-value-call 'make-rectangle*
                           (bounding-rectangle* child))
                         region)
                    (apply function child function-args)))
                record
                (%record-to-spatial-tree-rectangle (bounding-rectangle region))
                :most-recent-last
                '()))))

(defmethod recompute-extent-for-changed-child :around ((record standard-tree-output-record) child old-min-x old-min-y old-max-x old-max-y)
  (when (eql record (output-record-parent child))
    (let ((entry (%entry-in-children-cache record child)))
     (spatial-trees:delete entry (%tree-record-children record))
     (setf (tree-output-record-entry-cached-rectangle entry) nil)
     (spatial-trees:insert entry (%tree-record-children record))))
  (call-next-method))

;;;

(defmethod match-output-records ((record t) &rest args)
  (apply #'match-output-records-1 record args))

(defmethod replay-output-record :around
    ((record gs-ink-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-drawing-options (stream :ink (graphics-state-ink record))
    (call-next-method)))

(defmethod* (setf output-record-position) :before
    (nx ny (record gs-ink-mixin))
    (with-standard-rectangle* (:x1 x1 :y1 y1) record
      (let* ((dx (- nx x1))
             (dy (- ny y1))
             (tr (make-translation-transformation dx dy)))
        (with-slots (ink) record
          (setf ink (transform-region tr ink))))))

(defrecord-predicate gs-ink-mixin (ink)
  (if-supplied (ink)
    (design-equalp (slot-value record 'ink) ink)))

(defmethod replay-output-record :around
    ((record gs-clip-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (let ((clipping-region (graphics-state-clip record)))
    (if (or (eq clipping-region +everywhere+) ; !!!
            (region-contains-region-p clipping-region (medium-clipping-region stream)))
        (call-next-method)
        (with-drawing-options (stream :clipping-region (graphics-state-clip record))
          (call-next-method)))))

(defrecord-predicate gs-clip-mixin ((:clipping-region clipping-region))
  (if-supplied (clipping-region)
    (region-equal (graphics-state-clip record) clipping-region)))

;;; 16.3.2. Graphics Displayed Output Records
(defclass standard-displayed-output-record (gs-clip-mixin gs-ink-mixin
                                            basic-output-record
                                            displayed-output-record)
  ((ink :reader displayed-output-record-ink)
   (stream :initarg :stream))
  (:documentation "Implementation class for DISPLAYED-OUTPUT-RECORD.")
  (:default-initargs :stream nil))

(defmethod replay-output-record :around
    ((record gs-line-style-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-drawing-options (stream :line-style (graphics-state-line-style record))
    (call-next-method)))

(defrecord-predicate gs-line-style-mixin (line-style)
  (if-supplied (line-style)
    (line-style-equalp (slot-value record 'line-style) line-style)))

(defmethod replay-output-record :around
    ((record gs-text-style-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-drawing-options (stream :text-style (graphics-state-text-style record))
    (call-next-method)))

(defrecord-predicate gs-text-style-mixin (text-style)
  (if-supplied (text-style)
    (text-style-equalp (slot-value record 'text-style) text-style)))

(defmethod replay-output-record :around
    ((record gs-transformation-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-drawing-options (stream :transformation (graphics-state-transformation record))
    (call-next-method)))

(defrecord-predicate gs-transformation-mixin (transformation)
  (if-supplied (transformation)
    (transformation-equal (graphics-state-transformation record) transformation)))

(defclass standard-graphics-displayed-output-record
    (standard-displayed-output-record
     graphics-displayed-output-record)
  ())

(defmethod match-output-records-1 and
  ((record standard-displayed-output-record)
   &key (x1 nil x1-p) (y1 nil y1-p)
   (x2 nil x2-p) (y2 nil y2-p)
   (bounding-rectangle nil bounding-rectangle-p))
  (if bounding-rectangle-p
      (region-equal record bounding-rectangle)
      (multiple-value-bind (my-x1 my-y1 my-x2 my-y2)
          (bounding-rectangle* record)
        (macrolet ((coordinate=-or-lose (key mine)
                     `(if (typep ,key 'coordinate)
                          (coordinate= ,mine ,key)
                          (error 'type-error
                                 :datum ,key
                                 :expected-type 'coordinate))))
          (and (or (null x1-p)
                   (coordinate=-or-lose x1 my-x1))
               (or (null y1-p)
                   (coordinate=-or-lose y1 my-y1))
               (or (null x2-p)
                   (coordinate=-or-lose x2 my-x2))
               (or (null y2-p)
                   (coordinate=-or-lose y2 my-y2)))))))

(defmethod output-record-equal and ((record standard-displayed-output-record)
                                    (record2 standard-displayed-output-record))
  (region-equal record record2))

(defclass coord-seq-mixin ()
  ((coord-seq :accessor coord-seq :initarg :coord-seq))
  (:documentation "Mixin class that implements methods for records that contain
   sequences of coordinates."))

(defun coord-seq-bounds (coord-seq border)
  (setf border (ceiling border))
  (let* ((min-x (elt coord-seq 0))
         (min-y (elt coord-seq 1))
         (max-x min-x)
         (max-y min-y))
    (do-sequence ((x y) coord-seq)
      (minf min-x x)
      (minf min-y y)
      (maxf max-x x)
      (maxf max-y y))
    (values (floor (- min-x border))
            (floor (- min-y border))
            (ceiling (+ max-x border))
            (ceiling (+ max-y border)))))

;;; record must be a standard-rectangle

(defmethod* (setf output-record-position) :around
    (nx ny (record coord-seq-mixin))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      record
    (let ((dx (- nx x1))
          (dy (- ny y1))
          (coords (slot-value record 'coord-seq)))
      (multiple-value-prog1
          (call-next-method)
        (let ((odd nil))
          (map-into coords
                    (lambda (val)
                      (prog1
                          (if odd
                              (incf val dy)
                              (incf val dx))
                        (setf odd (not odd))))
                    coords))))))

(defun sequence= (seq1 seq2 &optional (test 'equal))
  (and (= (length seq1) (length seq2))
       (every test seq1 seq2)))

(defmethod match-output-records-1 and ((record coord-seq-mixin)
                                       &key (coord-seq nil coord-seq-p))
  (or (null coord-seq-p)
      (let* ((my-coord-seq (slot-value record 'coord-seq)))
        (sequence= my-coord-seq coord-seq #'coordinate=))))

(defmacro generate-medium-recording-body (class-name args)
  (let ((arg-list (loop for arg in args
                     nconc `(,(intern (symbol-name arg) :keyword) ,arg))))
    `(progn
       (when (stream-recording-p stream)
         (let ((record
                ;; initialize the output record with a copy of coord-seq, as the
                ;; replaying code will modify it to be positioned relative to
                ;; the output-record's position and making a temporary is
                ;; (arguably) less bad than untrasnforming the coords back to
                ;; how they were.
                (let (,@(when (member 'coord-seq args)
                          `((coord-seq (copy-seq coord-seq)))))
                  (make-instance ',class-name :stream stream ,@arg-list))))
           (stream-add-output-record stream record)))
       (when (stream-drawing-p stream)
         (call-next-method)))))

;;; DEF-GRECORDING: This is the central interface through which
;;; recording is implemented for drawing functions. The body provided
;;; is used to compute the bounding rectangle of the rendered
;;; output. DEF-GRECORDING will define a class for the output record,
;;; with slots corresponding to the drawing function arguments. It
;;; also defines an INITIALIZE-INSTANCE method computing the bounding
;;; rectangle of the record. It defines a method for the medium
;;; drawing function specialized on output-recording-stream, which is
;;; responsible for creating the output record and adding it to the
;;; stream history. It also defines a REPLAY-OUTPUT-RECORD method,
;;; which calls the medium drawing function based on the recorded
;;; slots.

(defmacro def-grecording (name ((&rest mixins) &rest args)
                               (&key (class t)
                                     (medium-fn t)
                                     (replay-fn t)) &body body)
  (let ((method-name (symbol-concat '#:medium- name '*))
        (class-name (symbol-concat name '#:-output-record))
        (medium (gensym "MEDIUM"))
        (class-vars `((stream :initarg :stream)
                      ,@(loop for arg in args
                           collect `(,arg
                                     :initarg ,(intern (symbol-name arg)
                                                       :keyword))))))
    `(progn
       ,@(when class
           `((defclass ,class-name (,@mixins standard-graphics-displayed-output-record)
               ,class-vars)
             (defmethod initialize-instance :after ((graphic ,class-name) &key)
               (with-slots (stream ink clipping-region line-style text-style ,@args)
                   graphic
                 (let ((medium (sheet-medium stream)))
                   (setf (rectangle-edges* graphic) (progn ,@body)))))))
       ,@(when medium-fn
           `((defmethod ,method-name :around ((stream output-recording-stream) ,@args)
                        ;; XXX STANDARD-OUTPUT-RECORDING-STREAM ^?
                        (generate-medium-recording-body ,class-name ,args))))
       ,@(when replay-fn
           `((defmethod replay-output-record ((record ,class-name) stream
                                              &optional (region +everywhere+)
                                                (x-offset 0) (y-offset 0))
               (declare (ignore x-offset y-offset region))
               (with-slots (,@args) record
                 (let ((,medium (sheet-medium stream)))
                   ;; Graphics state is set up in :around method.
                   (,method-name ,medium ,@args)))))))))

(def-grecording draw-point ((gs-line-style-mixin) point-x point-y) ()
  (let ((border (graphics-state-line-style-border graphic medium)))
    (with-transformed-position ((medium-transformation medium) point-x point-y)
      (setf (slot-value graphic 'point-x) point-x
            (slot-value graphic 'point-y) point-y)
      (values (- point-x border)
              (- point-y border)
              (+ point-x border)
              (+ point-y border)))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-point-output-record))
    (with-standard-rectangle* (:x1 x1 :y1 y1)
        record
      (with-slots (point-x point-y) record
        (let ((dx (- nx x1))
              (dy (- ny y1)))
          (multiple-value-prog1
              (call-next-method)
            (incf point-x dx)
            (incf point-y dy))))))

(defrecord-predicate draw-point-output-record (point-x point-y)
  (and (if-supplied (point-x coordinate)
         (coordinate= (slot-value record 'point-x) point-x))
       (if-supplied (point-y coordinate)
         (coordinate= (slot-value record 'point-y) point-y))))

(def-grecording draw-points ((coord-seq-mixin gs-line-style-mixin) coord-seq) ()
  (let ((transformed-coord-seq (transform-positions (medium-transformation medium) coord-seq))
        (border (graphics-state-line-style-border graphic medium)))
    (setf (slot-value graphic 'coord-seq) transformed-coord-seq)
    (coord-seq-bounds transformed-coord-seq border)))

(def-grecording draw-line ((gs-line-style-mixin)
                           point-x1 point-y1 point-x2 point-y2) ()
  (let ((transform (medium-transformation medium))
        (border (graphics-state-line-style-border graphic medium)))
    (with-transformed-position (transform point-x1 point-y1)
      (with-transformed-position (transform point-x2 point-y2)
        (setf (slot-value graphic 'point-x1) point-x1
              (slot-value graphic 'point-y1) point-y1
              (slot-value graphic 'point-x2) point-x2
              (slot-value graphic 'point-y2) point-y2)
        (values (- (min point-x1 point-x2) border)
                (- (min point-y1 point-y2) border)
                (+ (max point-x1 point-x2) border)
                (+ (max point-y1 point-y2) border))))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-line-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      record
    (with-slots (point-x1 point-y1 point-x2 point-y2) record
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (multiple-value-prog1
            (call-next-method)
          (incf point-x1 dx)
          (incf point-y1 dy)
          (incf point-x2 dx)
          (incf point-y2 dy))))))

(defrecord-predicate draw-line-output-record (point-x1 point-y1
                                              point-x2 point-y2)
  (and (if-supplied (point-x1 coordinate)
         (coordinate= (slot-value record 'point-x1) point-x1))
       (if-supplied (point-y1 coordinate)
         (coordinate= (slot-value record 'point-y1) point-y1))
       (if-supplied (point-x2 coordinate)
         (coordinate= (slot-value record 'point-x2) point-x2))
       (if-supplied (point-y2 coordinate)
         (coordinate= (slot-value record 'point-y2) point-y2))))

(def-grecording draw-lines ((coord-seq-mixin gs-line-style-mixin) coord-seq) ()
  (let* ((transformation (medium-transformation medium))
         (transformed-coord-seq (transform-positions transformation coord-seq))
         (border (graphics-state-line-style-border graphic medium)))
    (setf coord-seq transformed-coord-seq)
    (coord-seq-bounds transformed-coord-seq border)))

;;; (setf output-record-position) and predicates for draw-lines-output-record
;;; are taken care of by methods on superclasses.

;;; Helper function
(defun normalize-coords (dx dy &optional unit)
  (let ((norm (sqrt (+ (* dx dx) (* dy dy)))))
    (cond ((= norm 0.0d0)
           (values 0.0d0 0.0d0))
          (unit
           (let ((scale (/ unit norm)))
             (values (* dx scale) (* dy scale))))
          (t (values (/ dx norm) (/ dy norm))))))

(defun polygon-record-bounding-rectangle
    (coord-seq closed filled line-style border miter-limit)
  (cond (filled
         (coord-seq-bounds coord-seq 0))
        ((eq (line-style-joint-shape line-style) :round)
         (coord-seq-bounds coord-seq border))
        (t (let* ((x1 (elt coord-seq 0))
                  (y1 (elt coord-seq 1))
                  (min-x x1)
                  (min-y y1)
                  (max-x x1)
                  (max-y y1)
                  (len (length coord-seq)))
             (unless closed
               (setq min-x (- x1 border)  min-y (- y1 border)
                     max-x (+ x1 border)  max-y (+ y1 border)))
             ;; Setup for iterating over the coordinate vector.  If
             ;; the polygon is closed, deal with the extra segment.
             (multiple-value-bind (initial-xp initial-yp
                                   final-xn final-yn
                                   initial-index final-index)
                 (if closed
                     (values (elt coord-seq (- len 2))
                             (elt coord-seq (- len 1))
                             x1 y1
                             0 (- len 2))
                     (values x1 y1
                             (elt coord-seq (- len 2))
                             (elt coord-seq (- len 1))
                             2 (- len 4)))
               (ecase (line-style-joint-shape line-style)
                 (:miter
                  ;;FIXME: Remove successive positively proportional segments
                  (loop with sin-limit = (sin (* 0.5 miter-limit))
                        and xn and yn
                        for i from initial-index to final-index by 2
                        for xp = initial-xp then x
                        for yp = initial-yp then y
                        for x = (elt coord-seq i)
                        for y = (elt coord-seq (1+ i))
                        do (setf (values xn yn)
                                 (if (eql i final-index)
                                     (values final-xn final-yn)
                                     (values (elt coord-seq (+ i 2))
                                             (elt coord-seq (+ i 3)))))
                           (multiple-value-bind (ex1 ey1)
                               (normalize-coords (- x xp) (- y yp))
                             (multiple-value-bind (ex2 ey2)
                                 (normalize-coords (- x xn) (- y yn))
                               (let* ((cos-a (+ (* ex1 ex2) (* ey1 ey2)))
                                      (sin-a/2 (sqrt (* 0.5 (- 1.0 cos-a)))))
                                 (if (< sin-a/2 sin-limit)
                                     (let ((nx (* border
                                                  (max (abs ey1) (abs ey2))))
                                           (ny (* border
                                                  (max (abs ex1) (abs ex2)))))
                                       (minf min-x (- x nx))
                                       (minf min-y (- y ny))
                                       (maxf max-x (+ x nx))
                                       (maxf max-y (+ y ny)))
                                     (let ((length (/ border sin-a/2)))
                                       (multiple-value-bind (dx dy)
                                           (normalize-coords (+ ex1 ex2)
                                                             (+ ey1 ey2)
                                                             length)
                                         (minf min-x (+ x dx))
                                         (minf min-y (+ y dy))
                                         (maxf max-x (+ x dx))
                                         (maxf max-y (+ y dy))))))))))
                 ((:bevel :none)
                  (loop with xn and yn
                        for i from initial-index to final-index by 2
                        for xp = initial-xp then x
                        for yp = initial-yp then y
                        for x = (elt coord-seq i)
                        for y = (elt coord-seq (1+ i))
                        do (setf (values xn yn)
                                 (if (eql i final-index)
                                     (values final-xn final-yn)
                                     (values (elt coord-seq (+ i 2))
                                             (elt coord-seq (+ i
                                                                 3)))))
                           (multiple-value-bind (ex1 ey1)
                               (normalize-coords (- x xp) (- y yp))
                             (multiple-value-bind (ex2 ey2)
                                 (normalize-coords (- x xn) (- y yn))
                               (let ((nx (* border (max (abs ey1) (abs ey2))))
                                     (ny (* border (max (abs ex1) (abs ex2)))))
                                 (minf min-x (- x nx))
                                 (minf min-y (- y ny))
                                 (maxf max-x (+ x nx))
                                 (maxf max-y (+ y ny))))))))
               (unless closed
                 (multiple-value-bind (x y)
                     (values (elt coord-seq (- len 2))
                             (elt coord-seq (- len 1)))
                   (minf min-x (- x border))
                   (minf min-y (- y border))
                   (maxf max-x (+ x border))
                   (maxf max-y (+ y border)))))
             (values min-x min-y max-x max-y)))))

(def-grecording draw-polygon ((coord-seq-mixin gs-line-style-mixin)
                              coord-seq closed filled) ()
  (let ((transformed-coord-seq (transform-positions (medium-transformation medium) coord-seq))
        (border (graphics-state-line-style-border graphic medium)))
    (setf coord-seq transformed-coord-seq)
    (polygon-record-bounding-rectangle transformed-coord-seq
                                       closed filled line-style border
                                       (medium-miter-limit medium))))

(defrecord-predicate draw-polygon-output-record (closed filled)
  (and (if-supplied (closed)
         (eql (slot-value record 'closed) closed))
       (if-supplied (filled)
         (eql (slot-value record 'filled) filled))))

(def-grecording draw-rectangle ((gs-line-style-mixin)
                                left top right bottom filled) (:medium-fn nil)
  (let* ((transform (medium-transformation medium))
         (border     (graphics-state-line-style-border graphic medium))
         (pre-coords (expand-rectangle-coords left top right bottom))
         (coords     (transform-positions transform pre-coords)))
    (setf (values left top) (transform-position transform left top))
    (setf (values right bottom) (transform-position transform right bottom))
    (polygon-record-bounding-rectangle coords t filled line-style border
                                       (medium-miter-limit medium))))

(defmethod medium-draw-rectangle* :around ((stream output-recording-stream) left top right bottom filled)
  (let ((tr (medium-transformation stream)))
    (if (rectilinear-transformation-p tr)
        (generate-medium-recording-body draw-rectangle-output-record
                                        (left top right bottom filled))
        (medium-draw-polygon* stream
                              (expand-rectangle-coords left top right bottom)
                              t
                              filled))))

(def-grecording draw-rectangles ((coord-seq-mixin gs-line-style-mixin)
                                 coord-seq filled) (:medium-fn nil)
  (let* ((transform (medium-transformation medium))
         (border (graphics-state-line-style-border graphic medium)))
    (let ((transformed-coord-seq
           (map-repeated-sequence 'vector 2
                                  (lambda (x y)
                                    (with-transformed-position (transform x y)
                                      (values x y)))
                                  coord-seq)))
      (polygon-record-bounding-rectangle transformed-coord-seq
                                         t filled line-style border
                                         (medium-miter-limit medium)))))

(defmethod medium-draw-rectangles* :around ((stream output-recording-stream) coord-seq filled)
  (let ((tr (medium-transformation stream)))
    (if (rectilinear-transformation-p tr)
        (generate-medium-recording-body draw-rectangles-output-record
                                        (coord-seq filled))
        (do-sequence ((left top right bottom) coord-seq)
          (medium-draw-polygon* stream (vector left top
                                               left bottom
                                               right bottom
                                               right top)
                                t filled)))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-rectangle-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      record
    (with-slots (left top right bottom) record
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (multiple-value-prog1
            (call-next-method)
          (incf left dx)
          (incf top dy)
          (incf right dx)
          (incf bottom dy))))))

(defrecord-predicate draw-rectangle-output-record (left top right bottom filled)
  (and (if-supplied (left coordinate)
         (coordinate= (slot-value record 'left) left))
       (if-supplied (top coordinate)
         (coordinate= (slot-value record 'top) top))
       (if-supplied (right coordinate)
         (coordinate= (slot-value record 'right) right))
       (if-supplied (bottom coordinate)
         (coordinate= (slot-value record 'bottom) bottom))
       (if-supplied (filled)
         (eql (slot-value record 'filled) filled))))

(def-grecording draw-ellipse ((gs-line-style-mixin)
                              center-x center-y
                              radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                              start-angle end-angle filled) ()
  (let ((transform (medium-transformation medium)))
    (setf (values center-x center-y)
          (transform-position transform center-x center-y))
    (setf (values radius-1-dx radius-1-dy)
          (transform-distance transform radius-1-dx radius-1-dy))
    (setf (values radius-2-dx radius-2-dy)
          (transform-distance transform radius-2-dx radius-2-dy))
    ;; I think this should be untransform-angle below, as the ellipse angles
    ;; go counter-clockwise in screen coordinates, whereas our transformations
    ;; rotate clockwise in the default coorinate system.. this is quite possibly
    ;; wrong depending on how one reads the spec, but just reversing it here
    ;; will break other things.  -Hefner
    (setf start-angle (untransform-angle transform start-angle))
    (setf end-angle   (untransform-angle transform end-angle))
    (when (reflection-transformation-p transform)
      (rotatef start-angle end-angle))
    (multiple-value-bind (min-x min-y max-x max-y)
        (bounding-rectangle* (make-ellipse* center-x center-y
                                            radius-1-dx radius-1-dy
                                            radius-2-dx radius-2-dy
                                            :start-angle start-angle
                                            :end-angle end-angle))
      (if filled
          (values min-x min-y max-x max-y)
          (let ((border (graphics-state-line-style-border graphic medium)))
            (values (floor (- min-x border))
                    (floor (- min-y border))
                    (ceiling (+ max-x border))
                    (ceiling (+ max-y border))))))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-ellipse-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      record
    (with-slots (center-x center-y) record
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (multiple-value-prog1
            (call-next-method)
          (incf center-x dx)
          (incf center-y dy))))))

(defrecord-predicate draw-ellipse-output-record (center-x center-y filled)
  (and (if-supplied (center-x coordinate)
                    (coordinate= (slot-value record 'center-x) center-x))
       (if-supplied (center-y coordinate)
                    (coordinate= (slot-value record 'center-y) center-y))
       (if-supplied (filled)
                    (eql (slot-value record 'filled) filled))))
;;;; Patterns

;;;; Text

(defun enclosing-transform-polygon (transformation positions)
  (when (null positions)
    (error "Need at least one coordinate"))
  (loop
    with min-x = most-positive-fixnum
    with min-y = most-positive-fixnum
    with max-x = most-negative-fixnum
    with max-y = most-negative-fixnum
    for (xp yp) on positions by #'cddr
    do (multiple-value-bind (x y)
           (transform-position transformation xp yp)
         (setf min-x (min min-x x))
         (setf min-y (min min-y y))
         (setf max-x (max max-x x))
         (setf max-y (max max-y y)))
    finally (return (values min-x min-y max-x max-y))))

(def-grecording draw-text ((gs-text-style-mixin gs-transformation-mixin)
                           string point-x point-y start end align-x align-y
                           toward-x toward-y transform-glyphs)
    (:replay-fn nil)
  ;; FIXME!!! Text direction.
  (let* ((transformation (medium-transformation medium))
         (text-style (graphics-state-text-style graphic)))
    (setf (graphics-state-transformation graphic) transformation)
    (multiple-value-bind (left top right bottom)
        (text-bounding-rectangle* medium string
                                  :align-x align-x :align-y align-y
                                  :start start :end end
                                  :text-style text-style)
      (if transform-glyphs
          (enclosing-transform-polygon transformation (list (+ point-x left)  (+ point-y top)
                                                            (+ point-x right) (+ point-y top)
                                                            (+ point-x left)  (+ point-y bottom)
                                                            (+ point-x right) (+ point-y bottom)))
          (with-transformed-position (transformation point-x point-y)
            (values (+ point-x left) (+ point-y top)
                    (+ point-x right) (+ point-y bottom)))))))

(defmethod* (setf output-record-position) :around
  (nx ny (record draw-text-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      record
    (let ((dx (- nx x1))
          (dy (- ny y1)))
      (multiple-value-prog1 (call-next-method)
        (setf #1=(graphics-state-transformation record)
              (compose-translation-with-transformation #1# dx dy))))))

(defmethod replay-output-record
    ((record draw-text-output-record) stream
     &optional (region +everywhere+) (x-offset 0) (y-offset 0))
  (declare (ignore x-offset y-offset region))
  (with-slots (string point-x point-y start end align-x align-y toward-x
               toward-y transform-glyphs transformation)
      record
    (let ((medium (sheet-medium stream)))
      (medium-draw-text* medium string point-x point-y start end align-x
                         align-y toward-x toward-y transform-glyphs))))

(defrecord-predicate draw-text-output-record
    (string start end point-x point-y align-x align-y toward-x toward-y transform-glyphs)
  (and (if-supplied (string)
         (string= (slot-value record 'string) string))
       (if-supplied (start)
         (eql (slot-value record 'start) start))
       (if-supplied (end)
         (eql (slot-value record 'end) end))
       (if-supplied (point-x coordinate)
         (coordinate= (slot-value record 'point-x) point-x))
       (if-supplied (point-y coordinate)
         (coordinate= (slot-value record 'point-y) point-y))
       (if-supplied (align-x)
         (eq (slot-value record 'align-x) align-x))
       (if-supplied (align-y)
         (eq (slot-value record 'align-y) align-y))
       (if-supplied (toward-x coordinate)
         (coordinate= (slot-value record 'toward-x) toward-x))
       (if-supplied (toward-y coordinate)
         (coordinate= (slot-value record 'toward-y) toward-y))
       (if-supplied (transform-glyphs)
         (eq (slot-value record 'transform-glyphs) transform-glyphs))))

;;; 16.3.3. Text Displayed Output Record

(defclass styled-string (gs-text-style-mixin gs-clip-mixin gs-ink-mixin)
  ((start-x :initarg :start-x)
   (string :initarg :string :reader styled-string-string)))

(defmethod output-record-equal and ((record styled-string)
                                    (record2 styled-string))
  (and (coordinate= (slot-value record 'start-x)
                    (slot-value record2 'start-x))
       (string= (slot-value record 'string)
                (slot-value record2 'string))))

(defclass standard-text-displayed-output-record
    (text-displayed-output-record standard-displayed-output-record)
  ((initial-x1 :initarg :start-x)
   (initial-y1 :initarg :start-y)
   (strings :initform nil)
   (baseline :initform 0)
   (width :initform 0)
   (max-height :initform 0)
   ;; FIXME (or rework this comment):
   ;; CLIM does not separate the notions of the text width and the bounding box;
   ;; however, we need to, because some fonts will render outside the logical
   ;; coordinates defined by the start position and the width. LEFT and RIGHT
   ;; here (and below) deal with this in a manner completely hidden from the
   ;; user. Should we export TEXT-BOUNDING-RECTANGLE*?
   (left :initarg :start-x)
   (right :initarg :start-x)
   (start-x :initarg :start-x)
   (start-y :initarg :start-y)
   (end-x :initarg :start-x)
   (end-y :initarg :start-y)
   (medium :initarg :medium :initform nil)))

(defmethod initialize-instance :after
    ((obj standard-text-displayed-output-record) &key stream)
  (when stream
    (setf (slot-value obj 'medium) (sheet-medium stream))))

;;; Forget match-output-records-1 for standard-text-displayed-output-record; it
;;; doesn't make much sense because these records have state that is not
;;; initialized via initargs.

(defmethod output-record-equal and
    ((record standard-text-displayed-output-record)
     (record2 standard-text-displayed-output-record))
  (with-slots
        (initial-x1 initial-y1 start-x start-y left right end-x end-y strings)
      record2
    (and (coordinate= (slot-value record 'initial-x1) initial-x1)
         (coordinate= (slot-value record 'initial-y1) initial-y1)
         (coordinate= (slot-value record 'start-x) start-x)
         (coordinate= (slot-value record 'start-y) start-y)
         (coordinate= (slot-value record 'left) left)
         (coordinate= (slot-value record 'right) right)
         (coordinate= (slot-value record 'end-x) end-x)
         (coordinate= (slot-value record 'end-y) end-y)
         (coordinate= (slot-value record 'baseline)
                      (slot-value record2 'baseline))
         (eql (length (slot-value record 'strings)) (length strings));XXX
         (loop for s1 in (slot-value record 'strings)
               for s2 in strings
               always (output-record-equal s1 s2)))))

(defmethod print-object ((self standard-text-displayed-output-record) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (start-x start-y strings) self
      (format stream "~D,~D ~S"
              start-x start-y
              (mapcar #'styled-string-string strings)))))

(defmethod* (setf output-record-position) :around
    (nx ny (record standard-text-displayed-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1) record
    (with-slots (start-x start-y end-x end-y strings baseline) record
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (multiple-value-prog1
            (call-next-method)
          (incf start-x dx)
          (incf start-y dy)
          (incf end-x dx)
          (incf end-y dy)
          (loop for s in strings
                do (incf (slot-value s 'start-x) dx)))))))

(defmethod replay-output-record ((record standard-text-displayed-output-record)
                                 stream
                                 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region x-offset y-offset))
  (with-slots (strings baseline max-height start-y) record
    (with-sheet-medium (medium stream) ;is sheet a sheet-with-medium-mixin? --GB
      ;; FIXME:
      ;; 1. SLOT-VALUE...
      ;; 2. It should also save a "current line".
      (setf (slot-value stream 'baseline) baseline)
      (loop for substring in strings
         do (with-slots (start-x string) substring
              ;; FIXME: a bit of an abstraction inversion.  Should the styled
              ;; strings here not simply be output records?  Then we could just
              ;; replay them and all would be well.  -- CSR, 20060528.
              ;;
              ;; But then we'd have to implement the output record
              ;; protocols for them. Are we allowed no internal
              ;; structure of our own? -- Hefner, 20080118

              ;; Some optimization might be possible here.
              (with-drawing-options (stream :ink (graphics-state-ink substring)
                                            :clipping-region (graphics-state-clip substring)
                                            :text-style (graphics-state-text-style substring))
                (with-identity-transformation (stream)
                  (draw-text* (sheet-medium stream)
                              string start-x (+ start-y (stream-baseline stream))))))))))

(defmethod output-record-start-cursor-position
    ((record standard-text-displayed-output-record))
  (with-slots (start-x start-y) record
    (values start-x start-y)))

(defmethod output-record-end-cursor-position
    ((record standard-text-displayed-output-record))
  (with-slots (end-x end-y) record
    (values end-x end-y)))

(defmethod tree-recompute-extent
    ((text-record standard-text-displayed-output-record))
  (with-standard-rectangle* (:y1 y1)
      text-record
    (with-slots (max-height left right) text-record
      (setf (rectangle-edges* text-record)
            (values (coordinate left)
                    y1
                    (coordinate right)
                    (coordinate (+ y1 max-height))))))
  text-record)

(defmethod add-character-output-to-text-record
    ((text-record standard-text-displayed-output-record)
     character text-style char-width height new-baseline
     &aux (start 0) (end 1))
  (add-string-output-to-text-record text-record character
                                    start end text-style
                                    char-width height new-baseline))

(defmethod add-string-output-to-text-record ((text-record standard-text-displayed-output-record)
                                             string start end text-style string-width height new-baseline)
  (setf end (or end (etypecase string
                      (character 1)
                      (string (length string)))))
  (let ((length (max 0 (- end start))))
    (with-slots (strings baseline width max-height left right start-y end-x end-y medium)
        text-record
      (let* ((strings-last-cons (last strings))
             (last-string (first strings-last-cons)))
        (if (and last-string
                 (match-output-records last-string
                                       :text-style text-style
                                       :ink (medium-ink medium)
                                       :clipping-region (medium-clipping-region medium)))
            ;; Simply append the string to the last one.
            (let* ((last-string (styled-string-string last-string))
                   (last-string-length (length last-string))
                   (start1 (length last-string))
                   (end1 (+ start1 length)))
              (when (< (array-dimension last-string 0) end1)
                (adjust-array last-string (max end1 (* 2 last-string-length))))
              (setf (fill-pointer last-string) end1)
              (etypecase string
                (character (setf (char last-string (1- end1)) string))
                (string (replace last-string string
                                 :start1 start1 :end1 end1
                                 :start2 start :end2 end))))
            (let ((styled-string (make-instance
                                  'styled-string
                                  :start-x end-x
                                  :text-style text-style
                                  :medium medium
                                  :string (make-array length
                                                      :element-type 'character
                                                      :adjustable t
                                                      :fill-pointer t))))
              (nconcf strings (list styled-string))
              (etypecase string
                (character (setf (char last-string 0) string))
                (string (replace (styled-string-string styled-string) string
                                 :start2 start :end2 end))))))
      (multiple-value-bind (minx miny maxx maxy)
          (text-bounding-rectangle* medium string
                                    :text-style text-style
                                    :start start :end end)
        (declare (ignore miny maxy))
        (setq baseline (max baseline new-baseline)
              ;; KLUDGE: note that END-X here really means
              ;; START-X of the new string.
              left (min left (+ end-x minx))
              end-x (+ end-x string-width)
              right (+ end-x (max 0 (- maxx string-width)))
              max-height (max max-height height)
              end-y (max end-y (+ start-y max-height))
              width (+ width string-width))))
    (tree-recompute-extent text-record)))

(defmethod text-displayed-output-record-string
    ((record standard-text-displayed-output-record))
  (with-slots (strings) record
    (if (= 1 (length strings))
        (styled-string-string (first strings))
        (with-output-to-string (result)
          (loop for styled-string in strings
            do (write-string (styled-string-string styled-string) result))))))

;;; 16.3.4. Top-Level Output Records

(defclass standard-sequence-output-history
    (standard-sequence-output-record stream-output-history-mixin)
  ())

(defclass standard-tree-output-history
    (standard-tree-output-record stream-output-history-mixin)
  ())

;;; 16.4. Output Recording Streams
(defclass standard-output-recording-stream (output-recording-stream
                                            always-repaint-background-mixin)
  ((recording-p :initform t :reader stream-recording-p)
   (drawing-p :initform t :accessor stream-drawing-p)
   (output-history :initform (make-instance 'standard-tree-output-history)
                   :initarg :output-record
                   :reader stream-output-history)
   (current-output-record :accessor stream-current-output-record)
   (current-text-output-record :initform nil
                               :accessor stream-current-text-output-record))
  (:documentation "This class is mixed into some other stream class to
add output recording facilities. It is not instantiable."))

(defmethod initialize-instance :after
    ((stream standard-output-recording-stream) &rest args)
  (declare (ignore args))
  (let ((history (stream-output-history stream)))
    (setf (slot-value history 'stream) stream
          (slot-value stream 'output-history) history
          (stream-current-output-record stream) history)))

;;; 16.4.1 The Output Recording Stream Protocol
(defmethod (setf stream-recording-p)
    (recording-p (stream standard-output-recording-stream))
  (let ((old-val (slot-value stream 'recording-p)))
    (unless (eq old-val recording-p)
      (setf (slot-value stream 'recording-p) recording-p)
      (stream-close-text-output-record stream))
    recording-p))

(defmethod stream-add-output-record
    ((stream standard-output-recording-stream) record)
  (add-output-record record (stream-current-output-record stream)))

(defmethod stream-replay
    ((stream standard-output-recording-stream) &optional region)
  (replay (stream-output-history stream) stream region))

(defun output-record-ancestor-p (ancestor child)
  (loop for record = child then parent
     for parent = (output-record-parent record)
     when (eq parent nil) do (return nil)
     when (eq parent ancestor) do (return t)))

(defun rounded-bounding-rectangle (region)
  ;; return a bounding rectangle whose coordinates have been rounded to
  ;; lock into the pixel grid.  Includes some extra safety to make
  ;; sure antialiasing around the theoretical limits are included, too.
  (with-bounding-rectangle* (x1 y1 x2 y2) region
    (make-rectangle*  (floor (- x1 0.5))
                      (floor (- y1 0.5))
                      (ceiling (+ x2 0.5))
                      (ceiling (+ y2 0.5)))))

(defmethod erase-output-record (record (stream standard-output-recording-stream)
                                &optional (errorp t))
  (with-output-recording-options (stream :record nil)
    (let ((region (rounded-bounding-rectangle record))
          (parent (output-record-parent record)))
      (cond
        ((output-record-ancestor-p (stream-output-history stream) record)
         (delete-output-record record parent))
        (errorp
         (error "~S is not contained in ~S." record stream)))
      (with-bounding-rectangle* (x1 y1 x2 y2) region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+)
        (stream-replay stream region)))))

;;; 16.4.3. Text Output Recording
(defmethod stream-text-output-record
    ((stream standard-output-recording-stream) text-style)
  (declare (ignore text-style))
  (let ((record (stream-current-text-output-record stream)))
    (unless (and record (typep record 'standard-text-displayed-output-record))
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (setf record (make-instance 'standard-text-displayed-output-record
                                    :x-position cx :y-position cy
                                    :start-x cx :start-y cy
                                    :stream stream)
              (stream-current-text-output-record stream) record)))
    record))

(defmethod stream-close-text-output-record ((stream standard-output-recording-stream))
  (when-let ((record (stream-current-text-output-record stream)))
    (setf (stream-current-text-output-record stream) nil)
    #|record stream-current-cursor-position to (end-x record) - already done|#
    (stream-add-output-record stream record)
    ;; STREAM-WRITE-OUTPUT on recorded stream inhibits eager drawing to collect
    ;; whole output record in order to align line's baseline between strings of
    ;; different height. See \"15.3 The Text Cursor\". -- jd 2019-01-07
    (when (stream-drawing-p stream)
      (replay record stream))))

(defmethod stream-add-character-output ((stream standard-output-recording-stream)
                                        character text-style width height baseline)
  (add-character-output-to-text-record (stream-text-output-record stream text-style)
                                       character text-style width height baseline))

(defmethod stream-add-string-output ((stream standard-output-recording-stream)
                                     string start end text-style
                                     width height baseline)
  (add-string-output-to-text-record (stream-text-output-record stream text-style)
                                    string start end text-style
                                    width height baseline))

;;; Text output catching methods
(defmethod stream-write-output ((stream standard-output-recording-stream) line
                                &optional (start 0) end)
  (unless (stream-recording-p stream)
    (return-from stream-write-output
      ;; Stream will replay output when the output record is closed to maintain
      ;; the baseline. If we are not recording and this method is invoked we
      ;; draw string eagerly (if it is set for drawing). -- jd 2019-01-07
      (when (stream-drawing-p stream)
        (call-next-method))))
  (let* ((medium (sheet-medium stream))
         (text-style (medium-text-style medium))
         (height (text-style-height text-style medium))
         (ascent (text-style-ascent text-style medium)))
    (if (characterp line)
        (stream-add-character-output stream line text-style
                                     (stream-character-width
                                      stream line :text-style text-style)
                                     height
                                     ascent)
        (stream-add-string-output stream line start end text-style
                                  (stream-string-width stream line
                                                       :start start :end end
                                                       :text-style text-style)
                                  height
                                  ascent))))

(defmethod stream-finish-output :after ((stream standard-output-recording-stream))
  (stream-close-text-output-record stream))

(defmethod stream-force-output :after ((stream standard-output-recording-stream))
  (stream-close-text-output-record stream))

(defmethod stream-terpri :after ((stream standard-output-recording-stream))
  (stream-close-text-output-record stream))

(defmethod* (setf stream-cursor-position) :after (x y (stream standard-output-recording-stream))
  (declare (ignore x y))
  (stream-close-text-output-record stream))

;;; 16.4.4. Output Recording Utilities

(defmethod invoke-with-output-recording-options
  ((stream output-recording-stream) continuation record draw)
  "Calls CONTINUATION on STREAM enabling or disabling recording and drawing
according to the flags RECORD and DRAW."
  (letf (((stream-recording-p stream) record)
         ((stream-drawing-p stream) draw))
    (funcall continuation stream)))

(defmethod invoke-with-new-output-record
    ((stream output-recording-stream) continuation record-type
     &rest initargs &key parent)
  (with-keywords-removed (initargs (:parent))
    (stream-close-text-output-record stream)
    (let ((new-record (apply #'make-instance record-type initargs)))
      (letf (((stream-current-output-record stream) new-record))
        ;; Should we switch on recording? -- APD
        (funcall continuation stream new-record)
        (force-output stream))
      (if parent
          (add-output-record new-record parent)
          (stream-add-output-record stream new-record))
      new-record)))

(defmethod invoke-with-output-to-output-record
    ((stream output-recording-stream) continuation record-type
     &rest initargs)
  (stream-close-text-output-record stream)
  (let ((new-record (apply #'make-instance record-type initargs)))
    (with-output-recording-options (stream :record t :draw nil)
      (with-temporary-margins (stream :left   '(:absolute 0)
                                      :top    '(:absolute 0)
                                      :right  '(:relative 0)
                                      :bottom '(:relative 0))
        (letf (((stream-current-output-record stream) new-record)
               ((stream-cursor-position stream) (values 0 0)))
          (funcall continuation stream new-record)
          (force-output stream))))
    new-record))

(defmethod make-design-from-output-record (record)
  ;; FIXME
  (declare (ignore record))
  (error "Not implemented."))


;;; Additional methods
(defmethod handle-repaint :around ((stream output-recording-stream) region)
  (with-output-recording-options (stream :record nil)
    (call-next-method)))

;;; FIXME: Change things so the rectangle below is only drawn in response
;;;        to explicit repaint requests from the user, not exposes from X.
;;; FIXME: Use DRAW-DESIGN*, that is fix DRAW-DESIGN*.
(defmethod handle-repaint ((stream output-recording-stream) region)
  (unless (region-equal region +nowhere+) ; ignore repaint requests for +nowhere+
    (let ((region (if (region-equal region +everywhere+)
                      ;; fallback to the sheet's region for +everwhere+.
                      (sheet-region stream)
                      (bounding-rectangle region))))
      (stream-replay stream region))))

(defmethod scroll-extent :around ((stream output-recording-stream) x y)
  (declare (ignore x y))
  (when (stream-drawing-p stream)
    (call-next-method)))

;;; FIXME: think about merging behavior by using WITH-LOCAL-COORDINATES and
;;; WITH-FIRST-QUADRANT-COORDINATES which both work on both mediums and
;;; streams. Also write a documentation chapter describing behavior and
;;; providing some examples.
(defgeneric invoke-with-room-for-graphics
    (cont stream &key first-quadrant height move-cursor record-type))

;;; ----------------------------------------------------------------------------
;;; Complicated, underspecified...
;;;
;;; From examining old Genera documentation, I believe that
;;; with-room-for-graphics is supposed to set the medium transformation to
;;; give the desired coordinate system; i.e., it doesn't preserve any
;;; rotation, scaling or translation in the current medium transformation.
(defmethod invoke-with-room-for-graphics (cont (stream extended-output-stream)
                                          &key (first-quadrant t)
                                          height
                                          (move-cursor t)
                                          (record-type
                                           'standard-sequence-output-record))
  ;; I am not sure what exactly :height should do.           ; [avengers pun]
  ;; --GB 2003-05-25                                         ; -----------------
  ;; The current behavior is consistent with 'classic' CLIM  ; where is genera?
  ;; --Hefner 2004-06-19                                     ;
  ;; Don't know if it still is :)                            ;  what is genera?
  ;; -- Moore 2005-01-26                                     ;
  ;; I think that it doesn't matter ;)                       ;   why is genera?
  ;; -- jd 2018-08-11                                        ; -----------------
  ;;
  ;; More seriously though (comments left for giggles), HEIGHT defaults to the
  ;; output-record height unless specified by the programmer. In that case
  ;; output is clipped to that height and exactly that amount of space is
  ;; reserved for drawing (so if the output-record is smaller we have some empty
  ;; space, if it is bigger it is clipped). In case of panes which does not
  ;; record it will be the only means to assure space in case of the
  ;; FIRST-QUADRANT = T (Y-axis inverted). -- jd
  ;;
  ;; FIXME: clip the output record to HEIGHT if the argument is supplied.
  ;; ADDME: add width argument for clipping (McCLIM extension)
  (multiple-value-bind (cx cy)
      (stream-cursor-position stream)
    (with-sheet-medium (medium stream)
      (letf (((medium-transformation medium)
              (if first-quadrant
                  (make-scaling-transformation 1 -1)
                  +identity-transformation+)))
        (let ((record (with-output-to-output-record (stream record-type)
                        (funcall cont stream))))
          ;; Bounding  rectangle is in sheet coordinates!
          (with-bounding-rectangle* (x1 y1 x2 y2)
              record
            (declare (ignore x2))
            (if first-quadrant
                (setf (output-record-position record)
                      (values (max cx (+ cx x1))
                              (if height
                                  (max cy (+ cy (- height (abs y1))))
                                  cy)))
                (setf (output-record-position record)
                      (values (max cx (+ cx x1)) (max cy (+ cy y1)))))
            (when (stream-recording-p stream)
              (stream-add-output-record stream record))
            (when (stream-drawing-p stream)
              (replay record stream))
            (if move-cursor
                (let ((record-height (- y2 y1)))
                  (setf (stream-cursor-position stream)
                        (values cx
                                (if first-quadrant
                                    (+ cy (max (- y1)
                                               (or height 0)
                                               record-height))
                                    (+ cy (max (or height 0)
                                               record-height))))))
                (setf (stream-cursor-position stream) (values cx cy)))
            record))))))

;;; FIXME: add clipping to HEIGHT and think of how MOVE-CURSOR could be
;;; implemented (so i-w-r-f-g returns an imaginary cursor progress).
(defmethod invoke-with-room-for-graphics (cont stream
                                          &key (first-quadrant t)
                                            height
                                            (move-cursor t)
                                            (record-type nil))
  (declare (ignore move-cursor record-type))
  (with-sheet-medium (medium stream)
    (multiple-value-bind (dx dy)
        (transform-position (medium-transformation medium) 0 0)
      (letf (((medium-transformation medium) (compose-translation-with-transformation
                                              (if first-quadrant
                                                  (make-scaling-transformation 1 -1)
                                                  +identity-transformation+)
                                              dx (if first-quadrant
                                                     (+ dy (or height 100))
                                                     dy))))
        (funcall cont stream)))))

;;; ----------------------------------------------------------------------------
;;;  Baseline
;;;

(defgeneric output-record-baseline (record))

(defmethod output-record-baseline ((record output-record))
  "Fall back method"
  (with-bounding-rectangle* (x1 y1 x2 y2)
      record
    (declare (ignore x1 x2))
    (values (- y2 y1) nil)))

(defmethod output-record-baseline ((record standard-text-displayed-output-record))
  (with-slots (baseline) record
    (values baseline t)))

(defmethod output-record-baseline ((record compound-output-record))
  (map-over-output-records (lambda (sub-record)
                             (multiple-value-bind (baseline definitive)
                                 (output-record-baseline sub-record)
                               (when definitive
                                 (return-from output-record-baseline
                                   (values baseline t)))))
                           record)
  (call-next-method))

;;; ----------------------------------------------------------------------------
;;;  copy-textual-output
;;;

(defun copy-textual-output-history (window stream &optional region record)
  (unless region (setf region +everywhere+))
  (unless record (setf record (stream-output-history window)))
  (let* ((text-style (medium-default-text-style window))
         (char-width (stream-character-width window #\n :text-style text-style))
         (line-height (+ (stream-line-height window :text-style text-style)
                         (stream-vertical-spacing window))))
    ;; humble first ...
    (let ((cy nil)
          (cx 0))
      (labels ((grok-record (record)
                 (cond ((typep record 'standard-text-displayed-output-record)
                        (with-slots (start-y start-x end-x strings) record
                          (setf cy (or cy start-y))
                          (when (> start-y cy)
                            (dotimes (k (round (- start-y cy) line-height))
                              (terpri stream))
                            (setf cy start-y
                                  cx 0))
                          (dotimes (k (round (- start-x cx) char-width))
                            (princ " " stream))
                          (setf cx end-x)
                          (dolist (string strings)
                            (with-slots (string) string
                              (princ string stream)))))
                       (t
                        (map-over-output-records-overlapping-region #'grok-record
                                                                    record region)))))
        (grok-record record)))))
