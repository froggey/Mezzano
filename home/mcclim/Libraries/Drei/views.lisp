;;; -*- Mode: Lisp; Package: DREI -*-

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
;;;
;;; This file contains definitions for Dreis "views", objects that are
;;; used as the actual content of a Drei instance. Drei displays a
;;; view of something, most commonly a buffer, and this view imposes
;;; its own rules on how it sees the buffer contents, the typical
;;; example being parsing it and displaying it with syntax
;;; highlighting. The special buffer classes used by views are also
;;; defined in this file.

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions.

(define-condition user-condition-mixin ()
  ()
  (:documentation "Conditions of this type are caught by the Drei
command loop and their report displayed to the user in the
minibuffer, instead of being propagated further (and possibly
invoking the debugger)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tabify

(defvar *use-tabs-for-indentation* nil
  "If non-NIL, use tabs when indenting lines. Otherwise, use spaces.")

(defclass tabify-mixin ()
  ((%tab-space-count :initform 8
                     :accessor tab-space-count
                     :initarg :tab-space-count)
   ;; We save the old values for performance. Doesn't take text-style
   ;; into account (for performance!)
   (%space-width :accessor recorded-space-width
                 :initform nil)
   (%tab-width :accessor recorded-tab-width
               :initform nil)
   (%recorded-stream :accessor recorded-stream
                     :initform nil)
   (%use-tabs :accessor use-tabs
              :initform *use-tabs-for-indentation*
              :initarg :use-tabs)
   (%tab-stops :accessor tab-stops
               :initform '()
               :initarg :tab-stops
               :documentation "A list of tab-stops in device units.
If empty, tabs every TAB-WIDTH are assumed.")))

(defun maybe-update-recordings (stream tabify)
  (with-accessors ((space-width recorded-space-width)
                   (tab-width recorded-tab-width)
                   (recorded-stream recorded-stream)) tabify
    (unless (eq stream recorded-stream)
      ;; Update the recorded values.
      (setf space-width (climb:text-style-character-width (medium-text-style stream)
                                                          stream
                                                          #\Space)
            tab-width (stream-character-width stream #\Tab)
            recorded-stream stream))))

(defgeneric space-width (stream tabify)
  (:documentation "Return the width of a space character on
`stream' in device units (most likely pixels).")
  (:method ((stream extended-output-stream) (tabify tabify-mixin))
    (maybe-update-recordings stream tabify)
    (recorded-space-width tabify)))

(defgeneric tab-width (stream tabify)
  (:documentation "Return the width of a tab character on
`stream' in device units (most likely pixels).")
  (:method ((stream extended-output-stream) (tabify tabify-mixin))
    (if (tab-space-count tabify)
        (* (tab-space-count tabify) (space-width stream tabify))
        (recorded-tab-width tabify))))

(defgeneric next-tab-stop (stream tabify x)
  (:documentation "Return the distance to the next tab-stop after `x'
on `stream' in device units (most likely pixels).")
  (:method ((stream extended-output-stream) (tabify tabify-mixin) x)
    (flet ((round-up (x width)
             (- width (mod x width))))
      (if (tab-stops tabify)
          (let ((next (find-if (lambda (pos) (> pos x)) (tab-stops tabify))))
            (or (and next (- next x)) (round-up x (space-width stream tabify))))
          (round-up x (tab-width stream tabify))))))

(defgeneric (setf tab-stop-columns) (column-list tabify)
  (:documentation "Set the TAB-STOPS of view at the character column offsets
in `column-list'.")
  (:method (column-list (tabify tabify-mixin))
    (setf (tab-stops tabify) 
          (and column-list
               (sort (mapcar (lambda (col) (* col (space-width (recorded-stream tabify) tabify)))
                             column-list) 
                     #'<)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo

(defgeneric undo-tree (buffer)
  (:documentation "The undo-tree object associated with the
buffer. This usually contains a record of every change that has
been made to the buffer since it was created."))

(defgeneric undo-accumulate (buffer)
  (:documentation "A list of the changes that have been made to
`buffer' since the last time undo was added to the undo tree for
the buffer. The list returned by this function is initially
NIL (the empty list). The :before methods on
`insert-buffer-object', `insert-buffer-sequence', and
`delete-buffer-range' push undo records on to this list."))

(defgeneric performing-undo (buffer)
  (:documentation "If true, the buffer is currently performing an
undo operation. The :before methods on `insert-buffer-object',
`insert-buffer-sequence', and `delete-buffer-range' push undo
records onto the undo accumulator only if `performing-undo' is
false, so that no undo information is added as a result of an
undo operation."))

(defclass undo-mixin ()
  ((tree :initform (make-instance 'standard-undo-tree)
         :reader undo-tree
         :documentation "Returns the undo-tree of the buffer.")
   (undo-accumulate :initform '()
                    :accessor undo-accumulate
                    :documentation "The undo records created
since the start of the undo context.")
   (performing-undo :initform nil
                    :accessor performing-undo
                    :documentation "True if we are currently
performing undo, false otherwise."))
  (:documentation "This is a mixin class that buffer classes can
inherit from. It contains an undo tree, an undo accumulator and a
flag specifyng whether or not it is currently performing
undo. The undo tree and undo accumulators are initially empty."))

(defclass drei-undo-record (standard-undo-record)
  ((buffer :initarg :buffer
           :documentation "The buffer to which the record
belongs."))
  (:documentation "A base class for all output records in
Drei."))

(defclass simple-undo-record (drei-undo-record)
  ((offset :initarg :offset
           :reader undo-offset
           :documentation "The offset that determines the
position at which the undo operation is to be executed."))
  (:documentation "A base class for output records that modify
buffer contents at a specific offset."))

(defclass insert-record (simple-undo-record)
  ((objects :initarg :objects
            :documentation "The sequence of objects that are to
be inserted whenever flip-undo-record is called on an instance of
insert-record."))
  (:documentation "Whenever objects are deleted, the sequence of
objects is stored in an insert record containing a mark."))

(defclass delete-record (simple-undo-record)
  ((length :initarg :length
           :documentation "The length of the sequence of objects
to be deleted whenever `flip-undo-record' is called on an
instance of `delete-record'."))
  (:documentation "Whenever objects are inserted, a
`delete-record' containing a mark is created and added to the
undo tree."))

(defclass change-record (simple-undo-record)
  ((objects :initarg :objects
            :documentation "The sequence of objects that are to 
replace the records that are currently in the buffer at the 
offset whenever flip-undo-record is called on an instance of 
change-record"))
  (:documentation "Whenever objects are modified, a 
`change-record' containing a mark is created and added to the 
undo tree."))

(defclass compound-record (drei-undo-record)
  ((records :initform '()
            :initarg :records
            :documentation "The undo records contained by this
compound record."))
  (:documentation "This record simply contains a list of other
records."))

(defmethod print-object  ((object delete-record) stream)
  (with-slots (offset length) object
    (format stream "[offset: ~a length: ~a]" offset length)))

(defmethod print-object  ((object insert-record) stream)
  (with-slots (offset objects) object
    (format stream "[offset: ~a inserted objects: ~a]" offset objects)))

(defmethod print-object  ((object change-record) stream)
  (with-slots (offset objects) object
    (format stream "[offset: ~a changed objects: ~a]" offset objects)))

(defmethod print-object  ((object compound-record) stream)
  (with-slots (records) object
    (format stream "[records: ~a]" records)))

(defmethod insert-buffer-object :before ((buffer undo-mixin) offset object)
  (declare (ignore object))
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
                         :buffer buffer :offset offset :length 1)
          (undo-accumulate buffer))))

(defmethod insert-buffer-sequence :before ((buffer undo-mixin) offset sequence)
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
                         :buffer buffer :offset offset :length (length sequence))
          (undo-accumulate buffer))))

(defmethod delete-buffer-range :before ((buffer undo-mixin) offset n)
  (unless (performing-undo buffer)
    (push (make-instance 'insert-record
                         :buffer buffer :offset offset
                         :objects (buffer-sequence buffer offset (+ offset n)))
          (undo-accumulate buffer))))

(defmethod (setf buffer-object) :before (new-object (buffer undo-mixin) offset)
  (unless (performing-undo buffer)
    (push (make-instance 'change-record
                         :buffer buffer
                         :offset offset
                         :objects (buffer-sequence buffer offset (1+ offset)))
          (undo-accumulate buffer))))

(defmacro with-undo ((get-buffers-exp) &body body)
  "This macro executes the forms of `body', registering changes
made to the list of buffers retrieved by evaluating
`get-buffers-exp'. When `body' has run, for each buffer it will
call `add-undo' with an undo record and the undo tree of the
buffer.  If the changes done by `body' to the buffer has resulted
in only a single undo record, it is passed as is to `add-undo'.
If it contains several undo records, a compound undo record is
constructed out of the list and passed to `add-undo'.  Finally,
if the buffer has no undo records, `add-undo' is not called at
all."
  (with-gensyms (buffer)
    `(progn
       (dolist (,buffer ,get-buffers-exp)
         (setf (undo-accumulate ,buffer) '()))
       (unwind-protect (progn ,@body)
         (dolist (,buffer ,get-buffers-exp)
           (cond ((null (undo-accumulate ,buffer)) nil)
                 ((null (cdr (undo-accumulate ,buffer)))
                  (add-undo (car (undo-accumulate ,buffer))
                            (undo-tree ,buffer)))
                 (t
                  (add-undo (make-instance 'compound-record
                                           :buffer ,buffer
                                           :records (undo-accumulate ,buffer))
                            (undo-tree ,buffer)))))))))

(defmethod flip-undo-record :around ((record drei-undo-record))
  (with-slots (buffer) record
    (let ((performing-undo (performing-undo buffer)))
      (setf (performing-undo buffer) t)
      (unwind-protect (call-next-method)
        (setf (performing-undo buffer) performing-undo)))))

(defmethod flip-undo-record ((record insert-record))
  (with-slots (buffer offset objects) record
    (let ((%buffer buffer)
          (%offset offset)
          (%objects objects))
      (change-class record 'delete-record
                    :length (length %objects))
      (insert-buffer-sequence %buffer %offset %objects))))

(defmethod flip-undo-record ((record delete-record))
  (with-slots (buffer offset length) record
    (let ((%buffer buffer)
          (%offset offset)
          (%length length))
      (change-class record 'insert-record
                    :objects (buffer-sequence %buffer %offset (+ %offset %length)))
      (delete-buffer-range %buffer %offset %length))))

(defmethod flip-undo-record ((record change-record))
  (with-slots (buffer offset objects) record
    (loop for i from 0 below (length objects)
          do (rotatef (aref objects i) (buffer-object buffer (+ i offset))))))

(defmethod flip-undo-record ((record compound-record))
  (with-slots (records) record
    (mapc #'flip-undo-record records)
    (setf records (nreverse records))))

(defgeneric clear-undo-history (undo-maintainer)
  (:documentation "Clear the undo history for `undo-maintainer',
preventing the undoing to before the state of whatever
`undo-maintainer' is maintaining undo for."))

(defmethod clear-undo-history ((undo-maintainer undo-mixin))
  (setf (slot-value undo-maintainer 'tree)
        (make-instance 'standard-undo-tree)
        (undo-accumulate undo-maintainer) '()))

;;; undo-mixin delegation (here because of the package)

(defmethod undo-tree ((buffer delegating-buffer))
  (undo-tree (implementation buffer)))

(defmethod undo-accumulate ((buffer delegating-buffer))
  (undo-accumulate (implementation buffer)))

(defmethod (setf undo-accumulate) (object (buffer delegating-buffer))
  (setf (undo-accumulate (implementation buffer)) object))

(defmethod performing-undo ((buffer delegating-buffer))
  (performing-undo (implementation buffer)))

(defmethod (setf performing-undo) (object (buffer delegating-buffer))
  (setf (performing-undo (implementation buffer)) object))

(defmethod clear-undo-history ((buffer delegating-buffer))
  (clear-undo-history (implementation buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readonly

(defclass read-only-mixin ()
  ((read-only-p :initform nil
                :accessor read-only-p
                :initarg :read-only)))

(define-condition buffer-read-only (user-condition-mixin simple-error)
  ((buffer :reader condition-buffer :initarg :buffer))
  (:report (lambda (condition stream)
             (format stream "Attempt to change read only buffer: ~a"
                     (condition-buffer condition))))
  (:documentation "This condition is signalled whenever an attempt
is made to alter a buffer which has been set read only."))

(defmethod insert-buffer-object ((buffer read-only-mixin) offset object)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod insert-buffer-sequence ((buffer read-only-mixin) offset sequence)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod delete-buffer-range ((buffer read-only-mixin) offset n)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod (setf buffer-object) (object (buffer read-only-mixin) offset)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod read-only-p ((buffer delegating-buffer))
  (read-only-p (implementation buffer)))

(defmethod (setf read-only-p) (flag (buffer delegating-buffer))
  (setf (read-only-p (implementation buffer)) flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Single-line buffer.

(defclass single-line-mixin ()
  ((%single-line-p :initform nil
                   :accessor single-line-p
                   :initarg :single-line))
  (:documentation "Prevent the insertion of #\Newline characters
into the buffer if `single-line-p' is true."))

(define-condition buffer-single-line (user-condition-mixin simple-error)
  ((buffer :reader condition-buffer :initarg :buffer))
  (:report "Attempt to insert newline into single-line buffer.")
  (:documentation "This condition is signalled whenever an
attempt is made to insert a #\Newline character into a
single-line buffer."))

(defmethod insert-buffer-object :before ((buffer single-line-mixin) offset (object (eql #\Newline)))
  (when (single-line-p buffer)
    (error 'buffer-single-line :buffer buffer)))

(defmethod insert-buffer-sequence :before ((buffer single-line-mixin) offset sequence)
  (when (and (single-line-p buffer)
             (find #\Newline sequence))
    (error 'buffer-single-line :buffer buffer)))

(defmethod (setf buffer-object) :before ((object (eql #\Newline)) (buffer single-line-mixin) offset)
  (when (single-line-p buffer)
    (error 'buffer-single-line :buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Drei buffer.

(defclass extended-standard-buffer (single-line-mixin
                                    read-only-mixin standard-buffer
                                    undo-mixin abbrev-mixin
                                    observable-buffer-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass extended-binseq2-buffer (single-line-mixin
                                   read-only-mixin binseq2-buffer
                                   p-undo-mixin abbrev-mixin
                                   observable-buffer-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass drei-buffer (delegating-buffer esa-buffer-mixin
                       observable-buffer-mixin)
  ((point :initarg :point :initform nil :accessor point-of))
  (:default-initargs :implementation (make-instance 'extended-standard-buffer)))

(defmethod initialize-instance :after ((buffer drei-buffer) &rest args
                                       &key read-only single-line
                                         initial-contents)
  (declare (ignore args))
  (with-accessors ((point point)
                   (implementation implementation)) buffer
    (when initial-contents
      (check-type initial-contents array)
      (insert-buffer-sequence buffer 0 initial-contents))
    (setf point (make-buffer-mark buffer 0 :right))
    (setf (read-only-p implementation) read-only
          (single-line-p implementation) single-line)
    ;; Hack: we need to be told whenever the undo facilities in the
    ;; implementation buffer changes the buffer contents.
    (add-observer (implementation buffer) buffer)))

(defmethod observer-notified ((observer drei-buffer)
                              (observable observable-buffer-mixin)
                              data)
  (notify-observers observer (constantly data)))

(defmethod notify-observers :after ((buffer drei-buffer)
                                    &optional data-fn)
  (declare (ignore data-fn))
  ;; This means that any buffer modification sets the needs-saving
  ;; flag to true. It might be nice if undo back to the last saved
  ;; state would set it to false.
  (setf (needs-saving buffer) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View classes.

(defclass drei-view (tabify-mixin subscriptable-name-mixin)
  ((%active :accessor active
            :initform t
            :initarg :active
            :type boolean
            :documentation "A boolean value indicating whether
the view is \"active\". This should control highlighting when
redisplaying.")
   (%modified-p :accessor modified-p
                :initform nil
                :initarg :modified-p
                :documentation "This value is true if the view
contents have been modified since the last time this value was
set to false.")
   (%no-cursors :accessor no-cursors
                :initarg :no-cursors
                :initform nil
                :documentation "True if the view does not display
cursors.")
   (%full-redisplay-p :accessor full-redisplay-p
                      :initform nil
                      :documentation "True if the view should be
fully redisplayed the next time it is redisplayed.")
   (%use-editor-commands :accessor use-editor-commands-p
                         :initarg :use-editor-commands
                         :initform nil
                         :documentation "If the view is supposed
to support standard editor commands (for inserting objects,
moving cursor, etc), this will be true. If you want your view to
support standard editor commands, you should *not* inherit from
`editor-table' - the command tables containing the editor
commands will be added automatically, as long as this value is
true.")
   (%extend-pane-bottom :accessor extend-pane-bottom
                        :initarg :extend-pane-bottom
                        :initform nil
                        :documentation "Resize the output pane
vertically during redisplay (using `change-space-requirements'),
in order to fit the whole buffer. If this value is false,
redisplay will stop when the bottom of the pane is reached."))
  (:metaclass modual-class)
  (:documentation "The base class for all Drei views. A view
observes some other object and provides a visual representation
for Drei.")
  (:default-initargs :name "*scratch*"))

(defmethod print-object ((view drei-view) stream)
  (print-unreadable-object (view stream :type t :identity t)
    (format stream "name: ~a ~a" (name view) (subscript view))))

(defmethod available-modes append ((modual drei-view))
  *global-modes*)

(defmethod mode-applicable-p or ((modual drei-view) mode-name)
  (mode-applicable-p (syntax modual) mode-name))

(defgeneric synchronize-view (view &key &allow-other-keys)
  (:documentation "Synchronize the view with the object under
observation - what exactly this entails, and what keyword
arguments are supported, is up to the individual view
subclass.")
  (:method ((view drei-view) &key)
    nil))

(defgeneric view-command-tables (view)
  (:documentation "Return a list of command tables containing
commands relevant for `view'.")
  (:method-combination append)
  (:method append ((view drei-view))
    '(view-table)))

(defgeneric create-view-cursors (output-stream view)
  (:documentation "Create cursors for `view' that are to be
displayed on `output-stream'.")
  (:method nconc (output-stream (view drei-view))
    '())
  (:method :around (output-stream (view drei-view))
    (unless (no-cursors view)
      (call-next-method)))
  (:method-combination nconc))

(defgeneric clear-redisplay-information (view)
  (:documentation "Clear any redisplay information `view' may
retain, so that a full redisplay will be performed the next time
it is redisplayed."))

(defgeneric clone-view (view &rest initargs)
  (:documentation "Clone the view object `view'. `Initargs' can
be used to supply different values to the initargs of the
class. A default method doing slot-by-slot copying of `view' has
been defined that should be appropriate for most view classes.")
  (:method ((view view) &rest initargs)
    ;; We iterate over the slots of `view', remembering the value and
    ;; initarg for any slot with an initarg, and use this to create
    ;; the new `make-instance' form. We assume that any slot with no
    ;; initarg will get its value from an `initialize-instance' method
    ;; or similar.
    (apply #'make-instance (class-of view)
           (append initargs
                   (loop for slot in (c2mop:class-slots (class-of view))
                         for slot-initarg = (first (c2mop:slot-definition-initargs slot))
                         for slot-name = (c2mop:slot-definition-name slot)
                         for slot-boundp = (slot-boundp view slot-name)
                         when (and slot-initarg slot-boundp)
                           nconc (list slot-initarg (slot-value view slot-name)))))))

(defgeneric page-down (pane view)
  (:documentation "Scroll `view', which is displayed on `pane', a
page up."))

(defgeneric page-up (pane view)
  (:documentation "Scroll `view', which is displayed on `pane', a
page up."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer view

(defclass drei-buffer-view (drei-view)
  ((%buffer :accessor buffer
            :initarg :buffer
            :type drei-buffer
            :documentation "The buffer that is observed by this
buffer view.")
   (%top :accessor top
         :documentation "The top of the displayed buffer, that
is, the mark indicating the first visible object in the buffer.")
   (%bot :accessor bot
         :documentation "The bottom of the displayed buffer, that
is, the mark indicating the last visible object in the buffer.")
   (%cache-string :reader cache-string
                  :initform (make-array 0 :element-type 'character
                                          :adjustable t
                                          :fill-pointer 0)
                  :documentation "A string used during redisplay
to reduce consing. Instead of consing up a new string every time
we need to pull out a buffer region, we put it in this
string. The fill pointer is automatically set to zero whenever
the string is accessed through the reader.")
   (%displayed-lines :accessor displayed-lines
                     :initform (make-array 0 :element-type 'displayed-line
                                             :initial-element (make-displayed-line))
                     :type array
                     :documentation "An array of the
`displayed-line' objects displayed by the view. Not all of these
are live.")
   (%displayed-lines-count :accessor displayed-lines-count
                           :initform 0
                           :type integer
                           :documentation "The number of lines in
the views `displayed-lines' array that are actually live, that
is, used for display right now.")
   (%max-line-width :accessor max-line-width
                    :initform 0
                    :type number
                    :documentation "The width of the longest
displayed line in device units.")
   (%lines :initform (make-instance 'standard-flexichain)
           :reader lines
           :documentation "The lines of the buffer, stored in a
format that makes it easy to retrieve information about them.")
   (%lines-prefix :accessor lines-prefix-size
                  :documentation "The number of unchanged
objects at the start of the buffer since the list of lines was
last updated.")
   (%lines-suffix :accessor lines-suffix-size
                  :documentation "The number of unchanged objects
at the end of the buffer since since the list of lines was last
updated.")
   (%last-seen-buffer-size :accessor last-seen-buffer-size
                           :documentation "The buffer size the
last time a change to the buffer was registered."))
  (:metaclass modual-class)
  (:documentation "A view that contains a `drei-buffer'
object. The buffer is displayed on a simple line-by-line basis,
with top and bot marks delimiting the visible region. These marks
are automatically set if applicable."))

(defmethod initialize-instance :after ((view drei-buffer-view) &rest initargs
                                       &key buffer single-line read-only
                                         initial-contents)
  (declare (ignore initargs))
  (with-accessors ((top top) (bot bot)
                   (lines-prefix lines-prefix-size)
                   (lines-suffix lines-suffix-size)
                   (buffer-size last-seen-buffer-size)) view
    (unless buffer
      ;; So many fun things are defined on (setf buffer) that we use
      ;; slot-value here. This is just a glorified initform anyway.
      (setf (slot-value view '%buffer) (make-instance 'drei-buffer
                                                      :single-line single-line
                                                      :read-only read-only
                                                      :initial-contents initial-contents)))
    (setf top (make-buffer-mark (buffer view) 0 :left)
          bot (clone-mark top :right)
          lines-prefix 0
          lines-suffix 0
          buffer-size (size (buffer view)))
    (update-line-data view)))

(defmethod (setf top) :after (new-value (view drei-buffer-view))
  (invalidate-all-strokes view))

(defmethod (setf bot) :after (new-value (view drei-buffer-view))
  (invalidate-all-strokes view))

(defmethod (setf buffer) :after (buffer (view drei-buffer-view))
  (invalidate-all-strokes view)
  (with-accessors ((top top) (bot bot) (lines lines)
                   (lines-prefix lines-prefix-size)
                   (lines-suffix lines-suffix-size)
                   (buffer-size last-seen-buffer-size)) view
    (setf top (make-buffer-mark buffer 0 :left)
          bot (clone-mark top :right)
          lines-prefix 0
          lines-suffix 0
          buffer-size 0)
    (delete-elements* lines 0 (nb-elements lines))
    (update-line-data view)))

(defmethod cache-string :around ((view drei-buffer-view))
  (let ((string (call-next-method)))
    (setf (fill-pointer string) 0)
    string))

(defun buffer-view-p (view)
  "Return true if `view' is a `drei-buffer-view'."
  (typep view 'drei-buffer-view))

(defmethod clear-redisplay-information ((view drei-buffer-view))
  (invalidate-all-strokes view))

(defun overlaps (x1 x2 y1 y2)
  "Return true if the x1/x2 region overlaps with y1/y2."
  (or (<= x1 y1 x2)
      (<= y1 x1 y2)
      (<= y1 x1 x2 y2)
      (<= x1 y1 y1 x2)))

(defclass buffer-line ()
  ((%start-mark :reader start-mark
                :initarg :start-mark
                :documentation "The mark at which this line starts.")
   (%end-mark :reader end-mark
              :initarg :end-mark
              :documentation "The mark at which this line ends.")
   (%chunks :accessor chunks
            :initform (make-array 5
                                  :adjustable t
                                  :fill-pointer 0)
            :documentation "A list of cons-cells, with the car
being a buffer offset relative to the `start-mark' of the line,
and the cdr being T if the chunk covers a non-character, and NIL
if it covers a character sequence."))
  (:documentation "An object describing a single line in the
buffer associated with a `drei-buffer-view'"))

(defmethod initialize-instance :after ((line buffer-line)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop with buffer = (buffer (start-mark line))
        with line-start-offset = (offset (start-mark line))
        with line-end-offset = (+ line-start-offset (line-length line))
        with chunk-start-offset = line-start-offset
        for chunk-info = (get-chunk buffer
                                    line-start-offset
                                    chunk-start-offset line-end-offset)
        do (vector-push-extend chunk-info (chunks line))
           (setf chunk-start-offset (+ (car chunk-info)
                                       line-start-offset))
        when (= chunk-start-offset line-end-offset)
          do (loop-finish)))

(defmethod start-offset ((line buffer-line))
  (offset (start-mark line)))

(defmethod end-offset ((line buffer-line))
  (offset (end-mark line)))

(defun line-length (line)
  "Return the length of the `buffer-line' object `line'."
  (- (end-offset line) (start-offset line)))

(defvar *maximum-chunk-size* 100
  "The maximum amount of objects put into a single chunk by a
`drei-buffer-view'. Actual chunks may be smaller if a #\Newline
character is encountered.")

(defun get-chunk (buffer line-start-offset chunk-start-offset line-end-offset)
  "Return a chunk in the form of a cons cell. The chunk will
start at `chunk-start-offset' and extend no further than
`line-end-offset'."
  (let* ((chunk-end-offset (buffer-find-nonchar
                            buffer chunk-start-offset
                            (min (+ *maximum-chunk-size*
                                    chunk-start-offset)
                                 line-end-offset))))
    (cond ((= chunk-start-offset line-end-offset)
           (cons (- chunk-end-offset
                    line-start-offset) nil))
          ((or (not (= chunk-end-offset chunk-start-offset))
               (and (offset-beginning-of-line-p buffer chunk-start-offset)
                    (offset-end-of-line-p buffer chunk-end-offset)))
           (cons (- chunk-end-offset
                    line-start-offset) nil))
          ((not (characterp (buffer-object buffer chunk-end-offset)))
           (cons (- (1+ chunk-end-offset)
                    line-start-offset) t)))))

(defun update-line-data (view)
  "Update the sequence of lines stored by the `drei-buffer-view'
`view'."
  (with-accessors ((prefix-size lines-prefix-size)
                   (suffix-size lines-suffix-size)) view
    (when (<= prefix-size (- (size (buffer view)) suffix-size))
      (let ((low-mark (make-buffer-mark (buffer view) prefix-size :left))
            (high-mark (make-buffer-mark
                        (buffer view) (- (size (buffer view)) suffix-size) :left)))
        (beginning-of-line low-mark)
        (end-of-line high-mark)
        (with-accessors ((lines lines)) view
          (let ((low-index 0)
                (high-index (nb-elements lines)))
            ;; Binary search for the start of changed lines.
            (loop while (< low-index high-index)
                  do (let* ((middle (floor (+ low-index high-index) 2))
                            (line-start (start-mark (element* lines middle))))
                       (cond ((mark> low-mark line-start)
                              (setf low-index (1+ middle)))
                             (t
                              (setf high-index middle)))))
            ;; Discard lines that have to be re-analyzed.
            (loop while (and (< low-index (nb-elements lines))
                             (mark<= (start-mark (element* lines low-index))
                                     high-mark))
                  do (delete* lines low-index))
            ;; Analyze new lines.
            (loop while (mark<= low-mark high-mark)
                  for i from low-index
                  do (progn (let ((line-start-mark (clone-mark low-mark :left))
                                  (line-end-mark (clone-mark (end-of-line low-mark) :right)))
                              (insert* lines i (make-instance 'buffer-line
                                                              :start-mark line-start-mark
                                                              :end-mark line-end-mark))
                              (if (end-of-buffer-p low-mark)
                                  (loop-finish)
                                  ;; skip newline
                                  (forward-object low-mark)))))))))
    (setf prefix-size (size (buffer view))
          suffix-size (size (buffer view)))))

(defmethod observer-notified ((view drei-buffer-view) (buffer drei-buffer)
                              changed-region)
  (destructuring-bind (start-offset . end-offset) changed-region
    (with-accessors ((prefix-size lines-prefix-size)
                     (suffix-size lines-suffix-size)
                     (buffer-size last-seen-buffer-size)) view
      ;; Figure out whether the change involved insertion or deletion of
      ;; a newline.
      (let* ((line-index (index-of-line-containing-offset view start-offset))
             (line (element* (lines view) line-index))
             (newline-change
               (or (loop for index from start-offset below end-offset
                         when (equal (buffer-object (buffer view) index) #\Newline)
                           return t)
                   ;; If the line is joined with the one before or
                   ;; after it, a newline object has been removed.
                   (or (when (< (1+ line-index) (nb-elements (lines view)))
                         (= (start-offset (element* (lines view) (1+ line-index)))
                            (end-offset line)))
                       (when (plusp line-index)
                         (= (end-offset (element* (lines view) (1- line-index)))
                            (start-offset line)))))))
        ;; If the line structure changed, everything after the newline is suspect.
        (invalidate-strokes-in-region view start-offset
                                      (if newline-change
                                          (max start-offset (offset (bot view)))
                                          end-offset)
                                      :modified t
                                      :to-line-end t)
        (setf prefix-size (min start-offset prefix-size)
              suffix-size (min (- (size buffer) end-offset) suffix-size)
              buffer-size (size buffer))
        ;; If the line structure changed, we need to update the line
        ;; data, or we can't pick up future changes correctly.
        (when newline-change
          (update-line-data view))))))

(defmethod synchronize-view ((view drei-buffer-view) &key)
  (update-line-data view))

;;; Exploit the stored line information.

(defun offset-in-line-p (line offset)
  "Return true if `offset' is in the buffer region delimited by
`line'."
  (<= (offset (start-mark line)) offset
      (end-offset line)))

(defun index-of-line-containing-offset (view mark-or-offset)
  "Return the index of the line `mark-or-offset' is in for
`view'. `View' must be a `drei-buffer-view'."
  ;; Perform binary search looking for line containing `offset1'.
  (as-offsets ((offset mark-or-offset))
    (with-accessors ((lines lines)) view
      (loop with low-index = 0
            with high-index = (nb-elements lines)
            for middle = (floor (+ low-index high-index) 2)
            for this-line = (element* lines middle)
            for line-start = (start-mark this-line)
            do (cond ((offset-in-line-p this-line offset)
                      (loop-finish))
                     ((mark> offset line-start)
                      (setf low-index (1+ middle)))
                     ((mark< offset line-start)
                      (setf high-index middle)))
            finally (return middle)))))

(defun line-containing-offset (view mark-or-offset)
  "Return the line `mark-or-offset' is in for `view'. `View'
must be a `drei-buffer-view'."
  ;; Perform binary search looking for line containing `offset1'.
  (as-offsets ((offset mark-or-offset))
    (with-accessors ((lines lines)) view
      (element* lines (index-of-line-containing-offset view offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax views

(defclass drei-syntax-view (drei-buffer-view)
  ((%syntax :accessor syntax
            :documentation "An instance of the syntax class used
for this syntax view.")
   (%prefix-size :accessor prefix-size
                 :initform 0
                 :documentation "The number of unchanged objects
at the beginning of the buffer.")
   (%suffix-size :accessor suffix-size
                 :initform 0
                 :documentation  "The number of unchanged objects
at the end of the buffer.")
   (%recorded-buffer-size :accessor buffer-size
                          :initform -1
                          :documentation "The size of the buffer
the last time the view was synchronized."))
  (:metaclass modual-class)
  (:documentation "A buffer-view that maintains a parse tree of
the buffer, or otherwise pays attention to the syntax of the
buffer."))

(defmethod initialize-instance :after ((view drei-syntax-view) &rest args
                                       &key (syntax *default-syntax*))
  (declare (ignore args))
  (check-type syntax (or symbol syntax))
  (with-accessors ((buffer buffer)
                   (suffix-size suffix-size)
                   (prefix-size prefix-size)) view
    (setf (slot-value view '%syntax)
          (if (symbolp syntax)
              (make-syntax-for-view view syntax)
              syntax))
    (add-observer (syntax view) view)
    (add-observer buffer view)))

(defmethod (setf buffer) :before ((buffer drei-buffer) (view drei-syntax-view))
  ;; Remove the observation of the old buffer.
  (with-accessors ((old-buffer buffer)) view
    (remove-observer old-buffer view)))

(defmethod (setf buffer) :after ((buffer drei-buffer) (view drei-syntax-view))
  ;; Add observation of the new buffer.
  (add-observer buffer view)
  ;; We need a new syntax object of the same type as the old one, and
  ;; to zero out the unchanged-prefix-values.
  (with-accessors ((view-syntax syntax)) view
    (setf view-syntax (make-syntax-for-view view (class-of view-syntax)))))

(defmethod (setf syntax) :before (syntax (view drei-syntax-view))
  (remove-observer (syntax view) view))

(defmethod (setf syntax) :after (syntax (view drei-syntax-view))
  (add-observer syntax view)
  (setf (prefix-size view) 0
        (suffix-size view) 0
        (buffer-size view) -1))

(defun syntax-view-p (view)
  "Return true if `view' is a `drei-syntax-view'."
  (typep view 'drei-syntax-view))

(defmethod mode-enabled-p or ((modual drei-syntax-view) mode-name)
  (mode-enabled-p (syntax modual) mode-name))

(defmethod enable-mode ((modual drei-syntax-view) mode-name &rest initargs)
  (if (mode-applicable-p (syntax modual) mode-name)
      (apply #'enable-mode (syntax modual) mode-name initargs)
      (call-next-method)))

(defmethod disable-mode ((modual drei-syntax-view) mode-name)
  (if (mode-applicable-p (syntax modual) mode-name)
      (disable-mode (syntax modual) mode-name)
      (call-next-method)))

(defmethod observer-notified ((view drei-syntax-view) (buffer drei-buffer)
                              changed-region)
  (destructuring-bind (start-offset . end-offset) changed-region
    (with-accessors ((prefix-size prefix-size)
                     (suffix-size suffix-size)
                     (modified-p modified-p)) view
      (setf prefix-size (min start-offset prefix-size)
            suffix-size (min (- (size buffer) end-offset) suffix-size)
            modified-p t)))
  (call-next-method))

(defmethod observer-notified ((view drei-syntax-view) (syntax syntax)
                              changed-region)
  (destructuring-bind (start-offset . end-offset) changed-region
    (invalidate-strokes-in-region view start-offset end-offset :modified t)))

(defun needs-resynchronization (view)
  "Return true if the view of the buffer of `view' is
potentially out of date. Return false otherwise."  
  (not (= (prefix-size view) (suffix-size view)
          (buffer-size view) (size (buffer view)))))

(defmethod synchronize-view ((view drei-syntax-view)
                             &key (begin 0) (end (size (buffer view)))
                               force-p)
  "Synchronize the syntax view with the underlying
buffer. `Begin' and `end' are offsets specifying the region of
the buffer that must be synchronised, defaulting to 0 and the
size of the buffer respectively."
  (assert (>= end begin))
  ;; If nothing changed, then don't call the other methods.
  (when (or (needs-resynchronization view) force-p)
    (let ((prefix-size (prefix-size view))
          (suffix-size (suffix-size view)))
      ;; Set some minimum values here so if `update-syntax' calls
      ;; `update-parse' itself, we won't end with infinite recursion.
      (setf (prefix-size view) (max (if (> begin prefix-size)
                                        prefix-size
                                        end)
                                    prefix-size)
            (suffix-size view) (max (if (>= end (- (size (buffer view)) suffix-size))
                                        (max (- (size (buffer view)) begin) suffix-size)
                                        suffix-size)
                                    suffix-size)
            (buffer-size view) (size (buffer view)))
      (multiple-value-bind (parsed-start parsed-end)
          (update-syntax (syntax view) prefix-size suffix-size begin end)
        (assert (>= parsed-end parsed-start))
        ;; Now set the proper new values for prefix-size and
        ;; suffix-size.
        (setf (prefix-size view) (max (if (>= prefix-size parsed-start)
                                          parsed-end
                                          prefix-size)
                                      prefix-size)
              (suffix-size view) (max (if (>= parsed-end (- (size (buffer view)) suffix-size))
                                          (- (size (buffer view)) parsed-start)
                                          suffix-size)
                                      suffix-size)))))
  (call-next-method))

(defun make-syntax-for-view (view syntax-symbol &rest args)
  (apply #'make-instance syntax-symbol
         :buffer (buffer view)
         :updater-fns (list (lambda (begin end)
                              (synchronize-view view :begin begin :end end)))
         args))

(defgeneric pump-state-for-offset-with-syntax (view syntax offset)
  (:documentation "Return a pump state that will enable pumping
strokes from `offset' in the buffer of `view' as specified by
`syntax' (via `stroke-pump-for-syntax'). The pump state is not
guaranteed to be valid past the next call to
`stroke-pump-for-syntax' or `synchronize-view'. The results are
undefined if `offset' is not at the beginning of a line."))

(defgeneric stroke-pump-with-syntax (view syntax stroke pump-state)
  (:documentation "Put stroke information in `stroke' as
specified by `syntax', returns new pump-state. `Pump-state' must
either be the result of a call to
`pump-state-for-offset-with-syntax' or be the return value of an
earlier call to `stroke-pump-with-syntax'. A pump state is not
guaranteed to be valid past the next call to
`stroke-pump-with-syntax' or `synchronize-view'. It is
permissible for `pump-state' to be destructively modified by this
function."))

(defclass point-mark-view (drei-buffer-view)
  ((%point :initform nil :initarg :point :accessor point-of)
   (%mark :initform nil :initarg :mark :accessor mark-of)
   (%goal-column :initform nil :accessor goal-column
                 :documentation "The column that point will be
attempted to be positioned in when moving by line."))
  (:metaclass modual-class)
  (:documentation "A view class containing a point and a mark
into its buffer."))

(defmethod initialize-instance :after ((view point-mark-view)
                                       &rest args)
  (declare (ignore args))
  (with-accessors ((point point) (mark mark)
                   (buffer buffer)) view
    (setf point (clone-mark (point buffer)))
    (setf mark (clone-mark (point buffer)))))

(defmethod (setf buffer) :before ((buffer drei-buffer) (view point-mark-view))
  ;; Set the point of the old buffer to the current point of the view,
  ;; so the next time the buffer is revealed, it will remember its
  ;; point.
  (setf (point (buffer view)) (point view)))

(defmethod (setf buffer) :after ((buffer drei-buffer) (view point-mark-view))
  (with-accessors ((point point) (mark mark)) view
    (setf point (clone-mark (point buffer))
          mark (clone-mark (point buffer) :right))))

(defun point-mark-view-p (view)
  "Return true if `view' is a `point-mark-view'"
  (typep view 'point-mark-view))

(defclass textual-drei-syntax-view (drei-syntax-view point-mark-view textual-view)
  ((%auto-fill-mode :initform nil :accessor auto-fill-mode)
   (%auto-fill-column :initform 70 :accessor auto-fill-column)
   (%region-visible-p :initform nil :accessor region-visible-p)
   ;; for dynamic abbrev expansion
   (%original-prefix :initform nil :accessor original-prefix)
   (%prefix-start-offset :initform nil :accessor prefix-start-offset)
   (%dabbrev-expansion-mark :initform nil :accessor dabbrev-expansion-mark)
   (%overwrite-mode :initform nil :accessor overwrite-mode))
  (:metaclass modual-class)
  (:default-initargs :use-editor-commands t)
  (:documentation "The \"default\" Drei view class. It displays a
textual representation of the buffer, possibly with syntax
highlighting, and maintains point and mark marks into the buffer,
in order to permit useful editing commands."))

(defgeneric invalidate-strokes (view syntax)
  (:documentation "Called just before redisplay of the
`textual-drei-syntax-view' `view' in order to give `syntax',
which is the syntax of `view', a chance to mark part of the
display as invalid due to do something not caused by buffer
modification (for example, parenthesis matching). This function
should return a list of pairs of buffer offsets, each pair
delimiting a buffer region that should be redrawn.")
  (:method ((view textual-drei-syntax-view) (syntax syntax))
    nil))

(defun invalidate-as-appropriate (view invalid-regions)
  "Invalidate strokes of `view' overlapping regions in
`invalid-regions'. `Invalid-regions' is a list of conses of
buffer offsets delimiting regions."
  (loop with top-offset = (offset (top view))
        with bot-offset = (offset (bot view))
        for (start . end) in invalid-regions
        do (as-region (start end)
             (when (overlaps start end top-offset bot-offset)
               (invalidate-strokes-in-region view start end
                                             :modified t :to-line-end t)))))

(defmethod display-drei-view-contents :around (stream (view textual-drei-syntax-view))
  (let ((invalid-regions (invalidate-strokes view (syntax view))))
    (invalidate-as-appropriate view invalid-regions)
    (call-next-method)
    ;; We do not expect whatever ephemeral state that caused
    ;; invalidation to stick around until the next redisplay, so
    ;; whatever it imposed on us, mark as dirty immediately.
    (invalidate-as-appropriate view invalid-regions)))

(defmethod create-view-cursors nconc ((output-stream extended-output-stream)
                                      (view textual-drei-syntax-view))
  (unless (no-cursors view)
    (list (make-instance 'point-cursor :view view :output-stream output-stream)
          (make-instance 'mark-cursor :view view :output-stream output-stream))))

(defmethod view-command-tables append ((view textual-drei-syntax-view))
  (syntax-command-tables (syntax view)))

(defmethod use-editor-commands-p ((view textual-drei-syntax-view))
  t)
