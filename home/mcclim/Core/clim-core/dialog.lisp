;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2003 by Tim Moore (moore@bricoworks.com)
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

#| Random notes:

An accepting-values stream diverts the calls to accept into calling
accept-present-default, as described in the spec.  The output record
produced by accept-present-default, as well as the current value of
that query, arguments that were passed to accept, etc. are stored in a
query object. The stream stores all the query objects for this
invocation of accepting-values. The record created and returned by
accept-present-default must be a subclass of updating-output-record.

After the initial output records are drawn, invoke-accepting-values
blocks accepting commands. The state of the dialog state machine is changed
via these commands. The commands currently are:

COM-SELECT-QUERY query-id -- calls the method select-query with the
corresponding query object and output record object. When select-query returns
the "next" field, if any, is selected so the user can move from field to field
easily.

COM-CHANGE-QUERY query-id value -- This command is used to directly change the
value of a query field that does not need to be selected first for input. For
example, a user would click directly on a radio button without selecting the
gadget first.

COM-DESELECT-QUERY -- deselects the currently selected query.

COM-QUERY-EXIT -- Exits accepting-values

COM-QUERY-ABORT -- Aborts accepting-values

COM-DO-COMMAND-BUTTON -- Execute the body of a command-button after a push

These commands are generated in two ways. For query fields that are entirely
based on CLIM drawing commands and presentations, these are emitted by
presentation translators. There is a presentation type selectable-query that
throws com-select-query for the :select gesture. Fields that are based on
gadgets have to throw presentations from their callbacks. This can be done
using  the method on p. 305 of the Franz CLIM user guide, or by using  the
McCLIM function throw-object-ptype.

After a command is executed the body of accepting-values is rerun, calling
accept-present-default again to update the fields' graphic appearance. [This
may be calling these methods too often and may change in the future]. The
values returned by the user's calls to accept come from the query objects.


If a query field is selectable than it should implement the method
select-query:

SELECT-QUERY stream query record -- Make a query field active and do any
input. This should change the query object and setf (changedp query). This
method might be interrupted at any time if the user selects another field.

|#

(in-package :clim-internals)

(defclass query ()
  ((query-identifier :accessor query-identifier :initarg :query-identifier)
   (ptype :accessor ptype :initarg :ptype)
   (view :accessor view :initarg :view)
   (default :accessor default :initarg :default :initform nil)
   (default-supplied-p :accessor default-supplied-p
     :initarg :default-supplied-p :initform nil)
   (value :accessor value :initarg :value :initform nil)
   (changedp :accessor changedp :initform nil)
   (record :accessor record :initarg :record)
   (activation-gestures :accessor activation-gestures
                        :initform *activation-gestures*
                        :documentation "Binding of *activation-gestures* on
entry to this accept")
   (delimeter-gestures :accessor delimiter-gestures
                       :initform *delimiter-gestures*
                       :documentation "Binding of *delimeter-gestures* on entry
to this accept")
   (accept-arguments :accessor accept-arguments :initarg :accept-arguments)
   (accept-condition :accessor accept-condition :initarg :accept-condition
                     :initform nil
                     :documentation "Condition signalled, if any, during
accept of this query")))

(defgeneric select-query (stream query record)
  (:documentation "Does whatever is needed for input (e.g., calls accept) when
a query is selected for input. It is responsible for updating the
  query object when a new value is entered in the query field." ))

(defgeneric deselect-query (stream query record)
  (:documentation "Deselect a query field: turn the cursor off, turn off
highlighting, etc." ))

(defmethod select-query (stream query record)
  (declare (ignore stream query record))
  nil)

(defmethod deselect-query (stream query record)
  (declare (ignore stream query record))
  nil)

(defclass accepting-values-record (standard-updating-output-record)
  ()
  (:documentation "Record class for an entire dialog and its elements."))

(defgeneric editing-stream (record)
  (:documentation "Reader for getting the editing stream, if any, from an
  accepting-values record."))

(defmethod editing-stream ((record accepting-values-record))
  nil)

(defclass accepting-values-stream (standard-encapsulating-stream)
  ((queries :accessor queries :initform nil)
   (selected-query :accessor selected-query :initform nil)
   (align-prompts :accessor align-prompts :initarg :align-prompts
                  :initform nil)
   (last-pass :accessor last-pass :initform nil
              :documentation "Flag that indicates the last pass through the
  body of ACCEPTING-VALUES, after the user has chosen to exit. This controls
  when conditions will be signalled from calls to ACCEPT.")))

(defmethod stream-default-view ((stream accepting-values-stream))
  +textual-dialog-view+)

(define-condition av-exit (condition)
  ())

;;; The accepting-values state machine is controlled by commands. Each
;;; action (e.g., "select a text field") terminates

(define-command-table accept-values)    ; :inherit-from nil???

(defvar *default-command* '(accepting-values-default-command))

;;; The fields of the query have presentation type query.  Fields that
;;; are "selectable", like the default text editor field, have type
;;; selectable-query.  The presentation object is the query
;;; identifier.

(define-presentation-type query () :inherit-from t)

(define-presentation-type selectable-query () :inherit-from 'query)

(define-presentation-type exit-button () :inherit-from t)

(define-presentation-type abort-button () :inherit-from t)

(defvar *accepting-values-stream* nil)

(defmacro with-stream-in-own-window ((&optional (stream '*query-io*)
                                                &rest window-args
                                                &key
                                                label foreground background
                                                height width)
                                                  (&rest further-streams)
                                     &rest body)
  (declare (ignorable label foreground background height width))
  `(let* ((,stream (open-window-stream :input-buffer
                                       (climi::frame-event-queue
                                        *application-frame*)
                                       ,@window-args))
          ,@(mapcar (lambda (a-stream)
                      (list a-stream stream))
                    further-streams))
     (unwind-protect
          (progn
            ,@body)
       (close ,stream))))

(defmacro accepting-values
    ((&optional (stream t)
                &rest args
                &key own-window exit-boxes initially-select-query-identifier
                modify-initial-query resynchronize-every-pass resize-frame
                align-prompts label scroll-bars select-first-query
                x-position y-position width height command-table frame-class
                (foreground nil foregroundp) (background nil backgroundp)
                (text-style nil text-style-p))
     &body body)
  (declare (ignorable exit-boxes initially-select-query-identifier
                      modify-initial-query resynchronize-every-pass resize-frame
                      align-prompts scroll-bars select-first-query
                      x-position y-position width height command-table frame-class
                      text-style))
  (setq stream (stream-designator-symbol stream '*standard-input*))
  (with-gensyms (accepting-values-continuation)
    (with-keywords-removed (args (:foreground :background :text-style))
      (let* ((with-text-style-body
                 (if text-style-p
                     `((with-drawing-options (,stream :text-style ,text-style)
                         ,@body))
                     body))
             (return-form
              `(flet ((,accepting-values-continuation (,stream)
                        ,@with-text-style-body))
                 (invoke-accepting-values ,stream
                                          #',accepting-values-continuation
                                          ,@args)))
             (true-form `(with-stream-in-own-window
                             (,stream
                              :label ,label
                              :height ,height
                              :width ,width
                              ,@(and foregroundp `(:foreground ,foreground))
                              ,@(and backgroundp `(:background ,background)))
                           (*standard-input* *standard-output*)
                           ,return-form)))
        ;; To avoid unreachable-code warnings, if `own-window' is a
        ;; boolean constant, don't generate the `if' form.
        (cond ((eq own-window t) true-form)
              ((eq own-window nil) return-form)
              (t `(if ,own-window
                      ,true-form
                      ,return-form)))))))

(defun invoke-accepting-values
    (stream body
     &key own-window exit-boxes
     (initially-select-query-identifier nil initially-select-p)
     select-first-query
     modify-initial-query resynchronize-every-pass resize-frame
     align-prompts label scroll-bars
     x-position y-position width height
     (command-table 'accept-values)
     (frame-class 'accept-values))
  (declare (ignore own-window exit-boxes modify-initial-query
    resize-frame scroll-bars x-position y-position width height frame-class))
  (when (and align-prompts ;; t means the same as :right
             (not (eq align-prompts :left)))
    (setf align-prompts :right))
  (multiple-value-bind (cx cy) (stream-cursor-position stream)
    (let* ((return-values nil)
           (*accepting-values-stream*
            (make-instance 'accepting-values-stream
                           :stream stream
                           :align-prompts align-prompts))
           (arecord (updating-output (stream :record-type 'accepting-values-record)
                      (when label
                        (format stream label)
                        (terpri stream))
                      (if align-prompts
                          (formatting-table (stream)
                            #1=(setf return-values
                                     (multiple-value-list
                                      (funcall body *accepting-values-stream*))))
                          #1#)
                      (unless (queries *accepting-values-stream*)
                        (cerror "Exit returning body values."
                                "~s must contain at least one call to ~s."
                                'accepting-values 'accept)
                        (return-from invoke-accepting-values return-values))
                      (display-exit-boxes *application-frame*
                                          stream
                                          (stream-default-view
                                           *accepting-values-stream*))))
           (first-time t)
           (current-command (if initially-select-p
                                `(com-select-query
                                  ,initially-select-query-identifier)
                                `(com-select-query
                                  ,(query-identifier
                                    (first
                                     (queries *accepting-values-stream*))))))
           (*accelerator-gestures* (compute-inherited-keystrokes command-table)))
      (letf (((frame-command-table *application-frame*)
              (find-command-table command-table)))
        (unwind-protect
             (handler-case
                 (loop
                    (if first-time
                        (setq first-time nil)
                        (when resynchronize-every-pass
                          (redisplay arecord stream)))
                    (with-input-context
                        ('(command :command-table accept-values))
                      (object)
                      (progn
                        (when (and select-first-query
                                   (not initially-select-p))
                          (setf current-command
                                `(com-select-query
                                  ,(query-identifier
                                    (first
                                     (queries *accepting-values-stream*))))
                                select-first-query nil))
                        (handler-case
                            (progn
                              (apply (command-name current-command)
                                     (command-arguments current-command))
                              ;; If current command returns without throwing a
                              ;; command, go back to the default command
                              (setq current-command *default-command*))
                          (accelerator-gesture (c)
                            (let ((command (lookup-keystroke-command-item
                                            (accelerator-gesture-event c) command-table)))
                              (if (listp command)
                                  (setq current-command
                                        (if (clim:partial-command-p command)
                                            (funcall clim:*partial-command-parser*
                                                     command-table stream command
                                                     (position clim:*unsupplied-argument-marker* command))
                                            command))
                                  ;; may be it is a gesture of the frame's command-table
                                  (signal c))))))
                      (t (setq current-command object)))
                    (redisplay arecord stream))
               (av-exit ()
                 (finalize-query-records *accepting-values-stream*)
                 (setf (last-pass *accepting-values-stream*) t)
                 (redisplay arecord stream)))
          (dolist (query (queries *accepting-values-stream*))
            (finalize (editing-stream (record query)) nil))
          (erase-output-record arecord stream)
          (setf (stream-cursor-position stream)
                (values cx cy))))
      (apply 'values return-values))))

(defgeneric display-exit-boxes (frame stream view))

(defmethod display-exit-boxes (frame stream (view textual-dialog-view))
  (declare (ignore frame))
  (updating-output (stream :unique-id 'buttons :cache-value t)
    (fresh-line stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream)
          (with-output-as-presentation (stream nil 'exit-button)
            (surrounding-output-with-border
                (stream :shape :rounded :radius 6
                        :background +gray80+ :highlight-background +gray90+)
              (format stream "OK"))))
        (formatting-cell (stream)
          (with-output-as-presentation
              (stream nil 'abort-button)
            (surrounding-output-with-border
             (stream :shape :rounded :radius 6
                     :background +gray80+ :highlight-background +gray90+)
             (format stream "Cancel"))))))
    (terpri stream)))

(defmethod stream-accept ((stream accepting-values-stream) type
                          &rest rest-args
                          &key
                          (view (stream-default-view stream))
                          (default nil default-supplied-p)
                          default-type
                          provide-default
                          insert-default
                          replace-input
                          history
                          active-p
                          prompt
                          prompt-mode
                          display-default
                          (query-identifier prompt)
                          activation-gestures
                          additional-activation-gestures
                          delimiter-gestures
                          additional-delimiter-gestures)
  (declare (ignore default-type provide-default insert-default replace-input
                   history active-p prompt-mode display-default
                   activation-gestures additional-activation-gestures
                   delimiter-gestures additional-delimiter-gestures))
  (let ((query (find query-identifier (queries stream)
                     :key #'query-identifier :test #'equal))
        (align (align-prompts stream)))
    (unless query
      ;; If there's no default but empty input could return a sensible value,
      ;; use that as a default.
      (unless default-supplied-p
        (setq default
              (ignore-errors (accept-from-string type
                                                 ""
                                                 :view +textual-view+ ))))
      (setq query (make-instance 'query
                                 :query-identifier query-identifier
                                 :ptype type
                                 :view view
                                 :default default
                                 :default-supplied-p default-supplied-p
                                 :value default))
      (setf (queries stream) (nconc (queries stream) (list query)))
      (when default-supplied-p
        (setf (changedp query) t)))
    (setf (accept-arguments query) rest-args)
    ;; If the program changes the default, that becomes the value.
    (unless (equal default (default query))
      (setf (default query) default)
      (setf (value query) default))
    (flet ((do-prompt (stream)
             (apply #'prompt-for-accept stream type view rest-args))
           (do-accept-present-default (stream)
             (funcall-presentation-generic-function
              accept-present-default
              type (encapsulating-stream-stream stream) view
              (value query)
              default-supplied-p nil query-identifier)))
      (let ((query-record nil))
        (if align
            (formatting-row (stream)
              (formatting-cell (stream :align-x align :align-y :center)
                (do-prompt stream))
              (formatting-cell (stream)
                (setq query-record (do-accept-present-default stream))))
            (progn
              (do-prompt stream)
              (setq query-record (do-accept-present-default stream))))
        (setf (record query) query-record)
        (when (and (last-pass stream) (accept-condition query))
          (signal (accept-condition query)))
        (multiple-value-prog1
            (values (value query) (ptype query) (changedp query))
          (setf (default query) default)
          (setf (ptype query) type)
          #+nil (setf (changedp query) nil))))))


(defmethod prompt-for-accept ((stream accepting-values-stream)
                              type view
                              &rest args)
  (declare (ignore view))
  (apply #'prompt-for-accept-1 stream type :display-default nil args))

(define-command (com-query-exit :command-table accept-values
                                :keystroke (#\[ :control)
                                :name nil
                                :provide-output-destination-keyword nil)
    ()
  (signal 'av-exit))

(add-keystroke-to-command-table 'accept-values '(#\Return :meta) :command 'com-query-exit :errorp nil)

(define-command (com-query-abort :command-table accept-values
                                 :keystroke (#\] :control)
                                 :name nil
                                 :provide-output-destination-keyword nil)
    ()
  (and (find-restart 'abort)
       (invoke-restart 'abort)))

(add-keystroke-to-command-table 'accept-values '(#\g :control) :command 'com-query-abort :errorp nil)

(define-command (com-change-query :command-table accept-values
                                  :name nil
                                  :provide-output-destination-keyword nil)
    ((query-identifier t)
     (value t))
  (when *accepting-values-stream*
    (let ((query (find query-identifier (queries *accepting-values-stream*)
                       :key #'query-identifier :test #'equal)))
      (when query
        (setf (value query) value)
        (setf (changedp query) t)))))

(define-command (com-select-query :command-table accept-values
                                  :name nil
                                  :provide-output-destination-keyword nil)
    ((query-identifier t))
  (when *accepting-values-stream*
    (with-accessors ((selected-query selected-query))
        *accepting-values-stream*
      (let* ((query-list (member query-identifier
                                 (queries *accepting-values-stream*)
                                 :key #'query-identifier :test #'equal))
             (query (car query-list)))
        (when selected-query

          (unless (equal query-identifier (query-identifier selected-query))
            (deselect-query *accepting-values-stream*
                            selected-query
                            (record selected-query))))
        (when query
          (setf selected-query query)
          (select-query *accepting-values-stream* query (record query))
          (let ((command-ptype '(command :command-table accept-values)))
            (if (cdr query-list)
                (throw-object-ptype `(com-select-query ,(query-identifier
                                                         (cadr query-list)))
                                    command-ptype)
                (throw-object-ptype '(com-deselect-query) command-ptype))))))))

(define-command (com-deselect-query :command-table accept-values
                                    :name nil
                                    :provide-output-destination-keyword nil)
    ()
  (when *accepting-values-stream*
    (with-accessors ((selected-query selected-query))
        *accepting-values-stream*
      (when selected-query
        (deselect-query *accepting-values-stream*
                        selected-query
                        (record selected-query))
        (setf selected-query nil)))))

(define-command (com-next-query :command-table accept-values
                                :keystroke (#\n :meta)
                                :name nil
                                :provide-output-destination-keyword nil)
    ()
  (when *accepting-values-stream*
    (let ((queries (queries *accepting-values-stream*)))
      (with-accessors ((selected-query selected-query))
          *accepting-values-stream*
        (let ((query-pos (position selected-query queries)))
          (if query-pos
              (setq query-pos (1+ query-pos))
              (setq query-pos 0))
          (when (>= query-pos (length queries))
            (setq query-pos 0))
          (com-select-query (query-identifier (elt queries query-pos))))))))

(define-command (com-prev-query :command-table accept-values
                                :keystroke (#\p :meta)
                                :name nil
                                :provide-output-destination-keyword nil)
    ()
  (when *accepting-values-stream*
    (let ((queries (queries *accepting-values-stream*)))
      (with-accessors ((selected-query selected-query))
          *accepting-values-stream*
        (let ((query-pos (position selected-query queries)))
          (if query-pos
              (setq query-pos (1- query-pos))
              (setq query-pos (1- (length queries))))
          (when (< query-pos 0)
            (setq query-pos (1- (length queries))))
          (com-select-query (query-identifier (elt queries query-pos))))))))

(defclass av-text-record (accepting-values-record)
  ((editing-stream :accessor editing-stream)
   (snapshot :accessor snapshot :initarg :snapshot :initform nil
             :documentation "A copy of the stream buffer before accept
is called. Used to determine if any editing has been done by user")))

(defparameter *no-default-cache-value* (cons nil nil))

(define-default-presentation-method accept-present-default
    (type stream (view textual-dialog-view) default default-supplied-p
     present-p query-identifier)
  (declare (ignore present-p))
  (let* ((editing-stream nil)
         (record (updating-output (stream :unique-id query-identifier
                                          :cache-value (if default-supplied-p
                                                           default
                                                           *no-default-cache-value*)
                                          :record-type 'av-text-record)
                   (with-output-as-presentation
                       (stream query-identifier 'selectable-query
                               :single-box t)
                     (surrounding-output-with-border
                         (stream :shape :rounded
                                 :radius 3 :background clim:+background-ink+
                                 :foreground clim:+foreground-ink+
                                 :move-cursor t)
                       ;;; FIXME: In this instance we really want borders that
                       ;;; react to the growth of their children. This should
                       ;;; be straightforward unless there is some involvement
                       ;;; of incremental redisplay.
                       ;;; KLUDGE: Arbitrary min-width.
                       (setq editing-stream
                             (make-instance 'standard-input-editing-stream
                                            :stream stream
                                            :cursor-visibility nil
                                            :min-width (- (bounding-rectangle-max-x stream)
                                                          (stream-cursor-position stream)
                                                          100)))))
                   (when default-supplied-p
                     (input-editing-rescan-loop ;XXX probably not needed
                      editing-stream
                      (lambda (s)
                        (presentation-replace-input s default type view
                                                    :rescan t)))))))
    (when editing-stream
      (setf (editing-stream record) editing-stream))
    record))

(defun av-do-accept (query record interactive)
  (let* ((estream (editing-stream record))
         (ptype (ptype query))
         (view (view query))
         (default (default query))
         (default-supplied-p (default-supplied-p query))
         (accept-args (accept-arguments query))
         (*activation-gestures* (apply #'make-activation-gestures
                                       :existing-activation-gestures
                                       (activation-gestures query)
                                       accept-args))
         (*delimiter-gestures* (apply #'make-delimiter-gestures
                                      :existing-delimiter-args
                                      (delimiter-gestures query)
                                      accept-args)))
    ;; If there was an error on a previous pass, set the insertion pointer to
    ;; 0 so the user has a chance to edit the field without causing another
    ;; error. Otherwise the insertion pointer should already be at the end of
    ;; the input (because it was activated); perhaps we should set it anyway.
    (when (accept-condition query)
      (setf (stream-insertion-pointer estream) 0))
    (reset-scan-pointer estream)
    (setf (accept-condition query) nil)
    ;; If a condition is thrown, then accept should return the old value and
    ;; ptype.
    (block accept-condition-handler
      (setf (changedp query) nil)
      (setf (values (value query) (ptype query))
            (input-editing-rescan-loop
             estream
             #'(lambda (s)
                 (handler-bind
                     ((error
                       #'(lambda (c)
                           (format *trace-output*
                                   "accepting-values accept condition: ~A~%"
                                   c)
                           (if interactive
                               (progn
                                 (beep)
                                 (setf (stream-insertion-pointer estream)
                                       (max 0 (1- (stream-scan-pointer estream))))
                                 (immediate-rescan estream)
                                 (format *trace-output* "Ack!~%"))
                               (progn
                                 (setf (accept-condition query) c)
                                 (return-from accept-condition-handler
                                   c))))))
                   (if default-supplied-p
                       (accept ptype :stream s
                               :view view :prompt nil :default default)
                       (accept ptype :stream s :view view :prompt nil))))))
      (setf (changedp query) t))))




;;; The desired
(defmethod select-query (stream query (record av-text-record))
  (declare (ignore stream))
  (let ((estream (editing-stream record))
        (ptype (ptype query))
        (view (view query)))
    (declare (ignore ptype view))	;for now
    (with-accessors ((stream-input-buffer stream-input-buffer))
        estream
      (setf (cursor-visibility estream) t)
      (setf (snapshot record) (copy-seq stream-input-buffer))
      (av-do-accept query record t))))


;;; If the query has not been changed (i.e., ACCEPT didn't return) and there is
;;; no error, act as if the user activated the query.
(defmethod deselect-query (stream query (record av-text-record))
  (let ((estream (editing-stream record)))
    (setf (cursor-visibility estream) nil)
    (when (not (or (changedp query) (accept-condition query)))
      (finalize-query-record query record))))


(defgeneric finalize-query-record (query record)
  (:documentation "Do any cleanup on a query before the accepting-values body
is run for the last time"))

(defmethod finalize-query-record (query record)
  nil)

;;; If the user edits a text field, selects another text field and
;;; then exits from accepting-values without activating the first
;;; field, the values returned would be some previous value or default
;;; for the field, not what's on the screen.  That would be completely
;;; bogus.  So, if a field has been edited but not activated, activate
;;; it now.  Unfortunately that's a bit hairy.

(defmethod finalize-query-record (query (record av-text-record))
  (let ((estream (editing-stream record)))
    (when (and (not (changedp query))
               (snapshot record)
               (not (equalp (snapshot record)
                            (stream-input-buffer estream))))
      (let* ((activation-gestures (apply #'make-activation-gestures
                                         :existing-activation-gestures
                                         (activation-gestures query)
                                         (accept-arguments query)))
             (gesture (car activation-gestures)))
        (when gesture
          (let ((c (character-gesture-name gesture)))
            (activate-stream estream c)
            (reset-scan-pointer estream)
            (av-do-accept query record nil)))))))

(defun finalize-query-records (av-stream)
  (loop for query in (queries av-stream)
        do (finalize-query-record query (record query))))


(define-presentation-to-command-translator com-select-field
    (selectable-query com-select-query accept-values
     :gesture :select
     :documentation "Select field for input"
     :pointer-documentation "Select field for input"
     :echo nil
     :tester ((object)
              (let ((selected (selected-query *accepting-values-stream*)))
                (or (null selected)
                    (not (eq (query-identifier selected) object))))))
  (object)
  `(,object))

(define-presentation-to-command-translator com-exit-button
    (exit-button com-query-exit accept-values
     :gesture :select
     :documentation "Exit dialog"
     :pointer-documentation "Exit dialog"
     :echo nil)
    (object)
  ())

(define-presentation-to-command-translator com-abort-button
    (abort-button com-query-abort accept-values
     :gesture :select
     :documentation "Abort dialog"
     :pointer-documentation "Abort dialog"
     :echo nil)
    (object)
  ())

(defun accepting-values-default-command ()
  (loop
   (read-gesture :stream *accepting-values-stream*)))


;;;; notify-user

;;; See http://openmap.bbn.com/hypermail/clim/0028.html for example usage.

;;; TODO:
;;;   - associated-window argument?
;;;   - What is the correct return value from notify-user? We currently return
;;;     the name of the action given in the :exit-boxes argument.
;;;   - Invoke abort restart? Not necessary as it is with accepting-values,
;;;     but probably what "Classic CLIM" does.
;;;   - What are the default exit boxes? Just "Okay"? Okay and cancel?
;;;   - Reimplement using accepting-values, if accepting-values is ever
;;;     improved to produce comparable dialogs.
;;;   - Should the user really be able to close the window from the WM?

(defmethod notify-user (frame message &rest args)
  (apply #'frame-manager-notify-user
         (if frame (frame-manager frame) (find-frame-manager))
         message
         :frame frame
         args))

(define-application-frame generic-notify-user-frame ()
  ((message-string :initarg :message-string)
   (exit-boxes :initarg :exit-boxes)
   (title :initarg :title)
   (style :initarg :style)
   (text-style :initarg :text-style)
   (return-value :initarg nil :initform :abort))
  (:pane (generate-notify-user-dialog *application-frame*)))

(defun generate-notify-user-dialog (frame)
  (with-slots (message-string exit-boxes text-style) frame
  (vertically ()
    (spacing (:thickness 6)
      (make-pane 'label-pane :label (or message-string "I'm speechless.") :text-style text-style))
    (spacing (:thickness 4)
      (make-pane 'hbox-pane :contents (cons '+fill+ (generate-exit-box-buttons exit-boxes)))))))

(defun generate-exit-box-buttons (specs)
  (mapcar
   (lambda (spec)
     (destructuring-bind (action string &rest args) spec
       (spacing (:thickness 2)
         (apply #'make-pane
                'push-button
                :label string
                :text-style (make-text-style :sans-serif :roman :small) ; XXX
                :activate-callback
                (lambda (gadget)
                  (declare (ignore gadget))
                  ;; This is fboundp business is weird, and only implied by a
                  ;; random message on the old CLIM list. Does the user function
                  ;; take arguments?
                  (when (or (typep action 'function) (fboundp action))
                    (funcall action))
                  (setf (slot-value *application-frame* 'return-value) action)
                  ;; This doesn't work:
                  #+NIL
                  (when (eql action :abort)
                    (and (find-restart 'abort)
                         (invoke-restart 'abort)))
                  (frame-exit *application-frame*))
                args))))
   specs))


(defmethod frame-manager-notify-user
    (frame-manager message-string &key frame associated-window
                   (title "")
                   documentation
                   (exit-boxes '((:exit "OK")))
                   ; The 'name' arg is in the spec but absent from the Lispworks
                   ; manual, and I can't imagine what it would do differently
                   ; than 'title'.
                   name
                   style
                   (text-style (make-text-style :sans-serif :roman :small)))
  (declare (ignore associated-window documentation))
  ;; Keywords from notify-user:
  ;; associated-window title documentation exit-boxes name style text-style
  (let ((frame (make-application-frame 'generic-notify-user-frame
                                       :calling-frame frame
                                       :pretty-name title
                                       :message-string message-string
                                       :frame-manager frame-manager
                                       :exit-boxes exit-boxes
                                       :title (or name title)
                                       :style style
                                       :text-style text-style)))
    (run-frame-top-level frame)
    (slot-value frame 'return-value)))


;;; An accept-values button sort of behaves like an accepting-values query with
;;; no value.
(defmacro accept-values-command-button
    ((&optional (stream t) &rest key-args
                &key documentation query-identifier cache-value cache-test
                (view '+push-button-view+) resynchronize &allow-other-keys)
                  prompt
     &body body)
  (declare (ignorable documentation query-identifier cache-value cache-test
                      resynchronize))
  (setq stream (stream-designator-symbol stream '*standard-input*))
  (with-gensyms (command-button-continuation prompt-function)
    (with-keywords-removed (key-args (:view))
      (let* ((prompt-arg (if (stringp prompt)
                             prompt
                             `#',prompt-function))
             (button-body
              `(flet ((,command-button-continuation ()
                        ,@body))
                 (invoke-accept-values-command-button
                  ,stream
                  #',command-button-continuation
                  ,view
                  ,prompt-arg
                  ,@key-args))))
        (if (stringp prompt)
            button-body
            `(flet ((,prompt-function (,stream) ,prompt))
               ,button-body))))))

(defclass command-button-state ()
  ((continuation :reader continuation :initarg :continuation)
   (query-identifier :reader query-identifier :initarg :query-identifier)
   (documentation :reader button-documentation :initarg :documentation)
   (resynchronize :reader resynchronize :initarg :resynchronize)))

(defgeneric invoke-accept-values-command-button
    (stream continuation view prompt
     &key documentation query-identifier cache-value cache-test resynchronize
       &allow-other-keys))

(defmethod invoke-accept-values-command-button
    ((stream accepting-values-stream) continuation view prompt
     &key (documentation prompt)
       (query-identifier `(:command-button ,prompt)) (cache-value t)
       (cache-test #'eql) resynchronize)
  (let ((stream (encapsulating-stream-stream stream)))
      (updating-output (stream
                        :cache-value cache-value
                        :cache-test cache-test
                        :unique-id query-identifier
                        :id-test #'equal
                        :record-type 'accepting-values-record)
        (with-output-as-presentation
            (stream
             (make-instance 'command-button-state
                            :continuation continuation
                            :query-identifier query-identifier
                            :documentation documentation
                            :resynchronize resynchronize)
             'command-button-state)
          (surrounding-output-with-border
              (stream :shape :rounded :radius 6
                      :background +gray80+ :highlight-background +gray90+)
            (if (functionp prompt)
                (funcall prompt stream)
                (princ prompt stream)))))))

(define-command (com-do-command-button :command-table accept-values
                                       :name nil
                                       :provide-output-destination-keyword nil)
    ((query-identifier t)
     (continuation nil))
  (funcall continuation)
  (when *accepting-values-stream*
    (let ((query (find query-identifier (queries *accepting-values-stream*)
                       :key #'query-identifier :test #'equal)))
      (when query
        (setf (changedp query) t)))))

(define-presentation-to-command-translator com-command-button
    (command-button-state com-do-command-button accept-values
     :gesture :select
     :documentation ((object stream)
                     (let ((doc (button-documentation object)))
                       (if (functionp doc)
                           (funcall doc stream)
                           (princ doc stream))))
     :echo nil
     :menu nil)
    (object)
  (list (query-identifier object) (continuation object)))
