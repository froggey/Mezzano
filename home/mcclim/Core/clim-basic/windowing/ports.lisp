;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)

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

;;; Server path and global port registry

(defvar *default-server-path* nil)

;;; - CLX is the de-facto reference backend. We have few flavours of
;;;   it where the one using Lisp TTF renderer implementation and
;;;   Xrender extensions is default.
;;;
;;; - Null are in this list mostly to document its existence, and is
;;;   not currently a complete backend we would want to make a
;;;   default.  Put it after CLX, so that it won't actually be
;;;   reached.
(defvar *server-path-search-order*
  '(:mezzano
    #.(cond ((member :mcclim-ffi-freetype *features*) :clx-ff)
            ((member :mcclim-clx-fb       *features*) :clx-fb)
            ((member :mcclim-ugly         *features*) :clx)
            (t :clx-ttf))
    :null))

(defun find-default-server-path ()
  (loop for port in *server-path-search-order*
        if (get port :port-type)
           do (return-from find-default-server-path (list port))
        finally (error "No CLIM backends have been loaded!")))

(defvar *all-ports* nil)

(defun find-port (&key (server-path *default-server-path*))
  (if (null server-path)
      (setq server-path (find-default-server-path)))
  (if (atom server-path)
      (setq server-path (list server-path)))
  (setq server-path
        (funcall (get (first server-path) :server-path-parser) server-path))
  (loop for port in *all-ports*
        if (equal server-path (port-server-path port))
        do (return port)
        finally (let ((port-type (get (first server-path) :port-type))
                      port)
                  (if (null port-type)
                      (error "Don't know how to make a port of type ~S"
                             server-path))
                  (setq port
                        (funcall 'make-instance port-type
                                 :server-path server-path))
                  (push port *all-ports*)
                  (return port))))

(defmacro with-port ((port-var server &rest args &key &allow-other-keys)
                     &body body)
  `(invoke-with-port (lambda (,port-var) ,@body) ,server ,@args))

(defun invoke-with-port (continuation server &rest args &key &allow-other-keys)
  (let* ((path (list* server args))
         (port (find-port :server-path path)))
    (unwind-protect
         (funcall continuation port)
      (destroy-port port))))

;;; Basic port

(defclass basic-port (port)
  ((server-path :initform nil
		:initarg :server-path
		:reader port-server-path)
   (properties :initform nil
	       :initarg :properties)
   (grafts :initform nil
	   :accessor port-grafts)
   (frame-managers :initform nil
		   :reader frame-managers)
   (sheet->mirror :initform (make-hash-table :test #'eq))
   (mirror->sheet :initform (make-hash-table :test #'eq))
   (pixmap->mirror :initform (make-hash-table :test #'eq))
   (mirror->pixmap :initform (make-hash-table :test #'eq))
   (event-process
    :initform nil
    :initarg  :event-process
    :accessor port-event-process
    :documentation "In a multiprocessing environment, the particular process
                    reponsible for calling PROCESS-NEXT-EVENT in a loop.")
   (lock
    :initform (make-recursive-lock "port lock")
    :accessor port-lock)
   (text-style-mappings :initform (make-hash-table :test #'eq)
                        :reader port-text-style-mappings)
   (pointer-sheet :initform nil :accessor port-pointer-sheet
		  :documentation "The sheet the pointer is over, if any")
   ;; The difference between grabbed-sheet and pressed-sheet is that
   ;; the former takes all pointer events while pressed-sheet receives
   ;; replicated pointer motion events. -- jd 2019-08-21
   (grabbed-sheet :initform nil :accessor port-grabbed-sheet
		  :documentation "The sheet the pointer is grabbing, if any")
   (pressed-sheet :initform nil :accessor port-pressed-sheet
		  :documentation "The sheet the pointer is pressed on, if any")))

(defmethod port-keyboard-input-focus (port)
  (when (null *application-frame*)
    (error "~S called with null ~S"
           'port-keyboard-input-focus '*application-frame*))
  (port-frame-keyboard-input-focus port *application-frame*))

(defmethod (setf port-keyboard-input-focus) (focus port)
  (when (null *application-frame*)
    (error "~S called with null ~S"
           '(setf port-keyboard-input-focus) '*application-frame*))
  ;; XXX: pane frame is not defined for all streams (for instance not for
  ;; CLIM:STANDARD-EXTENDED-INPUT-STREAM), so this sanity check would lead to
  ;; error on that.
  ;; XXX: also should we allow reading objects from foreign application frames?
  ;; This was the case on Genera and is requested by users from time to time...
  #+ (or)
  (unless (eq *application-frame* (pane-frame focus))
    (error "frame mismatch in ~S" '(setf port-keyboard-input-focus)))
  (setf (port-frame-keyboard-input-focus port *application-frame*) focus))

(defgeneric port-frame-keyboard-input-focus (port frame))
(defgeneric (setf port-frame-keyboard-input-focus) (focus port frame))

(defmethod destroy-port :before ((port basic-port))
  (when (and *multiprocessing-p* (port-event-process port))
    (destroy-process (port-event-process port))
    (setf (port-event-process port) nil)))


;;; Mirrors

(defmethod port-lookup-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (gethash sheet (slot-value port 'sheet->mirror)))

(defmethod port-lookup-mirror ((port basic-port) (sheet basic-sheet))
  (port-lookup-mirror port (sheet-mirrored-ancestor sheet)))

(defgeneric port-lookup-sheet (port mirror))

(defmethod port-lookup-sheet ((port basic-port) mirror)
  (gethash mirror (slot-value port 'mirror->sheet)))

(defmethod port-register-mirror
    ((port basic-port) (sheet mirrored-sheet-mixin) mirror)
  (setf (gethash sheet (slot-value port 'sheet->mirror)) mirror)
  (setf (gethash mirror (slot-value port 'mirror->sheet)) sheet)
  nil)

(defgeneric port-unregister-mirror (port sheet mirror))

(defmethod port-unregister-mirror
    ((port basic-port) (sheet mirrored-sheet-mixin) mirror)
  (remhash sheet (slot-value port 'sheet->mirror))
  (remhash mirror (slot-value port 'mirror->sheet))
  nil)

(defmethod realize-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to realize the mirror of a generic mirrored-sheet"))

(defmethod destroy-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to destroy the mirror of a generic mirrored-sheet"))

(defmethod mirror-transformation ((port basic-port) mirror)
  (declare (ignore mirror))
  (error "MIRROR-TRANSFORMATION is not implemented for generic ports"))

(defmethod port-properties ((port basic-port) indicator)
  (with-slots (properties) port
    (getf properties indicator)))

(defmethod (setf port-properties) (value (port basic-port) indicator)
  (with-slots (properties) port
    (setf (getf properties indicator) value)))

;;; This function determines the sheet to which the pointer event
;;; should be delivered. The right thing is not obvious:
;;;
;;; - we may assume that event-sheet is set correctly by port
;;; - we may find the innermost sheet's child and deliver to it
;;; - we may deliver event to sheet's graft and let it dispatch
;;;
;;; Third option would require a default handle-event method to call
;;; handle-event on its child under the cursor. For now we implement
;;; the second option with the innermost child. In general case we
;;; need z-ordering for both strategies. -- jd 2019-08-21
(defun compute-pointer-event-sheet (event &aux (sheet (event-sheet event)))
  ;; Traverse all descendants of EVENT's sheet which contain EVENT's
  ;; pointer position. The innermost such child that does not have a
  ;; direct mirror should be the new pointer event sheet (we do not
  ;; consider children with a direct mirror since for those EVENT's
  ;; sheet would have been the child in question).
  (labels ((rec (sheet x y)
             (if-let ((child (child-containing-position sheet x y))) ; TODO this only considers enabled children
               (if (sheet-direct-mirror child)
                   sheet
                   (multiple-value-call #'rec
                     child (untransform-position
                            (sheet-transformation child) x y)))
               sheet)))
    (get-pointer-position (sheet event)
      (rec sheet x y))))

(defun common-ancestor (sheet-a sheet-b)
  (flet ((candidatep (sheet)
           (not (or (null sheet) (graftp sheet)))))
    (loop (cond ((eq sheet-a sheet-b)
                 (return sheet-a))
                ((or (not (candidatep sheet-a))
                     (not (candidatep sheet-b)))
                 (return nil))
                ((sheet-ancestor-p sheet-b sheet-a)
                 (return sheet-a))
                (t
                 (setf sheet-a (sheet-parent sheet-a)))))))

;;; Function is responsible for making a copy of an immutable event
;;; and adjusting its coordinates to be in the target-sheet
;;; coordinates. Optionally it may change event's class.
(defun dispatch-event-copy (target-sheet event &optional new-class
                            &aux (sheet (event-sheet event)))
  (if (and (eql target-sheet sheet)
           (or (null new-class)
               (eql new-class (class-of event))))
      (dispatch-event sheet event)
      (let* ((event-class (if (null new-class)
                              (class-of event)
                              (find-class new-class)))
             (new-event (shallow-copy-object event event-class)))
        (when (typep new-event 'pointer-event)
          (get-pointer-position (target-sheet new-event)
            (setf (slot-value new-event 'x) x
                  (slot-value new-event 'y) y
                  (slot-value new-event 'sheet) target-sheet)))
        (dispatch-event target-sheet new-event))))

;;; Synthesizing and dispatching boundary events
;;;
;;; PORT only generates boundary-events for mirrored sheets. For
;;; sheets without a mirror, we must synthesize boundary-events.
;;;
;;; This function works in two phases:
;;;
;;; 1) Retrieve the current pointer sheet of PORT and compute a new
;;;    pointer sheet for PORT. This has to be done differently,
;;;    depending on whether EVENT is an enter, exit or any other kind
;;;    of event.
;;;
;;; 2) Based on the old and new pointer sheets, synthesize and
;;;    dispatch boundary events and potentially dispatch EVENT.
;;;
;;; In the first phase, if EVENT is not an exit event, the new port
;;; pointer sheet is the innermost unmirrored child containing the
;;; pointer position of EVENT. If EVENT is an exit event, the new
;;; pointer sheet is the parent of the sheet of EVENT or NIL if the
;;; parent is a graft.
;;;
;;; In the second phase, exit and enter events are synthesized and
;;; dispatched based on the old and new pointer sheet of PORT (both
;;; can be NIL). If EVENT is an enter or exit event, it is dispatched
;;; as part of this process.
(defun synthesize-boundary-events (port event)
  (let* ((event-sheet (event-sheet event))
         (old-pointer-sheet (port-pointer-sheet port))
         (new-pointer-sheet old-pointer-sheet)
         (dispatch-event-p nil))
    ;; First phase: compute new pointer sheet for PORT.
    (flet ((update-pointer-sheet (new-sheet)
             (unless (eql old-pointer-sheet new-sheet)
               (setf (port-pointer-sheet port) new-sheet
                     new-pointer-sheet new-sheet))))
      (typecase event
        ;; Ignore grab-enter and ungrab-leave boundary events.
        ((or pointer-grab-enter-event pointer-ungrab-leave-event))
        ;; For enter events, update PORT's pointer sheet to the
        ;; innermost child of EVENT's sheet containing EVENT's pointer
        ;; position. Mark EVENT to be dispatched together with
        ;; synthesize events.
        (pointer-enter-event
         ;; Only perform the update and dispatch EVENT if either
         ;; 1) EVENT is not a POINTER-UNGRAB-ENTER-EVENT
         ;; 2) EVENT is a POINTER-UNGRAB-ENTER-EVENT and its sheet is
         ;;    not an ancestor of the old pointer sheet (i.e. ensure
         ;;    that processing EVENT does not re-enter any sheets)
         (when (or (not (typep event 'pointer-ungrab-enter-event))
                   (not (and old-pointer-sheet
                             (sheet-ancestor-p old-pointer-sheet event-sheet))))
           (update-pointer-sheet (compute-pointer-event-sheet event))
           (setf dispatch-event-p t)))
        ;; For exit events, update PORT's pointer sheet to the parent
        ;; of EVENT's sheet, or NIL if that parent is a graft. Mark
        ;; EVENT to be dispatched together with synthesize events.
        (pointer-exit-event
         ;; Only perform the update and dispatch EVENT if either
         ;; 1) EVENT is not a POINTER-GRAB-ENTER-EVENT
         ;; 2) EVENT is a POINTER-GRAB-ENTER-EVENT and the old pointer
         ;;    sheet is an ancestor of its sheet (i.e. ensure that
         ;;    processing EVENT only exits sheets that are currently
         ;;    on the (imaginary) stack of entered sheets).
         (when (or (not (typep event 'pointer-grab-leave-event))
                   (and old-pointer-sheet
                        (sheet-ancestor-p event-sheet old-pointer-sheet)))
           (when (and event-sheet old-pointer-sheet)
             (let ((parent (sheet-parent event-sheet)))
               (update-pointer-sheet (if (graftp parent) nil parent))))
           (setf dispatch-event-p t)))
        ;; For non-boundary events, update PORT's pointer sheet to the
        ;; innermost child of EVENT's sheet containing EVENT's pointer
        ;; position (like for enter events). However, do not dispatch
        ;; EVENT (will be done elsewhere).
        (otherwise
         ;; Only update the pointer sheet if the current pointer sheet
         ;; is non-NIL since we can get such events with the current
         ;; pointer sheet being NIL and the pointer position being
         ;; outside of the top-level sheet's region due to grabbing.
         (when old-pointer-sheet
           (update-pointer-sheet (compute-pointer-event-sheet event))))))

    ;; Second phase: synthesize and dispatch boundary events.
    (flet ((should-synthesize-for-sheet-p (sheet)
             (or (and dispatch-event-p
                      (eq sheet event-sheet))
                 (not (sheet-direct-mirror sheet))))
           (synthesize-enter (sheet)
             (dispatch-event-copy sheet event 'pointer-enter-event))
           (synthesize-exit (sheet)
             (dispatch-event-copy sheet event 'pointer-exit-event)))
      (let ((common-ancestor (when (and old-pointer-sheet new-pointer-sheet)
                               (common-ancestor old-pointer-sheet
                                                new-pointer-sheet))))
        ;; Distribute exit events for OLD-POINTER-SHEET and its
        ;; non-direct-mirrored ancestors (innermost first).
        (do ((sheet old-pointer-sheet (sheet-parent sheet)))
            ((or (eq sheet common-ancestor)
                 (graftp sheet)))
          (when (should-synthesize-for-sheet-p sheet)
            (synthesize-exit sheet)))
        ;; Distribute enter events for NEW-POINTER-SHEET and its
        ;; non-direct-mirrored ancestors (innermost last).
        (do ((sheet new-pointer-sheet (sheet-parent sheet))
             (sheets '()))
            ((or (eq sheet common-ancestor)
                 (graftp sheet))
             (map nil #'synthesize-enter sheets))
          (when (should-synthesize-for-sheet-p sheet)
            (push sheet sheets)))))

    new-pointer-sheet))

(defmethod distribute-event ((port basic-port) event)
  (dispatch-event (event-sheet event) event))

;;; In the most general case we can't tell whether all sheets are
;;; mirrored or not. So this default method for pointer-events
;;; operates under the assumption that we must deliver events to
;;; sheets which doesn't have a mirror and that the sheet grabbing and
;;; pressing is implemented locally. -- jd 2019-08-21
(defmethod distribute-event ((port basic-port) (event pointer-event))
  ;; When we receive pointer event we need to take into account
  ;; unmirrored sheets and grabbed/pressed sheets.
  ;;
  ;; - Grabbed sheet steals all pointer events (non-local exit)
  ;; - Pressed sheet receives replicated motion events
  ;; - Pressing/releasing the button assigns pressed-sheet
  ;; - Pressing the button sends the focus event
  ;; - Pointer motion may result in synthesized boundary events
  ;; - Events are delivered to the innermost child of the sheet
  (when-let ((grabbed-sheet (port-grabbed-sheet port)))
    (return-from distribute-event
      (unless (typep event 'pointer-boundary-event)
        (dispatch-event-copy grabbed-sheet event))))
  ;; Synthesize boundary events and update the port-pointer-sheet.
  (let ((pressed-sheet (port-pressed-sheet port))
        (new-pointer-sheet (synthesize-boundary-events port event)))
    ;; Set the pointer cursor.
    (when-let ((cursor-sheet (or pressed-sheet new-pointer-sheet)))
      (let* ((event-sheet (event-sheet event))
             (old-pointer-cursor (port-lookup-current-pointer-cursor
                                  port event-sheet))
             (new-pointer-cursor (sheet-pointer-cursor cursor-sheet)))
        (unless (eql old-pointer-cursor new-pointer-cursor)
          (set-sheet-pointer-cursor port event-sheet new-pointer-cursor))))
    ;; Handle some events specially.
    (typecase event
      ;; Pressing pointer button over a sheet makes a sheet
      ;; pressed. Pressed sheet is assigned only when there is
      ;; currently none.
      (pointer-button-press-event
       (when (null pressed-sheet)
         (setf (port-pressed-sheet port) new-pointer-sheet)))
      ;; Releasing the button sets the pressed sheet to NIL.
      (pointer-button-release-event
       (when pressed-sheet
         (unless (eql pressed-sheet new-pointer-sheet)
           (dispatch-event-copy pressed-sheet event))
         (setf (port-pressed-sheet port) nil)))
      ;; Boundary events are dispatched in SYNTHESIZE-BOUNDARY-EVENTS.
      (pointer-boundary-event
       (return-from distribute-event))
      ;; Unless pressed sheet is already a target of the motion event,
      ;; event is duplicated and dispatched to it.
      (pointer-motion-event
       (when (and pressed-sheet (not (eql pressed-sheet new-pointer-sheet)))
         (dispatch-event-copy pressed-sheet event))))
    ;; Distribute event to the innermost child (may be none).
    (when new-pointer-sheet
      (dispatch-event-copy new-pointer-sheet event))))

(defmacro with-port-locked ((port) &body body)
  (let ((fn (gensym "CONT.")))
    `(labels ((,fn ()
                ,@body))
       (declare (dynamic-extent #',fn))
       (invoke-with-port-locked ,port #',fn))))

(defgeneric invoke-with-port-locked (port continuation))

(defmethod invoke-with-port-locked ((port basic-port) continuation)
  (with-recursive-lock-held ((port-lock port))
    (funcall continuation)))

(defun map-over-ports (function)
  (mapc function *all-ports*))

(defmethod restart-port ((port basic-port))
  nil)

(defmethod destroy-port ((port basic-port))
  nil)

(defmethod destroy-port :around ((port basic-port))
  (unwind-protect
       (call-next-method)
    (setf *all-ports* (remove port *all-ports*))))

;;; Graft

(defmethod make-graft
    ((port basic-port) &key (orientation :default) (units :device))
  (make-instance 'graft :port port :mirror nil
                        :orientation orientation :units units))

(defmethod make-graft :around ((port basic-port) &key orientation units)
  (declare (ignore orientation units))
  (let ((graft (call-next-method)))
    (push graft (port-grafts port))
    graft))

(defmethod map-over-grafts (function (port basic-port))
  (mapc function (port-grafts port)))

(defun find-graft (&key (port nil)
		     (server-path *default-server-path*)
		     (orientation :default)
		     (units :device))
  (when (null port)
    (setq port (find-port :server-path server-path)))
  (map-over-grafts #'(lambda (graft)
		       (if (and (eq orientation (graft-orientation graft))
				(eq units (graft-units graft)))
			   (return-from find-graft graft)))
		   port)
  (make-graft port :orientation orientation :units units))

;;; Pixmap

(defmethod port-lookup-mirror ((port basic-port) (pixmap pixmap))
  (gethash pixmap (slot-value port 'pixmap->mirror)))

;;; FIXME: The generic function PORT-LOOKUP-PIXMAP appear not to be
;;; used anywhere.
(defgeneric port-lookup-pixmap (port mirror))

(defmethod port-lookup-pixmap ((port basic-port) mirror)
  (gethash mirror (slot-value port 'mirror->pixmap)))

(defmethod port-register-mirror ((port basic-port) (pixmap pixmap) mirror)
  (setf (gethash pixmap (slot-value port 'pixmap->mirror)) mirror)
  (setf (gethash mirror (slot-value port 'mirror->pixmap)) pixmap)
  nil)

(defmethod port-unregister-mirror ((port basic-port) (pixmap pixmap) mirror)
  (remhash pixmap (slot-value port 'pixmap->mirror))
  (remhash mirror (slot-value port 'mirror->pixmap))
  nil)

(defmethod realize-mirror ((port basic-port) (pixmap mirrored-pixmap))
  (declare (ignorable port pixmap))
  (error "Don't know how to realize the mirror on a generic port"))

(defmethod destroy-mirror ((port basic-port) (pixmap mirrored-pixmap))
  (declare (ignorable port pixmap))
  (error "Don't know how to destroy the mirror on a generic port"))

(defmethod port-allocate-pixmap ((port basic-port) sheet width height)
  (declare (ignore sheet width height))
  (error "ALLOCATE-PIXMAP is not implemented for generic PORTs"))

(defmethod port-deallocate-pixmap ((port basic-port) pixmap)
  (declare (ignore pixmap))
  (error "DEALLOCATE-PIXMAP is not implemented for generic PORTs"))


(defgeneric port-force-output (port)
  (:documentation "Flush the output buffer of PORT, if there is one."))

(defmethod port-force-output ((port basic-port))
  (values))

;;; Design decision: Recursive grabs are a no-op.

(defgeneric port-grab-pointer (port pointer sheet)
  (:documentation "Grab the specified pointer.")
  (:method ((port basic-port) pointer sheet)
    (declare (ignorable port pointer sheet))
    (warn "Port ~A has not implemented pointer grabbing." port))
  (:method :around ((port basic-port) pointer sheet)
    (declare (ignorable port pointer sheet))
    (unless (port-grabbed-sheet port)
      (setf (port-grabbed-sheet port) sheet)
      (call-next-method))))

(defgeneric port-ungrab-pointer (port pointer sheet)
  (:documentation "Ungrab the specified pointer.")
  (:method ((port basic-port) pointer sheet)
    (declare (ignorable port pointer sheet))
    (warn "Port ~A  has not implemented pointer grabbing." port))
  (:method :around ((port basic-port) pointer sheet)
    (declare (ignorable port pointer sheet))
    (when (port-grabbed-sheet port)
      (setf (port-grabbed-sheet port) nil)
      (call-next-method))))

(defmacro with-pointer-grabbed ((port sheet &key pointer) &body body)
  (with-gensyms (the-port the-sheet the-pointer)
    `(let* ((,the-port ,port)
	    (,the-sheet ,sheet)
	    (,the-pointer (or ,pointer (port-pointer ,the-port))))
       (if (not (port-grab-pointer ,the-port ,the-pointer ,the-sheet))
           (warn "Port ~A failed to grab a pointer." ,the-port)
           (unwind-protect
                (handler-bind
                    ((serious-condition
                      #'(lambda (c)
			  (declare (ignore c))
			  (port-ungrab-pointer ,the-port
                                               ,the-pointer
                                               ,the-sheet))))
                  ,@body)
	     (port-ungrab-pointer ,the-port ,the-pointer ,the-sheet))))))

(defgeneric set-sheet-pointer-cursor (port sheet cursor)
  (:documentation "Sets the cursor associated with SHEET. CURSOR is a symbol, as described in the Franz user's guide."))

(defmethod set-sheet-pointer-cursor ((port basic-port) sheet cursor)
  (declare (ignore sheet cursor))
  (warn "Port ~A has not implemented sheet pointer cursors." port))

;;;;
;;;; Font listing extension
;;;;

(defgeneric port-all-font-families
    (port &key invalidate-cache &allow-other-keys)
  (:documentation
   "Returns the list of all FONT-FAMILY instances known by PORT.
With INVALIDATE-CACHE, cached font family information is discarded, if any."))

(defgeneric font-family-name (font-family)
  (:documentation
   "Return the font family's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style family for this port."))

(defgeneric font-family-port (font-family)
  (:documentation "Return the port this font family belongs to."))

(defgeneric font-family-all-faces (font-family)
  (:documentation
   "Return the list of all font-face instances for this family."))

(defgeneric font-face-name (font-face)
  (:documentation
   "Return the font face's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style face for this port."))

(defgeneric font-face-family (font-face)
  (:documentation "Return the font family this face belongs to."))

(defgeneric font-face-all-sizes (font-face)
  (:documentation
   "Return the list of all font sizes known to be valid for this font,
if the font is restricted to particular sizes.  For scalable fonts, arbitrary
sizes will work, and this list represents only a subset of the valid sizes.
See font-face-scalable-p."))

(defgeneric font-face-scalable-p (font-face)
  (:documentation
   "Return true if this font is scalable, as opposed to a bitmap font.  For
a scalable font, arbitrary font sizes are expected to work."))

(defgeneric font-face-text-style (font-face &optional size)
  (:documentation
   "Return an extended text style describing this font face in the specified
size.  If size is nil, the resulting text style does not specify a size."))

(defclass font-family ()
  ((font-family-port :initarg :port :reader font-family-port)
   (font-family-name :initarg :name :reader font-family-name))
  (:documentation "The protocol class for font families.  Each backend
defines a subclass of font-family and implements its accessors.  Font
family instances are never created by user code.  Use port-all-font-families
to list all instances available on a port."))

(defmethod print-object ((object font-family) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A" (font-family-name object))))

(defclass font-face ()
  ((font-face-family :initarg :family :reader font-face-family)
   (font-face-name :initarg :name :reader font-face-name))
  (:documentation "The protocol class for font faces  Each backend
defines a subclass of font-face and implements its accessors.  Font
face instances are never created by user code.  Use font-family-all-faces
to list all faces of a font family."))

(defmethod print-object ((object font-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A, ~A"
	    (font-family-name (font-face-family object))
	    (font-face-name object))))

;;; fallback font listing implementation:

(defclass basic-font-family (font-family) ())
(defclass basic-font-face (font-face) ())

(defmethod port-all-font-families ((port basic-port) &key invalidate-cache)
  (declare (ignore invalidate-cache))
  (flet ((make-basic-font-family (name)
	   (make-instance 'basic-font-family :port port :name name)))
    (list (make-basic-font-family "FIX")
	  (make-basic-font-family "SERIF")
	  (make-basic-font-family "SANS-SERIF"))))

(defmethod font-family-all-faces ((family basic-font-family))
  (flet ((make-basic-font-face (name)
	   (make-instance 'basic-font-face :family family :name name)))
    (list (make-basic-font-face "ROMAN")
	  (make-basic-font-face "BOLD")
	  (make-basic-font-face "BOLD-ITALIC")
	  (make-basic-font-face "ITALIC"))))

(defmethod font-face-all-sizes ((face basic-font-face))
  (list 1 2 3 4 5 6 7))

(defmethod font-face-scalable-p ((face basic-font-face))
  nil)

(defmethod font-face-text-style ((face basic-font-face) &optional size)
  (make-text-style
   (find-symbol (string-upcase (font-family-name (font-face-family face)))
		:keyword)
   (if (string-equal (font-face-name face) "BOLD-ITALIC")
       '(:bold :italic)
       (find-symbol (string-upcase (font-face-name face)) :keyword))
   (ecase size
     ((nil) nil)
     (1 :tiny)
     (2 :very-small)
     (3 :small)
     (4 :normal)
     (5 :large)
     (6 :very-large)
     (7 :huge))))
