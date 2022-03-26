;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)
;;;  (c) copyright 2004 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2019 by Daniel Kochmański (daniel@turtleware.eu)

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

(defun default-tracking-handler (&key event &allow-other-keys)
  (handle-event (event-sheet event) event))

(deftype tracking-pointer-clause ()
  `(member :pointer-motion :pointer-button-press :pointer-button-release
           :presentation :presentation-button-press :presentation-button-release
           :keyboard))

(defclass tracking-pointer-state ()
  ((motion-handler
    :reader motion-handler
    :initarg :pointer-motion)
   (button-press-handler
    :reader button-press-handler
    :initarg :pointer-button-press)
   (buttton-release-handler
    :reader button-release-handler
    :initarg :pointer-button-release)
   (presentation-handler
    :reader presentation-handler
    :initarg :presentation)
   (presentation-button-release-handler
    :reader presentation-button-release-handler
    :initarg :presentation-button-release)
   (presentation-button-press-handler
    :reader presentation-button-press-handler
    :initarg :presentation-button-press)
   (keyboard-handler
    :reader keyboard-handler
    :initarg :keyboard)
   (tracked-sheet
    :reader tracked-sheet
    :initarg :sheet)
   (tracked-pointer
    :reader tracked-pointer
    :initarg :pointer)
   (multiple-window
    :reader multiple-window
    :initarg :multiple-window)
   (transformp
    :reader transformp
    :initarg :transformp)
   (context-type
    :reader context-type
    :initarg :context-type)
   (highlight
    :reader highlight
    :initarg :highlight)
   (%highlighted-presentation
    :accessor %highlighted-presentation
    :initform nil))
  (:default-initargs :pointer-motion         #'default-tracking-handler
                     :pointer-button-press   #'default-tracking-handler
                     :pointer-button-release #'default-tracking-handler
                     :keyboard               #'default-tracking-handler
                     ;; Presentation handlers default to NIL so we can
                     ;; resort to their "raw" counterparts when due.
                     :presentation nil
                     :presentation-button-press nil
                     :presentation-button-release nil
                     :multiple-window nil
                     :transformp nil
                     :context-type t
                     :highlight nil))

(defmethod initialize-instance :after
    ((instance tracking-pointer-state)
     &key pointer
          (presentation nil p-p)
          (presentation-button-press nil pbp-p)
          (presentation-button-release nil pbr-p)
          (highlight nil h-p))
  (declare (ignore presentation
                   presentation-button-press
                   presentation-button-release
                   highlight))
  (unless pointer
    (setf (slot-value instance 'tracked-pointer)
          (port-pointer (port (tracked-sheet instance)))))
  (unless h-p
    (setf (slot-value instance 'highlight)
          (or p-p pbp-p pbr-p))))

(defgeneric sheet-find-presentation (sheet context-type x y)
  (:method (sheet context-type x y)
    nil)
  (:method ((stream output-recording-stream) context-type x y)
    (labels ((innermost-first (record)
               (map-over-output-records-containing-position #'innermost-first record x y)
               (when (and (presentationp record)
                          (presentation-subtypep (presentation-type record) context-type))
                 (return-from sheet-find-presentation record))))
      (innermost-first (stream-output-history stream)))))

;;; Function is responsible for handling events in tracking-pointer
;;; macro.
(defgeneric track-event (state event x y)
  (:method ((state tracking-pointer-state) event x y)
    (default-tracking-handler :event event))
  (:method ((state tracking-pointer-state) (event keyboard-event) x y)
    (funcall (keyboard-handler state) :gesture event :event event :x x :y y)))

(macrolet ((frob (event-type presentation-handler normal-handler)
             `(defmethod track-event ((state tracking-pointer-state)
                                      (event ,event-type)
                                      x y)
                (let ((window (event-sheet event)))
                  (when-let ((highlighted (%highlighted-presentation state)))
                    (highlight-output-record highlighted window :unhighlight))
                  (alexandria:when-let*
                      ((context-type (context-type state))
                       (handler (,presentation-handler state))
                       (presentation (sheet-find-presentation window context-type x y)))
                    (when (highlight state)
                      (setf (%highlighted-presentation state) presentation)
                      (highlight-output-record presentation window :highlight))
                    (return-from track-event
                      (funcall handler
                               :presentation presentation
                               :event event :window window :x x :y y)))
                  (funcall (,normal-handler state) :event event :window window :x x :y y)))))
  (frob pointer-motion-event         presentation-handler                motion-handler)
  (frob pointer-button-press-event   presentation-button-press-handler   button-press-handler)
  (frob pointer-button-release-event presentation-button-release-handler button-release-handler))

(defun invoke-tracking-pointer (state)
  (let* ((tracked-sheet (tracked-sheet state))
         (pointer (tracked-pointer state))
         (multiple-window (multiple-window state))
         (transformp (transformp state))
         (modifier-state))
    (flet ((track-pointer-event (event)
             (multiple-value-call #'track-event state event
               (let ((sheet (event-sheet event)))
                 (get-pointer-position (sheet event)
                   (if (not transformp)
                       (values x y)
                       (with-sheet-medium (medium sheet)
                         (transform-position (medium-transformation medium) x y))))))))
      ;; Synthesize a pointer motion event for the current pointer
      ;; position so that appropriate handlers are called even if no
      ;; event immediately follows the INVOKE-TRACKING-POINTER call.
      ;; This ensures, for example, that feedback and/or pointer
      ;; documentation are initialized right away in the context of
      ;; presentation drag and drop.
      ;;
      ;; However, to prevent things like drag and drop feedback being
      ;; drawn to the wrong sheet, discard the synthesized event if
      ;; its sheet is not a tracked sheet. This can happen if
      ;; MULTIPLE-WINDOW is false, INVOKE-TRACKING-POINTER is invoked
      ;; via, say, a keyboard gesture or programmatically and the
      ;; pointer is not over TRACKED-SHEET.
      (let ((event (synthesize-pointer-motion-event pointer)))
        (setf modifier-state (event-modifier-state event))
        (when (or multiple-window
                  (eql tracked-sheet (event-sheet event)))
          (track-pointer-event event)))
      (loop for event = (event-read tracked-sheet)
            ;; We let HANDLE-EVENT take care of events that are not
            ;; for TRACKED-SHEET (unless MULTIPLE-WINDOW is true). On
            ;; the other hand, we pass events for TRACKED-SHEET (or
            ;; all events if MULTIPLE-WINDOW is true) to TRACK-EVENT.
            do (cond ((not (or multiple-window
                               (eql tracked-sheet (event-sheet event))))
                      ;; Event is not intercepted.
                      (handle-event (event-sheet event) event))
                     ((typep event 'pointer-event)
                      (track-pointer-event event))
                     (t
                      (track-event state event nil nil)))
            ;; As a special exception, whenever a device event changes
            ;; the modifier state, we synthesize an event, so that
            ;; mouse-only and non-MULTIPLE-WINDOW handling can still
            ;; react to changed keyboard modifiers.
            when (typep event 'device-event)
            do (let ((new-state (event-modifier-state event)))
                 (when (not (eql modifier-state new-state))
                   (track-pointer-event
                    (synthesize-pointer-motion-event pointer)))
                 (setf modifier-state new-state))))))

(defmacro tracking-pointer
    ((sheet &rest args &key pointer multiple-window transformp context-type highlight)
     &body body)
  (declare (ignore pointer multiple-window transformp context-type highlight))
  (setq sheet (stream-designator-symbol sheet '*standard-output*))
  ;; The Spec specifies the tracking-pointer clause arguments as,
  ;; e.g., (&key presentation event x y), implying that the user must
  ;; write the &key keyword, but real code doesn't do that. Check if
  ;; &key is in the arg list and add it if it is not.
  (flet ((fix-args (name args)
           (let ((aok nil)
                 (args (if (eq (car args) '&key)
                           args
                           (cons '&key args))))
             (dolist (arg (cdr args))
               (cond ((find arg '(window event gesture presentation x y) :test #'string=))
                     ((eq arg '&allow-other-keys)
                      (setf aok t))
                     (t
                      (error "TRACKING-POINTER: ~s is not a valid argument for a clause ~s."
                             arg name))))
             (unless aok
               (setq args (append args '(&allow-other-keys))))
             args)))
    (loop
       for (name arglist . body) in body
       for handler-name = (gensym (symbol-name name))
       do (unless (typep name 'tracking-pointer-clause)
            (error "TRACKING-POINTER: ~s is not a valid clause name." name))
       collect `(,handler-name ,(fix-args name arglist) ,@body) into bindings
       collect `#',handler-name into fn-names
       append  `(,name #',handler-name) into initargs
       finally (return `(flet ,bindings
                          (declare (dynamic-extent ,@fn-names))
                          (invoke-tracking-pointer
                           (make-instance 'tracking-pointer-state
                                          :sheet ,sheet ,@args ,@initargs)))))))


;;; DRAG-OUTPUT-RECORD and DRAGGING-OUTPUT.
(defun make-default-feedback-function (erase repaint dx dy)
  (lambda (record sheet x0 y0 x y action)
    (declare (ignore x0 y0))
    (setf (output-record-position record) (values (+ x dx) (+ y dy)))
    (ecase action
      (:erase
       (if (output-recording-stream-p sheet)
           (maybe-funcall erase record sheet)
           (repaint-sheet sheet (bounding-rectangle record))))
      (:repaint
       (when repaint
         (repaint-sheet sheet (bounding-rectangle record))))
      (:draw
       (replay-output-record record sheet))
      (:finish
       (when (output-recording-stream-p sheet)
         (stream-add-output-record sheet record))
       (replay-output-record record sheet)))))

(defun make-custom-feedback-function (function)
  (lambda (record sheet x0 y0 x y action)
    (ecase action
      ((:erase :repaint)
       (funcall function record sheet x0 y0 x y :erase))
      ((:draw :finish)
       (funcall function record sheet x0 y0 x y :draw)))))

(defmethod drag-output-record
    ((stream output-recording-stream) (record output-record)
     &key feedback finish-on-release multiple-window
       (erase #'erase-output-record) (repaint t))
  (nest
   (multiple-value-bind (abs-x0 abs-y0) (pointer-position (port-pointer (port stream))))
   (multiple-value-bind (str-x0 str-y0) (stream-pointer-position stream))
   (multiple-value-bind (rx ry)         (output-record-position record))
   (let* (;; If output-record has a parent it means we need to re-add it
          ;; to the destination stream after dragging. That makes
          ;; possible implementation of dragging-output-record macro
          ;; without introducing extra flags. -- jd 2019-08-07
          (parent (output-record-parent record))
          ;; last-sheet, last-x and last-y show the last pointer
          ;; position as observed from pointer-motion handler. This is
          ;; necessary to provide correct arguments for erasure.
          last-sheet last-x last-y
          ;; Mouse position relative to the record start is
          ;; necessary to drag it without repositioning it to
          ;; start where the cursor does.
          (dx (- rx str-x0))
          (dy (- ry str-y0))
          (feedback-fn
           (if feedback
               (make-custom-feedback-function feedback)
               (make-default-feedback-function erase repaint dx dy)))))
   (flet ((finish (window x y)
            (multiple-value-bind (x0 y0)
                (let ((graft (graft window)))
                  (untransform-position (sheet-delta-transformation window graft) abs-x0 abs-y0))
              (funcall feedback-fn record last-sheet x0 y0 last-x last-y :repaint)
              (unless (null parent)
                (funcall feedback-fn record window x0 y0 x y :finish)))
            (return-from drag-output-record (values x y))))
     (setf (stream-current-output-record stream)
           (stream-output-history stream))
     ;; feedback function may draw something not resembling
     ;; output record so we erase and draw it right away.
     (funcall feedback-fn record stream str-x0 str-y0 str-x0 str-y0 :erase)
     (funcall feedback-fn record stream str-x0 str-y0 str-x0 str-y0 :draw)
     (setf last-sheet stream
           last-x str-x0
           last-y str-y0)
     (tracking-pointer (stream :multiple-window multiple-window)
       (:pointer-motion
        (&key window x y)
        (unless (and (eql last-sheet window)
                     (= last-x x)
                     (= last-y y))
          (multiple-value-bind (x0 y0)
              (if (eql window stream)
                  (values str-x0 str-y0)
                  (let* ((graft (graft window))
                         (tr (sheet-delta-transformation window graft)))
                    (untransform-position tr abs-x0 abs-y0)))
            (funcall feedback-fn record last-sheet x0 y0 last-x last-y :repaint)
            (funcall feedback-fn record window x0 y0 x y :draw))
          (setf last-sheet window
                last-x x
                last-y y)))
       (:pointer-button-press
        (&key event x y)
        (unless finish-on-release
          (finish (event-sheet event) x y)))
       (:pointer-button-release
        (&key event x y)
        (when finish-on-release
          (finish (event-sheet event) x y)))))))

(defmacro dragging-output ((&optional (stream '*standard-output*) &rest args
                                      &key (repaint t) finish-on-release multiple-window)
                           &body body)
  (declare (ignore repaint finish-on-release multiple-window))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-gensyms (erase record)
    `(let ((,record (with-output-to-output-record (,stream) ,@body)))
       (flet ((,erase (record sheet)
                ;; Default function would signal error.
                (erase-output-record record sheet nil)))
         (drag-output-record ,stream ,record :erase #',erase ,@args)))))
