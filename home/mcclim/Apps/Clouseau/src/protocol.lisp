;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; Place protocol
;;;
;;; Introspection and mutation of place objects.

(defgeneric parent (place))

(defgeneric root (place))

(defgeneric children (place)
  (:documentation
   "Return a list of child places of PLACE."))

(defgeneric ensure-child (container cell class place thunk)
  (:documentation
   "Return the child for CONTAINER, CELL, CLASS of PLACE or call THUNK.

If PLACE already contains a child place for CONTAINER, CELL, CLASS
that child place is used. Otherwise THUNK is called without arguments
and must return a new child place which is then stored in PLACE.

Return the existing child or newly made child place."))

(defgeneric container (place)
  (:documentation
   "Return the object containing the place represented by PLACE."))

(defgeneric cell (place)
  (:documentation
   "Return the location within the container represented by PLACE."))

(defgeneric state (place)
  (:documentation
   "Return NIL or an object state for the value of PLACE."))

(defgeneric ensure-state (object place thunk)
  (:documentation
   "Return the existing object state for OBJECT in PLACE or call THUNK.

If PLACE already has an object state and it is applicable to
OBJECT (determined via `state-applicable-p'), that object state is
used. Otherwise, THUNK is called without arguments and must return a
new object state which is then stored in PLACE.

Return the existing or newly made object state."))

(defgeneric supportsp (place operation))

(defgeneric accepts-value-p (place value))

(defgeneric valuep (place))

(defun safe-valuep (place)
  (ignore-errors (valuep place)))

(defgeneric value (place))

(defgeneric (setf value) (new-value place))

(defgeneric remove-value (place))

;;; Default behavior

(defmethod root ((place t))
  (if-let ((parent (parent place)))
    (root parent)
    place))

;;; Object state protocol
;;;
;;; An object state instance holds information pertaining to a
;;; place-object pair.

(defgeneric place (state)
  (:documentation
   "Return the place with which STATE is associated."))

(defgeneric object (state)
  (:documentation
   "Return the object with which STATE is associated."))

;; TODO should a `style' generic function be part of the protocol?

(defgeneric state-applicable-p (state object place)
  (:documentation
   "Return true if STATE is suitable for OBJECT in PLACE."))

(defgeneric object-state-class (object place)
  (:documentation
   "Return the name of a state class suitable for OBJECT in PLACE."))

(defgeneric make-object-state (object place)
  (:documentation
   "Return a state instance suitable for OBJECT in PLACE."))

;;; Change protocol

(defgeneric note-changed (place))

;;; Object inspection protocol

(defvar *place* nil)

(defvar *parent-place*)

(defvar *depth*)

(defvar *seen*)

(defgeneric call-with-root-place (thunk place stream &key view))

(defgeneric inspect-place (place stream)
  (:documentation
   "Present PLACE to STREAM.

By default, retrieve the value of PLACE and inspect it using
`inspect-object'.

User code normally does not have to define methods on this generic
functions."))

(defgeneric inspect-object (object stream)
  (:documentation
   "Present OBJECT to STREAM.

By default, calls `inspector-object-using-state'.

STREAM is the stream to which OBJECT should be presented.

User code normally does not have to define methods on this generic
functions."))

(defgeneric inspect-object-using-state (object state style stream)
  (:argument-precedence-order state object style stream)
  (:documentation
   "Present OBJECT to STREAM according to STATE and STYLE.

STATE stores information that is permanently associated with OBJECT.

STYLE on the other hand consists of transient information such as
whether OBJECT should be presented in expanded or collapsed form.

STREAM is the stream to which OBJECT should be presented.

Example:

  (defmethod clouseau:inspect-object-using-state ((object symbol)
                                                  (state  clouseau:inspected-object)
                                                  (style  (eql :expanded-body))
                                                  (stream t))
    (clouseau:formatting-place (object 'clouseau:reader-place 'symbol-name
                                present-place present-object)
      (write-string \"Symbol name\" stream)
      (present-place stream)
      (present-object stream)))"))

(defgeneric note-object-occurrence (object state presentation stream)
  (:documentation
   "Note that PRESENTATION is a representation of OBJECT in STREAM.

STATE is the state associated with OBJECT.

The main purpose of this generic function is tracking multiple
occurrences of objects so the circularity can be indicated."))

(defun call-without-noting-object-occurrences (thunk)
  "Call THUNK with NOTE-OBJECT-OCCURRENCE calls devoid of effects."
  (let ((*seen* nil))
    (funcall thunk)))

(defmacro without-noting-object-occurrences (() &body body)
  "Execute BODY with NOTE-OBJECT-OCCURRENCE calls devoid of effects."
  `(call-without-noting-object-occurrences (lambda () ,@body)))

;;; Default behavior

(defmethod call-with-root-place (thunk place stream
                                 &key (view (make-instance 'inspector-view)))
  (climi::letf (((stream-default-view stream) view))
    (let ((*depth*        -1)
          (*seen*         (make-hash-table :test #'eq))
          (*parent-place* place))
      (with-end-of-line-action (stream :allow)
        (funcall thunk)))))

(defmethod inspect-place ((place t) (stream t))
  (if (not (valuep place))
      (with-style (stream :unbound)
        (write-string "unbound" stream))
      (handler-bind
          ((error (lambda (condition)
                    (when (typep condition *handle-errors*)
                      (with-style (stream :error)
                        (format stream "Could not inspect place: ~A" condition))
                      (return-from inspect-place)))))
        (let ((value   (value place))
              (*place* place)
              (*depth* (1+ *depth*)))
          (inspect-object value stream)))))

(defmethod inspect-object ((object t) (stream t))
  (let* ((place        *place*)
         (state        (ensure-state object place
                                     (lambda ()
                                       (make-object-state object place))))
         (style        (style state))
         (presentation (with-output-as-presentation
                           (stream state (presentation-type-of state)
                                   :single-box t)
                         (let ((*place*        nil)
                               (*parent-place* place))
                           (inspect-object-using-state
                            object state style stream)))))
    (note-object-occurrence object state presentation stream)
    state))

(defmethod note-object-occurrence :around ((object t)
                                           (state  t)
                                           (presentation t)
                                           (stream       t))
  ;; If we encounter OBJECT for the first time within this traversal,
  ;; reset the occurrences recorded within STATE in case. The
  ;; NOTE-OBJECT-OCCURRENCE method for OBJECT might be a no-op, so
  ;; here is the only place we can do this.
  (when-let ((seen *seen*))
    (unless (gethash object seen)
      (setf (occurrences state) nil)))
  (call-next-method))

(defmethod note-object-occurrence ((object       t)
                                   (state        t)
                                   (presentation t)
                                   (stream       t))
  (when-let ((seen *seen*))
    ;; Slight optimization: for the first occurrence of OBJECT, put
    ;; PRESENTATION instead of a list into the hash-table. When
    ;; encountering a second occurrence, replace the presentation with
    ;; a list.
    (let ((existing (gethash object seen)))
      (cond ((null existing)
             (setf (gethash object seen) presentation))
            ((not (consp existing))
             (let ((cell (cons nil (list presentation existing))))
               (setf (gethash object seen)                        cell
                     (occurrences (presentation-object existing)) cell
                     (occurrences state)                          cell)))
            (t
             (push presentation (cdr existing))
             (setf (occurrences state) existing))))))

;;; Inspector state protocol

(defgeneric root-place (inspector-state &key run-hook-p))

(defgeneric (setf root-place) (new-value inspector-state &key run-hook-p))

(defgeneric root-object (inspector-state &key run-hook-p)
  (:documentation
   "Return the root object stored in INSPECTOR-STATE.

RUN-HOOK-P is accepted but ignore for parity with the `setf'
function."))

(defgeneric (setf root-object) (new-value inspector-state &key run-hook-p)
  (:documentation
   "Store NEW-VALUE as the root object in INSPECTOR-STATE.

RUN-HOOK-P controls whether observers should be notified via the
change hook of INSPECTOR-STATE."))

(defgeneric change-hook (inspector-state))

(defgeneric present-inspected-object-graph (state stream &key view))

;;; Default behavior

(defmethod present-inspected-object-graph (state stream &rest args &key view)
  (declare (ignore view))
  (let ((root-place      (root-place state))
        (*handle-errors* (handle-errors state)))
    (apply #'call-with-root-place
           (lambda ()
             (inspect-place root-place stream))
           root-place stream args)))
