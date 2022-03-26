(in-package #:climi)

(defclass standard-port (clim:basic-port)
  ((mirrored-sheet->current-pointer-cursor :initform (make-hash-table :test #'eq))
   (selections :initform (make-hash-table) :reader standard-port-selections)))

(defmethod port-lookup-current-pointer-cursor ((port standard-port) sheet)
  (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)))

(defmethod set-sheet-pointer-cursor :before ((port standard-port) sheet cursor)
  (setf (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)) cursor))

(defmethod port-unregister-mirror :after ((port standard-port) sheet mirror)
  (remhash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)))

(defun stored-object (port selection)
  (check-type port standard-port)
  (check-type selection symbol)
  (gethash selection (standard-port-selections port)))

(defsetf stored-object (port selection) (value)
  (alexandria:once-only (port selection)
    `(progn
       (check-type ,port standard-port)
       (check-type ,selection symbol)
       (setf (gethash ,selection (standard-port-selections ,port)) ,value))))

(defun remove-stored-object (port selection)
  (check-type port standard-port)
  (check-type selection symbol)
  (remhash selection (standard-port-selections port)))

(defun get-object-table (object)
  (check-type object selection-object)
  (let ((sheet (selection-object-owner object)))
    (etypecase sheet
      (pane (frame-command-table (pane-frame sheet)))
      (sheet (find-command-table 'global-command-table)))))

;;; How selection boxes work
;;;
;;; User may use any number of selection boxes designated by a symbol. Some boxes
;;; may be treated specially – for instance on x11 :clipboard and :primary are
;;; integrated with the display server boxes. Backends are allowed to
;;; EQL-specialize other boxes too. If a box is not specialized then it is
;;; available only for frames which are in the same lisp image.
;;;
;;; Some boxes have default treatment:
;;;
;;;   :LOCAL-SELECTION
;;;
;;;     Active selection on clim-stream made with a gesture.
;;;
;;;   :PRIMARY
;;;
;;;     Current selection on a display server.  Gesture on clim-stream will change
;;;     the selection, but another window may overwrite it. Content of this box is
;;;     pasted when appropriate gesture is made.
;;;
;;;   :CLIPBOARD
;;;
;;;     Clipboard holds a result of the last kill operation which may be used in a
;;;     yank operation.
;;;
;;; End users:
;;;
;;;   Some predefined gestures are defined to make the end user life
;;;   easier. For text-selection:
;;;
;;;     Shift-Mouse-L: select text by dragging cursor (sets the box :primary)
;;;     Shift-Mouse-R: move nearest point of existing selection
;;;     Shift-Mouse-M: paste content of the box :primary
;;;
;;;   On text-editing fields kill command puts content in the box :clipboard and
;;;   yank command takes the content from said box and puts it in the buffer.
;;;
;;; Instances of class SELECTION-OBJECT are used for both requesting and
;;; publishing the selection. Methods specialized on port are specialized on this
;;; class of object and it is a responsibility of methods specialized on sheets to
;;; allocate these objects for functions PUBLISH-SELECTION, RELEASE-SELECTION and
;;; REQUEST-SELECTION.
;;;
;;; SELECTION-OBJECT-CONTENT - content to publish/clear or a selection for rquest
;;; SELECTION-OBJECT-TYPES - presentation types associated with the content
;;; SELECTION-OBJECT-OWNER - sheet associated with the content
;;;
;;; Backend developers:
;;;
;;;   Backend developers should write methods for CLIMB:CLEAR-SELECTION,
;;;   CLIMB:PUBLISH-SELECTION and CLIMB:REQUEST-SELECTION for each box which
;;;   should have a special behavior associated with it.
;;;
;;;   CLIMB:SELECTION-OBJECT is a class with the following accessors:
;;;
;;;     CLIMB:SELECTION-OBJECT-CONTENT - content to publish/clear or a selection for request
;;;     CLIMB:SELECTION-OBJECT-TYPES - presentation types associated with the content
;;;     CLIMB:SELECTION-OBJECT-OWNER - sheet associated with the content
;;;
;;;   CLIMB:PUBLISH-SELECTION publisher selection object type
;;;
;;;     PUBLISHER should be specialized on developed port, SELECTION should have
;;;     EQL-specialization on a symbol denoting the box and OBJECT should be
;;;     specialized on CLIMB:SELECTION-OBJECT. Argument TYPE should be ignored.
;;;
;;;   CLIMB:RELEASE-SELECTION publisher selection object
;;;
;;;     PUBLISHER should be specialized on developed port and SELECTION should
;;;     have EQL-specialization on a symbol denoting the box. Result of this
;;;     operation is making the selection unavailable for new requests.
;;;
;;;   CLIMB:REQUEST-SELECTION requester selection requested
;;;
;;;     REQUESTER should be specialized on developed port. SELECTION should have
;;;     EQL-specialization on a symbol denoting the box. REQUESTED is an instance
;;;     of CLIMB:SELECTION-OBJECT. Type associated with this object is treated as
;;;     input-context (acceptable type). When it is possible to synchronously
;;;     return the selection of requested type function returns selection content
;;;     and its type. Otherwise NIL is returned.
;;;
;;; Selection translations
;;;
;;;   Selection object table is a command table where function
;;;   REQUEST-SLECTION should look for translations in case of the
;;;   presentation type mismatch. Selection translators are defined
;;;   with DEFINE-SELECTION-TRANSLATOR. Algorithm works as follows:
;;;
;;;   1. Return object if its type is subtype of a requested one.
;;;   2. Look for a matching translator in the publisher table.
;;;   3. Look for a matching translator in the requester table.

(defclass selection-object ()
  ((content :initarg :content :accessor selection-object-content)
   (type    :initarg :type    :accessor selection-object-type)
   (owner   :initarg :owner   :reader   selection-object-owner)))

(defgeneric publish-selection (publisher selection object type)
  (:documentation "Publish the object in the selection box.")
  (:method ((sheet basic-sheet) selection object type)
    (check-type selection symbol)
    (publish-selection (port sheet)
                       selection
                       (make-instance 'selection-object
                                      :content object
                                      :type type
                                      :owner sheet)
                       nil))
  (:method ((port standard-port) selection (object selection-object) type)
    (declare (ignore type))
    (setf (stored-object port selection) object)))

(defgeneric release-selection (publisher selection object)
  (:documentation "Release the selection box.")
  (:method ((sheet basic-sheet) selection object)
    (declare (ignore object))
    (check-type selection symbol)
    (alexandria:when-let* ((port (port sheet))
                           (object (stored-object port selection)))
      (release-selection port selection object)))
  (:method ((port standard-port) selection object)
    (declare (ignore object))
    (remove-stored-object port selection)))

(defgeneric request-selection (requester selection requested)
  (:documentation "Request an object from the selection box.")
  (:method ((sheet basic-sheet) selection type)
    (check-type selection symbol)
    (let ((object (make-instance 'selection-object
                                 :content selection
                                 :type type
                                 :owner sheet)))
      (request-selection (port sheet) selection object)))
  (:method ((port standard-port) selection request)
    (alexandria:when-let ((object (stored-object port selection)))
      (let ((selected-type (selection-object-type object))
            (requested-type (selection-object-type request)))
        (when (presentation-subtypep selected-type requested-type)
          (return-from request-selection
            (values (selection-object-content object) selected-type)))
        (let ((table-1 (get-object-table object))
              (table-2 (get-object-table request))
              (presentation (make-instance 'standard-presentation
                                           :object (selection-object-content object)
                                           :type selected-type
                                           :view +textual-view+)))
          (flet ((try-translators (translators visited)
                   (some (lambda (tr)
                           (and (typep tr 'selection-translator)
                                (not (member tr visited))
                                (test-presentation-translator tr presentation requested-type nil nil nil nil)
                                (call-presentation-translator tr presentation requested-type nil nil nil nil nil)))
                         translators)))
            (let ((tr1 (find-presentation-translators selected-type requested-type table-1)))
              (or (try-translators tr1 nil)
                  (try-translators (find-presentation-translators selected-type requested-type table-2)
                                   tr1)))))))))
