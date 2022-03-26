;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
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

;;; Implementation of most of the CLIM-facing parts of Drei, including
;;; the pane and gadget itself as well as the command tables. The
;;; solely input-editor oriented stuff is in input-editor.lisp.

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Drei gadget and pane.
;;;
;;; An application can use Drei in two different ways - by using
;;; `drei-pane' directly, and controlling the command loop itself
;;; (this is what Climacs does), which offers complete control, but
;;; may end up being crummy if the application is not primarily a text
;;; editor, or it can opt to use the Drei gadget by using the keyword
;;; symbol `:drei' as the type argument to `make-pane'. This will
;;; create a Drei gadget that acts independently of the application
;;; command loop (through event handlers), in effect, it should be a
;;; drop-in replacement for the standard CLIM `:text-editor' gadget.

(defvar *background-color* +white+)
(defvar *foreground-color* +black+)
(defvar *show-mark* nil
  "If true, show a visual representation of the mark.")

;;; Cursors.

;;; NOTE: Despite the name, this does not have anything to do with
;;; CLIM cursors, though perhaps this facility should be built on top
;;; of what CLIM already provides. That seemed a bit (=a lot) hairy,
;;; though.

;;; Cursors are output records. After a cursor is created, The owning
;;; Drei instance should add it to the output stream. The owner of the
;;; cursor (a Drei instance) is responsible for removing the cursor
;;; once it is done with it. Cursors can be active/inactive and
;;; enabled/disabled and have the same activity-status as their
;;; associated view.
(defclass drei-cursor (standard-sequence-output-record)
  ((%view :reader view
          :initarg :view
          :initform (error "A Drei cursor must be associated with a Drei view")
          :type drei-view)
   (%output-stream :reader output-stream
                   :initarg :output-stream
                   :initform (error "A Drei cursor must be associated with an output stream")
                   :type extended-output-stream)
   (%mark :reader mark-of
          :initarg :mark
          :initform (error "A Drei cursor must be associated with a mark."))
   (%enabled :accessor enabled
             :initarg :enabled
             :initform t
             :type boolean
             :documentation "When a cursor is enabled, it will be
drawn when its associated Drei instance is drawn. When it is not
enabled, it will simply be ignored during redisplay.")
   (%active-ink :accessor active-ink
                :initarg :active-ink
                :initform +flipping-ink+
                :type design
                :documentation "The ink used to draw the cursor
when it is active.")
   (%inactive-ink :accessor inactive-ink
                  :initarg :inactive-ink
                  :initform +blue+
                  :type color
                  :documentation "The ink used to draw the cursor
when it is inactive."))
  (:documentation "A visual representation of a given mark in a
Drei buffer. The most important role for instances of subclasses
of this class is to visually represent the position of point."))

(defgeneric active (cursor)
  (:documentation "Whether the cursor is active or
not. An active cursor is drawn using the active ink, and an
inactive is drawn using the inactive ink. Typically, a cursor
will be active when the associated Drei view has focus.")
  (:method ((cursor drei-cursor))
    (active (view cursor))))

(defgeneric ink (cursor)
  (:documentation "Return the ink object that should be used for
  displaying the given cursor."))

(defmethod ink ((cursor drei-cursor))
  (if (active cursor)
      (active-ink cursor)
      (inactive-ink cursor)))

(defmethod (setf enabled) :after ((new-val null) (cursor drei-cursor))
  (clear-output-record cursor))

(defclass point-cursor (drei-cursor)
  ()
  (:default-initargs
   :mark nil)
  (:documentation "A class that should be used for the visual
representation of the point of a Drei instance."))

(defmethod mark-of ((cursor point-cursor))
  (point (view cursor)))

(defclass mark-cursor (drei-cursor)
  ()
  (:default-initargs
   :active-ink +dark-green+
   :inactive-ink +dark-green+
   :mark nil)
  (:documentation "A class that should be used for the visual
representation of the mark of a Drei instance."))

(defmethod mark-of ((cursor mark-cursor))
  (mark (view cursor)))

(defmethod enabled ((cursor mark-cursor))
  (and (call-next-method) *show-mark*))

(defgeneric visible-1 (cursor view)
  (:documentation "Is `cursor', associated with `view', visible?
If this function returns true, it is assumed that it is safe to
display `cursor' to the editor stream. If just one of the
applicable methods returns false, the entire function returns
false.")
  (:method-combination and)
  (:method and (cursor view)
    (enabled cursor)))

(defun visible-p (cursor)
  "Return true if `cursor' is visible. This is a trampoline
function that calls `visible-1' with `cursor' and the view of
`cursor'."
  (visible-1 cursor (view cursor)))

;;; Drei instances.

(defclass drei-pane (drei application-pane)
  ()
  (:default-initargs
   :incremental-redisplay nil
   :end-of-line-action :scroll
   :background *background-color*
   :foreground *foreground-color*
   :display-function 'display-drei-pane
   :active nil)
  (:metaclass modual-class)
  (:documentation "An actual, instantiable Drei pane that
permits (and requires) the host application to control the
command loop completely."))

(defmethod stream-default-view ((stream drei-pane))
  (view stream))

(defmethod display-drei ((drei drei-pane) &rest args)
  (declare (ignore args))
  (redisplay-frame-pane (pane-frame drei) drei))

(defmethod editor-pane ((drei drei-pane))
  ;; The whole point of the `drei-pane' class is that it's its own
  ;; display surface.
  drei)

(defmethod visible-1 and (cursor (view drei-buffer-view))
  ;; We should only redisplay when the cursor is on display, or
  ;; `offset-to-screen-position' will return a non-number. Also don't
  ;; display if the view hasn't been displayed yet.
  (and (<= (offset (top view))
           (offset (mark cursor))
           (offset (bot view)))
       (plusp (displayed-lines-count view))))

(defmethod (setf view) :after (new-val (drei drei-pane))
  (window-clear drei))

(defmethod (setf cursors) :around (new-cursors (drei drei-pane))
  (let ((old-cursors (cursors drei)))
    (call-next-method)
    (dolist (old-cursor old-cursors)
      (erase-output-record old-cursor drei nil))
    (dolist (new-cursor new-cursors)
      (stream-add-output-record drei new-cursor))))

(defmethod note-sheet-grafted :after ((pane drei-pane))
  (setf (stream-default-view pane) (view pane)))

;;; The fun is that in the gadget version of Drei, we do not control
;;; the application command loop, and in fact, need to operate
;;; completely independently of it - we can only act when our port
;;; deigns to bestow an event upon the gadget. So, we basically have
;;; to manually take care of reading gestures (asynchronously),
;;; redisplaying, updating the syntax and all the other fun details.
;;;
;;; On top of this, we have to account for the fact that some other
;;; part of the application might catch the user's fancy, and since we
;;; do not (and can not) control the command loop, we can not prevent
;;; the user from "leaving" the gadget at inconvenient times (such as
;;; in the middle of entering a complex set of gestures, or answering
;;; questions asked by a command). So, we keep some state information
;;; in the `drei-gadget-pane' object and use it to cobble together our
;;; own poor man's version of an ESA command loop. Syntax updating is
;;; done after a command has been executed, and only then (or by
;;; commands at their own discretion).
(defclass drei-gadget-pane (drei-pane value-gadget action-gadget
                                      asynchronous-command-processor
                                      dead-key-merging-command-processor)
  ((%currently-processing :initform nil
                          :accessor currently-processing-p)
   (%previous-focus :accessor previous-focus :initform nil
                    :documentation "The pane that previously had
keyboard focus"))
  (:metaclass modual-class)
  (:default-initargs
   :command-executor 'execute-drei-command)
  (:documentation "An actual, instantiable Drei gadget with
 event-based command processing."))

(defmethod initialize-instance :after ((drei drei-gadget-pane) &rest args)
  (declare (ignore args))
  ;; Heh, it seems that the :ACTIVE initarg steps over McCLIM's toes
  ;; and affects whether the gadget is active or not (which is
  ;; different from whether the Drei is active). It must be active by
  ;; default!
  (activate-gadget drei))

(defmethod gadget-value ((gadget drei-gadget-pane))
  ;; This is supposed to be a string, but a Drei buffer can contain
  ;; literal objects. We return a string if we can, an array
  ;; otherwise. This is a bit slow, as we cons up the array and then
  ;; probably a new one for the string, most of the time.
  (let ((contents (buffer-sequence (buffer (view gadget))
                                   0 (size (buffer (view gadget))))))
    (if (every #'characterp contents)
        (coerce contents 'string)
        contents)))

(defmethod (setf gadget-value) (new-value (gadget drei-gadget-pane)
                                &key (invoke-callback t))
  ;; I think we're supposed to permit this, even if the buffer is
  ;; non-editable.
  (letf (((read-only-p (buffer (view gadget))) nil))
    (performing-drei-operations (gadget :with-undo nil :redisplay nil)
      (delete-buffer-range (buffer (view gadget)) 0 (size (buffer (view gadget))))
      (insert-buffer-sequence (buffer (view gadget)) 0 new-value)))
  (when invoke-callback
    (value-changed-callback gadget
                            (gadget-client gadget)
                            (gadget-id gadget)
                            new-value)))

(defmethod armed-callback :after ((gadget drei-gadget-pane) client id)
  (declare (ignore client id))
  (setf (active gadget) t)
  (display-drei gadget))

(defmethod disarmed-callback :after ((gadget drei-gadget-pane) client id)
  (declare (ignore client id))
  (setf (active gadget) nil)
  (display-drei gadget))

(defgeneric handle-gesture (drei gesture)
  (:documentation "This generic function is called whenever a
Drei gadget variant has determined that a keyboard event
corresponds to a useful gesture that should be handled. A useful
gesture is, for example, one that is not simply a click on a
modifier key."))

(defun propagate-changed-value (drei)
  (when (modified-p (view drei))
    (when (gadget-value-changed-callback drei)
      (value-changed-callback drei
                              (gadget-client drei)
                              (gadget-id drei)
                              (gadget-value drei)))
    (setf (modified-p (view drei)) nil)))

(defmethod handle-gesture ((drei drei-gadget-pane) gesture)
  (let ((*command-processor* drei)
        (*abort-gestures* *esa-abort-gestures*)
        (*standard-input* drei))
    (accepting-from-user (drei)
      (handler-case (process-gesture drei gesture)
        (unbound-gesture-sequence (c)
          (display-message "~A is unbound" (gesture-name (gestures c))))
        (abort-gesture ()
          (display-message "Aborted")))
      (display-drei drei :redisplay-minibuffer t)
      (propagate-changed-value drei))))

;;; This is the method that functions as the entry point for all Drei
;;; gadget logic.
(defmethod handle-event ((gadget drei-gadget-pane) (event key-press-event))
  (unless (and (currently-processing-p gadget) (directly-processing-p gadget))
    (letf (((currently-processing-p gadget) t))
      (let ((gesture (convert-to-gesture event)))
        (when (proper-gesture-p gesture)
          (with-bound-drei-special-variables (gadget :prompt (format nil "~A " (gesture-name gesture)))
            (handle-gesture gadget gesture)))))))

(defmethod handle-event ((gadget drei-gadget-pane) (event pointer-button-press-event))
  (if (and (eql (event-modifier-state event) +shift-key+)
           (eql (pointer-event-button event) +pointer-middle-button+))
      (multiple-value-bind (content type presentp)
          (clime:request-selection gadget :primary 'string)
        (declare (ignore type))
        (when presentp
          (letf (((currently-processing-p gadget) t))
            (insert-sequence (point (view gadget)) content)
            (display-drei gadget :redisplay-minibuffer t)
            (propagate-changed-value gadget))))
      (call-next-method)))

(defmethod handle-event :before
    ((gadget drei-gadget-pane) (event pointer-button-press-event))
  (let ((previous (stream-set-input-focus gadget)))
    (when (and previous (typep previous 'gadget))
      (disarmed-callback previous (gadget-client previous) (gadget-id previous)))
    (armed-callback gadget (gadget-client gadget) (gadget-id gadget))))

(defmethod invoke-accepting-from-user ((drei drei-gadget-pane) (continuation function))
  ;; When an `accept' is called during the execution of a command for
  ;; the Drei gadget, we must deactivate the gadget in order to not
  ;; eat keyboard events.
  (unwind-protect (progn (disarmed-callback drei t t)
                         (funcall continuation))
    (armed-callback drei t t)))

(defmethod additional-command-tables append ((drei drei-gadget-pane)
                                             (table drei-command-table))
  `(exclusive-gadget-table
    ,(frame-command-table *application-frame*)))

(defclass drei-area (drei displayed-output-record region
                          command-processor
                          instant-macro-execution-mixin)
  ((%background-ink :initarg :background-ink
                    :reader background-ink
                    :initform +background-ink+)
   (%min-width :reader min-width
               :initarg :min-width
               :initform 0
               :documentation "The minimum width of the Drei
editable area. Should be an integer >= 0 or T, meaning that it
will extend to the end of the viewport, if the Drei area is in a
scrolling arrangement.")
   (%position :accessor area-position
              :initarg :area-position
              :documentation "The position of the Drei
editing area in the coordinate system of the encapsulated
stream. An (X,Y) list, not necessarily the same as the position
of the associated output record.")
   (%parent-output-record :accessor output-record-parent
                          :initarg :parent
                          :initform nil
                          :documentation "The parent output
record of the Drei area instance."))
  (:metaclass modual-class)
  (:default-initargs
   :command-executor 'execute-drei-command)
  (:documentation "A Drei editable area implemented as an output
record."))

(defmethod initialize-instance :after ((area drei-area)
				       &key x-position y-position)
  (check-type x-position number)
  (check-type y-position number)
  (setf (area-position area) (list x-position y-position)
        (extend-pane-bottom (view area)) t))

(defmethod (setf view) :after ((new-view drei-view) (drei drei-area))
  (setf (extend-pane-bottom new-view) t))

(defmethod (setf cursors) :after (new-cursors (drei drei-area))
  (dolist (new-cursor (cursors drei))
    (setf (output-record-parent new-cursor) drei)))

(defmethod esa-current-window ((drei drei-area))
  (editor-pane drei))

(defmethod display-drei ((drei drei-area) &rest args)
  (declare (ignore args))
  (display-drei-area drei))

(defmethod execute-drei-command ((drei drei-area) command)
  (let ((*standard-input* (or *minibuffer* *standard-input*)))
    (call-next-method)))

;;; Implementation of the displayed-output-record and region protocol
;;; for Drei areas. The redisplay-related stuff is in
;;; drei-redisplay.lisp.

(defmethod output-record-position ((record drei-area))
  (values-list (area-position record)))

(defmethod* (setf output-record-position) ((new-x number) (new-y number)
                                           (record drei-area))
  (multiple-value-bind (old-x old-y) (output-record-position record)
    (setf (area-position record) (list new-x new-y))
    (dolist (cursor (cursors record))
      (multiple-value-bind (cursor-x cursor-y) (output-record-position cursor)
        (setf (output-record-position cursor)
              (values (+ (- cursor-x old-x) new-x)
                      (+ (- cursor-y old-y) new-y)))))))

(defmethod output-record-start-cursor-position ((record drei-area))
  (output-record-position record))

(defmethod* (setf output-record-start-cursor-position) ((new-x number) (new-y number)
                                                       (record drei-area))
  (setf (output-record-position record) (values new-x new-y)))

(defmethod output-record-hit-detection-rectangle* ((record drei-area))
  (bounding-rectangle* record))

(defmethod output-record-refined-position-test ((record drei-area) x y)
  t)

(defmethod displayed-output-record-ink ((record drei-area))
  +foreground-ink+)

(defmethod output-record-children ((record drei-area))
  (cursors record))

(defmethod output-record-count ((record drei-area))
  (length (cursors record)))

(defmethod map-over-output-records-containing-position
    (function (record drei-area) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (dolist (cursor (cursors record))
    (when (region-contains-position-p cursor x y)
      (apply function cursor function-args))))

(defmethod map-over-output-records-overlapping-region
    (function (record drei-area) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (dolist (cursor (cursors record))
    (when (region-intersects-region-p cursor region)
      (apply function cursor function-args))))

(defmethod bounding-rectangle* ((drei drei-area))
  (with-accessors ((pane editor-pane)
                   (min-width min-width)) drei
    (let* ((style (medium-text-style pane))
           (style-width (text-style-width style pane))
           (height (text-style-height style pane)))
      (multiple-value-bind (x1 y1 x2 y2)
          (drei-bounding-rectangle* drei)
        (when (= x1 y1 x2 y2 0)
          ;; It hasn't been displayed yet, so stuff the position into
          ;; it...
          (setf x1 (first (area-position drei))
                y1 (second (area-position drei))))
        (values x1 y1
                (max x2 (+ x1 style-width)
                     (cond ((numberp min-width)
                            (+ x1 min-width))
                           ;; Must be T, then.
                           ((pane-viewport pane)
                            (+ x1 (bounding-rectangle-width (pane-viewport-region pane))))
                           (t 0)))
                (max y2 (+ y1 height)))))))

(defmethod replay-output-record :after ((drei drei-area) (stream extended-output-stream)
                                        &optional (x-offset 0) (y-offset 0) (region +everywhere+))
  (declare (ignore x-offset y-offset region))
  (dolist (cursor (cursors drei))
    (replay cursor stream)))

(defmethod recompute-extent-for-changed-child  ((drei drei-area) (child output-record)
                                                old-min-x old-min-y old-max-x old-max-y)
  nil)

(defmethod rectangle-edges* ((rectangle drei-area))
  (bounding-rectangle* rectangle))

(defmethod region-union ((region1 drei-area) (region2 region))
  (region-union (bounding-rectangle region1) region2))

(defmethod region-union ((region1 region) (region2 drei-area))
  (region-union region1 (bounding-rectangle region2)))

(defmethod region-intersection ((region1 drei-area) (region2 region))
  (region-intersection (bounding-rectangle region1) region2))

(defmethod region-intersection ((region1 region) (region2 drei-area))
  (region-intersection region1 (bounding-rectangle region2)))

(defmethod region-difference ((region1 drei-area) (region2 region))
  (region-difference (bounding-rectangle region1) region2))

(defmethod region-difference ((region1 region) (region2 drei-area))
  (region-difference region1 (bounding-rectangle region2)))

(defmethod transform-region (transformation (region drei-area))
  (transform-region transformation (bounding-rectangle region)))

;; For areas, we need to switch to ESA abort gestures after we have
;; left the CLIM gesture reading machinery, but before we start doing
;; ESA gesture processing.
(defmethod process-gesture :around ((command-processor drei-area) gesture)
  (let ((*abort-gestures* *esa-abort-gestures*))
    (call-next-method)))

(defmethod (setf active) :after (new-val (drei drei-area))
  (replay drei (editor-pane drei)))

(defmethod additional-command-tables append ((drei drei-area) (table drei-command-table))
  `(exclusive-input-editor-table))

(defclass drei-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
   :background +light-gray+ :max-height 20
   :height 20 :min-height 20))

(defclass drei-constellation (vrack-pane)
  ((drei :initform (error "A Drei instance must be provided for the constellation.")
         :accessor drei
         :initarg :drei)
   (minibuffer :initform (error "A minibuffer instance must be provided for the constellation.")
               :accessor minibuffer
               :initarg :minibuffer))
  (:documentation "A constellation of a Drei gadget instance and
  a minibuffer."))

(defmethod display-drei :after ((drei drei) &key redisplay-minibuffer)
  (when (and *minibuffer* redisplay-minibuffer)
    ;; We need to use :force-p t to remove any existing output from
    ;; the pane.
    (redisplay-frame-pane (pane-frame *minibuffer*) *minibuffer* :force-p t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programmer interface stuff
;;;
;;; We want it to be dead-easy to integrate Drei in CLIM applications.

;;; XXX This is brittle. If an :around method, that does funky
;;; (side-effecting) stuff, runs before this method, things might
;;; break. Let's hope nothing of that sort happens (this works in
;;; McCLIM. The method, not the hoping.)
(defmethod make-pane-1 :around (fm (frame application-frame)
                                   (type (eql :drei))
                                   &rest args &key
                                   (syntax nil) (initial-contents "")
                                   (minibuffer t) (border-width 1)
                                   (scroll-bars :horizontal)
                                   (drei-class 'drei-gadget-pane)
                                   (view 'textual-drei-syntax-view))
  (check-type initial-contents array)
  (check-type border-width integer)
  (check-type scroll-bars (member t :both :vertical :horizontal nil))
  (with-keywords-removed (args (:minibuffer :scroll-bars :border-width
                                                         :syntax :drei-class :view))
    (let* ((borderp (and border-width (plusp border-width)))
           (minibuffer-pane (cond ((eq minibuffer t)
                                   (make-pane 'drei-minibuffer-pane))
                                  ((typep minibuffer 'minibuffer-pane)
                                   minibuffer)
                                  ((null minibuffer)
                                   nil)
                                  (t (error "Provided minibuffer
is not T, NIL or a `minibuffer-pane'."))))
           (drei-pane (apply #'make-pane-1 fm frame drei-class
                       :minibuffer minibuffer-pane
                       :view (make-instance view)
                       args))
           (pane drei-pane)
           (view (view drei-pane)))
      (letf (((read-only-p (buffer view)) nil))
        (insert-buffer-sequence (buffer view) 0 initial-contents))
      (if syntax
          (setf (syntax view)
                (make-instance (or (when (syntaxp syntax)
                                     syntax)
                                   (syntax-from-name (string syntax))
                                   (error "Syntax ~A not found" (string syntax)))
                 :buffer (buffer view))))
      (when scroll-bars
        (setf pane (scrolling (:scroll-bar scroll-bars)
                     pane)))
      (when minibuffer
        (setf pane (make-pane 'drei-constellation
                    :drei drei-pane
                    :minibuffer minibuffer-pane
                    :contents (list pane minibuffer-pane))))
      (when borderp
        (setf pane (climi::bordering
                       (:border-width border-width)
                     pane)))
      pane)))
