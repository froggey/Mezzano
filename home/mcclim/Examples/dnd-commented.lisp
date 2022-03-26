;;;;  Copyright (c) 2019 Daniel Kochmański
;;;;
;;;;    License: BSD-2-Clause.

;;;  In this demo I'm going to present a few ways how drag-and-drop
;;;  behavior could be added to the application.


(defpackage #:clim-demo.drag-and-drop-example
  (:use #:clim-lisp #:clim)
  (:export #:dnd-commented))
(in-package #:clim-demo.drag-and-drop-example)


;;;  CUSTOM-PANE is built outside of CLIM abstractions which are
;;;  arranged around streams and output records. It will draft how
;;;  drag-and-drop could be implemented from scratch.
(defclass custom-pane (basic-pane clime:always-repaint-background-mixin)
  ((pos-x :initform 50)
   (pos-y :initform 50)
   (dragp :initform nil)
   (color :initform +dark-red+ :accessor color)
   (counter :initform 0 :accessor counter)))

;;; Repaint draws a circle and a click counter in a center of it. When
;;; it is dragged then only its outline is shown.
(defmethod handle-repaint ((client custom-pane) region)
  (with-slots (pos-x pos-y color counter dragp) client
    (window-clear client)
    (draw-circle* client pos-x pos-y 25 :ink color
                  :filled (not dragp)
                  :line-thickness 3 :line-dashes t)
    (draw-text* client (format nil "~s" counter) pos-x pos-y
                :align-x :center :align-y :center
                :text-size :huge)))

;;; Pressing button anywhere enabled dragging and randomly changes the
;;; circle color. After that it repaints the sheet.
(defmethod handle-event ((client custom-pane) (event pointer-button-press-event))
  (with-slots (color counter dragp) client
    (setf color (make-contrasting-inks 8 (random 8)))
    (setf dragp t)
    (incf counter 1))
  (repaint-sheet client +everywhere+))

;;; Releasing button anywhere stops dragging.
(defmethod handle-event ((client custom-pane) (event pointer-button-release-event))
  (with-slots (dragp) client
    (setf dragp nil))
  (repaint-sheet client +everywhere+))

;;; When dragging is T and pointer is moving around then we update the
;;; circle position to follow the cursor.
(defmethod handle-event ((client custom-pane) (event pointer-motion-event))
  (with-slots (pos-x pos-y dragp) client
    (when dragp
      (setf pos-x (pointer-event-x event)
            pos-y (pointer-event-y event))
      (repaint-sheet client +everywhere+))))


;;; We define multiple sheets for testing what happens when dragging
;;; pointer moves the object over varioud kinds of sheets.
(define-application-frame dnd-commented ()
  ()
  (:panes (int :interactor :min-width 400)
          (app :application :display-function #'display)
          (scr :application :display-function #'display)
          (hlp :application :display-function 'show-help
               :end-of-line-action :wrap*
               :end-of-page-action :allow
               :scroll-bars :vertical
               :display-time t)
          (cmd :command-menu)
          (pan custom-pane))
  (:menu-bar t)
  (:pointer-documentation t)
  (:layouts (default
             (vertically ()
               (horizontally ()
                 (vertically ()
                   (labelling (:label "Custom") pan)
                   (labelling (:label "Description") hlp))
                 (vertically ()
                   (labelling (:label "Command menu") cmd)
                   (labelling (:label "Scratchpad") scr))
                 (labelling (:label "Application") app)
                 (labelling (:label "Interactor") int))))
            (custom
             (vertically ()
               (horizontally ()
                 (labelling (:label "Scratchpad") scr)
                 (labelling (:label "Command menu") cmd)
                 (labelling (:label "Custom") pan)
                 (labelling (:label "Application")  app)                 )
               (horizontally ()
                 (labelling (:label "Interactor")   int)
                 (labelling (:label "Description")  hlp))))))


(defun show-help (frame stream)
  (declare (ignore frame))
  (with-text-family (stream :fix)
    (format stream "~
~
This demo is is made to present various approaches to implementing ~
drag and drop. See the source code for inline comments describing ~
implementation. We explore four different approaches: custom panes ~
which handle events and repaint all by themself, TRACKING-POINTER ~
semi-automated approach, dragging-output (and draw-output-record) and ~
finally drag-and-drop translators.

Custom pane
-----------

Ad-hoc implementation not concerning itself with any interoperation ~
with presentations and typed input. Pane is not a stream. Clicking ~
anywhere on the pane starts dragging and releasing the button ends ~
it. All work is done in two methods: HANDLE-REPAINT and HANDLE-EVENT.

Pointer tracking
----------------

Macro TRACKING-POINTER is a basic block for implementing drag-and-drop ~
in applications. It allows specifying input context and drawing may be ~
done from handlers which are assigned to different event types (like ~
pointer motion). Using POINTER-TRACKING and POINTER-TRACKING* commands ~
disables other CLIM processing and allows dragging \"What-X\" elements ~
to the \"What acceptor\". The difference between to commands is that ~
one allows dragging across different sheets while the other doesn't ~
allow that.

Dragging-output
---------------

Dragging-output is built on top of TRACKING-POINTER. It allows moving ~
output records and see a visible feedback. To test it click on the ~
cross and click at its destination. Circle should be shown as a ~
feedback. No further action is done.

Drag and drop translators
-------------------------

These translators in principle should allow producing an object of ~
type C by dragging A towards B. Translator body is responsible for ~
producing the object. Common use for that is returning a command for ~
which there is an active input-context from DEFAULT-FRAME-TOP-LEVEL.

In demo we have two base presentations: FIGURE and BASKET. We put ~
figures to their corresponding baskets (CIRCLEs to CIRCLE-BASKET, ~
SQUAREs to SQUARE-BASKET, TRIANGLEs to TRIANGLE-BASKET). There is a ~
general purpose basket called FIGURE-BASKET to which all figures ~
should fit. That's a basic operation mode.

Gray circles do not inherit from FIGURE and they are not accepted in ~
figure baskets. There is a GREY-CIRCLE-ACCEPTOR for them. We define ~
two additional compound (OR-ed) presentation types:

SIDEFUL-FIGURE  -- (or SQUARE TRIANGLE)
SIDELESS-FIGURE -- (or CIRCLE GREY-CIRCLE)

For both these types there are SIDEFUL-FIGURE-BASKET and ~
SIDLESS-FIGURE-ACCEPTOR.

Known issues
------------

- Compound destination types can't be dropped onto.

Fixing that requires reworking frame-drag-and-drop method to accept ~
any presentation in tracking-pointer and check type as we go.")))

;;; Switching layout rearranges panes.
(define-dnd-commented-command (com-rotate-layout :name t :menu t :keystroke (#\r :control)) ()
  (ecase (frame-current-layout *application-frame*)
    (default (setf (frame-current-layout *application-frame*) 'custom))
    (custom (setf (frame-current-layout *application-frame*) 'default))))

;;; Dummy command printing a string in the interactor.
(define-dnd-commented-command (com-print-string :name t) ((a string))
  (format (find-pane-named *application-frame* 'int) a))


;;; Pointer tracking

;;; WHAT-1 and WHAT-2 are presentation types disjoint from the figure.
(define-presentation-type what-1   ())
(define-presentation-type what-2   ())
(define-presentation-type what-3   ())
(define-presentation-type what-acceptor ())

(define-presentation-method present (obj (type what-1) stream view &key)
  (surrounding-output-with-border (stream :shape :drop-shadow :ink +dark-red+)
    (draw-text* stream "WHAT-1" 0 0 :align-y :top :ink +dark-red+)))

(define-presentation-method present (obj (type what-2) stream view &key)
  (surrounding-output-with-border (stream :shape :drop-shadow :ink +dark-grey+)
    (draw-text* stream "WHAT-2" 0 0 :align-y :top :ink +dark-red+)))

(define-presentation-method present (obj (type what-3) stream view &key)
  (surrounding-output-with-border (stream :shape :drop-shadow :ink +dark-grey+)
    (draw-text* stream "WHAT-3" 0 0 :align-y :top :ink +dark-grey+)))

(define-presentation-method present (obj (type what-acceptor) stream view &key)
  (draw-rectangle* stream 0 0 250 40 :line-dashes t :filled nil :ink +dark-red+)
  (draw-text* stream "WHAT Acceptor" 125 20
              :align-x :center :align-y :center
              :text-size :normal
              :ink +dark-red+))

;;; Pointer tracking
;;;
;;; McCLIM dragging is built on top of the pointer tracking. We may
;;; use directly TRACKING-POINTER macro but more convenient way is to
;;; depend on DRAG-OUTPUT-RECORD method or DRAGGING-OUTPUTQ macro.

(defun com-pointer-tracking-internal (multi)
  (let ((cursor-state :motion)
        (damaged-sheet nil)
        (damaged-region +nowhere+))

    (labels ((draw-cursor-state (&optional new-state)
               (when new-state
                 (setf cursor-state new-state))
               (ecase cursor-state
                 (:motion +black+)
                 (:clicked +red+)
                 (:dragging +cyan+)))

             (draw-cursor (sheet x y)
               (when damaged-sheet (handle-repaint damaged-sheet damaged-region))
               (let ((rect (make-rectangle* (- x 5) (- y 5) (+ x 5) (+ y 5))))
                 (if (output-recording-stream-p sheet)
                     (with-output-recording-options (sheet :record nil :draw t)
                       (draw-design sheet rect :ink (draw-cursor-state)))
                     (draw-design sheet rect :ink (draw-cursor-state)))
                 (setf damaged-sheet sheet
                       damaged-region rect)))

             (damage-region (region)
               (setf damaged-region (region-union damaged-region region))))
      (window-clear *pointer-documentation-output*)
      (format *pointer-documentation-output* "Press SPACE to end TRACKING-POINTER.")
      (finish-output *pointer-documentation-output*)
      (let ((tracked-sheet (find-pane-named *application-frame* 'scr)))
        (tracking-pointer (tracked-sheet :context-type '(or what-1 what-2 what-acceptor)
                                         :multiple-window multi)
          (:pointer-button-press
           (&key event x y)
           (draw-cursor-state :clicked)
           (draw-cursor (event-sheet event) x y))
          (:pointer-button-release
           (&key event x y)
           (draw-cursor-state :motion)
           (draw-cursor (event-sheet event) x y))
          (:pointer-motion
           (&key window x y)
           (draw-cursor window x y))
          (:presentation-button-press
           (&key presentation event x y)
           (when (presentation-subtypep (presentation-type presentation) '(or what-1 what-2))
             (draw-cursor-state :dragging))
           (draw-cursor (event-sheet event) x y))
          (:presentation-button-release
           (&key presentation event x y)
           (when (presentation-subtypep (presentation-type presentation) 'what-acceptor)
             #|do something|#)
           (draw-cursor-state :motion)
           (draw-cursor (event-sheet event) x y))
          (:presentation
           (&key presentation window x y)
           (draw-cursor window x y)
           (with-bounding-rectangle* (x1 y1 x2 y2) presentation
             (let ((dx (/ (+ x1 x2) 2))
                   (dy (/ (+ y1 y2) 2)))
               (with-output-recording-options (window :record nil)
                 (draw-arrow* window (+ dx 32) (+ dy 32) dx dy :ink +red+ :line-thickness 2))
               (let ((rect (make-rectangle* dx dy (+ dx 32) (+ dy 32))))
                 (damage-region rect)))))
          (:keyboard
           (&key gesture)
           (case (keyboard-event-character gesture)
             (#\space
              (window-clear *pointer-documentation-output*)
              (return-from com-pointer-tracking-internal)))))))))

(define-dnd-commented-command
    (com-pointer-tracking :menu t)
    ()
  (com-pointer-tracking-internal t))

(define-dnd-commented-command
    (com-pointer-tracking* :menu t)
    ()
  (com-pointer-tracking-internal nil))


;;; Figure is a supertype for square, circle and triangle. Then we
;;; define an union types for figure depending on whenever it has
;;; sides.

(define-presentation-type figure   ())
(define-presentation-type square   () :inherit-from 'figure)
(define-presentation-type circle   () :inherit-from 'figure)
(define-presentation-type triangle () :inherit-from 'figure)
;;; disconnected from figure inheritance hierarchy
(define-presentation-type grey-circle ())
(define-presentation-type-abbreviation sideful-figure ()
  `(or square triangle))
(define-presentation-type-abbreviation sideless-figure ()
  `(or circle grey-circle))

(define-presentation-method present (obj (type square) stream view &key)
  (draw-design stream (make-rectangle* 10 10 30 30) :ink +dark-magenta+))

(define-presentation-method present (obj (type grey-circle) stream view &key)
  (draw-design stream (make-ellipse* 20 20 0 10 10 0) :ink +dark-grey+))

(define-presentation-method present (obj (type circle) stream view &key)
  (draw-design stream (make-ellipse* 20 20 0 10 10 0) :ink +dark-green+))

(define-presentation-method present (obj (type triangle) stream view &key)
  (draw-design stream (make-polygon* '(10 30 30 30 20 10)) :ink +dark-blue+))

(define-presentation-type basket ())
(define-presentation-type figure-basket ()            :inherit-from 'basket)
(define-presentation-type square-basket ()            :inherit-from 'basket)
(define-presentation-type circle-basket ()            :inherit-from 'basket)
(define-presentation-type triangle-basket ()          :inherit-from 'basket)
(define-presentation-type sideful-figure-basket ()    :inherit-from 'basket)
(define-presentation-type grey-circle-acceptor ()     :inherit-from 'basket)
(define-presentation-type sideless-figure-acceptor () :inherit-from 'basket)

(define-presentation-method present (obj (type grey-circle-acceptor) stream view &key)
  (draw-rectangle* stream 0 0 250 40 :line-dashes t :filled nil :ink +dark-grey+)
  (draw-text* stream "Grey circle acceptor" 125 20
              :align-x :center :align-y :center
              :text-size :normal))

(define-presentation-method present (obj (type sideless-figure-acceptor) stream view &key)
  (draw-rectangle* stream 0 0 250 40 :line-dashes t :filled nil :ink +dark-grey+)
  (draw-text* stream "Sideless figure acceptor" 125 20
              :align-x :center :align-y :center
              :text-size :normal))

(define-presentation-method present (obj (type basket) stream view &key)
  (draw-rectangle* stream 0 0 250 40 :line-dashes t :filled nil :ink +dark-blue+)
  (draw-text* stream (format nil "~a" type) 125 20
              :align-x :center :align-y :center
              :text-size :normal :ink +dark-blue+))

;;; dragging-output macro
(define-presentation-type cross ())
(define-presentation-type cross* ())

(define-presentation-method present (obj (type cross) stream view &key)
  (draw-rectangle* stream 0 20 50 30 :filled nil)
  (draw-rectangle* stream 20 0 30 50 :filled nil))

(define-presentation-method present (obj (type cross*) stream view &key)
  (draw-rectangle* stream 0 20 50 30 :filled nil :ink +dark-red+)
  (draw-rectangle* stream 20 0 30 50 :filled nil :ink +dark-red+))

(defun get-pointer-position (pane)
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

(define-dnd-commented-command (com-drag-cross) ((original cross))
  (declare (ignore original))
  (let ((stream (find-pane-named *application-frame* 'scr)))
    (dragging-output (stream :multiple-window t)
      (draw-circle stream (get-pointer-position stream) 20 :filled nil))))

(define-presentation-to-command-translator tr-drag-cross
    (cross com-drag-cross dnd-commented :echo nil)
    (object)
  (list object))


;;; drag-and-drop translators
(define-drag-and-drop-translator drag-figure (figure command figure-basket dnd-commented)
    (object destination-object)
  `(com-print-string ,(format nil "~s -> ~s" object destination-object)))

(define-drag-and-drop-translator drag-triangle (triangle command triangle-basket dnd-commented)
    (object destination-object)
  `(com-print-string ,(format nil "~s -> ~s" object destination-object)))

(define-drag-and-drop-translator drag-square (square command square-basket dnd-commented)
    (object destination-object)
  `(com-print-string ,(format nil "~s -> ~s" object destination-object)))

(define-drag-and-drop-translator drag-circle (circle command circle-basket dnd-commented)
    (object destination-object)
  `(com-print-string ,(format nil "~s -> ~s" object destination-object)))

(define-drag-and-drop-translator drag-circle*
    (grey-circle command grey-circle-acceptor dnd-commented)
    (object destination-object)
  `(com-print-string ,(format nil "~s -> ~s" object destination-object)))

(define-drag-and-drop-translator drag-sideful
    (sideful-figure command sideful-figure-basket dnd-commented)
    (object destination-object)
  `(com-print-string ,(format nil "~s -> ~s" object destination-object)))

(define-drag-and-drop-translator drag-sideless
    (sideless-figure command sideless-figure-acceptor dnd-commented)
    (object destination-object)
  `(com-print-string ,(format nil "~s -> ~s" object destination-object)))


(defmethod display ((frame dnd-commented) pane)
  (with-translation (pane 10 10)
    (dotimes (i 3)
      (dotimes (j 3)
        (with-translation (pane (* i 30) (* j 30))
          (present (gensym "square") 'square :stream pane)))))

  (with-translation (pane 110 10)
    (dotimes (i 3)
      (dotimes (j 3)
        (with-translation (pane (* i 30) (* j 30))
          (present (gensym "circle")
                   (if (< i 2)
                       'circle
                       'grey-circle)
                   :stream pane)))))

  (with-translation (pane 10 110)
    (dotimes (i 3)
      (dotimes (j 3)
        (with-translation (pane (* i 30) (* j 30))
          (present (gensym "triangle") 'triangle :stream pane)))))

  (with-translation (pane 130 130)
    (present (gensym "what-1") 'what-1 :stream pane)
    (with-translation (pane 0 25) (present (gensym "what-2") 'what-2 :stream pane))
    (with-translation (pane 0 50) (present (gensym "what-3") 'what-3 :stream pane)))

  (with-translation (pane 200 130)
    (present (gensym "cross") 'cross :stream pane))

  (loop
     for y from 210 by 50
     for basket in '(what-acceptor
                     grey-circle-acceptor
                     sideless-figure-acceptor
                     basket
                     figure-basket
                     square-basket
                     circle-basket
                     triangle-basket
                     sideful-figure-basket)
     do (with-translation (pane 10 y)
          (present basket basket :stream pane :single-box t))))
