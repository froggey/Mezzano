;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000 by
;;; Arthur Lemmens (lemmens@simplex.nl),
;;; Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;; and Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;; Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2006 by Troels Henriksen (athas@sigkill.dk)

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

;;; This file contains the concrete implementation of the text-field
;;; and text-editor gadgets. It is loaded rather late, because it
;;; requires Drei.

(in-package :clim-internals)

;;; The text editor gadget(s) is implemented as a class implementing
;;; the text editor gadget protocol, but containing an editor
;;; substrate object that takes care of the actual editing logic,
;;; redisplay, etc. The substrates need to be gadgets themselves and
;;; are defined here.

(defparameter *default-text-field-text-style*
  (make-text-style :fix :roman :normal))

(defclass editor-substrate-mixin (value-gadget)
  ((activation-gestures :reader activation-gestures
                        :initarg :activation-gestures)
   (user :reader user-gadget
         :initarg :user-gadget
         :documentation "The editor gadget using this editor substrate."
         :initform (error "Editor substrates must have a user.")))
  (:documentation "A mixin class for text editor gadget substrates.")
  (:default-initargs :activation-gestures '()))

(defmethod gadget-id ((gadget editor-substrate-mixin))
  (gadget-id (user-gadget gadget)))

(defmethod (setf gadget-id) (value (gadget editor-substrate-mixin))
  (setf (gadget-id (user-gadget gadget)) value))

(defmethod gadget-client ((gadget editor-substrate-mixin))
  (gadget-client (user-gadget gadget)))

(defmethod (setf gadget-client) (value (gadget editor-substrate-mixin))
  (setf (gadget-client (user-gadget gadget)) value))

(defmethod gadget-armed-callback ((gadget editor-substrate-mixin))
  (gadget-armed-callback (user-gadget gadget)))

(defmethod gadget-disarmed-callback ((gadget editor-substrate-mixin))
  (gadget-disarmed-callback (user-gadget gadget)))

(defclass text-field-substrate-mixin (editor-substrate-mixin)
  ()
  (:documentation "A mixin class for editor substrates used for text field gadgets."))

(defclass text-editor-substrate-mixin (editor-substrate-mixin)
  ((ncolumns :reader text-editor-ncolumns
             :initarg :ncolumns
             :initform nil
             :type (or null integer))
   (nlines :reader text-editor-nlines
           :initarg :nlines
           :initform nil
           :type (or null integer)))
  (:documentation "A mixin class for editor substrates used for text editor gadgets."))

;;; Now, define the Drei substrate.

(defclass drei-editor-substrate (drei:drei-gadget-pane
                                 editor-substrate-mixin)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "A class for Drei-based editor substrates."))

(defmethod (setf gadget-value) :after (value (gadget drei-editor-substrate)
                                             &key invoke-callback)
  (declare (ignore invoke-callback))
  ;; Hm! I wonder if this can cause trouble.  I think not.
  (drei:display-drei gadget))

(defclass drei-text-field-substrate (text-field-substrate-mixin
                                     drei-editor-substrate)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "The class for Drei-based text field substrates."))

(defmethod drei:handle-gesture ((drei drei-text-field-substrate) gesture)
  (if (with-activation-gestures ((activation-gestures drei))
        (activation-gesture-p gesture))
      (activate-callback drei (gadget-client drei) (gadget-id drei))
      (call-next-method)))

(defmethod compose-space ((pane drei-text-field-substrate) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let ((width (text-size medium (gadget-value pane)))
	  (height (+ (stream-vertical-spacing pane)
		     (text-style-height (medium-text-style medium) medium))))
      (make-space-requirement :height height :min-height height :max-height height
			      :width width :min-width width))))

(defclass drei-text-editor-substrate (text-editor-substrate-mixin
                                      drei-editor-substrate)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "The class for Drei-based text editor substrates."))

(defmethod compose-space ((pane drei-text-editor-substrate) &key width height)
  (with-sheet-medium (medium pane)
    (let* ((text-style (medium-text-style medium))
           (line-height (+ (text-style-height text-style medium)
                           (stream-vertical-spacing pane)))
           (column-width (text-style-width text-style medium)))
      (with-accessors ((ncolumns text-editor-ncolumns)
                       (nlines text-editor-nlines)) pane
        (let ((width (if ncolumns
                         (+ (* ncolumns column-width))
                         width))
              (height (if nlines
                          (+ (* nlines line-height))
                          height)))
          (space-requirement-combine* #'(lambda (req1 req2)
                                          (or req2 req1))
                                      (call-next-method)
                                      :width width :max-width width :min-width column-width
                                      :height height :max-height height :min-height line-height))))))

(defmethod allocate-space ((pane drei-text-editor-substrate) w h)
  (resize-sheet pane w h))

;;; The class for using these substrates in the gadgets.

(defclass editor-substrate-user-mixin (value-gadget)
  ((substrate :accessor substrate
              :documentation "The editing substrate used for this text field."))
  (:documentation "A mixin class for creating gadgets using editor substrates."))

(defmethod gadget-value ((gadget editor-substrate-user-mixin))
  (gadget-value (substrate gadget)))

(defmethod (setf gadget-value) (value (gadget editor-substrate-user-mixin)
                                &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (gadget-value (substrate gadget)) value))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.8 The concrete text-field Gadget

(defclass text-field-pane (text-field
                           vrack-pane
                           editor-substrate-user-mixin)
  ((activation-gestures :accessor activation-gestures
                        :initarg :activation-gestures
                        :documentation "A list of gestures that
cause the activate callback to be called."))
  (:default-initargs
   :value ""
   :activation-gestures *standard-activation-gestures*))

(defmethod initialize-instance :after ((object text-field-pane)
                                       &key id client armed-callback
                                       disarmed-callback
                                       activation-gestures activate-callback
                                         value value-changed-callback
                                         (editable-p t))
  ;; Make an editor substrate object for the gadget.
  (let ((pane (make-pane 'drei-text-field-substrate
                         :user-gadget object
                         :id id
                         :client client
                         :text-style (or (pane-text-style object)
                                         *default-text-field-text-style*)
                         :armed-callback armed-callback
                         :disarmed-callback disarmed-callback
                         :activation-gestures activation-gestures
                         :activate-callback activate-callback
                         :value value
                         :value-changed-callback value-changed-callback
                         :editable-p editable-p)))
    (setf (gadget-value pane) value
          (substrate object) pane)
    (sheet-adopt-child object pane)))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.9 The concrete text-editor Gadget

(defclass text-editor-pane (text-editor
                            vrack-pane
                            editor-substrate-user-mixin)
  ()
  (:default-initargs :activation-gestures '()))

(defmethod initialize-instance :after ((object text-editor-pane)
                                       &key armed-callback
                                         disarmed-callback
                                         activation-gestures activate-callback
                                         scroll-bars
                                         ncolumns nlines
                                         value
                                         (editable-p t))
  ;; Make an editor substrate object for the gadget. Propagate the
  ;; substrate's value-changed callback to our own
  ;; `value-changed-callback' method.
  (let* ((minibuffer (when scroll-bars
                       (make-pane 'drei::drei-minibuffer-pane)))
         (substrate (make-pane 'drei-text-editor-substrate
                               :user-gadget object
                               :minibuffer minibuffer
                               :text-style (pane-text-style object)
                               :armed-callback armed-callback
                               :disarmed-callback disarmed-callback
                               :activation-gestures activation-gestures
                               :activate-callback activate-callback
                               :scroll-bars scroll-bars
                               :ncolumns ncolumns
                               :nlines nlines
                               :value-changed-callback
                               (lambda (gadget value)
                                 (declare (ignore gadget))
                                 (value-changed-callback
                                  object (gadget-client object) (gadget-id object)
                                  value))
                               :editable-p editable-p))
         (sheet (cond ((and scroll-bars minibuffer)
                       (vertically ()
                         (scrolling (:scroll-bars scroll-bars)
                           substrate)
                         minibuffer))
                      (scroll-bars
                       (scrolling (:scroll-bars scroll-bars)
                         substrate))
                      (minibuffer
                       (vertically ()
                         substrate
                         minibuffer))
                      (:otherwise substrate))))
    (setf (substrate object) substrate
          (gadget-value substrate) value)
    (sheet-adopt-child object sheet)))
