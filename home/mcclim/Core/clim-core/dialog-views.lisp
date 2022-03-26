;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2005 by Tim Moore (moore@bricoworks.com)
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

;;; Classes for the gadget dialog views. Eventually.

;;; Views described in the Franz User manual (CLIM 2.2). Sec. 8.6.13

(macrolet
    ((define-gadget-view (name)
       ;; The gadget class stores the information required to make an
       ;; instances of its corresponding gadget class: the name of the
       ;; gadget class and a list of initargs. The INITIALIZE-INSTANCE
       ;; method ensures that the view only accepts initargs also
       ;; accepted by the gadget class.
       (let* ((class-name (alexandria:symbolicate name '-view))
              (variable-name (alexandria:symbolicate '+ name '-view+))
              (class (c2mop:ensure-finalized (find-class name)))
              (slots (c2mop:class-slots class))
              (slot-initargs (alexandria:mappend #'c2mop:slot-definition-initargs slots))
              (default-initargs (map 'list #'first (c2mop:class-default-initargs class)))
              ;; REMOVE-DUPLICATES is a workaround for an SBCL bug
              (allowed-initargs (union (remove-duplicates slot-initargs) default-initargs))
              (parameters (map 'list (alexandria:compose #'intern #'string)
                               allowed-initargs)))
         `(progn
            (defclass ,class-name (gadget-view)
              ((gadget-name :allocation :class :reader view-gadget-name
                            :initform ',name)
               (gadget-initargs :accessor view-gadget-initargs)))
            (defmethod initialize-instance :after
                ((instance ,class-name)
                 &rest initargs &key ,@parameters)
              (declare (ignore ,@parameters))
              (setf (view-gadget-initargs instance) initargs))
            (defvar ,variable-name (make-instance ',class-name))
            ',name))))

  (define-gadget-view toggle-button)
  (define-gadget-view push-button)
  (define-gadget-view radio-box)
  (define-gadget-view check-box)
  (define-gadget-view slider)
  (define-gadget-view text-field)
  (define-gadget-view text-editor)
  (define-gadget-view list-pane)
  (define-gadget-view option-pane))

(defmethod make-gadget-pane-from-view ((view gadget-view) stream &rest initargs)
  (let ((frame *application-frame*)
        (initargs (append initargs (view-gadget-initargs view))))
    (with-look-and-feel-realization ((frame-manager frame) frame)
      (apply #'make-pane (view-gadget-name view) initargs))))

(defmethod make-output-record-from-view ((view gadget-view) stream query-identifier &rest initargs)
  (updating-output (stream
                    :cache-value t      ; don't redisplay
                    :unique-id query-identifier
                    :record-type 'accepting-values-record)
    (let ((gadget (apply #'make-gadget-pane-from-view view stream initargs)))
      (with-output-as-gadget (stream)
        gadget))))

(defun %standard-value-changed-callback (query-identifier &optional value-transform)
  ;; Return a function to be used as value-changed-callback of a
  ;; gadget.  The returned function changes the value of the query
  ;; associated with the gadget inside a dialog. The query is
  ;; identified by QUERY-IDENTIFIER.  If VALUE-TRANSFORM is NIL the
  ;; new value of the query will be the value of the gadget, otherwise
  ;; the VALUE-TRANSFORM function will be called with the value of the
  ;; gadget as argument and the returned value will be the new value
  ;; of the query.
  (lambda (pane value)
    (declare (ignore pane))
    (let ((new-value (if value-transform
                         (funcall value-transform value)
                         value)))
      (throw-object-ptype `(com-change-query ,query-identifier ,new-value)
                          '(command :command-table accept-values)))))

;;; Use textual-dialog-view as default for Views not implemented

(define-default-presentation-method accept-present-default
    (type stream (view gadget-view) default default-supplied-p
          present-p query-identifier)
  (funcall-presentation-generic-function accept-present-default
                                         type
                                         stream
                                         +textual-dialog-view+
                                         default default-supplied-p
                                         present-p
                                         query-identifier))

(define-default-presentation-method accept
    (type stream (view gadget-view) &key default default-type)
  (funcall-presentation-generic-function accept
                                         type
                                         stream
                                         +textual-dialog-view+
                                         :default default
                                         :default-type default-type))

;;; toggle-button-view

(define-presentation-method accept-present-default
    ((type boolean) stream (view toggle-button-view) default default-supplied-p
     present-p query-identifier)
  (unless default-supplied-p
    (setq default nil))
  (make-output-record-from-view view stream query-identifier
                                :value default
                                :value-changed-callback
                                (%standard-value-changed-callback query-identifier)))

;;; radio-box-view

(define-presentation-method accept-present-default
    ((type completion) stream (view radio-box-view) default default-supplied-p
     present-p query-identifier)
  (let* ((buttons (loop for choice in sequence
                        collect (make-pane 'toggle-button
                                           :indicator-type :one-of
                                           :id (funcall value-key choice)
                                           :label (funcall name-key choice))))
         (selection (find default buttons :test test :key #'gadget-id)))
    (make-output-record-from-view view stream query-identifier
                                  :choices buttons
                                  :current-selection selection
                                  :value-changed-callback
                                  (%standard-value-changed-callback
                                   query-identifier #'gadget-id))))

;;; check-box-view

(define-presentation-method accept-present-default
    ((type subset-completion) stream (view check-box-view) default default-supplied-p
     present-p query-identifier)
  (let* ((buttons (loop for choice in sequence collect
                      (make-pane 'toggle-button
                                 :indicator-type :some-of
                                 :id (funcall value-key choice)
                                 :label (funcall name-key choice))))
         (selection (remove-if-not (lambda (x)
                                     (find (gadget-id x) default :test test))
                                   buttons)))
    (make-output-record-from-view view stream query-identifier
                                  :choices buttons
                                  :current-selection selection
                                  :value-changed-callback
                                  (%standard-value-changed-callback
                                   query-identifier (lambda (item)
                                                      (map 'list #'gadget-id item))))))

;;; option-pane-view

(define-presentation-method accept-present-default
    ((type completion) stream (view option-pane-view) default default-supplied-p
     present-p query-identifier)
  (flet ((generate-callback (query-identifier)
           (lambda (pane item)
             (declare (ignore pane))
             (when *accepting-values-stream*
               (when-let ((query (find query-identifier
                                       (queries *accepting-values-stream*)
                                       :key #'query-identifier :test #'equal)))
                 (setf (value query) item)
                 (setf (changedp query) t))))))
    (make-output-record-from-view view stream query-identifier
                                  :mode :exclusive
                                  :test test
                                  :value default
                                  :name-key name-key
                                  :value-key value-key
                                  :items sequence
                                  :value-changed-callback (generate-callback query-identifier))))

;;; list-pane-view

(define-presentation-method accept-present-default
    ((type completion) stream (view list-pane-view) default default-supplied-p
     present-p query-identifier)
  (make-output-record-from-view view stream query-identifier
                                :mode :exclusive
                                :test test
                                :value default
                                :name-key name-key
                                :value-key value-key
                                :items sequence
                                :value-changed-callback
                                (%standard-value-changed-callback query-identifier)))

(define-presentation-method accept-present-default
    ((type subset-completion) stream (view list-pane-view) default default-supplied-p
     present-p query-identifier)
  (make-output-record-from-view view stream query-identifier
                                :mode :nonexclusive
                                :test test
                                :value default
                                :name-key name-key
                                :value-key value-key
                                :items sequence
                                :value-changed-callback
                                (%standard-value-changed-callback query-identifier)))

;;; slider-view

(define-presentation-method accept-present-default
    ((type real) stream (view slider-view) default default-supplied-p
     present-p query-identifier)
  (make-output-record-from-view view stream query-identifier
                                :value default
                                :max-value high ; presentation parameter
                                :min-value low ; presentation parameter
                                :show-value-p t
                                :value-changed-callback (%standard-value-changed-callback query-identifier)))

(define-presentation-method accept-present-default
    ((type integer) stream (view slider-view) default default-supplied-p
     present-p query-identifier)
  (make-output-record-from-view view stream query-identifier
                                :value default
                                :max-value high ; presentation parameter
                                :min-value low ; presentation parameter
                                :show-value-p t
                                :number-of-quanta (- high low)
                                :value-changed-callback
                                (%standard-value-changed-callback query-identifier #'round)))

;;; text-field

(define-presentation-method accept-present-default
    ((type string) stream (view text-field-view) default default-supplied-p
                   present-p query-identifier)
  (unless default-supplied-p
    (setf default ""))
  (let* ((width (or (getf (view-gadget-initargs view) :width)
                    (- (stream-cursor-final-position stream)
                       (stream-cursor-initial-position stream))))
         (gadget (make-gadget-pane-from-view view stream
                                             :width width
                                             :value default
                                             :value-changed-callback
                                             (%standard-value-changed-callback query-identifier))))
    (updating-output (stream
                      :cache-value t    ; don't redisplay
                      :unique-id query-identifier
                      :record-type 'accepting-values-record)
      (with-output-as-presentation (stream query-identifier 'selectable-query
                                           :single-box t)
        (surrounding-output-with-border (stream :shape :rounded
                                                :radius 3 :background clim:+background-ink+
                                                :foreground clim:+foreground-ink+
                                                :move-cursor t)
          (with-output-as-gadget (stream)
            gadget))))))

;;; text-editor-view

(define-presentation-method accept-present-default
    ((type string) stream (view text-editor-view) default default-supplied-p
     present-p query-identifier)
  (unless default-supplied-p
    (setf default ""))
  (let ((gadget (make-gadget-pane-from-view view stream
                                            :value default
                                            :value-changed-callback
                                            (%standard-value-changed-callback query-identifier))))
    (updating-output (stream
                      :cache-value t    ; don't redisplay
                      :unique-id query-identifier
                      :record-type 'accepting-values-record)
      (with-output-as-presentation (stream query-identifier 'selectable-query
                                           :single-box t)
        (surrounding-output-with-border (stream :shape :rounded
                                                :radius 3 :background clim:+background-ink+
                                                :foreground clim:+foreground-ink+
                                                :move-cursor t)
          (with-output-as-gadget (stream)
            gadget))))))

;;; A gadget that's not in the spec but which would  be useful.
(defclass pop-up-menu-view (gadget-dialog-view)
  ()
  (:documentation "A dialog view that presents the elements of a
COMPLETION presentation type as a pop-up menu."))

(defparameter +pop-up-menu-view+ (make-instance 'pop-up-menu-view))

;;; By storing these parameters and options from the COMPLETION
;;; presentation type  in this object, we avoid having to dig them
;;; out of the presentation type on each call to select-query. That
;;; would not be possible if we are accepting a subtype of COMPLETION.
(defclass av-pop-up-menu-record (accepting-values-record)
  ((pop-up-sequence :accessor pop-up-sequence :initform nil)
   (pop-up-test :accessor pop-up-test :initform nil)
   (pop-up-value-key :accessor pop-up-value-key :initform nil)
   (pop-up-name-key :accessor pop-up-name-key :initform nil)))

(define-presentation-method accept-present-default
    ((type completion) stream (view pop-up-menu-view)
     default default-supplied-p present-p query-identifier)
  (declare (ignore present-p))
  (unless default-supplied-p
    (setq default (funcall value-key (elt sequence 0))))
  (let ((record (updating-output (stream :unique-id query-identifier
                                  :cache-value default
                                  :record-type 'av-pop-up-menu-record)
                  (with-output-as-presentation
                      (stream query-identifier 'selectable-query)
                    (surrounding-output-with-border
                        (stream :shape :inset :move-cursor t)
                      (write-string (funcall name-key default) stream))))))
    (setf (pop-up-sequence record) sequence)
    (setf (pop-up-test record) test)
    (setf (pop-up-value-key record) value-key)
    (setf (pop-up-name-key record) name-key)
    record))

(defmethod select-query (stream query (record av-pop-up-menu-record))
  (declare (ignore stream))
  (let* ((value-key (pop-up-value-key record))
         (name-key (pop-up-name-key record)))
    (multiple-value-bind (new-value item event)
        (menu-choose (map 'list
                          #'(lambda (item)
                              `(,(funcall name-key item)
                                 :value ,(funcall value-key item)))
                          (pop-up-sequence record)))
      (declare (ignore item))
      (when event
        (setf (value query) new-value)
        (setf (changedp query) t)))))

(defmethod deselect-query (stream query (record av-pop-up-menu-record))
  (declare (ignore stream query))
  nil)

(defmethod finalize-query-record (query (record av-pop-up-menu-record))
  (declare (ignore query))
  nil)
