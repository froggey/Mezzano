;;;; (C) Copyright 2019, 2020 Jan Moringen
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

(defpackage #:clim-demo.file-manager
  (:use
   #:clim-lisp
   #:clim)

  (:shadow
   #:directory)

  (:import-from #:alexandria
   #:compose
   #:if-let)

  (:export
   #:file-manager))

(in-package #:clim-demo.file-manager)

;;; Model
;;;
;;; A simple filesystem model with files and directories.

(defclass file ()
  ((%name      :initarg  :name
               :reader   name)
   (%directory :initarg  :directory
               :accessor directory)))

(defun make-file (name)
  (make-instance 'file :name name))

(defclass directory (file)
  ((%children :accessor children
              :initform '())))

(defmethod shared-initialize :after ((instance   directory)
                                     (slot-names t)
                                     &key (children nil children-supplied-p))
  (when children-supplied-p
    (setf (children instance) '())
    (map nil (alexandria:curry #'adopt instance) children)))

(defun make-directory (name &rest children)
  (make-instance 'directory :name name :children children))

(defmethod adopt ((parent directory) (child file))
  (setf (directory child) parent)
  (push child (children parent)))

(defmethod disown ((parent directory) (child file))
  (setf (directory child) nil)
  (alexandria:removef (children parent) child))

(defclass root (directory) ())

(defun make-root (&rest children)
  (make-instance 'root :name "<root>" :children children))

(defclass trash (directory)
  ()
  (:default-initargs :name "Trash"))

(defmethod adopt ((parent trash) (child file)))

;;; Operations
;;;
;;; Copying and moving objects in the filesystem model.

(defmethod copy ((from file) (to directory))
  (adopt to (make-file (name from))))

(defmethod copy ((from directory) (to directory))
  (let ((new (make-directory (name from))))
    (adopt to new)
    (map nil (alexandria:rcurry #'copy new) (children from))))

(defmethod move ((from file) (to directory))
  (disown (directory from) from)
  (adopt to from))

;;; Presentation methods

(defclass file-manager-view (view) ())

(define-presentation-method present ((object file)
                                     (type   file)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +yellow+)
    (princ (name object) stream)))

(defun print-directory-contents (items stream)
  (formatting-item-list (stream)
    (map nil (lambda (child)
               (formatting-cell (stream)
                 (present child (presentation-type-of child)
                          :stream stream :single-box t)))
         (sort (copy-list items) #'string< :key #'name))))

(define-presentation-method present ((object directory)
                                     (type   directory)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +beige+)
    (princ (name object) stream)
    (with-translation (stream (+ (stream-cursor-position stream) 8) 0)
      (draw-polygon* stream '(0 0 10 0 12 3 16 3 16 16 0 16)))
    (terpri stream)
    (print-directory-contents (children object) stream)))

(define-presentation-method present ((object root)
                                     (type   root)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region stream)
    (draw-rectangle* stream (+ x1 4) (+ y1 4) (- x2 4) (- y2 4) :filled nil))
  (stream-increment-cursor-position stream 8 8)
  (print-directory-contents (children object) stream))

(define-presentation-method present ((object trash)
                                     (type   trash)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +beige+)
    (princ (name object) stream)
    (with-translation (stream (+ (stream-cursor-position stream) 8) 0)
      (draw-polygon* stream '(0 4 4 2 12 2 16 4 12 4 12 16 4 16 4 4)))))

;;; Application

(defvar *explanation*
  (format nil "Drag and drop files and directories to move, copy, ~
               etc.~@
               The modifier keys change which action is performed when ~
               the object is dropped:~@
               • no modifier → the item is moved~@
               • meta (often alt key) → the item is copied~@
               Moving things into the \"Trash\" directory deletes them.~@
               Be careful with recursive directory copies."))

(define-application-frame file-manager ()
  ((%root :initarg  :root
          :reader   root
          :initform (make-root
                     (make-instance 'trash)
                     (labels ((rec (class &optional (name (class-name class)))
                                (if-let ((children (c2mop:class-direct-subclasses class)))
                                  (apply #'make-directory name (mapcar #'rec children))
                                  (make-file name))))
                       (rec (find-class 'number) "NUMBER subclasses"))
                     (apply #'make-directory "Features"
                            (mapcar #'make-file
                                    (subseq *features* 0 (min (length *features*) 10)))))))
  (:panes
   (explananation :label :label      *explanation*
                         :background +beige+)
   (files :application :display-function (lambda (frame pane)
                                           (let ((root (root frame)))
                                             (present root (presentation-type-of root)
                                                      :stream     pane
                                                      :single-box t)))
                       :default-view (make-instance 'file-manager-view))
   (interactor :interactor))
  (:layouts
   (:default
    (vertically ()
      explananation
      (7/9 files)
      (make-pane 'clime:box-adjuster-gadget)
      (1/9 interactor))))
  (:menu-bar              nil)
  (:pointer-documentation t)
  (:default-initargs
   :width 800 :height 600))

(defmethod frame-standard-output ((frame file-manager))
  (frame-standard-input frame))

;;; Commands

(define-command (com-new-file :name t :command-table file-manager)
    ((directory directory)
     (name      string))
  (adopt directory (make-file name)))

(define-presentation-to-command-translator directory->com-new-file
    (directory com-new-file file-manager
               :gesture :select :priority -1
               :tester ((object) (not (typep object 'trash)))
               :documentation ((object stream) (format stream "New file")))
    (directory)
  `(,directory ,(accept 'string :prompt "name")))

(define-command (com-new-directory :name t :command-table file-manager)
    ((directory directory)
     (name      string))
  (adopt directory (make-directory name)))

(define-presentation-to-command-translator directory->com-new-directory
    (directory com-new-directory file-manager
               :gesture :select :priority -1
               :tester ((object) (not (typep object 'trash)))
               :documentation ((object stream) (format stream "New directory")))
    (directory)
  `(,directory ,(accept 'string :prompt "name")))

(defun drag-file-feedback (frame from-presentation stream x0 y0 x1 y1 state mode)
  (declare (ignore frame))
  (case state
    (:highlight
     (with-output-recording-options (stream :record nil :draw t)
       (multiple-value-bind (old-x old-y)
           (output-record-position from-presentation)
         (multiple-value-bind (width height)
             (bounding-rectangle-size from-presentation)
           (let ((offset-x (- old-x x0))
                 (offset-y (- old-y y0)))
             (setf (output-record-position from-presentation)
                   (values (+ x1 offset-x) (+ y1 offset-y)))
             (replay-output-record from-presentation stream)
             (setf (output-record-position from-presentation) (values old-x old-y))
             (case mode
               (:invalid
                (let ((x (+ x1 offset-x width 4 4))
                      (y (+ y1 offset-y height 4 -4)))
                  (draw-circle* stream x y 5 :filled nil :ink +gray50+ :line-thickness 2)
                  (draw-line* stream (- x 4) (- y 4) (+ x 4) (+ y 4) :ink +gray50+ :line-thickness 2)))
               (t
                (let ((text (string-downcase mode)))
                  (draw-text* stream text (+ x1 offset-x width 4) (+ y1 offset-y height 4))))))))))
    (:unhighlight
     (multiple-value-bind (old-x old-y) (output-record-position from-presentation)
       (multiple-value-bind (width height) (bounding-rectangle-size from-presentation)
         (let* ((offset-x   (- old-x x0))
                (offset-y   (- old-y y0))
                (text       (string-downcase mode)))
           (multiple-value-bind (text-width text-height dx dy baseline)
               (text-size stream text)
             (declare (ignore dx dy))
             (let ((descent (- text-height baseline)))
               (repaint-sheet stream (make-rectangle* (+ x1 offset-x -1 -8)
                                                      (+ y1 offset-y -1 -8)
                                                      (+ x1 offset-x width 2 4 text-width 8)
                                                      (+ y1 offset-y height 2 4 descent 8)))))))))))

(defun drag-file-feedback/copy (frame from-presentation stream x0 y0 x1 y1 state)
  (drag-file-feedback frame from-presentation stream x0 y0 x1 y1 state :copy))

(defun drag-file-feedback/move (frame from-presentation stream x0 y0 x1 y1 state)
  (drag-file-feedback frame from-presentation stream x0 y0 x1 y1 state :move))

(defun drag-file-feedback/invalid (frame from-presentation stream x0 y0 x1 y1 state)
  (drag-file-feedback frame from-presentation stream x0 y0 x1 y1 state :invalid))

(defun drag-documentation (object destination-object event description stream)
  (format stream "~A ~A ~A~
                  ~:[~*~; to ~:*~A ~A~]~
                  ~@[ with modifiers ~D~]"
          description (type-of object) (name object)
          (when destination-object
            (type-of destination-object))
          (when destination-object
            (name destination-object))
          (when event
            (event-modifier-state event)))
  (force-output stream))

(defun not-same-or-old-parent (object destination-object)
  (not (or (eq object destination-object)
           (eq (directory object) destination-object))))

(define-drag-and-drop-translator drag-file/invalid
    (file command directory file-manager
     :gesture t
     :priority -1
     :tester ((object) (not (typep object 'root)))
     :feedback drag-file-feedback/invalid
     :pointer-documentation ((object destination-object stream event)
                             (drag-documentation
                              object destination-object event "Cannot drag" stream)))
    (object)
  (declare (ignore object))
  nil)

(define-command (com-copy-file :command-table file-manager)
    ((from file) (to directory))
  (format t "Copying ~A to ~A~%" (name from) (name to))
  (copy from to))

(define-drag-and-drop-translator drag-file/copy
    (file command directory file-manager
     :gesture t
     :tester ((object) (not (typep object 'root)))
     :destination-tester ((object destination-object event)
                          (and (= (event-modifier-state event) +meta-key+)
                               (not (typep destination-object 'trash))
                               (not-same-or-old-parent
                                object destination-object)))
     :feedback drag-file-feedback/copy
     :pointer-documentation ((object destination-object stream)
                             (drag-documentation
                              object destination-object nil "Copy" stream)))
    (object destination-object)
  `(com-copy-file ,object ,destination-object))

(define-command (com-move-file :command-table file-manager)
    ((from file) (to directory))
  (format t "Moving ~A to ~A~%" (name from) (name to))
  (move from to))

(define-drag-and-drop-translator drag-file/move
    (file command directory file-manager
     :gesture t
     :tester ((object) (not (typep object 'root)))
     :destination-tester ((object destination-object event)
                          (and (zerop (event-modifier-state event))
                               (not-same-or-old-parent
                                object destination-object)))
     :feedback drag-file-feedback/move
     :pointer-documentation ((object destination-object stream)
                             (drag-documentation
                              object destination-object nil "Move" stream)))
    (object destination-object)
  `(com-move-file ,object ,destination-object))

;;; Interface

(defun run-file-manager ()
  (run-frame-top-level (make-application-frame 'file-manager)))
