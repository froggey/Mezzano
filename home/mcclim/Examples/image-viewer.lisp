;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2008 by
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

;;; A simple program for displaying images of formats known to McCLIM.

(in-package :clim-demo)

(defclass image-viewer-gadget (value-gadget)
  ()
  (:documentation "An abstract gadget for displaying images. The
value of the gadget is the image being displayed.")
  (:default-initargs :value nil))

(defmethod (setf gadget-value) :after (new-value (gadget image-viewer-gadget)
                                                 &key &allow-other-keys)
  (handle-repaint gadget (or (pane-viewport-region gadget)
                             (sheet-region gadget))))

(defclass image-viewer-pane (image-viewer-gadget basic-gadget)
  ()
  (:documentation "A concrete gadget for displaying images. The
value of the gadget is the image being displayed."))

(defmethod handle-repaint ((pane image-viewer-pane) region)
  (declare (ignore region))
  ;; Clear the old image.
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)    
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+))
  ;; Draw the new one, if there is one.
  (when (gadget-value pane)
    (let ((image-height (pattern-height (gadget-value pane)))
          (image-width (pattern-width (gadget-value pane))))
      ;; Try to ensure there is room for the new image.
      (change-space-requirements pane :height image-height :width image-width)
      ;; Draw it in the center.
      (handler-case (draw-pattern*
                     pane (gadget-value pane)
                     (/ (- (bounding-rectangle-width pane) image-width) 2)
                     (/ (- (bounding-rectangle-height pane) image-height) 2))
        (error (e)
          (with-text-style (pane (make-text-style nil :italic nil))
            (draw-text* pane (format nil "Error while drawing image: ~a" e)
                        0 0 :align-y :top)))))))

(define-application-frame image-viewer ()
  ((%image-pathname :accessor image-pathname
                    :initarg :image-pathname
                    :initform nil))
  (:menu-bar t)
  (:panes
   (viewer (make-pane 'image-viewer-pane))
   (interactor :interactor
               :text-style (make-text-style :sans-serif nil nil)
               :min-height 100))
  (:layouts
   (default (vertically ()
              (4/5 (labelling (:label "Image")
                     viewer))
              (1/5 interactor))))
  (:top-level ((lambda (frame)
                 (default-frame-top-level frame)))))

(define-image-viewer-command (com-display-image :name t :menu t)
    ((image-pathname pathname
      :default (user-homedir-pathname) :insert-default t))
  (if (probe-file image-pathname)
      (let* ((type (funcall (case (readtable-case *readtable*)
                              (:upcase #'string-upcase)
                              (:downcase #'string-downcase)
                              (t #'identity))
                            (pathname-type image-pathname)))
             (format (find-symbol type (find-package :keyword)))
             (viewer (find-pane-named *application-frame* 'viewer)))
        (handler-case (progn
                        (setf (gadget-value viewer)
                              (make-pattern-from-bitmap-file image-pathname :format format)
                              (image-pathname *application-frame*) image-pathname)
                        (format t "~A image loaded succesfully" type))
          (unsupported-bitmap-format ()
            (format t "Image format ~A not recognized" type))))
      (format t "No such file: ~A" image-pathname)))

(define-image-viewer-command (com-blank-image :name t :menu t)
    ()
  (setf (gadget-value (find-pane-named *application-frame* 'viewer)) nil))

(defun image-viewer (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'image-viewer)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Image viewer")
        (run))))
