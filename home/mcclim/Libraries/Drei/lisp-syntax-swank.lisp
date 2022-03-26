;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX; -*-

;;;  (c) copyright 2005-2007 by
;;;           Robert Strandh (strandh@labri.fr)
;;;           David Murray (splittist@yahoo.com)
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

;;; An implementation of some of the editor-centric functionality of
;;; the Lisp syntax using calls to Swank functions.

(in-package :drei-lisp-syntax)

(defclass swank-local-image ()
  ())

;; We need these modules loaded.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Oh my! This is so we "gracefully" handle older Swanks that do not
  ;; have `swank-require'. We just hope they have the symbols we need
  ;; anyway.
  (ignore-errors (swank::swank-require :swank-c-p-c)
                 (swank::swank-require :swank-arglists)))

;; If this file is loaded, make local Swank the default way of
;; interacting with the image.

(defmethod shared-initialize :after
    ((obj lisp-syntax) slot-names &key)
  (declare (ignore slot-names))
  (setf (image obj) (make-instance 'swank-local-image)))

#-clim-without-swank
(defun default-image ()
  "The default image for when the current syntax does not mandate
anything itself (for example if it is not a Lisp syntax)."
  (make-instance 'swank-local-image))

(define-command (com-enable-swank-for-view :name t :command-table lisp-table)
    ()
  (unless (find-package :swank)
    (let ((*standard-output* *terminal-io*))
      (handler-case (asdf:oos 'asdf:load-op :swank)
        (asdf:missing-component ()
          (esa:display-message "Swank not available.")))))
  (setf (image (current-syntax))
        (make-instance 'swank-local-image)))

(defmethod compile-string-for-drei ((image swank-local-image) (string string) package
                                    (view drei-buffer-view) (buffer-mark mark))
  (let* ((view-name (name view))
         (buffer-file-name (filepath (buffer view)))
         (swank::*buffer-package* package)
         (swank::*buffer-readtable* *readtable*))
    (let*  ((result (swank::compile-string-for-emacs
                     string view-name (offset buffer-mark) (princ-to-string buffer-file-name)
                     nil))
            (notes (loop for note in (swank::compilation-result-notes result)
                         collect (make-compiler-note note))))
      (values (list (swank::compilation-result-successp result)
                    (swank::compilation-result-duration result)) notes))))

(defmethod compile-file-for-drei ((image swank-local-image) filepath package &optional load-p)
  (declare (ignore image))
  (let* ((swank::*buffer-package* package)
         (swank::*buffer-readtable* *readtable*)
         (*compile-verbose* nil)
         (result (swank::compile-file-for-emacs filepath load-p))
         (notes (loop for note in (swank::compilation-result-notes result)
                      collect (make-compiler-note note))))
    (values (list (swank::compilation-result-successp result)
                  (swank::compilation-result-duration result)) notes)))

(defmethod find-definitions-for-drei ((image swank-local-image) symbol)
  (declare (ignore image))
  (flet ((fully-qualified-symbol-name (symbol)
           (let ((*package* (find-package :keyword)))
             (format nil "~S" symbol))))
    (let* ((name (fully-qualified-symbol-name symbol))
           (swank::*buffer-package* *package*)
           (swank::*buffer-readtable* *readtable*))
      (swank::find-definitions-for-emacs name))))

(defmethod get-class-keyword-parameters ((image swank-local-image) class)
  (declare (ignore image))
  (loop for arg in (swank::extra-keywords/make-instance 'make-instance class)
     collect (list (list (swank::keyword-arg.keyword arg) (swank::keyword-arg.arg-name arg))
                   (swank::keyword-arg.default-arg arg))))

(defmethod arglist ((image swank-local-image) (symbol symbol))
  (declare (ignore image))
  (let ((arglist (swank::arglist symbol)))
    (unless (eq arglist :not-available)
      arglist)))

(defmethod simple-completions ((image swank-local-image) string default-package)
  (declare (ignore image))
  (swank::completions string (package-name default-package)))

(defmethod fuzzy-completions ((image swank-local-image) symbol-name default-package &optional limit)
  (declare (ignore image))
  (let ((fuzzy-completions-symbol
         (find-symbol (symbol-name '#:fuzzy-completions) :swank)))
    (when fuzzy-completions-symbol
      (funcall fuzzy-completions-symbol (package-name default-package) :limit limit))))
