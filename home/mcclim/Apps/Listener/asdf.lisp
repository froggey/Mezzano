;;; This is a lisp listener.

;;; (C) Copyright 2009 by Andy Hefner (ahefner@gmail.com)

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

(in-package :clim-listener)

;;;; CLIM defintions for interacting with ASDF

(define-command-table asdf-commands :inherit-from nil)

(define-presentation-type asdf-system ())
(define-presentation-type asdf-system-definition () :inherit-from 'pathname)

(defclass asdf-attribute-view (textual-view)
  ((ignorable-attributes :reader ignorable-attributes
                         :initform nil :initarg :ignore)
   (note-unloaded :reader note-unloaded :initform nil :initarg :note-unloaded)
   (default-label :reader default-attr-label :initform "" :initarg :default)))

(defmethod ignorable-attributes (view) nil)
(defmethod note-unloaded (view) nil)
(defmethod default-attr-label (view) "")

(defun asdf-loaded-systems ()
  "Retrieve a list of loaded systems from ASDF"
  (map 'list #'asdf:find-system (asdf:already-loaded-systems)))

(defun asdf-get-central-registry ()
  asdf::*central-registry*)

(defun asdf-registry-system-files ()
  "Retrieve the list of unique pathnames contained within the ASDF registry folders"
  (uiop:while-collecting (systems)
    (dolist (reg (asdf-get-central-registry))
      (mapc (lambda (path)
	      (when (string-equal (pathname-type path) "asd")
		(systems (truename path))))
	    (cl-fad:list-directory (eval reg))))))

(defun asdf-operation-pretty-name (op)
  (case op
    (asdf:compile-op "compiled")
    (asdf:load-op    "loaded")
    (:unloaded       "unloaded")
    (otherwise   (prin1-to-string op))))

(defun asdf-system-history (system)
  (let (history)
    (maphash (lambda (operation time)
               (declare (ignore time))
               (push operation history))
             (slot-value system 'asdf::operation-times))
    (nreverse history)))

(define-presentation-method presentation-typep (object (type asdf-system))
  (typep object 'asdf:system))

(define-presentation-method present (object (type asdf-system) stream
                                            (view textual-view)
                                            &key acceptably)
  (if acceptably
      (princ (asdf:coerce-name object) stream)
      (let* ((history (asdf-system-history object))
             (loaded-p (find 'asdf:load-op history))
             (eff-history (set-difference history (ignorable-attributes view))))
        (when (and (note-unloaded view) (not loaded-p))
          (push :unloaded eff-history))
        (format stream "~A~A"
                (asdf:coerce-name object)
                (if (null eff-history)
                    (default-attr-label view)
                    (format nil " (~{~a~^, ~})"
                            (mapcar 'asdf-operation-pretty-name eff-history)))))))

(define-presentation-method accept ((type asdf-system) stream
                                    (view textual-view) &key)
  (multiple-value-bind (object success)
      (completing-from-suggestions (stream)
        (dolist (system (asdf-loaded-systems))
          (suggest (asdf:coerce-name system) system)))
    (if success
        object
        (simple-parse-error "Unknown system"))))

(define-command (com-list-systems :name "List Systems"
                                  :command-table asdf-commands
                                  :menu t)
    ()
  (format-items
   (asdf-loaded-systems)
   :printer (lambda (item stream)
              (present item 'asdf-system
                       :stream stream
                       :view (make-instance 'asdf-attribute-view
                                            :note-unloaded t
                                            :ignore '(asdf:compile-op asdf:load-op))))
   :presentation-type 'asdf-system))

(define-command (com-show-available-systems :name "Show System Files"
                                            :command-table asdf-commands
                                            :menu t)
    ()
  (format-items (asdf-registry-system-files)
                :presentation-type 'asdf-system-definition))

(define-command (com-operate-on-system :name "Operate On System"
                                       :command-table asdf-commands
                                       :menu t)
    ((system '(type-or-string asdf-system) :prompt "system")
     (operation '(member asdf::compile-op asdf::load-op)
                :default 'asdf::load-op
                :prompt "operation"))
  (asdf:oos operation system))

(define-command (com-load-system :name "Load System"
                                 :command-table asdf-commands
                                 :menu t)
    ((system '(type-or-string asdf-system) :prompt "system"))
  (asdf:oos 'asdf:compile-op system)
  (asdf:oos 'asdf:load-op system))

(defmethod mime-type-to-command ((mime-type text/x-lisp-system) pathname)
  (values `(com-load-system ,pathname)
          "Load System"
          (format nil "Load System ~A" pathname)))
