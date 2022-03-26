
;;; This is a lisp listener.

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

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

;;; Listener view
;;;
;;; FIXME: this TEXTUAL-VIEW thing is a lie: we can draw graphics.
;;; However, all the various presentation methods around the world are
;;; specialized on textual view, and it sucks to have to reimplement
;;; them all.
(defclass listener-view (textual-view) ())

(defclass listener-pointer-documentation-view 
    (listener-view pointer-documentation-view)
  ())

(defparameter +listener-view+ (make-instance 'listener-view))
(defparameter +listener-pointer-documentation-view+
  (make-instance 'listener-pointer-documentation-view))

(define-presentation-method present :around
  ((object sequence) (type sequence) stream (view listener-view)
   &key acceptably for-context-type)
  (present object 'expression :stream stream :view view
           :acceptably acceptably :for-context-type for-context-type))

(define-presentation-method accept :around
  ((type sequence) stream (view listener-view) &key default default-type)
  (declare (ignorable default default-type))
  ;; oh, my word.  although TYPE here might look like it's bound to
  ;; the presentation type itself, in fact it is bound to the
  ;; parameter of the SEQUENCE presentation type.  We need the
  ;; presentation type itself, so we reconstruct it.
  (let ((ptype (list 'sequence type)))
    (let* ((token (read-token stream))
	   (result (handler-case (read-from-string token)
		     (error (c)
		       (declare (ignore c))
		       (simple-parse-error 
			"Error parsing ~S for presentation type ~S"
			token ptype)))))
      (if (presentation-typep result ptype)
	  (values result ptype)
	  (input-not-of-required-type result ptype)))))

;;; Listener interactor stream.  If only STREAM-PRESENT were
;;; specializable on the VIEW argument, this wouldn't be necessary.
;;; However, it isn't, so we have to play this game.  We currently
;;; only use this to get single-box presentation highlighting.

(defclass listener-interactor-pane (interactor-pane) ())

(defmethod stream-present :around 
    ((stream listener-interactor-pane) object type
     &rest args &key (single-box nil sbp) &allow-other-keys)
  (declare (ignore single-box sbp))
  (apply #'call-next-method stream object type :single-box t args)
  ;; we would do this, but CLIM:PRESENT calls STREAM-PRESENT with all
  ;; the keyword arguments explicitly.  *sigh*.
  #+nil 
  (if sbp
      (call-next-method)
      (apply #'call-next-method stream object type :single-box t args)))

;;; Listener application frame
(define-application-frame listener (standard-application-frame)
    ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t))
    (:panes (interactor-container
             (make-clim-stream-pane
              :type 'listener-interactor-pane
              :name 'interactor :scroll-bars t
              :default-view +listener-view+))
            (doc :pointer-documentation :default-view +listener-pointer-documentation-view+)
            (wholine (make-pane 'wholine-pane
                                :display-function 'display-wholine :scroll-bars nil
                                :display-time :command-loop :end-of-line-action :allow)))
  (:top-level (default-frame-top-level :prompt 'print-listener-prompt))
  (:command-table (listener
                   :inherit-from (application-commands
                                  lisp-commands
                                  #+(or) asdf-commands
                                  filesystem-commands
                                  show-commands)
                   :menu (("Listener"   :menu application-commands)
                          ("Lisp"       :menu lisp-commands)
                          ("Filesystem" :menu filesystem-commands)
                          ("Show"       :menu show-commands))))
  (:disabled-commands com-pop-directory com-drop-directory com-swap-directory)
  (:menu-bar t)
  (:layouts (default
	      (vertically ()
                interactor-container
                doc
                wholine))))

;;; Package selection popup

(define-listener-command (com-choose-package)
    ()
  (let ((new-package (menu-choose (sort (mapcar (lambda (package) (cons (package-name package)
                                                                        package))
                                                (list-all-packages))
                                        #'string<
                                        :key #'car)
                                  :label "Choose Package")))
    (when new-package
      (setf *package* new-package))))

(define-presentation-to-command-translator choose-package-translator
    (listener-current-package com-choose-package listener
     :echo nil
     :priority 100  ; These presentations appear in exactly one context, so give this a high priority.
     :documentation ((object stream)
                     (declare (ignore object))
                     (format stream "Choose package")))
    (object))

;;; Lisp listener command loop

(define-presentation-type empty-input ())

(define-presentation-method present 
    (object (type empty-input) stream view &key &allow-other-keys)
  (princ "" stream))

;;; Sneaky - we want to use :fix text for the command prompt, but
;;; use the default :sans-serif in accepting-values dialogs. Those
;;; are invokved by the :around method on r-f-c, so if we bind
;;; the text style here in the primary method, we're okay.

(defmethod read-frame-command ((frame listener) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (multiple-value-bind (object type)
      (let ((*command-dispatchers* '(#\,)))
        (with-text-style (stream (make-text-style :fix :roman :normal))
          (accept 'command-or-form :stream stream :prompt nil 
                  :default "hello" :default-type 'empty-input)))
    (cond
      ((presentation-subtypep type 'empty-input)
       ;; Do nothing.
       `(com-eval (values)))
      ((presentation-subtypep type 'command) object)
      (t `(com-eval ,object)))))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (with-output-as-presentation (stream *package* 'package :single-box t)
    (print-package-name stream))
  (princ "> " stream)
  (let ((h (- (bounding-rectangle-height (stream-output-history stream))
              (bounding-rectangle-height (or (pane-viewport stream) stream)))))
    (scroll-extent stream 0 (max 0 h))))

(defmethod frame-standard-output ((frame listener))
  (get-frame-pane frame 'interactor))

(defun run-listener (&key (new-process nil)
                          (debugger t)
                          (width 790)
                          (height 550)
                          port
                          frame-manager
                          (process-name "Listener")
                          (package :clim-user))
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (find-port)))))
         (frame (make-application-frame 'listener
                                       :frame-manager fm
                                       :width width
                                       :height height)))
    (flet ((run () 
             (let ((*package* (find-package package)))
               (unwind-protect
                    (if debugger
                        (clim-debugger:with-debugger () (run-frame-top-level frame))
                        (run-frame-top-level frame))
                 (disown-frame fm frame)))))
      (if new-process
          (values (clim-sys:make-process #'run :name process-name)
                  frame)
          (run)))))
