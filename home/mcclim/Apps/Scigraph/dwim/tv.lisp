;;; -*- Syntax: Common-lisp; Package: DWIM -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :dwim)

;;;
;;; Operations associated with tv windows (mostly).
;;;

(defun window-under-mouse ()
  (clim:pointer-sheet
   (clim:port-pointer
    (clim:port clim:*application-frame*))))

(defun stream-line-height (stream &optional TEXT-STYLE)
  (if TEXT-STYLE
      (truncate (clim:stream-line-height stream
                                         :text-style TEXT-STYLE))
      (truncate (clim:stream-line-height stream))))

(defun stream-character-width (stream &optional (char #\m))
  ;; "m" is the usual character (the term "ems" is often used in typesetting
  ;; to indicate units of width).
  (if (clim:extended-output-stream-p stream)
      (clim:stream-character-width STREAM char)
      8))

(defmethod stream-cursor-position* (stream)
  (if (clim:extended-output-stream-p stream)
      (multiple-value-bind (x y)
          (clim:stream-cursor-position stream)
        ;; Its nice to assume that cursor positions are fixnums,
        ;; even though postscript streams meaningfully use floats.
        (values (truncate x) (truncate y)))
      (values 0 0)))

(defmethod stream-set-cursor-position* (stream x y)
  (setf (clim:stream-cursor-position stream) (values x y)))

(defmethod stream-viewport (stream)
  ;;(declare (values left top right bottom))
  (cond ((not (clim:extended-output-stream-p stream)))
        ((or (and (type-specifier-p 'clim-postscript::postscript-stream)
                  (typep stream 'clim-postscript::postscript-stream))
             (and (type-specifier-p 'clim-pdf::clim-pdf-stream)
                  (typep stream 'clim-pdf::clim-pdf-stream)))
         ;; width  = inches x 72
         ;; height = inches x 72
         (values 0 0 #.(* 72 7) #.(* 72 10)))
        (t
         (let ((v (and (not (typep (clim:medium-sheet
                                    (clim:sheet-medium stream))
                                   'climi::pixmap))
                       (clim:window-viewport stream))))
           (if v (clim:rectangle-edges* v)
               (values 0 0
                       (clim:bounding-rectangle-width stream)
                       (clim:bounding-rectangle-height stream)))))))

(defmethod stream-viewport-size (stream)
  ;;(declare (values width height))
  (multiple-value-bind (left top right bottom) (stream-viewport stream)
      (values (- right left) (- bottom top)))) 

;;;
;;; Mouse stuff
;;;

(defun interactive-stream-p (stream)
  (clim:extended-input-stream-p stream))

(defmethod stream-pointer-position* (stream)
  "Get position of mouse, in stream coordinates."
  (multiple-value-bind (x y)
      (clim:stream-pointer-position stream)
    (values (truncate x) (truncate y))))

;;;
;;; Frame stuff
;;;

(defvar *sheet-roots* nil)
(defvar *deactivated-frames* nil)
(defvar *activated-frames* nil)

(defmethod frame-top-level-process ((frame t))
  "Access the process associated with this frame."
  (climi::frame-process frame))

(defun get-reusable-frame (manager type)
  (declare (ignorable manager))
  (let ((choices (clim:frame-manager-frames manager)))
      (dolist (item choices)
	(when (and (typep item type) (eq (clim:frame-state item) :disabled))
	  (return item)))))

(defun deactivate-frame (frame)
  (setq *activated-frames* (remove frame *activated-frames* :key #'car))
  (push frame *deactivated-frames*))

(defmethod reset-frame (frame &key title)
  "Prepare a frame for reuse."
  (setf (clim:frame-pretty-name frame) title)
	      (clim:reset-frame frame))

;;; You pay a price for this, so set it to nil if resources are scarce.
(defvar *enable-backing-store* :when-mapped
  "One of :always, :when-mapped, :not-useful, or nil")

(defmethod start-frame (frame &key
			      (wait-until-done t)
			      master
			      (backing-store *enable-backing-store*))
  (cond (master
	   (let ((b (clim:stream-input-buffer
		     (clim:frame-top-level-sheet master)))
		 (top-level-window (clim:frame-top-level-sheet frame)))
	     (labels ((set-input-buffer (window buffer)
			(setf (clim:stream-input-buffer window) buffer)
			(dolist (w (clim:sheet-children window))
			  (set-input-buffer w buffer))))
	       (set-input-buffer top-level-window b)
	       (clim:enable-frame frame)
	       (clim:redisplay-frame-panes frame :force-p t)
	       ;; return the window just created
	       (values top-level-window))))
	  ((not wait-until-done)
	   (process-run-function
	    "Frame Top Level"
	    'start-frame frame
	    :wait-until-done t
	    :master nil
	    :backing-store backing-store)
	   frame)
	  (T
	   (push (list frame (clim-sys:current-process)) *activated-frames*)
	   (unwind-protect
	       (progn
		 (clim:run-frame-top-level frame))
	     (deactivate-frame frame)))))

(defun make-application-frame (type &key parent title 
					 (left 10) (top 10)
					 (width 500) (height 500))
  (let ((frame (clim:make-application-frame
                type
                :pretty-name title
                :left left :top top
                :width width :height height
                :frame-manager
                parent)))
    frame))

(defmethod window-set-viewport-position* (stream left top)
  (setf (clim:window-viewport-position stream) (values left top)))

(defun suggest-frame-size (frame-manager width height)
  (let ((graft (clim:graft (port frame-manager))))
    (when graft
      (setq width (min width (clim:graft-width graft :units :device))
            height (min height (clim:graft-height graft :units :device)))))
  (values width height))

(defun launch-frame 
    (type
     &key
     (backing-store :when-mapped)	; specific to X windows
     create				; NIL => try first to reuse an old instance
     master
     (title "Window Frame")
     (left 0) (bottom 0)
     (width 600) (height 400)
     (wait-until-done nil)		; T => spawn its own process
     (initializer nil)			; function of 1 arg
     &allow-other-keys)
  "The preferred way to make and expose an application frame."
  ;; MASTER is either NIL or another frame.
  ;; If it is a frame, the second frame acts as an extension of the first.
  (declare (ignorable left bottom))
  (let* ((manager (if master (frame-manager master)
		    (find-frame-manager)))
	 (frame (if (not create) (get-reusable-frame manager type))))
    (when frame (reset-frame frame :title title))
    (if frame
	(clim:layout-frame frame width height)
      (setq frame (make-application-frame type
					  ;;:left (max 0 left) :top (max 0 (- height bottom))
					  :parent manager
					  :width width :height height
					  :title title)))
    ;;; CLH: FIXME
    ;;; This was a noop on CLIM. What should we be doing here?
    ;;;
    ;;;   (move-frame frame (max 0 left) (max 0 bottom))
    ;;;
    (multiple-value-bind (w h) (suggest-frame-size manager width height)
      (when (or (not (eql w width)) (not (eql h height)))
	 (clim:layout-frame frame w h)))
    (when initializer
      (let* ((application  frame)
	     (clim:*application-frame* frame))
	(funcall initializer application)))
    (start-frame frame
		 :wait-until-done wait-until-done
		 :master master
		 :backing-store backing-store)))

(defmacro for-each-frame ((symbol) &body body)
  "Iteratively bind SYMBOL to all enabled frames."
  `(clim:map-over-ports
    #'(lambda (port)
        (unless (eq (clim:port-type port) :postscript)
          (dolist (,symbol (clim:frame-manager-frames
                            (clim:find-frame-manager :port port)))
            (when (member (clim:frame-state ,symbol) '(:shrunk :enabled))
              ,@body))))))




