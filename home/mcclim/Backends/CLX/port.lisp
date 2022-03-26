;;; -*- Mode: Lisp; Package: CLIM-CLX; -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2001, 2014, 2016 by
;;;           Robert Strandh (robert.strandh@gmail.com)

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

(in-package :clim-clx)


;;; CLX-PORT class

(defclass clx-pointer (clx-basic-pointer)
  ())

(defclass clx-port (clim-xcommon:keysym-port-mixin
                    clx-selection-mixin
		    clx-basic-port)
  ((color-table :initform (make-hash-table :test #'eq))

   (design-cache :initform (make-hash-table :test #'eq))))

(defclass clx-render-port (clx-port) ())


(defun automagic-clx-server-path (port-type)
  (let ((name (get-environment-variable "DISPLAY")))
    (assert name (name)
            "Environment variable DISPLAY is not set")
    (let* (; this code courtesy telent-clx.
           (slash-i (or (position #\/ name) -1))
           (colon-i (position #\: name :start (1+ slash-i)))
           (decnet-colon-p (and colon-i (eql (elt name (1+ colon-i)) #\:)))
           (host (subseq name (1+ slash-i) colon-i))
           (dot-i (and colon-i (position #\. name :start colon-i)))
           (display (and colon-i
                      (parse-integer name
                                     :start (if decnet-colon-p
                                                (+ colon-i 2)
                                                (1+ colon-i))
                                     :end dot-i)))
           (screen (and dot-i
                     (parse-integer name :start (1+ dot-i))))
           (protocol
            (cond ((or (string= host "") (string-equal host "unix")) :local)
                  (decnet-colon-p :decnet)
                  ((> slash-i -1) (intern
                                   (string-upcase (subseq name 0 slash-i))
                                   :keyword))
                  (t :internet))))
      (list port-type
	    :host host
	    :display-id (or display 0)
	    :screen-id (or screen 0)
	    :protocol protocol))))

(defun helpfully-automagic-clx-server-path (port-type)
  (restart-case (automagic-clx-server-path port-type)
    (use-localhost ()
      :report "Use local display"
      #+windows (parse-clx-server-path `(,port-type :host "localhost" :protocol :internet))
      #-windows (parse-clx-server-path `(,port-type :host "" :protocol :unix)))))

(defun parse-clx-server-path (path)
  (let* ((port-type (pop path))
         (mirroring (mirror-factory (getf path :mirroring))))
    (remf path :mirroring)
    (if path
        `(,port-type
          :host ,(getf path :host "localhost")
          :display-id ,(getf path :display-id 0)
          :screen-id ,(getf path :screen-id 0)
          :protocol ,(getf path :protocol :internet)
          ,@(when mirroring (list :mirroring mirroring)))
        (append (helpfully-automagic-clx-server-path port-type)
                (when mirroring
                  (list :mirroring mirroring))))))

(setf (get :x11 :port-type) 'clx-port)
(setf (get :x11 :server-path-parser) 'parse-clx-server-path)
(setf (get :clx :port-type) 'clx-port)
(setf (get :clx :server-path-parser) 'parse-clx-server-path)

(defmethod initialize-instance :after ((port clx-port) &key)
  (let ((options (cdr (port-server-path port))))
    (push (apply #'make-instance 'clx-frame-manager :port port options)
          (slot-value port 'frame-managers))
    (setf (slot-value port 'pointer)
          (make-instance 'clx-pointer :port port)))
  (initialize-clx port))

(defmethod print-object ((object clx-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (when (slot-boundp object 'display)
      (let ((display (slot-value object 'display)))
	(when display
	  (format stream "~S ~S ~S ~S"
		  :host (xlib:display-host display)
		  :display-id (xlib:display-display display)))))))


(defun realize-mirror-aux (port sheet
				&key (width 100) (height 100) (x 0) (y 0)
				(override-redirect :off)
				(map t)
				(backing-store :not-useful)
                                (save-under :off)
				(event-mask `(:exposure
					      :key-press :key-release
					      :button-press :button-release
                                              :owner-grab-button
					      :enter-window :leave-window
					      :structure-notify
					      :pointer-motion
					      :button-motion)))
  (when (null (port-lookup-mirror port sheet))
    ;;(update-mirror-geometry sheet (%%sheet-native-transformation sheet))
    (let* ((desired-color (typecase sheet
                            (permanent-medium-sheet-output-mixin ;; sheet-with-medium-mixin
                              (medium-background sheet))
                            (pane ; CHECKME [is this sensible?] seems to be
                              (let ((background (pane-background sheet)))
                                (if (typep background 'color)
                                    background
                                    +white+)))
                            (t
                              +white+)))
           (color (multiple-value-bind (r g b)
                      (color-rgb desired-color)
                    (xlib:make-color :red r :green g :blue b)))
	   (screen (clx-port-screen port))
           (pixel (xlib:alloc-color (xlib:screen-default-colormap screen) color))
	   (mirror-region (%sheet-mirror-region sheet))
	   (mirror-transformation (%sheet-mirror-transformation sheet))
           (window (xlib:create-window
                    :parent (sheet-xmirror (sheet-parent sheet))
                    :width (if mirror-region
                               (round-coordinate (bounding-rectangle-width mirror-region))
                               width)
                    :height (if mirror-region
				(round-coordinate (bounding-rectangle-height mirror-region))
				height)
                    :x (if mirror-transformation
			   (round-coordinate (nth-value 0 (transform-position
							   mirror-transformation
							   0 0)))
			   x)
                    :y (if mirror-transformation
                           (round-coordinate (nth-value 1 (transform-position
                                                           mirror-transformation
                                                           0 0)))
                           y)
                    :override-redirect override-redirect
                    :backing-store backing-store
                    :save-under save-under
                    :gravity :north-west
                    ;; Evil Hack -- but helps enormously (Has anybody
                    ;; a good idea how to sneak the concept of
                    ;; bit-gravity into CLIM)? --GB
                    :bit-gravity (if (typep sheet 'climi::extended-output-stream)
                                     :north-west
                                     :forget)
                    :background pixel
                    :event-mask (apply #'xlib:make-event-mask
                                       event-mask))))
      (port-register-mirror (port sheet) sheet window)
      (when map
        (xlib:map-window window)
        (xlib:display-finish-output (clx-port-display port)))))
  (port-lookup-mirror port sheet))

(defmethod realize-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  ;;mirrored-sheet-mixin is always in the top of the Class Precedence List
  (%realize-mirror port sheet))

(defmethod %realize-mirror ((port clx-port) (sheet basic-sheet))
  (realize-mirror-aux port sheet
                      :map (sheet-enabled-p sheet)))

(defmethod %realize-mirror ((port clx-port) (sheet top-level-sheet-mixin))
  (let* ((q (compose-space sheet))
         (window (realize-mirror-aux
                  port sheet
                  :map nil
                  :width (round-coordinate (space-requirement-width q))
                  :height (round-coordinate (space-requirement-height q))))
         (name (clime:sheet-name sheet))
         (instance-name (string-downcase name))
         (class-name (string-capitalize name))
         (pretty-name (clime:sheet-pretty-name sheet)))
    (xlib:set-wm-class window instance-name class-name)
    (%set-window-name window pretty-name)
    (%set-window-icon-name window pretty-name)
    (setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
    (setf (xlib:wm-protocols window) `(:wm_take_focus :wm_delete_window))
    (xlib:change-property window
                          :WM_CLIENT_LEADER (list (xlib:window-id window))
                          :WINDOW 32)))

(defmethod %realize-mirror ((port clx-port) (sheet unmanaged-sheet-mixin))
  (realize-mirror-aux port sheet
		      :override-redirect :on
                      :save-under :on
		      :map nil))

(defmethod make-graft ((port clx-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'clx-graft
		 :port port :mirror (clx-port-window port)
		 :orientation orientation :units units))
        (width (xlib:screen-width (clx-port-screen port)))
        (height (xlib:screen-height (clx-port-screen port))))
    (let ((region (make-bounding-rectangle 0 0 width height)))
      (climi::%%set-sheet-region region graft))
    graft))

(defmethod make-medium ((port clx-port) sheet)
  (make-instance 'clx-medium
		 ;; :port port
		 ;; :graft (find-graft :port port)
		 :sheet sheet))

(defmethod make-medium ((port clx-render-port) sheet)
  (make-instance 'clx-render-medium
		 ;; :port port
		 ;; :graft (find-graft :port port)
		 :sheet sheet))

(defmethod graft ((port clx-port))
  (first (port-grafts port)))

;;; Pixmap

(defmethod realize-mirror ((port clx-port) (pixmap pixmap))
  (when (null (port-lookup-mirror port pixmap))
    (let* ((window (sheet-xmirror (pixmap-sheet pixmap)))
	   (pix (xlib:create-pixmap
		    :width (round (pixmap-width pixmap))
		    :height (round (pixmap-height pixmap))
		    :depth (xlib:drawable-depth window)
		    :drawable window)))
      (port-register-mirror port pixmap pix))
    (values)))

(defmethod destroy-mirror ((port clx-port) (pixmap pixmap))
  (alexandria:when-let ((mirror (port-lookup-mirror port pixmap)))
    (when-let ((picture (find-if (alexandria:of-type 'xlib::picture)
                                 (xlib:pixmap-plist mirror))))
      (xlib:render-free-picture picture))
    (xlib:free-pixmap mirror)
    (port-unregister-mirror port pixmap mirror)))

(defmethod port-allocate-pixmap ((port clx-port) sheet width height)
  (let ((pixmap (make-instance 'mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port clx-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

;; Top-level-sheet

;; this is evil.
(defmethod allocate-space :after ((pane top-level-sheet-mixin) width height)
  (when (sheet-direct-xmirror pane)
    (with-slots (space-requirement) pane
      '(setf (xlib:wm-normal-hints (sheet-direct-xmirror pane))
            (xlib:make-wm-size-hints
             :width (round width)
             :height (round height)
             :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
             :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
             :min-width (round (space-requirement-min-width space-requirement))
             :min-height (round (space-requirement-min-height space-requirement)))))))

(defmethod port-force-output ((port clx-port))
  (xlib:display-force-output (clx-port-display port)))
