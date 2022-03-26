;;; -*- Mode: Lisp; Package: CLIM-NULL; -*-

;;;  (c) copyright 2005 by Christophe Rhodes (c.rhodes@gold.ac.uk)
;;;  (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)

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

(in-package :clim-null)

(defclass null-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass null-port (basic-port)
  ((id)
   (pointer :accessor port-pointer :initform (make-instance 'null-pointer))
   (window :initform nil :accessor null-port-window)))

(defun parse-null-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :null :port-type) 'null-port)
(setf (get :null :server-path-parser) 'parse-null-server-path)

(defmethod initialize-instance :after ((port null-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "NULL-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'null-frame-manager :port port)
	(slot-value port 'climi::frame-managers)))

(defmethod print-object ((object null-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defmethod port-set-mirror-region ((port null-port) mirror mirror-region)
  ())
                                   
(defmethod port-set-mirror-transformation
    ((port null-port) mirror mirror-transformation)
  ())

(defmethod realize-mirror ((port null-port) (sheet mirrored-sheet-mixin))
  nil)

(defmethod destroy-mirror ((port null-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod mirror-transformation ((port null-port) mirror)
  ())

(defmethod port-enable-sheet ((port null-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port null-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port null-port))
  nil)

(defmethod process-next-event ((port null-port) &key wait-function (timeout nil))
  (cond ((maybe-funcall wait-function)
         (values nil :wait-function))
        ((not (null timeout))
         (sleep timeout)
         (if (maybe-funcall wait-function)
             (values nil :wait-function)
             (values nil :timeout)))
        ((not (null wait-function))
         (loop do (sleep 0.1)
               until (funcall wait-function)
               finally (return (values nil :wait-function))))
        (t
         (error "Game over. Listening for an event on Null backend."))))

(defmethod make-graft
    ((port null-port) &key (orientation :default) (units :device))
  (make-instance 'null-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port null-port) sheet)
  (make-instance 'null-medium :sheet sheet))

(defmethod text-style-mapping
    ((port null-port) (text-style text-style) &optional character-set)
  (declare (ignore port text-style character-set))
  nil)

(defmethod (setf text-style-mapping) (font-name
                                      (port null-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

(defmethod graft ((port null-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port null-port) sheet width height)
  (declare (ignore sheet width height))
  ;; FIXME: this isn't actually good enough; it leads to errors in
  ;; WITH-OUTPUT-TO-PIXMAP
  nil)

(defmethod port-deallocate-pixmap ((port null-port) pixmap)
  #+nil
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer null-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer null-pointer))
  nil)

(defmethod port-modifier-state ((port null-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer null-pointer))
  nil)

(defmethod port-frame-keyboard-input-focus ((port null-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus) 
    (focus (port null-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod (setf port-keyboard-input-focus) (focus (port null-port))
  focus)

(defmethod port-keyboard-input-focus ((port null-port))
  nil)

(defmethod port-force-output ((port null-port))
  nil)

(defmethod distribute-event :around ((port null-port) event)
  (declare (ignore event))
  nil)

(defmethod set-sheet-pointer-cursor ((port null-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)        
