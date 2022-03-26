;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2006 by Tim Moore (moore@bricoworks.com)
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

;;; Collect all the class definitions in the Spec in one file that is compiled
;;; and loaded early.

(in-package :clim-internals)

(defmacro define-protocol-class (name super-classes &optional slots &rest options)
  (let* ((sym-name (symbol-name name))
         (protocol-predicate (alexandria:symbolicate
                              sym-name
                              (if (find #\- sym-name) "-" "")
                              '#:p))
         (predicate-docstring
           (concatenate 'string
                        "Protocol predicate checking for class " sym-name)))
    `(progn
       (defclass ,name ,super-classes ,slots ,@options)

       ;; This adds a DUMMY slot to the protocol class that signals an
       ;; error in its initfunction. Thus attempting to make an
       ;; instance of the class signals an error.
       ;;
       ;; For subclasses, the slot is not added (as the method is
       ;; EQL-specialized on the protocol class itself) so that no
       ;; runtime time or space overhead is incurred.
       (defmethod c2mop:compute-slots ((class (eql (find-class ',name))))
         (list* (make-instance 'c2mop:standard-effective-slot-definition
                               :name         'dummy
                               :allocation   :instance
                               :initform     '#1=(error "~S is a protocol class ~
                                                         and thus cannot be ~
                                                         instantiated"
                                                        ',name)
                               :initfunction (lambda () #1#))
                (call-next-method)))

       (let ((the-class (find-class ',name)))
         (setf (documentation the-class 'type) "CLIM protocol class"))

       (defgeneric ,protocol-predicate (object)
         (:method ((object t))
           nil)
         (:method ((object ,name))
           t)
         (:documentation ,predicate-docstring))

       ',name)))

;; "Part I: Overview and Conventions" doesn't have any protocol
;; classes defined, so no need for a separate page for it.


;;;; Part II: Geometry Substrate

;;; 3.1 General Regions
(define-protocol-class region (design))

(define-protocol-class point (region bounding-rectangle))
(define-protocol-class path (region bounding-rectangle))
(define-protocol-class area (region bounding-rectangle))
(define-protocol-class region-set (region bounding-rectangle))

(define-protocol-class polyline (path))
(define-protocol-class line (polyline))
(define-protocol-class elliptical-arc (path))

(define-protocol-class polygon (area))
(define-protocol-class rectangle (polygon))
(define-protocol-class ellipse (area))

;;; 4.1 Bounding Rectangles
(define-protocol-class bounding-rectangle ())

;;; 5.1 Transformations
(define-protocol-class transformation ())


;;;; Part III: Windowing Substrate

;;; 7.1 Basic Sheet Classes
(define-protocol-class sheet (bounding-rectangle))

;;; 8.2 Standard Device Events
(define-protocol-class event ()
  ())

;;; 8.3.1 Output Properties

(define-protocol-class medium ()
  ())

;;; 9.2 Ports
(define-protocol-class port ())


;;;; Part IV: Sheet and Medium Output Facilities

;;; 10.3 Line Styles

(define-protocol-class line-style ())

;;; 11.1 Text Styles

(define-protocol-class text-style ()
  ())

;;; 13.2 Basic Designs

(define-protocol-class design ())

;;; 13.3 Color class

(define-protocol-class color (design))

;;; 13.4

(define-protocol-class opacity (design))


;;;; Part V: Extended Stream Output Facilities

;; CLIM Specification says that E-O-S is a subclass of OUTPUT-STREAM,
;; but it does not says what is it. We infere it is a base class for
;; all clim output streams (output-recording-stream included).
(define-protocol-class output-stream
    (fundamental-character-output-stream)
  ())

;;; 15.2 Extended Output Streams
(define-protocol-class extended-output-stream
    (output-stream)
  ())

;;; 15.3 The Text Cursor
(define-protocol-class cursor ())

;;; 16.2 Output Records
(define-protocol-class output-record (bounding-rectangle)
  ())

(define-protocol-class displayed-output-record (output-record)
  ())

;;; 16.3.2 Graphics Displayed Output Records
(define-protocol-class graphics-displayed-output-record
    (displayed-output-record)
  ())

;;; 16.3.3 Text Displayed Output Record
(define-protocol-class text-displayed-output-record (displayed-output-record)
  ())

;;; 16.4 Output Recording Streams
(define-protocol-class output-recording-stream (output-stream)
  ())

;;; 17.3.1 Table Formatting Protocol
(define-protocol-class table-output-record (output-record))

;;; 17.3.2 Row and Column Formatting Protocol
(define-protocol-class row-output-record (output-record))
(define-protocol-class column-output-record (output-record))

;;; 17.3.3 Cell Formatting Protocol
(define-protocol-class cell-output-record (output-record))

;;; 17.3.4 Item List Formatting Protocol
(define-protocol-class item-list-output-record ()
  ())

;;; 18.2 The Graph Formatting Protocol
(define-protocol-class graph-output-record (output-record))
(define-protocol-class graph-node-output-record (output-record))

;;; 21.3 Incremental Redisplay Protocol
(define-protocol-class updating-output-record (output-record))


;;;; Part VI: Extended Stream Input Facilities

;;; 22.2 Extended Input Streams

(define-protocol-class extended-input-stream
    (fundamental-character-input-stream)
  ())

;;; 22.4 The Pointer Protocol

(define-protocol-class pointer ()
  ())

;;; 23.2 Presentations
(define-protocol-class presentation ())

;;; 23.6 Views
(define-protocol-class view ())

;;; 24.1.1 The Input Editing Stream Protocol
(define-protocol-class input-editing-stream ())


;;;; Part VII: Building Applications

;;; 27.2 Command Tables
(define-protocol-class command-table ()
  ())

;;; 28.2 Application Frames
(define-protocol-class application-frame ()
  ())

;;; 28.5 Frame Managers
;;; XXX The slot definitions shouldn't be here, but there is no
;;; standard-frame-manager and I don't want to add these slots to all the frame
;;; manager classes right now.
(define-protocol-class frame-manager ()
  ((port :initarg :port :reader port)
   (frames :initform nil :reader frame-manager-frames)))

;;; 29.2 Basic Pane Construction

(define-protocol-class pane (sheet)
  ())

;;; 30.3 Basic Gadget Classes
;;; XXX Slots definitions should be banished.
(define-protocol-class gadget (pane)
  ())


;;;; Part VIII: Appendices

;;; C.1 Encapsulating Streams
(define-protocol-class encapsulating-stream ()
  ())
