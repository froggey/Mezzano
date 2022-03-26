;;;; Copyright (C) 2018, 2019 Jan Moringen
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

(cl:in-package #:clouseau)

(defun ensure-place (container cell place-class)
  (let ((parent *parent-place*))
    (labels ((make-place ()
               (make-instance place-class
                              :parent    parent
                              :container container
                              :cell      cell)))
      (declare (dynamic-extent #'make-place))
      (ensure-child container cell place-class parent #'make-place))))

(defun make-place-formatters (container place-class cell)
  (let ((place (ensure-place container place-class cell)))
    (values (lambda (stream)
              (present place 'place :stream stream))
            (lambda (stream)
              (inspect-place place stream))
            place)))

(defmacro formatting-place ((container place-class cell
                             present-place present-object
                             &key (place-var (gensym "PLACE")))
                            &body body)
  "Execute BODY with PRESENT-PLACE and PRESENT-OBJECT bound to print functions.

Before BODY is executed, an instance of PLACE-CLASS representing the
child of CONTAINER selected by PLACE-CLASS and CELL is created and
stored in the place associated with CONTAINER unless such an instance
already exists.

PRESENT-PLACE is bound to a function that, when called with a stream
as its sole argument, outputs a presentation corresponding to the
created place to the stream. The produced presentation will be of
presentation-type `place'.

PRESENT-OBJECT is bound to a function that, when called with a stream
as its sole argument, outputs a presentation corresponding to the
child of CONTAINER selected by PLACE-CLASS and CELL.

Example:

This application of the macro

  (clouseau:formatting-place (object 'clouseau:reader-place 'symbol-name
                             present-place present-object)
    (write-string \"Symbol name\" stream) ; Label
    (present-place stream)              ; Write place presentation
    (present-object stream))            ; Write value presentation

outputs the name of SYMBOL as an immutable place to STREAM like this:

  Symbol name → <value>
  ^ Label       ^ Value presentation
              ^ Place presentation"
  `(let ((,place-var (ensure-place ,container ,cell ,place-class)))
     (flet (,@(when present-place
                `((,present-place (stream)
                    (present ,place-var 'place :stream stream))))
            ,@(when present-object
                `((,present-object (stream)
                    (inspect-place ,place-var stream)))))
       ,@body)))

(defun format-place-cells (stream object place-class cell
                           &key (label nil labelp)
                                label-style
                                (place-style :slot-like)
                                object-style)
  "Present the child of OBJECT selected by PLACE-CLASS and CELL to STREAM.

Retrieve an existing place instance for the child of OBJECT selected
by PLACE-CLASS and CELL or make a new instance of PLACE-CLASS.

Each of the following is written to STREAM within a separate
`clim:formatting-cell':

1. If non-NIL, LABEL is written to STREAM. If LABEL is a function, it
   is called with STREAM as its sole argument.

   If LABEL-STYLE is non-NIL, it must be either a keyword naming one
   of the styles known to `clouseau:call-with-style' or a list of
   arguments suitable for `clim:invoke-with-drawing-options'. Either
   way, the specified style is applied when outputting LABEL.

2. The place instance is presented using `clim:present' with the
   presentation type `clouseau:place'.

   PLACE-STYLE works like LABEL-STYLE.

3. The child of OBJECT selected by PLACE-CLASS and CELL is inspected
   using `clouseau:inspect-place'.

   OBJECT-STYLE works like LABEL-STYLE.

Example:

The call

  (clouseau:format-place-cells
   stream object 'clouseau:reader-place 'symbol-name
   :label \"Symbol name\"))

outputs the name of SYMBOL as an immutable place to STREAM in three
table cells like this:

  cell 1        cell 2   cell 3
  v             v        v
  Symbol name | →      | <value>
  ^ Label                ^ Value presentation
                ^ Place presentation"
  (formatting-place (object place-class cell place object)
    (labels ((present-label (stream)
               (typecase label
                 (null)
                 (function (funcall label stream))
                 (t (princ label stream))))
             (present-place (stream)
               (when labelp
                 (formatting-cell (stream)
                   (if label-style
                       (call-with-style #'present-label stream label-style)
                       (present-label stream))))
               (formatting-cell (stream) (place stream))))
      (if place-style
          (call-with-style #'present-place stream place-style)
          (present-place stream)))
    (formatting-cell (stream)
      (if object-style
          (call-with-style #'object stream object-style)
          (object stream)))))

(defun format-place-row (stream object place-class cell
                         &rest args
                         &key label label-style place-style object-style)
  "Like `format-place-cells' but surrounded by `clim:formatting-row'."
  (declare (ignore label label-style place-style object-style))
  (formatting-row (stream)
    (apply #'format-place-cells stream object place-class cell args)))
