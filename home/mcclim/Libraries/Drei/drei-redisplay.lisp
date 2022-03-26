;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
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

;;; Declarations and definitions of the generic functions and helper
;;; utilities needed for the Drei redisplay engine

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display of Drei instances.
;;;
;;; The basic Drei redisplay functions:

(defgeneric display-drei-view-contents (stream view)
  (:documentation "The purpose of this function is to display the
contents of a Drei view to some output surface. `Stream' is the
CLIM output stream that redisplay should be performed on, `view'
is the Drei view instance that is being displayed. Methods
defined for this generic function can draw whatever they want,
but they should not assume that they are the only user of
`stream', unless the `stream' argument has been specialized to
some application-specific pane class that can guarantee this. For
example, when accepting multiple values using the
`accepting-values' macro, several Drei instances will be
displayed simultaneously on the same stream. It is permitted to
only specialise `stream' on `clim-stream-pane' and not
`extended-output-stream'. When writing methods for this function,
be aware that you cannot assume that the buffer will contain only
characters, and that any subsequence of the buffer is coercable
to a string. Drei buffers can contain arbitrary objects, and
redisplay methods are required to handle this (though they are
not required to handle it nicely, they can just ignore the
object, or display the `princ'ed representation.)")
  (:method :around ((stream extended-output-stream) (view drei-view))
           (letf (((stream-default-view stream) view))
             (call-next-method))))

(defgeneric display-drei-view-cursor (stream view cursor)
  (:documentation "The purpose of this function is to display a
visible indication of a cursor of a Drei view to some output
surface. `Stream' is the CLIM output stream that drawing should
be performed on, `view' is the Drei view object that is being
redisplayed, `cursor' is the cursor object to be displayed (a
subclass of `drei-cursor') and `syntax' is the syntax object of
`view'. Methods on this generic function can draw whatever they
want, but they should not assume that they are the only user of
`stream', unless the `stream' argument has been specialized to
some application-specific pane class that can guarantee this. It
is permitted to only specialise `stream' on `clim-stream-pane'
and not `extended-output-stream'. It is recommended to use the
function `offset-to-screen-position' to determine where to draw
the visual representation for the cursor. It is also recommended
to use the ink specified by `cursor' to perform the drawing, if
applicable. This method will only be called by the Drei redisplay
engine when the cursor is active and the buffer position it
refers to is on display - therefore, `offset-to-screen-position'
is *guaranteed* to not return NIL or T.")
  (:method :around ((stream extended-output-stream) (view drei-view)
                    (cursor drei-cursor))
           (when (visible-p cursor)
             (letf (((stream-default-view stream) view))
               (call-next-method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The standard redisplay implementation for buffer views.

(defstruct face
  "A face is a description of how to draw (primarily) text, it
consists of an ink (a colour) and a text style. The text style
may be incomplete, in which case it is merged with the default
text style whenever it needs to be used."
  (ink +foreground-ink+)
  (style nil))

(defconstant +default-stroke-drawer-dispatcher+
  #'(lambda (stream view stroke cursor-x cursor-y default-drawing-fn draw)
      (funcall default-drawing-fn stream view stroke cursor-x cursor-y draw))
  "A simple function of seven arguments that simply calls the
first argument as a function with the remaining sex
arguments. Used as the default drawing-function of
`drawing-options' objects.")

(defstruct drawing-options
  "A set of options for how to display a stroke."
  (face (make-face))
  (function +default-stroke-drawer-dispatcher+))

(defun drawing-options-equal (o1 o2)
  "Return true if `o1' and `o2' are equal, that is, they specify
the same options. Does not take the drawing-function into account
due to the halting problem (and also, for more practical
reasons), with the exception that no `drawing-options' object
with a non-`stroke-drawing-fn' drawing function is equivalent to
a `drawing-options' with a `stroke-drawing-fn' drawing function."
  (let ((f1 (drawing-options-face o1))
        (f2 (drawing-options-face o2)))
    (and (equal (face-ink f1) (face-ink f2))
         (equal (face-style f1) (face-style f2))
         (or (not (eq (drawing-options-function o1)
                      +default-stroke-drawer-dispatcher+))
             (eq (drawing-options-function o2)
                 +default-stroke-drawer-dispatcher+))
         (or (not (eq (drawing-options-function o2)
                      +default-stroke-drawer-dispatcher+))
             (eq (drawing-options-function o1)
                 +default-stroke-drawer-dispatcher+)))))

(defvar +default-drawing-options+ (make-drawing-options)
  "The default set of drawing options used for strokes when
nothing else has been specified, or when the default is good
enough. Under these options, the region will be printed as a
string with the default foreground color.")

(defstruct (dimensions :conc-name)
  "A simple mutable rectangle structure. The coordinates should
be absolute coordinates in the coordinate system of a sheet. A
special `center' slot is also provided to enable the recording of
what might be considered a *logical* centre of the dimensions on
the vertical axis. `Center' should be relative to `y1'."
  (x1 0)
  (y1 0)
  (x2 0)
  (y2 0)
  (center 0))

(defun dimensions-height (dimensions)
  "Return the width of the provided `dimensions' object."
  (- (y2 dimensions) (y1 dimensions)))

(defun dimensions-width (dimensions)
  "Return the width of the provided `dimensions' object."
  (- (x2 dimensions) (x1 dimensions)))

(defun coordinates-intersects-dimensions (dimensions x1 y1 x2 y2)
  "Return true if the rectangle defined by (x1, y1), (x2, y2)
intersects with the rectangle defined by `dimensions'."
  (and (or (<= x1 (x1 dimensions) x2)
           (<= x1 (x2 dimensions) x2)
           (<= (x1 dimensions) x1 (x2 dimensions))
           (<= (x1 dimensions) x2 (x2 dimensions)))
       (or (<= y1 (y1 dimensions) y2)
           (<= y1 (y2 dimensions) y2)
           (<= (y1 dimensions) y1 (y2 dimensions))
           (<= (y1 dimensions) y2 (y2 dimensions)))))

(defstruct (displayed-stroke (:conc-name stroke-))
  "A stroke is a description of how a buffer region (`start-offset',
`end-offset') is displayed on the screen. If `dirty' is true,
something has obscured or scribbled over the part of the screen
area taken up by the stroke. If `modified' is true, this stroke
object might output something different than the last time it was
redisplayed, and should thus update any caches or similar. When
`modified' is set, `dirty' probably also should be set.
`widths' is an array of cumulative screen-resolution widths of
the `parts', being a run of characters or a non-graphic character:
see ANALYSE-STROKE-STRING."
  (start-offset)
  (end-offset)
  (drawing-options +default-drawing-options+)
  (dirty t)
  (modified t)
  (dimensions (make-dimensions))
  (widths)
  (parts))

(defun stroke-at-end-of-line (buffer stroke)
  "Return true if the end offset of `stroke' is at the end of a
line in `buffer'. Otherwise, return nil. The end offset of
`stroke' must be a valid offset for `buffer' or an error will be
signalled."
  (offset-end-of-line-p buffer (stroke-end-offset stroke)))

(defstruct (displayed-line (:conc-name line-))
  "A line on display. A line delimits a buffer region (always
bounded by newline objects or border beginning/end) and contains
strokes. `Stroke-count' tells how many of the stroke objects in
`stroke' are actually live, and how many are old, stale objects
to prevent the need for consing if new strokes are added to the
line."
  (start-offset 0)
  (end-offset)
  (dimensions (make-dimensions))
  (strokes (make-array 0 :adjustable t))
  (stroke-count 0))

(defgeneric pump-state-for-offset (view offset)
  (:documentation "Return a pump state that will enable pumping
strokes from `offset' in the buffer of `view' (via
`stroke-pump'). The pump state is not guaranteed to be valid past
the next call to `stroke-pump' or `synchronize-view'. The results
are undefined if `offset' is not at the beginning of a line.")
  (:method ((view drei-syntax-view) (offset integer))
    (pump-state-for-offset-with-syntax view (syntax view) offset)))

(defgeneric stroke-pump (view stroke pump-state)
  (:documentation "Put stroke information in `stroke', returns
new pump-state. `Pump-state' must either be the result of a call
to `pump-state-for-offset' or be the return value of an earlier
call to `stroke-pump'.  A pump state is not guaranteed to be
valid past the next call to `stroke-pump' or
`synchronize-view'. It is permissible for `pump-state' to be
destructively modified by this function.")
  (:method ((view drei-syntax-view) stroke pump-state)
    (stroke-pump-with-syntax view (syntax view) stroke pump-state)))

(defun clear-rectangle* (stream x1 y1 x2 y2)
  "Draw on `stream' from (x1,y1) to (x2,y2) with the background
ink for the stream."
  (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+))

(defun invalidate-stroke (stroke &key modified cleared)
  "Invalidate `stroke' by setting its dirty-bit to true. If
`modified' or `cleared' is true, also set the modified-bit to
true. If `cleared' is true, inform the stroke that its previous
output has been cleared by someone, and that it does not need to
clear it itself during its next redisplay."
  (setf (stroke-dirty stroke) t
        (stroke-modified stroke)
        (or (stroke-modified stroke)
            modified
            cleared))
  (when cleared
    (setf (x1 (stroke-dimensions stroke)) 0
          (y1 (stroke-dimensions stroke)) 0
          (x2 (stroke-dimensions stroke)) 0
          (y2 (stroke-dimensions stroke)) 0)))

(defun invalidate-line-strokes (line &key modified cleared)
  "Invalidate all the strokes of `line' by setting their
dirty-bit to true. If `modified' or `cleared' is true, also set
their modified-bit to true. If `cleared' is true, inform the
strokes that their previous output has been cleared by someone,
and that they do not need to clear it themselves during their
next redisplay."
  (loop for stroke across (line-strokes line)
     do (invalidate-stroke stroke :modified modified
                                  :cleared cleared)))

(defun invalidate-all-strokes (view &key modified cleared)
  "Invalidate all the strokes of `view' by setting their
dirty-bit to true. If `modified' or `cleared' is true, also set
their modified-bit to true. If `cleared' is true, inform the
strokes that their previous output has been cleared by someone,
and that they do not need to clear it themselves during their
next redisplay."
  (loop for line across (displayed-lines view)
     do (invalidate-line-strokes line
         :modified modified :cleared cleared)))

(defmacro do-displayed-lines ((line-sym view) &body body)
  "Loop over lines on display for `view', evaluating `body' with
`line-sym' bound to the `displayed-line' object for each line."
  (check-type line-sym symbol)
  (with-gensyms (line-index)
    (once-only (view)
      `(dotimes (,line-index (displayed-lines-count ,view))
         (let ((,line-sym (aref (displayed-lines ,view) ,line-index)))
           ,@body)))))

(defmacro do-undisplayed-lines ((line-sym view) &body body)
  "Loop over lines not on display for `view', evaluating `body'
with `line-sym' bound to the `displayed-line' object for each
line."
  (check-type line-sym symbol)
  (with-gensyms (line-index)
    (once-only (view)
      `(dotimes (,line-index (- (length (displayed-lines ,view)) (displayed-lines-count ,view)))
         (let ((,line-sym (aref (displayed-lines ,view)
                                (+ (displayed-lines-count ,view) ,line-index))))
           ,@body)))))

(defmacro do-displayed-line-strokes ((stroke-sym line &optional) &body body)
  "Loop over the displayed strokes of `line', evaluating `body'
with `stroke-sym' bound to the `displayed-stroke' object for each
line."
  (check-type stroke-sym symbol)
  (with-gensyms (stroke-index)
    (once-only (line)
      `(dotimes (,stroke-index (line-stroke-count ,line))
         (let* ((,stroke-sym (aref (line-strokes ,line) ,stroke-index)))
           ,@body)))))

(defmacro do-undisplayed-line-strokes ((stroke-sym line &optional) &body body)
  "Loop over the undisplayed strokes of `line', evaluating `body'
with `stroke-sym' bound to the `displayed-stroke' object for each
line."
  (check-type stroke-sym symbol)
  (with-gensyms (stroke-index)
    (once-only (line)
      `(dotimes (,stroke-index (- (length (line-strokes ,line)) (line-stroke-count ,line)))
         (let* ((,stroke-sym (aref (line-strokes ,line)
                                   (+ (line-stroke-count ,line) ,stroke-index))))
           ,@body)))))

(defun invalidate-strokes-in-region (view start-offset end-offset
                                     &key modified cleared to-line-end)
  "Invalidate all the strokes of `view' that overlap the region
`start-offset'/`end-offset' by setting their dirty-bit to
true. If `modified' or `cleared' is true, also set their
modified-bit to true. If `cleared' is true, inform the strokes
that their previous output has been cleared by someone, and that
they do not need to clear it themselves during their next
redisplay. If `to-line-end' is true, if a line is in the region,
strokes in it will be invalidated until the end, even if line-end
is beyond the region."
  (as-region (start-offset end-offset)
    ;; If the region is outside the visible region, no-op.
    (when (and (plusp (displayed-lines-count view)) ; If there is any display...
               (overlaps start-offset end-offset
                         (offset (top view)) (offset (bot view))))
      (let ((line1-index (index-of-displayed-line-containing-offset view start-offset))
            (line2-index (index-of-displayed-line-containing-offset view end-offset)))
        (loop for line = (line-information view line1-index)
              when (<= start-offset
                       (line-start-offset line) (line-end-offset line)
                       end-offset)
              ;; The entire line is within the region.
              do (invalidate-line-strokes line :modified modified
                                               :cleared cleared)
              ;; Only part of the line is within the region.
              else do (do-displayed-line-strokes (stroke line)
                        (when (overlaps start-offset
                                        (if to-line-end (line-end-offset line) end-offset)
                                        (stroke-start-offset stroke)
                                        (stroke-end-offset stroke))
                          (invalidate-stroke stroke :modified modified
                                                    :cleared cleared)))
              if (= line1-index line2-index) do (loop-finish)
              else do (incf line1-index))))))

(defun find-stroke-containing-offset (view offset)
  "Find the stroke of `view' that displays the buffer offset
`offset'. If no such stroke can be found, this function returns
NIL."
  (do-displayed-lines (line view)
    (when (<= (line-start-offset line) offset (line-end-offset line))
      (do-displayed-line-strokes (stroke line)
        (when (and (<= (stroke-start-offset stroke) offset
                       (end-offset (stroke-end-offset stroke))))
          (return-from find-stroke-containing-offset stroke))))))

(defun index-of-displayed-line-containing-offset (view offset)
  "Return the index of the `displayed-line' object containing
`offset'. If `offset' is before the displayed lines, return 0. If
`offset' is after the displayed lines, return the index of the
last line."
  (with-accessors ((lines displayed-lines)) view
    (cond ((< offset (line-start-offset (aref lines 0)))
           0)
          ((> offset (line-end-offset (last-displayed-line view)))
           (1- (displayed-lines-count view)))
          (t
           ;; Binary search for the line.
           (loop with low-index = 0
                 with high-index = (displayed-lines-count view)
                 for middle = (floor (+ low-index high-index) 2)
                 for this-line = (aref lines middle)
                 for line-start = (line-start-offset this-line)
                 for line-end = (line-end-offset this-line)
                 do (cond ((<= line-start offset line-end)
                           (loop-finish))
                          ((> offset line-start)
                           (setf low-index (1+ middle)))
                          ((< offset line-start)
                           (setf high-index middle)))
                 finally (return middle))))))

(defun ensure-line-information-size (view min-size)
  "Ensure that the array of lines for `view' contains at least
`min-size' elements."
  (with-accessors ((displayed-lines displayed-lines)) view
    (setf displayed-lines
          (ensure-array-size displayed-lines min-size
                             #'make-displayed-line))))

(defun line-information (view index)
  "Return the `index'th `displayed-line' object of `view'."
  (ensure-line-information-size view (1+ index))
  (elt (displayed-lines view) index))

(defun last-displayed-line (view)
  "Return the last line on display for `view', will result in an
error if there is no such line (note that even an empty buffer
consists of a single line on display, as long as it has been
redislayed at some point)."
  (elt (displayed-lines view) (1- (displayed-lines-count view))))

(defun ensure-line-stroke-information-size (line min-size)
  "Ensure that the array of strokes in `line' contains at least
`min-size' elements."
  (with-accessors ((line-strokes line-strokes)) line
    (setf line-strokes
          (ensure-array-size line-strokes min-size
                             #'make-displayed-stroke))))

(defun line-stroke-information (line stroke-number)
  "Return the `index'th `displayed-stroke' object of `line'."
  (ensure-line-stroke-information-size line (1+ stroke-number))
  (aref (line-strokes line) stroke-number))

(defun line-last-stroke (line)
  "Return the last stroke in `line', will result in an error if
there is no such stroke (note that even an empty line consists of
a single stroke on display, as long as it has been redislayed at
some point)."
  (aref (line-strokes line) (1- (line-stroke-count line))))

(defun put-stroke (view line pump-state line-change offset)
  "Use `stroke-pump' with `pump-state' to get a new stroke for
`view', and add it to the sequence of displayed strokes in
`line'. `Line-change' should be a relative offset specifying how
much the start-offset of `line' has changed since the last time
it was redisplayed. `Offset' is the offset at which the next
stroke will start."
  (let ((stroke (line-stroke-information line (line-stroke-count line))))
    (unless (stroke-modified stroke)
      (incf (stroke-start-offset stroke) line-change)
      (incf (stroke-end-offset stroke) line-change)
      (when (/= (stroke-start-offset stroke) offset)
        (invalidate-stroke stroke :modified t)))
    (prog1 (stroke-pump view stroke pump-state)
      (incf (line-stroke-count line))
      (setf (line-end-offset line) (stroke-end-offset stroke)))))

(defun record-stroke (stroke parts widths x1 y1 x2 y2
                      &optional (drawn t) (center (/ (- y2 y1) 2)))
  "Record the fact that `stroke' has been drawn (if `drawn' is
true), that it consists of parts `parts' with the widths
`widths', and that it covers the specified area on screen. Sets
the dirty-bit of `stroke' to false if `drawn' is true, and always
sets the modified-bit of `stroke' to false, as it updates the
dimensions."
  (let ((dimensions (stroke-dimensions stroke)))
    (setf (stroke-dirty stroke) (and (stroke-dirty stroke) (not drawn))
          (stroke-modified stroke) nil
	  (stroke-parts stroke) parts
          (stroke-widths stroke) widths
	  (x1 dimensions) x1
          (y1 dimensions) y1
          (x2 dimensions) x2
          (y2 dimensions) y2
          (center dimensions) center)))

(defun non-graphic-char-rep (object)
  "Return the appropriate representation of `object', a non-graphic char.
This will be a string of the format \"^[letter]\" for non-graphic chars
with a char-code of less than #o200, \"\\[octal code]\" for those above
#o200, and the #\\Tab character in the case of a #\\Tab.
NOTE: Assumes an ASCII/Unicode character encoding."
  (let ((code (char-code object)))
    (cond ((eql object #\Tab)
	   object)
	  ((< code #o200)
	   (format nil "^~C" (code-char (+ code (char-code #\@)))))
	  (t
	   (format nil "\\~O" code)))))

(defun analyse-stroke-string (string)
  "Return a list of parts of `string', where each part is a continuous
run of graphic characters or a single non-graphic character. Each element
in the list is of the form START, END, and one of NIL (meaning a run
of graphic characters) or an object representing the non-graphic char."
  (loop with len = (length string)
	for left = 0 then (+ right 1)
	for right = (or (position-if-not #'graphic-char-p string :start left)
			len)
	unless (= left right)
	  collect (list left right)
	  into parts
	until (>= right len)
	collect (list right 
		      (+ right 1) 
		      (non-graphic-char-rep (aref string right)))
	  into parts
	finally (return parts)))

(defun calculate-stroke-width (stroke-string text-style stream x-position)
  "Calculate the width information of `stroke-string' when
displayed with `text-style' (which must be fully specified) on
`stream', starting at the horizontal device unit offset
`x-position'. Three values will be returned: the total width of
the stroke, the parts of the stroke and the widths of the parts
of the stroke."
  (loop with parts = (analyse-stroke-string stroke-string)
     with width = 0
     with widths = (make-array 1 :adjustable t :fill-pointer t :initial-element 0)
     for (start end object) in parts
     do (cond ((eql object #\Tab)
               (incf width
		     (next-tab-stop stream (stream-default-view stream)
				    (+ width x-position)))
               (vector-push-extend width widths))
              (object
               (multiple-value-bind (w)
                   (text-size stream object :text-style text-style)
                 (incf width w)
                 (vector-push-extend width widths)))
              (t
               (multiple-value-bind (w)
                   (text-size stream stroke-string
                    :start start :end end
                    :text-style text-style)
                 (incf width w)
                 (vector-push-extend width widths))))
     finally (return (values width parts widths))))

(defvar +roman-face-style+ (make-text-style nil :roman nil)
  "A text style specifying a roman face, but with unspecified
family and size.")

(defun stroke-drawing-fn (stream view stroke cursor-x cursor-y draw)
  "Draw `stroke' to `stream' baseline-adjusted at the position (`cursor-x',
`cursor-y'). `View' is the view object that `stroke' belongs
to. If `draw' is true, actually draw the stroke to `stream',
otherwise, just calculate its size. It is assumed that the buffer
region delimited by `stroke' only contains characters. `Stroke'
is drawn with face given by the drawing options of `stroke',
using the default text style of `stream' to fill out any
holes. The screen area beneath `stroke' will be cleared before
any actual output takes place."
  (with-accessors ((start-offset stroke-start-offset)
                   (end-offset stroke-end-offset)
                   (dimensions stroke-dimensions)
                   (drawing-options stroke-drawing-options)
		   (widths stroke-widths)
		   (parts stroke-parts)) stroke
    (let* ((stroke-string (in-place-buffer-substring
                           (buffer view) (cache-string view)
                           start-offset end-offset))
           (merged-text-style (merge-text-styles
                               (face-style (drawing-options-face drawing-options))
                               (medium-merged-text-style (sheet-medium stream))))
           ;; Ignore face when computing height, otherwise we get
           ;; bouncy lines when things like parenmatching bolds parts
           ;; of the line.
           (roman-text-style (merge-text-styles +roman-face-style+ merged-text-style))
           (text-style-ascent (text-style-ascent roman-text-style (sheet-medium stream)))
           (text-style-descent (text-style-descent roman-text-style (sheet-medium stream))))
      (with-accessors ((x1 x1) (x2 x2) (center center)) dimensions
        (multiple-value-bind (width stroke-parts part-widths)
	    (if (stroke-modified stroke)
		(calculate-stroke-width stroke-string merged-text-style stream cursor-x)
		(values (- x2 x1) parts widths))
          (when draw
            (loop
              for (start end object) in stroke-parts
              for width across part-widths
              do (cond ((eql object #\Tab)
                        nil)
                       (object
                        (draw-text* stream object (+ cursor-x width)
                                    cursor-y
                                    :text-style merged-text-style
                                    :ink +darkblue+
                                    :align-y :baseline))
                       (t
                        (draw-text* stream stroke-string (+ cursor-x width)
                                    cursor-y
                                    :start start :end end
                                    :text-style merged-text-style
                                    :ink (face-ink (drawing-options-face drawing-options))
                                    :align-y :baseline)))))
	  (record-stroke stroke stroke-parts part-widths
                         cursor-x (- cursor-y text-style-ascent)
			 (+ width cursor-x) (+ cursor-y text-style-descent)
			 draw text-style-ascent))))))

(defun update-stroke-dimensions (stream view stroke cursor-x cursor-y)
  "Calculate the dimensions of `stroke' on `stream'
at (`cursor-x', `cursor-y'), but without actually drawing
anything. Will use the function specified in the drawing-options
of `stroke' to carry out the actual calculations."
  (unless (and (= cursor-x (x1 (stroke-dimensions stroke)))
               (= cursor-y (y1 (stroke-dimensions stroke)))
               (not (stroke-dirty stroke))
               (mark<= (stroke-end-offset stroke) (bot view)))
    (invalidate-stroke stroke :modified t))
  (when (stroke-dirty stroke)
    (funcall (drawing-options-function (stroke-drawing-options stroke)) stream view stroke
             cursor-x cursor-y #'stroke-drawing-fn nil)))

(defvar *highlight-strokes* nil
  "If true, draw a box around all strokes and a line through
their baseline..")

(defvar *stroke-boundary-ink* +red+
  "The ink with which stroke boundaries will be highlighted when
`*highlight-strokes* is true.")

(defvar *stroke-baseline-ink* +blue+
  "The ink with which stroke baselines will be highlighted when
`*highlight-strokes* is true.")

(defun draw-stroke (pane view stroke cursor-x cursor-y)
  "Draw `stroke' on `pane' with a baseline at
`cursor-y'. Drawing starts at the horizontal offset
`cursor-x'. Stroke must thus have updated dimensional
information. Nothing will be done unless `stroke' is dirty."
  (when (stroke-dirty stroke)
    (with-accessors ((x1 x1) (y1 y1) (x2 x2) (y2 y2)
                     (center center)) (stroke-dimensions stroke)
      (when (> x2 (bounding-rectangle-width pane))
        (change-stream-space-requirements pane :width x2))
      (when (> y2 (bounding-rectangle-height pane))
        (change-stream-space-requirements pane :height y2))
      (funcall (drawing-options-function (stroke-drawing-options stroke))
               pane view stroke cursor-x cursor-y #'stroke-drawing-fn t)
      (when *highlight-strokes*
        (draw-rectangle* pane x1 y1 (1- x2) (1- y2) :filled nil :ink *stroke-boundary-ink*)
        (draw-line* pane x1 (+ y1 center) x2 (+ y1 center) :ink *stroke-baseline-ink*)))))

(defun end-line (line x1 y1 line-width line-height)
  "End the addition of strokes to `line' for now, and update the
dimensions of `line'."
  (let ((dimensions (line-dimensions line)))
    (setf (x1 dimensions) x1
          (y1 dimensions) y1
          (x2 dimensions) (+ x1 line-width)
          (y2 dimensions) (+ y1 line-height))))

(defun end-line-cleaning-up (view line line-x1 line-y1
                             line-width line-height)
  "End the addition of strokes to `line' for now, and update the
dimensions of `line'."
  (end-line line line-x1 line-y1 line-width line-height)
  (setf (max-line-width view)
        (max (max-line-width view)
             (dimensions-width (line-dimensions line))))
  ;; This way, strokes that have at one point been left undisplayed
  ;; will always be considered modified when they are filled
  ;; again. The return is for optimisation, we know that an unused
  ;; stroke can only be followed by other unused strokes.
  (do-undisplayed-line-strokes (stroke line)
    (if (null (stroke-start-offset stroke))
        (return)
        (progn (setf (stroke-start-offset stroke) nil)
               (invalidate-stroke stroke :modified t)))))

(defun draw-line-strokes (pane view initial-pump-state
                          start-offset cursor-x cursor-y
                          view-width)
  "Pump strokes from `view', using `initial-pump-state' to begin
with, and draw them on `pane'. The line is set to start at the
buffer offset `start-offset', and will be drawn starting
at (`cursor-x', `cursor-y'). `View-width' is the width of the
view in device units, as calculated by the previous output
iteration."
  (let* ((line (line-information view (displayed-lines-count view)))
         (orig-x-offset cursor-x)
         (offset-change (- start-offset (line-start-offset line)))
         (line-spacing (stream-vertical-spacing pane)))
    (setf (line-start-offset line) start-offset
          (line-stroke-count line) 0)
    ;; So yeah, this is fairly black magic, but it's not actually
    ;; ugly, just complex.
    (multiple-value-bind (line-width baseline descent pump-state)
        ;; Pump all the line strokes and calculate their dimensions.
        (loop with offset = start-offset
              for index from 0
              for stroke = (line-stroke-information line index)
              for stroke-dimensions = (stroke-dimensions stroke)
              for pump-state = (put-stroke view line initial-pump-state offset-change offset)
              then (put-stroke view line pump-state offset-change offset)
              do (update-stroke-dimensions pane view stroke cursor-x cursor-y)
              (setf cursor-x (x2 stroke-dimensions))
              (setf offset (stroke-end-offset stroke))
              maximizing (- (dimensions-height stroke-dimensions)
                            (center stroke-dimensions)) into descent
              maximizing (+ (center stroke-dimensions) cursor-y) into baseline
              summing (dimensions-width stroke-dimensions) into line-width
              when (stroke-at-end-of-line (buffer view) stroke)
              return (values line-width baseline descent pump-state))
      (let ((line-height (- (+ baseline descent) cursor-y)))
        ;; Loop over the strokes and clear the parts of the pane that
        ;; has to be redrawn, trying to minimise the number of calls to
        ;; `clear-rectangle*'..
        (flet ((maybe-clear (x1 x2)
                 (unless (= x1 x2)
                   (clear-rectangle* pane x1 cursor-y x2
                                     (+ cursor-y line-height line-spacing)))))
          (loop with last-clear-x = orig-x-offset
             for stroke-index below (line-stroke-count line)
             for stroke = (aref (line-strokes line) stroke-index)
             for stroke-dimensions = (stroke-dimensions stroke)
             do (unless (= baseline (+ cursor-y (center stroke-dimensions)))
                  (invalidate-stroke stroke))
             (unless (stroke-dirty stroke)
               (maybe-clear last-clear-x (x1 stroke-dimensions))
               (setf last-clear-x (x2 stroke-dimensions)))
             ;; This clears from end of line to the end of the sheet.
             finally (maybe-clear last-clear-x (+ last-clear-x view-width))))
        ;; Now actually draw them in a way that makes sure they all
        ;; touch the bottom of the line.
        (loop for stroke-index below (line-stroke-count line)
           for stroke = (aref (line-strokes line) stroke-index)
           for stroke-dimensions = (stroke-dimensions stroke)
           do (draw-stroke pane view stroke (x1 stroke-dimensions) baseline)
           finally (progn (end-line-cleaning-up view line orig-x-offset cursor-y
                                                line-width line-height)
                          (incf (displayed-lines-count view))
                          (return (values pump-state line-height))))))))

(defun clear-stale-lines (pane view old-width old-height)
  "Clear from the last displayed line to the end of `pane' and
mark undisplayed line objects as dirty. `Old-width'/`old-height'
are the old dimensions of the display of `view' in device units."
  ;; This way, strokes of lines that have at one point been left
  ;; undisplayed will always be considered modified when they are
  ;; filled again. The return is for optimisation, we know that an
  ;; unused stroke can only be followed by other unused strokes.
  (do-undisplayed-lines (line view)
    (setf (line-stroke-count line) 0)
    (do-undisplayed-line-strokes (stroke line)
      (if (null (stroke-start-offset stroke))
          (return)
          (progn (setf (stroke-start-offset stroke) nil)
                 (invalidate-stroke stroke :modified t)))))
  (with-bounding-rectangle* (x1 y1 x2 y2) view
    (declare (ignore x2))
    (when (> old-height (- y2 y1))
      (clear-rectangle* pane x1 y2 (+ x1 old-width) (+ y1 old-height)))))

(defun object-drawer ()
  "Return a closure capable of functioning as a stroke drawer. It
expects its stroke to cover a single-object non-character buffer
region, which will be presented with its appropriate presentation
type (found via `presentation-type-of') to generate output."
  (let (output-record
        baseline
	(widths (make-array 2 :initial-contents (list 0 0)))
	(parts (list 0 1)))
    #'(lambda (stream view stroke cursor-x cursor-y
               default-drawing-fn draw)
        (declare (ignore default-drawing-fn))
        (with-accessors ((start-offset stroke-start-offset)
                         (drawing-options stroke-drawing-options)) stroke
          (let* ((object (buffer-object (buffer view) start-offset)))
            (when (or (null output-record) (stroke-modified stroke))
              (setf output-record
                    (with-output-to-output-record (stream)
                      (present object (presentation-type-of object) :stream stream))
                    baseline (clim-extensions:output-record-baseline output-record)))
            ;; You will not believe this! If `cursor-x' is 0, it seems
            ;; like the changing position is ignored. So add some
            ;; minuscule amount to it, and all will be well. 0.1
            ;; device units shouldn't even be visible.
            (let ((width (bounding-rectangle-width output-record)))
              (setf (output-record-position output-record)
                    (values (+ cursor-x 0.1) (- cursor-y baseline)))
              (when draw
                (replay output-record stream))
	      (setf (aref widths 1) width)
              (record-stroke stroke parts widths
                             cursor-x (- cursor-y baseline)
                             (+ width cursor-x) cursor-y
                             draw baseline)))))))

(defmethod display-drei-view-contents ((pane basic-pane) (view drei-buffer-view))
  (with-bounding-rectangle* (x1 y1 x2 y2) view
    (let* ((old-width (- x2 x1))
           (old-height (- y2 y1))
           (start-offset (offset (beginning-of-line (top view))))
           (pump-state (pump-state-for-offset view start-offset))
           (pane-height (bounding-rectangle-height (or (pane-viewport pane) pane)))
           (current-line-height 0))
      ;; For invalidation of the parts of the display that have
      ;; changed.
      (synchronize-view view :begin (offset (top view)) :end (max (offset (bot view))
                                                                  (offset (top view))))
      (setf (displayed-lines-count view) 0
            (max-line-width view) 0)
      (multiple-value-bind (cursor-x cursor-y) (stream-cursor-position pane)
        (with-output-recording-options (pane :record nil :draw t)
          (loop for line = (line-information view (displayed-lines-count view))
             do (multiple-value-bind (new-pump-state line-height)
                    (draw-line-strokes pane view pump-state start-offset
                                       cursor-x cursor-y old-width)
                  (setf pump-state new-pump-state
                        start-offset (1+ (line-end-offset line))
                        current-line-height line-height)
                  (incf cursor-y (+ line-height (stream-vertical-spacing pane))))
             when (or (and (not (extend-pane-bottom view))
                           (>= (y2 (line-dimensions line)) (- pane-height current-line-height)))
                      (= (line-end-offset line) (size (buffer view))))
             return (progn
                      (setf (offset (bot view)) (line-end-offset line))
                      (clear-stale-lines pane view old-width old-height))))))))

;;; A default redisplay implementation that should work for subclasses
;;; of `drei-buffer-view'. Syntaxes that don't want to implement their
;;; own redisplay behavior can just call these.

(defstruct (pump-state
             (:constructor make-pump-state
                           (line-index offset chunk-index))) 
  "A pump state object used by the `drei-buffer-view'. `Line' is
the line object `offset' is in, and `line-index' is the index of
`line' in the list of lines maintained by the view that created
this pump state."
  line-index offset chunk-index)

(defun chunk-for-offset (buffer-line offset)
  "Return the index of the first chunk of `buffer-line' that
contains `offset'."
  (position (- offset (offset (start-mark buffer-line)))
            (chunks buffer-line) :test #'<= :key #'car))

(defun buffer-view-pump-state-for-offset (view offset)
  "Return a pump state usable for pumpting strokes for `view' (a
`drei-buffer-view') from `offset'."
  ;; Perform binary search looking for line starting with `offset'.
  (synchronize-view view :begin offset)
  (with-accessors ((lines lines)) view
    (loop with low-index = 0
          with high-index = (nb-elements lines)
          for middle = (floor (+ low-index high-index) 2)
          for this-line = (element* lines middle)
          for line-start = (start-mark this-line)
          do (cond ((offset-in-line-p this-line offset)
                      (loop-finish))
                   ((mark> offset line-start)
                    (setf low-index (1+ middle)))
                   ((mark< offset line-start)
                    (setf high-index middle)))
          finally (return (make-pump-state
                           middle offset (chunk-for-offset this-line offset))))))

(defun fetch-chunk (line chunk-index)
  "Retrieve the `chunk-index'th chunk from `line'. The return
value is either an integer, in which case it specifies the
end-offset of a string chunk relative to the start of the line,
or a function, in which case it is the drawing function for a
single-object non-character chunk."
  (destructuring-bind (relative-chunk-end-offset . objectp)
      (aref (chunks line) chunk-index)
    (if objectp (object-drawer) (+ relative-chunk-end-offset
                                   (offset (start-mark line))))))

(defun buffer-view-stroke-pump (view stroke pump-state)
  "Pump redisplay data into `stroke' based on `pump-state' and
the information managed by `view', which must be a
`drei-buffer-view'."
  ;; `Pump-state' will be destructively modified.
  (prog1 pump-state
    (with-accessors ((line-index pump-state-line-index)
                     (offset pump-state-offset)
                     (chunk-index pump-state-chunk-index)) pump-state
      (let* ((chunk (fetch-chunk
                     (element* (lines view) line-index) chunk-index))
             (drawing-options (if (functionp chunk)
                                  (make-drawing-options :function chunk)
                                  +default-drawing-options+))
             (end-offset (if (functionp chunk)
                             (1+ offset)
                             chunk)))
        (setf (stroke-start-offset stroke) offset
              (stroke-end-offset stroke) end-offset
              (stroke-drawing-options stroke) drawing-options)
        (if (offset-end-of-line-p (buffer view) end-offset)
            (setf line-index (1+ line-index)
                  chunk-index 0
                  offset (1+ end-offset))
            (setf chunk-index (1+ chunk-index)
                  offset end-offset))))))

(defmethod pump-state-for-offset ((view drei-buffer-view) (offset integer))
  (buffer-view-pump-state-for-offset view offset))

(defmethod stroke-pump ((view drei-buffer-view) stroke pump-state)
  (buffer-view-stroke-pump view stroke pump-state))

;;; The following is the equivalent of a turbocharger for the
;;; redisplay engine.
(defstruct (skipalong-pump-state
             (:constructor make-skipalong-pump-state (offset)))
  "A pump state for fast skipalong that doesn't involve
the (potentially expensive) actual stroke pump. It transparently
turns into a real pump state when it happens across invalid
strokes. `Offset' is the offset of the next stroke to be pumped."
  offset)

(defmethod stroke-pump :around ((view drei-buffer-view) (stroke displayed-stroke)
                                (pump-state skipalong-pump-state))
  (with-accessors ((state-offset skipalong-pump-state-offset)) pump-state
    (if (or (stroke-modified stroke)
            (/= (stroke-start-offset stroke) state-offset))
        (stroke-pump view stroke (pump-state-for-offset view state-offset))
        (progn (setf state-offset
                     (+ (stroke-end-offset stroke)
                        (if (offset-end-of-line-p
                             (buffer view) (stroke-end-offset stroke))
                            1 0)))
               pump-state))))

(defmethod stroke-pump :around ((view drei-buffer-view) (stroke displayed-stroke)
                                pump-state)
  (if (stroke-modified stroke)
      (call-next-method)
      (stroke-pump view stroke (make-skipalong-pump-state (stroke-start-offset stroke)))))

;;; Cursor handling.

(defun offset-in-stroke-position (stream view stroke offset)
  "Calculate the position in device units of `offset' in
`stroke', relative to the starting position of `stroke'. `Offset'
is an absolute offset into the buffer of `view',"
  (let ((string (in-place-buffer-substring
		 (buffer view) (cache-string view)
		 (stroke-start-offset stroke) offset)))
    (loop with pos = (- offset (stroke-start-offset stroke))
	  for width across (stroke-widths stroke)
	  for next upfrom 1
	  for (start end object) in (stroke-parts stroke)
	  when (and object (= pos end))
	    do (return (aref (stroke-widths stroke) next))
	  when (<= start pos end)
	    do (return (+ width
			  (text-size stream string
                                     :start start
                                     :end pos
                                     :text-style (merge-text-styles
                                                  (face-style
                                                   (drawing-options-face
                                                    (stroke-drawing-options stroke)))
                                                  (medium-merged-text-style (sheet-medium stream)))))))))

(defgeneric offset-to-screen-position (pane view offset)
  (:documentation "Returns the position of offset as a screen
position.  Returns `x', `y', `stroke-height', `object-width' as
values if offset is on the screen, NIL if offset is before the
beginning of the screen, and T if offset is after the end of the
screen. `Object-width' may be an approximation if `offset' is at
the end of the buffer."))

(defmethod offset-to-screen-position ((pane clim-stream-pane) (view drei-view) (offset number))
  (flet ((worker ()
           (do-displayed-lines (line view)
             (when (<= (line-start-offset line) offset (line-end-offset line))
               (with-accessors ((line-dimensions line-dimensions)) line
                 (do-displayed-line-strokes (stroke line)
                   (with-accessors ((start-offset stroke-start-offset)
                                    (end-offset stroke-end-offset)
                                    (stroke-dimensions stroke-dimensions)) stroke
                     (cond ((and (= start-offset offset)
                                 (/= start-offset end-offset))
                            (return-from worker
                              (values (x1 stroke-dimensions) (y1 stroke-dimensions)
                                      (dimensions-height stroke-dimensions)
                                      (if (= end-offset (1+ start-offset))
                                          (dimensions-width stroke-dimensions)
                                          (offset-in-stroke-position pane view stroke (1+ offset))))))
                           ((and (<= start-offset offset)
                                 (< offset end-offset))
                            (return-from worker
                              (let* ((relative-x-position (offset-in-stroke-position pane view stroke offset))
                                     (absolute-x-position (+ (x1 stroke-dimensions) relative-x-position)))
                                (values absolute-x-position (y1 stroke-dimensions)
                                        (dimensions-height stroke-dimensions)
                                        (if (= (1+ offset) end-offset)
                                            (- (x2 stroke-dimensions) absolute-x-position)
                                            (- (offset-in-stroke-position pane view stroke (1+ offset))
                                               relative-x-position)))))))))
                 ;; If we reach this point, we are just past the last
                 ;; stroke, so let's extract information from it.
                 (let ((stroke-dimensions (stroke-dimensions (line-last-stroke line))))
                   (return-from
                    worker (values (x2 stroke-dimensions) (y1 stroke-dimensions)
                                   (dimensions-height stroke-dimensions)))))))))
    (with-accessors ((buffer buffer) (top top) (bot bot)) view
      (let ((default-object-width
             (text-style-width
              (medium-merged-text-style (sheet-medium pane)) pane)))
        (cond
          ((< offset (offset top)) nil)
          ((< (offset bot) offset) t)
          (t
           ;; Search through strokes, returning when we find one that
           ;; `offset' is in. Strokes with >1 object are assumed to be
           ;; strings.
           (multiple-value-bind (x y stroke-height object-width) (worker)
             (if (and x y stroke-height)
                 (values x y stroke-height (or object-width default-object-width))
                 (let* ((first-line (aref (displayed-lines view) 0))
                        (dimensions (line-dimensions first-line)))
                   (values (x1 dimensions) (y1 dimensions)
                           (- (y2 dimensions) (y1 dimensions))
                           default-object-width))))))))))

(defmethod display-drei-view-cursor :around ((pane extended-output-stream)
                                             (view point-mark-view)
                                             (cursor drei-cursor))
  ;; Try to draw the cursor...
  (call-next-method)
  ;; If it is the point, and there was no space for it...
  (when (and (eq (mark cursor) (point view))
             (or (> (bounding-rectangle-max-x cursor)
                    (bounding-rectangle-max-x pane))
                 (> (if (extend-pane-bottom view)
                        (bounding-rectangle-max-y cursor)
                        0)
                    (bounding-rectangle-max-y pane))))
    ;; Embiggen the sheet.
    (change-stream-space-requirements pane
     :width (max (bounding-rectangle-max-x cursor)
                 (bounding-rectangle-max-x pane))
     :height (max (if (extend-pane-bottom view)
                      (bounding-rectangle-max-y cursor)
                      0)
                  (bounding-rectangle-max-y pane)))
    ;; And draw the cursor again.
    (call-next-method)))

(defmethod display-drei-view-cursor :around ((stream extended-output-stream)
                                             (view drei-buffer-view)
                                             (cursor drei-cursor))
  (clear-output-record cursor)
  (when (visible-p cursor)
    (prog1 (call-next-method)
      (with-bounding-rectangle* (x1 y1 x2 y2) cursor
        (do-displayed-lines (line view)
          (cond ((> (y1 (line-dimensions line)) y2)
                 (return))
                ((coordinates-intersects-dimensions
                  (line-dimensions line) x1 y1 x2 y2)
                 (block stroke-loop
                   (do-displayed-line-strokes (stroke line)
                     (cond ((> (x1 (stroke-dimensions stroke)) x2)
                            (return-from stroke-loop))
                           ((coordinates-intersects-dimensions
                             (stroke-dimensions stroke) x1 y1 x2 y2)
                            (setf (stroke-dirty stroke) t)
                            (setf (stroke-modified stroke) t))))))))
        (with-bounding-rectangle* (vx1 vy1 vx2 vy2) view
          (declare (ignore vy1 vx2 vy2))
          (setf (max-line-width view)
                (max (max-line-width view)
                     (- x2 vx1))))))))

(defmethod display-drei-view-cursor ((stream extended-output-stream)
                                     (view drei-buffer-view)
                                     (cursor drei-cursor))
  (multiple-value-bind (cursor-x cursor-y stroke-height object-width)
      (offset-to-screen-position stream view (offset (mark cursor)))
    (letf (((stream-current-output-record stream) cursor))
      (unless (zerop (* object-width stroke-height))
        (draw-rectangle* stream
                         cursor-x cursor-y
                         (+ cursor-x 3) (+ cursor-y stroke-height)
                         :ink (ink cursor))))))

(defmethod bounding-rectangle* ((view drei-buffer-view))
  "Return the bounding rectangle of the visual appearance of
`view' as four values, just as `bounding-rectangle*'. Will return
0, 0, 0, 0 when `view' has not been redisplayed."
  (if (zerop (displayed-lines-count view))
      (values 0 0 0 0)
      (let ((first-line (aref (displayed-lines view) 0))
            (last-line (last-displayed-line view)))
        (values (x1 (line-dimensions first-line))
                (y1 (line-dimensions first-line))
                (+ (x1 (line-dimensions first-line)) (max-line-width view))
                (y2 (line-dimensions last-line))))))

(defmethod bounding-rectangle-width ((view drei-buffer-view))
  (multiple-value-bind (x1 y1 x2)
      (bounding-rectangle* view)
    (declare (ignore y1))
    (- x2 x1)))

(defun drei-bounding-rectangle* (drei-instance)
  "Return the bounding rectangle of the visual appearance of
`drei-instance' as four values, just as `bounding-rectangle*'."
  (bounding-rectangle* (view drei-instance)))

(defun drei-bounding-rectangle-width (drei-instance)
  "Return the width of the bounding rectangle of `drei-instance',
calculated by `drei-bounding-rectangle*'."
  (multiple-value-bind (x1 y1 x2)
      (drei-bounding-rectangle* drei-instance)
    (declare (ignore y1))
    (- x2 x1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drei area redisplay.

;; XXX: Full redraw for every replay, should probably use the `region'
;; parameter to only invalidate some strokes.
(defmethod replay-output-record ((drei drei-area) (stream extended-output-stream) &optional
                                 (x-offset 0) (y-offset 0) (region +everywhere+))
  (declare (ignore x-offset y-offset region))
  (letf (((stream-cursor-position stream) (output-record-start-cursor-position drei)))
    (invalidate-all-strokes (view drei))
    (display-drei-view-contents stream (view drei))))

(defmethod replay-output-record ((cursor drei-cursor) stream &optional
                                 (x-offset 0) (y-offset 0) (region +everywhere+))
  (declare (ignore x-offset y-offset region))
  (with-output-recording-options (stream :record t :draw t)
    (display-drei-view-cursor stream (view cursor) cursor)))

(defun display-drei-area (drei)
  (with-accessors ((stream editor-pane) (view view)) drei
    (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) drei
      (replay drei stream)
      (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2) drei
        (unless (or (and (= new-x1 old-x1) (= new-y1 old-y2)
                         (= new-x2 old-x2) (= new-y2 old-y2))
                    (null (output-record-parent drei)))
          (recompute-extent-for-changed-child (output-record-parent drei) drei
                                              old-x1 old-y1 old-x2 old-y2))))
    (when (point-cursor drei)
      (with-bounding-rectangle* (x1 y1 x2 y2) (point-cursor drei)
        (when (pane-viewport stream)
          (let* ((viewport (pane-viewport stream))
                 (viewport-height (bounding-rectangle-height viewport))
                 (viewport-width (bounding-rectangle-width viewport))
                 (viewport-region (pane-viewport-region stream)))
            ;; Scroll if point went outside the visible area.
            (when (and (active drei)
                       (pane-viewport stream)
                       (not (and (region-contains-position-p viewport-region x2 y2)
                                 (region-contains-position-p viewport-region x1 y1))))
              (scroll-extent stream
                             (max 0 (- x2 viewport-width))
                             (max 0 (- y2 viewport-height))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drei pane redisplay.

(defgeneric handle-redisplay (pane view region)
  (:documentation "Handle redisplay of `view' upon `pane' (which
is a Drei pane) in the given region. Methods defined on this
function should mark their redisplay information as dirty based
on `region' and call the default method, which will in turn call
`display-drei' on `pane'.")
  (:method ((pane drei-pane) (view drei-view) (region region))
    (display-drei pane)))

(defmethod handle-repaint ((pane drei-pane) region)
  (handle-redisplay pane (view pane) region))

(defmethod handle-redisplay ((pane drei-pane) (view drei-buffer-view) (region region))
  (invalidate-all-strokes (view pane) :cleared t)
  (call-next-method))

(defun reposition-pane (drei-pane)
  "Try to put point close to the middle of the pane by moving top
half a pane-size up."
  (let* ((view (view drei-pane))
         (nb-lines-in-pane (number-of-lines-in-region (top view) (bot view))))
    (with-accessors ((top top) (point point)) view
      (setf (offset top) (offset point))
      (beginning-of-line top)
      (loop repeat (floor nb-lines-in-pane 2)
         do (beginning-of-line top)
         until (beginning-of-buffer-p top)
         do (decf (offset top))
         (beginning-of-line top))
      (invalidate-all-strokes view :modified t))))

(defun adjust-pane (drei-pane)
  "Reposition the pane if point is outside the region delimited
by the top/bot marks of its view. Returns true if adjustment was
needed."
  (when (typep (view drei-pane) 'point-mark-view)
    (with-accessors ((buffer buffer) (top top) (bot bot)
                     (point point)) (view drei-pane)
      (when (or (mark< point top)
                (mark> point bot))
        (reposition-pane drei-pane)
        t))))

(defmethod page-down (pane (view drei-buffer-view))
  (with-accessors ((top top) (bot bot)) view
    (when (mark> (size (buffer bot)) bot)
      (setf (offset top) (offset bot))
      (beginning-of-line top)
      (setf (offset (point view)) (offset top))
      (invalidate-all-strokes view :modified t))))

(defmethod page-up (pane (view drei-buffer-view))
  (with-accessors ((top top) (bot bot)) view
    (when (> (offset top) 0)
      (setf (offset (point view)) (offset top))
      (backward-object (point view))
      (beginning-of-line (point view)))))

(defgeneric fix-pane-viewport (pane view)
  (:documentation "Fix the size and scrolling of `pane', which
has `view'."))

(defmethod fix-pane-viewport ((pane drei-pane) (view drei-view))
  (let* ((output-width (drei-bounding-rectangle-width pane))
         (viewport (pane-viewport pane))
         (viewport-width (and viewport (bounding-rectangle-width viewport)))
         (pane-width (bounding-rectangle-width pane)))
    ;; If the width of the output is greater than the width of the
    ;; sheet, make the sheet wider. If the sheet is wider than the
    ;; viewport, but doesn't really need to be, make it thinner.
    (when (and viewport
               (> pane-width viewport-width)
               (>= viewport-width output-width))
      (change-stream-space-requirements pane :width output-width))))

(defmethod fix-pane-viewport :after ((pane drei-pane) (view point-mark-view))
  (when (and (pane-viewport pane) (active pane))
    (with-bounding-rectangle* (x1 y1 x2 y2) (point-cursor pane)
      (declare (ignore y1))
      (multiple-value-bind (x-position y-position) (transform-position (sheet-transformation pane) 0 0)
        (let ((viewport-width (bounding-rectangle-width (or (pane-viewport pane) pane)))
              (viewport-height (bounding-rectangle-height (or (pane-viewport pane) pane))))
          (cond ((> x2 (+ (abs x-position) viewport-width))
                 (scroll-extent pane (round (- x2 viewport-width)) 0))
                ((> (abs x-position) x2)
                 (scroll-extent pane (if (> viewport-width x1)
                                         0
                                         (round x1))
                                0)))
          (when (and (> y2 (+ y-position viewport-height))
                     (not (end-of-buffer-p (bot view))))
            (full-redisplay pane)
            ;; We start all over!
            (display-drei-pane (pane-frame pane) pane)))))))

(defmethod pane-needs-redisplay :around ((pane drei-pane))
  (values (call-next-method) nil))

(defgeneric fully-redisplay-pane (pane view)
  (:documentation "Fully redisplay `pane' showing `view', finally
setting the `full-redisplay-p' flag to false.")
  (:method :after (pane (view drei-view))
    (setf (full-redisplay-p view) nil)))

(defmethod fully-redisplay-pane ((drei-pane drei-pane)
                                 (view point-mark-view))
  (reposition-pane drei-pane))

(defmethod fully-redisplay-pane :after ((drei-pane drei-pane)
                                        (view drei-buffer-view))
  (invalidate-all-strokes view))

(defun display-drei-pane (frame drei-pane)
  "Display `pane'. If `pane' has focus, `current-p' should be
non-NIL."
  (let ((view (view drei-pane)))
    (with-accessors ((buffer buffer)) view
      (when (typep view 'point-mark-view)
        (when (full-redisplay-p view)
          (fully-redisplay-pane drei-pane view)))
      (setf (stream-cursor-position drei-pane) (values 0 0))
      (display-drei-view-contents drei-pane view)
      (if (adjust-pane drei-pane)
          (display-drei-pane frame drei-pane)
          ;; Point must be on top of all other cursors.
          (dolist (cursor (cursors drei-pane)
                   (fix-pane-viewport drei-pane (view drei-pane)))
            (replay cursor drei-pane))))))

(defgeneric full-redisplay (pane)
  (:documentation "Queue a full redisplay for `pane'."))

(defmethod full-redisplay ((pane drei-pane))
  (setf (full-redisplay-p (view pane)) t))
