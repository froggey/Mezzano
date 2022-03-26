;;; -*- Mode: lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)
;;;  (c) copyright 2007 by Andy Hefner (ahefner@gmail.com)
;;;  (c) copyright 2017 by Daniel Kochmański (daniel@turtleware.eu)
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

;;; TODO:
;;;  - Define a protocol which the graph formatter can utilize to determine
;;;    where graph edges should be connected to shaped output borders.

;;;  - ** Double check default value and intent of move-cursor argument.
;;;    If I understand things right, move-cursor t for underlining is usually
;;;    the wrong thing.

;;; FIXME:
;;;  - Various functions which try to accomodate line-thickness do not
;;;    attempt to consider possibility of a line-style argument.
;;;  - In a perfect world we could make the default shadow ink a tranlucent
;;;    ink, but the CLX backend isn't there yet. A stopgap measure could
;;;    simply blend against the pane-background.
;;;  - Using padding to control the rounded rectangles might be the wrong thing.

;;; Question
;;;  Would it make more sense to draw borders as part of replay (with recording
;;;  off, like a displayed record), and letting them effortlessly accomodate
;;;  changes in the bounding rectangle of the contents? This would only benefit
;;;  people doing unusual things with output records. How would be determine
;;;  bounds of the border?
;;;
;;; Answer
;;;  After hours of trial and error we know, that only drawing borders doesn't
;;;  work well. Determining bounding rectangle is not overly hard, but not
;;;  having border as output record doesn't play well with recomputing extent
;;;  etc. Accomodating changes in the bounding rectangle of the contents makes a
;;;  lot of sense also for usual things, like setf-ing inner record position or
;;;  adjusting table elements. As a solution we take a third path –
;;;  border-output-record is cleaned on each replay and border output records
;;;  are recreated then. Thanks to that we have output records *and* we follow
;;;  inner-record when it changes. --jd

(in-package :clim-internals)

(defclass bordered-output-record (standard-sequence-output-record)
  ((stream :reader border-stream :initarg :stream)
   (shape :reader shape :initarg :shape)
   (record :reader inner-record :initarg :inner-record)
   (drawing-options :accessor drawing-options
                    :initarg :drawing-options
                    :initform nil)
   under over))

(defgeneric make-bordered-output-record
    (stream shape record &key &allow-other-keys)
  (:documentation "Instantiates an output record of a class appropriate for the
  specified shape containing the given output record,
  and renders any decorations associated with the shape."))

(defgeneric draw-output-border-under
    (shape stream record &rest drawing-options &key &allow-other-keys)
  (:documentation
   "Draws the portion of border shape which is visible underneath the
    surrounded output"))

(defgeneric draw-output-border-over
    (shape stream record &rest drawing-options &key &allow-other-keys)
  (:documentation
   "Draws the portion of border shape which is visible above the surrounded
    output"))

;;; Keep this around just for fun, so we can list the defined border types.
(defvar *border-types* nil)

(defparameter *border-default-padding* 4)
(defparameter *border-default-radius*  7)
(defparameter *drop-shadow-default-offset* 6)

;;; Defining the border edges directly by the edges of the surrounded
;;; output record is wrong in the 'null bounding rectangle' case,
;;; occuring when the record has no chidren, or no children with
;;; non-null bounding rectangles.  Intuitively, the empty border
;;; should remain centered on the cursor.
(defmacro with-border-edges ((stream record) &body body)
  `(if (null-bounding-rectangle-p ,record)
    (multiple-value-bind (left top) (stream-cursor-position ,stream)
      (let ((right  (1+ left))
            (bottom (1+ top)))
        ,@body))
    (with-bounding-rectangle* (left top right bottom) ,record
      ,@body)))

(defmacro surrounding-output-with-border
    ((&optional stream &rest drawing-options &key (shape :rectangle)
                (move-cursor t)
                &allow-other-keys)
     &body body)
  (declare (ignore shape move-cursor))
  (setf stream (stream-designator-symbol stream '*standard-output*))
  (gen-invoke-trampoline 'invoke-surrounding-output-with-border
                         (list stream)
                         drawing-options
                         body))

(defmethod recompute-extent-for-changed-child
    ((record bordered-output-record) child x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (with-bounding-rectangle* (ox1 oy1 ox2 oy2) record
    (clear-output-record record)
    (%prepare-bordered-output-record record)
    (when-let ((parent (output-record-parent record)))
      (with-bounding-rectangle* (nx1 ny1 nx2 ny2) record
        (when (or (/= ox1 nx1) (/= oy1 ny1) (/= ox2 nx2) (/= oy2 ny2))
          (recompute-extent-for-changed-child parent record ox1 oy1 ox2 oy2))))))

(defun %prepare-bordered-output-record (border)
  (with-slots (under record over stream shape drawing-options) border
    (with-sheet-medium (medium stream)
      (flet ((capture-border (cont)
               (with-identity-transformation (medium)
                 (multiple-value-bind (cx cy) (output-record-position record)
                   (with-output-to-output-record (stream)
                     (setf (stream-cursor-position stream) (values cx cy))
                     (apply cont shape stream record drawing-options))))))
        (with-slots (under record over) border
          (setf under (capture-border #'draw-output-border-under)
                over  (capture-border #'draw-output-border-over))
          (add-output-record under  border)
          (add-output-record record border)
          (add-output-record over   border)))
      border)))

(defmethod make-bordered-output-record
    (stream shape inner-record &rest drawing-options)
  (let ((border (make-instance 'bordered-output-record
                               :stream stream
                               :shape shape
                               :inner-record inner-record
                               :drawing-options drawing-options)))
    (%prepare-bordered-output-record border)))

;;; This should have been exported by the CLIM package, otherwise you
;;; can't apply a computed list of drawing options.
(defun invoke-surrounding-output-with-border (stream cont
                                              &rest drawing-options
                                              &key (shape :rectangle)
                                              (move-cursor t)
                                              &allow-other-keys)
  (with-keywords-removed (drawing-options (:shape :move-cursor))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (let ((border (apply #'make-bordered-output-record
                           stream
                           shape
                           (with-output-to-output-record
                               (stream 'standard-sequence-output-record
                                       foo
                                       :x-position cx
                                       :y-position cy)
                             ;; w-o-t-o-r moved the cursor to the origin.
                             (setf (stream-cursor-position stream) (values cx cy))
                             (funcall cont stream)
                             (setf (values cx cy) (stream-cursor-position stream)))
                           drawing-options)))

        (when (stream-recording-p stream)
          (stream-add-output-record stream border))

        (when (stream-drawing-p stream)
          (with-output-recording-options (stream :record nil)
            (replay border stream)))

        (if move-cursor
            ;; move-cursor is true, move cursor to lower-right corner
            ;; of output.
            (with-bounding-rectangle* (left top right bottom) border
              (declare (ignore left top))
              (setf (stream-cursor-position stream) (values right bottom)))
            ;; move-cursor is false, preserve the cursor position from
            ;; after the output (I think this is right, it's useful
            ;; for :underline)
            (setf (stream-cursor-position stream) (values cx cy)))
        border))))

(defmethod draw-output-border-under
    (shape stream record &rest drawing-options &key &allow-other-keys)
  (declare (ignore drawing-options))
  (values))

(defmacro %%line-style-for-method ()
  `(or line-style
    (let ((mls (medium-line-style stream)))
      (make-line-style
       :unit      (or line-unit (line-style-unit mls))
       :thickness (or line-thickness (line-style-thickness mls))
       :cap-shape (or line-cap-shape (line-style-cap-shape mls))
       :dashes    (or line-dashes (line-style-dashes mls))))))

(defmacro %%adjusting-for-padding (&body body)
  `(let ((left   (- left   padding-left))
         (right  (+ right  padding-right))
         (top    (- top    padding-top))
         (bottom (+ bottom padding-bottom)))
    ,@body))

(defmacro %%adjusting-padding-for-line-style (&body body)
  `(let ((padding-left   (+ padding-left   (/ (or line-thickness 0) 2)))
         (padding-right  (+ padding-right  (/ (or line-thickness 0) 2)))
         (padding-top    (+ padding-top    (/ (or line-thickness 0) 2)))
         (padding-bottom (+ padding-bottom (/ (or line-thickness 0) 2))))
    ,@body))


(defmacro define-border-type (shape arglist &body body)
  (check-type arglist list)
  ;; The Franz User guide implies that &key isn't needed.
  (pushnew '&key arglist)
  `(progn
    (pushnew ',shape *border-types*)
    (defmethod draw-output-border-over ((shape (eql ',shape)) stream record
                                        &rest drawing-options)
      (with-border-edges (stream record)
        (apply (lambda (,@arglist &allow-other-keys)
                 ,@body)
               :stream stream
               :record record
               :left left
               :right right
               :top top
               :bottom bottom
               drawing-options)))))


;;;; Standard border types
(define-border-type :rectangle (stream left top right bottom
                                       ink outline-ink filled
                                       (padding *border-default-padding*)
                                       (padding-x padding)
                                       (padding-y padding)
                                       (padding-left   padding-x)
                                       (padding-right  padding-x)
                                       (padding-top    padding-y)
                                       (padding-bottom padding-y)
                                       line-style
                                       line-unit
                                       line-thickness
                                       line-cap-shape
                                       line-dashes)
  (%%adjusting-padding-for-line-style
    (%%adjusting-for-padding
      (let ((ink (or outline-ink
                     (and (not filled)
                          (or ink (medium-ink stream))))))
        (when ink
          (draw-rectangle* stream
                           left top right bottom
                           :line-style (%%line-style-for-method)
                           :ink ink
                           :filled nil))))))

(defmethod draw-output-border-under
    ((shape (eql :rectangle)) stream record
     &key background ink filled
     (padding *border-default-padding*)
     (padding-x padding)
     (padding-y padding)
     (padding-left   padding-x)
     (padding-right  padding-x)
     (padding-top    padding-y)
     (padding-bottom padding-y)
     shadow
     (shadow-offset *drop-shadow-default-offset*)
     line-thickness
     &allow-other-keys)

  (when (or background filled)
    (with-border-edges (stream record)
      (%%adjusting-padding-for-line-style
        (%%adjusting-for-padding
          (when (and shadow shadow-offset)
            (draw-rectangle* stream
                             (+ shadow-offset left)
                             (+ shadow-offset top)
                             (+ shadow-offset right)
                             (+ shadow-offset bottom)
                             :ink shadow
                             :filled t))
          (draw-rectangle* stream
                           left top
                           right bottom
                           :ink (or background ink +background-ink+)
                           :filled t))))))

(define-border-type :oval (stream left top right bottom
                                  (ink (medium-ink stream))
                                  outline-ink

                                  (padding *border-default-padding*)
                                  (padding-x padding)
                                  (padding-y padding)
                                  (padding-left   padding-x)
                                  (padding-right  padding-x)
                                  (padding-top    padding-y)
                                  (padding-bottom padding-y)

                                  line-style
                                  line-unit
                                  line-thickness
                                  line-cap-shape
                                  line-dashes)
  (%%adjusting-padding-for-line-style
    (%%adjusting-for-padding
      (when ink
        (draw-oval* stream
                    (/ (+ left right) 2) (/ (+  top bottom) 2)
                    (/ (- right left) 2) (/ (- bottom top) 2)
                    :line-style (%%line-style-for-method)
                    :ink (or outline-ink ink)
                    :filled nil)))))

(defmethod draw-output-border-under
    ((shape (eql :oval)) stream record &key
     background ink filled line-thickness
     (shadow-offset *drop-shadow-default-offset*)
     shadow
     (padding *border-default-padding*)
     (padding-x padding)
     (padding-y padding)
     (padding-left   padding-x)
     (padding-right  padding-x)
     (padding-top    padding-y)
     (padding-bottom padding-y)
     &allow-other-keys)
  (when (or filled background)
    (with-border-edges (stream record)
      (%%adjusting-padding-for-line-style
        (%%adjusting-for-padding
          (when shadow
            (draw-oval* stream
                        (+ shadow-offset (/ (+ left right) 2))
                        (+ shadow-offset (/ (+ top bottom) 2))
                        (/ (- right left) 2) (/ (- bottom top) 2)
                        :ink shadow
                        :filled t))
          (draw-oval* stream
                      (/ (+ left right) 2) (/ (+ top bottom) 2)
                      (/ (- right left) 2) (/ (- bottom top) 2)
                      :ink (or background ink +background-ink+)
                      :filled t))))))

;;; A filled :drop-shadow is almost identical to :rectangle with a
;;; :shadow keyword. So, just use :rectangle instead.
(define-border-type :drop-shadow (stream
                                  left top right bottom
                                  (filled nil)
                                  (shadow-offset 3)
                                  outline-ink
                                  background
                                  (ink (medium-ink stream))
                                  (padding *border-default-padding*)
                                  (padding-x padding)
                                  (padding-y padding)
                                  (padding-left   padding-x)
                                  (padding-right  padding-x)
                                  (padding-top    padding-y)
                                  (padding-bottom padding-y)
                                  line-style
                                  line-unit
                                  line-thickness
                                  line-cap-shape
                                  line-dashes)
  (%%adjusting-padding-for-line-style
    (%%adjusting-for-padding
      (draw-rectangle* stream
                       left  top
                       right bottom
                       :line-style (%%line-style-for-method)
                       :ink (or outline-ink ink)
                       :filled nil)
      ;; If the user has (wisely) chosen my more modern "filled"
      ;; style, we'll simply draw two rectangles, one offset from the
      ;; other, to provide a solid background color and shadow.  Note
      ;; that the background keyword implies :filled t.
      (unless (or filled background)
        (when (< shadow-offset 0)                ; FIXME!
          (setf shadow-offset 0))
        (draw-rectangle* stream
                         right (+ top shadow-offset)
                         (+ right shadow-offset) bottom
                         :ink (or outline-ink ink)
                         :filled t)
        (draw-rectangle* stream
                         (+ left shadow-offset) bottom
                         (+ right shadow-offset) (+ bottom shadow-offset)
                         :ink (or outline-ink ink)
                         :filled t)))))

(defmethod draw-output-border-under
    ((shape (eql :drop-shadow)) stream record &key
     (filled nil)
     (shadow-offset *drop-shadow-default-offset*)
     background
     outline-ink
     shadow
     (ink +foreground-ink+)
     line-thickness
     (padding *border-default-padding*)
     (padding-x padding)
     (padding-y padding)
     (padding-left   padding-x)
     (padding-right  padding-x)
     (padding-top    padding-y)
     (padding-bottom padding-y))
  (with-border-edges (stream record)
    (%%adjusting-padding-for-line-style
      (%%adjusting-for-padding
        (when (or filled background)
          (let* ((fill-color   (or background +background-ink+))
                 (shadow-color (or shadow outline-ink ink +background-ink+)))
            (draw-rectangle* stream
                             (+ shadow-offset left)
                             (+ shadow-offset top)
                             (+ shadow-offset right)
                             (+ shadow-offset bottom)
                             :filled t
                             :ink shadow-color)
            (draw-rectangle* stream left top right bottom
                             :filled t
                             :ink fill-color)))))))

(define-border-type :underline (stream record
                                       (ink (medium-ink stream))
                                       line-style
                                       line-unit
                                       line-thickness
                                       line-cap-shape
                                       line-dashes)
  (let ((line-style (%%line-style-for-method)))
    (labels ((fn (record)
               (loop for child across (output-record-children record) do
                     (typecase child
                       (text-displayed-output-record
                        (with-bounding-rectangle* (left top right bottom) child
                          (declare (ignore top))
                          (draw-line* stream left bottom right bottom
                                      :ink ink
                                      :line-style line-style)))
                       (updating-output-record nil)
                       (compound-output-record (fn child))))))
      (fn record))))

(define-border-type :crossout (stream record
                                       (ink (medium-ink stream))
                                       line-style
                                       line-unit
                                       line-thickness
                                       line-cap-shape
                                       line-dashes)
  (let ((line-style (%%line-style-for-method)))
    (labels ((fn (record)
               (loop for child across (output-record-children record) do
                     (typecase child
                       (text-displayed-output-record
                        (with-bounding-rectangle* (left top right bottom) child
                          (let ((middle (/ (+ bottom top) 2)))
                            (draw-line* stream left middle right middle
                                        :ink ink
                                        :line-style line-style))))
                       (updating-output-record nil)
                       (compound-output-record (fn child))))))
      (fn record))))

(define-border-type :inset (stream left top right bottom
                                   (padding *border-default-padding*)
                                   (padding-x padding)
                                   (padding-y padding)
                                   (padding-left   padding-x)
                                   (padding-right  padding-x)
                                   (padding-top    padding-y)
                                   (padding-bottom padding-y))
  (%%adjusting-for-padding
    (let ((dark  *3d-dark-color*)
          (light *3d-light-color*))
      (flet ((draw (left-edge right-edge bottom-edge top-edge light dark)
               (draw-line* stream left-edge bottom-edge left-edge top-edge
                           :ink dark)
               (draw-line* stream left-edge top-edge right-edge top-edge
                           :ink dark)
               (draw-line* stream right-edge bottom-edge right-edge top-edge
                           :ink light)
               (draw-line* stream left-edge bottom-edge right-edge bottom-edge
                           :ink light)))
        (draw left right bottom top light dark)
        (draw (1+ left) (1- right) (1- bottom) (1+ top) light dark)))))

;;; Padding defaults to radius. I'm not sure if this is right, but it
;;; lets you do things like forcing the radius on one side to zero,
;;; flattening that side, and stopping the edge from jamming against
;;; the output (saving you the trouble of having to manually hack the
;;; padding on one side to compensate). If someone can think of a
;;; better approach to defaulting the radius and padding arguments, do
;;; share.
(define-border-type :rounded (stream left top right bottom
                                     (radius *border-default-radius*)
                                     (radius-x radius)
                                     (radius-y radius)
                                     (radius-left   radius-x)
                                     (radius-right  radius-x)
                                     (radius-top    radius-y)
                                     (radius-bottom radius-y)
                                     (padding radius)
                                     (padding-x padding)
                                     (padding-y padding)
                                     (padding-left   padding-x)
                                     (padding-right  padding-x)
                                     (padding-top    padding-y)
                                     (padding-bottom padding-y)
                                     ink
                                     filled
                                     outline-ink
                                     line-style
                                     line-unit
                                     line-thickness
                                     line-cap-shape
                                     line-dashes)
  (%%adjusting-padding-for-line-style
    (%%adjusting-for-padding
      (let ((ink (or outline-ink
                     (and (not filled) (or ink +foreground-ink+)))))
        (when ink
          (draw-rounded-rectangle* stream left top right bottom
                                   :radius-left   radius-left   ;padding-left
                                   :radius-right  radius-right  ;padding-right
                                   :radius-top    radius-top    ;padding-top
                                   :radius-bottom radius-bottom ;padding-bottom
                                   :ink ink
                                   :filled nil
                                   :line-style (%%line-style-for-method)))))))

(defmethod draw-output-border-under
    ((shape (eql :rounded)) stream record &key
     (radius *border-default-radius*)
     (radius-x radius)
     (radius-y radius)
     (radius-left   radius-x)
     (radius-right  radius-x)
     (radius-top    radius-y)
     (radius-bottom radius-y)
     (padding radius)
     (padding-x padding)
     (padding-y padding)
     (padding-left   padding-x)
     (padding-right  padding-x)
     (padding-top    padding-y)
     (padding-bottom padding-y)
     ink
     filled
     background
     shadow
     (shadow-offset *drop-shadow-default-offset*)
     line-thickness)
  (with-border-edges (stream record)
    (%%adjusting-padding-for-line-style
      (%%adjusting-for-padding
        (when (or filled background)
          (when (and shadow shadow-offset)
            (draw-rounded-rectangle* stream
                                     (+ left shadow-offset)
                                     (+ top shadow-offset)
                                     (+ shadow-offset right)
                                     (+ shadow-offset bottom)
                                     :radius-left radius-left
                                     :radius-right radius-right
                                     :radius-top radius-top
                                     :radius-bottom radius-bottom
                                     :ink shadow
                                     :filled t))
          (let ((ink (or background ink +background-ink+)))
            (draw-rounded-rectangle* stream left top right bottom
                                     :radius-left radius-left
                                     :radius-right radius-right
                                     :radius-top radius-top
                                     :radius-bottom radius-bottom
                                     :ink ink
                                     :filled t)))))))

(define-border-type :ellipse (stream left top right bottom
                                     (padding *border-default-radius*)
                                     (padding-x padding)
                                     (padding-y padding)
                                     (padding-left   padding-x)
                                     (padding-right  padding-x)
                                     (padding-top    padding-y)
                                     (padding-bottom padding-y)
                                     ink outline-ink filled
                                     circle
                                     line-style
                                     line-unit
                                     min-radius
                                     (min-radius-x min-radius)
                                     (min-radius-y min-radius)
                                     line-thickness
                                     line-cap-shape
                                     line-dashes)
    (%%adjusting-padding-for-line-style
      (%%adjusting-for-padding
        (let ((ink (or outline-ink (and (not filled)
                                        (or ink +foreground-ink+)))))
          (when ink
            (let* ((cx (/ (+ right left) 2))
                   (cy (/ (+ top bottom) 2))
                   (radius-x (- right  cx))
                   (radius-y (- bottom cy))
                   (radius-x (if circle
                                 (sqrt (+ (* radius-x radius-x)
                                          (* radius-y radius-y)))
                                 radius-x))
                   (radius-y (if circle radius-x radius-y))
                   (fx (/ radius-x (cos (/ pi 4))))
                   (fy (/ radius-y (sin (/ pi 4))))
                   (fx (max fx (or min-radius-x 0)))
                   (fy (max fy (or min-radius-y 0))))
              (draw-ellipse* stream cx cy fx 0 0 fy
                             :filled nil :ink ink
                             :line-style (%%line-style-for-method))))))))

(defmethod draw-output-border-under
    ((shape (eql :ellipse)) stream record &key
     (padding *border-default-radius*)
     (padding-x padding)
     (padding-y padding)
     (padding-left   padding-x)
     (padding-right  padding-x)
     (padding-top    padding-y)
     (padding-bottom padding-y)
     ink background filled
     circle
     min-radius
     shadow
     (shadow-offset *drop-shadow-default-offset*)
     (min-radius-x min-radius)
     (min-radius-y min-radius)
     line-thickness)
  (with-border-edges (stream record)
    (%%adjusting-padding-for-line-style
      (%%adjusting-for-padding
        (let ((ink (or background (and filled (or ink +background-ink+)))))
          (when ink
            (let* ((cx (/ (+ right left) 2))
                   (cy (/ (+ top bottom) 2))
                   (radius-x (- right  cx))
                   (radius-y (- bottom cy))
                   (radius-x (if circle
                                 (sqrt (+ (* radius-x radius-x)
                                          (* radius-y radius-y)))
                                 radius-x))
                   (radius-y (if circle radius-x radius-y))
                   (fx (/ radius-x (cos (/ pi 4))))
                   (fy (/ radius-y (sin (/ pi 4))))
                   (fx (max fx (or min-radius-x 0)))
                   (fy (max fy (or min-radius-y 0))) )
              (when (and shadow shadow-offset)
                (draw-ellipse* stream (+ cx shadow-offset) (+ cy shadow-offset)
                               fx 0 0 fy :filled t :ink shadow))
              (draw-ellipse* stream cx cy fx 0 0 fy
                             :filled t :ink ink))))))))

(defmethod highlight-output-record
    ((record bordered-output-record) stream state)
  (format *trace-output* "b-o-r ~A ~A ~A~%" record stream state)
  (call-next-method))

(defgeneric highlight-output-record-tree (record stream state))

;;; Suppress highlighting of the border decoration itself:
(defmethod highlight-output-record-tree
    ((record bordered-output-record) stream state)
  (highlight-output-record-tree (slot-value record 'record) stream state))

;;;; Highlighting of bordered output records
(defclass highlighting-bordered-output-record (bordered-output-record)
  ((original-drawing-options :initarg :drawing-options
                             :reader original-drawing-options
                             :documentation "Preserves unmodified drawing-options which may be changed during highlight.")))

(defmethod highlight-output-record-tree
    ((record highlighting-bordered-output-record)
     stream state)
  ;; Was this border created with the required options for highlighting?
  (if (and (member state '(:highlight :unhighlight))
           (or (getf (drawing-options record) :highlight-background)
               (getf (drawing-options record) :highlight-outline)))
      (highlight-output-record record stream state)
      (call-next-method)))

(defmethod highlight-output-record
    ((record highlighting-bordered-output-record) stream state)
  (let ((drawing-options (drawing-options record)))
    (destructuring-bind (&key background
                              outline-ink
                              highlight-background
                              highlight-outline
                              &allow-other-keys)
        drawing-options
      (if (and (member state '(:highlight :unhighlight))
               (or highlight-background highlight-outline))
          (flet ((redraw (new-drawing-options)
                   (setf (drawing-options record) new-drawing-options)
                   (clear-output-record record)
                   (%prepare-bordered-output-record record)
                   ;; Great, this again..
                   (queue-repaint stream
                      (make-instance 'window-repaint-event
                                    :sheet stream
                                    :region (transform-region
                                            (sheet-native-transformation stream)
                                            record)))))
            (ecase state
              (:highlight
               (with-keywords-removed (drawing-options (:background :outline-ink))
                 (redraw
                  (list* :background
                         (or (and (eql t highlight-background)
                                  (highlight-shade
                                   (or background
                                       (getf drawing-options :ink)
                                       +background-ink+)))
                             highlight-background
                             background)
                         :outline-ink
                         (or (and (eql t highlight-outline)
                                  (highlight-shade
                                   (or outline-ink
                                       (getf drawing-options :ink)
                                       +foreground-ink+)))
                             highlight-outline
                             outline-ink)
                         drawing-options))))
              (:unhighlight (redraw (original-drawing-options record)))))
          (call-next-method)))))

(defmacro define-default-highlighting-method (shape)
  `(defmethod make-bordered-output-record
       (stream (shape (eql ,shape)) inner-record &rest drawing-options)
     (let ((border (make-instance 'highlighting-bordered-output-record
                                  :stream stream
                                  :shape shape
                                  :drawing-options drawing-options
                                  :inner-record inner-record)))
       (%prepare-bordered-output-record border))))

(define-default-highlighting-method :rectangle)
(define-default-highlighting-method :oval)
(define-default-highlighting-method :drop-shadow)
(define-default-highlighting-method :rounded)
(define-default-highlighting-method :ellipse)
