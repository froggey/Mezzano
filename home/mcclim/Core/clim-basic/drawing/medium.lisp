;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
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

;;;; TODO

;;; Text Styles

;;; - *UNDEFINED-TEXT-STYLE* is missing
;;; - Why is (EQ (MAKE-TEXT-STYLE NIL NIL 10) (MAKE-TEXT-STYLE NIL NIL 10.0005)) = T?
;;;   Does it matter?
;;; - Don't we want a weak hash-table for *TEXT-STYLE-HASH-TABLE*
;;;
;;; --GB 2002-02-26

;;; Notes

;;; The text-style protocol is kind of useless for now. How is an
;;; application programmer expected to implement new text-styles? I
;;; think we would need something like:
;;
;;;  TEXT-STYLE-CHARACTER-METRICS text-style character[1]
;;;    -> width, ascent, descent, left-bearing, right-bearing
;;
;;;  TEXT-STYLE-DRAW-TEXT text-style medium string x y
;;;  Or even better:
;;;  DESIGN-FROM-TEXT-STYLE-CHARACTER text-style character
;;
;;
;;; And when you start to think about it, text-styles are not fonts. So
;;; we need two protocols: A text style protocol and a font protocol.
;;
;;; A text style is then something, which maps a sequence of characters
;;; into a couple of drawing commands, while probably using some font.
;;
;;; While a font is something, which maps a _glyph index_ into a design.
;;
;;; Example: Underlined with extra word spacing is a text style, while
;;;          Adobe Times Roman 12pt is a font.
;;
;;; And [it can't be said too often] unicode is not a glyph encoding
;;; but more a kind of text formating.
;;;
;;; [1] or even a code position
;;; --GB

(in-package :clim-internals)

;;;;
;;;; 11 Text Styles
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric text-style-equalp (style1 style2)
    (:method ((style1 text-style) (style2 text-style))
      (eq style1 style2)))

  (defclass standard-text-style (text-style)
    ((family   :initarg :text-family
               :initform :fix
               :reader text-style-family)
     (face     :initarg :text-face
               :initform :roman
               :reader text-style-face)
     (size     :initarg :text-size
               :initform :normal
               :reader text-style-size)))

  (defmethod make-load-form ((obj standard-text-style) &optional env)
    (declare (ignore env))
    (with-slots (family face size) obj
      `(make-text-style ',family ',face ',size)))

  (defun family-key (family)
    (case family
      ((nil) 0)
      ((:fix) 1)
      ((:serif) 2)
      ((:sans-serif) 3)))

  (defun face-key (face)
    (typecase face
      (null 0)
      ((eql :roman) 1)
      ((eql :bold) 2)
      ((eql :italic) 3)
      ((cons (eql :bold) (cons (eql :italic))) 4)
      ((cons (eql :italic) (cons (eql :bold))) 4)))

  (defun size-key (size)
    (if (numberp size)
        (+ 10 (round (* 256 size)))
        (case size
          ((nil)         0)
          ((:tiny)       1)
          ((:very-small) 2)
          ((:small)      3)
          ((:normal)     4)
          ((:large)      5)
          ((:very-large) 6)
          ((:huge)       7)
          ((:smaller)    8)
          ((:larger)     9))))

  (defun text-style-key (family face size)
    (when-let ((size-key (size-key size))
               (face-key (face-key face))
               (family-key (family-key family)))
      (logior (ash size-key   8)
              (ash face-key   4)
              (ash family-key 0))))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *text-style-hash-table* (make-hash-table :test #'eql))
    (defvar *extended-text-style-hash-table* (make-hash-table :test #'equal)))

  (defun make-text-style (family face size)
    (if-let ((key (text-style-key family face size)))
      ;; Portable text styles have always been cached in McCLIM like
      ;; this: (as permitted by the CLIM spec for immutable objects,
      ;; section 2.4)
      ;; A 32-bit key has 24-bit for the output of `size-key'. That's
      ;; around font size 100000.
      (locally (declare (type (unsigned-byte 32) key))
        (ensure-gethash key *text-style-hash-table*
                        (make-text-style-1 family face size)))
      ;; Extended text styles using custom components is cached using
      ;; an appropriate hash table to ensure `EQL' of the same
      ;; extended text styles
      (ensure-gethash (list family face size) *extended-text-style-hash-table*
                      (make-text-style-1 family face size))))

  (defun make-text-style-1 (family face size)
    (make-instance 'standard-text-style
                   :text-family family
                   :text-face face
                   :text-size size)))

(defmethod print-object ((self standard-text-style) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~{~S~^ ~}" (multiple-value-list (text-style-components self)))))

(defmethod text-style-equalp ((style1 standard-text-style)
                              (style2 standard-text-style))
  (and (equal (text-style-family style1) (text-style-family style2))
       (equal (text-style-face style1) (text-style-face style2))
       (eql (text-style-size style1) (text-style-size style2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *default-text-style* (make-text-style :sans-serif :roman :normal))
  (defconstant *undefined-text-style* *default-text-style*)

  (defconstant *smaller-sizes* '(:huge :very-large :large :normal
                                 :small :very-small :tiny :tiny))

  (defconstant *larger-sizes* '(:tiny :very-small :small :normal
                                :large :very-large :huge :huge))

  (defconstant *font-scaling-factor* 4/3)
  (defconstant *font-min-size* 6)
  (defconstant *font-max-size* 48)

  (defconstant +font-sizes+
    '(:normal 14 :tiny 8 :very-small 10 :small 12 :large 18 :very-large 20 :huge 24)
    "Mapping between keyword and a font size."))

(defun text-style-components-fully-specified-p (family face size)
  (and family
       face
       size
       (or (realp size)
           (member size (load-time-value
                         (remove-if-not #'keywordp +font-sizes+) t)))))

(defun text-style-fully-specified-p (text-style)
  (multiple-value-bind (family face size) (text-style-components text-style)
    (text-style-components-fully-specified-p family face size)))

(defun find-smaller-size (size)
  (if (numberp size)
      (max (round (/ size *font-scaling-factor*)) *font-min-size*)
      (cadr (member size *smaller-sizes*))))

(defun find-larger-size (size)
  (if (numberp size)
      (min (round (* size *font-scaling-factor*)) *font-max-size*)
      (cadr (member size *larger-sizes*))))

(defmethod text-style-components ((text-style standard-text-style))
  (values (text-style-family   text-style)
          (text-style-face     text-style)
          (text-style-size     text-style)))

;;; Device-Font-Text-Style class

(defclass device-font-text-style (text-style)
  ((display-device :initarg :display-device :accessor display-device)
   (device-font-name :initarg :device-font-name :accessor device-font-name)))

(defmethod print-object ((self device-font-text-style) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~S on ~S" (device-font-name self) (display-device self))))

(defun device-font-text-style-p (s)
  (typep s 'device-font-text-style))

(defmethod text-style-components ((text-style device-font-text-style))
  (values :device :device :device))

(defmethod text-style-mapping ((port basic-port)
                               (text-style text-style)
                               &optional character-set)
  (declare (ignore port character-set))
  ;; INV the around method looks up the cache for the mapping. If we
  ;; end up here it is a game over. For consistency we return the
  ;; text-style like other methods. -- jd 2020-01-15
  text-style)

(defmethod text-style-mapping ((port basic-port)
                               text-style
                               &optional character-set)
  (text-style-mapping port (parse-text-style text-style) character-set))

(defmethod text-style-mapping :around ((port basic-port)
                                       (text-style text-style)
                                       &optional character-set)
  (declare (ignore character-set))
  ;; Cache mappings only for fully specified text styles.
  (let ((mappings (port-text-style-mappings port)))
    (if (or (device-font-text-style-p text-style)
            (text-style-fully-specified-p text-style))
        (ensure-gethash text-style mappings (call-next-method))
        (or (gethash text-style mappings) (call-next-method)))))

(defmethod (setf text-style-mapping) (mapping
                                      (port basic-port)
                                      text-style
                                      &optional character-set)
  (declare (ignore character-set))
  (setf (text-style-mapping port (parse-text-style text-style)) mapping))

(defmethod (setf text-style-mapping) (mapping
                                      (port basic-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore character-set))
  (when (listp mapping)
    (error "Delayed mapping is not supported.")) ; FIXME
  (setf (gethash text-style (port-text-style-mappings port))
        mapping))

(defmethod make-device-font-text-style (port font-name)
  (let ((text-style (make-instance 'device-font-text-style
                                   :display-device port
                                   :device-font-name font-name)))
    (setf (text-style-mapping port text-style) font-name)
    text-style))

;;; Text-style utilities

(defmethod merge-text-styles (s1 s2)
  (when (and (typep s1 'text-style)
             (eq s1 s2))
    (return-from merge-text-styles s1))
  (setq s1 (parse-text-style s1))
  (setq s2 (parse-text-style s2))
  (if (and (not (device-font-text-style-p s1))
           (not (device-font-text-style-p s2)))
      (let* ((family (or (text-style-family s1) (text-style-family s2)))
             (face1 (text-style-face s1))
             (face2 (text-style-face s2))
             (face (if (or (and (eq face1 :bold)   (eq face2 :italic))
                           (and (eq face1 :italic) (eq face2 :bold)))
                       '(:bold :italic)
                       (or face1 face2)))
             (size1 (text-style-size s1))
             (size2 (text-style-size s2))
             (size (case size1
                     ((nil) size2)
                     (:smaller (find-smaller-size size2))
                     (:larger (find-larger-size size2))
                     (t size1))))
        (make-text-style family face size))
      s1))

(defun parse-text-style (style)
  (cond ((text-style-p style) style)
        ((null style) (make-text-style nil nil nil)) ; ?
        ((and (listp style) (alexandria:length= 3 style))
         (destructuring-bind (family face size) style
           (make-text-style family face size)))
        (t (error "Invalid text style specification ~S." style))))

(defmacro with-text-style ((medium text-style) &body body)
  (when (eq medium t)
    (setq medium '*standard-output*))
  (check-type medium symbol)
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (parse-text-style ,text-style)))))

(defmethod invoke-with-text-style ((sheet sheet) continuation text-style)
  (let ((medium (sheet-medium sheet))) ; FIXME: WITH-SHEET-MEDIUM
    (with-text-style (medium text-style)
      (funcall continuation sheet))))

(defmethod invoke-with-text-style ((medium medium) continuation text-style)
  (invoke-with-drawing-options
   medium continuation
   :text-style (merge-text-styles text-style (medium-merged-text-style medium))))

;;; For compatibility with real CLIM, which apparently lets you call this
;;; on non-CLIM streams.

(defmethod invoke-with-text-style ((medium t) continuation text-style)
  (declare (ignore text-style))
  (funcall continuation medium))

(defmacro with-text-family ((medium family) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (make-text-style ,family nil nil)))))

(defmacro with-text-face ((medium face) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (make-text-style nil ,face nil)))))

(defmacro with-text-size ((medium size) &body body)
  (declare (type symbol medium))
  (when (eq medium t) (setq medium '*standard-output*))
  (with-gensyms (cont)
    `(flet ((,cont (,medium)
              ,(declare-ignorable-form* medium)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-text-style ,medium #',cont
                               (make-text-style nil nil ,size)))))


;;; GRAPHICS-STATE class

;;; Factor out the graphics state portions of the output records so
;;; they can be manipulated seperately e.g., by incremental
;;; display. The individual slots of a graphics state are factored
;;; into mixin classes so that each output record can capture only the
;;; state that it needs. -- moore
;;;
;;; Now graphics-state is an ancestor of both medium and some of
;;; output-records. Thanks to that we can treat medium as
;;; graphics-state without consing new objects and assign its state
;;; from another graphics-state object. -- jd


(defclass graphics-state ()
  ()
  (:documentation "Stores those parts of the medium/stream graphics state
  that need to be restored when drawing an output record"))

(defclass gs-transformation-mixin (graphics-state)
  ((transformation :initarg :transformation :accessor graphics-state-transformation
                   :documentation "Medium transformation.")))

(defmethod initialize-instance :after ((obj gs-transformation-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'transformation)))
    (setf (slot-value obj 'transformation) (graphics-state-transformation medium))))

(defclass gs-ink-mixin (graphics-state)
  ((ink :initarg :ink :accessor graphics-state-ink)))

(defmethod initialize-instance :after ((obj gs-ink-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'ink)))
    (setf (slot-value obj 'ink) (graphics-state-ink medium))))

(defclass gs-clip-mixin (graphics-state)
  ((clipping-region :initarg :clipping-region :accessor graphics-state-clip
                    :documentation "Clipping region in stream coordinates.")))

(defmethod initialize-instance :after ((obj gs-clip-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'clipping-region)))
    (setf (slot-value obj 'clipping-region) (graphics-state-clip medium))))

(defclass gs-line-style-mixin (graphics-state)
  ((line-style :initarg :line-style :accessor graphics-state-line-style)))

(defmethod initialize-instance :after ((obj gs-line-style-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'line-style)))
    (setf (slot-value obj 'line-style) (graphics-state-line-style medium))))

(defgeneric graphics-state-line-style-border (record medium)
  (:method ((record gs-line-style-mixin) (medium medium))
    (/ (line-style-effective-thickness (graphics-state-line-style record)
                                       medium)
       2))
  (:method ((record gs-line-style-mixin) (sheet sheet))
    (with-sheet-medium (medium sheet)
      (graphics-state-line-style-border record medium))))

(defclass gs-text-style-mixin (graphics-state)
  ((text-style :initarg :text-style :accessor graphics-state-text-style)))

(defmethod initialize-instance :after ((obj gs-text-style-mixin)
                                       &key
                                         (stream nil)
                                         (medium (when stream
                                                   (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'text-style)))
    (setf (slot-value obj 'text-style) (graphics-state-text-style medium))))

(defclass complete-medium-state
    (gs-ink-mixin gs-clip-mixin gs-line-style-mixin gs-text-style-mixin gs-transformation-mixin)
  ())

(defgeneric (setf graphics-state) (new-gs gs)
  (:method ((new-gs graphics-state) (gs graphics-state))
    #+(or) "This is a no-op, but :after methods don't work without a primary method.")
  (:method :after ((new-gs gs-ink-mixin) (gs gs-ink-mixin))
    (setf (graphics-state-ink gs) (graphics-state-ink new-gs)))
  (:method :after ((new-gs gs-clip-mixin) (gs gs-clip-mixin))
    (setf (graphics-state-clip gs) (graphics-state-clip new-gs)))
  (:method :after ((new-gs gs-line-style-mixin) (gs gs-line-style-mixin))
    (setf (graphics-state-line-style gs) (graphics-state-line-style new-gs)))
  (:method :after ((new-gs gs-text-style-mixin) (gs gs-text-style-mixin))
    (setf (graphics-state-text-style gs) (graphics-state-text-style new-gs)))
  (:method :after ((new-gs gs-transformation-mixin) (gs gs-transformation-mixin))
    (setf (graphics-state-transformation gs) (graphics-state-transformation new-gs))))


;;; MEDIUM class

(defclass transform-coordinates-mixin ()
  ;; This class is reponsible for transforming coordinates in an :around method
  ;; on medium-draw-xyz. It is mixed into basic-medium. We should document it
  ;; and mix it in the appropriate downstream backend-specific medium.  Mixing
  ;; in basic-medium makes hardware-based transformations hard. -- jd 2018-03-06
  ())

(defclass basic-medium (transform-coordinates-mixin complete-medium-state medium)
  ((foreground :initarg :foreground
               :initform +black+
               :accessor medium-foreground
               :reader foreground)
   (background :initarg :background
               :initform +white+
               :accessor medium-background
               :reader background)
   (ink :initarg :ink
        :initform +foreground-ink+
        :accessor medium-ink)
   (transformation :type transformation
                   :initarg :transformation
                   :initform +identity-transformation+
                   :accessor medium-transformation)
   (clipping-region :type region
                    :initarg :clipping-region
                    :initform +everywhere+
                    :documentation "Clipping region in the SHEET coordinates.")
   ;; always use this slot through its accessor, since there may
   ;; be secondary methods on it -RS 2001-08-23
   (line-style :initarg :line-style
               :initform (make-line-style)
               :accessor medium-line-style)
   ;; always use this slot through its accessor, since there may
   ;; be secondary methods on it -RS 2001-08-23
   (text-style :initarg :text-style
               :initform *default-text-style*
               :accessor medium-text-style)
   (default-text-style :initarg :default-text-style
                       :initform *default-text-style*
                       :accessor medium-default-text-style)
   (sheet :initarg :sheet
          :initform nil                 ; this means that medium is not linked to a sheet
          :reader medium-sheet
          :writer (setf %medium-sheet) ))
  (:documentation "The basic class, on which all CLIM mediums are built."))

(defmethod medium-drawable ((medium basic-medium))
  (when-let ((sheet (medium-sheet medium)))
    (sheet-mirror sheet)))

(defclass ungrafted-medium (basic-medium) ())

(defmethod initialize-instance :after ((medium basic-medium) &rest args)
  (declare (ignore args))
  ;; Initial CLIPPING-REGION is in coordinates, given by initial
  ;; TRANSFORMATION, but we store it in SHEET's coords.
  (with-slots (clipping-region) medium
    (setf clipping-region (transform-region (medium-transformation medium)
                                            clipping-region))))

(defmethod medium-clipping-region ((medium medium))
  (untransform-region (medium-transformation medium)
                      (slot-value medium 'clipping-region)))

(defmethod (setf medium-clipping-region) (region (medium medium))
  (setf (slot-value medium 'clipping-region)
        (transform-region (medium-transformation medium)
                          region)))

(defmethod (setf medium-clipping-region) :after (region (medium medium))
  (declare (ignore region))
  (let ((sheet (medium-sheet medium)))
    (when sheet
      (%invalidate-cached-device-regions sheet))))

(defmethod (setf medium-transformation) :after (transformation (medium medium))
  (declare (ignore transformation))
  (let ((sheet (medium-sheet medium)))
    (when sheet
      (%invalidate-cached-device-transformations sheet))))

(defmethod medium-merged-text-style ((medium medium))
  (merge-text-styles (medium-text-style medium) (medium-default-text-style medium)))

;;; with-sheet-medium moved to output.lisp. --GB
;;; with-sheet-medium-bound moved to output.lisp. --GB

(defmacro with-pixmap-medium ((medium pixmap) &body body)
  (let ((old-medium (gensym))
        (old-pixmap (gensym)))
    `(let* ((,old-medium (pixmap-medium ,pixmap))
            (,medium (or ,old-medium (make-medium (port ,pixmap) ,pixmap)))
            (,old-pixmap (medium-sheet ,medium)))
       (setf (pixmap-medium ,pixmap) ,medium)
       (setf (%medium-sheet ,medium) ,pixmap) ;is medium a basic medium? --GB
       (unwind-protect
            (progn
              ,@body)
         (setf (pixmap-medium ,pixmap) ,old-medium)
         (setf (%medium-sheet ,medium) ,old-pixmap)))))

;;; Medium Device functions

(defgeneric medium-device-transformation (medium))

(defmethod medium-device-transformation ((medium medium))
  (sheet-device-transformation (medium-sheet medium)))

(defgeneric medium-device-region (medium))

(defmethod medium-device-region ((medium medium))
  (sheet-device-region (medium-sheet medium)))


;;; Line-Style class

(defgeneric line-style-equalp (arg1 arg2))

(defclass standard-line-style (line-style)
  ((unit        :initarg :line-unit
                :initform :normal
                :reader line-style-unit
                :type (member :normal :point :coordinate))
   (thickness   :initarg :line-thickness
                :initform 1
                :reader line-style-thickness
                :type real)
   (joint-shape :initarg :line-joint-shape
                :initform :miter
                :reader line-style-joint-shape
                :type (member :miter :bevel :round :none))
   (cap-shape   :initarg :line-cap-shape
                :initform :butt
                :reader line-style-cap-shape
                :type (member :butt :square :round :no-end-point))
   (dashes      :initarg :line-dashes
                :initform nil
                :reader line-style-dashes
                :type (or (member t nil)
                          sequence))))

(defun make-line-style (&key (unit :normal) (thickness 1)
                          (joint-shape :miter) (cap-shape :butt)
                          (dashes nil))
  (make-instance 'standard-line-style
                 :line-unit unit
                 :line-thickness thickness
                 :line-joint-shape joint-shape
                 :line-cap-shape cap-shape
                 :line-dashes dashes))

(defmethod print-object ((self standard-line-style) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~{~S ~S~^ ~}"
            (mapcan (lambda (slot)
                      (when (slot-boundp self slot)
                        (list
                         (intern (symbol-name slot) :keyword)
                         (slot-value self slot))))
                    '(unit thickness joint-shape cap-shape dashes)))))

(defmethod line-style-effective-thickness (line-style medium)
  ;; FIXME
  (declare (ignore medium))
  (line-style-thickness line-style))

(defmethod medium-miter-limit ((medium medium))
  #.(* 2 single-float-epsilon))

(defmethod line-style-equalp ((style1 standard-line-style)
                              (style2 standard-line-style))
  (and (eql (line-style-unit style1) (line-style-unit style2))
       (eql (line-style-thickness style1) (line-style-thickness style2))
       (eql (line-style-joint-shape style1) (line-style-joint-shape style2))
       (eql (line-style-cap-shape style1) (line-style-cap-shape style2))
       (eql (line-style-dashes style1) (line-style-dashes style2))))


;;; Misc ops

(defmacro with-output-buffered ((medium &optional (buffer-p t)) &body body)
  (declare (type symbol medium))
  (when (eq medium t)
    (setq medium '*standard-output*))
  (let ((old-buffer (gensym)))
    `(let ((,old-buffer (medium-buffering-output-p ,medium)))
       (setf (medium-buffering-output-p ,medium) ,buffer-p)
       (unwind-protect (progn ,@body)
         (setf (medium-buffering-output-p ,medium) ,old-buffer)))))


;;; BASIC-MEDIUM class

(defmacro with-transformed-position ((transformation x y) &body body)
  `(multiple-value-bind (,x ,y) (transform-position ,transformation ,x ,y)
     ,@body))

(defmacro with-transformed-distance ((transformation dx dy) &body body)
  `(multiple-value-bind (,dx ,dy) (transform-distance ,transformation ,dx ,dy)
     ,@body))

(defmacro with-transformed-positions ((transformation coord-seq) &body body)
  `(let ((,coord-seq (transform-positions ,transformation ,coord-seq)))
     ,@body))


;;; Pixmaps

(defmethod medium-copy-area ((from-drawable basic-medium) from-x from-y width height
                             to-drawable to-x to-y)
  (declare (ignore from-x from-y width height to-drawable to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))

(defmethod medium-copy-area (from-drawable from-x from-y width height
                             (to-drawable basic-medium) to-x to-y)
  (declare (ignore from-drawable from-x from-y width height to-x to-y))
  (error "MEDIUM-COPY-AREA is not implemented for basic MEDIUMs"))


;;; Medium-specific Drawing Functions

(defmethod medium-draw-point* :around ((medium transform-coordinates-mixin) x y)
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x y)
      (call-next-method medium x y))))

(defmethod medium-draw-points* :around ((medium transform-coordinates-mixin) coord-seq)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
      (call-next-method medium coord-seq))))

(defmethod medium-draw-line* :around ((medium transform-coordinates-mixin) x1 y1 x2 y2)
  (let ((tr (medium-transformation medium)))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (call-next-method medium x1 y1 x2 y2)))))

(defmethod medium-draw-lines* :around ((medium transform-coordinates-mixin) coord-seq)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
      (call-next-method medium coord-seq))))

(defmethod medium-draw-polygon* :around ((medium transform-coordinates-mixin) coord-seq closed filled)
  (let ((tr (medium-transformation medium)))
    (with-transformed-positions (tr coord-seq)
      (call-next-method medium coord-seq closed filled))))

(defun expand-rectangle-coords (left top right bottom)
  "Expand the two corners of a rectangle into a polygon coord-seq"
  (vector left top right top right bottom left bottom))

(defmethod medium-draw-rectangle* :around ((medium transform-coordinates-mixin) left top right bottom filled)
  (let ((tr (medium-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (multiple-value-bind (left top right bottom)
            (transform-rectangle* tr left top right bottom)
          (call-next-method medium left top right bottom filled))
        (medium-draw-polygon* medium (expand-rectangle-coords left top right bottom)
                              t filled))) )

(defgeneric medium-draw-rectangles* (medium coord-seq filled))

(defmethod medium-draw-rectangles* :around ((medium transform-coordinates-mixin) position-seq filled)
  (let ((tr (medium-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (call-next-method medium (transform-positions tr position-seq) filled)
        (do-sequence ((left top right bottom) position-seq)
          (medium-draw-polygon* medium (vector left top
                                               left bottom
                                               right bottom
                                               right top)
                                t filled)))))

(defmethod medium-draw-ellipse* :around (medium center-x center-y
                                         radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                         start-angle end-angle filled)
  (when (<= (abs (- (mod start-angle (* 2 pi)) (mod end-angle (* 2 pi)))) short-float-epsilon)
    (setf start-angle 0
          end-angle (* 2 pi)))
  (call-next-method))

(defmethod medium-draw-ellipse* :around ((medium transform-coordinates-mixin) center-x center-y
                                         radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                         start-angle end-angle filled)
  (let* ((ellipse (make-elliptical-arc* center-x center-y
                                        radius-1-dx radius-1-dy
                                        radius-2-dx radius-2-dy
                                        :start-angle start-angle
                                        :end-angle end-angle))
         (transformed-ellipse (transform-region (medium-transformation medium)
                                                ellipse))
         (start-angle (ellipse-start-angle transformed-ellipse))
         (end-angle (ellipse-end-angle transformed-ellipse)))
    (multiple-value-bind (center-x center-y) (ellipse-center-point* transformed-ellipse)
      (multiple-value-bind (radius-1-dx radius-1-dy radius-2-dx radius-2-dy)
          (ellipse-radii transformed-ellipse)
        (call-next-method medium center-x center-y
                          radius-1-dx radius-1-dy
                          radius-2-dx radius-2-dy
                          start-angle end-angle filled)))))

(defmethod medium-draw-circle* :around (medium center-x center-y
                                        radius start-angle end-angle filled)
  (when (<= (abs (- (mod start-angle (* 2 pi)) (mod end-angle (* 2 pi)))) short-float-epsilon)
    (setf start-angle 0 end-angle (* 2 pi)))
  (call-next-method))

(defmethod medium-draw-circle* :around ((medium transform-coordinates-mixin) center-x center-y
                                        radius start-angle end-angle filled)
  (let* ((ellipse (make-elliptical-arc* center-x center-y
                                        radius 0
                                        0 radius
                                        :start-angle start-angle
                                        :end-angle end-angle))
         (transformed-ellipse (transform-region (medium-transformation medium)
                                                ellipse))
         (start-angle (ellipse-start-angle transformed-ellipse))
         (end-angle (ellipse-end-angle transformed-ellipse)))
    (multiple-value-bind (center-x center-y) (ellipse-center-point* transformed-ellipse)
      (call-next-method medium center-x center-y radius start-angle end-angle filled))))

(defmethod medium-copy-area :around ((from-drawable transform-coordinates-mixin)
                                     from-x from-y width height
                                     to-drawable to-x to-y)
  (with-transformed-position ((medium-transformation from-drawable)
                              from-x from-y)
    (call-next-method from-drawable from-x from-y width height
                      to-drawable to-x to-y)))

(defmethod medium-copy-area :around (from-drawable from-x from-y width height
                                     (to-drawable  transform-coordinates-mixin)
                                     to-x to-y)
  (with-transformed-position ((medium-transformation to-drawable)
                              to-x to-y)
    (call-next-method from-drawable from-x from-y width height
                      to-drawable to-x to-y)))

;;; Fall-through Methods For Multiple Objects Drawing Functions

(defmethod medium-draw-points* ((medium transform-coordinates-mixin) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (do-sequence ((x y) coord-seq)
        (medium-draw-point* medium x y)))))

(defmethod medium-draw-lines* ((medium transform-coordinates-mixin) position-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr position-seq)
      (do-sequence ((x1 y1 x2 y2) position-seq)
        (medium-draw-line* medium x1 y1 x2 y2)))))

(defmethod medium-draw-rectangles* ((medium transform-coordinates-mixin) coord-seq filled)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (with-transformed-positions (tr coord-seq)
      (do-sequence ((x1 y1 x2 y2) coord-seq)
        (medium-draw-rectangle* medium x1 y1 x2 y2 filled)))))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium basic-medium))
  nil)

(defmethod medium-force-output ((medium basic-medium))
  nil)

(defmethod medium-clear-area ((medium basic-medium) left top right bottom)
  (draw-rectangle* medium left top right bottom :ink +background-ink+))

(defmethod medium-beep ((medium basic-medium))
  nil)

;;;;;;;;;

(defmethod engraft-medium ((medium basic-medium) port sheet)
  (declare (ignore port))
  (setf (%medium-sheet medium) sheet))

(defmethod degraft-medium ((medium basic-medium) port sheet)
  (declare (ignore port sheet))
  (setf (%medium-sheet medium) nil))

(defmethod allocate-medium ((port port) sheet)
  (make-medium port sheet))

(defmethod deallocate-medium ((port port) medium)
  (declare (ignorable port medium))
  nil)

(defmethod port ((medium basic-medium))
  (and (medium-sheet medium)
       (port (medium-sheet medium))))

(defmethod graft ((medium basic-medium))
  (and (medium-sheet medium)
       (graft (medium-sheet medium))))
