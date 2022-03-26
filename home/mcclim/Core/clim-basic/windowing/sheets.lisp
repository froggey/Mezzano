;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com),
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The sheet protocol

(in-package :clim-internals)

(defgeneric raise-sheet-internal (sheet parent))
(defgeneric bury-sheet-internal (sheet parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input protocol

(defgeneric dispatch-event (client event))
(defgeneric queue-event (client event))
(defgeneric schedule-event (client event delay))
(defgeneric handle-event (client event))
(defgeneric event-read (client))
(defgeneric event-read-no-hang (client))
(defgeneric event-peek (client &optional event-type))
(defgeneric event-unread (client event))
(defgeneric event-listen (client))

;;; These DEFGENERIC forms are commented out because they appear
;;; in decls.lisp.
;(defgeneric sheet-direct-mirror (sheet))
;(defgeneric sheet-mirrored-ancestor (sheet))
;(defgeneric sheet-mirror (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; repaint protocol

(defgeneric dispatch-repaint (sheet region))
;(defgeneric queue-repaint (sheet region))
;(defgeneric handle-repaint (sheet region))
;(defgeneric repaint-sheet (sheet region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

(defgeneric note-sheet-grafted (sheet))
(defgeneric note-sheet-degrafted (sheet))
(defgeneric note-sheet-adopted (sheet))
(defgeneric note-sheet-disowned (sheet))
(defgeneric note-sheet-enabled (sheet))
(defgeneric note-sheet-disabled (sheet))
(defgeneric note-sheet-region-changed (sheet))
(defgeneric note-sheet-transformation-changed (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; non standard protocol

(defgeneric %invalidate-cached-device-transformations (sheet))
(defgeneric %invalidate-cached-device-regions (sheet)
  (:method (sheet) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; sheet protocol class

(defclass basic-sheet (sheet)
  ((region :type region
	   :initarg :region
	   :initform (make-bounding-rectangle 0 0 100 100)
	   :accessor sheet-region
           :writer %%set-sheet-region)
   (native-transformation :type (or null transformation)
			  :initform nil
                          :writer %%set-sheet-native-transformation
                          :reader %%sheet-native-transformation)
   (native-region :type (or null region)
                  :initarg :native-region
		  :initform nil)
   (device-transformation :type (or null transformation)
                          :initform nil)
   (device-region :type (or null region)
		  :initform nil)
   (pointer-cursor :accessor sheet-pointer-cursor
                   :initarg  :pointer-cursor
                   :initform :default)
   (enabled-p :type boolean
	      :initarg :enabled-p
              :initform t
              :accessor sheet-enabled-p)))

;;; Native region is volatile, and is only computed at the first
;;; request when it's equal to nil.
;;;

(defmethod sheet-parent ((sheet basic-sheet))
  nil)

(defmethod sheet-children ((sheet basic-sheet))
  nil)

;;; This method is a canary which signals, that something is wrong
;;; with the inheritance in the sheet class, i.e that basic-sheet is
;;; before the sheet-multiple-child-mixin in the cpl. -- jd 2020-01-20
(defmethod sheet-adopt-child ((sheet basic-sheet) (child sheet))
  (error "~S attempting to adopt ~S." sheet child))

(defmethod sheet-adopt-child :after ((sheet basic-sheet) (child sheet))
  (note-sheet-adopted child)
  (when (sheet-grafted-p sheet)
    (note-sheet-grafted child)))

(define-condition sheet-is-not-child (error) ())

(defmethod sheet-disown-child :before
    ((sheet basic-sheet) (child sheet) &key (errorp t))
  (when (and (not (member child (sheet-children sheet))) errorp)
    (error 'sheet-is-not-child)))

(defmethod sheet-disown-child :after
    ((sheet basic-sheet) (child sheet) &key (errorp t))
  (declare (ignore errorp))
  (note-sheet-disowned child)
  (when (sheet-grafted-p sheet)
    (note-sheet-degrafted child)))

(defmethod sheet-siblings ((sheet basic-sheet))
  (when (not (sheet-parent sheet))
    (error 'sheet-is-not-child))
  (remove sheet (sheet-children (sheet-parent sheet))))

(defmethod sheet-enabled-children ((sheet basic-sheet))
  (delete-if-not #'sheet-enabled-p (copy-list (sheet-children sheet))))

(defmethod sheet-ancestor-p ((sheet basic-sheet)
			     (putative-ancestor sheet))
  (or (eq sheet putative-ancestor)
      (and (sheet-parent sheet)
	   (sheet-ancestor-p (sheet-parent sheet) putative-ancestor))))

(defmethod raise-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(defmethod bury-sheet ((sheet basic-sheet))
  (error 'sheet-is-not-child))

(define-condition sheet-ordering-underspecified (error) ())

(defmethod reorder-sheets ((sheet basic-sheet) new-ordering)
  (when (set-difference (sheet-children sheet) new-ordering)
    (error 'sheet-ordering-underspecified))
  (when (set-difference new-ordering (sheet-children sheet))
    (error 'sheet-is-not-child))
  (setf (sheet-children sheet) new-ordering)
  sheet)

(defmethod sheet-viewable-p ((sheet basic-sheet))
  (and (sheet-parent sheet)
       (sheet-enabled-p sheet)
       (sheet-viewable-p (sheet-parent sheet))))

(defmethod sheet-occluding-sheets ((sheet basic-sheet) (child sheet))
  (labels ((fun (l)
		(cond ((eq (car l) child) '())
		      ((and (sheet-enabled-p (car l))
                            (region-intersects-region-p
                             (sheet-region (car l)) (sheet-region child)))
		       (cons (car l) (fun (cdr l))))
		      (t (fun (cdr l))))))
    (fun (sheet-children sheet))))

(defmethod map-over-sheets (function (sheet basic-sheet))
  (funcall function sheet)
  (map nil
       #'(lambda (child)
           (map-over-sheets function child))
       (sheet-children sheet)))

;;; Instead of defining yet another function we specialize on
;;; sequence. Thanks to that we can map over "all-but-parent" sheets
;;; with `(map-over-sheets function (sheet-children sheet))'.
(defmethod map-over-sheets (function (sheets list))
  (map nil
       #'(lambda (child)
           (map-over-sheets function child))
       sheets))

(defmethod (setf sheet-enabled-p) :around (enabled-p (sheet basic-sheet))
  (unless (eql enabled-p (sheet-enabled-p sheet))
    (call-next-method)
    (if enabled-p
        (note-sheet-enabled sheet)
        (note-sheet-disabled sheet))
    (dispatch-repaint (sheet-parent sheet)
                      (transform-region (sheet-transformation sheet)
                                        (sheet-region sheet)))))

(defmethod sheet-transformation ((sheet basic-sheet))
  (error "Attempting to get the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod (setf sheet-transformation) (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (error "Attempting to set the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod move-sheet ((sheet basic-sheet) x y)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (let ((dx (- x old-x))
            (dy (- y old-y)))
        (unless (and (zerop dx) (zerop dy))
          (setf (sheet-transformation sheet)
                (compose-translation-with-transformation
                 transform (- x old-x) (- y old-y))))))))

(defmethod resize-sheet ((sheet basic-sheet) width height)
  (setf (sheet-region sheet)
        (make-bounding-rectangle 0 0 width height)))

(defmethod move-and-resize-sheet ((sheet basic-sheet) x y width height)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (%set-sheet-region-and-transformation
       sheet
       (make-bounding-rectangle 0 0 width height)
       (compose-translation-with-transformation transform (- x old-x) (- y old-y))))))

(defmethod map-sheet-position-to-parent ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-position-to-child ((sheet basic-sheet) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-parent ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-child ((sheet basic-sheet) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-over-sheets-containing-position (function (sheet basic-sheet) x y)
  (map () #'(lambda (child)
              (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
                (when (region-contains-position-p (sheet-region child) tx ty)
                  (funcall function child))))
       (sheet-children sheet)))

(defmethod map-over-sheets-overlapping-region (function (sheet basic-sheet) region)
  (map () #'(lambda (child)
              (when (region-intersects-region-p
                     region
                     (transform-region
                      (if (eq child sheet)
                          +identity-transformation+
                          (sheet-transformation child))
                      (sheet-region child)))
                (funcall function child)))
       (sheet-children sheet)))

(defmethod child-containing-position ((sheet basic-sheet) x y)
  (loop for child in (sheet-children sheet)
	do (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
	     (when (and (sheet-enabled-p child)
			(region-contains-position-p (sheet-region child) tx ty))
	       (return child)))))

(defmethod children-overlapping-region ((sheet basic-sheet) (region region))
  (loop for child in (sheet-children sheet)
	if (and (sheet-enabled-p child)
		(region-intersects-region-p
		 region
		 (transform-region (sheet-transformation child)
				   (sheet-region child))))
	  collect child))

(defmethod children-overlapping-rectangle* ((sheet basic-sheet) x1 y1 x2 y2)
  (children-overlapping-region sheet (make-rectangle* x1 y1 x2 y2)))

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor (eql nil)))
  (cond ((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t +identity-transformation+)))

(define-condition sheet-is-not-ancestor (error) ())

(defmethod sheet-delta-transformation ((sheet basic-sheet) (ancestor sheet))
  (cond ((eq sheet ancestor) +identity-transformation+)
	((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t (error 'sheet-is-not-ancestor))))

(defmethod sheet-allocated-region ((sheet basic-sheet) (child sheet))
  (reduce #'region-difference
	  (mapcar #'(lambda (child)
                      (transform-region (sheet-transformation child)
                                        (sheet-region child)))
                  (cons child (sheet-occluding-sheets sheet child)))))

(defmethod sheet-direct-mirror ((sheet basic-sheet))
  nil)

(defmethod sheet-mirrored-ancestor ((sheet basic-sheet))
  (let ((parent (sheet-parent sheet)))
    (if (null parent)
	nil
	(sheet-mirrored-ancestor parent))))

(defmethod sheet-mirror ((sheet basic-sheet))
  (let ((mirrored-ancestor (sheet-mirrored-ancestor sheet)))
    (if (null mirrored-ancestor)
	nil
	(sheet-direct-mirror mirrored-ancestor))))

(defmethod graft ((sheet basic-sheet))
  nil)

(defmethod note-sheet-grafted ((sheet basic-sheet))
  (mapc #'note-sheet-grafted (sheet-children sheet)))

(defmethod note-sheet-degrafted ((sheet basic-sheet))
  (mapc #'note-sheet-degrafted (sheet-children sheet)))

(defmethod note-sheet-adopted ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disowned ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-enabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disabled ((sheet basic-sheet))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-region-changed ((sheet basic-sheet))
  nil)

(defmethod note-sheet-transformation-changed ((sheet basic-sheet))
  nil)

(defmethod sheet-native-transformation ((sheet basic-sheet))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
	    (if-let ((parent (sheet-parent sheet)))
	      (compose-transformations
               (sheet-native-transformation parent)
               (sheet-transformation sheet))
              +identity-transformation+)))
    native-transformation))

(defmethod sheet-native-region ((sheet basic-sheet))
  (with-slots (native-region) sheet
    (unless native-region
      (let ((this-native-region (transform-region
				 (sheet-native-transformation sheet)
				 (sheet-region sheet)))
	    (parent (sheet-parent sheet)))
	(setf native-region
	      (if (null parent)
		  this-native-region
		  (region-intersection this-native-region
				       (sheet-native-region parent))))))
    native-region))

(defmethod sheet-device-transformation ((sheet basic-sheet))
  (with-slots (device-transformation) sheet
    (unless device-transformation
      (setf device-transformation
            (let ((medium (sheet-medium sheet)))
              (compose-transformations
               (sheet-native-transformation sheet)
               (if medium
                   (medium-transformation medium)
                   +identity-transformation+)))))
    device-transformation))

(defmethod sheet-device-region ((sheet basic-sheet))
  (with-slots (device-region) sheet
    (unless device-region
      (setf device-region
            (if-let ((medium (sheet-medium sheet)))
              (region-intersection
               (sheet-native-region sheet)
               (transform-region (sheet-device-transformation sheet)
                                 (medium-clipping-region medium)))
              (sheet-native-region sheet))))
    device-region))

(defmethod invalidate-cached-transformations ((sheet basic-sheet))
  (with-slots (native-transformation device-transformation) sheet
    (setf native-transformation nil
          device-transformation nil))
  (mapc #'invalidate-cached-transformations (sheet-children sheet)))

(defmethod invalidate-cached-regions ((sheet basic-sheet))
  (with-slots (native-region device-region) sheet
    (setf native-region nil
          device-region nil))
  (mapc #'invalidate-cached-regions (sheet-children sheet)))

(defmethod %invalidate-cached-device-transformations ((sheet basic-sheet))
  (with-slots (device-transformation) sheet
    (setf device-transformation nil))
  (mapc #'%invalidate-cached-device-transformations (sheet-children sheet)))

(defmethod %invalidate-cached-device-regions ((sheet basic-sheet))
  (with-slots (device-region) sheet
    (setf device-region nil))
  (mapc #'%invalidate-cached-device-regions (sheet-children sheet)))

(defmethod (setf sheet-transformation) :after (transformation (sheet basic-sheet))
  (declare (ignore transformation))
  (invalidate-cached-transformations sheet)
  (invalidate-cached-regions sheet)
  (map-over-sheets #'(lambda (sheet)
                       (when (sheet-direct-mirror sheet)
                         (update-mirror-geometry sheet)))
                   sheet)
  (note-sheet-transformation-changed sheet))

(defmethod (setf sheet-region) :after (region (sheet basic-sheet))
  (declare (ignore region))
  (invalidate-cached-regions sheet)
  (map-over-sheets #'(lambda (sheet)
                       (when (sheet-direct-mirror sheet)
                         (update-mirror-geometry sheet)))
                   sheet)
  (note-sheet-region-changed sheet))

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet basic-sheet))
  (unless (sheet-direct-mirror sheet)
    (let ((msheet (sheet-mirrored-ancestor sheet)))
      (set-sheet-pointer-cursor (port msheet) msheet (sheet-pointer-cursor msheet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet parent mixin

(defclass sheet-parent-mixin ()
  ((parent :initform nil :accessor sheet-parent)))

(define-condition sheet-already-has-parent (error) ())
(define-condition sheet-is-ancestor (error) ())

(defmethod sheet-adopt-child :before (sheet (child sheet-parent-mixin))
  (when (and (sheet-parent child) (not (eq sheet (sheet-parent child))))
    (error 'sheet-already-has-parent))
  (when (sheet-ancestor-p sheet child)
    (error 'sheet-is-ancestor)))

(defmethod sheet-adopt-child :after (sheet (child sheet-parent-mixin))
  (setf (sheet-parent child) sheet))

(defmethod sheet-disown-child :after (sheet
				      (child sheet-parent-mixin)
				      &key (errorp t))
  (declare (ignore sheet errorp))
  (setf (sheet-parent child) nil))

(defmethod raise-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (raise-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (raise-mirror (port sheet) sheet)))

(defmethod bury-sheet ((sheet sheet-parent-mixin))
  (when (sheet-parent sheet)
    (bury-sheet-internal sheet (sheet-parent sheet)))
  (when (sheet-direct-mirror sheet)
    (bury-mirror (port sheet) sheet)))

(defmethod graft ((sheet sheet-parent-mixin))
  (and (sheet-parent sheet) (graft (sheet-parent sheet))))

(defmethod map-sheet-position-to-parent ((sheet sheet-parent-mixin) x y)
  (transform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-position-to-child ((sheet sheet-parent-mixin) x y)
  (untransform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-rectangle*-to-parent
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (transform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

(defmethod map-sheet-rectangle*-to-child
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (untransform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet leaf mixin

(defclass sheet-leaf-mixin () ())

(defmethod sheet-children ((sheet sheet-leaf-mixin))
  nil)

(defmethod sheet-adopt-child ((sheet sheet-leaf-mixin) (child sheet))
  (error "Leaf sheet attempting to adopt a child"))

(defmethod sheet-disown-child
    ((sheet sheet-leaf-mixin) (child sheet) &key (errorp t))
  (declare (ignorable errorp))
  (error "Leaf sheet attempting to disown a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet single child mixin

(defclass sheet-single-child-mixin ()
  ((child :initform nil :accessor sheet-child)))

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (and (sheet-child sheet) (list (sheet-child sheet))))

(define-condition sheet-supports-only-one-child (error)
  ((sheet :initarg :sheet)))

(defmethod print-object ((object sheet-supports-only-one-child) stream)
  (format stream "~A~%single-child-composite-pane is allowed to have only one child."
          (slot-value object 'sheet)))

(defmethod sheet-adopt-child :before ((sheet sheet-single-child-mixin)
				      (child sheet-parent-mixin))
  (when (sheet-child sheet)
    (error 'sheet-supports-only-one-child :sheet sheet)))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin)
			      (child sheet-parent-mixin))
  (setf (sheet-child sheet) child))

(defmethod sheet-disown-child ((sheet sheet-single-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-child sheet) nil))

(defmethod raise-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))

(defmethod bury-sheet-internal (sheet (parent sheet-single-child-mixin))
  (declare (ignorable sheet parent))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet multiple child mixin

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil :accessor sheet-children)))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin)
			      (child sheet-parent-mixin))
  (push child (sheet-children sheet)))

(defmethod sheet-disown-child ((sheet sheet-multiple-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-children sheet) (delete child (sheet-children sheet))))

(defmethod raise-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
	(cons sheet (delete sheet (sheet-children parent)))))

(defmethod bury-sheet-internal (sheet (parent sheet-multiple-child-mixin))
  (setf (sheet-children parent)
	(append (delete sheet (sheet-children parent)) (list  sheet))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sheet geometry classes

(defclass sheet-identity-transformation-mixin ()
  ())

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defclass sheet-transformation-mixin ()
  ((transformation :initform +identity-transformation+
                   :initarg :transformation
                   :accessor sheet-transformation)))

(defclass sheet-translation-mixin (sheet-transformation-mixin)
  ())

(defmethod (setf sheet-transformation) :before
    ((transformation transformation) (sheet sheet-translation-mixin))
  (unless (translation-transformation-p transformation)
    (error "Attempting to set the SHEET-TRANSFORMATION of a ~
            SHEET-TRANSLATION-TRANSFORMATION-MIXIN to a non ~
            translation transformation")))

(defclass sheet-y-inverting-transformation-mixin (sheet-transformation-mixin)
  ()
  (:default-initargs :transformation (make-transformation 1 0 0 -1 0 0)))

(defmethod (setf sheet-transformation) :before
    ((transformation transformation)
     (sheet sheet-y-inverting-transformation-mixin))
  (unless (y-inverting-transformation-p transformation)
    (error "Attempting to set the SHEET-TRANSFORMATION of a ~
            SHEET-Y-INVERTING-TRANSFORMATION-MIXIN to a non Y ~
            inverting transformation")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirrored sheet

;;; We assume the following limitations of the host window systems:
;;;
;;;  mirror transformations:
;;;   . can only be translations
;;;   . are limited to 16-bit signed integer deltas
;;;
;;;  mirror regions:
;;;   . can only be axis-aligend rectangles
;;;   . min-x = min-y = 0
;;;   . max-x, max-y < 2^16
;;;
;;; These are the limitations of the X Window System.

(defclass mirrored-sheet-mixin ()
  ((port :initform nil :initarg :port :accessor port)
   (native-transformation :initform +identity-transformation+)
   (mirror-transformation
    :documentation "Our idea of the current mirror transformation. Might not be
correct if a foreign application changes our mirror's geometry."
    :initform +identity-transformation+
    :accessor %sheet-mirror-transformation)
   (mirror-region
    :documentation "Our idea of the current mirror region. Might not be correct
if a foreign application changes our mirror's geometry. Also note that this
might be different from the sheet's native region."
    :initform nil
    :accessor %sheet-mirror-region)))

(defmethod sheet-direct-mirror ((sheet mirrored-sheet-mixin))
  (port-lookup-mirror (port sheet) sheet))

(defmethod sheet-mirrored-ancestor ((sheet mirrored-sheet-mixin))
  sheet)

(defmethod sheet-mirror ((sheet mirrored-sheet-mixin))
  (sheet-direct-mirror sheet))

(defmethod note-sheet-grafted :before ((sheet mirrored-sheet-mixin))
  (unless (port sheet)
    (error "~S called on sheet ~S, which has no port?!" 'note-sheet-grafted sheet))
  (realize-mirror (port sheet) sheet))

(defmethod note-sheet-degrafted :after ((sheet mirrored-sheet-mixin))
  (destroy-mirror (port sheet) sheet))

(defmethod (setf sheet-enabled-p) :after
    (new-value (sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    ;; We do this only if the sheet actually has a mirror.
    (if new-value
        (port-enable-sheet (port sheet) sheet)
        (port-disable-sheet (port sheet) sheet))))

(defmethod (setf sheet-pretty-name) :after (new-name (sheet mirrored-sheet-mixin))
  ;; SHEET might not yet have a mirror if this is called e.g. during
  ;; the pane generation phase of an application frame.
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (climb:port-set-mirror-name (port sheet) mirror new-name)))

(defmethod invalidate-cached-transformations ((sheet mirrored-sheet-mixin))
  (with-slots (native-transformation device-transformation) sheet
    (setf ;;native-transformation nil
     device-transformation nil))
  (loop for child in (sheet-children sheet)
     do (invalidate-cached-transformations child)))

;;; Coordinate swizzling
(defmethod sheet-native-region ((sheet mirrored-sheet-mixin))
  (with-slots (native-region) sheet
    (unless native-region
      (let ((this-region (transform-region (sheet-native-transformation sheet)
                                           (sheet-region sheet)))
            (parent (sheet-parent sheet)))
        (setf native-region
              (if parent
                  (region-intersection this-region
                                       (transform-region
                                        (invert-transformation
                                         (%sheet-mirror-transformation sheet))
                                        (sheet-native-region parent)))
                  this-region))))
    native-region))

#+ (or) ;; XXX: is this needed?
(defmethod sheet-native-transformation ((sheet mirrored-sheet-mixin))
  ;; XXX hm...
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
            (compose-transformations
             (invert-transformation
              (%sheet-mirror-transformation sheet))
             (compose-transformations
              (sheet-native-transformation (sheet-parent sheet))
              (sheet-transformation sheet)))))
    native-transformation))

;;; Top-level sheets

(defclass top-level-sheet-mixin ()
  (;; The NAME slot intentionally uses the same slot name as the NAME
   ;; in the PANE class so that both collapse into a single effective
   ;; slot in e.g. the TOP-LEVEL-SHEET-PANE class.
   (name :initarg :name :reader sheet-name)
   (%pretty-name :initarg :pretty-name :accessor clime:sheet-pretty-name)))

;;; Unmanaged sheet is not managed by the window manager.
(defclass unmanaged-sheet-mixin () ())


;;; Sheets as bounding rectangles

;;; Somewhat hidden in the spec, we read (section 4.1.1 "The Bounding
;;; Rectangle Protocol")
;;;

;;; | bounding-rectangle* region [Generic Function]
;;; |
;;; |      [...] The argument region must be either a bounded region [...] or
;;; |      some other object that obeys the bounding rectangle protocol, such
;;; |      as a sheet or an output record. [...]

(defmethod bounding-rectangle* ((sheet sheet))
  (bounding-rectangle* (sheet-region sheet)))

;;; The null sheet

(defclass null-sheet (basic-sheet) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dangerous codes
;;; postfix: %%%
;;;

;; used by invoke-with-double-buffering
(defmacro with-temp-mirror%%% ((mirrored-sheet new-mirror new-native-transformation new-region)
                               &body body)
  (alexandria:once-only (mirrored-sheet new-mirror new-native-transformation new-region)
    (alexandria:with-gensyms (port old-native-transformation old-region set-native)
      `(let ((,port (port sheet))
             (,old-native-transformation (sheet-native-transformation ,mirrored-sheet))
             (,old-region (sheet-region ,mirrored-sheet)))
         (flet ((,set-native (transform region sheet)
                  (invalidate-cached-regions sheet)
                  (invalidate-cached-transformations sheet)
                  (%%set-sheet-native-transformation transform sheet)
                  (setf (slot-value sheet 'region) region))
                ((setf sheet-direct-mirror) (new-mirror sheet)
                  (port-register-mirror ,port sheet new-mirror)))
           (letf (((sheet-parent ,mirrored-sheet) nil)
                  ((sheet-direct-mirror ,mirrored-sheet) ,new-mirror))
             (unwind-protect
                  (progn
                    (,set-native ,new-native-transformation ,new-region ,mirrored-sheet)
                    ,@body)
               (,set-native ,old-native-transformation ,old-region ,mirrored-sheet)
               (port-unregister-mirror ,port ,mirrored-sheet ,new-mirror))))))))
