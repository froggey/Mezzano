(in-package #:clim-internals)


;;;; Coordinate Swizzling

;;; This implements what I call "coordinate swizzling", the illusion that
;;; sheets can be arbitrary large. The key idea here is that there is a
;;; certain kind freedom in choosing the native transformation. A little
;;; diagram to illustrate the involved transformations:

;;;
;;;                  NT                           NT = native transformation
;;;    sheet  ---------------->  mirror          PNT = parent's NT
;;;      |                         |              MT = mirror transformation
;;;      |                         |               T = sheet transformation
;;;      |                         |
;;;    T |                         | MT
;;;      |                         |
;;;      |                         |
;;;      |                         |
;;;      v          PNT            v
;;;    parent ----------------> parent
;;;                             mirror
;;;

;;; To setup both the mirror transformation (MT) and the mirror region (MR),
;;; we start with the mirror region. The window systems limitations are here:
;;; We can only have a certain size and its upper-left corner must be at the
;;; origin.

;;; Now the parent already has a mirror region (PMR) assigned, which obeys to
;;; the very same size restrictions. Since every part of MR outside of (PMR o
;;; MT^-1) is not visible, the first idea is to just clip it by the visible
;;; part:

;;;  MR_1 = intersection (SR o NT, PMR o MT^-1)             [mirror space]

;;; Since both NT and MT^-1 are not yet known let us reformulate that region
;;; in the parent mirror space:

;;;  MR_2 = MR_1 o MT                                       [parent mirror space]
;;;       = intersection (SR o NT, PMR o MT^-1) o MT
;;;       = intersection (SR o NT o MT, PMR o MT^-1 o MT)
;;;       = intersection (SR o (T o PNT o MT^-1) o MT, PMR)
;;;       = intersection (SR o T o PNT, PMR)

;;; MR_2 now is a good candidate for a mirror region. Unfortunately it is
;;; still in parent mirror space, so we transform it back, yielding MR_3:

;;;  MR_3 = MR_2 o MT^-1
;;;       = intersection (SR o T o PNT, PMR) o MT^-1

;;; Here the only unknown is the mirror transformation MT, we can still
;;; choose any as long as the window system limitations are met for both MR
;;; and MT.

;;; 1. MT should be a translation, whose delta x and y components are within
;;;    limits.

;;; 2. The size limitation of MR is already met, since MR_3's size is no
;;;    larger than PMR's size (which mets the limitations). [Remember that MT
;;;    was defined to be some translation].

;;; 3. MR_3's upper left corner should also be at the origin which nicely
;;;    defines MT^-1: Just choose this upper left corner coordinates as MT's x
;;;    and y deltas.

;;; So we can meet all criteria. The NT can easily be set up by the identity:

;;;    NT = T o PNT o MT^-1

;;; Notes

;;; . when the native transformation changes, we need to:

;;;  a. Redraw the mirror's contents since the mapping from the sheet space
;;;     to the mirror space (that is the native transformation) just changed.
;;;     Translational changes in the native transformation can be catered by
;;;     blittering, but then have a nice synchronization problem: Suppose
;;;     a repaint event is underway as we blitter from some region R_1 to
;;;     region R_2. Say the repaint event's region intersects with R_1. In
;;;     this case we just blittered pixels which were considered dirty into
;;;     R_2. Redrawing R_1 now does not repair the defect, since R_2 now also
;;;     contains dirty pixels. => oops, redraw error.
;;
;;;  b. Since the above above calculation took the parent's native
;;;     transformation into account, (and even the naively wanted mirror
;;;     region depends on the parent's native transformation), we need to
;;;     redo mirror geometry calculation for any child.
;;
;;;  c. I imagine more aggressive output records which remember the actual
;;;     octets which need to be send to the X server. These would contain
;;;     mirror coordinates and will need to be recalculated, when the native
;;;     transformation changes.

;;; => Changing the native transformation can be expensive, so we want a way
;;;    to minimize changes to the native transformation.

;;; What did we do? We clipped the wanted mirror region, SR o NT, inside the
;;; parent's mirror region to meet the window system limitations. We can make
;;; this clip region larger as long as we still come up with an mirror region,
;;; which meets the limits.


;;; Mirror geometry functions

(defparameter *configuration-event-p* nil
  "Flag used to inhibit setting mirror region and transformation to prevent
infinite recursion on (setf sheet-*).")

(defun %set-mirror-geometry (sheet &key
                                     (MT (make-translation-transformation -5 -5))
                                     (MR (make-rectangle* 0 0 1 1))
                                     (invalidate-transformations nil))
  (setf (%sheet-mirror-region sheet) MR)
  (setf (%sheet-mirror-transformation sheet) MT)
  (when (and (sheet-direct-mirror sheet)
             (not (eql *configuration-event-p* sheet)))
    (let ((port (port sheet))
          (mirror (sheet-direct-mirror sheet)))
      (port-set-mirror-region port mirror MR)
      ;; TOP-LEVEL-SHEET-PANE is our window (and it is managed by the window
      ;; manager - decorations and such. We can't pinpoint exact translation. On
      ;; the other hand UNMANAGED-TOP-LEVEL-SHEET-PANE is essential for menus
      ;; and has exact position set (thanks to not being managed by WM).
      (unless (and (typep sheet 'top-level-sheet-mixin)
                   (null (typep sheet 'unmanaged-sheet-mixin)))
        (port-set-mirror-transformation port mirror MT)))
    (when invalidate-transformations
      (with-slots (native-transformation device-transformation) sheet
        (setf native-transformation nil
              device-transformation nil)))))

;;; Reflecting a Sheet's Geometry to the Mirror.
(defun %sheet-mirror-region* (sheet)
  ;; For grafts or top-level-sheet's always read the mirror region
  ;; from the server, since it is not under our control.
  (if (or (null (sheet-parent sheet))
          (null (sheet-parent (sheet-parent sheet)))
          (null (%sheet-mirror-region sheet)))
      (make-rectangle* 0 0 #x10000 #x10000)
      (%sheet-mirror-region sheet)))

(defun %effective-mirror-region (sheet)
  ;; XXX is this really needed, can't we deduce this information more easily?
  (let* ((parent (sheet-parent sheet))
         (ancestor (and parent (sheet-mirrored-ancestor parent))))
    (if ancestor
        (region-intersection (%sheet-mirror-region* sheet)
                             (untransform-region (%sheet-mirror-transformation sheet)
                                                 (%effective-mirror-region ancestor)))
        (%sheet-mirror-region* sheet))))

(defun update-mirror-geometry
    (sheet &aux (old-native-transformation (%%sheet-native-transformation sheet)))
  "This function reflects the current sheet region and sheet transformation
to the mirror. It also sets up the native transformation. This function is
supposed to be called whenever one of the following happens:

  - the sheet's transformation changed
  - the sheet's region changed
  - the parent's native transformation changed
  - the parent's transformation changed
  - the parent's mirror region changed

Also if the sheet's native transformation changes, the mirror's contents need
to be redrawn, which is achieved by calling PORT-DIRTY-MIRROR-REGION. TODO is this true?

Since changing the sheet's native transformation might thus be expensive,
this function tries to minimize changes to it. (although it does not try
very hard)."
  ;; We can't manipulate grafts (and rasters)
  (when (or (null (sheet-parent sheet)))
    (return-from update-mirror-geometry nil))
  ;; The native transformation has to changed or needs to be computed initially.
  (let* ((parent (sheet-parent sheet))
         (sheet-region-in-native-parent
           ;; this now is the wanted sheet mirror region
          (if (graftp parent)
              ;; For the TOP-LEVEL-SHEET-PANE (when the parent is a GRAFT) we don't
              ;; clip the sheet-region with the parent-region. -- admich 2019-05-30
              (transform-region (sheet-native-transformation parent)
                                (transform-region (sheet-transformation sheet)
                                                  (sheet-region sheet)))
              (transform-region (sheet-native-transformation parent)
                             (region-intersection (sheet-region parent)
                                                  (transform-region (sheet-transformation sheet)
                                                                    (sheet-region sheet)))))))
    (when (region-equal sheet-region-in-native-parent +nowhere+)
      (%set-mirror-geometry sheet :invalidate-transformations t)
      (return-from update-mirror-geometry))
    ;; mx1 .. my2 are is now the wanted mirror region in the
    ;; parent coordinate system.
    (with-bounding-rectangle* (mx1 my1 mx2 my2) sheet-region-in-native-parent
      (let* ((parent-mirror-region (%sheet-mirror-region* (sheet-mirrored-ancestor parent)))
             ;; pw, ph is the width/height of the mirror containing our sheet
             (pw (bounding-rectangle-width parent-mirror-region))
             (ph (bounding-rectangle-height parent-mirror-region)))
        (labels ((choose (MT)
                   ;; -> fits-p mirror-region
                   (multiple-value-bind (x1 y1) (transform-position MT 0 0)
                     (let ((x2  (if (<= mx2 pw)
                                    mx2
                                    (floor (+ pw (min mx2 (+ #x8000 x1) #x8000)) 2)))
                           (y2  (if (<= my2 ph)
                                    my2
                                    (floor (+ ph (min my2 (+ #x8000 y1) #x8000)) 2))))
                       (when (and (< (- x2 x1) #x8000)
                                  (or (<= (max (- pw #x8000) mx1) x1 0) (coordinate= x1 mx1))
                                  (< (- y2 y1) #x8000)
                                  (or (<= (max (- pw #x8000) my1) y1 0) (coordinate= y1 my1))
                                  (> (round (- x2 x1)) 0)
                                  (> (round (- y2 y1)) 0))
                         (values t (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1)))))))))
          ;; Try reusing the native transformation:
          (when old-native-transformation
            (let ((MT (compose-transformations
                       (compose-transformations (sheet-native-transformation parent)
                                                (sheet-transformation sheet))
                       (invert-transformation old-native-transformation))))
              (multiple-value-bind (fits-p MR) (choose MT)
                (when fits-p
                  (%set-mirror-geometry sheet :MT MT :MR MR)
                  (return-from update-mirror-geometry)))))
          ;; Try reusing the mirror transformation:
          (when-let ((MT (%sheet-mirror-transformation sheet)))
            (multiple-value-bind (fits-p MR) (choose MT)
              (when fits-p
                (let ((native-transformation
                        ;; NT = T o PNT o MT⁻¹
                        (compose-transformations
                         (invert-transformation MT)
                         (compose-transformations (sheet-native-transformation parent)
                                                  (sheet-transformation sheet)))))
                  ;; finally reflect the change to the host window system
                  (%set-mirror-geometry sheet :MT MT :MR MR)
                  ;; update the native transformation if neccessary.
                  (unless (and old-native-transformation
                               (transformation-equal native-transformation
                                                     old-native-transformation))
                    (invalidate-cached-transformations sheet)
                    (%%set-sheet-native-transformation native-transformation sheet)
                    (when old-native-transformation
                      ;; Full sheet contents are redrawn.
                      (dispatch-repaint sheet
                                        (untransform-region native-transformation
                                                            (%effective-mirror-region sheet))))))
                (return-from update-mirror-geometry)))))
        ;; Otherwise just choose the geometry
        ;; Conditions to be met:
        ;;  x2 < #x8000 + x1
        ;;  x1 in [max(pw - #x8000, mx1), 0] u {mx1}
        ;;  x2 in [pw, min (#x8000, mx2)]    u {mx2}
        ;;
        ;; It can still happend, that we cannot meet the
        ;; window system limitations => the sheet is
        ;; unvisible.
        (let* ((x1 (if (>= mx1 0) (round mx1) (floor (max (- pw #x8000) mx1) 2)))
               (y1 (if (>= my1 0) (round my1) (floor (max (- ph #x8000) my1) 2)))
               (x2 (if (<= mx2 pw) mx2 (floor (+ pw (min mx2 (- #x8000 x1))) 2)))
               (y2 (if (<= my2 ph) my2 (floor (+ ph (min my2 (- #x8000 y1))) 2)))
               (MT (make-translation-transformation x1 y1))
               (MR (make-rectangle* 0 0 (round (- x2 x1)) (round (- y2 y1))))
               (native-transformation
                 ;; NT = T o PNT o MT⁻¹
                 (compose-transformations
                  (invert-transformation MT)
                  (compose-transformations (sheet-native-transformation (sheet-parent sheet))
                                           (sheet-transformation sheet))))
               (old-native-transformation
                 (%%sheet-native-transformation sheet)))

          (cond ((and (> (round (- x2 x1)) 0)
                      (> (round (- y2 y1)) 0))
                 ;; finally reflect the change to the host window system
                 (setf (%sheet-mirror-region sheet) MR)
                 (setf (%sheet-mirror-transformation sheet) MT)
                 (when (and (sheet-direct-mirror sheet)
                            (not (eql *configuration-event-p* sheet)))
                   (let ((port (port sheet))
                         (mirror (sheet-direct-mirror sheet)))
                     (port-set-mirror-region port mirror MR)
                     (port-set-mirror-transformation port mirror MT)))
                 ;; update the native transformation if necessary.
                 (unless (and old-native-transformation
                              (transformation-equal native-transformation
                                                    old-native-transformation))
                   (invalidate-cached-transformations sheet)
                   (%%set-sheet-native-transformation native-transformation sheet)
                   (when old-native-transformation
                     ;; native transformation has changed - repaint the sheet
                     (dispatch-repaint sheet
                                       (untransform-region native-transformation
                                                           (%effective-mirror-region sheet))))))
                (t
                 (%set-mirror-geometry sheet :invalidate-transformations t))))))))
