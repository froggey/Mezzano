(in-package #:climi)

;;;;
;;;;  30.4a Concrete Gadget Classes
;;;;

;;; ------------------------------------------------------------------------------------------
;;; 30.4.1 The concrete push-button Gadget

(defclass push-button-pane (sheet-leaf-mixin
                            arm/disarm-repaint-mixin
                            activate/deactivate-repaint-mixin
                            enter/exit-arms/disarms-mixin
                            push-button)
  ((pressedp          :initform nil)
   (show-as-default-p :type boolean
                      :initform nil
                      :initarg :show-as-default-p
                      :accessor push-button-show-as-default-p))
  (:default-initargs
   :background *3d-normal-color*
   :align-x :center
   :align-y :center
   :x-spacing 4
   :y-spacing 4))

(defmethod compose-space ((gadget push-button-pane) &key width height)
  (declare (ignore width height))
  (let ((2*x-spacing (* 2 (pane-x-spacing gadget)))
        (2*y-spacing (* 2 (pane-y-spacing gadget)))
        (2*border-thickness (* 2 *3d-border-thickness*)))
    (space-requirement+* (space-requirement+* (compose-label-space gadget)
                                              :min-width 2*x-spacing
                                              :width 2*x-spacing
                                              :max-width +fill+
                                              :min-height 2*y-spacing
                                              :height 2*y-spacing
                                              :max-height +fill+)
                         :min-width 2*border-thickness
                         :width 2*border-thickness
                         :min-height 2*border-thickness
                         :height 2*border-thickness)))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-press-event))
  (with-slots (pressedp) pane
    (setf pressedp t)
    (dispatch-repaint pane +everywhere+)))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-release-event))
  (with-slots (armed pressedp) pane
    (when pressedp
      (setf pressedp nil)
      (when armed
        (activate-callback pane (gadget-client pane) (gadget-id pane)))
      (dispatch-repaint pane +everywhere+))))

(defmethod handle-repaint ((pane push-button-pane) region)
  (declare (ignore region))
  (with-slots (armed pressedp) pane
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
      (draw-bordered-rectangle* pane x1 y1 x2 y2
                                :border-width 1
                                :style (if (and pressedp armed) :inset :outset))
      (let* ((x-spacing (pane-x-spacing pane))
             (y-spacing (pane-y-spacing pane))
             (border-thickness *3d-border-thickness*)
             (x1 (+ x1 border-thickness x-spacing))
             (y1 (+ y1 border-thickness y-spacing))
             (x2 (- x2 border-thickness x-spacing))
             (y2 (- y2 border-thickness y-spacing)))
        (if (gadget-active-p pane)
            (draw-label* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
            (draw-engraved-label* pane x1 y1 x2 y2))))))



;;; ------------------------------------------------------------------------------------------
;;;  30.4.2 The concrete toggle-button Gadget

(defclass toggle-button-pane (sheet-leaf-mixin
                              ;; repaint behavior:
                              arm/disarm-repaint-mixin
                              activate/deactivate-repaint-mixin
                              value-changed-repaint-mixin
                              ;; event handling:
                              enter/exit-arms/disarms-mixin
                              ;; abstract gadget:
                              toggle-button)
  ((indicator-type :type (member :one-of :some-of)
                   :initarg :indicator-type
                   :reader toggle-button-indicator-type
                   :initform :some-of) )
  (:default-initargs :value nil
                     :align-x :left
                     :align-y :center
                     :x-spacing 2
                     :y-spacing 2
                     :background *3d-normal-color*))

(defmethod compose-space ((pane toggle-button-pane) &key width height)
  (declare (ignore width height))
  (let ((sr (compose-label-space pane)))
    (space-requirement+*
     (space-requirement+* sr
                          :min-width  (* 3 (pane-x-spacing pane))
                          :width      (* 3 (pane-x-spacing pane))
                          :max-width  +fill+
                          :min-height (* 2 (pane-y-spacing pane))
                          :height     (* 2 (pane-y-spacing pane))
                          :max-height +fill+)
     :min-width (space-requirement-height sr)
     :width     (space-requirement-height sr)
     :min-height 0
     :height 0)))

(defgeneric draw-toggle-button-indicator (gadget type value x1 y1 x2 y2))

(defmethod draw-toggle-button-indicator ((gadget toggle-button-pane) (type (eql :one-of)) value x1 y1 x2 y2)
  (multiple-value-bind (cx cy) (values (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
    (let ((radius (/ (- y2 y1) 2)))
      (draw-circle* gadget cx cy radius
                    :start-angle (* 1/4 pi)
                    :end-angle (* 5/4 pi)
                    :ink *3d-dark-color*)
      (draw-circle* gadget cx cy radius
                    :start-angle (* 5/4 pi)
                    :end-angle (* 9/4 pi)
                    :ink *3d-light-color*)
      (draw-circle* gadget cx cy (max 1 (- radius 2))
                    :ink (effective-gadget-input-area-color gadget))
      (when value
        (draw-circle* gadget cx cy (max 1 (- radius 4))
                      :ink (effective-gadget-foreground gadget))))))

(defmethod draw-toggle-button-indicator ((pane toggle-button-pane) (type (eql :some-of)) value
                                         x1 y1 x2 y2)
  (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-input-area-color pane))
  (draw-bordered-rectangle* pane x1 y1 x2 y2 :style :inset)
  (when value
    (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 3) (+ y1 3)
                                               (- x2 3) (- y2 3))
      (draw-line* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane) :line-thickness 2)
      (draw-line* pane x2 y1 x1 y2 :ink (effective-gadget-foreground pane) :line-thickness 2))))

(defmethod handle-repaint ((pane toggle-button-pane) region)
  (declare (ignore region))
  (when (sheet-grafted-p pane)
    (with-slots (armed) pane
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
        (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
        (let* ((as (text-style-ascent (pane-text-style pane) pane))
               (ds (text-style-descent (pane-text-style pane) pane)) )
          (multiple-value-bind (tx1 ty1 tx2 ty2)
              (values (+ x1 (pane-x-spacing pane))
                      (- (/ (+ y1 y2) 2) (/ (+ as ds) 2))
                      (+ x1 (pane-x-spacing pane) (+ as ds))
                      (+ (/ (+ y1 y2) 2) (/ (+ as ds) 2)))
            (draw-toggle-button-indicator pane (toggle-button-indicator-type pane) (gadget-value pane)
                                          tx1 ty1 tx2 ty2)
            (if (gadget-active-p pane)
                (draw-label* pane (+ tx2 (pane-x-spacing pane)) y1 x2 y2
                             :ink (effective-gadget-foreground pane))
                (draw-engraved-label* pane (+ tx2 (pane-x-spacing pane)) y1 x2 y2))))))))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when armed
      (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))))


;;; ------------------------------------------------------------------------------------------
;;;  30.4.3 The concrete menu-button Gadget

(defclass menu-button-pane (sheet-leaf-mixin
                            activate/deactivate-repaint-mixin
                            menu-button)
  ()
  (:default-initargs :background *3d-normal-color*
                     :x-spacing 3
                     :y-spacing 2
                     :align-x :left
                     :align-y :center))

(defmethod handle-repaint ((pane menu-button-pane) region)
  (declare (ignore region))
  (with-slots (x-spacing y-spacing) pane
    (let ((region (sheet-region pane)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
        (draw-rectangle* pane x1 y1 x2 y2
                         :ink (effective-gadget-background pane)
                         :filled t)
        (cond ((slot-value pane 'armed)
               (draw-bordered-rectangle* pane x1 y1 x2 y2 :style :outset :border-width *3d-border-thickness*))
              (t))
        (multiple-value-bind (x1 y1 x2 y2)
            (values (+ x1 x-spacing) (+ y1 y-spacing)
                    (- x2 x-spacing) (- y2 y-spacing))
          (if (gadget-active-p pane)
              (draw-label* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
              (draw-engraved-label* pane x1 y1 x2 y2)))))))

(defmethod compose-space ((gadget menu-button-pane) &key width height)
  (declare (ignore width height))
  (space-requirement+* (space-requirement+* (compose-label-space gadget)
                                            :min-width (* 2 (pane-x-spacing gadget))
                                            :width (* 2 (pane-x-spacing gadget))
                                            :max-width +fill+
                                            :min-height (* 2 (pane-y-spacing gadget))
                                            :height (* 2 (pane-y-spacing gadget))
                                            :max-height +fill+)
                       :min-width (* 2 *3d-border-thickness*)
                       :width (* 2 *3d-border-thickness*)
                       :min-height (* 2 *3d-border-thickness*)
                       :height (* 2 *3d-border-thickness*)))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.4 The concrete scroll-bar Gadget

(defclass scroll-bar-pane (sheet-leaf-mixin
                           3D-border-mixin
                           scroll-bar)
  ((event-state :initform nil)
   (drag-dy :initform nil)
   ;;; poor man's incremental redisplay
   ;; drawn state
   (up-state :initform nil)
   (dn-state :initform nil)
   (tb-state :initform nil)
   (tb-y1    :initform nil)
   (tb-y2    :initform nil)
   ;; old drawn state
   (old-up-state :initform nil)
   (old-dn-state :initform nil)
   (old-tb-state :initform nil)
   (old-tb-y1    :initform nil)
   (old-tb-y2    :initform nil)
   ;;
   (all-new-p    :initform t) )
  (:default-initargs :border-width 2
                     :border-style :inset
                     :background *3d-inner-color*))

(defmethod compose-space ((sb scroll-bar-pane) &key width height)
  (declare (ignore width height))
  (if (eq (gadget-orientation sb) :vertical)
      (make-space-requirement :min-width 1
                              :width *scrollbar-thickness*
                              :min-height (* 3 *scrollbar-thickness*)
                              :height (* 4 *scrollbar-thickness*))
      (make-space-requirement :min-height 1
                              :height *scrollbar-thickness*
                              :min-width (* 3 *scrollbar-thickness*)
                              :width (* 4 *scrollbar-thickness*))))

;;;; Redisplay

(defgeneric scroll-bar-transformation (scroll-bar))

(defgeneric scroll-bar-up-region (scroll-bar))

(defgeneric scroll-bar-down-region (scroll-bar))

(defgeneric scroll-bar-thumb-bed-region (scroll-bar-pane))

(defgeneric scroll-bar-thumb-region (scroll-bar-pane &optional value))

(defun scroll-bar/update-display (scroll-bar &optional (value (gadget-value scroll-bar)))
  (with-slots (up-state dn-state tb-state tb-y1 tb-y2
               old-up-state old-dn-state old-tb-state old-tb-y1 old-tb-y2
               all-new-p)
      scroll-bar
    ;;
    (scroll-bar/compute-display scroll-bar value)
    ;; redraw up arrow
    (unless (and (not all-new-p) (eql up-state old-up-state))
      (with-drawing-options (scroll-bar :transformation (scroll-bar-transformation scroll-bar))
        (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-up-region scroll-bar)
          (draw-rectangle* scroll-bar x1 y1 x2 y2 :ink *3d-inner-color*)
          (let ((pg (list (make-point (/ (+ x1 x2) 2) y1)
                          (make-point x1 y2)
                          (make-point x2 y2))))
            (case up-state
              (:armed
               (draw-polygon scroll-bar pg :ink *3d-inner-color*)
               (draw-bordered-polygon scroll-bar pg :style :inset :border-width 2))
              (otherwise
               (draw-polygon scroll-bar pg :ink *3d-normal-color*)
               (draw-bordered-polygon scroll-bar pg :style :outset :border-width 2) ))))) )
    ;; redraw dn arrow
    (unless (and (not all-new-p) (eql dn-state old-dn-state))
      (with-drawing-options (scroll-bar :transformation (scroll-bar-transformation scroll-bar))
        (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-down-region scroll-bar)
          (draw-rectangle* scroll-bar x1 y1 x2 y2 :ink *3d-inner-color*)
          (let ((pg (list (make-point (/ (+ x1 x2) 2) y2)
                          (make-point x1 y1)
                          (make-point x2 y1))))
            (case dn-state
              (:armed
               (draw-polygon scroll-bar pg :ink *3d-inner-color*)
               (draw-bordered-polygon scroll-bar pg :style :inset :border-width 2))
              (otherwise
               (draw-polygon scroll-bar pg :ink *3d-normal-color*)
               (draw-bordered-polygon scroll-bar pg :style :outset :border-width 2)))))))
    ;; thumb
    (unless (and (not all-new-p)
                 (and (eql tb-state old-tb-state)
                      (eql tb-y1 old-tb-y1)
                      (eql tb-y2 old-tb-y2)))
      (cond ((and (not all-new-p)
                  (eql tb-state old-tb-state)
                  (numberp tb-y1) (numberp old-tb-y1)
                  (numberp tb-y2) (numberp old-tb-y2)
                  (= (- tb-y2 tb-y1) (- old-tb-y2 old-tb-y1)))
             ;; Thumb is just moving, compute old and new region
             (multiple-value-bind (x1 ignore.1 x2 ignore.2)
                 (bounding-rectangle* (scroll-bar-thumb-bed-region scroll-bar))
               (declare (ignore ignore.1 ignore.2))
               ;; compute new and old region
               (with-sheet-medium (medium scroll-bar)
                 (with-drawing-options (medium :transformation (scroll-bar-transformation scroll-bar))
                   (multiple-value-bind (ox1 oy1 ox2 oy2) (values x1 old-tb-y1 x2 old-tb-y2)
                     (multiple-value-bind (nx1 ny1 nx2 ny2) (values x1 tb-y1 x2 tb-y2)
                       (declare (ignore nx2))
                       (copy-area medium ox1 oy1 (- ox2 ox1) (- oy2 oy1) nx1 ny1)
                       ;; clear left-overs from the old region
                       (if (< oy1 ny1)
                           (draw-rectangle* medium ox1 oy1 ox2 ny1 :ink *3d-inner-color*)
                           (draw-rectangle* medium ox1 oy2 ox2 ny2 :ink *3d-inner-color*)))) ))))
            (t
             ;; redraw whole thumb bed and thumb all anew
             (with-drawing-options (scroll-bar :transformation (scroll-bar-transformation scroll-bar))
               (with-bounding-rectangle* (bx1 by1 bx2 by2) (scroll-bar-thumb-bed-region scroll-bar)
                 (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-region scroll-bar value)
                   (draw-rectangle* scroll-bar bx1 by1 bx2 y1 :ink *3d-inner-color*)
                   (draw-rectangle* scroll-bar bx1 y2 bx2 by2 :ink *3d-inner-color*)
                   (draw-rectangle* scroll-bar x1 y1 x2 y2 :ink *3d-normal-color*)
                   (draw-bordered-polygon scroll-bar
                                          (polygon-points (make-rectangle* x1 y1 x2 y2))
                                          :style :outset
                                          :border-width 2)
                    ;;;;;;
                   (let ((y (/ (+ y1 y2) 2)))
                     (draw-bordered-polygon scroll-bar
                                            (polygon-points (make-rectangle* (+ x1 3) (- y 1) (- x2 3) (+ y 1)))
                                            :style :inset
                                            :border-width 1)
                     (draw-bordered-polygon scroll-bar
                                            (polygon-points (make-rectangle* (+ x1 3) (- y 4) (- x2 3) (- y 2)))
                                            :style :inset
                                            :border-width 1)
                     (draw-bordered-polygon scroll-bar
                                            (polygon-points (make-rectangle* (+ x1 3) (+ y 4) (- x2 3) (+ y 2)))
                                            :style :inset
                                            :border-width 1))))))))
    (setf old-up-state up-state
          old-dn-state dn-state
          old-tb-state tb-state
          old-tb-y1 tb-y1
          old-tb-y2 tb-y2
          all-new-p nil) ))

(defun scroll-bar/compute-display (scroll-bar value)
  (with-slots (up-state dn-state tb-state tb-y1 tb-y2
               event-state) scroll-bar
    (setf up-state (if (eq event-state :up-armed) :armed nil))
    (setf dn-state (if (eq event-state :dn-armed) :armed nil))
    (setf tb-state nil)                 ;we have no armed display yet
    (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-region scroll-bar value)
      (declare (ignore x1 x2))
      (setf tb-y1 y1
            tb-y2 y2))))

;;;; Utilities

;; We think all scroll bars as vertically oriented, therefore we have
;; SCROLL-BAR-TRANSFORMATION, which should make every scroll bar
;; look like being vertically oriented -- simplifies much code.

(defmethod scroll-bar-transformation ((sb scroll-bar))
  (ecase (gadget-orientation sb)
    (:vertical   +identity-transformation+)
    (:horizontal (make-transformation 0 1 1 0 0 0))))

(defun translate-range-value (a mina maxa mino maxo
                              &optional (empty-result (/ (+ mino maxo) 2)))
  "When \arg{a} is some value in the range from \arg{mina} to \arg{maxa},
   proportionally translate the value into the range \arg{mino} to \arg{maxo}."
  (if (zerop (- maxa mina))
      empty-result
      (+ mino (* (/ (- a mina)
                    (- maxa mina))
                 (- maxo mino)))))

;;;; SETF :after methods

(defmethod (setf gadget-min-value) :after (new-value (pane scroll-bar-pane))
  (declare (ignore new-value))
  (scroll-bar/update-display pane))

(defmethod (setf gadget-max-value) :after (new-value (pane scroll-bar-pane))
  (declare (ignore new-value))
  (scroll-bar/update-display pane))

(defmethod (setf scroll-bar-thumb-size) :after (new-value (pane scroll-bar-pane))
  (declare (ignore new-value))
  (scroll-bar/update-display pane))

(defmethod (setf gadget-value) :after (new-value (pane scroll-bar-pane) &key invoke-callback)
  (declare (ignore new-value invoke-callback))
  (scroll-bar/update-display pane))

(defmethod* (setf scroll-bar-values)
    (min-value max-value thumb-size value (scroll-bar scroll-bar-pane))
  (setf (slot-value scroll-bar 'min-value) min-value
        (slot-value scroll-bar 'max-value) max-value
        (slot-value scroll-bar 'thumb-size) thumb-size
        (slot-value scroll-bar 'value) value)
  (scroll-bar/update-display scroll-bar))

;;;; geometry

(defparameter +minimum-thumb-size-in-pixels+ 30)

(defmethod scroll-bar-up-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore maxy))
    (make-rectangle* minx miny
                     maxx (+ miny (- maxx minx)))))

(defmethod scroll-bar-down-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore miny))
    (make-rectangle* minx (- maxy (- maxx minx))
                     maxx maxy)))

(defun scroll-bar/thumb-bed* (sb)
  ;; -> y1 y2 y3
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (let ((y1 (+ miny (- maxx minx) 1))
          (y3 (- maxy (- maxx minx) 1)))
      (let ((ts (scroll-bar-thumb-size sb)))
        ;; This is the right spot to handle ts = :none or perhaps NIL
        (multiple-value-bind (range) (gadget-range sb)
          (let ((ts-in-pixels (round (* (- y3 y1) (/ ts (max 1 (+ range ts))))))) ; handle range + ts = 0
            (setf ts-in-pixels (min (- y3 y1) ;thumb can't be larger than the thumb bed
                                    (max +minimum-thumb-size-in-pixels+ ;but shouldn't be smaller than this.
                                         ts-in-pixels)))
            (values
             y1
             (- y3 ts-in-pixels)
             y3)))))))

(defmethod scroll-bar-thumb-bed-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore miny maxy))
    (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
      (declare (ignore y2))
      (make-rectangle* minx y1
                       maxx y3))))

(defun scroll-bar/map-coordinate-to-value (sb y)
  (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
    (declare (ignore y3))
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (translate-range-value y y1 y2 minv maxv minv))))

(defun scroll-bar/map-value-to-coordinate (sb v)
  (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
    (declare (ignore y3))
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (round (translate-range-value v minv maxv y1 y2 y1)))))

(defmethod scroll-bar-thumb-region ((sb scroll-bar-pane) &optional (value (gadget-value sb)))
  (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-bed-region sb)
    (declare (ignore y1 y2))
    (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
      (declare (ignore y1))
      (let ((y4 (scroll-bar/map-value-to-coordinate sb value)))
        (make-rectangle* x1 y4 x2 (+ y4 (- y3 y2)))))))

;;;; event handler

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (with-slots (event-state drag-dy) sb
      (cond ((region-contains-position-p (scroll-bar-up-region sb) x y)
             (scroll-up-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :up-armed)
             (scroll-bar/update-display sb))
            ((region-contains-position-p (scroll-bar-down-region sb) x y)
             (scroll-down-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :dn-armed)
             (scroll-bar/update-display sb))
            ;;
            ((region-contains-position-p (scroll-bar-thumb-region sb) x y)
             (setf event-state :dragging
                   drag-dy (- y (bounding-rectangle-min-y (scroll-bar-thumb-region sb)))))
            ;;
            ((region-contains-position-p (scroll-bar-thumb-bed-region sb) x y)
             (if (< y (bounding-rectangle-min-y (scroll-bar-thumb-region sb)))
                 (scroll-up-page-callback sb (gadget-client sb) (gadget-id sb))
                 (scroll-down-page-callback sb (gadget-client sb) (gadget-id sb))))
            (t
             nil)))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-motion-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (declare (ignore x))
    (with-slots (event-state drag-dy) sb
      (case event-state
        (:dragging
         (let* ((y-new-thumb-top (- y drag-dy))
                (new-value
                  (min (gadget-max-value sb)
                       (max (gadget-min-value sb)
                            (scroll-bar/map-coordinate-to-value sb y-new-thumb-top)))) )
           (scroll-bar/update-display sb new-value)
           (drag-callback sb (gadget-client sb) (gadget-id sb) new-value)) )))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-release-event))
  (with-slots (event-state) sb
    (case event-state
      (:up-armed (setf event-state nil))
      (:dn-armed (setf event-state nil))
      (:dragging
       (setf event-state nil)
       (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                      (pointer-event-x event) (pointer-event-y event))
         (declare (ignore x))
         ;; Update the gadget-value
         (with-slots (drag-dy) sb
           (let* ((y-new-thumb-top (- y drag-dy))
                  (new-value
                    (min (gadget-max-value sb)
                         (max (gadget-min-value sb)
                              (scroll-bar/map-coordinate-to-value sb y-new-thumb-top)))) )
             (setf (gadget-value sb :invoke-callback t) new-value)))))
      (otherwise
       (setf event-state nil))))
  (scroll-bar/update-display sb))

(defmethod handle-event ((pane scroll-bar-pane) (event pointer-scroll-event))
  (with-slots (event-state) pane
    (when (eq event-state nil) ; when not currently processing a button press
      (let ((function (if (minusp (pointer-event-delta-y event))
                          'scroll-up-page-callback
                          'scroll-down-page-callback)))
        (funcall function pane (gadget-client pane) (gadget-id pane))))))

(defmethod handle-repaint ((pane scroll-bar-pane) region)
  (with-slots (all-new-p) pane
    (setf all-new-p t)
    (scroll-bar/update-display pane)))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.5 The concrete slider Gadget

;; ----------------------------------------------------------
;; What should be done for having a better look for sliders
;;
;; We should find a way to draw the value, when show-value-p
;; is true, in a good position, or to dedicate a particular
;; sheet for this drawing (this sheet would be inside the
;; slider's sheet, probably his child).
;; ----------------------------------------------------------

(defgeneric convert-position-to-value (slider-pane position)
  (:documentation
   "Return gadget value for SLIDER-PANE corresponding to POSITION.

    POSITION can be a real number or a pointer event. Both designate a
    horizontal or vertical position in the gadget's coordinate
    system."))

(defgeneric convert-value-to-position (slider-pane)
  (:documentation
   "Return a position for SLIDER-PANE's gadget value.

    The returned position measures a distance along the horizontal or
    vertical axis of the gadget's coordinate system."))

;; This values should be changeable by user. That's
;; why they are parameters, and not constants.
(defparameter slider-button-short-dim 10)

(defclass slider-pane (sheet-leaf-mixin
                       value-changed-repaint-mixin
                       activate/deactivate-repaint-mixin
                       slider)
  ())

(defmethod compose-space ((pane slider-pane) &key width height)
  (declare (ignore width height))
  (let* ((value-size (ecase (gadget-orientation pane)
                       (:horizontal (text-style-ascent (pane-text-style pane) pane))
                       (:vertical (let* ((dp (slider-decimal-places pane))
                                         (s1 (format-value (gadget-min-value pane) dp))
                                         (s2 (format-value (gadget-max-value pane) dp)))
                                    (* (max (length s1) (length s2))
                                       (text-size pane #\0))))))
         (minor (if (gadget-show-value-p pane)
                    (* 2 (+ 10.0 value-size)) ; the value
                    (* 2 (1+ 8.0)))) ; the knob
         (major 128))
    (if (eq (gadget-orientation pane) :vertical)
        (make-space-requirement :min-width  minor :width  minor
                                :min-height major :height major)
        (make-space-requirement :min-width  major :width  major
                                :min-height minor :height minor))))

(defmethod handle-event ((pane slider-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t))
    (armed-callback pane (gadget-client pane) (gadget-id pane))))

(defmethod handle-event ((pane slider-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when (and armed
               (not (eq armed ':button-press)))
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (when armed
      (setf armed ':button-press))))

(defmethod handle-event ((pane slider-pane) (event pointer-motion-event))
  (with-slots (armed) pane
    (when (eq armed ':button-press)
      (let ((value (convert-position-to-value pane event)))
        (setf (gadget-value pane :invoke-callback nil) value)
        (drag-callback pane (gadget-client pane) (gadget-id pane) value)
        (dispatch-repaint pane (sheet-region pane))))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eq armed ':button-press)
      (setf armed t
            (gadget-value pane :invoke-callback t)
            (convert-position-to-value pane event))
      (dispatch-repaint pane (sheet-region pane)))))

(defmethod handle-event ((pane slider-pane) (event pointer-scroll-event))
  (with-slots (armed) pane
    (when (eq armed t) ; when armed and not currently dragging
      (let* ((old-value (gadget-value pane))
             (new-value (+ old-value (- (pointer-event-delta-y event))))
             (effective-new-value (alexandria:clamp new-value
                                                    (gadget-min-value pane)
                                                    (gadget-max-value pane))))
        (unless (eql effective-new-value old-value)
          (setf (gadget-value pane :invoke-callback t) effective-new-value)
          (dispatch-repaint pane (sheet-region pane)))))))

(defun format-value (value decimal-places)
  (if (<= decimal-places 0)
      (format nil "~D" (round value))
      (format nil "~,VF" decimal-places value)))

(defmethod handle-repaint ((pane slider-pane) region)
  (declare (ignore region))
  (let ((position (convert-value-to-position pane))
        (slider-button-half-short-dim (floor slider-button-short-dim 2))
        (background-color (pane-background pane))
        (inner-color (gadget-current-color pane)))
    (flet ((draw-knob (x y)
             (if (gadget-active-p pane)
                 (progn
                   (draw-circle* pane x y 8.0 :filled t :ink inner-color)
                   (draw-circle* pane x y 8.0 :filled nil :ink +black+)
                   (draw-circle* pane x y 7.0
                                 :filled nil :ink +white+
                                 :start-angle (* 0.25 pi)
                                 :end-angle   (* 1.25 pi))
                   (draw-circle* pane x y 7.0
                                 :filled nil :ink +black+
                                 :start-angle (* 1.25 pi)
                                 :end-angle   (* 2.25 pi)))
                 (progn
                   (draw-circle* pane (1+ x) (1+ y) 8.0 :filled t :ink *3d-light-color*)
                   (draw-circle* pane x y 8.0 :filled t :ink *3d-dark-color*))))
           (draw-value (x y)
             (let ((text (format-value (gadget-value pane)
                                       (slider-decimal-places pane))))
               (if (gadget-active-p pane)
                   (draw-text* pane text x y)
                   (progn
                     (draw-text* pane text (1+ x) (1+ y)
                                 :ink *3d-light-color*)
                     (draw-text* pane text x y
                                 :ink *3d-dark-color*))))))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
        (display-gadget-background pane background-color 0 0 (- x2 x1) (- y2 y1))
        (ecase (gadget-orientation pane)
          ((:vertical)
           (let ((middle (round (- x2 x1) 2)))
             (draw-bordered-polygon pane
                                    (polygon-points
                                     (make-rectangle*
                                      (- middle 2) (+ y1 slider-button-half-short-dim)
                                      (+ middle 2) (- y2 slider-button-half-short-dim)))
                                    :style :inset
                                    :border-width 2)
             (draw-knob middle position)
             (when (gadget-show-value-p pane)
               (draw-value (+ middle 10.0)
                           (- y2 slider-button-short-dim)))))
          ((:horizontal)
           (let ((middle (round (- y2 y1) 2)))
             (draw-bordered-polygon pane
                                    (polygon-points
                                     (make-rectangle*
                                      (+ x1 slider-button-half-short-dim) (- middle 2)
                                      (- x2 slider-button-half-short-dim) (+ middle 2)))
                                    :style :inset
                                    :border-width 2)
             (draw-knob position middle)
             (when (gadget-show-value-p pane)
               (draw-value (+ x1 slider-button-short-dim)
                           (- middle 10.0))))))))))

(flet ((compute-dims (slider)
         (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region slider))
           ;; Offset is the distance from the bounding region to the
           ;; slider's "rail" and then some, so the knob doesn't go
           ;; beyond the rail too much.
           (let ((offset (+ (floor slider-button-short-dim 2) 4)))
             (if (eq (gadget-orientation slider) :vertical)
                 (values (+ y1 offset) (- y2 offset))
                 (values (+ x1 offset) (- x2 offset)))))))

  (defmethod convert-value-to-position ((pane slider-pane))
    (multiple-value-bind (good-dim1 good-dim2) (compute-dims pane)
      (alexandria:lerp (unlerp (gadget-value pane)
                               (gadget-min-value pane) (gadget-max-value pane))
                       good-dim1 good-dim2)))

  (defmethod convert-position-to-value ((pane slider-pane) (position real))
    (multiple-value-bind (good-dim1 good-dim2) (compute-dims pane)
      (let* ((clamped (alexandria:clamp position good-dim1 good-dim2))
             (displacement (unlerp clamped good-dim1 good-dim2))
             (quantized (if-let ((quanta (slider-number-of-quanta pane)))
                          (/ (round (* displacement quanta)) quanta)
                          displacement)))
        (alexandria:lerp quantized (gadget-min-value pane) (gadget-max-value pane))))))

(defmethod convert-position-to-value ((pane slider-pane) (position pointer-event))
  (convert-position-to-value pane (if (eq (gadget-orientation pane) :vertical)
                                      (pointer-event-y position)
                                      (pointer-event-x position))))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.6 The concrete radio-box and check-box Gadgets

;; radio-box

(defclass radio-box-pane (sheet-multiple-child-mixin
                          rack-layout-mixin
                          activate/deactivate-repaint-mixin
                          activate/deactivate-children-mixin
                          radio-box)
  ()
  (:default-initargs :background *3d-normal-color*))

(defmethod initialize-instance :after ((pane radio-box-pane)
                                       &key choices current-selection orientation (active t) &allow-other-keys)
  (setf (box-layout-orientation pane) orientation)
  (setf (gadget-value pane) current-selection)
  (let ((children
          (mapcar (lambda (c)
                    (let ((c (if (stringp c)
                                 (make-pane 'toggle-button-pane :label c :value nil)
                                 c)))
                      (setf (gadget-value c) (if (eq c (radio-box-current-selection pane)) t nil))
                      (setf (gadget-client c) pane)
                      c))
                  choices)))
    (mapc (curry #'sheet-adopt-child pane) children))
  (unless active
    (deactivate-gadget pane)))

(defmethod (setf gadget-value) :after (button (radio-box radio-box-pane) &key invoke-callback)
  ;; this is silly, but works ...
  (dolist (c (sheet-children radio-box))
    (unless (eq (not (null (eq c button)))
                (not (null (gadget-value c))))
      (setf (gadget-value c :invoke-callback invoke-callback) (eq c button)) )))

;; check-box

(defclass check-box-pane (sheet-multiple-child-mixin
                          rack-layout-mixin
                          activate/deactivate-repaint-mixin
                          activate/deactivate-children-mixin
                          check-box)
  ()
  (:default-initargs :background *3d-normal-color*))

(defmethod initialize-instance :after ((pane check-box-pane)
                                       &key choices current-selection orientation (active t) &allow-other-keys)
  (setf (box-layout-orientation pane) orientation)
  (setf (gadget-value pane) current-selection)
  (let ((children
          (mapcar (lambda (c)
                    (let ((c (if (stringp c)
                                 (make-pane 'toggle-button-pane :label c :value nil)
                                 c)))
                      (setf (gadget-value c) (if (member c current-selection) t nil))
                      (setf (gadget-client c) pane)
                      c))
                  choices)))
    (mapc (curry #'sheet-adopt-child pane) children))
  (unless active
    (deactivate-gadget pane)))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.7 The concrete list-pane and option-pane Gadgets


;;; LIST-PANE

;; Note: According to the LispWorks CLIM User's Guide, they do some peculiar
;; things in their list pane. Instead of :exclusive and :nonexclusive modes,
;; they call them :one-of and :some-of. I've supported these aliases for
;; compatibility. They also state the default mode is :some-of, which
;; contradicts the CLIM 2.0 Spec and doesn't make a lot of sense.
;; McCLIM defaults to :one-of.

;; TODO: Improve performance in order to scale to extremely large lists.
;;        * Computing text-size for a 100k list items is expensive
;;        * Need to share text size and cache of computed name-key/value-key
;;          results with LIST-PANE when instantiated in the popup for
;;          the OPTION-PANE.
;;        * Improve repaint logic when items are selected to reduce flicker.
;;       Currently the list and option panes are usable up to several thousand
;;       items on a reasonably fast P4.

;; TODO: Consider appearance of nonexclusive option-pane when multiple items are
;;       selected.

;; TODO: I think the list/option gadgets currently ignore enabled/disabled status.

;; Notes
;;   A some-of/nonexclusive list pane (or option-pane popup window) supports
;;   the following behaviors:
;;       single-click: toggle selected item
;;        shift-click: select/deselect multiple items. Selection or deselection
;;                     is chosen according to the result of your previous click.
;;  McCLIM adds an initarg :prefer-single-selection. If true, a nonexclusive pane
;;  will deselect other items selected when a new selection is made. Multiple
;;  items can be selected using control-click, or shift-click as before. This
;;  imitates the behvior of certain GUIs and may be useful in applications.

(define-abstract-pane-mapping 'list-pane 'generic-list-pane)

(defclass generic-list-pane (sheet-leaf-mixin
                             activate/deactivate-repaint-mixin
                             value-changed-repaint-mixin
                             list-pane)
  ((highlight-ink :initform +royalblue4+
                  :initarg :highlight-ink
                  :reader list-pane-highlight-ink)
   (item-strings :initform nil
                 :documentation "Vector of item strings.")
   (item-values :initform nil
                :documentation "Vector of item values.")
   (items-width  :initform nil
                 :documentation "Width sufficient to contain all items")
   (last-action  :initform nil
                 :documentation "Last action performed on items in the pane, either
:select, :deselect, or NIL if none has been performed yet.")
   (last-index   :initform nil
                 :documentation "Index of last item clicked, for extending selections.")
   (prefer-single-selection :initform nil :initarg :prefer-single-selection
                            :documentation "For nonexclusive menus, emulate the common behavior of
preferring selection of a single item, but allowing extension of the
selection via the control modifier.")
   (items-length :initform nil
                 :documentation "Number of items")
   (items-origin :initform 0 :accessor items-origin
                 :documentation "Index of the first item to be rendered. This changes in
response to scroll wheel events."))
  (:default-initargs :background +white+
                     :foreground +black+))

(defmethod initialize-instance :after ((gadget meta-list-pane) &rest rest)
  (declare (ignorable rest))
  ;; Initialize slot value if not specified
  #+ (or) ;; XXX
  (when (slot-boundp gadget 'value)
    (setf (slot-value gadget 'value)
          (if (list-pane-exclusive-p gadget)
              (funcall (list-pane-value-key gadget) (first (list-pane-items gadget)))
              (mapcar #'list-pane-value-key (list (first (list-pane-items gadget)))))))

  (when (and (not (list-pane-exclusive-p gadget))
             (not (listp (gadget-value gadget))))
    (error "A :nonexclusive list-pane cannot be initialized with a value which is not a list."))
  (when (not (list-pane-exclusive-p gadget))
    (with-slots (value) gadget
      (setf value (copy-list value))))
  #+ (or)
  (when (and (list-pane-exclusive-p gadget)
             (> (length (gadget-value gadget)) 1))
    (error "An 'exclusive' list-pane cannot be initialized with more than one item selected.")))

(defmethod value-changed-callback :before
    ((gadget generic-list-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  ;; Maybe act as if a presentation was clicked on, but only if the
  ;; list pane only allows single-selection.
  (when (or (eq (list-pane-mode gadget) :one-of)
            (eq (list-pane-mode gadget) :exclusive))
    (let* ((i (position value (generic-list-pane-item-values gadget)))
           (item (elt (list-pane-items gadget) i))
           (ptype (funcall (list-pane-presentation-type-key gadget) item)))
      (when ptype
        (throw-object-ptype value ptype)))))

(defun list-pane-exclusive-p (pane)
  (or (eql (list-pane-mode pane) :exclusive)
      (eql (list-pane-mode pane) :one-of)))

(defmethod initialize-instance :after ((gadget generic-list-pane) &rest rest)
  (declare (ignorable rest))
  ;; For a nonexclusive list-pane, compute some reasonable default for the last
  ;; selected item to make shift-click do something useful.
  (when (not (list-pane-exclusive-p gadget))
    (with-slots (test last-action last-index) gadget
      (when (not (zerop (length (gadget-value gadget))))
        (setf last-action :select
              last-index
              (reduce #'max
                      (mapcar #'(lambda (item) (position item (generic-list-pane-item-values gadget) :test test))
                              (gadget-value gadget))))))))

(defgeneric generic-list-pane-item-strings (generic-list-pane))

(defmethod generic-list-pane-item-strings ((pane generic-list-pane))
  (with-slots (item-strings) pane
    (or item-strings
        (setf item-strings
              (map 'vector (lambda (item)
                             (let ((s (funcall (list-pane-name-key pane) item)))
                               (if (stringp s)
                                   s
                                   (princ-to-string s)))) ;defensive programming!
                   (list-pane-items pane))))))

(defgeneric generic-list-pane-item-values (generic-list-pane))

(defmethod generic-list-pane-item-values ((pane generic-list-pane))
  (with-slots (item-values) pane
    (or item-values
        (setf item-values
              (map 'vector (list-pane-value-key pane) (list-pane-items pane))))))

(defgeneric generic-list-pane-items-width (generic-list-pane))

(defmethod generic-list-pane-items-width ((pane generic-list-pane))
  (with-slots (items-width) pane
    (or items-width
        (setf items-width
              (reduce #'max (map 'vector (lambda (item-string)
                                           (text-size pane item-string))
                                 (generic-list-pane-item-strings pane))
                      :initial-value 0)))))

(defgeneric generic-list-pane-items-length (generic-list-pane))

(defmethod generic-list-pane-items-length ((pane generic-list-pane))
  (with-slots (items-length) pane
    (or items-length
        (setf items-length
              (length (generic-list-pane-item-strings pane))))))

(defgeneric generic-list-pane-item-height (generic-list-pane))

(defmethod generic-list-pane-item-height ((pane generic-list-pane))
  (+ (text-style-ascent  (pane-text-style pane) pane)
     (text-style-descent (pane-text-style pane) pane)))

(defmethod visible-items ((pane generic-list-pane))
  (or (slot-value pane 'visible-items)
      (generic-list-pane-items-length pane)))

(defmethod (setf visible-items) (new-value (pane generic-list-pane))
  (setf (slot-value pane 'visible-items) new-value))

(defmethod compose-space ((pane generic-list-pane) &key width height)
  (declare (ignore width height))
  (let* ((n (visible-items pane))
         (w (generic-list-pane-items-width pane))
         (h (* n (generic-list-pane-item-height pane))))
    (make-space-requirement :width w     :height h
                            :min-width w :min-height h
                            :max-width +fill+ :max-height +fill+)))

(defmethod allocate-space ((pane generic-list-pane) w h)
  (resize-sheet pane w h))

(defmethod scroll-quantum ((pane generic-list-pane))
  (generic-list-pane-item-height pane))

(defmethod handle-repaint ((pane generic-list-pane) region)
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region pane)
    (declare (ignore sx1 sy1))
    (with-bounding-rectangle* (rx0 ry0 rx1 ry1)
        (if (bounding-rectangle-p region)
            region
            (or (pane-viewport-region pane)   ; workaround for +everywhere+
                (sheet-region pane)))
      (let ((item-height (generic-list-pane-item-height pane))
            (highlight-ink (list-pane-highlight-ink pane)))
        (do* ((index (floor (- ry0 sy0) item-height) (1+ index))
              (elt-index (+ index (items-origin pane)) (1+ elt-index)))
             ((or (> (+ sy0 (* item-height index)) ry1)
                  (>= elt-index (generic-list-pane-items-length pane))
                  (>= elt-index (+ (items-origin pane) (visible-items pane)))))
          (let ((y0 (+ sy0 (* index item-height)))
                (y1 (+ sy0 (* (1+ index) item-height))))
            (multiple-value-bind (background foreground)
                (cond ((not (slot-boundp pane 'value))
                       (values (pane-background pane) (pane-foreground pane)))
                      ((if (list-pane-exclusive-p pane)
                           (funcall (list-pane-test pane)
                                    (elt (generic-list-pane-item-values pane)
                                         elt-index)
                                    (gadget-value pane))
                           (member (elt (generic-list-pane-item-values pane)
                                        elt-index)
                                   (gadget-value pane)
                                   :test (list-pane-test pane)))
                       (values highlight-ink (pane-background pane)))
                      (t (values (pane-background pane) (pane-foreground pane))))
              (draw-rectangle* pane rx0 y0 rx1 y1 :filled t :ink background)
              (let ((x sx0)
                    (y (+ y0 (text-style-ascent (pane-text-style pane) pane)))
                    (el (elt (generic-list-pane-item-strings pane)
                             elt-index)))
                (if (gadget-active-p pane)
                    (draw-text* pane el x y
                                :ink foreground
                                :text-style (pane-text-style pane))
                    (progn
                      (draw-text* pane el (1+ x) (1+ y)
                                  :ink *3d-light-color*
                                  :text-style (pane-text-style pane))
                      (draw-text* pane el (1+ x) (1+ y)
                                  :ink *3d-dark-color*
                                  :text-style (pane-text-style pane))))))))))))

(defun generic-list-pane-select-item (pane item-value)
  "Toggle selection  of a single item in the generic-list-pane.
Returns :select or :deselect, depending on what action was performed."
  (if (list-pane-exclusive-p pane)
      (progn
        (setf (gadget-value pane :invoke-callback t) item-value)
        :select)
      (let ((member (member item-value (gadget-value pane) :test (list-pane-test pane))))
        (setf (gadget-value pane :invoke-callback t)
              (cond ((list-pane-exclusive-p pane)
                     (list item-value))
                    (member
                     (remove item-value (gadget-value pane)
                             :test (list-pane-test pane)))
                    ((not member) (cons item-value (gadget-value pane)))))
        (if member :deselect :select))))

(defun generic-list-pane-add-selected-items (pane item-values)
  "Add a set of items to the current selection"
  (when (not (list-pane-exclusive-p pane))
    (setf (gadget-value pane :invoke-callback t)
          (remove-duplicates (append item-values
                                     (gadget-value pane))
                             :test (list-pane-test pane)))))

(defun generic-list-pane-deselect-items (pane item-values)
  "Remove a set of items from the current selection"
  (when (not (list-pane-exclusive-p pane))
    (setf (gadget-value pane :invoke-callback t)
          (labels ((fun (item-values result)
                     (if (null item-values)
                         result
                         (fun (rest item-values)
                              (delete (first item-values) result
                                      :test (list-pane-test pane))))))
            (fun item-values (gadget-value pane))))))

(defun generic-list-pane-item-from-x-y (pane mx my)
  "Given a pointer event, determine what item in the pane it has fallen upon.
Returns two values, the item itself, and the index within the item list."
  (declare (ignore mx))
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1)  (sheet-region pane)
    (declare (ignorable sx0 sx1 sy1))
    (with-slots (items) pane
      (let* ((item-height (generic-list-pane-item-height pane))
             (number-of-items (generic-list-pane-items-length pane))
             (n (+ (items-origin pane) (floor (- my sy0) item-height)))
             (index (and (>= n 0)
                         (< n number-of-items)
                         n))
             (item-value (and index (elt (generic-list-pane-item-values pane) index))))
        (values item-value index)))))

(defun generic-list-pane-handle-click (pane x y modifier)
  (multiple-value-bind (item-value index)
      (generic-list-pane-item-from-x-y pane x y)
    (if (list-pane-exclusive-p pane)
        ;; Exclusive mode
        (when index
          (setf (slot-value pane 'last-action)
                (generic-list-pane-select-item pane item-value)))
        ;; Nonexclusive mode
        (when index
          (with-slots (last-index last-action items prefer-single-selection) pane
            (cond
              ;; Add single selection
              ((not (zerop (logand modifier +control-key+)))
               (setf last-action (generic-list-pane-select-item pane item-value)))
              ;; Maybe extend selection
              ((not (zerop (logand modifier +shift-key+)))
               (if (and (numberp last-index)
                        (not (null last-action)))
                   ;; Extend last selection
                   (funcall (if (eql last-action :select)
                                #'generic-list-pane-add-selected-items
                                #'generic-list-pane-deselect-items)
                            pane
                            (coerce (subseq (generic-list-pane-item-values pane)
                                            (min last-index index)
                                            (1+ (max last-index index))) 'list))
                   (setf last-action (generic-list-pane-select-item pane item-value))))
              ;; Toggle single item
              (t (if prefer-single-selection
                     (setf (gadget-value pane :invoke-callback t) (list item-value)
                           last-action :select)
                     (setf last-action (generic-list-pane-select-item pane item-value)))))
            (setf last-index index))))))

(defun generic-list-pane-handle-click-from-event (pane event)
  (multiple-value-bind (x y) (values (pointer-event-x event) (pointer-event-y event))
    (generic-list-pane-handle-click pane x y (event-modifier-state event))))

(defclass ad-hoc-presentation (standard-presentation) ())

(defmethod output-record-hit-detection-rectangle*
    ((presentation ad-hoc-presentation))
  (values most-negative-fixnum most-negative-fixnum
          most-positive-fixnum most-positive-fixnum))

(defun generic-list-pane-handle-right-click (pane event)
  (multiple-value-bind (x y)
      (values (pointer-event-x event) (pointer-event-y event))
    (multiple-value-bind (item-value index)
        (generic-list-pane-item-from-x-y pane x y)
      (declare (ignore item-value))
      (let* ((item (elt (list-pane-items pane) index)))
        (meta-list-pane-call-presentation-menu pane item)))))

(defun generic-list-pane-scroll (pane amount)
  (let ((new-origin (+ (items-origin pane) amount)))
    (when (and (>= new-origin 0)
               (<= (+ new-origin (visible-items pane))
                   (generic-list-pane-items-length pane)))
      (setf (items-origin pane) new-origin)
      (handle-repaint pane +everywhere+))))

(defun meta-list-pane-call-presentation-menu (pane item)
  (let ((ptype (funcall (list-pane-presentation-type-key pane) item)))
    (when ptype
      (let ((presentation
              (make-instance 'ad-hoc-presentation
                             :object (funcall (list-pane-value-key pane) item)
                             :single-box t
                             :type ptype)))
        (call-presentation-menu
         presentation
         *input-context*
         *application-frame*
         pane
         42 42
         :for-menu t
         :label (format nil "Operation on ~A" ptype))))))

(defmethod handle-event ((pane generic-list-pane) (event pointer-button-press-event))
  (case (pointer-event-button event)
    (#.+pointer-left-button+
     (generic-list-pane-handle-click-from-event pane event)
     (setf (slot-value pane 'armed) nil))
    (#.+pointer-right-button+
     (generic-list-pane-handle-right-click pane event))
    (t
     (when (next-method-p) (call-next-method)))))

(defmethod handle-event ((pane generic-list-pane) (event pointer-button-release-event))
  (if (eql (pointer-event-button event) +pointer-left-button+)
      (when (slot-value pane 'armed)
        (generic-list-pane-handle-click-from-event pane event))
      (when (next-method-p) (call-next-method))))

(defgeneric (setf list-pane-items)
    (newval pane &key invoke-callback)
  (:documentation
   "Set the current list of items for this list pane.
The current GADGET-VALUE will be adjusted by removing values not
specified by the new items.  VALUE-CHANGED-CALLBACK will be called
if INVOKE-CALLBACK is given."))

(defmethod (setf list-pane-items)
    (newval (pane meta-list-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (slot-value pane 'items) newval))

(defmethod (setf list-pane-items)
    :after
    (newval (pane meta-list-pane) &key invoke-callback)
  (when (slot-boundp pane 'value)
    (let ((new-values
            (coerce (climi::generic-list-pane-item-values pane) 'list))
          (test (list-pane-test pane)))
      (setf (gadget-value pane :invoke-callback invoke-callback)
            (if (list-pane-exclusive-p pane)
                (if (find (gadget-value pane) new-values :test test)
                    (gadget-value pane)
                    nil)
                (intersection (gadget-value pane) new-values :test test)))))
  (change-space-requirements pane))

(defmethod (setf list-pane-items)
    (newval (pane generic-list-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (call-next-method)
  (with-slots (items items-length item-strings item-values items-width) pane
    (setf items-length (length newval))
    (setf item-strings nil)
    (setf item-values nil)
    (setf items-width nil)))

(defmethod (setf list-pane-items) :after
    (newval (pane generic-list-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((space (compose-space pane)))
    (change-space-requirements
     pane
     :height (space-requirement-height space)
     :width (space-requirement-width space)))
  ;; the whole sheet region must be cleaned.
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1)
      (sheet-region pane)
    (draw-rectangle* pane sx0 sy0 sx1 sy1 :filled t :ink (pane-background pane)))
  (handle-repaint pane +everywhere+))

;;; OPTION-PANE

(define-abstract-pane-mapping 'option-pane 'generic-option-pane)

(defclass generic-option-pane (sheet-leaf-mixin
                               value-changed-repaint-mixin
                               activate/deactivate-repaint-mixin
                               arm/disarm-repaint-mixin
                               enter/exit-arms/disarms-mixin
                               3d-border-mixin
                               option-pane)
  ((current-label :initform "" :accessor generic-option-pane-label)))

(defun option-pane-evil-backward-map (pane value)
  (let ((key-fn (list-pane-value-key pane)))
    (if (eql key-fn #'identity)            ;; SANE CASE
        value
        (find value (list-pane-items pane) ;; INSANE CASE
              :key key-fn :test (list-pane-test pane)))))

(defun generic-option-pane-compute-label-from-value (pane value)
  (flet ((label (value) (funcall (list-pane-name-key pane) (option-pane-evil-backward-map pane value))))
    (if (list-pane-exclusive-p pane)
        (if (or value
                (member nil (list-pane-items pane) ;; Kludge in case NIL is part of the item set..
                        :key (list-pane-value-key pane)
                        :test (list-pane-test pane)))
            (label value)
            "")
        (cond ((= 0 (length value)) "")
              ((= 1 (length value)) (label (first value)))
              (t "...")))))

(defun generic-option-pane-compute-label-from-item (pane item)
  (funcall (list-pane-name-key pane) item))

(defun generic-option-pane-compute-label (pane)
  (generic-option-pane-compute-label-from-value pane (gadget-value pane)))

(defmethod initialize-instance :after ((object generic-option-pane) &rest rest)
  (declare (ignore rest))
  (setf (slot-value object 'current-label)
        (if (slot-boundp object 'value)
            (generic-option-pane-compute-label object)
            "")))

(defmethod (setf gadget-value) :after (new-value (gadget generic-option-pane) &key &allow-other-keys)
  (setf (slot-value gadget 'current-label)
        (generic-option-pane-compute-label-from-value gadget new-value)))

(defgeneric generic-option-pane-widget-size (pane))

(defmethod generic-option-pane-widget-size (pane)
  ;; We now always make the widget occupying a square.
  (let ((h (bounding-rectangle-height pane)))
    (values h h)))

(defun draw-engraved-vertical-separator (pane x y0 y1 highlight-color shadow-color)
  (draw-line* pane (1+ x) (1+ y0) (1+ x) (1- y1) :ink highlight-color)
  (draw-line* pane x y1 (1+ x) y1 :ink highlight-color)
  (draw-line* pane x (1+ y0) x (1- y1) :ink shadow-color)
  (draw-line* pane x y0 (1+ x) y0 :ink shadow-color))

(defun generic-option-pane-text-size (pane)
  (text-size (sheet-medium pane) (slot-value pane 'current-label)
             :text-style (pane-text-style pane)))

(defun draw-vertical-arrow (sheet x0 y0 direction)
  (assert (or (eq direction :up)
              (eq direction :down)))
  (let* ((dx -4)
         (dy 4)
         (shape
           (if (eq direction :up)  ;; Hack-p?
               (list x0 y0
                     (+ x0 dx) (+ 1 y0 dy)
                     (- x0 dx) (+ 1 y0 dy))
               (list x0 y0
                     (+ 1 x0 dx) (+ y0 (- dy))
                     (- x0 dx)   (+ y0 (- dy))))))
    (draw-polygon* sheet shape :ink +black+)))

(defun generic-option-pane-compute-max-label-width (pane)
  (max
   (reduce #'max
           (mapcar #'(lambda (value)
                       (text-size (sheet-medium pane)
                                  (generic-option-pane-compute-label-from-item pane value)
                                  :text-style (pane-text-style pane)))
                   (list-pane-items pane)))
   (text-size (sheet-medium pane) "..." :text-style (pane-text-style pane))))

(defmethod compose-space ((pane generic-option-pane) &key width height)
  (declare (ignore width height))
  (let* ((horizontal-padding 8)         ;### 2px border + 2px padding each side
         (vertical-padding   8)         ;### this should perhaps be computed from
                                        ;### border-width and spacing.
         (l-width  (generic-option-pane-compute-max-label-width pane))
         (l-height (text-style-height (pane-text-style pane) (sheet-medium pane)))
         (total-width (+ horizontal-padding l-width
                         ;; widget width
                         l-height
                         8))
         (total-height (+ vertical-padding l-height)))
    (make-space-requirement :min-width total-width
                            :width total-width
                            :max-width +fill+
                            :min-height total-height
                            :height total-height
                            :max-height +fill+)))

(defgeneric generic-option-pane-draw-widget (pane))

(defmethod generic-option-pane-draw-widget (pane)
  (with-bounding-rectangle* (x0 y0 x1 y1) pane
    (declare (ignore x0))
    (multiple-value-bind (widget-width widget-height)
        (generic-option-pane-widget-size pane)
      (let ((center (floor (/ (- y1 y0) 2)))
            (height/2 (/ widget-height 2))
            (highlight-color (compose-over (compose-in +white+ (make-opacity 0.85))
                                           (pane-background pane)))
            (shadow-color (compose-over (compose-in +black+ (make-opacity 0.3))
                                        (pane-background pane))))
        (draw-engraved-vertical-separator pane
                                          (- x1 widget-width -1)
                                          (- center height/2)
                                          (+ center height/2)
                                          highlight-color shadow-color)
        (let* ((x (+ (- x1 widget-width) (/ widget-width 2)))
               (frob-x (+ (floor x) 0)))
          (draw-vertical-arrow pane frob-x (- center 6) :up)
          (draw-vertical-arrow pane frob-x (+ center 6) :down))))))

(defun rewrite-event-for-grab (grabber event)
  (multiple-value-bind (nx ny)
      (multiple-value-call #'untransform-position
        (sheet-delta-transformation grabber nil) ;; assumes this is the graft's coordinate system..
        (values (pointer-event-native-graft-x event)
                (pointer-event-native-graft-y event)))
    (with-slots (sheet x y) event
      (setf sheet grabber
            x nx
            y ny)))
  event)

(defun popup-compute-spaces (pane graft)
  (with-bounding-rectangle* (x0 top x1 bottom) (sheet-region pane)
    (multiple-value-call #'(lambda (x0 top x1 bottom)
                             (declare (ignore x0 x1))
                             (values (max 0 (1- top))
                                     (max 0 (- (graft-height graft) bottom))
                                     top
                                     bottom))
      (transform-position (sheet-delta-transformation pane nil) x0 top)  ;; XXX (see above)
      (transform-position (sheet-delta-transformation pane nil) x1 bottom))))

(defun popup-compute-height (parent-pane child-pane)
  "Decides whether to place the child-pane above or below the parent-pane, and
 how to do so. Returns three values: First T if the pane is too large to fit on
 the screen, otherwise NIL. Second, whether to place the child-pane above or
 below parent-pane. Third, the height which the popup should be constrained to
 if the first value is true."
  (let* ((sr (compose-space child-pane))
         (height (space-requirement-min-height sr)))
    (multiple-value-bind (top-space bottom-space)
        (popup-compute-spaces parent-pane (graft parent-pane))
      (let ((polite-ts (* 0.8 top-space))
            (polite-bs (* 0.8 bottom-space)))
        (cond ((and (<= polite-ts height)
                    (<= polite-bs height))
               (multiple-value-call #'values t
                 (if (> top-space bottom-space)
                     (values :above (* 0.7 top-space))
                     (values :below (* 0.7 bottom-space)))))
              ((> polite-bs height) (values nil :below height))
              (t (values nil :above height)))))))

(defun popup-init (parent manager frame)
  (let ((list-pane (apply #'make-pane-1 manager frame 'generic-list-pane
                          :items (list-pane-items parent)
                          :mode  (list-pane-mode parent)
                          :name-key (list-pane-name-key parent)
                          :value-key (list-pane-value-key parent)
                          :test (list-pane-test parent)
                          (and (slot-boundp parent 'value)
                               (list :value (gadget-value parent))))))
    (multiple-value-bind (scroll-p position height)
        (popup-compute-height parent list-pane)
      (with-bounding-rectangle* (cx0 cy0 cx1 cy1) parent
        (multiple-value-bind (x0 y0 x1 y1)
            (multiple-value-call #'values
              (transform-position (sheet-delta-transformation parent nil) cx0 cy0)
              (transform-position (sheet-delta-transformation parent nil) cx1 cy1))
          ;; Note: This :suggested-width/height business is really a silly hack
          ;;       which I could have easily worked around without adding kludges
          ;;       to the scroller-pane..
          (let* ((topmost-pane (if scroll-p
                                   ;; TODO investigate whether suggested-* arguments actually have an effect
                                   (scrolling (:scroll-bar :vertical
                                               :suggested-height height
                                               :suggested-width (if scroll-p (+ 30 (bounding-rectangle-width list-pane))))
                                     list-pane)
                                   list-pane))
                 (topmost-pane    (outlining (:thickness 1) topmost-pane))
                 (composed-height (space-requirement-height (compose-space topmost-pane :width (- x1 x0) :height height)))
                 (menu-frame      (make-menu-frame topmost-pane
                                                   :min-width (bounding-rectangle-width parent)
                                                   :left x0
                                                   :top (if (eq position :below)
                                                            y1
                                                            (- y0 composed-height 1)))))
            (values list-pane topmost-pane menu-frame)))))))

(defun popup-list-box (parent)
  (let* ((frame *application-frame*)
         (manager (frame-manager frame))
         ;; Popup state
         (final-change nil) ;; Menu should exit after next value change
         (inner-grab nil)    ;; Gadget is grabbing the pointer, used to simulate
         ;; X implicit pointer grabbing (for the scrollbar)
         (retain-value nil)
         (consume-and-exit nil) ;; If true, wait until a button release then exit
         (last-click-time nil)
         (last-item-index nil))
    (with-look-and-feel-realization (manager *application-frame*)
      (multiple-value-bind (list-pane topmost-pane menu-frame)
          (popup-init parent manager frame)
        (setf (slot-value list-pane 'armed) t)
        (adopt-frame manager menu-frame)
        (enable-frame menu-frame)
        (labels ((in-window (window child x y)
                   (and window
                        (sheet-ancestor-p child window)
                        (multiple-value-call #'region-contains-position-p
                          (sheet-region window)
                          (transform-position (sheet-delta-transformation child window) x y))))
                 (in-list (window x y)
                   (in-window list-pane window x y))
                 (in-menu (window x y)
                   (in-window topmost-pane window x y))
                 (compute-double-clicked ()
                   (let* ((now (get-internal-real-time)))
                     (prog1 (and last-click-time
                                 (< (/ (- now last-click-time) internal-time-units-per-second) *double-click-delay*))
                       (setf last-click-time now))))
                 (end-it ()
                   (throw 'popup-list-box-done nil)))

          (catch 'popup-list-box-done
            (setf (slot-value list-pane 'value-changed-callback)
                  (lambda (pane value)
                    (declare (ignore pane value))
                    (when (and final-change
                               (not consume-and-exit))
                      (end-it))))

            (tracking-pointer (list-pane :multiple-window t :highlight nil)
              (:pointer-motion (&key event window x y)
                               (cond (inner-grab (handle-event inner-grab (rewrite-event-for-grab inner-grab event)))
                                     ((and (list-pane-exclusive-p parent)
                                           (in-list window x y))
                                      (generic-list-pane-handle-click list-pane x y 0))
                                     ((in-menu window x y)
                                      (handle-event window event))))

              (:pointer-button-press (&key event x y)
                                     (if inner-grab
                                         (handle-event inner-grab event)
                                         (cond ((in-list (event-sheet event) x y)
                                                (multiple-value-bind (item current-index)
                                                    (generic-list-pane-item-from-x-y list-pane x y)
                                                  (declare (ignore item))
                                                  (let ((double-clicked (and (compute-double-clicked)
                                                                             (= (or last-item-index -1)
                                                                                (or current-index -2))))
                                                        (exclusive (list-pane-exclusive-p parent)))
                                                    (setf retain-value t
                                                          final-change   (or exclusive double-clicked)
                                                          last-item-index current-index
                                                          consume-and-exit (or exclusive
                                                                               (and (not exclusive)
                                                                                    double-clicked)))
                                                    (unless (and (not exclusive)
                                                                 double-clicked)
                                                      (handle-event list-pane event)))))
                                               ((in-menu (event-sheet event) x  y)
                                                (handle-event (event-sheet event) event)
                                                (setf inner-grab (event-sheet event)))
                                               (t (setf consume-and-exit t)))))

              (:pointer-button-release (&key event x y)
                                       (when consume-and-exit (end-it))
                                       (cond (inner-grab
                                              (handle-event inner-grab event)
                                              (setf inner-grab nil))
                                             ((in-list (event-sheet event) x y)
                                              (when (list-pane-exclusive-p parent)
                                                (setf retain-value t
                                                      final-change t)
                                                (handle-event list-pane event)))
                                             ((in-menu (event-sheet event) x y)
                                              (handle-event (event-sheet event) event)))))))
        ;; Cleanup and exit
        (when retain-value
          (setf (gadget-value parent :invoke-callback t)
                (gadget-value list-pane)))
        (disown-frame manager menu-frame)))))

(defmethod handle-event ((pane generic-option-pane) (event pointer-button-press-event))
  (popup-list-box pane)
  (disarm-gadget pane))

(defmethod handle-repaint ((pane generic-option-pane) region)
  (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
    (multiple-value-bind (widget-width widget-height)
        (generic-option-pane-widget-size pane)
      (declare (ignore widget-height))
      (draw-rectangle* pane x0 y0 x1 y1 :ink (effective-gadget-background pane))
      (let* ((tx1 (- x1 widget-width))
             (x (/ (- tx1 x0) 2))
             (y (/ (+ (- y1 y0)
                      (- (text-style-ascent (pane-text-style pane) pane)
                         (text-style-descent (pane-text-style pane) pane)))
                   2)))
        (if (gadget-active-p pane)
            (draw-text* pane (slot-value pane 'current-label)
                        x y
                        :align-x :center
                        :align-y :baseline)
            (progn
              (draw-text* pane (slot-value pane 'current-label)
                          (1+ x) (1+ y)
                          :align-x :center
                          :align-y :baseline
                          :ink *3d-light-color*)
              (draw-text* pane (slot-value pane 'current-label)
                          x y
                          :align-x :center
                          :align-y :baseline
                          :ink *3d-dark-color*))))
      (generic-option-pane-draw-widget pane))))


;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  30.5 Integrating Gadgets and Output Records
;;;;

;;
;; GADGET-OUTPUT-RECORD
;;

(defclass gadget-output-record (basic-output-record displayed-output-record)
  ((gadget :initarg :gadget :accessor gadget)))

(defmethod initialize-instance :after ((record gadget-output-record) &key x y)
  (setf (output-record-position record) (values x y)))

(defmethod note-output-record-got-sheet ((record gadget-output-record) sheet)
  (multiple-value-bind (x y)  (output-record-position record)
    (sheet-adopt-child sheet (gadget record))
    (allocate-space (gadget record)
                    (rectangle-width record)
                    (rectangle-height record))
    (multiple-value-bind (gx gy)
        (transform-position (sheet-transformation (gadget record)) 0 0)
      (unless (and (= x gx)
                   (= y gy))
        (move-sheet (gadget record) x y)))))

(defmethod note-output-record-lost-sheet ((record gadget-output-record) sheet)
  (sheet-disown-child sheet (gadget record)))

;; This is as good a place as any other to handle moving the position of the
;; gadget if the output record has moved. This is consistent with other
;; operations on output records that force you to manage repainting manually.
(defmethod replay-output-record  ((record gadget-output-record) stream
                                  &optional region x-offset y-offset)
  (declare (ignorable stream region x-offset y-offset))
  (multiple-value-bind (gx gy)
      (transform-position (sheet-transformation (gadget record)) 0 0)
    (multiple-value-bind (ox oy)
        (output-record-position record)
      (if (not (and (= ox gx)
                    (= oy gy)))
          (move-sheet (gadget record) ox oy)
          (repaint-sheet (gadget record) region)))))

(defun setup-gadget-record (sheet record)
  (let* ((child (gadget record))
         (sr (compose-space child))
         (width  (space-requirement-width sr))
         (height (space-requirement-height sr)))
    (multiple-value-bind (x y) (output-record-position record)
      (setf (rectangle-edges* record) (values x y (+ x width) (+ y height)))
      (when t ; :move-cursor t
        ;; Almost like LWW, except baseline of text should align with bottom
        ;; of gadget? FIXME
        (setf (stream-cursor-position sheet)
              (values (+ x (bounding-rectangle-width record))
                      (+ y (bounding-rectangle-height record))))))))

;; The CLIM 2.0 spec does not really say what this macro should return.
;; Existing code written for "Real CLIM" assumes it returns the gadget pane
;; object. I think returning the gadget-output-record would be more useful.
;; For compatibility I'm having it return (values GADGET GADGET-OUTPUT-RECORD)

(defmacro with-output-as-gadget ((stream &rest options) &body body)
  ;; NOTE - incremental-redisplay 12/28/05 will call this on redisplay
  ;; unless wrapped in (updating-output (stream :cache-value t) ...)
  ;; Otherwise, new gadget-output-records are generated but only the first
  ;; gadget is ever adopted, and an erase-output-record called on a newer
  ;; gadget-output-record will face a sheet-not-child error when trying
  ;; to disown the never adopted gadget.
  (setf stream (stream-designator-symbol stream '*standard-output*))
  (let ((gadget-output-record (gensym))
        (x (gensym))
        (y (gensym)))
    `(multiple-value-bind (,x ,y) (stream-cursor-position ,stream)
       (flet ((with-output-as-gadget-continuation (,stream record)
                (flet ((with-output-as-gadget-body (,stream)
                         (declare (ignorable ,stream))
                         (progn ,@body)))
                  (setf (gadget record)
                        (with-output-as-gadget-body ,stream)))))
         (declare (dynamic-extent #'with-output-as-gadget-continuation))
         (let ((,gadget-output-record
                 (invoke-with-output-to-output-record
                  ,stream
                  #'with-output-as-gadget-continuation
                  'gadget-output-record ,@options :x ,x :y ,y)))
           (setup-gadget-record ,stream ,gadget-output-record)
           (stream-add-output-record ,stream ,gadget-output-record)
           (values (gadget ,gadget-output-record) ,gadget-output-record))))))
;;;

(defclass orientation-from-parent-mixin () ())

(defgeneric orientation (gadget)
  (:method ((gadget orientation-from-parent-mixin))
    (etypecase (sheet-parent gadget)
      ((or hbox-pane hrack-pane) :vertical)
      ((or vbox-pane vrack-pane) :horizontal))))

(cl:defconstant +box-adjuster-gadget-major-size+ 6)
(cl:defconstant +box-adjuster-gadget-minor-size+ 1)

(defclass box-adjuster-gadget-dragging-state ()
  ((%left         :initarg :left
                  :reader  dragging-state-left)
   (%right        :initarg :right
                  :reader  dragging-state-right)
   (%clients-size :initarg :clients-size
                  :reader  dragging-state-clients-size)
   (%total-size   :initarg :total-size
                  :reader  dragging-state-total-size)))

(defclass box-adjuster-gadget (sheet-leaf-mixin
                               3d-border-mixin
                               orientation-from-parent-mixin
                               basic-gadget)
  ((dragging-state :initform nil
                   :accessor dragging-state))
  (:default-initargs :background *3d-inner-color*)
  (:documentation "The box-adjuster-gadget allows users to resize the panes
in a layout by dragging the boundary between the panes.  To use it, insert
it in a layout between two panes that are to be resizeable.  E.g.:
 (vertically ()
    top-pane
    (make-pane 'clim-extensions:box-adjuster-gadget)
    bottom-pane)"))

(defmethod compose-space ((gadget clim-extensions:box-adjuster-gadget)
                          &key width height)
  (declare (ignore width height))
  (let ((major-size +box-adjuster-gadget-major-size+)
        (minor-size +box-adjuster-gadget-minor-size+))
    (ecase (orientation gadget)
      (:vertical
       (make-space-requirement :min-width major-size :width major-size :max-width major-size
                               :min-height minor-size :height minor-size))
      (:horizontal
       (make-space-requirement :min-height major-size :height major-size :max-height major-size
                               :min-width minor-size :width minor-size)))))

(defmethod note-sheet-grafted ((sheet box-adjuster-gadget))
  (setf (sheet-pointer-cursor sheet) :move))

(defmethod handle-event ((gadget box-adjuster-gadget)
                         (event pointer-button-press-event))
  (let ((orientation (orientation gadget))
        (parent (sheet-parent gadget)))
    (multiple-value-bind (left right)
        (loop for (first second third) on (box-layout-mixin-clients parent)
              when (eq gadget (box-client-pane second))
                do (return (values first third)))
      (labels ((sheet-size (sheet)
                 (ecase orientation
                   (:vertical (bounding-rectangle-width sheet))
                   (:horizontal (bounding-rectangle-height sheet)))))
        (let ((clients-size (+ (sheet-size (box-client-pane left))
                               (sheet-size (box-client-pane right))))
              (total-size (sheet-size parent)))
          (setf (dragging-state gadget)
                (make-instance 'box-adjuster-gadget-dragging-state
                               :left         left
                               :right        right
                               :clients-size clients-size
                               :total-size   total-size)))))))

(defmethod handle-event ((gadget box-adjuster-gadget)
                         (event pointer-button-release-event))
  (setf (dragging-state gadget) nil))

(defmethod handle-event ((gadget box-adjuster-gadget)
                         (event pointer-motion-event))
  (when-let ((state (dragging-state gadget)))
    (let ((orientation (orientation gadget))
          (parent (sheet-parent gadget))
          (left (dragging-state-left state))
          (right (dragging-state-right state))
          (clients-size (dragging-state-clients-size state))
          (total-size (dragging-state-total-size state)))
      ;; Compute cursor position in coordinate system of left
      ;; child. Dividing by the combined size of left and right gives
      ;; the ratio the user is going for.
      (flet ((left-pointer-position ()
               (untransform-position
                (sheet-delta-transformation
                 (sheet-mirrored-ancestor (box-client-pane left)) nil)
                (climi::pointer-event-native-graft-x event)
                (climi::pointer-event-native-graft-y event))))
        (let ((position (- (ecase orientation
                             (:vertical (left-pointer-position))
                             (:horizontal (nth-value 1 (left-pointer-position))))
                           ;; Make GADGET's center track the cursor,
                           ;; not its edge.
                           (/ +box-adjuster-gadget-major-size+ 2))))
          (update-box-clients
           left right position clients-size total-size)
          (changing-space-requirements (:resize-frame nil)
            (change-space-requirements parent)))))))

(defun update-box-clients (left right position clients-size total-size)
  (let* ((as-fixed (clamp position 0 clients-size))
         (as-proportion (/ as-fixed clients-size))
         (non-fill-p nil))
    (flet ((update-client (client as-fixed as-proportion
                           &optional (can-fill-p t))
             (cond ((and (box-client-fillp client) can-fill-p))
                   ((box-client-fixed-size client)
                    (setf (box-client-fixed-size client) as-fixed
                          non-fill-p                     t))
                   (t ; proportional, fill with CAN-FILL-P false or none
                    ;; Translate proportion from size of left and
                    ;; right to parent's size (needed when the parent
                    ;; has more than two children).
                    (let ((proportion (* as-proportion
                                         (/ clients-size total-size))))
                      (setf (box-client-fillp client)      nil
                            (box-client-proportion client) proportion
                            non-fill-p                     t))))))
      ;; Adjust both clients, making sure at least one does not use fill.
      (update-client left  as-fixed                as-proportion       t)
      (update-client right (- total-size as-fixed) (- 1 as-proportion) non-fill-p))))

;;; Support for definition of callbacks and associated callback events. A
;;; callback event is used by a backend when a high-level notification of a
;;; gadget state change is delivered in the CLIM event process -- by a native
;;; gadget, for example -- and must be delivered in the application process.

(define-event-class callback-event (standard-event)
  ((sheet :initarg :gadget :reader event-gadget
          :documentation "An alias for sheet, for readability")
   (callback-function :initarg :callback-function :reader callback-function)
   (client :initarg :client :reader event-client)
   (client-id :initarg :client-id :reader event-client-id)
   (other-args :initarg :other-args :reader event-other-args :initform nil)))

(defun queue-callback (fn gadget client client-id &rest other-args)
  (queue-event gadget (make-instance 'callback-event
                                     :callback-function fn
                                     :gadget gadget
                                     :client client
                                     :client-id client-id
                                     :other-args other-args)))

(defmethod handle-event ((gadget basic-gadget) (event callback-event))
  (apply (callback-function event)
         (event-gadget event)
         (event-client event)
         (event-client-id event)
         (event-other-args event)))
