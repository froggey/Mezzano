;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000, 2014 by
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

(in-package :clim-internals)

(defgeneric menu-root (button))

(defgeneric menu-children (pane))

(defgeneric arm-menu (button))

(defgeneric disarm-menu (button))

(defgeneric arm-branch (pane))

(defgeneric destroy-substructure (pane))

(defmethod stream-force-output ((pane menu-button-pane))
  (with-sheet-medium (medium pane)
    (medium-force-output medium)))

(defmethod menu-root ((button menu-button-pane))
  (menu-root (gadget-client button)))

(defmethod arm-menu ((button menu-button-pane))
  (with-slots (client armed) button
    (unless armed
      (arm-menu client)
      (mapc #'disarm-menu (menu-children client))
      (arm-gadget button))
    (dispatch-repaint button (sheet-region button))))

(defmethod disarm-menu ((button menu-button-pane))
  (with-slots (armed) button
    (when armed
      (disarm-gadget button)
      (dispatch-repaint button (sheet-region button))
      (stream-force-output button))))

(defmethod handle-event ((pane menu-button-pane) (event pointer-enter-event))
  (when (slot-value (gadget-client pane) 'armed)
    (arm-branch pane)))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-press-event))
  (arm-branch pane))

;;; menu-button-leaf-pane

(defclass menu-button-leaf-pane (menu-button-pane)
  ((command :initform nil :initarg :command)))

(defmethod arm-branch ((button menu-button-leaf-pane))
  (with-slots (client) button
    (arm-menu client)
    (mapc #'destroy-substructure (menu-children client))
    (arm-menu button)))

(defmethod destroy-substructure ((button menu-button-leaf-pane))
  (disarm-gadget button))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-button-release-event))
  (with-slots (armed label client id) pane
    (unwind-protect
         (when armed
           (value-changed-callback pane client id label))
      (disarm-menu pane)
      (destroy-substructure (menu-root pane)))))

(defmethod handle-event ((pane menu-button-leaf-pane) (event pointer-exit-event))
  (disarm-menu pane))

;;; menu-button-submenu-pane

(defclass menu-button-submenu-pane (menu-button-pane)
  ((frame-manager :initform nil :initarg :frame-manager)
   (submenu-frame :initform nil)
   (bottomp :initform nil :initarg :bottomp)
   (command-table :initform nil :initarg :command-table)))

(defmethod menu-children ((submenu menu-button-submenu-pane))
  (with-slots (submenu-frame) submenu
    (if submenu-frame
        (sheet-children (first (sheet-children (frame-panes submenu-frame))))
        '())))

(defclass submenu-border (border-pane) ())

(defclass submenu-border-pane (raised-pane)
  ()
  (:default-initargs :border-width 2 :background *3d-normal-color*))

(defun make-menu-buttons (command-table-name client)
  "Map over the available menu items in the command table with
name `command-table-name', taking inherited menu items into
account, and create a list of menu buttons."
  (let ((menu-buttons '()))
    (map-over-command-table-menu-items
     (lambda (name gesture item)
       (declare (ignore name gesture))
       (push (make-menu-button-from-menu-item
              item client :command-table command-table-name :vertical t)
             menu-buttons))
     command-table-name)
    (nreverse menu-buttons)))

(defun create-substructure (sub-menu client)
  (let* ((frame (pane-frame client))
         (manager (frame-manager frame))
         (command-table-name (slot-value sub-menu 'command-table))
         (items (make-menu-buttons command-table-name client))
         (rack (make-pane-1 manager frame 'vrack-pane
                            :background *3d-normal-color* :contents items))
         (raised (make-pane-1 manager frame 'submenu-border :contents (list rack))))
    (with-slots (bottomp) sub-menu
      (with-bounding-rectangle* (xmin ymin xmax ymax) (sheet-region sub-menu)
        (multiple-value-bind (x y)
            (transform-position (sheet-delta-transformation sub-menu nil)
                                (if bottomp xmin xmax)
                                (if bottomp ymax ymin))
          (with-slots (frame-manager submenu-frame) sub-menu
            (setf frame-manager manager
                  submenu-frame (make-menu-frame raised :left x :top y))
            (adopt-frame manager submenu-frame)
            (enable-frame submenu-frame)
            (with-sheet-medium (medium raised)
              (medium-force-output medium))))))))

(defmethod destroy-substructure ((sub-menu menu-button-submenu-pane))
  (with-slots (frame-manager submenu-frame) sub-menu
    (when submenu-frame
      (mapc #'destroy-substructure (menu-children sub-menu))
      (disown-frame frame-manager submenu-frame)
      (disarm-gadget sub-menu)
      (dispatch-repaint sub-menu +everywhere+)
      (setf submenu-frame nil))))

(defmethod arm-branch ((sub-menu menu-button-submenu-pane))
  (with-slots (client frame-manager submenu-frame) sub-menu
    (arm-menu client)
    (if submenu-frame
        (progn (mapc #'destroy-substructure (menu-children sub-menu))
               (mapc #'disarm-menu (menu-children sub-menu)))
        (progn
          (mapc #'destroy-substructure (menu-children client))
          (create-substructure sub-menu sub-menu)))
    (arm-menu sub-menu)))

(defmethod handle-event ((pane menu-button-submenu-pane) (event pointer-button-release-event))
  (let ((pointer-sheet (port-pointer-sheet (port pane))))
    (unless (and (not (eq pane pointer-sheet))
                 (typep pointer-sheet 'menu-button-pane)
                 (gadget-active-p pointer-sheet)
                 (eq (menu-root pointer-sheet) (menu-root pane)))
      (destroy-substructure (menu-root pane)))))

;;; menu-button-vertical-submenu-pane
(defclass menu-button-vertical-submenu-pane (menu-button-submenu-pane) ())

(let* ((left-padding 10)
       (widget-size  5)
       (right-padding 4)
       (widget-width widget-size)
       (widget-height (* 2 widget-size))
       (total-width (+ left-padding widget-width right-padding))
       (total-height widget-height))

  (defmethod compose-space ((gadget menu-button-vertical-submenu-pane) &key width height)
    (declare (ignorable width height))
    (multiple-value-bind (width min-width max-width height min-height max-height)
        (space-requirement-components (call-next-method))
      (declare (ignorable max-width))
      (make-space-requirement :min-width (+ min-width total-width)
                              :width (+ width total-width)
                              :max-width +fill+
                              :min-height (max min-height total-height)
                              :height (max height total-height)
                              :max-height (if (zerop max-height) ; make-space-requirements default maximums are zero..
                                              0
                                              (max max-height total-height)))))

  (defmethod handle-repaint ((pane menu-button-vertical-submenu-pane) region)
    (call-next-method)
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (when (and (> (- x2 x1) total-width)
                 (> (- y2 y1) total-height))
        (let* ((center (/ (+ y1 y2) 2))
               (vbase (- center (/ widget-height 2)))
               (hbase (+ (- x2 total-width) left-padding))
               (shape (list hbase vbase
                            (+ hbase widget-size) (+ vbase widget-size)
                            hbase (+ vbase (* 2 widget-size)))))
          (draw-polygon* pane shape :ink +black+))))))

;;; menu-divider-leaf-pane

(defclass menu-divider-leaf-pane (sheet-leaf-mixin basic-gadget)
  ((label :initform nil :initarg :label)))

(defparameter *labelled-divider-text-style* (make-text-style :sans-serif :roman :small))

(defmethod destroy-substructure ((object menu-divider-leaf-pane)))
(defmethod arm-menu ((object menu-divider-leaf-pane)))
(defmethod disarm-menu ((object menu-divider-leaf-pane)))

(defmethod compose-space ((gadget menu-divider-leaf-pane) &key width height)
  (declare (ignorable width height))
  (multiple-value-bind (width height)
      (if-let ((label (slot-value gadget 'label)))
        (text-size gadget label :text-style *labelled-divider-text-style*)
        (values 0 4))
    (make-space-requirement
     :min-width width :width width
     :min-height height :height height :max-height height)))

(defmethod handle-repaint ((pane menu-divider-leaf-pane) region)
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    (declare (ignore y2))
    (if-let ((label (slot-value pane 'label)))
      (multiple-value-bind (width height fx fy baseline)
          (text-size pane label :text-style *labelled-divider-text-style*)
        (declare (ignore height fx fy))
        (let ((tx0 (+ x1 (/ (- (- x2 x1) width) 2)))
              (ty0 (+ 1 y1 baseline)))
          (draw-line* pane tx0 (1+ ty0) (+ tx0 width) (1+ ty0) :ink *3d-dark-color*)
          (draw-text* pane label tx0 ty0
                      :text-style *labelled-divider-text-style*)))
      (progn
        (draw-line* pane x1 (1+ y1) x2 (1+ y1) :ink *3d-dark-color*)
        (draw-line* pane x1 (+ 2 y1) x2 (+ 2 y1) :ink *3d-light-color*)))))


;;; Menu creation from command tables

(defun make-menu-button-from-menu-item (item client
                                        &key (bottomp nil)
                                             (vertical nil)
                                             command-table
                                             (presentation-type 'menu-item))
  (declare (ignore command-table))
  (let* ((name (command-menu-item-name item))
         (type (command-menu-item-type item))
         (value (command-menu-item-value item))
         (text-style (command-menu-item-text-style item))
         (frame (pane-frame client))
         (manager (frame-manager frame)))
    (flet ((make-sub-pane (class &rest initargs &key &allow-other-keys)
             (apply #'make-pane-1 manager frame class
                    :label name :client client :text-style text-style
                    initargs)))
      (case type
        (:command
         (let ((command-name (if (consp value) (car value) value)))
           (if (command-enabled command-name frame)
               (make-sub-pane 'menu-button-leaf-pane
                              :value-changed-callback
                              (lambda (gadget val)
                                (declare (ignore gadget val))
                                (throw-object-ptype item presentation-type)))
               (let ((pane (make-sub-pane 'menu-button-leaf-pane
                                          :value-changed-callback
                                          (lambda (gadget val)
                                            (declare (ignore gadget val))
                                            nil))))
                 (deactivate-gadget pane)
                 pane))))
        (:function
         (make-sub-pane 'menu-button-leaf-pane
                        :value-changed-callback
                        (lambda (gadget val)
                          (declare (ignore gadget val))
                          ;; FIXME: the spec requires us to pass a gesture to the
                          ;; function, but value-changed-callback doesn't provide
                          ;; one, so we pass NIL for now.
                          ;; FIXME: We don't have a numeric argument, either.
                          (let ((command (funcall value nil nil)))
                            (throw-object-ptype command 'command)))))
        (:divider
         (make-sub-pane 'menu-divider-leaf-pane))
        (:menu
         (make-sub-pane (if vertical
                            'menu-button-vertical-submenu-pane
                            'menu-button-submenu-pane)
                        :frame-manager manager
                        :command-table value
                        :bottomp bottomp))
        (otherwise (error "Don't know how to create a menu button for ~W" type))))))

;;
;; MENU-BAR
;;

(defclass menu-bar (hrack-pane)
  ((items :initform nil)
   (armed :initform nil)))

(defmethod (setf %pane-contents) :after (contents (pane menu-bar))
  (declare (ignore contents))
  (setf (slot-value pane 'items) (copy-list (sheet-children pane))))

(defmethod menu-children ((menu-bar menu-bar))
  (slot-value menu-bar 'items))

(defmethod menu-root ((object menu-bar))
  object)

(defmethod destroy-substructure ((object menu-bar))
  (loop for child in (menu-children object)
        do (destroy-substructure child)
           (dispatch-repaint child (sheet-region child)))
  (setf (slot-value object 'armed) nil))

(defmethod arm-menu ((object menu-bar))
  (setf (slot-value object 'armed) t))

(defmethod disarm-menu ((object menu-bar))
  (setf (slot-value object 'armed) nil))

(defun %make-menu-contents (command-table client)
  (with-slots (menu) (find-command-table command-table)
    (append (loop for item in menu
                  collect (make-menu-button-from-menu-item
                           item client
                           :bottomp t
                           :vertical nil
                           :command-table command-table))
            (list +fill+))))

(defun make-menu-bar (command-table
                      &key width height
                           (max-width +fill+) max-height
                           min-width min-height)
  (let ((menu-bar (make-pane-1 *pane-realizer* *application-frame* 'menu-bar
                               :background *3d-normal-color*
                               :width width :height height
                               :max-width max-width :max-height max-height
                               :min-width min-width :min-height min-height)))
    (setf (%pane-contents menu-bar)
          (%make-menu-contents command-table menu-bar))
    menu-bar))

(defun update-menu-bar (menu-bar-pane command-table)
  (when command-table
    (setf (%pane-contents menu-bar-pane)
          (%make-menu-contents command-table menu-bar-pane)))
  (change-space-requirements menu-bar-pane))

(defmethod handle-repaint ((pane menu-bar) region)
  (declare (ignore region))
  (with-slots (border-width) pane
    (multiple-value-call #'draw-bordered-rectangle*
      pane
      (bounding-rectangle* (sheet-region pane))
      :style :outset
      :border-width 2)))

(defmethod compose-space ((pane menu-bar) &key width height)
  (declare (ignore width height))
  (space-requirement+
   (call-next-method)
   (make-space-requirement :height 4 :max-height 4 :min-height 4)))

(defmethod box-layout-mixin/horizontally-allocate-space
    ((pane menu-bar) real-width real-height)
  (with-slots (x-spacing) pane
    (let ((widths
            (box-layout-mixin/horizontally-allocate-space-aux*
             pane real-width real-height))
          (x 2))
      (loop for child in (box-layout-mixin-clients pane)
            for width in widths
            do (when (box-client-pane child)
                 (layout-child (box-client-pane child)
                               :expand
                               :expand
                               x
                               2
                               width
                               (- real-height 4)))
               (incf x width)
               (incf x x-spacing)))))

(defmethod display-command-table-menu ((command-table standard-command-table)
                                       (stream fundamental-output-stream)
                                       &rest args
                                       &key max-width max-height n-rows n-columns
                                       x-spacing y-spacing initial-spacing
                                       row-wise (cell-align-x :left)
                                       (cell-align-y :top) (move-cursor t))
  (formatting-item-list (stream :max-width max-width :max-height max-height :n-rows n-rows
                                :n-columns n-columns :x-spacing x-spacing :y-spacing y-spacing
                                :initial-spacing initial-spacing :row-wise row-wise
                                :move-cursor move-cursor)
    (map-over-command-table-menu-items
     #'(lambda (item-name accelerator item)
         (declare (ignore accelerator))
         (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
           (cond ((eq (command-menu-item-type item) :menu)
                  (with-text-style (stream (make-text-style :serif '(:bold :italic) nil))
                    (write-string item-name stream)
                    (terpri stream))
                  (surrounding-output-with-border (stream)
                    (apply #'display-command-table-menu
                           (find-command-table (command-menu-item-value item))
                           stream args)))
                 ((eq (command-menu-item-type item) :command)
                  (let ((name (command-menu-item-name item)))
                    (with-output-as-presentation (stream (command-menu-item-value item) 'command)
                      (write-string name stream)))))))
     command-table)))

(defmethod display-command-menu (frame (stream fundamental-output-stream)
                                 &rest args &key
                                 (command-table (frame-command-table frame))
                                 initial-spacing row-wise max-width
                                 max-height n-rows n-columns
                                 (cell-align-x :left) (cell-align-y :top))
  (declare (ignore initial-spacing row-wise max-width max-height
                   n-rows n-columns cell-align-x cell-align-y))
  (with-keywords-removed (args (:command-table))
    (apply #'display-command-table-menu command-table stream args)))
