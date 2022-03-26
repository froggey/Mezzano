;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)

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

;;; Long time TODO (if someone wants to implement them - you are welcome):
;;;
;;; - Menu item options: :items, :type.
;;;
;;; - VIEW.
;;;
;;; - Caching.
;;;
;;; - Default item.

;;; Mid time TODO:
;;;
;;; - Documentation.
;;;
;;; - Empty menu.
;;;
;;; - :DIVIDER type menu items.

(in-package :clim-internals)

;; Spec function.
(defgeneric menu-choose
    (items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

;; Spec function.
(defgeneric frame-manager-menu-choose
    (frame-manager items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

;; Spec function.
(defgeneric menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation))

(defgeneric adjust-menu-size-and-position (menu &key x-position y-position)
  (:documentation "Adjust the size of the menu so it fits
  properly on the screen with regards to the menu entries. `menu'
  should be the menu pane. This is an internal,
  non-specification-defined function."))

(defun menu-item-value (menu-item)
  (cond ((atom menu-item)
         menu-item)
        ((atom (cdr menu-item))
         (cdr menu-item))
        (t (getf (cdr menu-item) :value (car menu-item)))))

(defun menu-item-display (menu-item)
  (if (atom menu-item)
      menu-item
      (car menu-item)))

(defun menu-item-options (menu-item)
  (if (and (consp menu-item)
           (consp (cdr menu-item)))
      (cdr menu-item) ; XXX Remove :VALUE?
      nil))

(defun menu-item-option (menu-item option &optional default)
  (if (listp menu-item)
      (getf (menu-item-options menu-item) option default)
      default))

(defun print-menu-item (menu-item &optional (stream *standard-output*))
  (let ((style (getf (menu-item-options menu-item) :style '(nil nil nil))))
    (with-text-style (stream style)
      (if (menu-item-option menu-item :active t)
          (princ (menu-item-display menu-item) stream)
          (with-drawing-options (stream :ink (compose-over
                                                (compose-in
                                                  ; XXX it should be (MEDIUM-INK),
                                                  ; but CLX backend is too stupid.
                                                  ; -- APD, 2002-08-07
                                                  (medium-foreground stream)
                                                  (make-opacity 0.5))
                                                (medium-background stream)))
            (princ (menu-item-display menu-item) stream))))))

;; Spec function.
(defun draw-standard-menu
    (stream presentation-type items default-item
     &key item-printer
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y)
  (declare (ignore default-item))
  (orf item-printer #'print-menu-item)
  (format-items items
                :stream stream
                :printer
                (lambda (item stream)
                  (ecase (menu-item-option item :type :item)
                    (:item
                     ;; This is a normal item, just output.
                     (let ((activep (menu-item-option item :active t)))
                       (with-presentation-type-decoded (name params options)
                           presentation-type
                         (let ((*allow-sensitive-inferiors* activep))
                           (with-text-style
                               (stream (menu-item-option
                                        item :style
                                        '(:sans-serif nil nil)))
                             (with-output-as-presentation
                                 (stream
                                  item
                                  `((,name ,@params)
                                    :description ,(getf (menu-item-options item) :documentation)
                                    ,@options)
                                  :single-box t)
                               (funcall item-printer item stream)))))))
                    (:label
                     ;; This is a static label, it should not be
                     ;; mouse-sensitive, but not grayed out either.
                     (with-text-style (stream (menu-item-option
                                               item :style
                                               '(:sans-serif nil nil)))
                       (funcall item-printer item stream)))
                    (:divider
                     ;; FIXME: Should draw a line instead.
                     (with-text-style (stream (menu-item-option
                                               item :style
                                               '(:sans-serif :italic nil)))
                       (funcall item-printer item stream)))))
                :presentation-type nil
                :x-spacing x-spacing
                :y-spacing y-spacing
                :n-columns n-columns
                :n-rows n-rows
                :max-width max-width
                :max-height max-height
                :cell-align-x cell-align-x
                :cell-align-y (or cell-align-y :top)
                :row-wise row-wise))

(defclass menu-pane (clim-stream-pane)
  ((menu-frame))
  (:default-initargs :background *3d-normal-color*))


;; When the menu frame is created it is disabled.
;; To make the menu visible it is required to call enable-menu.
(defgeneric enable-menu (pane))

(defmethod enable-menu ((pane menu-pane))
  (enable-frame (slot-value pane 'menu-frame)))

;; Spec macro.
;; The menu is not visible.
(defmacro with-menu ((menu &optional associated-window
                           &key (deexpose t) label scroll-bars)
                     &body body)
  (check-type menu symbol)
  (with-gensyms (with-menu-cont)
    `(flet ((,with-menu-cont (,menu)
              ,@body))
       (declare (dynamic-extent #',with-menu-cont))
       (invoke-with-menu #',with-menu-cont
                         ,associated-window ; XXX
                         ',deexpose         ; XXX!!!
                         ,label
                         ,scroll-bars))))

(defun invoke-with-menu (continuation associated-window deexpose
			 label scroll-bars)
  (let* ((associated-frame (if associated-window
                               (pane-frame associated-window)
                               *application-frame*))
         (fm (frame-manager associated-frame)))
    (with-look-and-feel-realization (fm associated-frame) ; hmm... checkme
      (let* ((menu-stream (make-pane-1 fm associated-frame 'menu-pane))
             (container (scrolling (:scroll-bar scroll-bars)
                          menu-stream))
	     (frame (make-menu-frame (raising ()
				       (if label
					   (labelling (:label label
						       :name 'label
						       :label-alignment :top)
					     container)
					   container))
				     :left nil
				     :top nil)))
        (adopt-frame fm frame)
	(setf (slot-value menu-stream 'menu-frame) frame)
        (unwind-protect
             (progn
               (setf (stream-end-of-line-action menu-stream) :allow
                     (stream-end-of-page-action menu-stream) :allow)
               (funcall continuation menu-stream))
          (when deexpose ; Checkme as well.
            (disown-frame fm frame)))))))

(define-presentation-type menu-item ())

(defmethod menu-choose
    (items &rest args &key associated-window &allow-other-keys)
  (let* ((associated-frame (if associated-window
                               (pane-frame associated-window)
                               *application-frame*))
         (frame-manager (frame-manager associated-frame)))
    (apply #'frame-manager-menu-choose frame-manager items args)))

(defmethod frame-manager-menu-choose
    (frame-manager items    ; XXX specialize on STANDARD-FRAME-MANAGER
     &rest options
     &key associated-window printer presentation-type
     (default-item nil default-item-p)
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows (n-columns 1) x-spacing y-spacing row-wise
     cell-align-x cell-align-y (scroll-bars :vertical)
     ;; We provide pointer documentation by default.
     (pointer-documentation *pointer-documentation-output*))
  (flet ((drawer (stream type)
           (draw-standard-menu stream type items
                               (if default-item-p
                                   default-item
                                   (first items))
                               :item-printer (or printer
                                                 #'print-menu-item)
                               :max-width max-width
                               :max-height max-height
                               :n-rows n-rows
                               :n-columns n-columns
                               :x-spacing x-spacing
                               :y-spacing y-spacing
                               :row-wise row-wise
                               :cell-align-x cell-align-x
                               :cell-align-y cell-align-y)))
    (multiple-value-bind (object event)
        (with-menu (menu associated-window
                         :label label
                         :scroll-bars scroll-bars)
          (when text-style
            (setf (medium-text-style menu) text-style))
          (letf (((stream-default-view menu) +textual-menu-view+))
            (menu-choose-from-drawer menu (or presentation-type 'menu-item)
                                     #'drawer
                                     :cache cache
                                     :unique-id unique-id
                                     :id-test id-test
                                     :cache-value cache-value
                                     :cache-test cache-test
                                     :pointer-documentation pointer-documentation)))
      (unless (null event)              ; Event is NIL if user aborted.
        (let ((subitems (menu-item-option object :items 'menu-item-no-items)))
          (if (eq subitems 'menu-item-no-items)
              (values (menu-item-value object) object event)
              (apply #'frame-manager-menu-choose
                     frame-manager subitems
                     options)))))))

(defun max-x-y (frame)
  "Return the maximum X and Y coordinate values for a menu for
`frame' (essentially, the screen resolution with a slight
padding.)"
  ;; FIXME? There may be a better way.
  (let* ((port (port (frame-manager frame)))
         (graft (find-graft :port port)))
    (values (- (graft-width graft) 50)
            (- (graft-height graft) 50))))

(defun menu-size (menu frame)
  "Return two values, the height and width of MENU (adjusted for
maximum size according to `frame')."
  (multiple-value-bind (max-width max-height)
      (max-x-y frame)
    (with-bounding-rectangle* (x1 y1 x2 y2) menu
      (declare (ignore x1 y1))
      (values (min x2 max-width)
              (min y2 max-height)))))

(defmethod adjust-menu-size-and-position ((menu menu-pane)
                                          &key x-position y-position)
  ;; Make sure the menu isn't higher or wider than the screen.
  (multiple-value-bind (menu-width menu-height)
      (menu-size (stream-output-history menu) *application-frame*)
    (change-space-requirements menu
			       :width menu-width
			       :height menu-height
                               :resize-frame t)

    ;; If we have scroll-bars, we need to do some calibration of the
    ;; size of the viewport.
    (when (pane-viewport menu)
     (multiple-value-bind (viewport-width viewport-height)
         (menu-size (pane-viewport menu) *application-frame*)
       (change-space-requirements (pane-scroller menu)
                                  ;; HACK: How are you supposed to
                                  ;; change the size of the viewport?
                                  ;; I could only find this way, where
                                  ;; I calculate the size difference
                                  ;; between the viewport and the
                                  ;; scroller pane, and set the
                                  ;; scroller pane to the desired size
                                  ;; of the viewport, plus the
                                  ;; difference (to make room for
                                  ;; scroll bars).
                                  :width (+ menu-width
                                            (- (pane-current-width (pane-scroller menu))
                                               viewport-width))
                                  :height (+ menu-height
                                             (- (pane-current-height (pane-scroller menu))
                                                viewport-height))
                                  :resize-frame t)))

    ;; Modify the size and location of the frame as well.
    (let ((top-level-pane (get-top-level-sheet menu)))
      (multiple-value-bind (frame-width frame-height)
          (menu-size top-level-pane *application-frame*)
        (multiple-value-bind (res-max-x res-max-y) (max-x-y *application-frame*)
          ;; Move the menu frame so that no entries are outside the visible
          ;; part of the screen.
          (let ((max-left (- res-max-x frame-width))
                (max-top (- res-max-y frame-height)))
            ;; XXX: This is an ugly way to find the screen position of
            ;; the menu frame, possibly even undefined.
            (multiple-value-bind (left top)
                (with-slots (dx dy) (sheet-transformation top-level-pane)
                  (values dx dy))
              (when x-position
                (setf left x-position))
              (when y-position
                (setf top y-position))
              ;; Adjust for maximum position if the programmer has not
              ;; explicitly provided coordinates.
              (if (null x-position)
               (when (> left max-left)
                 (setf left max-left)))
              (if (null y-position)
               (when (> top max-top)
                 (setf top max-top)))
              (move-sheet top-level-pane
                          (max left 0) (max top 0)))))))))

(defmethod adjust-menu-size-and-position (menu &key &allow-other-keys)
  ;; Nothing.
  nil)

;; Spec function.
(defmethod menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (declare (ignore cache unique-id
                   id-test cache-value cache-test default-presentation))
  (with-room-for-graphics (menu :first-quadrant nil)
    (funcall drawer menu presentation-type))
  
  (adjust-menu-size-and-position
   menu
   :x-position x-position
   :y-position y-position)
  ;; The menu is enabled (make visible) after the size is adjusted.
  (enable-menu menu)
  (let ((*pointer-documentation-output* pointer-documentation))
    (let ((*pointer-documentation-output* pointer-documentation))
      (handler-case
          (with-input-context (`(or ,presentation-type blank-area) :override t)
              (object type event) 
              (prog1 nil (loop (read-gesture :stream menu)))
            (blank-area nil)
            (t (values object event)))
        (abort-gesture () nil)))))
