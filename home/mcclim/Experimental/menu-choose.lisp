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
;;; - Menu item options: :active.
;;;
;;; - Documentation.
;;;
;;; - Menu position.
;;;
;;; - Empty menu.

;;; TODO:
;;;
;;; + returned values
;;; + menu frame size
;;; + layout

(in-package :CLIM-INTERNALS)

(defgeneric menu-choose
    (items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

(defgeneric frame-manager-menu-choose
    (frame-manager items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

(defgeneric menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation))

;;;
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
  (getf (menu-item-options menu-item) option default))

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

(defun draw-standard-menu
    (stream presentation-type items default-item
     &key item-printer
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y)
  (declare (ignore default-item))
  (orf item-printer #'print-menu-item)
  (format-items items
                :stream stream
                :printer (lambda (item stream)
                           (let ((activep (menu-item-option item :active t)))
                             (with-presentation-type-decoded (name params options)
                                 presentation-type
                               (let ((*allow-sensitive-inferiors* activep))                                 
                                 (with-text-style (stream (or (getf (menu-item-options item) :style)
                                                              '(:sans-serif nil nil)))
                                   (with-output-as-presentation                                     
                                       (stream
                                        item
                                        `((,name ,@params)
                                          :description ,(getf (menu-item-options item) :documentation)
                                          ,@options))
                                     (funcall item-printer item stream)))))))
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
                         ',deexpose ; XXX!!!
			 ,label
			 ,scroll-bars)))) 

(defun invoke-with-menu (continuation associated-window deexpose
			 label scroll-bars)
  (declare (ignore deexpose label scroll-bars))           ; FIXME!!!
  (let* ((associated-frame (if associated-window
                               (pane-frame associated-window)
                               *application-frame*))
         (fm (frame-manager associated-frame)))
    (with-look-and-feel-realization (fm associated-frame) ; hmm... checkme
      (let* ((stream (make-pane-1 fm associated-frame 'command-menu-pane
			          :background +gray80+))
	     (raised (make-pane-1 fm associated-frame 'raised-pane
			          :border-width 2 :background +gray80+
			          :contents (list stream)))
             (frame (make-menu-frame raised
                                     :left nil
                                     :top  nil)))
          (adopt-frame fm frame)
          (change-space-requirements stream :width 1 :height 1) ;What is that supposed to do? --GB 2003-03-16
                                                                ; Shadow bug somewhere else?
          (unwind-protect
               (progn
                 (setf (stream-end-of-line-action stream) :allow
                       (stream-end-of-page-action stream) :allow)
                 (funcall continuation stream))
            (disown-frame fm frame))))))

(define-presentation-type menu-item ())

;;;
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
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation)
  (flet ((drawer (stream type)
           (draw-standard-menu stream type items
                               (if default-item-p
                                   default-item
                                   (first items))
                               :item-printer (if printer
                                                 (lambda (item stream)
                                                   (funcall printer (menu-item-display item) stream))
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
        (with-menu (menu associated-window)
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
      (let ((subitems (menu-item-option object :items 'menu-item-no-items)))
        (if (eq subitems 'menu-item-no-items)
            (values (menu-item-value object) object event)
            (apply #'frame-manager-menu-choose
                   frame-manager subitems
                   options))))))

#+NIL
(defmethod menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (funcall drawer menu presentation-type)
  (when (typep menu 'command-menu-pane)
    (with-bounding-rectangle* (x1 y1 x2 y2)
        (stream-output-history menu)
      (declare (ignorable x1 y1 x2 y2))
      (change-space-requirements menu
                                 :width x2
                                 :height y2
                                 :resize-frame t)))
  (let ((*pointer-documentation-output* pointer-documentation))
    (handler-case
        (with-input-context (presentation-type :override t)
              (object type event)
          (loop (read-gesture :stream menu))
          (t (values object event)))
      (abort-gesture () (values nil)))))

(defmethod menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (with-room-for-graphics (menu :first-quadrant nil)
    (funcall drawer menu presentation-type))
  (when (typep menu 'command-menu-pane)
    (with-bounding-rectangle* (x1 y1 x2 y2)
        (stream-output-history menu)
      (declare (ignorable x1 y1 x2 y2))
      (change-space-requirements menu
                                 :width x2
                                 :height y2
                                 :resize-frame t)))
  (let ((*pointer-documentation-output* pointer-documentation))	
    (tracking-pointer (menu :context-type presentation-type
			    :multiple-window t :highlight t)
      (:pointer-button-press (&key event x y) ; Close if pointer clicked outside menu.
         (unless (and (sheet-ancestor-p (event-sheet event) menu)
                      (region-contains-position-p (sheet-region menu) x y))
           (return-from menu-choose-from-drawer (values nil))))
      (:presentation-button-release (&key event presentation x y)
        (if (and (sheet-ancestor-p (event-sheet event) menu)
                 (region-contains-position-p (sheet-region menu) x y))
            (return-from menu-choose-from-drawer
              (values (presentation-object presentation) event))
            (return-from menu-choose-from-drawer (values nil)))))))
