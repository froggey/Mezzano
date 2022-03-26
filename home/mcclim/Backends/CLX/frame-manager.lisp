;;; -*- Mode: Lisp; Package: CLIM-CLX -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
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

(in-package :clim-clx)

;;; CLX-FRAME-MANAGER class

(defclass clx-frame-manager (frame-manager)
  ((mirroring-fn :initarg :mirroring
                 :initform (mirror-factory :full)
                 :reader mirroring-p)
   (class-gensym :initarg :class-gensym
                 :initform (gensym "CLX-")
                 :reader class-gensym)))

;;; We use &ALLOW-OTHER-KEYS since the INITIALIZE-INSTANCE for
;;; CLX-PORT passes various initargs that CLX-FRAME-MANAGER doesn't
;;; necessarily accept.
(defmethod initialize-instance :after ((instance clx-frame-manager)
                                       &key &allow-other-keys))

;;; Default mirroring predicates
(defun mirror-factory (kind)
  (etypecase kind
    (null nil)
    (function kind)
    ((eql :single)
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (subtypep class 'top-level-sheet-pane))))
    ((eql :full)
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (subtypep class 'basic-pane))))
    ((eql :random) ;; for testing
     #'(lambda (class)
         (and (not (subtypep class 'mirrored-sheet-mixin))
              (or (subtypep class 'top-level-sheet-pane)
                  (zerop (random 2))))))))

;; Abstract pane lookup logic

(defmethod find-concrete-pane-class ((fm clx-frame-manager)
                                     pane-type &optional errorp)
  ;; This backend doesn't have any specialized pane implementations
  ;; but depending on circumstances it may add optional mirroring to
  ;; the class. Such automatically defined concrete class has the same
  ;; name but with a gensym prefix and symbol in the backend package.
  (declare (ignore errorp))
  (maybe-mirroring fm (call-next-method)))

;;; This is an example of how make-pane-1 might create specialized
;;; instances of the generic pane types based upon the type of the
;;; frame-manager. However, in the CLX case, we don't expect there to
;;; be any CLX specific panes. CLX uses the default generic panes
;;; instead.
(defun maybe-mirroring (fm concrete-pane-class)
  (when (funcall (mirroring-p fm) concrete-pane-class)
    (let ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                          (class-name concrete-pane-class)
                                          concrete-pane-class)))
      (multiple-value-bind (class-symbol foundp)
          (alexandria:ensure-symbol
           (alexandria:symbolicate (class-gensym fm) "-"
                                   (symbol-name concrete-pane-class-symbol))
           :clim-clx)
        (unless foundp
          (eval
           `(defclass ,class-symbol
                (mirrored-sheet-mixin
                 ,@(unless (subtypep concrete-pane-class 'sheet-with-medium-mixin)
                     '(permanent-medium-sheet-output-mixin))
                 ,concrete-pane-class-symbol)
              ()
              (:metaclass ,(type-of (find-class concrete-pane-class-symbol))))))
        (setf concrete-pane-class (find-class class-symbol)))))
  concrete-pane-class)

(defmethod adopt-frame :before ((fm clx-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (multiple-value-bind (x y)
        (xlib:query-pointer (clx-port-window (port fm)))
      (incf x 10)
      (setf (slot-value frame 'climi::left) x
            (slot-value frame 'climi::top) y))))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame menu-frame))
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (xlib:map-window (sheet-direct-xmirror (slot-value frame 'top-level-sheet)))))

(defgeneric tell-window-manager-about-space-requirements (pane))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame application-frame))
  (let ((sheet (slot-value frame 'top-level-sheet)))
    (let* ((top-level-sheet (frame-top-level-sheet frame))
           (mirror (sheet-direct-xmirror top-level-sheet)))
      (case (clim-extensions:find-frame-type frame)
        (:override-redirect (setf (xlib:window-override-redirect mirror) :on))
        (:dialog (xlib:change-property mirror
                                       :_NET_WM_WINDOW_TYPE
                                       (list (xlib:intern-atom (xlib:window-display mirror) :_NET_WM_WINDOW_TYPE_DIALOG))
                                       :atom 32)))
      (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
        (declare (ignore w h))
        (when (and x y)
          (setf (xlib:drawable-x mirror) x
                (xlib:drawable-y mirror) y))
        (tell-window-manager-about-space-requirements top-level-sheet))
      ;; :structure-notify events were not yet turned on, turn them
      ;; on now, so that we get informed about the windows position
      ;; (and possibly size), when the window gets maped.
      (setf (xlib:window-event-mask mirror)
            (logior (xlib:window-event-mask mirror)
                    (xlib:make-event-mask :structure-notify)))
      ;; Care for calling-frame, be careful not to trip on missing bits
      (let* ((calling-frame (frame-calling-frame frame))
             (tls (and calling-frame (frame-top-level-sheet calling-frame)))
             (calling-mirror (and tls (sheet-xmirror tls))))
        (when calling-mirror
          (setf (xlib:transient-for mirror)
                calling-mirror)))
      ;;
      (when (sheet-enabled-p sheet)
        (xlib:map-window mirror)))))

(defmethod tell-window-manager-about-space-requirements ((pane top-level-sheet-pane))
  (multiple-value-bind (w h x y) (climi::frame-geometry* (pane-frame pane))
    (declare (ignore w h))
    (let ((q (compose-space pane)))
      (let ((mirror (sheet-direct-xmirror pane)))
        (setf (xlib:wm-normal-hints mirror)
              (xlib:make-wm-size-hints
               :user-specified-position-p (and x y)
               :x x :y y
               :width  (round (space-requirement-width q))
               :height (round (space-requirement-height q))
               :max-width (min 65535 (round (space-requirement-max-width q)))
               :max-height (min 65535 (round (space-requirement-max-height q)))
               :min-width (round (space-requirement-min-width q))
               :min-height (round (space-requirement-min-height q))))))))

(defmethod tell-window-manager-about-space-requirements ((pane t))
  ;; hmm
  nil)

(defmethod note-space-requirements-changed :after ((graft clx-graft) pane)
  (tell-window-manager-about-space-requirements pane))

#+nil
(defmethod (setf clim:sheet-transformation) :around (transformation (sheet clx-pane-mixin))
  (log:info "transforming clx sheet: ~s" sheet)
  (unless (transformation-equal transformation (sheet-transformation sheet))
    (let ((old-transformation (sheet-transformation sheet)))
      (let ((climi::*inhibit-dispatch-repaint* nil))
        (call-next-method))
      #+nil
      (when (sheet-viewable-p sheet)
        (let* ((sheet-region (sheet-region sheet))
               (new-region (transform-region (sheet-transformation sheet) sheet-region))
               (old-region (transform-region old-transformation sheet-region)))
          (log:info "OLD: ~s    NEW: ~s" old-region new-region)
          #+nil
          (dispatch-repaint (sheet-parent sheet)
                            (region-union new-region old-region)))))))
