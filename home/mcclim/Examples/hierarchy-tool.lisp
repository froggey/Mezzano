;;;;  Copyright (c) 2018 Daniel Kochmański
;;;;
;;;;    License: BSD-2-Clause.

(defpackage #:clim-demo.hierarchy
  (:use #:clim-lisp #:clim)
  (:export #:hierarchy))
(in-package #:clim-demo.hierarchy)

(defun repaint (&key (name "Unnamed pane"))
  (lambda (pane region)
    (declare (ignore region))
    (with-bounding-rectangle* (x1 y1 x2 y2) pane
      (draw-rectangle* pane x1 y1 x2 y2
                       :ink (if (typep pane 'clim:mirrored-sheet-mixin)
                                clim:+dark-red+
                                clim:+dark-green+)
                       :clipping-region (region-difference
                                         (make-rectangle* x1 y1 x2 y2)
                                         (make-rectangle* (+ x1 10) (+ y1 10)
                                                          (- x2 10) (- y2 10))))
      (draw-rectangle* pane x1 y1 x2 y2 :filled nil)
      (draw-circle* pane (/ (- x2 x1) 2) (/ (- y2 y1) 2) 15
                    :ink (if (slot-value pane 'state)
                             clim:+yellow+
                             clim:+blue+))
      (draw-text* pane
                  name
                  (+ x1 10) (+ y1 10) :align-y :top))))

;;; 1. If none of the panes is stream-input-pane then keystroke gestures doesn't
;;; work.
;;;
;;; 2. Redisplaying frame multiple times causes size changes (we settle with
;;; gadgets for now (because of 1. and 2.).

(defclass layout-protocol-gadget (clim:basic-gadget)
  ((state :initform nil)
   (repaint-function :initarg :repaint-function :initform (error "no repaint function"))))

(defclass layout-protocol-gadget* (layout-protocol-gadget)
  ((repaint-function :initarg :repaint-function :initform (error "no repaint function"))))

(defmethod handle-repaint ((sheet layout-protocol-gadget) region)
  (funcall (slot-value sheet 'repaint-function) sheet region))

(defmethod handle-event ((sheet layout-protocol-gadget) (event pointer-enter-event))
  (setf (slot-value sheet 'state) t)
  (handle-repaint sheet +everywhere+))

(defmethod handle-event ((sheet layout-protocol-gadget) (event pointer-exit-event))
  (setf (slot-value sheet 'state) nil)
  (handle-repaint sheet +everywhere+))

(defparameter *panes* (make-hash-table :test #'equalp))
(defparameter *rpanes* (make-hash-table :test #'equalp))

(defun make-leaf (name mirror)
  (let ((pane (make-pane (if mirror
                             'layout-protocol-gadget
                             'layout-protocol-gadget*)
                         :width 100 :height 100
                         :name name
                         :repaint-function (repaint :name name))))
    (setf (gethash name *panes*) pane)
    pane))

(define-application-frame window-protocol ()
  ()
  (:pane (let* (;;leafs
                (pane5 (make-leaf "pane5" nil))
                (pane7 (make-leaf "pane7" t))
                (pane8 (make-leaf "pane8" t))
                (pane9 (make-leaf "pane9" nil))
                (pane0 (make-leaf "pane0" nil))
                (pane2 (make-leaf "pane2" t))
                (pane6 (make-pane :spacing-pane :name "pane6"
                                  :thickness 10
                                  :contents (list pane8)))
                (pane3 (make-pane :vrack-pane   :name "pane3"
                                  :contents (list pane5 pane6 pane7)))
                (pane4 (make-pane :vrack-pane :name "pane4"
                                  :contents (list pane9 pane0)))
                (pane1 (make-pane :hrack-pane :name "pane1"
                                  :contents (list pane2 pane3 pane4))))
           (setf (gethash "pane6" *panes*) pane6)
           (setf (gethash "pane3" *panes*) pane3)
           (setf (gethash "pane4" *panes*) pane4)
           (setf (gethash "pane1" *panes*) pane1)
           pane1))
  (:menu-bar nil))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type resize-handler ())
  (define-presentation-type move-handler ()))

;;; Redisplay causes some flicker - I'm leaving it for someone who will
;;; implement and document how to use double-buffered sheets.
(defclass refresh-event (window-manager-event) ())

(defun draw-rectangle** (pane sheet label x1 y1 x2 y2
                         &rest options &key ink background &allow-other-keys
                         &aux (climi::*drop-shadow-default-offset* 2))
  "Draws rectangle, its label and handlers for move/resize commands. Doesn't
really draw a rectangle :-)."
  (declare (ignorable options))
  (surrounding-output-with-border (pane :foreground +black+
                                        :background background
                                        ;:highlight-background +yellow+
                                        :ink ink
                                        :shape :drop-shadow
                                        :shadow-offset 3
                                        :padding 0
                                        :padding-bottom 0
                                        :padding-right 0)
    ;; border size
    (draw-point* pane x1 y1 :line-thickness 1 :ink +transparent-ink+)
    (draw-point* pane x2 y2 :line-thickness 1 :ink +transparent-ink+))
  (clim:with-output-as-presentation (pane sheet 'move-handler)
    (draw-text* pane label x1 y1 :align-y :top :align-x :left :text-size :small))
  (clim:with-output-as-presentation (pane sheet 'resize-handler)
    (draw-point* pane x2 y2 :line-thickness 11 :ink +dark-blue+)))

(defun fix-scaling (sheet)
  (with-bounding-rectangle* (x1 y1 x2 y2) sheet
    (let* ((offset 15)
           (width (- x2 x1))
           (height (- y2 y1))
           (scale-x (unless (zerop width)  (/ (- width  (* 2 offset)) width)))
           (scale-y (unless (zerop height) (/ (- height (* 2 offset)) height))))
      (if (and scale-x scale-y)
          (make-scaling-transformation* scale-x scale-y (/ width 2) (/ height 2))
          +identity-transformation+))))

(defun fix-transformation (sheet)
  "Provides ~proportional scaling wrt sheet parent (so the offset is kept - we
need that if we want to see composite sheets."
  (compose-transformations
   (if (climi::top-level-sheet-pane-p sheet)
       (make-scaling-transformation .8 .8)
       (sheet-transformation sheet))
   (fix-scaling sheet)))

(defun display-hierarchy-tool (frame pane)
  "Takes the inspected frame and draws its pane hierarchy."
  (labels ((draw-sheet (sheet)
             (with-bounding-rectangle* (x1 y1 x2 y2) sheet
               (with-drawing-options (pane :transformation (fix-transformation sheet))
                 (let ((color (if (typep sheet 'mirrored-sheet-mixin)
                                  +dark-red+
                                  +dark-green+)))
                   (with-output-as-presentation (pane sheet 'pane)
                     (draw-rectangle** pane sheet (string-downcase (pane-name sheet))
                                       x1 y1 x2 y2 :ink color
                                       :background
                                       (if (or (typep sheet 'climi::sheet-multiple-child-mixin)
                                               (typep sheet 'climi::sheet-single-child-mixin))
                                           +grey+
                                           +white+))))
                 (if (clipped-hierarchy frame)
                     (with-drawing-options (pane :clipping-region
                                                 (make-bounding-rectangle x1 y1 x2 y2))
                       (mapc #'draw-sheet (sheet-children sheet)))
                     (mapc #'draw-sheet (sheet-children sheet))))))
           (draw-sheet-native (sheet &optional (deep 0))
             (with-bounding-rectangle* (x1 y1 x2 y2) sheet
               (let ((width (- x2 x1))
                     (height (- y2 y1))
                     (color (if (typep sheet 'mirrored-sheet-mixin)
                                +dark-red+
                                +dark-green+)))
                 (multiple-value-bind (x0 y0)
                     (transform-position (sheet-native-transformation sheet) x1 y1)
                   (draw-rectangle* pane
                                    (+ x0 (* deep 5))
                                    (+ y0 (* deep 5))
                                    (+ x0 width (* deep -5))
                                    (+ y0 height (* deep -5))
                                    :ink color
                                    :filled nil)
                   (with-drawing-options
                       (pane :clipping-region
                             (if (null (clipped-hierarchy frame))
                                 +everywhere+
                                 (make-bounding-rectangle x0 y0 (+ x0 width) (+ y0 height)))
                             :transformation
                             (if (not (typep sheet 'mirrored-sheet-mixin))
                                 +identity-transformation+
                                 (sheet-native-transformation sheet)))
                     (dolist (s (sheet-children sheet))
                       (draw-sheet-native s (1+ deep)))))))))
    (let* ((fr (inspected-frame frame))
           (tp (and fr (frame-top-level-sheet fr))))
      (cond
        ((and fr tp (native-hierarchy frame))
         (draw-sheet-native tp))
        ((and fr tp)
         (draw-sheet tp))
        (t
         (format pane "

This application is meant to help with debugging CLIM pane hierarchies, mirrors,
layout protocol and such. It may morth into inspector definitions in the
future. Some useful notes:

- Updates to display are not automatic. Sometimes after changing size of panes
something change in it, but there might be extra window configuration event from
display server later which change more. To have the newest preview of pane
hierarchy press C-r.

- Proportions are little distorted to show composite panes surrounding their
children. Some scaling and translation is involved (deeper in the hierarchy
bigger the distortion is).

- If sheet is mirrored it is drawn with red border. Otherwise green border is
used. If background is grey then sheet is a composite pane, if the background is
white it is not.

- Some drag and drop support is provided. Drag pane label to move the pane
itself, drag circle at lower right corner to resize it. Note that due to
distortion you won't get exact coordinates you have dragged to. This could be
fixed with untransforming positions, but it's not worth it at this point of
time. Actual resize and move point will be a little off (but just a little).

- Resizing and moving panes doesn't invoke layout protocol. The exception is
size change of the top-level-sheet-pane (the topmost window).

- To make CLX-fb button working load system mcclim-clx-fb. This should load
backend definitions. First button should work right of the bat if you run on
Linux.

- Application which starts should react on mouse moving over the panes (circle
should be yellow if pointer is above the sheet).

- Clip children toggle button is used to limit drawing space to the parent area
in the representation (this should be the case for \"real\" drawing too."))))))

(defun mb (name path)
  "Helper creating a button which starts the application and dispatching
refresh-event to redisplay pane hierarchy when we start new application."
  (make-pane :push-button :label name
             :activate-callback
             (let ((hfr *application-frame*))
               #'(lambda (gadget)
                   (declare (ignore gadget))
                   (bt:make-thread (lambda ()
                                     (let ((clim:*default-server-path* path)
                                           (frame (make-application-frame 'window-protocol
                                                                          :pretty-name name)))
                                       (setf (inspected-frame hfr) frame)
                                       (clim:run-frame-top-level frame))))
                   (sleep 0.1)
                   (queue-event (frame-top-level-sheet hfr)
                                (make-instance 'refresh-event :sheet hfr))))))

(defun run-hierarchy (&optional iframe)
  (let* ((frame (make-application-frame 'hierarchy :iframe iframe)))
    (clim:run-frame-top-level frame)))

(define-application-frame hierarchy ()
  ((frame :initform nil :initarg :iframe :accessor inspected-frame)
   (clipp :initform nil :accessor clipped-hierarchy)
   (native :initform nil :accessor native-hierarchy))
  (:panes (app :application :display-function 'display-hierarchy-tool :scroll-bars nil
               :width 800 :height 600)
          (buttons
           (vertically ()
             (mb "Run CLX (full)" '(:clx :mirroring :full))
             (mb "Run CLX (random)" '(:clx :mirroring :random))
             (mb "Run CLX (single)" '(:clx :mirroring :single))
             (mb "Run CLX-fb" :clx-fb)
             '+fill+
             (make-pane :push-button :label "Inspect this frame"
                        :activate-callback
                        #'(lambda (gadget)
                            (declare (ignore gadget))
                            (setf (inspected-frame *application-frame*)
                                  *application-frame*)
                            (com-refresh)))
             (make-pane :toggle-button :label "Native coordinates"
                        :value-changed-callback
                        #'(lambda (gadget value)
                            (declare (ignore gadget))
                            (setf (native-hierarchy *application-frame*) value)
                            (com-refresh)))
             (make-pane :toggle-button :label "Clip children"
                        :value-changed-callback
                        #'(lambda (gadget value)
                            (declare (ignore gadget))
                            (setf (clipped-hierarchy *application-frame*) value)
                            (com-refresh)))))
          (interactor :interactor))
  (:layouts (default
                (vertically ()
                  (horizontally ()
                    (200 buttons)
                    (scrolling (:width 1280 :height 800) app))
                  (150 interactor))))
  (:menu-bar nil))

(defmethod handle-event ((client hierarchy) (event refresh-event))
  (redisplay-frame-pane client 'app))

(define-hierarchy-command (com-refresh :menu t :keystroke (#\r :control)) ()
  (queue-event (frame-top-level-sheet *application-frame*)
               (make-instance 'refresh-event :sheet *application-frame*)))

(define-hierarchy-command (com-move-sheet :name t)
    ((sheet pane) (x integer) (y integer))
  (move-sheet sheet x y))

(define-hierarchy-command (com-dx-sheet :name t)
    ((sheet pane) (x integer) (y integer))
  (let ((transform (sheet-transformation sheet)))
    (setf (sheet-transformation sheet)
          (compose-translation-with-transformation
           transform x y))))

(define-hierarchy-command (com-resize-sheet :name t)
    ((sheet pane) (x integer) (y integer))
  (resize-sheet sheet x y))

(define-hierarchy-command (com-move-and-resize-sheet :name t)
    ((sheet pane) (x integer) (y integer) (width integer) (height integer))
  (move-and-resize-sheet sheet x y width height))

(define-hierarchy-command (com-disable-sheet :name t)
    ((sheet pane))
  (setf (sheet-enabled-p sheet) nil))

(define-hierarchy-command (com-enable-sheet :name t)
    ((sheet pane))
  (setf (sheet-enabled-p sheet) t))

;;; Drag&Drop is buggy as hell
(define-presentation-to-command-translator translator-resize
    (resize-handler com-resize-sheet* hierarchy)
    (object)
  (list object))

(define-hierarchy-command (com-resize-sheet*)
    ((original resize-handler))
  (let ((pane (get-frame-pane *application-frame* 'app)))
    (multiple-value-bind (init-x init-y) (stream-pointer-position pane)
      (multiple-value-bind (x y)
          (dragging-output (pane :finish-on-release t)
            (multiple-value-bind (x y) (stream-pointer-position pane)
              (draw-circle* pane x y 5 :filled t)))
        (com-resize-sheet original
                          (+ (bounding-rectangle-width original) (- x init-x))
                          (+ (bounding-rectangle-height original) (- y init-y)))))))

(define-presentation-to-command-translator translator-move
    (move-handler com-move-sheet* hierarchy)
    (object)
  (list object))

(define-hierarchy-command (com-move-sheet*)
    ((original move-handler))
  (let ((pane (get-frame-pane *application-frame* 'app)))
    (multiple-value-bind (init-x init-y) (stream-pointer-position pane)
      (multiple-value-bind (x y)
          (dragging-output (pane :finish-on-release t)
            (multiple-value-bind (x y) (stream-pointer-position pane)
              (draw-circle* pane x y 5 :filled t)))
        (com-dx-sheet original (- x init-x) (- y init-y))))))
