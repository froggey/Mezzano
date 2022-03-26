(in-package #:clim-mezzano)

;; minimum mezzano frame size for resize events
(defparameter *minimum-width* 100)
(defparameter *minimum-height* 100)

;; These x y variables are always in mcclim "units", that is they
;; always apply to the *last-mouse-sheet*, not the mezzano frame
(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)

(defvar *last-modifier-state* 0)

(defvar *char->name* (make-hash-table :test #'eql))

;;;======================================================================
;;;
;;; mez-event->mcclim-event - converts mezzano events to mcclim events
;;;
;;;======================================================================

(defgeneric mez-event->mcclim-event (port event))

(defmethod mez-event->mcclim-event (port event)
  ;; Default case - log event and ignore
  (debug-format "mcclim backend unexpected event")
  (debug-format "    ~S" event)
  nil)

;;;======================================================================
;;; Keyboard Events
;;;======================================================================

(defun get-name (char)
  (let ((name (gethash char *char->name*)))
    (if name
        name
        (setf (gethash char *char->name*) (intern (string char) :keyword)))))

(defparameter +modifier-to-clim-alist+
  `((:shift    . ,+shift-key+)
    (:control  . ,+control-key+)
    (:meta     . ,+meta-key+)
    (:super    . ,+super-key+)))

(defun compute-modifier-state (modifier-keys)
  (let ((modifier 0))
    (dolist (key modifier-keys)
      (let ((modifier-info (assoc key +modifier-to-clim-alist+)))
        (if modifier-info
            (setf modifier (logior modifier (cdr modifier-info)))
            (debug-format "Unknown modifier key ~S" key))))
    (setf *last-modifier-state* modifier)))

(defmethod mez-event->mcclim-event (port (event mos:key-event))
  (let* ((releasep       (mos:key-releasep event))
         (char           (mos:key-key event))
         (name           (get-name char))
         (modifier-state (compute-modifier-state (mos:key-modifier-state event)))
         (mez-window     (mos:window event))
         (pointer        (port-pointer port))
         (sheet          (port-lookup-sheet port mez-window)))
    (when sheet
      (make-instance (if releasep 'key-release-event 'key-press-event)
                     :sheet (or (frame-properties (pane-frame sheet) 'focus)
                                sheet)
                     :key-name name
                     :key-character char
                     :x *last-mouse-x*
                     :y *last-mouse-y*
                     :graft-x (pointer-x pointer)
                     :graft-y (pointer-y pointer)
                     :modifier-state modifier-state))))

;;;======================================================================
;;; Pointer Events
;;;======================================================================

(defparameter +button-to-clim-alist+
  `((,(byte 1 0) . ,+pointer-left-button+)
    (,(byte 1 1) . ,+pointer-right-button+)
    (,(byte 1 2) . ,+pointer-middle-button+)
    (,(byte 1 3) . ,+pointer-wheel-up+)
    (,(byte 1 4) . ,+pointer-wheel-down+)))

(defun compute-mouse-buttons (buttons)
  (let ((result 0))
    (dolist (tr +button-to-clim-alist+)
      (when (ldb-test (car tr) buttons)
        (setf result (logior result (cdr tr)))))
    result))

(defun pointer-event (port sheet class &rest initargs)
  (let ((time 0)
        (pointer (port-pointer port)))
    (apply #'make-instance class
           :timestamp time
           :sheet sheet
           :pointer pointer
           :x *last-mouse-x*
           :y *last-mouse-y*
           :graft-x (pointer-x pointer)
           :graft-y (pointer-y pointer)
           :modifier-state *last-modifier-state*
           initargs)))

(defun pointer-motion-event (port sheet event)
  (declare (ignore event))
  (pointer-event port sheet 'pointer-motion-event))

(defun pointer-button-event (port sheet event)
  (let ((buttons (compute-mouse-buttons (mos:mouse-button-state event)))
        (change (compute-mouse-buttons (mos:mouse-button-change event))))
    (pointer-event port sheet
                   (if (= (logand buttons change) 0)
                       'pointer-button-release-event
                       'pointer-button-press-event)
                   :button change)))

(defun pointer-scroll-event (port sheet event)
  (let ((change (compute-mouse-buttons (mos:mouse-button-change event))))
    (pointer-event port sheet 'climi::pointer-scroll-event
                   :delta-y (case change
                              (#.+pointer-wheel-up+    2)
                              (#.+pointer-wheel-down+ -2)))))

(defun frame-mouse-event (sheet mez-frame event)
  (handler-case
      (mos:frame-mouse-event mez-frame event)
    (:no-error (&rest values)
      (declare (ignore values))
      nil)
    (mos:close-button-clicked ()
      (make-instance 'window-manager-delete-event :sheet sheet))))

(defun generate-window-configuration-event (sheet mez-window mez-frame)
  (multiple-value-bind (fl fr ft fb)
      (mos:frame-size mez-frame)
    (make-instance 'window-configuration-event
                   :sheet sheet
                   :region nil
                   ;; FIXME: These are one smaller than they should be.
                   ;; This is something to do with the window being created too small
                   ;; and getting adjusted by one pixel.
                   :width (- (mos:width mez-window) fl fr 1)
                   :height (- (mos:height mez-window) ft fb 1)
                   ;; FIXME: Window position updates aren't sent by the compositor
                   ;; when the window moves as windows are supposed to be
                   ;; position-independent, but mcclim is using absolute
                   ;; screen-relative coordinates for the mouse cursor and needs
                   ;; these to always be up to date.
                   :x (+ (mos:window-x mez-window) fl)
                   :y (+ (mos:window-y mez-window) ft))))

(defun mez-event->mcclim-event-mouse (port event mez-window sheet mez-frame mouse-x mouse-y)
  (cond ((and mez-frame
              (or (mos:in-frame-header-p mez-frame mouse-x mouse-y)
                  (mos:in-frame-border-p mez-frame mouse-x mouse-y)))
         (or (frame-mouse-event sheet mez-frame event)
             (when (plusp (mos:mouse-button-change event))
               (generate-window-configuration-event sheet mez-window mez-frame))))
        ((= (mos:mouse-button-change event) 0)
         (mezzano.gui.compositor:set-window-data mez-window :cursor :default)
         (pointer-motion-event port sheet event))

        ((member (compute-mouse-buttons (mos:mouse-button-change event)) '(#.+pointer-wheel-up+ #.+pointer-wheel-down+))
         (pointer-scroll-event port sheet event))

        (t
         (pointer-button-event port sheet event))))

(defmethod mez-event->mcclim-event (port (event mos:mouse-event))
  (let* ((pointer    (port-pointer port))
         (mez-window (mos:window event))
         (mouse-x    (mos:mouse-x-position event))
         (mouse-y    (mos:mouse-y-position event))
         (mez-mirror (port-lookup-mirror port mez-window))
         (sheet      (port-lookup-sheet port mez-window))
         (config-event nil))
    (when mez-mirror
      (with-slots (mez-frame dx dy width height) mez-mirror
        (when (or (not (eql (slot-value mez-mirror 'last-abs-x)
                            (mos:window-x mez-window)))
                  (not (eql (slot-value mez-mirror 'last-abs-y)
                            (mos:window-y mez-window))))
          ;; Window has moved, generate a window config event to put it in the right place.
          (setf config-event (generate-window-configuration-event
                              sheet mez-window mez-frame))
          (setf (slot-value mez-mirror 'last-abs-x) (mos:window-x mez-window)
                (slot-value mez-mirror 'last-abs-y) (mos:window-y mez-window)))
        (setf *last-mouse-x* mouse-x
              *last-mouse-y* mouse-y
              (pointer-x pointer) (+ mouse-x (mos:window-x mez-window))
              (pointer-y pointer) (+ mouse-y (mos:window-y mez-window)))
        (let ((evt (mez-event->mcclim-event-mouse port event mez-window sheet mez-frame mouse-x mouse-y)))
          (cond ((and config-event evt)
                 (list config-event evt))
                (config-event)
                (evt)))))))

;;;======================================================================
;;; Activation Events
;;;======================================================================

(defmethod mez-event->mcclim-event (port (event mezzano.gui.compositor:window-create-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror port mez-window))
         (sheet (port-lookup-sheet port mez-window)))
    (when mez-mirror
      ;; FIXME: Figure out what exactly is going on here. Two events seem to be needed to
      ;; get frames to actually do their initial paint properly.
      ;; The window configuration event sets the size & location of the frame appropriately
      ;; and can in some cases (coordinate issues?) trigger the initial draw properly.
      ;; Other times it doesn't trigger the draw and the frame remains undrawn!
      ;; Hence the second event to actually force a redraw.
      ;; This can lead to the frame being drawn twice, once per event, which causes flickering
      ;; and is lame.
      ;;
      ;; window-configuration-event only success:
      ;; 0: Enter HANDLE-EVENT (#<Mezzano-473929-Top-Level-Sheet-Pane CALCULATOR-APP 500025A6A289> #<Window-Configuration-Event 500025AACFD9>)
      ;; 1: Enter %SET-SHEET-REGION-AND-TRANSFORMATION (#<Mezzano-473929-Top-Level-Sheet-Pane CALCULATOR-APP 500025A6A289> #<Standard-Bounding-Rectangle X 0:80 Y 0:237> #<Standard-Translation :DX 472 :DY 274>)
      ;; 2: Enter (SETF SHEET-REGION) (#<Standard-Bounding-Rectangle X 0:80 Y 0:237> #<Mezzano-473929-Top-Level-Sheet-Pane CALCULATOR-APP 500025A6A289>)
      ;; new-region: #<Standard-Bounding-Rectangle X 0:80 Y 0:237>  old-region: #<Standard-Bounding-Rectangle X 0:81 Y 0:7607/32>
      ;; 3: Enter (SETF SHEET-REGION) (#<Standard-Bounding-Rectangle X 0:81 Y 0:237> #<Vrack-Pane NIL 500025A8A169>)
      ;; new-region: #<Standard-Bounding-Rectangle X 0:81 Y 0:237>  old-region: #<Standard-Bounding-Rectangle X 0:81 Y 0:7607/32>
      ;; Regions differ by a small fraction and trigger a repaint!
      ;;
      ;; Failure:
      ;; 0: Enter HANDLE-EVENT (#<Mezzano-473929-Top-Level-Sheet-Pane DEMODEMO 50002047EF69> #<Window-Configuration-Event 5000206C08D9>)
      ;; 1: Enter %SET-SHEET-REGION-AND-TRANSFORMATION (#<Mezzano-473929-Top-Level-Sheet-Pane DEMODEMO 50002047EF69> #<Standard-Bounding-Rectangle X 0:692 Y 0:662> #<Standard-Translation :DX 166 :DY 62>)
      ;; 2: Enter (SETF SHEET-REGION) (#<Standard-Bounding-Rectangle X 0:692 Y 0:662> #<Mezzano-473929-Top-Level-Sheet-Pane DEMODEMO 50002047EF69>)
      ;; new-region: #<Standard-Bounding-Rectangle X 0:692 Y 0:662>  old-region: #<Standard-Bounding-Rectangle X 0:354801/512 Y 0:679387/1024>
      ;; 3: Enter (SETF SHEET-REGION) (#<Standard-Bounding-Rectangle X 0:354801/512 Y 0:679387/1024> #<Vrack-Pane NIL 5000204A5909>)
      ;; new-region: #<Standard-Bounding-Rectangle X 0:354801/512 Y 0:679387/1024>  old-region: #<Standard-Bounding-Rectangle X 0:354801/512 Y 0:679387/1024>
      ;; 3: Leave (SETF SHEET-REGION) (NIL)
      ;;
      ;; Skip the config event for non-toplevel frames. They get resized and
      ;; generate a new, more appropriate, config event for that.
      (if (slot-value mez-mirror 'top-levelp)
          (list
           (generate-window-configuration-event sheet mez-window (slot-value mez-mirror 'mez-frame))
           (with-slots (width height) mez-mirror
             (make-instance 'window-repaint-event
                            :sheet sheet
                            :region (make-rectangle* 0 0 width height))))
          (with-slots (width height) mez-mirror
            (make-instance 'window-repaint-event
                           :sheet sheet
                           :region (make-rectangle* 0 0 width height)))))))

(defmethod mez-event->mcclim-event (port (event mos:window-activation-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror port mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (port-lookup-sheet port mez-window)))
    (when mez-frame
      (setf (mos:activep mez-frame) (mos:state event))
      (mos:draw-frame mez-frame)
      ;; Is there a lose focus event?
      (when (mos:state event)
        (make-instance 'window-manager-focus-event :sheet sheet)))))

(defmethod mez-event->mcclim-event (port (event mos:quit-event))
  (let* ((mez-window (mos:window event))
         (sheet (port-lookup-sheet port mez-window)))
    (when sheet
      (make-instance 'window-manager-delete-event
                     :sheet sheet))))

(defmethod mez-event->mcclim-event (port (event mos:window-close-event))
  ;;; TODO - what needs to happen here anything?
  )

;;;======================================================================
;;; Resize events
;;;======================================================================

(defmethod mez-event->mcclim-event (port (event mos:resize-request-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror port mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (fwidth (max *minimum-width* (mos:width event)))
         (fheight (max *minimum-height* (mos:height event))))
    (multiple-value-bind (dw dh) (size-deltas mez-mirror)
      (when (and mez-frame
                 (or (/= fwidth (mos:width mez-window))
                     (/= fheight (mos:height mez-window))))
        (let* ((surface (mos:make-surface fwidth fheight))
               (pixels (mos:surface-pixels surface))
               (width (- fwidth dw))
               (height(- fheight dh)))
          (mos:resize-frame mez-frame surface)
          (mos:resize-window mez-window surface :origin (mos:resize-origin event))

          (setf (slot-value mez-mirror 'mez-pixels) pixels
                (slot-value mez-mirror 'fwidth) fwidth
                (slot-value mez-mirror 'fheight) fheight
                (slot-value mez-mirror 'width) width
                (slot-value mez-mirror 'height) height)
          nil)))))

(defmethod mez-event->mcclim-event (port (event mos:resize-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror port mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (port-lookup-sheet port mez-window)))
    (when mez-frame
      (list
       (generate-window-configuration-event sheet mez-window mez-frame)
       (with-slots (width height) mez-mirror
         (make-instance 'window-repaint-event
                        :sheet sheet
                        :region (make-rectangle* 0 0 width height)))))))
