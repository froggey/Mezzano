;;;; Memory monitor
;;;;
;;;; Visually displays how physical memory is used and for monitoring
;;;; virtual memory usage.

(defpackage :mezzano.gui.memory-monitor
  (:use :cl :mezzano.gui.font)
  (:export #:spawn)
  (:local-nicknames (:gui :mezzano.gui)
                    (:comp :mezzano.gui.compositor)
                    (:theme :mezzano.gui.theme)
                    (:sync :mezzano.sync)
                    (:sup :mezzano.supervisor)
                    (:int :mezzano.internals)))

(in-package :mezzano.gui.memory-monitor)

(defclass graph-sampler ()
  ((%sample-function :initarg :function :reader sampler-function)
   (%colour :initarg :colour :reader sampler-colour)
   (%name :initarg :name :reader sampler-name)
   (%history :initform nil :accessor sampler-history)
   (%scale :initarg :scale :accessor sampler-scale)
   (%autoscale :initform 0 :accessor sampler-autoscale)
   (%last-scale :initform 0 :accessor sampler-last-scale))
  (:default-initargs :scale nil))

(defclass memory-monitor ()
  ((%frame :initarg :frame :accessor frame)
   (%window :initarg :window :accessor window)
   (%fifo :initarg :fifo :accessor fifo)
   (%font :initarg :font :accessor font)
   (%mode :initarg :mode :accessor mode)
   (%samplers :initarg :samplers :accessor samplers)
   (%graph-column :initform 0 :accessor graph-column)))

(defgeneric dispatch-event (app event)
  (:method (f e)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:key-event))
  (when (not (mezzano.gui.compositor:key-releasep event))
    (let* ((ch (mezzano.gui.compositor:key-key event)))
      (cond ((char-equal ch #\P)
             (setf (mode app) :physical-visualizer)
             (throw 'redraw nil))
            ((char-equal ch #\G)
             (setf (mode app) :graphs)
             (throw 'redraw nil))
            ((char-equal ch #\Space)
             ;; refresh current window
             (throw 'redraw nil))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame app)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame app)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event (frame app) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil)))
  ;; Tab clicky clicky.
  (when (and (not (logbitp 0 (mezzano.gui.compositor:mouse-button-state event)))
             (logbitp 0 (mezzano.gui.compositor:mouse-button-change event)))
    (multiple-value-bind (left right top bottom)
        (mezzano.gui.widgets:frame-size (frame app))
      (declare (ignore bottom))
      (let* ((mx (mezzano.gui.compositor:mouse-x-position event))
             (my (mezzano.gui.compositor:mouse-y-position event))
             (fb (mezzano.gui.compositor:window-buffer
                  (window app)))
             (width (- (mezzano.gui:surface-width fb) left right))
             (half-width (truncate width 2))
             (line-height (mezzano.gui.font:line-height (font app))))
        (when (and (<= left mx (+ left half-width))
                   (<= top my (+ top line-height)))
          (setf (mode app) :graphs)
          (throw 'redraw nil))
        (when (and (<= (+ left half-width) mx (+ left width right))
                   (<= top my (+ top line-height)))
          (setf (mode app) :physical-visualizer)
          (throw 'redraw nil))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:window-close-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event mezzano.gui.compositor:quit-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-request-event))
  (let ((old-width (mezzano.gui.compositor:width (window app)))
        (old-height (mezzano.gui.compositor:height (window app)))
        (new-width (max 100 (mezzano.gui.compositor:width event)))
        (new-height (max 100 (mezzano.gui.compositor:height event))))
    (when (or (not (eql old-width new-width))
              (not (eql old-height new-height)))
      (let ((new-framebuffer (mezzano.gui:make-surface
                              new-width new-height)))
        (mezzano.gui.widgets:resize-frame (frame app) new-framebuffer)
        (mezzano.gui.compositor:resize-window
         (window app) new-framebuffer
         :origin (mezzano.gui.compositor:resize-origin event))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-event))
  (throw 'redraw nil))

(defun get-maximum-physical-address ()
  ;; Iterate the memory map. Highest address wins.
  (let ((max-addr 0))
    (dotimes (i (mezzano.supervisor::n-memory-map-entries) nil)
      (setf max-addr (max max-addr (mezzano.supervisor::memory-map-entry-end i))))
    max-addr))

(defun get-page-flags (page max-addr)
  (cond ((and (< page max-addr)
              (mezzano.supervisor::physical-page-exists (truncate page #x1000)))
         (case (mezzano.supervisor::physical-page-frame-type (truncate page #x1000))
           (:free               #x001)
           (:wired              #x002)
           (:wired-backing      #x004)
           (:active             #x008)
           (:active-writeback   #x010)
           (:inactive-writeback #x020)
           (:page-table         #x040)
           (:other              #x100)
           (:other-external     0)
           (t                   #x200)))
        (t 0)))

(defun draw-tab-bar (app)
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (frame app))
    (declare (ignore bottom))
    (let* ((fb (mezzano.gui.compositor:window-buffer
                (window app)))
           (width (- (mezzano.gui:surface-width fb) left right))
           (half-width (truncate width 2))
           (font (font app))
           (line-height (mezzano.gui.font:line-height font)))
      (mezzano.gui:bitset :set
                          width 1
                          mezzano.gui.theme:*foreground*
                          fb left (+ top line-height))
      (multiple-value-bind (graphs-colour pv-colour)
          (ecase (mode app)
            (:physical-visualizer
             (values mezzano.gui.theme:*background* mezzano.gui.theme:*foreground*))
            (:graphs
             (values mezzano.gui.theme:*foreground* mezzano.gui.theme:*background*)))
        (mezzano.gui:bitset :set
                            half-width line-height
                            graphs-colour
                            fb left top)
        (mezzano.gui:bitset :set
                            half-width line-height
                            pv-colour
                            fb (+ left half-width) top)
        (flet ((draw-string* (string offset colour)
                 (let ((swidth (string-display-width string font)))
                   (draw-string string font
                                fb
                                (+ left offset (truncate half-width 2) (- (truncate swidth 2)))
                                (+ top (mezzano.gui.font:ascender font))
                                colour))))
          (draw-string* "Graphs" 0 pv-colour)
          (draw-string* "Physical Vizualizer" half-width graphs-colour)))
      (1+ (mezzano.gui.font:line-height font)))))

(defun update-display (app x y w h)
  (let ((tb-height (draw-tab-bar app)))
    (incf y tb-height)
    (decf h tb-height))
  (let* ((fb (mezzano.gui.compositor:window-buffer
              (window app)))
         (n-pixels (* w h))
         (flag-array (make-array n-pixels :initial-element 0)))
    ;; Get page flags for each pixel.
    (mezzano.supervisor:with-pseudo-atomic ()
      ;; No allocation within the pa region!
      (let* ((max-address (get-maximum-physical-address))
             (highest-interesting-address 0))
        ;; Dry run - discover the highest interesting address.
        (dotimes (page (ceiling max-address #x1000))
          (let ((flag (get-page-flags (* page #x1000) max-address)))
            (when (and (not (eql flag 0)) (not (eql flag #x200)))
              (setf highest-interesting-address (* page #x1000)))))
        ;; The real deal.
        (let ((pages-per-pixel (ceiling (ceiling highest-interesting-address #x1000) n-pixels)))
          (dotimes (i n-pixels)
            (let ((page-flags 0))
              (dotimes (j pages-per-pixel)
                (setf page-flags (logior (get-page-flags (* (+ j (* i pages-per-pixel)) #x1000) max-address)
                                         page-flags)))
              (setf (svref flag-array i) page-flags))))))
    ;; Now blast to the framebuffer.
    (dotimes (py h)
      (dotimes (px w)
        (setf (mezzano.gui:surface-pixel fb (+ x px) (+ y py))
              (case (svref flag-array (+ (* py w) px))
                (#x000 theme:*memory-monitor-not-present*)
                (#x001 theme:*memory-monitor-free*)
                (#x002 theme:*memory-monitor-wired*)
                (#x004 theme:*memory-monitor-wired-backing*)
                (#x008 theme:*memory-monitor-active*)
                (#x010 theme:*memory-monitor-active-writeback*)
                (#x020 theme:*memory-monitor-inactive-writeback*)
                (#x040 theme:*memory-monitor-page-table*)
                (#x100 theme:*memory-monitor-other*)
                (t theme:*memory-monitor-mixed*)))))))

(defparameter *graph-background-colour*
  theme:*memory-monitor-graph-background*)

(defparameter *graph-tracker-colour*
  theme:*memory-monitor-graph-tracker*)

(defparameter *graph-update-interval* 1/4)

(defun update-sampler (sampler column)
  (let ((history (sampler-history sampler))
        (value (funcall (sampler-function sampler))))
    (setf (aref history column) value)
    (when (eql (sampler-scale sampler) t)
      ;; Update autoscale limits.
      (setf (sampler-autoscale sampler)
            (max 1 (reduce #'max history :key (lambda (n) (or n 0))))))))

(defun draw-sampler-incremental (sampler column fb x y graph-height)
  (let* ((history (sampler-history sampler))
         (scale (case (sampler-scale sampler)
                  ((nil) 1)
                  ((t) (sampler-autoscale sampler))
                  (otherwise (sampler-scale sampler))))
         (raw-value (aref history column))
         (raw-last (or (aref history (mod (1- column) (length history)))
                       raw-value))
         (value (- 1 (min 1 (max 0 (/ (float raw-value) scale)))))
         (last (- 1 (min 1 (max 0 (/ (float raw-last) scale)))))
         (from (truncate (* (min value last) graph-height)))
         (to (truncate (* (max value last) graph-height))))
      (dotimes (i (- to from))
        (setf (gui:surface-pixel fb (+ x column) (+ y from i))
              (sampler-colour sampler)))
      (setf (gui:surface-pixel fb (+ x column) (+ y to))
            (sampler-colour sampler))))

(defun draw-sampler-full (sampler current-column fb x y graph-height)
  (let ((history (sampler-history sampler))
        (scale (case (sampler-scale sampler)
                 ((nil) 1)
                 ((t) (sampler-autoscale sampler))
                 (otherwise (sampler-scale sampler)))))
    (dotimes (column (length (sampler-history sampler)))
      (let ((raw-value (aref history column)))
        (when (not raw-value)
          (return-from draw-sampler-full))
        (let* ((raw-last (or (aref history (mod (1- column) (length history)))
                             raw-value))
               (value (- 1 (min 1 (max 0 (/ (float raw-value) scale)))))
               (last (- 1 (min 1 (max 0 (/ (float raw-last) scale)))))
               (from (truncate (* (min value last) graph-height)))
               (to (truncate (* (max value last) graph-height))))
          (dotimes (i (- to from))
            (setf (gui:surface-pixel fb (+ x column) (+ y from i))
                  (sampler-colour sampler)))
          (setf (gui:surface-pixel fb (+ x column) (+ y to))
                (sampler-colour sampler)))))))

(defun graph-main-loop (app)
  (sup:with-timer (timer :relative 0 :name "Memory monitor graph update")
    (multiple-value-bind (left right top bottom)
        (mezzano.gui.widgets:frame-size (frame app))
      (let* ((fb (mezzano.gui.compositor:window-buffer (window app)))
             (width (gui:surface-width fb))
             (height (gui:surface-height fb))
             (need-full-redraw t))
        (gui:bitset :set (- width left right) (- height top bottom)
                    *graph-background-colour*
                    fb left top)
        (mezzano.gui.compositor:damage-window
         (window app) left top (- width left right) (- height top bottom))
        (incf top (draw-tab-bar app))
        (dolist (sampler (samplers app))
          (when (and (sampler-history sampler)
                     (eql (length (sampler-history sampler)) (- width left right)))
            (return))
          (setf (graph-column app) 0)
          (setf (sampler-history sampler) (make-array (- width left right) :initial-element nil)))
        (loop
           (loop
              (multiple-value-bind (event validp)
                  (sync:mailbox-receive (fifo app) :wait-p nil)
                (when (not validp) (return))
                (dispatch-event app event)))
           (when (sup:timer-expired-p timer)
             (sup:timer-arm *graph-update-interval* timer)
             (dolist (sampler (samplers app))
               (update-sampler sampler (graph-column app))
               (when (not (eql (if (eql (sampler-scale sampler) t)
                                   (sampler-autoscale sampler)
                                   (sampler-scale sampler))
                               (sampler-last-scale sampler)))
                 (setf need-full-redraw t)))
             (cond (need-full-redraw
                    (gui:bitset :set (- width left right) (- height top bottom)
                                *graph-background-colour*
                                fb left top)
                    (dolist (sampler (samplers app))
                      (setf (sampler-last-scale sampler) (if (eql (sampler-scale sampler) t)
                                                             (sampler-autoscale sampler)
                                                             (sampler-scale sampler)))
                      (draw-sampler-full sampler (graph-column app) fb left top (- height top bottom)))
                    (setf need-full-redraw nil))
                   (t
                    (gui:bitset :set 1 (- height top bottom)
                                *graph-background-colour*
                                fb (+ left (graph-column app)) top)
                    (dolist (sampler (samplers app))
                      (draw-sampler-incremental sampler (graph-column app) fb left top (- height top bottom)))))
             (incf (graph-column app))
             (when (>= (graph-column app) (- width left right))
               (setf (graph-column app) 0))
             (gui:bitset :set 1 (- height top bottom)
                         *graph-tracker-colour*
                         fb (+ left (graph-column app))  top)
             (mezzano.gui.compositor:damage-window
              (window app) left top (- width left right) (- height top bottom)))
           (sync:wait-for-objects timer (fifo app)))))))

(defun general-area-usage ()
  (multiple-value-bind (used commit)
      (int::area-usage :general)
    (/ (float used) commit)))

(defun general-area-alloc ()
  (nth-value 0 (int::area-usage :general)))

(defun general-area-commit ()
  (nth-value 1 (int::area-usage :general)))

(defun cons-area-usage ()
  (multiple-value-bind (used commit)
      (int::area-usage :cons)
    (/ (float used) commit)))

(defun cons-area-alloc ()
  (nth-value 0 (int::area-usage :cons)))

(defun cons-area-commit ()
  (nth-value 1 (int::area-usage :cons)))

(defun pinned-area-usage ()
  (multiple-value-bind (used commit)
      (int::area-usage :pinned)
    (/ (float used) commit)))

(defun wired-area-usage ()
  (multiple-value-bind (used commit)
      (int::area-usage :wired)
    (/ (float used) commit)))

(defun function-area-usage ()
  (multiple-value-bind (used commit)
      (int::area-usage :function)
    (/ (float used) commit)))

(defun wired-function-area-usage ()
  (multiple-value-bind (used commit)
      (int::area-usage :wired-function)
    (/ (float used) commit)))

(defun make-graph-samplers ()
  (list (make-instance 'graph-sampler
                       :function 'general-area-usage
                       :colour theme:*memory-monitor-general-area-usage*
                       :name "General area usage")
        (make-instance 'graph-sampler
                       :function 'general-area-alloc
                       :colour theme:*memory-monitor-general-area-alloc*
                       :name "General area bytes allocated"
                       :scale t)
        (make-instance 'graph-sampler
                       :function 'general-area-commit
                       :colour theme:*memory-monitor-general-area-commit*
                       :name "General area bytes committed"
                       :scale t)
        (make-instance 'graph-sampler
                       :function 'cons-area-usage
                       :colour theme:*memory-monitor-cons-area-usage*
                       :name "Cons area usage")
        (make-instance 'graph-sampler
                       :function 'cons-area-alloc
                       :colour theme:*memory-monitor-cons-area-alloc*
                       :name "Cons area bytes allocated"
                       :scale t)
        (make-instance 'graph-sampler
                       :function 'cons-area-commit
                       :colour theme:*memory-monitor-cons-area-commit*
                       :name "Cons area bytes committed"
                       :scale t)
        (make-instance 'graph-sampler
                       :function 'pinned-area-usage
                       :colour theme:*memory-monitor-pinned-area-usage*
                       :name "Pinned area usage")
        (make-instance 'graph-sampler
                       :function 'wired-area-usage
                       :colour theme:*memory-monitor-wired-area-usage*
                       :name "Wired area usage")
        (make-instance 'graph-sampler
                       :function 'function-area-usage
                       :colour theme:*memory-monitor-function-area-usage*
                       :name "Function area usage")
        (make-instance 'graph-sampler
                       :function 'wired-function-area-usage
                       :colour theme:*memory-monitor-wired-function-area-usage*
                       :name "Wired function area usage")))

(defun main (open-width open-height)
  (with-simple-restart (abort "Close memory monitor")
    (catch 'quit
      (let ((fifo (mezzano.supervisor:make-fifo 50)))
        (mezzano.gui.compositor:with-window (window fifo (or open-width 500) (or open-height 500))
          (let* ((frame (make-instance 'mezzano.gui.widgets:frame
                                       :framebuffer (mezzano.gui.compositor:window-buffer window)
                                       :title "Memory monitor"
                                       :close-button-p t
                                       :resizablep t
                                       :damage-function (mezzano.gui.widgets:default-damage-function window)
                                       :set-cursor-function (mezzano.gui.widgets:default-cursor-function window)))
                 (app (make-instance 'memory-monitor
                                     :fifo fifo
                                     :window window
                                     :frame frame
                                     :font (mezzano.gui.font:open-font
                                            mezzano.gui.font:*default-monospace-font*
                                            mezzano.gui.font:*default-monospace-font-size*)
                                     :mode :graphs
                                     :samplers (make-graph-samplers))))
            (setf (mezzano.gui.compositor:name window) app)
            (mezzano.gui.widgets:draw-frame frame)
            (mezzano.gui.compositor:damage-window window
                                                  0 0
                                                  (mezzano.gui.compositor:width window)
                                                  (mezzano.gui.compositor:height window))
            (loop
               (multiple-value-bind (left right top bottom)
                   (mezzano.gui.widgets:frame-size frame)
                 (let ((width (- (mezzano.gui.compositor:width window) left right))
                       (height (- (mezzano.gui.compositor:height window) top bottom)))
                   (catch 'redraw
                     (ecase (mode app)
                       (:physical-visualizer
                        (update-display app left top width height)
                        (mezzano.gui.compositor:damage-window window
                                                              left top
                                                              width height)
                        (loop
                           (dispatch-event app (mezzano.supervisor:fifo-pop fifo))))
                       (:graphs
                        (graph-main-loop app)))))))))))))

(defun spawn (&optional width height)
  (mezzano.supervisor:make-thread (lambda () (main width height))
                                  :name "Memory monitor"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Memory monitor"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
