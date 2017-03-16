;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;; Simple tool for visually displaying how physical memory is used.

(defpackage :mezzano.gui.memory-monitor
  (:use :cl :mezzano.gui.font)
  (:export #:spawn))

(in-package :mezzano.gui.memory-monitor)

(defclass memory-monitor ()
  ((%frame :initarg :frame :accessor frame)
   (%window :initarg :window :accessor window)
   (%fifo :initarg :fifo :accessor fifo)))

(defgeneric dispatch-event (app event)
  (:method (f e)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:key-event))
  (when (not (mezzano.gui.compositor:key-releasep event))
    (let* ((ch (mezzano.gui.compositor:key-key event)))
      (cond ((char= ch #\Space)
             ;; refresh current window
             (throw 'redraw nil))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame app)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame app)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event (frame app) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

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

(defun update-display (fb x y w h)
  (let* ((n-pixels (* w h))
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
                (#x000 (mezzano.gui:make-colour 0 0 0)) ; not present.
                (#x001 (mezzano.gui:make-colour-from-octets 53 148 254)) ; free
                (#x002 (mezzano.gui:make-colour-from-octets 248 8 23)) ; wired
                (#x004 (mezzano.gui:make-colour-from-octets 143 80 10)) ; wired-backing
                (#x008 (mezzano.gui:make-colour-from-octets 147 253 21)) ; active
                (#x010 (mezzano.gui:make-colour-from-octets 81 145 7)) ; active-writeback
                (#x020 (mezzano.gui:make-colour-from-octets 82 9 146)) ; inactive-writeback
                (#x040 (mezzano.gui:make-colour-from-octets 251 131 216)) ; page-table
                (#x100 (mezzano.gui:make-colour-from-octets 121 121 121)) ; other
                (t (mezzano.gui:make-colour 1 1 1)))))))) ; mixed

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
                                     :frame frame)))
            (mezzano.gui.widgets:draw-frame frame)
            (mezzano.gui.compositor:damage-window window
                                                  0 0
                                                  (mezzano.gui.compositor:width window)
                                                  (mezzano.gui.compositor:height window))
            (loop
               (multiple-value-bind (left right top bottom)
                   (mezzano.gui.widgets:frame-size frame)
                 (let ((framebuffer (mezzano.gui.compositor:window-buffer window))
                       (width (- (mezzano.gui.compositor:width window) left right))
                       (height (- (mezzano.gui.compositor:height window) top bottom)))
                   ;; FIXME: Should do this in a seperate thread.
                   (update-display framebuffer left top width height)
                   (mezzano.gui.compositor:damage-window window
                                                         left top
                                                         width height)
                   (catch 'redraw
                     (loop
                        (dispatch-event app (mezzano.supervisor:fifo-pop fifo)))))))))))))

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
