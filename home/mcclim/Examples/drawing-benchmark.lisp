;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)

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

(in-package :clim-demo)

(define-application-frame drawing-benchmark ()
    ()
  (:panes
   (canvas :application
           :min-width 600
           :incremental-redisplay nil
           :display-time nil)
   (mode
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "rectangle" :id :rectangle))
      (make-pane 'toggle-button :label "text" :id :text)
      (make-pane 'toggle-button :label "text*" :id :text*)))
   (ink
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "random" :id :random))
      (make-pane 'toggle-button :label "red" :id +red+)
      (make-pane 'toggle-button :label "flipping ink" :id +flipping-ink+))))
  (:layouts
   (default
       (vertically ()
         (horizontally ()
           (labelling (:label "Mode") mode)
           (labelling (:label "Ink") ink))
         canvas))))

(defmethod run-drawing-benchmark (frame stream)
  (window-clear stream)
  (let* ((width (rectangle-width (sheet-region stream)))
         (height (rectangle-height (sheet-region stream)))
         (mode (gadget-id (gadget-value (find-pane-named frame 'mode))))
         (ink (gadget-id (gadget-value (find-pane-named frame 'ink))))
         (itups internal-time-units-per-second)
         (n 0)
         (start (get-internal-real-time))
         (stop (+ start (* 5 itups))))
    (do ()
        ((>= (get-internal-real-time) stop))
      (incf n)
      (let ((ink
             (if (eq ink :random)
                 (clim:make-rgb-color (random 1.0d0)
                                      (random 1.0d0)
                                      (random 1.0d0))
                 ink)))
        (with-output-recording-options (stream :record nil)
         (ecase mode
           (:rectangle
            (draw-rectangle* stream
                             10 10 (- width 10) (- height 10)
                             :ink ink
                             :filled t))
           (:text
            (dotimes (x 10)
              (draw-text* stream
                          "Bla blub hastenichgesehen noch viel mehr Text so fuellen wir eine Zeile."
                          0
                          (* x 20)
                          :ink ink)))
           (:text* ;; custom align-x/y triggers path which calls text-size
            (dotimes (x 10)
              (draw-text* stream
                          "Bla blub hastenichgesehen noch viel mehr Text so fuellen wir eine Zeile."
                          150
                          (* x 20)
                          :ink ink
                          :align-x :center
                          :align-y :center)))))))
    (finish-output stream)
    (medium-finish-output (sheet-medium stream))
    (climi::port-force-output (car climi::*all-ports*))
    (setf stop (get-internal-real-time))
    (window-clear stream)
    (format stream "Score: ~A operations/s~%" (float (/ n (/ (- stop start) itups))))
    (format *debug-io* "Score: ~A operations/s~%" (float (/ n (/ (- stop start) itups))))))

(define-drawing-benchmark-command (com-quit-drawing-benchmark :menu "Quit") ()
  (frame-exit *application-frame*))

(define-drawing-benchmark-command (com-run-drawing-benchmark :menu "Run") ()
  (run-drawing-benchmark *application-frame*
                         (frame-standard-output *application-frame*)))
