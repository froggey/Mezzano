(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :mcclim))

(cl:in-package #:clim-user)

; LTAG-start:hello-world-def-app
(define-application-frame hello-world ()
  ((greeting :initform "Hello World"
             :accessor greeting))
  (:pane (make-pane 'hello-world-pane)))
; LTAG-end

; LTAG-start:hello-world-defclass
(defclass hello-world-pane
  (clim-stream-pane) ())
; LTAG-end
; LTAG-start:hello-world-handle-repaint
;;; Behavior defined by the Handle Repaint Protocol
(defmethod handle-repaint ((pane hello-world-pane) region)
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    ;; Blank the pane out
    (draw-rectangle* pane 0 0 w h
                     :filled t
                     :ink (pane-background pane))
    ;; Draw greeting in center of pane
    (draw-text* pane
                (greeting *application-frame*)
                (floor w 2) (floor h 2)
                :align-x :center
                :align-y :center)))
; LTAG-end
