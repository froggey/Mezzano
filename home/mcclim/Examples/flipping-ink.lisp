(in-package clim-demo)

(define-application-frame flipping-ink ()
  ()
  ;(:geometry :min-width 100 :min-height 100)
  (:menu-bar nil)
  (:pane :application :display-function #'display :width 125 :height 125 :scroll-bars nil))

(defparameter *ink* (make-flipping-ink +red+ +blue+))

(defmethod display ((frame flipping-ink) pane)
  (draw-rectangle* pane 0 0 125 125 :ink +blue+)
  (draw-text* pane "JackDaniels" 10 10 :ink *ink* :align-y :top)
  (draw-text* pane "JackDaniels" 10 60 :ink *ink* :align-y :top)
  ;; (draw-rectangle* pane 25 25 100 100 :ink +red+)
  ;; (draw-rectangle* pane 35 35 90 90 :ink +black+)
  ;; (draw-rectangle* pane 5 5 85 85 :ink *ink*)
  (draw-rectangle* pane 60 60 120 120 :ink *ink*)
  (draw-rectangle* pane 40 40 80 80 :ink *ink*))
