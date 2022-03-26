(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :mcclim))

(cl:in-package #:clim-user)

(define-application-frame hello-frame () ()
  (:pane (make-instance 'hello-data-pane
                        :hs 200 :hs+ +fill+ :vs 200 :vs+ +fill+))
  (:settings :title "Hello from Lisp"))

(define-application-pane hello-data-pane ());; inherits basic-clim-pane
;; by default ())
;;  Behavior defined via CLOS class specialization

(defmethod handle-repaint ((pane hello-data-pane) region &key &allow-other-keys)
  (declare (ignore region))
  (let*  ((w (bounding-rectangle-width pane))
          (h (bounding-rectangle-height pane)))
    ;;  Blank the pane out
    (draw-rectangle* pane 0 0 w h :filled t :ink (pane-background pane))
    ;;  Center the label
    (draw-text* pane "Hello" (floor w 2) (floor h 2) :align-x :center :align-y :center)))

(defmethod button-release ((pane hello-data-pane)
                           (button-name (eql  :right))
                           &key x y &allow-other-keys)
  (draw-point* pane x y))
