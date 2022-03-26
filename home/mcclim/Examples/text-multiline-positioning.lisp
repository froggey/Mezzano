(in-package #:clim-demo)

(define-application-frame text-multiline-positioning ()
  ()
  (:menu-bar nil)
  (:pane :application :display-function #'display :scroll-bars nil))

(defun draw-lazy (pane align-x align-y)
  (let* ((string* (format nil
                          "~A~%~A~%~A~%~A~%~%~s~%~A"
                          "First line of text."
                          "The quick brown fox jumps over"
                          "the lazy dog. Belive it or not."
                          "Below is an empty line:"
                          ;; <intentionally empty>
                          (list :align-x align-x :align-y align-y)
                          "Last line of text. ytmMΣ音!")))
    ;; XXX: surrounding-output-with-border makes transformed text drawing MANY
    ;; times slower when replaying. This may be indicator, that we redraw things
    ;; too much when we manipulate output records. Also for some reason border
    ;; is a little off despite text-bounding-rectangle* being correct (uncomment
    ;; Core/clim-basic/recording.lisp:draw-text for a hack which shows the
    ;; bounding rectangle).
    (surrounding-output-with-border (pane :line-dashes t :line-thickness 1
                                          :padding-x 0
                                          :padding-y 0)
      (clim:draw-text* pane string*
                       0 0 :transform-glyphs t
                       :align-x align-x :align-y align-y))
    (clim:draw-line* pane -5 0 5 0 :ink +blue+ :line-thickness 1)
    (clim:draw-line* pane 0 -5 0 5 :ink +blue+ :line-thickness 1)
    (clim:draw-rectangle* pane -1 -1 2 2 :ink +dark-red+)))

(defmethod display ((frame text-multiline-positioning) pane)

  (with-drawing-options (pane :text-style (make-text-style nil nil 12)
                              ;; this is also good, but the test is more clear
                              ;; to understand without transformations.
                              #|:transformation (make-rotation-transformation (/ pi 4))|#)
    (let* ((align-x '((:left . 50) (:center  . 425) (:right  . 800)))
           (align-y '((:top  . 50)  (:center . 250) (:bottom . 450)))
           (align-xy '((:left :baseline 50 650)
                       (:center :baseline* 425 650)
                       (:right :baseline 800 650)))
           (rotated `((:right :center 50 850 ,(make-scaling-transformation -1.1 0.8))
                      (:center :center 425 850 ,(make-rotation-transformation (/ pi 4)))
                      (:center :top 800 850 ,(make-rotation-transformation (/ pi 2))))))
      (dolist (ax align-x)
        (dolist (ay align-y)
          (with-translation (pane (cdr ax) (cdr ay))
            (draw-lazy pane (car ax) (car ay)))))
      (dolist (axy align-xy)
        (with-translation (pane (third axy) (fourth axy))
          (draw-lazy pane (first axy) (second axy))))
      (dolist (axy rotated)
        (with-translation (pane (third axy) (fourth axy))
          (with-drawing-options (pane :transformation (fifth axy))
           (draw-lazy pane (first axy) (second axy))))))))

(define-text-multiline-positioning-command (com-refresh-multiline :keystroke #\space) ())
