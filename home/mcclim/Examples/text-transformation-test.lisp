(in-package #:clim-demo)

(define-application-frame text-transformations-test ()
  ()
  (:menu-bar nil)
  (:pane :application :display-function #'display :scroll-bars nil))

(defmethod display ((frame text-transformations-test) pane)
  (let* ((string "aTΣ音◌᷉")
         (y-offset 75)
         (x-offset 75)
         (text-style (make-text-style nil nil 18)))
    #+ (or)
    (surrounding-output-with-border (pane)
      ;; XXX: text-editor nor text-field call value-changed-callback. Probably
      ;; due to Drei mishmash.
      (with-output-as-gadget (pane)
        (make-pane 'text-editor :value "hello" :ncolumns 40 :nlines 2
                   :value-changed-callback (lambda (gadget client &optional id value)
                                             (declare (ignore gadget client id value))
                                             (log:error "HI")))))
    (with-drawing-options (pane :transformation (make-translation-transformation x-offset y-offset)
                                :text-style text-style)

      (clim:draw-point* pane 0 0 :ink +blue+ :line-thickness 10)
      (clim:draw-line* pane 0 0 0 (* 7 y-offset))
      (clim:draw-line* pane 0 0 (* 9 x-offset) 0)
      (clim:draw-line* pane (* 3 x-offset) 0 (* 3 x-offset) (* 7 y-offset))
      (clim:draw-line* pane (* 6 x-offset) 0 (* 6 x-offset) (* 7 y-offset))
      ;; Reference
      (clim:draw-text* pane string 0 0 :transform-glyphs nil :ink +dark-red+)

      ;; Multiline
      (with-drawing-options (pane :transformation (compose-translation-with-transformation
                                                   (clim:make-rotation-transformation
                                                    (- (/ pi 2)
                                                       (/ pi 32)))
                                                   (* 8 x-offset) (/ y-offset 2))
                                  :text-style text-style)
        (clim:draw-point* pane 0 0 :ink +blue+ :line-thickness 10)
        ;; XXX: two ~%~% cause error (empty line)
        (let ((string "The quick brown fox jumps over the lazy dog"))
          (clim:draw-text* pane (format nil "~A~%~A~%~A" string string string)
                           0 0 :transform-glyphs t :ink +dark-red+))
        (clim:draw-line* pane 0 0 500 0))

      ;; Multiline with towards
      (with-drawing-options (pane :transformation (compose-translation-with-transformation
                                                   (clim:make-rotation-transformation
                                                    (/ pi 32))
                                                   0 (* 8 y-offset))
                                  :text-style text-style)
        (let ((toward-x (* 8 x-offset))
              (toward-y (* -1 y-offset)))
         (clim:draw-point* pane 0 0 :ink +blue+ :line-thickness 10)
         (clim:draw-point* pane toward-x toward-y :ink +red+ :line-thickness 10)
         (clim:draw-line* pane 0 0 toward-x toward-y :ink +magenta+)
         (let ((string "The quick brown fox jumps over the lazy dog"))
           (clim:draw-text* pane (format nil "~A~%~A~%~A" string string string)
                            0 0 :transform-glyphs t :ink +gray+))
         (let ((string "The quick brown fox jumps over the lazy dog"))
           (clim:draw-text* pane (format nil "~A~%~A~%~A" string string string)
                            0 0 :transform-glyphs t :ink +dark-red+
                            :toward-x toward-x
                            :toward-y toward-y)))
        (clim:draw-line* pane 0 0 500 0))
      (do* ((scalings-x '(+1.5 +0.7 -1.0 -1.5 -0.7 +1.5 -1.5) (cdr scalings-x))
            (scalings-y '(+1.5 +0.7 -1.0 -1.5 -0.7 -1.5 +1.5) (cdr scalings-y))
            (toward-pairs (list (cons (* +2/3 x-offset) (* +2/3 x-offset))
                                (cons nil (* 2/3 y-offset))
                                (cons (* -2/3 x-offset) (* -2/3 x-offset))
                                (cons (* -1/2 x-offset) nil)
                                (cons (* -2/3 x-offset) (* +2/3 x-offset))
                                (cons (* +2/3 x-offset) (* -2/3 x-offset))
                                (cons x-offset nil))
                          (cdr toward-pairs))
            (pair #3=(car toward-pairs) #3#)
            (sx #1=(car scalings-x) #1#)
            (sy #2=(car scalings-y) #2#)
            (v 1 (1+ v)))
           ((= v 8))

        ;; Towards
        (with-drawing-options (pane :transformation (make-translation-transformation
                                                     (* v x-offset) 0))
         (let ((dx (or (car pair) 0))
               (dy (cdr pair)))
           (clim:draw-point* pane sx sy :ink +blue+ :line-thickness 10)
           (clim:draw-point* pane (or dx 0) (or dy 0) :ink +red+ :line-thickness 10)
           (clim:draw-line* pane sx sy (or dx 0) (or dy 0) :ink +magenta+)
           (clim:draw-text* pane string sx sy :transform-glyphs t :ink +grey+)
           (clim:draw-text* pane string sx sy :transform-glyphs t :ink +dark-red+
                            :toward-x dx
                            :toward-y dy)))
        ;; Rotations
        (with-drawing-options (pane :transformation (compose-translation-with-transformation
                                                     (clim:make-scaling-transformation sx sy)
                                                     (* 3 x-offset) (* v y-offset))
                                    :text-style text-style)
          (clim:draw-point* pane 0 0 :ink +blue+ :line-thickness 10)
          (clim:draw-text* pane string 0 0 :transform-glyphs t :ink +dark-red+)
          (clim:draw-line* pane 0 0 50 0))
        ;; Scaling
        (with-drawing-options (pane :transformation (compose-translation-with-transformation
                                                     (make-rotation-transformation (* v (/ pi 6)))
                                                     0 (* v y-offset))
                                    :text-style text-style)
          (clim:draw-point* pane 0 0 :ink +blue+ :line-thickness 10)
          (clim:draw-text* pane string 0 0 :transform-glyphs t :ink +dark-red+)
          (clim:draw-line* pane 0 0 50 0))
        ;; Combination of previous rotation and scaling (grey) + towards (red)
        (with-drawing-options (pane
                               :transformation (compose-translation-with-transformation
                                                ;(clim:make-scaling-transformation sx sy)
                                                (compose-transformations
                                                 (clim:make-scaling-transformation sx sy)
                                                  (make-rotation-transformation (* v (/ pi 6))))
                                                (* 6 x-offset) (* v y-offset))
                               :text-style text-style)
          (let ((dx (car pair))
                (dy (cdr pair)))
            (clim:draw-point* pane 0 0 :ink +blue+ :line-thickness 10)
            (clim:draw-point* pane (or dx 0) (or dy 0) :ink +red+ :line-thickness 10)
            ;; without towards is gray
            (clim:draw-line* pane 0 0 50 0)
            (clim:draw-text* pane string 0 0 :transform-glyphs t :ink +grey+)
            ;; with towards is red
            (clim:draw-line* pane 0 0 (or dx 0) (or dy 0) :ink +magenta+)
            (clim:draw-text* pane string 0 0 :transform-glyphs t :ink +dark-red+
                             :toward-x dx
                             :toward-y dy)))))))

(define-text-transformations-test-command (com-refresh-text-transform :keystroke #\space) ())
