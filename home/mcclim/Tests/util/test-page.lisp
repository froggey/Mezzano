(cl:in-package #:clim-test-util)

(defun print-test-pages (stream &rest functions)
  (loop for (page . rest) on functions
        do (funcall page stream)
        when rest do (new-page stream)))

(defun %print-test-page (stream)
  (labels ((text (x y width)
             (surrounding-output-with-border (stream :shape :rounded
                                                     :background +salmon+)
               (with-temporary-margins (stream :left  `(:absolute ,x)
                                               :top   `(:absolute ,y)
                                               :right `(:absolute ,(+ y width)))
                 (with-end-of-line-action (stream :wrap)
                   (loop :for i :from 0 :to 30
                         :for size = (+ 2 i)
                         :do (with-drawing-options (stream :text-size i)
                               (princ i stream)))))))
           (wheel (x y r color-function)
             (loop :with di = .1
                   :for i :from 0 :to (* 2 pi) :by di
                   :for color = (funcall color-function (/ i 2 pi))
                   :do (draw-circle* stream x y r
                                     :start-angle i :end-angle (+ i di)
                                     :filled t :ink color))
             (draw-circle* stream x y r
                           :filled nil :ink +black+
                           :line-thickness 4 :line-dashes '(8 8)))
           (wheels (x y r)
             (let* ((cx1 (+ x r))
                    (cx2 (+ cx1 (* 2 r) 8))
                    (cy  y))
               (wheel cx1 cy r (lambda (hue) (make-ihs-color 1 hue 1)))
               (wheel cx2 cy r (lambda (i) (make-ihs-color i 0 0)))))
           (graph (x y width height)
             (let ((end (* 20 pi)))
               (with-translation (stream x (+ y (/ height 2)))
                 (with-scaling (stream (/ width (sqrt end)) (/ height 2))
                   (draw-polygon*
                    stream (loop :for i :from 0 :to end :by .1
                                 :collect (sqrt i)
                                 :collect (* (/ i end) (sin i)))
                    :filled nil :closed nil :line-thickness 2)))))
           (character-width (x y)
             (with-translation (stream x y)
               (loop :with width = (stream-character-width stream #\M)
                     :with height = (nth-value 1 (text-size stream "Ty"))
                     :for position :from 0 :by width
                     :for character :across "AILTMy"
                     :do (draw-text* stream (string character) position 0)
                         (draw-rectangle* stream position 0 (+ position width) (- height)
                                          :filled nil :ink +red+
                                          :line-dashes '(4 4) :line-thickness .3)))))
    (text 0 0 200)
    (wheels 208 0 80)
    (graph 0 -108 200 100)
    (character-width 0 140)))

(defun print-test-page-1 (stream)
  (with-room-for-graphics (stream :first-quadrant nil)
    (%print-test-page stream)))

(defun print-test-page-2 (stream)
  (loop for i from 0 to 100 by 10
        do (draw-line* stream i 0 i 100 :line-thickness 2 :ink +black+)
           (draw-line* stream 0 i 100 i :line-thickness 2 :ink +black+))

  (draw-polygon* stream '(172  22   228  40   227  59   264  60   256  80   277  88
                          319  54   336  58   345  43   353  55   345  71   370  86
                          384 124   375 141   393 159   398 208   416 237   410 258
                          397 252   314 302   333 351   380 389   341 426   350 461
                          324 452   280 471   252 462   240 474   172 448   166 460
                          132 457   140 410   160 378   116 368    92 346    79 307
                          94 295    82 252    90 229    84 204   113 201   112 162
                          129 142   130 104   157 102   174 118   182  96   204  96
                          186  58   196 50)
                 :ink +dark-green+)

  (draw-line* stream 0 0 100 100 :ink +red+ :line-thickness 6)
  (draw-line* stream 20 0 80 20)
  (draw-lines* stream #(320 0 380 20
                        320 30 380 60)
               :line-thickness 3 :ink +orange+)
  (draw-point* stream 200 100 :ink +blue+)
  (draw-points* stream #(10 150 30 150 50 150) :ink +brown+ :line-thickness 10)
  (draw-polygon* stream #(120 120
                          150 120
                          150 150
                          135 160
                          120 150)
                 :ink +green+ :filled t :closed t)
  (draw-rectangle* stream 180 50 220 70 :ink +pink+ :filled nil :line-thickness 4)

  (draw-rectangles* stream #(180 150 220 170
                             180 190 220 210)
                    :ink +purple+ :line-thickness 3 :filled nil)
  (let ((design
          (mcclim-bezier:make-bezier-curve*
           (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140))))
    (mcclim-bezier:draw-bezier-design* stream design
                                       :line-thickness 5
                                       :ink +orange+))
  (let ((design
          (mcclim-bezier:make-bezier-area*
           (list 34 44 34 128 147 44 47 156 34 128 50 50 34 44))))
    (mcclim-bezier:draw-bezier-design* stream design
                                       :line-thickness 4
                                       :ink +sea-green+))
  (draw-text* stream "Test Page" 170 200
              :text-style (make-text-style :fix :bold :huge))
  (draw-text* stream "Bogus" 250 250)
  (draw-ellipse* stream 160 110 30 0 0 10 :filled nil :ink +orange+ :filled nil)
  (draw-ellipse* stream 160 310 20 0 0 30 :filled nil :ink +blue+ :filled t))

(defun make-random-color ()
  (make-rgb-color (/ (random 255) 255)
                  (/ (random 255) 255)
                  (/ (random 255) 255)))

(defun print-test-page-3 (stream)
  (loop repeat 200
        do (draw-line* stream (random 600) (random 900)
                       (random 600) (random 900)
                       :ink (make-random-color))))

(defun print-test-page-4 (stream)
  (loop repeat 1000
        do (draw-point* stream (random 600) (random 900)
                        :ink (make-random-color)
                        :line-thickness (random 50))))

(defun print-test-page-5 (stream)
   (formatting-table (stream :x-spacing 50
                             :y-spacing 20)
     (formatting-row (stream)
       (formatting-cell (stream)
         (declare (ignore stream)))
       (formatting-cell (stream :align-x :center
                                :align-y :bottom
                                :min-height 100)
         (draw-text* stream "(Test Page)" 170 30
                     :text-style (make-text-style :fix :bold :huge))))
     (loop for i from 1 to 15
           do (formatting-row (stream)
                (formatting-cell (stream :align-x :right
                                         :align-y :center
                                         :min-width 100)
                  (draw-point* stream 0 0 :line-thickness i))
                (formatting-cell (stream :align-x :center
                                         :align-y :center)
                  (draw-line* stream 0 0 200 0
                              :line-thickness i
                              :line-dashes (list (* i 2) (round i 2))))
                (formatting-cell (stream :align-x :right
                                         :align-y :center)
                  (draw-text* stream (format nil "~D" i) 0 0
                              :text-style (make-text-style
                                           :sans-serif :bold :huge)))))))

(defun print-test-page-6 (stream)
   (with-translation (stream 540 75)
     (with-scaling (stream 3)
       (with-rotation (stream (/ pi 2))
         (clim:draw-rectangle* stream 10 10 200 150 :filled nil
                                                    :line-thickness 2)
         (clim:draw-line* stream 200 10 10 150)
         (clim:draw-point* stream 180 25)
         (clim:draw-circle* stream 100 75 40 :filled nil)
         (clim:draw-ellipse* stream 160 110 30 0 0 10 :filled nil)
         (clim:draw-ellipse* stream 160 110 10 0 0 30)
         (clim:draw-polygon* stream '(20 20 50 80 40 20) :filled nil)
         (clim:draw-polygon* stream '(30 90 40 110 20 110))))))

(defun draw-rosette (stream x y radius n &rest drawing-options)
  (loop with alpha = (/ (* 2 pi) n)
        and radius = (/ radius 2)
        for i below n
        do (apply #'draw-circle* stream
                  (+ (* radius (cos (* alpha i))) x)
                  (+ (* radius (sin (* alpha i))) y)
                  radius
                  :filled nil
                  drawing-options)))

(defun print-test-page-7 (stream)
  (draw-rosette stream 300 300 200 18 :ink +steel-blue+ :line-thickness 2))

(defun print-test-page-8 (stream)
  (with-text-style (stream '(:serif nil :huge))
    (draw-text* stream "Text alignment test" 170 20
                :text-family :sans-serif
                :text-face :bold)
    (with-scaling (stream 2)
      (loop for align-y in '(:bottom :center :top)
            and y from 200 by 50
            do (loop for align-x in '(:right :center :left)
                     and x from 100 by 50
                     do (draw-text* stream (format nil "~A~A"
                                                   (elt (symbol-name align-x) 0)
                                                   (elt (symbol-name align-y) 0))
                                    x y
                                    :align-x align-x
                                    :align-y align-y)
                        (draw-point* stream x y :ink +red+
                                                :line-thickness 3
                                                :line-unit :point))))
    (draw-text* stream "Top: pQ" 50 200 :align-y :top)
    (draw-text* stream "Bottom: pQ" 170 200 :align-y :bottom)
    (draw-text* stream "Center: pQ" 290 200 :align-y :center)
    (draw-text* stream "Baseline: pQ" 410 200 :align-y :baseline)
    (draw-line* stream 50 200 535 200 :ink +red+)))

(defun print-test-page-9 (stream)
   (formatting-table (stream)
     (flet ((draw (stream angle line-joint-shape)
              (let ((record
                      (with-output-to-output-record (stream)
                        (draw-polygon* stream (list 20 0 100 0 50 (* 50 (tan angle)))
                                       :closed nil
                                       :filled nil
                                       :line-thickness 40
                                       :line-joint-shape line-joint-shape
                                       :line-cap-shape :round)
                        (draw-polygon* stream (list 20 0 100 0 50 (* 50 (tan angle)))
                                       :closed nil
                                       :filled nil
                                       :line-thickness 0.01
                                       :ink +green+))))
                (multiple-value-call #'draw-rectangle*
                  stream (bounding-rectangle* record)
                  :filled nil
                  :ink +red+ :line-thickness 0.01)
                (stream-add-output-record stream record)
                (replay record stream))))
       (loop with dag = 2
             with da = (* pi (/ dag 180))
             for i from -10 to 10
             for a = (* i da)
             unless (= i 0)
             do (formatting-row (stream)
                  (formatting-cell (stream) (print (* i dag) stream))
                  (formatting-cell (stream) (draw stream a :miter))
                  (formatting-cell (stream) (draw stream a :bevel))
                  (formatting-cell (stream) (draw stream a :round)))))))

(defvar *all-test-pages* '(print-test-page-1
                           print-test-page-2
                           print-test-page-3
                           print-test-page-4
                           print-test-page-5
                           print-test-page-6
                           print-test-page-7
                           print-test-page-8
                           print-test-page-9))
