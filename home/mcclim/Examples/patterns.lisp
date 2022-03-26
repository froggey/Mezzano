;;; --------------------------------------------------------------------------------------
;;;     Title: Pattern and design-related demos
;;;   Created: 2018-08-08
;;;    Author: Daniel Kochmański <daniel@turtleware.eu>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2018 by Daniel Kochmański

(in-package :clim-demo)

;;; uniform designs arranged in a pattern
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *text-right-margin* 380)
  (defparameter *block-height* 220)
  (defparameter *total-height* 1280)

  (let* ((array (make-array '(50 50) :initial-element 1 :element-type 'bit))
         (array2 (make-array '(20 20) :initial-element 1 :element-type 'fixnum))
         (array3 (make-array '(50 50) :initial-element 0.0f0 :element-type 'single-float))
         (2-designs  (list +dark-blue+ +dark-red+))
         (2-designs* (list +dark-salmon+ +dark-slate-grey+))
         (4-designs  (list +orange+ +dark-green+ +red+ +blue+))
         (4-designs* (list +purple+ +blue+ +red+ +grey+))
         (x-designs  (list (make-rectangular-tile (make-pattern array2 4-designs) 25 25)
                           (make-rectangular-tile (make-pattern array2 4-designs*) 25 25))))
    ;; set array for 4x4 checkboard
    (dotimes (i 25)
      (dotimes (j 25)
        (setf (aref array i j) 0
              (aref array (- 49 i) (- 49 j)) 0)))
    ;; set array2 for 5x5 checkboard
    (dotimes (i 20)
      (dotimes (j 20)
        (setf (aref array2 i j) (mod (+ (truncate i 5) (truncate j 5)) 4))))
    ;; set array3 for gradient stencil
    (dotimes (i 50)
      (dotimes (j 50)
        (setf (aref array3 i j) (/ (+ i j) 100.0f0))))

    (defparameter *patterns*
      (list
       ;; 50x50 checkboard indexed pattern
       (make-pattern array 2-designs)
       ;; 50x50 checkboard indexed pattern with indexed rectangular-tile inks
       (make-pattern array x-designs)
       ;; 30x30 checkboard 4-indexed pattern rotated by pi/4 and translated
       (transform-region (compose-translation-with-transformation
                          (make-rotation-transformation* (/ pi 4) 10.5 10.5)
                          12.5 12.5)
                         (make-pattern array2 4-designs))
       ;; 40x40 checkboard indexed rectangular-tile (ink is 20x20)
       (make-rectangular-tile (make-pattern array2 4-designs) 40 40)
       ;; 30x30 checkboard indexed rectangular-tile (ink is 50x50)
       (make-rectangular-tile (make-pattern array 2-designs*) 30 30)
       ;; 50x50 checkboard stencil
       (make-stencil array3)))))

(defparameter *general-description*
  "In this demo we explore draw-pattern* and draw-design capabilities when used
on patterns under various transformation and ink variations. We test these demos
in various arrangaments (drawn as a recorded stream and moved, simply painted on
a basic-sheet which is not a stream etc.). Under each test you will find a
description what should be seen with a possible failure description.

Images will be drawn on top of a rectangle which is transformed with normal
current medium transformation as a set of square patterns. To allow easy
function scanning draw-pattern* is drawn on top of a red dashed rectangle and
draw-design is drawn on top of a blue dashed rectangle.

Pattern 1: a blue-red 2x2 checkboard pattern (50x50)
Pattern 2: a checkboard rectangular-tile (25x25, ink 20x20) pattern (50x50)
Pattern 4: a checkboard pattern (20x20) rotated by pi/4 translated by [12.5, 12.5]
Pattern 3: a rectangular tile bigger than its ink (40x40 vs 20x20)
Pattern 4: a rectangular tile smaller than its ink (30x30 vs 5x50)
Pattern 5: a stencil with opacity increasing with XY from 0->1 (50x50) [disabled]")

(defparameter *options*
  "Keyboard shortcuts:
-----------------------------------------------------
1: draw-pattern*                             PATTERN
2: draw-design                                DESIGN
3: draw-rectangle with pattern being ink   RECTANGLE
-----------------------------------------------------
R: restart demo
Q: quit application frame
Space: redisplay application")

(defvar *draw* :pattern)

(defclass my-basic-pane (clim:basic-pane clime:always-repaint-background-mixin) ())

(define-application-frame pattern-design-test ()
  ()
  (:menu-bar nil)
  (:geometry :width 1440 :height 635)
  (:panes (info1 :application
                 :display-function #'(lambda (frame pane)
                                       (declare (ignore frame))
                                       (let ((*text-right-margin* 600))
                                         (draw-string pane *general-description* 20 30))))
          (info2 :application
                 :display-function #'(lambda (frame pane)
                                       (declare (ignore frame))
                                       (let ((*text-right-margin* 600))
                                         (with-drawing-options (pane :text-family :fix)
                                          (draw-string pane *options* 20 30)
                                          (draw-string pane
                                                       (format nil "Current draw is ~a." *draw*)
                                                       20 220)))))
          (pane1 :application :display-function #'display :scroll-bars :vertical)
          (pane2 :application :display-function #'display :scroll-bars nil)
          ;; Bug #7: clx-fb backend doesn't work with panes like this one.
          (pane4 (make-pane 'my-basic-pane :height *total-height*)))
  (:layouts (default (clim:vertically ()
                       (235
                        (clim:horizontally () info1 info2))
                       (clim:horizontally ()
                         (1/4 (labelling (:label "Application :SCROLL-BARS :BOTH")
                                pane1))
                         (1/4 (labelling (:label "Application")
                                (scrolling (:scroll-bars :vertical)
                                  pane2)))
                         ;; Bug #4: if basic-pane has its own scroll-bars, ink
                         ;; doesn't follow the scroll what gives a weird
                         ;; result. Fixing that for applicatioin pane broken
                         ;; basic-pane. WIP.
                         (1/4 (labelling (:label "Basic pane (no output recording)")
                                (scrolling (:scroll-bars :vertical) pane4))))))))

(defun draw-patterns (pane)
  (draw-rectangle* pane 5 5 (+ (* 60 (length *patterns*)) 5) 65
                   :filled nil :line-dashes t :line-thickness 3 :ink +dark-red+
                   ;; Bug # medium-gcontext has no primary method for
                   ;; uniform-compositum on CLX
                   :filled t :ink (climi::make-uniform-compositum +dark-red+ 0.5))
  (do* ((i 0 (1+ i))
        (x 10 (+ 60 x))
        (p* *patterns* (cdr p*))
        (pattern #1=(first p*) #1#))
       ((endp p*))
    (draw-pattern* pane pattern x 10)
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :filled nil)))

(defun draw-designs (pane)
  (draw-rectangle* pane 5 5 (+ (* 60 (length *patterns*)) 5) 65
                   :filled nil :line-dashes t :line-thickness 3 :ink +dark-blue+
                   :filled t :ink (climi::make-uniform-compositum +dark-blue+ 0.5))
  (do* ((i 0 (1+ i))
        (x 10 (+ 60 x))
        (p* *patterns* (cdr p*))
        (pattern #1=(first p*) #1#))
       ((endp p*))
    (draw-design pane pattern :transformation (make-translation-transformation x 10))
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :filled nil :line-dashes nil :line-thickness 2 :ink +grey42+)))

(defun draw-rects (pane)
  (draw-rectangle* pane 5 5 (+ (* 60 (length *patterns*)) 5) 65
                   :filled nil :line-dashes t :line-thickness 3 :ink +dark-green+
                   :filled t :ink (climi::make-uniform-compositum +dark-green+ 0.5))
  (do* ((i 0 (1+ i))
        (x 10 (+ 60 x))
        (p* *patterns* (cdr p*))
        (pattern #1=(first p*) #1#))
       ((endp p*))
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :ink pattern)
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :filled nil :line-dashes nil :line-thickness 2 :ink +grey42+)))

(defun %split-line (character string &key (count 1) from-end)
  (check-type count (integer 1))
  (alexandria:if-let ((pos (position-if #'(lambda (c)
                                            (and (char= character c)
                                                 (zerop (decf count))))
                                        string
                                        :from-end from-end)))
    (values (string-right-trim '(#\space) (subseq string 0 pos))
            (string-right-trim '(#\space) (subseq string (1+ pos))))
    string))

(defun %split-sequence-to-list (character string)
  (alexandria:if-let ((pos (position-if (alexandria:curry #'char= character)
                                        string)))
    (cons (subseq (string-right-trim (list character #\space) string) 0 pos)
          (%split-sequence-to-list character (subseq string (1+ pos))))
    (list (string-right-trim (list character #\space) string))))

(defun split-line-by-word (text margin &optional (text-size-fn #'length))
  "Splits line of text by word so the first part fits inside the margin. If
there is a non-empty remainder it is returned as a second value. If no word fits
in margin the first word is returned neverless. Returned strings are
right-trimmed for spaces."
  ;; XXX: do we want to trim leading space? Yes! but not on this demo :-)
  (setf text (string-trim '(#\newline #\space)
                          (string-right-trim '(#\space #\newline) text)))
  (when (<= (funcall text-size-fn text) margin)
    (return-from split-line-by-word text))
  (do* ((count 1 (1+ count))
        (results (multiple-value-list
                  (%split-line #\space text :count count :from-end t)))
        (string (first results))
        (remainder (second results)))
       ((or (<= (funcall text-size-fn string) margin)
            (alexandria:emptyp remainder))
        (if (alexandria:emptyp remainder)
            (%split-line #\space text)
            (values string remainder)))
    (multiple-value-setq (string remainder)
      (%split-line #\space text :count count :from-end t))))

(defun draw-string (pane string x y &rest args
                    &key (align-x :left) (align-y :baseline) &allow-other-keys)
  "Like format but works on medium and takes draw-text* arguments. Wraps by word"
  (let* ((eosp         (extended-output-stream-p pane))
         (medium       (if (not eosp)
                           pane
                           (sheet-medium pane)))
         (text-style   (medium-text-style medium))
         (text-ascent  (text-style-ascent text-style medium))
         (text-margin  (ecase align-x
                         (:left (- *text-right-margin* x))
                         (:right x)
                         (:center *text-right-margin*)))
         (dy y)
         (lines (do* ((strings (%split-sequence-to-list #\newline string)
                               ;; (list
                               ;;  ;; XXX: Hack to remove all newlines (M-q).
                               ;;  (format nil "~{~A~^ ~}"
                               ;;          (mapcar #'(lambda (s)
                               ;;                      (string-trim '(#\newline #\space) s))
                               ;;                  (%split-sequence-to-list #\newline string))))
                               )
                      (current #2=(pop strings) #2#)
                      (final-lines nil))
                     ((null current) (nreverse final-lines))
                  (multiple-value-bind (current rem)
                      (split-line-by-word current text-margin (alexandria:curry #'text-size medium))
                    (when rem (push rem strings))
                    (push current final-lines)
                    (incf dy text-ascent))))
         (start-y (ecase align-y
                    ((:top :baseline) y)
                    ((:bottom :baseline*) (- y (* (1- (length lines)) text-ascent)))
                    (:center (- y (* 0.5 (1- (length lines)) text-ascent))))))
    (when (member align-y '(:baseline :baseline*))
      (setf (getf args :align-y) :baseline))
    (dolist (line lines)
      (apply #'draw-text* pane line x start-y args)
      (incf start-y text-ascent))))

(defun test-example (pane &key first-quadrant transformation (description "") draw)
  ;; Bug #5: draw-text* doesn't work with strings which start with a newline.
  #+ (or) (draw-text* pane (format nil "~%foo") 10 10)
  ;; Bug #6: draw-text* is not drawn at all in with-room-for-graphics for
  ;; default first-quadrant (inverted Y). It should be either rotated or drawn
  ;; without rotation at the correct position.
  #+ (or) (with-room-for-graphics (pane)
            (draw-text* pane "foo" 10 10)
            (draw-text* pane "foo" 10 -10))
  ;; XXX
  (with-room-for-graphics (pane :first-quadrant first-quadrant :move-cursor nil)
    (with-drawing-options (pane :transformation transformation)
      (ecase draw
        (:design (draw-designs pane))
        (:pattern (draw-patterns pane))
        (:rectangle (draw-rects pane)))))
  (multiple-value-bind (x y)
      (if (extended-output-stream-p pane)
          (stream-cursor-position pane)
          (transform-position (medium-transformation (sheet-medium pane)) 0 0))
    (clim:with-identity-transformation (pane)
      (draw-string pane (format nil description)
                   (+ x 5)
                   (+ y *block-height* -10)
                   :align-y :bottom :align-x :left)
      (if first-quadrant
          (draw-arrow* pane
                       (+ *text-right-margin* 16) (+ y 64)
                       (+ *text-right-margin* 16) (+ y 16))
          (draw-arrow* pane
                       (+ *text-right-margin* 16) (+ y 16)
                       (+ *text-right-margin* 16) (+ y 64))))))

(defun ssop (pane &key (x 0 x-p) (y 0 y-p))
  (multiple-value-bind (ox oy) (stream-cursor-position pane)
    (setf (stream-cursor-position pane)
          (values (if x-p x ox)
                  (if y-p y oy)))))

(defmacro layout-examples ((pane) &body examples)
  (alexandria:with-gensyms (eosp)
    `(let ((,eosp (extended-output-stream-p ,pane)))
       ,@(mapcar (let ((y 0))
                   #'(lambda (ex)
                       (prog1 `(progn
                                 (if ,eosp
                                     (progn (ssop ,pane :x 0 :y ,y) ,ex)
                                     (with-translation (,pane 0 ,y) ,ex))
                                 (draw-line* pane
                                             0
                                             ,(+ y *block-height* -10)
                                             *text-right-margin*
                                             ,(+ y *block-height* -10)
                                             :ink +blue+
                                             :line-dashes t))
                         (incf y *block-height*))))
                 examples))))

(defmethod handle-repaint ((pane my-basic-pane) region)
  (declare (ignore region))
  (draw-rectangle* pane 0 0 (+ *text-right-margin* 100) *total-height* :ink +white+)
  (display *application-frame* pane)
  ;; (layout-examples (pane)
  ;;   (test-example pane :description "hello world")
  ;;   (test-example pane :description "hello world2"))
  #+ (or)
  (with-translation (pane 5 5)
    (test-example pane)))

(defmethod display ((frame pattern-design-test) pane &aux (draw *draw*))
  (do ((i 5 (+ i 16))
       (j 5 (+ j 16))
       (max-i *text-right-margin*)
       (max-j *total-height*))
      ((and (> i max-i)
            (> j max-j)))
    (when (<= i max-i)
      (draw-line* pane i 0 i max-j :ink +grey+))
    (when (<= j max-j)
      (draw-line* pane 0 j max-i j :ink +grey+)))
  ;; Bug #1: width/height is not translated correctly (if we have anything
  ;; before the pattern this height/width is substituted from the rest).
  (draw-line* pane
              *text-right-margin*
              0
              *text-right-margin*
              *total-height*
              :ink +red+
              :line-dashes t )
  (layout-examples (pane)
    (test-example pane :first-quadrant nil
                  :draw draw
                  :description "[1] Basic case. Patterns are drawn with current
                  transformation being just a translation. Ink should start at
                  top-left corner of the square (should be aligned with
                  it). Likely failures: ink has offset, ink not scrolling with a
                  square.")
    ;; Bug #2: if first-quadrant is t non-uniform design is not drawn.
    (test-example pane :first-quadrant t
                  :draw draw
                  :description "[2] Y-axis is reverted (FIRST-QUADRANT=T). Since
                  it is draw-pattern only translation is applied. Should look
                  like the test [1]. Likely failures: inverted ink, squares not
                  visible, additional vertical offset, error in basic-pane, same
                  as test [1].")
    (test-example pane
                  :first-quadrant nil
                  :draw draw
                  :transformation (make-rotation-transformation (/ pi 8))
                  :description "[5] Rotation by pi/4. Underlying rectangle is
                  rotated, squares are not rotated but their start position is
                  translated (so if they were rotated like the rectangle, they
                  would start in the same position). Likely failures: position
                  is not translated, ink is not translated (so in fact no
                  visible), ink is not scrolling with the square, on a
                  basic-pane rectangle itself may have wrong initial position.")
    (test-example pane
                  :first-quadrant t
                  :draw draw
                  :transformation (make-rotation-transformation (/ pi 8))
                  :description "[6] Rotation by pi/4 with
                  FIRST-QUADRANT=T. Underlying rectangle is rotated with
                  inverted Y-axis. squares are not rotated but their start
                  position is translated taking according to this
                  reversal. Likely failures: position is translated as in [5],
                  same as test [5].")))

(define-pattern-design-test-command (refresh-pattern-design :keystroke #\space) ()
  (format *debug-io* "."))

(progn (define-pattern-design-test-command (dpattern :keystroke #\1) ()
         (setf *draw* :pattern)
         #1=(map-over-sheets (lambda (sheet)
                               (redisplay-frame-pane *application-frame* sheet :force-p t)
                               (repaint-sheet sheet +everywhere+))
                             (frame-top-level-sheet *application-frame*)))
       (define-pattern-design-test-command (ddesign :keystroke #\2) ()
         (setf *draw* :design)
         #1#)
       (define-pattern-design-test-command (drectangle :keystroke #\3) ()
         (setf *draw* :rectangle)
         #1#))

(define-pattern-design-test-command (exit :keystroke #\q) ()
  (clim:frame-exit *application-frame*))

(define-pattern-design-test-command (com-restart :keystroke #\r) ()
  (run-demo 'pattern-design-test :background t)
  ;; frame-exit throws! we need to start a new demo before it, because rest of
  ;; the body is never executed.
  (clim:frame-exit *application-frame*))
