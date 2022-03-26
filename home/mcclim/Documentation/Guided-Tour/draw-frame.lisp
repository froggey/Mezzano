(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :mcclim))

(cl:in-package #:clim-user)

; LTAG-start:draw-frame-def-app
(define-application-frame draw-frame ()
  ((lines :accessor lines :initform nil)            ; lines of drawing
   (strings :accessor strings :initform nil))       ; texts of drawing
  (:panes (draw-pane (make-pane 'draw-pane))
          (interactor :interactor))
  (:layouts (default-default (vertically ()
                               (:fill draw-pane)
                               (1/4 interactor))))
  (:menu-bar t)
  (:command-definer t)
  (:top-level (default-frame-top-level)))

(defclass draw-pane
    (standard-extended-input-stream ; must have precedence over basic-pane
     basic-pane
     clime:always-repaint-background-mixin
     permanent-medium-sheet-output-mixin)
  ())

(defmethod handle-repaint ((pane draw-pane) region)
  (with-application-frame (frame)
    (call-next-method)                  ; Paints the background
    (dolist (line (lines frame))
      (draw-line pane (car line) (cdr line)))
    (dolist (pair (strings frame))
      (draw-text pane (cdr pair) (car pair)))))

; LTAG-end
(defmethod frame-standard-output ((frame draw-frame))
  (get-frame-pane frame 'interactor))

; LTAG-start:draw-frame-commands
(define-draw-frame-command (com-draw-add-string :menu t :name "Add String")
    ((string 'string) (x 'integer) (y 'integer))
  (push (cons (make-point x y) string)
        (strings *application-frame*))
  (update-draw-pane))

(define-draw-frame-command (com-draw-add-line :menu t :name "Add Line")
    ((x1 'integer) (y1 'integer) (x2 'integer) (y2 'integer))
  (with-slots (lines) *application-frame*
      (push (cons (make-point x1 y1) (make-point x2 y2))
            lines))
  (update-draw-pane))

(define-draw-frame-command (com-draw-clear :menu t :name "Clear") ()
  (with-slots (lines strings) *application-frame*
    (setf lines nil strings nil))
  (update-draw-pane))

;; Auxilary Method
(defun update-draw-pane ()
  (repaint-sheet (find-pane-named *application-frame* 'draw-pane) +everywhere+))
; LTAG-end

; LTAG-start:draw-frame-interfacing
(defmethod handle-event ((pane draw-pane) (event pointer-button-press-event))
  ;; Start line tracking when left pointer button is pressed
  (when (eql (pointer-event-button event) +pointer-left-button+)
    (track-line-drawing pane
                        (pointer-event-x event)
                        (pointer-event-y event))))

(defmethod handle-event ((pane draw-pane) (event key-press-event))
  (when (keyboard-event-character event)
    (multiple-value-bind (x y) (stream-pointer-position pane)
      ;; Start with empty string, as a key release event will be received anyway
      (track-text-drawing pane "" x y)))
  (update-draw-pane))

(defun track-line-drawing (pane startx starty)
  (let ((lastx startx)
        (lasty starty)
        endx endy)
    (block nil
      (with-drawing-options (pane :ink +flipping-ink+)
        (draw-line* pane startx starty lastx lasty)
        (tracking-pointer (pane)
          (:pointer-motion (&key x y)
            (draw-line* pane startx starty lastx lasty) ; delete old
            (draw-line* pane startx starty x y)         ; draw new
            (setf lastx x lasty y))
          (:pointer-button-release (&key event x y)
            (when (eql (pointer-event-button event) +pointer-left-button+)
              (draw-line* pane startx starty lastx lasty) ; delete old
              (setf endx x endy y)
              (return))))))
    (execute-frame-command
     *application-frame* `(com-draw-add-line ,startx ,starty ,endx ,endy))))

(defun track-text-drawing (pane current-string current-x current-y)
  (tracking-pointer (pane)
    (:pointer-motion (&key x y)
      ;; We can't use flipping ink for text, hence redraw.
      (handle-repaint pane +everywhere+)
      (setq current-x x current-y y)
      (draw-text* pane current-string x y))
    (:keyboard (&key gesture)
      (when (and (typep gesture 'key-release-event)
                 (keyboard-event-character gesture))
        (setf current-string
              (concatenate 'string
                           current-string
                           (string (keyboard-event-character gesture))))
        (handle-repaint pane +everywhere+)
        (draw-text* pane current-string current-x current-y)))
    (:pointer-button-release (&key event x y)
      (when (eql (pointer-event-button event) +pointer-left-button+)
        (execute-frame-command *application-frame*
          `(com-draw-add-string ,current-string ,x ,y))
        (return-from track-text-drawing nil)))))
; LTAG-end:draw-frame-part2
