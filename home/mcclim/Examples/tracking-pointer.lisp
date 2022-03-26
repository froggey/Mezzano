;;;;  Copyright (c) 2019 Daniel Kochmański
;;;;
;;;;    License: BSD-2-Clause.

(in-package #:clim-demo)

#|
This test would be far more complete if we had one more control box
because tracking pointer is specified to vary its behavior depending
on which clauses are specified. I'm dropping a ball for now and leave
all clauses "active" in this test.

(with-radio-box (:type :some-of)
  "pointer motion"
  "pointer press"
  "pointer release"
  "presentation motion"
  "presentation press"
  "presentation release"
  "keyboard")
|#

(define-application-frame tracking-pointer-test ()
  ((multiple-window-p :initform t :accessor multiple-window-p)
   (transformp :initform t :accessor transformp)
   (repaintp :initform t :accessor repaintp)
   (tracked-sheet :initform 'pane :accessor tracked-sheet)
   (context-type :initform 'foo  :accessor context-type))
  (:menu-bar nil)
  (:geometry :width 800 :height 600)
  (:panes (pane :basic-pane)
          (app1 :application :display-function #'display :scroll-bars nil)
          (app2 :application :display-function #'display)
          (int  :interactor))
  (:pointer-documentation t)
  (:layouts (default (vertically ()
                       (horizontally ()
                         (labelling (:label "app1 (application)") app1)
                         (labelling (:label "app2 (application)") app2))
                       (horizontally ()
                         (labelling (:label "pane (basic pane)") pane)
                         (labelling (:label "int  (interactor)") int))
                       ;; All below gadgets should be replacable with
                       ;; accepting-values.
                       (horizontally ()
                         (labelling (:label "Presentations")
                           (with-radio-box (:value-changed-callback
                                            (lambda (g v)
                                              (declare (ignore g))
                                              (setf (context-type *application-frame*)
                                                    (alexandria:switch (v :test #'string=
                                                                          :key #'gadget-label)
                                                      ("every" t)
                                                      ("none" nil)
                                                      ("blank" 'blank-area)
                                                      ("integer" 'integer)
                                                      ("(or foo string)" '(or foo string))
                                                      ("foo" 'foo)))))
                             "every" "none" "blank" "integer" "(or foo string)" "foo"))
                         (vertically ()
                           (labelling (:label "Tracked sheet")
                             (with-radio-box (:value-changed-callback
                                              (lambda (g v)
                                                (declare (ignore g))
                                                (setf (tracked-sheet *application-frame*)
                                                      (alexandria:switch (v :test #'string=
                                                                            :key #'gadget-label)
                                                        ("app1" 'app1)
                                                        ("app2" 'app2)
                                                        ("pane" 'pane)
                                                        ("int" 'int)))))
                               "app1" "app2" "int" "pane"))
                           (make-pane :toggle-button :label "Multiple Window" :value t
                                      :value-changed-callback
                                      (lambda (gadget value)
                                        (declare (ignore gadget))
                                        (setf (multiple-window-p *application-frame*) value)))
                           (make-pane :toggle-button :label "Transformp" :value t
                                      :value-changed-callback
                                      (lambda (gadget value)
                                        (declare (ignore gadget))
                                        (setf (transformp *application-frame*) value)))
                           (make-pane :toggle-button :label "Repaint damaged" :value t
                                      :value-changed-callback
                                      (lambda (gadget value)
                                        (declare (ignore gadget))
                                        (setf (repaintp *application-frame*) value)))
                           (make-pane 'push-button :label "Track pointer"
                                      :activate-callback
                                      (lambda (gadget)
                                        (declare (ignore gadget))
                                        (execute-frame-command *application-frame*
                                                               '(track-pointer))))))))))

(define-presentation-type foo ())

(defmethod display ((frame tracking-pointer-test) pane)
  (when (eql (pane-name pane) 'app2)
    (setf (medium-transformation (sheet-medium pane))
          (make-scaling-transformation* 2 2))
    (format pane "This pane has a scaling medium-transformation (2x2).~%"))
  (with-output-as-presentation (pane 42 'integer)
    (format pane "Presentation integer 42."))
  (fresh-line pane)
  (with-output-as-presentation (pane "42" 'string)
    (format pane "Presentation string \"42\"."))
  (fresh-line pane)
  (with-output-as-presentation (pane "42" 'foo)
    (format pane "Presentation FOO \"42\"."))
  (fresh-line pane)
  (with-room-for-graphics (pane)
    (draw-rectangle* pane 10 10 50 50 :ink +dark-blue+)))

(defun make-draw-cursor-function ()
  (let ((last-sheet nil)
        (last-region nil))
    (lambda (window x y id)
      (when (and (repaintp *application-frame*) last-sheet)
        (handle-repaint last-sheet (or last-region +everywhere+)))
      (if (output-recording-stream-p window)
          (with-output-recording-options (window :record nil)
            (draw-point* window x y :line-thickness 32)
            (draw-point* window x y :line-thickness 24 :ink +red+))
          (progn
            (draw-point* window x y :line-thickness 32)
            (draw-point* window x y :line-thickness 24 :ink +red+)))
      (setf last-sheet window
            last-region (make-rectangle* (- x 16) (- y 16)
                                         (+ x 16) (+ y 16)))
      (window-clear *pointer-documentation-output*)
      (format *pointer-documentation-output* "Press SPACE to exit.~%")
      (format *pointer-documentation-output* "Window: ~4a, Clause ID: ~s.~%" (pane-name window) id)
      (finish-output *pointer-documentation-output*))))

(define-tracking-pointer-test-command (track-pointer :name t) ()
  (let* ((tracked-sheet (find-pane-named *application-frame* (tracked-sheet *application-frame*)))
         (draw-cursor (make-draw-cursor-function)))
    (tracking-pointer (tracked-sheet :multiple-window (multiple-window-p *application-frame*)
                                     :transformp (transformp *application-frame*)
                                     :highlight t
                                     :context-type (context-type *application-frame*))
      (:pointer-motion
       (&key window x y)
       (funcall draw-cursor window x y :pointer-motion))
      (:pointer-button-press
       (&key event x y)
       (funcall draw-cursor (event-sheet event) x y :pointer-button-press))
      (:pointer-button-release
       (&key event x y)
       (funcall draw-cursor (event-sheet event) x y :pointer-button-release))
      (:presentation
       (&key presentation window x y)
       (declare (ignore presentation))
       (funcall draw-cursor window x y :presentation))
      (:presentation-button-press
       (&key presentation event x y)
       (declare (ignore presentation event x y)))
      (:presentation-button-release
       (&key presentation event x y)
       (declare (ignore presentation event x y)))
      (:keyboard
       (&key gesture)
       ;; when gesture is a space then it should be already character.
       (when (eventp gesture)
         (setq gesture (keyboard-event-character gesture)))
       (when (eql gesture #\space)
         (window-clear *pointer-documentation-output*)
         (return-from track-pointer))))))
