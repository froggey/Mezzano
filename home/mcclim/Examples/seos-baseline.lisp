
(in-package #:clim-demo)

(define-application-frame seos-baseline ()
  ()
  (:menu-bar seos-command-table)
  (:pane :application
         :width 400
         :height 400
         :display-function #'display
         :end-of-line-action :allow
         :end-of-page-action :allow
         :text-margins '(:left (:absolute 30)
                         :right (:relative 30)
                         :top (:relative 30)
                         :bottom (:absolute 370))))

(defun show-line (stream &rest args)
  (loop for (size text) on args by #'cddr do
       (with-drawing-options (stream :text-size size)
         (format stream text)))
  (terpri stream))

(defmethod display ((frame seos-baseline) pane)
  (declare (ignore frame))
  (show-line pane :normal "Hello " :huge "world!")
  (show-line pane
             :normal "Hello world "
             :normal "hiho" :large "hiho" :huge "hiho" :tiny "hiho" :normal "hiho"
             :normal "hiho" :large "hiho" :huge "hiho" :tiny "hiho" :normal "hiho"
             :normal "hiho" :large "hiho" :huge "hiho" :tiny "hiho" :normal "hiho"
             :normal "hiho" :large "hiho" :huge "hiho" :tiny "hiho" :normal "hiho")
  (show-line pane :huge "Third " :normal "line " :tiny "hello " :huge "world!")
  (show-line pane :normal "Last " :huge "line " :normal "bam bam")

  (terpri pane)
  (with-bounding-rectangle* (x1 y1 x2 y2) (clime:stream-page-region pane)
    (declare (ignore y1 y2))
    (let ((y (nth-value 1 (stream-cursor-position pane))))
      (draw-line* pane x1 y x2 y :ink +blue+ :line-dashes t)))
  (terpri pane)
  (format pane "All lines should have text aligned on the same baseline. Likely failures:

1. Parts of the text with different size aligned to the top (not baseline).
2. Pressing space cause redisplay and schedules repaint after 1s. This may exhibit different outlook of displayed and repainted output.
3. All lines in this description are long. Use menu to change end of line action. Current action is ~s.
4. When viewport is smaller than the whole scrolling area ALT scrolls to the very bottom.
6. There is one line below these points. Stream height may not be recalculated to take it into account because it doesn't have newline character in the end. When wrapped part of the text may not be rendered. Try pressing space.
7. Said last line may be rendered and recorded, but not scrolled to the end despite :SCROLL contract.
8. Some lines here are lengthy to test different wrapping scenarios. Page end action :WRAP is not very useful - it is not a bug that text is drawn on top of the previous one. Here comes a lot of letters with random spaces: AAA BBBBBBBBBBBBBBB CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD.

See the introduction in \"15.3 The Text Cursor\"."
          (stream-end-of-line-action pane))
  (draw-design pane (clime:stream-page-region pane)
               :ink +red+ :line-dashes t :filled nil))

(define-seos-baseline-command (com-redisplay :keystroke #\space) ()
  (schedule-event *standard-output*
                  (make-instance 'window-repaint-event
                                 :region +everywhere+
                                 :sheet *standard-output*)
                  1))

(make-command-table 'seos-command-table
                    :errorp nil
                    :menu '(("Line" :menu line-ct)
                            ("Page" :menu page-ct)))

(make-command-table 'line-ct :errorp nil
                    :menu '(("Allow" :command com-allow-line)
                            ("Scroll" :command com-scroll-line)
                            ("Wrap" :command com-wrap-line)
                            ("Wrap word" :command com-wrap*-line)))

(make-command-table 'page-ct :errorp nil
                    :menu '(("Allow" :command com-allow-page)
                            ("Scroll" :command com-scroll-page)
                            ("Wrap" :command com-wrap-page)))

(define-seos-baseline-command (com-allow-line :keystroke #\1) ()
  (setf (stream-end-of-line-action *standard-output*) :allow))

(define-seos-baseline-command (com-scroll-line :keystroke #\2) ()
  (setf (stream-end-of-line-action *standard-output*) :scroll))

(define-seos-baseline-command (com-wrap-line :keystroke #\3) ()
  (setf (stream-end-of-line-action *standard-output*) :wrap))

(define-seos-baseline-command (com-wrap*-line :keystroke #\4) ()
  (setf (stream-end-of-line-action *standard-output*) :wrap*))

(define-seos-baseline-command (com-allow-page :keystroke #\q) ()
  (setf (stream-end-of-page-action *standard-output*) :allow))

(define-seos-baseline-command (com-scroll-page :keystroke #\w) ()
  (setf (stream-end-of-page-action *standard-output*) :scroll))

(define-seos-baseline-command (com-wrap-page :keystroke #\e) ()
  (setf (stream-end-of-page-action *standard-output*) :wrap))

;(run-frame-top-level (make-application-frame 'seos-baseline))
