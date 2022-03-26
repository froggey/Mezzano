;;; This is the beginning of a Common Lisp debugger implemented in
;;; McCLIM. It uses the portable debugger interface developed for the
;;; Slime project, and the graphical layout is also heavily inspired
;;; by Slime. Because of Slime I hope that this works on other
;;; implementations than SBCL.

;;;
;;; Test:
;;;
;;; For at quick test, you can use this code snippet:
;;;
;;; (clim-debugger:with-debugger ()
;;;   (+ 3 'abc))
;;;
;;; This is also nice :-)
;;;
;;; (clim-debugger:with-debugger ()
;;;   (clim-listener:run-listener :new-process t))

;;;
;;; Problems/todo:
;;;
;;; - Elliott Johnson is to be thanked for the nice scroll-bars, but
;;;   for some reason they don't remember their position when clicking
;;;   on a stack-frame or "more".
;;;
;;; - Goto source location is not supported, but I think this could be
;;;   done through slime.
;;;
;;; - Frames could be navigable with arrow keys as well. How to do that?
;;;

(defpackage #:clim-debugger
  (:use #:clim #:clim-lisp #:clim-extensions)
  (:export #:debugger #:with-debugger #:install-debugger))

(in-package :clim-debugger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Misc   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Borrowed from Andy Hefner
(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Data model    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass debugger-info ()
  ((the-condition :accessor the-condition
                  :initarg :the-condition)
   (condition-message :accessor condition-message
                      :initarg  :condition-message)
   (type-of-condition :accessor type-of-condition
                      :initarg  :type-of-condition)
   (condition-extra :accessor condition-extra
                    :initarg  :condition-extra)
   (restarts :accessor restarts
             :initarg :restarts)
   (backtrace :accessor backtrace
              :initarg :backtrace)))

(defclass minimized-stack-frame-view (textual-view)())
(defclass maximized-stack-frame-view (textual-view)())

(defparameter +minimized-stack-frame-view+
  (make-instance 'minimized-stack-frame-view))
(defparameter +maximized-stack-frame-view+
  (make-instance 'maximized-stack-frame-view))

(defclass stack-frame ()
  ((clim-view       :accessor view :initform +minimized-stack-frame-view+)
   (frame-string    :accessor frame-string
                    :initarg  :frame-string)
   (frame-no        :accessor frame-no
                    :initarg :frame-no)
   (frame-variables :accessor frame-variables
                    :initarg :frame-variables)))

(defun compute-backtrace (start end)
  (loop for frame    in   (swank-backend::compute-backtrace start end)
     for frame-no from 0
     collect (make-instance
              'stack-frame
              :frame-string    (let ((*print-pretty* nil))
                                 (with-output-to-string (stream)
                                   (swank-backend::print-frame frame stream)))
              :frame-no        frame-no
              :frame-variables (swank-backend::frame-locals frame-no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   CLIM stuff   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass debugger-pane (application-pane)
  ((condition-info :reader  condition-info :initarg :condition-info)
   (active-frame :accessor active-frame :initform 0)
   (shown-frames :accessor shown-frames :initform 5)))

(defun make-debugger-pane ()
  (with-look-and-feel-realization ((frame-manager *application-frame*)
                                   *application-frame*)
    (make-pane 'debugger-pane
               :condition-info (the-condition *application-frame*)
               :display-function #'display-debugger
               :end-of-line-action :allow
               :end-of-page-action :scroll)))

(define-application-frame clim-debugger ()
  ((condition        :initform nil :accessor the-condition)
   (returned-restart :initform nil :accessor returned-restart))
  (:pointer-documentation t)
  (:panes (debugger-pane (make-debugger-pane)))
  (:layouts
   (default (scrolling () debugger-pane)))
  (:geometry :height 480 :width #.(* 480 slim:+golden-ratio+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Presentation types   ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-presentation-type stack-frame () :inherit-from 't)
(define-presentation-type restart     ())
(define-presentation-type more-type   ())
(define-presentation-type inspect     ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Gestures   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-gesture-name :prev    :keyboard (#\p :meta))
(define-gesture-name :next    :keyboard (#\n :meta))
(define-gesture-name :more    :keyboard (#\m))
(define-gesture-name :exit    :keyboard (#\q))
(define-gesture-name :eval    :keyboard (#\e))
(define-gesture-name :toggle  :keyboard #\tab)

;;; restart keyboard shortcuts
(macrolet ((invoke-x (x)
             (let* ((char (aref (format nil "~A" x) 0))
                    (name (alexandria:symbolicate "INVOKE-RESTART-" char)))
               `(progn
                  (define-clim-debugger-command (,name :keystroke (,char)) ()
                    (let* ((pane (clim:find-pane-named
                                  *application-frame* 'debugger-pane))
                           (restart (nth ,x (restarts (condition-info pane)))))
                      (when restart
                        (com-invoke-restart restart))))))))
  (invoke-x 0) (invoke-x 1) (invoke-x 2) (invoke-x 3) (invoke-x 4)
  (invoke-x 5) (invoke-x 6) (invoke-x 7) (invoke-x 8) (invoke-x 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Commands   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-clim-debugger-command (com-more :name "More backtraces"
                                        :keystroke :more)
    ()
  (let ((pane (clim:find-pane-named *application-frame* 'debugger-pane)))
    (setf #1=(shown-frames pane)
          (min (+ #1# 10) (length (backtrace (condition-info pane)))))))

(define-clim-debugger-command (com-invoke-inspector
                               :name "Invoke inspector")
    ((obj inspect))
  (clouseau:inspect obj :new-process t))

(define-clim-debugger-command (com-refresh :name "Refresh" :menu t
                                           :keystroke #\r) ()
  (change-space-requirements (frame-panes *application-frame*)))

(define-clim-debugger-command (com-next :keystroke :next)
    ()
  (let ((pane (clim:find-pane-named *application-frame* 'debugger-pane)))
    (incf (active-frame pane))
    (when (= (active-frame pane) (shown-frames pane))
      (com-more))
    (when (= (active-frame pane) (shown-frames pane))
      (decf (active-frame pane)))))

(define-clim-debugger-command (com-prev :keystroke :prev)
    ()
  (let ((pane (clim:find-pane-named *application-frame* 'debugger-pane)))
    (setf #1=(active-frame pane) (max (1- #1#) 0))))

#+(or) ; there's something really wonky with the way it reads the form
(define-clim-debugger-command (com-eval :name "Eval in frame" :menu t
                                        :keystroke :eval) ((form clim:string))
  (let* ((dbg-pane (clim:find-pane-named *application-frame* 'debugger-pane))
         (active-frame (active-frame dbg-pane)))
    (format *pointer-documentation-output*
            (swank:eval-string-in-frame
             form active-frame (swank-backend:frame-package active-frame)))))

(define-clim-debugger-command (com-quit :name "Quit" :menu t
                                        :keystroke :exit) ()
  (frame-exit *application-frame*))

(define-clim-debugger-command (com-invoke-restart :name "Invoke restart")
    ((restart 'restart))
  (setf (returned-restart *application-frame*) restart)
  (frame-exit *application-frame*))

(define-clim-debugger-command (com-toggle-stack-frame-view
                               :name "Toggle stack frame view")
    ((stack-frame 'stack-frame))

  (let ((dbg-pane (clim:find-pane-named *application-frame* 'debugger-pane)))
    (setf (active-frame dbg-pane) (frame-no stack-frame)))

  (if (eq +minimized-stack-frame-view+ (view stack-frame))
      (setf (view stack-frame) +maximized-stack-frame-view+)
      (setf (view stack-frame) +minimized-stack-frame-view+))
  (change-space-requirements (frame-panes *application-frame*)))

(define-clim-debugger-command (com-toggle-active-frame-view
                               :keystroke :toggle
                               :name "Toggle active")
    ()
  (let ((dbg-pane (clim:find-pane-named *application-frame* 'debugger-pane)))
    (com-toggle-stack-frame-view
     (nth (active-frame dbg-pane) (backtrace (condition-info dbg-pane))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Command translators   ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-presentation-to-command-translator more-backtraces
    (more-type com-more clim-debugger :gesture :select)
    (object)
  (list))

(define-presentation-to-command-translator invoke-inspector
    (inspect com-invoke-inspector clim-debugger :gesture :select)
    (object)
  (list object))

(define-presentation-to-command-translator toggle-stack-frame-view
    (stack-frame com-toggle-stack-frame-view clim-debugger :gesture :select)
    (object)
  (list object))

(define-presentation-to-command-translator invoke-restart
    (restart com-invoke-restart clim-debugger :gesture :select)
    (object)
  (list object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Display debugging info   ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-debugger (frame pane)
  (let ((*standard-output* pane))
    (slim:with-table (pane)
      (slim:row (slim:cell (bold (pane) (princ "Description:")))
                (slim:cell (princ (condition-message
                                   (condition-info pane)))))
      (slim:row (slim:cell (bold (pane) (princ "Condition:")))
                (slim:cell (with-drawing-options (pane :ink clim:+red+)
                             (with-output-as-presentation
                                 (pane (the-condition (condition-info pane)) 'inspect)
                               (princ (type-of-condition (condition-info pane)))))))
      (when (condition-extra (condition-info pane))
        (slim:row (slim:cell (bold (pane) (princ "Extra:")))
                  (slim:cell
                    (clim:with-text-family (pane :fix)
                      (format t "~A" (condition-extra (condition-info pane))))))))
    (fresh-line)

    (with-text-family (pane :sans-serif)
      (bold (pane) (format t "Restarts:")))
    (fresh-line)
    (format t " ")
    (slim:with-table (pane :x-spacing 10)
      (do* ((restarts (restarts (condition-info pane)) (cdr restarts))
            (r #1=(car restarts) #1#)
            (n 0 (1+ n)))
           ((null restarts) t)
        (with-output-as-presentation (pane r 'restart :single-box t)
          (slim:row
            (bold (pane)
              (slim:cell (format t "~A: " n)))
            (slim:cell
              (clim:with-drawing-options (pane :ink clim:+dark-violet+)
                (princ (restart-name r))))
            (slim:cell (princ r))))))
    (fresh-line)
    (display-backtrace frame pane)
    (change-space-requirements
     pane
     :width (bounding-rectangle-width (stream-output-history pane))
     :height (bounding-rectangle-height (stream-output-history pane)))))


(defun display-backtrace (frame pane)
  (declare (ignore frame))
  (with-text-family (pane :sans-serif)
    (bold (pane) (format pane "Backtrace:")))
  (fresh-line pane)
  (format pane "   ")
  (slim:with-table (pane)
    (do* ((back (backtrace (condition-info pane)) (cdr back))
          (stack-frame #1=(car back) #1#))
         ((or (null back)
              (= (frame-no stack-frame)
                 (shown-frames pane)))
          (when back
            (slim:row (slim:cell)
                      (slim:cell (bold (pane) (present pane 'more-type))))))
      (with-output-as-presentation
          (pane stack-frame 'stack-frame :single-box t)
        (slim:row
          (with-drawing-options (pane :ink clim:+gray41+)
            (slim:cell (format pane "~A: " (frame-no stack-frame))))
          (with-drawing-options (pane :ink (if (= (frame-no stack-frame)
                                                  (active-frame pane))
                                               clim:+red4+ clim:+blue4+))
            (slim:cell (present stack-frame 'stack-frame
                                :view (view stack-frame) :single-box t))))))))

(defun print-stack-frame-header (object stream)
  (let* ((frame-string (frame-string object))
         (new-line-pos (position #\newline frame-string)))
    (if new-line-pos
        (format stream "~A ..)" (subseq frame-string 0 new-line-pos))
        (princ frame-string stream))))

(define-presentation-method present (object (type stack-frame) stream
                                            (view minimized-stack-frame-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (print-stack-frame-header object stream))

(define-presentation-method present (object (type stack-frame) stream
                                            (view maximized-stack-frame-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (print-stack-frame-header object stream)
  (fresh-line stream)
  (if (null (frame-variables object))
      (with-text-family (stream :sans-serif) (format stream "  No locals."))
      (progn
        (with-text-family (stream :sans-serif)
          (bold (stream) (format stream "  Locals:")))
        (fresh-line stream)
        (format stream "     ")
        (slim:with-table (stream)
          (loop for (name n identifier id value val) in (frame-variables object)
             do (slim:row
                  (slim:cell (princ n))
                  (slim:cell (princ "="))
                  (slim:cell (present val 'inspect :single-box t)))))))
  (fresh-line stream))

(define-presentation-method present (object (type restart) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (bold (stream) (format stream "~A" (restart-name object))))

(define-presentation-method present (object (type more-type) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (bold (stream) (format stream "--- MORE ---")))

(define-presentation-method present (object (type inspect) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~A" object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Starting the debugger   ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-debugger-frame ()
  (run-frame-top-level
   (make-application-frame 'clim-debugger)))

(defun debugger (condition me-or-my-encapsulation)
  (let ((debugger-frame (make-application-frame 'clim-debugger)))
    (swank-backend::call-with-debugging-environment
     (lambda ()
       (unwind-protect
            (setf (the-condition debugger-frame)
                  (make-instance
                   'debugger-info
                   :the-condition        condition
                   :type-of-condition    (type-of condition)
                   :condition-message    (swank::safe-condition-message condition)
                   :condition-extra      (swank::condition-extras       condition)
                   :restarts             (compute-restarts)
                   :backtrace (compute-backtrace 0 nil)))
         (run-frame-top-level debugger-frame)
         (let ((restart (returned-restart debugger-frame)))
           (if restart
               (let ((*debugger-hook* me-or-my-encapsulation))
                 (invoke-restart-interactively restart))
               (abort))))))))


(defvar *debugger-bindings*
  `((*debugger-hook*                      . #'debugger)
    #+abcl (sys::*invoke-debugger-hook*   . #'debugger)
    #+ccl  (ccl:*break-hook*              . #'debugger)
    #+ecl  (ext:*invoke-debugger-hook*    . #'debugger)
    #+sbcl (sb-ext:*invoke-debugger-hook* . #'debugger)
    (bt:*default-special-bindings* . *debugger-bindings*)
    ,@bt:*default-special-bindings*))

(defmacro with-debugger (options &body body)
  (assert (null options) nil "Options should be empty.")
  `(let ((bt:*default-special-bindings* *debugger-bindings*)
         (*debugger-hook* #'debugger)
         #+abcl (sys::*invoke-debugger-hook* #'debugger)
         #+ccl (ccl:*break-hook* #'debugger)
         #+ecl (ext:*invoke-debugger-hook* #'debugger)
         #+sbcl (sb-ext:*invoke-debugger-hook* #'debugger))
     ,@body))

(defun install-debugger ()
  (setf *debugger-hook* #'debugger)
  #+abcl (setf sys::*invoke-debugger-hook*   #'debugger)
  #+ccl  (setf ccl:*break-hook*              #'debugger)
  #+ecl  (setf ext:*invoke-debugger-hook*    #'debugger)
  #+sbcl (setf sb-ext:*invoke-debugger-hook* #'debugger))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   For testing   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-break ()
  (with-simple-restart  (continue "Continue from interrupt.")
    (with-debugger ()
      (invoke-debugger
       (make-condition 'simple-error
                       :format-control "Debugger test")))))
