(cl:in-package #:clim-demo)

(define-application-frame av-test ()
  ((own-window-p :initform nil))
  (:menu-bar t)
  (:panes
   (screen :application
           :display-time t
	   :display-function #'av-test-display-screen
           :text-style (make-text-style :sans-serif :roman :normal))
   (own-window-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback
				       #'(lambda (this-gadget selected-gadget)
					   (declare (ignore this-gadget))
					   (with-slots (own-window-p) clim:*application-frame*
					     (setf own-window-p
						   (string=
						    (clim:gadget-label selected-gadget)
						    "yes")))))
      (clim:radio-box-current-selection "no")
      "yes"))
   (interactor :interactor :min-width 600)
   (doc :pointer-documentation))
  (:layouts
   (defaults
       (horizontally ()
	 (vertically ()
	  (labelling (:label "Own Window")
	    own-window-option)
	  +fill+)
	 (vertically ()
	   screen
	   interactor
	   doc)))))

(defun av-test-display-screen (frame pane)
  (declare (ignore frame))
  (with-text-size (pane :large)
    (fresh-line pane)
    (present '(com-accepting-interval) 'command :stream pane)
    (fresh-line pane)
    (present '(com-accepting-square) 'command :stream pane)
    (fresh-line pane)
    (present '(com-reset-clock-1) 'command :stream pane)
    (fresh-line pane)
    (present '(com-reset-clock-2) 'command :stream pane)
    (fresh-line pane)
    (present '(com-accepting-tag) 'command :stream pane)
    (fresh-line pane)
    (present '(com-menu-choose-1) 'command :stream pane)
    (fresh-line pane)
    (present '(com-menu-choose-2) 'command :stream pane)
    (fresh-line pane)
    (present '(com-menu-choose-3) 'command :stream pane)
    (fresh-line pane)
    (present '(com-menu-choose-4) 'command :stream pane)
    (fresh-line pane)
    (present '(com-accept-popup) 'command :stream pane)
    (fresh-line pane)
    (present '(com-accepting-with-list-pane-view) 'command :stream pane)
    (fresh-line pane)
    (present '(com-accepting-with-gadgets) 'command :stream pane)
    (fresh-line pane)))

(define-av-test-command (com-refresh-av-test
			 :name t
			 :menu t)
    ()
  (window-clear (find-pane-named *application-frame* 'screen))
  (window-clear (find-pane-named *application-frame* 'interactor))
  (av-test-display-screen *application-frame*
			  (find-pane-named *application-frame* 'screen)))

(define-av-test-command (com-accepting-interval
			 :name t
			 :menu nil)
    ()
  (with-slots (own-window-p) clim:*application-frame*
    (format t "Result: ~S~%" (multiple-value-list (accepting-interval :ow own-window-p))))
  (finish-output *standard-output*))

(define-av-test-command (com-accepting-square
			 :name t
			 :menu nil)
    ()
  (with-slots (own-window-p) clim:*application-frame*
    (format t "Result: ~S~%" (multiple-value-list (accepting-square :ow own-window-p))))
  (finish-output *standard-output*))

(define-av-test-command (com-reset-clock-1
			 :name t
			 :menu nil)
    ()
  (with-slots (own-window-p) clim:*application-frame*
    (format t "Result: ~S~%" (multiple-value-list (reset-clock-1 :ow own-window-p))))
  (finish-output *standard-output*))

(define-av-test-command (com-reset-clock-2
			 :name t
			 :menu nil)
    ()
  (with-slots (own-window-p) clim:*application-frame*
    (format t "Result: ~S~%" (multiple-value-list (reset-clock-2 :ow own-window-p))))
  (finish-output *standard-output*))

(define-av-test-command (com-accepting-tag
			 :name t
			 :menu nil)
    ()
  (with-slots (own-window-p) clim:*application-frame*
    (format t "Result: ~S~%" (multiple-value-list (accepting-tag :ow own-window-p))))
  (finish-output *standard-output*))

(define-av-test-command (com-menu-choose-1 :name t :menu nil)
    ()
  (format t "Result: ~S~%" (multiple-value-list (menu-choose-1)))
  (finish-output *standard-output*))

(define-av-test-command (com-menu-choose-2 :name t :menu nil)
    ()
  (format t "Result: ~S~%" (multiple-value-list (menu-choose-2)))
  (finish-output *standard-output*))

(define-av-test-command (com-menu-choose-3 :name t :menu nil)
    ()
  (format t "Result: ~S~%" (multiple-value-list (menu-choose-3)))
  (finish-output *standard-output*))

(define-av-test-command (com-menu-choose-4 :name t :menu nil)
    ()
  (format t "Result: ~S~%" (multiple-value-list (menu-choose-4)))
  (finish-output *standard-output*))


(define-av-test-command (com-accept-popup
			 :name t
			 :menu nil)
    ()
  (format *standard-output* "Popup Test. ")
  (format t "Result: ~S~%" (multiple-value-list (accept-popup '(1 2 3 4 5 6 7 8))))
  (finish-output *standard-output*))

(define-av-test-command (com-accepting-with-list-pane-view
			 :name t
			 :menu nil)
    ()
  (with-slots (own-window-p) clim:*application-frame*
    (format t "Result: ~S~%" (multiple-value-list (accepting-with-list-pane-view :ow own-window-p))))
  (finish-output *standard-output*))

(define-av-test-command (com-accepting-with-gadgets
			 :name t
			 :menu nil)
    ()
  (with-slots (own-window-p) clim:*application-frame*
    (format t "Result: ~S~%" (multiple-value-list (accepting-with-gadgets :ow own-window-p))))
  (finish-output *standard-output*))
