(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

(define-application-frame superapp ()
  ((numbers :initform (loop repeat 20 collect (list (random 100000000)))
	    :accessor numbers)
   (cursor :initform 0 :accessor cursor))
  (:pointer-documentation t)
  (:panes
    (app :application
	 :height 400 :width 600
	 :incremental-redisplay t
	 :display-function 'display-app)
    (int :interactor :height 200 :width 600))
  (:layouts
    (default (vertically () app int))))

;; As usual, the displaying code relates to a pane, not the application frame. 
(defun display-app (frame pane)

  (loop
     ;; taking items one-by-one from the frame slot 'numbers'
     for current-element in (numbers frame)

     ;; and increasing line-by-line  
     for line from 0

     ;; prints a star if the cursor is on that line
     ;; (Note that here, there is no incremental redisplay. The output
     ;; record of the star will be printed at each call of the display
     ;; function -- that is at each iteration of the command loop.)
     do (princ (if (= (cursor frame) line) "*" " ") pane)

     ;; and incrementally updates the rendering instructions of the
     ;; number on that line
     ;; (Note that 'numbers' was defined as a list of lists, each
     ;; sublist holding an individual number. The reason for that is
     ;; explained below, but this is why (car current-element) is
     ;; needed.)
     do (updating-output (pane :unique-id   current-element
			       :id-test     #'eq
			       :cache-value (car current-element)
			       :cache-test  #'eql)
	  (format pane "~a~%" (car current-element)))))


;;
;; Command definitions
;;

;; increase the value of the number on the current line
(define-superapp-command (com-add :name t) ((number 'integer))
  (incf (car (elt (numbers *application-frame*)
		  (cursor *application-frame*)))
	number))

;; move the cursor one line down (increasing the cursor position),
;; looping back to the beginning if going too far
(define-superapp-command (com-next :name t) ()
  (incf (cursor *application-frame*))
  (when (= (cursor *application-frame*)
	   (length (numbers *application-frame*)))
    (setf (cursor *application-frame*) 0)))

;; move the cursor one line up
(define-superapp-command (com-previous :name t) ()
  (decf (cursor *application-frame*))
  (when (minusp (cursor *application-frame*))
    (setf (cursor *application-frame*)
	  (1- (length (numbers *application-frame*))))))

;; Command to quit the app
(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))


;; Exported function to launch an instance of the application frame 
(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

