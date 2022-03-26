;;; part of application "business logic"
(defclass person ()
  ((%last-name :initarg :last-name :accessor last-name)
   (%first-name :initarg :first-name :accessor first-name)
   (%address :initarg :address :accessor address)
   (%membership-number :initarg :membership-number :reader membership-number)))

;;; constructor for the PERSON class.  Not strictly necessary. 
(defun make-person (last-name first-name address membership-number)
  (make-instance 'person 
                 :last-name last-name 
                 :first-name first-name
                 :address address
                 :membership-number membership-number))

;;; initial list of members of the organization we imagine for this example
(defparameter *members*
  (list (make-person "Doe" "Jane" "123, Glencoe Terrace" 12345)
        (make-person "Dupont" "Jean" "111, Rue de la Republique" 54321)
        (make-person "Smith" "Eliza" "22, Trafalgar Square" 121212)
        (make-person "Nilsson" "Sven" "Uppsalagatan 33" 98765)))

;;; the CLIM view class that corresponds to a list of members, one member
;;; per line of text in a CLIM application pane. 
(defclass members-view (view) ())

;;; since this view does not take any parameters in our simple example,
;;; we need only a single instance of it. 
(defparameter *members-view* (make-instance 'members-view))

;;; the application frame.  It contains instance-specific data
;;; such as the members of our organization. 
(define-application-frame views ()
  ((%members :initform *members* :accessor members))
  (:panes
   (main-pane :application :height 500 :width 500
              :display-function 'display-main-pane
              ;; notice the initialization of the default view of
              ;; the application pane. 
              :default-view *members-view*)
   (interactor :interactor :height 100 :width 500))
  (:layouts
   (default (vertically ()
              main-pane
              interactor))))

;;; the trick here is to define a generic display function
;;; that is called on the frame, the pane AND the view, 
;;; whereas the standard CLIM display functions are called 
;;; only on the frame and the pane.
(defgeneric display-pane-with-view (frame pane view))

;;; this is the display function that is called in each iteration
;;; of the CLIM command loop.  We simply call our own, more elaborate
;;; display function with the default view of the pane. 
(defun display-main-pane (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

;;; now we can start writing methods on our own display function
;;; for different views.  This one displays the data each member
;;; on a line of its own.
(defmethod display-pane-with-view (frame pane (view members-view))
  (loop for member in (members frame)
        do (with-output-as-presentation 
               (pane member 'person)
             (format pane "~a, ~a, ~a, ~a~%"
                     (membership-number member)
                     (last-name member)
                     (first-name member)
                     (address member)))))

;;; this CLIM view is used to display the information about
;;; a single person.  It has a slot that indicates what person
;;; we want to view. 
(defclass person-view (view)
  ((%person :initarg :person :reader person)))

;;; this method on our own display function shows the detailed 
;;; information of a single member. 
(defmethod display-pane-with-view (frame pane (view person-view))
  (let ((person (person view)))
    (format pane "Last name: ~a~%First Name: ~a~%Address: ~a~%Membership Number: ~a~%"
            (last-name person)
            (first-name person)
            (address person)
            (membership-number person))))

;;; entry point to start our applciation
(defun views-example ()
  (run-frame-top-level (make-application-frame 'views)))

;;; command to quit the application 
(define-views-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;;; command to switch the default view of the application pane
;;; (which is the value of *standard-output*) to the one that
;;; shows a member per line. 
(define-views-command (com-show-all :name t) ()
  (setf (stream-default-view *standard-output*) *members-view*))
    
;;; command to switch to a view that displays a single member. 
;;; this command takes as an argument the person to display.  
;;; In this application, the only way to satisfy the demand for
;;; the argument is to click on a line of the members view.  In 
;;; more elaborate application, you might be able to type a
;;; textual representation (using completion) of the person. 
(define-views-command (com-show-person :name t) ((person 'person))
  (setf (stream-default-view *standard-output*)
        (make-instance 'person-view :person person)))

