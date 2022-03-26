;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TOWN-EXAMPLE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Example CLIM Application: Large Cities of Germany
;;;    Topics: custom view classes, different present presentation methods for
;;;            different views, completion for accept presentation methods,
;;;            accepting-values dialogues, presentation to command translators,
;;;            partial commands in menus, automatically generated menus, 
;;;            and keystroke gestures for commands.
;;;     Usage: Compile and load the file and call (town-example:run) afterwards.
;;;   Created: 2005-08-17, Version 1.3 (same date)
;;;    Author: Max-Gerd Retzlaff <m.retzlaff@gmx.net>, http://bl0rg.net/~mgr
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Max-Gerd Retzlaff

;;; define the package

(defpackage #:clim-demo.town-example
  (:use #:clim #:clim-lisp)
  (:export #:run #:town-example))

(in-package #:clim-demo.town-example)

;;; view class for graphical presentations

(defclass graphical-view (view)
  ())

(defparameter +graphical-view+ (make-instance 'graphical-view))

;;; an application frame consistiong of one pane

(define-application-frame town-example ()
  ()
  (:panes
   (map :application :height 500 :width 500 :scroll-bars nil
        :background +dark-blue+  :display-function #'draw-map
        :default-view +graphical-view+)
   (pointer-doc :pointer-documentation)
   (interactor :interactor :height 163 ;; 130 105 95
               :scroll-bars nil))
   (:layouts
    (default (vertically ()
               map
               interactor
               pointer-doc)))
   (:top-level (default-frame-top-level :prompt #'town-example-prompt)))

(defun town-example-prompt (pane frame)
  (declare (ignore frame))
  (window-clear pane)
  (with-text-face (pane :roman)
    (write-string "> " pane)))

(defun draw-map (frame pane)
  "Draws a stylized map of Germany"
  (declare (ignore frame))
  (draw-polygon* pane '(172  22   228  40   227  59   264  60   256  80   277  88
                        319  54   336  58   345  43   353  55   345  71   370  86
                        384 124   375 141   393 159   398 208   416 237   410 258
                        397 252   314 302   333 351   380 389   341 426   350 461
                        324 452   280 471   252 462   240 474   172 448   166 460
                        132 457   140 410   160 378   116 368    92 346    79 307
                         94 295    82 252    90 229    84 204   113 201   112 162
                        129 142   130 104   157 102   174 118   182  96   204  96
                        186  58   196 50)
                 :ink +dark-green+)
  ;; present all towns (later..)
  (com-present-towns))

;;; a function to start the demonstration

(defun run ()
  (run-frame-top-level (make-application-frame 'town-example)))

;;; a command to quit the program

(define-town-example-command (com-quit :name t :menu t ;; show in menu
                                       :keystroke (#\q :meta)) ;; a keystroke
    ()
  (frame-exit *application-frame*))

;;; a class for towns

(defclass town ()
  ((name :initarg :name :accessor town-name)
   (coordinates :initarg :coordinates :accessor town-coordinates
               :initform (make-point 156 68)) ;; Helogoland..
   (population :initarg :population :accessor town-population
               :initform nil)))

;;; a hash to store the towns

(defvar *towns* (make-hash-table :test #'equal))

;;; slightly nicer function to create new town instances

(defun make-town (name x y &optional population)
  (let ((town (make-instance 'town :name name
                             :coordinates (make-point x y))))
    (when population
      (setf (town-population town) population))
    town))
  
;;; clos magic to automatically store all created towns in the hash

(defmethod initialize-instance :after ((town town) &key)
  (setf (gethash (town-name town) *towns*) town))

;;; function to find a town

(defun find-town (name &optional (errorp t))
  (or (gethash name *towns*) ;; not the best style..
      (and errorp (error "~&No town named ~S in the database." name))))

;;; printer method for town instances

(defun slot-value-or-something (object &key (slot 'name) (something "without name"))
  (if (slot-boundp object slot)
      (slot-value object slot)
      something))

(defmethod print-object ((town town) stream)
  (print-unreadable-object (town stream :type t)
    (write-string (slot-value-or-something town) stream)))

;;; create some towns.. (21th biggest cities of Germany on 12-31-2003)

(make-town "Berlin"            353 166 3390000)
(make-town "Hamburg"           229 106 1730000)
(make-town "Munich"            292 427 1250000)
(make-town "Cologne"           116 260  970000)
(make-town "Frankfurt"         179 310  640000)
(make-town "Dortmund"          136 226  590000)
(make-town "Essen"             120 228  590000)
(make-town "Stuttgart"         196 392  590000)
(make-town "Duesseldorf"       110 242  570000)
(make-town "Bremen"            187 134  540000)
(make-town "Hanover"           222 177  520000)
(make-town "Duisburg"          108 231  510000)
(make-town "Leipzig"           318 238  500000)
(make-town "Nuremberg"         272 352  490000)
(make-town "Dresden"           370 252  480000)
(make-town "Bochum"            130 228  390000)
(make-town "Wuppertal"         130 238  360000)
(make-town "Bielefeld"         176 198  330000)
(make-town "Bonn"              120 276  310000)
(make-town "Mannheim"          172 349  310000)
(make-town "Karlsruhe"         168 377  280000)


;;; a presantation type for town would look like this:
;;;
;;; (clim:define-presentation-type town ())
;;;
;;; But we don't have to specify it as clim-spec 8.6.2 says:
;;;   "If your presentation type has the same name as a class, doesn't
;;;    have any parameters or options, doesn't have a history, and
;;;    doesn't need a special description, you do not need to call
;;;    define-presentation-type."

;;; accept method for a town presentation (in any view mode):

(define-presentation-method accept ((type town) stream view &key)
  (values               ;suppress values after the first
   ;; provide completion over the names of the towns
   (completing-from-suggestions (Stream :partial-completers '(#\Space))
     (maphash #'suggest *towns*))))


;;; how to present a town in CLIM in text-mode:

(define-presentation-method present (town (type town) stream
                                          (view textual-view) &key)
  (write-string (town-name town) stream))

;;; .. and graphically:

(defparameter *population->town-circle-factor* 20000
  "Towns are graphically represented as circles.
Factor to reduce the size of the circles")

(defun population->town-circle-diameter (town)
  "Towns are graphically represented as circles."
;;;; first version: diameter is linearly proportional to the population
;;;   (round (/ (or (town-population town) 150000)
;;;             *population->town-circle-factor*))) ;; 100000
;;;
;;;; second version: area of the circle is linearly proportional to the population
;;;   (round (sqrt (/ (or (town-population town) 15000)
;;;                   *population->town-circle-factor* ;; 8000
;;;                   pi))))
;;;
;;;; third version: produces pleasant proportions
   (round (expt (/ (or (town-population town) 150000)
                   *population->town-circle-factor* ;; 20000
                   pi)
                3/4)))

(define-presentation-method present (town (type town) stream
                                          (view graphical-view) &key)
  (clim:draw-circle stream
                    (town-coordinates town)
                    (population->town-circle-diameter town)
                    :ink +dark-red+))

;;; command that presents all towns (used in #'draw-map)

;;; This could be a normal function instead, just replace the first line by:
;;;   (defun com-present-towns
(define-town-example-command (com-present-towns)
    ()
  (maphash (lambda (key value)
             (declare (ignore key))
             (clim:present value 'town))
             *towns*))

;;; show info about a town (using a pop-up window)

(define-town-example-command (com-show-town-info :name t :menu t
                                                 :keystroke (#\i :meta))
    ((town town :prompt " Which town? "))
  ;; (present town 'town :view +textual-view+)
  (notify-user *application-frame*
	       (format nil "~A has ~:d inhabitants."
		       (town-name town)
		       (or (town-population town) "some"))
	       :title (format nil "Information on ~A" (town-name town))
	       :text-style '(:serif :roman 15)))

;;; show info on town :select gesture (left click)

(define-presentation-to-command-translator info-for-town
    (town com-show-town-info town-example
          :gesture :select
          :documentation "Show info for this town.")
    (object)
  (list object))


;;; get distance between two towns

(defun get-distance-between-points (a b)
  "Ask Pythagoras or Euclid."
  (round (sqrt (+  (expt (- (point-x a)
                            (point-x b))
                         2)
                   (expt (- (point-y a)
                            (point-y b))
                         2)))))

(define-town-example-command (com-get-distance :name t :menu t
                                               :keystroke (#\d :meta))
    ((town-a town :prompt "Town a")
     (town-b town :prompt "Town b"))
  (notify-user *application-frame*
	       (format nil "It's ~d pixels from ~a to ~a."
		       (get-distance-between-points (town-coordinates town-a)
						    (town-coordinates town-b))
		       (town-name town-a)
		       (town-name town-b))
	       :title "Distance"
	       :text-style '(:serif :roman 15)))

;;; get distance on :describe gesture (middle click)
;;; (ask via accept for the second town)

;;; Defunct and not really nice. Note that the version below is simpler,
;;; working, more elegant, and more intuitive, as the user sees the same
;;; as if he had entered the com-get-distance command via the keyboard.
;;;
;;; (define-presentation-to-command-translator distance-between-two-towns
;;;     (town com-get-distance town-example
;;;           :gesture :describe
;;;           :documentation "Get distance from this town to another.")
;;;     (object)
;;;   (list object
;;;         (let (town-b)
;;;           (accepting-values-with-style-and-title (stream)
;;;             (format stream "~&Get distance~%")
;;;             (format stream "~%From ~a to: " (town-name object))
;;;             (setf town-b (accept 'town :prompt nil :stream stream :query-identifier 'tag)))
;;;           town-b)))

(define-presentation-to-command-translator distance-between-two-towns
    (town com-get-distance town-example
          :gesture :describe
          :documentation "Get distance from this town to another.")
    (object)
  (list object
        (let ((stream *query-io*))
          (format stream "  Get distance (Town a) ~a (Town b) " (town-name object))
          (accept 'town :prompt nil :stream stream :query-identifier 'tag))))
