(in-package :cl-user)

(defpackage #:clim-demo.tabdemo
  (:use #:clim #:clim-lisp #:clim-tab-layout)
  (:export #:tabdemo))

(in-package #:clim-demo.tabdemo)

;;; example and testing code

(define-presentation-type special-page ())

(define-application-frame tabdemo ()
    ()
  (:menu-bar tabdemo-menubar)
  (:panes
   (a :text-editor :value "Hello World from page A")
   (b :text-editor :value "Hello World from page B")
   (c :text-editor :value "This is page C speaking")
   (special-page :text-editor
		 :value "This page has a special presentation type")
   (io :interactor :height 150 :width 600)
   (pointer-doc :pointer-documentation))
  (:layouts
   (default
       (vertically ()
	 (with-tab-layout ('tab-page :name 'tabdemo-layout :height 200)
           ("A" a :drawing-options `(:text-style ,(make-text-style nil :bold nil)))
           ("B" b)
           ("C" c)
           ("Special Page" special-page :presentation-type 'special-page
                           :drawing-options `(:text-style ,(make-text-style nil nil :small))))
	 io
	 pointer-doc))))

(define-tabdemo-command (com-remove-tabdemo-page :name t)
    ((page tab-page :prompt "Tab page" :gesture :delete))
  (remove-page page))

(make-command-table 'tabdemo-pages-menu
		    :errorp nil
		    :menu '(("Add Extra Pane" :command com-add-extra-pane)
			    ("Randomize" :command com-randomize-tabdemo)
			    ("Quit" :command com-quit)))

(make-command-table 'tabdemo-properties-menu
		    :errorp nil
		    :menu '(("Change Page Title"
			     :command com-change-page-title)
			    ("Paint Page Red"
			     :command com-paint-page-red)
			    ("Paint Page Green"
			     :command com-paint-page-green)))

(make-command-table 'tabdemo-presentation-tests-menu
		    :errorp nil
		    :menu '(("Choose Any Page"
			     :command com-choose-any-page)
			    ("Choose Special Page"
			     :command com-choose-special-page)))

(make-command-table 'tabdemo-menubar
		    :errorp nil
		    :menu '(("Pages" :menu tabdemo-pages-menu)
			    ("Properties" :menu tabdemo-properties-menu)
			    ("Presentation Tests"
			     :menu tabdemo-presentation-tests-menu)))

;;; This is the main entry point for starting the demo.
(defun tabdemo ()
  (run-frame-top-level (make-application-frame 'tabdemo)))

;; FIXME: It only get errors due to bogus frame names with
;; FIND-PANE-NAMED.  Ignoring the symbol identity and case works
;; around that.
(defun sane-find-pane-named (frame name)
  (map-over-sheets #'(lambda (p)
                       (when (string-equal name (pane-name p))
                         (return-from sane-find-pane-named p)))
                   (frame-panes frame)))

(defun tabdemo-layout ()
  (sane-find-pane-named *application-frame* 'tabdemo-layout))

(define-tabdemo-command (com-add-extra-pane :name t)
    ()
  (let ((fm (frame-manager *application-frame*)))
    (with-look-and-feel-realization (fm *application-frame*)
      (add-page (make-instance 'tab-page
		  :title "X"
		  :pane (make-pane 'text-editor-pane
				   :value "This is an extra page"))
                (tabdemo-layout)
		t))))

(define-tabdemo-command (com-choose-any-page :name t)
    ()
  (format *standard-input* "You choice: ~A~%" (accept 'tab-page)))

(define-tabdemo-command (com-choose-special-page :name t)
    ()
  (accept 'special-page)
  (write-line "Correct answer!  That's the special page." *standard-input*))

(define-tabdemo-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(define-tabdemo-command (com-randomize-tabdemo :name t)
    ()
  (setf (tab-layout-pages (tabdemo-layout))
	(alexandria:shuffle (tab-layout-pages (tabdemo-layout)))))

(defmacro with-enabled-tab-layout-page (var &body body)
  `(let ((,var (tab-layout-enabled-page (tabdemo-layout))))
     (unless (null page)
       ,@body)))

(define-tabdemo-command (com-change-page-title :name t)
    ()
  (with-enabled-tab-layout-page page
    (setf (tab-page-title page)
	  (accept 'string
		  :prompt "New title"
		  :default (tab-page-title page)))))

(define-tabdemo-command (com-paint-page-red :name t)
    ()
  (with-enabled-tab-layout-page page
    (setf (getf (tab-page-drawing-options page) :ink) +red+)))

(define-tabdemo-command (com-paint-page-green :name t)
    ()
  (with-enabled-tab-layout-page page
    (setf (getf (tab-page-drawing-options page) :ink)
          +green+
          (getf (tab-page-drawing-options page) :text-style)
          (make-text-style nil nil :small))))
