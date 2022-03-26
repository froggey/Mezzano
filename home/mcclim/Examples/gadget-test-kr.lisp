;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Copied from colorslider

(in-package :clim-internals)

;; example gadget definition
(defclass gadget-test-pane (basic-gadget) ())

(in-package :clim-demo)

(defun gadget-test ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (setq frame (make-application-frame 'gadget-test
                   :frame-manager (make-instance 'clim-internals::pixie/clx-look
                                                      :port (find-port))))
; (setq frame (make-application-frame 'gadget-test))
  (setq fm (frame-manager frame))
  (setq port (port fm))
  (setq pane (first (frame-panes frame)))
  (setq medium (sheet-medium pane))
  (setq graft (graft frame))
  (setq vbox (climi::frame-pane frame))
  (run-frame-top-level frame))

(defun run-pixie-test (name)
  (loop for port in climi::*all-ports*
	do (destroy-port port))
  (setq climi::*all-ports* nil)
  (when name
    (run-frame-top-level
      (make-application-frame name
           :frame-manager (make-instance 'clim-internals:pixie/clx-look
                               :port (find-port))))))

(defmethod gadget-test-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (catch 'exit
    (clim-extensions:simple-event-loop))
  (frame-exit frame))

(let ((korean (make-text-style :sans-serif nil 12 :korean)))
  (make-command-table 'lisp-menu
                      :errorp nil
                      :menu `(("∞Ë±ﬁ" :menu lisp-sub-menu
                                      :text-style ,korean)
                              ("∏ÆΩ¿" :command test
                                      :text-style ,korean)
                              ("Lisp" :command test)))

  (make-command-table 'lisp-sub-menu
                      :errorp nil
                      :menu `(("Does"  :command test)
                              ("This"  :command test)
                              ("Work?" :command test)))

  (make-command-table 'edit-menu
                      :errorp nil
                      :menu `(("Edit" :command test)
                              ("Edit" :command test)
                              ("Edit" :command test)))

  (make-command-table `view-menu
                      :errorp nil
                      :menu '(("View" :command test)
                              ("View" :command test)
                              ("View" :command test)))

  (make-command-table `search-menu
                      :errorp nil
                      :menu '(("Search" :command test)
                              ("Search" :command test)
                              ("Search" :command test))))

(define-command test ()
  (format *error-output* "That was just a test~%")
  (finish-output *error-output*))

(macrolet ((make-pane-constructor (class-name)
             `(defmacro ,class-name (&rest options)
                `(make-pane ',',class-name ,@options))))
  (make-pane-constructor text-field)
  (make-pane-constructor text-edit)
  (make-pane-constructor slider)
  (make-pane-constructor push-button)
  (make-pane-constructor toggle-button))

(define-application-frame gadget-test
    () ()
    (:menu-bar
     (("Lisp"   :menu lisp-menu)
      ("Edit"   :menu edit-menu)
      ("View"   :menu view-menu)
      ("Search" :menu search-menu)))
    (:panes
;    (raised     (raising (:border-width 3 :background +Gray83+)
;                  (make-pane 'check-box :choices '("First" "Second" "Third"))))
     (tf1        :push-button
                 :text-style (make-text-style :fix :roman 24 :korean)
                 :label "∫ªπÆ¿Ã :fix ¿Ãø‰")
     (tf2        :push-button
                 :text-style (make-text-style :serif :roman 24 :korean)
                 :label "‹‚Ÿ˛¿Ã :serif ¿Ãø‰")
     (tf3        :push-button
                 :text-style (make-text-style :serif :italic 24 :korean)
                 :label "¿Ã≈≈∏Ø italic ¿Ã≈≈∏Ø")
     (tf4        :push-button
                 :text-style (make-text-style :sans-serif '(:bold :italic) 24 :korean)
                 :label "»π¿Ã ±Ω¿∫ »∞¿⁄∞˙ ¿Ã≈≈∏Ø bold-italic")
;    (text-edit  :text-editor
;                :value "Text Editor")
     (slider-h   :slider
                 :min-value 0
                 :max-value 100
                 :value 0
                 :show-value-p t
                 :orientation :horizontal)
     (slider-v   :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :value 0)
     (slider-v1  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :value 0)
     (slider-v2  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :value 0)
     (slider-v3  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :value 0)
     (slider-v4  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :value 0)
     (push-btn   (lowering (:border-width 3 :background +Gray83+)
                   (horizontally ()
                     (push-button
                       :label "Push Me")
                     (push-button
                       :label "No, Push Me")
                     (push-button
                       :label "Me!"))))
     (table (lowering (:border-width 3 :background +Gray83+)
              (tabling (:height 50)
                (list (push-button :label "A") (push-button :label "B"))
                (list (push-button :label "C") (push-button :label "D"))
                (list (push-button :label "E") (push-button :label "F")))))
     (toggle-btn :toggle-button
                 :label "Toggle"
                 :value t
                 :normal +red+
                 :highlighted +red+
                 :pushed-and-highlighted +red+)
     (scroll    (raising (:border-width 1 :background +Gray83+)
                   (scrolling (:background +Gray83+)
                     (horizontally ()
                       (vertically ()
                         (push-button :label "This is a button")
                         (push-button :label "That is a button")
                         (push-button :label "This is a button too"))
                       (with-radio-box (:orientation :vertical)
                         (clim:radio-box-current-selection "First")
                         "Second" "Third"
                         "Red" "Blue" "Orange"
                         "Elephant" "Dog" "Cat")
                       (with-radio-box (:orientation :vertical :type :some-of)
                         (clim:radio-box-current-selection "Fourth") "Fifth" "Sixth")
                       (with-radio-box (:orientation :vertical)
                         (clim:radio-box-current-selection "Seventh") "Eighth" "Ninth")
                       (with-radio-box (:orientation :vertical :type :some-of)
                         (clim:radio-box-current-selection "Tenth") "Eleventh" "Twelth")))))
     (radio-box  (with-radio-box (:orientation :horizontal)
                   (clim:radio-box-current-selection "One") "Two" "Three"))
     (check-box  (with-radio-box (:type :some-of :orientation :horizontal)
                   (clim:radio-box-current-selection "First") "Second" "Third")))
    (:layouts
     (default
       (raising (:border-width 5 :background +Gray83+)
         (vertically ()
           tf1 tf2 tf3 tf4
           slider-h
           (horizontally ()
             (vertically ()
               slider-v
               slider-v2)
             slider-v3
             slider-v4)
           push-btn
           table
           toggle-btn
           scroll
           radio-box
           check-box
           ))))
    (:top-level (gadget-test-frame-top-level . nil)))
