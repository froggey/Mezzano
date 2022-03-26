;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-DEMO; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The demo demo
;;;   Created: 2002-02-11
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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

(in-package :clim-demo)

(defun make-demo-button (title demo-frame-class)
  (make-pane 'push-button
             :label title
             :activate-callback
             (let ((frame nil))
               (lambda (&rest ignore)
                 (declare (ignore ignore))
                 (cond ((null frame)    ;; I broke this logic, sorry.. -Hefner
                        (setq frame
                          (run-frame-top-level
			   (make-application-frame
			    demo-frame-class
			    :calling-frame *application-frame*))))
                       (t
                        #+nil
                        (destroy-frame frame)))))))

(defun run-demo (name &key background)
  "Coerces `name' into symbol in package `clim-demo' and runs application
denoted by this symbol."
  (let ((frame (make-application-frame
                (find-symbol (string-upcase (string name))
                             (find-package "CLIM-DEMO")))))
    (if background
        (bt:make-thread #'(lambda () (run-frame-top-level frame))
                        :initial-bindings `((*default-server-path* . ',*default-server-path*)))
        (run-frame-top-level frame))
    frame))

(defgeneric display (frame pane)
  (:documentation "Generic method meant to be specialized at least on
the first argument to avoid creating too many functions with similar
name."))

(define-application-frame demodemo
    () ()
    (:menu-bar nil)
    (:layouts
     (default
         (vertically (:equalize-width t)
           (progn ;;spacing (:thickness 10)
             (labelling (:label "McCLIM Demos"
                                :text-style (make-text-style :sans-serif :roman :huge)
                                :align-x :center)))
           (progn ;; spacing (:thickness 10)
             (horizontally ()
               ;; '+fill+
               (labelling (:label "Demos")
                 (vertically (:equalize-width t)
                   (make-demo-button "CLIM-Fig"  'clim-fig)
                   (make-demo-button "Calculator"  'clim-demo.calculator:calculator-app)
                   (make-demo-button "Method Browser" 'method-browser)
                   (make-demo-button "Address Book"  'clim-demo.address-book:address-book)
                   (make-demo-button "Puzzle"  'puzzle)
                   (make-demo-button "Colorslider" 'colorslider)
                   (make-demo-button "Logic Cube" 'logic-cube)
                   (make-demo-button "Gadget Test"  'gadget-test)
                   (make-demo-button "D&D Translator" 'drag-test)
                   (make-demo-button "Draggable Graph" 'draggable-graph-demo)
		   (make-pane 'push-button
			      :label "Font Selector"
			      :activate-callback
			      (lambda (&rest ignore)
				(declare (ignore ignore))
				(format *trace-output* "~&You chose: ~A~%"
					(select-font))))
                   (make-demo-button "Tab Layout" 'clim-demo.tabdemo:tabdemo)
                   (make-demo-button "Summation" 'summation)
                   (make-demo-button "Slider demo" 'clim-demo.slider:sliderdemo)
                   (make-demo-button "German Towns" 'clim-demo.town-example:town-example)
                   (make-demo-button "Data Graph Toy" 'graph-toy)
                   (make-demo-button "Traffic lights" 'traffic-lights)
                   (make-demo-button "Image Transform" 'clim-demo.image-transform-demo:image-transform-demo)
                   (make-demo-button "Selection (clipboard)" 'selection-demo)
                   (make-demo-button "DND various" 'clim-demo.drag-and-drop-example:dnd-commented)
                   (make-demo-button "File manager" 'clim-demo.file-manager:file-manager)
                   #+ (or) (make-demo-button "Stopwatch" 'clim-demo.stopwatch:stopwatch)))
               (labelling (:label "Tests")
                 (vertically (:equalize-width t)
                   (make-demo-button "Stream test" 'stream-test)
                   (make-demo-button "Label Test" 'label-test)
                   (make-demo-button "Table Test" 'table-test)
                   (make-demo-button "Scroll Test" 'Scroll-test)
                   (make-demo-button "List Test" 'list-test)
                   (make-demo-button "Option Test" 'option-test)
                   (make-demo-button "HBOX Test"  'hbox-test)
                   (make-demo-button "Text Size Test"  'text-size-test)
                   (make-demo-button "Drawing Benchmark" 'drawing-benchmark)
                   (make-demo-button "Border Styles Test" 'bordered-output)
                   (make-demo-button "Misc. Tests" 'clim-demo.misc:misc-tests)
                   (make-demo-button "Render Image Tests" 'render-image-tests)
                   (make-demo-button "Drawing Tests" 'drawing-tests)
                   (make-demo-button "Accepting Values Test"  'av-test)
                   (make-demo-button "Frame and Sheet Names Test" 'frame-sheet-name-test)
                   (make-demo-button "Tracking Pointer test" 'tracking-pointer-test)))
               (labelling (:label "Regression Tests")
                 (vertically (:equalize-width t)
                   (make-demo-button "Image viewer" 'image-viewer)
                   (make-demo-button "Coordinate swizzling"
                                     'clim-demo.coord-swizzling:coordinate-swizzling)
                   (make-demo-button "Scroll Test 2" 'Scroll-test-2)
                   (make-demo-button "Tables with borders" 'table-demo)
                   (make-demo-button "Menu Test"  'clim-demo.menu-test:menu-test)
                   (make-demo-button "Drag and Drop" 'dragndrop)
                   (make-demo-button "Pane hierarchy viewer" 'clim-demo.hierarchy:hierarchy)
                   (make-demo-button "Patterns, designs and inks" 'pattern-design-test)
                   (make-demo-button "Flipping ink" 'flipping-ink)
                   (make-demo-button "Overlapping patterns" 'patterns-overlap)
                   (make-demo-button "Text transformations" 'text-transformations-test)
                   (make-demo-button "Text multiline positioning" 'text-multiline-positioning)
                   (make-demo-button "SEOS baseline and wrapping" 'seos-baseline)
                   (make-demo-button "Indentation" 'indentation)))))))))

(defun demodemo ()
  (run-frame-top-level (make-application-frame 'demodemo)))

(define-application-frame hbox-test
    () ()
    (:menu-bar nil)
    (:layouts
     (default
         (horizontally ()
           30
           (make-pane 'push-button :label "Okay"
                      :width '(50 :mm))
           '+fill+
           (make-pane 'push-button :label "Cancel")
           '+fill+
           (make-pane 'push-button :label "Help")
           5
           ) )))

(define-application-frame table-test
    () ()
    (:menu-bar nil)
    (:layouts
     (default
         (tabling (:background +red+)
           (list (make-pane 'push-button :label "Last Name" :max-height +fill+)
                 (make-pane 'push-button :label "First Name" #||:max-height +fill+||#))
           (list (make-pane 'push-button :label "C 1 0")
                 (make-pane 'push-button :label "C 1 1"))
           ) )))

(defun make-label-test-column (title label content)
  (flet ((make-label (align-x align-y)
           (let ((alignment-text (format nil "~S" (list align-x align-y)))
                 (text-style (make-text-style :sans-serif :roman :normal)))
             (macrolet ((frob (label &body contents)
                          `(labelling (:label ,label
                                       :align-x align-x
                                       :label-alignment align-y
                                       :foreground +white+
                                       :background +paleturquoise4+
                                       :text-style text-style)
                             ,@contents)))
               (ecase content
                 (:child (frob label
                           (make-pane 'push-button :label alignment-text
                                                   :text-style text-style
                                                   :max-width 1000
                                                   :max-height 1000)))
                 (:alignment (frob alignment-text))
                 (:label (frob label)))))))
    (labelling (:label title)
      (vertically (:spacing 5 :equalize-width t)
        (make-label :left :top)
        (make-label :center :top)
        (make-label :right :top)
        (make-label :left :bottom)
        (make-label :center :bottom)
        (make-label :right :bottom)))))

(define-application-frame label-test ()
  ()
  (:menu-bar nil)
  (:layouts
   (default
    (vertically (:equalize-width t)
      10
      (labelling (:label "CLIM Label Tests"
                  :align-x :center
                  :text-style (make-text-style :sans-serif :roman :huge)))
      10
      (9/10 (horizontally (:equalize-height t)
              ;; Please keep the silly "good" so the label text goes
              ;; below the baseline. -- jm 2019-12-14
              (1/4 (make-label-test-column
                    "Labels with content" #1="Some good label" :child))
              (1/4 (make-label-test-column
                    "Labels without content" #1# :alignment))
              (1/4 (make-label-test-column
                    "Multi-line w/ content"
                    #2=#.(format nil "Multi-line~%label")
                    :child))
              (1/4 (make-label-test-column
                    "Multi-line w/o content" #2# :label))))))))

(defclass foo-pane (basic-pane clime:always-repaint-background-mixin)
  ())

(defmethod compose-space ((pane foo-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width 800
                          :height 1e3))

(defmethod handle-repaint ((pane foo-pane) region)
  (draw-line* pane 50 50 200 50)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (declare (ignore x1 x2))
    (let ((k 20))
      (loop for y from (* k (floor (- y1 10) k)) below (+ y2 10) by k do
            (draw-text* pane (format nil "~D" y) 20 y)))))

(define-application-frame scroll-test
    () ()
    (:menu-bar nil)
    (:layouts
     (defaults
         (scrolling (:width 400 :height 400)
           (make-pane 'foo-pane)))))


;;; Scroll test 2
(define-application-frame scroll-test-2 ()
  ()
  (:menu-bar nil)
  (:geometry :width 1200 :height 400)
  (:panes (out :application-pane :display-function #'scroll-test-display)
          (bam :application-pane :display-function #'scroll-test-display)
          (foo :application-pane :display-function #'scroll-test-display)
          (qux :application-pane :display-function #'scroll-test-display))
  (:layouts (default
                (vertically ()
                  (horizontally ()
                    (labelling (:label "bam") (scrolling () bam))
                    (labelling (:label "foo") (scrolling () foo))
                    (labelling (:label "qux") (scrolling () qux)))
                  (labelling (:label "Description") out)))))

(defmethod scroll-test-display ((frame scroll-test-2) pane)
  (when (member (pane-name pane) '(bam qux))
    (draw-rectangle* pane -100 -100 +100 +100 :filled nil
                     :ink +red+ :line-thickness 5 :line-dashes t))
  (when (member (pane-name pane) '(foo qux))
    (draw-rectangle* pane +300 +300 +500 +500 :filled nil
                     :ink +red+ :line-thickness 5 :line-dashes t))

  (when (member (pane-name pane) '(out))
    (format pane "In this test we draw three panes: bam, foo and qux. You may try resizing the window and see what happens. If viewports are small enough, scroll-bars should appear to show bottom-right rectangle on \"foo\" and \"qux\". Upper-left rectangle has starting point having negative coordinates of its sheet, so scroll-bars won't update to uncover it (this may be supported in the future - in such case please change description of this test). Only bottom-right rectangle must be seen in full.

\"bam\" should have only partially drawn rectangle (upper-left corner).
\"foo\" is similar, but rectangle is drawn in bottom-right corner. This draw should extend scroll-bars, so user may see whole rectangle.
\"qux\" combines both previous panes. Part of the rectangle in the upper-left corner and (after scrolling) full rectangle on the bottom-right corner.")))

(define-application-frame list-test
    () ()
    (:menu-bar nil)
    (:panes
     (substring :text-field :value "INTER"
                :value-changed-callback
                (lambda (pane value)
                  (declare (ignore value))
                  (when (find-pane-named *application-frame* 'result-list)
                    (update-list-test pane))))
     (result-list
      (make-pane 'list-pane
		 :value 'clim:region-intersection
		 :items (apropos-list "INTER" :clim)
		 :presentation-type-key (constantly 'list-test-symbol)
		 :name-key (lambda (x) (format nil "~(~S~)" x))))
     (interactor :interactor :height 200))
    (:layouts
     (defaults
         (labelling (:label "Matching symbols"
                            :text-style (make-text-style :sans-serif :roman :normal))
           (vertically ()
	     (scrolling (:height 200)
	       result-list)
	     (horizontally ()
	       substring
	       (make-pane 'push-button
			  :label "Update"
			  :activate-callback 'update-list-test))
	     interactor)))))

(define-presentation-type list-test-symbol ())

(define-list-test-command com-describe-symbol
    ((sym list-test-symbol :gesture :select))
  ;; Let's print only three lines, we don't have space for more.
  (with-input-from-string (s (with-output-to-string (s) (describe sym s)))
    (dotimes (x 3)
      (write-line (read-line s nil "") *standard-input*))))

(defun update-list-test (pane)
  (declare (ignore pane))
  (setf (list-pane-items (find-pane-named *application-frame* 'result-list))
	(apropos-list (gadget-value
		       (find-pane-named *application-frame* 'substring))
		      :clim #+sbcl t)))

(define-application-frame option-test
    () ()
    (:menu-bar nil)
  (:panes (option-pane-1 :option-pane
                         :value 1
                         :items '(1 2 3 4 6 7)
                         :value-changed-callback (constantly nil))
          (option-pane-2 :option-pane
                         :value "Option 1"
                         :items '("Option 1" "Option 2" "Option 3" "Option 4" "Option 6" "Option 7")
                         :value-changed-callback (constantly nil)))
  (:layouts
   (:default
    (labelling (:label "Option panes example")
      (vertically ()
        (1/2 option-pane-1)
        (1/2 option-pane-2))))))

(format t "~&;; try (CLIM-DEMO:DEMODEMO)~%")
