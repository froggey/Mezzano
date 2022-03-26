;;;  (c) copyright 2000 Robert Strandh (robert.strandh@gmail.com)

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

(defpackage #:clim-demo.menu-test
  (:use #:clim #:clim-extensions #:clim-lisp)
  (:export #:menu-test))

(in-package #:clim-demo.menu-test)

(define-application-frame menu-test ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (screen :application
           :display-time nil
           :text-style (make-text-style :sans-serif :roman :normal)))
  (:layouts
   (defaults (vertically () screen))))

(define-menu-test-command com-file ()
  (format *standard-output* "You pressed the File button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-kenobi ()
  (format *standard-output* "You pressed the General Kenobi button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-konichiwa ()
  (format *standard-output* "You pressed the Konichiwa button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-get-in-the-robot ()
  (format *standard-output* "You pressed the Get in the robot button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-hi ()
  (format *standard-output* "You pressed the Hi button.~%")
  (finish-output *standard-output*))

(make-command-table 'kenobi-command-table
                    :errorp nil
                    :menu '(("General Kenobi"   :command com-kenobi
                                                :text-style (nil :bold nil))
                            ("Konichiwa"        :command com-konichiwa
                                                :text-style (nil :italic nil))
                            ("Get in the robot" :command com-get-in-the-robot
                                                :text-style (:fix nil :large))))

(make-command-table 'buffer-command-table
                    :errorp nil
                    :menu '(("Hello there" :menu kenobi-command-table)
                            ("Hi there"    :command com-hi)))

(make-command-table 'menubar-command-table
                    :errorp nil
                    :menu '(("Buffer" :menu buffer-command-table)
                            ("File"   :command com-file)))
