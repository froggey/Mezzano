;;;  (c) copyright 2019 Jan Moringen

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

(cl:in-package #:clim-postscript.test)

(in-suite :clim-postscript)

(clim:define-command-table output-destination-test-command-table)

(clim:define-command (com-test-output-destination
                      :command-table output-destination-test-command-table
                      :provide-output-destination-keyword t)
    ()
  (clim-test-util:print-test-page-1 *standard-output*))

(test output-destination.smoke
  "Smoke test for PostScript command output destination."

  (finishes
    (let ((destination (make-instance 'clim-postscript::postscript-destination
                                      :file "postscript-output-destination.ps")))
      (com-test-output-destination :output-destination destination))))
