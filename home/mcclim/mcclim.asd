;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2005 by
;;;           Andreas Fuchs (asf@boinkor.net)
;;;
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

;;; The actual McCLIM system that people should to use in their ASDF
;;; package dependency lists.
(in-package #:asdf-user)

(defsystem "mcclim"
  :author ("Alessandro Serra"
           "Alexey Dejneka"
           "Andreas Fuchs"
           "Andy Hefner"
           "Arnaud Rouanet"
           "Brian Mastenbrook"
           "Brian Spilsbury"
           "Christophe Rhodes"
           "Clemens Fruhwirth"
           "Cyrus Harmon"
           "Daniel Barlow"
           "Daniel Kochmański"
           "Duncan Rose"
           "Edena Pixel"
           "Elias Mårtenson"
           "Frank Buss"
           "Gilbert Baumann"
           "Iban Hatchondo"
           "Julien Boninfan"
           "Lionel Salabartan"
           "Max-Gerd Retzlaff"
           "Mike McDonald"
           "Nisar Ahmad"
           "Peter Mechleborg"
           "Rainer Joswig"
           "Robert Goldman"
           "Robert Strandh"
           "Rudi Schlatte"
           "Timothy Moore")
  :license "LGPL-2.1+"
  :version "0.9.7"
  :description "McCLIM is an implementation of the CLIM 2.0 specification."
  :long-description "McCLIM is an implementation of the CLIM 2.0 specification.

CLIM (Common Lisp Interface Manager) is an advanced graphical user
interface management system."
  :depends-on ("mcclim/looks" "mcclim/extensions")
  :in-order-to ((test-op (test-op "mcclim/test"))))

;;; A system that loads the appropriate backend for the current
;;; platform.
(defsystem #:mcclim/looks
  :depends-on (#:clim
               #-(or mcclim-beagle mcclim-ugly mezzano)
                                #:mcclim-clx/pretty  #| adds truetype        |#
               #+mcclim-ugly    #:mcclim-clx         #| raw clim-clx backend |#
               #+mcclim-beagle  #:mcclim-beagle      #| OSX native (clozure) |#
               #+mezzano        #:mcclim-mezzano     #| Mezzano (a LISP OS)  |#

               ;; null backend
               #:mcclim-null))

(defsystem #:mcclim/extensions
  :depends-on (#:mcclim-bitmaps
               #:conditional-commands
               #:mcclim-layouts/tab
               #:mcclim-bezier
               #:clim-pdf
               #:clim-postscript
               #:mcclim-franz))

(defmethod perform :after ((op load-op) (c (eql (find-system :mcclim))))
  (pushnew :clim *features*)) ; The fact that CLIM itself is available is true when all is loaded.

(defsystem "mcclim/test"
  :depends-on ("mcclim"
               "fiveam")
  :components ((:module "Tests"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "input-editing")
                             (:file "commands")
                             (:file "presentations")
                             (:file "text-selection")
                             (:file "text-formatting")
                             (:file "text-styles")
                             (:file "setf-star")
                             (:module "geometry"
                              :depends-on ("package")
                              :serial t
                              :components ((:file "transforms")
                                           (:file "regions")
                                           (:file "bounding-rectangles")))
                             (:module "drawing"
                              :depends-on ("package")
                              :components ((:file "medium")
                                           (:file "design"))))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:clim-tests '#:run-tests)))

(defsystem "mcclim/test-util"
  :depends-on ("mcclim")
  :components ((:module "Tests/util"
                :serial t
                :components ((:file "package")
                             (:file "test-page")))))

;; The fact that our CLIM implementation is McCLIM is already true now.
;; This feature is notably used by ESA and DREI, in cases where they need to
;; know whether they are compiled with McCLIM or another CLIM implementation.
(pushnew :mcclim *features*)
