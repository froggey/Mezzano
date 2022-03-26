;;; Flexichain
;;; ASDF system definition
;;;
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; The tester is not included, for it depends on clim.  The stupid
;;; implementation has also been left out, since it seems mostly useful
;;; for testing.
(cl:in-package #:asdf-user)

(defsystem :flexichain
  :name "flexichain"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
               (:file "flexichain-package")
               (:file "utilities" :depends-on ("flexichain-package"))
               (:file "flexichain" :depends-on ("utilities" "flexichain-package"))
               (:file "flexicursor" :depends-on ("flexichain"))
               (:file "flexirank" :depends-on ("flexichain"))))
