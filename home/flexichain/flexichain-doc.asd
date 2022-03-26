;;; flexichain-doc
;;; ASDF system definition
;;;
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;; Copyright (C) 2008       Cyrus Harmon (ch-lisp@bobobeach.com)
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

(asdf:defsystem :flexichain-doc
  :name "flexichain-doc"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components
  ((:module "Doc"
            :components 
            ((:static-file "Makefile")
             (:static-file flexichain-tex :pathname #p"flexichain.tex")
             (:static-file spec-macros-tex :pathname #p"spec-macros.tex")
             (:static-file circular-fig :pathname #p"circular.fig")
             (:static-file gap1-fig :pathname #p"gap1.fig")
             (:static-file gap2-fig :pathname #p"gap2.fig")
             (:static-file gap3-fig :pathname #p"gap3.fig")
             (:static-file "tex-dependencies")
             (:static-file "strip-dependence")))))

