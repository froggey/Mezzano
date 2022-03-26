
;;;---------------------------------------------------------------------------
;;; Copyright (c) 2005-2007 Robert P. Goldman and Smart Information
;;; Flow Technologies, d/b/a SIFT, LLC
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
;;;
;;; All rights reserved.
;;;
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    A system that adds a new type of graph to the
;;;    format-graph-from-roots protocol for McCLIM.
;;;
;;;
;;;---------------------------------------------------------------------------


(defpackage :mcclim-tree-with-cross-edges-system (:use :cl :asdf))
(in-package :mcclim-tree-with-cross-edges-system)

(defsystem :mcclim-tree-with-cross-edges
    :depends-on (:mcclim)
    :serial t
    :components
    ((:file "tree-with-cross-edges")))




