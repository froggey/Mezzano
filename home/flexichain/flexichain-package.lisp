;;; Flexichain
;;; Package definition
;;;
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; THIS LIBRARY IS FREE software; you can redistribute it and/or
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

(cl:in-package #:common-lisp-user)

(defpackage :flexichain
  (:use :common-lisp)
  (:export #:flexichain #:standard-flexichain
           #:flexi-error #:flexi-initialization-error
           #:flexi-position-error #:flexi-incompatible-type-error
           #:nb-elements #:flexi-empty-p
           #:insert* #:insert-vector* #:element* #:delete* #:delete-elements*
           #:push-start #:pop-start #:push-end #:pop-end #:rotate
           #:cursorchain #:standard-cursorchain 
           #:flexicursor #:standard-flexicursor
           #:left-sticky-flexicursor #:right-sticky-flexicursor
           #:chain
           #:clone-cursor #:cursor-pos
           #:at-beginning-error #:at-end-error
           #:at-beginning-p #:at-end-p
           #:move> #:move<
           #:insert #:insert-sequence
           #:element< #:element> #:delete< #:delete>
           #:flexirank-mixin #:element-rank-mixin #:rank
           #:flexi-first-p #:flexi-last-p
           #:flexi-next #:flexi-prev))
