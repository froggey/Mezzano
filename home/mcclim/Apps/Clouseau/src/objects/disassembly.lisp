;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; The standard DISASSEMBLE function will print out
;;; implementation-specific things to *STANDARD-OUTPUT* in an
;;; implementation-specific way. This is generally optimized for
;;; console output, and is therefore not the most aesthetically
;;; pleasing way for CLIM to display it.

(defun display-disassembly (function stream)
  (with-style (stream :disassembly)
    (let ((*standard-output* stream))
      (disassemble function))))
