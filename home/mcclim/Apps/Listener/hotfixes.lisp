(in-package :clim-internals)

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

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



;;; This file contains various hotfixes against McCLIM to make
;;; it do what I need it to do.

;; These two might be obsolete..
;; If you find various translators not working, uncomment these, otherwise
;; I think Tim Moore fixed things.
#+nil
(define-presentation-method presentation-subtypep ((type command-name)
						   maybe-supertype)
  (with-presentation-type-parameters (command-name maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command-name type)
	(command-table-inherits-from-p super-table command-table)))))
#+nil
(define-presentation-method presentation-subtypep ((type command)
						   maybe-supertype)
  (with-presentation-type-parameters (command maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command type)
	(command-table-inherits-from-p super-table command-table)))))


;; Only slightly improved reader support for McCLIM
;; Not really good enough to commit, but needed here.

(defun finish-rescan (stream)
  (setf (stream-scan-pointer stream) (stream-insertion-pointer stream))
  (setf (slot-value stream 'rescanning-p) nil))

#+nil
(defun accept-using-read (stream ptype default default-type defaultp
			  &key ((:read-eval *read-eval*) nil))
  ;;(format *trace-output* "~&Activation gestures were: ~A~%" *activation-gestures*)
  (letf () ;(((medium-foreground stream) +green+))
  (let ((*activation-gestures* nil))    
  (labels ((kludge-read ()
             (read-preserving-whitespace stream nil))                        
           (read-it ()
             (loop	             
               (handler-case (return-from read-it (values (kludge-read) ptype))
                 (parse-error (c)
                   (beep stream)
                   (when *pointer-documentation-output*
                     #+nil(setf (medium-foreground stream) +red+) ; This is silly.
                     (window-clear *pointer-documentation-output*)
                     (format *pointer-documentation-output* "~&A reader error ~A occured at ~A !~%"
                             nil (stream-scan-pointer stream)))
                   (finish-rescan stream)
                   #+nil(read-gesture :stream stream :input-wait-handler *input-wait-handler*)		 
                   (stream-read-char stream)		 
                   (immediate-rescan stream))))))
    (let ((result (read-it)))      
      (if (presentation-typep result ptype)
	  (values result ptype)
        (input-not-of-required-type result ptype)))))))


