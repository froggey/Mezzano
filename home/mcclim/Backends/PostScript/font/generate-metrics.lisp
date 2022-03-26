;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)

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

(in-package :clim-postscript-font)

(defvar *lisp-metrics-file*
  (merge-pathnames (make-pathname :name "standard-metrics"
                                  :type "lisp"
                                  :defaults *load-truename*)
                   *load-truename*))

(with-open-file (out *lisp-metrics-file*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (print '(in-package :clim-postscript) out)
  (let ((*default-pathname-defaults*
         (make-pathname :directory '(:absolute "usr" "share" "texmf" "fonts" "afm" "adobe")
                        :type "afm")))
    (loop for file in
         '("times/ptmr8a" "times/ptmb8a" "times/ptmri8a" "times/ptmbi8a"
           "courier/pcrr8a" "courier/pcrro8a" "courier/pcrb8a" "courier/pcrbo8a"
           "helvetic/phvr8a" "helvetic/phvro8a" "helvetic/phvb8a" "helvetic/phvbo8a")
         do (print `(define-font-metrics
                        ,@(mapcar (lambda (arg)
                                    (list 'quote arg))
                                  (multiple-value-list
                                   (with-open-file (stream (merge-pathnames file))
                                     (read-afm-stream stream)))))
                   out))))
