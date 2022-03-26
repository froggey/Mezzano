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

(cl:in-package #:clim-pdf.test)

(in-suite :clim-pdf)

(test smoke
  "Smoke test for the PDF backend."

  (flet ((invoke-with-pdf-stream (continuation
                                  &key filename title scale-to-fit)
           (with-open-file (stream filename :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede
                                            :element-type '(unsigned-byte 8))
             (clim-pdf:with-output-to-pdf-stream
                 (stream stream :header-comments `(:title ,title)
                                :scale-to-fit scale-to-fit)
               (funcall continuation stream)))))
    (loop for i from 1
          for filename = (format nil "pdf-test-~D.pdf" i)
          for title = (format nil "Test Page ~D" i)
          for page in clim-test-util:*all-test-pages*
          do (finishes
               (invoke-with-pdf-stream page :filename filename :title title)))
    (finishes
      (clim-pdf:with-output-to-pdf-stream
          (stream "pdf-test-all.pdf" :header-comments `(:title "All test pages in one document"))
        (loop for page in clim-test-util:*all-test-pages*
           do (funcall page stream)
             (clim:new-page stream))))
    (finishes
      (let ((clim-pdf::*pdf-left-margin* 10)
            (clim-pdf::*pdf-top-margin* 10)
            (clim-pdf::*pdf-right-margin* 10)
            (clim-pdf::*pdf-bottom-margin* 10))
        (clim-pdf:with-output-to-pdf-stream
            (stream "pdf-test-trim.pdf" :header-comments `(:title "Trim to output size") :trim-page-to-output-size t)
          (clim-test-util:print-test-page-1 stream))))
    (finishes
      (let ((clim-pdf::*pdf-left-margin* 10)
            (clim-pdf::*pdf-top-margin* 10)
            (clim-pdf::*pdf-right-margin* 10)
            (clim-pdf::*pdf-bottom-margin* 10))
        (clim-pdf:with-output-to-pdf-stream
            (stream "pdf-test-scale.pdf" :header-comments `(:title "Scale to fit") :scale-to-fit t :orientation :landscape)
          (clim-test-util:print-test-page-1 stream))))))

(test units.smoke
  "Smoke test for the PDF backend with different graft units."
  (finishes
   (clim-pdf:with-output-to-pdf-stream
       (stream "pdf-test-screen-sized.pdf" :units :screen-sized :header-comments `(:title "PDF with screen sized units"))
     (clim:draw-line* stream 0.5 0 0.5 1)
     (clim:draw-line* stream 0 0.5 1 0.5)
     (clim:draw-circle* stream 0.5 0.5 0.5 :filled nil)))
  (finishes
   (clim-pdf:with-output-to-pdf-stream
       (stream "pdf-test-millimeters.pdf" :units :millimeters :header-comments `(:title "PDF with millimeters units"))
     (loop for i from 10 to 200 do
          (clim:draw-line* stream i 10 i 200 :ink clim:+light-blue+)
          (clim:draw-line* stream 10 i 200 i :ink clim:+light-blue+))
     (loop for i from 10 to 200 by 5 do
          (clim:draw-line* stream i 10 i 200 :ink clim:+cadetblue+)
          (clim:draw-line* stream 10 i 200 i :ink clim:+cadetblue+))
     (loop for i from 10 to 200 by 10 do
          (clim:draw-line* stream i 10 i 200 :ink clim:+blue+)
          (clim:draw-line* stream 10 i 200 i :ink clim:+blue+)))))
