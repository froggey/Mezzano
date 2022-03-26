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

(test smoke
  "Smoke test for the PostScript backend."

  (flet ((invoke-with-postscript-stream (continuation
                                         &key filename title scale-to-fit)
           (with-open-file (stream filename :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede)
             (clim-postscript:with-output-to-postscript-stream
                 (stream stream :header-comments `(:title ,title)
                                :scale-to-fit scale-to-fit)
               (funcall continuation stream)))))
    (loop for i from 1
          for filename = (format nil "postscript-test-~D.ps" i)
          for title = (format nil "Test Page ~D" i)
          for page in clim-test-util:*all-test-pages*
          do (finishes
               (invoke-with-postscript-stream
                page :filename filename :title title)))
    (finishes
      (clim-postscript:with-output-to-postscript-stream
          (stream "postscript-test-all.ps"
                  :header-comments `(:title "All test pages in one document"))
        (loop for page in clim-test-util:*all-test-pages*
           do (funcall page stream)
             (clim:new-page stream))))
    (finishes
      (clim-postscript:with-output-to-postscript-stream
          (stream "postscript-test-trim.eps" :device-type :eps
                  :header-comments `(:title "Trim to output size"))
        (clim-test-util:print-test-page-1 stream)))
    (finishes
      (clim-postscript:with-output-to-postscript-stream
          (stream "postscript-test-scale.ps" :header-comments `(:title "Scale to fit")
                  :scale-to-fit t :orientation :landscape)
        (clim-test-util:print-test-page-1 stream)))))

(defun invoke-with-output-file (write-continuation read-continuation filename)
  (unwind-protect
       (progn
         (with-open-file (stream filename :direction :output :if-exists :supersede)
           (funcall write-continuation stream))
         (with-open-file (stream filename :direction :input)
           (funcall read-continuation stream)))
    (ignore-errors (delete-file filename))))

(defmacro with-output-file ((filename)
                            ((write-stream) &body write-body)
                            ((read-stream) &body read-body))
  `(invoke-with-output-file (lambda (,write-stream) ,@write-body)
                            (lambda (,read-stream) ,@read-body)
                            ,filename))

(test ps-output
  "Test producing a PS document."

  (with-output-file ("postscript-test.ps")
    ((stream)
     (clim:with-output-to-postscript-stream (stream stream :device-type :a4)
       (clim:draw-text* stream "Hello, World!" 20 20)))
    ((stream)
     (let ((first-line (read-line stream)))
       (is (eql 11 (mismatch first-line "%!PS-Adobe-")))
       (is (eq nil (position #\Space first-line)))
       (do ((line (read-line stream) (read-line stream)))
           ((not (eql (mismatch line "%%") 2))
            (error "Failed to find bounding box"))
         (when (eql (mismatch line "%%BoundingBox: ") 15)
           (with-input-from-string (string line :start 15)
             (let ((llx (read string))
                   (lly (read string))
                   (urx (read string))
                   (ury (read string)))
               (is-true (numberp llx))
               (is-true (numberp lly))
               (is-true (numberp urx))
               (is-true (numberp ury))
               (return t)))))))))

(test eps-output
  "Test producing an EPS document."

  (with-output-file ("postscript-test.eps")
    ((stream)
     (clim:with-output-to-postscript-stream (stream stream :device-type :eps)
       (clim:draw-rectangle* stream 1 1 19 21)))
    ((stream)
     (let ((first-line (read-line stream)))
       (is (eql 11 (mismatch first-line "%!PS-Adobe-")))
       (is-true (search "EPSF" first-line))
       (do ((line (read-line stream) (read-line stream)))
           ((not (eql (mismatch line "%%") 2))
            (error "Failed to find bounding box"))
         (when (eql (mismatch line "%%BoundingBox: ") 15)
           (with-input-from-string (string line :start 15)
             (let ((llx (read string))
                   (lly (read string))
                   (urx (read string))
                   (ury (read string)))
               (is-true (numberp llx))
               (is-true (numberp lly))
               (is-true (numberp urx))
               (is-true (numberp ury))
               ;; our EPS files have lower bounds of 0.
               (is (= 0 llx))
               (is (= 0 lly))
               (is (>= 20 (- urx llx) 18))
               (is (>= 22 (- ury lly) 20))
               (return t)))))))))
