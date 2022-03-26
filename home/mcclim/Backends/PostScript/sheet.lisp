;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)

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

;;; TODO:
;;;
;;; - do smth with POSTSCRIPT-GRAFT.

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-POSTSCRIPT-STREAM should offer a :PAPER-SIZE option.
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;; - font metrics are missing
;;;
;;;--GB

(in-package :clim-postscript)

(defun write-font-to-postscript-stream (stream text-style)
  (with-open-file (font-stream
		   (clim-postscript-font:postscript-device-font-name-font-file
                    (clim-internals::device-font-name text-style))
		   :direction :input
		   :external-format :latin-1)
    (let ((font (make-string (file-length font-stream))))
      (read-sequence font font-stream)
      (write-string font (medium-drawable stream)))))

(defmacro with-output-to-postscript-stream ((stream-var file-stream
                                             &rest options)
                                            &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-postscript-stream #',cont
                                                ,file-stream ,@options))))

(defun invoke-with-output-to-postscript-stream (continuation
                                                file-stream &key (device-type :a4)
                                                              multi-page scale-to-fit
                                                              (orientation :portrait)
                                                              header-comments)
  (flet ((make-it (file-stream)
           (climb:with-port (port :ps :stream file-stream
                                  :device-type device-type
                                  :page-orientation orientation)
             (let ((stream (make-postscript-stream port device-type
                                                   multi-page scale-to-fit
                                                   orientation header-comments))
                   (trim-page-to-output-size (eql device-type :eps))
                   translate-x translate-y)
               (sheet-adopt-child (find-graft :port port) stream)
               (unwind-protect
                    (with-slots (title for) stream
                      (with-output-recording-options (stream :record t :draw nil)
                        (with-graphics-state (stream)
                          ;; we need at least one level of saving -- APD, 2002-02-11
                          (funcall continuation stream)
                          (unless trim-page-to-output-size
                            (new-page stream)))) ; Close final page.
                      (format file-stream "%!PS-Adobe-3.0~@[ EPSF-3.0~*~]~@
                                           %%Creator: McCLIM~@
                                           %%Title: ~A~@
                                           %%For: ~A~@
                                           %%LanguageLevel: 2~%"
                              trim-page-to-output-size title for)
                      (if trim-page-to-output-size
                          (let ((record (stream-output-history stream)))
                            (with-bounding-rectangle* (lx ly ux uy) record
                              (setf translate-x (- (floor lx))
                                    translate-y (ceiling uy))
                              (format file-stream "%%BoundingBox: ~A ~A ~A ~A~%"
                                      0 0
                                      (+ translate-x (ceiling ux))
                                      (- translate-y (floor ly)))))
                          (let ((width (graft-width (graft stream)))
                                (height (graft-height (graft stream)))
                                (paper (device-type port)))
                            (format file-stream "%%BoundingBox: 0 0 ~A ~A~@
                                               %%DocumentMedia: ~A ~A ~A 0 () ()~@
                                               %%Pages: (atend)~%"
                                    width height paper width height)))
                      (format file-stream "%%DocumentNeededResources: (atend)~@
                                           %%EndComments~%~%")
                      (write-postscript-dictionary file-stream)
                      (dolist (text-style (clim-postscript-font:device-fonts (sheet-medium stream)))
                        (write-font-to-postscript-stream (sheet-medium stream) text-style))
                      (start-page stream)
                      (format file-stream "~@[~A ~]~@[~A translate~%~]" translate-x translate-y)

                      (with-output-recording-options (stream :draw t :record nil)
                        (with-graphics-state (stream)
                          (if trim-page-to-output-size
                              (replay (stream-output-history stream) stream)
                              (let ((last-page (first (postscript-pages stream))))
                                (dolist (page (reverse (postscript-pages stream)))
                                  (climi::letf (((sheet-transformation stream)
                                                 (make-postscript-transformation
                                                  (sheet-native-region (graft stream))
                                                  page
                                                  scale-to-fit)))
                                    (replay page stream))
                                  (unless (eql page last-page)
                                    (emit-new-page stream))))))))

                 (with-slots (current-page document-fonts) stream
                   (format file-stream "end~%showpage~%~@
                                        %%Trailer~@
                                        %%Pages: ~D~@
                                        %%DocumentNeededResources: ~{font ~A~%~^%%+ ~}~@
                                        %%EOF~%"
                           current-page (reverse document-fonts))
                   (finish-output file-stream)))))))
    (typecase file-stream
      ((or pathname string)
       (with-open-file (stream file-stream :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
         (make-it stream)))
      (t (make-it file-stream)))))

(defun start-page (stream)
  (with-slots (current-page transformation) stream
    (let ((file-stream (sheet-mirror stream)))
      (format file-stream "%%Page: ~D ~:*~D~%" (incf current-page))
      (format file-stream "~A begin~%" *dictionary-name*))))

(defmethod new-page ((stream postscript-stream))
  (push (stream-output-history stream) (postscript-pages stream))
  (let ((history (make-instance 'standard-tree-output-history :stream stream)))
    (setf (slot-value stream 'climi::output-history) history
	  (stream-current-output-record stream) history))
  (setf (stream-cursor-position stream)
        (stream-cursor-initial-position stream)))

(defun emit-new-page (stream)
  ;; FIXME: it is necessary to do smth with GS -- APD, 2002-02-11
  ;; FIXME^2:  what do you mean by that? -- TPD, 2005-12-23
  (postscript-restore-graphics-state stream)
  (format (sheet-mirror stream) "end~%showpage~%")
  (start-page stream)
  (postscript-save-graphics-state stream))


;;; Output Protocol

(defmethod make-medium ((port postscript-port) (sheet postscript-stream))
  (make-instance 'postscript-medium :sheet sheet))

(defmethod medium-miter-limit ((medium postscript-medium))
  #.(* pi (/ 11 180))) ; ?

;;; Some strange functions

(defmethod pane-viewport ((pane postscript-stream))
  nil)

(defmethod scroll-extent ((pane postscript-stream) x y)
  (declare (ignore x y))
  (values))

;;; POSTSCRIPT-GRAFT

(defclass postscript-graft (graft)
  ())

(defmethod initialize-instance :after ((graft postscript-graft) &key)
  (setf (slot-value graft 'native-transformation) nil)
  (setf (slot-value graft 'native-region) nil))

(defun graft-length (length units)
  (* length (ecase units
              (:device       1)
              (:inches       (/ 72))
              (:millimeters  (/ 254 720))
              (:screen-sized (/ length)))))

(defmethod graft-width ((graft postscript-graft) &key (units :device))
  (unless (eql (sheet-native-region graft) +everywhere+)
    (graft-length (bounding-rectangle-width (sheet-native-region graft)) units)))

(defmethod graft-height ((graft postscript-graft) &key (units :device))
  (unless (eql (sheet-native-region graft) +everywhere+)
    (graft-length (bounding-rectangle-height (sheet-native-region graft)) units)))

(defmethod sheet-region ((sheet postscript-graft))
  (let ((units (graft-units sheet)))
    (make-rectangle* 0 0
                     (graft-width sheet :units units)
                     (graft-height sheet :units units))))

(defmethod sheet-native-region ((sheet postscript-graft))
  (with-slots (native-region) sheet
    (unless native-region
      (setf native-region
            (paper-region (device-type (port sheet))
                          (page-orientation (port sheet)))))
    native-region))

;; This is necessary because McCLIM didn't reset the
;; native-transformation for basic-sheet. -- admich 2020-01-30
(defmethod invalidate-cached-transformations ((sheet postscript-graft))
  (with-slots (native-transformation device-transformation) sheet
    (setf native-transformation nil
     device-transformation nil))
  (loop for child in (sheet-children sheet)
     do (invalidate-cached-transformations child)))

(defun graft-units-transformation (graft)
  (ecase (graft-units graft)
    (:device +identity-transformation+)
    (:inches (make-scaling-transformation* 72 72))
    (:millimeters (make-scaling-transformation* (/ 720 254) (/ 720 254)))
    (:screen-sized (make-scaling-transformation* (graft-width graft) (graft-height graft)))))

(defun graft-orientation-transformation (graft)
  (ecase (graft-orientation graft)
    (:graphics +identity-transformation+)
    (:default (if (eql (sheet-native-region graft) +everywhere+)
                  (make-reflection-transformation* 0 0 1 0)
                  (compose-transformations
                   (make-translation-transformation
                    0
                    (bounding-rectangle-height (sheet-native-region graft)))
                   (make-reflection-transformation* 0 0 1 0))))))

(defmethod sheet-native-transformation ((sheet postscript-graft))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation
            (compose-transformations
             (graft-orientation-transformation sheet)
             (graft-units-transformation sheet))))
    native-transformation))

;;; Port

(setf (get :ps :port-type) 'postscript-port)
(setf (get :ps :server-path-parser) 'parse-postscript-server-path)

(defun parse-postscript-server-path (path)
  path)
