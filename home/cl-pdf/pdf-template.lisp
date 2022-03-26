;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;;# PDF Templates by Tim Daly jr
;;;
;;; Copyright (c) 2005 Tim Daly Jr.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in
;;;    the documentation and/or other materials provided with the
;;;    distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY TIM DALY JR. ``AS IS'' AND ANY
;;; EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL TIM DALY JR. OR
;;; HIS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Converting a page of a PDF document into a template allows you to
;;; draw it just like an image or some text.  This is useful for
;;; watermarking, merging documents, and creating templates that can
;;; be filled in.

;;; This code was developed using Adobe's PDF Reference, Third
;;; Edition, Version 1.4, which is currently available here:

;;; http://partners.adobe.com/public/developer/en/pdf/PDFReference.pdf

;;; We refer to this document as "PDF-REF <Section-ID>" below.

;;;## Package definition

(in-package #:pdf)

;;;## Template Objects

(defparameter *default-template-size* #(0 0 595 841)
  ;; A4 page size (same default as page object)
  ;; the initform of the bounds slot in the pdf::page class.
  "Default size of a template's bounding box.")

;;; A template is implemented as a Form XObject, see PDF-REF 4.9.
(defclass template (indirect-object)
  ((name  :accessor name
          ;; I'm assuming that you can use any name you want to.
          :initform (gen-name "/CLXOBJ") :initarg :name)
   (bounds  :accessor bounds
            :initform *default-template-size* :initarg :bounds)
   (resources :accessor resources
              :initform (make-instance 'dictionary))))

(defmethod initialize-instance :after ((template template) &rest init-options
                                       &key no-compression
                                       &allow-other-keys)
  (declare (ignore init-options))
  ;; ProcSet is not really necessary, see PDF-REF 9.1.
  (add-dict-value (resources template) "/ProcSet" "[ /PDF /Text ]")
  (setf (content template)
        (make-instance 'pdf-stream
                       :no-compression no-compression
                       :dict-values `(("/Type" . "/Xobject")
                                      ("/Subtype" . "/Form")
                                      ("/FormType" . "1")
                                      ("/BBox" . ,#'(lambda () (bounds template)))
                                      ("/Resources" . ,(resources template))))))

(defun draw-template (template)
  "Cause a template to be drawn on the current page."
  ;; See PDF-REF 4.7.
  (format *page-stream* "~a Do~%" (name template)))


(defun add-templates-to-page (&rest templates)
  "Add TEMPLATES to the resources dictionary of the current page, so
that they can be drawn by e.g., DRAW-TEMPLATE."
  (dolist (template templates)
    (add-dict-value (xobjects *page*) (name template) template)))

(defconstant +identity-matrix+
  (if (boundp '+identity-matrix+)
      (symbol-value '+identity-matrix+)
      #(1 0 0 1 0 0)))

(defun make-template-from-page (old-page &key scale rotate
                                (translate-x 0) (translate-y 0))
  (when (typep old-page 'indirect-object)
    (setf old-page (content old-page)))
  (let ((template (make-instance 'template :bounds
                                 (or (resolve-page-dict-value old-page "/MediaBox")
                                     *default-template-size*))))
    (let ((old-content-stream (resolve-dict-value old-page "/Contents"))
          (new-content-stream (content template)))
      ;; copy the old content stream 
      (setf (content new-content-stream)
            (content old-content-stream))
      ;; copy the other dict entries, e.g. filter, length, from the old stream
      (loop for ((name . value)) on (dict-values old-content-stream) do
            (change-dict-value new-content-stream name (copy-pdf-structure value))))

    (let ((form-matrix +identity-matrix+))
      ;; rotate the template
      (when (and rotate (not (zerop rotate)))        
        (setf form-matrix
              (let ((translate-to-origin (translation-matrix
                                          (- (/ (template-width template) 2))
                                          (- (/ (template-height template) 2))))
                    (translate-back (translation-matrix
                                     (/ (template-width template) 2)
                                     (/ (template-height template) 2))))
                (multiply-tranformation-matrices
                 (multiply-tranformation-matrices translate-to-origin
                                                  (rotation-matrix rotate))
                 translate-back))))
      ;; scale the template
      (when scale
        (setf form-matrix
              (multiply-tranformation-matrices
               form-matrix
               (scale-matrix scale scale))))

      ;;translate the template
      (setf form-matrix
            (multiply-tranformation-matrices
             form-matrix
             (translation-matrix translate-x translate-y)))

      ;; set the template's form matrix, see PDF-REF Table 4.41
      (change-dict-value (content template) "/Matrix" form-matrix))

    ;; copy any metadata associated with the page.  Not sure if this
    ;; is what we want.
    (when (get-dict-value old-page "/Metadata")
      (add-dict-value (content template) "/Metadata"
                      (copy-pdf-structure (get-dict-value old-page "/Metadata"))))

    ;; copy the page's resources, such as images and fonts.
    (let ((old-resources (resolve-page-dict-value old-page "/Resources")))
      (loop for ((name . value)) on (dict-values old-resources) do
            (change-dict-value (resources template) name (copy-pdf-structure value))))
    
    template))

(defun template-width (template)
  (svref (bounds template) 2))

(defun template-height (template)
  (svref (bounds template) 3))

;;;## Transformation Matrices (See PDF-REF 4.2.3)      
(defun rotation-matrix (deg)
  (let* ((angle (/ (* pi deg) 180))
         (s (sin angle))
         (c (cos angle)))
    (vector c s (- s) c 0 0)))

(defun translation-matrix (x y)
  (vector 1 0 0 1 x y))

(defun scale-matrix (x y)
  (vector x 0 0 y 0 0))

(defun multiply-tranformation-matrices (a b)
  "C_ik = A_ij B_jk, where a matrix like this:  [ a b 0 ]
                                                [ c d 0 ]
                                                [ e f 1 ]

is stored liked this:  [ a b c d e f ]."
  (let ((a_11 (svref a 0))
        (a_12 (svref a 1))
        (a_21 (svref a 2))
        (a_22 (svref a 3))
        (a_31 (svref a 4))
        (a_32 (svref a 5))

        (b_11 (svref b 0))
        (b_12 (svref b 1))
        (b_21 (svref b 2))
        (b_22 (svref b 3))
        (b_31 (svref b 4))
        (b_32 (svref b 5)))
    (vector (+ (* a_11 b_11) (* a_12 b_21))
            (+ (* a_11 b_12) (* a_12 b_22))
            (+ (* a_21 b_11) (* a_22 b_21))
            (+ (* a_21 b_12) (* a_22 b_22))
            (+ (* a_31 b_11) (* a_32 b_21) b_31)
            (+ (* a_31 b_12) (* a_32 b_22) b_32))))

;;;## Copying PDF Structure

(defgeneric copy-pdf-structure (arg)
  (:documentation "Copy some piece of a PDF, creating fresh indirect
objects."))

(defmethod copy-pdf-structure ((dict dictionary))
  (let ((new-dict (make-instance 'dictionary)))
    (loop for ((name . value)) on (dict-values dict) do
          (change-dict-value new-dict name (copy-pdf-structure value)))
    new-dict))

(defmethod copy-pdf-structure ((obj indirect-object))
  (make-instance 'indirect-object
                 :content (copy-pdf-structure (content obj))))

(defmethod copy-pdf-structure ((obj pdf-stream))
  (let ((new-stream (make-instance 'pdf-stream :empty t)))
    (setf (content new-stream)
          (copy-pdf-structure (content obj)))
    ;; I think this suppresses compressing the stream again..?
    (setf (no-compression new-stream) t)
    (loop for ((name . value)) on (dict-values obj) do
          (change-dict-value new-stream name (copy-pdf-structure value)))

    new-stream))

(defmethod copy-pdf-structure ((obj sequence))
  (map (type-of obj)
       (lambda (o)
         (copy-pdf-structure o))
       obj))

(defmethod copy-pdf-structure ((obj t))
;; This can be useful for debugging:  
;;  (format t "warning, not copying ~A~%" (type-of obj))
  obj)

;;;## A couple of utility functions.

(defun resolve-dict-value (obj key)
  "If a dictionary value is an indirect object, return the indirect
object's content."
  (do ((value (get-dict-value obj key) (content value)))
      ((not (typep value 'indirect-object)) value)))


(defun resolve-page-dict-value (page key)
  "Look KEY up in the page dictionary.  If it is not found, look it up
in the parent page dictionary."
  ;; Some values in a page dictionary can be inherited from the parent
  ;; page's dictionary, such as the MediaBox.  This is a handy way to
  ;; look them up.  See PDF-REF Table 3.18.
  (let ((value (resolve-dict-value page key)))
    (or value
        (let ((parent-page (resolve-dict-value page "/Parent")))
          (and parent-page
               (resolve-page-dict-value parent-page key))))))



;;;## Example Usage

;;; Try something like this after loading pdf-template.lisp:
;;;
;;; (pdf:test-template "/tmp/ex7.pdf" 1 "/tmp/template.pdf")
#+nil
(defun test-template (in-file page-number out-file)
  "Create a new PDF with the given file and page number drawn several
times.  This test requires pdf-parser to be loaded."
  (let* ((old-doc (read-pdf-file in-file))
         (old-root (root-page old-doc))
         (old-page (aref (pages old-root) page-number))
         (old-page-bounds (or (resolve-page-dict-value (content old-page) "/MediaBox")
                              *default-template-size*))
         (width (svref old-page-bounds 2))
         (height (svref old-page-bounds 3)))
    (with-document ()
      (with-page (:bounds old-page-bounds)
        (let ((top-template (make-template-from-page
                             old-page :scale 2/5 :translate-x (* 3/10 width)
                             :translate-y (* 3/5 height)))
              (right-template (make-template-from-page
                               old-page :scale 2/5 :rotate 90
                               :translate-x (* 3/5 width) :translate-y (* 3/10 height)))
              (left-template (make-template-from-page
                              old-page :scale 2/5 :rotate -90
                              :translate-y (* 3/10 height )))
              (bottom-template (make-template-from-page
                                old-page :scale 2/5 :rotate 180
                                :translate-x (* 3/10 width))))
          (add-templates-to-page top-template right-template
                                 left-template bottom-template)
          (draw-template top-template)
          (draw-template bottom-template)
          (draw-template left-template)
          (draw-template right-template)))
      (write-document out-file))))

