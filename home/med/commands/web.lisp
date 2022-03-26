;;;; Copyright (c) 2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :med.web
  (:use :cl :med))

(in-package :med.web)

(defparameter *render-methods*
  '(("head" head)
    ("script" :ignore)
    ("style" :ignore)
    ("hr" hr)
    ("br" br)
    ("p" br)
    ("img" img)))

(defun head (element attributes body buffer)
  (loop
     for tag in body
     when (and (listp tag)
               (string-equal (first tag) "title")
               (stringp (third tag))
               (not (zerop (length (third tag)))))
     do (setf (buffer-property buffer 'med::title) (third tag))))

(defun img (element attributes body buffer)
  (let ((alt (second (assoc "alt" attributes :test #'string-equal)))
        (src (second (assoc "src" attributes :test #'string-equal))))
    (render (or alt src "") buffer)))

(defun hr (element attributes body buffer)
  (when (eql (character-left-of (buffer-point buffer)) #\Space)
    (med::delete-char buffer -1))
  (insert buffer #\Newline)
  (dotimes (i 60)
    (insert buffer #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
  (insert buffer #\Newline))

(defun br (element attributes body buffer)
  (when (eql (character-left-of (buffer-point buffer)) #\Space)
    (med::delete-char buffer -1))
  (insert buffer #\Newline)
  (dolist (tag body)
    (render tag buffer)))

(defun render (node buffer)
  (etypecase node
    (string
     ;; I think that HTML defines more whitespace characters...
     (loop
        with point = (buffer-point buffer)
        for ch across node
        do (case ch
             ((#\Newline #\Tab #\Space)
              (unless (member (character-left-of point)
                              '(#\Space #\Newline nil))
                (insert buffer #\Space)))
             (t (insert buffer ch)))))
    (list
     (destructuring-bind (element attributes &rest body)
         node
       (let ((method (or (second (assoc element *render-methods*
                                        :test #'string-equal))
                         :descend)))
         (if (keywordp method)
             (ecase method
               (:ignore)
               (:descend
                (dolist (tag body)
                  (render tag buffer))))
             (funcall method element attributes body buffer)))))))

(defun browse-file (path)
  (setf path (merge-pathnames path))
  (dolist (buffer (buffer-list))
    (when (equal (buffer-property buffer 'webpage) path)
      (setf (last-buffer *editor*) (current-buffer *editor*))
      (switch-to-buffer buffer)
      (return-from browse-file buffer)))
  (let* ((buffer (make-instance 'buffer))
         (dom (html5-parser:parse-html5 path
                                        :encoding :utf-8
                                        :dom :xmls)))
    (setf (buffer-property buffer 'med::title) (format nil "~A" path))
    (render dom buffer)
    (push buffer (buffer-list))
    (setf (buffer-property buffer 'webpage) path)
    (move-beginning-of-buffer buffer)
    (setf (last-buffer *editor*) (current-buffer *editor*))
    (med::rename-buffer buffer (buffer-property buffer 'med::title))
    (setf (buffer-modified buffer) nil)
    (switch-to-buffer buffer)
    buffer))

(defun browse-command ()
  (browse-file
   (read-from-minibuffer "Find file: "
                         :default (namestring
                                   (or (buffer-property (current-buffer *editor*)
                                                        'med::default-pathname-defaults)
                                       *default-pathname-defaults*))
                         :completer #'file-completer)))
