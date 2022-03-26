(in-package :med)

;;; Lines.

(defclass line ()
  ((%next :initarg :next :accessor next-line)
   (%prev :initarg :prev :accessor previous-line)
   (%data :initarg :data :accessor data)
   (%version :initarg :version :accessor line-version)
   (%number :initarg :number :accessor line-number)
   (%mark-list :initarg :mark-list :accessor line-mark-list)
   (%buffer :initarg :buffer :accessor line-buffer))
  (:default-initargs :next nil
                     :prev nil
                     :data (make-array 0 :element-type 'cons :adjustable t :fill-pointer 0)
                     :version 0
                     :number 0
                     :mark-list '()
                     :buffer nil))

(defgeneric line-character (line charpos))
(defgeneric line-attributes (line charpos))
(defgeneric (setf line-character) (value line charpos))
(defgeneric (setf line-attributes) (value line charpos))
(defgeneric line-length (line))

(defmethod line-character ((line line) charpos)
  (car (aref (data line) charpos)))

(defmethod line-attributes ((line line) charpos)
  (cdr (aref (data line) charpos)))

(defmethod (setf line-character) (value (line line) charpos)
  (setf (car (aref (data line) charpos)) value))

(defmethod (setf line-attributes) (value (line line) charpos)
  (setf (cdr (aref (data line) charpos)) value))

(defmethod line-length ((line line))
  (length (data line)))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "N:~D V:~D" (line-number object) (line-version object))))

