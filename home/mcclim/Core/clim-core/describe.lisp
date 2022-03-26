;;; -*- Mode: Lisp; Package: COMMON-LISP -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)

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

(in-package :clim-lisp)

(defun describe (object &optional stream)
  (let ((stream (case stream
                  ((nil) *standard-output*)
                  ((t)   *terminal-io*)
                  (t     stream))))
    (describe-object object stream))
  (values))

(defgeneric describe-object (object stream))

(defgeneric describe-object-self (object stream))

(defgeneric describe-object-type (object stream))

;;; For these methods, stream should be of type
;;; (or EXTENDED-OUTPUT-STREAM OUTPUT-RECORDING-STREAM)
;;; but CLIM-STREAM-PANE is used instead.

(clim-internals::with-system-redefinition-allowed
    (defmethod describe-object ((object t) stream)
      (let ((*print-array* nil))
        (standard-describe-object-header object stream))))

(defun standard-describe-object-header (object stream &key kind suffix)
  (describe-object-self object stream)
  (format stream " is~@[ ~A~] of type " kind)
  (describe-object-type object stream)
  (when suffix
    (funcall suffix stream))
  (terpri stream))

(defun present-simply (object stream
                       &optional (type (clim:presentation-type-of object)))
  (clim:present object type :stream stream))

(defmethod describe-object-self ((object t) stream)
  (present-simply object stream))

(defmethod describe-object-type ((object t) stream)
  (present-simply (type-of object) stream))

;;;

(defmethod describe-object ((object symbol) stream)
  (standard-describe-object-header object stream)
  (cond
    ((not (boundp object))
     (format stream "   it is unbound~%"))
    (t
     (format stream "   it has a value of ")
     (present-simply (symbol-value object) stream)
     (terpri)))
  (format stream "   it is in the ")
  (present-simply (symbol-package object) stream)
  (format stream " package~%")
  (when (fboundp object)
    (format stream "   it has a function definition of ~S~%" (symbol-function object))
    (format stream "      which has the argument list ")
    (let ((arglist #+excl (excl:arglist (symbol-function object))
                   #+cmu (kernel:%function-arglist (symbol-function object))
                   #+sbcl (sb-introspect:function-lambda-list (symbol-function object))
                   #+clisp (ext:arglist (symbol-function object))
                   #+lispworks (lw:function-lambda-list (symbol-function object))
                   #-(or excl cmu sbcl clisp lispworks) "( ??? )"))
      (when arglist
        (present-simply arglist stream)))
    (terpri))
  (format stream "   it has a property list of ~S~%" (symbol-plist object)))

(defmethod describe-object-self ((object symbol) stream)
  (present-simply object stream))

(defmethod describe-object ((object number) stream)
  (standard-describe-object-header object stream :kind "a number"))

(defmethod describe-object ((object string) stream)
  (standard-describe-object-header
   object stream :suffix (lambda (stream)
                           (format stream " with a length of ")
                           (present-simply (length object) stream 'clim:integer))))

(defmethod describe-object ((object package) stream)
  (standard-describe-object-header
   object stream :suffix (lambda (stream)
                           (format stream " its name is ")
                           (present-simply (package-name object) stream)))
  (format stream "   it has the nicknames of ")
  (clim:present (package-nicknames object) 'clim:expression :stream stream)
  (terpri stream)
  (format stream "   it uses these packages: ")
  (clim:present (package-use-list object) 'clim:expression :stream stream)
  (terpri stream)
  (format stream "   it is used by the packages: ")
  (clim:present (package-used-by-list object) 'clim:expression :stream stream)
  (terpri stream))

(labels ((present-slot/text (slot object stream width)
           (let ((name (c2mop:slot-definition-name slot)))
             (cond
               ((slot-boundp object name)
                (format stream "      ~v@A: " width name)
                (clim:present (slot-value object name) 'clim:expression :stream stream)
                (terpri stream))
               (t
                (format stream "      ~v@A: <unbound>~%" width name)))))

         (present-instance-slots/text (object stream)
           (let* ((slots (c2mop:class-slots (class-of object)))
                  (width (loop for slot in slots
                            maximizing (length (symbol-name (c2mop:slot-definition-name slot))))))
             (map nil (alexandria:rcurry #'present-slot/text object stream width) slots)))

         (present-slot/table (slot object stream)
           (let ((name (c2mop:slot-definition-name slot)))
             (clim:formatting-row (stream)
               (clim:formatting-cell (stream :align-x :right)
                 (present-simply name stream)
                 (write-char #\: stream))
               (clim:formatting-cell (stream)
                 (if (slot-boundp object name)
                     (clim:present (slot-value object name) 'clim:expression
                                   :stream stream)
                     (format stream "<unbound>"))))))

         (present-instance-slots/table (object stream)
           (let ((slots (c2mop:class-slots (class-of object))))
             (clim:formatting-table (stream)
               (map nil (alexandria:rcurry #'present-slot/table object stream) slots))))

         (describe-instance (object stream kind)
           (standard-describe-object-header object stream :kind kind)
           (format stream "   it has the following slots:~%")
           (if (typep stream 'clim:output-recording-stream)
               (present-instance-slots/table object stream)
               (present-instance-slots/text object stream))))

  (defmethod describe-object ((object standard-object) stream)
    (describe-instance object stream "an instance"))

  (defmethod describe-object ((object structure-object) stream)
    (describe-instance object stream "a structure")))
