;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PP2; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: PPRINT for McCLIM
;;;   Created: 2003-05-18
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;;       $Id: pprint.lisp,v 1.2 2003/05/18 08:56:19 gilbert Exp $
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Gilbert Baumann

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

;;;; Notes

;; - INVOKE-WITH-LOGICAL-BLOCK
;;
;; - The specification is explict about depth abbreviation and dotted
;;   So the common implementation should share this. Specifically that
;;   is the DESCEND-INTO and the implementation of POP and
;;   EXIT-IF-EXHAUSTED. Whether DESCEND-INTO might also be worth
;;   protocolizing is an open question. Although a case can be made
;;   for also protocolizing that.

;; - Hefner wants nifty unreadable objects therefore we might think
;;   about protolizing print-unreadable-object too.

;; - How we can integrate this into a _practically_ closed source lisp
;;   like ACL or Lispworks is uncertain. But looking at the macro
;;   expansion both seem to use XP so the source is there and we could
;;   figure out which functions need to be adviced / replaced.

;; - I have no clue how *PRINT-CIRCLE* and *PRINT-SHARE* are
;;   implemented, we need to figure that out.

(defpackage :pp2
  (:use :clim :clim-lisp)
  (:import-from :SB-PRETTY
   #:INVOKE-WITH-LOGICAL-BLOCK
   #:STREAM-PPRINT-TAB 
   #:STREAM-PPRINT-NEWLINE
   #:STREAM-PPRINT-INDENT))

(in-package :pp2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Patching SBCL into shape
;;;;

(declaim (notinline sb-pretty::pretty-stream-p))

(defvar *orig-invoke-with-logical-block*
  #'invoke-with-logical-block)

(defvar *orig-pretty-stream-p*
  #'sb-pretty::pretty-stream-p)

(progn ;;eval-when (compile eval load)
  (fmakunbound 'sb-pretty::pretty-stream-p)
  (defgeneric sb-pretty::pretty-stream-p (stream))
  (defmethod sb-pretty::pretty-stream-p (stream)
    (funcall *orig-pretty-stream-p* stream)))

(fmakunbound 'stream-pprint-newline)
(defgeneric stream-pprint-newline (stream kind))

(defmethod stream-pprint-newline (stream kind)
  (when (sb-pretty::print-pretty-on-stream-p stream)
    (sb-pretty::enqueue-newline stream kind)))

(fmakunbound 'stream-pprint-indent)
(defgeneric stream-pprint-indent (stream relative-to n))

(defmethod stream-pprint-indent (stream relative-to n)
  (when (sb-pretty::print-pretty-on-stream-p stream)
    (sb-pretty::enqueue-indent stream relative-to n)))

(fmakunbound 'stream-pprint-tab)
(defgeneric stream-pprint-tab (stream kind colnum colinc))

(defmethod stream-pprint-tab (stream kind colnum colinc)
  (when (sb-pretty::print-pretty-on-stream-p stream)
    (sb-pretty::enqueue-tab stream kind colnum colinc)))

(fmakunbound 'invoke-with-logical-block)
(defgeneric invoke-with-logical-block (stream continuation object
                                              &key prefix per-line-prefix suffix))

(defmethod invoke-with-logical-block (stream continuation object
                                             &rest args
                                             &key prefix per-line-prefix suffix)
  (apply *orig-invoke-with-logical-block* stream continuation object args))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;
;;;;

(define-application-frame foo ()
  ()
  (:pointer-documentation t)
  (:panes
   (io :interactor
    :width 600
    :height 1000
    ;;:text-style (make-text-style :sans-serif :roman :normal)
    ))
  (:layouts
   (default
       io)))

(defun foo ()
  (run-frame-top-level(make-application-frame'foo)))

(defparameter *my-dispatch*
  (copy-pprint-dispatch nil))

(set-pprint-dispatch 'string
                     (lambda (stream thing)
                       (let ((*print-pretty* nil))
                         (with-drawing-options (stream :ink +blue+)
                           (prin1 thing stream))))
                     0
                     *my-dispatch*)

(defparameter *thing*
  '(defun pprint-tab (kind colnum colinc &optional stream)
    "If STREAM (which defaults to *STANDARD-OUTPUT*) is a pretty-printing
   stream, perform tabbing based on KIND, otherwise do nothing. KIND can
   be one of:
     :LINE - Tab to column COLNUM. If already past COLNUM tab to the next
       multiple of COLINC.
     :SECTION - Same as :LINE, but count from the start of the current
       section, not the start of the line.
     :LINE-RELATIVE - Output COLNUM spaces, then tab to the next multiple of
       COLINC.
     :SECTION-RELATIVE - Same as :LINE-RELATIVE, but count from the start
       of the current section, not the start of the line."
    (declare (type (member :line :section :line-relative :section-relative) kind)
     (type unsigned-byte colnum colinc)
     (type (or stream (member t nil)) stream)
     (values null))
    (let ((stream (case stream
                    ((t) *terminal-io*)
                    ((nil) *standard-output*)
                    (t stream))))
      (stream-pprint-tab stream kind colnum colinc))
    nil))

(defparameter *thing*
  '(defun invoke-with-logical-block (stream continuation object
                                     &key prefix per-line-prefix (suffix ""))
    (declare (type function continuation))
    (with-pretty-stream (stream stream)
      (if (listp object)
          (descend-into (stream)
                        (let ((count 0))
                          (start-logical-block stream
                                               (the (or null string) (or prefix per-line-prefix))
                                               (if per-line-prefix t nil)
                                               (the string suffix))
                          (block .block.
                            (flet ((pp-pop ()
                                     (unless (listp object)
                                       (write-string ". " stream)
                                       (output-object object stream)
                                       (return-from .block. nil))
                                     (when (and (not *print-readably*)
                                                (eql count *print-length*))
                                       (write-string "..." stream)
                                       (return-from .block. nil))
                                     (when (and object
                                                (plusp count)
                                                (check-for-circularity
                                                 object))
                                       (write-string ". " stream)
                                       (output-object object stream)
                                       (return-from .block. nil))
                                     (incf count)
                                     (pop object)))
                              (funcall continuation
                                       stream
                                       #'pp-pop
                                       #'(lambda ()
                                           (when (null object)
                                             (return-from .block. nil)))) ))
                          ;; FIXME: Don't we need UNWIND-PROTECT to ensure this
                          ;; always gets executed?
                          (end-logical-block stream)))
          (output-object object stream)))))

(define-foo-command com-foo ()
  (let ((*print-case* :downcase)
        (*print-pprint-dispatch*
         *my-dispatch*))
    (with-text-style
        (*standard-output*
         (make-text-style :sans-serif :roman :normal))
      (pprint *thing*)
      )))

(define-foo-command com-bar ()
  (let ((*print-pretty* t))
    (describe *package*)))

(define-foo-command com-pascal ()
  (cl-user::pascal-write '(defun sqt (n &aux sqt)
                           (declare (float n) (float sqt))
                           (setq sqt 1.0)
                           (loop (when (< (abs (- (* sqt sqt) n))
                                          1.0E-4)
                                   (return nil))
                               (setq sqt 
                                     (/ (+ sqt (/ n sqt)) 2.0)))
                           sqt)))

(define-foo-command com-paskal ()
  (with-text-style
      (*standard-output*
       #+NIL(make-text-style :serif :roman :normal)
       (make-device-font-text-style
        (port *standard-output*)
        "-*-times-medium-r-*-*-*-140-*-*-*-*-iso8859-15"))
    (cl-user::pascal-write
     '(defun sqt (n &aux sqt)
       (declare (float n) (float sqt))
       (setq sqt 1.0)
       (loop (when (< (abs (- (* sqt sqt) n))
                      1.0E-4)
               (return nil))
           (setq sqt 
                 (/ (+ sqt (/ n sqt)) 2.0))
         (setq sqt 
               (/ (+ sqt (/ n sqt)) 2.0))
         (setq sqt 
               (/ (+ sqt (/ n sqt)) 2.0))
         (print "hallo")
         (if (< x 10)
             (print "dadadidum"))
         )
       sqt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;
;;;;
;;;;

(defvar *pretty-printing-in-effect-p* nil)

(defmethod invoke-with-logical-block ((stream extended-output-stream)
                                      continuation
                                      object
                                      &key prefix per-line-prefix (suffix ""))
  (labels ((doit (stream object)
             (if (listp object)
                 (sb-kernel:descend-into
                  (stream)
                  (let ((count 0))
                    (start-logical-block stream
                                         (the (or null string) (or prefix per-line-prefix))
                                         (if per-line-prefix t nil)
                                         (the string suffix))
                    (block .block.
                      (flet ((pp-pop ()
                               (unless (listp object)
                                 (write-string ". " stream)
                                 (prin1 object stream)
                                 (return-from .block. nil))
                               (when (and (not *print-readably*)
                                          (eql count *print-length*))
                                 (write-string "..." stream)
                                 (return-from .block. nil))
                               (when (and object
                                          (plusp count)
                                          (sb-pretty::check-for-circularity
                                           object))
                                 (write-string ". " stream)
                                 (prin1 object stream)
                                 (return-from .block. nil))
                               (incf count)
                               (pop object)))
                        (funcall continuation
                                 stream
                                 #'pp-pop
                                 #'(lambda ()
                                     (when (null object)
                                       (return-from .block. nil)))) ))
                    ;; FIXME: Don't we need UNWIND-PROTECT to ensure this
                    ;; always gets executed?
                    (end-logical-block stream suffix)))
                 (prin1 object stream))))
    (cond (*pretty-printing-in-effect-p*
           (with-output-as-presentation
               (stream object (clim:presentation-type-of object))
             (doit stream object)))
          (t
           (let ((*pretty-printing-in-effect-p* t)
                 (*phase* *phase*))
             (with-end-of-line-action (stream :allow)
               (multiple-value-bind (cx cy) (stream-cursor-position stream)
                 (with-end-of-page-action (stream :allow)
                   (start-phase-1 stream)
                   (with-output-recording-options (stream :record nil :draw nil)
                     (doit stream object)))
                 (start-phase-2 stream cx cy)
                 (with-output-as-presentation (stream object (clim:presentation-type-of object))
                   (doit stream object)))))))))

(defmethod sb-pretty::pretty-stream-p ((stream extended-output-stream))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Items
;;;;

(defvar *items*)
(defvar *last-observerd-x*)
(defvar *phase* :illegal)
(defvar *level* 0)
(defvar *pointer* 0)
(defvar *block-stack* nil)

(defclass item ()
  ((x :initarg :x
      :reader item-x)))

(defclass newline (item)
  ((kind :initarg :kind
         :reader newline-kind)
   (level :initarg :level
          :reader newline-level)
   (preceeding-section
    :initform nil
    :accessor newline-preceeding-section)
   (following-section
    :initform nil
    :accessor newline-following-section)
   (containing-section
    :initform nil
    :accessor newline-containing-section)
   (taken-p :initform nil
            :accessor newline-taken-p)
   ))

(defclass indent (item)
  ((relative-to :initarg :relative-to
                :reader indent-relative-to)
   (amount :initarg :amount
           :reader indent-amount)))

(defmethod print-object ((object indent) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S ~S ~S" (item-x object) (indent-relative-to object) (indent-amount object))))

(defmethod print-object ((object newline) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S ~S ~S ~S,~S,~S"
            (item-x object)
            (newline-kind object)
            (newline-level object)
            (newline-preceeding-section object)
            (newline-following-section object)
            (newline-containing-section object))))

(defmethod print-object ((object item) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (item-x object))))

(defclass block-start (item)
  ((level
    :initarg :level
    :reader block-level)
   (x0 :accessor block-x0)
   (ind :accessor block-ind)
   (linear-newline-taken-p
    :initform nil
    :accessor block-linear-newline-taken-p)
   ))

(defclass block-end (item)
  ((level
    :initarg :level
    :reader block-level)))

;;;;

(defun start-logical-block (stream prefix per-line-p suffix)
  (when prefix (write-string prefix stream))
  (ecase *phase*
    (:collect
     (incf *level*)
     (push (make-instance 'block-start
            :level *level*
            :x (stream-cursor-position stream))
      *items*))
    (:doit
     (unless (typep (aref *items* *pointer*) 'block-start)
       (error "You're not playing by the rules."))
     (push (aref *items* *pointer*) *block-stack*)
     (setf (block-x0 (car *block-stack*)) (stream-cursor-position stream))
     (setf (block-ind (car *block-stack*)) (stream-cursor-position stream))
     (incf *pointer*)
     ))
  ;;
   )

(defun end-logical-block (stream suffix)
  (ecase *phase*
    (:collect
     (push (make-instance 'block-end
            :level *level*
            :x (stream-cursor-position stream))
      *items*)
     (decf *level*))
    (:doit
     (unless (typep (aref *items* *pointer*) 'block-end)
       (error "You're not playing by the rules."))
     (pop *block-stack*)
     (incf *pointer*)
     ))
  (write-string suffix stream))

(defparameter *print-w* 500)

(defparameter *debug-p* nil)

(defmethod stream-pprint-newline ((stream extended-output-stream) kind)
  (when *pretty-printing-in-effect-p*
    (ecase *phase*
      (:collect
       (push (make-instance 'newline
              :x (stream-cursor-position stream)
              :level *level*
              :kind kind)
        *items*))
      (:doit
       (unless (typep (aref *items* *pointer*) 'newline)
         (error "You're not playing by the rules."))
       (labels ((dobreak ()
                  (setf (newline-taken-p (aref *items* *pointer*)) t)
                  (terpri stream)
                  (multiple-value-bind (x y) (stream-cursor-position stream)
                    (setf (stream-cursor-position stream)
                          (values (block-ind (car *block-stack*))
                                  y)))))
         (case kind
           (:fill
            (when *debug-p*
              (princ "{f}"))
            (let* ((i (car (newline-following-section (aref *items* *pointer*))))
                   (j (cdr (newline-following-section (aref *items* *pointer*))))
                   (w (- (item-x (aref *items* j))
                         (item-x (aref *items* i)))))
              (cond ((or
                      (some #'(lambda (x)
                                (typecase x
                                  (newline (newline-taken-p x))))
                            (subseq *items*
                                    (car (newline-preceeding-section (aref *items* *pointer*)))
                                    (cdr (newline-preceeding-section (aref *items* *pointer*)))))
                      (>= (+ (stream-cursor-position stream)
                             w)
                          *print-w*))
                     (dobreak))
                    (t
                     )
                    )))
           (:mandatory
            (dobreak))
           (:linear
            (let* ((i (car (newline-containing-section (aref *items* *pointer*))))
                   (j (cdr (newline-containing-section (aref *items* *pointer*))))
                   (w (- (item-x (aref *items* j))
                         (item-x (aref *items* i)))))
              #||                       ;
              (princ "{l")
              (princ i) (princ ".")(princ j)(princ ".")(princ w)
              (princ "}")
              ||#
              (when *debug-p*
                (let ((*print-pretty* nil))
                  (princ "{L")
                  (princ w)
                  (princ ":")
                  (princ (stream-cursor-position stream))
                  (princ (cons i j))
                  (princ ".")
                  (princ (newline-level (aref *items* *pointer*)))
                  (princ "}")))
              (cond ((or
                      #+NIL
                      (not
                       (section-fits-p i j (stream-cursor-position stream)))
                      (>=;;(+ (stream-cursor-position stream) w)
                       (+ (block-x0 (car *block-stack*)) w)
                       *print-w*)
                      #+NIL
                      (some #'(lambda (x)
                                (typecase x
                                  (newline (newline-taken-p x))))
                            (subseq *items* i j))
                      ;;(block-linear-newline-taken-p(car *block-stack*))
                      )
                     (setf (block-linear-newline-taken-p(car *block-stack*))
                           t)
                     (dobreak))
                    (t
                     nil))))))
       (incf *pointer*)
       ))))

(defun fill-newline-breaks-p (*pointer* x0)
  (let* ((i (car (newline-following-section (aref *items* *pointer*))))
         (j (cdr (newline-following-section (aref *items* *pointer*))))
         (w (- (item-x (aref *items* j))
               (item-x (aref *items* i)))))
    (or
     (some #'(lambda (x)
               (typecase x
                 (newline (newline-taken-p x))))
           (subseq *items*
                   (car (newline-preceeding-section (aref *items* *pointer*)))
                   (cdr (newline-preceeding-section (aref *items* *pointer*)))))
     (not
      (section-fits-p i j x0))
     (>= (+ x0 w)
         *print-w*))))

(defvar *yet* nil)
    
(defun section-fits-p (i j x0)
  (cond ((member (cons i j) *yet* :test #'equal)
         t)
        (t
         (let ((*yet* (cons (cons i j) *yet*)))
           (let ((w (- (item-x (aref *items* j)) (item-x (aref *items* i))))
                 (dx (- (item-x (aref *items* i)) x0)))
             (block foo
               (when (< (+ x0 w) *print-w*)
                 (loop for k from i below j 
                       for q = (aref *items* k) do
                       (typecase q
                         (newline
                          (case (newline-kind q)
                            (:mandatory
                             (return-from foo nil))
                            (:linear
                             #+NIL
                             (when (some #'(lambda (x)
                                             (typecase x
                                               (newline (newline-taken-p x))))
                                         (subseq *items*
                                                 (car (newline-containing-section q))
                                                 (cdr (newline-containing-section q))))
                               (return nil)))
                            (:fill
                             (when (fill-newline-breaks-p k (+ (item-x q) dx))
                               (return-from foo nil)))))))
                 t)))))))

(defmethod stream-pprint-indent ((stream extended-output-stream) relative-to amount)
  (when *pretty-printing-in-effect-p*
    (ecase *phase*
      (:collect
       (push (make-instance 'indent
              :relative-to relative-to
              :amount amount
              :x (stream-cursor-position stream))
        *items*))
      (:doit
       (case relative-to
         (:current
          (setf (block-ind (car *block-stack*))
           (+ (stream-cursor-position stream)
            (* amount (text-size stream "m")))))
         (:block
          (setf (block-ind (car *block-stack*))
           (+ (block-x0 (car *block-stack*))
            (* amount (text-size stream "m"))))))
       (incf *pointer*)
       ))))

;;;;


;;;;

(defun start-phase-1 (stream)
  (setf *items* nil)
  (setf *last-observerd-x* (stream-cursor-position stream))
  (setf *phase* :collect) )

(defun start-phase-2 (stream cx cy)
  (setf *items* (coerce (reverse *items*) 'vector))
  ;;
  (let ((sections nil))
    (loop for i from 0
          for q across *items*
          do
          (typecase q
            (newline
             (setf (newline-preceeding-section q)
                   (find-preceeding-section i))
             (setf (newline-following-section q)
                   (find-following-section i)))))
    (loop for i from 0
          for q across *items*
          do
          (typecase q
            (newline
             ;; we do this by the book ...
             (let ((min (cons 0 (length *items*))))
               (loop for k across *items* do
                     (typecase k
                       (newline
                        (labels ((consider (x)
                                   (when (and (< (car x) i) (< i (cdr x))
                                              (< (- (cdr x) (car x))
                                                 (- (cdr min) (car min))))
                                     (setf min x))))
                          (consider (newline-preceeding-section k))
                          (consider (newline-following-section k))))))
               (setf (newline-containing-section q) min)))))
    )
  (setf *items*
        (coerce
         (append (coerce *items* 'list)
                 (list (make-instance 'item :x (stream-cursor-position stream))))
         'vector))
  ;;
  (when *debug-p*
    (print *items* *trace-output*))
  (setf *pointer* 0)
  (setf *phase* :doit)
  (setf (stream-cursor-position stream) (values cx cy))
  )

(defun find-preceeding-section (i)
  (cons
   (1+
    (loop for j from (- i 1) by -1 do
          (typecase (aref *items* j)
            (newline
             (when (= (newline-level (aref *items* j))
                      (newline-level (aref *items* i)))
               (return j)))
            (block-start
             (when (= (block-level (aref *items* j))
                      (newline-level (aref *items* i)))
               (return j))))))
   i))

(defun find-following-section (i)
  (cons
   i
   (loop for j from (+ i 1) do
         (when (= j (length *items*))
           (return j))
         (typecase (aref *items* j)
           (newline
            (when (<= (newline-level (aref *items* j))
                      (newline-level (aref *items* i)))
              (return j))) ))))

;;;;

;;; OUTPUT-PRETTY-OBJECT is called by OUTPUT-OBJECT when
;;; *PRINT-PRETTY* is true.
(defun sb-pretty::output-pretty-object (object stream)
  (cond ((and (extended-output-stream-p stream)
              (atom object)
              *print-escape*)
         (when *debug-p*
           (print object *trace-output*))
         (with-output-as-presentation (stream object (clim:presentation-type-of object))
           (funcall (sb-pretty::pprint-dispatch object) stream object)))
        (t
         (sb-pretty::with-pretty-stream (stream)
                                        (funcall (sb-pretty::pprint-dispatch object) stream object)))))


(set-pprint-dispatch '(cons (member block))
                     #'(lambda (stream thing)
                         (cond ((extended-output-stream-p stream)
                                (surrounding-output-with-border
                                 (stream :shape :drop-shadow)
                                 (with-drawing-options (stream :ink +blue+)
                                   (pprint-linear stream thing))))
                               (t
                                (pprint-linear stream thing)))))

(set-pprint-dispatch
 '(cons (member unless when with-pretty-stream block if descend-into))
 (lambda (stream thing)
   (pprint-logical-block
    (stream thing
            :prefix "("
            :suffix ")")
    (cond ((extended-output-stream-p stream)
           (with-drawing-options
               (stream :ink +blue4+)
             (with-text-style (stream (make-device-font-text-style
                                       (port stream)
                                       "-*-helvetica-bold-r-*-*-*-120-*-*-*-*-iso8859-15"))
               (prin1 (pprint-pop) stream))))
          (t
           (prin1 (pprint-pop) stream)))
    (write-string " " stream)
    (pprint-newline :miser)
    (prin1 (pprint-pop) stream)
    (write-string " " stream)
    (pprint-indent :block 2)
    (loop
        (pprint-exit-if-list-exhausted)
        (pprint-newline :mandatory stream)
      (prin1 (pprint-pop)))))
 0
 *my-dispatch*)

(defmethod climi::text-style-equalp (x y) (eq x y))

;;
;; $Log: $
;;
