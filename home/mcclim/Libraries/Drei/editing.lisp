;;; -*- Mode: Lisp; Package: DREI-EDITING; -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Drei editing

;;; See information in motion.lisp
;;;
;;; An editing function is a function named FORWARD-<frob>-<unit> or
;;; BACKWARD-<frob>-<unit>, or just <frob>-<unit> in the case where
;;; discering between forward and backward commands does not make
;;; sense (an example is TRANSPOSE-<unit>).
;;;
;;; A proper unit is a unit for which all the functions required by
;;; the motion protocol has been implemented, this can be trivially
;;; done by using the macro DREI-MOTION:DEFINE-MOTION-COMMANDS.
;;;
;;; Given a proper unit,
;;;
;;;   (DEFINE-EDIT-FNS <unit>)
;;;
;;; defines the editing functions FORWARD-DELETE-<unit>,
;;; BACKWARD-DELETE-<unit>, FORWARD-KILL-<unit>, BACKWARD-KILL-<unit>
;;; and TRANSPOSE-<unit>.
;;;
;;; This file also holds definitions for other miscellaneus
;;; editing-related functions. The definitions in this file should
;;; have to do with immediate editing, understood as insertion,
;;; deletion or movement of buffer contents. Transformation of buffer
;;; contents (such as converting the case of a region) should not be
;;; here.

(in-package :drei-editing)

(defmacro define-edit-fns (unit &key plural)
  (labels ((concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings)))
           (symbol (&rest strings)
             (intern (apply #'concat strings))))
    (let* ((unit-name (string-downcase unit))
           (plural (or plural (concat unit-name "s")))
           (upper-plural (string-upcase plural))
           (forward-delete (symbol "FORWARD-DELETE-" unit))
           (backward-delete (symbol "BACKWARD-DELETE-" unit))
           (forward-kill (symbol "FORWARD-KILL-" unit))
           (backward-kill (symbol "BACKWARD-KILL-" unit))
           (transpose (symbol "TRANSPOSE-" upper-plural))
           (forward (find-symbol (concat "FORWARD-" (string-upcase unit))))
           (backward (find-symbol (concat "BACKWARD-" (string-upcase unit)))))
      (unless (and forward backward)
        (error "The unit ~A is not known." unit))
      `(progn
         (defgeneric ,forward-delete
             (mark syntax &optional count limit-action)
           (:documentation
            ,(concat "Delete COUNT " plural " beginning from MARK.")))
         (defmethod ,forward-delete
             (mark syntax &optional (count 1)
              (limit-action #'error-limit-action))
           (let ((mark2 (clone-mark mark)))
             (when (,forward mark2 syntax count limit-action)
               (delete-region mark mark2)
               t)))
         (defmethod ,forward-delete :around
             (mark syntax &optional (count 1)
                   (limit-action #'error-limit-action))
           (cond ((minusp count)
                  (,backward-delete mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,backward-delete
             (mark syntax &optional count limit-action)
           (:documentation
            ,(concat "Delete COUNT " plural " backwards beginning from MARK.")))
         (defmethod ,backward-delete
             (mark syntax &optional (count 1)
              (limit-action #'error-limit-action))
           (let ((mark2 (clone-mark mark)))
             (when (,backward mark2 syntax count limit-action)
               (delete-region mark mark2)
               t)))
         (defmethod ,backward-delete :around
             (mark syntax &optional (count 1)
                   (limit-action #'error-limit-action))
           (cond ((minusp count)
                  (,forward-delete mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,forward-kill
             (mark syntax &optional count concatenate-p limit-action)
           (:documentation
            ,(concat "Kill COUNT " plural " beginning from MARK.")))
         (defmethod ,forward-kill
             (mark syntax &optional (count 1) concatenate-p
              (limit-action #'error-limit-action))
           (let ((start (offset mark)))
             (,forward mark syntax count limit-action)
             (unless (mark= mark start)
               (if concatenate-p
                   (if (plusp count)
                       (kill-ring-concatenating-push *kill-ring*
                                                     (region-to-sequence start mark))
                       (kill-ring-reverse-concatenating-push *kill-ring*
                                                             (region-to-sequence start mark)))
                   (kill-ring-standard-push *kill-ring*
                                            (region-to-sequence start mark)))
               (delete-region start mark)
               t)))
         (defmethod ,forward-kill :around
             (mark syntax &optional (count 1) concatenate-p
                   (limit-action #'error-limit-action))
           (declare (ignore concatenate-p))
           (cond ((minusp count)
                  (,backward-kill mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,backward-kill
             (mark syntax &optional count concatenate-p limit-action)
           (:documentation
            ,(concat "Kill COUNT " plural " backwards beginning from MARK.")))
         (defmethod ,backward-kill
             (mark syntax &optional (count 1) concatenate-p
              (limit-action #'error-limit-action))
           (let ((start (offset mark)))
             (,backward mark syntax count limit-action)
             (unless (mark= mark start)
               (if concatenate-p
                   (if (plusp count)
                       (kill-ring-concatenating-push *kill-ring*
                                                     (region-to-sequence start mark))
                       (kill-ring-reverse-concatenating-push *kill-ring*
                                                             (region-to-sequence start mark)))
                   (kill-ring-standard-push *kill-ring*
                                            (region-to-sequence start mark)))
               (delete-region start mark)
               t)))
         (defmethod ,backward-kill :around
             (mark syntax &optional (count 1) concatenate-p
                   (limit-action #'error-limit-action))
           (declare (ignore concatenate-p))
           (cond ((minusp count)
                  (,forward-kill mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,transpose
             (mark syntax)
           (:documentation
            ,(concat "Transpose two " plural " at MARK.")))
         (defmethod ,transpose
             ((mark right-sticky-mark) syntax)
           (let ((start1 (clone-mark mark)))
             (,backward start1 syntax 1 nil)
             (let ((end1 (clone-mark start1)))
               (,forward end1 syntax 1 #'error-limit-action)
               (let ((start2 (clone-mark end1)))
                 (,forward start2 syntax 1 #'error-limit-action)
                 (let ((end2 (clone-mark start2)))
                   (,backward start2 syntax 1 nil)
                   (as-region (start1 end1)
                     (as-region (start2 end2)
                       (when (mark> start1 start2)
                         (psetf start1 start2
                                end1 end2
                                start2 start1
                                end2 end1))
                       (if (mark> end1 start2)
                           (error-limit-action mark (offset mark) 0 ,unit-name syntax)
                           (let ((obj2 (extract-region start2 end2)))
                             (insert-sequence start2 (extract-region start1 end1))
                             (insert-sequence start1 obj2)
                             (setf (offset mark) (offset end2)))))))))))
         (defmethod ,transpose
             ((mark left-sticky-mark) syntax)
           (let ((start1 (clone-mark mark)))
             (,backward start1 syntax 1 nil)
             (let ((end1 (clone-mark start1)))
               (,forward end1 syntax 1 #'error-limit-action)
               (let ((start2 (clone-mark end1)))
                 (,forward start2 syntax 1 #'error-limit-action)
                 (let ((end2 (clone-mark start2)))
                   (,backward start2 syntax 1 nil)
                   (as-region (start1 end1)
                     (as-region (start2 end2)
                       (when (mark> start1 start2)
                         (psetf start1 start2
                                end1 end2
                                start2 start1
                                end2 end1))
                       (if (mark> end1 start2)
                           (error-limit-action mark (offset mark) 0 ,unit-name syntax)
                           (let ((obj2 (extract-region start2 end2)))
                             (insert-sequence start2 (extract-region start1 end1))
                             (insert-sequence start1 obj2)
                             (setf (offset mark) (offset end2)))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Object editing

(defun forward-delete-object (mark &optional (count 1) limit-action)
  "Kill `count' objects beginning from `mark'."
  (let ((offset (offset mark)))
    (handler-case (progn (forward-object mark count)
                         (delete-region offset mark))
      (invalid-motion ()
        (when limit-action
          (funcall limit-action mark (offset mark)
                   count "object" nil))))))

(defun backward-delete-object (mark &optional (count 1) limit-action)
  "Kill `count' objects backwards beginning from `mark'."
  (let ((offset (offset mark)))
    (handler-case (progn (backward-object mark count)
                         (delete-region offset mark))
      (invalid-motion ()
        (when limit-action
          (funcall limit-action mark (offset mark)
                   (- count) "object" nil))))))

(defun forward-kill-object (mark &optional (count 1) concatenate-p limit-action)
  "Kill `count' objects beginning from `mark'."
  (let ((start (offset mark)))
    (handler-case (progn (forward-object mark count)
                         (if concatenate-p
                             (if (plusp count)
                                 (kill-ring-concatenating-push
                                  *kill-ring* (region-to-sequence start mark))
                                 (kill-ring-reverse-concatenating-push
                                  *kill-ring* (region-to-sequence start mark)))
                             (kill-ring-standard-push
                              *kill-ring* (region-to-sequence start mark)))
                         (delete-region start mark))
      (invalid-motion ()
        (when limit-action
          (funcall limit-action mark (offset mark)
                   (- count) "object" nil))))))

(defun backward-kill-object (mark &optional (count 1) concatenate-p limit-action)
  "Kill `count' objects backwards beginning from `mark'."
  (let ((start (offset mark)))
    (handler-case (progn (backward-object mark count)
                         (if concatenate-p
                             (if (plusp count)
                                 (kill-ring-concatenating-push
                                  *kill-ring* (region-to-sequence start mark))
                                 (kill-ring-reverse-concatenating-push
                                  *kill-ring* (region-to-sequence start mark)))
                             (kill-ring-standard-push
                              *kill-ring* (region-to-sequence start mark)))
                         (delete-region start mark))
      (invalid-motion ()
        (when limit-action
          (funcall limit-action mark (offset mark)
                   (- count) "object" nil))))))

(defun transpose-objects (mark)
  "Transpose two objects at `mark'."
  (unless (beginning-of-buffer-p mark)
    (when (end-of-line-p mark)
      (backward-object mark))
    (unless (beginning-of-buffer-p mark)
      (let ((object (object-after mark)))
        (delete-range mark)
        (backward-object mark)
        (insert-object mark object)
        (forward-object mark)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Line editing

(define-edit-fns line)
(define-edit-fns line-start)

;; Autogenerated TRANSPOSE-LINES is not good enough.
(defmethod transpose-lines
    ((mark left-sticky-mark) syntax)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (backward-line mark syntax))
  (let* ((bol (offset mark))
	 (eol (progn (end-of-line mark)
		     (offset mark)))
	 (line (buffer-sequence (buffer mark) bol eol)))
    (delete-region bol mark)
    ;; Remove newline at end of line as well.
    (unless (end-of-buffer-p mark)
      (delete-range mark))
    (end-of-line mark)
    (forward-line mark syntax 0)
    (insert-sequence mark line)
    (insert-object mark #\Newline)))

(defmethod transpose-lines
    ((mark right-sticky-mark) syntax)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (backward-line mark syntax))
  (let* ((bol (offset mark))
	 (eol (progn (end-of-line mark)
		     (offset mark)))
	 (line (buffer-sequence (buffer mark) bol eol)))
    (delete-region bol mark)
    ;; Remove newline at end of line as well.
    (unless (end-of-buffer-p mark)
      (delete-range mark))
    (end-of-line mark)
    (insert-object mark #\Newline)
    (forward-line mark syntax 0)
    (insert-sequence mark line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Word editing

(define-edit-fns word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Page editing

(define-edit-fns page)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Paragraph editing

(define-edit-fns paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Sentence editing

(define-edit-fns sentence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Expression editing

(define-edit-fns expression)
(define-edit-fns definition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; List editing

(define-edit-fns list)
