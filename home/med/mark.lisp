(in-package :med)
;;; Marks.


(defclass mark ()
  ((%line :initarg :line :reader mark-line)
   (%charpos :initarg :charpos :reader mark-charpos)
   ;; :left, :right or :temporary.
   (%kind :initarg :kind :reader mark-kind)))

(defgeneric (setf mark-line) (value mark))
(defgeneric (setf mark-charpos) (value mark))
(defgeneric (setf mark-kind) (value mark))

(defmethod print-object ((object mark) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S:~D ~S" (mark-line object) (mark-charpos object) (mark-kind object))))

;;; Mark management.

(defun make-mark (line charpos &optional kind)
  (setf kind (or kind :temporary))
  (check-type kind (member :left :right :temporary))
  (let ((mark (make-instance 'mark
                             :line line
                             :charpos charpos
                             :kind kind)))
    (unless (eql kind :temporary)
      (push mark (line-mark-list line)))
    mark))

(defmethod (setf mark-line) (value (mark mark))
  (unless (eql (mark-kind mark) :temporary)
    (setf (line-mark-list (mark-line mark)) (remove mark (line-mark-list (mark-line mark))))
    (push mark (line-mark-list value)))
  (setf (slot-value mark '%charpos) (min (line-length value) (mark-charpos mark))
        (slot-value mark '%line) value))

(defmethod (setf mark-charpos) (value (mark mark))
  (check-type value (integer 0))
  (assert (<= value (line-length (mark-line mark))) (value) "Tried to move mark past end of line.")
  (setf (slot-value mark '%charpos) value))

(defmethod (setf mark-kind) (value (mark mark))
  (check-type value (member :temporary :left :right))
  (unless (eql (mark-kind mark) :temporary)
    ;; Remove from existing mark list.
    (setf (line-mark-list (mark-line mark)) (remove mark (line-mark-list (mark-line mark)))))
  (unless (eql value :temporary)
    ;; Add to mark list.
    (push mark (line-mark-list (mark-line mark))))
  (setf (slot-value mark '%kind) value))

(defun copy-mark (mark &optional kind)
  (make-mark (mark-line mark) (mark-charpos mark) kind))

(defun delete-mark (mark)
  (setf (line-mark-list (mark-line mark)) (remove mark (line-mark-list (mark-line mark)))))

(defmacro with-mark ((name where &optional kind) &body body)
  `(let ((,name nil))
     (unwind-protect
          (progn
            (setf ,name (copy-mark ,where ,kind))
            ,@body)
       (when ,name
         (delete-mark ,name)))))

(defun move-mark-to-mark (move-this-one here)
  (setf (mark-line move-this-one) (mark-line here)
        (mark-charpos move-this-one) (mark-charpos here)))

(defun mark= (a b)
  (and (eql (mark-line a) (mark-line b))
       (eql (mark-charpos a) (mark-charpos b))))

(defun mark< (a b)
  (or (< (line-number (mark-line a)) (line-number (mark-line b)))
      (and (eql (line-number (mark-line a)) (line-number (mark-line b)))
           (< (mark-charpos a) (mark-charpos b)))))

(defun mark> (a b)
  (mark< b a))

(defun mark<= (a b)
  (not (mark> a b)))

(defun mark>= (a b)
  (not (mark< a b)))

(defun point-to-mark (buffer mark)
  (move-mark-to-mark (buffer-point buffer) mark))

(defun mark-to-point (buffer mark)
  (move-mark-to-mark mark (buffer-point buffer)))

(defun mark-at-point-p (buffer mark)
  (mark= mark (buffer-point buffer)))

(defun start-of-line-p (mark)
  (eql (mark-charpos mark) 0))

(defun end-of-line-p (mark)
  (eql (mark-charpos mark) (line-length (mark-line mark))))

;;; Mark stuff.

(defun set-mark (buffer)
  (cond
    ;; If the mark is active and the point is at mark, then
    ;; deactivate the mark.
    ((and (buffer-mark-active buffer)
          (mark-at-point-p buffer (buffer-mark buffer)))
     (setf (buffer-mark-active buffer) nil))
    ;; If the mark is not active, then activate it.
    ((not (buffer-mark-active buffer))
     (setf (buffer-mark-active buffer) t)))
  ;; Always move the mark to point.
  (mark-to-point buffer (buffer-mark buffer)))

(defun exchange-point-and-mark (buffer)
  (let ((saved (copy-mark (buffer-mark buffer))))
    (move-mark-to-mark (buffer-mark buffer) (buffer-point buffer))
    (move-mark-to-mark (buffer-point buffer) saved)
    (setf (buffer-mark-active buffer) t)))

