(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :mcclim))

(cl:in-package #:clim-user)

(defclass cell () ((content :accessor content :initarg :content)))

(defun make-cell (&rest args)
  (apply #'make-instance 'cell args))

(define-presentation-type cell ())

(defvar loop-detector nil)

(defun c (h w)
  (if (not loop-detector)
      (error "No loop-detector in dynamic environment.")
    (if (aref loop-detector h w)
        (error "Evaluation loop detected.")
      (progn
        (setf (aref loop-detector h w) t)
        (eval (content (aref (frame-cells *application-frame*) h w)))))))

(define-presentation-method present (cell (type cell) stream (view textual-view) &key)
  (let ((loop-detector (make-array (array-dimensions (frame-cells *application-frame*))
                                   :initial-element nil)))
    (format stream "~A" (eval (content cell)))))

(defclass cell-unparsed-text (textual-dialog-view) ())

(defconstant +cell-unparsed-text-view+ (make-instance 'cell-unparsed-text))

(define-presentation-method present (cell (type cell) stream (view cell-unparsed-text) &key)
  (format stream "~A" (content cell)))

(define-presentation-method accept ((type cell) stream (view cell-unparsed-text) &key)
  (make-cell :content (read stream)))

(defmethod print-object ((cell cell) stream)
  (format stream "<CELL ~A>" (content cell)))

(defun initial-cells ()
  (insert-column (insert-column (insert-row (insert-row #2A() 0) 0) 0) 0))

(defun insert-row (old-cells insert-before)
  (destructuring-bind (height width) (array-dimensions old-cells)
    (let ((new-cells (make-array `(,(1+ height) ,width))))
      (dotimes (i (1+ height))
        (dotimes (j width)
          (setf (aref new-cells i j)
                (cond
                 ((eq i insert-before) (make-cell :content 0))
                 ((< i insert-before) (aref old-cells i j))
                 ((> i insert-before) (aref old-cells (1- i) j))))))
      new-cells)))

(defun insert-column (old-cells insert-before)
  (destructuring-bind (height width) (array-dimensions old-cells)
    (let ((new-cells (make-array `(,height ,(1+ width)))))
      (dotimes (i height)
        (dotimes (j (1+ width))
          (setf (aref new-cells i j)
                (cond
                 ((eq j insert-before) (make-cell :content 0))
                 ((< j insert-before) (aref old-cells i j))
                 ((> j insert-before) (aref old-cells i (1- j)))))))
      new-cells)))

(defun cell-to-grid-position (cell cells)
  (destructuring-bind (height width) (array-dimensions cells)
    (dotimes (i height)
      (dotimes (j width)
        (if (eq cell (aref cells i j))
            (return-from cell-to-grid-position (values i j)))))))


(define-application-frame spreadsheet ()
  ((cells :accessor frame-cells :initform (initial-cells)))
  (:panes
   (cellpane :application :display-function #'display-cells)
   (interactor :interactor))
  (:layouts
   (default (vertically () cellpane interactor)))
  (:menu-bar nil)
  (:pointer-documentation t))

(defmethod frame-standard-output ((frame spreadsheet))
  (find-pane-named frame 'interactor))

(defun display-cells (frame pane)
  (destructuring-bind (height width) (array-dimensions (frame-cells frame))
    (formatting-table (pane)
      (dotimes (i height)
        (formatting-row (pane)
          (dotimes (j width)
            (formatting-cell (pane)
              (present (aref (frame-cells frame) i j) 'cell :stream pane))))))))

(define-spreadsheet-command (com-change-cell :name t)
    ((cell 'cell :gesture :select))
  (let ((frame-input (frame-standard-input *application-frame*))
        new-cell)
    (accepting-values (frame-input)
      (setq new-cell (accept 'cell :stream frame-input
                                   :view +cell-unparsed-text-view+
                                   :default cell)))
    (setf (content cell) (content new-cell))))

(defmacro with-cell-position (args cell &body body)
  `(multiple-value-bind (,(first args) ,(second args))
       (cell-to-grid-position ,cell (frame-cells *application-frame*))
    ,@body))

(define-spreadsheet-command (com-cell-to-position :name t)
    ((cell 'cell :gesture :menu))
  (with-cell-position (y x) cell
    (format t "X: ~A Y: ~A~&" x y)))

(define-spreadsheet-command (com-insert-row-before-cell :name t)
    ((cell 'cell :gesture :menu))
  (with-application-frame (frame)
    (with-cell-position (y x) cell
      (declare (ignore y))
      (setf (frame-cells frame) (insert-row (frame-cells frame) x)))))

(define-spreadsheet-command (com-insert-row-after-cell :name t)
    ((cell 'cell :gesture :menu))
  (with-application-frame (frame)
    (with-cell-position (y x) cell
      (declare (ignore y))
      (setf (frame-cells frame) (insert-row (frame-cells frame) (1+ x))))))

(define-spreadsheet-command (com-insert-column-before-cell :name t)
    ((cell 'cell :gesture :menu))
  (with-application-frame (frame)
    (with-cell-position (y x) cell
      (declare (ignore x))
      (setf (frame-cells frame) (insert-column (frame-cells frame) y)))))

(define-spreadsheet-command (com-insert-column-after-cell :name t)
    ((cell 'cell :gesture :menu))
  (with-application-frame (frame)
    (with-cell-position (y x) cell
      (declare (ignore x))
      (setf (frame-cells frame) (insert-column (frame-cells frame) (1+ y))))))
