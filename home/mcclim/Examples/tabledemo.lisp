(in-package #:clim-demo)

(define-application-frame table-demo ()
  ((tab1-active-row  :initform nil :accessor active-row1)
   (tab1-active-cell :initform nil :accessor active-cell1)
   (tab2-active-col  :initform nil :accessor active-col2)
   (tab2-active-cell :initform nil :accessor active-cell2))
  (:menu-bar nil)
  (:pane :application :display-function 'display)
  ;; comment this for funny effect
  (:geometry :width 750 :height 400))

(define-presentation-type table-coordinates ())
(define-presentation-type table-coordinates2 ())

(define-table-demo-command (com-select-cell) ((object t))
  (setf (active-row1 *application-frame*) (car object)
        (active-cell1 *application-frame*) (cdr object)))

(define-table-demo-command (com-select-cell2) ((object t))
  (setf (active-col2 *application-frame*) (car object)
        (active-cell2 *application-frame*) (cdr object)))

(define-presentation-to-command-translator select-cell
    (table-coordinates com-select-cell table-demo)
    (object)
  (list object))

(define-presentation-to-command-translator select-cell2
    (table-coordinates2 com-select-cell2 table-demo)
    (object)
  (list object))

;;; let's cause some parenthesis cancer
(defmacro maybe-border (options test &body body)
  `(if ,test
       (surrounding-output-with-border ,options ,@body)
       (progn ,@body)))

(defmethod display ((frame table-demo) pane)
  (declare (ignorable frame))
  (setf (stream-cursor-position pane) (values 100 100))
  (with-text-family (pane :fix)
    (formatting-table (pane :x-spacing 20 :y-spacing 5)
      (dotimes (row 5)
        (maybe-border (pane :shape :rounded
                            :background +light-goldenrod+
                            :outline-ink +dark-blue+
                            :shadow +gray+
                            :line-thickness 2
                            :line-dashes t)
            (eql row (active-row1 frame))
          (formatting-row (pane)
            (dotimes (cell 5)
              (maybe-border (pane :shape :oval
                                  :background +light-sea-green+
                                  :outline-ink +red+
                                  :shadow +gray+
                                  :line-thickness 2
                                  :line-dashes t)
                  (and (eql row (active-row1 frame))
                       (eql cell (active-cell1 frame)))
               (formatting-cell (pane)
                 (with-output-as-presentation (pane (cons row cell)
                                                    'table-coordinates
                                                    :single-box t)
                   (format pane "row ~s, cell ~s" row cell)))))))))
    (terpri)
    (multiple-value-bind (x y) (stream-cursor-position pane)
      (declare (ignore x))
      (setf (stream-cursor-position pane) (values 100 y)))
    (formatting-table (pane :x-spacing 20 :y-spacing 5)
      (dotimes (column 5)
        (maybe-border (pane :shape :rounded
                            :background +light-goldenrod+
                            :outline-ink +dark-blue+
                            :shadow +gray+
                            :line-thickness 2
                            :line-dashes t)
            (eql column (active-col2 frame))
          (formatting-column (pane)
            (dotimes (cell 5)
              (maybe-border (pane :shape :oval
                                  :background +light-sea-green+
                                  :outline-ink +red+
                                  :shadow +gray+
                                  :line-thickness 2
                                  :line-dashes t)
                  (and (eql column (active-col2 frame))
                       (eql cell (active-cell2 frame)))
                (formatting-cell (pane)
                  (with-output-as-presentation (pane (cons column cell)
                                                     'table-coordinates2
                                                     :single-box t)
                    (format pane "col ~s, cell ~s" column cell)))))))))))

(defun tabledemo ()
  (run-frame-top-level (make-application-frame 'table-demo)))
