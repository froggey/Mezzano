(in-package #:clim-demo)

(define-application-frame selection-demo ()
  ((pasted-element :initform nil :accessor pasted-element)
   (display-count :initform 0 :accessor display-count)
   (selection-box :initform :clipboard :accessor selection-box))
  (:panes (app :application :display-function #'display
               :display-time :command-loop
               :end-of-line-action :wrap*
               :text-margins '(:top (:relative 20)
                               :left (:relative 30)
                               :right (:absolute 400)))
          (pas :application :display-function #'display-paste
               :display-time :command-loop)
          (int :Interactor))
  (:layouts (default (vertically (:width 800)
                       (350 app)
                       (200 pas)
                       (200 int))))
  (:pointer-documentation t))

(defmethod display ((frame selection-demo) pane)
  (incf (display-count frame))
  (flet ((show-number (n)
           (format pane "~r" n)))
    (dotimes (v 50)
      (with-output-as-presentation (pane v 'integer)
        (if (= v (display-count frame))
            (with-drawing-options (pane :ink +red+)
              (show-number v))
            (show-number v)))
      (write-string " " pane)))
  (fresh-line pane)
  (terpri pane)
  (with-output-as-gadget (pane)
    (make-pane 'push-button :label "Paste (see #773)"
               :activate-callback (lambda (gadget)
                                    (declare (ignore gadget))
                                    (execute-frame-command frame '(com-paste)))))
  (terpri pane)
  (with-output-as-gadget (pane)
    (make-pane 'option-pane
               :items '(:clipboard :primary :local-selection :internal)
               :name-key (lambda (n) (string-downcase (format nil "~s" n)))
               :value-changed-callback (lambda (gadget new-value)
                                         (declare (ignore gadget))
                                         (setf (selection-box *application-frame*) new-value))
               :value (selection-box *application-frame*)))
  (fresh-line pane)
  (with-translation (pane 450 20)
    (present (make-instance 'image)))
  (with-translation (pane 575 20)
    (present (make-instance 'image :caption "Figure 1: Glider.") 'figure))
  (with-temporary-margins (pane :top '(:absolute 150)
                                :left '(:absolute 450)
                                :right '(:absolute 750)
                                :move-cursor nil)
    (setf (stream-cursor-position pane) (values 450 150))
    (present (format nil "Short text (chrzęść Mårt)."))
    (terpri)
    (present (format nil "This whole text is a string: ~{~R~^, ~}." (alexandria:iota 15 :start 42 :step 6)))
    (fresh-line pane)
    (terpri pane)
    (surrounding-output-with-border (pane)
      (with-output-as-gadget (pane)
        (make-pane :text-editor :width 300)))))

(defun display-paste (frame pane)
  (let ((*standard-output* pane))
    (format t "Selection box ~s." (selection-box frame))

    (setf (stream-cursor-position pane) (values 15 25))
    (macrolet ((thunk (type)
                 `(formatting-cell (pane)
                    (multiple-value-bind (object ptype)
                        (request-selection pane (selection-box frame) ,type)
                      (if object
                          (present object (or ptype (presentation-type-of object)))
                          (format pane "(nothing)")))))
               (thunks (&rest types)
                 `(formatting-row (pane)
                    ,@(mapcar (lambda (type) `(thunk ,type)) types))))
      (surrounding-output-with-border (pane)
        (formatting-table (pane :x-spacing 25)
          (formatting-row (pane)
            (formatting-cell (pane) (format pane "T"))
            (formatting-cell (pane) (format pane "String"))
            (formatting-cell (pane) (format pane "Integer"))
            (formatting-cell (pane) (format pane "Float"))
            (formatting-cell (pane) (format pane "Image"))
            (formatting-cell (pane) (format pane "Figure")))
          (thunks t 'string 'integer 'float 'image 'figure))))))

(defclass image ()
  ((image :initform (make-pattern-from-bitmap-file
                     (merge-pathnames
                      #p"images/glider.png"
                      (asdf:system-source-directory :clim-examples)))
          :reader pattern)
   (caption :initform nil :initarg :caption :reader caption)))

(define-presentation-type image ())
(define-presentation-type figure ())
;;; XXX: see #778.
(define-presentation-type :|text/html| nil)

(define-presentation-method present (object (type image) stream view &key)
  (ignore-errors (draw-design stream (pattern object))))

(define-presentation-method present (object (type figure) stream view &key)
  (ignore-errors
   (draw-design stream (pattern object))
   (draw-text* stream (caption object) 0 105 :align-y :top :text-size :small)))

(define-presentation-action copy-integer
    (integer nil selection-demo :gesture :select)
    (object)
  (publish-selection *standard-output* (selection-box *application-frame*) object 'integer))

(define-presentation-action copy-string
    (string nil selection-demo :gesture :select)
    (object)
  (publish-selection *standard-output* (selection-box *application-frame*) object 'string))

(define-presentation-action copy-figure
    (figure nil selection-demo :gesture :select)
    (object)
  (publish-selection *standard-output* (selection-box *application-frame*) object 'figure))

(define-presentation-action copy-image
    (image nil selection-demo :gesture :select)
    (object)
  (publish-selection *standard-output* (selection-box *application-frame*) object 'image))

(define-presentation-action copy-float
    (float nil selection-demo :gesture :select)
    (object)
  (publish-selection *standard-output* (selection-box *application-frame*) object 'float))

(define-selection-translator int->float
    (integer float global-command-table) (object)
  (coerce object 'float))

(define-selection-translator figure->int
    (figure integer global-command-table) (object)
  (values 31337 'integer))

(define-selection-translator image->int
    (image integer global-command-table) (object)
  42)

(define-selection-translator int->string
    (integer string global-command-table) (object)
  (format nil "~d" object))

(define-selection-translator figure->html
    (figure :|text/html| global-command-table) (object)
  (format nil "<figure>magic pufff</figure>"))

(define-selection-translator html->float
    (:|text/html| float global-command-table) (object)
  3.471111)

(define-selection-translator float->html
    (float :|text/html| global-command-table) (object)
  (format nil "<float>~s</float>" object))

;;; this selection translator shows how to add testers to translators.
(defun test--string->int (obj &rest args)
  (declare (ignore args))
  (parse-integer obj :junk-allowed t))

(define-selection-translator string->int
    (string integer global-command-table :tester test--string->int) (object)
  (values (parse-integer object :junk-allowed t) 'integer))

(define-selection-translator figure->string
    (figure string global-command-table) (object)
  (caption object))

(define-selection-translator figure->image
    (figure image global-command-table) (object)
  object)

(define-selection-demo-command (com-paste :menu t) ()
  (setf (pasted-element *application-frame*)
        (request-selection *standard-output* (selection-box *application-frame*) t)))

(define-selection-demo-command (com-print-integer :name t)
    ((a integer))
  (format *standard-input* "integer: ~a" a))

;; (defun cl-user::run ()
;;   (run-frame-top-level
;;    (make-application-frame 'selection-demo)))

;; (bt:make-thread #'cl-user::run)
