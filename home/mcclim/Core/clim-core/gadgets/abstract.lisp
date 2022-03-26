(in-package #:climi)

;;;; -------------------------------------------------------------------------
;;;;
;;;;  30.4 Abstract Gadget Classes
;;;;

;;; 30.4.1 The abstract push-button Gadget

(defclass push-button (labelled-gadget-mixin action-gadget)
  ())

;;; 30.4.2 The abstract toggle-button Gadget

(defclass toggle-button (labelled-gadget-mixin value-gadget)
  ()
  (:documentation "The value is either t either nil"))

;;; 30.4.3 The abstract menu-button Gadget

(defclass menu-button (labelled-gadget-mixin value-gadget)
  ()
  (:documentation "The value is a button"))

;;; 30.4.4 The abstract scroll-bar Gadget

(defclass scroll-bar (value-gadget oriented-gadget-mixin range-gadget-mixin)
  ((drag-callback :initarg :drag-callback
                  :initform nil
                  :reader scroll-bar-drag-callback)
   (scroll-to-bottom-callback :initarg :scroll-to-bottom-callback
                              :initform nil
                              :reader scroll-bar-scroll-to-bottom-callback)
   (scroll-to-top-callback :initarg :scroll-to-top-callback
                           :initform nil
                           :reader scroll-bar-scroll-to-top-callback)
   (scroll-down-line-callback :initarg :scroll-down-line-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-line-callback)
   (scroll-up-line-callback :initarg :scroll-up-line-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-line-callback)
   (scroll-down-page-callback :initarg :scroll-down-page-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-page-callback)
   (scroll-up-page-callback :initarg :scroll-up-page-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-page-callback)
   (thumb-size :initarg :thumb-size :initform 1/4
               :accessor scroll-bar-thumb-size
               :documentation "The size of the scroll bar thumb (slug) in the
  units of the gadget value. When the scroll bar is drawn the empty region of
  the scroll bar and the thumb are drawn in proportion  to the values of the
  gadget range and thumb size."))
  (:default-initargs :value 0
                     :min-value 0
                     :max-value 1
                     :orientation :vertical))

(defmethod drag-callback ((pane scroll-bar) client gadget-id value)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-drag-callback pane) value))

(defmethod scroll-to-top-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-top-callback pane)))

(defmethod scroll-to-bottom-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-bottom-callback pane)))

(defmethod scroll-up-line-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-line-callback pane)))

(defmethod scroll-up-page-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-page-callback pane)))

(defmethod scroll-down-line-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-line-callback pane)))

(defmethod scroll-down-page-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-page-callback pane)))

;;; 30.4.5 The abstract slider Gadget

(defclass slider (labelled-gadget-mixin
                  value-gadget
                  oriented-gadget-mixin
                  range-gadget-mixin)
  ((drag-callback  :initform nil
                   :initarg :drag-callback
                   :reader slider-drag-callback)
   (show-value-p   :type boolean
                   :initform nil
                   :initarg :show-value-p
                   :accessor gadget-show-value-p)
   (decimal-places :initform 0
                   :initarg :decimal-places
                   :reader slider-decimal-places)
   (number-of-quanta :initform nil
                     :initarg :number-of-quanta
                     :reader slider-number-of-quanta))
  (:documentation "The value is a real number, and default value for orientation is :vertical,
and must never be nil.")
  (:default-initargs :orientation :vertical))

(defmethod drag-callback ((pane slider) client gadget-id value)
  (declare (ignore client gadget-id))
  (when (slider-drag-callback pane)
    (funcall (slider-drag-callback pane) pane value)))

;;; 30.4.6 The abstract radio-box and check-box Gadgets

;; The only real difference between a RADIO-BOX and a CHECK-BOX is the
;; number of allowed selections.

(defclass radio-box (value-gadget oriented-gadget-mixin)
  ()
  (:documentation "The value is a button")
  (:default-initargs
   :value nil))

;; RADIO-BOX-CURRENT-SELECTION is just a synonym for GADGET-VALUE:

(defmethod radio-box-current-selection ((radio-box radio-box))
  (gadget-value radio-box))

(defmethod (setf radio-box-current-selection) (new-value (radio-box radio-box))
  (setf (gadget-value radio-box) new-value))

(defmethod radio-box-selections ((pane radio-box))
  (loop for child in (sheet-children pane)
        when (typep child 'toggle-button)
          collect child))

(defmethod value-changed-callback :before (value-gadget (client radio-box) gadget-id value)
  (declare (ignorable value-gadget gadget-id value))
  ;; Note that we ignore 'value', this is because if value is non-NIL,
  ;; then the toggle button was turned off, which would make no
  ;; toggle-button turned on => constraint "always exactly one
  ;; selected" missed. So simply turning this toggle button on again
  ;; fixes it.
  (unless (or (and (not value)
                   (not (eq (gadget-value client) value-gadget)))
              (and value
                   (eq (gadget-value client) value-gadget)))
    (setf (gadget-value client :invoke-callback t) value-gadget)))

;;;; CHECK-BOX

(defclass check-box (value-gadget oriented-gadget-mixin)
  ()
  (:documentation "The value is a list of buttons")
  (:default-initargs
   :value nil
   :orientation :vertical))

;; CHECK-BOX-CURRENT-SELECTION is just a synonym for GADGET-VALUE:
(defmethod check-box-current-selection ((check-box check-box))
  (gadget-value check-box))

(defmethod (setf check-box-current-selection) (new-value (check-box check-box))
  (setf (gadget-value check-box) new-value))

(defmethod check-box-selections ((pane check-box))
  (loop for child in (sheet-children pane)
        when (typep child 'toggle-button)
          collect child))

(defmethod value-changed-callback :before (value-gadget (client check-box) gadget-id value)
  (declare (ignorable gadget-id))
  (if value
      (setf (gadget-value client :invoke-callback t)
            (adjoin value-gadget (gadget-value client)))
      (setf (gadget-value client :invoke-callback t)
            (remove value-gadget (gadget-value client)))))

(defmethod (setf gadget-value) :after (buttons (check-box check-box) &key invoke-callback)
  ;; this is silly, but works ...
  (dolist (c (sheet-children check-box))
    (unless (eq (not (null (member c buttons)))
                (not (null (gadget-value c))))
      (setf (gadget-value c :invoke-callback invoke-callback) (member c buttons)) )))

(defmacro with-radio-box ((&rest options
                           &key (type :one-of) (orientation :vertical) &allow-other-keys)
                          &body body)
  (let ((contents (gensym "CONTENTS-"))
        (selected-p (gensym "SELECTED-P-"))
        (initial-selection (gensym "INITIAL-SELECTION-")))
    `(let ((,contents nil)
           (,selected-p nil)
           (,initial-selection nil))
       (declare (special ,selected-p))
       (flet ((make-pane (type &rest options)
                (cond ((eq type 'toggle-button)
                       (let ((pane (apply #'make-pane type
                                          :value ,selected-p
                                          :indicator-type ',type
                                          options)))
                         (push pane ,contents)
                         (when ,selected-p
                           (push pane ,initial-selection))))
                      (t
                       (error "oops")))))
         (macrolet ((radio-box-current-selection (subform)
                      `(let ((,',selected-p t))
                         (declare (special ,',selected-p))
                         ,(cond ((stringp subform)
                                 `(make-pane 'toggle-button :label ,subform))
                                (t
                                 subform)))))
           ,@(mapcar (lambda (form)
                       (cond ((stringp form)
                              `(make-pane 'toggle-button :label ,form))
                             (t
                              form)))
                     body)))
       (make-pane ',(if (eq type :one-of)
                        'radio-box
                        'check-box)
                  :orientation ',orientation
                  :current-selection ,(if (eq type :one-of)
                                          `(or (first ,initial-selection)
                                               (first ,contents))
                                          `,initial-selection)
                  :choices (reverse ,contents)
                  ,@options))))

;;; 30.4.7 The abstract list-pane and option-pane Gadgets

(defclass meta-list-pane ()
  ((mode        :initarg :mode
                :initform :exclusive
                :reader list-pane-mode
                :type (member :one-of :some-of :exclusive :nonexclusive))
   (items       :initarg :items
                :initform nil
                :reader list-pane-items
                :type sequence)
   (name-key    :initarg :name-key
                :initform #'princ-to-string
                :reader list-pane-name-key
                :documentation "A function to be applied to items to gain a printable representation")
   (value-key   :initarg :value-key
                :initform #'identity
                :reader list-pane-value-key
                :documentation "A function to be applied to items to gain its value
                                for the purpose of GADGET-VALUE.")
   (presentation-type-key :initarg :presentation-type-key
                          :initform (constantly nil)
                          :reader list-pane-presentation-type-key
                          :documentation "A function to be applied to items to find the presentation types for their values, or NIL.")
   (test        :initarg :test
                :initform #'eql
                :reader list-pane-test
                :documentation "A function to compare two items for equality.")))

(defclass list-pane (meta-list-pane value-gadget)
  ((visible-items :initarg :visible-items ; Clim 2.2 compatibility
                  :initform nil
                  :documentation "Maximum number of visible items in list"))
  (:documentation
   "The instantiable class that implements an abstract list pane, that is, a gadget
    whose semantics are similar to a radio box or check box, but whose visual
    appearance is a list of buttons.")
  (:default-initargs :value nil))

(defclass option-pane (meta-list-pane value-gadget)
  ()
  (:documentation
   "The instantiable class that implements an abstract option pane, that is, a
    gadget whose semantics are identical to a list pane, but whose visual
    appearance is a single push button which, when pressed, pops up a menu of
    selections."))

;;; 30.4.8 The abstract text-field Gadget

(defclass text-field (value-gadget action-gadget)
  ((editable-p :accessor editable-p :initarg editable-p :initform t))
  (:documentation "The value is a string")
  (:default-initargs :value ""))

(defmethod initialize-instance :after ((gadget text-field) &rest rest)
  (unless (getf rest :normal)
    (setf (slot-value gadget 'current-color) +white+
          (slot-value gadget 'normal) +white+)))

;;; 30.4.9 The abstract text-editor Gadget

(defclass text-editor (text-field)
  ((ncolumns :reader text-editor-ncolumns
             :initarg :ncolumns
             :initform nil
             :type (or null integer))
   (nlines :reader text-editor-nlines
           :initarg :nlines
           :initform nil
           :type (or null integer)))
  (:documentation "The value is a string"))
