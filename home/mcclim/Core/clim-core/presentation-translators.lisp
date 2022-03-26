;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998-2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001-2002 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2019 by Daniel Kochmański (daniel@turtleware.eu)

;;; Implementation of presentation translators as defined in 23.7.

(in-package #:climi)


;;; 23.7.1 Defining Presentation Translators
(defclass presentation-translator ()
  ((name :reader name :initarg :name)
   (from-type :reader from-type :initarg :from-type)
   (to-type :reader to-type :initarg :to-type)
   (gesture :reader gesture :initarg :gesture)
   (tester :reader tester :initarg :tester)
   (tester-definitive :reader tester-definitive :initarg :tester-definitive)
   (documentation :reader translator-documentation :initarg :documentation)
   (pointer-documentation :reader pointer-documentation
                          :initarg :pointer-documentation)
   (menu :reader menu :initarg :menu)
   (priority :reader priority :initarg :priority :initform 0)
   (translator-function :reader translator-function
                        :initarg :translator-function)))

(defmethod initialize-instance :after ((obj presentation-translator)
                                       &key &allow-other-keys)
  (unless (slot-boundp obj 'pointer-documentation)
    (setf (slot-value obj 'pointer-documentation)
          (translator-documentation obj))))

(defmethod print-object ((obj presentation-translator) stream)
  (print-unreadable-object (obj stream :identity t)
    (format stream "Translator ~S from ~S to ~S"
            (name obj) (from-type obj) (to-type obj))))

(defclass presentation-action (presentation-translator)
  ())

(defmethod initialize-instance :after ((obj presentation-action)
                                       &key &allow-other-keys)
  (setf (slot-value obj 'tester-definitive) t))

(defmethod print-object ((obj presentation-action) stream)
  (print-unreadable-object (obj stream :identity t)
    (format stream "Action from ~S to ~S" (from-type obj) (to-type obj))))

;; Wraps the tester function with a test that determines if the
;; command is enabled.
(defclass presentation-command-translator (presentation-translator)
  ())

(defmethod initialize-instance :after ((obj presentation-command-translator)
                                       &key tester command-name)
  (setf (slot-value obj 'tester)
        #'(lambda (&rest args)
            (if (command-enabled command-name *application-frame*)
                (maybe-apply tester args)
                nil))))

(defclass selection-translator (presentation-translator)
  ())

;;; This lives in a command table

(defvar *current-translator-cache-generation* 0
  "This is incremented whenever presentation translators are defined,
and used to ensure that presentation-translators-caches are up to date.")

(defclass translator-table ()
  ((translators :accessor translators :initarg :translators
                :initform (make-hash-table :test #'eq)
                :documentation "Translators keyed by name.")
   (simple-type-translators :accessor simple-type-translators
                            :initarg :simple-type-translators
                            :initform (make-hash-table :test #'eq)
                            :documentation "Holds transators with a simple
  from-type (i.e. doesn't contain \"or\" or \"and\").")
   (translator-cache-generation :accessor translator-cache-generation
                                :initform 0)
   (presentation-translators-cache
    :writer (setf presentation-translators-cache)
    :initform (make-hash-table :test #'equal))))

(defun invalidate-translator-caches ()
  (incf *current-translator-cache-generation*))

(defgeneric presentation-translators-cache (table))

(defmethod presentation-translators-cache ((table translator-table))
  (with-slots ((cache presentation-translators-cache)
               (generation translator-cache-generation))
      table
    (unless (or (= generation *current-translator-cache-generation*)
                (zerop (hash-table-size cache)))
      (clrhash cache))
    (setf generation *current-translator-cache-generation*)
    cache))

(defun default-translator-tester (object-arg &key &allow-other-keys)
  (declare (ignore object-arg))
  t)

(defun add-translator (table translator)
  ;; Remove old one.
  (when-let ((old (gethash (name translator) (translators table))))
    (alexandria:removef (gethash (presentation-type-name (from-type old))
                                 (simple-type-translators table))
                        old))
  (invalidate-translator-caches)
  (setf (gethash (name translator) (translators table)) translator)
  (push translator (gethash (from-type translator)
                            (simple-type-translators table)))
  translator)

(defun remove-translator (table translator)
  (alexandria:removef (gethash (presentation-type-name (from-type translator))
                               (simple-type-translators table))
                      translator)
  (remhash (name translator) (translators table))
  (invalidate-translator-caches)
  translator)

(defun make-translator-fun (args body)
  (cond ((null args)
         (warn "OBJECT parameter is obligatory (adding ignored parameter)")
         (let ((object-arg (gensym "OBJECT-ARG")))
           `(lambda (,object-arg &key &allow-other-keys)
              (declare (ignore ,object-arg))
              ,@body)))
        (t
         `(lambda (,(car args) &key ,@(cdr args) &allow-other-keys)
            (declare (ignorable ,(car args)))
            ,@body))))

(defun make-documentation-fun (doc-arg)
  (cond ((and doc-arg (symbolp doc-arg))
         doc-arg)
        ((consp doc-arg)
         (make-translator-fun (car doc-arg) (cdr doc-arg)))
        ((stringp doc-arg)
         `(lambda (object &key stream &allow-other-keys)
            (declare (ignore object))
            (write-string ,doc-arg stream)))
        ((null doc-arg)
         `(lambda (object &key presentation stream &allow-other-keys)
            (present object (presentation-type presentation)
                     :stream stream :sensitive nil)))
        (t (error "Can't handle doc-arg ~S" doc-arg))))

(defmacro define-presentation-translator
    (name (from-type to-type command-table &rest translator-options &key
           (gesture :select)
           (tester 'default-translator-tester testerp)
           (tester-definitive (if testerp nil t))
           (documentation nil documentationp)
           (pointer-documentation nil pointer-documentation-p)
           (menu t)
           (priority 0)
           (translator-class 'presentation-translator)
           &allow-other-keys)
     arglist
     &body body)
  ;; null tester should be the same as no tester
  (unless tester
    (setq tester 'default-translator-tester)
    (setq tester-definitive t))
  (let* ((real-from-type (expand-presentation-type-abbreviation from-type))
         (real-to-type (expand-presentation-type-abbreviation to-type)))
    ;; I did not investigate yet to decide what is the right thing. Let's issue
    ;; a warning for a time being. See #778. -- jd 2019-06-11
    (cond ((not (ignore-errors (get-ptype-metaclass real-from-type)))
           (alexandria:simple-style-warning "~s does not have a presentation metaclass. Translator may not work." real-from-type))
          #+ (or)
          ((not (ignore-errors (get-ptype-metaclass real-to-type)))
           (alexandria:simple-style-warning "~s does not have a presentation metaclass. Translator may not work." real-to-type)))
    (with-keywords-removed (translator-options
                            (:gesture :tester :tester-definitive :documentation
                             :pointer-documentation :menu :priority
                             :translator-class))
      `(add-translator (presentation-translators (find-command-table ',command-table))
                       (make-instance
                        ',translator-class
                        :name ',name
                        :from-type ',real-from-type
                        :to-type ',real-to-type
                        :gesture ,(if (eq gesture t)
                                      t
                                      `(gethash ',gesture *gesture-names*))
                        :tester ,(if (symbolp tester)
                                     `',tester
                                     `#',(make-translator-fun (car tester)
                                                              (cdr tester)))
                        :tester-definitive ',tester-definitive
                        :documentation #',(make-documentation-fun
                                           (if documentationp
                                               documentation
                                               (command-name-from-symbol
                                                name)))
                        ,@(when pointer-documentation-p
                            `(:pointer-documentation
                              #',(make-documentation-fun
                                  pointer-documentation)))
                        :menu ',menu
                        :priority ,priority
                        :translator-function #',(make-translator-fun arglist body)
                        ,@translator-options)))))

(defmacro define-presentation-action
    (name (from-type to-type command-table &key
           (gesture :select)
           (tester 'default-translator-tester)
           (documentation nil documentationp)
           (pointer-documentation nil pointer-documentation-p)
           (menu t)
           (priority 0))
     arglist
     &body body)
  (let* ((real-from-type (expand-presentation-type-abbreviation from-type))
         (real-to-type (expand-presentation-type-abbreviation to-type)))
    `(add-translator (presentation-translators (find-command-table ',command-table))
      (make-instance
       'presentation-action
       :name ',name
       :from-type ',real-from-type
       :to-type ',real-to-type
       :gesture ,(if (eq gesture t)
                     t
                     `(gethash ',gesture *gesture-names*))
       :tester ,(if (symbolp tester)
                    `',tester
                    `#',(make-translator-fun (car tester)
                                             (cdr tester)))
       :tester-definitive t
       :documentation #',(make-documentation-fun (if documentationp
                                                     documentation
                                                     (command-name-from-symbol
                                                      name)))
       ,@(when pointer-documentation-p
               `(:pointer-documentation
                 #',(make-documentation-fun pointer-documentation)))
       :menu ',menu
       :priority ,priority
       :translator-function #',(make-translator-fun arglist body)))))

(defmacro define-presentation-to-command-translator
    (name (from-type command-name command-table &key
           (gesture :select)
           (tester 'default-translator-tester)
           (documentation nil documentationp)
           (pointer-documentation (command-name-from-symbol command-name))
           (menu t)
           (priority 0)
           (echo t))
     arglist
     &body body)
  (let ((command-args (gensym "COMMAND-ARGS")))
    `(define-presentation-translator ,name
         (,from-type (command :command-table ,command-table) ,command-table
                     :gesture ,gesture
                     :tester ,tester
                     :tester-definitive t
                     ,@(and documentationp `(:documentation ,documentation))
                     :pointer-documentation ,pointer-documentation
                     :menu ,menu
                     :priority ,priority
                     :translator-class presentation-command-translator
                     :command-name ',command-name)
       ,arglist
       (let ((,command-args (let () ,@body)))
         (values (cons ',command-name ,command-args)
                 '(command :command-table ,command-table)
                 '(:echo ,echo))))))

(defmacro define-selection-translator
    (name (from-type to-type command-table &rest args &key &allow-other-keys)
     arglist &body body)
  (let* ((forbidden-args '(context-type frame event window x y))
         (intersection (intersection arglist forbidden-args :test #'string=)))
    (unless (null intersection)
      (error "Selection translator ~s arglist can't have args ~a but has ~a."
             name forbidden-args intersection)))
  (with-keywords-removed (args (:translator-class :tester-definitive :gesture))
    `(define-presentation-translator ,name
         (,from-type ,to-type ,command-table
                     :gesture :select
                     :tester-definitive t
                     :translator-class selection-translator
                     ,@args)
         ,arglist
       ,@body)))


;;; 23.7.2 Presentation Translator Functions

;;; This is to implement the requirement on presentation translators
;;; for doing subtype calculations without reference to type
;;; parameters.  We are generous in that we return T when we are
;;; unsure, to give translator testers a chance to accept or reject
;;; the translator.  This is essentially
;;;   (multiple-value-bind (yesp surep)
;;;       (presentation-subtypep maybe-subtype type)
;;;     (or yesp (not surep)))
;;; except faster.
(defun stupid-subtypep (maybe-subtype type)
  "Return t if maybe-subtype is a presentation subtype of type, regardless of
  parameters."
  (when (or (eq maybe-subtype nil) (eq type t))
    (return-from stupid-subtypep t))
  (when (eql maybe-subtype type)
    (return-from stupid-subtypep t))
  (let ((maybe-subtype-name (presentation-type-name maybe-subtype))
        (type-name (presentation-type-name type)))
    (cond
      ;; see DEFUN PRESENTATION-SUBTYPEP for some caveats
      ((eq maybe-subtype-name 'or)
       (let ((or-types (decode-parameters maybe-subtype)))
         (every (lambda (x) (stupid-subtypep x type)) or-types)))
      ((eq type-name 'and)
       (stupid-subtypep maybe-subtype (car (decode-parameters type))))
      ((eq type-name 'or)
       (let ((or-types (decode-parameters type)))
         (some (lambda (x) (stupid-subtypep maybe-subtype x)) or-types)))
      ((eq maybe-subtype-name 'and)
       ;; this clause is actually not conservative, but probably in a
       ;; way that no-one will complain about too much.  Basically, we
       ;; will only return T if the first type in the AND (which is
       ;; treated specially by CLIM) is subtypep the maybe-supertype
       (stupid-subtypep (car (decode-parameters maybe-subtype)) type))
      (t
       (let ((subtype-meta (get-ptype-metaclass maybe-subtype-name))
             (type-meta (get-ptype-metaclass type-name)))
         (unless (and subtype-meta type-meta)
           (return-from stupid-subtypep nil))
         (map-over-ptype-superclasses #'(lambda (super)
                                          (when (eq type-meta super)
                                            (return-from stupid-subtypep t)))
                                      maybe-subtype-name)
         nil)))))

(defun find-presentation-translators (from-type to-type command-table)
  (let* ((command-table (find-command-table command-table))
         (from-name (presentation-type-name from-type))
         (to-name (presentation-type-name to-type))
         (cache-key (cons from-name to-name))
         (cache-table (presentation-translators-cache
                       (presentation-translators command-table)))
         (cached-translators (gethash cache-key cache-table)))
    (when cached-translators
      (return-from find-presentation-translators cached-translators))
    (let ((translator-vector (make-array 8 :adjustable t :fill-pointer 0))
          (table-counter 0))
      (do-command-table-inheritance (table command-table)
        (let ((translator-map (simple-type-translators
                               (presentation-translators table))))
          (flet ((get-translators (super)
                   (loop
                      for translator in (gethash (type-name super) translator-map)
                      if (stupid-subtypep (to-type translator) to-type)
                      do (vector-push-extend (cons translator table-counter)
                                             translator-vector))))
            (map-over-ptype-superclasses #'get-translators from-name)))
        (incf table-counter))
      (let ((from-super-names nil))
        (map-over-ptype-superclasses #'(lambda (super)
                                         (push (type-name super)
                                               from-super-names))
                                     from-name)
        (setq from-super-names (nreverse from-super-names))
        ;; The Spec mentions "high order priority" and "low order priority"
        ;; without saying what that is!  Fortunately, the Franz CLIM user guide
        ;; says that high order priority is (floor priority 10), low order
        ;; priority is (mod priority 10) That's pretty wacked...
        (flet ((translator-lessp (a b)
                 (nest
                  (destructuring-bind (translator-a . table-num-a) a)
                  (destructuring-bind (translator-b . table-num-b) b)
                  (multiple-value-bind (hi-a low-a) (floor (priority translator-a) 10))
                  (multiple-value-bind (hi-b low-b) (floor (priority translator-b) 10))
                  (let* ((a-name (presentation-type-name (from-type translator-a)))
                         (b-name (presentation-type-name (from-type translator-b)))
                         (a-precedence (position a-name from-super-names))
                         (b-precedence (position b-name from-super-names)))
                    (cond
                      ;; 1. High order priority
                      ((> hi-a hi-b) (return-from translator-lessp t))
                      ((< hi-a hi-b) (return-from translator-lessp nil))
                      ;; 2. More specific "from type"
                      ((< a-precedence b-precedence) (return-from translator-lessp t))
                      ((> a-precedence b-precedence) (return-from translator-lessp nil))
                      ;; 3. Low order priority
                      ((> low-a low-b) (return-from translator-lessp t))
                      ((< low-a low-b) (return-from translator-lessp nil))
                      ;; 4. Command table inheritance
                      (t (< table-num-a table-num-b)))))))
          ;; Add translators to their caches.
          (let ((translators (map 'list #'car (sort translator-vector #'translator-lessp))))
            (setf (gethash cache-key cache-table)
                  (remove-duplicates translators))))))))

;;; :button is a pointer button state, for performing matches where we want to
;;; restrict the match to certain gestures but don't have a real event.

(defun test-presentation-translator
    (translator presentation context-type frame window x y
     &key event (modifier-state 0) for-menu button
     &aux (from-type (from-type translator)))
  (flet ((match-gesture (gesture event modifier-state)
           (or (eq gesture t)
               for-menu
               (loop
                  with modifiers = (if event
                                       (event-modifier-state event)
                                       modifier-state)
                  for g in gesture
                  thereis (and (eql modifiers (caddr g))
                               (or (and button (eql button (cadr g)))
                                   (and (null button)
                                        (or (null event)
                                            (eql (pointer-event-button
                                                  event)
                                                 (cadr g))))))))))
    (and (match-gesture (gesture translator) event modifier-state)
         (or (null (decode-parameters from-type))
             (presentation-typep (presentation-object presentation) from-type))
         (or (null (tester translator))
             (funcall (tester translator) (presentation-object presentation)
                      :presentation presentation :context-type context-type
                      :frame frame :window window :x x :y y :event event))
         (or (tester-definitive translator)
             (null (decode-parameters context-type))
             (presentation-typep
              (call-presentation-translator translator presentation context-type
                                            frame event window x y)
              context-type))
         t)))

(defun map-applicable-translators (func presentation input-context frame window x y
                                   &key event (modifier-state 0) for-menu button)
  (labels ((process-presentation (context presentation)
             (let* ((context-ptype (first context))
                    (maybe-translators
                     (find-presentation-translators (presentation-type presentation)
                                                    context-ptype
                                                    (frame-command-table frame))))
               (loop for translator in maybe-translators
                  when (and (or (not for-menu) (eql for-menu (menu translator)))
                            (test-presentation-translator
                             translator presentation context-ptype
                             frame window x y
                             :event event :modifier-state modifier-state
                             :for-menu for-menu :button button))
                  do (funcall func translator presentation context))))
           (mopscp (context record)
             "maps recursively over all presentations in record, including record."
             (if (and x y)
                 (map-over-output-records-containing-position
                  (curry #'mopscp context) record x y)
                 (map-over-output-records (curry #'mopscp context) record))
             ;; presentation-contains-position is in presentation-defs.lisp
             (when (and (presentationp record)
                        (or (and (null x) (null y)) ; allow wildcards
                            (presentation-contains-position record x y)))
               (process-presentation context record))))
    (if (and (presentationp presentation)
             (presentation-subtypep (presentation-type presentation) 'blank-area))
        (loop
           for context in input-context
           do (process-presentation context presentation))
        (loop
           for context in input-context
           do (mopscp context presentation)))))

(defun window-modifier-state (window)
  "Provides default modifier state for presentation translator functions."
  ;; There are many things which may go wrong (NULL window, sheet
  ;; without a port etc). We clamp errors to 0 meaning "no modifiers".
  (or (ignore-errors (pointer-modifier-state (port-pointer (port window)))) 0))

(defun find-applicable-translators
    (presentation input-context frame window x y
     &key event (modifier-state (window-modifier-state window)) for-menu fastp)
  (let ((results nil))
    (flet ((fast-func (translator presentation context)
             (declare (ignore translator presentation context))
             (return-from find-applicable-translators t))
           (slow-func (translator presentation context)
             (push (list translator presentation (input-context-type context))
                   results)))
      (map-applicable-translators (if fastp #'fast-func #'slow-func)
                                  presentation input-context frame window x y
                                  :event event
                                  :modifier-state modifier-state
                                  :for-menu for-menu)
      (nreverse results))))

(defun presentation-matches-context-type
    (presentation context-type frame window x y &key event (modifier-state 0))
  (let* ((ptype (expand-presentation-type-abbreviation (presentation-type presentation)))
         (ctype (expand-presentation-type-abbreviation context-type))
         (table (frame-command-table frame)))
    (and (some (lambda (translator)
                 (test-presentation-translator translator presentation ctype
                                               frame window x y
                                               :event event
                                               :modifier-state modifier-state))
               (find-presentation-translators ptype ctype table))
         t)))

(defgeneric call-presentation-translator
    (translator presentation context-type frame event window x y)
  (:method ((translator presentation-translator) presentation context-type
            frame event window x y)
    ;; Let the translator return an explict ptype of nil to, in effect, abort the
    ;; presentation throw.
    (multiple-value-call
        #'(lambda (object &optional (ptype context-type) options)
            (values object ptype options))
      (funcall (translator-function translator) (presentation-object presentation)
               :presentation presentation :context-type context-type
               :frame frame :event event :window window :x x :y y)))
  (:method ((translator presentation-action) presentation context-type
            frame event window x y)
    (funcall (translator-function translator) (presentation-object presentation)
             :presentation presentation :context-type context-type
             :frame frame :event event :window window :x x :y y)
    (values nil nil nil)))

(defun document-presentation-translator (translator
                                         presentation
                                         context-type
                                         frame
                                         event
                                         window
                                         x y
                                         &key (stream *standard-output*)
                                         (documentation-type :normal))
  (funcall (if (eq documentation-type :normal)
               (translator-documentation translator)
               (pointer-documentation translator))
           (presentation-object presentation)
           :presentation presentation
           :context-type context-type
           :frame frame
           :event event
           :window window
           :x x
           :y y
           :stream stream))

(defstruct presentation-translator-menu-item
  translator
  presentation
  context)

(defun call-presentation-menu (presentation input-context frame window x y
                               &key (for-menu t) label
                               &aux (items nil) (processed nil))
  (map-applicable-translators
   #'(lambda (translator presentation context
              &aux (key (cons translator presentation)))
       (unless (member key processed :test #'equal)
         (push key processed)
         (push
          `(,(make-presentation-translator-menu-item :translator translator
                                                     :presentation presentation
                                                     :context context)
             :documentation ,(with-output-to-string (stream)
                               (document-presentation-translator
                                translator
                                presentation
                                input-context
                                frame nil window x y
                                :stream stream)))
          items)))
   presentation input-context frame window x y :for-menu for-menu)
  (unless items
    (return-from call-presentation-menu))
  (setq items (nreverse items))
  (multiple-value-bind (item object event)
      (menu-choose
       items
       :label label
       :associated-window window
       :printer #'(lambda (item stream)
                    (let ((object (first item)))
                      (document-presentation-translator
                       (presentation-translator-menu-item-translator object)
                       (presentation-translator-menu-item-presentation object)
                       (presentation-translator-menu-item-context object)
                       frame nil window x y
                       :stream stream)))
       :label label
       :pointer-documentation *pointer-documentation-output*)
    (declare (ignore object))
    (when item
      (multiple-value-bind (object ptype options)
          (call-presentation-translator
           (presentation-translator-menu-item-translator item)
           (presentation-translator-menu-item-presentation item)
           (input-context-type (presentation-translator-menu-item-context item))
           frame event window x y)
        (when ptype
          (funcall (cdr (presentation-translator-menu-item-context item))
                   object ptype event options))))))



;;; 23.7.3 Finding Applicable Presentations

(defun find-innermost-presentation-match
    (input-context top-record frame window x y event modifier-state button)
  "Helper function that implements the \"innermost-smallest\" input-context
  presentation matching algorithm.  Returns presentation, translator, and
  matching input context."
  (let ((result nil)
        (result-translator nil)
        (result-context nil)
        (result-size nil))
    (map-applicable-translators
     #'(lambda (translator presentation context)
         (if (and result-context (not (eq result-context context)))
             ;; Return inner presentation
             (return-from find-innermost-presentation-match
               (values result result-translator result-context))
             (multiple-value-bind (min-x min-y max-x max-y)
                 (output-record-hit-detection-rectangle* presentation)
               (let ((size (* (- max-x min-x) (- max-y min-y))))
                 (when (or (not result) (< size result-size))
                   (setq result presentation)
                   (setq result-translator translator)
                   (setq result-context context)
                   (setq result-size size))))))
     top-record
     input-context
     frame
     window
     x y
     :event event
     :modifier-state modifier-state
     :button button)
    (when result
      (return-from find-innermost-presentation-match
        (values result result-translator result-context)))
    (map-applicable-translators
     #'(lambda (translator presentation context)
         (return-from find-innermost-presentation-match
           (values presentation translator context)))
     *null-presentation*
     input-context
     frame
     window
     x y
     :event event
     :modifier-state modifier-state
     :button button))
  nil)

(defun find-innermost-applicable-presentation
    (input-context window x y
     &key (frame *application-frame*)
     (modifier-state (window-modifier-state window))
     event)
  (values (find-innermost-presentation-match input-context
                                             (stream-output-history window)
                                             frame
                                             window
                                             x y
                                             event
                                             modifier-state
                                             nil)))

(defun find-innermost-presentation-context
    (input-context window x y
     &key (top-record (stream-output-history window))
     (frame *application-frame*)
     event
     (modifier-state (window-modifier-state window))
     button)
  (find-innermost-presentation-match input-context
                                     top-record
                                     frame
                                     window
                                     x y
                                     event
                                     modifier-state
                                     button))

(defun throw-highlighted-presentation (presentation input-context event)
  (let ((x (pointer-event-x event))
        (y (pointer-event-y event))
        (window (event-sheet event)))
    (multiple-value-bind (p translator context)
        (find-innermost-presentation-match input-context
                                           presentation
                                           *application-frame*
                                           (event-sheet event)
                                           x y
                                           event
                                           0
                                           nil)
      (when p
        (multiple-value-bind (object ptype options)
            (call-presentation-translator translator
                                          p
                                          (input-context-type context)
                                          *application-frame*
                                          event
                                          window
                                          x y)
          (when ptype
            (funcall (cdr context) object ptype event options)))))))

(defun throw-object-ptype (object type &key (input-context *input-context*) sheet)
  "Throw an object and presentation type within input-context without
a presentation"
  (throw-highlighted-presentation
                          (make-instance 'standard-presentation
                                         :object object :type type
                                         :single-box t)
                          input-context
                          (make-instance 'pointer-button-press-event
                                         :sheet sheet
                                         :x 0 :y 0
                                         :modifier-state 0
                                         :button +pointer-left-button+)))

(defun highlight-applicable-presentation (frame stream input-context
                                          &optional (prefer-pointer-window t))
  (let* ((queue (stream-input-buffer stream))
         (event (event-queue-peek queue))
         (sheet (and event (event-sheet event))))
    (when (and event
               (or (and (typep event 'pointer-event)
                        (or prefer-pointer-window
                            (eq stream sheet)))
                   (typep event 'keyboard-event)))
      (frame-input-context-track-pointer frame input-context sheet event)
      ;; Stream only needs to see button press events.
      ;; XXX Need to think about this more.  Should any pointer events be
      ;; passed through?  If there's no presentation, maybe?
      ;;
      ;; If we don't pass through other pointer events we let stream eat
      ;; scrolling events and similar - nacceptable. -- jd, 2017-11-26
      (when (typep event 'pointer-button-press-event)
        (event-queue-read queue)
        (funcall *pointer-button-press-handler* stream event)))))

;;; FIXME missing functions
;;;
;;; set-highlighted-presentation
;;; unhighlight-highlighted-presentation
