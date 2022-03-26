(in-package :clim-clx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLX implementation of clipboard management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utilities
(defun ensure-presentation (object type)
  (typecase object
    (presentation
     object)
    (selection-object
     (make-instance 'standard-presentation
                    :object (selection-object-content object)
                    :type type :view nil))
    (t
     (make-instance 'standard-presentation
                    :object object
                    :type type :view nil))))

(defun call-selection-translator (object selected-type requested-type table)
  (when (subtypep selected-type requested-type)
    (return-from call-selection-translator (values object selected-type)))
  (let ((pres (ensure-presentation object selected-type)))
    (when-let ((tr (find-selection-translator pres selected-type requested-type table)))
      (call-presentation-translator tr pres requested-type nil nil nil nil nil))))

(defun find-selection-translator (object selected-type requested-type table
                                  &aux (pres (ensure-presentation object selected-type)))
  (some (lambda (tr)
          (and (typep tr 'climi::selection-translator)
               (test-presentation-translator tr pres requested-type nil nil nil nil)
               tr))
        (find-presentation-translators selected-type requested-type table)))

(defun find-selection-translators (selected-type requested-type table)
  (remove-if-not (lambda (tr) (typep tr 'climi::selection-translator))
                 (find-presentation-translators selected-type requested-type table)))

;;; Port mixin

(defclass clx-selection-mixin ()
  (;; For requests which await objects from other applications:
   (outstanding-request :initform nil :accessor clipboard-outstanding-request)))

(macrolet ((frob (selection)
             `(defmethod climb:publish-selection ((port clx-selection-mixin)
                                                  (selection (eql ,selection))
                                                  (object climb:selection-object)
                                                  object-type)
                (declare (ignore port object-type))
                (let* ((window (sheet-direct-xmirror (climb:selection-object-owner object)))
                       (display (xlib:window-display window)))
                  ;; We're not actually supposed to call set-selection-owner without
                  ;; a timestamp due to the following statemnt in ICCCM:
                  ;;
                  ;;     Clients attempting to acquire a selection must set the time
                  ;;     value of the SetSelectionOwner request to the timestamp of
                  ;;     the event triggering the acquisition attempt, not to
                  ;;     CurrentTime. A zero-length append to a property is a way to
                  ;;     obtain a timestamp for this purpose; the timestamp is in
                  ;;     the corresponding PropertyNotify event.
                  ;;
                  ;; The reasons for his seems to be to ensure that the ownership is
                  ;; actually transferred correctly. This shouldn't be a major issue
                  ;; in practice, and it significantly simplifies the implementation.
                  (xlib:set-selection-owner display selection window nil)
                  (if (eq (xlib:selection-owner display selection) window)
                      (call-next-method)
                      (warn "Couldn't set X11 selection ~s owner to ~s."
                            selection window))))))
  (frob :primary)
  (frob :clipboard))

(macrolet ((frob (selection)
             `(defmethod climb:release-selection ((port clx-selection-mixin)
                                                  (selection (eql ,selection))
                                                  (object climb:selection-object))
                (alexandria:when-let*
                    ((sheet (climb:selection-object-owner object))
                     (window (sheet-direct-xmirror sheet))
                     (display (xlib:window-display window)))
                  (when (eq window (xlib:selection-owner display selection))
                    (xlib:set-selection-owner display selection nil nil)))
                (call-next-method))))
  (frob :primary)
  (frob :clipboard))

;;;  Paste support
;;;
;;;  When a client requests the content of the clipboard or selection, a REQUESTED-OBJECT
;;;  parameter is used to indicate what kind of of object the caller expects to
;;;  receive. Selection objects are explained in ports.lisp.
;;;
;;;  First we shortcut result if the selection owner is a sheet in the same image. Then we
;;;  try to negotiate the type – ask for :TARGETS. If response is empty we default to
;;;  :STRING. Then we verify whether we can translate proposed types to the requested one
;;;  and if it is possible we request that type with convert-selection. Otherwise NIL is
;;;  returned.

(defun clx-request-selection (port selection requested-object)
  (setf (clipboard-outstanding-request port) requested-object)
  (let ((sheet (selection-object-owner requested-object))
        (window (sheet-xmirror (selection-object-owner requested-object)))
        (to-type (selection-object-type requested-object))
        (table (climi::get-object-table requested-object)))
    (labels ((wait-for-request ()
               (flet ((wait-fn ()
                        (or (not (clipboard-outstanding-request port))
                            (not (eq selection (selection-object-content requested-object))))))
                 (declare (dynamic-extent #'wait-fn))
                 ;; event-listen-or-wait will return on any event
                 ;; becoming available in the queue, that's why we
                 ;; loop until our condition is met (or timeout).
                 (loop
                    with timeout-time = (1+ (climi::now))
                    for decay = (- timeout-time (climi::now))
                    do
                      (cond ((wait-fn)   (return t))
                            ((< decay 0) (return nil))
                            (t (event-listen-or-wait sheet
                                                     :wait-function #'wait-fn
                                                     :timeout decay))))))
             (negotiate-selection-type ()
               (xlib:convert-selection selection :targets window :mcclim nil)
               ;; Either window is closed or peer does not understand
               ;; targets. We will try to request string in that case.
               (unless (and (wait-for-request)
                            (clipboard-outstanding-request port))
                 (setf (clipboard-outstanding-request port) requested-object
                       (selection-object-content requested-object) '(:utf8_string)))
               ;; We look for the first proposed type which is suitable for the
               ;; selection translation. If not found it is NIL.
               ;;
               ;; XXX: we should collect all applicable translators and their
               ;; types and sort them by priority as they are sorted in
               ;; find-presentation-translators. -- jd 2019-06-14
               (setf (selection-object-type requested-object) nil)
               (some (lambda (from-type)
                       (when (or (and (not (eql to-type t))
                                      (subtypep from-type to-type))
                                 (find-selection-translators from-type to-type table))
                         (setf (selection-object-type requested-object) from-type)))
                     (selection-object-content requested-object))))
      (declare (dynamic-extent #'wait-for-request))
      (negotiate-selection-type)
      ;; rewind the content to selection
      (when-let ((from-type (selection-object-type requested-object)))
        (setf (selection-object-content requested-object) selection)
        (xlib:convert-selection selection from-type window :mcclim nil)
        (when-let ((result (and (wait-for-request)
                                (clipboard-outstanding-request port))))
          (call-selection-translator (selection-object-content result)
                                     (selection-object-type result)
                                     to-type
                                     table))))))

(macrolet ((frob (selection)
             `(defmethod request-selection ((port clx-selection-mixin)
                                            (selection (eql ,selection))
                                            (requested-object selection-object))
                ;; Shortcut the request if the selection is owned by a window in the
                ;; same image. This improves translations and overall responsiveness.
                (alexandria:when-let*
                    ((object (climi::stored-object port selection))
                     (sheet (climb:selection-object-owner object))
                     (window (sheet-direct-xmirror sheet))
                     (display (xlib:window-display window)))
                  (if (eq window (xlib:selection-owner display selection))
                      (return-from request-selection (call-next-method))
                      (climb:release-selection port selection nil)))
                (clx-request-selection port selection requested-object))))
  (frob :primary)
  (frob :clipboard))

;;; define basic translators

;; Usually :utf8_string target exists if the output is textual and that's what
;; we really want. If we had wanted to support clients not capable of copying
;; :utf8_string (but capble of copying :string) we could use the following
;; translators. In order to make them work though we should implement priorities
;; to prevent a situation when :string is picked instead of :utf8_string (emacs
;; then skips characters which are not encodable and some clients quote utf-16
;; characters with \uXXXX syntax. There may be more oddballs. -- jd 2019-06-14

;; (define-presentation-type clx-string    ())
;; (define-presentation-type :string       () :inherit-from 'clx-string)
;; (define-presentation-type :|text/plain| () :inherit-from 'clx-string)
;; (clime:define-selection-translator clx-string->lisp-string
;;       (clx-string string global-command-table :priority 0)
;;     (object)
;;   (let ((payload (coerce object '(vector (unsigned-byte 8)))))
;;     (babel:octets-to-string :encoding :iso-8859-1)))

(define-presentation-type clx-utf8-string             ())
(define-presentation-type :utf8_string                () :inherit-from 'clx-utf8-string)
(define-presentation-type :|text/plain;charset=utf-8| () :inherit-from 'clx-utf8-string)

(clime:define-selection-translator clx-utf8-string->lisp-string
      (clx-utf8-string string global-command-table :priority 1)
      (object)
  (let ((payload (coerce object '(vector (unsigned-byte 8)))))
    (babel:octets-to-string payload :encoding :utf-8)))

(defun process-selection-request (port window sheet target property requestor selection time
                                  &aux object from-type table)
  (declare (ignorable window sheet))
  (when (setf object (climi::stored-object port selection))
    (setf from-type (selection-object-type object)
          table (climi::get-object-table object)))
  (unless (and property object)
    ;; we can't set NIL property (this may happen with bogus peers) -- jd 2019-06-04
    (xlib:send-event requestor :selection-notify nil
                     :event-window requestor :selection selection :target target
                     :property property :time time)
    (setf (clipboard-outstanding-request port) nil)
    (return-from process-selection-request))
  (cond
    ;; We treat TARGETS specially: we look for all selection
    ;; translators from the type and return them. If the target type
    ;; is a keyword, we return it as is. Strings are treated in a
    ;; similar manner.
    ((eql target :targets)
     (let* ((to-type (loop
                        for tr in (find-selection-translators from-type t table)
                        for type = (climi::to-type tr)
                        if (keywordp type)
                        collect type
                        else if (eql type 'string)
                        collect :utf8_string))
            (targets (mapcar (alexandria:curry #'xlib:intern-atom
                                               (xlib:window-display window))
                             (cond ((keywordp from-type)
                                    (list* :targets from-type to-type))
                                   ((eql from-type 'string)
                                    (list* :targets :utf8_string to-type))
                                   (t (list* :targets to-type))))))
       (if (alexandria:length= 1 targets)
           (setf property :none)
           (xlib:change-property requestor property targets :atom 32))))
    ;; If a specific type is requested we look for a specific
    ;; translator (i.e :|text/html|).
    ((find-selection-translators from-type target table)
     (if-let ((content (call-selection-translator object from-type target table)))
       (typecase content
         (string
          (let ((content (babel:string-to-octets content :encoding :utf-8)))
            (xlib:change-property requestor property content :utf8_string 8)))
         ((vector (unsigned-byte 8))
          (xlib:change-property requestor property content target 8))
         ((vector (unsigned-byte 16))
          (xlib:change-property requestor property content target 16))
         ((vector (unsigned-byte 32))
          (xlib:change-property requestor property content target 32))
         (otherwise
          (warn "CLX backend can't handle selection of type ~s." (type-of content))
          (setf property :none)))
       (setf property :none)))
    ;; If there is no a translator specifically tailored for the request we use
    ;; string normalization tricks for known types and look if translators for
    ;; them exist. First we look for a translator then we format object
    ;; aesthetically.
    ((member target '(:utf8_string :|text/plain;charset=utf-8|))
     (let ((content (selection-object-content object)))
       (if-let ((string (or (call-selection-translator content from-type 'string table)
                            (format nil "~a" content))))
         (let ((payload (babel:string-to-octets string :encoding :utf-8)))
          (xlib:change-property requestor property payload :utf8_string 8))
         (setf property :none))))
    ((member target '(:string :|text/plain|))
     (let ((content (selection-object-content object)))
       (if-let ((string (or (call-selection-translator content from-type 'string table)
                            (format nil "~a" content))))
         (let ((payload (babel:string-to-octets string :encoding :iso-8859-1)))
           (xlib:change-property requestor property payload :string 8))
         (setf property :none))))
    ;; Finally we give up and send back nothing.
    (t
     (warn "could not find translator for -> ~s ~s" from-type target)
     (setf property :none)))
  (xlib:send-event requestor :selection-notify nil ; window, event-key, event-mask
                   ;; &rest args
                   :event-window requestor
                   :selection selection
                   :target target
                   :property property
                   :time time))

(defun process-selection-notify (port window target property selection time
                                 &aux (request (clipboard-outstanding-request port)))
  (declare (ignore time))
  (unless (and request
               property
               (eq (climb:selection-object-content request) selection)
               (not (eq property :none)))
    ;; This means that the peer can't return a requested type. This
    ;; should not happen when we negotiate types. -- jd 2019-06-05
    (setf (clipboard-outstanding-request port) nil)
    (return-from process-selection-notify))
  (if (eq target :targets)
      (setf (selection-object-content request)
            (mapcar (alexandria:curry #'xlib:atom-name (xlib:window-display window))
                    (xlib:get-property window property)))
      (setf (selection-object-content request)
            (xlib:get-property window property))))

(defun process-selection-clear (port selection)
  (climb:release-selection port selection nil))
