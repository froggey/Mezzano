(cl:in-package #:clim-demo)

(define-application-frame frame-sheet-name-test ()
  ()
  (:menu-bar nil)
  (:pane
   (let* ((frame *application-frame*)
          (sheet (frame-top-level-sheet frame)))
     (outlining (:thickness 10 :background +gray84+)
       (tabling (:min-width 600 :spacing 8)
         (list (labelling (:label " ")) ; work around problems with empty labels for now
               (labelling (:label "Name"))
               (labelling (:label "Pretty Name")))
         (list (labelling (:label "Sheet"))
               (make-pane :label :label (prin1-to-string (sheet-name sheet)))
               (outlining (:thickness 4 :background +white+)
                 (make-pane :text-field :name 'sheet-pretty-name
                                        :value (clime:sheet-pretty-name sheet)
                                        :value-changed-callback
                                        (lambda (gadget new-value)
                                          (declare (ignore gadget))
                                          (setf (clime:sheet-pretty-name sheet)
                                                new-value)))))
         (list (labelling (:label "Frame"))
               (make-pane :label :label (prin1-to-string (frame-name frame)))
               (outlining (:thickness 4 :background +white+)
                 (make-pane :text-field :value (frame-pretty-name frame)
                                        :value-changed-callback
                                        (lambda (gadget new-value)
                                          (declare (ignore gadget))
                                          (setf (frame-pretty-name frame)
                                                new-value)))))))))
  (:default-initargs
   :pretty-name "Frame and Sheet Name Test"))

(defmethod clime:note-frame-pretty-name-changed ((frame-manager t)
                                                 (frame frame-sheet-name-test)
                                                 (new-name t))
  (alexandria:when-let* ((pane  (find-pane-named frame 'sheet-pretty-name))
                         (sheet (frame-top-level-sheet frame)))
    (setf (gadget-value pane :invoke-callback nil)
          (clime:sheet-pretty-name sheet))))
