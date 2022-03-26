;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998-2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001-2002 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2019 by Daniel Kochmański (daniel@turtleware.eu)

;;; Implementation of drag-and-drop translators as defined in 23.7.

(in-package #:climi)

(defvar *dragged-presentation* nil
  "Bound to the presentation dragged in a drag-and-drop context")
(defvar *dragged-object* nil
  "Bound to the object dragged in a drag-and-drop context")

(defparameter *finish-on-release* t
  "Default behavior for drag-and-drop.")

(defclass drag-n-drop-translator (presentation-translator)
  ((destination-type :reader destination-type :initarg :destination-type)
   (feedback :reader feedback :initarg :feedback)
   (highlighting :reader highlighting :initarg :highlighting)
   (finish-on-release :reader finish-on-release :initarg :finish-on-release)
   (destination-tester :reader destination-tester :initarg :destination-tester)
   (destination-translator :reader destination-translator :initarg :destination-translator))
  (:default-initargs :finish-on-release *finish-on-release*
                     :destination-tester 'default-translator-tester))

(defmethod initialize-instance :after ((obj drag-n-drop-translator)
                                       &key documentation
                                         pointer-documentation
                                         destination-translator)
  (declare (ignore destination-translator))
  (setf (slot-value obj 'documentation) documentation)
  (when pointer-documentation
    (setf (slot-value obj 'pointer-documentation) pointer-documentation)))

(defmacro define-drag-and-drop-translator
    (name (from-type to-type destination-type command-table
                     &rest args &key
                     (gesture :select)
                     (tester 'default-translator-tester)
                     (destination-tester 'default-translator-tester)
                     documentation
                     (pointer-documentation nil pointer-doc-p)
                     (menu t)
                     (priority 0)
                     (feedback 'frame-drag-and-drop-feedback)
                     (highlighting 'frame-drag-and-drop-highlighting)
                     (finish-on-release *finish-on-release*)
                     (multiple-window nil))
     arglist &body body)
  (declare (ignore tester gesture documentation pointer-documentation
                   menu priority finish-on-release))
  (let* ((real-dest-type (expand-presentation-type-abbreviation destination-type))
         (name-string (command-name-from-symbol name))
         (drag-string (format nil "Drag to ~A" name-string))
         (pointer-doc (if pointer-doc-p
                          nil
                          `(:pointer-documentation
                            ((object destination-object stream)
                             (declare (ignore object))
                             (write-string (if destination-object
                                               ,name-string
                                               ,drag-string)
                                           stream))))))
    (with-keywords-removed (args (:feedback :highlighting :destination-tester))
      (with-gensyms (object)
        `(define-presentation-translator ,name
             (,from-type ,to-type ,command-table
                         :tester-definitive t
                         ,@args
                         ,@pointer-doc
                         :feedback #',feedback
                         :highlighting #',highlighting
                         :destination-type ',real-dest-type
                         :destination-translator #',(make-translator-fun arglist body)
                         :destination-tester ,(if (symbolp destination-tester)
                                                  `',destination-tester
                                                  `#',(make-translator-fun (car destination-tester)
                                                                           (cdr destination-tester)))
                         :translator-class drag-n-drop-translator)
             (,object presentation context-type frame event window x y)
           (declare (ignore ,object))
           (invoke-drag-and-drop ',name ',command-table
                                 presentation context-type
                                 frame event
                                 ,multiple-window window x y))))))

;;; It is unspecified what happens when there are multiple dnd
;;; translators which are applicable at the same time. McCLIM sets
;;; default feedback and highlight functions to those associated with
;;; the translator which got us here. When we drag over presentation
;;; which has applicable dnd translator we use its functions until the
;;; sensitive area is left. -- jd 2019-07-06
(defun invoke-drag-and-drop (translator-name command-table
                             from-presentation context-type frame event
                             multiple-window window
                             x y)
  (declare (ignore command-table))
  (let* ((*dragged-presentation* from-presentation)
         (*dragged-object* (presentation-object from-presentation))
         (translators (collect (collect-translator)
                        ;; Translator order matters.
                        (map-applicable-translators
                         (lambda (trans presentation context)
                           (declare (ignore presentation context))
                           (when (typep trans 'drag-n-drop-translator)
                             (collect-translator trans)))
                         from-presentation (list context-type) frame
                         window x y
                         :modifier-state (window-modifier-state window)
                         :event event)
                        (collect-translator)))
         ;; Default feedback and highlight functions are those of the
         ;; translator that got us here. Initial highlight value nil
         ;; will cause the source presentation of the dragged object
         ;; to be unhighlighted at the start. -- jd 2019-07-06
         (translator (find translator-name translators :key #'name))
         (finish-on-release (finish-on-release translator))
         (feedback-fn (feedback translator))
         (highlight-fn nil)
         (destination-presentation nil)
         (initial-x x)
         (initial-y y)
         (last-presentation nil)
         (last-event nil))
    (flet ((find-dest-translator (presentation event window x y)
             (loop for translator in translators
                when (and (presentation-subtypep (presentation-type presentation)
                                                 (destination-type translator))
                          (test-drag-and-drop translator presentation
                                              context-type frame event window x y))
                do (return-from find-dest-translator translator))
             nil)
           (erase-old ()
             (when last-event
               (let ((window (event-sheet last-event))
                     (x (pointer-event-x last-event))
                     (y (pointer-event-y last-event)))
                 (maybe-funcall feedback-fn frame from-presentation
                                window initial-x initial-y x y :unhighlight)
                 (when last-presentation
                   (maybe-funcall highlight-fn frame last-presentation
                                  window :unhighlight))))))
      (block do-tracking
        (tracking-pointer (window :context-type `(or ,@(mapcar #'destination-type translators))
                                  ;; context-type should be T and we
                                  ;; should do closer examination
                                  ;; inside :presentation clause
                                  :highlight nil
                                  :multiple-window multiple-window)
          (:presentation (&key presentation window event x y)
            (let ((dest-translator (find-dest-translator presentation event window x y)))
              (erase-old)
              (if dest-translator
                  (setf last-event event
                        last-presentation presentation
                        feedback-fn (feedback dest-translator)
                        highlight-fn (highlighting dest-translator))
                  (setf last-event event
                        last-presentation nil
                        feedback-fn (feedback translator)
                        highlight-fn (highlighting translator)))
              ;; Do not highlight the presentation if there is no
              ;; applicable translator. -- jd 2019-08-20
              (when dest-translator
                (maybe-funcall highlight-fn frame presentation window :highlight))
              (maybe-funcall feedback-fn frame from-presentation
                             window initial-x initial-y x y :highlight)
              (document-drag-n-drop (or dest-translator translator) last-presentation
                                    context-type frame event window x y)))
          (:pointer-motion (&key event window x y)
            (erase-old)
            (setq last-event event
                  last-presentation nil)
            (maybe-funcall feedback-fn frame from-presentation
                           window initial-x initial-y x y :highlight)
            (document-drag-n-drop translator nil
                                  context-type frame event window
                                  x y))
          (:presentation-button-press (&key presentation event)
            (unless finish-on-release
              (setq destination-presentation presentation
                    last-event event)
              (return-from do-tracking nil)))
          (:pointer-button-press ()
            (unless finish-on-release
              (setq last-event event)
              (return-from do-tracking nil)))
          (:presentation-button-release (&key presentation event)
            (when finish-on-release
              (setq destination-presentation presentation
                    last-event event)
              (return-from do-tracking nil)))
          (:pointer-button-release (&key event)
            (when finish-on-release
              (setq last-event event)
              (return-from do-tracking nil)))))
      (erase-old)
      (when-let ((stream *pointer-documentation-output*))
        (window-clear stream))
      (if-let ((final-translator (and destination-presentation
                                      (find-dest-translator destination-presentation
                                                            last-event
                                                            (event-sheet last-event)
                                                            (pointer-event-x last-event)
                                                            (pointer-event-y last-event)))))
        (funcall (destination-translator final-translator)
                 *dragged-object*
                 :presentation *dragged-presentation*
                 :destination-object (presentation-object destination-presentation)
                 :destination-presentation destination-presentation
                 :context-type context-type
                 :frame frame
                 :event event
                 :window window
                 :x x
                 :y y)
        (values nil nil)))))

(defun test-drag-and-drop
    (translator presentation context-type frame event window x y)
  (let ((function (destination-tester translator)))
    (or (null function)
        (funcall function *dragged-object*
                 :presentation *dragged-presentation*
                 :destination-object (presentation-object presentation)
                 :destination-presentation presentation
                 :context-type context-type
                 :frame frame :event event :window window :x x :y y))))

(defun document-drag-n-drop
    (translator presentation context-type frame event window x y)
  (when-let ((stream *pointer-documentation-output*))
    (let ((function (pointer-documentation translator)))
      (window-clear stream)
      (with-end-of-page-action (stream :allow)
        (with-end-of-line-action (stream :allow)
          (funcall function
                   *dragged-object*
                   :presentation *dragged-presentation*
                   :destination-object (and presentation
                                            (presentation-object presentation))
                   :destination-presentation presentation
                   :context-type context-type
                   :frame frame
                   :event event
                   :window window
                   :x x
                   :y y
                   :stream stream)))
      (force-output stream))))
