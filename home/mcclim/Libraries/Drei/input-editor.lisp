;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2001 by 
;;;           Tim Moore (moore@bricoworks.com)
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

;;; Implementation of various bits and parts needed for Drei to
;;; function as the input-editor of McCLIM. Meaning, this is an
;;; interface between input-editing-streams and Drei instances. We
;;; also try not to mess too much with CLIM-INTERNALS to be somewhat
;;; portable (but not too much).

(in-package :drei)

;; Note that we use `stream-scan-pointer' to access the scan pointer
;; of the stream in the protocol methods, despite the fact that the
;; `drei-input-editing-mixin' class does not have a scan pointer. We
;; assume that the subclass defines a scan pointer.
(defclass drei-input-editing-mixin ()
  ((%drei-instance :accessor drei-instance-of
                   :initarg :drei-instance)
   (%input-position :accessor input-position
                    :initform 0)
   (%activation-gesture :accessor activation-gesture
                        :initform nil)
   (%rescanning-p :reader stream-rescanning-p
                  :writer (setf stream-rescanning)
                  :initform nil)
   (%input-buffer-array :accessor input-buffer-array
                        :initform nil
                        :documentation "After a command has been
executed, the contents of the Drei area instance shall be
replaced by the contents of this array, if non-NIL."))
  (:documentation "An mixin that helps in implementing Drei-based
input-editing streams. This class should not be directly
instantiated."))

(defmethod initialize-instance :after ((obj drei-input-editing-mixin)
				       &rest args
				       &key stream
				       (cursor-visibility t)
                                       (min-width 0))
  (declare (ignore min-width))
  ;;(check-type min-width (or (integer 0) (eql t)))
  (check-type stream clim-stream-pane)
  (multiple-value-bind (cx cy)
      (stream-cursor-position stream)
    (let ((max-width (- (stream-text-margin stream) cx)))
      (with-keywords-removed (args (:initial-contents))
	(setf (drei-instance obj)
	      (apply #'make-instance
		     'drei-area
		     :editor-pane stream
		     :x-position cx
		     :y-position cy
		     :active cursor-visibility
		     :max-width max-width
                     :allow-other-keys t
		     args)))
      ;; XXX Really add it here?
      (stream-add-output-record stream (drei-instance obj)))))

(defmethod stream-default-view ((stream drei-input-editing-mixin))
  (view (drei-instance stream)))

(defmethod stream-insertion-pointer
    ((stream drei-input-editing-mixin))
  (offset (point (view (drei-instance stream)))))

(defmethod (setf stream-insertion-pointer)
    ((new-value integer) (stream drei-input-editing-mixin))
  (setf (offset (point (view (drei-instance stream)))) new-value))

(defmethod cursor-visibility ((stream drei-input-editing-mixin))
  (if (point-cursor (drei-instance stream))
      (active (point-cursor (drei-instance stream)))
      ;; Uh... no I guess?
      nil))

(defmethod (setf cursor-visibility)
    (visibility (stream drei-input-editing-mixin))
  (setf (active (drei-instance stream)) visibility
        (cursors-visible (drei-instance stream)) visibility))

(defclass drei-unselectable-presentation (presentation)
  ()
  (:documentation "A presentation that will not be highlightable,
and can thus be safely used for implementing stuff such as noise
strings."))

(define-presentation-translator unselectable-presentation-to-nothing
    (drei-unselectable-presentation t global-command-table
                                    :menu nil
                                    :tester ((object)
                                             (declare (ignore object))))
    (object)
  (declare (ignore object)))

(defclass noise-string (drei-unselectable-presentation)
  ((%string :initarg :string
            :initform (error "A noise string must represent some string.")
            :reader noisy-string)
   (%text-style :initarg :text-style
                :initform (make-text-style :serif :italic nil)
                :reader text-style))
  (:documentation "Buffer objects of this class will be skipped
by the input-editor gesture reader. They should not be used
outside the input editor."))

(define-presentation-method present ((object noise-string) (type noise-string)
                                     stream (view textual-view) &key &allow-other-keys)
  (with-text-style (stream (text-style object))
    (princ (noisy-string object) stream)))

(defclass accept-result (presentation)
  ((%object :initarg :object
            :initform (error "An object must be provided for the accept result.")
            :reader object)
   (%result-type :initarg :result-type
                 :initform t
                 :reader result-type))
  (:documentation "Buffer objects of this class are inserted into
the buffer when the user clicks on an applicable presentation
while in an input context for the input-editor. They should not
be used outside the input-editor."))

(define-presentation-method present (object (type accept-result) stream
                                            (view textual-view) &rest args
                                            &key)
  (apply #'present (object object) (result-type object) :stream stream :view view args))

(defmethod prompt-for-accept :around ((stream drei-input-editing-mixin) type view
                                      &rest args &key &allow-other-keys)
  (declare (ignore args))
  ;; XXX: In Drei, the "input position" (a lovably underspecified
  ;; concept in the CLIM spec) is just after any input prompt. We do
  ;; not set the input position (or print the prompt) if we are
  ;; already at the input position or if we are rescanning. This is so
  ;; we can support fancy accept methods such as the one for
  ;; `command-or-form'
  (unless (stream-rescanning-p stream)
    ;; Put the prompt in the proper place, but be super careful not to
    ;; mess with the insertion pointer.
    (let ((ip-clone (clone-mark (point (view (drei-instance stream))))))
      (unwind-protect (progn (setf (stream-insertion-pointer stream)
                                   (stream-scan-pointer stream))
                             (call-next-method))
        (setf (stream-insertion-pointer stream) (offset ip-clone)))
      (redraw-input-buffer stream)))
  ;; We skip ahead of any noise strings to put us past the
  ;; prompt. This is safe, because the noise strings are to be
  ;; ignored anyway, but we need to be ahead to set the input
  ;; position properly (ie. after the prompt).
  (loop with buffer = (buffer (view (drei-instance stream)))
        until (>= (stream-scan-pointer stream) (size buffer))
        while (or (typep #1=(buffer-object buffer (stream-scan-pointer stream)) 'noise-string)
                  (delimiter-gesture-p #1#))
        do (incf (stream-scan-pointer stream)))
  (setf (input-position stream) (stream-scan-pointer stream)))

(defmethod stream-accept :after ((stream drei-input-editing-mixin) type &key &allow-other-keys)
  ;; If we end up asking for more input using the stream, we do not
  ;; want to permit the user to undo input for this context.
  (clear-undo-history (buffer (view (drei-instance stream)))))

(defun buffer-array-mismatch (sequence1 sequence2 
                              &key (from-end nil)
                              (start1 0) 
                              (start2 0))
  "Like `cl:mismatch', but supporting fewer keyword arguments,
and the two sequences can be Drei buffers instead."
  (flet ((seq-elt (seq i)
           (typecase seq
             (drei-buffer (buffer-object seq i))
             (array (aref seq i))))
         (seq-length (seq)
           (typecase seq
             (drei-buffer (size seq))
             (array (length seq)))))
    (if from-end
        (loop
           for index1 downfrom (1- (seq-length sequence1)) to 0
           for index2 downfrom (1- (seq-length sequence2)) to 0
           unless (= index1 index2 0)
           if (or (= index1 0)
                  (= index2 0))
           return index1
           unless (eql (seq-elt sequence1 index1)
                       (seq-elt sequence2 index2))
           return (1+ index1))

        (do* ((i1 start1 (1+ i1))
              (i2 start2 (1+ i2))
              x1 x2)
             ((and (>= i1 (seq-length sequence1))
                   (>= i2 (seq-length sequence2))) nil)
          (if (>= i1 (seq-length sequence1)) (return i1))
          (if (>= i2 (seq-length sequence2)) (return i1))
          (setq x1 (seq-elt sequence1 i1))
          (setq x2 (seq-elt sequence2 i2))
          (unless (eql x1 x2)
            (return i1))))))

(defun synchronize-drei-buffer (stream)
  "If the `input-buffer-array' of `stream' is non-NIL, copy the
contents of the array to the Drei buffer. This will set the
contents of the buffer to the contents of the array up to the
fill pointer."
  (with-accessors ((array input-buffer-array)) stream
    (let ((buffer (buffer (view (drei-instance stream)))))
      (when array
        ;; Attempt to minimise the changes to the buffer, so the
        ;; position of marks will not be changed too much. Find the
        ;; first mismatch between buffer contents and array contents.
        (multiple-value-bind (index buffer-end array-end)
            (let* ((buffer-array-mismatch-begin (or (buffer-array-mismatch
                                                     buffer array)
                                                    0))
                   (buffer-buffer-array-mismatch-end (or (buffer-array-mismatch
                                                          buffer array :from-end t
                                                          :start2 buffer-array-mismatch-begin)
                                                         buffer-array-mismatch-begin))
                   (array-buffer-array-mismatch-end (or (buffer-array-mismatch
                                                         array buffer :from-end t
                                                         :start2 buffer-array-mismatch-begin)
                                                        buffer-array-mismatch-begin)))
              (values buffer-array-mismatch-begin
                      (max buffer-buffer-array-mismatch-end buffer-array-mismatch-begin)
                      (max array-buffer-array-mismatch-end buffer-array-mismatch-begin)))
          (let ((insertion-pointer (stream-insertion-pointer stream)))
            (when index       ; NIL if buffer and array are identical.
              ;; Delete from the first mismatch to the end of the
              ;; mismatch.
              (delete-buffer-range buffer index (- buffer-end index))
              ;; Also delete from the end of the buffer if the array
              ;; is smaller than the buffer.
              (when (> (size buffer) (length array))
                (delete-buffer-range buffer (length array)
                                     (- (size buffer)
                                        (length array))))
              ;; Insert from the mismatch to end mismatch from the
              ;; array into the buffer.
              (insert-buffer-sequence buffer index (subseq array index array-end))
              ;; Finally, see if it is possible to maintain the old
              ;; position of the insertion pointer.
              (setf (stream-insertion-pointer stream)
                    (min insertion-pointer (size buffer))))))))))

(defun synchronize-input-buffer-array (stream)
  "If the `input-buffer-array' of `stream' is non-NIL, copy the
contents of the Drei buffer to the array. The fill pointer of the
array will point to after the last element."
  (with-accessors ((array input-buffer-array)) stream
    (let ((buffer (buffer (view (drei-instance stream)))))
      (when array
        (let ((new-array (buffer-sequence buffer 0 (size buffer))))
          (setf array
                ;; We probably lose if `adjust-array' doesn't
                ;; destructively modify `array.
                (adjust-array array (length new-array)
                              :initial-contents new-array
                              :fill-pointer (length new-array))))))))

(defun update-drei-buffer (stream)
  "Update the Drei buffer of the Drei instance used by `stream'
if the `input-buffer-array' of `stream' is non-NIl. This will set
the contents of the buffer to the contents of the array up to the
fill pointer. Changes to the buffer will be recordes as
undoable. When this function returns, the `input-buffer-array' of
`stream' will be NIL. Also, the syntax will be up-to-date."
  (with-undo ((list (buffer (view (drei-instance stream)))))
    (synchronize-drei-buffer stream))
  (setf (input-buffer-array stream) nil))

;; While the CLIM spec says that user-commands are not allowed to do
;; much with the input buffer, the Franz User Guide provides some
;; examples that hint to the opposite. How do we make modifications of
;; the input-buffer, which must be a standard array with a fill
;; pointer, to be applied to the "real" buffer? This is how: when this
;; method is called, we store the object in the stream object. In the
;; command loop, we check the stream object and update the buffer
;; (using `update-drei-buffer') to reflect the changes done to the
;; buffer.
(defmethod stream-input-buffer ((stream drei-input-editing-mixin))
  ;; NOTE: This is very slow (consing up a whole new array - twice!),
  ;; please do not use it unless you want to be compatible with other
  ;; editor substrates. Use the Drei buffer directly instead.
  (unless (input-buffer-array stream)
    ;; Create dummy array and synchronize it to the buffer contents.
    (setf (input-buffer-array stream) (make-array 0 :fill-pointer 0))
    (synchronize-input-buffer-array stream))
  (input-buffer-array stream))

(defmethod replace-input ((stream drei-input-editing-mixin) (new-input array)
			  &key
			  (start 0)
			  (end (length new-input))
			  (buffer-start (input-position stream))
			  (rescan nil rescan-supplied-p))
  (check-type start integer)
  (check-type end integer)
  (check-type buffer-start integer)
  ;; Since this is a CLIM-specified function, we have to make sure the
  ;; input-buffer-array is taken into consideration, because some
  ;; input-editor-command might call this function and expect the
  ;; changes to be reflected in the array it holds. Also, if changes
  ;; have been made to the array, they need to be propagated to the
  ;; buffer before we do anything.
  (synchronize-drei-buffer stream)
  (let* ((drei (drei-instance stream))
         (view (view drei))
         (new-contents (subseq new-input start end))
         (old-contents (buffer-sequence (buffer view)
                                        buffer-start
                                        (stream-scan-pointer stream)))
         (equal (and (= (length new-contents)
                        (length old-contents))
                     (every #'equal new-contents old-contents))))
    (let ((begin-mark (clone-mark (point view))))
      (unless equal
        (setf (offset begin-mark) buffer-start)
        (delete-region begin-mark (stream-scan-pointer stream))
        (insert-sequence begin-mark new-contents)
        ;; Make the buffer reflect the changes in the array.
        (synchronize-input-buffer-array stream))
      (display-drei drei)
      ;; XXX: This behavior for the :rescan parameter is not mentioned
      ;; explicitly in any CLIM guide, but McCLIM input-editing
      ;; machinery relies on it.
      (if (and (or rescan (not equal))
               (not (and (null rescan) rescan-supplied-p)))
          (queue-rescan stream)
          (incf (stream-scan-pointer stream) (- (length new-contents)
                                                (length old-contents))))
      ;; We have to return "the position in the input buffer". We
      ;; return the insertion position.
      (stream-insertion-pointer stream))))

(defun present-acceptably-to-string (object type view for-context-type)
  "Return two values - a string containing the printed
representation of `object' when presented with `type' and `view',
and an object. The second value will be NIL if the string is
\"acceptable\", that is, acceptable as input to the accept method
for `type', or `object' if it isn't."
  (flet ((present-it (acceptably)
	   (present-to-string object type
			      :view view
			      :acceptably acceptably
			      :for-context-type for-context-type)))
    (let* ((acceptably t)
	   (printed-rep nil))
      (handler-case
	  (setq printed-rep (present-it t))
	(error ()
	  (setq acceptably nil)
	  (setq printed-rep (present-it nil))))
      (values printed-rep (if acceptably
			      nil
			      object)))))

(defmethod presentation-replace-input
    ((stream drei-input-editing-mixin) object type view
     &rest args &key (buffer-start (input-position stream))
     rescan query-identifier (for-context-type type) (accept-result t))
  (declare (ignore query-identifier buffer-start rescan))
  ;; If the input is non-readable and `accept-result' is non-NIL, we
  ;; insert an `accept-result' object into the buffer, otherwise we
  ;; just insert the object itself. This is a non-specified
  ;; convenience extension (so we have to use :allow-other-keys t when
  ;; using it).
  (with-keywords-removed (args (:type :view :query-identifier :for-context-type))
    (multiple-value-bind (printed-rep accept-object)
        (present-acceptably-to-string object type view for-context-type)
      (apply #'replace-input stream
             (if accept-object
                 (vector (if accept-result
                             (make-instance 'accept-result
                                            :object accept-object
                                            :result-type type)
                             accept-object))
                 printed-rep)
             args))))

;; The purpose of this method is to ensure that things such as lists
;; should are not completely inserted as literal objects if they have
;; unreadable elements.
(defmethod presentation-replace-input
    ((stream drei-input-editing-mixin) object (type (eql 'expression)) view
     &rest args &key
     (buffer-start (input-position stream)) rescan
     query-identifier (for-context-type type))
  (declare (ignore query-identifier rescan for-context-type buffer-start))
  ;; Build up an array, `insertion', and use `replace-input' to insert
  ;; it.
  (let ((insertion (make-array 10 :adjustable t :fill-pointer 0)))
    (labels ((insert-object (object)
               (vector-push-extend object insertion))
             (insert-objects (objects)
               (setf insertion (adjust-array insertion
                                             (+ (length insertion)
                                                (length objects))
                                             :fill-pointer (+ (fill-pointer insertion)
                                                              (length objects))))
               (setf (subseq insertion (- (fill-pointer insertion)
                                          (length objects))) objects))
             (insert-list-in-stream (list)
               (insert-object #\()
               (mapl #'(lambda (cons)
                         (present-object (first cons))
                         (when (rest cons)
                           (insert-object #\Space)))
                     list)
               (insert-object #\)))
             (present-object (object)
               (multiple-value-bind (printed-rep accept-object)
                   (present-acceptably-to-string object 'expression
                                                 +textual-view+ 'expression)
                 (if (null accept-object)
                     (insert-objects printed-rep)
                     (typecase object
                       (list (insert-list-in-stream object))
                       (array (insert-object #\#)
                              (insert-list-in-stream object))
                       (function (let ((name (nth-value 2 (function-lambda-expression object))))
                                   (insert-objects (or (format nil "#'~A" name)
                                                       (vector object)))))
                       ;; Okay, we give up, just insert it.
                       (t (insert-object object)))))))
      (present-object object))
    (with-keywords-removed (args (:type :view :query-identifier :for-context-type))
      (apply #'replace-input stream insertion args))))

(defmethod presentation-replace-input
    ((stream drei-input-editing-mixin) object (type (eql 'form)) view
     &rest args &key
     (buffer-start (input-position stream)) rescan
     query-identifier (for-context-type type))
  (declare (ignore query-identifier rescan for-context-type buffer-start))
  (apply #'presentation-replace-input stream object 'expression view args))

(defvar *drei-input-editing-stream* nil
  "Used to provide CLIM-specified input-editing-commands with the
input-editing-stream. Bound when executing a command.")

;;; Have to reexamine how many of the keyword arguments to
;;; stream-read-gesture should really be passed to the encapsulated
;;; stream.
;;;
;;; OK, now I know :) They should all be passed, except for peek-p.
;;; However, the loop that calls stream-read-gesture on the
;;; encapsulated stream needs to return null if we see a :timeout or
;;; :eof.
;;;
;;; Activation gesture handling has been moved out of
;;; stream-process-gesture to stream-read-gesture and
;;; stream-unread-gesture. This allows a gesture to be read in while
;;; it is not an activation gesture, unread, and then read again as an
;;; activation gesture. This kind of game seems to be needed for
;;; reading forms properly. -- moore
(defmethod stream-read-gesture ((stream drei-input-editing-mixin)
				&rest rest-args &key peek-p
				&allow-other-keys)
  (with-keywords-removed (rest-args (:peek-p))
    (rescan-if-necessary stream)
    (with-accessors ((insertion-pointer stream-insertion-pointer)
                     (scan-pointer stream-scan-pointer)
                     (activation-gesture activation-gesture)) stream
      (let ((buffer (buffer (view (drei-instance stream))))
            (last-was-noisy nil)) ; T if last passed gesture is noise-string
        (loop (loop while (< scan-pointer insertion-pointer)
                    while (< scan-pointer (size buffer))
                    do (let ((gesture (buffer-object buffer scan-pointer)))
                         ;; Skip noise strings.
                         (cond ((typep gesture 'noise-string)
                                (incf scan-pointer)
                                (setf last-was-noisy t))
                               ((and (not peek-p)
                                     (typep gesture 'accept-result))
                                (incf scan-pointer)
                                (climi::throw-object-ptype (object gesture)
                                                           (result-type gesture)))
                               ;; Note that this implies that
                               ;; `stream-read-gesture' may return accept
                               ;; results, which might as well be arbitrary
                               ;; objects to the code calling
                               ;; `stream-read-gesture', since it can't really
                               ;; do anything with them except for asserting
                               ;; that they exist. According to the spec,
                               ;; "accept results are treated as a single
                               ;; gesture", and this kind of behavior is
                               ;; necessary to make sure `stream-read-gesture'
                               ;; doesn't simply claim that there are no more
                               ;; gestures in the input-buffer when the
                               ;; remaining gesture(s) is an accept result.
                               ((typep gesture 'accept-result)
                                (return-from stream-read-gesture gesture))
                               (t
                                (unless peek-p
                                  (incf scan-pointer))
                                (return-from stream-read-gesture gesture)))))
         (unless last-was-noisy      ; This prevents double-prompting.
           (setf (stream-rescanning stream) nil))
         (when activation-gesture
           (return-from stream-read-gesture
             (prog1 activation-gesture
               (unless peek-p
                 (setf activation-gesture nil)))))
         ;; In McCLIM, stream-process-gesture is responsible for
         ;; inserting characters into the buffer, changing the
         ;; insertion pointer and possibly setting up the
         ;; activation-gesture slot.
         (loop with gesture and type
               do (setf (values gesture type)
                        (apply #'stream-read-gesture
                               (encapsulating-stream-stream stream) rest-args))
               when (null gesture)
               do (return-from stream-read-gesture (values gesture type))
               when (stream-process-gesture stream gesture type)
               do (loop-finish)))))))

(defmethod stream-unread-gesture ((stream drei-input-editing-mixin)
				  gesture)
  (with-accessors ((scan-pointer stream-scan-pointer)
                   (activation-gesture activation-gesture)) stream
    (when (> scan-pointer 0)
      (if (and (eql scan-pointer (stream-insertion-pointer stream))
               (activation-gesture-p gesture))
          (setf activation-gesture gesture)
          (decf scan-pointer)))))

(defun read-gestures-and-act (stream first-gesture type)
  "Read gestures from `stream' and act upon them as per the
semantics of `process-gesture'. This basically means that we read
gestures and process a command, returning NIL if we don't
consider it an \"editing command\", rescan if it changed
something before the scan pointer, and just return the gesture if
it inserted stuff after the scan pointer. `First-gesture' must be
the gesture that will be read in the first call to
`stream-read-gesture' for the stream encapsulated by
`stream'. The second return value of this function will be `type'
if stuff is inserted after the insertion pointer."
  (assert (<= (input-position stream) (stream-scan-pointer stream)))
  (let* ((drei (drei-instance stream))
         (buffer (buffer (view drei)))
         (*command-processor* drei)
         (was-directly-processing (directly-processing-p drei))
         (*drei-input-editing-stream* stream)
         (old-buffer-contents (buffer-sequence buffer 0 (size buffer))))
    (with-bound-drei-special-variables (drei :prompt "M-x ")
      (update-drei-buffer stream)
      ;; Since we have an unread gesture in the encapsulated stream,
      ;; we should use that for further input. *standard-input* is
      ;; bound back to the minibuffer (maybe) when an actual command
      ;; is executed.
      (let ((*standard-input* (encapsulating-stream-stream stream)))
        ;; Commands are permitted to signal immediate rescans, but
        ;; we may need to do some stuff first.
        (unwind-protect
             (accepting-from-user (drei)
               ;; We narrow the buffer to the last object before
               ;; input-position, so the user will not be able to
               ;; delete arguments prompts or other things.
               (drei-core:with-narrowed-buffer (drei
                                                (loop for index from
                                                      (1- (input-position stream)) above 0
                                                      when (typep (buffer-object buffer index)
                                                                  'noise-string)
                                                      return (1+ index)
                                                      finally (return 0))
                                                t t)
                 (handler-case (process-gestures-or-command drei)
                   (unbound-gesture-sequence (c)
                     (display-message "~A is unbound" (gesture-name (gestures c))))
                   (abort-gesture (c)
                     (if (member (abort-gesture-event c)
                                 *abort-gestures*
                          :test #'event-matches-gesture-name-p)
                         (signal 'abort-gesture :event (abort-gesture-event c))
                         (when was-directly-processing
                           (display-message "Aborted")))))))
          (update-drei-buffer stream)))
      (let ((first-mismatch (buffer-array-mismatch buffer old-buffer-contents)))
        (display-drei drei :redisplay-minibuffer t)
        (cond ((null first-mismatch)
               ;; No change actually took place, even though IP may
               ;; have moved.
               nil)
              ((< first-mismatch (stream-scan-pointer stream))
               ;; Eek, change before scan pointer - this probably
               ;; changes the scan, so we'll have to rescan
               ;; everything. Bummer!
               (immediate-rescan stream))
              (t
               ;; Something happened, but since we haven't even gotten
               ;; to scanning that part of the buffer yet, it doesn't
               ;; really matter. All that matters is that something
               ;; happened, and that it modified the buffer. This is a
               ;; somewhat liberal reading of the CLIM spec.
               (values first-gesture type)))))))

(defmethod stream-process-gesture ((stream drei-input-editing-mixin)
				   gesture type)
  ;; If some other command processor has taken control, we do not want
  ;; to assume that an activation gesture really is an activation
  ;; gesture. For example, #\Newline should not cause input activation
  ;; if isearch is being performed.
  (when (and (or (activation-gesture-p gesture)
                 (climi::gesture-match gesture *completion-gestures*)
                 (climi::gesture-match gesture *help-gestures*)
                 (climi::gesture-match gesture *possibilities-gestures*))
             (directly-processing-p (drei-instance stream)))
    (end-of-buffer (point (view (drei-instance stream))))
    (unless (= (stream-scan-pointer stream)
               (size (buffer (view (drei-instance stream)))))
      (queue-rescan stream))
    (setf (activation-gesture stream) gesture)
    (rescan-if-necessary stream)
    (return-from stream-process-gesture gesture))
  (when (proper-gesture-p gesture)
    (let ((*original-stream* (encapsulating-stream-stream stream)))
      (unread-gesture gesture :stream (encapsulating-stream-stream stream))))
  (read-gestures-and-act stream gesture type))

(defmethod reset-scan-pointer ((stream drei-input-editing-mixin)
			       &optional (scan-pointer 0))
  (setf (stream-scan-pointer stream) scan-pointer
        (stream-rescanning stream) t
        (input-position stream) (min scan-pointer (input-position stream))))

;; This has been cribbed from SPLIT-SEQUENCE and lightly modified.
(defun split-sequence (delimiter seq &key
                                       (count nil)
                                       (remove-empty-subseqs nil)
                                       (start 0)
                                       (end nil)
                                       (test nil test-supplied)
                                       (test-not nil test-not-supplied)
                                       (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be
included in the result; otherwise they will be discarded.  All
other keywords work analogously to those for CL:SUBSTITUTE. The
second return value is an index suitable as an argument to
CL:SUBSEQ into the sequence indicating where processing stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (loop for left = start then (+ right 1)
       for right = (min (or (apply #'position delimiter seq 
                                   :start left
                                   other-keys)
                            len)
                        end)
       unless (and (= right left) 
                   remove-empty-subseqs) ; empty subseq we don't want
       if (and count (>= nr-elts count))
       ;; We can't take any more. Return now.
       return (values subseqs left)
       else
       collect (subseq seq left right) into subseqs
       and sum 1 into nr-elts
       until (>= right end)
       finally (return (values subseqs right)))))

(defmethod input-editor-format ((stream drei-input-editing-mixin)
				format-string
				&rest format-args)
  "Insert a noise string at the insertion-pointer of `stream'."
  ;; Since everything inserted with this method is noise strings, we
  ;; do not bother to modify the scan pointer or queue rescans.
  (let* ((drei (drei-instance stream))
         (output (apply #'format nil format-string format-args)))
    (when (or (stream-rescanning-p stream)
              (zerop (length output)))
      (return-from input-editor-format nil))
    ;; A noise string really should not contain a newline or Drei will
    ;; malfunction. Of course, the newlines inserted this way aren't
    ;; actually noise-strings. FIXME.
    (loop for (seq . rest) on (split-sequence #\Newline output)
          when (plusp (length seq))
          do (insert-object (point (view drei))
                            (make-instance 'noise-string
                             :string seq))
          unless (null rest)
          do (insert-object (point (view drei)) #\Newline))))

(defmethod redraw-input-buffer ((stream drei-input-editing-mixin)
                                &optional (start-position 0))
  (declare (ignore start-position))
  ;; We ignore `start-position', because it would be more work to
  ;; figure out what to redraw than to just redraw everything.
  ;; We assume that this function is mostly called from non-Drei-aware
  ;; code, and thus synchronise the input-editor-array with the Drei
  ;; buffer before redisplaying.
  (update-drei-buffer stream)
  (display-drei (drei-instance stream)))

(defmethod erase-input-buffer ((stream drei-input-editing-mixin)
                               &optional (start-position 0))
  (declare (ignore start-position))
  ;; No-op, just to save older CLIM programs from dying.
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; `Add-input-editor-command'
;;;
;;; The CLIM spec requires us to define a completely unusable function
;;; for mapping gestures to functions in the input editor. Since the
;;; CLIM spec does not define, or even suggest, any kind of
;;; programmatic access to the data structures of the input-editor for
;;; these function, it is utterly impossible to write portable
;;; input-editor functions using this facility. Fortunately, Franz's
;;; user guide saves us. An input-editor-command defined via this
;;; facility takes four arguments: the input-editing stream, the input
;;; buffer (ugh!), the gesture used to invoke the command, and the
;;; accumulated numeric argument.

(defun add-input-editor-command (gestures function)
  "Set up Drei so performing `gestures' will result in the
invocation of `function'. Only works for Drei-based input-editing
streams. `Function' will be called with four arguments: the
input-editing stream, the input buffer, the gesture used to
invoke the command, and the accumulated numeric argument."
  (set-key `(,(lambda (numeric-argument)
                      (funcall function *drei-input-editing-stream*
                               (stream-input-buffer *drei-input-editing-stream*)
                               gestures
                               numeric-argument)) ,*numeric-argument-marker*)
           'exclusive-input-editor-table
           gestures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Presentation type specialization.

;;; When starting out with reading `command-or-form', we use Lisp
;;; syntax, so things like Structedit works. If it turns out to be a
;;; command, switch back to Fundamental.

(define-presentation-method accept :around
  ((type command-or-form)
   (stream drei-input-editing-mixin)
   view &key)
  (with-drei-options ((drei-instance stream)
                      :syntax "Lisp"
                      :keep-syntax t)
    (call-next-method)))

(define-presentation-method accept :around
  ((type command)
   (stream drei-input-editing-mixin)
   view &key)
  (with-drei-options ((drei-instance stream)
                      :syntax "Fundamental"
                      :keep-syntax nil)
    (call-next-method)))

(define-presentation-method accept :around
  ((type expression)
   (stream drei-input-editing-mixin)
   view
   &key)
  (with-drei-options ((drei-instance stream)
                      :syntax "Lisp"
                      :keep-syntax t)
    (redraw-input-buffer stream)
    (call-next-method)))

(define-presentation-method accept ((type expression)
                                    (stream drei-input-editing-mixin)
                                    (view textual-view)
				    &key)
  (let ((*completion-gestures* nil)
        (*possibilities-gestures* nil))
    (with-delimiter-gestures (nil :override t)
      (loop
         named control-loop
         with start-scan-pointer = (stream-scan-pointer stream)
         with drei = (drei-instance stream)
         with syntax = (syntax (view drei))
         ;; The input context permits the user to mouse-select displayed
         ;; Lisp objects and put them into the input buffer as literal
         ;; objects.
         for gesture = (with-input-context ('expression :override nil)
                           (object type)
                           (read-gesture :stream stream)
                         (expression (performing-drei-operations (drei :with-undo t
                                                                       :redisplay t)
                                       (presentation-replace-input
                                        stream object type (view drei)
                                        :buffer-start (stream-insertion-pointer stream)
                                        :allow-other-keys t
                                        :accept-result nil
                                        :rescan t))
                                     (rescan-if-necessary stream)
                                     nil))
         ;; True if `gesture' was freshly read from the user, and not
         ;; just retrieved from the buffer during a rescan.
         for freshly-inserted = (and (plusp (stream-scan-pointer stream))
                                     (not (equal (buffer-object
                                                  (buffer (view drei))
                                                  (1- (stream-scan-pointer stream)))
                                                 gesture)))
         for form = (drei-lisp-syntax::form-after syntax (input-position stream))
         ;; We do not stop until the input is complete and an activation
         ;; gesture has just been provided. The freshness check is so
         ;; #\Newline characters in the input will not cause premature
         ;; activation.
         until (and (activation-gesture-p gesture)
                    (or (and freshly-inserted
                             (drei-lisp-syntax::form-complete-p form))))
         when (and (activation-gesture-p gesture)
                   (null form))
         do ;; We have to remove the buffer contents (whitespace,
            ;; comments or error states, if this happens) or code
            ;; above us will not believe us when we tell them that the
            ;; input is empty
           (delete-buffer-range (buffer (view drei)) start-scan-pointer
                                (- (stream-scan-pointer stream)
                                   start-scan-pointer))
           (setf (stream-scan-pointer stream) start-scan-pointer)
           (simple-parse-error "Empty input")
         ;; We only want to process the gesture if it is fresh, because
         ;; if it isn't, it has already been processed at some point in
         ;; the past.
         when (and (activation-gesture-p gesture)
                   freshly-inserted)
         do (with-activation-gestures (nil :override t)
              (stream-process-gesture stream gesture nil))
         finally (unread-gesture gesture :stream stream)
         (let* ((object (handler-case
                            (drei-lisp-syntax:form-to-object syntax form
                             :read t
                             :package *package*)
                          (drei-lisp-syntax:form-conversion-error (e)
                            ;; Move point to the problematic form
                            ;; and signal a rescan.
                            (setf (activation-gesture stream) nil)
                            (handle-drei-condition drei e)
                            (display-drei drei :redisplay-minibuffer t)
                            (immediate-rescan stream))))
                (ptype (presentation-type-of object)))
           (return-from control-loop
             (values object
                     (if (presentation-subtypep ptype 'expression)
                         ptype 'expression))))))))
