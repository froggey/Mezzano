;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)

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

(in-package :clim-internals)

;;; Note: in the methods defined on output streams, I often use
;;;       the sheet's medium as the argument to the draw-* routines.
;;;       This is so that they don't get recorded if the stream also
;;;       happens to be an output-recording-stream. - MikeMac 1/7/99

;;; Standard-Output-Stream class
(defclass standard-output-stream (output-stream text-selection-mixin) ())

(defmethod stream-recording-p ((stream output-stream)) nil)
(defmethod stream-drawing-p ((stream output-stream)) t)

;;; Cursor-Mixin class
(defclass cursor-mixin ()
  ((sheet :initarg :sheet
          :reader cursor-sheet)
   (x :initform 0 :initarg :x-position)
   (y :initform 0 :initarg :y-position)
   (width :initform 8)
   (appearance :type (member :solid :hollow)
               :initarg :appearance :initform :hollow
               :accessor cursor-appearance)
   ;; XXX what does "cursor is active" mean?
   ;; It means that the sheet (stream) updates the cursor, though
   ;; currently the cursor appears to be always updated after stream
   ;; text operations. -- moore
   (cursor-active :initform nil
                  :accessor cursor-active)
   (cursor-state :initform nil
                 :accessor cursor-state)))

(defgeneric cursor-height (cursor))

(defmethod print-object ((cursor cursor-mixin) stream)
  (with-slots (x y) cursor
    (print-unreadable-object (cursor stream :type t :identity t)
      (format stream "~D ~D " x y))))

(defgeneric flip-screen-cursor (cursor))

;;; XXX What to do when we can't draw the cursor immediately (like,
;;; we're not drawing?) The whole flip-screen-cursor idea breaks down.

(defmethod (setf cursor-state) :around (state (cursor cursor-mixin))
  (unless (eq state (slot-value cursor 'cursor-state))
    (flip-screen-cursor cursor))
  (call-next-method))

(defun decode-cursor-visibility (visibility)
  "Given :on, :off, or nil, returns the needed active and state attributes for the cursor."
  (ecase visibility
    ((:on t) (values t t))
    (:off    (values t nil))
    ((nil)   (values nil nil))))

(defmethod cursor-visibility ((cursor cursor-mixin))
  (let ((a (cursor-active cursor))
        (s (cursor-state cursor)))
    (cond ((and a s) :on)
          ((and a (not s)) :off)
          (t nil))))

(defmethod (setf cursor-visibility) (nv (cursor cursor-mixin))
  (multiple-value-bind (active state)
      (decode-cursor-visibility nv)
    (setf (cursor-state cursor)  state
          (cursor-active cursor) active)))

(defmethod cursor-position ((cursor cursor-mixin))
  (with-slots (x y) cursor
    (values x y)))

(defmethod* (setf cursor-position) (nx ny (cursor cursor-mixin))
  (with-slots (x y) cursor
    (letf (((cursor-state cursor) nil))
      (multiple-value-prog1
          (setf (values x y) (values nx ny))))
    (when (and (cursor-active cursor)
               (output-recording-stream-p (cursor-sheet cursor)))
      (stream-close-text-output-record (cursor-sheet cursor)))))

(defmethod flip-screen-cursor ((cursor cursor-mixin))
  (when (stream-drawing-p (cursor-sheet cursor))
    (with-slots (x y sheet width) cursor
      (let ((height (cursor-height cursor)))
        (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                         x y
                         (+ x width) (+ y height)
                         :filled (ecase (cursor-appearance cursor)
                                   (:solid t) (:hollow nil))
                         :ink +flipping-ink+)))))

(defgeneric display-cursor (cursor state))

(defmethod display-cursor ((cursor cursor-mixin) state)
  (unless (stream-drawing-p (cursor-sheet cursor))
    (return-from display-cursor nil))
  (with-slots (x y sheet width) cursor
    (let ((height (cursor-height cursor)))
      (case state
        (:draw
         (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                          x y
                          (+ x width) (+ y height)
                          :filled (ecase (cursor-appearance cursor)
                                    (:solid t) (:hollow nil))
                          :ink +foreground-ink+))
        (:erase
         ;; This is how I'd like this to work, as painting over with
         ;; the background ink is repugnant. I leave this disabled
         ;; because I'm concerned about infinite recursion if
         ;; replay-output-record calls here. --Hefner
         #+nil (repaint-sheet (cursor-sheet cursor)
                              (make-bounding-rectangle x y (+ 1 x width)
                                                       (+ 1 y height)))
         (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                          x y
                          (+ x width) (+ y height)
                          :filled (ecase (cursor-appearance cursor)
                                    (:solid t) (:hollow nil))
                          :ink +background-ink+))))))

;;; Standard-Text-Cursor class

(defclass standard-text-cursor (cursor-mixin cursor)
  ())

(defmethod cursor-height ((cursor standard-text-cursor))
  (%stream-char-height (cursor-sheet cursor)))


;;; Standard-Extended-Output-Stream class

(defclass standard-extended-output-stream (extended-output-stream
                                           standard-output-stream
                                           standard-page-layout
                                           filling-output-mixin)
  ((cursor :accessor stream-text-cursor)
   (foreground :initarg :foreground :reader foreground)
   (background :initarg :background :reader background)
   (text-style :initarg :text-style :reader stream-text-style)
   (vspace :initarg :vertical-spacing :reader stream-vertical-spacing)
   (eol :initarg :end-of-line-action :accessor stream-end-of-line-action)
   (eop :initarg :end-of-page-action :accessor stream-end-of-page-action)
   (view :initarg :default-view :accessor stream-default-view)
   (baseline :initform 0 :reader stream-baseline)
   ;; the max char height of the current line
   (char-height :initform 0 :accessor %stream-char-height))
  (:default-initargs
   :foreground +black+ :background +white+ :text-style *default-text-style*
   :vertical-spacing 2 :end-of-page-action :scroll :end-of-line-action :wrap
   :default-view +textual-view+))

(defmethod stream-force-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-force-output medium)))

(defmethod stream-finish-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-finish-output medium)))

(defmethod initialize-instance :after
    ((stream standard-extended-output-stream) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-bind (x-start y-start)
      (stream-cursor-initial-position stream)
    (setf (stream-text-cursor stream)
          (make-instance 'standard-text-cursor
                         :sheet stream
                         :x-position x-start
                         :y-position y-start)))
  (setf (cursor-active (stream-text-cursor stream)) t))


(defmethod stream-cursor-position ((stream standard-extended-output-stream))
  (cursor-position (stream-text-cursor stream)))

(defmethod* (setf stream-cursor-position)
    (x y (stream standard-extended-output-stream))
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defmethod stream-set-cursor-position ((stream standard-extended-output-stream) x y)
  (setf (stream-cursor-position stream) (values x y)))

(defmethod stream-increment-cursor-position
    ((stream standard-extended-output-stream) dx dy)
  (multiple-value-bind (x y) (cursor-position (stream-text-cursor stream))
    (let ((dx (or dx 0))
          (dy (or dy 0)))
    (setf (cursor-position (stream-text-cursor stream))
          (values (+ x dx) (+ y dy))))))

;;;

(defmethod handle-repaint :around ((stream standard-extended-output-stream)
                                   region)
  (declare (ignorable region))
  (let ((cursor (stream-text-cursor stream)))
    (if (cursor-state cursor)
        ;; Erase the cursor so that the subsequent flip operation will
        ;; make a cursor, whether or not the next-method erases the
        ;; location of the cursor.
        ;; XXX clip to region?  No one else seems to...
        ;; Sure clip to region! --GB
        (letf (((cursor-state cursor) nil))
          (call-next-method))
        (call-next-method))))

;; In next few functions we can't call (setf stream-cursor-position) because
;; that would close the text-output-record unnecessarily. Using underlying
;; text-cursor with (setf cursor-position) is fine when the cursor is "off".
;; Otherwise output record would be closed anyway. -- jd 2019-01-07

(defmacro with-cursor-off (stream &body body)
  `(letf (((cursor-visibility (stream-text-cursor ,stream)) nil))
     ,@body))

(defmacro with-end-of-line-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-line-action ,stream) ,action))
     ,@body))

(defmacro with-end-of-page-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-page-action ,stream) ,action))
     ,@body))

(defgeneric maybe-end-of-page-action (stream y)
  (:method ((stream standard-extended-output-stream) y)
    ;; fixme: remove assumption about page vertical direction -- jd 2019-03-02
    (let ((bottom-margin (nth-value 1 (stream-cursor-final-position stream)))
          (end-of-page-action (stream-end-of-page-action stream)))
      (when (> y bottom-margin)
        (%note-stream-end-of-page stream end-of-page-action y)
        (ecase end-of-page-action
          ((:scroll :allow)  nil)
          ((:wrap :wrap*)
           (setf (cursor-position (stream-text-cursor stream))
                 (values (nth-value 0 (stream-cursor-position stream))
                         (nth-value 1 (stream-cursor-initial-position stream))))))))))

(defgeneric %note-stream-end-of-page (stream action new-height)
  (:method (stream action new-height)
    nil))

(defgeneric seos-write-newline (stream &optional soft-newline-p)
  (:method :after ((stream filling-output-mixin) &optional soft-newline-p)
    (when-let ((after-line-break-fn (after-line-break stream)))
      (funcall after-line-break-fn stream soft-newline-p)))
  (:method ((stream standard-extended-output-stream) &optional soft-newline-p)
    (declare (ignorable soft-newline-p))
    (let* ((current-cy       (nth-value 1 (stream-cursor-position stream)))
           (vertical-spacing (stream-vertical-spacing stream))
           (updated-cy       (+ current-cy
                                (%stream-char-height stream)
                                vertical-spacing)))
      (setf (cursor-position (stream-text-cursor stream))
            (values (stream-cursor-initial-position stream)
                    updated-cy))
      ;; this will close the output record if recorded
      (unless nil ;soft-newline-p
        (finish-output stream))
      (let* ((medium       (sheet-medium stream))
             (text-style   (medium-text-style medium))
             (new-baseline (text-style-ascent text-style medium))
             (new-height   (text-style-height text-style medium)))
        (maybe-end-of-page-action stream (+ updated-cy new-height))
        (setf (slot-value stream 'baseline) new-baseline
              (%stream-char-height stream)  new-height)))))

(defun seos-write-string (stream string &optional (start 0) end)
  (setq end (or end (length string)))
  (when (= start end)
    (return-from seos-write-string))
  (let* ((medium (sheet-medium stream))
         (text-style (medium-text-style medium))
         ;; fixme: remove assumption about the text direction (LTR).
         (left-margin  (stream-cursor-initial-position stream))
         (right-margin (stream-cursor-final-position stream)))
    (maxf (slot-value stream 'baseline) (text-style-ascent text-style medium))
    (maxf (%stream-char-height stream)  (text-style-height text-style medium))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (maybe-end-of-page-action stream (+ cy (%stream-char-height stream)))
      (let* ((width (stream-string-width stream string
                                         :start start :end end
                                         :text-style text-style))
             (eol-action (stream-end-of-line-action stream))
             (eol-p (> (+ cx width) right-margin)))
        (when (or (null eol-p) (member eol-action '(:allow :scroll)))
          (stream-write-output stream string start end)
          (incf cx width)
          (setf (cursor-position (stream-text-cursor stream)) (values cx cy))
          (when (and (> cx right-margin)
                     (eql eol-action :scroll))
            (multiple-value-bind (tx ty)
                (bounding-rectangle-position (sheet-region stream))
              (scroll-extent stream (+ tx width) ty)))
          (return-from seos-write-string))
        ;; All new lines from here on are soft new lines, we could skip
        ;; closing the text-output-record and have multiline records to
        ;; allow gimmics like a dynamic reflow). Also text-style doesn't
        ;; change until the end of this function. -- jd 2019-01-10
        ;;
        ;; Writing a newline may cause the cursor increment, so we
        ;; need to compute split for each line after the soft newline
        ;; has been written. -- jd 2020-03-01
        (loop with width = (if (text-style-fixed-width-p text-style medium)
                               (text-style-width text-style medium)
                               (lambda (string start end)
                                 (text-size medium string
                                            :text-style text-style
                                            :start start :end end)))
              with margin = (- right-margin left-margin)
              with cursor = (stream-text-cursor stream)
              with break  = (ecase eol-action
                              (:wrap nil)
                              (:wrap* t))
              for offset = (- (cursor-position cursor) left-margin)
              for split  = (car (line-breaks string width
                                             :count 1
                                             :initial-offset offset
                                             :margin margin
                                             :break-strategy break
                                             :start start :end end))
              do (stream-write-output stream string start split)
                 (when (= split end)
                   (setf (cursor-position cursor)
                         (values (+ left-margin
                                    (stream-string-width stream string
                                                         :start start :end split
                                                         :text-style text-style))
                                 (nth-value 1 (cursor-position cursor))))
                   (return))
                 (seos-write-newline stream t)
                 (setf start split))))))

(defgeneric stream-write-output (stream line &optional start end)
  (:documentation
   "Writes the character or string LINE to STREAM. This function
produces no more than one line of output i.e., doesn't wrap."))

;;; The cursor is in stream coordinates.
(defmethod stream-write-output ((stream standard-extended-output-stream) line
                                &optional (start 0) end)
  ;; Do not capture medium transformation - this is a stream operation and we
  ;; draw at the current cursor position. -- jd 2019-01-04
  (with-identity-transformation (stream)
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (draw-text* stream line cx (+ cy (stream-baseline stream))
                  :start start :end end))))

(defmethod stream-write-char ((stream standard-extended-output-stream) char)
  (with-cursor-off stream
    (if (char= #\Newline char)
        (seos-write-newline stream)
        (seos-write-string stream (string char)))))

(defmethod stream-write-string ((stream standard-extended-output-stream) string
                                &optional (start 0) end)
  (let ((seg-start start)
        (end (or end (length string))))
    (with-cursor-off stream
      (loop for i from start below end do
        (when (char= #\Newline
                     (char string i))
          (seos-write-string stream string seg-start i)
          (seos-write-newline stream)
          (setq seg-start (1+ i))))
      (seos-write-string stream string seg-start end))))

(defmethod stream-character-width ((stream standard-extended-output-stream) char
                                   &key (text-style nil))
  (with-sheet-medium (medium stream)
    (text-style-character-width (or text-style (medium-text-style medium))
                                medium
                                char)))

(defmethod stream-string-width ((stream standard-extended-output-stream) string
                                &key (start 0) (end nil) (text-style nil))
  (with-sheet-medium (medium stream)
    (if (null text-style)
        (setq text-style (medium-text-style (sheet-medium stream))))
    (multiple-value-bind (total-width total-height final-x final-y baseline)
        (text-size medium string :text-style text-style
                   :start start :end end)
      (declare (ignore total-height final-y baseline))
      (values final-x total-width))))

(defmethod stream-text-margin ((stream standard-extended-output-stream))
  (bounding-rectangle-max-x (stream-page-region stream)))

(defmethod (setf stream-text-margin) (margin (stream standard-extended-output-stream))
  (setf (stream-text-margins stream)
        (if margin
            `(:right (:absolute ,margin))
            `(:right (:relative 0)))))

(defmethod stream-line-height ((stream standard-extended-output-stream)
                               &key (text-style nil))
  (+ (text-style-height (or text-style (medium-text-style (sheet-medium stream)))
                        (sheet-medium stream))
     (stream-vertical-spacing stream)))

(defmethod stream-line-width ((stream standard-extended-output-stream))
  (bounding-rectangle-width (stream-page-region stream)))

(defmethod stream-line-column ((stream standard-extended-output-stream))
  (let ((line-width (- (stream-cursor-position stream)
                       (stream-cursor-initial-position stream))))
    (if (minusp line-width)
        nil
        (floor line-width (stream-string-width stream "m")))))

(defmethod stream-start-line-p ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (= x (stream-cursor-initial-position stream))))

(defmacro with-room-for-graphics ((&optional (stream t)
                                             &rest arguments
                                             &key (first-quadrant t)
                                             height
                                             (move-cursor t)
                                             (record-type ''standard-sequence-output-record))
                                  &body body)
  (declare (ignore first-quadrant height move-cursor record-type))
  (let ((cont (gensym "CONT."))
        (stream (stream-designator-symbol stream '*standard-output*)))
    `(labels ((,cont (,stream)
                ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-room-for-graphics #',cont ,stream ,@arguments))))

(defmethod beep (&optional medium)
  (if medium
      (medium-beep medium)
      (when (sheetp *standard-output*)
        (medium-beep (sheet-medium *standard-output*)))))

(defgeneric scroll-quantum (pane)
  (:documentation "Returns the number of pixels respresenting a 'line', used
to computed distance to scroll in response to mouse wheel events."))

(defmethod scroll-quantum ((sheet standard-extended-output-stream))
  (stream-line-height sheet))

;;; Backend part of the output destination mechanism
;;;
;;; See clim-core/commands.lisp for the "user interface" part.

(defgeneric invoke-with-standard-output (continuation destination)
  (:documentation
   "Call CONTINUATION (with no arguments) with *STANDARD-OUTPUT*
rebound according to DESTINATION."))

(defmethod invoke-with-standard-output (continuation (destination null))
  ;; Call CONTINUATION without rebinding *STANDARD-OUTPUT* at all.
  (funcall continuation))

(defclass output-destination ()
  ())

(defclass stream-destination (output-destination)
  ((destination-stream :accessor destination-stream
                       :initarg :destination-stream)))

(defmethod invoke-with-standard-output
    (continuation (destination stream-destination))
  (let ((*standard-output* (destination-stream destination)))
    (funcall continuation)))

(defclass file-destination (output-destination)
  ((file :reader destination-file :initarg :file)))

(defmethod destination-element-type ((destination file-destination))
  :default)

(defmethod invoke-with-standard-output
    (continuation (destination file-destination))
  (with-open-file (*standard-output* (destination-file destination)
                                     :element-type (destination-element-type
                                                    destination)
                                     :direction :output
                                     :if-exists :supersede)
    (funcall continuation)))

(defparameter *output-destination-types*
  '(("Stream" stream-destination)))

(defun register-output-destination-type (name class-name)
  (let ((class (find-class class-name nil)))
    (cond ((null class)
           (error "~@<~S is not the name of a class.~@:>" class-name))
          ((not (subtypep class #1='output-destination))
           (error "~@<~A is not a subclass of ~S.~@:>" class #1#))))
  (pushnew (list name class-name) *output-destination-types* :test #'equal))
