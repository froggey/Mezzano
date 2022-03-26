;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-LISTENER; -*-
;;;
;;; Miscellaneous utilities, UI tools, gross hacks, and non-portable bits.
;;;
;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)
;;;
;;; See toplevel file 'Copyright' for the copyright details.
;;;

(in-package :clim-listener)

;; multiple-value-or, ugh. Normal OR drops values except from the last form.
(defmacro mv-or (&rest forms)
  (if (null forms)  nil
    (let ((tmp (gensym)))
      `(let ((,tmp (multiple-value-list ,(first forms))))
         (if (first ,tmp) (values-list ,tmp) (mv-or ,@(rest forms)))))))

(defun resolve-stream-designator (desi default)
  (if (eq desi t)
      default
      (or desi default)))

;;; Native namestring. cl:namestring is allowed to do anything it wants to
;;; the filename, and some lisps do (CCL, for instance).
(defun native-namestring (pathname-designator)
  #+sbcl (sb-ext:native-namestring pathname-designator)
  #+openmcl  (ccl::native-untranslated-namestring pathname-designator)
  #+mezzano pathname-designator
  #-(or sbcl openmcl mezzano) (namestring pathname-designator))

(defun native-enough-namestring (pathname &optional
                                 (defaults *default-pathname-defaults*))
  (native-namestring (enough-namestring pathname defaults)))

;;;; CLIM/UI utilities

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(defmacro bordering ((stream shape) &body body)
  `(surrounding-output-with-border (,stream :shape ,shape :move-cursor t)
     ,@body))

(defmacro underlining ((stream) &body body)
  `(surrounding-output-with-border (,stream :shape :underline :move-cursor nil)
    ,@body))

(defun note (string &rest args)
  (let ((stream *query-io*))
    (italic (stream)
      (with-text-family (stream :sans-serif)
        (fresh-line stream)
        (apply #'format *query-io* string args)
        (fresh-line stream)))))


(defun vertical-gap (stream &optional (fraction 3))
  (when (eq stream t) (setf stream *standard-output*))
  (stream-increment-cursor-position stream 0
      (truncate (/ (text-style-ascent (medium-text-style stream) stream) fraction))))

(defun invoke-as-heading (cont &optional (ink +royal-blue+))
  (with-drawing-options (t :ink ink :text-style (make-text-style :sans-serif :bold nil))
    (fresh-line)
    (underlining (t)
      (funcall cont))
    (fresh-line)
    (vertical-gap t)))

(defun heading (control-string &rest args)
  (invoke-as-heading
   (lambda ()
     (apply 'format t control-string args))))

(defun indent-to (stream x &optional (spacing 0) )
  "Advances cursor horizontally to coordinate X. If the cursor is already past
this point, increment it by SPACING, which defaults to zero."
  (stream-increment-cursor-position stream
				    (if (> (stream-cursor-position stream) x)
					spacing
                                        (- x (stream-cursor-position stream)))
                                    0))

(defun invoke-and-center-output (stream-pane continuation
                                 &key (horizontally t) (vertically t) (hpad 0) (vpad 0))
  (let ((record (with-output-to-output-record (stream-pane)
                  (funcall continuation))))
    (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region stream-pane)
      (with-bounding-rectangle* (rx0 ry0 rx1 ry1) (bounding-rectangle record)
        (setf (output-record-position record)
              (values (if horizontally
                          (+ rx0 (/ (- (- sx1 sx0)
                                       (- rx1 rx0))
                                    2))
                          (+ rx0 hpad))
                      (if vertically
                          (+ ry0 (/ (- (- sy1 sy0)
                                       (- ry1 ry0))
                                    2))
                          (+ ry0 vpad))))))
    (add-output-record record (stream-output-history stream-pane))
    (repaint-sheet stream-pane record)))

;;; Pathnames are awful.

(defun strip-filespec (pathname)
  "Removes name, type, and version components from a pathname."
  (make-pathname :name nil
                 :type nil
                 :version nil
		 #+scl :query #+scl nil
		 :defaults pathname))

(defun parent-directory (pathname)
  "Returns a pathname designating the directory 'up' from PATHNAME"
  (let ((dir (pathname-directory pathname ))) ;(if (probe-file pathname)
                                               ;    pathname
    (when (and (eq (first dir) :absolute)
               (rest dir))
      ;; merge-pathnames merges :back, but not :up
      (strip-filespec
       (merge-pathnames (make-pathname :host (pathname-host pathname)
                                       :device (pathname-device pathname)
                                       :directory '(:relative :back))
                        (truename pathname))))))

;;;; Abbreviating item formatter

;;; Doesn't work as well as I'd like, due to the table formatter not sizing
;;; columns as anticipated.

(defparameter *abbreviating-minimum-items* 6
  "Minimum number of items needed to invoke abbreviation. This must be at least one.")
(defparameter *abbreviating-outlier-threshold* 2.0
  "Number of standard deviations beyond the mean needed to invoke abbreviation.")
(defparameter *abbreviating-minimum-ratio* 1.2
  "A minimum ratio of item width to the mean width, below which abbreviation will not be invoked. This is a safeguard to treat very uniform inputs more sanely, where the test against the standard deviation might behave undesirably.")
(defparameter *abbreviating-maximum-cutoff* 1.8
  "A maximum ratio of item width to mean width, beyond which abbreviation will always be invoked. This is useful to handle cases where one extreme outlier throws the standard deviation all out of whack.")

(defun text-output-record-style (record)
  "Returns the text style used in RECORD, a text-displayed-output-record."
  (climi::graphics-state-text-style (first (slot-value record 'climi::strings))))

(defun find-text-record (record)
  "If RECORD contains exactly one text-displayed-output-record, we can abbreviate it. Otherwise, give up. Returns a string containing the contents of the record, and a text style."
  (let ((count 0)
        text-style
        result)
    (labels ((walk (record)
               (typecase record
                 (climi::compound-output-record
                  (map-over-output-records #'walk record))
                 (text-displayed-output-record
                  (setf result record)
                  (setf text-style (text-output-record-style record))
                  (incf count)))))
      (walk record)
      (when (= count 1)
        (values result text-style)))))

(defun abbrev-guess-pos (medium string text-style desired-width start end)
  "Makes a guess where to split STRING between START and END in order to fit within WIDTH. Returns the ending character index."
  (let* ((length (- end start))
         (actual-width (text-size medium string :text-style text-style :start start :end end))
         (pixels-per-char (/ actual-width length))
         (guess (truncate (/ desired-width pixels-per-char))))
    (when (< actual-width desired-width)  ; This almost certainly shouldn't happen.
      (return-from abbrev-guess-pos end)) ; But it could.
    (+ start guess)))

;; FIXME: I can do a bit better than this.
;; I'd like to use this only as a fallback, eventually.
(defun abbreviate-string (medium string text-style max-width)
  "Returns an abbreviated version of STRING hopefully less than MAX-WIDTH,
as it would be displayed on MEDIUM using TEXT-STYLE"
  (let* ((ellipsis-width (text-size medium "..." :text-style text-style))
         (working-width (- max-width ellipsis-width)))
    (when (<= working-width 0)   ; weird, just give up.
      (return-from abbreviate-string string))
    ;; FIXME: I was planning to do several stages of refining the guess, but I got lazy..
    ;; Now that I've thought about it, this sort of guesswork with counting text-sizes is
    ;; pretty disgusting anyway, and I ought to just be counting character sizes, and assume
    ;; that text-size isn't just somehow magically faster than doing it myself.
    ;; so.. FIXME!
    (concatenate 'string
                 (subseq string 0 (abbrev-guess-pos medium string text-style working-width 0 (length string)))
                 "...")))

(defun abbreviate-record (stream record width abbreviator)
  "Attempts to abbreviate the text contained in an output RECORD on STREAM to fit within WIDTH, using the function ABBREVIATOR to produce a shortened string."
  (multiple-value-bind (text-record text-style)
      (find-text-record record)
    (when text-record
      (multiple-value-bind (x y)
          (output-record-position text-record)
        (let* ((parent (output-record-parent text-record))
               (medium (slot-value text-record 'medium))
               (abbreviation (funcall abbreviator medium (text-displayed-output-record-string text-record)
                                      text-style (- width (- (bounding-rectangle-width record)
                                                             (bounding-rectangle-width text-record))))))
          (delete-output-record text-record parent)
          (with-text-style (medium text-style)
            (let ((new-record (with-output-to-output-record (stream)
                                (write-string abbreviation stream))))
              (setf (output-record-position new-record) (values x y))
              (add-output-record new-record parent)
              #+IGNORE (tree-recompute-extent parent)))))))
  record)

(defun abbreviating-format-items (items &rest args &key stream printer
                                        presentation-type (abbreviator #'abbreviate-string)
                                        &allow-other-keys)
  "Similar to FORMAT-ITEMS, but abbreviates excessively long text using a
function specified by :ABBREVIATOR. Abbreviate is controlled by the variables
*ABBREVIATING-OUTLIER-THRESHOLD*, *ABBREVIATING-MINIMUM-RATIO*, and
*ABBREVIATING-MAXIMUM-CUTOFF*."
  (setf stream (resolve-stream-designator stream *standard-output*))
  (let* ((length  (length items))
         (printer (or printer (lambda (item stream)
                               (present item presentation-type :stream stream))))
         (hash (make-hash-table :test 'eq :size (truncate (* 1.5 length))))
         (mean 0.0)
         (deviation 0.0))

    (when (< length *abbreviating-minimum-items*)
      (apply #'format-items items args)
      (return-from abbreviating-format-items))

    (dolist (item items)
      (let ((record (with-output-to-output-record (stream)
                      (with-end-of-line-action (stream :allow)
                        (funcall printer item stream)))))
        (setf (gethash item hash) record)
        (incf mean (bounding-rectangle-width record))))
    (setf mean (/ mean length))
    (maphash (lambda (key val)
               (declare (ignore key))
               (incf deviation (expt (- mean (bounding-rectangle-width val)) 2)))
             hash)
    (unless (= length 1)
      (setf deviation (sqrt (/ deviation (1- length)))))

    (setf args (copy-list args))
    (remf args :printer)

    (let* ((stddev-max-width (+ mean (* *abbreviating-outlier-threshold* deviation)))
           (ratio-max-width  (* mean *abbreviating-minimum-ratio*))
           (cutoff-width     (* mean *abbreviating-maximum-cutoff*))
           (max-width        (min cutoff-width (max stddev-max-width ratio-max-width))))
   ;   (hef:debugf mean deviation stddev-max-width ratio-max-width max-width)
      (apply #'format-items items
             :printer (lambda (item stream)
                        (let ((record (gethash item hash)))
                          (when (and (> (bounding-rectangle-width record) stddev-max-width)
                                     (> (bounding-rectangle-width record) ratio-max-width))
                            (setf record (abbreviate-record stream record max-width abbreviator)))
                          (stream-add-output-record stream record)))
             args))))


;;; An attempt at integrating RUN-PROGRAM closer with lisp.

;;; This code creates a macro on the #! character sequence which expands
;;; to a lambda closed over a call to RUN-PROGRAM invoked the program
;;; named by the following string, ex. (#!rm :r :f "foodir")

;;; My apologies to anyone using the #! character for something useful.


;; TODO:
;;  * Environment variables?
;;  * Figure out what to do with the input/output streams
;;  * Ability to pipe programs together, input/output redirection.
;;  * Utilities for getting data in and out of unix programs through streams
;;  * Pseudoterminal support (yeah, right)

;; We attempt to translate keywords and a few types of lisp objects
;; used as arguments to make program wrappers feel more "lispy".

(defgeneric transform-program-arg (arg))

(defmethod transform-program-arg ((arg t))
  (values (prin1-to-string arg)))

(defmethod transform-program-arg ((arg string))
  arg)

(defmethod transform-program-arg ((arg sequence))
  (values-list (map 'list #'transform-program-arg arg)))

(defmethod transform-program-arg ((arg symbol))
  (let ((name (string-downcase (symbol-name arg))))
    (if (keywordp arg)
        (values (concatenate 'string (if (= 1 (length name)) "-" "--") name))
      name)))  ;; do some less horrible DWIM downcasing of vanilla symbols? hmm..

(defmethod transform-program-arg ((arg pathname))
  (if (wild-pathname-p arg)
      (values-list (mapcar #'transform-program-arg
                           (directory arg))) ;; (with-fingers-crossed ...)
    (values (namestring arg))))

(defun transform-program-arguments (args)
  (let ((list nil))
    (dolist (arg args)
      (setf list (nconc list (multiple-value-list (transform-program-arg arg)))))
    list))

(defun program-wrapper (name)
  "Returns a closure which invokes the NAMEd program through the operating system,
with some attempt to convert arguments intelligently."
  (lambda (&rest args)
    (let ((output *standard-output*))
      (uiop:run-program (list* name (transform-program-arguments args))
                        :force-shell nil :output output :error-output output
                        :input nil #+NIL (resolve-stream-designator *run-input* *standard-input*)))
    ;; It might be useful to return the exit status of the process, but our run-program
    ;; wrapper doesn't
    (values)))

(defun read-stringlet (stream)
  (with-output-to-string (out)
    (unread-char
     (do ((c (read-char stream) (read-char stream)))
         ((or (member c '(#\Space #\Tab #\Newline #\Linefeed #\Page #\Return)) ;; What..??
              (multiple-value-bind (a b) (get-macro-character c)
                (and a (not b))))
          c)
       (when (eql c #\\)
         (setf c (read-char stream)))
       (write-char c out))
     stream)))

;;; Don't install this by default, because no one uses it.
#+NIL
(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char p)
      (declare (ignore char p))
      (let ((name (read-stringlet stream)))
        `(lambda (&rest args)
           (apply (program-wrapper ,name) args)))))

;;;; Graphing and various helpers

(defparameter *min-x* -7)
(defparameter *max-x* 7)
(defparameter *min-y* -7)
(defparameter *max-y* 7)
(defparameter *graph-size* 600)
(defparameter *graph-width* nil)
(defparameter *graph-height* nil)
(defparameter *graph-ink* +black+)

(defun draw-thin-bar-graph-1 (medium function scale min max dx)
  (loop for i from 0 below (floor (- max min) dx)
        for x = min then (+ x dx)
        do (draw-line* medium i 0 i (* scale (funcall function x)))))

(defun draw-vector-bar-graph
    (vector &key (stream *standard-output*) (scale-y 1) (ink +black+)
     (key 'identity) (start 0) (end nil))
  (let ((range (- (reduce 'max vector :start start :end end :key key)
                  0 #+NIL (reduce 'min vector :start start :end end :key key)))) ; totally wrong

    (with-room-for-graphics (stream :first-quadrant t)
      (with-new-output-record (stream)
        (with-drawing-options (stream :ink ink)
          (unless (zerop range)
            (when (eql t scale-y)
              (setf scale-y (/ 250 range)))
            (draw-thin-bar-graph-1
             stream
             (lambda (i) (funcall key (aref vector i)))
             scale-y start (or end (length vector)) 1)))))))

;(defun draw-coordinate-labels (stream value-min val-max stream-min stream-max)
;
;  (text-size stream (format nil "~4F" value)

;; Broken - min-y/max-y aren't, in the sense that it won't clip to
;; those values.
(defun draw-function-filled-graph
    (function &key (stream *standard-output*)
     (min-x *min-x*) (max-x *max-x*)
     (min-y *min-y*) (max-y *max-y*)
     size
     (width  (or size *graph-width* *graph-size*))
     (height (or size *graph-height* *graph-size*))
     (ink *graph-ink*))
  (with-room-for-graphics (stream :first-quadrant t)
    (with-new-output-record (stream)
      (with-drawing-options (stream :ink ink)
        (draw-thin-bar-graph-1 stream function
                               (float (/ height (- max-y min-y)) 0.0f0)
                               min-x max-x
                               (/ (- max-x min-x) width))))))
