;; Gilbert Baumann's hacks for using the CMUCL debugger within a CLIM stream.

(in-package :climi)

#+cmu19a
(progn (setf (ext:package-definition-lock (find-package "DEBUG")) nil)
       (setf (ext:package-definition-lock (find-package "COMMON-LISP")) nil)
       (setf (ext:package-definition-lock (find-package "EXT")) nil))

;; a patch
(defmethod stream-listen ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop for char = (read-gesture-or-reason estream :timeout 0 :peek-p t)
	  do (if (read-result-p char)
		 (loop-finish)
		 (stream-read-gesture estream)) ; consume pointer gesture
	  finally (return (characterp char)))))

;; a patch, not sure about this one as constructing an event seems wrong.
(defmethod stream-unread-gesture ((stream standard-extended-input-stream)
				  gesture)
  (with-encapsulating-stream (estream stream)
    (repush-gesture (if (characterp gesture)
                        (make-instance 'key-press-event
                                       :modifier-state 0
                                       :key-name gesture
                                       :key-character gesture
                                       :sheet estream
                                       :x 0 :y 0 :graft-x 0 :graft-y 0)
                        gesture)
                    (stream-input-buffer estream))))

(in-package #:debug)

(#+CMU19C
 ext:without-package-locks
 #-CMU19C
 progn

(defun internal-debug ()
  (let ((*in-the-debugger* t)
	(*read-suppress* nil))
    (unless (typep *debug-condition* 'step-condition)
      (clear-input *debug-io*)
      (format *debug-io* "~2&Debug  (type H for help)~2%"))
     (debug-loop) ))
    ;; (mp:without-scheduling (debug-loop))))

(defun invoke-debugger (condition)
  "The CMU Common Lisp debugger.  Type h for help."
  (when *debugger-hook*
    (let ((hook *debugger-hook*)
	  (*debugger-hook* nil))
      (funcall hook condition hook)))
  (unix:unix-sigsetmask 0)
  (let* ((*debug-condition* condition)
	 (*debug-restarts* (compute-restarts condition))
	 (*standard-input* *debug-io*)  ;in case of setq
	 (*standard-output* *debug-io*) ;''  ''  ''  ''
	 (*error-output* *debug-io*)
	 ;; Rebind some printer control variables.
	 (kernel:*current-level* 0)
	 (*print-readably* nil)
	 (*read-eval* t))
    (let ((*debugger-hook*
           (lambda (cond hook)
             (let ((*debugger-hook* nil)
                   (*debug-io* sys:*tty*))
               (invoke-debugger cond)))))
      (real-invoke-debugger condition))))

(defun debug-prompt ()
  (let ((*standard-output* *debug-io*))
    (progn
      (terpri)
      (prin1 (di:frame-number *current-frame*))
      (dotimes (i *debug-command-level*) (princ "]"))
      (princ " ")
      (force-output))))

(defparameter *debug-prompt* #'debug-prompt
  "This is a function of no arguments that prints the debugger prompt
   on *debug-io*.")
)

(in-package "LISP")

(#+CMU19C
 ext:without-package-locks
 #-CMU19C
 progn

(defun get-stream-command (stream)
  "This takes a stream and waits for text or a command to appear on it.  If
   text appears before a command, this returns nil, and otherwise it returns
   a command."
  (let ((cmdp nil #+NIL (funcall (lisp-stream-misc stream) stream :get-command)))
    (cond (cmdp)
	  ((listen stream)
	   nil)
	  (t
	   ;; This waits for input and returns nil when it arrives.
	   (unread-char (read-char stream) stream)))))
)
