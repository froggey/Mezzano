;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(declaim (special *debug-io*
                  *standard-input*
                  *standard-output*))

(defun debug-info-name (info)
  (second info))

(defun debug-info-local-variable-locations (info)
  "Returns a list of (NAME STACK-SLOT) for each local variable."
  (third info))

(defun debug-info-closure-layout (info)
  "Returns two values, the stack slot of the closure vector and the layout of the vector.
Returns NIL if the function captures no variables."
  (values (fourth info) (fifth info)))

(defun debug-info-source-pathname (info)
  (sixth info))

(defun debug-info-source-top-level-form-number (info)
  (seventh info))

(defun debug-info-lambda-list (info)
  (eighth info))

(defun debug-info-docstring (info)
  (ninth info))

(defparameter *debugger-depth* 0)
(defvar *debugger-condition* nil)
(defvar *current-debug-frame* nil)

(defvar *default-frames-to-print* 15)

(defun function-from-frame (frame)
  (let* ((return-address (memref-signed-byte-64 (second frame) 1)))
    (return-address-to-function return-address)))

(defun read-frame-slot (frame slot)
  (memref-t (memref-unsigned-byte-64 (second frame) 0) (- (1+ slot))))

(defun write-frame-slot (frame slot value)
  (setf (memref-t (memref-unsigned-byte-64 (second frame) 0) (- (1+ slot)))
        value))

(defun show-debug-frame ()
  (format t "Frame ~D(~X): ~S~%"
          (first *current-debug-frame*)
          (second *current-debug-frame*)
          (function-from-frame *current-debug-frame*)))

(defun debugger-show-variables (frame)
  (let* ((fn (function-from-frame frame))
         (info (function-debug-info fn))
         (var-id 0))
    (format t "Locals:~%")
    (loop
       for (name stack-slot) in (debug-info-local-variable-locations info)
       do (format t "  ~D ~S: ~S~%" (incf var-id) name (read-frame-slot frame stack-slot)))
    (multiple-value-bind (env-slot env-layout)
        (sys.int::debug-info-closure-layout info)
      (when env-slot
        (format t "Closed-over variables:~%")
        (let ((env-object (read-frame-slot frame env-slot)))
          (dolist (level env-layout)
            (loop
               for i from 1
               for name in level
               when name
               do (format t "  ~D ~S: ~S~%" (incf var-id) name (svref env-object i)))
            (setf env-object (svref env-object 0))))))))

(defun debugger-read-variable (frame id)
  (let* ((fn (function-from-frame frame))
         (info (function-debug-info fn))
         (var-id 0))
    (loop
       for (name stack-slot) in (debug-info-local-variable-locations info)
       when (eql (incf var-id) id)
       do (return-from debugger-read-variable
            (values (read-frame-slot frame stack-slot) name)))
    (multiple-value-bind (env-slot env-layout)
        (sys.int::debug-info-closure-layout info)
      (when env-slot
        (let ((env-object (read-frame-slot frame env-slot)))
          (dolist (level env-layout)
            (loop
               for i from 1
               for name in level
               when (and name
                         (eql (incf var-id) id))
               do
                 (return-from debugger-read-variable
                   (values (svref env-object i) name)))
            (setf env-object (svref env-object 0)))))))
  (format t "Unknown variable id ~S." id))

(defun debugger-write-variable (frame id value)
  (let* ((fn (function-from-frame frame))
         (info (function-debug-info fn))
         (var-id 0))
    (loop
       for (name stack-slot) in (debug-info-local-variable-locations info)
       when (eql (incf var-id) id)
       do
         (write-frame-slot frame stack-slot value)
         (return-from debugger-write-variable name))
    (multiple-value-bind (env-slot env-layout)
        (sys.int::debug-info-closure-layout info)
      (when env-slot
        (let ((env-object (read-frame-slot frame env-slot)))
          (dolist (level env-layout)
            (loop
               for i from 1
               for name in level
               when (and name
                         (eql (incf var-id) id))
               do
                 (setf (svref env-object i) value)
                 (return-from debugger-write-variable name))
            (setf env-object (svref env-object 0)))))))
  (format t "Unknown variable id ~S." id))

(defun enter-debugger (condition)
  (with-standard-io-syntax
    (let* ((*standard-input* *debug-io*)
           (*standard-output* *debug-io*)
           (debug-level *debugger-depth*)
           (*debugger-depth* (1+ *debugger-depth*))
           (restarts (compute-restarts))
           (restart-count (length restarts))
           (*debugger-condition* condition)
           (frames nil)
           (n-frames 0)
           (*current-debug-frame*))
      (let ((prev-fp nil))
        (map-backtrace
         (lambda (i fp)
           (incf n-frames)
           (push (list (1- i) fp prev-fp) frames)
           (setf prev-fp fp))))
      (setf frames (nreverse frames))
      ;; Can't deal with the top-most frame.
      (decf n-frames)
      (pop frames)
      ;; Remove a few more frames that're done.
      (setf *current-debug-frame* (first frames))
      (fresh-line)
      (write condition :escape nil :readably nil)
      (fresh-line)
      (show-restarts restarts)
      (fresh-line)
      (backtrace *default-frames-to-print*)
      (fresh-line)
      (write-line "Enter a restart number or evaluate a form. :help for help.")
      (clear-input)
      (loop
         (let ((* nil) (** nil) (*** nil)
               (/ nil) (// nil) (/// nil)
               (+ nil) (++ nil) (+++ nil)
               (- nil))
           (loop
              (with-simple-restart (abort "Return to debugger top level.")
                (fresh-line)
                (format t "~A/~D] " (package-shortest-name *package*) debug-level)
                (finish-output)
                (let ((form (read)))
                  (fresh-line)
                  (typecase form
                    (integer
                     (if (and (>= form 0) (< form restart-count))
                         (invoke-restart-interactively (nth (- restart-count form 1) restarts))
                         (format t "Restart number ~D out of bounds.~%" form)))
                    (keyword
                     (case form
                       (:help
                        (format t "~&You are in the debugger. Commiserations!~%")
                        (format t "Commands:~%")
                        (format t "  :help      This help message.~%")
                        (format t "  :restarts  Display available restarts.~%")
                        (format t "  :up        Move to a higher (inner) frame.~%")
                        (format t "  :down      Move to a lower (outer) frame.~%")
                        (format t "  :bottom    Move to the lowest (outermost) fra\me.~%")
                        (format t "  :top       Move to the highest (innermost) frame.~%")
                        (format t "  :current   Print the current frame.~%")
                        (format t "  :vars      Display variables in the current frame.~%")
                        (format t "             Beware, *print-line* and *print-level* don't work yet.~%")
                        (format t "  :read      Read a variable by id (from :vars) from the current frame.~%")
                        (format t "  :write     Write a variable by id (from :vars) to the current frame.~%")
                        (format t "  :bt        Print a complete backtrace.~%")
                        (format t "  :condition Condition that caused the debugger to be invoked.~%")
                        (format t "  :terminate Terminate the current thread.~%")
                        (format t "Integers are treated at restart IDs.~%")
                        (format t "Good luck.~%"))
                       (:restarts
                        (fresh-line)
                        (show-restarts restarts))
                       (:up
                        (if (>= (first *current-debug-frame*) n-frames)
                            (format t "At innermost frame!~%")
                            (setf *current-debug-frame* (nth (1+ (first *current-debug-frame*)) frames)))
                        (show-debug-frame))
                       (:down
                        (if (zerop (first *current-debug-frame*))
                            (format t "At outermost frame!~%")
                            (setf *current-debug-frame* (nth (1- (first *current-debug-frame*)) frames)))
                        (show-debug-frame))
                       (:bottom
                        (setf *current-debug-frame* (first frames))
                        (show-debug-frame))
                       (:top
                        (setf *current-debug-frame* (first (last frames)))
                        (show-debug-frame))
                       (:current
                        (show-debug-frame))
                       (:vars
                        (show-debug-frame)
                        (debugger-show-variables *current-debug-frame*))
                       (:read
                        (format t "~&ID: ")
                        (finish-output)
                        (let ((result (multiple-value-list (debugger-read-variable *current-debug-frame* (read)))))
                          (setf *** **
                                ** *
                                * (first result)
                                /// //
                                // /
                                / result
                                +++ ++
                                ++ +
                                + form)
                          (when result
                            (dolist (v result)
                              (fresh-line)
                              (write v)))))
                       (:write
                        (let (slot value)
                          (format t "~&ID: ")
                          (finish-output)
                          (setf slot (read))
                          (format t "~&Value: ")
                          (finish-output)
                          (setf value (eval (read)))
                          (debugger-write-variable *current-debug-frame* slot value)))
                       (:bt
                        (backtrace))
                       (:condition
                        (let ((result (multiple-value-list *debugger-condition*)))
                          (setf *** **
                                ** *
                                * (first result)
                                /// //
                                // /
                                / result
                                +++ ++
                                ++ +
                                + form)
                          (when result
                            (dolist (v result)
                              (fresh-line)
                              (write v)))))
                       (:terminate
                        (throw 'mezzano.supervisor:terminate-thread nil))
                       (t (format t "Unknown command ~S~%" form))))
                    (t (let ((result (multiple-value-list (let ((- form))
                                                            (eval form)))))
                         (setf *** **
                               ** *
                               * (first result)
                               /// //
                               // /
                               / result
                               +++ ++
                               ++ +
                               + form)
                         (when result
                           (dolist (v result)
                             (fresh-line)
                             (write v))))))))))))))

(defun show-restarts (restarts)
  (let ((restart-count (length restarts)))
    (format t "Available restarts:~%")
    (loop
       for i from 0
       for restart in restarts
       do (format t "~S ~S: ~A~%" (- restart-count i 1) (restart-name restart) restart))))

(defun map-backtrace (fn)
  (do ((i 0 (1+ i))
       (fp (read-frame-pointer)
           (memref-unsigned-byte-64 fp 0)))
      ;; Stop when return address or frame pointer is zero.
      ((or (= fp 0)
           (eql (memref-signed-byte-64 fp 1) 0)))
    (funcall fn i fp)))

(defun backtrace (&optional limit)
  (map-backtrace
   (lambda (i fp)
     (when (and limit (> i limit))
       (return-from backtrace))
     (let* ((return-address (memref-unsigned-byte-64 fp 1))
            (fn (return-address-to-function return-address))
            (name (when (functionp fn) (function-name fn))))
       (format t "~&~X ~X ~S" fp return-address name)))))

(defvar *traced-functions* '())
(defvar *trace-depth* 0)
(defvar *suppress-trace* nil)

(defmacro trace (&rest functions)
  `(%trace ,@(mapcar (lambda (f) (list 'quote f)) functions)))

(defun %trace (&rest functions)
  (dolist (fn functions)
    (when (and (not (member fn *traced-functions* :key 'car :test 'equal))
               (fboundp fn))
      (let ((name fn)
            (old-definition (fdefinition fn)))
        (push (list fn old-definition) *traced-functions*)
        (setf (fdefinition fn)
              (lambda (&rest args)
                (declare (dynamic-extent args)
                         (lambda-name trace-wrapper))
                (if *suppress-trace*
                    (apply old-definition args)
                    (let ((*suppress-trace* t))
                      (write *trace-depth* :stream *trace-output*)
                      (write-string ": Enter " *trace-output*)
                      (write name :stream *trace-output*)
                      (write-char #\Space *trace-output*)
                      (write args :stream *trace-output*)
                      (terpri *trace-output*)
                      (let ((result :error))
                        (unwind-protect
                             (handler-bind ((error (lambda (condition) (setf result condition))))
                               (setf result (multiple-value-list (let ((*trace-depth* (1+ *trace-depth*))
                                                                       (*suppress-trace* nil))
                                                                   (apply old-definition args)))))
                          (write *trace-depth* :stream *trace-output*)
                          (write-string ": Leave " *trace-output*)
                          (write name :stream *trace-output*)
                          (write-char #\Space *trace-output*)
                          (write result :stream *trace-output*)
                          (terpri *trace-output*))
                        (values-list result)))))))))
  *traced-functions*)

(defun %untrace (&rest functions)
  (if (null functions)
      (dolist (fn *traced-functions* (setf *traced-functions* '()))
        (setf (fdefinition (first fn)) (second fn)))
      (dolist (fn functions)
        (let ((x (assoc fn *traced-functions* :test 'equal)))
          (when x
            (setf *traced-functions* (delete x *traced-functions*))
            (setf (fdefinition (first x)) (second x)))))))

(defmacro untrace (&rest functions)
  `(%untrace ,@(loop for fn in functions collect `',fn)))
