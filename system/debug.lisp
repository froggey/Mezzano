;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(declaim (special *debug-io*
                  *standard-input*
                  *standard-output*))

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
         (info (function-pool-object fn 1))
         (var-id 0))
    (when (and (listp info) (eql (first info) :debug-info))
      (format t "Locals:~%")
      (dolist (var (third info))
        (format t "  ~D ~S: ~S~%" (incf var-id) (first var) (read-frame-slot frame (second var))))
      (when (fourth info)
        (format t "Closed-over variables:~%")
        (let ((env-object (read-frame-slot frame (fourth info))))
          (dolist (level (fifth info))
            (do ((i 1 (1+ i))
                 (var level (cdr var)))
                ((null var))
              (when (car var)
                (format t "  ~D ~S: ~S~%" (incf var-id) (car var) (svref env-object i))))
            (setf env-object (svref env-object 0))))))))

(defun debugger-read-variable (frame id)
  (let* ((fn (function-from-frame frame))
         (info (function-pool-object fn 1))
         (var-id 0))
    (when (and (listp info) (eql (first info) :debug-info))
      (dolist (var (third info))
        (when (eql (incf var-id) id)
          (return-from debugger-read-variable
            (values (read-frame-slot frame (second var)) (first var)))))
      (when (fourth info)
        (let ((env-object (read-frame-slot frame (fourth info))))
          (dolist (level (fifth info))
            (do ((i 1 (1+ i))
                 (var level (cdr var)))
                ((null var))
              (when (and (car var)
                         (eql (incf var-id) id))
                (return-from debugger-read-variable
                  (values (svref env-object i) (car var)))))
            (setf env-object (svref env-object 0)))))))
  (format t "Unknown variable id ~S." id))

(defun debugger-write-variable (frame id value)
  (let* ((fn (function-from-frame frame))
         (info (function-pool-object fn 1))
         (var-id 0))
    (when (and (listp info) (eql (first info) :debug-info))
      (dolist (var (third info))
        (when (eql (incf var-id) id)
          (write-frame-slot frame (second var) value)
          (return-from debugger-write-variable (first var))))
      (when (fourth info)
        (let ((env-object (read-frame-slot frame (fourth info))))
          (dolist (level (fifth info))
            (do ((i 1 (1+ i))
                 (var level (cdr var)))
                ((null var))
              (when (and (car var)
                         (eql (incf var-id) id))
                (setf (svref env-object i) value)
                (return-from debugger-write-variable (car var))))
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
      (loop
         (let ((* nil) (** nil) (*** nil)
               (/ nil) (// nil) (/// nil)
               (+ nil) (++ nil) (+++ nil)
               (- nil))
           (loop
              (with-simple-restart (abort "Return to debugger top level.")
                (fresh-line)
                (format t "~D] " debug-level)
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
    (write-string "Available restarts:")(terpri)
    (do ((i 0 (1+ i))
	 (r restarts (cdr r)))
	((null r))
      (format t "~S ~S: ~A~%" (- restart-count i 1) (restart-name (car r)) (car r)))))

(defun map-backtrace (fn)
  (do ((i 0 (1+ i))
       (fp (read-frame-pointer)
           (memref-unsigned-byte-64 fp 0)))
      ((= fp 0))
    (funcall fn i fp)))

(defun backtrace (&optional limit)
  (map-backtrace
   (lambda (i fp)
     (when (and limit (> i limit))
       (return-from backtrace))
     (write-char #\Newline)
     (write-integer fp 16)
     (write-char #\Space)
     (let ((return-address (memref-unsigned-byte-64 fp 1)))
       (when (zerop return-address)
         (return-from backtrace))
       (let* ((fn (return-address-to-function return-address))
              (name (when (functionp fn) (function-name fn))))
         (write-integer (lisp-object-address fn) 16)
         (when name
           (write-char #\Space)
           (write name)))))))

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
                         (system:lambda-name trace-wrapper))
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
  `(%untrace ,(loop for fn in functions collect `',fn)))
