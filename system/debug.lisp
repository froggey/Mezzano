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
  (if (integerp (fourth info))
      (values (fourth info) (fifth info))
      nil))

(defun debug-info-precise-closure-layout (info)
  "Returns two values, the name of the variable containing the closure vector and the layout of the vector.
Returns NIL if the function captures no variables."
  (if (symbolp (fourth info))
      (values (fourth info) (fifth info))
      nil))

(defun debug-info-source-pathname (info)
  (sixth info))

(defun debug-info-source-top-level-form-number (info)
  (seventh info))

(defun debug-info-lambda-list (info)
  (eighth info))

(defun debug-info-docstring (info)
  (ninth info))

(defun debug-info-precise-variable-data (info)
  (tenth info))

(defparameter *debugger-depth* 0)
(defvar *debugger-condition* nil)
(defvar *current-debug-frame* nil)

(defvar *default-frames-to-print* 15)

(defun function-from-frame (frame)
  ;; Decrement the return address by one to point at the calls
  ;; instead of the following instruction.
  (let ((return-address (1- (memref-signed-byte-64 (second frame) 1))))
    (multiple-value-bind (fn offset)
        (return-address-to-function return-address)
      ;; HACK, replace RAISE-INVALID-ARGUMENT-ERROR with the real caller.
      (when (eql fn #'raise-invalid-argument-error)
        (loop
           for (name location repr . plist) in (local-variables-at-offset fn offset)
           when (eql name 'function)
           do
             (setf fn (read-frame-slot frame location repr)
                   offset 0)
             (return)))
      (values fn offset))))

(defun decode-debug-register (reg)
  #+x86-64
  (elt *debug-x86-64-register-encodings* reg)
  #+arm64
  (elt *debug-arm64-register-encodings* reg))

(defun decode-precise-debug-info (function encoded-info)
  (let ((result '())
        (index 0))
    (labels ((consume ()
               (prog1
                   (let ((value (aref encoded-info index)))
                     value)
                 (incf index)))
             (read-vu32 ()
               (let ((shift 0)
                     (result 0))
                 (loop
                    (let ((b (consume)))
                      (setf result (logior result (ash (logand b #x7F) shift)))
                    (unless (logtest b #x80)
                      (return))
                    (incf shift 7)))
                 result))
             (read-location ()
               (let* ((loc (consume))
                      (real-loc (cond ((eql loc #xFF)
                                       (read-vu32))
                                      ((logtest loc #x80)
                                       (logand loc #x7F))
                                      (t
                                       (decode-debug-register loc))))
                      (repr (read-vu32))
                      (real-repr (ecase repr
                                   (#.+debug-repr-value+ :value)
                                   (#.+debug-repr-single-float+ 'single-float)
                                   (#.+debug-repr-double-float+ 'double-float)
                                   (#.+debug-repr-mmx-vector+ 'mezzano.simd:mmx-vector)
                                   (#.+debug-repr-sse-vector+ 'mezzano.simd:sse-vector)
                                   (#.+debug-repr-fixnum+ 'fixnum)
                                   (#.+debug-repr-unsigned-byte-64+ :unsigned-byte-64)
                                   (#.+debug-repr-signed-byte-64+ :signed-byte-64))))
                 (values real-loc real-repr))))
      (loop
         (when (>= index (length encoded-info))
           (return))
         (let ((offset (read-vu32))
               (compressed-layout '()))
           (loop
              (let ((op (consume)))
                (ecase (logand op #xF0)
                  (#.+debug-end-entry-op+
                   (return))
                  (#.+debug-add-var-op+
                   (let ((var (function-pool-object function (read-vu32)))
                         (hiddenp (eql op +debug-add-hidden-var-op+)))
                     (multiple-value-bind (location repr)
                         (read-location)
                       (push `(:add ,var ,location ,repr :hidden ,hiddenp)
                             compressed-layout))))
                  (#.+debug-drop-n-op+
                   (push `(:drop ,(logand op #x0F)) compressed-layout))
                  (#.+debug-drop-op+
                   (push `(:drop ,(read-vu32)) compressed-layout))
                  ((#.+debug-update-n-op+ #.+debug-update-op+)
                   (let ((var (if (eql op +debug-update-op+)
                                  (read-vu32)
                                  (logand op #x0F))))
                     (multiple-value-bind (location repr)
                         (read-location)
                       (push `(:update ,var ,location ,repr) compressed-layout)))))))
           (push `(,offset :compressed-layout ,(reverse compressed-layout)) result)))
      (reverse result))))

(defun decompress-debug-layout (prev-layout ops)
  (let ((current (copy-list prev-layout)))
    (dolist (op ops)
      (ecase (first op)
        (:add
         (setf current (append current
                               (list (rest op)))))
        (:drop
         (destructuring-bind (n) (rest op)
           (setf current (subseq current 0 n))))
        (:update
         (destructuring-bind (index loc repr) (rest op)
           (let ((existing (nth index current)))
             (setf (nth index current) (list* (first existing)
                                              loc repr
                                              (cdddr existing))))))))
    current))

(defun decompress-precise-debug-info (info)
  (loop
     with last-layout = ()
     for entry in info
     collect (destructuring-bind (offset &key compressed-layout)
                 entry
               (let ((new-layout (decompress-debug-layout last-layout compressed-layout)))
                 (setf last-layout new-layout)
                 `(,offset :layout ,new-layout)))))

(defun read-frame-slot (frame slot &optional (repr :value))
  (typecase slot
    (integer
     ;; Stack frame index.
     (let ((address (+ (memref-unsigned-byte-64 (second frame) 0)
                       (* (- (1+ slot)) 8))))
       (case repr
         ((:value fixnum)
          (memref-t address))
         (:unsigned-byte-64
          (memref-unsigned-byte-64 address))
         (:signed-byte-64
          (memref-signed-byte-64 address))
         (single-float
          (%integer-as-single-float (memref-unsigned-byte-32 address)))
         (double-float
          (%integer-as-double-float (memref-unsigned-byte-64 address)))
         (t
          :$inaccessible-value$))))
    ((cons (eql quote))
     ;; Constant.
     (second slot))
    (cons
     ;; Environment vector path.
     (case repr
       (:value
        (multiple-value-bind (value failurep)
            (ignore-errors
              (let ((value (read-frame-slot frame (first slot))))
                (dolist (index (rest slot))
                  (setf value (elt value index)))
                value))
          ;; HACK: Work around a compiler bug. Sometimes the environment gets overwritten.
          (if failurep
              :$inaccessible-value$
              value)))
       (t
        :$inaccessible-value$)))
    (t
     ;; A register?
     :$inaccessible-value$)))

(defun write-frame-slot (frame slot value &optional (repr :value))
  (typecase slot
    (integer
     ;; Stack frame index.
     (let ((address (+ (memref-unsigned-byte-64 (second frame) 0)
                       (* (- (1+ slot)) 8))))
       (case repr
         ((:value fixnum)
          (setf (memref-t address) value))
         (:unsigned-byte-64
          (setf (memref-unsigned-byte-64 address) value))
         (:signed-byte-64
          (setf (memref-signed-byte-64 address) value))
         (single-float
          (setf (memref-unsigned-byte-32 address) (%single-float-as-integer value)))
         (double-float
          (setf (memref-unsigned-byte-64 address) (%double-float-as-integer value)))
         (t
          (error "Cannot write to this variable. Unknown representation ~S." repr)))))
    ((cons (eql quote))
     (error "Cannot write to this variable. Is constant."))
    (cons
     ;; Environment vector path.
     ;; Looks like: (environment-slot [parent-indices...] value-index)
     (case repr
       (:value
        (loop
           for (x . xs) on slot
           for vec = (read-frame-slot frame x) then (if xs (elt vec x) vec)
           when (null xs) do
             (setf (elt vec x) value)
             (loop-finish))
        value)
       (t
        (error "Cannot write to this variable. Unsupported representation ~S for environment access" repr))))
    (t
     ;; A register?
     (error "Cannot write to this variable. Unsupported location ~S" slot))))

(defun show-debug-frame ()
  (multiple-value-bind (function offset)
      (function-from-frame *current-debug-frame*)
    (format t "Frame ~D(~X): ~S + ~D~%"
            (first *current-debug-frame*)
            (second *current-debug-frame*)
            function offset)))

(defun local-variables-at-offset (function offset)
  (let ((data (decompress-precise-debug-info
               (decode-precise-debug-info
                function
                (debug-info-precise-variable-data
                 (function-debug-info function))))))
    (when (null data)
      ;; Precise information not present. Fall back on the stack map.
      (return-from local-variables-at-offset
        (loop
           for (name stack-slot) in (debug-info-local-variable-locations
                                     (function-debug-info function))
           collect (list name stack-slot :value))))
    (loop
       with last-layout = '()
       for entry in data
       do (destructuring-bind (entry-offset &key layout &allow-other-keys)
              entry
            (when (> entry-offset offset)
              (return last-layout))
            (setf last-layout layout))
       finally (return last-layout))))

(defun frame-locals (frame &key show-hidden)
  (multiple-value-bind (fn offset)
      (function-from-frame frame)
    (let* ((info (function-debug-info fn))
           (var-id -1)
           (result '())
           (locals (local-variables-at-offset fn offset)))
      (loop
         for (name location repr . plist) in locals
         do (destructuring-bind (&key hidden)
                plist
              (when (or show-hidden (not hidden))
                (push (list name (incf var-id) location repr) result))))
      (multiple-value-bind (env-slot env-layout)
          (sys.int::debug-info-closure-layout info)
        (when env-slot
          (let ((base-path (list env-slot)))
            (dolist (level env-layout)
              (loop
                 for i from 1
                 for name in level
                 when name
                 do (push (list name (incf var-id) (reverse (list* i base-path)) :value) result))
              (push 0 base-path)))))
      (multiple-value-bind (env-var env-layout)
          (sys.int::debug-info-precise-closure-layout info)
        (when env-var
          (let ((env-slot (second (find env-var locals :key #'first))))
            (when env-slot
              (let ((base-path (list env-slot)))
                (dolist (level env-layout)
                  (loop
                     for i from 1
                     for name in level
                     when name
                     do (push (list name (incf var-id) (reverse (list* i base-path)) :value) result))
                  (push 0 base-path)))))))
      (reverse result))))

(defun debugger-show-variables (frame)
  (loop
     for (name id location repr) in (frame-locals frame)
     do (format t "  ~D ~S: ~S~%" id name (read-frame-slot frame location repr))))

(defun debugger-read-variable (frame id)
  (let ((info (find id (frame-locals frame) :key #'second)))
    (cond (info
           (destructuring-bind (name id location repr)
               info
             (declare (ignore id))
             (values (read-frame-slot frame location repr) name)))
          (t
           (format t "Unknown variable id ~S." id)))))

(defun debugger-write-variable (frame id value)
  (let ((info (find id (frame-locals frame) :key #'second)))
    (cond (info
           (destructuring-bind (name id location repr)
               info
             (declare (ignore id))
             (write-frame-slot frame location value repr)
             name))
          (t
           (format t "Unknown variable id ~S." id)))))

(defun debugger-main (&optional condition)
  (with-standard-io-syntax
    (let* ((*print-readably* nil)
           (*standard-input* *debug-io*)
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

(defparameter *the-debugger* 'debugger-main)

(defun enter-debugger (condition)
  (funcall (or *the-debugger*
               'debugger-main)
           condition))

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
            (fn (function-from-frame (list nil fp nil)))
            (name (when (functionp fn) (function-name fn))))
       (format t "~&~X ~X ~S" fp return-address name)))))

(defvar *traced-functions* '())
(defvar *trace-depth* 0)
(defvar *suppress-trace* nil)

;;; There is additional support in (setf function-reference-function) for trace wrappers.
(defclass trace-wrapper ()
  ((%name :initarg :name :reader trace-wrapper-name)
   (%original :initarg :original :accessor trace-wrapper-original))
  (:metaclass mezzano.clos:funcallable-standard-class))

(defmethod print-object ((instance trace-wrapper) stream)
  (print-unreadable-object (instance stream :identity t)
    (format stream "Trace wrapper for ~A" (trace-wrapper-name instance))))

(defun trace-wrapper (name wrapper args)
  (let ((old-definition (trace-wrapper-original wrapper)))
    (if *suppress-trace*
        (apply old-definition args)
        (let ((*suppress-trace* t))
          (let ((*print-readably* nil)
                (*print-length* 5)
                (*print-level* 5))
            (format *trace-output* "~D: Enter ~A ~:S~%" *trace-depth* name args))
          (let ((result :error))
            (unwind-protect
                 (handler-bind ((error (lambda (condition) (setf result condition))))
                   (setf result (multiple-value-list (let ((*trace-depth* (1+ *trace-depth*))
                                                           (*suppress-trace* nil))
                                                       (apply old-definition args)))))
              (let ((*print-readably* nil)
                    (*print-length* 5)
                    (*print-level* 5))
                (format *trace-output* "~D: Leave ~A ~:S~%" *trace-depth* name result)))
            (values-list result))))))

(defun %trace (&rest functions)
  (dolist (name functions)
    (when (fboundp name)
      (pushnew name *traced-functions* :test #'equal)
      (let* ((fref (function-reference name))
             (fn (function-reference-function fref)))
        (unless (typep fn 'trace-wrapper)
          (let ((wrapper (make-instance 'trace-wrapper
                                        :name name
                                        :original fn)))
            (mezzano.clos:set-funcallable-instance-function
             wrapper
             (lambda (&rest args)
               (declare (dynamic-extent args)
                        (lambda-name trace-wrapper))
               (trace-wrapper name wrapper args)))
            (setf (function-reference-function fref) wrapper))))))
  *traced-functions*)

(defun untrace-function (name)
  (let* ((fref (function-reference name))
         (fn (function-reference-function fref)))
    (setf *traced-functions* (remove name *traced-functions* :test #'equal))
    (when (typep fn 'trace-wrapper)
      ;; Directly set the fref to override the existing trace wrapper.
      (setf (function-reference-function fref) (trace-wrapper-original fn)))))

(defun %untrace (&rest functions)
  (dolist (name (or functions
                    *traced-functions*))
    (untrace-function name)))

(defmacro trace (&rest functions)
  `(%trace ,@(mapcar (lambda (f) (list 'quote f)) functions)))

(defmacro untrace (&rest functions)
  `(%untrace ,@(loop for fn in functions collect `',fn)))
