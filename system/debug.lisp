;;;; The debugger and other debugging support

(in-package :mezzano.internals)

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

(defun (setf debug-info-docstring) (new-value info)
  (setf (ninth info) new-value))

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
                    (when (not (logtest b #x80))
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
                                   (#.+debug-repr-short-float+ 'short-float)
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
         (short-float
          (%integer-as-short-float (memref-unsigned-byte-16 address)))
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
         (short-float
          (setf (memref-unsigned-byte-16 address) (%short-float-as-integer value)))
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
    (when (function-reference-p fn)
      (let ((real-fn (function-reference-function fn)))
        (when (not real-fn)
          (return-from frame-locals '()))
        (setf fn real-fn
              offset 0)))
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
           (restarts (compute-restarts condition))
           (restart-count (length restarts))
           (*debugger-condition* condition)
           (frames nil)
           (n-frames 0)
           (*current-debug-frame*))
      (let ((prev-fp nil))
        (%map-backtrace
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
      (backtrace :limit *default-frames-to-print*)
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

(defparameter *global-debugger* nil)

(defun enter-debugger (condition)
  (funcall (or *global-debugger*
               'debugger-main)
           condition))

(defun show-restarts (restarts)
  (let ((restart-count (length restarts)))
    (format t "Available restarts:~%")
    (loop
       for i from 0
       for restart in restarts
       do (format t "~S ~S: ~A~%" (- restart-count i 1) (restart-name restart) restart))))

(defun %map-backtrace (fn)
  (do ((i 0 (1+ i))
       (fp (read-frame-pointer)
           (memref-unsigned-byte-64 fp 0)))
      ;; Stop when return address or frame pointer is zero.
      ((or (= fp 0)
           (eql (memref-signed-byte-64 fp 1) 0)))
    (funcall fn i fp)))

(defstruct frame
  (depth 0 :read-only t)
  (fp 0 :read-only t))

(defstruct local-variable
  (name nil :read-only t)
  location
  representation)

(defun frame-local-variables (frame &key show-hidden)
  (loop
     for (name id location repr) in (frame-locals (list 0 (frame-fp frame) nil)
                                                  :show-hidden show-hidden)
     collect (make-local-variable :name name :location location :representation repr)))

(defun local-variable-value (frame local-variable)
  (let ((value (read-frame-slot (list 0 (frame-fp frame) nil)
                                (local-variable-location local-variable)
                                (local-variable-representation local-variable))))
    (if (eql value :$inaccessible-value$)
        (values nil nil)
        (values value t))))

(defun frame-function (frame)
  "Return the function associated with frame.
Returns the function that was called, the actual function object being
executed, and the offset into it."
  (multiple-value-bind (actual-function offset)
      (function-from-frame (list nil (frame-fp frame) nil))
    (when (function-reference-p actual-function)
      (return-from frame-function
        (values actual-function actual-function offset)))
    (let ((function actual-function)
          (name (function-name actual-function)))
      ;; Poke around in some CLOS discriminating functions to get
      ;; better names.
      (when (or (and (consp name)
                     (eql (first name) 'mezzano.clos::1-effective-discriminator))
                (equal name '(lambda :in mezzano.clos::std-compute-discriminating-function))
                (equal name '(lambda :in mezzano.clos::compute-n-effective-discriminator))
                (equal name '(lambda :in mezzano.clos::compute-reader-discriminator))
                (equal name '(lambda :in mezzano.clos::compute-writer-discriminator)))
        (loop
           for var in (frame-local-variables frame)
           when (eql (local-variable-name var) 'mezzano.clos::gf)
           do
             (setf function (local-variable-value frame var))
             (return)))
      (values function actual-function offset))))

(defun print-frame (frame &key (stream *debug-io*))
  (format stream "~S" (frame-function frame)))

(defgeneric function-source-location (function &key))

(defmethod function-source-location ((function compiled-function) &key (offset 0))
  (let* ((info (function-debug-info function))
         (pathname (mezzano.internals::debug-info-source-pathname info))
         (tlf (mezzano.internals::debug-info-source-top-level-form-number info)))
    (make-source-location :file pathname
                          :top-level-form-number tlf)))

(defmethod function-source-location ((function standard-generic-function) &key offset)
  (declare (ignore offset))
  (let ((location (slot-value function 'mezzano.clos::source-location)))
    (when location
      (function-source-location location))))

(defmethod function-source-location ((function function-reference) &key offset)
  (declare (ignore offset))
  (let ((fn (function-reference-function function)))
    (when fn
      (function-source-location fn))))

(defun map-backtrace (fn)
  (%map-backtrace
   (lambda (depth fp)
     (funcall fn (make-frame :depth depth :fp fp)))))

(defun backtrace (&key (stream *debug-io*) limit)
  "Print a backtrace on STREAM."
  (%map-backtrace
   (lambda (i fp)
     (when (and limit (> i limit))
       (return-from backtrace))
     (let* ((return-address (memref-unsigned-byte-64 fp 1))
            (fn (frame-function (make-frame :depth i :fp fp)))
            (name (or (if (functionp fn) (function-name fn)) fn)))
       (format stream "~&~X ~X ~S" fp return-address name)))))

(defvar *traced-functions* '())
(defvar *trace-depth* 0)
(defvar *suppress-trace* nil)

;;; There is additional support in (setf function-reference-function) for trace wrappers.
(defclass trace-wrapper ()
  ((%name :initarg :name :reader trace-wrapper-name)
   (%original :initarg :original :accessor trace-wrapper-original)
   (%break-on-trace :initarg :break-on-trace :accessor trace-wrapper-break-on-trace))
  (:metaclass mezzano.clos:funcallable-standard-class)
  (:default-initargs :break-on-trace nil))

(defmethod print-object ((instance trace-wrapper) stream)
  (print-unreadable-object (instance stream :identity t)
    (format stream "Trace wrapper for ~A" (trace-wrapper-name instance))))

(defun trace-method-invocation (gf method function args)
  (if *suppress-trace*
      (apply function args)
      (let ((*suppress-trace* t))
        (let ((*print-readably* nil)
              (*print-length* 5)
              (*print-level* 5))
          (format *trace-output* "~D: Enter ~A ~A ~:S~%" *trace-depth* gf method args))
        (let ((result :error))
          (unwind-protect
               (handler-bind ((error (lambda (condition) (setf result condition))))
                 (setf result (multiple-value-list (let ((*trace-depth* (1+ *trace-depth*))
                                                         (*suppress-trace* nil))
                                                     (apply function args)))))
            (let ((*print-readably* nil)
                  (*print-length* 5)
                  (*print-level* 5))
              (format *trace-output* "~D: Leave ~A ~A ~:S~%" *trace-depth* gf method result)))
          (values-list result)))))

(defun trace-wrapper (name wrapper args)
  (let ((old-definition (trace-wrapper-original wrapper)))
    (if *suppress-trace*
        (apply old-definition args)
        (let ((*suppress-trace* t))
          (let ((*print-readably* nil)
                (*print-length* 5)
                (*print-level* 5))
            (format *trace-output* "~D: Enter ~A ~:S~%" *trace-depth* name args))
          (let ((bot-condition (trace-wrapper-break-on-trace wrapper)))
            (when (and bot-condition (eval bot-condition))
              (break "Trace break for ~S with args ~:S" old-definition args)))
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

(defun trace-function (name &rest options)
  (when (fboundp name)
    (pushnew name *traced-functions* :test #'equal)
    (let* ((fref (function-reference name))
           (fn (function-reference-function fref)))
      (cond ((typep fn 'trace-wrapper)
             (apply #'reinitialize-instance fn options))
            (t
             (let ((wrapper (apply #'make-instance
                                   'trace-wrapper
                                   :name name
                                   :original fn
                                   options)))
               (mezzano.clos:set-funcallable-instance-function
                wrapper
                (lambda (&rest args)
                  (declare (dynamic-extent args)
                           (lambda-name trace-wrapper))
                  (trace-wrapper name wrapper args)))
               (setf (function-reference-function fref) wrapper)))))))

(defun %trace (&rest functions)
  (loop
     with break-on-trace = nil
     with methods = nil
     while functions
     for name = (pop functions)
     do
       (cond ((eql name :break)
              (setf break-on-trace (pop functions)))
             ((eql name :methods)
              (setf methods (pop functions)))
             (t
              (trace-function name :break-on-trace break-on-trace)
              (when (and methods
                         (fboundp name))
                (let ((f (fdefinition name)))
                  (when (typep f 'standard-generic-function)
                    (setf (mezzano.clos:generic-function-trace-p f) t)))))))
  *traced-functions*)

(defun untrace-function (name)
  (let* ((fref (function-reference name))
         (fn (function-reference-function fref)))
    (setf *traced-functions* (remove name *traced-functions* :test #'equal))
    (when (typep fn 'trace-wrapper)
      (let ((original (trace-wrapper-original fn)))
        (when (typep original 'standard-generic-function)
          (setf (mezzano.clos:generic-function-trace-p original) nil))
      ;; Directly set the fref to override the existing trace wrapper.
        (setf (function-reference-function fref) original)))))

(defun %untrace (&rest functions)
  (dolist (name (or functions
                    *traced-functions*))
    (untrace-function name)))

(defmacro trace (&rest functions)
  `(%trace ,@(mapcar (lambda (f) (list 'quote f)) functions)))

(defmacro untrace (&rest functions)
  `(%untrace ,@(loop for fn in functions collect `',fn)))

(defun find-all-frefs ()
  (remove-duplicates
   (coerce (get-all-objects
            #'function-reference-p)
          'list)))

(defun get-all-frefs-in-function (function)
  (when (funcallable-instance-p function)
    (setf function (funcallable-instance-function function)))
  (when (closure-p function)
    (setf function (%closure-function function)))
  (loop
     for i below (function-pool-size function)
     for entry = (function-pool-object function i)
     when (function-reference-p entry)
     collect entry
     when (compiled-function-p entry) ; closures
     append (get-all-frefs-in-function entry)))

(defun specializer-name (specializer)
  (if (typep specializer 'standard-class)
      (mezzano.clos:class-name specializer)
      specializer))

(defun list-callers (function-designator)
  (loop
     with fref-for-fn = (function-reference function-designator)
     with callers = '()
     for fref in (find-all-frefs)
     for fn = (function-reference-function fref)
     for name = (function-reference-name fref)
     when fn
     do
       (cond ((typep fn 'standard-generic-function)
              (dolist (m (mezzano.clos:generic-function-methods fn))
                  (let* ((mf (mezzano.clos:method-function m))
                         (mf-frefs (get-all-frefs-in-function mf)))
                    (when (member fref-for-fn mf-frefs)
                      (push `((defmethod ,name
                                  ,@(mezzano.clos:method-qualifiers m)
                                ,(mapcar #'specializer-name
                                         (mezzano.clos:method-specializers m)))
                              ,(function-source-location mf))
                            callers)))))
               ((member fref-for-fn
                        (get-all-frefs-in-function fn))
                (push `((defun ,name) ,(function-source-location fn)) callers)))
     finally (return callers)))

(defun list-callees (function-designator)
  (let* ((fn (fdefinition function-designator))
         ;; Grovel around in the function's constant pool looking for
         ;; function-references.  These may be for #', but they're
         ;; probably going to be for normal calls.
         ;; TODO: This doesn't work well on interpreted functions or
         ;; funcallable instances.
         (callees (remove-duplicates (get-all-frefs-in-function fn))))
    (loop
       for fref in callees
       for name = (function-reference-name fref)
       for fn = (function-reference-function fref)
       when fn
       collect `((defun ,name) ,(function-source-location fn)))))

(defgeneric function-lambda-list (function))

(defmethod function-lambda-list ((name (eql 'block)))
  '(name &body body))

(defmethod function-lambda-list ((name (eql 'catch)))
  '(tag &body body))

(defmethod function-lambda-list ((name (eql 'eval-when)))
  '(situations &body body))

(defmethod function-lambda-list ((name (eql 'flet)))
  '(definitions &body body))

(defmethod function-lambda-list ((name (eql 'function)))
  '(name))

(defmethod function-lambda-list ((name (eql 'go)))
  '(tag))

(defmethod function-lambda-list ((name (eql 'if)))
  '(test-form then-form &optional else-form))

(defmethod function-lambda-list ((name (eql 'labels)))
  '(definitions &body body))

(defmethod function-lambda-list ((name (eql 'let)))
  '(bindings &body body))

(defmethod function-lambda-list ((name (eql 'let*)))
  '(bindings &body body))

(defmethod function-lambda-list ((name (eql 'load-time-value)))
  '(form &optional read-only-p))

(defmethod function-lambda-list ((name (eql 'locally)))
  '(&body body))

(defmethod function-lambda-list ((name (eql 'macrolet)))
  '(definitions &body body))

(defmethod function-lambda-list ((name (eql 'multiple-value-call)))
  '(function-form &rest values-forms))

(defmethod function-lambda-list ((name (eql 'multiple-value-prog1)))
  '(values-form &rest forms))

(defmethod function-lambda-list ((name (eql 'progn)))
  '(&rest forms))

(defmethod function-lambda-list ((name (eql 'progv)))
  '(symbols values &body body))

(defmethod function-lambda-list ((name (eql 'quote)))
  '(value))

(defmethod function-lambda-list ((name (eql 'return-from)))
  '(name &optional result))

(defmethod function-lambda-list ((name (eql 'setq)))
  '(&rest pairs))

(defmethod function-lambda-list ((name (eql 'symbol-macrolet)))
  '(bindings &body body))

(defmethod function-lambda-list ((name (eql 'tagbody)))
  '(&rest statements))

(defmethod function-lambda-list ((name (eql 'the)))
  '(value-type form))

(defmethod function-lambda-list ((name (eql 'throw)))
  '(tag result-form))

(defmethod function-lambda-list ((name (eql 'unwind-protect)))
  '(protected-form &body cleanup-forms))

(defmethod function-lambda-list ((name symbol))
  (cond ((macro-function name)
         (mezzano.debug:macro-function-lambda-list name))
        (t
         (function-lambda-list (fdefinition name)))))

(defmethod function-lambda-list ((name list))
  (function-lambda-list (fdefinition name)))

(defmethod function-lambda-list ((function compiled-function))
  (debug-info-lambda-list
   (function-debug-info function)))

(defmethod function-lambda-list ((function mezzano.clos:generic-function))
  (mezzano.clos:generic-function-lambda-list function))
