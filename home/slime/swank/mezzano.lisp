;;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-mezzano.lisp --- SLIME backend for Mezzano
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;

;;; Administrivia

(defpackage swank/mezzano
  (:use cl swank/backend))

(in-package swank/mezzano)

;;; swank-mop

(import-swank-mop-symbols :mezzano.clos '(:slot-definition-documentation))

(defun swank-mop:slot-definition-documentation (slot)
  (documentation slot t))

(defimplementation gray-package-name ()
  "MEZZANO.GRAY")

;;;; TCP server

(defimplementation create-socket (host port &key backlog)
  (mezzano.network.tcp:tcp-listen
   host port
   :backlog (or backlog 10)))

(defimplementation local-port (socket)
  (nth-value 1 (mezzano.network:local-endpoint socket)))

(defimplementation close-socket (socket)
  (mezzano.network.tcp:close-tcp-listener socket))

(defimplementation accept-connection (socket &key external-format
                                             buffering timeout)
  (declare (ignore buffering))
  (when (not (mezzano.sync:wait-for-objects-with-timeout timeout socket))
    ;; It's not really clear what a timeout should do, this is what SCL does.
    ;; None of the other implementations support it.
    (error "Timeout accepting connection on socket: ~S~%" socket))
  (if external-format
      (mezzano.network.tcp:tcp-accept socket
                                      :element-type 'character
                                      :external-format external-format)
      (mezzano.network.tcp:tcp-accept socket
                                      :element-type '(unsigned-byte 8))))

(defimplementation preferred-communication-style ()
  :spawn)

;;;; Unix signals
;;;; ????

(defimplementation getpid ()
  0)

;;;; Packages

(defimplementation package-local-nicknames (package)
  (mezzano.extensions:package-local-nicknames package))

;;;; Compilation

(defun signal-compiler-condition (condition severity)
  (signal 'compiler-condition
          :original-condition condition
          :severity severity
          :message (format nil "~A" condition)
          :location nil))

(defimplementation call-with-compilation-hooks (func)
  (handler-bind
      ((error
        (lambda (c)
          (signal-compiler-condition c :error)))
       (warning
        (lambda (c)
          (signal-compiler-condition c :warning)))
       (style-warning
        (lambda (c)
          (signal-compiler-condition c :style-warning))))
    (funcall func)))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore buffer line column policy))
  (let* ((*load-pathname* (ignore-errors (pathname filename)))
         (*load-truename* (when *load-pathname*
                            (ignore-errors (truename *load-pathname*))))
         (mezzano.internals::*top-level-form-number* `(:position ,position)))
    (with-compilation-hooks ()
      (eval (read-from-string (concatenate 'string "(progn " string " )")))))
  t)

(defimplementation swank-compile-file (input-file output-file load-p
                                                  external-format
                                                  &key policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (multiple-value-prog1
        (compile-file input-file
                      :output-file output-file
                      :external-format external-format)
      (when load-p
        (load output-file)))))

(defimplementation find-external-format (coding-system)
  (if (or (equal coding-system "utf-8")
          (equal coding-system "utf-8-unix"))
      :utf-8
      nil))

;;;; Debugging

;; Definitely don't allow this.
(defimplementation install-debugger-globally (function)
  (declare (ignore function))
  nil)

(defvar *current-backtrace*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*current-backtrace* '()))
    (mezzano.debug:map-backtrace
     (lambda (frame)
       (push frame *current-backtrace*)))
    (setf *current-backtrace* (reverse *current-backtrace*))
    ;; Drop the topmost frame, which is finished call to MAP-BACKTRACE.
    (pop *current-backtrace*)
    ;; And the next one for good measure.
    (pop *current-backtrace*)
    (funcall debugger-loop-fn)))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((mezzano.debug:*global-debugger* (lambda (cond) (funcall hook cond nil)))
        (*debugger-hook* hook))
    (funcall fun)))

(defimplementation compute-backtrace (start end)
  (subseq *current-backtrace* start end))

(defimplementation print-frame (frame stream)
  (mezzano.debug:print-frame frame :stream stream))

(defimplementation frame-source-location (frame-number)
  (let* ((frame (nth frame-number *current-backtrace*))
         (fn (mezzano.debug:frame-function frame)))
    (function-location fn)))

(defimplementation frame-locals (frame-number)
  (loop
     with frame = (nth frame-number *current-backtrace*)
     for id = 0
     for var in (mezzano.debug:frame-local-variables frame)
     collect (list :name (mezzano.debug:local-variable-name var)
                   :id id
                   :value (multiple-value-bind (value validp)
                              (mezzano.debug:local-variable-value frame var)
                            (if validp value :<not-available>)))))

(defimplementation frame-var-value (frame-number var-id)
  (let* ((frame (nth frame-number *current-backtrace*))
         (locals (mezzano.debug:frame-local-variables frame))
         (var (nth var-id locals)))
    (assert var () "Invalid variable id ~D for frame number ~D."
             var-id frame-number)
    (multiple-value-bind (value validp)
        (mezzano.debug:local-variable-value frame var)
      (if validp value :<not-available>))))

;;;; Definition finding

(defun top-level-form-position (pathname tlf)
  (ignore-errors
    (with-open-file (s pathname)
      (loop
         repeat tlf
         do (with-standard-io-syntax
              (let ((*read-suppress* t)
                    (*read-eval* nil))
                (read s nil))))
      (let ((default (make-pathname :host (pathname-host s))))
        (make-location `(:file ,(enough-namestring s default))
                       `(:position ,(1+ (file-position s))))))))

(defun mezzano-location->swank-location (location)
  (let ((pathname (mezzano.debug:source-location-file location))
        (tlf (mezzano.debug:source-location-top-level-form-number location)))
    (cond ((and (consp tlf)
                (eql (first tlf) :position))
           (let ((default (make-pathname :host (pathname-host pathname))))
             (make-location `(:file ,(enough-namestring pathname default))
                            `(:position ,(second tlf)))))
          (t
           (top-level-form-position pathname tlf)))))

(defun function-location (function)
  "Return a location object for FUNCTION."
  (mezzano-location->swank-location
   (mezzano.debug:function-source-location function)))

(defimplementation find-definitions (name)
  (loop
     for (dspec mloc) in (mezzano.extensions:find-definitions name)
     for sloc = (mezzano-location->swank-location mloc)
     when sloc
     collect (list dspec sloc)))

;;;; XREF
;;; Simpler variants.

(defimplementation list-callers (function-name)
  (loop
     for (dspec location) in (mezzano.debug:list-callers function-name)
     collect (list dspec (mezzano-location->swank-location location))))

(defimplementation list-callees (function-name)
  (loop
     for (dspec location) in (mezzano.debug:list-callees function-name)
     collect (list dspec (mezzano-location->swank-location location))))

;;;; Trace

(defimplementation toggle-trace (spec)
  (cond ((and (consp spec)
              (eql (first spec) :defgeneric))
         ;; Tracing a generic function and all methods
         (cond ((member spec (eval '(trace)) :test #'equal) ;; check if traced
                (eval `(untrace ,spec))
                (format nil "~S is now untraced." spec))
               (t
                (eval `(trace :methods t ,spec))
                (format nil "~S and all methods are now traced." spec))))
        ((or (symbolp spec)
             (and (consp spec)
                  (member (first spec) '(setf mezzano.extensions:cas))))
         (cond ((member spec (eval '(trace)) :test #'equal) ;; check if traced
                (eval `(untrace ,spec))
                (format nil "~S is now untraced." spec))
               (t
                (eval `(trace ,spec))
                (format nil "~S is now traced." spec))))
        (t
         (format nil "Unsupported trace specifier ~S." spec))))

;;;; Documentation

(defimplementation arglist (name)
  (let ((macro (when (symbolp name)
                 (macro-function name)))
        (fn (if (functionp name)
                name
                (ignore-errors (fdefinition name)))))
    (cond
      (macro
       (mezzano.debug:macro-function-lambda-list name))
      (fn
       (cond
         ((typep fn 'mezzano.clos:standard-generic-function)
          (mezzano.clos:generic-function-lambda-list fn))
         (t
          (mezzano.debug:function-lambda-list fn))))
      (t :not-available))))

(defimplementation type-specifier-p (symbol)
  (or (mezzano.internals::type-specifier-p symbol)
      :not-available))

(defimplementation function-name (function)
  (mezzano.debug:function-name function))

(defimplementation valid-function-name-p (form)
  "Is FORM syntactically valid to name a function?
   If true, FBOUNDP should not signal a type-error for FORM."
  (flet ((length=2 (list)
           (and (not (null (cdr list))) (null (cddr list)))))
    (or (symbolp form)
        (and (consp form) (length=2 form)
             (or (eq (first form) 'setf)
                 (eq (first form) 'mezzano.extensions:cas))
             (symbolp (second form))))))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (when (boundp symbol)
      (setf (getf result :variable) (documentation symbol 'variable)))
    (when (and (fboundp symbol)
               (not (macro-function symbol)))
      (setf (getf result :function)
            (documentation symbol 'function)))
    (when (or (fboundp `(setf ,symbol))
              (mezzano.extensions:setf-expander-function symbol))
      (setf (getf result :setf)
            (documentation symbol 'setf)))
    (when (special-operator-p symbol)
      (setf (getf result :special-operator)
            (documentation symbol 'function)))
    (when (macro-function symbol)
      (setf (getf result :macro)
            (documentation symbol 'function)))
    (when (compiler-macro-function symbol)
      (setf (getf result :compiler-macro)
            (documentation symbol 'compiler-macro)))
    (when (type-specifier-p symbol)
      (setf (getf result :type)
            (documentation symbol 'type)))
    (when (find-class symbol nil)
      (setf (getf result :class)
            (documentation symbol 'type)))
    result))


;;;; Multithreading

;; Weak :KEY-OR-VALUE as this contains both id->thread and thread->id mappings.
(defvar *thread-ids-for-emacs* (make-hash-table :weakness :key-or-value))
(defvar *next-thread-id-for-emacs* 0)
(defvar *thread-id-for-emacs-lock* (mezzano.supervisor:make-mutex
                                    "SWANK thread ID table"))

(defimplementation spawn (fn &key name)
  (mezzano.supervisor:make-thread fn :name name))

(defimplementation thread-id (thread)
  (mezzano.supervisor:with-mutex (*thread-id-for-emacs-lock*)
    (let ((id (gethash thread *thread-ids-for-emacs*)))
      (when (null id)
        (setf id (incf *next-thread-id-for-emacs*)
              (gethash thread *thread-ids-for-emacs*) id
              (gethash id *thread-ids-for-emacs*) thread))
      id)))

(defimplementation find-thread (id)
  (mezzano.supervisor:with-mutex (*thread-id-for-emacs-lock*)
    (gethash id *thread-ids-for-emacs*)))

(defimplementation thread-name (thread)
  (mezzano.supervisor:thread-name thread))

(defimplementation thread-status (thread)
  (format nil "~:(~A~)" (mezzano.supervisor:thread-state thread)))

(defimplementation current-thread ()
  (mezzano.supervisor:current-thread))

(defimplementation all-threads ()
  (mezzano.supervisor:all-threads))

(defimplementation thread-alive-p (thread)
  (not (eql (mezzano.supervisor:thread-state thread) :dead)))

(defimplementation interrupt-thread (thread fn)
  (mezzano.supervisor:establish-thread-foothold thread fn))

(defimplementation kill-thread (thread)
  ;; Documentation says not to execute unwind-protected sections, but there's
  ;; no way to do that.
  ;; And killing threads at arbitrary points without unwinding them is a good
  ;; way to hose the system.
  (mezzano.supervisor:terminate-thread thread))

(defvar *mailbox-lock* (mezzano.supervisor:make-mutex "mailbox lock"))
;; This must be weak so that dead threads aren't held on to forever.
(defvar *mailboxes* (make-hash-table :weakness :key))

(defstruct (mailbox (:conc-name mailbox.))
  thread
  (mutex (mezzano.supervisor:make-mutex "slime mailbox lock"))
  (cvar (mezzano.supervisor:make-condition-variable "slime mailbox cvar"))
  (queue '() :type list))

(defun mailbox (thread)
  "Return THREAD's mailbox."
  (mezzano.supervisor:with-mutex (*mailbox-lock*)
    (let ((entry (gethash thread *mailboxes*)))
      (when (not entry)
        (setf entry (make-mailbox :thread thread)
              (gethash thread *mailboxes*) entry))
      entry)))

(defimplementation wake-thread (thread)
  ;; WAKE-THREAD is currently only called via INTERRUPT-THREAD by
  ;; QUEUE-THREAD-INTERRUPT and executed by the thread itself. No
  ;; work is actually required to pull the thread out of the
  ;; CONDITION-WAIT call in RECEIVE-IF. Interrupting a thread
  ;; that's waiting on a cvar will cause a spurious wakeup.
  ;; However, we should hit the condition variable anyway just in
  ;; case WAKE-THREAD starts being called from other places in
  ;; the future.
  (when (not (eql (mezzano.supervisor:current-thread) thread))
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox))
           (cvar (mailbox.cvar mbox)))
      (mezzano.supervisor:with-mutex (mutex)
        (mezzano.supervisor:condition-notify cvar t)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    (mezzano.supervisor:with-mutex (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message)))
      (mezzano.supervisor:condition-notify (mailbox.cvar mbox) t))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox (current-thread)))
         (mutex (mailbox.mutex mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
       (check-slime-interrupts)
       (mezzano.supervisor:with-mutex (mutex)
         (let* ((q (mailbox.queue mbox))
                (tail (member-if test q)))
           (when tail
             (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
             (return (car tail))))
         (when (eq timeout t) (return (values nil t)))
         (mezzano.supervisor:condition-wait (mailbox.cvar mbox) mutex)))))

(defvar *registered-threads* (make-hash-table))
(defvar *registered-threads-lock*
  (mezzano.supervisor:make-mutex "registered threads lock"))

(defimplementation register-thread (name thread)
  (declare (type symbol name))
  (mezzano.supervisor:with-mutex (*registered-threads-lock*)
    (etypecase thread
      (null
       (remhash name *registered-threads*))
      (mezzano.supervisor:thread
       (setf (gethash name *registered-threads*) thread))))
  nil)

(defimplementation find-registered (name)
  (mezzano.supervisor:with-mutex (*registered-threads-lock*)
    (values (gethash name *registered-threads*))))

(defimplementation wait-for-input (streams &optional timeout)
  (loop
       (let ((ready '()))
         (dolist (s streams)
           (when (or (listen s)
                     (and (typep s 'mezzano.network.tcp::tcp-stream)
                          (mezzano.network.tcp::tcp-connection-closed-p s)))
             (push s ready)))
         (when ready
           (return ready))
         (when (check-slime-interrupts)
           (return :interrupt))
         (when timeout
           (return '()))
         (sleep 1)
         (when (numberp timeout)
           (decf timeout 1)
           (when (not (plusp timeout))
             (return '()))))))

;;;;  Locks

(defstruct recursive-lock
  mutex
  (depth 0))

(defimplementation make-lock (&key name)
  (make-recursive-lock
   :mutex (mezzano.supervisor:make-mutex name)))

(defimplementation call-with-lock-held (lock function)
  (cond ((mezzano.supervisor:mutex-held-p
          (recursive-lock-mutex lock))
         (unwind-protect
              (progn (incf (recursive-lock-depth lock))
                     (funcall function))
           (decf (recursive-lock-depth lock))))
        (t
         (mezzano.supervisor:with-mutex ((recursive-lock-mutex lock))
           (multiple-value-prog1
               (funcall function)
             (assert (eql (recursive-lock-depth lock) 0)))))))

;;;; Weak datastructures

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weakness :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  "Like MAKE-HASH-TABLE, but weak w.r.t. the values."
  (apply #'make-hash-table :weakness :value args))

(defimplementation hash-table-weakness (hashtable)
  "Return nil or one of :key :value :key-or-value :key-and-value"
  (mezzano.extensions:hash-table-weakness hashtable))

;;;; Character names

(defimplementation character-completion-set (prefix matchp)
  ;; TODO: Unicode characters too.
  (loop
     for names in mezzano.internals::*char-name-alist*
     append
       (loop
          for name in (rest names)
          when (funcall matchp prefix name)
          collect name)))

;;;; Inspector

(defmethod emacs-inspect ((o function))
  (case (mezzano.internals::%object-tag o)
    (#.mezzano.internals::+object-tag-function+
     (append
      (label-value-line*
       (:name (mezzano.internals::function-name o))
       (:arglist (arglist o))
       (:debug-info (mezzano.internals::function-debug-info o)))
      `("Constants:" (:newline))
      (loop for i below (mezzano.internals::function-pool-size o)
         append (label-value-line i (mezzano.internals::function-pool-object o i)))
      `("Code:" (:newline)
                ,(with-output-to-string (*standard-output*)
                   (let ((*print-lines* nil))
                     (disassemble o))))))
    (#.mezzano.internals::+object-tag-closure+
     (append
      (label-value-line :function (mezzano.internals::%closure-function o))
      `("Closed over values:" (:newline))
      (loop
         for i below (mezzano.internals::%closure-length o)
         append (label-value-line i (mezzano.internals::%closure-value o i)))))
    (t
     (call-next-method))))

(defmethod emacs-inspect ((o mezzano.extensions:weak-pointer))
  (multiple-value-bind (key value livep)
      (mezzano.extensions:weak-pointer-pair o)
    (if livep
        '("Dead")
        (label-value-line*
         (:key key)
         (:value value)))))

(defmethod emacs-inspect ((o mezzano.internals::function-reference))
  (label-value-line*
   (:name (mezzano.internals::function-reference-name o))
   (:function (mezzano.internals::function-reference-function o))))
