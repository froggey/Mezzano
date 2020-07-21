;;;; Low-level debugging output for the supervisor.

(in-package :mezzano.supervisor)

;; The debug stream functions use line-buffered output, but output can
;; be forced using the DEBUG-FORCE-OUTPUT function.

;; The debug pseudostream must be a function that takes one or two arguments.
;; The first argument is a keyword specifying the operation,
;; the second argument is the data for the operation.
;; Operations:
;;     :read-char     Read a character from the stream.
;;     :clear-input   Clear any available input.
;;     :write-char    Write a character to the stream.
;;     :write-string  Write a string to the stream.
;;     :force-output  Same as CL:FORCE-OUTPUT.
(sys.int::defglobal *debug-pseudostream*)

;; Log buffer defined by the cold-generator.
;; Must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
(sys.int::defglobal sys.int::*supervisor-log-buffer*)
(sys.int::defglobal *supervisor-log-buffer-position*)

(defun initialize-debug-log ()
  (setf *debug-pseudostream* (lambda (&rest ignored) (declare (ignore ignored))))
  (cond ((boundp '*supervisor-log-buffer-position*)
         (debug-log-buffer-write-char #\Newline))
        (t
         (setf *supervisor-log-buffer-position* 0))))

(defun debug-set-output-pseudostream (pseudostream)
  (setf *debug-pseudostream* pseudostream))

(defmacro call-debug-pseudostream (command &rest arguments)
  `(when (boundp '*debug-pseudostream*)
     (funcall *debug-pseudostream*
              ,command ,@arguments)))

(defun debug-read-char ()
  (call-debug-pseudostream :read-char))

(defun debug-clear-input ()
  (call-debug-pseudostream :clear-input))

(defmacro with-utf-8-bytes ((char byte) &body body)
  (let ((code (gensym "CODE")))
    `(let ((,code (char-code ,char)))
       (cond
        ((< ,code #x80)
         (let ((,byte ,code))
           ,@body))
        ((< ,code #x800)
         (let ((,byte (logior #b11000000 (ldb (byte 5 6) ,code))))
           ,@body)
         (let ((,byte (logior #b10000000 (ldb (byte 6 0) ,code))))
           ,@body))
        ((< ,code #x10000)
         (let ((,byte (logior #b11100000 (ldb (byte 4 12) ,code))))
           ,@body)
         (let ((,byte (logior #b10000000 (ldb (byte 6 6) ,code))))
           ,@body)
         (let ((,byte (logior #b10000000 (ldb (byte 6 0) ,code))))
           ,@body))
        (t
         (let ((,byte (logior #b11110000 (ldb (byte 3 18) ,code))))
           ,@body)
         (let ((,byte (logior #b10000000 (ldb (byte 6 12) ,code))))
           ,@body)
         (let ((,byte (logior #b10000000 (ldb (byte 6 6) ,code))))
           ,@body)
         (let ((,byte (logior #b10000000 (ldb (byte 6 0) ,code))))
           ,@body))))))

;; FIXME: Needs to be done under lock.
(defun debug-log-buffer-write-byte (byte)
  (setf (sys.int::%object-ref-unsigned-byte-8 sys.int::*supervisor-log-buffer*
                                              *supervisor-log-buffer-position*)
        byte)
  (setf *supervisor-log-buffer-position* (rem (1+ *supervisor-log-buffer-position*)
                                              (sys.int::%object-header-data
                                               sys.int::*supervisor-log-buffer*))))

(defun debug-log-buffer-write-char (char)
  (with-utf-8-bytes (char byte)
    (debug-log-buffer-write-byte byte)))

(defun debug-write-string (string &optional buf)
  (cond (buf
         (dotimes (i (string-length string))
           (debug-write-char (char string i) buf)))
        (t
         (dotimes (i (string-length string))
           (debug-log-buffer-write-char (char string i)))
         (call-debug-pseudostream :write-string string))))

(defun debug-write-char (char &optional buf)
  (cond (buf
         (let ((buf-data (car buf)))
           ;; To get inline wired accessors....
           (declare (type (simple-array (unsigned-byte 8) (*)) buf-data)
                    (optimize speed (safety 0)))
           (let ((len (length buf-data)))
             (with-utf-8-bytes (char byte)
               (let ((current (cdr buf)))
                 (when (>= current len)
                   (debug-flush-buffer buf)
                   (setf current 0))
                 (setf (aref buf-data (the fixnum current)) byte)
                 (setf (cdr buf) (1+ current)))))))
        (t
         (debug-log-buffer-write-char char)
         (call-debug-pseudostream :write-char char))))

;;; Print a negative fixnum. Use negative numbers to avoid problems
;;; near most-negative-fixnum.
(defun debug-write-fixnum-1 (fixnum base buf)
  (unless (zerop fixnum)
    (multiple-value-bind (quot rem)
        (truncate fixnum base)
      (debug-write-fixnum-1 quot base buf)
      (debug-write-char (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- rem)) buf))))

(defun debug-write-fixnum (fixnum &optional (base 16) buf)
  (cond ((plusp fixnum)
         (debug-write-fixnum-1 (- fixnum) base buf))
        ((minusp fixnum)
         (debug-write-char #\- buf)
         (debug-write-fixnum-1 fixnum base buf))
        (t (debug-write-char #\0 buf))))

(defun debug-write (thing &optional (depth 0) buf)
  (when (> depth 10)
    (debug-write-string "#<recursion-limit-exceeded>" buf)
    (return-from debug-write))
  (cond ((sys.int::character-array-p thing)
         (debug-write-string thing buf))
        ((symbolp thing)
         (debug-write-string (symbol-name thing) buf))
        ((sys.int::fixnump thing)
         (debug-write-fixnum thing 16 buf))
        ((consp thing)
         (debug-write-string "(" buf)
         (debug-write (first thing) (1+ depth) buf)
         (do ((i (rest thing) (rest i)))
             ((not (consp i))
              (when i
                (debug-write-string " . " buf)
                (debug-write i (1+ depth) buf))
              (debug-write-string ")" buf))
           (debug-write-string " " buf)
           (debug-write (first i) (1+ depth) buf)))
        (t (debug-write-string "#<" buf)
           (debug-write-fixnum (sys.int::lisp-object-address thing) 16 buf)
           (cond
             ((threadp thing)
              (debug-write-string " Thread " buf)
              (debug-write (thread-name thing) (1+ depth) buf))
             ((mutex-p thing)
              (debug-write-string " Mutex " buf)
              (debug-write (wait-queue-name thing) (1+ depth) buf)
              (debug-write-string " :Owner " buf)
              (debug-write (mutex-owner thing) (1+ depth) buf))
             ((rw-lock-p thing)
              (debug-write-string " Rw-Lock " buf)
              (debug-write (rw-lock-name thing) (1+ depth) buf)
              (debug-write-string " :State " buf)
              (debug-write (rw-lock-state thing) (1+ depth) buf))
             ((condition-variable-p thing)
              (debug-write-string " Condition-Variable " buf)
              (debug-write (wait-queue-name thing) (1+ depth) buf))
             ((event-p thing)
              (debug-write-string " Event " buf)
              (debug-write (wait-queue-name thing) (1+ depth) buf))
             ((watcher-p thing)
              (debug-write-string " Watcher " buf)
              (debug-write (wait-queue-name thing) (1+ depth) buf))
             ((wait-queue-p thing)
              (debug-write-string " Wait-Queue " buf)
              (debug-write (wait-queue-name thing) (1+ depth) buf))
             ((simple-irq-p thing)
              (debug-write-string " Simple-Irq :Number " buf)
              (debug-write (irq-platform-number (simple-irq-irq thing)) (1+ depth) buf)
              (debug-write-string " :State " buf)
              (debug-write (simple-irq-state thing) (1+ depth) buf)
              (debug-write-string " :Event-State " buf)
              (debug-write (event-state (simple-irq-event thing)) (1+ depth) buf)
              (when (simple-irq-latch thing)
                (debug-write-string " :Latch " buf)
                (debug-write (simple-irq-latch thing) (1+ depth) buf)))
             ((irq-p thing)
              (debug-write-string " Irq :Number " buf)
              (debug-write (irq-platform-number thing) (1+ depth) buf)))
           (debug-write-string ">" buf))))

(defun debug-flush-buffer (buf)
  (let ((buf-data (car buf)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buf-data)
             (optimize speed (safety 0)))
    ;; FIXME: Needs to be done under lock.
    (dotimes (i (cdr buf))
      (debug-log-buffer-write-byte (aref buf-data (the fixnum i)))))
  (call-debug-pseudostream :flush-buffer buf))

(defun debug-print-line-1 (things)
  (let* ((buf-data (make-array 100 :element-type '(unsigned-byte 8)))
         (buf (cons buf-data 0)))
    (declare (dynamic-extent buf-data buf))
    (dolist (thing things)
      (debug-write thing 0 buf))
    (debug-write-char #\Newline buf)
    (debug-flush-buffer buf)
    (debug-force-output)))

(defun debug-print-line (&rest things)
  (declare (dynamic-extent things))
  (debug-print-line-1 things))

(defun debug-start-line-p ()
  (call-debug-pseudostream :start-line-p))

(defun debug-force-output ()
  (call-debug-pseudostream :force-output))

(sys.int::defglobal *panic-in-progress* nil)

(defun panic-print-backtrace-function (return-address)
  (debug-write return-address)
  (debug-write-char #\Space)
  ;; Writing the name itself is fraught with danger. Print it in a seperate
  ;; call from the frame-pointer & return address so that a page-fault
  ;; won't abort the whole entry.
  (with-page-fault-hook
      (()
       (debug-write-string "#<unknown>")
       (abandon-page-fault))
    (cond ((sys.int::within-functin-area-p return-address)
           (let* ((function (sys.int::return-address-to-function return-address))
                  (info (sys.int::%object-header-data function))
                  (mc-size (ldb sys.int::+function-header-code-size+ info))
                  ;; First entry in the constant pool.
                  (address (logand (sys.int::lisp-object-address function) -16))
                  (name (sys.int::memref-t address (* mc-size 2))))
             (debug-write name)))
          (t
           (debug-write-string "#<invalid>")))))

(defun panic-print-backtrace (initial-frame-pointer)
  (with-page-fault-hook
      (()
       (debug-print-line "#<truncated>")
       (abandon-page-fault))
    (do ((fp initial-frame-pointer
             (sys.int::memref-signed-byte-64 fp 0)))
        ((eql fp 0))
      (when (not (eql (ldb sys.int::+address-tag+ fp) sys.int::+address-tag-stack+))
        (debug-print-line fp " #<truncated>")
        (return-from panic-print-backtrace))
      (let ((return-address (sys.int::memref-signed-byte-64 fp 1)))
        (when (eql return-address 0)
          (return))
        (debug-write fp)
        (debug-write-char #\Space)
        (panic-print-backtrace-function return-address)
        (debug-print-line)))))

(defun panic (&rest things)
  (declare (dynamic-extent things))
  (panic-1 things nil))

(defun dump-thread-saved-pc (thread)
  (with-page-fault-hook
      (()
       (debug-print-line "<truncated>")
       (abandon-page-fault))
    (let ((return-address (if (thread-full-save-p thread)
                              (thread-state-rip thread)
                              (sys.int::memref-signed-byte-64 (thread-state-rsp thread)
                                                              #-arm64 0
                                                              #+arm64 1))))
      (when (eql return-address 0)
        (return-from dump-thread-saved-pc))
      (debug-write-string "             ")
      (debug-write return-address)
      (debug-write-char #\Space)
      (panic-print-backtrace-function return-address)
      (debug-print-line))))

(defun dump-thread (thread fp)
  (with-page-fault-hook
      (()
       (debug-print-line "<truncated>")
       (abandon-page-fault))
    (debug-print-line "Thread " thread " " (thread-state thread) " " (thread-wait-item thread)))
  (with-page-fault-hook
      (()
       (debug-print-line "<truncated>")
       (abandon-page-fault))
    (debug-print-line "TIFH: " (thread-inhibit-footholds thread) " TPFH: " (thread-pending-footholds thread)))
  (when (not (eql thread (current-thread)))
    (dump-thread-saved-pc thread))
  (panic-print-backtrace fp))

(defun dump-irq (irq)
  (debug-print-line "IRQ " irq " - " (irq-platform-number irq) " (" (irq-count irq) " delivered)")
  (dolist (a (irq-attachments irq))
    (debug-print-line "  " a " " (irq-attachment-device a)
                      (if (irq-attachment-exclusive-p a) " [exclusive]" "")
                      (if (irq-attachment-pending-eoi a) " EOI pending" ""))))

(defun dump-threads ()
  (dump-thread (current-thread) (sys.int::read-frame-pointer))
  (when (boundp '*all-threads*)
    (do ((thread *all-threads*
                 (thread-global-next thread)))
        ((null thread))
      (when (not (eql thread (current-thread)))
        (debug-print-line "----------")
        (dump-thread thread (thread-frame-pointer thread))))))

(defun debug-dump ()
  (debug-print-line "Local CPU is " (local-cpu))
  (with-page-fault-hook
      (()
       (debug-print-line "<truncated>")
       (abandon-page-fault))
    (dump-run-queues))
  (debug-print-line "IRQ state:")
  (with-page-fault-hook
      (()
       (debug-print-line "<truncated>")
       (abandon-page-fault))
    (map-platform-irqs #'dump-irq))
  (dump-active-timers)
  (dump-threads))

(defun debug-magic-button ()
  ;; Try to bring all the other CPUs to a complete stop before doing anything.
  (stop-other-cpus-for-debug-magic-button)
  (debug-print-line "---- Begin magic button dump ----")
  (debug-dump)
  (debug-print-line "---- End magic button dump ----")
  (resume-other-cpus-for-debug-magic-button))

(defun panic-1 (things extra)
  (safe-without-interrupts (things extra)
    (broadcast-panic-ipi)
    (when (and (boundp '*panic-in-progress*)
               *panic-in-progress*)
      (arch-pre-panic)
      (loop (%arch-panic-stop)))
    ;; Stop the world, just in case printing the backtrace requires paging stuff in.
    (setf *world-stopper* (current-thread)
          *panic-in-progress* t)
    (debug-force-output)
    (set-panic-light)
    (arch-pre-panic)
    (debug-print-line "----- PANIC -----")
    (with-page-fault-hook
        (()
         (debug-write-string "--truncated--")
         (abandon-page-fault))
      (debug-print-line-1 things)
      (when extra
        (funcall extra)))
    (debug-dump)
    (loop (%arch-panic-stop))))

(defmacro ensure (condition &rest things)
  "A simple supervisor-safe ASSERT-like macro."
  `(when (not ,condition)
     (panic ',condition ": " ,@things)))

(in-package :mezzano.supervisor)

(sys.int::defglobal *cold-unread-char*)

(defun sys.int::cold-write-char (c stream)
  (declare (ignore stream))
  (debug-write-char c))

(defun sys.int::cold-start-line-p (stream)
  (declare (ignore stream))
  (debug-start-line-p))

(defun sys.int::cold-line-column (stream)
  (if (sys.int::cold-start-line-p stream)
      0
      ;; Not true, but mostly good enough for XP.
      1))

(defun sys.int::cold-line-length (stream)
  (declare (ignore stream))
  nil)

(defun sys.int::cold-read-char (stream)
  (declare (ignore stream))
  (cond (*cold-unread-char*
         (prog1 *cold-unread-char*
           (setf *cold-unread-char* nil)))
        (t (debug-read-char))))

(defun sys.int::cold-unread-char (character stream)
  (declare (ignore stream))
  (when *cold-unread-char*
    (error "Multiple unread-char!"))
  (setf *cold-unread-char* character))

(defun sys.int::cold-clear-input (stream)
  (declare (ignore stream))
  (debug-clear-input))

(defun sys.int::cold-listen (stream)
  (declare (ignore stream))
  ;; Read is not currently implemented for any debug stream.
  nil)

(defun check-supervisor-error (datum arguments)
  (when (eql (current-thread) sys.int::*pager-thread*)
    (panic "Error in pager: " datum " " arguments)))

;;; Early error functions, replaced later as part of cold load.

(defun sys.int::assert-error (test-form datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Assert error " test-form " " datum " " arguments))

(defun sys.int::raise-undefined-function (&rest args sys.int::&closure fref)
  (declare (ignore args))
  (let ((name (sys.int::%object-ref-t fref sys.int::+fref-name+)))
    (cond ((consp name)
           (panic "Undefined function (" (symbol-name (car name)) " " (symbol-name (car (cdr name))) ")"))
          (t (panic "Undefined function " (symbol-name name))))))

(defun sys.int::make-deferred-undefined-function (fref)
  (let ((name (sys.int::%object-ref-t fref sys.int::+fref-name+)))
    (cond ((consp name)
           (panic "Undefined function (" (symbol-name (car name)) " " (symbol-name (car (cdr name))) ")"))
          (t (panic "Undefined function " (symbol-name name))))))

(defun sys.int::raise-unbound-error (symbol)
  (panic "Unbound symbol " (symbol-name symbol)))

(defun sys.int::raise-type-error (datum expected-type)
  (panic "Type error. Expected " expected-type " got " datum))

(defun sys.int::raise-invalid-argument-error (&rest args sys.int::&closure function)
  (declare (ignore args))
  (panic "Invalid arguments to " function))

(defun error (datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Early ERROR. " datum " " arguments))

(defun cerror (continue-message datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Early CERROR. " continue-message " " datum " " arguments))

(defun enter-debugger (condition)
  (panic "Early enter debugger. " condition))

(defun invoke-debugger (condition)
  (panic "Early invoke debugger. " condition))

(defun warn (datum &rest arguments)
  (declare (dynamic-extent arguments))
  (debug-print-line "Early WARN: " datum " " arguments))
