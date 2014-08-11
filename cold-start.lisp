(in-package :sys.int)

(declaim (special *cold-toplevel-forms*
                  *package-system*
                  *additional-cold-toplevel-forms*
                  *initial-obarray*
                  *initial-keyword-obarray*
                  *initial-setf-obarray*
                  *initial-structure-obarray*
                  *kboot-tag-list*)
         (special *terminal-io*
                  *standard-output*
                  *standard-input*
                  *debug-io*
                  *cold-stream-screen*
                  *keyboard-shifted*))
(declaim (special *features* *macroexpand-hook*))

;;; Stuff duplicated/reimplemented from stream.lisp.
;;; stream.lisp builds on CLOS, which is not present in the cold image.

(defun write-char (character &optional stream)
  (cold-write-char character stream))

(defun start-line-p (stream)
  (cold-start-line-p stream))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (cold-read-char stream))

(defun unread-char (character &optional stream)
  (cold-unread-char character stream))

(defun peek-char (&optional peek-type s (eof-error-p t) eof-value recursive-p)
  (cond ((eql peek-type nil)
         (let ((ch (cold-read-char s)))
           (cold-unread-char ch s)
           ch))
        ((eql peek-type t)
         (do ((ch (cold-read-char s)
                  (cold-read-char s)))
             ((not (whitespace[2]p ch))
              (cold-unread-char ch s)
              ch)))
        ((characterp peek-type)
         (error "TODO: character peek."))
        (t (error "Bad peek type ~S." peek-type))))

(defun read-line (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (do ((result (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
       (c (read-char input-stream eof-error-p nil recursive-p)
          (read-char input-stream eof-error-p nil recursive-p)))
      ((or (null c)
           (eql c #\Newline))
       (if (and (null c) (eql (length result) 0))
           (values eof-value t)
           (values result (null c))))
    (vector-push-extend c result)))

(defun yes-or-no-p (&optional control &rest arguments)
  (declare (dynamic-extent arguments))
  (when control
    (write-char #\Newline)
    (apply 'format t control arguments)
    (write-char #\Space))
  (format t "(Yes or No) ")
  (loop
     (let ((line (read-line)))
       (when (string-equal line "yes")
         (return t))
       (when (string-equal line "no")
         (return nil)))
     (write-char #\Newline)
     (format t "Please respond with \"yes\" or \"no\". ")))

(defvar *cold-stream*)
(defun streamp (object)
  (eql object *cold-stream*))

(defun %with-stream-editor (stream recursive-p function)
  (funcall function))

(defun make-case-correcting-stream (stream case)
  stream)

;;; Initial PRINT-OBJECT, replaced when CLOS is loaded.
(defun print-object (object stream)
  (print-unreadable-object (object stream :type t :identity t)))

;;; Early debug stuff.

(defun low-level-backtrace (&optional limit (resolve-names t))
  (do ((i 0 (1+ i))
       (fp (read-frame-pointer)
           (memref-unsigned-byte-64 fp 0)))
      ((or (and limit (> i limit))
           (= fp 0)))
    (write-char #\Newline *cold-stream*)
    (write-integer fp 16 *cold-stream*)
    (write-char #\Space *cold-stream*)
    (let* ((ret-addr (memref-unsigned-byte-64 fp 1))
           (fn (when resolve-names
                 (%%assemble-value (base-address-of-internal-pointer ret-addr) +tag-object+)))
           (name (when (functionp fn) (function-name fn))))
      (write-integer ret-addr 16 *cold-stream*)
      (when (and resolve-names name)
        (write-char #\Space *cold-stream*)
        (write name :stream *cold-stream*)))))
(setf (fdefinition 'backtrace) #'low-level-backtrace)

(defun error (datum &rest arguments)
  (write-char #\!)
  (write datum)
  (write-char #\Space)
  (write arguments)
  (low-level-backtrace)
  (loop (%hlt)))

(defun enter-debugger (condition)
  (write-char #\!)
  (write condition)
  (low-level-backtrace)
  (loop (%hlt)))

(defun invoke-debugger (condition)
  (write-char #\!)
  (write condition)
  (low-level-backtrace)
  (loop (%hlt)))

;;; Pathname stuff before pathnames exist (file.lisp defines real pathnames).

(defun pathnamep (x) nil)
(defun pathnames-equal (x y) nil)

(declaim (special * ** ***))

(defun repl ()
  (let ((* nil) (** nil) (*** nil))
    (loop
       (fresh-line)
       (write-char #\>)
       (let ((form (read)))
         (fresh-line)
         (let ((result (multiple-value-list (eval form))))
           (setf *** **
                 ** *
                 * (first result))
           (when result
             (dolist (v result)
               (fresh-line)
               (write v))))))))

(defvar *early-initialize-hook* '())
(defvar *initialize-hook* '())

(defun add-hook (hook function)
  (unless (boundp hook)
    (setf (symbol-value hook) '()))
  (pushnew function (symbol-value hook)))

;;; GC/low level support/logging stuff.

(defun set-gc-light ())
(defun clear-gc-light ())

(defun emergency-halt (message)
  (%cli)
  (mumble-string message)
  (low-level-backtrace nil nil)
  (loop (%hlt)))

(defun gc-trace (object direction prefix)
  (setf (io-port/8 #xE9) (char-code direction))
  (setf (io-port/8 #xE9) (logand (char-code prefix) #xFF))
  (let ((pointer (%pointer-field object))
        (tag (%tag-field object)))
    (dotimes (i 15)
      (setf (io-port/8 #xE9) (hexify (logand (ash pointer (* -4 (- 14 i))) #b1111))))
    (setf (io-port/8 #xE9) (hexify tag))
    (setf (io-port/8 #xE9) #x0A)))

(defun mumble (message &optional (nl t))
  (mumble-string message)
  (when nl
    (mumble-char #\Newline)))

;;; Used while the GC is copying, so no lookup tables.
(defun hexify (nibble)
  (cond ((<= 0 nibble 9)
         (+ nibble (char-code #\0)))
        (t (+ (- nibble 10) (char-code #\A)))))

(defun mumble-char (char &optional (position 0))
  (let ((code (logand (char-code char) #xFF)))
    (setf (io-port/8 #xE9) code)
    (when (eql code #x0A)
      (loop (when (logbitp 5 (io-port/8 (+ #x3F8 5))) (return)))
      (setf (io-port/8 #x3F8) #x0D))
    (loop (when (logbitp 5 (io-port/8 (+ #x3F8 5))) (return)))
    (setf (io-port/8 #x3F8) code)
    (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 position)
          (logior code #x7000))))

(defun mumble-string (message)
  (dotimes (i (array-dimension message 0))
    (mumble-char (char message i) i)))
(defun mumble-hex (number &optional (message "") (nl nil))
  (mumble-string message)
  (dotimes (i 16)
    (mumble-char (code-char (hexify (logand (ash number (* -4 (- 15 i))) #b1111))) i))
  (when nl (mumble-char #\Newline)))

;;; Fake streams & fake stream functions, used by the mini loader to load
;;; multiboot/kboot modules.
(defun mini-vector-stream (vector)
  (cons vector 0))

(defun %read-byte (stream)
  (prog1 (aref (car stream) (cdr stream))
    (incf (cdr stream))))

(defun %read-sequence (seq stream)
  (replace seq (car stream)
           :start2 (cdr stream)
           :end2 (+ (cdr stream) (length seq)))
  (incf (cdr stream) (length seq)))

;;;; Simple EVAL for use in cold images.
(defun eval-cons (form)
  (case (first form)
    ((if) (if (eval (second form))
              (eval (third form))
              (eval (fourth form))))
    ((function) (if (and (consp (second form)) (eql (first (second form)) 'lambda))
                    (let ((lambda (second form)))
                      (when (second lambda)
                        (error "Not supported: Lambdas with arguments."))
                      (lambda ()
                        (eval `(progn ,@(cddr lambda)))))
                    (fdefinition (second form))))
    ((quote) (second form))
    ((progn) (do ((f (rest form) (cdr f)))
                 ((null (cdr f))
                  (eval (car f)))
               (eval (car f))))
    ((setq) (do ((f (rest form) (cddr f)))
                ((null (cddr f))
                 (setf (symbol-value (car f)) (eval (cadr f))))
              (setf (symbol-value (car f)) (eval (cadr f)))))
    (t (multiple-value-bind (expansion expanded-p)
           (macroexpand form)
         (if expanded-p
             (eval expansion)
             (apply (first form) (mapcar 'eval (rest form))))))))

(defun eval (form)
  (typecase form
    (cons (eval-cons form))
    (symbol (symbol-value form))
    (t form)))

;;; Load bootloader-provided modules.
(defun load-modules ()
  (when *kboot-tag-list*
    (flet ((p/8 (addr) (memref-unsigned-byte-8 (+ #x8000000000 addr) 0))
           (p/16 (addr) (memref-unsigned-byte-16 (+ #x8000000000 addr) 0))
           (p/32 (addr) (memref-unsigned-byte-32 (+ #x8000000000 addr) 0))
           (p/64 (addr) (memref-unsigned-byte-64 (+ #x8000000000 addr) 0)))
      (let ((addr *kboot-tag-list*)
            ;; For sanity checking.
            (max-addr (+ *kboot-tag-list* 1024)))
        (loop (when (>= addr max-addr) (return))
           (let ((type (p/32 (+ addr 0)))
                 (size (p/32 (+ addr 4))))
             (when (and (eql addr *kboot-tag-list*)
                        (not (eql type +kboot-tag-core+)))
               (format t "CORE tag not first in the list?~%")
               (return))
             (case type
               (#.+kboot-tag-none+ (return))
               (#.+kboot-tag-core+
                (unless (eql addr *kboot-tag-list*)
                  (format t "CORE tag not first in the list?~%")
                  (return))
                (setf max-addr (+ *kboot-tag-list* (p/32 (+ addr 16)))))
               (#.+kboot-tag-module+
                (let* ((address (p/64 (+ addr 8)))
                       (size (p/32 (+ addr 16)))
                       (array (make-array size
                                          :element-type '(unsigned-byte 8)
                                          :memory (+ #x8000000000 address)))
                       (stream (mini-vector-stream array)))
                  (format t "Loading KBoot module at ~X~%" address)
                  (mini-load-llf stream))))
             (incf addr (round-up size 8))))))))

;;; Used during cold-image bringup, various files will redefine packages before
;;; the package system is loaded.

(defvar *deferred-%defpackage-calls* '())

(defun %defpackage (&rest arguments)
  (push arguments *deferred-%defpackage-calls*))

;;; Needed for IN-PACKAGE before the package system is bootstrapped.
(defun find-package-or-die (name)
  t)

;;; Until the process system is loaded.
(defun %maybe-preempt-from-interrupt-frame ()
  nil)

(defun initialize-lisp ()
  "A grab-bag of things that must be done before Lisp will work properly.
Cold-generator sets up just enough stuff for functions to be called, for
structures to exist, and for memory to be allocated, but not much beyond that."
  (setf *next-symbol-tls-slot* 256
        *array-types* #(t
                        fixnum
                        bit
                        (unsigned-byte 2)
                        (unsigned-byte 4)
                        (unsigned-byte 8)
                        (unsigned-byte 16)
                        (unsigned-byte 32)
                        (unsigned-byte 64)
                        (signed-byte 1)
                        (signed-byte 2)
                        (signed-byte 4)
                        (signed-byte 8)
                        (signed-byte 16)
                        (signed-byte 32)
                        (signed-byte 64)
                        single-float
                        double-float
                        short-float
                        long-float
                        (complex single-float)
                        (complex double-float)
                        (complex short-float)
                        (complex long-float)
                        xmm-vector)
        ;; Ugh! Set the small static area boundary tag.
        (memref-unsigned-byte-64 *small-static-area* 0) (- (* 1 1024 1024) 2)
        (memref-unsigned-byte-64 *small-static-area* 1) #b100
        *package* nil
        *cold-stream* (make-cold-stream)
        *terminal-io* *cold-stream*
        *standard-output* *cold-stream*
        *standard-input* *cold-stream*
        *debug-io* *cold-stream*
        *screen-offset* (cons 0 0)
        *cold-stream-screen* '(:serial #x3F8)
        *keyboard-shifted* nil
        *early-initialize-hook* '()
        *initialize-hook* '()
        * nil
        ** nil
        *** nil
        /// nil
        // nil
        / nil
        +++ nil
        ++ nil
        + nil
        *default-control-stack-size* 16384
        *default-binding-stack-size* 512)
  (setf *print-base* 10.
        *print-escape* t
        *print-readably* nil
        *print-safe* nil)
  (setf *features* '(:unicode :little-endian :x86-64 :mezzanine :ieee-floating-point :ansi-cl :common-lisp)
        *macroexpand-hook* 'funcall
        most-positive-fixnum #.(- (expt 2 (- 64 +n-fixnum-bits+ 1)) 1)
        most-negative-fixnum #.(- (expt 2 (- 64 +n-fixnum-bits+ 1))))
  ;; Initialize defstruct and patch up all the structure types.
  (bootstrap-defstruct)
  (dotimes (i (length *initial-structure-obarray*))
    (setf (%struct-slot (svref *initial-structure-obarray* i) 0) *structure-type-type*))
  (write-line "Cold image coming up...")
  ;; Hook FREFs up where required.
  (dotimes (i (length *initial-fref-obarray*))
    (let* ((fref (svref *initial-fref-obarray* i))
           (name (%array-like-ref-t fref 0)))
      (when (consp name)
        (setf (get (second name) 'setf-fref) fref))))
  ;; Run toplevel forms.
  (let ((*package* *package*))
    (dotimes (i (length *cold-toplevel-forms*))
      (eval (svref *cold-toplevel-forms* i))))
  ;; Constantify every keyword.
  (dotimes (i (length *initial-obarray*))
    (when (eql (symbol-package (aref *initial-obarray* i)) :keyword)
      (setf (symbol-mode (aref *initial-obarray* i)) :constant)))
  (dolist (sym '(nil t most-positive-fixnum most-negative-fixnum))
    (setf (symbol-mode sym) :constant))
  ;; Pull in the real package system.
  ;; If anything goes wrong before init-package-sys finishes then things
  ;; break in terrible ways.
  (dotimes (i (length *package-system*))
    (eval (svref *package-system* i)))
  (initialize-package-system)
  (dolist (args (reverse *deferred-%defpackage-calls*))
    (apply #'%defpackage args))
  (makunbound '*deferred-%defpackage-calls*)
  (let ((*package* *package*))
    (dotimes (i (length *additional-cold-toplevel-forms*))
      (eval (svref *additional-cold-toplevel-forms* i))))
  ;; Flush the bootstrap stuff.
  (makunbound '*initial-obarray*)
  (makunbound '*package-system*)
  (makunbound '*additional-cold-toplevel-forms*)
  (makunbound '*cold-toplevel-forms*)
  (makunbound '*initial-fref-obarray*)
  (makunbound '*initial-structure-obarray*)
  (setf (fdefinition 'initialize-lisp) #'reinitialize-lisp)
  (gc)
  (reinitialize-lisp))

(defun reinitialize-lisp ()
  (init-isa-pic)
  (cold-stream-init)
  (gc-init-system-memory)
  (setf *cold-stream-log* (make-array 10000 :element-type 'character :adjustable t :fill-pointer 0))
  (mapc 'funcall *early-initialize-hook*)
  (%sti)
  (pci-device-scan)
  (write-line "Hello, world.")
  (load-modules)
  (mapc 'funcall *initialize-hook*)
  (terpri)
  (write-char #\*)
  (write-char #\O)
  (write-char #\K)
  (terpri)
  (setf *bootlog* *cold-stream-log*)
  (makunbound '*cold-stream-log*)
  (repl)
  (loop (%hlt)))

#+(or)(define-lap-function %%common-entry ()
  (:gc :no-frame)
  ;; This is the common entry code.
  ;; The KBoot & GRUB setup code both jump here.
  ;; The stack is not valid, the machine is in 64-bit mode with
  ;; our page tables, GDT & IDT loaded.
  ;; Calling this from Lisp is probably a bad idea.
  ;; Preset the initial stack group.
  (sys.lap-x86:mov64 :r8 (:constant *initial-stack-group*))
  (sys.lap-x86:mov64 :r8 (:r8 #.(+ (- sys.int::+tag-object+) 8 (* sys.c::+symbol-value+ 8))))
  (sys.lap-x86:mov64 :csp (:r8 #.(- (* (1+ +stack-group-offset-control-stack-base+) 8) +tag-object+)))
  (sys.lap-x86:add64 :csp (:r8 #.(- (* (1+ +stack-group-offset-control-stack-size+) 8) +tag-object+)))
  ;; Clear binding stack.
  (sys.lap-x86:mov64 :rdi (:r8 #.(- (* (1+ +stack-group-offset-binding-stack-base+) 8) +tag-object+)))
  (sys.lap-x86:mov64 :rcx (:r8 #.(- (* (1+ +stack-group-offset-binding-stack-size+) 8) +tag-object+)))
  (sys.lap-x86:sar64 :rcx 3)
  (sys.lap-x86:xor32 :eax :eax)
  (sys.lap-x86:rep)
  (sys.lap-x86:stos64)
  ;; Set the binding stack pointer.
  (sys.lap-x86:mov64 (:r8 #.(- (* (1+ +stack-group-offset-binding-stack-pointer+) 8)
                               +tag-object+))
                     :rdi)
  ;; Clear TLS binding slots.
  (sys.lap-x86:lea64 :rdi (:r8 #.(- (* (1+ +stack-group-offset-tls-slots+) 8)
                                    +tag-object+)))
  (sys.lap-x86:mov64 :rax :unbound-tls-slot)
  (sys.lap-x86:mov32 :ecx #.+stack-group-tls-slots-size+)
  (sys.lap-x86:rep)
  (sys.lap-x86:stos64)
  ;; Initialize GS.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:mov64 :rdx :r8)
  (sys.lap-x86:sar64 :rdx 32)
  (sys.lap-x86:mov64 :rcx #xC0000101)
  (sys.lap-x86:wrmsr)
  ;; Mark the SG as active.
  (sys.lap-x86:gs)
  (sys.lap-x86:and64 (#.(+ (- +tag-object+)
                           (ash (1+ +stack-group-offset-flags+)
                                +n-fixnum-bits+)))
                     #.(ash (lognot (1- (ash 1 +stack-group-state-size+)))
                            (+ +stack-group-state-position+
                               +n-fixnum-bits+)))
  ;; SSE init.
  ;; Set CR4.OSFXSR and CR4.OSXMMEXCPT.
  (sys.lap-x86:movcr :rax :cr4)
  (sys.lap-x86:or64 :rax #x00000600)
  (sys.lap-x86:movcr :cr4 :rax)
  ;; Clear CR0.EM and set CR0.MP.
  (sys.lap-x86:movcr :rax :cr0)
  (sys.lap-x86:and64 :rax -5)
  (sys.lap-x86:or64 :rax #x00000002)
  (sys.lap-x86:movcr :cr0 :rax)
  ;; Clear FPU/SSE state.
  (sys.lap-x86:push #x1F80)
  (sys.lap-x86:ldmxcsr (:rsp))
  (sys.lap-x86:add64 :rsp 8)
  (sys.lap-x86:fninit)
  ;; Clear frame pointer.
  (sys.lap-x86:mov64 :cfp 0)
  ;; Clear data registers.
  (sys.lap-x86:xor32 :r8d :r8d)
  (sys.lap-x86:xor32 :r9d :r9d)
  (sys.lap-x86:xor32 :r10d :r10d)
  (sys.lap-x86:xor32 :r11d :r11d)
  (sys.lap-x86:xor32 :r12d :r12d)
  (sys.lap-x86:xor32 :ebx :ebx)
  ;; Prepare for call.
  (sys.lap-x86:mov64 :r13 (:function initialize-lisp))
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Call the entry function.
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  ;; Crash if it returns.
  here
  (sys.lap-x86:ud2)
  (sys.lap-x86:jmp here))
