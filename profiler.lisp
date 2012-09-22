(in-package #:sys.int)

(defmacro with-profiling (options &body body)
  `(%with-profiling (lambda () ,@body) ,@options))

;;; This gets baked into the profile interrupt handler.
(defvar *profile-buffer* nil
  "A cons of (simple-array . fixnum), holding the buffer itself and
an offset into the buffer. Should be allocated in static space.")

(defvar *profile-sample-interval* 1000)

(defvar *profile-buffer-default-size* 1048000
  "Number of entries to store in the profile buffer.")

(defun initialize-profile-buffer ()
  (setf *profile-buffer* (cons-in-area (make-array *profile-buffer-default-size*
                                                   :area :static)
                                       0
                                       :static)))

(defun reset-profiler ()
  (when *profile-buffer*
    (setf (cdr *profile-buffer*) 0))
  t)

(defun configure-pit (hz)
  (let ((period (truncate 1193181 hz)))
    (setf (io-port/8 #x43) #x36 ; channel 0, LSB/MSB, mode 3, binary
          (io-port/8 #x40) (ldb (byte 8 0) period)
          (io-port/8 #x40) (ldb (byte 8 8) period))))

(defun install-profile-handler (buffer)
  (multiple-value-bind (mc pool)
      (sys.lap-x86:assemble
          ;; (car rbx)  buffer storage
          ;; (cdr rbx)  buffer fill
          `((sys.lap-x86:push :rbx)
            (sys.lap-x86:mov64 :rbx (:constant ,buffer))
            ;; Start with the current stack group.
            (sys.lap-x86:mov32 :ecx #xC0000101) ; IA32_GS_BASE
            (sys.lap-x86:rdmsr)
            (sys.lap-x86:shl64 :rdx 32)
            (sys.lap-x86:or64 :rax :rdx)
            ;; Mangle the control stack and the stack-group so it looks
            ;; like a suspended stack group.
            ;; Stack alignment is probably violated here.
            (sys.lap-x86:push (:rbp 8)) ; Interrupt RIP.
            (sys.lap-x86:push 0) ; RFLAGS.
            (sys.lap-x86:push :lsp)
            (sys.lap-x86:push :lfp)
            (sys.lap-x86:push (:rbp)) ; Interrupt CFP.
            ;; Update the saved CSP in the stack group.
            (sys.lap-x86:mov64 (:rax (- (* 3 8) #b0111)) :csp)
            ;; Current stack group now looks like a suspended stack group.
            ;; See if there's enough room in the buffer to push at least one word.
            (sys.lap-x86:mov64 :rdx (:car :rbx))
            (sys.lap-x86:mov64 :rcx (:simple-array-header :rdx))
            (sys.lap-x86:shr64 :rcx 8)
            (sys.lap-x86:shl64 :rcx 3)
            ;; rcx  buffer size (fixnum)
            (sys.lap-x86:cmp64 (:cdr :rbx) :rcx)
            (sys.lap-x86:jge give-up-early)
            ;; Push the profile entry size.
            (sys.lap-x86:mov64 :rdi (:cdr :rbx))
            (sys.lap-x86:mov64 (:rdx 1 :rdi) 0)
            (sys.lap-x86:push :rdi)
            (sys.lap-x86:add64 (:cdr :rbx) 8)
            (sys.lap-x86:push :rax)
            ;; (:csp 8) holds the offset of the profile entry size cell.
            ;; (:csp 0) holds the current stack group being processed.
            ;; Start processing stack groups.
            ;; Push the stack group pointer.
            (sys.lap-x86:mov64 :rax (:csp))
            (sys.lap-x86:call push-value)
            ;; Fetch the saved control stack frame pointer.
            (sys.lap-x86:mov64 :rax (:csp))
            (sys.lap-x86:mov64 :rdi (:rax (- (* 3 8) #b0111)))
            (sys.lap-x86:mov64 :rdi (:rdi))
            (sys.lap-x86:jmp scan-stack-test)
            scan-stack-loop
            ;; Pull the saved function object.
            (sys.lap-x86:mov64 :rax (:rdi -16))
            ;; Small sanity check
            (sys.lap-x86:mov8 :dl :al)
            (sys.lap-x86:and8 :dl #b1111)
            (sys.lap-x86:cmp8 :dl #b1100)
            (sys.lap-x86:jne not-function)
            (sys.lap-x86:call push-value)
            not-function
            ;; Advance frame pointer.
            (sys.lap-x86:mov64 :rdi (:rdi))
            scan-stack-test
            (sys.lap-x86:test64 :rdi :rdi)
            (sys.lap-x86:jnz scan-stack-loop)
            ;; Terminate this run.
            (sys.lap-x86:mov64 :rax nil)
            (sys.lap-x86:call push-value)
            ;; Switch to the next stack group (TODO).
            ;; Finish up.
            ;; Unmangle the control stack.
            (sys.lap-x86:add64 :csp ,(* 2 8))
            give-up-early
            (sys.lap-x86:add64 :csp ,(* 5 8))
            (sys.lap-x86:pop :rbx)
            (sys.lap-x86:ret)
            ;; Subroutine: Push a value on the buffer.
            ;; Value in RAX. Buffer cons in RBX. Buffer length in RCX.
            ;; Profile entry size offset at (:csp 16)  (+ 8 for the return address)
            ;; Clobbers RAX, RDX, RSI
            push-value
            (sys.lap-x86:cmp64 (:cdr :rbx) :rcx)
            (sys.lap-x86:jge pv-ret)
            (sys.lap-x86:mov64 :rdx (:car :rbx))
            (sys.lap-x86:mov64 :rsi (:cdr :rbx))
            (sys.lap-x86:mov64 (:rdx 1 :rsi) :rax)
            (sys.lap-x86:mov64 :rsi (:csp 16))
            (sys.lap-x86:add64 (:rdx 1 :rsi) 8)
            (sys.lap-x86:add64 (:cdr :rbx) 8)
            pv-ret
            (sys.lap-x86:ret))
        :base-address 12
        :initial-symbols (list (cons nil (lisp-object-address nil)))
        :info (list 'profiler-interrupt))
    ;; PC PIT.
    (setf (isa-pic-interrupt-handler 0) (make-function mc pool))))

(defun %with-profiling (fn)
  (unless *profile-buffer*
    (initialize-profile-buffer)
    (install-profile-handler *profile-buffer*))
  (unwind-protect
       (progn
         (configure-pit *profile-sample-interval*)
         ;; Enable PIT IRQ, starting the profiler.
         (setf (isa-pic-irq-mask 0) nil)
         (funcall fn))
    ;; Disable the PIT IRQ, stopping the profiler.
    (setf (isa-pic-irq-mask 0) t)))

(defun profiler-data ()
  (do ((i 0 (1+ i))
       (current-thing '())
       (entries '()))
      ((>= i (cdr *profile-buffer*))
       (when current-thing
         (push current-thing entries))
       (nreverse entries))
    (cond ((functionp (aref (car *profile-buffer*) i))
           (push (aref (car *profile-buffer*) i) current-thing))
          (current-thing
           (push current-thing entries)
           (setf current-thing '())))))

(defun treeify-profile (profile)
  (let ((top-level (make-hash-table)))
    (labels ((insert-element (current e)
               (when e
                 (multiple-value-bind (node nodep)
                     (gethash (first e) current)
                   (unless nodep
                     (setf node (list 0 0 (make-hash-table))
                           (gethash (first e) current) node))
                   (if (rest e)
                       (incf (first node))
                       (incf (second node)))
                   (insert-element (third node) (rest e))))))
      (mapc (lambda (e) (insert-element top-level e)) profile))
    top-level))

(defun graph-tree (tree-hash &key (minimum 0))
  (let ((total-samples 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (incf total-samples (first v))
               (incf total-samples (second v)))
             tree-hash)
    (labels ((frob (tree-hash depth)
               (let ((data '()))
               (maphash (lambda (k v) (push (list* k v) data)) tree-hash)
               (sort data '> :key (lambda (x) (+ (second x) (third x))))
               (dolist (x data)
                 (let ((total (+ (second x) (third x))))
                   (when (>= total minimum)
                     (dotimes (i depth) (write-char #\Space))
                     (format t "~A ~D ~D  ~D%~%" (first x) (second x) (third x)
                             ;; No floating point yet :(
                             (truncate (* total 100) total-samples))
                   (frob (fourth x) (1+ depth))))))))
      (frob tree-hash 0))))

(defun graph-profile (&rest args)
  (apply 'graph-tree (treeify-profile (profiler-data)) args))
