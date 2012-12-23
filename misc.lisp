(in-package #:sys.int)

(declaim (special * ** ***
                  + ++ +++
                  / // ///
                  -))

(defun repl ()
  (let ((* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil)
        (+ nil) (++ nil) (+++ nil)
        (- nil))
    (loop
       (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
         (fresh-line)
         (write-char #\>)
         (let ((form (read)))
           (fresh-line)
           (let ((result (multiple-value-list (let ((- form))
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
                 (write v)))))))))

(defun run ()
  (sys.net::net-setup)
  (sys.graphics::register-screen :bochs *bochs-framebuffer* *bochs-back-buffer* 'bochs-flip-buffer)
  (sys.graphics::invoke-graphics))

(defun hexdump-range (start end &optional stream)
  (when (< start end)
    (write-sequence (make-array (- end start)
                                :element-type '(unsigned-byte 8)
                                :memory (+ #x8000000000 start))
                    stream)))

(defmacro with-deferred-gc (options &body body)
  "Execute BODY with the garbage collector deferred.
BODY must not allocate!"
  `(progn ,@body))


(defgeneric documentation (x doc-type))
(defgeneric (setf documentation) (new-value x doc-type))

(defmethod documentation (x doc-type) nil)
(defmethod (setf documentation) (new-value x doc-type) new-value)

;; (%cpuid leaf ecx) -> eax ebx ecx edx
(sys.int::define-lap-function %cpuid ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax 3)
  (sys.lap-x86:mov64 :rcx :r9)
  (sys.lap-x86:sar64 :rcx 3)
  (sys.lap-x86:cpuid)
  (sys.lap-x86:lea64 :r8 ((:rax 8)))
  (sys.lap-x86:lea64 :r9 ((:rbx 8)))
  (sys.lap-x86:lea64 :r10 ((:rcx 8)))
  (sys.lap-x86:lea64 :r11 ((:rdx 8)))
  (sys.lap-x86:mov32 :ecx #.(ash 4 3))
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun cpuid (leaf &optional (rcx 0))
  (check-type leaf (unsigned-byte 32))
  (check-type rcx (unsigned-byte 32))
  (%cpuid leaf rcx))

(defun make-string (size &key initial-element (element-type 'character))
  (if initial-element
      (make-array size :element-type element-type :initial-element initial-element)
      (make-array size :element-type element-type)))

(defvar *loaded-adsdf-systems* '())

(defun assemble-lap (code &optional name)
  (multiple-value-bind (mc constants)
      (sys.lap-x86:assemble code
        :base-address 12
        :initial-symbols (list (cons nil (lisp-object-address 'nil))
                               (cons t (lisp-object-address 't))
                               (cons 'undefined-function (lisp-object-address *undefined-function-thunk*)))
        :info (list name))
    (make-function mc constants)))
