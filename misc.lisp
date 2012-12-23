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

(defun decode-cpuid-vendor (vendor-1 vendor-2 vendor-3)
  (let ((vendor (make-string (* 4 3))))
    (setf (char vendor 0) (code-char (ldb (byte 8 0) vendor-1))
          (char vendor 1) (code-char (ldb (byte 8 8) vendor-1))
          (char vendor 2) (code-char (ldb (byte 8 16) vendor-1))
          (char vendor 3) (code-char (ldb (byte 8 24) vendor-1))
          (char vendor 4) (code-char (ldb (byte 8 0) vendor-2))
          (char vendor 5) (code-char (ldb (byte 8 8) vendor-2))
          (char vendor 6) (code-char (ldb (byte 8 16) vendor-2))
          (char vendor 7) (code-char (ldb (byte 8 24) vendor-2))
          (char vendor 8) (code-char (ldb (byte 8 0) vendor-3))
          (char vendor 9) (code-char (ldb (byte 8 8) vendor-3))
          (char vendor 10) (code-char (ldb (byte 8 16) vendor-3))
          (char vendor 11) (code-char (ldb (byte 8 24) vendor-3)))
    vendor))

(defun make-string (size &key initial-element (element-type 'character))
  (if initial-element
      (make-array size :element-type element-type :initial-element initial-element)
      (make-array size :element-type element-type)))

(defvar *loaded-adsdf-systems* '())

(defmacro multiple-value-setq (vars form)
  (let ((temps (mapcar (lambda (sym) (gensym (string sym))) vars)))
    `(multiple-value-bind ,temps
         ,form
       ,@(mapcar (lambda (var temp) (list 'setq var temp))
                 vars temps))))

(defun provide (module-name)
  (declare (ignore module-name)))

(defun assemble-lap (code &optional name)
  (multiple-value-bind (mc constants)
      (sys.lap-x86:assemble code
        :base-address 12
        :initial-symbols (list (cons nil (lisp-object-address 'nil))
                               (cons t (lisp-object-address 't))
                               (cons 'undefined-function (lisp-object-address *undefined-function-thunk*)))
        :info (list name))
    (make-function mc constants)))

(setf (fdefinition 'delete) #'remove
      (fdefinition 'delete-if) #'remove-if)

(defun special-operator-p (symbol)
  (check-type symbol symbol)
  (member symbol '(block catch eval-when flet function go if labels
                   let let* load-time-value locally macrolet
                   multiple-value-call multiple-value-prog1
                   progn progv quote return-from setq symbol-macrolet
                   tagbody the throw unwind-protect)))

(defun lisp-implementation-type ()
  "LispOS")

(defun lisp-implementation-version ()
  "devel")

;; (short-name long-name)
(defvar *site-info* nil)

(defun short-site-name () (first *site-info*))
(defun long-site-name () (second *site-info*))

;; (instance)
(defvar *machine-info* nil)
(defun machine-instance () (first *machine-info*))
(defun machine-type () "x86-64")
(defun machine-version ()
  (multiple-value-bind (cpuid-max vendor-1 vendor-3 vendor-2)
      (cpuid 0)
    (declare (ignore cpuid-max))
    (decode-cpuid-vendor vendor-1 vendor-2 vendor-3)))

;;; LispOS uses no supporting software.
(defun software-type () nil)
(defun software-version () nil)

(unless (boundp 'lambda-list-keywords)
  (defconstant lambda-list-keywords '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

;; I sure hope so...
(setf (fdefinition 'stable-sort) #'sort)
