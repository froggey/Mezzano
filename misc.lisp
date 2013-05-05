(in-package :sys.int)

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
         (format t "~A> " (package-shortest-name *package*))
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
  (when (null sys.graphics::*screens*)
    (cond (*bochs-framebuffer*
           (sys.graphics::register-screen :bochs *bochs-framebuffer* *bochs-back-buffer* 'bochs-flip-buffer))
          ((eql (first *cold-stream-screen*) :framebuffer)
           (let ((buffer (make-array (array-dimensions (second *cold-stream-screen*))
                                     :element-type '(unsigned-byte 32)))
                 (width (second (array-dimensions (second *cold-stream-screen*))))
                 (height (first (array-dimensions (second *cold-stream-screen*)))))
             (sys.graphics::register-screen :cold-framebuffer buffer buffer
                                            (lambda ()
                                              (%bitblt height width buffer 0 0 (second *cold-stream-screen*) 0 0)))))
          ((typep *terminal-io* 'framebuffer-stream)
           (let ((fb (slot-value *terminal-io* 'framebuffer))
                 (buffer (make-array (array-dimensions fb)
                                     :element-type '(unsigned-byte 32)))
                 (width (second (array-dimensions fb)))
                 (height (first (array-dimensions fb))))
             (sys.graphics::register-screen :framebuffer buffer buffer
                                            (lambda ()
                                              (%bitblt height width buffer 0 0 fb 0 0)))))
          (t (error "No screens."))))
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

(defvar *load-verbose* nil)
(defvar *load-print* nil)

(defun load-lisp-source (stream)
  (let ((*readtable* *readtable*)
        (*package* *package*)
        (*load-truename* stream)
        (*load-pathname* stream)
        (eof (cons nil nil)))
    (loop (let ((form (read stream nil eof)))
            (when (eql form eof) (return))
            (when *load-print* (format t ";; Loading ~S~%" form))
            (eval form)))
    t))

(defun load-from-stream (stream)
  (when *load-verbose*
    (format t ";;; Loading from ~S~%" stream))
  (if (subtypep (stream-element-type stream) 'character)
      (load-lisp-source stream)
      (mini-load-llf stream)))

(defvar *load-pathname* nil)
(defvar *load-truename* nil)

(defun load (filespec &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist t)
             (external-format :default))
  (let ((*load-verbose* verbose)
        (*load-print* print))
    (cond ((streamp filespec)
           (let* ((*load-pathname* (pathname filespec))
                  (*load-truename* (pathname filespec)))
             (load-from-stream filespec)))
          (t (let* ((path (merge-pathnames filespec))
                    (*load-pathname* (pathname path))
                    (*load-truename* (pathname path)))
               (with-open-file (stream filespec
                                       :if-does-not-exist (if if-does-not-exist
                                                              :error
                                                              nil)
                                       :element-type (if (string-equal (pathname-type path) "LLF")
                                                         '(unsigned-byte 8)
                                                         'character)
                                       :external-format (if (string-equal (pathname-type path) "LLF")
                                                            :default
                                                            external-format))
                 (when stream
                   (load-from-stream stream))))))))

(defmacro multiple-value-setq (vars form)
  (let ((temps (mapcar (lambda (sym) (gensym (string sym))) vars)))
    `(multiple-value-bind ,temps
         ,form
       ,@(mapcar (lambda (var temp) (list 'setq var temp))
                 vars temps))))

(defun assemble-lap (code &optional name debug-info gc-info-offset gc-info-size)
  (declare (ignore gc-info-offset gc-info-size))
  (multiple-value-bind (mc constants)
      (sys.lap-x86:assemble code
        :base-address 12
        :initial-symbols (list (cons nil (lisp-object-address 'nil))
                               (cons t (lisp-object-address 't))
                               (cons 'undefined-function (lisp-object-address *undefined-function-thunk*)))
        :info (list name debug-info))
    (make-function mc constants)))

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

(defconstant array-rank-limit 8) ; ###
(defconstant array-dimension-limit (ash 1 48))
(defconstant array-total-size-limit (ash 1 48))

(defconstant char-code-limit #x110000)

(defun string-left-trim (character-bag string)
  (setf string (string string))
  (let ((n-from-left (dotimes (i (length string)
                               ;; All the characters must be trimmed!
                               (return-from string-left-trim ""))
                       (when (not (find (char string i) character-bag :test #'char=))
                         (return i)))))
    (if (zerop n-from-left)
        string
        (subseq string n-from-left))))

(defun count (item sequence)
  (let ((n 0))
    (dotimes (i (length sequence))
      (when (eql item (elt sequence i))
        (incf n)))
    n))

(defvar *modules* '())
(defvar *require-hooks* '())

(defun provide (module-name)
  (pushnew (string module-name) *modules*
           :test #'string=)
  (values))

(defun require (module-name &optional pathname-list)
  (unless (member (string module-name) *modules*
                  :test #'string=)
    (if pathname-list
        (if (listp pathname-list)
            (dolist (pathname pathname-list)
              (load pathname))
            (load pathname-list))
        (dolist (hook *require-hooks*
                 (error "Unable to REQUIRE module ~A." module-name))
          (when (funcall hook module-name)
            (return)))))
  (values))

(defmethod print-object ((c cell-error) s)
  (print-unreadable-object (c s :type t)
    (write (cell-error-name c) :stream s)))

(defmethod print-object ((c simple-condition) s)
  (print-unreadable-object (c s :type t)
    (apply #'format s
           (simple-condition-format-control c)
           (simple-condition-format-arguments c))))

(defmethod print-object ((object package) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~:(~S~)" (package-shortest-name object))))

(defmethod print-object ((object pci-device) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~2,'0X:~X:~X"
            (pci-device-bus object)
            (pci-device-device object)
            (pci-device-function object))))

(defmethod print-object ((o structure-definition) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (structure-name o) :stream stream)))

(defun pprint-indent (relative-to n &optional stream)
  (check-type relative-to (member :block :current))
  (check-type n real)
  nil)

(defun pprint-newline (kind &optional stream)
  (check-type kind (member :linear :fill :miser :mandatory))
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)
  (check-type kind (member :line :section :line-relative :section-relative))
  (check-type colnum (integer 0))
  (check-type colinc (integer 0))
  nil)

(defmacro pprint-logical-block ((stream-symbol
                                 object
                                 &key
                                 (prefix nil prefix-p)
                                 (per-line-prefix nil per-line-prefix-p)
                                 (suffix ""))
                                &body body)
  (check-type stream-symbol symbol)
  (assert (not (and prefix-p per-line-prefix-p)) ()
          ":PREFIX and :PER-LINE-PREFIX are mutually exclusive.")
  (let ((obj-sym (gensym "object"))
        (block-name (gensym "pprint-logical-block"))
        (pop-count (gensym "pop-count"))
        (pprint-pop-fn (gensym "pprint-pop")))
    (case stream-symbol
      ((t) (setf stream-symbol '*terminal-io*))
      ((nil) (setf stream-symbol '*standard-output*)))
    `(%pprint-logical-block ,stream-symbol ,object
                            ;; FIXME: when prefix is supplied, make sure it
                            ;; does not evaluate to NIL before passing in.
                            ,(cond (prefix-p prefix)
                                   ((not per-line-prefix-p) "")
                                   (t nil))
                            ;; Same here.
                            ,per-line-prefix
                            ,suffix
                            ;; FIXME: Declares & stuff.
                            (lambda (,stream-symbol ,obj-sym &aux (,pop-count 0))
                              (declare (ignorable ,obj-sym))
                              (block ,block-name
                                (flet ((,pprint-pop-fn ()
                                         (cond ((or (not (listp ,obj-sym))
                                                    (circlep ,obj-sym))
                                                (write-string ". " ,stream-symbol)
                                                (write ,obj-sym :stream ,stream-symbol)
                                                (return-from ,block-name))
                                               ((and *print-length* (>= ,pop-count *print-length*))
                                                (write-string "..." ,stream-symbol)
                                                (return-from ,block-name))
                                               (t (incf ,pop-count)
                                                  (pop ,obj-sym)))))
                                  (macrolet ((pprint-pop ()
                                               (list ',pprint-pop-fn))
                                             (pprint-exit-if-list-exhausted ()
                                               `(when (null ,',obj-sym)
                                                  (return-from ,',block-name))))
                                    ,@body)))))))

(defun %pprint-logical-block (base-stream object prefix per-line-prefix suffix fn)
  (declare (ignore per-line-prefix))
  (when prefix (write-string prefix base-stream))
  (funcall fn base-stream object)
  (when suffix (write-string suffix base-stream))
  nil)

(defun circlep (object)
  (declare (ignore object))
  nil)

(defstruct (random-state
             (:constructor %make-random-state (bits)))
  bits)

(defvar *random-state* (%make-random-state 0))

(defun make-random-state (&optional state)
  (case state
    ((t) (%make-random-state 0))
    ((nil) (copy-random-state *random-state*))
    (otherwise (copy-random-state state))))

(defun random (limit &optional (random-state *random-state*))
  (rem (incf (random-state-bits random-state)) limit))
