;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Support functions for cross-compilation.

(in-package :sys.c)

(define-condition sys.int::simple-style-warning (style-warning simple-condition) ())

(defvar sys.int::*top-level-form-number* nil)

(defvar *target-architecture*)

(defvar *system-macros* (make-hash-table :test 'eq))
(defvar *system-compiler-macros* (make-hash-table :test 'equal))
(defvar *system-symbol-macros* (make-hash-table :test 'eq))
(defvar *system-symbol-declarations* (make-hash-table :test 'eq))

(defstruct (structure-type
             (:constructor sys.int::make-struct-type
                           (name slots parent area)))
  (name)
  (slots)
  (parent)
  (area))

(defun sys.int::make-struct-definition (&rest blah)
  (apply #'sys.int::make-struct-type blah))

(defstruct (structure-slot
             (:constructor sys.int::make-struct-slot-definition
                           (name accessor initform type read-only)))
  name
  accessor
  initform
  type
  read-only)

(defun sys.int::structure-slot-name (x) (structure-slot-name x))
(defun sys.int::structure-slot-accessor (x) (structure-slot-accessor x))
(defun sys.int::structure-slot-initform (x) (structure-slot-initform x))
(defun sys.int::structure-slot-type (x) (structure-slot-type x))
(defun sys.int::structure-slot-read-only (x) (structure-slot-read-only x))

(defvar *structure-types* (make-hash-table :test 'eq))

(defun ldb (bytespec integer)
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec)))))

(defun dpb (newbyte bytespec integer)
  (let ((mask (1- (ash 1 (byte-size bytespec)))))
    (logior (ash (logand newbyte mask) (byte-position bytespec))
            (logand integer (lognot (ash mask (byte-position bytespec)))))))

(define-setf-expander ldb (bytespec int &environment env)
  (multiple-value-bind (temps vals stores
                              store-form access-form)
      (cl:get-setf-expansion int env);Get setf expansion for int.
    (let ((btemp (gensym))     ;Temp var for byte specifier.
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores)) ;Temp var for int to store.
          (bs-size (gensym))   ; Temp var for byte specifier size.
          (bs-position (gensym))) ; Temp var for byte specifier position.
      (when (cdr stores) (error "Can't expand this."))
      ;; Return the setf expansion for LDB as five values.
      (values (cons btemp temps)       ;Temporary variables.
              (cons bytespec vals)     ;Value forms.
              (list store)             ;Store variables.
              `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                 ,store-form
                 ,store)               ;Storing form.
              `(ldb ,btemp ,access-form) ;Accessing form.
              ))))

(defparameter *char-name-alist*
  ;; C0 control characters, prioritize friendly names.
  '((#x0000 "Null" "Nul")
    (#x0001 "Start-Of-Heading" "Soh")
    (#x0002 "Start-Of-Text" "Stx")
    (#x0003 "End-Of-Text" "Etx")
    (#x0004 "End-Of-Transmission" "Eot")
    (#x0005 "Enquiry" "Enq")
    (#x0006 "Acknowledge" "Ack")
    (#x0007 "Bell" "Bel")
    (#x0008 "Backspace" "Bs")
    (#x0009 "Tab" "Horizontal-Tab" "Ht")
    (#x000A "Newline" "Linefeed" "Lf")
    (#x000B "Vertical-Tab" "Vt")
    (#x000C "Page" "Ff")
    (#x000D "Return" "Carriage-Return" "Cr")
    (#x000E "Shift-Out" "So")
    (#x000F "Shift-In" "Si")
    (#x0010 "Data-Link-Escape" "Dle")
    (#x0011 "Device-Control-One" "Xon" "Dc1")
    (#x0012 "Device-Control-Two" "Dc2")
    (#x0013 "Device-Control-Three" "Xoff" "Dc3")
    (#x0014 "Device-Control-Four" "Dc4")
    (#x0015 "Negative-Acknowledge" "Nak")
    (#x0016 "Synchronous-Idle" "Syn")
    (#x0017 "End-Of-Transmission-Block" "Etb")
    (#x0018 "Cancel" "Can")
    (#x0019 "End-Of-Medium" "Em")
    (#x001A "Substitute" "Sub")
    (#x001B "Escape" "Esc")
    (#x001C "File-Seperator" "Fs")
    (#x001D "Group-Seperator" "Gs")
    (#x001E "Record-Seperator" "Rs")
    (#x001F "Unit-Seperator" "Us")
    (#x0020 "Space" "Sp")
    (#x007F "Rubout" "Delete" "Del")
    ;; C1 control characters.
    ;; Full names from SBCL and abbrivations from Wikipedia.
    (#x0080 "C80" "Pad")
    (#x0081 "C81" "Hop")
    (#x0082 "Break-Permitted" "Bph")
    (#x0083 "No-Break-Permitted" "Nbh")
    (#x0084 "C84" "Ind")
    (#x0085 "Next-Line" "Nel")
    (#x0086 "Start-Selected-Area" "Ssa")
    (#x0087 "End-Selected-Area" "Esa")
    (#x0088 "Character-Tabulation-Set" "Hts")
    (#x0089 "Character-Tabulation-With-Justification" "Htj")
    (#x008A "Line-Tabulation-Set" "Vts")
    (#x008B "Partial-Line-Forward" "Pld")
    (#x008C "Partial-Line-Backward" "Plu")
    (#x008D "Reverse-Linefeed" "Ri")
    (#x008E "Single-Shift-Two" "Ss2")
    (#x008F "Single-Shift-Three" "Ss3")
    (#x0090 "Device-Control-String" "Dcs")
    (#x0091 "Private-Use-One" "Pu1")
    (#x0092 "Private-Use-Two" "Pu2")
    (#x0093 "Set-Transmit-State" "Sts")
    (#x0094 "Cancel-Character" "Cch")
    (#x0095 "Message-Waiting" "Mw")
    (#x0096 "Start-Guarded-Area" "Spa")
    (#x0097 "End-Guarded-Area" "Epa")
    (#x0098 "Start-String" "Sos")
    (#x0099 "C99" "Sgci")
    (#x009A "Single-Character-Introducer" "Sci")
    (#x009B "Control-Sequence-Introducer" "Csi")
    (#x009C "String-Terminator" "St")
    (#x009D "Operating-System-Command" "Osc")
    (#x009E "Privacy-Message" "Pm")
    (#x009F "Application-Program-Command" "Apc")
    ;; Special PC keys.
    (#x104001 "F1")
    (#x104002 "F2")
    (#x104003 "F3")
    (#x104004 "F4")
    (#x104005 "F5")
    (#x104006 "F6")
    (#x104007 "F7")
    (#x104008 "F8")
    (#x104009 "F9")
    (#x10400A "F10")
    (#x10400B "F11")
    (#x10400C "F12")
    (#x10400D "F13")
    (#x10400E "F14")
    (#x10400F "F15")
    (#x104010 "Insert")
    (#x104011 "Delete")
    (#x104012 "Home")
    (#x104013 "End")
    (#x104014 "Page-Up" "PageUp" "PgUp")
    (#x104015 "Page-Down" "PageDown" "PgDn")
    (#x104016 "Left-Arrow")
    (#x104017 "Right-Arrow")
    (#x104018 "Up-Arrow")
    (#x104019 "Down-Arrow")
    (#x10401A "Menu")
    (#x10401B "Print-Screen")
    (#x10401C "SysRq")
    (#x10401D "Pause")
    (#x10401E "Break")
    (#x10401F "Caps-Lock" "Capslock")
    (#x104020 "Left-Shift")
    (#x104021 "Right-Shift")
    (#x104022 "Left-Control")
    (#x104023 "Right-Control")
    (#x104024 "Left-Meta")
    (#x104025 "Right-Meta")
    (#x104026 "Left-Super")
    (#x104027 "Right-Super")
    (#x104028 "Left-Hyper")
    (#x104029 "Right-Hyper")
    (#x1040F0 "KP-0")
    (#x1040F1 "KP-1")
    (#x1040F2 "KP-2")
    (#x1040F3 "KP-3")
    (#x1040F4 "KP-4")
    (#x1040F5 "KP-5")
    (#x1040F6 "KP-6")
    (#x1040F7 "KP-7")
    (#x1040F8 "KP-8")
    (#x1040F9 "KP-9")
    (#x1040FA "KP-Period")
    (#x1040FB "KP-Divide")
    (#x1040FC "KP-Multiply")
    (#x1040FD "KP-Minus")
    (#x1040FE "KP-Plus")
    (#x1040FF "KP-Enter")
    ;; ACPI keys.
    (#x104080 "Power")
    (#x104081 "Sleep")
    (#x104082 "Wake")
    ;; Windows Multimedia keys.
    (#x104090 "Next-Track")
    (#x104091 "Previous-Track")
    (#x104092 "Stop")
    (#x104093 "Play")
    (#x104094 "Mute")
    (#x104095 "Volume-Up")
    (#x104096 "Volume-Down")
    (#x104097 "Media-Select")
    (#x104098 "E-Mail")
    (#x104099 "Calculator")
    (#x10409A "Computer")
    (#x10409B "WWW-Search")
    (#x10409C "WWW-Home")
    (#x10409D "WWW-Back")
    (#x10409E "WWW-Forward")
    (#x10409F "WWW-Stop")
    (#x1040A0 "WWW-Refresh")
    (#x1040A1 "WWW-Favorites")))

(defun character-reader (stream ch p)
  (declare (ignore ch p))
  (let ((x (read-char stream t nil t))
        (y (peek-char nil stream nil nil t)))
    (if (or (eql nil y)
            (get-macro-character y)
            (member y '(#\Space #\Tab #\Newline) :test #'char-equal))
        ;; Simple form: Single character followed by EOF or a non-constituent character.
        ;; Just return the character that was read.
        x
        ;; Reading a character name, similar to read-token, but no special handling
        ;; is done for packages or numbers.
        (let ((token (make-array 1
                                 :element-type 'character
                                 :initial-element x
                                 :adjustable t
                                 :fill-pointer t)))
          (do ((z (read-char stream nil nil t)
                  (read-char stream nil nil t)))
              ((or (eql nil z)
                   (when (or (get-macro-character z)
                             (member z '(#\Space #\Tab #\Newline) :test #'char-equal))
                     (unread-char z stream)
                     t)))
            (vector-push-extend z token))
          ;; Finished reading the token, convert it to a character
          (let ((c (cross-name-char token)))
            (when (and (not c) (not *read-suppress*))
              (error 'simple-reader-error :stream stream
                     :format-control "Unrecognized character name ~S."
                     :format-arguments (list token)))
            c)))))

(defun cross-name-char (name)
  (or (loop for (code . names) in *char-name-alist*
         when (member name names :test #'string-equal)
         do (return (code-char code)))
      (name-char name)))

(defvar *cross-readtable* (copy-readtable nil))
(set-dispatch-macro-character #\# #\\ 'character-reader *cross-readtable*)

(defun read-backquote (stream first)
  (declare (ignore first))
  (list 'sys.int::backquote (read stream t nil t)))

(defun read-comma (stream first)
  (declare (ignore first))
  (case (peek-char nil stream t)
    (#\@ (read-char stream t nil t)
         (list 'sys.int::bq-comma-atsign (read stream t nil t)))
    (#\. (read-char stream t nil t)
         (list 'sys.int::bq-comma-dot (read stream t nil t)))
    (otherwise
     (list 'sys.int::bq-comma (read stream t nil t)))))

(set-macro-character #\` 'read-backquote nil *cross-readtable*)
(set-macro-character #\, 'read-comma nil *cross-readtable*)

(defun eval-feature-test (test)
  "Evaluate the feature expression TEST."
  (etypecase test
    (symbol (member test sys.int::*features*))
    (cons (case (car test)
            (:not (when (or (null (cdr test)) (cddr test))
                    (error "Invalid feature expression ~S" test))
                  (not (eval-feature-test (cadr test))))
            (:and (dolist (subexpr (cdr test) t)
                    (when (not (eval-feature-test subexpr))
                      (return nil))))
            (:or (dolist (subexpr (cdr test) nil)
                   (when (eval-feature-test subexpr)
                     (return t))))
            (t (error "Invalid feature expression ~S" test))))))

(defun read-features (stream suppress-if-false)
  "Common function to implement #+ and #-."
  (let* ((test (let ((*package* (find-package "KEYWORD")))
                 (read stream t nil t)))
         (*read-suppress* (or *read-suppress*
                              (if suppress-if-false
                                  (not (eval-feature-test test))
                                  (eval-feature-test test))))
         (value (read stream t nil t)))
    (if *read-suppress*
        (values)
        value)))

(defun read-feature-plus (stream ch p)
  (declare (ignore ch p))
  (read-features stream t))

(defun read-feature-minus (stream ch p)
  (declare (ignore ch p))
  (read-features stream nil))

(set-dispatch-macro-character #\# #\+ 'read-feature-plus *cross-readtable*)
(set-dispatch-macro-character #\# #\- 'read-feature-minus *cross-readtable*)

(defmethod lookup-variable-in-environment (symbol (environment null))
  (multiple-value-bind (expansion expandedp)
      (gethash symbol *system-symbol-macros*)
    (if expandedp
        (make-instance 'symbol-macro :name symbol :expansion expansion)
        (make-instance 'special-variable
                       :name symbol
                       :implicitly-declared (not (sys.int::variable-information symbol))))))

(defmethod lookup-function-in-environment (name (environment null))
  (make-instance 'top-level-function :name name))

(defmethod inline-info-in-environment (name (environment null))
  (function-inline-info name))

(defmethod lookup-block-in-environment (tag (environment null))
  nil)

(defmethod lookup-go-tag-in-environment (tag (environment null))
  nil)

(defmethod environment-macro-definitions-only ((environment null))
  nil)

(defmethod compiler-macro-function-in-environment (name (environment null))
  (gethash name *system-compiler-macros*))

(defmethod macro-function-in-environment (symbol (environment null))
  (gethash symbol *system-macros*))

(defmethod lookup-variable-declared-type-in-environment (symbol (environment null))
  (mezzano.runtime::symbol-type symbol))

(defmethod optimize-qualities-in-environment ((environment null))
  '())

(defun compiler-macro-function (name &optional env)
  (compiler-macro-function-in-environment name env))

(defun (setf compiler-macro-function) (value name &optional env)
  (assert (eql env nil))
  (setf (gethash name *system-compiler-macros*) value))

(defun macro-function (symbol &optional env)
  (macro-function-in-environment symbol env))

(defun macroexpand (form &optional env)
  (let ((did-expand nil))
    (loop (multiple-value-bind (expansion expanded-p)
              (macroexpand-1 form env)
            (unless expanded-p
              (return (values expansion did-expand)))
            (setf did-expand t
                  form expansion)))))

(defun macroexpand-1 (form &optional env)
  (cond ((symbolp form)
         (let ((var (lookup-variable-in-environment form env)))
           (cond ((typep var 'symbol-macro)
                  (values (symbol-macro-expansion var) t))
                 (t
                  (values form nil)))))
        ((consp form)
         (let ((fn (macro-function (first form) env)))
           (if fn
               (values (funcall *macroexpand-hook* fn form env) t)
               (values form nil))))
        (t (values form nil))))

(defun remove-&environment (orig-lambda-list)
  (do* ((lambda-list (copy-list orig-lambda-list))
        (prev nil i)
        (i lambda-list (cdr i)))
       ((null i) (values lambda-list nil))
    (when (eql (first i) '&environment)
      (assert (not (null (cdr i))) ()
              "Missing variable after &ENVIRONMENT.")
      (if prev
          (setf (cdr prev) (cddr i))
          (setf lambda-list (cddr i)))
      (assert (not (member '&environment lambda-list)) ()
              "Duplicate &ENVIRONMENT variable in lambda-list ~S." orig-lambda-list)
      (return (values lambda-list (second i))))))

(defmacro def-x-macro (name lambda-list &body body)
  (let ((whole))
    (multiple-value-bind (fixed-lambda-list env)
        (remove-&environment lambda-list)
      (when (null env)
        (setf env (gensym)))
      (if (eql (first fixed-lambda-list) '&whole)
          (setf whole (second fixed-lambda-list)
                fixed-lambda-list (cddr fixed-lambda-list))
          (setf whole (gensym)))
      `(setf (gethash ',name *system-macros*)
             (lambda (,whole ,env)
               (declare (ignorable ,whole ,env))
               (destructuring-bind ,fixed-lambda-list (cdr ,whole)
                 (block ,name ,@body)))))))

(defvar *macroexpand-hook* 'funcall)

(defun constantp (form &optional env)
  (if (or (eql form 'nil) (eql form 't)
          (keywordp form)
          (and (not (symbolp form))
               (not (consp form))))
      t
      nil))

(defun sys.int::variable-information (symbol)
  (cond ((or (member symbol '(nil t))
             (keywordp symbol)
             (cl:constantp symbol))
         :constant)
        (t (gethash symbol *system-symbol-declarations*))))

(defvar *output-fasl*)
(defvar *output-map*)
(defvar *output-dry-run*)
(defvar *pending-llf-commands*)

(defun x-compile-top-level-implicit-progn (forms env mode)
  (dolist (f forms)
    (x-compile-top-level f env mode)))

(defun x-compile-top-level-lms-body (forms env mode)
  "Common code for handling the body of LOCALLY, MACROLET and SYMBOL-MACROLET forms at the top-level."
  (multiple-value-bind (body declares)
      (parse-declares forms)
    (x-compile-top-level-implicit-progn
     body (extend-environment env :declarations declares) mode)))

(defun make-macrolet-env (definitions env)
  (extend-environment env
                      :functions (loop
                         for def in definitions
                                    collect (hack-macrolet-definition def env))))

(defun macroexpand-top-level-form (form env)
  (cond ((and (listp form)
              (>= (list-length form) 3)
              (eql (first form) 'sys.int::define-lap-function)
              (listp (third form)))
         ;; Don't expand DEFINE-LAP-FUNCTION.
         (values form nil))
        (t
         ;; Preserve the above behaviour when recursively macroexpanding.
         (multiple-value-bind (expansion expandedp)
             (macroexpand-1 form env)
           (cond (expandedp
                  (values (macroexpand-top-level-form expansion env)
                          t))
                 (t
                  (values expansion nil)))))))

(defun x-compile-top-level (form env &optional (mode :not-compile-time))
  "Cross-compile a top-level form.
3.2.3.1 Processing of Top Level Forms."
  (let ((expansion (macroexpand-top-level-form form env)))
    (cond ((consp expansion)
           (case (first expansion)
             ;; 3. If the form is a progn form, each of its body forms is sequentially
             ;;    processed as a top level form in the same processing mode.
             ((progn)
              (x-compile-top-level-implicit-progn (rest expansion) env mode))
             ;; 4. If the form is a locally, macrolet, or symbol-macrolet, compile-file
             ;;    establishes the appropriate bindings and processes the body forms as
             ;;    top level forms with those bindings in effect in the same processing mode.
             ((locally)
              (x-compile-top-level-lms-body (rest expansion) env mode))
             ((macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (make-macrolet-env definitions env) mode)))
             ((symbol-macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (make-symbol-macrolet-env definitions env) mode)))
             ;; 5. If the form is an eval-when form, it is handled according to figure 3-7.
             ((eval-when)
              (destructuring-bind (situation &body body) (rest expansion)
                (multiple-value-bind (compile load eval)
                    (sys.int::parse-eval-when-situation situation)
                  ;; Figure 3-7. EVAL-WHEN processing
                  (cond
                    ;; Process as compile-time-too.
                    ((or (and compile load)
                         (and (not compile) load eval (eql mode :compile-time-too)))
                     (x-compile-top-level-implicit-progn body env :compile-time-too))
                    ;; Process as not-compile-time.
                    ((or (and (not compile) load eval (eql mode :not-compile-time))
                         (and (not compile) load (not eval)))
                     (x-compile-top-level-implicit-progn body env :not-compile-time))
                    ;; Evaluate.
                    ((or (and compile (not load))
                         (and (not compile) (not load) eval (eql mode :compile-time-too)))
                     (x-eval `(progn ,@body) env))
                    ;; Discard.
                    ((or (and (not compile) (not load) eval (eql mode :not-compile-time))
                         (and (not compile) (not load) (not eval)))
                     nil)
                    (t (error "Impossible!"))))))
             ;; 6. Otherwise, the form is a top level form that is not one of the
             ;;    special cases. In compile-time-too mode, the compiler first
             ;;    evaluates the form in the evaluation environment and then minimally
             ;;    compiles it. In not-compile-time mode, the form is simply minimally
             ;;    compiled. All subforms are treated as non-top-level forms.
             (t (when (eql mode :compile-time-too)
                  (x-eval expansion env))
                (when *output-fasl*
                  (x-compile expansion env)))))
          (t (when (eql mode :compile-time-too)
               (x-eval expansion env))
             (when *output-fasl*
               (x-compile expansion env))))))

;; TODO: Wrap form in a bunch of macrolets.
(defun x-eval (form env)
  (when *compile-print*
    (let ((*print-length* 3)
          (*print-level* 2))
      (format t ";; X-eval: ~S~%" form)))
  (eval form))

(defstruct cross-function
  mc
  constants
  fixups
  gc-info)

(defstruct (cross-fref (:constructor make-cross-fref (name)))
  name)

;; Should be a weak hash table.
(defvar *fref-table* (make-hash-table :test #'equal))

(defun resolve-fref (name)
  (alexandria:ensure-gethash name *fref-table*
                             (make-cross-fref name)))

(defun sys.int::assemble-lap (code &optional name debug-info wired architecture)
  (declare (ignore wired))
  (multiple-value-bind (mc constants fixups symbols gc-data)
      (let ((sys.lap:*function-reference-resolver* #'resolve-fref))
        (declare (special sys.lap:*function-reference-resolver*)) ; blech.
        (ecase architecture
          (:x86-64
           (sys.lap-x86:assemble code
             :base-address 16
             :initial-symbols '((nil . :fixup)
                                (t . :fixup)
                                (:unbound-value . :fixup)
                                (:undefined-function . :fixup)
                                (:closure-trampoline . :fixup)
                                (:funcallable-instance-trampoline . :fixup))
             :info (list name debug-info)))
          (:arm64
           (mezzano.lap.arm64:assemble code
             :base-address 16
             :initial-symbols '((nil . :fixup)
                                (t . :fixup)
                                (:unbound-value . :fixup)
                                (:undefined-function . :fixup)
                                (:closure-trampoline . :fixup)
                                (:funcallable-instance-trampoline . :fixup))
             :info (list name debug-info)))))
    (declare (ignore symbols))
    (make-cross-function :mc mc
                         :constants constants
                         :fixups fixups
                         :gc-info gc-data)))

(defun write-llf-header (output-stream input-file)
  (declare (ignore input-file))
  ;; TODO: write the source file name out as well.
  (write-sequence #(#x4C #x4C #x46 #x01) output-stream)
  (save-integer sys.int::*llf-version* output-stream)
  (save-integer (ecase *target-architecture*
                  (:x86-64 sys.int::+llf-arch-x86-64+)
                  (:arm64 sys.int::+llf-arch-arm64+))
                output-stream))

(defun save-integer (integer stream)
  (let ((negativep (minusp integer)))
    (when negativep (setf integer (- integer)))
    (do ()
        ((zerop (logand integer (lognot #x3F)))
         (write-byte (logior integer (if negativep #x40 0)) stream))
      (write-byte (logior #x80 (logand integer #x7F))
                  stream)
      (setf integer (ash integer -7)))))

(defun char-bits (char)
  (declare (ignore char))
  0)

;;; FIXME: This should allow saving of all attributes and arbitrary codes.
(defun save-character (character stream)
  (let ((code (char-code character)))
    (assert (zerop (char-bits character)) (character))
    (assert (and (<= 0 code #x1FFFFF)
                 (not (<= #xD800 code #xDFFF)))
            (character))
    (cond ((<= code #x7F)
           (write-byte code stream))
          ((<= #x80 code #x7FF)
           (write-byte (logior (ash (logand code #x7C0) -6) #xC0) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          ((or (<= #x800 code #xD7FF)
               (<= #xE000 code #xFFFF))
           (write-byte (logior (ash (logand code #xF000) -12) #xE0) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          ((<= #x10000 code #x10FFFF)
           (write-byte (logior (ash (logand code #x1C0000) -18) #xF0) stream)
           (write-byte (logior (ash (logand code #x3F000) -12) #x80) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          (t (error "TODO character ~S." character)))))

(defgeneric save-one-object (object object-map stream))

(defmethod save-one-object ((object cross-fref) omap stream)
  (save-object (cross-fref-name object) omap stream)
  (write-byte sys.int::+llf-function-reference+ stream))

(defmethod save-one-object ((object cross-function) omap stream)
  (let ((constants (cross-function-constants object)))
    (dotimes (i (length constants))
      (save-object (aref constants i) omap stream))
    (save-object (cross-function-fixups object) omap stream)
    (write-byte sys.int::+llf-function+ stream)
    (write-byte sys.int::+object-tag-function+ stream) ; tag, normal function.
    (save-integer (length (cross-function-mc object)) stream)
    (save-integer (length constants) stream)
    (save-integer (length (cross-function-gc-info object)) stream)
    (write-sequence (cross-function-mc object) stream)
    (write-sequence (cross-function-gc-info object) stream)))

(defmethod save-one-object ((object cons) omap stream)
  (cond ((alexandria:proper-list-p object)
         (let ((len 0))
           (dolist (o object)
             (save-object o omap stream)
             (incf len))
           (write-byte sys.int::+llf-proper-list+ stream)
           (save-integer len stream)))
        (t (save-object (cdr object) omap stream)
           (save-object (car object) omap stream)
           (write-byte sys.int::+llf-cons+ stream))))

(defmethod save-one-object ((object symbol) omap stream)
  (cond ((symbol-package object)
         (write-byte sys.int::+llf-symbol+ stream)
         (save-integer (length (symbol-name object)) stream)
         (dotimes (i (length (symbol-name object)))
           (save-character (char (symbol-name object) i) stream))
         (let ((package (symbol-package object)))
           (when (eql package (find-package :cross-cl))
             (setf package (find-package :cl)))
           (save-integer (length (package-name package)) stream)
           (dotimes (i (length (package-name package)))
             (save-character (char (package-name package) i) stream))))
        (t (save-object (symbol-name object) omap stream)
           ;; Should save flags?
           (if (boundp object)
               (save-object (symbol-value object) omap stream)
               (write-byte sys.int::+llf-unbound+ stream))
           (if (fboundp object)
               (save-object (symbol-function object) omap stream)
               (write-byte sys.int::+llf-unbound+ stream))
           (save-object (symbol-plist object) omap stream)
           (write-byte sys.int::+llf-uninterned-symbol+ stream))))

(defmethod save-one-object ((object string) omap stream)
  (write-byte sys.int::+llf-string+ stream)
  (save-integer (length object) stream)
  (dotimes (i (length object))
    (save-character (char object i) stream)))

(defmethod save-one-object ((object integer) omap stream)
  (write-byte sys.int::+llf-integer+ stream)
  (save-integer object stream))

(defmethod save-one-object ((object vector) omap stream)
  (cond ((every #'integerp object)
         (write-byte sys.int::+llf-integer-vector+ stream)
         (save-integer (length object) stream)
         (dotimes (i (length object))
           (save-integer (aref object i) stream)))
        (t (dotimes (i (length object))
             (save-object (aref object i) omap stream))
           (write-byte sys.int::+llf-simple-vector+ stream)
           (save-integer (length object) stream))))

(defmethod save-one-object ((object character) omap stream)
  (write-byte sys.int::+llf-character+ stream)
  (save-character object stream))

(defmethod save-one-object ((object structure-type) omap stream)
  (save-object (structure-type-name object) omap stream)
  (save-object (structure-type-slots object) omap stream)
  (save-object (structure-type-parent object) omap stream)
  (save-object (structure-type-area object) omap stream)
  (write-byte sys.int::+llf-structure-definition+ stream))

(defmethod save-one-object ((object structure-slot) omap stream)
  (save-object (structure-slot-name object) omap stream)
  (save-object (structure-slot-accessor object) omap stream)
  (save-object (structure-slot-initform object) omap stream)
  (save-object (structure-slot-type object) omap stream)
  (save-object (structure-slot-read-only object) omap stream)
  (write-byte sys.int::+llf-structure-slot-definition+ stream))

(defun %single-float-as-integer (value)
  (check-type value single-float)
  #+sbcl (ldb (byte 32 0) (sb-kernel:single-float-bits value))
  #-(or sbcl) (error "Not implemented on this platform!"))

(defun %double-float-as-integer (value)
  (check-type value double-float)
  #+sbcl (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits value)) 32)
                 (ldb (byte 32 0) (sb-kernel:double-float-low-bits value)))
  #-(or sbcl) (error "Not implemented on this platform!"))

(defmethod save-one-object ((object float) omap stream)
  (etypecase object
    (single-float
     (write-byte sys.int::+llf-single-float+ stream)
     (save-integer (%single-float-as-integer object) stream))
    (double-float
     (write-byte sys.int::+llf-double-float+ stream)
     (save-integer (%double-float-as-integer object) stream))))

(defmethod save-one-object ((object ratio) omap stream)
  (write-byte sys.int::+llf-ratio+ stream)
  (save-integer (numerator object) stream)
  (save-integer (denominator object) stream))

(defmethod save-one-object ((object array) omap stream)
  (dotimes (i (array-total-size object))
    (save-object (row-major-aref object i) omap stream))
  (write-byte sys.int::+llf-array+ stream)
  (save-integer (array-rank object) stream)
  (dolist (dim (array-dimensions object))
    (save-integer dim stream)))

(defmethod save-one-object ((object bit-vector) omap stream)
  (write-byte sys.int::+llf-bit-vector+ stream)
  (save-integer (length object) stream)
  (dotimes (i (ceiling (length object) 8))
    (let ((octet 0))
      (dotimes (j 8)
        (when (>= (+ (* i 8) j) (length object)) (return))
        (setf (ldb (byte 1 j) octet) (bit object j)))
      (write-byte octet stream))))

(defmethod save-one-object ((object byte) omap stream)
  (write-byte sys.int::+llf-byte+ stream)
  (save-integer (byte-size object) stream)
  (save-integer (byte-position object) stream))

(defmethod save-one-object ((object complex) omap stream)
  (etypecase (realpart object)
    (rational
     (write-byte sys.int::+llf-complex-rational+ stream)
     (save-integer (numerator (realpart object)) stream)
     (save-integer (denominator (realpart object)) stream)
     (save-integer (numerator (imagpart object)) stream)
     (save-integer (denominator (imagpart object)) stream))
    (single-float
     (write-byte sys.int::+llf-complex-single-float+ stream)
     (save-integer (%single-float-as-integer (realpart object)) stream)
     (save-integer (%single-float-as-integer (imagpart object)) stream))
    (double-float
     (write-byte sys.int::+llf-complex-double-float+ stream)
     (save-integer (%double-float-as-integer (realpart object)) stream)
     (save-integer (%double-float-as-integer (imagpart object)) stream))))

(defun save-object (object omap stream)
  (let ((info (alexandria:ensure-gethash object omap (list (hash-table-count omap) 0 nil))))
    (cond (*output-dry-run*
           (incf (second info))
           (when (eql (second info) 1)
             (save-one-object object omap stream)))
          (t (when (not (third info))
               (save-one-object object omap stream)
               (setf (third info) t)
               (unless (eql (second info) 1)
                 (write-byte sys.int::+llf-add-backlink+ stream)
                 (save-integer (first info) stream)))
             (unless (eql (second info) 1)
                 (write-byte sys.int::+llf-backlink+ stream)
                 (save-integer (first info) stream))))))

(defun add-to-llf (action &rest objects)
  (push (list* action objects) *pending-llf-commands*))

(defun cross-load-time-value (form read-only-p)
  (declare (ignore read-only-p))
  (let ((ltv-sym (gensym "LOAD-TIME-VALUE-CELL")))
    (x-compile `(locally
                    (declare (special ,ltv-sym))
                  (setq ,ltv-sym ,form))
               nil)
    `(symbol-value ',ltv-sym)))

(defvar *failed-fastload-by-symbol* (make-hash-table))

;; One of:
;;   'symbol
;;   #'symbol
;;   #'(SETF symbol)
;;   #'(CAS symbol)
(defun valid-funcall-function-p (form)
  (and (consp form)
       (consp (cdr form))
       (null (cddr form))
       (or (and (eql (first form) 'quote)
                (symbolp (second form)))
           (and (eql (first form) 'function)
                (let ((name (second form)))
                  (or (symbolp name)
                      (and (consp name)
                           (consp (cdr name))
                           (null (cddr name))
                           (member (first name) '(setf sys.int::cas))
                           (symbolp (second name)))))))))

;; Convert a valid funcall function to the function name.
(defun funcall-function-name (form)
  (second form))

(defun x-compile (form env)
  (cond
    ;; Special case (define-lap-function name (options...) code...)
    ;; Don't macroexpand this, as it expands into a bunch of difficult to
    ;; recognize nonsense.
    ((and (consp form)
          (eql (first form) 'sys.int::define-lap-function)
          (>= (length form) 3))
     (destructuring-bind (name (&optional lambda-list frame-layout environment-vector-offset environment-vector-layout) &body code)
         (cdr form)
       (let ((docstring nil))
         (when (stringp (first code))
           (setf docstring (pop code)))
         (x-compile
          `(sys.int::%defun ',name
                            ',(sys.int::assemble-lap
                               code
                               name
                               (list :debug-info
                                     name
                                     frame-layout
                                     environment-vector-offset
                                     environment-vector-layout
                                     (when *compile-file-pathname*
                                       (princ-to-string *compile-file-pathname*))
                                     sys.int::*top-level-form-number*
                                     lambda-list
                                     docstring)
                               nil
                               *target-architecture*))
          env))))
    (t
     (x-compile-for-value form env)
     (add-to-llf sys.int::+llf-drop+))))

(defun x-compile-for-value (form env)
  ;; FIXME: This should probably use compiler-macroexpand.
  (let ((expansion (macroexpand form env)))
    (cond
      ((symbolp expansion)
       (add-to-llf sys.int::+llf-funcall-n+ expansion 'symbol-value 1))
      ((not (consp expansion))
       ;; Self-evaluating form.
       (add-to-llf nil expansion))
      ((eql (first expansion) 'quote)
       (add-to-llf nil (second expansion)))
      ((and (eql (first expansion) 'function)
            (consp (second expansion))
            (eql (first (second expansion)) 'lambda))
       (let* ((*load-time-value-hook* 'cross-load-time-value)
              (fn (compile-lambda (second expansion) env *target-architecture*)))
         (declare (special *load-time-value-hook*))
         (add-to-llf nil fn)))
      ((eql (first expansion) 'setq)
       (x-compile-for-value `(funcall #'(setf symbol-value)
                                      ,(third form)
                                      ',(second form))
                            env))
      ((special-operator-p (first expansion))
       ;; Can't convert this, convert it to a zero-argument function and
       ;; call that. PROGN to avoid problems with DECLARE.
       (incf (gethash (first expansion) *failed-fastload-by-symbol* 0))
       (let* ((*load-time-value-hook* 'cross-load-time-value)
              (fn (compile-lambda `(lambda ()
                                     (declare (sys.int::lambda-name
                                               (sys.int::toplevel ,(when *compile-file-pathname*
                                                                         (princ-to-string *compile-file-pathname*))
                                                                  ,sys.int::*top-level-form-number*)))
                                     (progn ,expansion))
                                  env
                                  *target-architecture*)))
         (declare (special *load-time-value-hook*))
         (add-to-llf sys.int::+llf-funcall-n+ fn 0)))
      (t
       ;; That should just leave ordinary calls.
       (let ((name (first expansion))
             (args (rest expansion)))
         ;; Unpeel funcall forms.
         (loop
            (cond ((and (eql name 'funcall)
                        (consp args)
                        (valid-funcall-function-p (first args)))
                   (setf name (funcall-function-name (first args))
                         args (rest args)))
                  (t
                   (return))))
         (dolist (arg args)
           (x-compile-for-value arg env))
         (add-to-llf sys.int::+llf-funcall-n+ name (length args)))))))

(defun cross-compile-file (input-file &key
                           (output-file (make-pathname :type "llf" :defaults input-file))
                           (verbose *compile-verbose*)
                           (print *compile-print*)
                           (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (with-open-file (*output-fasl* output-file
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
      (write-llf-header *output-fasl* input-file)
      (let* ((*readtable* (copy-readtable *cross-readtable*))
             (*output-map* (make-hash-table))
             (*pending-llf-commands* nil)
             (*package* (find-package "CL-USER"))
             (*compile-print* print)
             (*compile-verbose* verbose)
             (*compile-file-pathname* (pathname (merge-pathnames input-file)))
             (*compile-file-truename* (truename *compile-file-pathname*))
             (*gensym-counter* 0)
             (cl:*features* *features*)
             (sys.int::*top-level-form-number* 0))
        (when *compile-verbose*
          (format t ";; Cross-compiling ~S~%" input-file))
        (loop for form = (read input nil input) do
             (when (eql form input)
               (return))
             (when *compile-print*
               (let ((*print-length* 3)
                     (*print-level* 2))
                 (format t ";; X-compiling: ~S~%" form)))
             (x-compile-top-level form nil)
             (incf sys.int::*top-level-form-number*))
        ;; Now write everything to the fasl.
        ;; Do two passes to detect circularity.
        (let ((commands (reverse *pending-llf-commands*)))
          (let ((*output-dry-run* t))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o *output-map* (make-broadcast-stream)))))
          (let ((*output-dry-run* nil))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o *output-map* *output-fasl*))
              (when (car cmd)
                (write-byte (car cmd) *output-fasl*)))))
        (write-byte sys.int::+llf-end-of-load+ *output-fasl*))))
  output-file)

(defun load-for-cross-compiler (input-file &key
                           (verbose *compile-verbose*)
                           (print *compile-print*)
                           (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (let* ((*readtable* (copy-readtable *cross-readtable*))
           (*package* (find-package "CL-USER"))
           (*compile-print* print)
           (*compile-verbose* verbose)
           (*compile-file-pathname* (pathname (merge-pathnames input-file)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*output-fasl* nil)
           (*gensym-counter* 0))
      (when *compile-verbose*
        (format t ";; Cross-loading ~S~%" input-file))
      (loop for form = (read input nil input) do
           (when (eql form input)
             (return))
           (when *compile-print*
             (let ((*print-length* 3)
                   (*print-level* 2))
               (format t ";; X-loading: ~S~%" form)))
           (x-compile-top-level form nil :not-compile-time))))
  t)

(defun save-compiler-builtins (path target-architecture)
  (with-open-file (*output-fasl* path
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede
                   :direction :output)
    (format t ";; Writing compiler builtins to ~A.~%" path)
    (write-llf-header *output-fasl* path)
    (let* ((builtins (ecase target-architecture
                       (:x86-64 (mezzano.compiler.codegen.x86-64:generate-builtin-functions))
                       (:arm64 (mezzano.compiler.codegen.arm64:generate-builtin-functions))))
           (*readtable* (copy-readtable *cross-readtable*))
           (*output-map* (make-hash-table))
           (*pending-llf-commands* nil)
           (*package* (find-package "CL-USER"))
           (*compile-print* *compile-print*)
           (*compile-verbose* *compile-verbose*)
           (*compile-file-pathname* (pathname (merge-pathnames path)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*gensym-counter* 0))
      (dolist (b builtins)
        (let ((form `(sys.int::%defun ',(first b) ,(second b))))
          (let ((*print-length* 3)
                (*print-level* 3))
            (format t ";; Compiling form ~S.~%" form))
          (x-compile form nil)))
      ;; Now write everything to the fasl.
      ;; Do two passes to detect circularity.
      (let ((commands (reverse *pending-llf-commands*)))
        (let ((*output-dry-run* t))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (save-object o *output-map* (make-broadcast-stream)))))
        (let ((*output-dry-run* nil))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (save-object o *output-map* *output-fasl*))
            (when (car cmd)
              (write-byte (car cmd) *output-fasl*)))))
      (write-byte sys.int::+llf-end-of-load+ *output-fasl*))))

(deftype sys.int::non-negative-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defun sys.int::fixnump (object)
  (sys.c::fixnump object))

(defun mezzano.runtime::left-shift (integer count)
  (check-type integer integer)
  (check-type count (integer 1))
  (ash integer count))

(defun mezzano.runtime::right-shift (integer count)
  (check-type integer integer)
  (check-type count (integer 1))
  (ash integer (- count)))

(defun sys.int::eval-in-lexenv (lambda env)
  ;; Just quietly ignore the environment for now...
  ;; It's not currently needed by any of the cross-compiled files.
  (declare (ignore env))
  (eval lambda))
