;;;; Support functions for cross-compilation.

(in-package :cross-support)

(defmacro mezzano.extensions:cas (place old new)
  ;; As a special cross-build exception, support hash-tables.
  (cond ((and (consp place)
              (eql (first place) 'gethash))
         (destructuring-bind (key hash-table &optional default)
             (rest place)
           `(cas-hash-table ,key ,hash-table ,default ,old ,new)))
        (t
         `(error "Cross-cas ~S not supported" place))))

(defun sys.int::%defun (name lambda &optional documentation)
  (declare (ignore documentation))
  ;; Completely ignore CAS functions when cross compiling, they're not needed.
  (unless (and (consp name) (eql (first name) 'mezzano.extensions:cas))
    (setf (fdefinition name) lambda))
  name)

(defun fboundp (name)
  (if (and (consp name) (eql (first name) 'mezzano.extensions:cas))
      nil
      (cl:fboundp name)))

(in-package :mezzano.compiler)

(define-condition sys.int::simple-style-warning (style-warning simple-condition) ())

(defvar *target-architecture*)

(in-package :mezzano.internals)

(defclass structure-definition ()
  ((name :initarg :name :reader structure-definition-name)
   (slots :initarg :slots :reader structure-definition-slots)
   (parent :initarg :parent :reader structure-definition-parent)
   (area :initarg :area :reader structure-definition-area)
   (size :initarg :size :reader structure-definition-size)
   (layout :initarg :layout :accessor structure-definition-layout)
   (sealed :initarg :sealed :reader structure-definition-sealed)
   (docstring :initarg :docstring :reader structure-definition-docstring)
   (mezzano.clos::has-standard-constructor
    :initarg :has-standard-constructor
    :reader structure-definition-has-standard-constructor)))

(defmethod class-name ((class structure-definition))
  (structure-definition-name class))

(defun %make-struct-definition (name slots parent area size layout sealed docstring has-standard-constructor)
  (make-instance 'structure-definition
                 :name name
                 :slots slots
                 :parent parent
                 :area area
                 :size size
                 :layout layout
                 :sealed sealed
                 :docstring docstring
                 :has-standard-constructor has-standard-constructor))

(defun structure-definition-p (object)
  (typep object 'structure-definition))

(defun mezzano.clos:class-precedence-list (class)
  ;; Materialize slightly-broken precedence lists for structure definitions too.
  (if (typep class 'structure-definition)
      (if (structure-definition-parent class)
          (list* class (mezzano.clos:class-precedence-list (structure-definition-parent class)))
          (list class))
      (c2mop:class-precedence-list class)))

(defun mezzano.clos::safe-class-precedence-list (class)
  (mezzano.clos:class-precedence-list class))

(defstruct layout
  class
  hash
  obsolete
  heap-size
  heap-layout
  area
  instance-slots
  compatible)

(defun make-struct-definition (name slots parent area size layout sealed docstring has-standard-constructor)
  (let* ((def (sys.int::%make-struct-definition name slots parent area size nil sealed docstring has-standard-constructor))
         (layout-object (make-layout
                         :class def
                         :obsolete nil
                         :heap-size size
                         :heap-layout layout
                         :area area
                         ;; ### Not currently supported for structs.
                         :instance-slots nil)))
    (setf (structure-definition-layout def) layout-object)
    def))

(defclass structure-slot-definition ()
  ((name :initarg :name :reader structure-slot-definition-name)
   (accessor :initarg :accessor :reader structure-slot-definition-accessor)
   (initform :initarg :initform :reader structure-slot-definition-initform)
   (type :initarg :type :reader structure-slot-definition-type)
   (read-only :initarg :read-only :reader structure-slot-definition-read-only :reader mezzano.clos:structure-slot-definition-read-only)
   (location :initarg :location :reader structure-slot-definition-location)
   (fixed-vector :initarg :fixed-vector :reader structure-slot-definition-fixed-vector)
   (align :initarg :align :reader structure-slot-definition-align)
   (dcas-sibling :initarg :dcas-sibling :reader structure-slot-definition-dcas-sibling :reader mezzano.clos:structure-slot-definition-dcas-sibling)
   (documentation :initarg :documentation :reader structure-slot-definition-documentation)))

(defun sys.int::make-struct-slot-definition (name accessor initform type read-only location fixed-vector align dcas-sibling documentation)
  (make-instance 'structure-slot-definition
                 :name name
                 :accessor accessor
                 :initform initform
                 :type type
                 :read-only read-only
                 :location location
                 :fixed-vector fixed-vector
                 :align align
                 :dcas-sibling dcas-sibling
                 :documentation documentation))

(defmethod print-object ((object structure-slot-definition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S"
            (list :name (structure-slot-definition-name object)
                  :accessor (structure-slot-definition-accessor object)
                  :initform (structure-slot-definition-initform object)
                  :type (structure-slot-definition-type object)
                  :read-only (structure-slot-definition-read-only object)
                  :location (structure-slot-definition-location object)
                  :fixed-vector (structure-slot-definition-fixed-vector object)
                  :align (structure-slot-definition-align object)
                  :dcas-sibling (structure-slot-definition-dcas-sibling object)
                  :documentation (structure-slot-definition-documentation object)))))

(defstruct (instance-header
             (:constructor mezzano.compiler::%%make-instance-header
                           (layout)))
  layout)

(in-package :mezzano.compiler)

(defun mezzano.runtime::%make-instance-header (layout)
  (assert (not (eql layout t)))
  (%%make-instance-header layout))

(defun mezzano.runtime::%unpack-instance-header (header)
  (sys.int::instance-header-layout header))

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
          (stemp (first stores))) ;Temp var for int to store.
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

(defun mask-field (bytespec integer)
  (logand integer (dpb -1 bytespec 0)))

(in-package :mezzano.internals)

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
    (#x10402A "Scroll-Lock" "Scrolllock")
    (#x10402B "Num-Lock" "Numlock")
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

(defun name-char (name)
  (or (loop for (code . names) in *char-name-alist*
         when (member name names :test #'string-equal)
         do (return (code-char code)))
      (cl:name-char name)))

(in-package :mezzano.compiler)

(defmethod lookup-variable-in-environment (symbol (environment null))
  (multiple-value-bind (expansion expandedp)
      (gethash symbol cross-support::*system-symbol-macros*)
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
  (gethash name cross-support::*system-compiler-macros*))

(defmethod macro-function-in-environment (symbol (environment null))
  (gethash symbol cross-support::*system-macros*))

(defmethod lookup-variable-declared-type-in-environment (symbol (environment null))
  (mezzano.runtime::symbol-type symbol))

(defmethod optimize-qualities-in-environment ((environment null))
  '())

(defun sys.int::%define-compiler-macro (name function)
  (setf (compiler-macro-function name) function)
  name)

(defun compiler-macro-function (name &optional env)
  (compiler-macro-function-in-environment name env))

(defun (setf compiler-macro-function) (value name &optional env)
  (assert (eql env nil))
  (setf (gethash name cross-support::*system-compiler-macros*) value))

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

(in-package :mezzano.internals)

(defvar *macroexpand-hook* 'funcall)

(defun constantp (form &optional env)
  (declare (ignore env))
  (if (or (eql form 'nil) (eql form 't)
          (keywordp form)
          (and (consp form)
               (eql (first form) 'quote))
          (and (not (symbolp form))
               (not (consp form))))
      t
      nil))

(defun sys.int::variable-information (symbol)
  (cond ((or (member symbol '(nil t))
             (keywordp symbol)
             (cl:constantp symbol))
         :constant)
        (t
         (values (gethash symbol cross-support::*system-symbol-declarations*)))))

;; TODO: Wrap form in a bunch of macrolets.
(defun x-eval (form env)
  (declare (ignore env))
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

(defstruct (function-reference (:constructor make-cross-fref (name)))
  name)

;; FIXME: Should be a weak hash table. How to deal with setf/cas names?
(defvar *fref-table* (make-hash-table :test #'equal))
(defparameter *function-reference-hook-hack* nil)

(defun resolve-fref (name)
  (alexandria:ensure-gethash name *fref-table*
                             (make-cross-fref name)))

(defun sys.int::function-reference (name)
  (if *function-reference-hook-hack*
      (funcall *function-reference-hook-hack* name)
      (resolve-fref name)))

(defun valid-function-name-p (name)
  (typep name '(or symbol
                (cons (member setf cas)
                 (cons symbol null)))))

(defvar *symbol-global-value-cell-table* (make-hash-table :test #'equal :weakness :key))

(in-package :mezzano.runtime)

(defstruct (symbol-value-cell
             (:constructor sys.int::make-cross-symbol-global-value-cell (name)))
  name)

(in-package :mezzano.internals)

(defun mezzano.runtime::symbol-global-value-cell (symbol)
  (alexandria:ensure-gethash symbol *symbol-global-value-cell-table*
                             (make-cross-symbol-global-value-cell symbol)))

(defun mezzano.runtime::symbol-global-value-cell-p (object)
  (check-type object mezzano.runtime::symbol-value-cell)
  t)

(defun mezzano.runtime::symbol-value-cell-symbol (object)
  (mezzano.runtime::symbol-value-cell-name object))

(defun char-bits (char)
  (declare (ignore char))
  0)

(defun sys.int::%single-float-as-integer (value)
  (check-type value single-float)
  (let ((tmp (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent tmp))
    (setf (nibbles:ieee-single-ref/le tmp 0) value)
    (nibbles:ub32ref/le tmp 0)))

(defun sys.int::%double-float-as-integer (value)
  (check-type value double-float)
  (let ((tmp (make-array 8 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent tmp))
    (setf (nibbles:ieee-double-ref/le tmp 0) value)
    (nibbles:ub64ref/le tmp 0)))

(defun sys.int::%integer-as-single-float (value)
  (check-type value (unsigned-byte 32))
  (let ((tmp (make-array 4 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent tmp))
    (setf (nibbles:ub32ref/le tmp 0) value)
    (nibbles:ieee-single-ref/le tmp 0)))

(defun sys.int::%integer-as-double-float (value)
  (check-type value (unsigned-byte 64))
  (let ((tmp (make-array 8 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent tmp))
    (setf (nibbles:ub64ref/le tmp 0) value)
    (nibbles:ieee-double-ref/le tmp 0)))

(defun sys.int::%short-float-as-integer (value)
  (cross-support::cross-short-float-value value))

(defun sys.int::%integer-as-short-float (value)
  (check-type value (unsigned-byte 16))
  (cross-support::make-cross-short-float :value value))

(deftype sys.int::non-negative-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defun sys.int::fixnump (object)
  (mezzano.compiler::fixnump object))

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

(defparameter *cross-logical-base* "SYS:SOURCE")

(defun namestring (pathname)
  ;; Convert back to a relative path, then emit as a logical pathname.
  (let ((p (enough-namestring pathname)))
    (format nil "~:@(~A;~{~A;~}~A.~A.NEWEST~)"
            *cross-logical-base*
            (rest (pathname-directory p))
            (pathname-name p)
            (pathname-type p))))

(in-package :mezzano.compiler)

(defun function-inline-info (name)
  (values (eql (gethash name cross-support::*inline-modes*) t)
          (gethash name cross-support::*inline-forms*)))

(in-package :mezzano.internals)

(defgeneric mezzano.clos:class-slots (class)
  (:method ((class sys.int::structure-definition))
    (sys.int::structure-definition-slots class)))

(defgeneric mezzano.clos:class-sealed (class)
  (:method ((class sys.int::structure-definition))
    (sys.int::structure-definition-sealed class)))

(defgeneric mezzano.clos:class-layout (class)
  (:method ((class sys.int::structure-definition))
    (sys.int::structure-definition-layout class)))

(defgeneric mezzano.clos:class-allocation-area (class)
  (:method ((class sys.int::structure-definition))
    (sys.int::structure-definition-area class)))

(defgeneric mezzano.clos:slot-definition-name (slot-definition)
  (:method ((slot-definition sys.int::structure-slot-definition))
    (sys.int::structure-slot-definition-name slot-definition)))

(defgeneric mezzano.clos:slot-definition-type (slot-definition)
  (:method ((slot-definition sys.int::structure-slot-definition))
    (sys.int::structure-slot-definition-type slot-definition)))

(defgeneric mezzano.clos:slot-definition-location (slot-definition)
  (:method ((slot-definition sys.int::structure-slot-definition))
    (sys.int::structure-slot-definition-location slot-definition)))

(defgeneric mezzano.clos:slot-definition-initform (slot-definition)
  (:method ((slot-definition sys.int::structure-slot-definition))
    (sys.int::structure-slot-definition-initform slot-definition)))

(defgeneric mezzano.clos:structure-slot-definition-fixed-vector (slot-definition)
  (:method ((slot-definition sys.int::structure-slot-definition))
    (sys.int::structure-slot-definition-fixed-vector slot-definition)))

(defgeneric mezzano.clos:structure-slot-definition-align (slot-definition)
  (:method ((slot-definition sys.int::structure-slot-definition))
    (sys.int::structure-slot-definition-align slot-definition)))

(defun mezzano.clos:ensure-class (name &rest initargs)
  (remf initargs :source-location)
  (apply #'c2mop:ensure-class name initargs))

(defun sys.int::known-declaration-p (declaration)
  ;; The normal version also checks type specifiers, but I don't like that style.
  ;; Always use (type foo ..)  over (foo ..)
  (member declaration '(special constant sys.int::global inline notinline
                        sys.int::maybe-inline type ftype declaration optimize)))

(in-package :mezzano.internals)

(defgeneric location-tracking-stream-line (stream)
  (:method (stream) nil))
(defgeneric location-tracking-stream-character (stream)
  (:method (stream) nil))

(defclass location-tracking-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((%stream :initarg :stream :reader location-tracking-stream-stream)
   (%namestring :initarg :namestring :reader location-tracking-stream-namestring)
   (%line :initarg :line :accessor location-tracking-stream-line)
   (%character :initarg :character :accessor location-tracking-stream-character)
   (%unread-character :accessor location-tracking-stream-unread-character))
  (:default-initargs :character 0 :line 1))

(defgeneric location-tracking-stream-location (stream)
  (:documentation "Return a SOURCE-LOCATION indicating the current location in the stream. Returns NIL if location tracking is unavailable.
This should only fill in the START- slots and ignore the END- slots.")
  (:method (stream) nil))

(defmethod location-tracking-stream-location ((stream location-tracking-stream))
  (make-source-location
   :file (location-tracking-stream-namestring stream)
   :top-level-form-number *top-level-form-number*
   :position (let ((inner (location-tracking-stream-stream stream)))
               (if (typep inner 'file-stream)
                   (file-position inner)
                   nil))
   :line (location-tracking-stream-line stream)
   :character (location-tracking-stream-character stream)))

(defmethod trivial-gray-streams:stream-read-char ((stream location-tracking-stream))
  (let ((ch (read-char (location-tracking-stream-stream stream) nil :eof)))
    (cond ((eql ch :eof))
          ((eql ch #\Newline)
           (incf (location-tracking-stream-line stream))
           (setf (location-tracking-stream-unread-character stream)
                 (location-tracking-stream-character stream))
           (setf (location-tracking-stream-character stream) 0))
          (t
           (setf (location-tracking-stream-unread-character stream)
                 (location-tracking-stream-character stream))
           (incf (location-tracking-stream-character stream))))
    ch))

(defmethod trivial-gray-streams:stream-unread-char ((stream location-tracking-stream) character)
  (when (eql character #\Newline)
    (decf (location-tracking-stream-line stream)))
  (setf (location-tracking-stream-character stream)
        (location-tracking-stream-unread-character stream))
  (unread-char character (location-tracking-stream-stream stream)))

(defun mezzano.supervisor:make-rw-lock (&optional name)
  (declare (ignore name))
  :rw-lock)

(defun mezzano.supervisor:rw-lock-read-acquire (lock &optional wait-p)
  (declare (ignore lock wait-p))
  t)

(defun mezzano.supervisor:rw-lock-read-release (lock)
  (declare (ignore lock))
  (values))

(defun mezzano.supervisor:rw-lock-write-acquire (lock &optional wait-p)
  (declare (ignore lock wait-p))
  t)

(defun mezzano.supervisor:rw-lock-write-release (lock)
  (declare (ignore lock))
  (values))

(defmacro mezzano.supervisor:with-rw-lock-read ((lock) &body body)
  `(progn ,lock nil ,@body))

(defmacro mezzano.supervisor:with-rw-lock-write ((lock) &body body)
  `(progn ,lock nil ,@body))

(defun mezzano.extensions:add-find-definitions-hook (hook)
  (declare (ignore hook))
  (values))

(macrolet ((x (nib int)
             `(progn (defun ,int (vec index) (,nib vec index))
                     (defun (setf ,int) (val vec index) (setf (,nib vec index) val)))))
   (x nibbles:ub16ref/le mezzano.extensions:ub16ref/le)
   (x nibbles:ub32ref/le mezzano.extensions:ub32ref/le)
   (x nibbles:ub64ref/le mezzano.extensions:ub64ref/le))

;; Used as part of reading short-floats
(defun coerce (object result-type)
  (cond ((eql result-type 'short-float)
         (ecase object
           (0.0d0 (cross-support::make-cross-short-float :value #x0000))
           (1.0d0 (cross-support::make-cross-short-float :value #x3C00))))
        (t
         (cl:coerce object result-type))))

(defun complex (realpart imagpart)
  (cond ((or (cross-support::cross-short-float-p realpart)
             (cross-support::cross-short-float-p imagpart))
         ;; TODO: Promote as appropriate.
         (assert (cross-support::cross-short-float-p realpart))
         (assert (cross-support::cross-short-float-p imagpart))
         (cross-support::make-cross-complex-short-float
          :realpart realpart
          :imagpart imagpart))
        (t
         (cl:complex realpart imagpart))))

(defun short-float-p (object)
  (cross-support::cross-short-float-p object))

;; Because we have some deftype aliases in play, they need to be expanded
;; before being passed to the host's typep. The alternative would be hooking
;; %DEFTYPE and registering them with the host at definition time.
(defun typep (object type)
  (if (symbolp type)
      (let* ((type-info (and (boundp '*type-info*)
                             (gethash type *type-info*)))
             (type-expander (and type-info
                                 ;; type-info-type-expander
                                 (aref (cross-support::cross-struct-data type-info)
                                       3))))
        (if type-expander
            (typep object (funcall type-expander (if (listp type) type (list type)) nil))
            (cl:typep object type)))
      (cl:typep object type)))

(defun mezzano.internals.numbers.logical::bytep (x)
  (cl:typep x 'cross-support::byte))

(defclass mezzano.clos::class-reference ()
  ((%name :initarg :name :reader mezzano.clos::class-reference-name)))

(defparameter *class-reference-table* (make-hash-table))

(defun mezzano.clos::class-reference (name)
  (let ((reference (gethash name *class-reference-table*)))
    (unless reference
      (setf reference (make-instance 'mezzano.clos::class-reference :name name))
      (setf (gethash name *class-reference-table*) reference))
    reference))

(defun mezzano.runtime::symbol-constant-p (symbol)
  (or (keywordp symbol)
      (member symbol '(nil t))
      (eql (gethash symbol cross-support::*system-symbol-declarations*) :constant)))

(defun mezzano.runtime::symbol-global-p (symbol)
  (eql (gethash symbol cross-support::*system-symbol-declarations*) :global))
