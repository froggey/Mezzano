;;;; Architecture-independent runtime support functions.

(in-package :mezzano.runtime)

;; There is a single instance of this used as the unbound value marker by symbols.
(defstruct (unbound-value
             (:area :wired))
  tag)

;; Custom wrapper function because of the declaration.
(defun (setf sys.int::%%special-stack-pointer) (value)
  (declare (sys.int::suppress-ssp-checking))
  (setf (sys.int::%%special-stack-pointer) value))

(defun sys.int::%%unwind-to (target-special-stack-pointer)
  (declare (sys.int::suppress-ssp-checking))
  (loop (when (eq target-special-stack-pointer (sys.int::%%special-stack-pointer))
          (return))
     (assert (sys.int::%%special-stack-pointer))
     (etypecase (sys.int::%object-ref-t (sys.int::%%special-stack-pointer) 1)
       (symbol-value-cell
        (sys.int::%%unbind))
       (simple-vector
        (sys.int::%%disestablish-block-or-tagbody))
       (function
        (sys.int::%%disestablish-unwind-protect))
       (mezzano.delimited-continuations:prompt-tag
        ;; Delimited continuation marker.
        ;; Restore thread's stack object.
        (setf (mezzano.supervisor::thread-stack (mezzano.supervisor::current-thread))
              (sys.int::%object-ref-t (sys.int::%%special-stack-pointer) 3))
        (setf (sys.int::%%special-stack-pointer)
              (svref (sys.int::%%special-stack-pointer) 0))))))

(defmacro do-variable-bindings ((value symbol &optional result) &body body)
  (let ((ssp (gensym "SSP"))
        (sym (gensym)))
    `(loop
        with ,sym = ,symbol
        for ,ssp = (sys.int::%%special-stack-pointer) then (sys.int::%object-ref-t ,ssp 0)
        until (null ,ssp)
        when (and (symbol-value-cell-p ,ssp)
                  (eq (symbol-value-cell-symbol ,ssp) ,sym))
        do (let ((,value (sys.int::%object-ref-t ,ssp 2)))
             ,@body)
        finally (return ,result))))

;; Scary note: This is not treated like a normal dynamic variable.
;; The variable itself is not a list, instead the active binding
;; cells are used to simulate one.
(defvar *active-catch-handlers*)
(defun sys.int::%catch (tag fn)
  ;; Catch is used in low levelish code, so must avoid allocation.
  (flet ((exit-fn (values)
           (return-from sys.int::%catch (values-list values))))
    (declare (dynamic-extent (function exit-fn)))
    (let* ((vec (vector tag #'exit-fn))
           (*active-catch-handlers* vec))
      (declare (dynamic-extent vec))
      (funcall fn))))

(defun sys.int::%throw (tag values)
  ;; Note! The VALUES list has dynamic extent!
  ;; This is fine, as the exit function calls VALUES-LIST on it before unwinding.
  (do-variable-bindings (value '*active-catch-handlers*
                         (error 'sys.int::bad-catch-tag-error :tag tag))
    (when (eq (svref value 0) tag)
      (funcall (svref value 1) values))))

(defun sys.int::%coerce-to-callable (object)
  (typecase object
    (function object)
    (symbol
     ;; Fast-path for symbols.
     (let* ((fref (or (sys.int::%object-ref-t object sys.int::+symbol-function+)
                      (sys.int::function-reference object)))
            (fn (sys.int::%object-ref-t fref sys.int::+fref-function+)))
         (if (eq fn fref)
             ;; Return a function that will signal an undefined-function error
             ;; with appropriate restarts when called.
             ;; This is not inlined so as to avoid closing over object in
             ;; the common case.
             (sys.int::make-deferred-undefined-function fref)
             fn)))
    (t
     (sys.int::raise-type-error object '(or function symbol))
     (%%unreachable))))

(declaim (inline %object-slot-address))
(defun %object-slot-address (object slot)
  (+ (sys.int::lisp-object-address object)
     (- sys.int::+tag-object+)
     8
     (* slot 8)))

(defun %%object-of-type-p (object object-tag)
  (eq (sys.int::%object-tag object) object-tag))

(declaim (inline sys.int::%object-of-type-p))
(defun sys.int::%object-of-type-p (object object-tag)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (%%object-of-type-p object object-tag)))

(defun %%object-of-type-range-p (object first-object-tag last-object-tag)
  (<= first-object-tag
      (sys.int::%object-tag object)
      last-object-tag))

(declaim (inline sys.int::%object-of-type-range-p))
(defun sys.int::%object-of-type-range-p (object first-object-tag last-object-tag)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (%%object-of-type-range-p object first-object-tag last-object-tag)))

(declaim (inline sys.int::%type-check))
(defun sys.int::%type-check (object object-tag expected-type)
  (unless (sys.int::%object-of-type-p object object-tag)
    (sys.int::raise-type-error object expected-type)
    (sys.int::%%unreachable)))

(defun sys.int::%value-has-tag-p (value tag)
  (eql (sys.int::%tag-field value) tag))

(defun sys.int::%value-has-immediate-tag-p (object immediate-tag)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-immediate+)
       (eq (ldb sys.int::+immediate-tag+ (sys.int::lisp-object-address object))
           immediate-tag)))

(declaim (inline characterp))
(defun characterp (object)
  (sys.int::%value-has-immediate-tag-p object sys.int::+immediate-tag-character+))

(declaim (inline functionp))
(defun functionp (object)
  (sys.int::%object-of-type-range-p
   object
   sys.int::+first-function-object-tag+
   sys.int::+last-function-object-tag+))

(in-package :mezzano.internals)

(defun %progv (symbols values fn)
  (cond (symbols
         ;; Bind one.
         ;; Bindings must be done one at a time because the compiler
         ;; cannot emit an arbitrary number of special stack entries in a
         ;; single function.
         ;; It'd be possible to do in assembly, but complicated enough
         ;; that it'd not be worthwhile.
         (let ((symbol (car symbols))
               (value (if values
                          (car values)
                          (%unbound-value))))
           (check-type symbol symbol)
           (mezzano.runtime::modifying-symbol-value symbol)
           (when values
             (mezzano.runtime::check-symbol-value-type value symbol))
           (%%bind (mezzano.runtime::symbol-global-value-cell symbol) value)
           (multiple-value-prog1
               (%progv (cdr symbols) (cdr values) fn)
             (%%unbind))))
        (t ;; No more to bind
         (funcall fn))))

(defun within-functin-area-p (address)
  (<= sys.int::*wired-function-area-limit*
      address
      sys.int::*function-area-limit*))

(defun return-address-to-function (return-address)
  "Convert a return address to a function pointer.
Dangerous! The return address must be kept live as a return address on a
thread's stack if this function is called from normal code."
  ;; Return address must be within the function area.
  (mezzano.supervisor:ensure
   (within-functin-area-p return-address)
   "Return address " return-address " in wrong area")
  ;; Walk backwards looking for an object header with a function type and
  ;; an appropriate entry point.
  (loop
     with address = (logand return-address -16)
     with raise-undefined-function-fref-addr =
       (sys.int::%function-reference-code-location
        (get-raise-undefined-function-fref))
     ;; Be careful when reading to avoid bignums.
     for potential-header-type = (ldb (byte +object-type-size+ +object-type-shift+)
                                      (memref-unsigned-byte-8 address 0))
     do
       (when (or (and
                  ;; Only compiled functions contain code.
                  (eql potential-header-type +object-tag-function+)
                  ;; Check entry point halves individually, avoiding bignums.
                  ;; Currently the entry point of every non-closure function
                  ;; points to the base-address + 16.
                  (eql (logand (+ address 16) #xFFFFFFFF)
                       (memref-unsigned-byte-32 (+ address 8) 0))
                  (eql (logand (ash (+ address 16) -32) #xFFFFFFFF)
                       (memref-unsigned-byte-32 (+ address 12) 0)))
                 (and
                  ;; Frefs do too...
                  (eql potential-header-type +object-tag-function-reference+)
                  ;; The entry point of an fref always points at the address
                  ;; of the raise-undefined-function fref entry point.
                  (eql (logand raise-undefined-function-fref-addr #xFFFFFFFF)
                       (memref-unsigned-byte-32 (+ address 8) 0))
                  (eql (logand (ash raise-undefined-function-fref-addr -32) #xFFFFFFFF)
                       (memref-unsigned-byte-32 (+ address 12) 0))))
         (return (values (%%assemble-value address sys.int::+tag-object+)
                         (- return-address address))))
       (decf address 16)))

(declaim (inline memref-t (setf memref-t) (cas memref-t)))
(defun memref-t (base &optional (index 0))
  (%memref-t base index))
(defun (setf memref-t) (value base &optional (index 0))
  (setf (%memref-t base index) value))
(defun (cas memref-t) (old new base &optional (index 0))
  (cas (%memref-t base index) old new))

(declaim (inline memref-unsigned-byte-8 (setf memref-unsigned-byte-8) (cas memref-unsigned-byte-8)))
(defun memref-unsigned-byte-8 (base &optional (index 0))
  (%memref-unsigned-byte-8 base index))
(defun (setf memref-unsigned-byte-8) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-8 base index) value))
(defun (cas memref-unsigned-byte-8) (old new base &optional (index 0))
  (cas (%memref-unsigned-byte-8 base index) old new))

(declaim (inline memref-unsigned-byte-16 (setf memref-unsigned-byte-16) (cas memref-unsigned-byte-16)))
(defun memref-unsigned-byte-16 (base &optional (index 0))
  (%memref-unsigned-byte-16 base index))
(defun (setf memref-unsigned-byte-16) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-16 base index) value))
(defun (cas memref-unsigned-byte-16) (old new base &optional (index 0))
  (cas (%memref-unsigned-byte-16 base index) old new))

(declaim (inline memref-unsigned-byte-32 (setf memref-unsigned-byte-32) (cas memref-unsigned-byte-32)))
(defun memref-unsigned-byte-32 (base &optional (index 0))
  (%memref-unsigned-byte-32 base index))
(defun (setf memref-unsigned-byte-32) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-32 base index) value))
(defun (cas memref-unsigned-byte-32) (old new base &optional (index 0))
  (cas (%memref-unsigned-byte-32 base index) old new))

(declaim (inline memref-unsigned-byte-64 (setf memref-unsigned-byte-64) (cas memref-unsigned-byte-64)))
(defun memref-unsigned-byte-64 (base &optional (index 0))
  (%memref-unsigned-byte-64 base index))
(defun (setf memref-unsigned-byte-64) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-64 base index) value))
(defun (cas memref-unsigned-byte-64) (old new base &optional (index 0))
  (cas (%memref-unsigned-byte-64 base index) old new))

(declaim (inline memref-signed-byte-8 (setf memref-signed-byte-8) (cas memref-signed-byte-8)))
(defun memref-signed-byte-8 (base &optional (index 0))
  (%memref-signed-byte-8 base index))
(defun (setf memref-signed-byte-8) (value base &optional (index 0))
  (setf (%memref-signed-byte-8 base index) value))
(defun (cas memref-signed-byte-8) (old new base &optional (index 0))
  (cas (%memref-signed-byte-8 base index) old new))

(declaim (inline memref-signed-byte-16 (setf memref-signed-byte-16) (cas memref-signed-byte-16)))
(defun memref-signed-byte-16 (base &optional (index 0))
  (%memref-signed-byte-16 base index))
(defun (setf memref-signed-byte-16) (value base &optional (index 0))
  (setf (%memref-signed-byte-16 base index) value))
(defun (cas memref-signed-byte-16) (old new base &optional (index 0))
  (cas (%memref-signed-byte-16 base index) old new))

(declaim (inline memref-signed-byte-32 (setf memref-signed-byte-32) (cas memref-signed-byte-32)))
(defun memref-signed-byte-32 (base &optional (index 0))
  (%memref-signed-byte-32 base index))
(defun (setf memref-signed-byte-32) (value base &optional (index 0))
  (setf (%memref-signed-byte-32 base index) value))
(defun (cas memref-signed-byte-32) (old new base &optional (index 0))
  (cas (%memref-signed-byte-32 base index) old new))

(declaim (inline memref-signed-byte-64 (setf memref-signed-byte-64) (cas memref-signed-byte-64)))
(defun memref-signed-byte-64 (base &optional (index 0))
  (%memref-signed-byte-64 base index))
(defun (setf memref-signed-byte-64) (value base &optional (index 0))
  (setf (%memref-signed-byte-64 base index) value))
(defun (cas memref-signed-byte-64) (old new base &optional (index 0))
  (cas (%memref-signed-byte-64 base index) old new))

(declaim (inline memref-single-float (setf memref-single-float) (cas memref-single-float)))
(defun memref-single-float (base &optional (index 0))
  (%integer-as-single-float (%memref-unsigned-byte-32 base index)))
(defun (setf memref-single-float) (value base &optional (index 0))
  (check-type value single-float)
  (setf (%memref-unsigned-byte-32 base index) (%single-float-as-integer value)))
(defun (cas memref-single-float) (old new base &optional (index 0))
  (check-type old single-float)
  (check-type new single-float)
  (%integer-as-single-float
   (cas (%memref-unsigned-byte-32 base index)
        (%single-float-as-integer old)
        (%single-float-as-integer new))))

(declaim (inline memref-double-float (setf memref-double-float) (cas memref-double-float)))
(defun memref-double-float (base &optional (index 0))
  (%integer-as-double-float (%memref-unsigned-byte-64 base index)))
(defun (setf memref-double-float) (value base &optional (index 0))
  (check-type value double-float)
  (setf (%memref-unsigned-byte-64 base index) (%double-float-as-integer value)))
(defun (cas memref-double-float) (old new base &optional (index 0))
  (check-type old double-float)
  (check-type new double-float)
  (%integer-as-double-float
   (cas (%memref-unsigned-byte-64 base index)
        (%double-float-as-integer old)
        (%double-float-as-integer new))))

(declaim (inline (cas %object-ref-t)))
(defun %object-ref-t (object index)
  (%object-ref-t object index))
(defun (setf %object-ref-t) (value object index)
  (setf (%object-ref-t object index) value))
(defun (cas %object-ref-t) (old new object index)
  (multiple-value-bind (successp actual-value)
      (%cas-object object index old new)
    (declare (ignore successp))
    actual-value))

(declaim (inline %object-ref-unsigned-byte-8 (setf %object-ref-unsigned-byte-8) (cas %object-ref-unsigned-byte-8)))
(defun %object-ref-unsigned-byte-8 (object index)
  (%%object-ref-unsigned-byte-8 object index))
(defun (setf %object-ref-unsigned-byte-8) (value object index)
  (check-type value (unsigned-byte 8))
  (setf (%%object-ref-unsigned-byte-8 object index) value))
(defun (cas %object-ref-unsigned-byte-8) (old new object index)
  (check-type old (unsigned-byte 8))
  (check-type new (unsigned-byte 8))
  (cas (%%object-ref-unsigned-byte-8 object index) old new))

(declaim (inline %object-ref-unsigned-byte-16 (setf %object-ref-unsigned-byte-16) (cas %object-ref-unsigned-byte-16)))
(defun %object-ref-unsigned-byte-16 (object index)
  (%%object-ref-unsigned-byte-16 object index))
(defun (setf %object-ref-unsigned-byte-16) (value object index)
  (check-type value (unsigned-byte 16))
  (setf (%%object-ref-unsigned-byte-16 object index) value))
(defun (cas %object-ref-unsigned-byte-16) (old new object index)
  (check-type old (unsigned-byte 16))
  (check-type new (unsigned-byte 16))
  (cas (%%object-ref-unsigned-byte-16 object index) old new))

(declaim (inline %object-ref-unsigned-byte-32 (setf %object-ref-unsigned-byte-32) (cas %object-ref-unsigned-byte-32)))
(defun %object-ref-unsigned-byte-32 (object index)
  (%%object-ref-unsigned-byte-32 object index))
(defun (setf %object-ref-unsigned-byte-32) (value object index)
  (check-type value (unsigned-byte 32))
  (setf (%%object-ref-unsigned-byte-32 object index) value))
(defun (cas %object-ref-unsigned-byte-32) (old new object index)
  (check-type old (unsigned-byte 32))
  (check-type new (unsigned-byte 32))
  (cas (%%object-ref-unsigned-byte-32 object index) old new))

(declaim (inline %object-ref-unsigned-byte-64 (setf %object-ref-unsigned-byte-64) (cas %object-ref-unsigned-byte-64)))
(defun %object-ref-unsigned-byte-64 (object index)
  (%%object-ref-unsigned-byte-64 object index))
(defun (setf %object-ref-unsigned-byte-64) (value object index)
  (check-type value (unsigned-byte 64))
  (setf (%%object-ref-unsigned-byte-64 object index) value))
(defun (cas %object-ref-unsigned-byte-64) (old new object index)
  (check-type old (unsigned-byte 64))
  (check-type new (unsigned-byte 64))
  (cas (%%object-ref-unsigned-byte-64 object index) old new))

(declaim (inline %object-ref-signed-byte-8 (setf %object-ref-signed-byte-8) (cas %object-ref-signed-byte-8)))
(defun %object-ref-signed-byte-8 (object index)
  (%%object-ref-signed-byte-8 object index))
(defun (setf %object-ref-signed-byte-8) (value object index)
  (check-type value (signed-byte 8))
  (setf (%%object-ref-signed-byte-8 object index) value))
(defun (cas %object-ref-signed-byte-8) (old new object index)
  (check-type old (signed-byte 8))
  (check-type new (signed-byte 8))
  (cas (%%object-ref-signed-byte-8 object index) old new))

(declaim (inline %object-ref-signed-byte-16 (setf %object-ref-signed-byte-16) (cas %object-ref-signed-byte-16)))
(defun %object-ref-signed-byte-16 (object index)
  (%%object-ref-signed-byte-16 object index))
(defun (setf %object-ref-signed-byte-16) (value object index)
  (check-type value (signed-byte 16))
  (setf (%%object-ref-signed-byte-16 object index) value))
(defun (cas %object-ref-signed-byte-16) (old new object index)
  (check-type old (signed-byte 16))
  (check-type new (signed-byte 16))
  (cas (%%object-ref-signed-byte-16 object index) old new))

(declaim (inline %object-ref-signed-byte-32 (setf %object-ref-signed-byte-32) (cas %object-ref-signed-byte-32)))
(defun %object-ref-signed-byte-32 (object index)
  (%%object-ref-signed-byte-32 object index))
(defun (setf %object-ref-signed-byte-32) (value object index)
  (check-type value (signed-byte 32))
  (setf (%%object-ref-signed-byte-32 object index) value))
(defun (cas %object-ref-signed-byte-32) (old new object index)
  (check-type old (signed-byte 32))
  (check-type new (signed-byte 32))
  (cas (%%object-ref-signed-byte-32 object index) old new))

(declaim (inline %object-ref-signed-byte-64 (setf %object-ref-signed-byte-64) (cas %object-ref-signed-byte-64)))
(defun %object-ref-signed-byte-64 (object index)
  (%%object-ref-signed-byte-64 object index))
(defun (setf %object-ref-signed-byte-64) (value object index)
  (check-type value (signed-byte 64))
  (setf (%%object-ref-signed-byte-64 object index) value))
(defun (cas %object-ref-signed-byte-64) (old new object index)
  (check-type old (signed-byte 64))
  (check-type new (signed-byte 64))
  (cas (%%object-ref-signed-byte-64 object index) old new))

(declaim (inline %object-ref-short-float (setf %object-ref-short-float) (cas %object-ref-short-float)))
(defun %object-ref-short-float (object index)
  (%%object-ref-short-float object index))
(defun (setf %object-ref-short-float) (value object index)
  (check-type value short-float)
  (setf (%%object-ref-short-float object index) value)
  value)
(defun (cas %object-ref-short-float) (old new object index)
  (check-type old short-float)
  (check-type new short-float)
  (cas (%%object-ref-short-float object index) old new))

(declaim (inline %%object-ref-short-float (setf %%object-ref-short-float) (cas %%object-ref-short-float)))
(defun %%object-ref-short-float (object index)
  (%integer-as-short-float (%%object-ref-unsigned-byte-16 object index)))
(defun (setf %%object-ref-short-float) (value object index)
  (setf (%%object-ref-unsigned-byte-16 object index)
        (%short-float-as-integer value))
  value)
(defun (cas %%object-ref-short-float) (old new object index)
  (%integer-as-short-float
   (cas (%%object-ref-unsigned-byte-16 object index)
        (%short-float-as-integer old)
        (%short-float-as-integer new))))

(declaim (inline %object-ref-single-float (setf %object-ref-single-float) (cas %object-ref-single-float)))
(defun %object-ref-single-float (object index)
  (%%object-ref-single-float object index))
(defun (setf %object-ref-single-float) (value object index)
  (check-type value single-float)
  (setf (%%object-ref-single-float object index) value)
  value)
(defun (cas %object-ref-single-float) (old new object index)
  (check-type old single-float)
  (check-type new single-float)
  (cas (%%object-ref-single-float object index) old new))

(declaim (inline %%object-ref-single-float (setf %%object-ref-single-float) (cas %%object-ref-single-float)))
(defun %%object-ref-single-float (object index)
  (%integer-as-single-float (%%object-ref-unsigned-byte-32 object index)))
(defun (setf %%object-ref-single-float) (value object index)
  (setf (%%object-ref-unsigned-byte-32 object index)
        (%single-float-as-integer value))
  value)
(defun (cas %%object-ref-single-float) (old new object index)
  (%integer-as-single-float
   (cas (%%object-ref-unsigned-byte-32 object index)
        (%single-float-as-integer old)
        (%single-float-as-integer new))))

(declaim (inline %object-ref-double-float (setf %object-ref-double-float) (cas %object-ref-double-float)))
(defun %object-ref-double-float (object index)
  (%integer-as-double-float (%object-ref-unsigned-byte-64 object index)))
(defun (setf %object-ref-double-float) (value object index)
  (check-type value double-float)
  (setf (%object-ref-unsigned-byte-64 object index)
        (%double-float-as-integer value))
  value)
(defun (cas %object-ref-double-float) (old new object index)
  (check-type old double-float)
  (check-type new double-float)
  (%integer-as-double-float
   (cas (%object-ref-unsigned-byte-64 object index)
        (%double-float-as-integer old)
        (%double-float-as-integer new))))

(declaim (inline %object-ref-unsigned-byte-8-unscaled (setf %object-ref-unsigned-byte-8-unscaled) (cas %object-ref-unsigned-byte-8-unscaled)))
(defun %object-ref-unsigned-byte-8-unscaled (object index)
  (%%object-ref-unsigned-byte-8-unscaled object index))
(defun (setf %object-ref-unsigned-byte-8-unscaled) (value object index)
  (check-type value (unsigned-byte 8))
  (setf (%%object-ref-unsigned-byte-8-unscaled object index) value))
(defun (cas %object-ref-unsigned-byte-8-unscaled) (old new object index)
  (check-type old (unsigned-byte 8))
  (check-type new (unsigned-byte 8))
  (cas (%%object-ref-unsigned-byte-8-unscaled object index) old new))

(declaim (inline %object-ref-unsigned-byte-16-unscaled (setf %object-ref-unsigned-byte-16-unscaled) (cas %object-ref-unsigned-byte-16-unscaled)))
(defun %object-ref-unsigned-byte-16-unscaled (object index)
  (%%object-ref-unsigned-byte-16-unscaled object index))
(defun (setf %object-ref-unsigned-byte-16-unscaled) (value object index)
  (check-type value (unsigned-byte 16))
  (setf (%%object-ref-unsigned-byte-16-unscaled object index) value))
(defun (cas %object-ref-unsigned-byte-16-unscaled) (old new object index)
  (check-type old (unsigned-byte 16))
  (check-type new (unsigned-byte 16))
  (cas (%%object-ref-unsigned-byte-16-unscaled object index) old new))

(declaim (inline %object-ref-unsigned-byte-32-unscaled (setf %object-ref-unsigned-byte-32-unscaled) (cas %object-ref-unsigned-byte-32-unscaled)))
(defun %object-ref-unsigned-byte-32-unscaled (object index)
  (%%object-ref-unsigned-byte-32-unscaled object index))
(defun (setf %object-ref-unsigned-byte-32-unscaled) (value object index)
  (check-type value (unsigned-byte 32))
  (setf (%%object-ref-unsigned-byte-32-unscaled object index) value))
(defun (cas %object-ref-unsigned-byte-32-unscaled) (old new object index)
  (check-type old (unsigned-byte 32))
  (check-type new (unsigned-byte 32))
  (cas (%%object-ref-unsigned-byte-32-unscaled object index) old new))

(declaim (inline %object-ref-unsigned-byte-64-unscaled (setf %object-ref-unsigned-byte-64-unscaled) (cas %object-ref-unsigned-byte-64-unscaled)))
(defun %object-ref-unsigned-byte-64-unscaled (object index)
  (%%object-ref-unsigned-byte-64-unscaled object index))
(defun (setf %object-ref-unsigned-byte-64-unscaled) (value object index)
  (check-type value (unsigned-byte 64))
  (setf (%%object-ref-unsigned-byte-64-unscaled object index) value))
(defun (cas %object-ref-unsigned-byte-64-unscaled) (old new object index)
  (check-type old (unsigned-byte 64))
  (check-type new (unsigned-byte 64))
  (cas (%%object-ref-unsigned-byte-64-unscaled object index) old new))

(declaim (inline %object-ref-signed-byte-8-unscaled (setf %object-ref-signed-byte-8-unscaled) (cas %object-ref-signed-byte-8-unscaled)))
(defun %object-ref-signed-byte-8-unscaled (object index)
  (%%object-ref-signed-byte-8-unscaled object index))
(defun (setf %object-ref-signed-byte-8-unscaled) (value object index)
  (check-type value (signed-byte 8))
  (setf (%%object-ref-signed-byte-8-unscaled object index) value))
(defun (cas %object-ref-signed-byte-8-unscaled) (old new object index)
  (check-type old (signed-byte 8))
  (check-type new (signed-byte 8))
  (cas (%%object-ref-signed-byte-8-unscaled object index) old new))

(declaim (inline %object-ref-signed-byte-16-unscaled (setf %object-ref-signed-byte-16-unscaled) (cas %object-ref-signed-byte-16-unscaled)))
(defun %object-ref-signed-byte-16-unscaled (object index)
  (%%object-ref-signed-byte-16-unscaled object index))
(defun (setf %object-ref-signed-byte-16-unscaled) (value object index)
  (check-type value (signed-byte 16))
  (setf (%%object-ref-signed-byte-16-unscaled object index) value))
(defun (cas %object-ref-signed-byte-16-unscaled) (old new object index)
  (check-type old (signed-byte 16))
  (check-type new (signed-byte 16))
  (cas (%%object-ref-signed-byte-16-unscaled object index) old new))

(declaim (inline %object-ref-signed-byte-32-unscaled (setf %object-ref-signed-byte-32-unscaled) (cas %object-ref-signed-byte-32-unscaled)))
(defun %object-ref-signed-byte-32-unscaled (object index)
  (%%object-ref-signed-byte-32-unscaled object index))
(defun (setf %object-ref-signed-byte-32-unscaled) (value object index)
  (check-type value (signed-byte 32))
  (setf (%%object-ref-signed-byte-32-unscaled object index) value))
(defun (cas %object-ref-signed-byte-32-unscaled) (old new object index)
  (check-type old (signed-byte 32))
  (check-type new (signed-byte 32))
  (cas (%%object-ref-signed-byte-32-unscaled object index) old new))

(declaim (inline %object-ref-signed-byte-64-unscaled (setf %object-ref-signed-byte-64-unscaled) (cas %object-ref-signed-byte-64-unscaled)))
(defun %object-ref-signed-byte-64-unscaled (object index)
  (%%object-ref-signed-byte-64-unscaled object index))
(defun (setf %object-ref-signed-byte-64-unscaled) (value object index)
  (check-type value (signed-byte 64))
  (setf (%%object-ref-signed-byte-64-unscaled object index) value))
(defun (cas %object-ref-signed-byte-64-unscaled) (old new object index)
  (check-type old (signed-byte 64))
  (check-type new (signed-byte 64))
  (cas (%%object-ref-signed-byte-64-unscaled object index) old new))

(declaim (inline %object-ref-single-float-unscaled (setf %object-ref-single-float-unscaled) (cas %object-ref-single-float-unscaled)))
(defun %object-ref-single-float-unscaled (object index)
  (%%object-ref-single-float-unscaled object index))
(defun (setf %object-ref-single-float-unscaled) (value object index)
  (check-type value single-float)
  (setf (%%object-ref-single-float-unscaled object index) value)
  value)
(defun (cas %object-ref-single-float-unscaled) (old new object index)
  (check-type old single-float)
  (check-type new single-float)
  (cas (%%object-ref-single-float-unscaled object index) old new))

(declaim (inline %%object-ref-single-float-unscaled (setf %%object-ref-single-float-unscaled) (cas %%object-ref-single-float-unscaled)))
(defun %%object-ref-single-float-unscaled (object index)
  (%integer-as-single-float (%%object-ref-unsigned-byte-32-unscaled object index)))
(defun (setf %%object-ref-single-float-unscaled) (value object index)
  (setf (%%object-ref-unsigned-byte-32-unscaled object index)
        (%single-float-as-integer value))
  value)
(defun (cas %%object-ref-single-float-unscaled) (old new object index)
  (%integer-as-single-float
   (cas (%%object-ref-unsigned-byte-32-unscaled object index)
        (%single-float-as-integer old)
        (%single-float-as-integer new))))

(declaim (inline %object-ref-double-float-unscaled (setf %object-ref-double-float-unscaled) (cas %object-ref-double-float-unscaled)))
(defun %object-ref-double-float-unscaled (object index)
  (%integer-as-double-float (%object-ref-unsigned-byte-64-unscaled object index)))
(defun (setf %object-ref-double-float-unscaled) (value object index)
  (check-type value double-float)
  (setf (%object-ref-unsigned-byte-64-unscaled object index)
        (%double-float-as-integer value))
  value)
(defun (cas %%object-ref-double-float-unscaled) (old new object index)
  (check-type old double-float)
  (check-type new double-float)
  (%integer-as-double-float
   (cas (%%object-ref-unsigned-byte-64-unscaled object index)
        (%double-float-as-integer old)
        (%double-float-as-integer new))))

(declaim (inline %in-bounds-p))
(defun %in-bounds-p (index limit)
  "Test 0 <= INDEX < LIMIT. INDEX & LIMIT must both be fixnums."
  ;; Negative 2's compliment values are extremely large positive values
  ;; when treated as unsigned, this folds two tests into one.
  (mezzano.runtime::%fixnum-<-unsigned (the fixnum index) (the fixnum limit)))

(declaim (inline %bounds-check))
(defun %bounds-check (object slot)
  (unless (fixnump slot)
    (raise-type-error slot 'fixnum)
    (sys.int::%%unreachable))
  (unless (%in-bounds-p slot (%object-header-data object))
    (raise-bounds-error object slot)
    (sys.int::%%unreachable)))

(declaim (inline %bounds-check-range))
(defun %bounds-check-range (object slot range)
  (unless (fixnump slot)
    (raise-type-error slot 'fixnum)
    (sys.int::%%unreachable))
  (unless (and (not (mezzano.runtime::%fixnum-< slot 0))
               (mezzano.runtime::%fixnum-< slot (mezzano.compiler::%fast-fixnum--
                                                 (%object-header-data object)
                                                 range)))
    (raise-bounds-range-error object slot range)
    (sys.int::%%unreachable)))

(declaim (inline %complex-bounds-check))
(defun %complex-bounds-check (array index dim axis)
  (unless (fixnump index)
    (raise-type-error index 'fixnum)
    (sys.int::%%unreachable))
  (unless (%in-bounds-p index dim)
    (sys.int::raise-complex-bounds-error array index dim axis)
    (sys.int::%%unreachable)))

;; This is handled specially by the compiler & doesn't go through the
;; normal builtin mechanism. Specifically provide a wrapper function
;; for it.
(defun %%unreachable ()
  (%%unreachable))

(defun encode-weak-pointer-weakness (weakness)
  (ecase weakness
    (:key +weak-pointer-weakness-key+)
    (:value +weak-pointer-weakness-value+)
    (:key-and-value +weak-pointer-weakness-and+)
    (:key-or-value +weak-pointer-weakness-or+)))

(defun decode-weak-pointer-weakness (weakness)
  (ecase weakness
    (#.+weak-pointer-weakness-key+ :key)
    (#.+weak-pointer-weakness-value+ :value)
    (#.+weak-pointer-weakness-and+ :key-and-value)
    (#.+weak-pointer-weakness-or+ :key-or-value)))
