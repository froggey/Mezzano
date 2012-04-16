(in-package #:genesis)

(defstruct genesis-function
  (lap-function nil)
  source
  source-environment
  (suppress-builtins :default)
  lap-code
  assembled-code
  constants)

(defstruct genesis-closure
  function
  environment)

(defstruct genesis-stack-group
  name
  control-stack-base
  control-stack-size
  data-stack-base
  data-stack-size
  binding-stack-base
  binding-stack-size)

(defvar *function-preloads* nil)
(defvar *symbol-preloads* nil)

(defvar *crunched-symbol-names* (make-hash-table :weakness :key))

(defun crunched-symbol-name (symbol)
  (alexandria:ensure-gethash symbol *crunched-symbol-names*
                             (crunch-string (symbol-name symbol))))

(defgeneric map-slots (fn value))

(defmethod map-slots (fn (value symbol))
  (funcall fn (crunched-symbol-name value))
  (when (boundp value)
    (funcall fn (symbol-value value)))
  (let ((p (genesis-symbol-package value)))
    (when p
      (funcall fn p)))
  (let ((x (assoc value *function-preloads*)))
    (cond (x (funcall fn (cdr x)))
          ((fboundp value)
           (funcall fn (symbol-function value)))))
  (when (genesis-symbol-plist value)
    (funcall fn (genesis-symbol-plist value))))

(defmethod map-slots (fn (value vector))
  (dotimes (i (array-dimension value 0))
    (funcall fn (aref value i))))

(defmethod map-slots (fn (value character)))

;;; Neither bignums nor fixnums have any slots.
(defmethod map-slots (fn (value integer)))

(defmethod map-slots (fn (value genesis-struct))
  (let ((slots (genesis-struct-slots value)))
    (dotimes (i (array-dimension slots 0))
      (funcall fn (aref slots i)))))

(defmethod map-slots (fn (value cons))
  (funcall fn (car value))
  (funcall fn (cdr value)))

(defmethod map-slots (fn (value genesis-function))
  ;; TODO...
  (when (genesis-function-source value)
    (funcall fn (genesis-function-source value)))
  (dotimes (i (length (genesis-function-constants value)))
    (funcall fn (elt (genesis-function-constants value) i))))

(defmethod map-slots (fn (value genesis-closure))
  (funcall fn (genesis-closure-function value))
  (funcall fn (genesis-closure-environment value)))

(defmethod map-slots (fn (value genesis-std-instance))
  (funcall fn (genesis-std-instance-class value))
  (funcall fn (genesis-std-instance-slots value)))

(defmethod map-slots (fn (value array-header))
  (funcall fn (array-header-dimensions value))
  (funcall fn (array-header-fill-pointer value))
  (funcall fn (array-header-info value))
  (funcall fn (array-header-storage value)))

(defmethod map-slots (fn (value genesis-stack-group))
  (funcall fn (genesis-stack-group-name value))
  (funcall fn (genesis-stack-group-control-stack-base value))
  (funcall fn (genesis-stack-group-control-stack-size value))
  (funcall fn (genesis-stack-group-data-stack-base value))
  (funcall fn (genesis-stack-group-data-stack-size value))
  (funcall fn (genesis-stack-group-binding-stack-base value))
  (funcall fn (genesis-stack-group-binding-stack-size value)))

(defun object-size (x)
  "Return the size of an object in words."
  (etypecase x
    (symbol 6)
    (genesis-struct
     ;; 1 word header, one word per slot.
     (+ 1 (length (genesis-struct-slots x))))
    (cons 2)
    (genesis-std-instance 2)
    (array-header 4)
    ((vector base-char)
     ;; One 8-bit byte per character, plus 1 word header.
     (+ 1 (ceiling (array-dimension x 0) 8)))
    ((vector character)
     ;; 4 8-bit bytes per character, plus 1 word header.
     (+ 1 (ceiling (* (array-dimension x 0) 4) 8)))
    ((vector (unsigned-byte 8))
     ;; 1 8-bit bytes per element, plus 1 word header.
     (+ 1 (ceiling (array-dimension x 0) 8)))
    ((vector (unsigned-byte 16))
     ;; 2 8-bit bytes per element, plus 1 word header.
     (+ 1 (ceiling (* (array-dimension x 0) 2) 8)))
    ((vector (unsigned-byte 32))
     ;; 4 8-bit bytes per element, plus 1 word header.
     (+ 1 (ceiling (* (array-dimension x 0) 4) 8)))
    ((vector (unsigned-byte 64))
     ;; 8 8-bit bytes per element, plus 1 word header.
     (+ 1 (ceiling (* (array-dimension x 0) 8) 8)))
    ((vector t)
     ;; One word per element, plus 1 word header.
     (+ 1 (ceiling (* (array-dimension x 0) 8) 8)))
    (genesis-closure 6)
    (integer
     ;; TODO: bignum
     (let ((word-count (1+ (ceiling (1+ (integer-length x)) 64))))
       (when (oddp word-count)
         (incf word-count))
       word-count))
    (genesis-function
     (+ (length (genesis-function-constants x))
	(* (ceiling (+ (length (genesis-function-assembled-code x)) 12) 16) 2)))
    (genesis-stack-group 512)))

(defun object-tag (x)
  "Return the tag of an object."
  (etypecase x
    (symbol (symbol-value (genesis-intern "+TAG-SYMBOL+")))
    (genesis-struct (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    (cons (symbol-value (genesis-intern "+TAG-CONS+")))
    (genesis-std-instance (symbol-value (genesis-intern "+TAG-STD-INSTANCE+")))
    (array-header (symbol-value (genesis-intern "+TAG-ARRAY-HEADER+")))
    ((vector base-char) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    ((vector character) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    ((vector (unsigned-byte 8)) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    ((vector (unsigned-byte 16)) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    ((vector (unsigned-byte 32)) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    ((vector (unsigned-byte 64)) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    ((vector t) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    (integer (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))
    (genesis-closure (symbol-value (genesis-intern "+TAG-FUNCTION+")))
    (genesis-function (symbol-value (genesis-intern "+TAG-FUNCTION+")))
    (genesis-stack-group (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))

;;; Initial memory layout:
;;; 0-2MB         Not mapped, catching bad accesses to zero.
;;; 2MB-2GB       Static region.
;;; 2GB-3GB       Dynamic area A.
;;; 3GB-4GB       Dynamic area B (second semi-space).
;;; 512GB-1024GB  Physical memory linear map. (second Page Directory Pointer table entry)
;;; How about a read-only region somewhere?
(defparameter *static-area-base*  #x0000200000)
(defparameter *static-area-size*  (- #x0080000000 *static-area-base*))
(defparameter *dynamic-area-base* #x0080000000)
(defparameter *dynamic-area-size* #x0080000000) ; 2GB
(defparameter *linear-map*        #x8000000000)

(defconstant +page-table-present+  #b0000000000001)
(defconstant +page-table-writable+ #b0000000000010)
(defconstant +page-table-user+     #b0000000000100)
(defconstant +page-table-pwt+      #b0000000001000)
(defconstant +page-table-pcd+      #b0000000010000)
(defconstant +page-table-accessed+ #b0000000100000)
(defconstant +page-table-dirty+    #b0000001000000)
(defconstant +page-table-large+    #b0000010000000)
(defconstant +page-table-global+   #b0000100000000)
(defconstant +page-table-pat+      #b1000000000000)

(defvar *lap-symbols* nil)

(defun convert-environment (env)
  "Convert a Genesis eval environment to a compiler environment."
  ;; Compiler environments are currently annoyingly complicated and actually
  ;; consist of two seperate environments.
  ;; The first environment is used by pass1 and describes the which variables
  ;; are lexical variables and which are special variables.
  ;; The second environment is used by the code generator and describes the
  ;; layout of environment vectors.
  (let ((lexical-variables '()))
    (labels ((make-lexical-variable (def)
               ;; Call in to the compiler and allocate a variable.
               (genesis-eval (list (genesis-eval (list (genesis-intern "INTERN")
                                                       "MAKE-LEXICAL-VARIABLE"
                                                       "SYS.C"))
                                   (genesis-intern "NAME" t)
                                   (list (genesis-intern "QUOTE")
                                         (slot-value def 'name)))))
             (frob-for-pass1 (e)
               (when e
                 (cons (let ((node (first e)))
                         (ecase (first node)
                           (:bindings
                            (append (list (genesis-intern "BINDINGS" t))
                                    ;; Special variables.
                                    (mapcar (lambda (var)
                                              (cons var var))
                                            (second node))
                                    (mapcar (lambda (var)
                                              (let ((cvar (make-lexical-variable var)))
                                                (push (cons var cvar)
                                                      lexical-variables)
                                                (cons (slot-value var 'name) cvar)))
                                            (third node))))))
                       (frob-for-pass1 (rest e)))))
             (frob-for-codegen (e)
               (when e
                 (let ((node (first e)))
                   (ecase (first node)
                     (:bindings
                      (let ((vars (mapcar (lambda (v)
                                            (cdr (assoc v lexical-variables)))
                                          (remove-if 'local-p (third node)))))
                        (if vars
                            (cons (nreverse vars) (frob-for-codegen (rest e)))
                            (frob-for-codegen (rest e))))))))))
      (cons (frob-for-pass1 env)
            (frob-for-codegen env)))))

(defun strip-array-header (vector)
  (cond ((array-header-p vector)
	 (subseq (array-header-storage vector)
		 0
		 (or (array-header-fill-pointer vector)
		     nil)))
	(t vector)))

(defun assemble-lap (code &optional name)
  (multiple-value-bind (mc constants)
      (genesis-eval (list (genesis-eval (list (genesis-intern "INTERN") "ASSEMBLE" "SYS.LAP-X86"))
                          (list (genesis-intern "QUOTE") code)
                          (genesis-intern "BASE-ADDRESS" t) 12
                          (genesis-intern "INITIAL-SYMBOLS" t) (list (genesis-intern "QUOTE") *lap-symbols*)
                          (genesis-intern "INFO" t) (list (genesis-intern "QUOTE") (list name))))
    (values (strip-array-header mc) (strip-array-header constants))))

(defun compile-genesis-function (object)
  (cond ((genesis-function-lap-function object)
         (multiple-value-bind (assembled-code constants)
             (assemble-lap (cddr (genesis-function-source object))
                           (first (second (genesis-function-source object))))
           (setf (genesis-function-lap-code object) (cddr (genesis-function-source object))
                 (genesis-function-assembled-code object) (strip-array-header assembled-code)
                 (genesis-function-constants object) (strip-array-header constants))))
        (t (progv (list (genesis-eval (list (genesis-intern "INTERN") "*SUPPRESS-BUILTINS*" "SYS.C")))
               (list (cond ((eql (genesis-function-suppress-builtins object) :default)
                            (symbol-value (genesis-eval (list (genesis-intern "INTERN") "*SUPPRESS-BUILTINS*" "SYS.C"))))
                           (t (genesis-function-suppress-builtins object))))
             (let ((fn (genesis-eval (list (genesis-eval (list (genesis-intern "INTERN") "COMPILE-LAMBDA" "SYS.C"))
                                           (list (genesis-intern "QUOTE") (genesis-function-source object))
                                           (list (genesis-intern "QUOTE") (convert-environment (genesis-function-source-environment object)))))))
               #+nil(let ((*print-circle* nil))
                      (format t "Asm: ~S~%" (genesis-function-lap-code fn)))
               (setf (genesis-function-lap-code object) (genesis-function-lap-code fn)
                     (genesis-function-assembled-code object) (genesis-function-assembled-code fn)
                     (genesis-function-constants object) (genesis-function-constants fn))))))
  object)

(defbuiltin #:assemble-lap (code &optional name)
  (multiple-value-bind (mc constants)
      (assemble-lap code name)
    (make-genesis-function :lap-code code
			   :assembled-code (strip-array-header mc)
			   :constants (strip-array-header constants))))

(defun generate-dump-layout (undefined-function-thunk &optional extra-static-objects)
  "Scan the entire Genesis environment, creating a final memory layout for a dump image."
  (let ((static-objects (make-hash-table))
	(static-offset 0)
	(dynamic-objects (make-hash-table))
	(dynamic-offset 0)
	(compiled-functions (make-hash-table))
	(visited-objects (make-hash-table))
        ;; Scan from NIL.
	(roots (append extra-static-objects (list undefined-function-thunk nil)))
	(*lap-symbols* '()))
    (labels ((add-static-object (x)
               (unless (gethash x static-objects)
                 (setf (gethash x static-objects) static-offset)
                 ;; Two word heap header.
                 (incf static-offset (+ (object-size x) 2))
                 (when (oddp static-offset)
                   (incf static-offset))))
	     (add-object (x)
	       ;; Ignore objects that already have addresses
	       (cond ((or (gethash x static-objects)
			  (gethash x dynamic-objects)))
		     ;; All functions go in the static region.
		     ((or (genesis-function-p x)
			  (genesis-closure-p x))
		      (add-static-object x))
		     ;; NIL and T are done above.
		     ((or (eql x nil)
			  (eql x (genesis-intern "T"))))
		     ;; Characters, fixnums and single-floats don't go anywhere.
		     ((or (characterp x)
			  (typep x '(signed-byte 61))
			  (typep x 'single-float)))
		     (t (setf (gethash x dynamic-objects) dynamic-offset)
			(incf dynamic-offset (object-size x))
			(when (oddp dynamic-offset)
			  (incf dynamic-offset)))))
	     (visit (object)
	       (when (functionp object)
		 (let ((fn (gethash object compiled-functions)))
		   (unless fn
		     (let ((info (gethash object *function-info*)))
		       (unless info
			 (format t "No source info available for ~S.~%" object)
			 (return-from visit))
		       (cond ((functionp (first info))
			      ;; A closure.
			      (setf fn (make-genesis-closure :function (first info)
							     :environment (second info))
				    (gethash object compiled-functions) fn))
			     (t ;; A regular function.
			      (when (null (third info))
				  (setf (cddr info) (list (make-genesis-function :source (first info)
                                                                                 :source-environment (second info)))))
			      (setf fn (third info)
				    (gethash object compiled-functions) fn)))))
		   (setf object fn)))
	       (unless (gethash object visited-objects)
		 (setf (gethash object visited-objects) t)
		 ;; Functions must be compiled first.
		 (when (and (genesis-function-p object)
			    (genesis-function-source object)
			    (null (genesis-function-lap-code object)))
		     (let ((*print-level* 3)
			   (*print-length* 4))
		       (format t "Compiling function ~S~%" (genesis-function-source object)))
		     (compile-genesis-function object))
		 (map-slots #'visit object))))
      ;; Give addresses to NIL, T, and the undefined function thunk. This is required for LAP.
      (add-static-object 'nil)
      (add-static-object (genesis-intern "T"))
      (add-static-object undefined-function-thunk)
      (push (cons 'nil (logior (+ (* (+ (gethash 'nil static-objects) 2) 8) *static-area-base*) 2))
            *lap-symbols*)
      (push (cons (genesis-intern "T") (logior (+ (* (+ (gethash (genesis-intern "T") static-objects) 2) 8) *static-area-base*) 2))
            *lap-symbols*)
      (push (cons (genesis-intern "UNDEFINED-FUNCTION") (logior (+ (* (+ (gethash undefined-function-thunk static-objects) 2) 8) *static-area-base*)
                                                                (symbol-value (genesis-intern "+TAG-FUNCTION+"))))
            *lap-symbols*)
      ;; Visit all visible objects, including extra objects.
      (dolist (r roots)
	(visit r))
      ;; Add the extra static objects to the static area.
      ;; Functions will be compiled by this point and their sizes known.
      (dolist (obj extra-static-objects)
	(add-static-object obj))
      ;; Add all objects to the memory layout.
      (alexandria:maphash-keys #'add-object visited-objects)
      (format t "~S static objects, ~S words.~%"
	      (hash-table-count static-objects)
	      static-offset)
      (format t "~S dynamic objects, ~S words.~%"
	      (hash-table-count dynamic-objects)
	      dynamic-offset)
      ;; Log counts & sizes of each type.
      (let ((types (make-hash-table :test 'equal)))
	(alexandria:maphash-keys
	 (lambda (k)
	   (multiple-value-bind (type size)
	       (etypecase k
		 ((or base-char character)
		  (values 'character 0))
		 ((signed-byte 61)
		  (values 'fixnum 0))
		 (integer
		  (values 'bignum (object-size k)))
		 (single-float
		  (values 'single-float 0))
		 (symbol
		  (values 'symbol 6))
		 (genesis-struct
		  (values 'structure (1+ (length (genesis-struct-slots k)))))
		 (cons
		  (values 'cons 2))
		 (genesis-std-instance
		  (values 'std-instance 2))
		 (array-header
		  (values 'array-header 4))
		 ((vector base-char)
		  (values 'simple-base-string (1+ (ceiling (length k) 8))))
		 ((vector character)
		  (values 'simple-string (1+ (ceiling (* (length k) 4) 8))))
		 ((vector (unsigned-byte 8))
		  (values '(simple-array (unsigned-byte 8) (*)) (1+ (ceiling (length k) 8))))
		 ((vector (unsigned-byte 16))
		  (values '(simple-array (unsigned-byte 16) (*)) (1+ (ceiling (* (length k) 2) 8))))
		 ((vector (unsigned-byte 32))
		  (values '(simple-array (unsigned-byte 32) (*)) (1+ (ceiling (* (length k) 4) 8))))
		 ((vector (unsigned-byte 64))
		  (values '(simple-array (unsigned-byte 64) (*)) (1+ (length k))))
		 ((vector t)
		  (values 'simple-vector (1+ (length k))))
		 (genesis-closure
		  (values 'closure 6))
		 (genesis-function
                  (values 'function
                          (+ (ceiling (+ (length (genesis-function-assembled-code k)) 12) 8)
                             (length (genesis-function-constants k)))))
                 (genesis-stack-group
                  (values 'stack-group 512)))
	     (let ((x (alexandria:ensure-gethash type types (list 0 0))))
	       (incf (first x))
	       (incf (second x) size))))
	 visited-objects)
	(maphash (lambda (k v) (format t "~D ~A object~P.~%" v k v))
		 types))
      (values static-objects static-offset
	      dynamic-objects dynamic-offset
	      compiled-functions))))

;;; Generate the layout of the dump in physical memory.
;;; Also generate the initial page tables.
(defun generate-physical-dump-layout (static-size dynamic-size)
  (let* ((load-offset #x200000) ; Physical base address.
	 (static-base load-offset)
         (static-area-full-size (* 32 1024 1024))
         (static-area-size (* (ceiling (* static-size 8) #x200000) #x200000))
	 (dynamic-base (+ static-base static-area-size))
         (dynamic-area-full-size (* 32 1024 1024))
         (dynamic-area-size (* (ceiling (* dynamic-size 8) #x200000) #x200000))
	 (support-base (+ dynamic-base dynamic-area-size))
	 (end-address support-base)
         (zero-fill-start)
         (zero-fill-end)
         (oldspace-pml2-entry)
         (newspace-pml2-entry))
    (assert (<= static-area-size static-area-full-size))
    (assert (<= dynamic-area-size dynamic-area-full-size))
    ;; This is just to stop things getting silly.
    (when (>= end-address (* 2 1024 1024 1024))
      (error "End address too large, giving up."))
    ;; Build page tables.
    ;; Everything is aligned to 2MB boundaries so that 2MB pages can be used.
    (let* ((page-tables (make-array (list (+ 1 2 8) 512)
				    :element-type '(unsigned-byte 32)
				    :initial-element 0))
	   ;; One PML4
	   (pml4 end-address)
	   ;; A PML3 to cover the normal region (0-512GB).
	   (pml3-normal (incf end-address #x1000))
	   ;; A PML3 to cover the linear region (512GB-1024GB).
	   (pml3-linear (incf end-address #x1000))
	   ;; Four PML2s to cover the first 4GB of the normal region.
	   ;; See virtual memory map above.
	   (pml2-normal (incf end-address #x1000))
	   ;; Four PML2s to map the first 4GB of the linear region.
	   (pml2-linear (incf end-address (* 4 #x1000))))
      (incf end-address (* 4 #x1000))
      ;; Ugh.
      (setf oldspace-pml2-entry (+ pml3-normal 24))
      (setf newspace-pml2-entry (+ pml3-normal 16))
      ;; Align to 2MB.
      (setf zero-fill-start (* (ceiling end-address #x200000) #x200000)
            zero-fill-end zero-fill-start)
      ;; Create the PML4.
      (setf (aref page-tables 0 0) (logior pml3-normal
					   +page-table-present+
					   +page-table-writable+)
	    (aref page-tables 0 1) (logior pml3-linear
					   +page-table-present+
					   +page-table-writable+))
      ;; Create the normal PML3.
      ;; Fourth entry is initially marked not-present.
      (dotimes (i 4)
	(setf (aref page-tables 1 i) (logior (+ pml2-normal (* i #x1000))
					     (if (= i 3) 0 +page-table-present+)
					     +page-table-writable+)))
      (flet ((map-large-page (virt &optional phys)
	       (multiple-value-bind (dir x)
		   (truncate virt (* 1024 1024 1024))
                 (unless phys
                   (setf phys zero-fill-end)
                   (incf zero-fill-end #x200000))
		 (let ((ofs (truncate x #x200000)))
		   (setf (aref page-tables (+ 3 dir) ofs) (logior phys
								  +page-table-present+
								  +page-table-writable+
								  +page-table-large+))))))
	;; Map static space.
	(dotimes (i (/ static-area-size #x200000))
	  (map-large-page (+ *static-area-base* (* i #x200000))
			  (+ static-base (* i #x200000))))
        (dotimes (i (/ (- static-area-full-size static-area-size) #x200000))
	  (map-large-page (+ *static-area-base* static-area-size (* i #x200000))))
	;; Map dynamic space A.
	(dotimes (i (/ dynamic-area-size #x200000))
	  (map-large-page (+ *dynamic-area-base* (* i #x200000))
			  (+ dynamic-base (* i #x200000))))
        (dotimes (i (/ (- dynamic-area-full-size dynamic-area-size) #x200000))
	  (map-large-page (+ *dynamic-area-base* dynamic-area-size (* i #x200000))))
        ;; Map dynamic space B.
        (dotimes (i (/ dynamic-area-full-size #x200000))
          (map-large-page (+ *dynamic-area-base* (* i #x200000) (/ *dynamic-area-size* 2)))))
      ;; Create the linear map PML3 and PML2s.
      (dotimes (i 4)
	(setf (aref page-tables 2 i) (logior (+ pml2-linear (* i #x1000))
					     +page-table-present+
					     +page-table-writable+))
	(dotimes (j 512)
	  (setf (aref page-tables (+ 7 i) j) (logior (+ (* i 1024 1024 1024) (* j #x200000))
						     +page-table-present+
						     +page-table-writable+
						     +page-table-large+))))
      (format t "Static-base: ~X  Dynamic-base: ~X  Support-base: ~X  End: ~X (~X)~%"
	      static-base dynamic-base support-base end-address zero-fill-end)
      (values load-offset static-base dynamic-base support-base end-address zero-fill-end
	      pml4 page-tables oldspace-pml2-entry newspace-pml2-entry))))

(defun make-setup-function (gdt idt initial-page-table entry-function initial-stack-group)
  (multiple-value-bind (mc constants)
      (sys.lap-x86:assemble
	  `((sys.lap-x86:!code32)
	    ;; Horrible hack: Use the middle of the initial-page-table as a temporary stack.
	    (sys.lap-x86:mov32 :esp ,(+ initial-page-table 512))
	    ;; Compute the start of the function.
	    (sys.lap-x86:call get-eip)
	    get-eip
	    (sys.lap-x86:pop :esi)
	    ;; Set ECX to the start of the function.
	    (sys.lap-x86:sub32 :esi get-eip)
	    ;; Switch to the less-temporary temporary stack and clear whatever was just trashed.
	    (sys.lap-x86:push 0)
	    (sys.lap-x86:lea32 :esp (:esi initial-stack))
	    ;; Patch the GDTR and IDTR registers.
	    (sys.lap-x86:mov32 :ecx (:esi (:constant-address ,gdt)))
	    (sys.lap-x86:add32 :ecx 1)
	    (sys.lap-x86:mov32 (:esi (+ gdtr 2)) :ecx)
	    (sys.lap-x86:mov32 :ecx (:esi (:constant-address ,idt)))
	    (sys.lap-x86:add32 :ecx 1)
	    (sys.lap-x86:mov32 (:esi (+ idtr 2)) :ecx)
	    ;; Enable long mode.
	    (sys.lap-x86:movcr :eax :cr4)
	    (sys.lap-x86:or32 :eax #x000000A0)
	    (sys.lap-x86:movcr :cr4 :eax)
	    (sys.lap-x86:mov32 :eax ,initial-page-table)
	    (sys.lap-x86:movcr :cr3 :eax)
	    (sys.lap-x86:mov32 :ecx #xC0000080)
	    (sys.lap-x86:rdmsr)
	    (sys.lap-x86:or32 :eax #x00000100)
	    (sys.lap-x86:wrmsr)
	    (sys.lap-x86:movcr :eax :cr0)
	    (sys.lap-x86:or32 :eax #x80000000)
	    (sys.lap-x86:movcr :cr0 :eax)
	    (sys.lap-x86:lgdt (:esi gdtr))
	    (sys.lap-x86:lidt (:esi idtr))
	    ;; There was a far jump here, but that's hard to make position-independent.
	    (sys.lap-x86:push #x0008)
	    (sys.lap-x86:lea32 :eax (:esi long64))
	    (sys.lap-x86:push :eax)
	    (sys.lap-x86:retf)
	    (sys.lap-x86:!code64)
	    long64
	    (sys.lap-x86:xor32 :eax :eax)
	    (sys.lap-x86:movseg :ds :eax)
	    (sys.lap-x86:movseg :es :eax)
	    (sys.lap-x86:movseg :fs :eax)
	    (sys.lap-x86:movseg :gs :eax)
	    (sys.lap-x86:movseg :ss :eax)
            ;; Preset the initial stack group.
            (sys.lap-x86:mov64 :r8 (:constant ,initial-stack-group))
            (sys.lap-x86:mov64 :csp (:r8 ,(- (* 5 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))
            (sys.lap-x86:add64 :csp (:r8 ,(- (* 6 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))
            (sys.lap-x86:mov64 :lsp (:r8 ,(- (* 7 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))
            (sys.lap-x86:add64 :lsp (:r8 ,(- (* 8 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))
            ;; Clear binding stack.
            (sys.lap-x86:mov64 :rdi (:r8 ,(- (* 9 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))
            (sys.lap-x86:mov64 :rcx (:r8 ,(- (* 10 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))
            (sys.lap-x86:sar64 :rcx 3)
            (sys.lap-x86:xor32 :eax :eax)
            (sys.lap-x86:rep)
            (sys.lap-x86:stos64)
            ;; Set the binding stack pointer.
            (sys.lap-x86:mov64 (:r8 ,(- (* 1 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))) :rdi)
            ;; Clear TLS binding slots.
            (sys.lap-x86:lea64 :rdi (:r8 ,(- (* 12 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))))
            (sys.lap-x86:mov64 :rax -2)
            (sys.lap-x86:mov32 :ecx 500)
            (sys.lap-x86:rep)
            (sys.lap-x86:stos64)
            ;; Mark the SG as running/unsafe.
            (sys.lap-x86:mov64 (:r8 ,(- (* 2 8) (symbol-value (genesis-intern "+TAG-ARRAY-LIKE+")))) 0)
            ;; Initialize GS.
            (sys.lap-x86:mov64 :rax :r8)
            (sys.lap-x86:mov64 :rdx :r8)
            (sys.lap-x86:sar64 :rdx 32)
            (sys.lap-x86:mov64 :rcx #xC0000101)
            (sys.lap-x86:wrmsr)
	    ;; Clear frame pointers.
	    (sys.lap-x86:mov64 :cfp 0)
	    (sys.lap-x86:mov64 :lfp 0)
	    ;; Clear data registers.
	    (sys.lap-x86:xor32 :r8d :r8d)
	    (sys.lap-x86:xor32 :r9d :r9d)
	    (sys.lap-x86:xor32 :r10d :r10d)
	    (sys.lap-x86:xor32 :r11d :r11d)
	    (sys.lap-x86:xor32 :r12d :r12d)
	    (sys.lap-x86:xor32 :ebx :ebx)
	    ;; Prepare for call.
	    (sys.lap-x86:mov64 :r13 (:constant ,entry-function))
	    (sys.lap-x86:xor32 :ecx :ecx)
	    ;; Call the entry function.
	    (sys.lap-x86:call (:symbol-value :r13))
	    ;; Crash if it returns.
	    here
	    (sys.lap-x86:ud2)
	    (sys.lap-x86:jmp here)
	    #+nil(:align 4) ; TODO!! ######
	    ;; 8 word stack for startup.
	    (:d64/le 0 0 0 0 0 0 0 0)
	    initial-stack
	    gdtr
	    (:d16/le ,(1- (* (length gdt) 8)))
	    (:d32/le 0)
	    idtr
	    (:d16/le ,(1- (* (length idt) 8)))
	    (:d32/le 0))
	:base-address 12)
    (make-genesis-function :source nil
			   :lap-code nil
			   :assembled-code mc
			   :constants constants)))

(defun make-undefined-function-thunk ()
  (multiple-value-bind (mc constants)
      (sys.lap-x86:assemble
	  `((sys.lap-x86:mov64 :r8 :r13)
	    (sys.lap-x86:mov32 :ecx ,(* 1 8))
	    (sys.lap-x86:mov64 :r13 (:constant ,(genesis-eval (list (genesis-intern "INTERN")
                                                                    "RAISE-UNDEFINED-FUNCTION"
                                                                    "SYSTEM.INTERNALS"))))
	    (sys.lap-x86:jmp (:symbol-function :r13)))
	:base-address 12)
    (make-genesis-function :source nil
			   :lap-code nil
			   :assembled-code mc
			   :constants constants)))

(defgeneric dump-object (object value-table image offset))

(defun value-of (object value-table)
  (typecase object
    ((signed-byte 61) (ldb (byte 64 0) (ash object 3)))
    (character (logior (ash (char-int object) 4) (symbol-value (genesis-intern "+TAG-CHARACTER+"))))
    (t (or (gethash object value-table)
	   (error "Unknown value ~S." object)))))

(defun compute-symbol-flags (symbol)
  (logior (ecase (get symbol :genesis-symbol-mode)
            ((nil) #b00)
            ((:special) #b01)
            ((:constant) #b10)
            ((:symbol-macro) #b11))
          ;; TLS offset. Zero means no allocated slot.
          0))

(defmethod dump-object ((object symbol) value-table image offset)
  ;; +0 Name.
  (setf (nibbles:ub64ref/le image (+ offset 0)) (value-of (crunched-symbol-name object) value-table))
  ;; +8 Package.
  (setf (nibbles:ub64ref/le image (+ offset 8)) (value-of (genesis-symbol-package object) value-table))
  ;; +16 Value.
  (setf (nibbles:ub64ref/le image (+ offset 16)) (let ((x (assoc object *symbol-preloads*)))
                                                   (cond (x (value-of (cdr x) value-table))
                                                         ((boundp object)
                                                          (value-of (symbol-value object) value-table))
                                                         (t (symbol-value (genesis-intern "+TAG-UNBOUND-VALUE+"))))))
  ;; +24 Function.
  ;; Some functions may not be dumpable. They must be replaced with the
  ;; undefined function value.
  (setf (nibbles:ub64ref/le image (+ offset 24)) (let ((x (assoc object *function-preloads*)))
                                                   (cond (x (gethash (cdr x) value-table))
                                                         ((and (fboundp object)
                                                               (gethash (symbol-function object) value-table))
                                                          (gethash (symbol-function object) value-table))
                                                         (t (gethash :undefined-function value-table)))))
  ;; +32 Plist.
  (setf (nibbles:ub64ref/le image (+ offset 32)) (value-of (genesis-symbol-plist object) value-table))
  ;; +40 Flags & stuff
  (setf (nibbles:ub64ref/le image (+ offset 40)) (value-of (compute-symbol-flags object) value-table)))

(defmethod dump-object ((object cons) value-table image offset)
  ;; +0 CAR.
  (setf (nibbles:ub64ref/le image (+ offset 0)) (value-of (car object) value-table))
  ;; +8 CDR.
  (setf (nibbles:ub64ref/le image (+ offset 8)) (value-of (cdr object) value-table)))

(defmethod dump-object ((object genesis-std-instance) value-table image offset)
  ;; +0 Class.
  (setf (nibbles:ub64ref/le image (+ offset 0)) (value-of (genesis-std-instance-class object) value-table))
  ;; +8 Slots.
  (setf (nibbles:ub64ref/le image (+ offset 8)) (value-of (genesis-std-instance-slots object) value-table)))

(defmethod dump-object ((object array-header) value-table image offset)
  ;; +0 Dimensions.
  (setf (nibbles:ub64ref/le image (+ offset 0)) (value-of (array-header-dimensions object) value-table))
  ;; +8 Fill-pointer.
  (setf (nibbles:ub64ref/le image (+ offset 8)) (value-of (array-header-fill-pointer object) value-table))
  ;; +16 Info.
  (setf (nibbles:ub64ref/le image (+ offset 16)) (value-of (array-header-info object) value-table))
  ;; +24 Storage.
  (setf (nibbles:ub64ref/le image (+ offset 24)) (value-of (array-header-storage object) value-table)))

(defun make-sa-header-word (length tag)
  (when (stringp tag)
    (setf tag (symbol-value (genesis-intern tag))))
  (logior (ash length 8) (ash tag 1)))

(defmethod dump-object ((object vector) value-table image offset)
  (let ((type (array-element-type object)))
    (cond ((eql type 't)
	   ;; +0 Header word.
	   (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length object) "+ARRAY-TYPE-T+"))
	   (dotimes (i (length object))
	     (setf (nibbles:ub64ref/le image (+ offset 8 (* i 8))) (value-of (aref object i) value-table))))
	  ((eql type 'base-char)
	   ;; +0 Header word.
	   (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length object) "+ARRAY-TYPE-BASE-CHAR+"))
	   (dotimes (i (length object))
	     (setf (aref image (+ offset 8 i)) (char-int (char object i)))))
	  ((eql type 'character)
	   ;; +0 Header word.
	   (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length object) "+ARRAY-TYPE-CHARACTER+"))
	   (dotimes (i (length object))
	     (setf (nibbles:ub32ref/le image (+ offset 8 (* i 4))) (char-int (char object i)))))
	  ((and (subtypep type '(unsigned-byte 8)) (subtypep '(unsigned-byte 8) type))
	   ;; +0 Header word.
	   (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length object) "+ARRAY-TYPE-UNSIGNED-BYTE-8+"))
	   (dotimes (i (length object))
	     (setf (aref image (+ offset 8 i)) (aref object i))))
	  ((and (subtypep type '(unsigned-byte 16)) (subtypep '(unsigned-byte 16) type))
	   ;; +0 Header word.
	   (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length object) "+ARRAY-TYPE-UNSIGNED-BYTE-16+"))
	   (dotimes (i (length object))
	     (setf (nibbles:ub16ref/le image (+ offset 8 (* i 2))) (aref object i))))
	  ((and (subtypep type '(unsigned-byte 32)) (subtypep '(unsigned-byte 32) type))
	   ;; +0 Header word.
	   (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length object) "+ARRAY-TYPE-UNSIGNED-BYTE-32+"))
	   (dotimes (i (length object))
	     (setf (nibbles:ub32ref/le image (+ offset 8 (* i 4))) (aref object i))))
	  ((and (subtypep type '(unsigned-byte 64)) (subtypep '(unsigned-byte 64) type))
	   ;; +0 Header word.
	   (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length object) "+ARRAY-TYPE-UNSIGNED-BYTE-64+"))
	   (dotimes (i (length object))
	     (setf (nibbles:ub64ref/le image (+ offset 8 (* i 8))) (aref object i))))
	  (t (error "Invalid array type. ~S ~S." type object)))))

(defmethod dump-object ((object genesis-struct) value-table image offset)
  ;; +0 Header word.
  (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (length (genesis-struct-slots object)) "+ARRAY-TYPE-STRUCT+"))
  ;; Slots.
  (dotimes (i (length (genesis-struct-slots object)))
    (setf (nibbles:ub64ref/le image (+ offset 8 (* i 8))) (value-of (aref (genesis-struct-slots object) i) value-table)))
  ;; Must set the REHASH-REQUIRED flag on hash-tables. (6th slot)
  (when (genesis-eval (list (genesis-intern "HASH-TABLE-P") object))
    (setf (nibbles:ub64ref/le image (+ offset 8 (* 6 8))) (value-of (genesis-intern "T") value-table))))

(defmethod dump-object ((object integer) value-table image offset)
  ;; +0 Header word.
  (setf (nibbles:ub64ref/le image offset) (make-sa-header-word (ceiling (1+ (integer-length object)) 64)
                                                               "+ARRAY-TYPE-BIGNUM+"))
  (dotimes (i (ceiling (1+ (integer-length object)) 64))
    (setf (nibbles:ub64ref/le image (+ offset 8 (* i 8)))
          (ldb (byte 64 (* i 64)) object))))

(defmethod dump-object ((object genesis-function) value-table image offset)
  (when (genesis-function-assembled-code object)
    (let* ((mc (genesis-function-assembled-code object))
	   (constants (genesis-function-constants object)))
      ;; +0 Function tag. (TODO: closures, generic functions, etc)
      (setf (aref image (+ offset 0)) (symbol-value (genesis-intern "+FUNCTION-TYPE-FUNCTION+")))
      ;; +1 Flags.
      (setf (aref image (+ offset 1)) 0)
      ;; +2 Size of the machine-code section & header word.
      (setf (nibbles:ub16ref/le image (+ offset 2)) (ceiling (+ (length mc) 12) 16))
      ;; +4 Constant pool size.
      (setf (nibbles:ub16ref/le image (+ offset 4)) (length constants))
      ;; +6 Number of slots. (TODO)
      (setf (nibbles:ub16ref/le image (+ offset 6)) 0)
      ;; +12 The code.
      (dotimes (i (length (genesis-function-assembled-code object)))
	(setf (aref image (+ offset 12 i)) (aref mc i)))
      ;; Constant pool (aligned).
      (dotimes (i (length (genesis-function-constants object)))
	(setf (nibbles:ub64ref/le image (+ offset (* (ceiling (+ (length mc) 12) 16) 16) (* i 8)))
	      (value-of (aref constants i) value-table))))))

(defmethod dump-object ((object genesis-closure) value-table image offset)
  ;; +0 Function tag.
  (setf (aref image (+ offset 0)) (symbol-value (genesis-intern "+FUNCTION-TYPE-CLOSURE+")))
  ;; +1 Flags.
  (setf (aref image (+ offset 1)) 0)
  ;; +2 Size of the machine-code section & header word.
  (setf (nibbles:ub16ref/le image (+ offset 2)) 2)
  ;; +4 Constant pool size.
  (setf (nibbles:ub16ref/le image (+ offset 4)) 2)
  ;; +6 Number of slots.
  (setf (nibbles:ub16ref/le image (+ offset 6)) 0)
  ;; +12 The code.
  (setf (aref image (+ offset 12)) #x48 ;; mov64 :rbx (:rip 21)/pool[1]
	(aref image (+ offset 13)) #x8B
	(aref image (+ offset 14)) #x1D
	(aref image (+ offset 15)) #x15
	(aref image (+ offset 16)) #x00
	(aref image (+ offset 17)) #x00
	(aref image (+ offset 18)) #x00
	(aref image (+ offset 19)) #xFF ;; jmp (:rip 7)/pool[0]
	(aref image (+ offset 20)) #x25
	(aref image (+ offset 21)) #x07
	(aref image (+ offset 22)) #x00
	(aref image (+ offset 23)) #x00
	(aref image (+ offset 24)) #x00)
  ;; +32 Constant pool.
  (setf (nibbles:ub64ref/le image (+ offset 32)) (value-of (genesis-closure-function object) value-table)
	(nibbles:ub64ref/le image (+ offset 40)) (value-of (genesis-closure-environment object) value-table)))

(defmethod dump-object ((object genesis-stack-group) value-table image offset)
  ;; +0 Array tag.
  (setf (nibbles:ub64ref/le image (+ offset 0)) (make-sa-header-word 511 "+ARRAY-TYPE-STACK-GROUP+"))
  ;; +8 Binding stack pointer.
  (setf (nibbles:ub64ref/le image (+ offset 8)) (* (+ (genesis-stack-group-binding-stack-base object)
                                                      (genesis-stack-group-binding-stack-size object))
                                                   8))
  ;; +16 State word. Unsafe, active.
  (setf (nibbles:ub64ref/le image (+ offset 16)) 0)
  ;; +24 Saved control stack pointer.
  (setf (nibbles:ub64ref/le image (+ offset 24)) (* (+ (genesis-stack-group-control-stack-base object)
                                                       (genesis-stack-group-control-stack-size object)) 8))
  ;; +32 Name.
  (setf (nibbles:ub64ref/le image (+ offset 32)) (value-of (genesis-stack-group-name object) value-table))
  ;; +40 Control stack base.
  (setf (nibbles:ub64ref/le image (+ offset 40)) (value-of (genesis-stack-group-control-stack-base object) value-table))
  ;; +48 Control stack size.
  (setf (nibbles:ub64ref/le image (+ offset 48)) (value-of (genesis-stack-group-control-stack-size object) value-table))
  ;; +56 Data stack base.
  (setf (nibbles:ub64ref/le image (+ offset 56)) (value-of (genesis-stack-group-data-stack-base object) value-table))
  ;; +64 Data stack size.
  (setf (nibbles:ub64ref/le image (+ offset 64)) (value-of (genesis-stack-group-data-stack-size object) value-table))
  ;; +72 Binding stack base.
  (setf (nibbles:ub64ref/le image (+ offset 72)) (value-of (genesis-stack-group-binding-stack-base object) value-table))
  ;; +80 Binding stack size.
  (setf (nibbles:ub64ref/le image (+ offset 80)) (value-of (genesis-stack-group-binding-stack-size object) value-table))
  ;; +88 Resumer.
  (setf (nibbles:ub64ref/le image (+ offset 88)) (value-of nil value-table))
  ;; +96 Start of TLS slots.
  (dotimes (i (- 512 12))
    (setf (nibbles:ub64ref/le image (+ offset 96 (* i 8))) #xFFFFFFFFFFFFFFFE)))

(defun genesis-eval-string (string)
  (with-input-from-string (stream string)
    (genesis-eval (genesis-eval (list (genesis-intern "READ") stream)))))

(defparameter *builtin-suppression-mode* :default)

(defun fastload-form (form)
  (cond ((and (listp form)
              (= (list-length form) 4)
              (eql (first form) (genesis-intern "FUNCALL"))
              (listp (second form))
              (= (list-length (second form)) 2)
              (eql (first (second form)) (genesis-intern "FUNCTION"))
              (listp (second (second form)))
              (= (list-length (second (second form))) 2)
              (eql (first (second (second form))) (genesis-intern "SETF"))
              (eql (second (second (second form))) (genesis-intern "FDEFINITION"))
              (listp (third form))
              (= (list-length (third form)) 2)
              (eql (first (third form)) (genesis-intern "FUNCTION"))
              (listp (second (third form)))
              (eql (first (second (third form))) (genesis-intern "LAMBDA"))
              (listp (third form))
              (= (list-length (fourth form)) 2)
              (eql (first (fourth form)) (genesis-intern "QUOTE")))
         ;; FORM looks like (FUNCALL #'(SETF FDEFINITION) #'(LAMBDA ...) 'name)
         ;; Check if there's an existing function or an existing preload.
         (let ((name (resolve-function-name (second (fourth form)))))
           (when (and (not (assoc name *function-preloads*))
                      (not (and (fboundp name)
                                (gethash (symbol-function name) *function-info*))))
             ;; FIXME: macrolet and symbol-macrolet must be tracked.
             (push (cons name (make-genesis-function :source (second (third form))
                                                     :source-environment nil
                                                     :suppress-builtins *builtin-suppression-mode*))
                   *function-preloads*)
             t)))
        ((and (listp form)
              (>= (list-length form) 3)
              (eql (first form) (genesis-intern "DEFINE-LAP-FUNCTION"))
              (listp (third form))
              (= (list-length (third form)) 0))
         ;; FORM looks like (DEFINE-LAP-FUNCTION name (options) code...)
         (let ((name (resolve-function-name (second form))))
           (when (or (assoc name *function-preloads*)
                     (and (fboundp name)
                          (gethash (symbol-function name) *function-info*)))
             (error "TODO: define-lap-function ~S conflicts with existing function." (second form)))
           (push (cons name (make-genesis-function :source (list* (genesis-intern "LAMBDA-LAP")
                                                                  (list (second form))
                                                                  (cdddr form))
                                                   :source-environment nil
                                                   :lap-function t))
                 *function-preloads*)
           t))))

(defun make-toplevel-function (&rest files)
  (let ((toplevel-forms '()))
    (flet ((frob (form)
             (genesis-eval (list (genesis-intern "HANDLE-TOP-LEVEL-FORM")
                                 (list (genesis-intern "QUOTE") form)
                                 (lambda (form env)
                                   (declare (ignore env))
                                   (format t "; Load ~S~%" form)
                                   (or (fastload-form form)
                                       (push form toplevel-forms)))
                                 (lambda (form env)
                                   (when env
                                     (error "TODO: Eval in env."))
                                   (format t "; Eval ~S~%" form)
                                   (genesis-eval form))))))
      ;; Built-ins must not be suppressed when compiling their wrapper functions.
      (let ((*builtin-suppression-mode* nil))
        (mapc (lambda (x)
                (frob (list (genesis-intern "FUNCALL")
                            (list (genesis-intern "FUNCTION") (list (genesis-intern "SETF") (genesis-intern "FDEFINITION")))
                            (list (genesis-intern "FUNCTION") (second x))
                            (list (genesis-intern "QUOTE") (first x)))))
              (genesis-eval (list (genesis-eval (list (genesis-intern "INTERN") "GENERATE-BUILTIN-FUNCTIONS" "SYS.C"))))))
      (dolist (file files)
        (with-open-file (s file)
          (progv (list (genesis-intern "*PACKAGE*")) (list (genesis-eval-string "(find-package '#:cl-user)"))
            (do* ((form (genesis-eval (list (genesis-intern "READ") s nil (list (genesis-intern "QUOTE") s)))
                        (genesis-eval (list (genesis-intern "READ") s nil (list (genesis-intern "QUOTE") s)))))
                 ((eql form s))
              (frob form))))))
    (format t "Toplevel:~%~{~S~%~}" (reverse toplevel-forms))
    (make-genesis-function :source (list (genesis-intern "LAMBDA") '()
                                         (list (genesis-intern "DECLARE")
                                               (list (genesis-intern "LAMBDA-NAME")
                                                     (genesis-intern "TOPLEVEL")))
                                         (cons (genesis-intern "PROGN")
                                               (nreverse toplevel-forms)))
			   :source-environment nil)))

;;; Build a (u-b 8) array holding a bootable image.
(defun generate-dump ()
  (let* ((multiboot-header (make-array 8 :element-type '(unsigned-byte 32)))
	 (gdt (make-array 256 :element-type '(unsigned-byte 64)))
	 (idt (make-array (* 256 2) :element-type '(unsigned-byte 64)))
         (*function-preloads* '())
         (*symbol-preloads* '())
	 (entry-function (make-toplevel-function "../runtime-support.lisp" "../gc.lisp"
                                                 "../runtime-array.lisp" "../runtime-numbers.lisp"
                                                 "../character.lisp" "../printer.lisp" "../debug.lisp"
                                                 "../type.lisp" "../eval.lisp" "../cold-stream.lisp"
                                                 "../stream.lisp" "../format.lisp" "../stack-group.lisp"
                                                 "../interrupt-compiler.lisp" "../keyboard.lisp"
                                                 "../pci.lisp" "../framebuffer.lisp" "../bochs-vbe.lisp"
                                                 "../test.lisp"))
         (initial-stack-group (make-genesis-stack-group :name "Initial stack group"))
	 ;; FIXME: Unhardcode this, the physical address of the PML4.
	 (setup-code (make-setup-function gdt idt (- #x200000 #x1000) (genesis-intern "*INITIAL-FUNCTION*") initial-stack-group))
	 (undefined-function-thunk (make-undefined-function-thunk))
         (unifont-data (with-open-file (s "../unifontfull-5.1.20080820.hex")
                         (build-unicode:generate-unifont-table s))))
    (push (cons (genesis-intern "*MULTIBOOT-HEADER*") multiboot-header) *symbol-preloads*)
    (push (cons (genesis-intern "*GDT*") gdt) *symbol-preloads*)
    (push (cons (genesis-intern "*IDT*") idt) *symbol-preloads*)
    (push (cons (genesis-intern "*SETUP-CODE*") setup-code) *symbol-preloads*)
    (push (cons (genesis-intern "*UNDEFINED-FUNCTION-THUNK*") undefined-function-thunk) *symbol-preloads*)
    (push (cons (genesis-intern "*UNIFONT-BMP*") unifont-data) *symbol-preloads*)
    (push (cons (genesis-intern "*INITIAL-FUNCTION*") entry-function) *symbol-preloads*)
    (multiple-value-bind (unicode-info name-store encoding-table name-trie)
        (build-unicode:generate-unicode-data-tables (build-unicode:read-unicode-data "../UnicodeData.txt"))
      (push (cons (genesis-intern "*UNICODE-INFO*") unicode-info) *symbol-preloads*)
      (push (cons (genesis-intern "*UNICODE-NAME-STORE*") name-store) *symbol-preloads*)
      (push (cons (genesis-intern "*UNICODE-ENCODING-TABLE*") encoding-table) *symbol-preloads*)
      (push (cons (genesis-intern "*UNICODE-NAME-TRIE*") name-trie) *symbol-preloads*))
    (multiple-value-bind (static-objects static-size dynamic-objects dynamic-size function-map)
	(generate-dump-layout undefined-function-thunk (append (list multiboot-header setup-code gdt idt
                                                                     initial-stack-group entry-function)
                                                               ;; GC-related symbols must be stored in static space.
                                                               (mapcar 'genesis-intern
                                                                       '("*OLDSPACE*" "*NEWSPACE*"
                                                                         "*NEWSPACE-OFFSET*" "*SEMISPACE-SIZE*"
                                                                         "*OLDSPACE-PAGING-BITS*"
                                                                         "*NEWSPACE-PAGING-BITS*"
                                                                         "*STATIC-BUMP-POINTER*" "*STATIC-MARK-BIT*"
                                                                         "*VERBOSE-GC*"))
                                                               (mapcar 'cdr *function-preloads*)
                                                               (mapcar 'cdr *symbol-preloads*)))
      (multiple-value-bind (load-base phys-static-base phys-dynamic-base dynamic-end support-end image-end initial-cr3 page-tables
                            oldspace-pml2-entry newspace-pml2-entry)
	  (generate-physical-dump-layout static-size dynamic-size)
	(let* ((control-stack-size (* 4096 8))
               (data-stack-size (* 4096 8))
               (binding-stack-size 1024)
               (bss-size (+ (* control-stack-size 8)
                            (* data-stack-size 8)
                            (* binding-stack-size 8)))
               (image-size (- support-end load-base))
               (image (make-array (+ image-size #x1000) :element-type '(unsigned-byte 8)))
               (object-values (make-hash-table)))
          (format t "Entry function MC size: ~D bytes~%" (length (genesis-function-assembled-code entry-function)))
	  (format t "Image size: ~D kilowords (~D kilobytes)~%" (/ image-size 1024.0 8) (/ image-size 1024.0))
	  ;; Produce a map from objects to their values.
	  (flet ((add-object (obj base-address)
		   (setf (gethash obj object-values) (logior base-address
							     (object-tag obj)))))
	    (maphash (lambda (obj addr)
                       ;; Static objects have a two word header.
                       (add-object obj (+ (* (+ addr 2) 8) *static-area-base*)))
		     static-objects)
	    (maphash (lambda (obj addr)
		       (add-object obj (+ (* addr 8) *dynamic-area-base*)))
		     dynamic-objects)
	    ;; Functions.
	    (maphash (lambda (fn obj)
		       (setf (gethash fn object-values) (gethash obj object-values)))
		     function-map)
	    ;; Special objects.
	    (setf (gethash :undefined-function object-values) (gethash undefined-function-thunk object-values)))
          ;; Additionally, produce a map file for use with bochs.
          (with-open-file (s "../crap.map" :direction :output :if-exists :supersede :if-does-not-exist :create)
            (flet ((frob (object)
                     (when (symbolp object)
                       (let ((fn (or (cdr (assoc object *function-preloads*))
                                     (when (fboundp object)
                                       (symbol-function object)))))
                         (when (and fn (gethash fn object-values))
                           (format s "~8,'0X ~A~%"
                                   (gethash fn object-values)
                                   (symbol-name object)))))))
              (maphash (lambda (obj addr)
                         (declare (ignore addr))
                         (frob obj))
		     static-objects)
              (maphash (lambda (obj addr)
                         (declare (ignore addr))
                         (frob obj))
		     dynamic-objects)))
	  ;; Print out the locations of various objects.
	  (format t "Multiboot header at ~X.~%" (gethash multiboot-header object-values))
	  (format t "GDT at ~X. IDT at ~X.~%" (gethash gdt object-values) (gethash idt object-values))
	  (format t "PML4 at ~X.~%" (- #x200000 #x1000))
	  (format t "Entry point at ~X.~%" (gethash setup-code object-values))
	  (format t "NIL at ~X.~%" (gethash 'nil object-values))
	  (format t "UFT at ~X.~%" (gethash undefined-function-thunk object-values))
          (format t "Initial stack group at ~X.~%" (gethash initial-stack-group object-values))
          (format t "GC pointer at ~X.~%" dynamic-size)
          ;; Set the  GC pointers
          (push (cons (genesis-intern "*OLDSPACE*") (+ *dynamic-area-base* (/ *dynamic-area-size* 2))) ; bytes
                *symbol-preloads*)
          (push (cons (genesis-intern "*NEWSPACE*") *dynamic-area-base*) ; bytes
                *symbol-preloads*)
          (push (cons (genesis-intern "*NEWSPACE-OFFSET*") dynamic-size) ; words
                *symbol-preloads*)
          (push (cons (genesis-intern "*SEMISPACE-SIZE*") (/ (* 32 1024 1024) 8)) ; words
                *symbol-preloads*)
          (push (cons (genesis-intern "*OLDSPACE-PAGING-BITS*") (+ *linear-map* oldspace-pml2-entry))
                *symbol-preloads*)
          (push (cons (genesis-intern "*NEWSPACE-PAGING-BITS*") (+ *linear-map* newspace-pml2-entry))
                *symbol-preloads*)
          (push (cons (genesis-intern "*STATIC-BUMP-POINTER*") (+ *static-area-base* (* static-size 8)))
                *symbol-preloads*)
          (push (cons (genesis-intern "*STATIC-MARK-BIT*") 0)
                *symbol-preloads*)
	  ;; Fill in the multiboot struct.
	  (setf (aref multiboot-header 0) #x1BADB002
		(aref multiboot-header 1) #x00010003
		(aref multiboot-header 2) (ldb (byte 32 0) (- (+ #x1BADB002 #x00010003)))
		;; Strip away the tag bits and advance past the header word.
		(aref multiboot-header 3) (+ (logand (gethash multiboot-header object-values) #xFFFFFFF0) 8)
		(aref multiboot-header 4) (- load-base #x1000)
		(aref multiboot-header 5) support-end
		(aref multiboot-header 6) (+ image-end bss-size)
		(aref multiboot-header 7) (gethash setup-code object-values))
	  ;; And the GDT.
	  (setf (aref gdt 0) 0
		(aref gdt 1) #x00209A0000000000)
	  (let ((*print-base* 16))
	    (format t "Multiboot header: ~S~%" multiboot-header))
          ;; Initialize stack pointers.
          (setf (genesis-stack-group-control-stack-base initial-stack-group) (/ (+ *linear-map* image-end) 8)
                (genesis-stack-group-control-stack-size initial-stack-group) control-stack-size
                (genesis-stack-group-binding-stack-base initial-stack-group) (/ (+ *linear-map* image-end (* control-stack-size 8)) 8)
                (genesis-stack-group-binding-stack-size initial-stack-group) binding-stack-size
                (genesis-stack-group-data-stack-base initial-stack-group) (/ (+ *linear-map* image-end (* control-stack-size 8)
                                                                                (* binding-stack-size 8)) 8)
                (genesis-stack-group-data-stack-size initial-stack-group) data-stack-size)
          (format t "Control stack base: ~X~%" (* (genesis-stack-group-control-stack-base initial-stack-group) 8))
          (format t "Data stack base:    ~X~%" (* (genesis-stack-group-data-stack-base initial-stack-group) 8))
          (format t "Binding stack base: ~X~%" (* (genesis-stack-group-binding-stack-base initial-stack-group) 8))
	  ;; Dump static objects.
	  (maphash (lambda (obj addr)
		     (dump-object obj object-values image (+ 4096 (* (+ addr 2) 8))))
		   static-objects)
	  ;; Dump dynamic objects.
	  (maphash (lambda (obj addr)
		     (dump-object obj object-values image (+ 4096 (- phys-dynamic-base load-base) (* addr 8))))
		   dynamic-objects)
	  ;; Copy the PML4 to the head of the image.
	  (dotimes (i 512)
	    (setf (nibbles:ub64ref/le image (* i 8)) (aref page-tables 0 i)))
	  ;; Copy the pages tables (including a redundant copy of the PML4) to the support area.
	  (dotimes (i (array-total-size page-tables))
	    (setf (nibbles:ub64ref/le image (+ 4096 (- dynamic-end load-base) (* i 8))) (row-major-aref page-tables i)))
	  (with-open-file (s "../crap.image" :direction :output :element-type '(unsigned-byte 8)
			     :if-exists :supersede :if-does-not-exist :create)
	    (write-sequence image s))))))
  t)

(defun flush-compiled-function-cache ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (setf (cddr v) nil))
           *function-info*))
