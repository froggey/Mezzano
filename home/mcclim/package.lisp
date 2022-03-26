;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

(in-package :common-lisp-user)

;;;
;;; CLIM-LISP
;;;

;; Our CLIM-LISP also contains gray streams, as I consider them part
;; of Common Lisp.

;; If you want to patch a CL symbol, you define it in CLIM-LISP-PATCH
;; and export it.

#.(let ((all-ansi-symbols
         '(#:&allow-other-keys #:&aux #:&body #:&environment #:&key #:&optional #:&rest #:&whole #:*
           #:** #:*** #:*break-on-signals* #:*compile-file-pathname* #:*compile-file-truename*
           #:*compile-print* #:*compile-verbose* #:*debug-io* #:*debugger-hook*
           #:*default-pathname-defaults* #:*error-output* #:*features* #:*gensym-counter*
           #:*load-pathname* #:*load-print* #:*load-truename* #:*load-verbose* #:*macroexpand-hook*
           #:*modules* #:*package* #:*print-array* #:*print-base* #:*print-case* #:*print-circle*
           #:*print-escape* #:*print-gensym* #:*print-length* #:*print-level* #:*print-lines*
           #:*print-miser-width* #:*print-pprint-dispatch* #:*print-pretty* #:*print-radix*
           #:*print-readably* #:*print-right-margin* #:*query-io* #:*random-state* #:*read-base*
           #:*read-default-float-format* #:*read-eval* #:*read-suppress* #:*readtable*
           #:*standard-input* #:*standard-output* #:*terminal-io* #:*trace-output* #:+ #:++ #:+++ #:-
           #:/ #:// #:/// #:/= #:1+ #:1- #:< #:<= #:= #:> #:>= #:abort #:abs #:acons #:acos #:acosh
           #:add-method #:adjoin #:adjust-array #:adjustable-array-p #:allocate-instance
           #:alpha-char-p #:alphanumericp #:and #:append #:apply #:apropos #:apropos-list #:aref
           #:arithmetic-error #:arithmetic-error-operands #:arithmetic-error-operation #:array
           #:array-dimension #:array-dimension-limit #:array-dimensions #:array-displacement
           #:array-element-type #:array-has-fill-pointer-p #:array-in-bounds-p #:array-rank
           #:array-rank-limit #:array-row-major-index #:array-total-size #:array-total-size-limit
           #:arrayp #:ash #:asin #:asinh #:assert #:assoc #:assoc-if #:assoc-if-not #:atan #:atanh
           #:atom #:base-char #:base-string #:bignum #:bit #:bit-and #:bit-andc1 #:bit-andc2
           #:bit-eqv #:bit-ior #:bit-nand #:bit-nor #:bit-not #:bit-orc1 #:bit-orc2 #:bit-vector
           #:bit-vector-p #:bit-xor #:block #:boole #:boole-1 #:boole-2 #:boole-and #:boole-andc1
           #:boole-andc2 #:boole-c1 #:boole-c2 #:boole-clr #:boole-eqv #:boole-ior #:boole-nand
           #:boole-nor #:boole-orc1 #:boole-orc2 #:boole-set #:boole-xor #:boolean #:both-case-p
           #:boundp #:break #:broadcast-stream #:broadcast-stream-streams #:built-in-class #:butlast
           #:byte #:byte-position #:byte-size #:caaaar #:caaadr #:caaar #:caadar #:caaddr #:caadr
           #:caar #:cadaar #:cadadr #:cadar #:caddar #:cadddr #:caddr #:cadr #:call-arguments-limit
           #:call-method #:call-next-method #:car #:case #:catch #:ccase #:cdaaar #:cdaadr #:cdaar
           #:cdadar #:cdaddr #:cdadr #:cdar #:cddaar #:cddadr #:cddar #:cdddar #:cddddr #:cdddr
           #:cddr #:cdr #:ceiling #:cell-error #:cell-error-name #:cerror #:change-class #:char
           #:char-code #:char-code-limit #:char-downcase #:char-equal #:char-greaterp #:char-int
           #:char-lessp #:char-name #:char-not-equal #:char-not-greaterp #:char-not-lessp
           #:char-upcase #:char/= #:char< #:char<= #:char= #:char> #:char>= #:character #:characterp
           #:check-type #:cis #:class #:class-name #:class-of #:clear-input #:clear-output #:close
           #:clrhash #:code-char #:coerce #:compilation-speed #:compile #:compile-file
           #:compile-file-pathname #:compiled-function #:compiled-function-p #:compiler-macro
           #:compiler-macro-function #:complement #:complex #:complexp #:compute-applicable-methods
           #:compute-restarts #:concatenate #:concatenated-stream #:concatenated-stream-streams
           #:cond #:condition #:conjugate #:cons #:consp #:constantly #:constantp #:continue
           #:control-error #:copy-alist #:copy-list #:copy-pprint-dispatch #:copy-readtable
           #:copy-seq #:copy-structure #:copy-symbol #:copy-tree #:cos #:cosh #:count #:count-if
           #:count-if-not #:ctypecase #:debug #:decf #:declaim #:declaration #:declare #:decode-float
           #:decode-universal-time #:defclass #:defconstant #:defgeneric #:define-compiler-macro
           #:define-condition #:define-method-combination #:define-modify-macro
           #:define-setf-expander #:define-symbol-macro #:defmacro #:defmethod #:defpackage
           #:defparameter #:defsetf #:defstruct #:deftype #:defun #:defvar #:delete
           #:delete-duplicates #:delete-file #:delete-if #:delete-if-not #:delete-package
           #:denominator #:deposit-field #:describe #:describe-object #:destructuring-bind
           #:digit-char #:digit-char-p #:directory #:directory-namestring #:disassemble
           #:division-by-zero #:do #:do* #:do-all-symbols #:do-external-symbols #:do-symbols
           #:documentation #:dolist #:dotimes #:double-float #:double-float-epsilon
           #:double-float-negative-epsilon #:dpb #:dribble #:dynamic-extent #:ecase #:echo-stream
           #:echo-stream-input-stream #:echo-stream-output-stream #:ed #:eighth #:elt
           #:encode-universal-time #:end-of-file #:endp #:enough-namestring
           #:ensure-directories-exist #:ensure-generic-function #:eq #:eql #:equal #:equalp #:error
           #:etypecase #:eval #:eval-when #:evenp #:every #:exp #:export #:expt #:extended-char
           #:fboundp #:fceiling #:fdefinition #:ffloor #:fifth #:file-author #:file-error
           #:file-error-pathname #:file-length #:file-namestring #:file-position #:file-stream
           #:file-string-length #:file-write-date #:fill #:fill-pointer #:find #:find-all-symbols
           #:find-class #:find-if #:find-if-not #:find-method #:find-package #:find-restart
           #:find-symbol #:finish-output #:first #:fixnum #:flet #:float #:float-digits
           #:float-precision #:float-radix #:float-sign #:floating-point-inexact
           #:floating-point-invalid-operation #:floating-point-overflow #:floating-point-underflow
           #:floatp #:floor #:fmakunbound #:force-output #:format #:formatter #:fourth #:fresh-line
           #:fround #:ftruncate #:ftype #:funcall #:function #:function-keywords
           #:function-lambda-expression #:functionp #:gcd #:generic-function #:gensym #:gentemp #:get
           #:get-decoded-time #:get-dispatch-macro-character #:get-internal-real-time
           #:get-internal-run-time #:get-macro-character #:get-output-stream-string #:get-properties
           #:get-setf-expansion #:get-universal-time #:getf #:gethash #:go #:graphic-char-p
           #:handler-bind #:handler-case #:hash-table #:hash-table-count #:hash-table-p
           #:hash-table-rehash-size #:hash-table-rehash-threshold #:hash-table-size #:hash-table-test
           #:host-namestring #:identity #:if #:ignorable #:ignore #:ignore-errors #:imagpart #:import
           #:in-package #:incf #:initialize-instance #:inline #:input-stream-p #:inspect #:integer
           #:integer-decode-float #:integer-length #:integerp #:interactive-stream-p #:intern
           #:internal-time-units-per-second #:intersection #:invalid-method-error #:invoke-debugger
           #:invoke-restart #:invoke-restart-interactively #:isqrt #:keyword #:keywordp #:labels
           #:lambda #:lambda-list-keywords #:lambda-parameters-limit #:last #:lcm #:ldb #:ldb-test
           #:ldiff #:least-negative-double-float #:least-negative-long-float
           #:least-negative-normalized-double-float #:least-negative-normalized-long-float
           #:least-negative-normalized-short-float #:least-negative-normalized-single-float
           #:least-negative-short-float #:least-negative-single-float #:least-positive-double-float
           #:least-positive-long-float #:least-positive-normalized-double-float
           #:least-positive-normalized-long-float #:least-positive-normalized-short-float
           #:least-positive-normalized-single-float #:least-positive-short-float
           #:least-positive-single-float #:length #:let #:let* #:lisp-implementation-type
           #:lisp-implementation-version #:list #:list* #:list-all-packages #:list-length #:listen
           #:listp #:load #:load-logical-pathname-translations #:load-time-value #:locally #:log
           #:logand #:logandc1 #:logandc2 #:logbitp #:logcount #:logeqv #:logical-pathname
           #:logical-pathname-translations #:logior #:lognand #:lognor #:lognot #:logorc1 #:logorc2
           #:logtest #:logxor #:long-float #:long-float-epsilon #:long-float-negative-epsilon
           #:long-site-name #:loop #:loop-finish #:lower-case-p #:machine-instance #:machine-type
           #:machine-version #:macro-function #:macroexpand #:macroexpand-1 #:macrolet #:make-array
           #:make-broadcast-stream #:make-concatenated-stream #:make-condition
           #:make-dispatch-macro-character #:make-echo-stream #:make-hash-table #:make-instance
           #:make-instances-obsolete #:make-list #:make-load-form #:make-load-form-saving-slots
           #:make-method #:make-package #:make-pathname #:make-random-state #:make-sequence
           #:make-string #:make-string-input-stream #:make-string-output-stream #:make-symbol
           #:make-synonym-stream #:make-two-way-stream #:makunbound #:map #:map-into #:mapc #:mapcan
           #:mapcar #:mapcon #:maphash #:mapl #:maplist #:mask-field #:max #:member #:member-if
           #:member-if-not #:merge #:merge-pathnames #:method #:method-combination
           #:method-combination-error #:method-qualifiers #:min #:minusp #:mismatch #:mod
           #:most-negative-double-float #:most-negative-fixnum #:most-negative-long-float
           #:most-negative-short-float #:most-negative-single-float #:most-positive-double-float
           #:most-positive-fixnum #:most-positive-long-float #:most-positive-short-float
           #:most-positive-single-float #:muffle-warning #:multiple-value-bind #:multiple-value-call
           #:multiple-value-list #:multiple-value-prog1 #:multiple-value-setq #:multiple-values-limit
           #:name-char #:namestring #:nbutlast #:nconc #:next-method-p #:nil #:nintersection #:ninth
           #:no-applicable-method #:no-next-method #:not #:notany #:notevery #:notinline #:nreconc
           #:nreverse #:nset-difference #:nset-exclusive-or #:nstring-capitalize #:nstring-downcase
           #:nstring-upcase #:nsublis #:nsubst #:nsubst-if #:nsubst-if-not #:nsubstitute
           #:nsubstitute-if #:nsubstitute-if-not #:nth #:nth-value #:nthcdr #:null #:number #:numberp
           #:numerator #:nunion #:oddp #:open #:open-stream-p #:optimize #:or #:otherwise
           #:output-stream-p #:package #:package-error #:package-error-package #:package-name
           #:package-nicknames #:package-shadowing-symbols #:package-use-list #:package-used-by-list
           #:packagep #:pairlis #:parse-error #:parse-integer #:parse-namestring #:pathname
           #:pathname-device #:pathname-directory #:pathname-host #:pathname-match-p #:pathname-name
           #:pathname-type #:pathname-version #:pathnamep #:peek-char #:phase #:pi #:plusp #:pop
           #:position #:position-if #:position-if-not #:pprint #:pprint-dispatch
           #:pprint-exit-if-list-exhausted #:pprint-fill #:pprint-indent #:pprint-linear
           #:pprint-logical-block #:pprint-newline #:pprint-pop #:pprint-tab #:pprint-tabular #:prin1
           #:prin1-to-string #:princ #:princ-to-string #:print #:print-not-readable
           #:print-not-readable-object #:print-object #:print-unreadable-object #:probe-file
           #:proclaim #:prog #:prog* #:prog1 #:prog2 #:progn #:program-error #:progv #:provide
           #:psetf #:psetq #:push #:pushnew #:quote #:random #:random-state #:random-state-p #:rassoc
           #:rassoc-if #:rassoc-if-not #:ratio #:rational #:rationalize #:rationalp #:read
           #:read-byte #:read-char #:read-char-no-hang #:read-delimited-list #:read-from-string
           #:read-line #:read-preserving-whitespace #:read-sequence #:reader-error #:readtable
           #:readtable-case #:readtablep #:real #:realp #:realpart #:reduce #:reinitialize-instance
           #:rem #:remf #:remhash #:remove #:remove-duplicates #:remove-if #:remove-if-not
           #:remove-method #:remprop #:rename-file #:rename-package #:replace #:require #:rest
           #:restart #:restart-bind #:restart-case #:restart-name #:return #:return-from #:revappend
           #:reverse #:room #:rotatef #:round #:row-major-aref #:rplaca #:rplacd #:safety #:satisfies
           #:sbit #:scale-float #:schar #:search #:second #:sequence #:serious-condition #:set
           #:set-difference #:set-dispatch-macro-character #:set-exclusive-or #:set-macro-character
           #:set-pprint-dispatch #:set-syntax-from-char #:setf #:setq #:seventh #:shadow
           #:shadowing-import #:shared-initialize #:shiftf #:short-float #:short-float-epsilon
           #:short-float-negative-epsilon #:short-site-name #:signal #:signed-byte #:signum
           #:simple-array #:simple-base-string #:simple-bit-vector #:simple-bit-vector-p
           #:simple-condition #:simple-condition-format-arguments #:simple-condition-format-control
           #:simple-error #:simple-string #:simple-string-p #:simple-type-error #:simple-vector
           #:simple-vector-p #:simple-warning #:sin #:single-float #:single-float-epsilon
           #:single-float-negative-epsilon #:sinh #:sixth #:sleep #:slot-boundp #:slot-exists-p
           #:slot-makunbound #:slot-missing #:slot-unbound #:slot-value #:software-type
           #:software-version #:some #:sort #:space #:special #:special-operator-p #:speed #:sqrt
           #:stable-sort #:standard #:standard-char #:standard-char-p #:standard-class
           #:standard-generic-function #:standard-method #:standard-object #:step #:storage-condition
           #:store-value #:stream #:stream-element-type #:stream-error #:stream-error-stream
           #:stream-external-format #:streamp #:string #:string-capitalize #:string-downcase
           #:string-equal #:string-greaterp #:string-left-trim #:string-lessp #:string-not-equal
           #:string-not-greaterp #:string-not-lessp #:string-right-trim #:string-stream #:string-trim
           #:string-upcase #:string/= #:string< #:string<= #:string= #:string> #:string>= #:stringp
           #:structure #:structure-class #:structure-object #:style-warning #:sublis #:subseq
           #:subsetp #:subst #:subst-if #:subst-if-not #:substitute #:substitute-if
           #:substitute-if-not #:subtypep #:svref #:sxhash #:symbol #:symbol-function
           #:symbol-macrolet #:symbol-name #:symbol-package #:symbol-plist #:symbol-value #:symbolp
           #:synonym-stream #:synonym-stream-symbol #:t #:tagbody #:tailp #:tan #:tanh #:tenth
           #:terpri #:the #:third #:throw #:time #:trace #:translate-logical-pathname
           #:translate-pathname #:tree-equal #:truename #:truncate #:two-way-stream
           #:two-way-stream-input-stream #:two-way-stream-output-stream #:type #:type-error
           #:type-error-datum #:type-error-expected-type #:type-of #:typecase #:typep #:unbound-slot
           #:unbound-slot-instance #:unbound-variable #:undefined-function #:unexport #:unintern
           #:union #:unless #:unread-char #:unsigned-byte #:untrace #:unuse-package #:unwind-protect
           #:update-instance-for-different-class #:update-instance-for-redefined-class
           #:upgraded-array-element-type #:upgraded-complex-part-type #:upper-case-p #:use-package
           #:use-value #:user-homedir-pathname #:values #:values-list #:variable #:vector
           #:vector-pop #:vector-push #:vector-push-extend #:vectorp #:warn #:warning #:when
           #:wild-pathname-p #:with-accessors #:with-compilation-unit #:with-condition-restarts
           #:with-hash-table-iterator #:with-input-from-string #:with-open-file #:with-open-stream
           #:with-output-to-string #:with-package-iterator #:with-simple-restart #:with-slots
           #:with-standard-io-syntax #:write #:write-byte #:write-char #:write-line #:write-sequence
           #:write-string #:write-to-string #:y-or-n-p #:yes-or-no-p #:zerop))
        ;; XXX: we could use `closer-common-lisp', but some of McCLIM
        ;; MOP code is not conformant it seems (we have problems with
        ;; unknown `:default' argument dropping us in the debugger).
        ;(packages '(:closer-common-lisp))
        (packages '(:common-lisp))
        (gray-symbols
         '(#:fundamental-stream
           #:fundamental-input-stream
           #:fundamental-output-stream
           #:fundamental-character-stream
           #:fundamental-binary-stream
           #:fundamental-character-input-stream
           #:fundamental-character-output-stream
           #:fundamental-binary-input-stream
           #:fundamental-binary-output-stream
           #:stream-read-char
           #:stream-unread-char
           #:stream-read-char-no-hang
           #:stream-peek-char
           #:stream-listen
           #:stream-read-line
           #:stream-clear-input
           #:stream-write-char
           #:stream-line-column
           #:stream-start-line-p
           #:stream-write-string
           #:stream-terpri
           #:stream-fresh-line
           #:stream-finish-output
           #:stream-force-output
           #:stream-advance-to-column
           #:stream-clear-output
           #:stream-read-byte
           #:stream-write-byte ))
        (gray-packages '(#:trivial-gray-streams)))
    ;;
    (labels ((seek-symbol (name packages)
               ;; seek the a symbol named 'name' in `packages'
               (or (some #'(lambda (p)
                             (multiple-value-bind (sym res) (find-symbol (symbol-name name) p)
                               (if (eql res :external)
                                   (list sym)
                                   nil)))
                         packages)
                   (progn (format t "~&there is no ~A." name)
                          (force-output)
                          nil)))
             (dump-defpackage (&aux imports export-ansi export-gray)
               (labels ((push-import-from (symbol package)
                          (let ((pair (assoc package imports)))
                            (if pair
                                (push symbol (cdr pair))
                                (push `(,package . (,symbol))
                                      imports))))
                        (grok (symbols packages)
                          (let ((res nil))
                            (dolist (nam symbols)
                              (let ((sym (seek-symbol nam packages)))
                                (when sym
                                  (push (car sym) res)
                                  (cond
                                    ((and (find-package :clim-lisp-patch)
                                          (multiple-value-bind (sym2 res) (find-symbol (symbol-name nam) :clim-lisp-patch)
                                            (and sym2 (eq res :external))))
                                     ;;
                                     (format t "~&;; ~S is patched." sym)
                                     (force-output)
                                     (push-import-from nam :clim-lisp-patch))
                                    (t
                                     (setf sym (car sym))
                                     ;; clisp has no (:import ..) arg!
                                     (push-import-from
                                      (symbol-name sym)
                                      (package-name (symbol-package sym))))))))
                            res)))
                                  ;;
                 ;; Don't redefine a perfectly working CL:DESCRIBE,
                 ;; which more often than not has special knowledge
                 ;; about objects you can't possibly gain though some
                 ;; portable implementation.
                 ;; --GB 2004-11-20
                 (setf all-ansi-symbols (remove '#:describe all-ansi-symbols :test #'string-equal))
                 (setf all-ansi-symbols (remove '#:describe-object all-ansi-symbols :test #'string-equal))
                 ;;
                 (setf export-ansi (grok all-ansi-symbols packages))
                 (setf export-gray (grok gray-symbols gray-packages))
                 `(progn
                   (defpackage :clim-lisp (:use)
                     ,@(mapcar (lambda (spec)
                                 (destructuring-bind (package . syms) spec
                                   `(:import-from ,package ,@syms)))
                               imports)
                     (:shadow #:describe #:describe-object)
                     (:export #:describe #:describe-object)
                     (:export
                      ,@(mapcar #'symbol-name export-ansi)
                      ,@(mapcar #'symbol-name export-gray) )) ))))
      (dump-defpackage) ))

(defpackage :clim
  (:use)
  ;;
  (:import-from :clim-lisp
   #:and
   #:boolean
   #:character
   #:close
   #:complex
   #:float
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   #:fundamental-binary-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   #:fundamental-character-stream
   #:fundamental-input-stream
   #:fundamental-output-stream
   #:fundamental-stream
   #:input-stream-p
   #:integer
   #:interactive-stream-p
   #:keyword
   #:member
   #:nil
   #:null
   #:number
   #:open-stream-p
   #:or
   #:output-stream-p
   #:pathname
   #:ratio
   #:rational
   #:real
   #:sequence
   #:stream-advance-to-column
   #:stream-clear-input
   #:stream-clear-output
   #:stream-element-type
   #:stream-finish-output
   #:stream-force-output
   #:stream-fresh-line
   #:stream-line-column
   #:stream-listen
   #:stream-peek-char
   #:stream-read-byte
   #:stream-read-char
   #:stream-read-char-no-hang
   #:stream-read-line
   #:stream-start-line-p
   #:stream-terpri
   #:stream-unread-char
   #:stream-write-byte
   #:stream-write-char
   #:stream-write-string
   #:streamp
   #:string
   #:symbol
   #:t)
  ;;
  (:export

   ;; this list of exported symbols was automatically generated from the
   ;; specification as of version 1.17 of this very file, please think twice
   ;; before fiddling with it. thanks! --gb 2002-11-10

   #:*abort-gestures*                   ;variable
   #:*accelerator-gestures*             ;variable
   #:*activation-gestures*              ;variable
   #:*application-frame*                ;variable
   #:*command-argument-delimiters*      ;variable
   #:*command-dispatchers*              ;variable
   #:*command-name-delimiters*          ;variable
   #:*command-parser*                   ;variable
   #:*command-unparser*                 ;variable
   #:*completion-gestures*              ;variable
   #:*default-frame-manager*            ;variable
   #:*default-server-path*              ;variable
   #:*default-text-style*               ;constant
   #:*delimiter-gestures*               ;variable
   #:*help-gestures*                    ;variable
   #:*input-context*                    ;variable
   #:*input-wait-handler*               ;variable
   #:*input-wait-test*                  ;variable
   #:*null-presentation*                ;constant
   #:*numeric-argument-marker*          ;variable
   #:*original-stream*                  ;variable
   #:*partial-command-parser*           ;variable
   #:*pointer-button-press-handler*     ;variable
   #:*pointer-documentation-output*     ;variable
   #:*possibilities-gestures*           ;variable
   #:*standard-activation-gestures*     ;variable
   #:*undefined-text-style*             ;constant
   #:*unsupplied-argument-marker*       ;variable
   #:+background-ink+                   ;constant
   #:+black+                            ;constant
   #:+blue+                             ;constant
   #:+control-key+                      ;constant
   #:+cyan+                             ;constant
   #:+everywhere+                       ;constant
   #:+fill+                             ;constant
   #:+flipping-ink+                     ;constant
   #:+foreground-ink+                   ;constant
   #:+gadget-dialog-view+               ;constant
   #:+gadget-menu-view+                 ;constant
   #:+gadget-view+                      ;constant
   #:+green+                            ;constant
   #:+hyper-key+                        ;constant
   #:+identity-transformation+          ;constant
   #:+magenta+                          ;constant
   #:+meta-key+                         ;constant
   #:+nowhere+                          ;constant
   #:+pointer-documentation-view+       ;constant
   #:+pointer-left-button+              ;constant
   #:+pointer-middle-button+            ;constant
   #:+pointer-right-button+             ;constant
   #:+red+                              ;constant
   #:+shift-key+                        ;constant
   #:+super-key+                        ;constant
   #:+textual-dialog-view+              ;constant
   #:+textual-menu-view+                ;constant
   #:+textual-view+                     ;constant
   #:+transparent-ink+                  ;constant
   #:+white+                            ;constant
   #:+yellow+                           ;constant
   #:abort-gesture                      ;condition
   #:abort-gesture-event                ;generic function
   #:accelerator-gesture                ;condition
   #:accelerator-gesture-event          ;generic function
   #:accelerator-gesture-numeric-argument ;generic function
   #:accept                             ;presentation method
   #:accept                             ;function
   #:accept-1                           ;function
   #:accept-from-string                 ;function
   #:accept-present-default             ;presentation method
   #:accept-values                      ;frame
   #:accept-values-command-button       ;macro
   #:accept-values-resynchronize        ;generic function
   #:accepting-values                   ;macro
   #:action-gadget                      ;class
   #:activate-callback                  ;callback
   #:activate-gadget                    ;generic function
   #:activation-gesture-p               ;function
   #:add-character-output-to-text-record ;generic function
   #:add-command-to-command-table       ;function
   #:add-gesture-name                   ;function
   #:add-input-editor-command           ;function
   #:add-keystroke-to-command-table     ;function
   #:add-menu-item-to-command-table     ;function
   #:add-output-record                  ;generic function
   #:add-presentation-translator-to-command-table ;function
   #:add-string-output-to-text-record   ;generic function
   #:adjust-item-list-cells             ;generic function
   #:adjust-multiple-columns            ;generic function
   #:adjust-table-cells                 ;generic function
   #:adopt-frame                        ;generic function
   #:allocate-medium                    ;generic function
   #:allocate-pixmap                    ;generic function
   #:allocate-space                     ;generic function
   #:and                                ;presentation type
   #:application-frame                  ;protocol class
   #:application-frame-p                ;predicate
   #:application-pane                   ;pane
   #:apply-presentation-generic-function ;macro
   #:area                               ;protocol class
   #:areap                              ;predicate
   #:armed-callback                     ;callback
   #:augment-draw-set                   ;generic function
   #:basic-gadget                       ;class
   #:basic-medium                       ;class
   #:basic-pane                         ;class
   #:basic-port                         ;class
   #:basic-sheet                        ;class
   #:bboard-pane                        ;pane
   #:beep                               ;generic function
   #:blank-area                         ;presentation type
   #:boolean                            ;presentation type
   #:bounding-rectangle                 ;protocol class
   #:bounding-rectangle                 ;generic function
   #:bounding-rectangle*                ;generic function
   #:bounding-rectangle-height          ;generic function
   #:bounding-rectangle-max-x           ;generic function
   #:bounding-rectangle-max-y           ;generic function
   #:bounding-rectangle-min-x           ;generic function
   #:bounding-rectangle-min-y           ;generic function
   #:bounding-rectangle-p               ;predicate
   #:bounding-rectangle-position        ;generic function
   #:bounding-rectangle-size            ;generic function
   #:bounding-rectangle-width           ;generic function
   #:bury-frame                         ;generic function
   #:bury-mirror                        ;generic function
   #:bury-sheet                         ;generic function
   #:cache-output-record                ;generic function
   #:call-presentation-menu             ;function
   #:call-presentation-translator       ;function
   #:cell-align-x                       ;generic function
   #:cell-align-y                       ;generic function
   #:cell-min-height                    ;generic function
   #:cell-min-width                     ;generic function
   #:cell-output-record                 ;protocol class
   #:cell-output-record-p               ;predicate
   #:change-space-requirements          ;generic function
   #:changing-space-requirements        ;macro
   #:character                          ;presentation type
   #:check-box                          ;class
   #:check-box-current-selection        ;generic function
   #:check-box-pane                     ;class
   #:check-box-selections               ;generic function
   #:child-containing-position          ;generic function
   #:children-overlapping-rectangle*    ;generic function
   #:children-overlapping-region        ;generic function
   #:class-presentation-type-name       ;function
   #:clear-output-record                ;generic function
   #:client-setting                     ;setf method (through no reader)
   #:clim-stream-pane                   ;pane
   #:close                              ;generic function
   #:color                              ;protocol class
   #:color-ihs                          ;generic function
   #:color-rgb                          ;generic function
   #:colorp                             ;predicate
   #:column-output-record               ;protocol class
   #:column-output-record-p             ;predicate
   #:command                            ;presentation type
   #:command-accessible-in-command-table-p ;function
   #:command-already-present            ;error
   #:command-arguments                  ;function
   #:command-enabled                    ;generic function
   #:command-line-command-parser        ;function
   #:command-line-command-unparser      ;function
   #:command-line-name-for-command      ;function
   #:command-line-read-remaining-arguments-for-partial-command ;function
   #:command-menu-item-options          ;function
   #:command-menu-item-type             ;function
   #:command-menu-item-value            ;function
   #:command-menu-pane                  ;pane
   #:command-name                       ;presentation type
   #:command-name                       ;function
   #:command-name-from-symbol           ;function
   #:command-not-accessible             ;error
   #:command-not-present                ;error
   #:command-or-form                    ;presentation type
   #:command-present-in-command-table-p ;function
   #:command-table                      ;protocol class
   #:command-table-already-exists       ;error
   #:command-table-complete-input       ;function
   #:command-table-error                ;error
   #:command-table-inherit-from         ;generic function
   #:command-table-name                 ;generic function
   #:command-table-not-found            ;error
   #:command-table-p                    ;predicate
   #:complete-from-generator            ;function
   #:complete-from-possibilities        ;function
   #:complete-input                     ;function
   #:completing-from-suggestions        ;macro
   #:completion                         ;presentation type
   #:complex                            ;presentation type
   #:compose-in                         ;generic function
   #:compose-out                        ;generic function
   #:compose-over                       ;generic function
   #:compose-rotation-with-transformation ;function
   #:compose-scaling-with-transformation ;function
   #:compose-space                      ;generic function
   #:compose-transformation-with-rotation ;function
   #:compose-transformation-with-scaling ;function
   #:compose-transformation-with-translation ;function
   #:compose-transformations            ;generic function
   #:compose-translation-with-transformation ;function
   #:compute-difference-set             ;generic function
   #:compute-new-output-records         ;generic function
   #:contrasting-dash-pattern-limit     ;generic function
   #:contrasting-inks-limit             ;generic function
   #:coordinate                         ;type
   #:coordinate                         ;function
   #:copy-area                          ;generic function
   #:copy-from-pixmap                   ;function
   #:copy-textual-output-history        ;function
   #:copy-to-pixmap                     ;function
   #:cursor                             ;protocol class
   #:cursor-active                      ;generic function
   #:cursor-focus                       ;generic function
   #:cursor-position                    ;generic function
   #:cursor-set-position                ;generic function
   #:cursor-sheet                       ;generic function
   #:cursor-state                       ;generic function
   #:cursor-visibility                  ;generic function
   #:cursorp                            ;predicate
   #:deactivate-gadget                  ;generic function
   #:deallocate-medium                  ;generic function
   #:deallocate-pixmap                  ;generic function
   #:decache-child-output-record        ;generic function
   #:default-describe-presentation-type ;function
   #:default-frame-top-level            ;generic function
   #:define-application-frame           ;macro
   #:define-border-type                 ;macro
   #:define-command                     ;macro
   #:define-command-table               ;macro
   #:define-default-presentation-method ;macro
   #:define-drag-and-drop-translator    ;macro
   #:define-gesture-name                ;macro
   #:define-graph-type                  ;macro
   #:define-presentation-action         ;macro
   #:define-presentation-generic-function ;macro
   #:define-presentation-method         ;macro
   #:define-presentation-to-command-translator ;macro
   #:define-presentation-translator     ;macro
   #:define-presentation-type           ;macro
   #:define-presentation-type-abbreviation ;macro
   #:degraft-medium                     ;generic function
   #:delegate-sheet-delegate            ;generic function
   #:delegate-sheet-input-mixin         ;class
   #:delete-gesture-name                ;function
   #:delete-output-record               ;generic function
   #:delimiter-gesture-p                ;function
   #:describe-presentation-type         ;presentation method
   #:describe-presentation-type         ;function
   #:design                             ;protocol class
   #:designp                            ;predicate
   #:destroy-frame                      ;generic function
   #:destroy-mirror                     ;generic function
   #:destroy-port                       ;generic function
   #:device-event                       ;class
   #:disable-frame                      ;generic function
   #:disarmed-callback                  ;callback
   #:disown-frame                       ;generic function
   #:dispatch-event                     ;generic function
   #:display-command-menu               ;generic function
   #:display-command-table-menu         ;generic function
   #:display-exit-boxes                 ;generic function
   #:displayed-output-record            ;protocol class
   #:displayed-output-record-ink        ;generic function
   #:displayed-output-record-p          ;predicate
   #:distribute-event                   ;generic function
   #:do-command-table-inheritance       ;macro
   #:documentation-key                  ;presentation type option
   #:document-presentation-translator   ;function
   #:drag-callback                      ;callback
   #:drag-callback                      ;callback
   #:drag-output-record                 ;generic function
   #:dragging-output                    ;macro
   #:draw-arrow                         ;function
   #:draw-arrow*                        ;function
   #:draw-circle                        ;function
   #:draw-circle*                       ;function
   #:draw-design                        ;generic function
   #:draw-ellipse                       ;function
   #:draw-ellipse*                      ;function
   #:draw-line                          ;function
   #:draw-line*                         ;function
   #:draw-lines                         ;function
   #:draw-lines*                        ;function
   #:draw-oval                          ;function
   #:draw-oval*                         ;function
   #:draw-pattern*                      ;function
   #:draw-point                         ;function
   #:draw-point*                        ;function
   #:draw-points                        ;function
   #:draw-points*                       ;function
   #:draw-polygon                       ;function
   #:draw-polygon*                      ;function
   #:draw-rectangle                     ;function
   #:draw-rectangle*                    ;function
   #:draw-rectangles                    ;function
   #:draw-rectangles*                   ;function
   #:draw-standard-menu                 ;function
   #:draw-text                          ;function
   #:draw-text*                         ;function
   #:ellipse                            ;protocol class
   #:ellipse-center-point               ;generic function
   #:ellipse-center-point*              ;generic function
   #:ellipse-end-angle                  ;generic function
   #:ellipse-radii                      ;generic function
   #:ellipse-start-angle                ;generic function
   #:ellipsep                           ;predicate
   #:elliptical-arc                     ;protocol class
   #:elliptical-arc-p                   ;predicate
   #:enable-frame                       ;generic function
   #:encapsulating-stream               ;protocol class
   #:encapsulating-stream-p             ;predicate
   #:encapsulating-stream-stream        ;generic function
   #:engraft-medium                     ;generic function
   #:erase-input-buffer                 ;generic function
   #:erase-output-record                ;generic function
   #:even-scaling-transformation-p      ;generic function
   #:event                              ;protocol class
   #:event-listen                       ;generic function
   #:event-matches-gesture-name-p       ;function
   #:event-modifier-state               ;generic function
   #:event-peek                         ;generic function
   #:event-read                         ;generic function
   #:event-read-no-hang                 ;generic function
   #:event-sheet                        ;generic function
   #:event-timestamp                    ;generic function
   #:event-type                         ;generic function
   #:event-unread                       ;generic function
   #:eventp                             ;predicate
   #:execute-frame-command              ;generic function
   #:expand-presentation-type-abbreviation ;function
   #:expand-presentation-type-abbreviation-1 ;function
   #:expression                         ;presentation type
   #:extended-input-stream              ;protocol class
   #:extended-input-stream-p            ;predicate
   #:extended-output-stream             ;protocol class
   #:extended-output-stream-p           ;predicate
   #:filling-output                     ;macro
   #:find-applicable-translators        ;function
   #:find-cached-output-record          ;generic function
   #:find-child-output-record           ;generic function
   #:find-command-from-command-line-name ;function
   #:find-command-table                 ;function
   #:find-frame-manager                 ;function
   #:find-graft                         ;function
   #:find-innermost-applicable-presentation ;function
   #:find-keystroke-item                ;function
   #:find-menu-item                     ;function
   #:find-pane-for-frame                ;generic function
   #:find-pane-named                    ;generic function
   #:find-port                          ;function
   #:find-presentation-translator       ;function
   #:find-presentation-translators      ;function
   #:find-presentation-type-class       ;function
   #:float                              ;presentation type
   #:form                               ;presentation type
   #:format-graph-from-roots            ;function
   #:format-items                       ;function
   #:format-textual-list                ;function
   #:formatting-cell                    ;macro
   #:formatting-column                  ;macro
   #:formatting-item-list               ;macro
   #:formatting-row                     ;macro
   #:formatting-table                   ;macro
   #:frame-all-layouts                  ;generic function
   #:frame-calling-frame                ;generic function
   #:frame-command-table                ;generic function
   #:frame-current-layout               ;generic function
   #:frame-current-panes                ;generic function
   #:frame-document-highlighted-presentation ;generic function
   #:frame-drag-and-drop-feedback       ;generic function
   #:frame-drag-and-drop-highlighting   ;generic function
   #:frame-error-output                 ;generic function
   #:frame-exit                         ;condition
   #:frame-exit                         ;generic function
   #:frame-exit-frame                   ;generic function
   #:frame-find-innermost-applicable-presentation ;generic function
   #:frame-input-context-button-press-handler ;generic function
   #:frame-maintain-presentation-histories ;generic function
   #:frame-manager                      ;protocol class
   #:frame-manager                      ;generic function
   #:frame-manager-frames               ;generic function
   #:frame-manager-menu-choose          ;generic function
   #:frame-manager-notify-user          ;generic function
   #:frame-manager-p                    ;predicate
   #:frame-name                         ;generic function
   #:frame-panes                        ;generic function
   #:frame-parent                       ;generic function
   #:frame-pointer-documentation-output ;generic function
   #:frame-pretty-name                  ;generic function
   #:frame-properties                   ;generic function
   #:frame-query-io                     ;generic function
   #:frame-replay                       ;generic function
   #:frame-standard-input               ;generic function
   #:frame-standard-output              ;generic function
   #:frame-state                        ;generic function
   #:frame-top-level-sheet              ;generic function
   #:funcall-presentation-generic-function ;macro
   #:fundamental-binary-input-stream    ;class
   #:fundamental-binary-output-stream   ;class
   #:fundamental-binary-stream          ;class
   #:fundamental-character-input-stream ;class
   #:fundamental-character-output-stream ;class
   #:fundamental-character-stream       ;class
   #:fundamental-input-stream           ;class
   #:fundamental-output-stream          ;class
   #:fundamental-stream                 ;class
   #:gadget                             ;protocol class
   #:gadget-activate-callback           ;generic function
   #:gadget-active-p                    ;generic function
   #:gadget-armed-callback              ;generic function
   #:gadget-client                      ;generic function
   #:gadget-dialog-view                 ;class
   #:gadget-disarmed-callback           ;generic function
   #:gadget-id                          ;generic function
   #:gadget-label                       ;generic function
   #:gadget-label-align-x               ;generic function
   #:gadget-label-align-y               ;generic function
   #:gadget-max-value                   ;generic function
   #:gadget-menu-view                   ;class
   #:gadget-min-value                   ;generic function
   #:gadget-orientation                 ;generic function
   #:gadget-output-record               ;class
   #:gadget-range                       ;generic function
   #:gadget-range*                      ;generic function
   #:gadget-show-value-p                ;generic function
   #:gadget-value                       ;generic function
   #:gadget-value-changed-callback      ;generic function
   #:gadget-view                        ;class
   #:gadgetp                            ;predicate
   #:generate-graph-nodes               ;generic function
   #:generate-panes                     ;generic function
   #:generic-list-pane                  ;class
   #:generic-option-pane                ;class
   #:get-frame-pane                     ;generic function
   #:global-command-table               ;command table
   #:graft                              ;generic function
   #:graft-height                       ;generic function
   #:graft-orientation                  ;generic function
   #:graft-pixels-per-inch              ;function
   #:graft-pixels-per-millimeter        ;function
   #:graft-units                        ;generic function
   #:graft-width                        ;generic function
   #:graph-node-children                ;generic function
   #:graph-node-object                  ;generic function
   #:graph-node-output-record           ;protocol class
   #:graph-node-output-record-p         ;predicate
   #:graph-node-parents                 ;generic function
   #:graph-output-record                ;protocol class
   #:graph-output-record-p              ;predicate
   #:graph-root-nodes                   ;generic function
   #:graphics-displayed-output-record   ;protocol class
   #:graphics-displayed-output-record-p ;predicate
   #:grid-pane                          ;pane
   #:handle-event                       ;generic function
   #:handle-repaint                     ;generic function
   #:hbox-pane                          ;pane
   #:highlight-applicable-presentation  ;function
   #:highlight-output-record            ;generic function
   #:highlight-presentation             ;presentation method
   #:highlighter                        ;presentation type option
   #:horizontally                       ;macro
   #:hrack-pane                         ;pane
   #:identity-transformation-p          ;generic function
   #:immediate-repainting-mixin         ;class
   #:immediate-rescan                   ;generic function
   #:immediate-sheet-input-mixin        ;class
   #:incremental-redisplay              ;generic function
   #:indenting-output                   ;macro
   #:input-context-type                 ;function
   #:input-editing-stream               ;protocol class
   #:input-editing-stream-p             ;predicate
   #:input-editor-format                ;generic function
   #:input-not-of-required-type         ;error
   #:input-not-of-required-type         ;function
   #:input-stream-p                     ;generic function
   #:integer                            ;presentation type
   #:interactive-stream-p               ;predicate
   #:interactor-pane                    ;pane
   #:invalidate-cached-regions          ;generic function
   #:invalidate-cached-transformations  ;generic function
   #:invert-transformation              ;generic function
   #:invertible-transformation-p        ;generic function
   #:invoke-updating-output             ;generic function
   #:invoke-with-drawing-options        ;generic function
   #:invoke-with-new-output-record      ;generic function
   #:invoke-with-output-recording-options ;generic function
   #:invoke-with-output-to-output-record ;generic function
   #:invoke-with-text-style             ;generic function
   #:item-list-output-record            ;protocol class
   #:item-list-output-record-p          ;predicate
   #:key-press-event                    ;class
   #:key-release-event                  ;class
   #:keyboard-event                     ;class
   #:keyboard-event-character           ;generic function
   #:keyboard-event-key-name            ;generic function
   #:keyword                            ;presentation type
   #:label-pane                         ;pane
   #:labelled-gadget-mixin              ;class
   #:labelling                          ;macro
   #:layout-frame                       ;generic function
   #:layout-graph-edges                 ;generic function
   #:layout-graph-nodes                 ;generic function
   #:line                               ;protocol class
   #:line-end-point                     ;generic function
   #:line-end-point*                    ;generic function
   #:line-start-point                   ;generic function
   #:line-start-point*                  ;generic function
   #:line-style                         ;protocol class
   #:line-style-cap-shape               ;generic function
   #:line-style-dashes                  ;generic function
   #:line-style-joint-shape             ;generic function
   #:line-style-p                       ;predicate
   #:line-style-thickness               ;generic function
   #:line-style-unit                    ;generic function
   #:linep                              ;predicate
   #:list-pane                          ;class
   #:lookup-keystroke-command-item      ;function
   #:lookup-keystroke-item              ;function
   #:make-3-point-transformation        ;function
   #:make-3-point-transformation*       ;function
   #:make-application-frame             ;function
   #:make-bounding-rectangle            ;function
   #:make-clim-application-pane         ;function
   #:make-clim-interactor-pane          ;function
   #:make-clim-stream-pane              ;function
   #:make-command-table                 ;function
   #:make-contrasting-dash-patterns     ;function
   #:make-contrasting-inks              ;function
   #:make-design-from-output-record     ;generic function
   #:make-device-font-text-style        ;function
   #:make-ellipse                       ;function
   #:make-ellipse*                      ;function
   #:make-elliptical-arc                ;function
   #:make-elliptical-arc*               ;function
   #:make-flipping-ink                  ;function
   #:make-gray-color                    ;function
   #:make-ihs-color                     ;function
   #:make-line                          ;function
   #:make-line*                         ;function
   #:make-line-style                    ;function
   #:make-medium                        ;generic function
   #:make-modifier-state                ;function
   #:make-opacity                       ;function
   #:make-pane                          ;function
   #:make-pane-1                        ;generic function
   #:make-pattern                       ;function
   #:make-pattern-from-bitmap-file      ;function
   #:make-point                         ;function
   #:make-polygon                       ;function
   #:make-polygon*                      ;function
   #:make-polyline                      ;function
   #:make-polyline*                     ;function
   #:make-presentation-type-specifier   ;function
   #:make-rectangle                     ;function
   #:make-rectangle*                    ;function
   #:make-rectangular-tile              ;function
   #:make-reflection-transformation     ;function
   #:make-reflection-transformation*    ;function
   #:make-rgb-color                     ;function
   #:make-rotation-transformation       ;function
   #:make-rotation-transformation*      ;function
   #:make-scaling-transformation        ;function
   #:make-scaling-transformation*       ;function
   #:make-space-requirement             ;function
   #:make-stencil                       ;function
   #:make-text-style                    ;function
   #:make-transformation                ;function
   #:make-translation-transformation    ;function
   #:map-over-command-table-commands    ;function
   #:map-over-command-table-keystrokes  ;function
   #:map-over-command-table-menu-items  ;function
   #:map-over-command-table-names       ;function
   #:map-over-command-table-translators ;function
   #:map-over-frames                    ;function
   #:map-over-grafts                    ;function
   #:map-over-item-list-cells           ;generic function
   #:map-over-output-records		;generic function
   #:map-over-output-records-containing-position ;generic function
   #:map-over-output-records-overlapping-region ;generic function
   #:map-over-polygon-coordinates       ;generic function
   #:map-over-polygon-segments          ;generic function
   #:map-over-ports                     ;function
   #:map-over-presentation-type-supertypes ;presentation method
   #:map-over-presentation-type-supertypes ;function
   #:map-over-region-set-regions        ;generic function
   #:map-over-row-cells                 ;generic function
   #:map-over-row-cells                 ;generic function
   #:map-over-sheets                    ;generic function
   #:map-over-sheets-containing-position ;generic function
   #:map-over-sheets-overlapping-region ;generic function
   #:map-over-table-elements            ;generic function
   #:map-sheet-position-to-child        ;generic function
   #:map-sheet-position-to-parent       ;generic function
   #:map-sheet-rectangle*-to-child      ;generic function
   #:map-sheet-rectangle*-to-parent     ;generic function
   #:match-output-records               ;generic function
   #:medium                             ;protocol class
   #:medium-background                  ;generic function
   #:medium-background                  ;generic function
   #:medium-beep                        ;generic function
   #:medium-buffering-output-p          ;generic function
   #:medium-clear-area                  ;generic function
   #:medium-clipping-region             ;generic function
   #:medium-clipping-region             ;generic function
   #:medium-copy-area                   ;generic function
   #:medium-current-text-style          ;generic function
   #:medium-default-text-style          ;generic function
   #:medium-default-text-style          ;generic function
   #:medium-draw-ellipse*               ;generic function
   #:medium-draw-line*                  ;generic function
   #:medium-draw-lines*                 ;generic function
   #:medium-draw-point*                 ;generic function
   #:medium-draw-points*                ;generic function
   #:medium-draw-polygon*               ;generic function
   #:medium-draw-rectangle*             ;generic function
   #:medium-draw-rectangles*            ;generic function
   #:medium-draw-text*                  ;generic function
   #:medium-drawable                    ;generic function
   #:medium-finish-output               ;generic function
   #:medium-force-output                ;generic function
   #:medium-foreground                  ;generic function
   #:medium-foreground                  ;generic function
   #:medium-ink                         ;generic function
   #:medium-ink                         ;generic function
   #:medium-line-style                  ;generic function
   #:medium-line-style                  ;generic function
   #:medium-merged-text-style           ;generic function
   #:medium-sheet                       ;generic function
   #:medium-text-style                  ;generic function
   #:medium-text-style                  ;generic function
   #:medium-transformation              ;generic function
   #:medium-transformation              ;generic function
   #:mediump                            ;predicate
   #:member                             ;presentation type abbrev
   #:member-alist                       ;presentation type abbrev
   #:member-sequence                    ;presentation type abbrev
   #:menu-button                        ;class
   #:menu-button-pane                   ;class
   #:menu-choose                        ;generic function
   #:menu-choose-command-from-command-table ;function
   #:menu-choose-from-drawer            ;generic function
   #:menu-command-parser                ;function
   #:menu-item                          ;presentation type (mentioned in the description of draw-standard-menu)
   #:menu-item-display                  ;function
   #:menu-item-options                  ;function
   #:menu-item-value                    ;function
   #:menu-read-remaining-arguments-for-partial-command ;function
   #:merge-text-styles                  ;generic function
   #:mirrored-sheet-mixin               ;class
   #:modifier-state-matches-gesture-name-p ;function
   #:move-and-resize-sheet              ;generic function
   #:move-sheet                         ;generic function
   #:name-key                           ;presentation type option
   #:new-page                           ;function
   #:nil                                ;presentation type
   #:note-command-disabled              ;generic function
   #:note-command-enabled               ;generic function
   #:note-frame-deiconified             ;generic function
   #:note-frame-disabled                ;generic function
   #:note-frame-enabled                 ;generic function
   #:note-frame-iconified               ;generic function
   #:note-gadget-activated              ;generic function
   #:note-gadget-deactivated            ;generic function
   #:note-output-record-child-changed   ;generic function
   #:note-sheet-adopted                 ;generic function
   #:note-sheet-degrafted               ;generic function
   #:note-sheet-disabled                ;generic function
   #:note-sheet-disowned                ;generic function
   #:note-sheet-enabled                 ;generic function
   #:note-sheet-grafted                 ;generic function
   #:note-sheet-region-changed          ;generic function
   #:note-sheet-transformation-changed  ;generic function
   #:note-space-requirements-changed    ;generic function
   #:notify-user                        ;generic function
   #:null                               ;presentation type
   #:null-or-type                       ;presentation type abbrev
   #:number                             ;presentation type
   #:opacity                            ;protocol class
   #:opacity-value                      ;generic function
   #:opacityp                           ;predicate
   #:open-stream-p                      ;generic function
   #:open-window-stream                 ;function
   #:option-pane                        ;class
   #:or                                 ;presentation type
   #:oriented-gadget-mixin              ;class
   #:outlined-pane                      ;pane
   #:outlining                          ;macro
   #:output-record                      ;protocol class
   #:output-record-cache-value          ;generic function
   #:output-record-children             ;generic function
   #:output-record-contents-ok          ;generic function
   #:output-record-count                ;generic function
   #:output-record-displayer            ;generic function
   #:output-record-end-cursor-position  ;generic function
   #:output-record-fixed-position       ;generic function
   #:output-record-hit-detection-rectangle* ;generic function
   #:output-record-p                    ;predicate
   #:output-record-parent               ;generic function
   #:output-record-position             ;generic function
   #:output-record-refined-position-test ;generic function
   #:output-record-start-cursor-position ;generic function
   #:output-record-unique-id            ;generic function
   #:output-recording-stream            ;protocol class
   #:output-recording-stream-p          ;predicate
   #:output-stream-p                    ;generic function
   #:pane                               ;protocol class
   #:pane-background                    ;generic function
   #:pane-foreground                    ;generic function
   #:pane-frame                         ;generic function
   #:pane-name                          ;generic function
   #:pane-needs-redisplay               ;generic function
   #:pane-scroller                      ;generic function
   #:pane-text-style                    ;generic function
   #:pane-viewport                      ;generic function
   #:pane-viewport-region               ;generic function
   #:panep                              ;predicate
   #:parse-text-style                   ;function
   #:partial-command-p                  ;function
   #:partial-completers                 ;presentation type option
   #:path                               ;protocol class
   #:pathname                           ;presentation type
   #:pathp                              ;predicate
   #:pattern-height                     ;generic function
   #:pattern-width                      ;generic function
   #:permanent-medium-sheet-output-mixin ;class
   #:pixmap-depth                       ;generic function
   #:pixmap-height                      ;generic function
   #:pixmap-width                       ;generic function
   #:point                              ;protocol class
   #:point-position                     ;generic function
   #:point-x                            ;generic function
   #:point-y                            ;generic function
   #:pointer                            ;protocol class
   #:pointer-boundary-event             ;class
   #:pointer-boundary-event-kind        ;generic function
   #:pointer-button-event               ;class
   #:pointer-button-hold-event          ;class
   #:pointer-button-press-event         ;class
   #:pointer-button-release-event       ;class
   #:pointer-button-state               ;generic function
   #:pointer-click-and-hold-event       ;class
   #:pointer-click-event                ;class
   #:pointer-cursor                     ;generic function
   #:pointer-documentation-pane         ;pane
   #:pointer-documentation-view         ;class
   #:pointer-double-click-event         ;class
   #:pointer-enter-event                ;class
   #:pointer-event                      ;class
   #:pointer-event-button               ;generic function
   #:pointer-event-native-x             ;generic function
   #:pointer-event-native-y             ;generic function
   #:pointer-event-pointer              ;generic function
   #:pointer-event-x                    ;generic function
   #:pointer-event-y                    ;generic function
   #:pointer-exit-event                 ;class
   #:pointer-motion-event               ;class
   #:pointer-position                   ;generic function
   #:pointer-sheet                      ;generic function
   #:pointerp                           ;predicate
   #:pointp                             ;predicate
   #:polygon                            ;protocol class
   #:polygon-points                     ;generic function
   #:polygonp                           ;predicate
   #:polyline                           ;protocol class
   #:polyline-closed                    ;generic function
   #:polylinep                          ;predicate
   #:port                               ;protocol class
   #:port                               ;generic function
   #:port-keyboard-input-focus          ;generic function
   #:port-name                          ;generic function
   #:port-properties                    ;generic function
   #:port-server-path                   ;generic function
   #:port-type                          ;generic function
   #:portp                              ;predicate
   #:present                            ;presentation method
   #:present                            ;function
   #:present-to-string                  ;function
   #:presentation                       ;protocol class
   #:presentation-default-preprocessor  ;presentation method
   #:presentation-matches-context-type  ;function
   #:presentation-modifier              ;generic function
   #:presentation-object                ;generic function
   #:presentation-refined-position-test ;presentation method
   #:presentation-replace-input         ;generic function
   #:presentation-single-box            ;generic function
   #:presentation-subtypep              ;presentation method
   #:presentation-subtypep              ;function
   #:presentation-type                  ;generic function
   #:presentation-type-direct-supertypes ;function
   #:presentation-type-history          ;presentation method
   #:presentation-type-name             ;function
   #:presentation-type-of               ;function
   #:presentation-type-options          ;function
   #:presentation-type-parameters       ;function
   #:presentation-type-specifier-p      ;presentation method
   #:presentation-type-specifier-p      ;function
   #:presentation-typep                 ;presentation method
   #:presentation-typep                 ;function
   #:presentationp                      ;predicate
   #:print-menu-item                    ;function
   #:printer                            ;presentation type option
   #:process-next-event                 ;generic function
   #:prompt-for-accept                  ;generic function
   #:prompt-for-accept-1                ;function
   #:propagate-output-record-changes    ;generic function
   #:propagate-output-record-changes-p  ;generic function
   #:push-button                        ;class
   #:push-button-pane                   ;class
   #:push-button-show-as-default        ;generic function
   #:queue-event                        ;generic function
   #:queue-repaint                      ;generic function
   #:queue-rescan                       ;generic function
   #:radio-box                          ;class
   #:radio-box-current-selection        ;generic function
   #:radio-box-pane                     ;class
   #:radio-box-selections               ;generic function
   #:raise-frame                        ;generic function
   #:raise-mirror                       ;generic function
   #:raise-sheet                        ;generic function
   #:range-gadget-mixin                 ;class
   #:ratio                              ;presentation type
   #:rational                           ;presentation type
   #:read-bitmap-file                   ;generic function
   #:read-command                       ;function
   #:read-command-using-keystrokes      ;function
   #:read-frame-command                 ;generic function
   #:read-gesture                       ;function
   #:read-token                         ;function
   #:real                               ;presentation type
   #:realize-mirror                     ;generic function
   #:recompute-contents-ok              ;generic function
   #:recompute-extent-for-changed-child ;generic function
   #:recompute-extent-for-new-child     ;generic function
   #:rectangle                          ;protocol class
   #:rectangle-edges*                   ;generic function
   #:rectangle-height                   ;generic function
   #:rectangle-max-point                ;generic function
   #:rectangle-max-x                    ;generic function
   #:rectangle-max-y                    ;generic function
   #:rectangle-min-point                ;generic function
   #:rectangle-min-x                    ;generic function
   #:rectangle-min-y                    ;generic function
   #:rectangle-size                     ;generic function
   #:rectangle-width                    ;generic function
   #:rectanglep                         ;predicate
   #:rectilinear-transformation-p       ;generic function
   #:redisplay                          ;function
   #:redisplay-frame-pane               ;generic function
   #:redisplay-frame-panes              ;generic function
   #:redisplay-output-record            ;generic function
   #:redisplayable-stream-p             ;generic function
   #:redraw-input-buffer                ;generic function
   #:reflection-transformation-p        ;generic function
   #:reflection-underspecified          ;error
   #:region                             ;protocol class
   #:region-contains-position-p         ;generic function
   #:region-contains-region-p           ;generic function
   #:region-difference                  ;generic function
   #:region-equal                       ;generic function
   #:region-intersection                ;generic function
   #:region-intersects-region-p         ;generic function
   #:region-set                         ;protocol class
   #:region-set-p                       ;predicate
   #:region-set-regions                 ;generic function
   #:region-union                       ;generic function
   #:regionp                            ;predicate
   #:remove-command-from-command-table  ;function
   #:remove-keystroke-from-command-table ;function
   #:remove-menu-item-from-command-table ;function
   #:remove-presentation-translator-from-command-table ;function
   #:reorder-sheets                     ;generic function
   #:repaint-sheet                      ;generic function
   #:replace-input                      ;generic function
   #:replay                             ;function
   #:replay-output-record               ;generic function
   #:rescan-if-necessary                ;generic function
   #:reset-frame                        ;generic function
   #:reset-scan-pointer                 ;generic function
   #:resize-sheet                       ;generic function
   #:restart-port                       ;generic function
   #:restraining                        ;macro
   #:restraining-pane                   ;pane
   #:rigid-transformation-p             ;generic function
   #:row-output-record                  ;protocol class
   #:row-output-record-p                ;predicate
   #:run-frame-top-level                ;generic function
   #:scaling-transformation-p           ;generic function
   #:scroll-bar                         ;class
   #:scroll-bar-drag-callback           ;generic function
   #:scroll-bar-pane                    ;class
   #:scroll-bar-scroll-down-line-callback ;generic function
   #:scroll-bar-scroll-down-page-callback ;generic function
   #:scroll-bar-scroll-to-bottom-callback ;generic function
   #:scroll-bar-scroll-to-top-callback  ;generic function
   #:scroll-bar-scroll-up-line-callback ;generic function
   #:scroll-bar-scroll-up-page-callback ;generic function
   #:scroll-down-line-callback          ;callback
   #:scroll-down-page-callback          ;callback
   #:scroll-extent                      ;generic function
   #:scroll-to-bottom-callback          ;callback
   #:scroll-to-top-callback             ;callback
   #:scroll-up-line-callback            ;callback
   #:scroll-up-page-callback            ;callback
   #:scroller-pane                      ;pane
   #:scrolling                          ;macro
   #:sequence                           ;presentation type
   #:sequence-enumerated                ;presentation type
   #:set-highlighted-presentation       ;function
   #:sheet                              ;protocol class
   #:sheet-adopt-child                  ;generic function
   #:sheet-allocated-region             ;generic function
   #:sheet-ancestor-p                   ;generic function
   #:sheet-children                     ;generic function
   #:sheet-delta-transformation         ;generic function
   #:sheet-device-region                ;generic function
   #:sheet-device-transformation        ;generic function
   #:sheet-direct-mirror                ;generic function
   #:sheet-disown-child                 ;generic function
   #:sheet-enabled-children             ;generic function
   #:sheet-enabled-p                    ;generic function
   #:sheet-event-queue                  ;generic function
   #:sheet-grafted-p                    ;generic function
   #:sheet-identity-transformation-mixin ;class
   #:sheet-leaf-mixin                   ;class
   #:sheet-medium                       ;generic function
   #:sheet-mirror                       ;generic function
   #:sheet-mirrored-ancestor            ;generic function
   #:sheet-multiple-child-mixin         ;class
   #:sheet-mute-input-mixin             ;class
   #:sheet-mute-output-mixin            ;class
   #:sheet-mute-repainting-mixin        ;class
   #:sheet-native-region                ;generic function
   #:sheet-native-transformation        ;generic function
   #:sheet-occluding-sheets             ;generic function
   #:sheet-parent                       ;generic function
   #:sheet-parent-mixin                 ;class
   #:sheet-region                       ;generic function
   #:sheet-siblings                     ;generic function
   #:sheet-single-child-mixin           ;class
   #:sheet-transformation               ;generic function
   #:sheet-transformation-mixin         ;class
   #:sheet-translation-mixin            ;class
   #:sheet-viewable-p                   ;generic function
   #:sheet-with-medium-mixin            ;class
   #:sheet-y-inverting-transformation-mixin ;class
   #:sheetp                             ;predicate
   #:shrink-frame                       ;generic function
   #:simple-completion-error            ;condition
   #:simple-parse-error                 ;error
   #:simple-parse-error                 ;function
   #:singular-transformation            ;error
   #:slider                             ;class
   #:slider-drag-callback               ;generic function
   #:slider-pane                        ;class
   #:space-requirement                  ;class
   #:space-requirement+                 ;function
   #:space-requirement+*                ;function
   #:space-requirement-combine          ;function
   #:space-requirement-components       ;generic function
   #:space-requirement-height           ;generic function
   #:space-requirement-max-height       ;generic function
   #:space-requirement-max-width        ;generic function
   #:space-requirement-min-height       ;generic function
   #:space-requirement-min-width        ;generic function
   #:space-requirement-width            ;generic function
   #:spacing                            ;macro
   #:spacing-pane                       ;pane
   #:standard-application-frame         ;class
   #:standard-bounding-rectangle        ;class
   #:standard-cell-output-record        ;class
   #:standard-column-output-record      ;class
   #:standard-command-table             ;class
   #:standard-ellipse                   ;class
   #:standard-elliptical-arc            ;class
   #:standard-encapsulating-stream      ;class
   #:standard-extended-input-stream     ;class
   #:standard-extended-output-stream    ;class
   #:standard-graph-node-output-record  ;class
   #:standard-graph-output-record       ;class
   #:standard-input-editing-stream      ;class
   #:standard-input-stream              ;class
   #:standard-item-list-output-record   ;class
   #:standard-line                      ;class
   #:standard-line-style                ;class
   #:standard-output-recording-stream   ;class
   #:standard-output-stream             ;class
   #:standard-point                     ;class
   #:standard-pointer                   ;class
   #:standard-polygon                   ;class
   #:standard-polyline                  ;class
   #:standard-presentation              ;class
   #:standard-rectangle                 ;class
   #:standard-region-difference         ;class
   #:standard-region-intersection       ;class
   #:standard-region-union              ;class
   #:standard-repainting-mixin          ;class
   #:standard-row-output-record         ;class
   #:standard-sequence-output-history   ;class
   #:standard-sequence-output-record    ;class
   #:standard-sheet-input-mixin         ;class
   #:standard-sheet-output-mixin        ;class
   #:standard-table-output-record       ;class
   #:standard-text-cursor               ;class
   #:standard-text-style                ;class
   #:standard-tree-output-history       ;class
   #:standard-tree-output-record        ;class
   #:standard-updating-output-record    ;class
   #:stream-accept                      ;generic function
   #:stream-add-character-output        ;generic function
   #:stream-add-output-record           ;generic function
   #:stream-add-string-output           ;generic function
   #:stream-advance-to-column           ;generic function
   #:stream-advance-to-column           ;generic function
   #:stream-baseline                    ;generic function
   #:stream-character-width             ;generic function
   #:stream-clear-input                 ;generic function
   #:stream-clear-input                 ;generic function
   #:stream-clear-output                ;generic function
   #:stream-clear-output                ;generic function
   #:stream-close-text-output-record    ;generic function
   #:stream-current-output-record       ;generic function
   #:stream-cursor-position             ;generic function
   #:stream-default-view                ;generic function
   #:stream-drawing-p                   ;generic function
   #:stream-element-type                ;generic function
   #:stream-end-of-line-action          ;generic function
   #:stream-end-of-page-action          ;generic function
   #:stream-finish-output               ;generic function
   #:stream-finish-output               ;generic function
   #:stream-force-output                ;generic function
   #:stream-force-output                ;generic function
   #:stream-fresh-line                  ;generic function
   #:stream-fresh-line                  ;generic function
   #:stream-increment-cursor-position   ;generic function
   #:stream-input-buffer                ;generic function
   #:stream-input-wait                  ;generic function
   #:stream-insertion-pointer           ;generic function
   #:stream-line-column                 ;generic function
   #:stream-line-column                 ;generic function
   #:stream-line-height                 ;generic function
   #:stream-listen                      ;generic function
   #:stream-listen                      ;generic function
   #:stream-output-history              ;generic function
   #:stream-output-history-mixin        ;class
   #:stream-pathname                    ;generic function
   #:stream-peek-char                   ;generic function
   #:stream-peek-char                   ;generic function
   #:stream-pointer-position            ;generic function
   #:stream-present                     ;generic function
   #:stream-process-gesture             ;generic function
   #:stream-read-byte                   ;generic function
   #:stream-read-char                   ;generic function
   #:stream-read-char                   ;generic function
   #:stream-read-char-no-hang           ;generic function
   #:stream-read-char-no-hang           ;generic function
   #:stream-read-gesture                ;generic function
   #:stream-read-line                   ;generic function
   #:stream-read-line                   ;generic function
   #:stream-recording-p                 ;generic function
   #:stream-redisplaying-p              ;generic function
   #:stream-replay                      ;generic function
   #:stream-rescanning-p                ;generic function
   #:stream-scan-pointer                ;generic function
   #:stream-set-cursor-position         ;generic function
   #:stream-set-input-focus             ;generic function
   #:stream-start-line-p                ;generic function
   #:stream-start-line-p                ;generic function
   #:stream-string-width                ;generic function
   #:stream-terpri                      ;generic function
   #:stream-terpri                      ;generic function
   #:stream-text-cursor                 ;generic function
   #:stream-text-margin                 ;generic function
   #:stream-text-output-record          ;generic function
   #:stream-truename                    ;generic function
   #:stream-unread-char                 ;generic function
   #:stream-unread-char                 ;generic function
   #:stream-unread-gesture              ;generic function
   #:stream-vertical-spacing            ;generic function
   #:stream-write-byte                  ;generic function
   #:stream-write-char                  ;generic function
   #:stream-write-char                  ;generic function
   #:stream-write-string                ;generic function
   #:stream-write-string                ;generic function
   #:streamp                            ;generic function
   #:string                             ;presentation type
   #:subset                             ;presentation type abbrev
   #:subset-alist                       ;presentation type abbrev
   #:subset-completion                  ;presentation type
   #:subset-sequence                    ;presentation type abbrev
   #:substitute-numeric-argument-marker ;function
   #:suggest                            ;function
   #:surrounding-output-with-border     ;macro
   #:symbol                             ;presentation type
   #:t                                  ;presentation type
   #:table-output-record                ;protocol class
   #:table-output-record-p              ;predicate
   #:table-pane                         ;pane
   #:tabling                            ;macro
   #:temporary-medium-sheet-output-mixin ;class
   #:test                               ;presentation type parameter
   #:test-presentation-translator       ;function
   #:text-displayed-output-record       ;protocol class
   #:text-displayed-output-record-p     ;predicate
   #:text-displayed-output-record-string ;generic function
   #:text-editor                        ;class
   #:text-editor-pane                   ;class
   #:text-field                         ;class
   #:text-field-pane                    ;class
   #:text-size                          ;generic function
   #:text-style                         ;protocol class
   #:text-style-ascent                  ;generic function
   #:text-style-components              ;generic function
   #:text-style-descent                 ;generic function
   #:text-style-face                    ;generic function
   #:text-style-family                  ;generic function
   #:text-style-fixed-width-p           ;generic function
   #:text-style-height                  ;generic function
   #:text-style-mapping                 ;generic function
   #:text-style-p                       ;predicate
   #:text-style-size                    ;generic function
   #:text-style-width                   ;generic function
   #:textual-dialog-view                ;class
   #:textual-menu-view                  ;class
   #:textual-view                       ;class
   #:throw-highlighted-presentation     ;function
   #:timer-event                        ;class
   #:title-pane                         ;pane
   #:toggle-button                      ;class
   #:toggle-button-indicator-type       ;generic function
   #:toggle-button-pane                 ;class
   #:token-or-type                      ;presentation type abbrev
   #:tracking-pointer                   ;macro
   #:transform-distance                 ;generic function
   #:transform-position                 ;generic function
   #:transform-rectangle*               ;generic function
   #:transform-region                   ;generic function
   #:transformation                     ;protocol class
   #:transformation-equal               ;generic function
   #:transformation-error               ;error
   #:transformation-underspecified      ;error
   #:transformationp                    ;predicate
   #:translation-transformation-p       ;generic function
   #:tree-recompute-extent              ;generic function
   #:type-or-string                     ;presentation type abbrev
   #:unhighlight-highlighted-presentation ;function
   #:unread-gesture                     ;function
   #:untransform-distance               ;generic function
   #:untransform-position               ;generic function
   #:untransform-rectangle*             ;generic function
   #:untransform-region                 ;generic function
   #:updating-output                    ;macro
   #:updating-output-record             ;protocol class
   #:updating-output-record-p           ;predicate
   #:user-command-table                 ;command table
   #:value-changed-callback             ;callback
   #:value-gadget                       ;class
   #:value-key                          ;presentation type parameter
   #:vbox-pane                          ;pane
   #:vertically                         ;macro
   #:view                               ;protocol class
   #:viewp                              ;predicate
   #:vrack-pane                         ;pane
   #:window-clear                       ;generic function
   #:window-configuration-event         ;class
   #:window-erase-viewport              ;generic function
   #:window-event                       ;class
   #:window-event-mirrored-sheet        ;generic function
   #:window-event-native-region         ;generic function
   #:window-event-region                ;generic function
   #:window-manager-delete-event        ;class
   #:window-manager-event               ;class
   #:window-refresh                     ;generic function
   #:window-repaint-event               ;class
   #:window-viewport                    ;generic function
   #:window-viewport-position           ;generic function
   #:with-accept-help                   ;macro
   #:with-activation-gestures           ;macro
   #:with-application-frame             ;macro
   #:with-bounding-rectangle*           ;macro
   #:with-command-table-keystrokes      ;macro
   #:with-delimiter-gestures            ;macro
   #:with-drawing-options               ;macro
   #:with-end-of-line-action            ;macro
   #:with-end-of-page-action            ;macro
   #:with-first-quadrant-coordinates    ;macro
   #:with-frame-manager                 ;macro
   #:with-graft-locked                  ;macro
   #:with-identity-transformation       ;macro
   #:with-input-context                 ;macro
   #:with-input-editing                 ;macro
   #:with-input-editor-typeout          ;macro
   #:with-input-focus                   ;macro
   #:with-local-coordinates             ;macro
   #:with-look-and-feel-realization     ;macro
   #:with-menu                          ;macro
   #:with-new-output-record             ;macro
   #:with-output-as-gadget              ;macro
   #:with-output-as-presentation        ;macro
   #:with-output-buffered               ;macro
   #:with-output-recording-options      ;macro
   #:with-output-to-output-record       ;macro
   #:with-output-to-pixmap              ;macro
   #:with-output-to-postscript-stream   ;macro
   #:with-port-locked                   ;macro
   #:with-presentation-type-decoded     ;macro
   #:with-presentation-type-options     ;macro
   #:with-presentation-type-parameters  ;macro
   #:with-radio-box                     ;macro
   #:with-room-for-graphics             ;macro
   #:with-rotation                      ;macro
   #:with-scaling                       ;macro
   #:with-sheet-medium                  ;macro
   #:with-sheet-medium-bound            ;macro
   #:with-text-face                     ;macro
   #:with-text-family                   ;macro
   #:with-text-size                     ;macro
   #:with-text-style                    ;macro
   #:with-translation                   ;macro
   #:write-token                        ;function
   )

  ;;;; symbols, which were exported as of 2002-02-09, but no longer are.

  ;; dispatch-repaint:
  ;; several mentions in silica.tex.

  ;; invoke-accept-values-command-button
  ;; mentioned in dialogs.tex.

  ;; labelled

  ;; labelled-gadget, through there is a labelled-gadget-mixin
  ;; mute-repainting-mixin, through there is a sheet-mute-repainting-mixin
  ;; oriented-gadget, through there is a oriented-gadget-mixin

  ;; pointer-button-click-event
  ;; this is mentioned in silica.tex. spelling error?

  ;; bordering
  ;; border-pane
  ;; this is mentioned in LW CLIM documentation

  ;; spacer-pane
  ;; this is mentioned in our own documentation (!) but absent

  ;;;; absolutly no mention of the following in the spec:

  ;; display-cursor
  ;; draw-triangle
  ;; draw-triangle*
  ;; gadget-label-text-style
  ;; key-modifier-state-match-p
  ;; pointer-button-click-and-hold-event
  ;; pointer-button-double-click-event
  ;; pointer-port
  ;; push-button-show-as-default-p

  ;;; Vendor extensions which are exported from the CLIM package,
  ;;; including CLIM 2.2 symbols.
  (:export
   ;; I forget where the wheel symbols come from. They aren't in the
   ;; Franz guide. Are they from Lispworks, or did McCLIM invent them?
   #:+pointer-wheel-up+
   #:+pointer-wheel-down+
   #:+pointer-wheel-left+
   #:+pointer-wheel-right+
   ;; Franz and CLIM 2.2 Stuff:
   #:+text-field-view+                  ;constant (Franz User's Guide)
   #:find-application-frame		;function (in Franz User's Guide)
   #:format-graph-from-root            ;function
   #:list-pane-view                     ;class
   #:+list-pane-view+                   ;constant
   #:option-pane-view                   ;class
   #:+option-pane-view+                 ;constant
   #:port-modifier-state		;generic function (in franz user guide)
   #:port-pointer			;generic function (in franz user guide)
   #:push-button-view                   ;class
   #:+push-button-view+                 ;constant
   #:radio-box-view                     ;class
   #:+radio-box-view+                   ;class
   #:check-box-view                     ;class
   #:+check-box-view+                   ;class
   #:read-bitmap-file                   ;function
   #:slider-view                        ;slider-view
   #:+slider-view+                      ;constant
   #:text-editor-view                   ;class
   #:+text-editor-view+                 ;constant
   #:text-field-view                    ;class (Franz User's Guide)
   #:toggle-button-view                 ;class
   #:+toggle-button-view+               ;constant
   #:sheet-pointer-cursor)

   ;;; x11 color names - some are not in the spec - mikemac
  (:export
   #:+snow+ #:+ghost-white+ #:+ghostwhite+ #:+white-smoke+
   #:+whitesmoke+ #:+gainsboro+ #:+floral-white+ #:+floralwhite+
   #:+old-lace+ #:+oldlace+ #:+linen+ #:+antique-white+
   #:+antiquewhite+ #:+papaya-whip+ #:+papayawhip+ #:+blanched-almond+
   #:+blanchedalmond+ #:+bisque+ #:+peach-puff+ #:+peachpuff+
   #:+navajo-white+ #:+navajowhite+ #:+moccasin+ #:+cornsilk+
   #:+ivory+ #:+lemon-chiffon+ #:+lemonchiffon+ #:+seashell+
   #:+honeydew+ #:+mint-cream+ #:+mintcream+ #:+azure+
   #:+alice-blue+ #:+aliceblue+ #:+lavender+ #:+lavender-blush+
   #:+lavenderblush+ #:+misty-rose+ #:+mistyrose+ #:+white+
   #:+black+ #:+dark-slate-gray+ #:+darkslategray+ #:+dark-slate-grey+
   #:+darkslategrey+ #:+dim-gray+ #:+dimgray+ #:+dim-grey+
   #:+dimgrey+ #:+slate-gray+ #:+slategray+ #:+slate-grey+
   #:+slategrey+ #:+light-slate-gray+ #:+lightslategray+ #:+light-slate-grey+
   #:+lightslategrey+ #:+gray+ #:+grey+ #:+light-grey+
   #:+lightgrey+ #:+light-gray+ #:+lightgray+ #:+midnight-blue+
   #:+midnightblue+ #:+navy+ #:+navy-blue+ #:+navyblue+
   #:+cornflower-blue+ #:+cornflowerblue+ #:+dark-slate-blue+ #:+darkslateblue+
   #:+slate-blue+ #:+slateblue+ #:+medium-slate-blue+ #:+mediumslateblue+
   #:+light-slate-blue+ #:+lightslateblue+ #:+medium-blue+ #:+mediumblue+
   #:+royal-blue+ #:+royalblue+ #:+blue+ #:+dodger-blue+
   #:+dodgerblue+ #:+deep-sky-blue+ #:+deepskyblue+ #:+sky-blue+
   #:+skyblue+ #:+light-sky-blue+ #:+lightskyblue+ #:+steel-blue+
   #:+steelblue+ #:+light-steel-blue+ #:+lightsteelblue+ #:+light-blue+
   #:+lightblue+ #:+powder-blue+ #:+powderblue+ #:+pale-turquoise+
   #:+paleturquoise+ #:+dark-turquoise+ #:+darkturquoise+ #:+medium-turquoise+
   #:+mediumturquoise+ #:+turquoise+ #:+cyan+ #:+light-cyan+
   #:+lightcyan+ #:+cadet-blue+ #:+cadetblue+ #:+medium-aquamarine+
   #:+mediumaquamarine+ #:+aquamarine+ #:+dark-green+ #:+darkgreen+
   #:+dark-olive-green+ #:+darkolivegreen+ #:+dark-sea-green+ #:+darkseagreen+
   #:+sea-green+ #:+seagreen+ #:+medium-sea-green+ #:+mediumseagreen+
   #:+light-sea-green+ #:+lightseagreen+ #:+pale-green+ #:+palegreen+
   #:+spring-green+ #:+springgreen+ #:+lawn-green+ #:+lawngreen+
   #:+green+ #:+chartreuse+ #:+medium-spring-green+ #:+mediumspringgreen+
   #:+green-yellow+ #:+greenyellow+ #:+lime-green+ #:+limegreen+
   #:+yellow-green+ #:+yellowgreen+ #:+forest-green+ #:+forestgreen+
   #:+olive-drab+ #:+olivedrab+ #:+dark-khaki+ #:+darkkhaki+
   #:+khaki+ #:+pale-goldenrod+ #:+palegoldenrod+ #:+light-goldenrod-yellow+
   #:+lightgoldenrodyellow+ #:+light-yellow+ #:+lightyellow+ #:+yellow+
   #:+gold+ #:+light-goldenrod+ #:+lightgoldenrod+ #:+goldenrod+
   #:+dark-goldenrod+ #:+darkgoldenrod+ #:+rosy-brown+ #:+rosybrown+
   #:+indian-red+ #:+indianred+ #:+saddle-brown+ #:+saddlebrown+
   #:+sienna+ #:+peru+ #:+burlywood+ #:+beige+
   #:+wheat+ #:+sandy-brown+ #:+sandybrown+ #:+tan+
   #:+chocolate+ #:+firebrick+ #:+brown+ #:+dark-salmon+
   #:+darksalmon+ #:+salmon+ #:+light-salmon+ #:+lightsalmon+
   #:+orange+ #:+dark-orange+ #:+darkorange+ #:+coral+
   #:+light-coral+ #:+lightcoral+ #:+tomato+ #:+orange-red+
   #:+orangered+ #:+red+ #:+hot-pink+ #:+hotpink+
   #:+deep-pink+ #:+deeppink+ #:+pink+ #:+light-pink+
   #:+lightpink+ #:+pale-violet-red+ #:+palevioletred+ #:+maroon+
   #:+medium-violet-red+ #:+mediumvioletred+ #:+violet-red+ #:+violetred+
   #:+magenta+ #:+violet+ #:+plum+ #:+orchid+
   #:+medium-orchid+ #:+mediumorchid+ #:+dark-orchid+ #:+darkorchid+
   #:+dark-violet+ #:+darkviolet+ #:+blue-violet+ #:+blueviolet+
   #:+purple+ #:+medium-purple+ #:+mediumpurple+ #:+thistle+
   #:+snow1+ #:+snow2+ #:+snow3+ #:+snow4+
   #:+seashell1+ #:+seashell2+ #:+seashell3+ #:+seashell4+
   #:+antiquewhite1+ #:+antiquewhite2+ #:+antiquewhite3+ #:+antiquewhite4+
   #:+bisque1+ #:+bisque2+ #:+bisque3+ #:+bisque4+
   #:+peachpuff1+ #:+peachpuff2+ #:+peachpuff3+ #:+peachpuff4+
   #:+navajowhite1+ #:+navajowhite2+ #:+navajowhite3+ #:+navajowhite4+
   #:+lemonchiffon1+ #:+lemonchiffon2+ #:+lemonchiffon3+ #:+lemonchiffon4+
   #:+cornsilk1+ #:+cornsilk2+ #:+cornsilk3+ #:+cornsilk4+
   #:+ivory1+ #:+ivory2+ #:+ivory3+ #:+ivory4+
   #:+honeydew1+ #:+honeydew2+ #:+honeydew3+ #:+honeydew4+
   #:+lavenderblush1+ #:+lavenderblush2+ #:+lavenderblush3+ #:+lavenderblush4+
   #:+mistyrose1+ #:+mistyrose2+ #:+mistyrose3+ #:+mistyrose4+
   #:+azure1+ #:+azure2+ #:+azure3+ #:+azure4+
   #:+slateblue1+ #:+slateblue2+ #:+slateblue3+ #:+slateblue4+
   #:+royalblue1+ #:+royalblue2+ #:+royalblue3+ #:+royalblue4+
   #:+blue1+ #:+blue2+ #:+blue3+ #:+blue4+
   #:+dodgerblue1+ #:+dodgerblue2+ #:+dodgerblue3+ #:+dodgerblue4+
   #:+steelblue1+ #:+steelblue2+ #:+steelblue3+ #:+steelblue4+
   #:+deepskyblue1+ #:+deepskyblue2+ #:+deepskyblue3+ #:+deepskyblue4+
   #:+skyblue1+ #:+skyblue2+ #:+skyblue3+ #:+skyblue4+
   #:+lightskyblue1+ #:+lightskyblue2+ #:+lightskyblue3+ #:+lightskyblue4+
   #:+slategray1+ #:+slategray2+ #:+slategray3+ #:+slategray4+
   #:+lightsteelblue1+ #:+lightsteelblue2+ #:+lightsteelblue3+ #:+lightsteelblue4+
   #:+lightblue1+ #:+lightblue2+ #:+lightblue3+ #:+lightblue4+
   #:+lightcyan1+ #:+lightcyan2+ #:+lightcyan3+ #:+lightcyan4+
   #:+paleturquoise1+ #:+paleturquoise2+ #:+paleturquoise3+ #:+paleturquoise4+
   #:+cadetblue1+ #:+cadetblue2+ #:+cadetblue3+ #:+cadetblue4+
   #:+turquoise1+ #:+turquoise2+ #:+turquoise3+ #:+turquoise4+
   #:+cyan1+ #:+cyan2+ #:+cyan3+ #:+cyan4+
   #:+darkslategray1+ #:+darkslategray2+ #:+darkslategray3+ #:+darkslategray4+
   #:+aquamarine1+ #:+aquamarine2+ #:+aquamarine3+ #:+aquamarine4+
   #:+darkseagreen1+ #:+darkseagreen2+ #:+darkseagreen3+ #:+darkseagreen4+
   #:+seagreen1+ #:+seagreen2+ #:+seagreen3+ #:+seagreen4+
   #:+palegreen1+ #:+palegreen2+ #:+palegreen3+ #:+palegreen4+
   #:+springgreen1+ #:+springgreen2+ #:+springgreen3+ #:+springgreen4+
   #:+green1+ #:+green2+ #:+green3+ #:+green4+
   #:+chartreuse1+ #:+chartreuse2+ #:+chartreuse3+ #:+chartreuse4+
   #:+olivedrab1+ #:+olivedrab2+ #:+olivedrab3+ #:+olivedrab4+
   #:+darkolivegreen1+ #:+darkolivegreen2+ #:+darkolivegreen3+ #:+darkolivegreen4+
   #:+khaki1+ #:+khaki2+ #:+khaki3+ #:+khaki4+
   #:+lightgoldenrod1+ #:+lightgoldenrod2+ #:+lightgoldenrod3+ #:+lightgoldenrod4+
   #:+lightyellow1+ #:+lightyellow2+ #:+lightyellow3+ #:+lightyellow4+
   #:+yellow1+ #:+yellow2+ #:+yellow3+ #:+yellow4+
   #:+gold1+ #:+gold2+ #:+gold3+ #:+gold4+
   #:+goldenrod1+ #:+goldenrod2+ #:+goldenrod3+ #:+goldenrod4+
   #:+darkgoldenrod1+ #:+darkgoldenrod2+ #:+darkgoldenrod3+ #:+darkgoldenrod4+
   #:+rosybrown1+ #:+rosybrown2+ #:+rosybrown3+ #:+rosybrown4+
   #:+indianred1+ #:+indianred2+ #:+indianred3+ #:+indianred4+
   #:+sienna1+ #:+sienna2+ #:+sienna3+ #:+sienna4+
   #:+burlywood1+ #:+burlywood2+ #:+burlywood3+ #:+burlywood4+
   #:+wheat1+ #:+wheat2+ #:+wheat3+ #:+wheat4+
   #:+tan1+ #:+tan2+ #:+tan3+ #:+tan4+
   #:+chocolate1+ #:+chocolate2+ #:+chocolate3+ #:+chocolate4+
   #:+firebrick1+ #:+firebrick2+ #:+firebrick3+ #:+firebrick4+
   #:+brown1+ #:+brown2+ #:+brown3+ #:+brown4+
   #:+salmon1+ #:+salmon2+ #:+salmon3+ #:+salmon4+
   #:+lightsalmon1+ #:+lightsalmon2+ #:+lightsalmon3+ #:+lightsalmon4+
   #:+orange1+ #:+orange2+ #:+orange3+ #:+orange4+
   #:+darkorange1+ #:+darkorange2+ #:+darkorange3+ #:+darkorange4+
   #:+coral1+ #:+coral2+ #:+coral3+ #:+coral4+
   #:+tomato1+ #:+tomato2+ #:+tomato3+ #:+tomato4+
   #:+orangered1+ #:+orangered2+ #:+orangered3+ #:+orangered4+
   #:+red1+ #:+red2+ #:+red3+ #:+red4+
   #:+deeppink1+ #:+deeppink2+ #:+deeppink3+ #:+deeppink4+
   #:+hotpink1+ #:+hotpink2+ #:+hotpink3+ #:+hotpink4+
   #:+pink1+ #:+pink2+ #:+pink3+ #:+pink4+
   #:+lightpink1+ #:+lightpink2+ #:+lightpink3+ #:+lightpink4+
   #:+palevioletred1+ #:+palevioletred2+ #:+palevioletred3+ #:+palevioletred4+
   #:+maroon1+ #:+maroon2+ #:+maroon3+ #:+maroon4+
   #:+violetred1+ #:+violetred2+ #:+violetred3+ #:+violetred4+
   #:+magenta1+ #:+magenta2+ #:+magenta3+ #:+magenta4+
   #:+orchid1+ #:+orchid2+ #:+orchid3+ #:+orchid4+
   #:+plum1+ #:+plum2+ #:+plum3+ #:+plum4+
   #:+mediumorchid1+ #:+mediumorchid2+ #:+mediumorchid3+ #:+mediumorchid4+
   #:+darkorchid1+ #:+darkorchid2+ #:+darkorchid3+ #:+darkorchid4+
   #:+purple1+ #:+purple2+ #:+purple3+ #:+purple4+
   #:+mediumpurple1+ #:+mediumpurple2+ #:+mediumpurple3+ #:+mediumpurple4+
   #:+thistle1+ #:+thistle2+ #:+thistle3+ #:+thistle4+
   #:+gray0+ #:+grey0+ #:+gray1+ #:+grey1+
   #:+gray2+ #:+grey2+ #:+gray3+ #:+grey3+
   #:+gray4+ #:+grey4+ #:+gray5+ #:+grey5+
   #:+gray6+ #:+grey6+ #:+gray7+ #:+grey7+
   #:+gray8+ #:+grey8+ #:+gray9+ #:+grey9+
   #:+gray10+ #:+grey10+ #:+gray11+ #:+grey11+
   #:+gray12+ #:+grey12+ #:+gray13+ #:+grey13+
   #:+gray14+ #:+grey14+ #:+gray15+ #:+grey15+
   #:+gray16+ #:+grey16+ #:+gray17+ #:+grey17+
   #:+gray18+ #:+grey18+ #:+gray19+ #:+grey19+
   #:+gray20+ #:+grey20+ #:+gray21+ #:+grey21+
   #:+gray22+ #:+grey22+ #:+gray23+ #:+grey23+
   #:+gray24+ #:+grey24+ #:+gray25+ #:+grey25+
   #:+gray26+ #:+grey26+ #:+gray27+ #:+grey27+
   #:+gray28+ #:+grey28+ #:+gray29+ #:+grey29+
   #:+gray30+ #:+grey30+ #:+gray31+ #:+grey31+
   #:+gray32+ #:+grey32+ #:+gray33+ #:+grey33+
   #:+gray34+ #:+grey34+ #:+gray35+ #:+grey35+
   #:+gray36+ #:+grey36+ #:+gray37+ #:+grey37+
   #:+gray38+ #:+grey38+ #:+gray39+ #:+grey39+
   #:+gray40+ #:+grey40+ #:+gray41+ #:+grey41+
   #:+gray42+ #:+grey42+ #:+gray43+ #:+grey43+
   #:+gray44+ #:+grey44+ #:+gray45+ #:+grey45+
   #:+gray46+ #:+grey46+ #:+gray47+ #:+grey47+
   #:+gray48+ #:+grey48+ #:+gray49+ #:+grey49+
   #:+gray50+ #:+grey50+ #:+gray51+ #:+grey51+
   #:+gray52+ #:+grey52+ #:+gray53+ #:+grey53+
   #:+gray54+ #:+grey54+ #:+gray55+ #:+grey55+
   #:+gray56+ #:+grey56+ #:+gray57+ #:+grey57+
   #:+gray58+ #:+grey58+ #:+gray59+ #:+grey59+
   #:+gray60+ #:+grey60+ #:+gray61+ #:+grey61+
   #:+gray62+ #:+grey62+ #:+gray63+ #:+grey63+
   #:+gray64+ #:+grey64+ #:+gray65+ #:+grey65+
   #:+gray66+ #:+grey66+ #:+gray67+ #:+grey67+
   #:+gray68+ #:+grey68+ #:+gray69+ #:+grey69+
   #:+gray70+ #:+grey70+ #:+gray71+ #:+grey71+
   #:+gray72+ #:+grey72+ #:+gray73+ #:+grey73+
   #:+gray74+ #:+grey74+ #:+gray75+ #:+grey75+
   #:+gray76+ #:+grey76+ #:+gray77+ #:+grey77+
   #:+gray78+ #:+grey78+ #:+gray79+ #:+grey79+
   #:+gray80+ #:+grey80+ #:+gray81+ #:+grey81+
   #:+gray82+ #:+grey82+ #:+gray83+ #:+grey83+
   #:+gray84+ #:+grey84+ #:+gray85+ #:+grey85+
   #:+gray86+ #:+grey86+ #:+gray87+ #:+grey87+
   #:+gray88+ #:+grey88+ #:+gray89+ #:+grey89+
   #:+gray90+ #:+grey90+ #:+gray91+ #:+grey91+
   #:+gray92+ #:+grey92+ #:+gray93+ #:+grey93+
   #:+gray94+ #:+grey94+ #:+gray95+ #:+grey95+
   #:+gray96+ #:+grey96+ #:+gray97+ #:+grey97+
   #:+gray98+ #:+grey98+ #:+gray99+ #:+grey99+
   #:+gray100+ #:+grey100+ #:+dark-grey+ #:+darkgrey+
   #:+dark-gray+ #:+darkgray+ #:+dark-blue+ #:+darkblue+
   #:+dark-cyan+ #:+darkcyan+ #:+dark-magenta+ #:+darkmagenta+
   #:+dark-red+ #:+darkred+ #:+light-green+ #:+lightgreen+ ))

(defpackage :clim-sys
  (:use)
  ;;
  (:export
   #:defresource
   #:using-resource
   #:allocate-resource
   #:deallocate-resource
   #:clear-resource
   #:map-resource
   ;;
   #:*multiprocessing-p*
   #:make-process
   #:destroy-process
   #:current-process
   #:all-processes
   #:processp
   #:process-name
   #:process-state
   #:process-whostate
   #:process-wait
   #:process-wait-with-timeout
   #:process-yield
   #:process-interrupt
   #:disable-process
   #:enable-process
   #:restart-process
   #:without-scheduling
   #:atomic-incf
   #:atomic-decf
   ;;
   #:make-lock
   #:with-lock-held
   #:make-recursive-lock
   #:with-recursive-lock-held
   ;;
   #:make-condition-variable
   #:condition-wait
   #:condition-notify
   ;;
   #:defgeneric*
   #:defmethod*))

(defpackage :clim-extensions
  (:use)
  (:nicknames :clime)
  (:export
   ;; events
   #:event-read-with-timeout
   #:event-listen-or-wait
   #:schedule-event
   ;; sheets
   #:top-level-sheet-mixin
   #:unmanaged-sheet-mixin
   #:sheet-name
   #:sheet-pretty-name

   #:always-repaint-background-mixin
   #:never-repaint-background-mixin
   #:background
   #:foreground
   ;; #:line-style-effective-thickness
   ;; #:line-style-effective-dashes
   ;; medium
   #:medium-miter-limit
   #:medium-draw-circle*
   ;; panes
   #:raised-pane #:raising
   #:lowered-pane #:lowering
   #:viewport-pane
   #:device-font-text-style-p
   #:draw-image
   #:image-pane
   #:draw-label
   #:label-pane-label
   #:box-adjuster-gadget
   #:compose-space-aux
   #:simple-event-loop
   #:invoke-with-output-to-pointer-documentation
   #:with-output-to-pointer-documentation
   #:frame-display-pointer-documentation-string
   #:list-pane-items
   #:output-record-baseline
   #:merging-dead-keys

   #:draw-output-border-over
   #:draw-output-border-under
   #:make-bordered-output-record
   #:bordered-output-record

   #:draw-rounded-rectangle*

   #:highlight-output-record-tree
   #:text-selection-mixin
   #:mouse-wheel-scroll-mixin
   ;; page abstraction (seos mixin)
   #:stream-cursor-initial-position
   #:stream-cursor-final-position
   #:stream-page-region
   #:stream-text-margins
   #:stream-line-width
   #:with-temporary-margins
   #:invoke-with-temporary-page
   ;; designs and patterns
   #:pattern
   #:image-pattern
   #:rectangular-tile
   #:transformed-design
   #:transformed-pattern
   #:effective-transformed-design
   #:rectangular-tile-design
   ;; readers
   #:pattern-array
   #:transformed-design-design
   #:transformed-design-transformation
   ;; inks
   #:indirect-ink
   #:indirect-ink-p
   #:indirect-ink-ink
   #:color-rgba
   #:design-ink
   ;; Font listing extension:
   #:font-family
   #:font-face
   #:port-all-font-families
   #:font-family-name
   #:font-family-port
   #:font-family-all-faces
   #:font-face-name
   #:font-face-family
   #:font-face-all-sizes
   #:font-face-scalable-p
   #:font-face-text-style

   #:define-bitmap-file-reader
   #:unsupported-bitmap-format
   #:bitmap-format
   #:*default-vertical-scroll-bar-position*
   ;; frame manager
   #:find-frame-type
   #:note-frame-pretty-name-changed
   ;; images
   #:rgb-image
   #:xpm-parse-file
   #:*xpm-x11-colors*
   ;; selection
   #:define-selection-translator
   #:release-selection
   #:publish-selection
   #:request-selection
   ;; franz
   #:pointer-place-rubber-band-line*
   #:pointer-input-rectangle*
   #:pointer-input-rectangle))

;;; Symbols that must be defined by a backend.
;;;
;;; To start with, I grabbed the methods defined by the CLX backend
;;; whose symbol package is CLIM or CLIMI.

(defpackage :clim-backend
  (:nicknames :climb)
  (:use :clim :clim-extensions)
  (:export
   ;; CLIM-INTERNALS
   #:make-graft
   #:medium-draw-circle*
   #:mirror-transformation
   #:port-allocate-pixmap
   #:port-deallocate-pixmap
   #:port-disable-sheet
   #:port-enable-sheet
   #:port-force-output
   #:port-frame-keyboard-input-focus
   #:port-grab-pointer
   #:port-ungrab-pointer
   #:with-pointer-grabbed
   #:port-set-mirror-name
   #:port-set-mirror-region
   #:port-set-mirror-transformation
   #:queue-callback
   #:set-sheet-pointer-cursor
   #:synthesize-pointer-motion-event
   #:window-manager-focus-event
   #:with-port
   #:invoke-with-port
   #:find-concrete-pane-class
   ;; Text-style
   #:text-style-character-width
   #:text-bounding-rectangle*
   #:normalize-font-size
   #:parse-text-style*
   ;; Font abstraction
   #:font-face
   #:font-size
   #:font-character-width
   #:font-string-width
   #:font-string-glyph-codes
   #:font-glyph-code-char
   #:font-text-extents
   #:font-ascent
   #:font-descent
   #:font-leading
   #:font-tracking
   #:font-fixed-width
   #:font-kerning-p
   #:font-glyph-width
   #:font-glyph-height
   #:font-glyph-top
   #:font-glyph-left
   #:font-glyph-bottom
   #:font-glyph-right
   #:font-glyph-dx
   #:font-glyph-dy
   ;; Mixins available for backends
   #:multiline-text-medium-mixin
   #:font-rendering-medium-mixin
   #:approx-bbox-medium-mixin
   #:transform-coordinates-mixin
   ;; From CLIM (mentioned in the spec)
   #:adopt-frame
   #:allocate-space
   #:destroy-mirror
   #:destroy-port
   #:graft
   #:graftp
   #:graft-height
   #:graft-width
   #:handle-repaint
   #:make-medium
   #:make-pane-1
   #:medium-beep
   #:medium-buffering-output-p
   #:medium-clear-area
   #:medium-clipping-region
   #:medium-copy-area
   #:medium-draw-ellipse*
   #:medium-draw-line*
   #:medium-draw-lines*
   #:medium-draw-point*
   #:medium-draw-points*
   #:medium-draw-polygon*
   #:medium-draw-rectangle*
   #:medium-draw-rectangles*
   #:medium-draw-text*
   #:medium-finish-output
   #:medium-force-output
   #:medium-line-style
   #:medium-text-style
   #:note-space-requirements-changed
   #:pointer-button-state
   #:pointer-modifier-state
   #:pointer-position
   #:realize-mirror
   #:text-size
   #:text-style-ascent
   #:text-style-descent
   #:text-style-height
   #:text-style-mapping
   #:text-style-width
   ;; CLIM-EXTENSIONS
   #:medium-miter-limit
   #:medium-draw-circle*
   ;; selection
   #:release-selection
   #:publish-selection
   #:request-selection
   #:selection-object
   #:selection-object-content
   #:selection-object-type
   #:selection-object-owner
   ;; command output destinations
   #:invoke-with-standard-output
   #:register-output-destination-type
   #:output-destination
   #:stream-destination
   #:destination-stream
   #:file-destination
   #:destination-file
   #:destination-element-type))

(defpackage :clim-internals
  (:use :clim :clim-sys :clim-extensions :clim-backend :clim-lisp)
  (:nicknames :climi)
  #+excl
  (:import-from :excl compile-system load-system)
  (:import-from #:alexandria
                #:clamp
                #:make-keyword
                #:ensure-gethash
		#:last-elt
		#:with-gensyms
                #:if-let
                #:when-let)
  (:intern #:letf))

(defpackage :clim-user
  (:use :clim :clim-lisp))
