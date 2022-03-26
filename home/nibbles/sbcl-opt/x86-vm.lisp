;;;; x86-vm.lisp -- VOP definitions for SBCL

#+sbcl
(cl:in-package :sb-vm)

#+(and sbcl x86) (progn

(define-vop (%check-bound)
  (:translate nibbles::%check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (bound :scs (any-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum tagged-num
              (:constant (member 2 4 8 16)))
  (:info offset)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:generator 5
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index)))
      ;; We want to check the conditions:
      ;;
      ;; 0 <= INDEX
      ;; INDEX < BOUND
      ;; 0 <= INDEX + OFFSET
      ;; (INDEX + OFFSET) < BOUND
      ;;
      ;; We can do this naively with two unsigned checks:
      ;;
      ;; INDEX <_u BOUND
      ;; INDEX + OFFSET <_u BOUND
      ;;
      ;; If INDEX + OFFSET <_u BOUND, though, INDEX must be less than
      ;; BOUND.  We *do* need to check for 0 <= INDEX, but that has
      ;; already been assured by higher-level machinery.
      (inst lea temp (make-ea :dword :index index :disp (fixnumize offset)))
      (inst cmp temp bound)
      (inst jmp :a error)
      (move result index))))

#.(flet ((frob (setterp signedp big-endian-p)
           (let* ((name (funcall (if setterp
                                     #'nibbles::byte-set-fun-name
                                     #'nibbles::byte-ref-fun-name)
                                 16 signedp big-endian-p))
                  (internal-name (nibbles::internalify name))
                  (result-sc (if signedp 'signed-reg 'unsigned-reg))
                  (result-type (if signedp 'signed-num 'unsigned-num)))
             `(define-vop (,name)
                (:translate ,internal-name)
                (:policy :fast-safe)
                (:args (vector :scs (descriptor-reg))
                       (index :scs (immediate unsigned-reg))
                       ,@(when setterp
                           `((value :scs (,result-sc) :target result))))
                (:arg-types simple-array-unsigned-byte-8
                            positive-fixnum
                            ,@(when setterp
                                `(,result-type)))
                ,@(when (or setterp big-endian-p)
                    `((:temporary (:sc unsigned-reg :offset eax-offset
                                       :from ,(if setterp
						  '(:load 0)
						  '(:argument 2))
                                       :to (:result 0)) eax)))
                (:results (result :scs (,result-sc)))
                (:result-types ,result-type)
                (:generator 3
                  (let* ((base-disp (- (* vector-data-offset n-word-bytes)
                                       other-pointer-lowtag))
                         (memref (sc-case index
                                   (immediate
                                    (make-ea :word :base vector
                                             :disp (+ (tn-value index) base-disp)))
                                   (t
                                    (make-ea :word :base vector
                                             :index index
                                             :disp base-disp)))))
                    ,(when setterp
                       '(move eax value))
                    ,(when (and setterp big-endian-p)
                       '(inst rol ax-tn 8))
                    ,(if setterp
                         '(inst mov memref ax-tn)
                         `(inst ,(if big-endian-p
                                     'mov
                                     (if signedp
                                         'movsx
                                         'movzx))
                                ,(if big-endian-p
                                     'ax-tn
                                     'result)
                                memref))
                    ,@(if setterp
                          '((move result value))
                          (when big-endian-p
                            `(eax       ; hack so that it looks used
                              (inst rol ax-tn 8)
                              (inst ,(if signedp 'movsx 'movzx)
                                    result ax-tn))))))))))
    (loop for i from 0 upto #b111
          for setterp = (logbitp 2 i)
          for signedp = (logbitp 1 i)
          for big-endian-p = (logbitp 0 i)
          collect (frob setterp signedp big-endian-p) into forms
          finally (return `(progn ,@forms))))

#.(flet ((frob (setterp signedp big-endian-p)
           (let* ((name (funcall (if setterp
                                     #'nibbles::byte-set-fun-name
                                     #'nibbles::byte-ref-fun-name)
                                 32 signedp big-endian-p))
                  (internal-name (nibbles::internalify name))
                  (result-sc (if signedp 'signed-reg 'unsigned-reg))
                  (result-type (if signedp 'signed-num 'unsigned-num)))
             `(define-vop (,name)
                (:translate ,internal-name)
                (:policy :fast-safe)
                (:args (vector :scs (descriptor-reg))
                       (index :scs (immediate unsigned-reg))
                       ,@(when setterp
                           `((value :scs (,result-sc) :target result))))
                (:arg-types simple-array-unsigned-byte-8
                            positive-fixnum
                            ,@(when setterp
                                `(,result-type)))
                ,@(when (and setterp big-endian-p)
                    `((:temporary (:sc unsigned-reg
                                       :from (:load 0)
                                       :to (:result 0)) temp)))
                (:results (result :scs (,result-sc)))
                (:result-types ,result-type)
                (:generator 3
                  (let* ((base-disp (- (* vector-data-offset n-word-bytes)
                                       other-pointer-lowtag))
                         (memref (sc-case index
                                   (immediate
                                    (make-ea :dword :base vector
                                             :disp (+ (tn-value index) base-disp)))
                                   (t
                                    (make-ea :dword :base vector :index index
                                             :disp base-disp)))))
                    ,@(when (and setterp big-endian-p)
                        `((inst mov temp value)
                          (inst bswap temp)))
                    ,(if setterp
                         `(inst mov memref ,(if big-endian-p
                                                'temp
                                                'value))
                         '(inst mov result memref))
                    ,(if setterp
                         '(move result value)
                         (when big-endian-p
                           '(inst bswap result)))))))))
    (loop for i from 0 upto #b111
          for setterp = (logbitp 2 i)
          for signedp = (logbitp 1 i)
          for big-endian-p = (logbitp 0 i)
          collect (frob setterp signedp big-endian-p) into forms
          finally (return `(progn ,@forms))))

);#+(and sbcl x86)
