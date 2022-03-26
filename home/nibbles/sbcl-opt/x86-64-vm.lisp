;;;; x86-64-vm.lisp -- VOP definitions SBCL

#+sbcl
(cl:in-package :sb-vm)

#+(and sbcl x86-64) (progn

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
      (inst lea temp (make-ea :qword
                              :index index :disp (fixnumize offset)))
      (inst cmp temp bound)
      (inst jmp :a error)
      (move result index))))

#.(flet ((frob (bitsize setterp signedp big-endian-p)
           (let* ((name (funcall (if setterp
                                     #'nibbles::byte-set-fun-name
                                     #'nibbles::byte-ref-fun-name)
                                 bitsize signedp big-endian-p))
                  (internal-name (nibbles::internalify name))
                  (operand-size (ecase bitsize
                                  (16 :word)
                                  (32 :dword)
                                  (64 :qword)))
                  (ref-mov-insn (ecase bitsize
                                  (16
                                   (if big-endian-p
                                       'movzx
                                       (if signedp 'movsx 'movzx)))
                                  (32
                                   (if big-endian-p
                                       'mov
                                       (if signedp 'movsxd 'movzxd)))
                                   (64 'mov)))
                  (result-sc (if signedp 'signed-reg 'unsigned-reg))
                  (result-type (if signedp 'signed-num 'unsigned-num)))
             (flet ((movx (insn dest source source-size)
                      (if (and (find-package "SB-X86-64-ASM")
                               (not (find-symbol "MOVZXD" "SB-X86-64-ASM")))
                          ;; new assembler
                          (if (eq insn 'mov)
                              `(inst ,insn ,dest ,source)
                              `(inst ,(case insn (movsxd 'movsx) (movzxd 'movzx) (t insn))
                                     '(,source-size :qword) ,dest ,source))
                          `(inst ,insn ,dest ,source)))
                    (swap-tn-inst-form (tn-name)
                      (if (= bitsize 16)
                          `(inst rol ,tn-name 8)
                          `(inst bswap ,tn-name))))
               `(define-vop (,name)
                  (:translate ,internal-name)
                  (:policy :fast-safe)
                  (:args (vector :scs (descriptor-reg))
                         (index :scs (immediate unsigned-reg))
                         ,@(when setterp
                             `((value* :scs (,result-sc) :target result))))
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
                           (result-in-size (reg-in-size result ,operand-size))
                           ,@(when setterp
                               `((value (reg-in-size value* ,operand-size))))
                           ,@(when (and setterp big-endian-p)
                               `((temp (reg-in-size temp ,operand-size))))
                           (memref (sc-case index
                                     (immediate
                                      (make-ea ,operand-size :base vector
                                                            :disp (+ (tn-value index) base-disp)))
                                     (t
                                      (make-ea ,operand-size
                                               :base vector :index index
                                               :disp base-disp)))))
                      (declare (ignorable result-in-size))
                      ,@(when (and setterp big-endian-p)
                          `((inst mov temp value)
                            ,(swap-tn-inst-form 'temp)))
                      ,(if setterp
                           `(inst mov memref ,(if big-endian-p
                                                  'temp
                                                  'value))
                           (movx ref-mov-insn
                                 (if (and big-endian-p (= bitsize 32))
                                       'result-in-size
                                       'result)
                                 'memref operand-size))
                      ,@(if setterp
                            '((move result value*))
                            (when big-endian-p
                              `(,(swap-tn-inst-form (if (/= bitsize 64)
                                                        'result-in-size
                                                        'result))
                                ,(when (and (/= bitsize 64) signedp)
                                   (movx 'movsx 'result 'result-in-size
                                         operand-size))))))))))))
    (loop for i from 0 upto #b10111
          for bitsize = (ecase (ldb (byte 2 3) i)
                          (0 16)
                          (1 32)
                          (2 64))
          for setterp = (logbitp 2 i)
          for signedp = (logbitp 1 i)
          for big-endian-p = (logbitp 0 i)
          collect (frob bitsize setterp signedp big-endian-p) into forms
          finally (return `(progn ,@forms))))

);#+(and sbcl x86-64)
