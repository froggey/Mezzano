(in-package :mezzano.delimited-continuations)

(sys.int::define-lap-function %call-with-prompt ((prompt-tag thunk handler stack))
  ;; r8 = tag; r9 = thunk; r10 = handler; r11 = stack.
  (:gc :no-frame :layout #*0)
  (lap:push :rbp)
  (:gc :no-frame :layout #*00)
  (lap:mov64 :rbp :rsp)
  (:gc :frame)
  PHONY-RETURN-ADDRESS
  ;; Work with footholds inhibitied.
  (lap:gs)
  (lap:add64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; Find the top of the new stack.
  (lap:mov64 :rsp (:object :r11 0)) ; stack.base
  (lap:add64 :rsp (:object :r11 1)) ; stack.size
  (lap:sar64 :rsp #.sys.int::+n-fixnum-bits+)
  ;; Construct the delimited continuation frame on the stack.
  (lap:lea64 :rax (:rip PHONY-RETURN-ADDRESS))
  (lap:push :rax) ; Fake return address for the GC.
  (lap:push :rbp) ; Old stack pointer.
  (lap:mov64 :rbp :rsp) ; Nested frame, weird.
  (:gc :frame)
  (lap:push :r11) ; Continuation's base stack object. Important, keeps the stack live when it isn't attached to a thread or delimited continuation object. Happens during an unwind.
  (:gc :frame :layout #*1)
  (lap:gs)
  (lap:mov64 :r12 (:object nil #.mezzano.supervisor::+thread-stack+))
  (lap:push :r12) ; Thread's previous stack object.
  (:gc :frame :layout #*11)
  (lap:push :r10) ; Handler
  (:gc :frame :layout #*111)
  (lap:push :r8) ; Tag
  (:gc :frame :layout #*1111)
  (lap:gs)
  (lap:mov64 :r12 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+))
  (lap:push :r12) ; SSP link
  (lap:push #x300) ; SV header for SSP
  ;; Switch the thread over to the new stack object.
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-stack+) :r11)
  ;; Activate continuation.
  (lap:lea64 :rax (:rsp #.sys.int::+tag-object+))
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+) :rax)
  ;; Reenable footholds
  (lap:gs)
  (lap:sub64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  (lap:jnz CALL-THUNK)
  (lap:gs)
  (lap:cmp64 (:object nil #.mezzano.supervisor::+thread-pending-footholds+) nil)
  (lap:jne RUN-FOOTHOLDS-1)
  CALL-THUNK
  ;; Invoke the thunk.
  (lap:xor32 :ecx :ecx)
  (lap:mov64 :rbx :r9)
  (lap:call (:object :rbx #.sys.int::+function-entry-point+))
  (:gc :frame :layout #*1111 :multiple-values 0)
  ;; Inhibit footholds again.
  (lap:gs)
  (lap:add64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; Note: Control will end up back here if the continuation is aborted & resumed.
  ;; Pop continuation.
  (lap:mov64 :rax (:rsp 8))
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+) :rax)
  ;; Restore thread stack.
  (lap:mov64 :rbx (:rsp 32))
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-stack+) :rbx)
  ;; Ordinary return, no need to invalidate anything.
  (lap:leave)
  (:gc :frame :multiple-values 0)
  ;; Reenable footholds, back on the old stack.
  (lap:gs)
  (lap:sub64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; FIXME: Need to run pending footholds here, but that requires saving the
  ;; multiple values. They'll get run eventually, maybe.
  (lap:leave)
  (:gc :no-frame :layout #*0 :multiple-values 0)
  (lap:ret)
  RUN-FOOTHOLDS-1
  (:gc :frame :layout #*1111)
  ;; Save the thunk
  (lap:sub64 :rsp 16)
  (lap:mov64 (:rsp 8) :r9)
  (:gc :frame :layout #*1111001)
  (lap:xor32 :ecx :ecx)
  (lap:call (:named-call mezzano.supervisor::run-pending-footholds))
  (lap:mov64 :r9 (:rsp 8))
  (lap:add64 :rsp 16)
  (:gc :frame :layout #*1111)
  (lap:jmp CALL-THUNK))

(sys.int::define-lap-function %abort-to-prompt ((prompt values))
  ;; r8 = prompt; r9 = values.
  (:gc :no-frame :layout #*0)
  (lap:push :rbp)
  (:gc :no-frame :layout #*00)
  (lap:mov64 :rbp :rsp)
  (:gc :frame)
  (lap:push :r8) ; 0, prompt
  (:gc :frame :layout #*1)
  (lap:push :r9) ; 1, values
  (:gc :frame :layout #*11)
  (lap:mov32 :ecx #.(ash 4 #.sys.int::+n-fixnum-bits+))
  (lap:mov32 :r8d #.(ash #.sys.int::+object-tag-delimited-continuation+
                         #.sys.int::+n-fixnum-bits+)) ; tag
  (lap:xor32 :r9d :r9d) ; data
  (lap:mov32 :r10d #.(ash 5 #.sys.int::+n-fixnum-bits+)) ; size. entry, stack, state, sp, info
  (lap:mov32 :r11d nil) ; area
  (lap:call (:named-call mezzano.runtime::%allocate-object))
  (lap:mov64 :r9 (:stack 0)) ; r9 = prompt
  (lap:mov64 :r10 (:stack 1)) ; r10 = values
  ;; r8 = new continuation, r9 = prompt, r10 = values.
  ;; Work with footholds inhibitied.
  (lap:gs)
  (lap:add64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; Store the entry point...
  (lap:mov64 :r11 (:function %resume-delimited-continuation))
  (lap:mov64 :r11 (:object :r11 #.sys.int::+fref-function+))
  (lap:mov64 :rax (:object :r11 #.sys.int::+function-entry-point+))
  (lap:mov64 (:object :r8 #.sys.int::+function-entry-point+) :rax)
  ;; Stack, this is the current stack, not the continuation's original stack. Required to support nested continuations.
  (lap:gs)
  (lap:mov64 :r11 (:object nil #.mezzano.supervisor::+thread-stack+))
  (lap:mov64 (:object :r8 #.sys.int::+delimited-continuation-stack+) :r11)
  (lap:gs)
  (lap:mov64 :r11 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+))
  (lap:mov64 (:stack 0) :r11)
  (lap:mov64 (:object :r8 #.sys.int::+delimited-continuation-stack-pointer+) :rsp)
  (lap:mov64 (:object :r8 #.sys.int::+delimited-continuation-prompt+) :r9)
  ;; The stack & stack pointer values in the continuation have been initialized.
  ;; The GC will now scan the object, making it safe to detach the stack segments.
  ;; Prepare to return.
  ;; Restore the thread's stack & special stack pointers.
  (lap:mov64 :r11 (:object :r9 3)) ; original stack object
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-stack+) :r11)
  (lap:mov64 :r11 (:object :r9 0)) ; original ssp
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+) :r11)
  ;; Clear the binding cache, don't want entries to point to the continuation's stack.
  ;; This must be done after restoring the ssp, otherwise an interrupt may occur & repopulate the
  ;; cache with stale entries.
  (lap:xor32 :ecx :ecx)
  CLEAR-LOOP
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-symbol-cache+ :rcx) :symbol-binding-cache-sentinel)
  (lap:add64 :rcx 1)
  (lap:cmp64 :rcx #.mezzano.supervisor::+thread-symbol-cache-size+)
  (lap:jne CLEAR-LOOP)
  ;; Switch back to the original stack.
  ;; There's a small moment here where the GC can scan both the continuation and the thread
  ;; before the continuation's stack is uncoupled from the thread and end up scanning
  ;; the thread's stack twice. This isn't currently a problem, but something to keep in mind.
  (lap:mov64 :rbp (:object :r9 5))
  (:gc :frame :layout #*)
  (lap:lea64 :rsp (:rbp -16)) ; +2 stack slots, going to construct a cons on the stack.
  ;; Zap parts of the saved stack.
  ;; Clear the return address & frame pointer to delimit the end of the continuation.
  ;; Return address must be cleared first.
  (lap:mov64 (:object :r9 6) 0) ; return address
  (lap:mov64 (:object :r9 5) 0) ; frame pointer
  ;; Clear the original stack object so it doesn't hang around.
  (lap:mov64 (:object :r9 3) 0)
  ;; Fetch the handler & clear that too
  (lap:mov64 :rbx (:object :r9 2))
  (lap:mov64 (:object :r9 2) 0)
  ;; Set the tag to a value that will never be aborted to.
  (lap:mov64 :r11 (:symbol-global-cell *ignore-prompt-tag*))
  (lap:mov64 :r11 (:object :r11 #.sys.int::+symbol-value-cell-value+))
  (lap:mov64 (:object :r9 1) :r11)
  ;; Continuation is now ready for use.
  (lap:mov64 :r11 (:constant :resumable))
  (lap:mov64 (:object :r8 #.sys.int::+delimited-continuation-state+) :r11)
  ;; Now call the handler.
  ;; Construct a cons on the stack for apply. (cons continuation values)
  (lap:mov64 (:stack 1) :r8) ; continuation
  (:gc :frame :layout #*01)
  (lap:mov64 (:stack 0) :r10) ; values
  (:gc :frame :layout #*11)
  ;; Reenable footholds
  (lap:gs)
  (lap:sub64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  (lap:jnz CALL-HANDLER)
  (lap:gs)
  (lap:cmp64 (:object nil #.mezzano.supervisor::+thread-pending-footholds+) nil)
  (lap:jne RUN-FOOTHOLDS-1)
  CALL-HANDLER
  (lap:mov64 :r8 :rbx) ; first arg, handler
  (lap:lea64 :r9 (:rsp #.sys.int::+tag-cons+)) ; second arg, continuation + values
  (lap:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+))
  (lap:call (:named-call mezzano.runtime::%apply))
  (:gc :frame :multiple-values 0)
  (lap:leave)
  (:gc :no-frame :layout #*0 :multiple-values 0)
  (lap:ret)
  RUN-FOOTHOLDS-1
  (:gc :frame :layout #*11)
  ;; Save the handler
  (lap:sub64 :rsp 16)
  (lap:mov64 (:rsp 8) :rbx)
  (:gc :frame :layout #*111)
  (lap:xor32 :ecx :ecx)
  (lap:call (:named-call mezzano.supervisor::run-pending-footholds))
  (lap:mov64 :rbx (:rsp 8))
  (lap:add64 :rsp 16)
  (:gc :frame :layout #*11)
  (lap:jmp CALL-HANDLER))

(sys.int::define-lap-function %reinvoke-delimited-continuation ((continuation))
  ;; Called by CALL-WITH-PROMPT to resume a delimited continuation after claiming it and updating the handler & tag.
  ;; Fiddle with the calling convention and resume normally.
  (:gc :no-frame :layout #*0)
  (lap:mov64 :rbx :r8) ; rbx = continuation.
  (lap:xor32 :ecx :ecx) ; no arguments
  ;; Tail call to the main routine.
  (lap:jmp (:named-call %resume-delimited-continuation-1)))

(sys.int::define-lap-function %resume-delimited-continuation (())
  ;; rbx=cont; args in regs
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  ;; Claim the continuation then jump to the common code.
  (lap:mov64 :rax (:constant :resumable))
  (lap:mov64 :r13 (:constant :consumed))
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx :extra-registers :rax)
  (lap:lock lap:cmpxchg (:object :rbx #.sys.int::+delimited-continuation-state+) :r13)
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (lap:jnz ERR)
  (lap:jmp (:named-call %resume-delimited-continuation-1))
  ERR
  (lap:jmp (:named-call raise-consumed-continuation-resumed)))

(sys.int::define-lap-function %resume-delimited-continuation-1 (())
  ;; rbx=cont; args in regs; r13=scratch
  ;; Resuming a claimed delimited-continuation.
  (lap:push :rbp)
  (:gc :no-frame :layout #*00 :incoming-arguments :rcx)
  (lap:mov64 :rbp :rsp)
  (:gc :frame :incoming-arguments :rcx)
  ;; Must have NIL in R8 when returning 0 values.
  (lap:test64 :rcx :rcx)
  (lap:cmov64z :r8 (:constant nil))
  (lap:cmp64 :rcx #.(ash 5 sys.int::+n-fixnum-bits+))
  (lap:jle ONLY-REG-ARGS)
  (lap:cmp64 :rcx #.(ash #.multiple-values-limit sys.int::+n-fixnum-bits+))
  (lap:jg TOO-MANY-VALUES)
  ;; Copy stack arguments into the MV area.
  (lap:push :rcx) ; save value count into (:stack 0)
  (:gc :frame :incoming-arguments 0)
  (lap:mov32 :eax :ecx)
  (lap:mov32 :ecx #.(ash 5 sys.int::+n-fixnum-bits+))
  (lap:xor32 :edx :edx)
  (:gc :frame :multiple-values 0 :incoming-arguments 0)
  MV-UNPACK-LOOP
  (lap:mov64 :r13 (:rbp 16 (:rdx 8))) ; Load value from stack.
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-mv-slots+ :rdx) :r13)
  (:gc :frame :multiple-values 1 :incoming-arguments 0)
  (lap:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+))
  (:gc :frame :multiple-values 0 :incoming-arguments 0)
  (lap:add64 :rdx 1)
  (lap:cmp64 :rcx :rax)
  (lap:jne MV-UNPACK-LOOP)
  (:gc :frame :multiple-values 0)
  (lap:add64 :rsp 8) ; drop saved count
  ONLY-REG-ARGS
  (:gc :frame :multiple-values 0)
  ;; Work with footholds inhibitied.
  (lap:gs)
  (lap:add64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; Fetch the prompt from the continuation.
  (lap:mov64 :rax (:object :rbx #.sys.int::+delimited-continuation-prompt+))
  ;; Update the saved original stack object.
  (lap:gs)
  (lap:mov64 :r13 (:object nil #.mezzano.supervisor::+thread-stack+))
  (lap:mov64 (:object :rax 3) :r13) ; original stack object
  ;; Replace current stack object with the continuation's.
  (lap:mov64 :r13 (:object :rbx #.sys.int::+delimited-continuation-stack+))
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-stack+) :r13)
  ;; Link the continuation's stack onto the current stack.
  (lap:mov64 (:object :rax 5) :rbp) ; frame pointer
  (lap:lea64 :rdx (:rip PHONY-RETURN-ADDRESS))
  (lap:mov64 (:object :rax 6) :rdx) ; return address
  ;; Switch over to the proper stack frame.
  ;; The stack frame looks like (from rsp upwards, see %ABORT-TO-PROMPT):
  ;; +0 unused (dirty, not live)
  ;; +8 top ssp
  ;; +16 fp
  ;; +24 ra
  (lap:mov64 :rsp (:object :rbx #.sys.int::+delimited-continuation-stack-pointer+))
  (lap:lea64 :rbp (:rsp 16))
  (:gc :frame :multiple-values 0)
  ;; Link into the special stack.
  (lap:gs)
  (lap:mov64 :r13 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+))
  (lap:mov64 (:object :rax 0) :r13)
  (lap:mov64 :r13 (:stack 0)) ; saved ssp
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+) :r13)
  ;; Clear the binding cache, there may be stale entries.
  ;; This must be done after restoring the ssp, otherwise an interrupt may occur & repopulate the
  ;; cache with stale entries.
  (lap:xor32 :edx :edx)
  CLEAR-LOOP
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-symbol-cache+ :rdx) :symbol-binding-cache-sentinel)
  (lap:add64 :rdx 1)
  (lap:cmp64 :rdx #.mezzano.supervisor::+thread-symbol-cache-size+)
  (lap:jne CLEAR-LOOP)
  ;; Setup complete.
  ;; Hose the old continuation object so it doesn't keep old values live.
  (lap:mov64 (:object :rbx #.sys.int::+delimited-continuation-prompt+) 0)
  (lap:mov64 (:object :rbx #.sys.int::+delimited-continuation-stack-pointer+) 0)
  (lap:mov64 (:object :rbx #.sys.int::+delimited-continuation-stack+) 0)
  ;; Reenable footholds, on the right stack.
  (lap:gs)
  (lap:sub64 (:object nil #.mezzano.supervisor::+thread-inhibit-footholds+)
             #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; FIXME: Need to run pending footholds here, but that requires saving the
  ;; multiple values. They'll get run eventually, maybe.
  ;; Now return to the function that originally aborted.
  (lap:leave)
  (:gc :no-frame :layout #*0 :multiple-values 0)
  (lap:ret)
  TOO-MANY-VALUES
  (:gc :frame)
  PHONY-RETURN-ADDRESS ; match %CALL-WITH-PROMPT's GC info
  (lap:mov64 :r8 (:constant "Resuming continuation ~S with too many values (MULTPLE-VALUES-LIMIT exceeded)"))
  (lap:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (lap:call (:named-call error)))
