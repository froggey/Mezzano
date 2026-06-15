# The ABI.

This document is written from an x86-64 perspective. Arm64 is similar, with
arm64 registers being mapped directly to x86-64 registers to ease porting.
Architecture-specific differences should be explicitly noted.

## Register usage in compiled code.

`RAX`, `RCX`, `RDX`, `RSI`, and `RDI` are all caller-save and only store immediate
or raw values. These registers were chosen because they're all used as implicit
operands in a number of x86 instructions. They are never scanned by the GC.
Exception: The `cmpxchg` and `cmpxchg16b` instruction use these registers to hold
values, so must be executed with `:extra-registers` metadata to indicate liveness.

`RSP` is the stack pointer.
`RBP` is the frame pointer, it is callee-save.

`RBX`, `R8`, `R9`, `R10`, `R11`, `R12`, and `R13` are all caller-save and are used
to store Lisp values. The GC will scan these registers.

`R14` contains the constant NIL (equivalent to arm64 x26

`R15` is not used or scanned by the GC due to historical reasons.

The direction flag (`RFLAGS.DF`) must be set to zero (forward) on function
entry and return. Other flags are caller-save.

`FS_BASE` always points to the current CPU's structure.
`GS_BASE` always points to the current thread.

x87/MMX/SSE state is caller-save and the x87/MMX state is not defined across
calls. The callee must execute EMMS before it uses x87 instructions.
AVX is not supported.
The FPU state is not implicitly preserved when an interrupt handler is called.

### Arm64 registers.

These assignments were chosen arbitrarily. A future enhancement to the port
would be to use the full register set.

| Arm64 | x86-64    | Meaning                                      |
|-------|-----------|----------------------------------------------|
| `x0`  | `r8`      | First argument (value)                       |
| `x1`  | `r9`      | Second argument (value)                      |
| `x2`  | `r10`     | Third argument (value)                       |
| `x3`  | `r11`     | Fourth argument (value)                      |
| `x4`  | `r12`     | Fifth argument (value)                       |
| `x5`  | `rcx`     | Argument count (integer)                     |
| `x6`  | `rbx`     | Closure environment (value)                  |
| `x7`  | `r13`     | Temporary (value)                            |
| `x8`  |           | Unused                                       |
| `x9`  | `rax`     | Temporary (integer)                          |
| `x10` | `rdx`     | Temporary (integer)                          |
| `x11` | `rsi`     | Temporary (integer)                          |
| `x12` | `rdi`     | Temporary (integer)                          |
| `x13` | `r14`     | Temporary, callee-save (value)               |
| `x14` | `r15`     | Temporary, callee-save (value)               |
| `x15` |           | Unused                                       |
| `x16` |           | Unused                                       |
| `x17` |           | Unused                                       |
| `x18` |           | Unused                                       |
| `x18` |           | Unused                                       |
| `x20` |           | Unused                                       |
| `x21` |           | Unused                                       |
| `x22` |           | Unused                                       |
| `x23` |           | Unused                                       |
| `x24` |           | Unused                                       |
| `x25` |           | Unused                                       |
| `x26` |           | NIL                                          |
| `x27` | `FS_BASE` | Current CPU (value-ish)                      |
| `x28` | `GS_BASE` | Current thread (value-ish)                   |
| `x29` | `rbp`     | Frame pointer (integer)                      |
| `x30` |           | Link register (integer), saved in `cs` field |

`fpsc`/`fpcr` packed together and saved in `ss` field.

`:extra-registers :rax` indicates that the value in `x9` is an interior
pointer into `x1`, used for ll/sc.

## Common calling convention.

The number of arguments is passed in `RCX` as a fixnum.
The first 5 arguments are passed in `R8`, `R9`, `R10`, `R11`, `R12`.
Remaining arguments are passed on the stack immediately above the return
address. The callee is free to modify arguments on the stack, and the caller
must remove them from the stack when the caller returns.

The stack pointer must have bit 3 set and bits 2-0 clear on entry to a function.
In other words, it must be offset 8 bytes from a 16-byte boundary.

| Stack pointer | Value          |
|---------------|----------------|
| xx8+(N-4)*8   | Argument N     |
| xx8+8         | Argument 5     |
| xx8+0         | Return address |

### arm64

Arm64 uses a link register for return addresses, and callee is responsible for
pushing the return address on the stack. The stack must always be 16-byte aligned.
On entry, the stack pointer points at the first stack argument, if any.

| Stack pointer | Value      |
|---------------|------------|
| sp+(N-5)*8    | Argument N |
| sp+0          | Argument 5 |

If the callee needs to save the link register it must be the first entry on the callee
owned part of the stack. This is effectively the same as the x86-64 layout.

### Calling convention for funcallable objects.

Funcallable objects are normal compiled functions, closures and
funcallable-instances.
The object is loaded into `RBX`, then the entry point is called.

`(call (:object :rbx +function-entry-point+))`

#### arm64

Since arm64 can only do indirect calls through registers, not memory, the
function entry point must be loaded into a scratch integer register first.
Typically `x9` is used for this, but it's not part of the ABI.

```asm
(ldr :x9 (:object :x6 +function-entry-point+))
(blr :x9)
```

### Calling convention for function references.

Function references (frefs) are used to provide a uniform representation of
function names that can be called efficiently.
They can be called directly using the `:named-call` assembly syntax.

`(call (:named-call <function-name>))`

`<function-name>` will be resolved to an fref by the assembler, similar
to `:function` syntax.

TODO: Expand on the details of the fref fast & slow paths.

#### arm64

Arm64 does not support the fref fast path, instead all calls are performed
using indirect calls.

Restrictions on the range of the `bl` instruction makes it unlikely that
the fast path will ever be supported.

Fref calls instead are done by loading the fref object into a temporary
value register (typically, but not specifically, `x7`), reading the
function out of it into the closure register `x6`, then the call proceeds
as the funcall case.

#### Function references and undefined functions.

The first word if a function reference always points to the entry point
of the undefined function trampoline. When a function is not defined, the
fref is configured for closure mode and has its function pointed at itself.
Calling an fref that is configured this way will call the undefined function
trampoline with the fref as the closure argument. It then tail-calls to
`sys.int::raise-undefined-function`, with the fref as the closure argument
and with the original arguments intact.

#### Function references and closures.

When a function reference's function is set to a closure, fref is configured
so that it loads the associated function into `RBX` and then performs an
indirect call to the entry point of the function.

### Funcallable-instances.

The entry point of a funcallable instance always points to the funcallable-
instance trampoline. This trampoline loads the appropriate function from
the funcallable instance and tails calls to it with the original arguments.
They are treated like closures by function references.

## Return convention.

The number of returned values is passed in `RCX`.
When returning zero values, `R8` is loaded with `nil` so that the caller does not
need to test for 0 values when it expects a single return value.
The first 5 return values are passed in `R8`, `R9`, `R10`, `R11`, `R12`.
Remaining return values are passed in the multiple value save area in the
thread object.

`(:object nil (+ #.+thread-mv-slots-start+ (- N 5)))`, with the `(gs)` prefix.

Note: Interrupt handlers borrow the current thread's context, and must take
care not to step on the multiple value save area. Returning 5 or fewer values
is safe.

### arm64

The multiple value save area is accessible via `x28` instead of via `gs`.

### Tail calls.

Tail calls can only be performed when 5 or fewer arguments are being passed
to the function because the caller must remove arguments from the stack
after a call. Arguments passed in registers require no cleanup.

## Non-local exits.

`block` and `tagbody` create NLX information objects on the stack. These do not
follow the normal object representation and are known by their address. They
are aligned to 16 bytes, which allows the address to be treated like a fixnum
even though it is a raw, unboxed value.

NLX information object layout:
| Offset | Meaning                     |
|--------|-----------------------------|
| +0     | Target address table        |
| +8     | Saved special stack pointer |
| +16    | Saved stack pointer         |
| +24    | Saved frame pointer         |

The target address table field points to a position-independent jump table.
There is one entry in the jump table per NLX go-tag or exactly one
for a `block`. Each entry points to a thunk that restores `RSP`/`RBP` while
maintaining correct GC metadata.

NLX invocation:

1. If the NLX was invoked by a `return-from`, then the result form is evaulated
   and any return values are saved on the stack as if by `multiple-value-prog1`.
2. `sys.int::%%unwind-to` is called to unwind the special stack to the saved
   position. If an unwind-protect cleanup function triggers another non-local
   exit, then `%%unwind-to` will never return and this non-local exit will be
   overridden.
3. If the NLX was caused by a `return-from`, then the saved values are reloaded
   using the multiple-value return convention.
4. The NLX information object is loaded into `RAX`.
5. The NLX thunk is jumped to.
6. The NLX thunk restores `RSP` and `RBP`, then jumps to the true target of the
   NLX.

The NLX thunk exists in the function containing `block`/`tagbody`, not the function
performing the NLX because the function performing the NLX does not have the
correct GC metadata available during compilation.

Mezzano currently implements `exit-extent:medium`, but this may change.

`block`/`return-from` may use a more optimized value passing convention depending
on the number of values expected at the block exit.

### CATCH and THROW.

`catch` and `throw` are implemented using `block` and `return-from`, similar to
conditions and restarts.

## The special stack.

"Stack" is slightly misleading, the special stack is a linked-list of
dynamic-extent objects treated with stack discipline. Element 0 (as accessed
by `%object-ref-t`) is the previous object in the list, or `nil` to terminate
the list. The type of element 1 (as accessed by `%object-ref-t`) determines what
kind of stack entry this is.

| Element 1 Type      | Meaning                                 |
|---------------------|-----------------------------------------|
| `symbol-value-cell` | Dynamic binding                         |
| `simple-vector`     | NLX information location                |
| `function`          | `unwind-protect` cleanup pseudo-closure |

### Dynamic bindings.

Dynamic binding is implemented using deep binding.

Each binding has an associated value cell. A value cell is a special object
containing a link to the previous special stack entry, the associated global
symbol value cell, and the value of the binding. Symbols have a global value
cell that contains the symbol's global value, this is used when there are no
value cells for that symbol on the stack. To determine the active value cell
for a symbol, the special stack is searched upwards and the most recent value
cell for that symbol found is the the active value cell. If there are no value
cells on the stack or if the symbol has been declared global, then the symbol's
global value cell is used. Value cell lookup is implemented by `symbol-value-cell`.

A cache from symbols to value cells is maintained to optimize value lookup.
The cache is a 128 element array embedded in the thread object, indexed
by `(ldb (byte 7 4) (lisp-object-address <symbol-global-value-cell>))`. Cache
entries are updated on binding/unbinding to ensure that the cache is always up
to date.

Value cell layout:
| Index | Meaning                                               |
|-------|-------------------------------------------------------|
| 0     | Link to previous special stack entry                  |
| 1     | Symbol's global value cell                            |
| 2     | This binding's value                                  |
| 3     | The original symbol (global symbol value cells only!) |

### BLOCK/TAGBODY NLX information.

This entry is used to support invalidation of blocks and go-tags during an
unwind. NLX information objects are captured like normal variables and stored
in environment vectors. These environment vectors are `simple-vector` objects.
During a `go` or `return-from`, the exit is checked for validity, if the
NLX information object stored in the environment is `nil`, then the exit
is treated as expired. Either `sys.int::raise-bad-block` or
`sys.int::raise-bad-go-tag` will be called to signal an appropriate error.
When the dynamic-extent of the `block` or `tagbody` associated with the NLX finishs,
the entry in the environment vector is invalidated by replaced the address of
the NLX with `nil`.

Element 1 of this stack entry is the environment vector containing the NLX
information object. Element 2 is the index into the vector.

This special stack entry is is popped by `(setf (svref elt-1 elt-2) nil)`,
which invalidates the exit.

This does not protect against cross-thread non-local exits, they will break
the system terribly instead.

### Unwind-protect.

`unwind-protect` cleanup forms are enclosed in a zero-argument lambda, which in
turn may close over variables in the enclosing function. This lambda is not
represented using a normal closure object, instead the environment vector and
the associated function object are stored directly in the special stack.

The cleanup function is called by loading the function into one of the value
registers, loading the environment vector into `RBX`, loading `RCX` with fixnum 0,
and then calling the function's entry point.

## GC metadata.

Every instruction in the system has a set of GC related properties associated
with it that mostly define the layout of the stack and registers before that
instruction is executed.

The complete GC information for a function is stored in a packed representation
after the function's constant pool.

### :FRAME/:NO-FRAME

Specifies if a frame-pointer-based stack frame is active.

If a frame-pointer-based stack frame is used, then the return address is available
at `(+ :rbp 8)` and the previous frame-pointer is pointed to directly by `RBP`.

Otherwise, if a frameless stack is used then the return address is available at
`(+ :rsp (* (1- (length <layout>))) 8)`.

#### arm64

As a link register is used on arm64, if `:frame nil :layout #*` (no frame-pointer
and the callee's stack frame is empty), then the return address is available in the
link register `:x30`.

### :INTERRUPT

A boolean. Indicates that the frame is an interrupt frame and contains the full
state of the machine. See the thread full state save description and
`scavenge-interrupt-stack-frame`.

Defaults to false.

### :LAYOUT

A bit-vector indicating liveness of a given stack slot.
With a frame:  `(bit layout n)` = `(livep (- :rbp (* (1+ n) 8)))`
With no frame: `(bit layout n)` = `(livep (+ :rsp (* n 8)))`

Defaults to #*.

When no frame-pointer-based frame is used, this is used to determine the total size
of the stack frame, including the return address (if any).

### :INCOMING-ARGUMENTS

This can be `nil`, `:rcx` or [0,14]. It is ignored when `nil`.
This is used to specify how many live values there are in the caller's stack frame,
above the return address.
If it is `:rcx`, then there are `(min (- <rcx> 5) 0)` live values.
If it is an integer, then the integer specifies a stack slot that contains the
total number of arguments, including register arguments. The address of the
slot is calculated using the same method as `:layout`.
`:rcx` is not valid over a function call, as `:rcx` is caller-save.

Defaults to `nil`.

### :MULTIPLE-VALUES

This can be `nil` or [0,14]. It is ignored when `nil`.
When non-`nil`, this means that the multiple-value calling convention is active
and that there `:rcx + N` values live. If there are more than 5 values, then the
GC will scan the thread's multiple value save area as required.

This is ignored unless the function has been interrupted.

TODO: The flexibility here is unnecessary. It only needs to be able to represent
`nil`, `rcx+0` or `rcx+1`.

`:rcx` holds a fixnum.

Defaults to `nil`.

### :PUSHED-VALUES

An integer, `(signed-byte 32)`. There are N additional live values on the stack,
from the current stack pointer value upwards.

Defaults to 0.

This is only valid when `:frame` is active.

### :PUSHED-VALUES-REGISTER

This must be `:rcx` or `nil`. It is ignored when `nil`.
The register it specifies contains a fixnum, the number of additional live
values on the stack. This is additive with `:pushed-values`.

Defaults to `nil`.

This is only valid when `:frame` is active and the function is not in a call.

### :BLOCK-OR-TAGBODY-THUNK

This must be `:rax` or `nil`. It is ignored when `nil`.
When present, the stack & frame pointer are taken from the NLX information
object in `:rax`, and `:rsp`/`:rbp` are ignored.

Defaults to `nil`.

### :EXTRA-REGISTERS

Specifies that additional registers contain values.

Defaults to `nil`, indicating no special behaviour.
This is only valid when the function is not in a call.

#### x86-64

Used to support the `cmpxchg` & `cmpxchg16b` instructions.

| Value          | Meaning           |
|----------------|-------------------|
| `:rax`         | Only RAX          |
| `:rax-rcx `    | RAX and RCX       |
| `:rax-rcx-rdx` | RAX, RCX, and RDX |

#### arm64

Used to support the atomic instructions.

| Value          | Meaning                                 |
|----------------|-----------------------------------------|
| `:rax`         | x9 contains an interior pointer into x1 |
| `:rax-rcx `    | Invalid                                 |
| `:rax-rcx-rdx` | Invalid                                 |

### :RESTART

Specifies that the function must be restarted when a GC occurs.
When true, the program counter will be reet to the start of the
metadata region by the GC.

Defaults to `nil`.
This is only valid when the function is not in a call.

## Dynamic extent.

The GC supports allocating some kinds of objects on the stack.

To prevent infinite loops when scavenging, the GC will ignore almost all objects
allocated on the stack. Dynamic-extent objects have no mark bits, so there's no
way to tell if such an object has already been scanned. This is generally not an
issue as most objects with dynamic-extent have a fixed size and can be allocated
directly within a function's frame. The contents of these objects will be scavenged
as part of the GC's normal stack frame scavenging.

This only works when the object is fixed-size and allocated within the frame.
DX `&rest` lists do not fit this pattern (or any allocation with unknown size).
Instead the list is allocated on the stack, but outside the fixed-layout frame,
and a special object pointer to it is constructed. The pointer is tagged with
`+tag-dx-root-object+`. The GC detects  this while scavenging stack frames,
and will then explicitly scan the pointed-to object.

The implication of this is that most objects allocated with dynamic-extent are
"static" (in the SSA sense). In other words, each dynamic invocation of a
specifc DX allocation call will return the same (`eq`) object each time. It will
not grow the stack pointer and allocate more stack for the object.

```lisp
;; FOO and BAR are bound to different object, but the objects are the same
;; each iteration of the loop
;; (eq foo-n foo-m) => true
;; (eq bar-n bar-m) => true
;; (eq foo bar) => false
(loop
  (let ((foo (vector x y z))
        (bar (vector a b c)))
    (declare (dynamic-extent foo bar))
    (do-something foo)))
```

`&rest` lists with dynamic extent have a further issue, in that their size is
not obviously known. The GC cannot just `cdr` down the list scanning each element
because scavenge rules cause DX pointers to be ignored. It would stop after scanning
the first cons of the list. The compiler cannot generate a dx-root object for each cons
either, because the number of conses is not known statically at compile time. To solve
this, the compiler instead allocates a simple-vector on the stack large enough
to contain the entire list (properly aligned). As conses have no header, they can
exist directly within this simple vector. The list is contained entirely within
the simple-vector, and exactly one dx-root object is needed for it.

## Saved state for threads.

When switching threads, the machine state must be somehow saved in the thread object.
There are two ways this happens, depending on why the thread switch occurs.

### Partial save/voluntary switch.

If the thread switch occurs voluntarily (usually due to blocking on a sync object
or similar), then only a small amount of thread state needs to be saved. The majority
of the machine state is caller-save and a voluntary switch is effectively a function
call.

#### x86-64

Only the stack & frame pointers are saved in the thread state. `RIP` is saved on the
stack, with the stack pointer pointing at it (as if after a call). There are no other
values on the stack.

#### arm64

Only the stack & frame pointers are saved in the thread state. However the frame pointer
is saved redundantly, it is also saved on the stack along with the return address/link
register. The two copies should be identical, however the stack copy takes priority.
`SP+0` points to the saved frame pointer, `SP+8` points to the saved return address.
There are no other values on the stack. Saving two values preserves the alignment of
the stack.

### Full save/involuntary switch.

If the thread switch occurs involuntarily (usually due to an interrupt), then the entire
state of the machine needs to be saved. Nothing out of the ordinary happens here.
The whole general register state is saved into the appropriate slots in the
thread structure, the multiple-value area may or may not contain live values depending
on the interrupted function, and the fpu state is saved.

### Interrupt frame.

The thread object only has space for a single complete state, there are occasions where
it's necessary to save the full state on the stack in a frame called an "interrupt frame".
So-called this because it came about when interrupts would save the state on the thread
stack. This no longer happens, but the name stuck around for a similar concept.

This only happens in when a thread is interrupted and forced to run another function,
such as via `establish-thread-foothold` or `pager-invoke`.

This frame has a fixed size, it's large enough to hold the general register file,
the floating point registers, and the multiple-value area.

FP points to the interior of the saved state, specifically pointing at the offset of
the saved FP register within the state (effectively forming a normal frame pointer
chain), PC points to a function that's set up to restore the state, and has gc
metadata that indicates that the frame is an interrupt frame.

| Offset        | Contents                                                      |
|---------------|---------------------------------------------------------------|
| `sp+0`        | Interrupt save area. Holds GPRs matching the interrupt layout |
| `sp+20*8`     | FPU state                                                     |
| `sp+20*8+512` | MV area                                                       |

#### x86-64

A normal interrupt frame will have metadata of `(:gc :frame :interrupt t)`, however
x86-64 supports an additional mode of `(:gc :no-frame :interrupt t)`, which indicates
that sp is pointing directly at the `iret` frame part of the state and is ready
to return.
