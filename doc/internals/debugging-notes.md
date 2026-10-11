# Some random useful code snippets and notes on debugging tools

## Magic button

Press `M-F11` to dump all thread stacks and some other things to the cold-stream.
This can be a little bit flaky because it's implemented at a low level.
The left meta (alt) key must be used, the right key won't work, and sometimes the
state machine can get out of sync. Tap the meta key a couple times to get it back.

## To get a disassembly in the cross-environment:

Not actually a diassembly, it's the input to the assembler. Not a round-trip through the assembler & disassembler.

```lisp
(let ((mezzano.compiler::*trace-asm* t)
      (mezzano.compiler::*load-time-value-hook* 'mezzano.internals::compile-file-load-time-value))
  (mezzano.compiler::compile-lambda '<lambda-to-disassemble>))
```

## Tracing optimizer transforms

Bind `mezzano.compiler::*report-after-optimize-passes*`

## Loading up the gdb tools (aarch64)

1. Start qemu with the `-s -S` options (one starts the gdb stub, the other starts stopped)
2. Start gdb
3. `target remote :1234`
4. `source tools/gdb.scm`
5. `gu (load-symbols "../mezzano.map")`
6. do other init stuff
7. `c`

Most of this can be done through options when starting gdb

```sh
aarch64-elf-gdb -ex "source tools/gdb.scm" -ex "gu (load-symbols \"../mezzano.map\")" -ex "target remote :1234"
```

### Useful gdb ops

`display /i $pc` - print current pc & instruction every step

print current pc & function & instruction every step (better than above!)
```gdb
define hook-stop
ltrace
end
```

also this, which dynamically looks up function names like `unwind2`
```gdb
define hook-stop
ltrace2
end
```

`gu (break "BOOTLOADER-ENTRY-POINT")` - break on a symbol
`gu (where)` - print current function name
`gu (unwind)` - backtrace
`gu (unwind2)` - backtrace, but try to extract current function name from memory instead of the map file

### Stack layout (arm64)

The fault stack has this layout after an interrupt:

```
0x200000a1ff50:	0x00000080018b3109	0x0000000000400009 ; x7 x6
0x200000a1ff60:	0x0000007fff814ec9	0x000000000040f109 ; x5 x4
0x200000a1ff70:	0x00000080018b3109	0x000000000040f109 ; x3 x2
0x200000a1ff80:	0x0000000000400009	0x000000800174b9e9 ; x1 x0
0x200000a1ff90:	0x0000000000000000	0x000000002e4f3ec0 ; x12 x11
0x200000a1ffa0:	0x00000080018b3109	0x000000000000000f ; x6 x10
0x200000a1ffb0:	0x0000000000000002	0x00000080018b3110 ; x5 x9
0x200000a1ffc0:	0x000020800070ca90	0x00000080018b3140 ; x29(fp) pc
0x200000a1ffd0:	0x000000800011dd88	0x0000000080000004 ; x30(lr) spsr
0x200000a1ffe0:	0x000020800070ca90	0x0000000000000000 ; sp fpsr/fpcr
0x200000a1fff0:	0x000000000047e5c9	0x0000000000000000 ; cpu pad
```

## Building from the repl

```lisp
; from the MBuild/Mezzano directory
(asdf:load-system :lispos)
(asdf:load-system :lispos-file)
(file-server::spawn-file-server)
(cold-generator:set-up-cross-compiler :architecture :arm64)
(cold-generator::make-image "../../mezzano" :image-size (* 5 1024 1024 1024) :header-path "tools/disk-header.bin")
```

Once the system boots up either:
```lisp
(snapshot-and-exit) ; take a snapshot and terminate the basic repl
```
or:
```lisp
(throw 'mezzano.supervisor::terminate-thread nil) ; just terminate the basic repl
```
or nothing and live with the basic repl hanging around, a snapshot is taken at the end of IPL anyway.

## Low-Level DeBugger (LLDB)

lldb (in system/lldb.lisp) is roughly equivalent to gdb in terms of how it works, in the sense
that it operates directly on threads at the assembly level, as opposed to the "normal" style of
Common Lisp debugger that operates within a thread on conditions, signals, and restarts (plus
maybe some other extra stuff, like backtraces, frame inspection, restarting and resumption, etc)

It has a small suite of building blocks for manipulating threads at a low-level. Primarily
stopping threads, single-stepping at the instruction level, and register inspection.

The only high-level tool provided here is `trace-execution`. It takes a function to call,
and steps through it printing every instruction executed. This was originally developed
for profiling CLOS dispatch. Unlike the statistical profiler, this gives an exact trace
of instructions and functions executed.

## Statistical Profiler

Lives in the `mezzano.profiler` package.

Simple operation:
```lisp
(mezzano.profiler:with-profiling (<options...>)
  <forms-to-profile...>)
```

`:thread t` to sample just the current thread or `:thread nil` for all threads.
`mezzano.supervisor::*profile-sample-during-gc*` to sample when the gc is running.

Returns a profile object which can be passed to `mezzano.profiler:save-profile`.

`(mezzano.profiler:save-profile "profile.txt" <profile> :verbosity :flame-graph)`

Drop "profile.txt" in https://www.speedscope.app/ .

## Ansi-Tests

```lisp
(in-package :cl-user)
(load (merge-pathnames "ansi-test/init.lsp" (user-homedir-pathname)) :verbose t :print t)
(rt:disable-note :nil-vectors-are-strings)
(mapc #'rt:rem-test
      '(cl-test::make-array.23 ; Exhausts memory
        cl-test::make-array.28 ; Stack overflows
        cl-test::print.cons.7 ; Missing circularity detection
        cl-test::print.vector.circle.1 ; Stack overflows
        ;; These all take ages.
        cl-test::find-all-symbols.1
        cl-test::do-all-symbols.1
        cl-test::do-all-symbols.2
        cl-test::do-all-symbols.3
        cl-test::do-all-symbols.4
        cl-test::bignum.float.compare.7
        cl-test::bignum.float.compare.8
        cl-test::rational.double-float.random.compare.1
        cl-test::rational.long-float.random.compare.1
        cl-test::print.vector.random.1
        ))
;; Since we're evaluating lots, better to avoid the compiler
(setf mezzano.internals::*eval-hook* 'mezzano.full-eval:eval-in-lexenv)
(time (regression-test:do-tests))
```

To skip a test:
```lisp
(throw 'rt::*in-test* nil)
```

## Native cross-assembly

It's possible to cross-assemble and disassemble code for a specific architecture while running on another architecture.

```lisp
(assemble-lap '((mezzano.lap.x86:nop)
                (mezzano.lap.x86:movnti64 (:rax) :r13)
                (mezzano.lap.x86:movnti32 (:rax) :edi)
                (mezzano.lap.x86:movntdq (:rbx) :xmm1)
                (mezzano.lap.x86:movntps (:rcx) :xmm2)
                (mezzano.lap.x86:movntpd (:rdx) :xmm3)
                (mezzano.lap.x86:nop))
              nil nil nil :x86-64)
(disassemble * :architecture :x86-64)
;; #<Compiled-Function 8001E73D49>:
;;       8001E73D50: 90                             (NOP)
;;       8001E73D51: 4C 0F C3 28                    (MOVNTI64 :R13 (:RAX))
;;       8001E73D55: 0F C3 38                       (MOVNTI32 :EDI (:RAX))
;;       8001E73D58: 66 0F E7 0B                    (MOVNTDQ (:RBX) :XMM1)
;;       8001E73D5C: 0F 2B 11                       (MOVNTPS (:RCX) :MM2)
;;       8001E73D5F: 66 0F 2B 1A                    (MOVNTPD (:RDX) :XMM3)
;;       8001E73D63: 90                             (NOP)
```
