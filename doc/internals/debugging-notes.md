# Some random useful code snippets

## To get a disassembly in the cross-environment:

Not actually a diassembly, it's the input to the assembler. Not a round-trip through the assembler & disassembler.

```lisp
(let ((mezzano.compiler::*trace-asm* t))
  (mezzano.compiler::compile-lambda '<lambda-to-disassemble>))
```

## Loading up the gdb tools (aarch64)

1. Start qemu with the `-s -S` options (one starts the gdb stub, the other starts stopped)
2. Start gdb
3. `target remote :1234`
4. `source tools/gdb.scm`
5. `gu (load-symbols "../mezzano.map")`
6. do other init stuff
7. `c`

Most of this can be done through options when starting gdb

### Useful gdb ops

`display /i $pc` - print current pc & instruction every step

print current pc & function & instruction every step (better than above!)
```gdb
define hook-stop
ltrace
end
```

`gu (break "BOOTLOADER-ENTRY-POINT")` - break on a symbol
`gu (where)` - print current function name
`gu (unwind)` - backtrace

## Building from the repl

```lisp
; from the MBuild/Mezzano directory
(asdf:load-system :lispos)
(asdf:load-system :lispos-file)
(file-server::spawn-file-server)
(cold-generator:set-up-cross-compiler :architecture :arm64)
(cold-generator::make-image "../../mezzano" :image-size (* 5 1024 1024 1024) :header-path "tools/disk-header.bin")
```
