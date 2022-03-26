TRIVIAL-FEATURES
================

This is a first *draft* of a description of what symbols should be
present in `CL:*FEATURES*` for various platforms.  A possible future
direction of this documentation might be a CDR document, if it turns
out to be a good idea.  (Making the language of this document much
more precise will be necessary then.)

We will start by limiting ourselves to OS, CPU and endianness features
on Windows and POSIX platforms.

There are various possible implementation strategies ranging from null
implementations (when the host Lisp already pushes the wanted feature)
to using FFI (e.g. calling uname() to grab system information.


Specification
-------------

### ENDIANNESS

Either `:LITTLE-ENDIAN` or `:BIG-ENDIAN` should present in
`\*FEATURES\*`.  For the time being, we will not concern ourselves
with other orderings, switchable endianness, etc.


### OPERATING SYSTEM

On Windows, `:WINDOWS` should be present in `*FEATURES*`.

On POSIX systems, the "sysname" information from uname(3) should be
used to push the appropriate symbol to `*FEATURES*` by upcasing that
string (or downcasing for the "modern" lisps) and interning it in the
keyword package.

Examples:

  - `:DARWIN`
  - `:LINUX`
  - `:NETBSD`
  - `:OPENBSD`
  - `:FREEBSD`

For convenience, `:UNIX` should be pushed when running on
POSIX/UNIX-like operating system (that doesn't include Windows) and
`:BSD` should be present when running on BSD-based systems (that
includes Darwin)

[add `:MACH` too?]


### CPU

These features should be mutually exclusive:

  - `:X86`
  - `:X86-64`
  - `:PPC`
  - `:PPC64`
  - `:MIPS`
  - `:ALPHA`
  - `:SPARC`
  - `:SPARC64`
  - `:HPPA`
  - `:HPPA64`

[add more ...]

[note: it's debatable whether `:X86` shouldn't also be exported on
x86-64, and `:PPC` on ppc64.  SBCL doesn't.  Other ways
to handle, for example, the x86/x86-64 case would be to export
something like `:PC386` in both cases or have an additional `:X86-32`. Or
finally, have just `:X86`, `:PPC`, etc, and add `:32-BIT-CPU` and
`:64-BIT-CPU` features.]


Unreferenced References
-----------------------

  * [CLHS: Variable \*FEATURES\*][1]
  * [Maintaining Portable Lisp Programs][2], by Christophe Rhodes


[1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_featur.htm
[2]: http://www-jcsu.jesus.cam.ac.uk/~csr21/papers/features.pdf
