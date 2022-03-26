# Introduction #

When dealing with network protocols and file formats, it's common to
have to read or write 16-, 32-, or 64-bit datatypes in signed or
unsigned flavors.  Common Lisp sort of supports this by specifying
`:element-type` for streams, but that facility is underspecified and
there's nothing similar for read/write from octet vectors.  What most
people wind up doing is rolling their own small facility for their
particular needs and calling it a day.

This library attempts to be comprehensive and centralize such
facilities.  Functions to read 16-, 32-, and 64-bit quantities from
octet vectors in signed or unsigned flavors are provided; these
functions are also `SETF`able.  Since it's sometimes desirable to
read/write directly from streams, functions for doing so are also
provided.  On some implementations, reading/writing IEEE singles/doubles
(i.e. `single-float` and `double-float`) will also be supported.

In addition to centralizing such facilities, NIBBLES also aspires to
become a place where compiler optimizations can be written once and used
everywhere.  The intention is that (eventually):

``` common-lisp
(nibbles:sb32ref/le vector index)
```

will compile (with any necessary safety checks) to a `MOVSX`
instruction on an x86oid processor in SBCL (or other implementations)
if `vector` and `index` are of appropriate types.

I remember reading a post on comp.lang.lisp that suggested the designers
of Common Lisp ignored the realities of octets and endianness and so
forth.  This library is a small step towards remedying that deficiency.
