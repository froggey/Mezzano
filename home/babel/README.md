[![Build Status](https://travis-ci.org/cl-babel/babel.svg?branch=master)](https://travis-ci.org/cl-babel/babel)

Babel is a charset encoding/decoding library, not unlike GNU libiconv,
but completely written in Common Lisp.

It strives to achieve decent performance.  To that effect, we use
OpenMCL's approach of calculating the destination buffer size in
advance.  Most of the encoding/decoding algorithms have been adapted
from OpenMCL's source.

Another important goal is reusability.  Similarly to SBCL, we define
an interface wherein the algorithms can be reused between a variety of
data types so long we're dealing with conversions between octets and
unicode code points.

Babel comes with converters between strings and (unsigned-byte 8)
vectors but can be easily extended to deal with, e.g., strings and
foreign memory, vectors and Closure's runes, etc...

