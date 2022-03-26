[![Build Status](https://travis-ci.org/pmai/Deflate.svg?branch=master)](https://travis-ci.org/pmai/Deflate)

This library is an implementation of Deflate ([RFC 1951][]) decompression,
with optional support for ZLIB-style ([RFC 1950][]) and gzip-style
([RFC 1952][]) wrappers of deflate streams.  It currently does not handle
compression, although this is a natural extension.

The implementation should be portable across all ANSI compliant CL
implementations, but has been optimized mostly for SBCL and CMU CL
(and other implementations that can generate fast code for word-sized
integer calculations based on standard type declarations), and
somewhat (mostly the otherwise very expensive CRC-32 calculations) for
Lispworks.  The performance is still a bit off from zlib/gzip (by a
factor of around 3-3.5 on my systems), and while much of the
performance loss is likely to be in the stream-based I/O, a less naive
implementation of the huffman decoding step is also likely to benefit
performance a bit.

The implementation is licensed under the MIT-style license contained
in the file COPYING and the header of each source file.

Please direct any feedback to pmai@pmsf.de.  A git repository of this
library is available under http://github.com/pmai/Deflate/tree/master

[RFC 1951]: https://tools.ietf.org/html/rfc1951
[RFC 1950]: https://tools.ietf.org/html/rfc1950
[RFC 1952]: https://tools.ietf.org/html/rfc1952
