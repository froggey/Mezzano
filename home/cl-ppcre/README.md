# CL-PPCRE - Portable Perl-compatible regular expressions for Common Lisp

## Abstract

CL-PPCRE is a portable regular expression library for Common Lisp
which has the following features:

* It is **compatible with Perl** (especially when used in conjunction
  with [cl-interpol](http://weitz.de/cl-interpol/), to allow
  compatible parsing of regexp strings).
* It is pretty **fast**.
* It is **portable** between ANSI-compliant Common Lisp
  implementations.
* It is **thread-safe**.
* In addition to specifying regular expressions as strings like in
  Perl you can also use **S-expressions**.
* It comes with a
  **[BSD-style license](http://www.opensource.org/licenses/bsd-license.php)**
  so you can basically do with it whatever you want.

CL-PPCRE has been used successfully in various applications like
[BioBike](http://nostoc.stanford.edu/Docs/),
[clutu](http://clutu.com/),
[LoGS](http://www.hpc.unm.edu/~download/LoGS/),
[CafeSpot](http://cafespot.net/),
[Eboy](http://www.eboy.com/), or
[The Regex Coach](http://weitz.de/regex-coach/).

Further documentation can be found in `docs/index.html`, or on
[the cl-ppcre homepage](https://edicl.github.io/cl-ppcre/).
