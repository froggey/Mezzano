cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details

WARNING: cl-pdf is moving to https://github.com/mbattyani/cl-pdf.git

CL-PDF is a cross-platform Common Lisp library for generating PDF files.
It does not need any third-party tools from Adobe or others.
When it is used with cl-typesetting it provides a complete typesetting system.

CL-PDF is released with a FreeBSD style license so it is usable for commercial work.

Currently there is no docs, only some examples.
There are mailing lists for discussing cl-pdf:
http://common-lisp.net/mailman/listinfo/cl-pdf-devel
http://common-lisp.net/mailman/listinfo/cl-pdf-announce

To install it:
   1 Customize config.lisp
   2 Choose the zlib implementation to use in cl-pdf.asd
     Or get an implementation specific zlib in the contrib directory.
     Or disable the compression in config.lisp
   3 Load the cl-pdf library using the asdf or mk:defsystem files.
   4 Use it...

In case of problems, first disable the zlib compression and try again...
You need to get a post 23-dec-2002 UFFI version if you use the UFFI zlib binding.

Contributions ar welcome!
For questions, comments, bugs, typos, etc. use the mailing list

You can look at some CL-PDF examples here:
http://www.fractalconcept.com/asp/html/e-cl-pdf.html


Marc Battyani
