[the following works as of September 12, 2004] 

Apply the latest patches available at www.cormanlisp.com, and
recompile Corman Lisp as explained in the manual (page 23)

Install ASDF as explained at http://www.weitz.de/corman-asdf/

A few changes are required to make cl-pdf work on Corman Lisp (the
following assumes that it's installed at c:/cl-pdf)

* Patch iterate.lisp (c:/cl-pdf/iterate/iterate.lisp)

 453c453 
 <   (set-dispatch-macro-character #\# #\L 'sharpL-reader) 
 --- 
 >   (set-dispatch-macro-character #\# #\L #'sharpL-reader)
 1312c1312 
 < (eval-when (:compile-toplevel :load-toplevel) 
 --- 
 > (eval-when (:compile-toplevel :load-toplevel :execute)
 1758c1758 
 <     (coerce #\null type)) 
 --- 
 >     (coerce #\nul type))
 3435c3435 
 <   (when *old-sharpl-func* 
 --- 
 >   (when (and (boundp '*old-sharpL-func*) *old-sharpL-func*)

* Make sure corman-patches.lisp is loaded at the beginning and
  corman-patches-2.lisp is loaded at the end (modify
  c:/cl-pdf/cl-pdf.asd as follows):

 29c29,31
 <              (:file "config" :depends-on ("defpackage"))
 ---
 >              #+cormanlisp (:file "corman-patches")
 >              #+cormanlisp (:file "corman-patches-2" :depends-on ("pdf-base"))
 >              (:file "config" :depends-on ("defpackage" #+cormanlisp "corman-patches"))

* cl-pdf is ready to be used:

(load "y:/cl-pdf/cl-pdf.asd")

(asdf:oos 'asdf:load-op :cl-pdf)

* You can test the examples

(load "y:/cl-pdf/examples/examples.lisp")

(progn
 (example1 #P"c:/ex1.pdf") 
 (example2 #P"c:/ex2.pdf") 
 (example3 #P"c:/ex3.pdf") 
 (example4 #P"c:/ex4.pdf") 
 (example5 50 50 :file #P"c:/ex5.pdf") 
 (example6 #P"c:/ex6.pdf")
 (example7 #P"c:/ex7.pdf"))

There is a problem with the image in the last example. cl-pdf
generates text files, and Corman Lisp expands line feeds
(#\Newline->CR/LF) breaking binary fragments (images and compressed
parts).

If you want to enable compression or import image files, you need to
use the pdf::with-binary-files macro, that redefines temporarily the
function expand-line-feeds to avoid the expansion.

(setf pdf::*compress-streams* t)

(pdf::with-binary-files 
 (example1 #P"c:/ex1.pdf") 
 (example2 #P"c:/ex2.pdf") 
 (example3 #P"c:/ex3.pdf") 
 (example4 #P"c:/ex4.pdf") 
 (example5 50 50 :file #P"c:/ex5.pdf") 
 (example6 #P"c:/ex6.pdf")
 (example7 #P"c:/ex7.pdf"))
