FILES = package.lisp iterate.lisp iterate-test.lisp iterate-pg.lisp iterate.asd
TEXFILES = doc/iter-man.tex doc/iter-bare.tex doc/aimemo.sty doc/GNUmakefile
RCSFILES = package.lisp,v iterate.lisp,v iterate-test.lisp,v iterate-pg.lisp,v doc/iter-man.tex,v
PDFFILES = doc/iter-man.pdf doc/iter-bare.pdf

distrib:
	tar czf iterate.tgz $(FILES) $(TEXFILES) $(PDFFILES)

devel:
	tar czf iterate-rcs.tgz $(FILES) $(RCSFILES) $(TEXFILES) Makefile
