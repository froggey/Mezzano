uiop := uiop
ifeq (${user},)
userat :=
else
userat := ${user}@
endif
website:=${userat}common-lisp.net:/project/asdf/public_html/
output = ${uiop}.html ${uiop}.pdf
webfiles = ${output}

.PHONY: clean html pdf include clean-include clean-crap info doc texinfo website

all: pdf html info clean-crap

website: all
	rsync -lt --no-g ${webfiles} ${website}

clean-include:
	rm -rf include

clean-crap:
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr *.vrs

clean: clean-include
	rm -rf  *.pdf *.html *.info ${uiop}.texinfo ${uiop} *.fasl

fasl: docstrings.lisp
	sbcl --no-userinit --eval '(require :asdf)' \
	--eval '(compile-file "docstrings.lisp")' \
	--eval '(quit)'

include: fasl
	sbcl --no-userinit --eval '(require :asdf)' \
	--load docstrings \
	--eval '(sb-texinfo:generate-includes "include/" (list :UIOP/PACKAGE :UIOP/COMMON-LISP :UIOP/UTILITY :UIOP/VERSION :UIOP/OS :UIOP/PATHNAME :UIOP/FILESYSTEM :UIOP/STREAM :UIOP/IMAGE :UIOP/LISP-BUILD :UIOP/LAUNCH-PROGRAM :UIOP/RUN-PROGRAM :UIOP/CONFIGURATION :UIOP/BACKWARD-DRIVER))' \
	--eval '(quit)'

texinfo : include
	sbcl --no-userinit --eval '(require :asdf)' \
	--load docstrings \
	--eval "(sb-texinfo:complete-texinfo-template \"template.texinfo\" \"${uiop}.texinfo\" \"include/\")" \
	--eval '(quit)'

pdf: texinfo
	texi2pdf ${uiop}.texinfo

html: texinfo
	makeinfo --html --no-split ${uiop}.texinfo

htmls: texinfo
	makeinfo --html ${uiop}.texinfo

info: texinfo
	makeinfo ${uiop}.texinfo
