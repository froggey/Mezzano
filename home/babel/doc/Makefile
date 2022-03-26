# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-

MANUAL := "babel"
SYSTEM := "babel"
PACKAGES := babel babel-encodings
TITLE := "Babel Manual"
CSS := "default"

export LISP ?= sbcl
export SBCL_OPTIONS ?= --noinform

.PHONY: all clean html pdf upload

all:
	texinfo-docstrings all $(SYSTEM) $(MANUAL) $(TITLE) $(CSS) $(PACKAGES)

pdf:
	texinfo-docstrings pdf $(SYSTEM) $(MANUAL) $(TITLE) $(CSS) $(PACKAGES)

html:
	texinfo-docstrings html $(SYSTEM) $(MANUAL) $(TITLE) $(CSS) $(PACKAGES)

upload:
#	rsync -av --delete -e ssh manual common-lisp.net:/project/FOO/public_html/
#	scp -r manual common-lisp.net:/project/cffi/public_html/

clean:
	find . \( -name "*.pdf" -o -name "*.html" -o -name "*.info" -o -name "*.aux" -o -name "*.cp" -o -name "*.fn" -o -name "*.fns" -o -name "*.ky" -o -name "*.log" -o -name "*.pg" -o -name "*.toc" -o -name "*.tp" -o -name "*.vr" -o -name "*.dvi" -o -name "*.cps" -o -name "*.vrs" \) -exec rm {} \;
	rm -rf include manual

# vim: ft=make ts=4 noet
