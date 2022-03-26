### Makefile for SLIME
#
# This file is in the public domain.

# Variables
#
EMACS=emacs
LISP=sbcl

LOAD_PATH=-L .

ELFILES := slime.el slime-autoloads.el slime-tests.el $(wildcard lib/*.el)
ELCFILES := $(ELFILES:.el=.elc)

default: compile contrib-compile

all: compile

help:
	@printf "\
Main targets\n\
all        -- see compile\n\
compile    -- compile .el files\n\
check      -- run tests in batch mode\n\
clean      -- delete generated files\n\
doc-help   -- print help about doc targets\n\
help-vars  -- print info about variables\n\
help       -- print this message\n"

help-vars:
	@printf "\
Main make variables:\n\
EMACS     -- program to start Emacs ($(EMACS))\n\
LISP      -- program to start Lisp ($(LISP))\n\
SELECTOR  -- selector for ERT tests ($(SELECTOR))\n"

# Compilation
#
slime.elc: slime.el lib/hyperspec.elc

%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)

# Automated tests
#
SELECTOR=t

check: compile
	$(EMACS) -Q --batch $(LOAD_PATH)				\
		--eval "(require 'slime-tests)"				\
		--eval "(slime-setup)"					\
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval '(slime-batch-test (quote $(SELECTOR)))'

# run tests interactively
#
# FIXME: Not terribly useful until bugs in ert-run-tests-interactively
# are fixed.
test: compile
	$(EMACS) -Q -nw $(LOAD_PATH)					\
		--eval "(require 'slime-tests)"				\
		--eval "(slime-setup)"					\
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval '(slime-batch-test (quote $(SELECTOR)))'

compile-swank:
	echo '(load "swank-loader.lisp")' '(swank-loader:init :setup nil)' \
	| $(LISP)

run-swank:
	{ echo \
	'(load "swank-loader.lisp")' \
	'(swank-loader:init)' \
	'(swank:create-server)' \
	&& cat; } \
	| $(LISP)

elpa-slime:
	echo "Not implemented yet: elpa-slime target" && exit 255

elpa: elpa-slime contrib-elpa

# Cleanup
#
FASLREGEX = .*\.\(fasl\|ufasl\|sse2f\|lx32fsl\|abcl\|fas\|lib\|trace\)$$

clean-fasls:
	find . -regex '$(FASLREGEX)' -exec rm -v {} \;
	[ ! -d ~/.slime/fasl ] || rm -rf ~/.slime/fasl

clean: clean-fasls
	find . -iname '*.elc' -exec rm {} \;


# Contrib stuff. Should probably also go to contrib/
#
MAKECONTRIB=$(MAKE) -C contrib EMACS="$(EMACS)" LISP="$(LISP)"
contrib-check-% check-%:
	$(MAKECONTRIB) $(@:contrib-%=%)
contrib-elpa:
	$(MAKECONTRIB) elpa-all
contrib-compile:
	$(MAKECONTRIB) compile

# Doc
#
doc-%:
	$(MAKE) -C doc $(@:doc-%=%)
doc: doc-help

.PHONY: clean elpa compile check doc dist
