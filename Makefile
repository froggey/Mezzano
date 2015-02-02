# This GNU makefile builds the mezzano image, and creates a Virtual Box vmdk
# refering this image, when VBoxManage is present in the PATH.
# An example of alternative image creation target is given: $(OTHER).image.

all:help

HELP_FMT="%s %-30s \# %s\\n"
M=$(notdir $(MAKE))
vars:
	@echo M=$(M)
	@echo HELP_FMT=$(HELP_FMT)
help:
	@printf $(HELP_FMT) $(M) image "makes the mezzano image."
	@printf $(HELP_FMT) $(M) vmdk "makes the mezzano.vmdk file for Virtual Box."
	@printf $(HELP_FMT) $(M) launch-file-server "launches the file server in a screen."

image:mezzano.image
vmdk: mezzano.vmdk

%.vmdk:%.image
	-rm -f $@
	type -p VBoxManage >/dev/null  && VBoxManage internalcommands createrawvmdk -filename $@ -rawdisk $(abspath $<)

mezzano.image:$(shell find . -name \*.lisp -print\)
	sbcl --no-userinit --load build.lisp --eval '(sb-ext:quit)'

other:other-mezzano.vmdk
OTHER=other-mezzano
$(OTHER).image:Makefile $(shell find . -name \*.lisp -print\)
	sbcl --no-userinit --eval '(defvar *image-name* "'$(OTHER)'")' --load build.lisp --eval '(sb-ext:quit)'
# Alternative quicklisp directory: --eval '(defvar *quicklisp-directory* #P/alternative/quicklisp/)'


launch-file-server:
	screen -d -m -t mezzanofileserver  -c mezzano.screenrc \
         bash -c 'export LC_CTYPE=en_US.UTF-8 ; while sleep 2 ; do sbcl --no-userinit --load file-server.lisp  ; done'

.PHONY: help image vmdk other start-file-server
.SUFFIXES: image vmdk
