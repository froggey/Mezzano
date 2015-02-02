# This GNU makefile builds the mezzano image, and creates a Virtual Box vmdk
# refering this image, when VBoxManage is present in the PATH.
# An example of alternative image creation target is given: $(OTHER).image.

all:mezzano.vmdk
image:mezzano.image

%.vmdk:%.image
	-rm -f $@
	type -p VBoxManage 2>/dev/null  && VBoxManage internalcommands createrawvmdk -filename $@ -rawdisk $(abspath $<)

mezzano.image:Makefile $(shell find . -name \*.lisp -print\)
	sbcl --no-userinit --load build.lisp --eval '(sb-ext:quit)'

other:other-mezzano.vmdk
OTHER=other-mezzano
$(OTHER).image:Makefile $(shell find . -name \*.lisp -print\)
	sbcl --no-userinit --eval '(defvar *image-name* "'$(OTHER)'")' --load build.lisp --eval '(sb-ext:quit)'
# Alternative quicklisp directory: --eval '(defvar *quicklisp-directory* #P"/alternative/quicklisp/")'

.PHONY:image other
