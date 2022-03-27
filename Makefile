# This must be the SBCL executable to use
SBCL := sbcl
# This must be the IP address of the file-server.
# Typically this is the local IP address of the machine running the
# file-server, not the public IP.
# Note! Addresses on 10.0.2/24 networks are not supported, as this conflicts
# with the network provided by qemu and VirtualBox.
FILE_SERVER_IP := 192.168.1.24

# Report an error if this hasn't been configured.
ifeq ($(FILE_SERVER_IP),192.168.0.555)
# Unless no target was specified.
ifneq ($(MAKECMDGOALS),)
$(error FILE_SERVER_IP has not been configured in the Makefile)
endif
endif

all:
	@echo "Quick start:"
	@echo " 0. Set SBCL path and FILE_SERVER_IP in Makefile."
	@echo "    Run git submodule update --init"
	@echo " 1. Run make cold-image-vmdk"
	@echo " 2. In a seperate terminal, run make run-file-server"
	@echo "    The file server needs to run while the VM is running."
	@echo " 3. Point VirtualBox at mezzano.vmdk and start the VM."

cold-image: build-cold-image.lisp asdf
	@echo File server address: $(FILE_SERVER_IP)
	@echo Source path: $(CURDIR)/Mezzano/
	@echo Home directory path: $(CURDIR)/home/
	echo "(in-package :mezzano.internals)" > config.lisp
	echo "(defparameter *file-server-host-ip* \"$(FILE_SERVER_IP)\")" >> config.lisp
	echo "(defparameter *home-directory-path* \"REMOTE:$(CURDIR)/home/\")" >> config.lisp
	echo "(defparameter *mezzano-source-path* \"REMOTE:$(CURDIR)/\")" >> config.lisp
	$(SBCL) --dynamic-space-size 2048 --load build-cold-image.lisp

cold-image-vmdk: cold-image
	$(eval VM_NAME = $(shell VBoxManage showmediuminfo mezzano.vmdk |awk '/^In use by VMs:/{print $$5}'))
	$(eval VM_UUID = $(shell VBoxManage showmediuminfo mezzano.vmdk |awk -F ': |[()]' '/^In use by VMs: .* \(UUID:(.*)\)/{print $$4}'))
	$(eval DISK_UUID = $(shell VBoxManage showmediuminfo mezzano.vmdk |awk '/^UUID:/{print $$2}'))
	@echo "VM_NAME: $(VM_NAME)"
	@echo "VM_UUID: $(VM_UUID)"
	@echo "DISK_UUID: $(DISK_UUID)"
	-VBoxManage storagectl "$(VM_NAME)" --name IDE --remove
	-VBoxManage closemedium disk mezzano.vmdk
	rm -f mezzano.vmdk
	VBoxManage convertfromraw --format vmdk mezzano.image mezzano.vmdk
ifneq ($(VM_NAME),)
# This fails when the image isn't attached to any VM and there's nothing to update.
	@echo "*** Failures from VBoxManage are harmless and can be ignored. ***"
	VBoxManage storagectl "$(VM_NAME)" --name IDE --add ide --controller PIIX4
	VBoxManage storageattach "$(VM_NAME)" --storagectl IDE --port 0 --device 0 --type hdd --medium mezzano.vmdk
endif

run-file-server:
	cd file-server/ && $(SBCL) --load ../run-file-server.lisp

asdf:
	make -C home/asdf

qemu:
	qemu-system-x86_64 -m 2G -hda mezzano.image -serial stdio -vga std -net user,hostfwd=tcp:127.0.0.1:4005-:4005 -net nic,model=virtio
kvm:
	qemu-system-x86_64 -m 2G -hda mezzano.image -serial stdio -vga std -net user,hostfwd=tcp:127.0.0.1:4005-:4005 -net nic,model=virtio -enable-kvm
qemu-arm64:
	qemu-system-aarch64 -machine virt -cpu cortex-a53 -m 2G -kernel Mezzano/tools/kboot/kboot-generic-arm64.bin -serial stdio -device virtio-gpu-device -device virtio-keyboard-device -device virtio-mouse-device -drive if=none,file=Mezzano/build-arm64/mezzano.image,id=blk,format=raw -device virtio-blk-device,drive=blk -netdev user,id=vmnic,hostname=qemu -device virtio-net-device,netdev=vmnic

clean:
	rm -rf home/.cache/common-lisp/ home/.slime/ home/asdf/build/
	find . -name '*.llf' -type f -exec rm {} +
	rm -rf mezzano.image mezzano.map mezzano.vmdk mezzano.symbol-table

.PHONY: run-file-server cold-image cold-image-vmdk qemu kvm clean all asdf
