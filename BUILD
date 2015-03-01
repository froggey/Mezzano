Prerequisites
-------------

SBCL 1.2.4 with the default external format set to UTF-8. Newer versions
should work, but have not been tested.
(setf sb-impl::*default-external-format* :utf-8)

Required systems, available via Quicklisp:
Alexandria
Iterate
Nibbles
CL-PPCRE
IOLIB (may require libfixposix)
CL-FAD

Home Directory
--------------

The stock ipl.lisp configuration expects a home directory containing all the
required libraries and fonts.

Inside the Mezzano source directory:
mkdir home
mkdir home/fonts

Download the DejaVu Sans and Mono fonts from http://dejavu-fonts.org/ and place the .ttf files in the fonts directory.

Download supporting libraries and extract them in home/
ASDF from https://github.com/froggey/asdf
trivial-features from https://github.com/froggey/trivial-features
zpb-ttf from https://github.com/froggey/zpb-ttf
cl-jpeg from https://github.com/froggey/cl-jpeg
chipz from https://github.com/froggey/chipz
alexandria from git://common-lisp.net/projects/alexandria/alexandria.git
   Revision 0c39310e is known to work
babel from https://github.com/cl-babel/babel
   Revision a994dec2 is known to work
iterate from http://common-lisp.net/project/iterate/releases/iterate-1.4.3.tar.gz
cl-vectors from http://projects.tuxee.net/cl-vectors/files/cl-vectors-0.1.5.tar.gz
png-read from https://github.com/Ramarren/png-read
   Revision dfdfc9a1 is known to work

Create an ASDF config file in home/.config/common-lisp/source-registry.conf containing:
(:source-registry
 (:tree "/full/path/to/home/")
 :inherit-configuration)

Build Instructions
------------------

Tested under SBCL 1.2.4

Load the remote filesystem server.
(ql:quickload :lispos-file)
(file-server::spawn-file-server)

Load the cross build environment.
(ql:quickload :lispos)

Initialize the empty cross environment.
(with-compilation-unit ()
  (sys.c::set-up-cross-compiler)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*supervisor-source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*warm-source-files*))

Modifiy ipl.lisp to taste.
You must change the IP address of the :remote host, and the pathnames used for
*DEFAULT-PATHNAME-DEFAULTS* and MEZZANO.FILE-SYSTEM::*HOME-DIRECTORY*.
Set *DEFAULT-PATHNAME-DEFAULTS* to the full path to the Mezzano source directory
and set MEZZANO.FILE-SYSTEM::*HOME-DIRECTORY* to the full path to the home
directory created above.
The desktop image can be disabled by removing the :IMAGE argument from line 122
and by removing the copy-file call on line 87.

Build a cold image.
(cold-generator::make-image "mezzano" :header-path "tools/disk_header")

This will produce a raw disk image called mezzano.image in the current directory.
The disk image can be run directly in qemu:
  qemu-system-x86_64 -hda mezzano.image -m 512 -vga std -serial stdio -net user -net nic,model=virtio
or it can be converted to a .vmdk for use in VirtualBox:
  VBoxManage convertfromraw --format vmdk mezzano.image mezzano.vmdk

Initially loading the whole system takes approximately 2 hours in
VirtualBox running on a 2.4GHz Core 2 Quad.
