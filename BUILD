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

Build a cold image.
(cold-generator::make-image "mezzano" :header-path "tools/disk_header")

This will produce a raw disk image called mezzano.image in the current directory.
