;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.internals)

;; Fast eval mode.
(setf sys.int::*eval-hook* 'mezzano.fast-eval:eval-in-lexenv)

;; Host where the initial system is kept.
;; Change the IP to the host computer's local IP.
(mezzano.file-system.remote:add-remote-file-host :remote sys.int::*file-server-host-ip*)
;; Use PATHNAME instead of #p because the cross-compiler doesn't support #p.
;; Point *DEFAULT-PATHNAME-DEFAULTS* at the full path to the source tree.
(setf *default-pathname-defaults* (pathname (concatenate 'string "REMOTE:" sys.int::*mezzano-source-path*)))
;; Point MEZZANO.FILE-SYSTEM::*HOME-DIRECTORY* at the home directory containing the libraries.
(setf mezzano.file-system::*home-directory* (pathname (concatenate 'string "REMOTE:" sys.int::*home-directory-path*)))

(push (list "SOURCE;**;*.*.*" (merge-pathnames "**/" *default-pathname-defaults*))
      (logical-pathname-translations "SYS"))

(push (list "HOME;**;*.*.*" (merge-pathnames "**/" (user-homedir-pathname)))
      (logical-pathname-translations "SYS"))

(defun sys.int::check-connectivity ()
  ;; Make sure that there's one network card.
  (loop
     with timeout = 30.0
     do
       (when (or mezzano.network.ip::*ipv4-interfaces*
                 (minusp timeout))
         (return))
       (sleep 0.1)
       (decf timeout 0.1))
  (when (null mezzano.network.ip::*ipv4-interfaces*)
    (format t "No network cards detected!~%~
Make sure there is a virtio-net NIC attached.~%")
    (return-from sys.int::check-connectivity))
  (when (not (null (rest mezzano.network.ip::*ipv4-interfaces*)))
    (format t "Multiple network cards detected! Not supported, but trying anyway.~%"))
  (format t "Using network card ~S.~%" (first (first mezzano.network.ip::*ipv4-interfaces*)))
  ;; Check connectivity to the file-server.
  (let ((fs-address (mezzano.network.ip:make-ipv4-address sys.int::*file-server-host-ip*)))
    (format t "File server has address ~A, port ~D.~%" fs-address mezzano.file-system.remote::*default-remote-file-port*)
    (when (mezzano.network.ip:address-equal
           (mezzano.network.ip:address-network fs-address 24)
           (mezzano.network.ip:make-ipv4-address "10.0.2.0"))
      (format t "Warning! This is on a 10/8 network and is not supported.~%"))
    (when (mezzano.network.ip:address-equal
           fs-address
           (mezzano.network.ip:make-ipv4-address "127.0.0.1"))
      (format t "Warning! This is the local loopback address, and is probably not what you want.~%"))
    (format t "Pinging file server host... ")
    (finish-output)
    (cond ((mezzano.network.ip:ping-host sys.int::*file-server-host-ip* :quiet t)
           (format t "OK!~%"))
          (t
           (format t "Failed! No responses received! This may not be reliable.~%"))))
  ;; Try basic access to the file server. Just enough to talk to it.
  (format t "Testing access to file server... ")
  (finish-output)
  (mezzano.file-system.remote:test-host-connectivity
   (mezzano.file-system:find-host :remote))
  (format t "OK!~%")
  ;; Check connectivity to the internet.
  (format t "Testing internet connectivity.~%")
  (format t "Attempting to resolve google.com... ")
  (finish-output)
  (let ((goog (mezzano.network::resolve-address "google.com" nil)))
    (cond (goog
           (format t "OK!~%")
           (format t "Has address ~A.~%" goog))
          (t
           (format t "Failed! Unable to resolve! This may not be reliable.~%")))))

(sys.int::check-connectivity)

;; Local FS. Loaded from the source tree, not the home directory.
(sys.int::cal "sys:source;file;local.lisp")
(eval (read-from-string "(mezzano.file-system.local:add-local-file-host :local)"))

;; ASDF.
(sys.int::cal "sys:home;asdf;build;asdf.lisp")

(defun home-source-registry ()
  `(:source-registry
    (:tree ,(user-homedir-pathname))
    :inherit-configuration))
(eval (read-from-string "(push 'home-source-registry asdf:*default-source-registries*)"))

(defun driver-source-registry ()
  `(:SOURCE-REGISTRY
    (:TREE ,(pathname (concatenate 'string "REMOTE:"
                                   sys.int::*mezzano-source-path*
                                   "drivers/")))
    :INHERIT-CONFIGURATION))
(eval (read-from-string "(push 'driver-source-registry asdf:*default-source-registries*)"))

;; Sound driver.
(sys.int::cal "sys:source;drivers;sound.lisp")
#+x86-64
(sys.int::cal "sys:source;drivers;intel-hda.lisp")

;; Split-sequence
(require :split-sequence)

;; A bunch of GUI related systems.
(require :zpb-ttf)
(require :cl-vectors)
(require :cl-paths-ttf)
;; TCE is required for Chipz's decompressor.
(let ((mezzano.compiler::*perform-tce* t)
      ;; Prevent extremely excessive inlining.
      (mezzano.compiler::*constprop-lambda-copy-limit* -1)
      ;; This inhibits TCE when enabled.
      (mezzano.compiler::*verify-special-stack* nil))
  (require :chipz))
(require :png-read)
(require :cl-jpeg)
(require :skippy)
(require :cl-video)
(require :cl-video-avi)
(require :cl-video-gif)
(require :cl-video-wav)
(require :cl-wav)
;; Swank doesn't really support logical pathname shenanigans.
(load (merge-pathnames "slime/swank-loader.lisp" (user-homedir-pathname)))
(eval (read-from-string "(swank-loader::init)"))
(eval (read-from-string "(swank:create-server :style :spawn :dont-close t :interface \"0.0.0.0\")"))

;; And the GUI.
(sys.int::cal "sys:source;gui;package.lisp")
(sys.int::cal "sys:source;gui;colour.lisp")
(sys.int::cal "sys:source;gui;surface.lisp")
(sys.int::cal "sys:source;gui;blit.lisp")
#+x86-64
(sys.int::cal "sys:source;gui;blit-x86-64-simd.lisp")
#+arm64
(sys.int::cal "sys:source;gui;blit-generic.lisp")
(sys.int::cal "sys:source;gui;keymaps.lisp")
(sys.int::cal "sys:source;gui;compositor.lisp")
#+x86-64
(sys.int::cal "sys:source;gui;input-drivers.lisp")
#+arm64
(sys.int::cal "sys:source;gui;input-drivers-virtio.lisp")
#+x86-64
(sys.int::cal "sys:source;gui;virtualbox-guest-helper.lisp")
(sys.int::cal "sys:source;system;unifont.lisp")
(sys.int::cal "sys:source;gui;basic-repl.lisp")
(eval (read-from-string "(mezzano.gui.basic-repl:spawn)"))
(sys.int::cal "sys:source;gui;font.lisp")
(sys.int::cal "sys:source;gui;image.lisp")

;; Fonts. Loaded from the home directory.
(ensure-directories-exist "LOCAL:>Fonts>")
(dolist (f (directory (merge-pathnames "Fonts/**/*.ttf" (user-homedir-pathname))))
  (sys.int::copy-file f
             (merge-pathnames "LOCAL:>Fonts>" f)
             '(unsigned-byte 8)))
(sys.int::copy-file (merge-pathnames "Fonts/LICENSE" (user-homedir-pathname))
                    "LOCAL:>Fonts>LICENSE"
                    'character)

;; Icons. Loaded from the source tree.
(ensure-directories-exist "LOCAL:>Icons>")
(dolist (f (directory "sys:source;gui;*.png"))
  (sys.int::copy-file f
                      (merge-pathnames "LOCAL:>Icons>" f)
                      '(unsigned-byte 8)))

;: Mouse cursors.
(flet ((load-cursor (path name &optional (hot-x 0) (hot-y 0))
         (let ((surf (funcall (read-from-string "mezzano.gui.image:load-image")
                              (merge-pathnames path "LOCAL:>Icons>"))))
           (funcall (read-from-string "mezzano.gui.compositor:register-mouse-cursor")
            (funcall (read-from-string "mezzano.gui.compositor:make-mouse-cursor")
                     surf :hot-x hot-x :hot-y hot-y)
            name))))
  (load-cursor "cursor-upleft.png"    :arrow-up-left     0   0)
  (load-cursor "cursor-upright.png"   :arrow-up-right    15  0)
  (load-cursor "cursor-downright.png" :arrow-down-right  15 15)
  (load-cursor "cursor-downleft.png"  :arrow-down-left   0  15)
  (load-cursor "cursor-up.png"        :arrow-up          8   0)
  (load-cursor "cursor-right.png"     :arrow-right       15  8)
  (load-cursor "cursor-down.png"      :arrow-down        8  15)
  (load-cursor "cursor-left.png"      :arrow-left        0   8))

;; Remaining GUI files.
(sys.int::cal "sys:source;gui;widgets.lisp")
(sys.int::cal "sys:source;line-edit-mixin.lisp")
(sys.int::cal "sys:source;gui;popup-io-stream.lisp")
;;(eval (read-from-string "(setf (sys.int::symbol-global-value '*terminal-io*) (make-instance 'mezzano.gui.popup-io-stream:lazy-popup-io-stream))"))
(sys.int::cal "sys:source;gui;xterm.lisp")
(sys.int::cal "sys:source;applications;telnet.lisp")
(sys.int::cal "sys:source;applications;mandelbrot.lisp")
(sys.int::cal "sys:source;applications;irc.lisp")
(require :med)
(sys.int::cal "sys:source;applications;peek.lisp")
(sys.int::cal "sys:source;applications;fancy-repl.lisp")
(sys.int::cal "sys:source;gui;desktop.lisp")
(sys.int::cal "sys:source;gui;image-viewer.lisp")
(sys.int::cal "sys:source;gui;trentino.lisp")
(sys.int::cal "sys:source;gui;music-player.lisp")
(sys.int::cal "sys:source;applications;filer.lisp")
(sys.int::cal "sys:source;applications;memory-monitor.lisp")
(sys.int::cal "sys:source;gui;starfield.lisp")
;;(eval (read-from-string "(setf mezzano.gui.compositor:*screensaver-spawn-function* 'mezzano.gui.starfield:spawn)"))

;; USB Driver
(require :mezzano-usb)
(require :mezzano-usb/class-drivers)
(require :mezzano-usb/ohci)

;; Other stuff.
(sys.int::cal "sys:source;drivers;intel-gma.lisp")
(sys.int::cal "sys:source;file;ext4.lisp")
(sys.int::cal "sys:source;file;http.lisp")
(sys.int::cal "sys:source;net;http-demo.lisp")
(sys.int::cal "sys:source;system;disassemble.lisp")
(sys.int::cal "sys:source;system;lldb.lisp")

;; Load the desktop image and start the desktop.
(sys.int::copy-file (merge-pathnames "19377769093_c9cb23b4d3_b.jpg" (user-homedir-pathname))
                    "LOCAL:>Desktop.jpeg"
                    '(unsigned-byte 8))
(defvar sys.int::*desktop* (eval (read-from-string "(mezzano.gui.desktop:spawn :image \"LOCAL:>Desktop.jpeg\")")))

(defvar sys.int::*init-file-path* "SYS:HOME;INIT.LISP")

(defun sys.int::load-init-file ()
  (sleep 0.5) ; give the network a little time to settle.
  (when (and (boundp 'sys.int::*init-file-path*)
             sys.int::*init-file-path*)
    (handler-case (load sys.int::*init-file-path*)
      (error (c)
        (format t "Unable to load init file ~S: ~A.~%"
                sys.int::*init-file-path*
                c)))))

(mezzano.supervisor:add-boot-hook 'sys.int::load-init-file :late)
(sys.int::load-init-file)

;; Done.
