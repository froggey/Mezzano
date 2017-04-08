;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cl-user)

(defun sys.int::setup-for-release ()
  (load "tools/load-sources.lisp")
  (setf (sys.int::symbol-global-value '*package*) (find-package :cl-user))
  (setf *default-pathname-defaults* (pathname "LOCAL:>")
        mezzano.file-system::*home-directory* *default-pathname-defaults*)
  (setf (mezzano.file-system:find-host :remote) nil)
  (when (y-or-n-p "Snapshot?")
    (sys.int::snapshot-and-exit)))

;; Fast eval mode.
(setf sys.int::*eval-hook* 'mezzano.fast-eval:eval-in-lexenv)

;; Host where the initial system is kept.
;; Change the IP to the host computer's local IP.
(mezzano.file-system.remote:add-simple-file-host :remote sys.int::*file-server-host-ip*)
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
  (when (null mezzano.network.ethernet::*cards*)
    (format t "No network cards detected!~%~
Make sure there is a virtio-net NIC attached.~%")
    (return-from sys.int::check-connectivity))
  (when (not (null (rest mezzano.network.ethernet::*cards*)))
    (format t "Multiple network cards detected! Not supported, but trying anyway.~%"))
  (format t "Using network card ~S.~%" (first mezzano.network.ethernet::*cards*))
  ;; Check connectivity to the file-server.
  (let ((fs-address (mezzano.network.ip:make-ipv4-address sys.int::*file-server-host-ip*)))
    (format t "File server has address ~A, port ~D.~%" fs-address mezzano.file-system.remote::*default-simple-file-port*)
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
  (let ((goog (sys.net::resolve-address "google.com" nil)))
    (cond (goog
           (format t "OK!~%")
           (format t "Has address ~A.~%" goog))
          (t
           (format t "Failed! Unable to resolve! This may not be reliable.~%")))))

(sys.int::check-connectivity)

;; Local FS. Loaded from the source tree, not the home directory.
(sys.int::cal "sys:source;file;local.lisp")
(eval (read-from-string "(mezzano.file-system.local:add-local-file-host :local)"))

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

;; Other stuff.
;; The desktop image, this can be removed or replaced.
;; If it is removed, then the line below that starts the desktop must be updated.
(sys.int::copy-file (merge-pathnames "Ducks.jpg" (user-homedir-pathname))
                    "LOCAL:>Desktop.jpeg"
                    '(unsigned-byte 8))

;; ASDF.
(sys.int::cal (merge-pathnames "asdf/asdf.lisp" (user-homedir-pathname)))
(defun home-source-registry ()
  `(:source-registry
    (:tree ,(user-homedir-pathname))
    :inherit-configuration))
(eval (read-from-string "(push 'home-source-registry asdf:*default-source-registries*)"))

;; A bunch of GUI related systems.
(require :zpb-ttf)
(require :cl-vectors)
(require :cl-paths-ttf)
;; TCE is required for Chipz's decompressor.
(let ((sys.c::*perform-tce* t)
      ;; Prevent extremely excessive inlining.
      (sys.c::*constprop-lambda-copy-limit* -1)
      ;; This inhibits TCE when enabled.
      (sys.c::*verify-special-stack* nil))
  (require :chipz))
(require :png-read)
(require :cl-jpeg)
(require :skippy)
(require :cl-video)
(require :cl-video-avi)
(require :cl-video-gif)
(require :cl-video-wav)
(require :cl-wav)
(require :swank)
(eval (read-from-string "(swank:create-server :style :spawn :dont-close t)"))

;; And the GUI.
(sys.int::cal "sys:source;gui;font.lisp")
(sys.int::cal "sys:source;gui;image.lisp")

;: Mouse cursors.
(flet ((load-cursor (path name &optional (hot-x 0) (hot-y 0))
         (let ((surf (funcall (read-from-string "mezzano.gui.image:load-image")
                              (merge-pathnames path "LOCAL:>Icons>"))))
           (mezzano.gui.compositor:register-mouse-cursor
            (mezzano.gui.compositor:make-mouse-cursor surf :hot-x hot-x :hot-y hot-y)
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
(sys.int::cal "sys:source;file;http.lisp")
;; If the desktop image was removed above, then remove the :IMAGE argument
;; from here.
(defvar sys.int::*desktop* (eval (read-from-string "(mezzano.gui.desktop:spawn :image \"LOCAL:>Desktop.jpeg\")")))

(defvar sys.int::*init-file-path* "SYS:HOME;INIT.LISP")

(defun sys.int::load-init-file ()
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
