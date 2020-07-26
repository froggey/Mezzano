(in-package :mezzano.internals)

(defun relativize-dir (path base)
  (labels ((frob (x y)
             (cond ((or (endp x)
                        (endp y)
                        (not (string-equal (first x) (first y))))
                    (list* :relative x))
                   (t
                    (frob (rest x) (rest y))))))
    (frob (pathname-directory path)
          (pathname-directory base))))

(defun guess-element-type-from-extension (type-name)
  (if (member type-name '("jpeg" "jpg" "png" "gif" "pdf" "ttf" "gz" "avi" "wav" "xpm") :test #'string-equal)
      '(unsigned-byte 8)
      'character))

(defun copy-many (files base-path dest-path &key filter (verbose t))
  (dolist (f (directory files))
    (when (and (pathname-name f)
               (or (not filter)
                   (funcall filter f)))
      (let* ((dir (relativize-dir f base-path))
             (real-path (merge-pathnames
                         (make-pathname :directory dir
                                        :name (pathname-name f)
                                        :type (pathname-type f)
                                        :defaults dest-path)
                         dest-path)))
        (ensure-directories-exist real-path)
        (when verbose
          (format t "Copy ~S -> ~S~%" f real-path))
        (with-simple-restart (skip "Skip copying file ~S" f)
          (copy-file f real-path (guess-element-type-from-extension (pathname-type f))))))))

(defun mcclim-setup ()
  ;; Fix up some case issues.
  ;; Only needed when ASDF is pointed at a logical pathname.
  (push `("HOME;|mcclim|;|Backends|;PDF;**;*.*.*" ,(merge-pathnames "mcclim/Backends/PDF/**/" (user-homedir-pathname)))
        (logical-pathname-translations "SYS"))
  (push `("HOME;|mcclim|;|Libraries|;ESA;**;*.*.*" ,(merge-pathnames "mcclim/Libraries/ESA/**/" (user-homedir-pathname)))
        (logical-pathname-translations "SYS"))
  (push `("HOME;|parsley|;|README|.|md|.NEWEST" ,(merge-pathnames "parsley/README.md" (user-homedir-pathname)))
        (logical-pathname-translations "SYS"))
  (push `("HOME;|pngload|;|README|.|md|.NEWEST" ,(merge-pathnames "pngload/README.md" (user-homedir-pathname)))
        (logical-pathname-translations "SYS"))
  ;; Clear the ASDF configuration so it has a better shot of finding the
  ;; ESA and PDF systems.
  (asdf:clear-configuration)
  (asdf:load-system :clim-examples)
  (asdf:load-system :cl-ppcre)
  (asdf:load-system :clim-listener)
  (asdf:load-system :clouseau)
  (setf *inspect-hook* (find-symbol "INSPECT" ':clouseau))
  ;(asdf:load-system :beirc)
  (eval (read-from-string "(mcclim-render-internals::register-all-ttf-fonts (clim:find-port))"))
  (let ((icon-path (translate-logical-pathname
                    (symbol-value
                     (read-from-string "clim-listener::*icon-path*")))))
    (copy-many (make-pathname :defaults icon-path
                              :name :wild
                              :type "xpm")
               icon-path
               #p"LOCAL:>Icons>"))
  (setf (symbol-value (read-from-string "clim-listener::*icon-path*")) #p"LOCAL:>Icons>")
  (clrhash (symbol-value (read-from-string "clim-listener::*icon-cache*")))
  ;;(setf (symbol-value (read-from-string "beirc::*beirc-user-init-file*"))
  ;;      #p"LOCAL:>beirc.lisp")
  (setf mezzano.gui.desktop::*icons*
        (append mezzano.gui.desktop::*icons*
                '(;;("LOCAL:>Icons>Chat.png" "beirc" "(beirc:beirc :new-process t)")
                  ("LOCAL:>Icons>Terminal.png" "McCLIM Listener" "(clim-listener:run-listener :new-process t)")
                  ("LOCAL:>Icons>Editor.png" "McCLIM Demos" "(mezzano.supervisor:make-thread (lambda ()
                                                                                               (let ((*terminal-io* (make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                                                                   :title \"McCLIM Demos console\"))
                                                                                                     (*standard-input* (make-synonym-stream '*terminal-io*))
                                                                                                     (*standard-output* (make-synonym-stream '*terminal-io*))
                                                                                                     (*error-output* (make-synonym-stream '*terminal-io*))
                                                                                                     (*trace-output* (make-synonym-stream '*terminal-io*))
                                                                                                     (*debug-io* (make-synonym-stream '*terminal-io*))
                                                                                                     (*query-io* (make-synonym-stream '*terminal-io*)))
                                                                                                 (clim-demo:demodemo)))
                                                              :name \"McCLIM Demos\")")))))

(defun file-filter (path)
  (not (or (member ".git" (pathname-directory path) :test #'string-equal)
           (member "notes" (pathname-directory path) :test #'string-equal)
           (member "prof" (pathname-directory path) :test #'string-equal)
           (member "iso-stage" (pathname-directory path) :test #'string-equal)
           (member "build-arm64" (pathname-directory path) :test #'string-equal)
           (member "build-x86-64" (pathname-directory path) :test #'string-equal)
           (eql (pathname-version path) :previous)
           (string-equal (pathname-name path) "atom-rombios")
           (string-equal (pathname-name path) "atom-rombios2M")
           (string-equal (pathname-name path) "cbmemc-1")
           (string-equal (pathname-name path) ".git")
           (string-equal (pathname-name path) "build-x86-64")
           (string-equal (pathname-name path) "build-arm64")
           (string-equal (pathname-type path) "log")
           (string-equal (pathname-type path) "llf")
           (string-equal (pathname-type path) "fasl")
           (string-equal (pathname-type path) "image")
           (string-equal (pathname-type path) "vmdk")
           (string-equal (pathname-type path) "map")
           (string-equal (pathname-type path) "iso")
           (string-equal (pathname-type path) "pcap")
           (string-equal (pathname-type path) "raw")
           (string-equal (pathname-name path) "vboxserio"))))

(defun load-quicklisp ()
  (copy-many (merge-pathnames "quicklisp-client/**/*.*" (user-homedir-pathname))
             (merge-pathnames "quicklisp-client/" (user-homedir-pathname))
             #p"local:>Quicklisp>"
             :filter 'file-filter)
  (load "LOCAL:>Quicklisp>setup.lisp"))

(defun copy-sources ()
  ;; Copy Mezz source.
  (copy-many (merge-pathnames "**/*.*" *default-pathname-defaults*)
             *default-pathname-defaults*
             #p"LOCAL:>Mezzano>"
             :filter 'file-filter)
  ;; And everything else.
  (copy-many (merge-pathnames "*/**/*.*" (user-homedir-pathname))
             (user-homedir-pathname)
             #p"LOCAL:>Source>"
             :filter (lambda (path)
                       (not (or (member ".git" (pathname-directory path) :test #'string-equal)
                                (member "Fonts" (pathname-directory path) :test #'string-equal)
                                (member "Iota" (pathname-directory path) :test #'string-equal)
                                (eql (pathname-version path) :previous)
                                (string-equal (pathname-name path) ".git")
                                (string-equal (pathname-type path) "llf")
                                (string-equal (pathname-type path) "fasl")
                                (string-equal (pathname-type path) "avi")))))

  (copy-many (merge-pathnames "*.avi" (user-homedir-pathname))
             (user-homedir-pathname)
             #p"LOCAL:>Videos>"))

(defun warm-up-class-constructors ()
  "Populate make-instance constructors for every class in the system."
  (loop
     for class across (get-all-objects (lambda (object) (typep object 'standard-class)))
     do (ignore-errors
          (format t "~S~%" class)
          (mezzano.clos::safe-class-constructor class))))

(defun load-iota ()
  ;; Doom & Quake
  ;; It's assumed that prboom.lisp and sdlquake.lisp have already been built
  (let ((*default-pathname-defaults* (merge-pathnames "Iota/" (user-homedir-pathname)))
        (mezzano.compiler::*max-optimizer-iterations* 200))
    (asdf:load-system :iota)
    (defpackage :prboom
      (:use :cl :llvm-runtime)
      (:export #:make-context #:spawn))
    (cal "prboom.lisp")
    (let ((*package* (find-package :prboom)))
      (eval (read-from-string
"(defun spawn ()
  (mezzano.supervisor:make-thread
   (lambda ()
     (with-simple-restart (quit \"Quit Doom\")
       (llvm-runtime:main-1
        (make-context :current-directory #p\"LOCAL:>Games>Doom>\")
        ;; -nosfx avoids the delay when quitting, SDL sound doesn't work anyway.
        '(\"prboom\" \"-nosfx\")
        '(\"HOME=/Games/Doom/\"))))
   :name \"prboom\"
   :initial-bindings `((*terminal-io* ,(make-broadcast-stream))
                       (*standard-input* ,(make-synonym-stream '*terminal-io*))
                       (*standard-output* ,(make-synonym-stream '*terminal-io*))
                       (*error-output* ,(make-synonym-stream '*terminal-io*))
                       (*trace-output* ,(make-synonym-stream '*terminal-io*))
                       (*debug-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                   :title \"prboom console\"))
                       (*query-io* ,(make-synonym-stream '*terminal-io*)))))")))
    (ensure-directories-exist "LOCAL:>Games>Doom>")
    (copy-file "prboom.wad" "LOCAL:>Games>Doom>prboom.wad" '(unsigned-byte 8))
    (copy-file "doom1.wad" "LOCAL:>Games>Doom>doom1.wad" '(unsigned-byte 8))
    (copy-file "caco.png" "LOCAL:>Icons>" '(unsigned-byte 8))
    (setf mezzano.gui.desktop::*icons*
          (append mezzano.gui.desktop::*icons*
                  (list '("LOCAL:>Icons>caco.png" "Doom" "(prboom:spawn)"))))
    (defpackage :sdlquake
      (:use :cl :llvm-runtime)
      (:export #:make-context #:spawn))
    (cal "sdlquake.lisp")
    (let ((*package* (find-package :sdlquake)))
      (eval (read-from-string
"(defun spawn ()
  (mezzano.supervisor:make-thread
   (lambda ()
     (with-simple-restart (quit \"Quit Quake\")
       (llvm-runtime:main-1
        (make-context :current-directory #p\"LOCAL:>Games>Quake>\")
        '(\"sdlquake\")
        '(\"HOME=/Games/Quake/\"))))
   :name \"sdlquake\"
   :initial-bindings `((*terminal-io* ,(make-broadcast-stream))
                       (*standard-input* ,(make-synonym-stream '*terminal-io*))
                       (*standard-output* ,(make-synonym-stream '*terminal-io*))
                       (*error-output* ,(make-synonym-stream '*terminal-io*))
                       (*trace-output* ,(make-synonym-stream '*terminal-io*))
                       (*debug-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                   :title \"sdlquake console\"))
                       (*query-io* ,(make-synonym-stream '*terminal-io*)))))")))
    (ensure-directories-exist "LOCAL:>Games>Quake>id1>")
    (copy-file "id1/pak0.pak" "LOCAL:>Games>Quake>id1>pak0.pak" '(unsigned-byte 8))
    (copy-file "quake.png" "LOCAL:>Icons>" '(unsigned-byte 8))
    (setf mezzano.gui.desktop::*icons*
          (append mezzano.gui.desktop::*icons*
                  (list '("LOCAL:>Icons>quake.png" "Quake" "(sdlquake:spawn)"))))))

;; Cutting a release:
;; Start from a clean checkout of mbuild
;; Ensure that Iota is present (or linked) and built in the home directory
;; Set *lisp-implementation-version* appropriately (system/environment.lisp)
;; Optional: Point ASDF source registry at (pathname "SYS:HOME;") (ipl.lisp, HOME-SOURCE-REGISTRY) instead of (user-homedir-pathname). This gets the source paths right for ED.
;; Build as normal
;; In the initial repl:
;; (load "tools/load-sources.lisp")
;; (prepare-release)
;; Position windows/readme/whatever. Prod desktop to make any missing icons appear
;; Snapshot after this completes, closing the repl & saving the image.
;; Use image-manip to cut the compressed image size down:
;; (flatten-image "input.image" "release.image" :header-path "tools/disk-header.bin" :output-size (* 4 1024 1024 1024))
;; ^ produces the final VM image
;; Run tools/kboot/build-native-image to produce the usb/cd hybrid image for real hw.
;;
;; Smoke tests:
;; make sure all apps open
;; connect to nao with telnet
;; connect with irc & beirc (check network disconnects ok on quit in peek)
;; quicklisp: (ql:quickload :anaphora)
;; connect to with slime
;; source locations: (ed '+) (ed 'ql:setup) (ed 'asdf:load-system)
(defun prepare-release ()
  (load-iota)
  (copy-sources)
  (mcclim-setup)
  (load-quicklisp)
  (cal "SYS:SOURCE;GUI;COLOUR-MATRIX-DEMO.LISP")
  (setf mezzano.gui.desktop::*icons*
        (append mezzano.gui.desktop::*icons*
                '(("LOCAL:>Icons>Mandelbrot.png" "Postprocessing Demo" "(mezzano.gui.fancy-repl:spawn :initial-function 'colour-matrix-demo:demo :title \"Display Postprocessing Demo\" :width 450 :height 90)"))))
  (asdf:load-system :mezzano-virgl)
  (setf mezzano.gui.desktop::*icons*
        (append mezzano.gui.desktop::*icons*
                '(("LOCAL:>Icons>Mandelbrot.png" "Virgl 3D Demos" "(mezzano.gui.virgl.demo-menu:spawn)"))))
  ;; Convince ASDF that ASDF and UIOP really are loaded & up to date.
  ;; This is just a hack for the demo & shouldn't be done in general.
  (asdf:register-immutable-system :uiop)
  (asdf:register-immutable-system :asdf)
  (asdf:disable-output-translations)
  (warm-up-class-constructors)
  (setf (symbol-value (read-from-string "bordeaux-threads:*default-special-bindings*"))
        '((*terminal-io* . (make-instance 'mezzano.gui.popup-io-stream:lazy-popup-io-stream))))
  ;; MED still uses this function...
  (setf (fdefinition 'mezzano.gui.compositor::fifo) #'mezzano.gui.compositor::mailbox)
  (setf (sys.int::symbol-global-value '*package*) (find-package :cl-user))
  (setf *default-pathname-defaults* (pathname "LOCAL:>")
        mezzano.file-system::*home-directory* *default-pathname-defaults*)
  (setf (mezzano.file-system:find-host :remote) nil)
  (setf mezzano.gui.compositor:*screensaver-spawn-function* 'mezzano.gui.starfield:spawn
        mezzano.gui.compositor:*screensaver-time* (* 15 60))
  (setf (logical-pathname-translations "SYS")
        '(("HOME;**;*.*.*" #P"LOCAL:>Source>**>")
          ("SOURCE;**;*.*.*" #P"LOCAL:>Mezzano>**>")))
  (setf mezzano.gui.desktop::*icons*
        (append mezzano.gui.desktop::*icons*
                '(:next-column
                  ("LOCAL:>Icons>Editor.png" "Supporters!" "(ed \"LOCAL:>Mezzano>SUPPORTERS.md\")")
                  ("LOCAL:>Icons>Editor.png" "Readme" "(ed \"LOCAL:>Mezzano>README.md\")")
                  ("LOCAL:>Icons>Editor.png" "Quickstart Guide" "(ed \"LOCAL:>Mezzano>doc>quickstart.md\")"))))
  #+(or)
  (setf (sys.int::symbol-global-value '*terminal-io*)
        (make-instance 'mezzano.gui.popup-io-stream:lazy-popup-io-stream))
  (when (y-or-n-p "Snapshot?")
    (sys.int::snapshot-and-exit)))
