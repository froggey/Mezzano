;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  #.(with-open-file (in "ipl-configuration.lisp")
      `(progn ,@(loop :for form = (read in nil in)
                      :until (eq form in)
                      :collect form))))


;; Fast eval mode.
(setf sys.int::*eval-hook* 'mezzano.fast-eval:eval-in-lexenv)

;; Host where the initial system is kept.
(mezzano.file-system.remote:add-simple-file-host :remote *file-server-ip*)
;; (setf *default-pathname-defaults*            #.(parse-namestring *file-server-source-directory* "REMOTE"))
;; (setf mezzano.file-system::*home-directory*  #.(parse-namestring *file-server-home-directory*   "REMOTE"))
(setf *default-pathname-defaults*            (make-pathname :host "REMOTE"
                                                            :directory '(:absolute "Users" "pjb" "src" "Mezzano")))
(setf mezzano.file-system::*home-directory*  (make-pathname :host "REMOTE"
                                                            :directory '(:absolute "Users" "pjb" "Documents" "Mezzano")))

(defun sys.int::snapshot-and-exit ()
  (mezzano.supervisor:make-thread (lambda ()
                                    (dotimes (i 100)
                                      (mezzano.supervisor:wait-for-heartbeat))
                                    (sys.int::gc)
                                    (mezzano.supervisor:snapshot)))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defun sys.int::cal (path)
  "Compile and load PATH.
If the compiled file is out of date, recompile it."
  (let ((compiled (compile-file-pathname path)))
    (when (or (not (probe-file compiled))
              (<= (file-write-date compiled) (file-write-date path)))
      (format t "; Compiling ~S~%" path)
      (ignore-errors (delete-file compiled))
      (compile-file path))
    (format t "; Loading ~S~%" compiled)
    (load compiled)))

(defun sys.int::copy-file (filespec new-name &optional (element-type 'character) (source-external-format :default) (destination-external-format :default))
  (let* ((source (merge-pathnames filespec))
         (dest (merge-pathnames new-name source)))
    (with-open-file (s source :element-type element-type :external-format source-external-format)
      (with-open-file (d dest :direction :output :element-type element-type :external-format destination-external-format)
        (cond ((subtypep element-type 'character)
               ;; FILE-LENGTH cannot be trusted when doing character IO. Go line-by-line.
               (loop
                  (multiple-value-bind (line missing-newline-p)
                      (read-line s nil)
                    (when (not line)
                      (return))
                    (write-string line d)
                    (when (not missing-newline-p)
                      (terpri d)))))
              (t (let ((seq (make-array (file-length s) :element-type element-type)))
                   (read-sequence seq s)
                   (write-sequence seq d))))))
    dest))

;; Local FS. Loaded from the source tree, not the home directory.
(sys.int::cal "file/local.lisp")
(eval (read-from-string "(mezzano.file-system.local:add-local-file-host :local)"))

;; Fonts. Loaded from the home directory.
(ensure-directories-exist "LOCAL:>Fonts>")
(dolist (f (directory (merge-pathnames "fonts/**/*.ttf" (user-homedir-pathname))))
  (sys.int::copy-file f
             (merge-pathnames "LOCAL:>Fonts>" f)
             '(unsigned-byte 8)))

;; Icons. Loaded from the source tree.
(ensure-directories-exist "LOCAL:>Icons>")
(dolist (f (directory "gui/*.png"))
  (sys.int::copy-file f
                      (merge-pathnames "LOCAL:>Icons>" f)
                      '(unsigned-byte 8)))

;; Other stuff.
;; The desktop image, this can be removed or replaced.
;; If it is removed, then the line below that starts the desktop must be updated.
(sys.int::copy-file (merge-pathnames *desktop-image* (user-homedir-pathname))
                    "LOCAL:>Desktop.jpeg"
                    '(unsigned-byte 8))

;; Loaded from the source tree.
(sys.int::copy-file "README"
                    "LOCAL:>README.text")

;; ASDF.
;; After ASDF is compiled for the first time it will fail to load with
;; an undefined-function EXPORT error. This can be fixed by rebooting.
(sys.int::cal (merge-pathnames "asdf/asdf.lisp" (user-homedir-pathname)))

(format t "~&ASDF loaded.~%") (finish-output)

;; A bunch of GUI related systems.
(require :zpb-ttf)
(require :cl-vectors)
(require :cl-paths-ttf)
;; TCE is required for Chipz's decompressor.
(let ((sys.c::*perform-tce* t))
  (require :chipz))
(require :png-read)
(require :cl-jpeg)

;; And the GUI.
(sys.int::cal "gui/font.lisp")
(sys.int::cal "gui/widgets.lisp")
(sys.int::cal "gui/desktop.lisp")
(sys.int::cal "line-edit-mixin.lisp")
(sys.int::cal "gui/popup-io-stream.lisp")
(sys.int::cal "gui/xterm.lisp")
(sys.int::cal "applications/telnet.lisp")
(sys.int::cal "applications/mandelbrot.lisp")
(sys.int::cal "applications/irc.lisp")
(sys.int::cal "applications/editor.lisp")
(sys.int::cal "applications/peek.lisp")
(sys.int::cal "applications/fancy-repl.lisp")
(sys.int::cal "gui/desktop.lisp")
(sys.int::cal "gui/image-viewer.lisp")
(sys.int::cal "applications/fs-viewer.lisp")
;; If the desktop image was removed above, then remove the :IMAGE argument
;; from here.
(setf sys.int::*desktop* (eval (read-from-string "(mezzano.gui.desktop:spawn :image \"LOCAL:>Desktop.jpeg\")")))

;; Done.
