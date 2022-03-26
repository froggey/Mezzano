(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:mcclim :cl-fad)))

(cl:in-package #:clim-user)

; LTAG-start:file-browser-all
(define-application-frame file-browser ()
  ((active-files :initform nil :accessor active-files))
  (:panes
   (file-browser :application
                 :display-function '(dirlist-display-files)
                 ;; Call the display-function whenever the command
                 ;; loop makes a ``full-cycle''
                 :display-time :command-loop)
   (interactor :interactor))
  (:layouts (default (vertically ()
                       (:fill file-browser)
                       (1/4 interactor))))
  (:menu-bar nil))

(define-presentation-type dir-pathname ()
  :inherit-from 'pathname)

(defmethod dirlist-display-files ((frame file-browser) pane)
  ;; Clear old displayed entries
  (clear-output-record (stream-output-history pane))

  (dolist (file (active-files frame))
    ;; Instead of write-string, we use present so that the link to
    ;; object file and the semantic information that file is
    ;; pathname is retained.
    (present file
             (if (cl-fad:directory-pathname-p file) 'dir-pathname 'pathname)
             :stream pane)
    (terpri pane)))

(define-file-browser-command (com-edit-directory :name "Edit Directory")
  ((dir 'dir-pathname))
  ;; the following was a previous attempt to deal with the oddities of
  ;; CL pathnames.  Unfortunately, it does not work properly with all
  ;; lisp implementations.  Because of these oddities, we really need
  ;; a layer like cl-fad to keep things straight. [2007/01/05:rpg]
;;;  (let ((dir (make-pathname :directory (pathname-directory dir)
;;;                         :name :wild :type :wild :version :wild
;;;                         :defaults dir)))
    (setf (active-files *application-frame*)
          (cl-fad:list-directory dir)))

(define-presentation-to-command-translator pathname-to-edit-command
    (dir-pathname                       ; source presentation-type
     com-edit-directory                 ; target-command
     file-browser                       ; command-table
     :gesture :select                   ; use this translator for pointer clicks
     :documentation "Edit this path")   ; used in context menu
    (object)                            ; argument List
    (list object))                      ; arguments for target-command

(define-file-browser-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(defmethod adopt-frame :after (frame-manager (frame file-browser))
  (declare (ignore frame-manager))
  (execute-frame-command frame
        `(com-edit-directory ,(make-pathname :directory '(:absolute)))))
; LTAG-end
