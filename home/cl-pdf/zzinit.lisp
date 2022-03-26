;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;
;; Proposed initialize! function and warning when loading without afm-files-directories, 
;;
;; Dave Cooper, david -dot- cooper -at- genworks -dot- com
;;


(in-package #:pdf)

(defun initialize! (&key afm-files-directories)
  "Directory list. Set the afm-files-directories to the correct runtime value, and
force loading of the font data."
  (when afm-files-directories 
    (when (atom afm-files-directories)
      (setq afm-files-directories (list afm-files-directories)))
    (setq *afm-files-directories* afm-files-directories))
  (confirm-afm-files-directories)
  (load-fonts t)
  *afm-files-directories*)


(defun confirm-afm-files-directories ()
  (let (nonexistent)
    (dolist (directory *afm-files-directories*)
      (unless (#-clisp probe-file #+clisp ext:probe-directory directory)
	(pushnew directory nonexistent :test #'equalp)))
    (when nonexistent 
      (warn "You have set the following non-existent director~@p in *afm-files-directories*:

~{~a~^~%~}

Before attempting to run any cl-pdf functions, you will want to
initialize the system with something like this:

  (pdf:initialize! :afm-files-directories (list \"/usr/share/fonts/afm/\"))
"
	    (length nonexistent) nonexistent))
    (if (or nonexistent (null *afm-files-directories*)) nil t)))

    
(confirm-afm-files-directories)


