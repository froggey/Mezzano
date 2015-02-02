(progn
 (defvar *file-server-ip* '(192 168 7 8))
 ;; Use MAKE-PATHNAME instead of #p because the cross-compiler doesn't support #p.
 (defvar *file-server-root-directory*
   (make-pathname :host :remote :directory '(:absolute "Users" "pjb" "src" "Mezzano")))
 (defvar *file-server-home-directory*
   (make-pathname :host :remote :directory '(:absolute "Users" "pjb" "Documents" "Mezzano")))
 )
