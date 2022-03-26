(put 'define-border-type 'common-lisp-indent-function 2) 
(put 'define-command 'common-lisp-indent-function 2)  
(put 'formatting-row 'common-lisp-indent-function 1) 
(put 'do-command-table-inheritance 'common-lisp-indent-function 1) 
(put 'define-presentation-to-command-translator 'common-lisp-indent-function 3) 
(put 'with-sheet-medium 'common-lisp-indent-function 1) 
(put 'with-new-output-record 'common-lisp-indent-function 1) 
(put 'with-activation-gestures 'common-lisp-indent-function 1) 
(put 'with-command-table-keystrokes 'common-lisp-indent-function 1) 
(put 'with-output-to-postscript-stream 'common-lisp-indent-function 1) 
(put 'with-radio-box 'common-lisp-indent-function 1) 
(put 'labelling 'common-lisp-indent-function 1) 
(put 'spacing 'common-lisp-indent-function 1)  
(put 'with-text-size 'common-lisp-indent-function 1) 
(put 'with-scaling 'common-lisp-indent-function 1)  
(put 'horizontally 'common-lisp-indent-function 1)  
(put 'surrounding-output-with-border 'common-lisp-indent-function 1) 
(put 'with-rotation 'common-lisp-indent-function 1) 
(put 'define-presentation-translator 'common-lisp-indent-function 3) 
(put 'with-bounding-rectangle* 'common-lisp-indent-function 2)  
(put 'vertically 'common-lisp-indent-function 1)  
(put 'with-graft-locked 'common-lisp-indent-function 1) 
(put 'formatting-table 'common-lisp-indent-function 1) 
(put 'updating-output 'common-lisp-indent-function 1) 
(put 'with-input-focus 'common-lisp-indent-function 1) 
(put 'tabling 'common-lisp-indent-function 1) 
(put 'indenting-output 'common-lisp-indent-function 1) 
(put 'changing-space-requirements 'common-lisp-indent-function 1) 
(put 'with-identity-transformation 'common-lisp-indent-function 1)  
(put 'with-end-of-page-action 'common-lisp-indent-function 1) 
(put 'with-translation 'common-lisp-indent-function 1) 
(put 'formatting-column 'common-lisp-indent-function 1) 
(put 'with-input-editing 'common-lisp-indent-function 1) 
(put 'with-text-face 'common-lisp-indent-function 1) 
(put 'with-drawing-options 'common-lisp-indent-function 1) 
(put 'with-presentation-type-decoded 'common-lisp-indent-function 2) 
(put 'with-output-recording-options 'common-lisp-indent-function 1) 
(put 'with-output-buffered 'common-lisp-indent-function 1) 
(put 'with-input-context 'common-lisp-indent-function 3) 
(put 'with-output-as-presentation 'common-lisp-indent-function 1)  
(put 'tracking-pointer 'common-lisp-indent-function 1)  
(put 'with-presentation-type-options 'common-lisp-indent-function 1) 
(put 'with-port-locked 'common-lisp-indent-function 1)  
(put 'accepting-values 'common-lisp-indent-function 1) 
(put 'with-room-for-graphics 'common-lisp-indent-function 1) 
(put 'define-presentation-action 'common-lisp-indent-function 3) 
(put 'with-presentation-type-parameters 'common-lisp-indent-function 1) 
(put 'with-output-as-gadget 'common-lisp-indent-function 1) 
(put 'with-frame-manager 'common-lisp-indent-function 1) 
(put 'with-text-family 'common-lisp-indent-function 1)  
(put 'with-end-of-line-action 'common-lisp-indent-function 1) 
(put 'formatting-item-list 'common-lisp-indent-function 1) 
(put 'with-sheet-medium-bound 'common-lisp-indent-function 1)  
(put 'with-output-to-output-record 'common-lisp-indent-function 1) 
(put 'with-output-to-pixmap 'common-lisp-indent-function 1) 
(put 'restraining 'common-lisp-indent-function 1) 
(put 'filling-output 'common-lisp-indent-function 1) 
(put 'scrolling 'common-lisp-indent-function 1) 
(put 'with-text-style 'common-lisp-indent-function 1) 
(put 'formatting-cell 'common-lisp-indent-function 1)  
(put 'with-look-and-feel-realization 'common-lisp-indent-function 1)  
(put 'with-menu 'common-lisp-indent-function 1) 
(put 'with-application-frame 'common-lisp-indent-function 1) 
(put 'outlining 'common-lisp-indent-function 1) 
(put 'with-delimiter-gestures 'common-lisp-indent-function 1)  
(put 'completing-from-suggestions 'common-lisp-indent-function 1)
(put 'tracking-pointer 'common-common-lisp-indent-function
     '((&whole 4 &rest 1) &rest (&whole 1 &lambda &body)))

;;
;;(DEFMACRO CLIM:DEFINE-COMMAND-TABLE
;;          (NAME &KEY (INHERIT-FROM '(GLOBAL-COMMAND-TABLE)) MENU))             
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-TYPE-ABBREVIATION
;;          (NAME PARAMETERS EQUIVALENT-TYPE &KEY OPTIONS))    
;;
;;(DEFMACRO CLIM:SLIDER (&REST OPTIONS))   
;;
;;(DEFMACRO CLIM:DEFINE-APPLICATION-FRAME
;;          (NAME SUPERCLASSES SLOTS &REST OPTIONS))      
;;
;;(DEFMACRO CLIM:DEFINE-GRAPH-TYPE (GRAPH-TYPE CLASS))   
;;
;;(DEFMACRO CLIM:PUSH-BUTTON (&REST OPTIONS))          
;;
;;(DEFMACRO CLIM:TOGGLE-BUTTON (&REST OPTIONS))             
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-GENERIC-FUNCTION
;;          (GENERIC-FUNCTION-NAME PRESENTATION-FUNCTION-NAME LAMBDA-LIST
;;           &REST OPTIONS))   
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-METHOD (NAME &REST ARGS))    
;;
;;(DEFMACRO CLIM:APPLY-PRESENTATION-GENERIC-FUNCTION (NAME &REST ARGS))         
;;
;;(DEFMACRO CLIM:TEXT-FIELD (&REST OPTIONS))     
;;
;;(DEFMACRO CLIM:DEFINE-GESTURE-NAME (NAME TYPE GESTURE-SPEC &KEY (UNIQUE T)))         
;;
;;(DEFMACRO CLIM:DEFINE-DEFAULT-PRESENTATION-METHOD (NAME &REST ARGS))   
;;
;;(DEFMACRO CLIM:FUNCALL-PRESENTATION-GENERIC-FUNCTION (NAME &REST ARGS))      
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-TYPE
;;          (NAME PARAMETERS
;;           &KEY OPTIONS INHERIT-FROM
;;           (DESCRIPTION (MAKE-DEFAULT-DESCRIPTION NAME)) (HISTORY T)
;;           PARAMETERS-ARE-TYPES))  
;;

;;; Generated with this humble code:

;; (defun quux ()
;;   (dolist (sym (remove-if-not #'macro-function
;;                               (apropos-list "" :CLIM t)))
;;     (let ((arglist (read-from-string (ilisp::arglist (symbol-name sym)(package-name (symbol-package sym))))))
;;       (let ((n (position '&body arglist)))
;;         (when n
;;           (format t "~&(put '~(~A~) 'common-lisp-indent-function ~D)"
;;                   sym n))
;;         (unless n
;;           (fresh-line)
;;           (pprint-logical-block (nil nil :per-line-prefix ";;")
;;              (print `(defmacro ,sym ,arglist))))))))
;; 

;; -- 
;; Gilbert Baumann, 2003-05-19
;; <unk6@rz.uni-karlsruhe.de>
