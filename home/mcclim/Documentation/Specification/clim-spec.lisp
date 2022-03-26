;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Lowercase: Yes -*-

(defsystem CLIM-spec
    (:default-pathname t
     :pretty-name "CLIM Specification"
     :maintain-journals nil
     :default-module-type :text)
  (:serial
    ;; Status
    "spec-status.text"
    
    ;; Include files
    "psfig+.tex"
    "spec-macros.tex"
    "clim.tex"
    
    ;; Acknowledgments
    "acknowledgments.tex"

    ;; Overview and Conventions
    "overview.tex"
    "conventions.tex"

    ;; Geometry Substrate
    "regions.tex"
    "bboxes.tex"
    "transforms.tex"
    
    ;; Windowing Substrate
    "silica.tex"
    
    ;; Basic (Sheet/Medium) Output Facilities
    "drawing-options.tex"
    "text-styles.tex"
    "graphics.tex"
    "colors.tex"
    "designs.tex"

    ;; Extended (Stream) Output Facilities
    "extended-output.tex"
    "output-recording.tex"
    "table-formatting.tex"
    "graph-formatting.tex"
    "bordered-output.tex"
    "text-formatting.tex"
    "redisplay.tex"

    ;; Extended (Stream) Input Facilities
    "extended-input.tex"
    "presentation-types.tex"
    "input-editing.tex"
    "menus.tex"
    "dialogs.tex"

    ;; Building Applications
    "commands.tex"
    "frames.tex"
    "panes.tex"
    "gadgets.tex"

    ;; Appendices
    "glossary.tex"
    "clim-sys.tex"
    "gray-streams.tex"
    "encapsulating-streams.tex"
    "changes.tex"))


#||
()

(defun dump-clim-symbols (&optional (output #p"sap:>swm>clim>pkgs.text"))
  (with-open-file (pkgs output :direction :output)
    (let ((sources (sct:get-all-system-input-files 'clim-spec))
          (keyword-pkg (find-package 'keyword)))
      (dolist (source sources)
        (when (string-equal (pathname-type source) "TEX")
          (with-open-file (stream source :direction :input)
            (let ((symbols nil))
              (loop
                (let ((line (read-line stream nil :eof)))
                  (when (eql line :eof)
                    (when symbols
                      (setq symbols (delete-duplicates symbols))
                      (setq symbols (sort symbols #'string-lessp))
                      (format pkgs ";; ~A~%" (pathname-name source))
                      (dolist (symbol symbols)
                        (format pkgs "~(~A~)~%" symbol))
                      (terpri pkgs))
                    (return))
                  (when (string-equal line "\\def" :end1 4)
                    (let* ((open  (position #\{ line :test #'char-equal))
                           (close (position #\} line :test #'char-equal))
                           (slash (position #\\ line :test #'char-equal :start 4)))
                      (when (and open close 
                                 (> close (1+ open))
                                 (not (and slash (< slash open))))
                        (let ((symbol (read-from-string (subseq line (1+ open) close))))
                          (when (and (listp symbol)
                                     (eql (first symbol) 'setf))
                            (setq symbol (second symbol)))
                          (unless (eql (symbol-package symbol) keyword-pkg)
                            (push symbol symbols)))))))))))))))
||#
