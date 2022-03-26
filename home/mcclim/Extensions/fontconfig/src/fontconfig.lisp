(in-package :mcclim-fontconfig)

(defvar *config* nil)

(defvar *property-names* '((:family . "family")
                          (:familylang . "familylang")
                          (:style . "style")
                          (:stylelang . "stylelang")
                          (:fullname . "fullname")
                          (:fullnamelang . "fullnamelang")
                          (:slant . "slant")
                          (:weight . "weight")
                          (:size . "size")
                          (:width . "width")
                          (:aspect . "aspect")
                          (:pixelsize . "pixelsize")
                          (:spacing . "spacing")
                          (:foundry . "foundry")
                          (:antialias . "antialias")
                          (:hinting . "hinting")
                          (:hintstyle . "hintstyle")
                          (:verticallayout . "verticallayout")
                          (:autohint . "autohint")
                          (:globaladvance . "globaladvance")
                          (:file . "file")
                          (:index . "index")
                          (:ftface . "ftface")
                          (:rasterizer . "rasterizer")
                          (:outline . "outline")
                          (:scalable . "scalable")
                          (:scale . "scale")
                          (:symbol . "symbol")
                          (:color . "color")
                          (:dpi . "dpi")
                          (:rgba . "rgba")
                          (:lcdfilter . "lcdfilter")
                          (:minspace . "minspace")
                          (:charset . "charset")
                          (:lang . "lang")
                          (:fontversion . "fontversion")
                          (:capability . "capability")
                          (:fontformat . "fontformat")
                          (:embolden . "embolden")
                          (:embeddedbitmap . "embeddedbitmap")
                          (:decorative . "decorative")
                          (:fontfeatures . "fontfeatures")
                          (:namelang . "namelang")
                          (:prgname . "prgname")
                          (:hash . "hash")
                          (:postscriptname . "postscriptname")))

(defmacro with-new-pattern ((sym) &body body)
  (alexandria:with-gensyms (pattern)
    `(let ((,pattern (fc-pattern-create)))
       (unwind-protect
            (let ((,sym ,pattern))
              ,@body)
         (fc-pattern-destroy ,pattern)))))

(defmacro with-new-char-set ((sym) &body body)
  (alexandria:with-gensyms (charset)
    `(let ((,charset (fc-char-set-create)))
       (unwind-protect
            (let ((,sym ,charset))
              ,@body)
         (fc-char-set-destroy ,charset)))))

(defun find-config ()
  (unless *config*
    (init-fontconfig))
  *config*)

(defun error-if-fail (result)
  (when (zerop result)
    (error 'fontconfig-error)))

(defun init-fontconfig ()
  (unless *config*
    (setq *config* (fc-init-load-config-and-fonts)))
  nil)

(defun get-version ()
  (fc-get-version))

(defun init-reinitialize ()
  (error-if-fail (fc-init-reinitialize)))

(defun init-bring-up-to-date ()
  (error-if-fail (fc-init-bring-up-to-date)))

(defun add-string-value-to-pattern (pattern key value)
  (cffi:with-foreign-strings ((s value :encoding :utf-8))
    (error-if-fail (fc-pattern-add-string pattern key s))))

(defun add-list-value-to-pattern (pattern key value)
  (ecase (car value)
    (:charset (with-new-char-set (charset)
                (dolist (ch (cdr value))
                  (error-if-fail (fc-char-set-add-char charset (etypecase ch
                                                                 (integer ch)
                                                                 (character (char-code ch))))))
                (error-if-fail (fc-pattern-add-char-set pattern key charset))))))

(defun add-value-to-pattern (pattern key value)
  (check-type key string)
  (etypecase value
    (string (add-string-value-to-pattern pattern key value))
    (integer (error-if-fail (fc-pattern-add-integer pattern key value)))
    (keyword (ecase value
               (:true (fc-pattern-add-bool pattern key 1))
               (:false (fc-pattern-add-bool pattern key 0))))
    (list (add-list-value-to-pattern pattern key value))))

(defun charset->lisp (charset-native)
  (cffi:with-foreign-objects ((bitmap 'fc-char32 +fc-charset-map-size+)
                              (next 'fc-char32))
    (let ((result (fc-char-set-first-page charset-native bitmap next)))
      (loop
        until (eql result +fc-charset-done+)
        collect (list result (cffi:foreign-array-to-lisp bitmap `(:array fc-char32 ,+fc-charset-map-size+)))
        do (setq result (fc-char-set-next-page charset-native bitmap next))))))

(defun str-list->lisp (str-list)
  (fc-str-list-first str-list)
  (loop
    for ptr = (fc-str-list-next str-list)
    until (cffi:null-pointer-p ptr)
    collect (cffi:foreign-string-to-lisp ptr :encoding :utf-8)))

(defun str-set->lisp (strset)
  (let ((str-list (fc-str-list-create strset)))
    (unwind-protect
         (str-list->lisp str-list)
      (fc-str-list-done str-list))))

(defun langset->lisp (langset)
  (str-set->lisp (fc-lang-set-get-langs langset)))

(defun value->lisp (value)
  (labels ((getslot (name)
             (cffi:foreign-slot-value value '(:struct fc-value) name)))
    (ecase (cffi:foreign-slot-value value '(:struct fc-value) 'type)
      (:fc-type-unknown :type-unknown)
      (:fc-type-void nil)
      (:fc-type-integer (getslot 'value-int))
      (:fc-type-double (getslot 'value-double))
      (:fc-type-string (values (cffi:foreign-string-to-lisp (getslot 'value-fchar8) :encoding :utf-8)))
      (:fc-type-bool (if (zerop (getslot 'value-bool)) nil t))
      (:fc-type-matrix :matrix-not-implemented)
      (:fc-type-char-set (charset->lisp (getslot 'value-char-set)))
      (:fc-type-ft-face :ft-face-not-implemented)
      (:fc-type-lang-set (langset->lisp (getslot 'value-lang-set)))
      (:fc-type-range :range-not-implemented))))

(defun pattern-get-internal (p object index)
  (cffi:with-foreign-objects ((value '(:struct fc-value)))
    (let ((result (fc-pattern-get p object index value)))
      (case result
        (:fc-result-match (value->lisp value))
        (:fc-result-no-match :no-match)
        (:fc-result-no-id :no-id)
        (t (error 'fontconfig-match-error :status result))))))

(defun config-home ()
  (let ((result (fc-config-home)))
    (values (cffi:foreign-string-to-lisp result))))

(defun pattern-to-lisp (pattern fields)
  (loop
    for field in fields
    for v = (assoc field *property-names*)
    when v
      append (let* ((name (cdr v))
                    (first-result (pattern-get-internal pattern name 0)))
               (cond ((eq first-result :no-match)
                      (list (cons field first-result)))
                     ((eq first-result :no-id)
                      nil)
                     (t
                      (cons (cons field first-result)
                            (loop
                              for i from 1
                              for result = (pattern-get-internal pattern name i)
                              until (member result '(:no-id :no-result))
                              collect (cons field result))))))))

(defun fill-pattern-from-values (pattern values)
  (when values
    (loop
      for (key . value) in values
      do (add-value-to-pattern pattern (prop-to-name key) value))))

(defun unparse-pattern (pattern)
  (let ((result (fc-name-unparse pattern)))
    (prog1
        (cffi:foreign-string-to-lisp result)
      (cffi:foreign-funcall "free" :pointer result :void))))

(defun %match-for-pattern (pattern fields)
  (cffi:with-foreign-objects ((result 'fc-result))
    (let* ((matched (fc-font-match (find-config) pattern result))
           (result-obj (cffi:mem-ref result 'fc-result)))
      (unwind-protect
           (progn
             (unless (eq result-obj :fc-result-match)
               (error 'fontconfig-match-error :status result-obj))
             (pattern-to-lisp matched fields))
        (unless (cffi:null-pointer-p matched)
          (fc-pattern-destroy matched))))))

(defun match-font (values fields &key (kind :match-pattern))
  (with-new-pattern (pattern)
    (fill-pattern-from-values pattern values)
    (error-if-fail (fc-config-substitute (find-config) pattern (ecase kind
                                                                 (:match-pattern :fc-match-pattern)
                                                                 (:match-font :fc-match-font))))
    (fc-default-substitute pattern)
    (%match-for-pattern pattern fields)))

(defmacro with-object-set ((sym objects) &body body)
  (alexandria:once-only (objects)
    (alexandria:with-gensyms (object-set obj)
      `(let ((,object-set (fc-object-set-create)))
         (unwind-protect
              (progn
                (loop
                  for ,obj in ,objects
                  do (fc-object-set-add ,object-set ,obj))
                (let ((,sym ,object-set))
                  ,@body))
           (fc-object-set-destroy ,object-set))))))

(defun prop-to-name (prop)
  (let ((d (assoc prop *property-names*)))
    (unless d
      (error "Unknown property: ~s" prop))
    (cdr d)))

(defun font-list (pattern fields)
  (with-object-set (object-set (mapcar #'prop-to-name fields))
    (with-new-pattern (p)
      (fill-pattern-from-values p pattern)
      (let ((f (fc-font-list (find-config) p object-set)))
        (loop
          with n = (cffi:foreign-slot-value f '(:struct fc-font-set) 'nfont)
          with fonts-ptr = (cffi:foreign-slot-value f '(:struct fc-font-set) 'fonts)
          for i from 0 below n
          for font = (cffi:mem-aref fonts-ptr :pointer i)
          collect (pattern-to-lisp font fields))))))

(defun list-font-dirs ()
  (let ((dirs (fc-config-get-font-dirs (find-config))))
    (unwind-protect
         (str-list->lisp dirs)
      (fc-str-list-done dirs))))

(defun font-render-prepare (pattern font fields)
  (with-new-pattern (p)
    (fill-pattern-from-values p pattern)
    (with-new-pattern (f)
      (fill-pattern-from-values f font)
      (let ((result-pattern (fc-font-render-prepare (find-config) p f)))
        (pattern-to-lisp result-pattern fields)))))

(defun font-render-prepare-match (pattern font fields)
  (with-new-pattern (p)
    (fill-pattern-from-values p pattern)
    (with-new-pattern (f)
      (fill-pattern-from-values f font)
      (let ((result-pattern (fc-font-render-prepare (find-config) p f)))
        (unwind-protect
             (%match-for-pattern result-pattern fields)
          (fc-pattern-destroy result-pattern))))))

(defun internal-font-render-prepare (matched pattern fields)
  (with-new-pattern (p)
    (fill-pattern-from-values p pattern)
    (let ((result-pattern (fc-font-render-prepare (find-config) p matched)))
      (unwind-protect
           (%match-for-pattern result-pattern fields)
        (fc-pattern-destroy result-pattern)))))

(defun font-render-prepare-match-with-font (font pattern fields)
  (with-new-pattern (p)
    (fill-pattern-from-values p font)
    (error-if-fail (fc-config-substitute (find-config) p :fc-match-font))
    (fc-default-substitute p)
    (cffi:with-foreign-objects ((result 'fc-result))
    (let* ((matched (fc-font-match (find-config) p result))
           (result-obj (cffi:mem-ref result 'fc-result)))
      (unwind-protect
           (progn
             (unless (eq result-obj :fc-result-match)
               (error 'fontconfig-match-error :status result-obj))
             (internal-font-render-prepare matched pattern fields))
        (unless (cffi:null-pointer-p matched)
          (fc-pattern-destroy matched)))))))

(defun query-freetype (file index fields)
  (cffi:with-foreign-objects ((count :int))
    (let ((pattern (fc-freetype-query file index (cffi:null-pointer) count)))
      (when (cffi:null-pointer-p pattern)
        (error "Unable to load file: ~s" file))
      (unwind-protect
           (pattern-to-lisp pattern fields)
        (fc-pattern-destroy pattern)))))

(defun app-font-add-dir (dir)
  (cffi:with-foreign-string (name (namestring dir) :encoding :utf-8)
    (let ((result (fc-config-app-font-add-dir (find-config) name)))
      (when (zerop result)
        (error "Unable to add font dir: ~s" dir)))))

(defun app-font-add-file (file)
  (cffi:with-foreign-string (name (namestring file) :encoding :utf-8)
    (let ((result (fc-config-app-font-add-file (find-config) name)))
      (when (zerop result)
        (error "Unable to add font file: ~s" file)))))

(deftype unicode-codepoint () '(integer 0 #.(* (expt 2 16) 17)))

(defun charset-contains-char-p (charset char)
  (let ((code (etypecase char
                (integer char)
                (character (char-code char)))))
    (loop
      for entry in charset
      for bitmap = (cadr entry)
      for bitmap-start of-type unicode-codepoint = (car entry)
      for bitmap-end of-type unicode-codepoint = (+ bitmap-start (* (length bitmap) 32))
      when (and (>= code bitmap-start)
                (< code bitmap-end))
        return (let ((start (- code bitmap-start))
                     (bitmap (cadr entry)))
                 (multiple-value-bind (word index)
                     (truncate start 32)
                   (plusp (ldb (byte 1 index) (aref bitmap word)))))
      until (<= code bitmap-end))))
