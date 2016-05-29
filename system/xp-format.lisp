(in-package :mezzano.xp)

;                        ---- COMPILED FORMAT ----

;Note that compiled format strings always print through xp streams even if
;they don't have any xp directives in them.  As a result, the compiled code
;can depend on the fact that the stream being operated on is an xp
;stream not an ordinary one.

(eval-when (:compile-toplevel :load-toplevel :execute)

(proclaim '(special *string* *used-args* *used-outer-args* *used-initial*
                    *get-arg-carefully* *inner-end* *outer-end* *at-top*))

(defvar *fn-table* (make-hash-table) "used to access fns for commands")

;Each of these functions expect to get called with two arguments
;start and end.  Start points to the first character after the ~
;marking the command.  End points to the first character after the
;command.  This includes the matching end command for paired commands.

(defmacro def-format-handler (char args &body body)
  (let ((name (intern (concatenate 'string "FORMAT-" (string char)) :mezzano.xp)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ,args ,@ body)
       (setf (gethash (char-upcase ,char) *fn-table*) (function ,name))
       (setf (gethash (char-downcase ,char) *fn-table*) (function ,name)))))

;Definitions of the forms used in the code created by PARSE.
;Note these functions assume the stream is in the var XP and is an xp stream,

; INITIAL holds the initial value of ARGS (for ~@*).
;Initial is always bound to (args) if it is bound at all.
;Note this uses args, but only when actually binding

(defun initial () (setq *used-initial* T) 'init)

(defmacro bind-initial (&body code)
  `(let* ((*used-initial* nil)
          (body (progn ,@ code)))
     (if *used-initial* (make-binding 'init (args) body) body)))

; ARGS holds the current argument list
;The val bound to args must always be computed (to use it up) even if args is not used.

(defun args () (setq *used-args* T) 'args)

(defmacro bind-args (doit? val &body code)
  (if (eq doit? T)
      `(let* ((val ,val)
              (*used-args* nil)
              (body (progn ,@ code)))
         (if *used-args* (make-binding 'args val body) (cons val body)))
      `(flet ((code () ,@ code))
         (if (not ,doit?) (code) ;important bindings not done if not doit?
             (let* ((val ,val)
                    (*used-args* nil)
                    (body (code)))
               (if *used-args* (make-binding 'args val body) (cons val body)))))))

(defun outer-args () (setq *used-outer-args* T) 'outer-args)

(defmacro bind-outer-args (&body code)
  `(let* ((*used-outer-args* nil)
          (body (progn ,@ code)))
     (if *used-outer-args* (make-binding 'outer-args (args) body) body)))

(defmacro maybe-bind (doit? var val &body code)
  `(let ((body (progn ,@ code)))
     (if ,doit? (make-binding ,var ,val body) body)))

(defun make-binding (var value body)
  `((let ((,var ,value)) ,@ body)))

(defun num-args () `(length ,(args)))

(defun get-arg ()
  (if *get-arg-carefully*
      (if *at-top* `(pprint-pop+top ,(args) xp) `(pprint-pop+ ,(args) xp))
      `(pop ,(args))))

(defmacro pprint-pop+ (args xp)
  `(if (pprint-pop-check+ ,args ,xp)
       (return-from logical-block nil)
       (pop ,args)))

(defun pprint-pop-check+ (args xp)
  (incf *current-length*)
  (cond ((not (listp args))  ;must be first so supersedes length abbrev
         (write-string++ ". " xp 0 2)
         (write+ args xp)
         T)
        ((and *print-length* ;must supersede circle check
              (not (< *current-length* *print-length*)))
         (write-string++ "..." xp 0 3)
         (setq *abbreviation-happened* T)
         T)
        ((and *circularity-hash-table* (not (zerop *current-length*)))
         (case (circularity-process xp args T)
           (:first ;; note must inhibit rechecking of circularity for args.
                   (write+ (cons (car args) (cdr args)) xp) T)
           (:subsequent T)
           (T nil)))))

(defmacro pprint-pop+top (args xp)
  `(if (pprint-pop-check+top ,args ,xp)
       (return-from logical-block nil)
       (pop ,args)))

(defun pprint-pop-check+top (args xp)
  (incf *current-length*)
  (cond ((not (listp args))  ;must be first so supersedes length abbrev
         (write-string++ ". " xp 0 2)
         (write+ args xp)
         T)
        ((and *print-length* ;must supersede circle check
              (not (< *current-length* *print-length*)))
         (write-string++ "..." xp 0 3)
         (setq *abbreviation-happened* T)
         T)))

(defun literal (start end)
  (let ((sub-end nil) next-newline (result nil))
    (loop (setq next-newline
                (position #\newline *string* :start start :end end))
          (setq sub-end (if next-newline next-newline end))
          (when (< start sub-end)
            (push (if (= start (1- sub-end))
                      `(write-char++ ,(aref *string* start) xp)
                      `(write-string++ ,(subseq *string* start sub-end) xp
                                    ,0 ,(- sub-end start)))
                  result))
          (when (null next-newline) (return nil))
          (push `(pprint-newline+ :unconditional xp) result)
          (setq start (1+ sub-end)))
    (if (null (cdr result)) (car result) (cons 'progn (nreverse result)))))

;This is available for putting on #".

(proclaim '(special *default-package*))

(defun format-string-reader (stream sub-char arg)
    (declare (ignore arg))
  (unread-char sub-char stream)
  `(function
    (lambda (s &rest args)
      (formatter-in-package ,(read stream) ,(package-name *package*)))))

(defmacro formatter-in-package (string reader-package)
  (formatter-fn string reader-package))

(defmacro formatter (string)
  `(function
    (lambda (s &rest args)
      (formatter-in-package ,string "CL-USER"))))

(defun formatter-fn (*string* *default-package*)
  (or (catch :format-compilation-error
        `(apply (function maybe-initiate-xp-printing)
                (function
                 (lambda (xp &rest args)
                  ,@(bind-initial
                     `((block top
                         ,@(let ((*get-arg-carefully* nil)
                                 (*at-top* t)
                                 (*inner-end* 'top)
                                 (*outer-end* 'top))
                             (compile-format 0 (length *string*))))))
                  (if ,(args) (copy-list ,(args))))) ;needed by symbolics.
                s args))
      `(apply #'format s *string* args)))

;The business with the catch above allows many (formatter "...") errors to be
;reported in a file without stopping the compilation of the file.

(defun maybe-compile-format-string (string force-fn?)
  (if (not (or force-fn? (fancy-directives-p string))) string
      (eval `(formatter ,string))))

;COMPILE-FORMAT gets called to turn a bit of format control string into code.

(defvar *testing-errors* nil "Used only when testing XP")

(defun err (id msg i)
  (if *testing-errors* (throw :testing-errors (list id i)))
  (warn "XP: cannot compile format string ~%~A~%~S~%~V@T|"
        msg *string* (1+ i))
  (throw :format-compilation-error nil))

(defun position-in (set start)
  (position-if (lambda (c) (find c set)) *string* :start start))

(defun position-not-in (set start)
  (position-if-not (lambda (c) (find c set)) *string* :start start))

(defun next-directive1 (start end)
  (let ((i (position #\~ *string* :start start :end end)) j)
    (when i
      (setq j (params-end (1+ i)))
      (when (char= (aref *string* j) #\/)
        (setq j (position #\/ *string* :start (1+ j) :end end))
        (when (null j)
          (err 3 "Matching / missing" (position #\/ *string* :start start)))))
    (values i j)))

(defun params-end (start) ;start points just after ~
  (let ((j start) (end (length *string*)))
    (loop
      (setq j (position-not-in "+-0123456789,Vv#:@" j))
      (when (null j) (err 1 "missing directive" (1- start)))
      (when (not (eq (aref *string* j) #\')) (return j))
      (incf j)
      (if (= j end) (err 2 "No character after '" (1- j)))
      (incf j))))

;Only called after correct parse is known.

(defun directive-start (end) ;end points at characters after params
  (loop
    (setq end (position #\~ *string* :end end :from-end T))
    (when (or (zerop end) (not (eq (aref *string* (1- end)) #\')))
      (return end))
    (decf end)))

(defun next-directive (start end)
  (let (i j ii k count c close
        (pairs '((#\( . #\)) (#\[ . #\]) (#\< . #\>) (#\{ . #\}))))
    (multiple-value-setq (i j) (next-directive1 start end))
    (when i
      (setq c (aref *string* j))
      (setq close (cdr (assoc c pairs)))
      (when close
        (setq k j count 0)
        (loop
          (multiple-value-setq (ii k) (next-directive1 k end))
          (when (null ii) (err 4 "No matching close directive" j))
          (when (eql (aref *string* k) c) (incf count))
          (when (eql (aref *string* k) close) (decf count)
            (when (minusp count) (setq j k) (return nil))))))
    (values c i j)))

;breaks things up at ~; directives.

(defun chunk-up (start end)
  (let ((positions (list start)) (spot start))
    (loop
      (multiple-value-bind (c i j) (next-directive spot end)
        (declare (ignore i))
        (when (null c) (return (nreverse (cons end positions))))
        (when (eql c #\;) (push (1+ j) positions))
        (setq spot j)))))

(defun fancy-directives-p (*string*)
  (let (i (j 0) (end (length *string*)) c)
    (loop
      (multiple-value-setq (i j) (next-directive1 j end))
      (when (not i) (return nil))
      (setq c (aref *string* j))
      (when (or (find c "_Ii/Ww") (and (find c ">Tt") (colonp j)))
        (return T)))))

(defun num-args-in-args (start &optional (err nil))
  (let ((n 0) (i (1- start)) c)
    (loop
      (setq i (position-not-in "+-0123456789," (1+ i)))
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (incf n))
            ((char= c #\#)
             (when err
               (err 21 "# not allowed in ~~<...~~> by (formatter \"...\")" start))
             (return nil))
            ((char= c #\') (incf i))
            (T (return n))))))

(defun compile-format (start end)
  (let ((result nil))
    (prog (c i j fn)
     L(multiple-value-setq (c i j) (next-directive start end))
      (when (if (null c) (< start end) (< start i))
        (push (literal start (if i i end)) result))
      (when (null c) (return (nreverse result)))
      (when (char= c #\newline)
        (multiple-value-bind (colon atsign)
            (parse-params (1+ i) nil :nocolonatsign T)
          (when atsign (push `(pprint-newline+ :unconditional xp) result))
          (incf j)
          (when (not colon)
            (setq j (position-if-not
                      (lambda (c)
                          (or (char= c #\tab) (char= c #\space)))
                      *string* :start j :end end))
            (when (null j) (setq j end)))
          (setq start j)
          (go L)))
      (setq fn (gethash c *fn-table*))
      (when (null fn) (err 5 "Unknown format directive" j))
      (incf j)
      (push (funcall fn (1+ i) j) result)
      (setq start j)
      (go L))))

;This gets called with start pointing to the character after the ~ that
;starts a command.  Defaults, is a list of default values for the
;parameters.  Max is the maximum number of parameters allowed.  Nocolon,
;noatsign, nocolonatsign can be used to specify what colon atsign
;combinations are permitted. Parse params returns three values, colon?,
;atsign? and a list of code chunks that correspond to the parameters
;specified.

(defun parse-params (start defaults &key (max (length defaults))
                     (nocolon nil) (noatsign nil) (nocolonatsign nil))
  (let ((colon nil) (atsign nil) (params nil) (i start) j c)
    (loop
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (push (get-arg) params) (incf i))
            ((char= c #\#) (push (num-args) params) (incf i))
            ((char= c #\') (incf i) (push (aref *string* i) params) (incf i))
            ((char= c #\,) (push nil params))
            (T (setq j (position-not-in "+-0123456789" i))
               (if (= i j) (return nil))
               (push (parse-integer *string* :start i :end j :radix 10.) params)
               (setq i j)))
      (if (char= (aref *string* i) #\,) (incf i) (return nil)))
    (setq params (nreverse params))
    (do ((ps params (cdr ps))
         (ds defaults (cdr ds))
         (nps nil))
        ((null ds) (setq params (nreconc nps ps)))
      (push (cond ((or (null ps) (null (car ps))) (car ds))
                  ((not (consp (car ps))) (car ps))
                  (T `(cond (,(car ps)) (T ,(car ds)))))
            nps))
    (if (and max (< max (length params))) (err 6 "Too many parameters" i))
    (loop
      (setq c (aref *string* i))
      (cond ((char= c #\:)
             (if colon (err 7 "Two colons specified" i))
             (setq colon T))
            ((char= c #\@)
             (if atsign (err 8 "Two atsigns specified" i))
             (setq atsign T))
            (T (return nil)))
      (incf i))
    (if (and colon nocolon) (err 9 "Colon not permitted" i))
    (if (and atsign noatsign) (err 10 "Atsign not permitted" i))
    (if (and colon atsign nocolonatsign)
        (err 11 "Colon and atsign together not permitted" i))
    (values colon atsign params)))

;Both these only called if correct parse already known.

(defun colonp (j) ;j points to directive name
  (or (eql (aref *string* (1- j)) #\:)
      (and (eql (aref *string* (1- j)) #\@)
           (eql (aref *string* (- j 2)) #\:))))

(defun atsignp (j) ;j points to directive name
  (or (eql (aref *string* (1- j)) #\@)
      (and (eql (aref *string* (1- j)) #\:)
           (eql (aref *string* (- j 2)) #\@))))

(def-format-handler #\/ (start end)
  (multiple-value-bind (colon atsign params) (parse-params start nil :max nil)
    (let* ((whole-name-start (1+ (params-end start)))
           (colon-pos (position #\: *string* :start whole-name-start :end (1- end)))
           (pkg (find-package
                  (if colon-pos
                      (string-upcase (subseq *string* whole-name-start colon-pos))
                      *default-package*)))
           (name-start (cond ((null colon-pos) whole-name-start)
                             ((and (< colon-pos (1- end))
                                   (char= #\: (aref *string* (1+ colon-pos))))
                              (+ colon-pos 2))
                             (T (1+ colon-pos))))
           (fn (intern (string-upcase (subseq *string* name-start (1- end))) pkg)))
      (if (not (find-if #'consp params))
          `(funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ params)
          (let ((vars (mapcar (lambda (arg)
                                  (declare (ignore arg))
                                  (gentemp))
                              params)))
            `(let ,(mapcar #'list vars params)
               (funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ vars)))))))

(def-format-handler #\A (start end)
  (if (not (= end (1+ start))) (simple-directive start end)
      `(let ((*print-escape* nil))
         (write+ ,(get-arg) XP))))

(def-format-handler #\S (start end)
  (if (not (= end (1+ start))) (simple-directive start end)
      `(let ((*print-escape* T))
         (write+ ,(get-arg) XP))))

;The basic Format directives "DBOXRCFEG$".  The key thing about all of
;these directives is that they just get a single arg and print a chunk of
;stuff.  Further they are complex enough that I just call the standard
;definition of FORMAT to get the work done.  What should really be being
;called is the internal routine that FORMAT uses to do the corresponding
;work.  However, this cannot be done in a portable way.

(def-format-handler #\D (start end) (simple-directive start end))
(def-format-handler #\B (start end) (simple-directive start end))
(def-format-handler #\O (start end) (simple-directive start end))
(def-format-handler #\X (start end) (simple-directive start end))
(def-format-handler #\R (start end) (simple-directive start end))
(def-format-handler #\C (start end) (simple-directive start end))
(def-format-handler #\F (start end) (simple-directive start end))
(def-format-handler #\E (start end) (simple-directive start end))
(def-format-handler #\G (start end) (simple-directive start end))
(def-format-handler #\$ (start end) (simple-directive start end))

(defun simple-directive (start end)
  (let ((n (num-args-in-args start)))
    (if n `(using-format xp ,(subseq *string* (1- start) end)
                         ,@ (copy-tree (make-list (1+ n) :initial-element (get-arg))))
        (multiple-value-bind (colon atsign params)
            (parse-params start nil :max 8)
          (let* ((arg-str (subseq "v,v,v,v,v,v,v,v" 0
                                  (max 0 (1- (* 2 (length params))))))
                 (str (concatenate 'string "~"
                                   arg-str
                                   (if colon ":" "")
                                   (if atsign "@" "")
                                   (subseq *string* (1- end) end))))
            `(using-format xp ,str ,@ params ,(get-arg)))))))

(defun using-format (xp string &rest args)
  (let ((result (apply #'cl:format nil string args)))
    (write-string+ result xp 0 (length result))))

;Format directives that get open coded "P%&~|T*?^"

(def-format-handler #\P (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
  (let ((arg (if colon `(car (backup-in-list 1 ,(initial) ,(args))) (get-arg))))
    (if atsign
        `(if (not (eql ,arg 1)) (write-string++ "ies" xp 0 3) (write-char++ #\y xp))
        `(if (not (eql ,arg 1)) (write-char++ #\s XP))))))

(def-format-handler #\% (start end) (declare (ignore end))
  (multiple-newlines start :unconditional))

(def-format-handler #\& (start end) (declare (ignore end))
  (multiple-newlines start :fresh))

(defun multiple-newlines (start kind)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon T :noatsign T)
      (declare (ignore colon atsign))
    (if (eql (car params) 1) `(pprint-newline+ ,kind xp)
        `(multiple-newlines1 xp ,kind ,(car params)))))

(defun multiple-newlines1 (xp kind num)
  (do ((n num (1- n))) ((not (plusp n)))
    (pprint-newline+ kind xp)
    (setq kind :unconditional)))

(def-format-handler #\| (start end) (declare (ignore end))
  (multiple-chars start #\Page))

(def-format-handler #\~ (start end) (declare (ignore end))
  (multiple-chars start #\~))

(defun multiple-chars (start char)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon t :noatsign t)
      (declare (ignore colon atsign))
    (if (eql (car params) 1) `(write-char++ ,char xp)
        `(multiple-chars1 xp ,(car params) ,char))))

(defun multiple-chars1 (xp num char)
  (do ((n num (1- n))) ((not (plusp n)))
    (write-char++ char xp)))

(def-format-handler #\T (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params) (parse-params start '(1 1))
    `(pprint-tab+ ,(if colon (if atsign :section-relative :section)
                             (if atsign :line-relative :line))
                  ,(pop params) ,(pop params) xp)))

(def-format-handler #\* (start end) (declare (ignore end))
  (if (atsignp (params-end start))
      (multiple-value-bind (colon atsign params)
          (parse-params start '(0) :nocolon t)
          (declare (ignore colon atsign))
        `(setq args (backup-to ,(car params) ,(initial) ,(args))))
      (multiple-value-bind (colon atsign params)
          (parse-params start '(1))
          (declare (ignore atsign))
        `(setq args
               ,(if colon `(backup-in-list ,(car params) ,(initial) ,(args))
                    `(nthcdr ,(car params) ,(args)))))))

;fancy stuff here, so will not get spurious indications of circularity.

(defun backup-in-list (num list some-tail)
  (backup-to (- (tail-pos list some-tail) num) list some-tail))

(defun backup-to (num list some-tail)
  (if (not *circularity-hash-table*) (nthcdr num list)
      (multiple-value-bind (pos share) (tail-pos list some-tail)
          (declare (ignore pos))
        (if (not (< num share)) (nthcdr num list)
            (do ((L (nthcdr num list) (cdr L))
                 (n (- share num) (1- n))
                 (R nil (cons (car L) R)))
                ((zerop n) (nreconc R L)))))))

;because of backup-to, a prefix of some-tail may have been copied (in which
;case it cannot share anything with list), but there is a cons in some-tail
;that is in list.  This can be used to determine the position of some-tail
;relative to list.  However, we have to be careful, because they both could
;be cdr recursive.

(defun tail-pos (list some-tail)
  (block outer
    (do ((n 0 (1+ n))
         (L list (cdr L)))
        (nil)
      (do ((m n (1- m))
           (ST some-tail (cdr ST)))
          (nil)
        (if (minusp m) (return nil))
        (if (eq ST L) (return-from outer (values m n)))))))

(def-format-handler #\? (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil :nocolon t)
      (declare (ignore colon))
    (if (not atsign) `(apply #'format xp ,(get-arg) ,(get-arg))
        `(let ((fn (process-format-string ,(get-arg) T)))
           (setq args (apply fn xp ,(args)))))))

(def-format-handler #\^ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 3 :noatsign t)
      (declare (ignore atsign))
    `(if ,(cond ((null params) `(null ,(if colon `(cdr ,(outer-args)) (args))))
                (t `(do-complex-^-test ,@ params)))
         (return-from ,(if colon *outer-end* *inner-end*) nil))))

(defun do-complex-^-test (a1 &optional (a2 nil) (a3 nil))
  (cond (a3 (and (<= a1 a2) (<= a2 a3)))
        (a2 (= a1 a2))
        (t (= 0 a1))))

;delimited pairs of format directives. "(){}[]<>;"

(def-format-handler #\[ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 1 :nocolonatsign T)
    (setq start (1+ (params-end start)))
    (let* ((chunks (chunk-up start end))
           (innards (do ((ns chunks (cdr ns))
                         (ms (cdr chunks) (cdr ms))
                         (result nil))
                        ((null ms) (return (nreverse result)))
                      (push (compile-format (car ns) (directive-start (car ms)))
                            result))))
      (cond (colon (when (not (= (length innards) 2))
                     (err 13 "Wrong number of clauses in ~~:[...~~]" (1- start)))
                   `(cond ((null ,(get-arg)) ,@ (car innards))
                          (T ,@ (cadr innards))))
            (atsign (when (not (= (length innards) 1))
                      (err 14 "Too many clauses in ~~@[...~~]" (1- start)))
                    `(cond ((car args) ,@ (car innards)) (T ,(get-arg))))
            (T (let* ((j -1) (len (- (length chunks) 2))
                      (else? (colonp (1- (nth len chunks)))))
                 `(case ,(if params (car params) (get-arg))
                    ,@(mapcar (lambda (unit)
                                  (incf j)
                                  `(,(if (and else? (= j len)) T j) ,@ unit))
                              innards))))))))

(def-format-handler #\( (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end start)))
    (setq end (directive-start end))
    `(progn (push-char-mode xp ,(cond ((and colon atsign) :UP)
                                      (colon :CAP1)
                                      (atsign :CAP0)
                                      (T :DOWN)))
            ,@(compile-format start end)
            (pop-char-mode xp))))

(def-format-handler #\; (start end) (declare (ignore start))
  (err 15 "~~; appears out of context" (1- end)))
(def-format-handler #\] (start end) (declare (ignore start))
  (err 16 "Unmatched closing directive" (1- end)))
(def-format-handler #\) (start end) (declare (ignore start))
  (err 17 "Unmatched closing directive" (1- end)))
(def-format-handler #\> (start end) (declare (ignore start))
  (err 18 "Unmatched closing directive" (1- end)))
(def-format-handler #\} (start end) (declare (ignore start))
  (err 19 "Unmatched closing directive" (1- end)))

(def-format-handler #\{ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(-1) :max 1)
    (let* ((force-once (colonp (1- end)))
           (n (car params))
           (bounded (not (eql n -1))))
      (setq start (1+ (params-end start)))
      (setq end (directive-start end))
      (car (maybe-bind bounded 'N n ;must be outermost if is V or #
             (maybe-bind (not (> end start)) 'FN  ;must be second
                         `(process-format-string ,(get-arg) T)
               (bind-args (not atsign) (get-arg)
                 `((prog () ,@(if force-once '((go S)))
                       L (if (null ,(args)) (return nil))
                       ,@(if force-once '(S))
                         ,@(if bounded '((if (= N 0) (return nil) (decf N))))
                         ,@(bind-outer-args
                             (bind-args colon (get-arg)
                               (bind-initial
                                 (let ((*get-arg-carefully*
                                         (and *get-arg-carefully* atsign))
                                       (*at-top* (and *at-top* atsign))
                                       (*outer-end* nil)
                                       (*inner-end* nil))
                                   (if (not colon)
                                       (if (not (> end start))
                                           `((setq args (apply FN xp ,(args))))
                                           (compile-format start end))
                                       (let ((*inner-end* 'inner))
                                         `((block inner
                                             ,@(if (not (> end start))
                                                   `((setq args (apply FN xp ,(args))))
                                                   (compile-format start end))))))))))
                         (go L))))))))))

(def-format-handler #\< (start end)
  (if (colonp (1- end))
      (handle-logical-block start end)
      (handle-standard-< start end)))

(defun handle-standard-< (start end)
  (let ((n (num-args-in-directive start end)))
    `(using-format xp ,(subseq *string* (1- start) end)
                   ,@ (copy-tree (make-list n :initial-element (get-arg))))))

(defun num-args-in-directive (start end)
  (let ((n 0) c i j)
    (incf n (num-args-in-args start T))
    (multiple-value-setq (j i) (next-directive1 start end))
    (loop
      (multiple-value-setq (c i j) (next-directive j end))
      (when (null c) (return n))
      (cond ((eql c #\;)
             (if (colonp j)
                 (err 22 "~~:; not supported in ~~<...~~> by (formatter \"...\")." j)))
            ((find c "*[^<_IiWw{Tt")
             (err 23 "~~<...~~> too complicated to be supported by (formatter \"...\")." j))
            ((eql c #\() (incf n (num-args-in-directive (1+ i) j)))
            ((find c "%&\|~") (incf n (num-args-in-args (1+ i) T)))
            ((eql c #\?)
             (when (atsignp j)
               (err 23 "~~<...~~> too complicated to be supported by (formatter \"...\")." j))
             (incf n 2))
            ((find c "AaSsDdBbOoXxRrCcFfEeGg$Pp")
             (incf n (1+ (num-args-in-args (1+ i) T))))))))

;The pretty-printing directives. "_IW<:>"

(def-format-handler #\_ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    `(pprint-newline+ ,(cond ((and colon atsign) :mandatory)
                             (colon :fill)
                             (atsign :miser)
                             (T :linear)) XP)))

(def-format-handler #\I (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0) :noatsign T)
      (declare (ignore atsign))
    `(pprint-indent+ ,(if colon :current :block) ,(car params) XP)))

(def-format-handler #\W (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (cond ((not (or colon atsign)) `(write+ ,(get-arg) XP))
          (T `(let (,@(if colon '((*print-pretty* T)))
                    ,@(if atsign '((*print-level* nil) (*print-length* nil))))
                (write+ ,(get-arg) XP))))))

(defun handle-logical-block (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end start)))
    (let* ((chunks (chunk-up start end))
           (on-each-line?
             (and (cddr chunks) (atsignp (1- (cadr chunks)))))
           (prefix
             (cond ((cddr chunks) (pop chunks)
                    (subseq *string* start (directive-start (car chunks))))
                   (colon "(")))
           (suffix
             (cond ((cddr chunks)
                    (subseq *string* (cadr chunks)
                            (directive-start (caddr chunks))))
                   (colon ")"))))
      (when (cdddr chunks) (err 24 "Too many subclauses in ~~<...~~:>" (1- start)))
      (when (and prefix (or (find #\~ prefix) (find #\newline prefix)))
        (err 25 "Prefix in ~~<...~~:> must be a literal string without newline" start))
      (when (and suffix (or (find #\~ suffix) (find #\newline suffix)))
        (err 26 "Suffix in ~~<...~~:> must be a literal string without newline"
             (cadr chunks)))
      (car (bind-args T (if atsign `(prog1 ,(args) (setq ,(args) nil)) (get-arg))
             (bind-initial
               `((pprint-logical-block+ (xp ,(args) ,prefix ,suffix ,on-each-line?
                                            ,(not (and *at-top* atsign)) ,atsign)
                   ,@(fill-transform (atsignp (1- end))
                       (let ((*get-arg-carefully* T)
                             (*at-top* (and *at-top* atsign))
                             (*inner-end* 'logical-block)
                             (*outer-end* 'logical-block))
                         (compile-format (car chunks)
                                         (directive-start (cadr chunks)))))))))))))

(defun check-block-abbreviation (xp args circle-check?)
  (cond ((not (listp args)) (write+ args xp) T)
        ((and *print-level* (> *current-level* *print-level*))
         (write-char++ #\# XP) (setq *abbreviation-happened* T) T)
        ((and *circularity-hash-table* circle-check?
              (eq (circularity-process xp args nil) :subsequent)) T)
        (T nil)))

(defun fill-transform (doit? body)
  (if (not doit?) body
      (mapcan (lambda (form)
                  (cond ((eq (car form) 'write-string++)
                         (fill-transform-literal (cadr form)))
                        ((eq (car form) 'write-char++)
                         (fill-transform-char (cadr form)))
                        (T (list form))))
              body)))

(defun fill-transform-char (char)
  (if (or (char= char #\space) (char= char #\tab))
      (list `(write-char++ ,char xp) '(pprint-newline+ :fill xp))
      `((write-char++ ,char xp))))

(defun fill-transform-literal (string)
  (flet ((white-space (c) (or (char= c #\space) (char= c #\tab))))
    (do ((index 0 end) (result) (end nil nil)) (nil)
      (let ((white (position-if #'white-space string :start index)))
        (when white
          (setq end (position-if-not #'white-space string :start (1+ white))))
        (when (null end)
          (setq end (length string)))
        (push `(write-string++ ,(subseq string index end) xp ,0 ,(- end index))
              result)
        (if white (push '(pprint-newline+ :fill xp) result))
        (if (null white) (return (nreverse result)))))))

 ) ;end of eval when for all (formatter "...") stuff.
