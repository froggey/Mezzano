;; NB: This test file is supposed to work using old defsystems:
;; not just ASDF 2.26, but also legacy defsystems from Allegro, Genera, LispWorks

(unless (find-package :asdf-test)
  (load (merge-pathnames
         (make-pathname :defaults *load-pathname*
                        :name "script-support" :directory '(:relative :back))
         *load-pathname*)))

(unless (find-package :asdf)
  (asdf-test::load-asdf)
  (asdf-test::frob-packages)
  (use-package :uiop :asdf))

(in-package :asdf-test)

(DBG :foo)

(defparameter *eval-notes* ())
(defun note-eval (when file)
  (format t "~&XXX ~S ~S~%" when file)
  (push `(,when ,file #|,*load-pathname* ,*compile-file-pathname*|#) *eval-notes*))
(defun eval-notes ()
  (prog1 (reverse *eval-notes*) (setf *eval-notes* nil)))
(defmacro eval-note (&optional x)
  `(progn
     (eval-when (:compile-toplevel) (note-eval :compile-toplevel ',x))
     (eval-when (:load-toplevel) (note-eval :load-toplevel ',x))
     (eval-when (:execute) (note-eval :execute ',x))))


(eval-note :tsp)

(defvar *tsp* (asdf::pathname-directory-pathname *load-pathname*))
(defparameter *defsystems* '(#+(or allegro genera lispworks) :native
                             #+mk-defsystem :mk-defsystem
                             #+asdf :asdf))
(defvar *default-defsystem* (first *defsystems*))
(defvar asdf::*asdf-cache* nil) ;; if defparameter instead of defvar, disable any surrounding cache
(defvar asdf::*asdf-session* nil) ;; if defparameter instead of defvar, disable any surrounding cache

(defun lisppath (filename) (asdf::subpathname *tsp* filename))
(defun faslpath (lisppath &optional (defsystem *default-defsystem*))
  (funcall
   (if (and (eq defsystem :asdf) (fboundp 'asdf::compile-file-pathname*))
       'asdf::compile-file-pathname*
       'compile-file-pathname)
   (etypecase lisppath
     (pathname lisppath)
     (string (lisppath lisppath)))))


(defun use-cache-p (defsystem)
  (and (eq defsystem :asdf)
       (cond
         ((asdf:version-satisfies (asdf:asdf-version) "3.1.7.30")
          asdf::*asdf-session*)
         ((asdf:version-satisfies (asdf:asdf-version) "2.27")
          asdf::*asdf-cache*))))

#+allegro
(excl:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp*)
  (:definitions
   "file1.lisp"
   "file2.lisp"))

#+genera
(sct:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp* :patchable nil)
  (:definitions
   "file1.lisp"
   "file2.lisp"))

#+lispworks
(scm:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp*)
  :members ("file1" "file2")
  :rules ((:in-order-to :compile ("file2")
           (:caused-by (:compile "file1"))
           (:requires (:load "file1")))))

#+asdf
(asdf:defsystem :test-stamp-propagation
  :pathname #.*tsp* :source-file nil
  :serial t
  :components
  ((:file "file1")
   (:file "file2")))

#+mk-defsystem
(mk:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp* :patchable nil)
  (:serial
   "file1.lisp"
   "file2.lisp"))

(defun reload (&optional (defsystem *default-defsystem*))
  (setf *eval-notes* nil)
  (setf *compile-verbose* t *load-verbose* t)
  (ecase defsystem
    #+asdf
    (:asdf
     (note-eval :compiling :system)
     (unless (use-cache-p :asdf) ;; faking the cache only works for one plan
       (asdf:compile-system :test-stamp-propagation))
     (note-eval :loading :system)
     (asdf:load-system :test-stamp-propagation))
    #+mk-defsystem
    (:mk-defsystem
     (note-eval :compiling :system)
     (mk:compile-system :test-stamp-propagation)
     (note-eval :loading :system)
     (mk:load-system :test-stamp-propagation))
    (:native
     (note-eval :compiling :system)
     #+allegro (excl:compile-system :test-stamp-propagation)
     #+lispworks (scm:compile-system :test-stamp-propagation)
     #+genera (sct:compile-system :test-stamp-propagation)
     (note-eval :loading :system)
     #+allegro (excl:load-system :test-stamp-propagation)
     #+lispworks (scm:load-system :test-stamp-propagation)
     #+genera (sct:load-system :test-stamp-propagation)))
  (let ((n (eval-notes)))
    (format t "~&EVAL-NOTES ~S~%" n)
    n))

(defun clear-sys (&optional (defsystem *default-defsystem*))
  #+asdf
  (when (eq defsystem :asdf)
    (asdf:clear-system :test-stamp-propagation)
    (asdf:defsystem :test-stamp-propagation
      :pathname #.*tsp* :source-file nil
      :serial t
      :components
      ((:file "file1")
       (:file "file2")))))

(defun touch (filename)
  #+genera filename ;; TODO: do something with it!
  #-genera
  (uiop:run-program `("touch" ,(uiop:native-namestring filename))
                    :output t :error-output t))

(defun clear-fasls (&optional (defsystem *default-defsystem*))
  (loop :for file :in '("file1.lisp" "file2.lisp")
        :for faslpath = (faslpath file defsystem)
        :do (if (use-cache-p defsystem)
                (mark-file-deleted faslpath)
                (delete-file-if-exists faslpath))))

(defun sanitize-log (log)
  (remove-duplicates
   (remove '(:loading :system) log :test 'equal)
   :test 'equal :from-end t))

(defun adjust-stamp-cache (base l1 f1 l2 f2)
  (clrhash (asdf::asdf-cache))
  (touch-file (lisppath "file1.lisp") :timestamp base :offset l1)
  (touch-file (faslpath "file1.lisp") :timestamp base :offset f1)
  (dolist (l (asdf:output-files 'asdf:compile-op '(:test-stamp-propagation "file1")))
    (touch-file l :timestamp base :offset f1))
  (touch-file (lisppath "file2.lisp") :timestamp base :offset l2)
  (touch-file (faslpath "file2.lisp") :timestamp base :offset f2)
  (dolist (l (asdf:output-files 'asdf:compile-op '(:test-stamp-propagation "file2")))
    (touch-file l :timestamp base :offset f2)))

(defun test-defsystem (&optional (defsystem *default-defsystem*))
  (format t "~%~%Testing stamp propagation by defsystem ~S~%" defsystem)
  #+(or allegro clisp)
  (progn (DBG "removing any old fasls from another flavor of the implementation")
         (clear-fasls defsystem))
  (when (use-cache-p defsystem)
    (adjust-stamp-cache (file-write-date (lisppath "file1.lisp")) -1000 -10000 -1000 -10000))
  (DBG "loading system")
  (reload defsystem)
  (clear-sys defsystem)
  (cond
    ((use-cache-p defsystem)
     (DBG "marking all files old but first source file, and reloading")
     (adjust-stamp-cache (file-write-date (lisppath "file1.lisp")) 0 -1000 -1000 -1000))
    (t
     (DBG "touching first source file and reloading")
     (sleep #-os-windows 3 #+os-windows 5)
     (touch (lisppath "file1.lisp"))))
  (DBG "defsystem should recompile & reload everything")
  (assert-equal (sanitize-log (reload defsystem))
                '((:compiling :system) (:compile-toplevel :file1) (:load-toplevel :file1)
                  (:compile-toplevel :file2) (:load-toplevel :file2)))
  (clear-sys defsystem)
  (cond
    ((use-cache-p defsystem)
     (DBG "marking the old fasl new, the second one up to date")
     (adjust-stamp-cache (file-write-date (lisppath "file1.lisp")) 100 500 100 100))
    (t
     (DBG "touching first fasl file and reloading")
     (sleep #-os-windows 3 #+os-windows 5)
     (touch (faslpath "file1.lisp" defsystem))))
  (DBG "defsystem should reload it, recompile & reload the other")
  (assert-equal (sanitize-log (reload defsystem))
                '((:compiling :system) (:load-toplevel :file1)
                  (:compile-toplevel :file2) (:load-toplevel :file2)))
  (DBG "cleaning up")
  (clear-fasls defsystem))

;; The test should work on ASDF3 and later:
;; (this is THE bug that motivated the rewrite from ASDF 2 to ASDF 3).
(nest
 #-asdf3 (signals error)
 ;; TODO: figure out why ABCL and XCL require this with-asdf-session.
 #+(and asdf3.3 (or abcl xcl)) (with-asdf-session () (load-system "asdf"))
 (test-defsystem :asdf))

;; Genera's and Lispworks' defsystem have a bug fix, though
;; users need to manually specify non-default dependencies
#+(or genera lispworks)
(test-defsystem :native)

;; Allegro's defsystem has the bug
#+(or allegro)
(signals error (test-defsystem :native))

;; MK-DEFSYSTEM has the bug
#+mk-defsystem
(signals error (test-defsystem :mk-defsystem))
