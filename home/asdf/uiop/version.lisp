(uiop/package:define-package :uiop/version
  (:recycle :uiop/version :uiop/utility :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:*uiop-version*
   #:parse-version #:unparse-version #:version< #:version<= ;; version support, moved from uiop/utility
   #:next-version
   #:deprecated-function-condition #:deprecated-function-name ;; deprecation control
   #:deprecated-function-style-warning #:deprecated-function-warning
   #:deprecated-function-error #:deprecated-function-should-be-deleted
   #:version-deprecation #:with-deprecation))
(in-package :uiop/version)

(with-upgradability ()
  (defparameter *uiop-version* "3.3.3.3")

  (defun unparse-version (version-list)
    "From a parsed version (a list of natural numbers), compute the version string"
    (format nil "~{~D~^.~}" version-list))

  (defun parse-version (version-string &optional on-error)
    "Parse a VERSION-STRING as a series of natural numbers separated by dots.
Return a (non-null) list of integers if the string is valid;
otherwise return NIL.

When invalid, ON-ERROR is called as per CALL-FUNCTION before to return NIL,
with format arguments explaining why the version is invalid.
ON-ERROR is also called if the version is not canonical
in that it doesn't print back to itself, but the list is returned anyway."
    (block nil
      (unless (stringp version-string)
        (call-function on-error "~S: ~S is not a string" 'parse-version version-string)
        (return))
      (unless (loop :for prev = nil :then c :for c :across version-string
                    :always (or (digit-char-p c)
                                (and (eql c #\.) prev (not (eql prev #\.))))
                    :finally (return (and c (digit-char-p c))))
        (call-function on-error "~S: ~S doesn't follow asdf version numbering convention"
                       'parse-version version-string)
        (return))
      (let* ((version-list
               (mapcar #'parse-integer (split-string version-string :separator ".")))
             (normalized-version (unparse-version version-list)))
        (unless (equal version-string normalized-version)
          (call-function on-error "~S: ~S contains leading zeros" 'parse-version version-string))
        version-list)))

  (defun next-version (version)
    "When VERSION is not nil, it is a string, then parse it as a version, compute the next version
and return it as a string."
    (when version
      (let ((version-list (parse-version version)))
        (incf (car (last version-list)))
        (unparse-version version-list))))

  (defun version< (version1 version2)
    "Given two version strings, return T if the second is strictly newer"
    (let ((v1 (parse-version version1 nil))
          (v2 (parse-version version2 nil)))
      (lexicographic< '< v1 v2)))

  (defun version<= (version1 version2)
    "Given two version strings, return T if the second is newer or the same"
    (not (version< version2 version1))))


(with-upgradability ()
  (define-condition deprecated-function-condition (condition)
    ((name :initarg :name :reader deprecated-function-name)))
  (define-condition deprecated-function-style-warning (deprecated-function-condition style-warning) ())
  (define-condition deprecated-function-warning (deprecated-function-condition warning) ())
  (define-condition deprecated-function-error (deprecated-function-condition error) ())
  (define-condition deprecated-function-should-be-deleted (deprecated-function-condition error) ())

  (defun deprecated-function-condition-kind (type)
    (ecase type
      ((deprecated-function-style-warning) :style-warning)
      ((deprecated-function-warning) :warning)
      ((deprecated-function-error) :error)
      ((deprecated-function-should-be-deleted) :delete)))

  (defmethod print-object ((c deprecated-function-condition) stream)
    (let ((name (deprecated-function-name c)))
      (cond
        (*print-readably*
         (let ((fmt "#.(make-condition '~S :name ~S)")
               (args (list (type-of c) name)))
           (if *read-eval*
               (apply 'format stream fmt args)
               (error "Can't print ~?" fmt args))))
        (*print-escape*
         (print-unreadable-object (c stream :type t) (format stream ":name ~S" name)))
        (t
         (let ((*package* (find-package :cl))
               (type (type-of c)))
           (format stream
                   (if (eq type 'deprecated-function-should-be-deleted)
                       "~A: Still defining deprecated function~:P ~{~S~^ ~} that promised to delete"
                       "~A: Using deprecated function ~S -- please update your code to use a newer API.~
~@[~%The docstring for this function says:~%~A~%~]")
                   type name (when (symbolp name) (documentation name 'function))))))))

  (defun notify-deprecated-function (status name)
    (ecase status
      ((nil) nil)
      ((:style-warning) (style-warn 'deprecated-function-style-warning :name name))
      ((:warning) (warn 'deprecated-function-warning :name name))
      ((:error) (cerror "USE FUNCTION ANYWAY" 'deprecated-function-error :name name))))

  (defun version-deprecation (version &key (style-warning nil)
                                        (warning (next-version style-warning))
                                        (error (next-version warning))
                                        (delete (next-version error)))
    "Given a VERSION string, and the starting versions for notifying the programmer of
various levels of deprecation, return the current level of deprecation as per WITH-DEPRECATION
that is the highest level that has a declared version older than the specified version.
Each start version for a level of deprecation can be specified by a keyword argument, or
if left unspecified, will be the NEXT-VERSION of the immediate lower level of deprecation."
    (cond
      ((and delete (version<= delete version)) :delete)
      ((and error (version<= error version)) :error)
      ((and warning (version<= warning version)) :warning)
      ((and style-warning (version<= style-warning version)) :style-warning)))

  (defmacro with-deprecation ((level) &body definitions)
    "Given a deprecation LEVEL (a form to be EVAL'ed at macro-expansion time), instrument the
DEFUN and DEFMETHOD forms in DEFINITIONS to notify the programmer of the deprecation of the function
when it is compiled or called.

Increasing levels (as result from evaluating LEVEL) are: NIL (not deprecated yet),
:STYLE-WARNING (a style warning is issued when used), :WARNING (a full warning is issued when used),
:ERROR (a continuable error instead), and :DELETE (it's an error if the code is still there while
at that level).

Forms other than DEFUN and DEFMETHOD are not instrumented, and you can protect a DEFUN or DEFMETHOD
from instrumentation by enclosing it in a PROGN."
    (let ((level (eval level)))
      (check-type level (member nil :style-warning :warning :error :delete))
      (when (eq level :delete)
        (error 'deprecated-function-should-be-deleted :name
               (mapcar 'second
                       (remove-if-not #'(lambda (x) (member x '(defun defmethod)))
                                      definitions :key 'first))))
      (labels ((instrument (name head body whole)
                 (if level
                     (let ((notifiedp
                            (intern (format nil "*~A-~A-~A-~A*"
                                            :deprecated-function level name :notified-p))))
                       (multiple-value-bind (remaining-forms declarations doc-string)
                           (parse-body body :documentation t :whole whole)
                         `(progn
                            (defparameter ,notifiedp nil)
                            ;; tell some implementations to use the compiler-macro
                            (declaim (inline ,name))
                            (define-compiler-macro ,name (&whole form &rest args)
                              (declare (ignore args))
                              (notify-deprecated-function ,level ',name)
                              form)
                            (,@head ,@(when doc-string (list doc-string)) ,@declarations
                                    (unless ,notifiedp
                                      (setf ,notifiedp t)
                                      (notify-deprecated-function ,level ',name))
                                    ,@remaining-forms))))
                     `(progn
                        (eval-when (:compile-toplevel :load-toplevel :execute)
                          (setf (compiler-macro-function ',name) nil))
                        (declaim (notinline ,name))
                        (,@head ,@body)))))
        `(progn
           ,@(loop :for form :in definitions :collect
               (cond
                 ((and (consp form) (eq (car form) 'defun))
                  (instrument (second form) (subseq form 0 3) (subseq form 3) form))
                 ((and (consp form) (eq (car form) 'defmethod))
                  (let ((body-start (if (listp (third form)) 3 4)))
                    (instrument (second form)
                                (subseq form 0 body-start)
                                (subseq form body-start)
                                form)))
                 (t
                  form))))))))
