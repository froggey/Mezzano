;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-garbage.lisp --- Trivial Garbage!
;;;
;;; This software is placed in the public domain by Luis Oliveira
;;; <loliveira@common-lisp.net> and is provided with absolutely no
;;; warranty.

#+xcvb (module ())

(defpackage #:trivial-garbage
  (:use #:cl)
  (:shadow #:make-hash-table)
  (:nicknames #:tg)
  (:export #:gc
           #:make-weak-pointer
           #:weak-pointer-value
           #:weak-pointer-p
           #:make-weak-hash-table
           #:hash-table-weakness
           #:finalize
           #:cancel-finalization)
  (:documentation
   "@a[http://common-lisp.net/project/trivial-garbage]{trivial-garbage}
    provides a portable API to finalizers, weak hash-tables and weak
    pointers on all major implementations of the Common Lisp
    programming language. For a good introduction to these
    data-structures, have a look at
    @a[http://www.haible.de/bruno/papers/cs/weak/WeakDatastructures-writeup.html]{Weak
    References: Data Types and Implementation} by Bruno Haible.

    Source code is available at
    @a[https://github.com/trivial-garbage/trivial-garbage]{github},
    which you are welcome to use for submitting patches and/or
    @a[https://github.com/trivial-garbage/trivial-garbage/issues]{bug
    reports}. Discussion takes place on
    @a[http://lists.common-lisp.net/cgi-bin/mailman/listinfo/trivial-garbage-devel]{trivial-garbage-devel
    at common-lisp.net}.

    @a[http://common-lisp.net/project/trivial-garbage/releases/]{Tarball
    releases} are available, but the easiest way to install this
    library is via @a[http://www.quicklisp.org/]{Quicklisp}:
    @code{(ql:quickload :trivial-garbage)}.

    @begin[Weak Pointers]{section}
    A @em{weak pointer} holds an object in a way that does not prevent
    it from being reclaimed by the garbage collector.  An object
    referenced only by weak pointers is considered unreachable (or
    \"weakly reachable\") and so may be collected at any time. When
    that happens, the weak pointer's value becomes @code{nil}.

    @aboutfun{make-weak-pointer}
    @aboutfun{weak-pointer-value}
    @aboutfun{weak-pointer-p}
    @end{section}

    @begin[Weak Hash-Tables]{section}
    A @em{weak hash-table} is one that weakly references its keys
    and/or values. When both key and value are unreachable (or weakly
    reachable) that pair is reclaimed by the garbage collector.

    @aboutfun{make-weak-hash-table}
    @aboutfun{hash-table-weakness}
    @end{section}

    @begin[Finalizers]{section}
    A @em{finalizer} is a hook that is executed after a given object
    has been reclaimed by the garbage collector.

    @aboutfun{finalize}
    @aboutfun{cancel-finalization}
    @end{section}"))

(in-package #:trivial-garbage)

;;;; GC

(defun gc (&key full verbose)
  "Initiates a garbage collection. @code{full} forces the collection
   of all generations, when applicable. When @code{verbose} is
   @em{true}, diagnostic information about the collection is printed
   if possible."
  (declare (ignorable verbose full))
  #+(or cmu scl) (ext:gc :verbose verbose :full full)
  #+sbcl (sb-ext:gc :full full)
  #+allegro (excl:gc (not (null full)))
  #+(or abcl clisp) (ext:gc)
  #+ecl (si:gc t)
  #+openmcl (ccl:gc)
  #+corman (ccl:gc (if full 3 0))
  #+lispworks (hcl:gc-generation (if full t 0))
  #+mezzano (mezzano.extensions:gc))

;;;; Weak Pointers

#+openmcl
(defvar *weak-pointers* (cl:make-hash-table :test 'eq :weak :value)
  "Weak value hash-table mapping between pseudo weak pointers and its values.")

#+(or allegro openmcl lispworks)
(defstruct (weak-pointer (:constructor %make-weak-pointer))
  #-openmcl pointer)

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to @code{object}. For
   portability reasons, @code{object} must not be @code{nil}."
  (assert (not (null object)))
  #+sbcl (sb-ext:make-weak-pointer object)
  #+(or cmu scl) (ext:make-weak-pointer object)
  #+clisp (ext:make-weak-pointer object)
  #+abcl (ext:make-weak-reference object)
  #+ecl (ext:make-weak-pointer object)
  #+allegro
  (let ((wv (excl:weak-vector 1)))
    (setf (svref wv 0) object)
    (%make-weak-pointer :pointer wv))
  #+openmcl
  (let ((wp (%make-weak-pointer)))
    (setf (gethash wp *weak-pointers*) object)
    wp)
  #+corman (ccl:make-weak-pointer object)
  #+lispworks
  (let ((array (make-array 1 :weak t)))
    (setf (svref array 0) object)
    (%make-weak-pointer :pointer array))
  #+mezzano (mezzano.extensions:make-weak-pointer object))

#-(or allegro openmcl lispworks)
(defun weak-pointer-p (object)
  "Returns @em{true} if @code{object} is a weak pointer and @code{nil}
   otherwise."
  #+sbcl (sb-ext:weak-pointer-p object)
  #+(or cmu scl) (ext:weak-pointer-p object)
  #+clisp (ext:weak-pointer-p object)
  #+abcl (typep object 'ext:weak-reference)
  #+ecl (typep object 'ext:weak-pointer)
  #+corman (ccl:weak-pointer-p object)
  #+mezzano (mezzano.extensions:weak-pointer-p object))

(defun weak-pointer-value (weak-pointer)
  "If @code{weak-pointer} is valid, returns its value. Otherwise,
   returns @code{nil}."
  #+sbcl (values (sb-ext:weak-pointer-value weak-pointer))
  #+(or cmu scl) (values (ext:weak-pointer-value weak-pointer))
  #+clisp (values (ext:weak-pointer-value weak-pointer))
  #+abcl (values (ext:weak-reference-value weak-pointer))
  #+ecl (values (ext:weak-pointer-value weak-pointer))
  #+allegro (svref (weak-pointer-pointer weak-pointer) 0)
  #+openmcl (values (gethash weak-pointer *weak-pointers*))
  #+corman (ccl:weak-pointer-obj weak-pointer)
  #+lispworks (svref (weak-pointer-pointer weak-pointer) 0)
  #+mezzano (mezzano.extensions:weak-pointer-value weak-pointer))

;;;; Weak Hash-tables

;;; Allegro can apparently create weak hash-tables with both weak keys
;;; and weak values but it's not obvious whether it's an OR or an AND
;;; relation. TODO: figure that out.

(defun weakness-keyword-arg (weakness)
  (declare (ignorable weakness))
  #+(or sbcl abcl ecl-weak-hash) :weakness
  #+(or clisp openmcl) :weak
  #+lispworks :weak-kind
  #+allegro (case weakness (:key :weak-keys) (:value :values))
  #+cmu :weak-p)

(defvar *weakness-warnings* '()
  "List of weaknesses that have already been warned about this
   session.  Used by `weakness-missing'.")

(defun weakness-missing (weakness errorp)
  "Signal an error or warning, depending on ERRORP, about lack of Lisp
   support for WEAKNESS."
  (cond (errorp
         (error "Your Lisp does not support weak ~(~A~) hash-tables."
                weakness))
        ((member weakness *weakness-warnings*) nil)
        (t (push weakness *weakness-warnings*)
         (warn "Your Lisp does not support weak ~(~A~) hash-tables."
               weakness))))

(defun weakness-keyword-opt (weakness errorp)
  (declare (ignorable errorp))
  (ecase weakness
    (:key
     #+(or lispworks sbcl abcl clisp openmcl ecl-weak-hash) :key
     #+(or allegro cmu) t
     #-(or lispworks sbcl abcl clisp openmcl allegro cmu ecl-weak-hash)
     (weakness-missing weakness errorp))
    (:value
     #+allegro :weak
     #+(or clisp openmcl sbcl abcl lispworks cmu ecl-weak-hash) :value
     #-(or allegro clisp openmcl sbcl abcl lispworks cmu ecl-weak-hash)
     (weakness-missing weakness errorp))
    (:key-or-value
     #+(or clisp sbcl abcl cmu) :key-or-value
     #+lispworks :either
     #-(or clisp sbcl abcl lispworks cmu)
     (weakness-missing weakness errorp))
    (:key-and-value
     #+(or clisp abcl sbcl cmu ecl-weak-hash) :key-and-value
     #+lispworks :both
     #-(or clisp sbcl abcl lispworks cmu ecl-weak-hash)
     (weakness-missing weakness errorp))))

(defun make-weak-hash-table (&rest args &key weakness (weakness-matters t)
                             #+openmcl (test #'eql)
                             &allow-other-keys)
  "Returns a new weak hash table. In addition to the standard
   arguments accepted by @code{cl:make-hash-table}, this function adds
   extra keywords: @code{:weakness} being the kind of weak table it
   should create, and @code{:weakness-matters} being whether an error
   should be signalled when that weakness isn't available (the default
   is to signal an error).  @code{weakness} can be one of @code{:key},
   @code{:value}, @code{:key-or-value}, @code{:key-and-value}.

   If @code{weakness} is @code{:key} or @code{:value}, an entry is
   kept as long as its key or value is reachable, respectively. If
   @code{weakness} is @code{:key-or-value} or @code{:key-and-value},
   an entry is kept if either or both of its key and value are
   reachable, respectively.

   @code{tg::make-hash-table} is available as an alias for this
   function should you wish to import it into your package and shadow
   @code{cl:make-hash-table}."
  (remf args :weakness)
  (remf args :weakness-matters)
  (if weakness
      (let ((arg (weakness-keyword-arg weakness))
            (opt (weakness-keyword-opt weakness weakness-matters)))
        (apply #'cl:make-hash-table
               #+openmcl :test #+openmcl (if (eq opt :key) #'eq test)
               (if arg
                   (list* arg opt args)
                   args)))
      (apply #'cl:make-hash-table args)))

;;; If you want to use this function to override CL:MAKE-HASH-TABLE,
;;; it's necessary to shadow-import it. For example:
;;;
;;;   (defpackage #:foo
;;;     (:use #:common-lisp #:trivial-garbage)
;;;     (:shadowing-import-from #:trivial-garbage #:make-hash-table))
;;;
(defun make-hash-table (&rest args)
  (apply #'make-weak-hash-table args))

(defun hash-table-weakness (ht)
  "Returns one of @code{nil}, @code{:key}, @code{:value},
   @code{:key-or-value} or @code{:key-and-value}."
  #-(or allegro sbcl abcl clisp cmu openmcl lispworks
        ecl-weak-hash)
  (declare (ignore ht))
  ;; keep this first if any of the other lisps bugously insert a NIL
  ;; for the returned (values) even when *read-suppress* is NIL (e.g. clisp)
  #.(if (find :sbcl *features*)
        (if (find-symbol "HASH-TABLE-WEAKNESS" "SB-EXT")
            (read-from-string "(sb-ext:hash-table-weakness ht)")
            nil)
        (values))
  #+abcl (sys:hash-table-weakness ht)
  #+ecl-weak-hash (ext:hash-table-weakness ht)
  #+allegro (cond ((excl:hash-table-weak-keys ht) :key)
                   ((eq (excl:hash-table-values ht) :weak) :value))
  #+clisp (ext:hash-table-weak-p ht)
  #+cmu (let ((weakness (lisp::hash-table-weak-p ht)))
          (if (eq t weakness) :key weakness))
  #+openmcl (ccl::hash-table-weak-p ht)
  #+lispworks (system::hash-table-weak-kind ht))

;;;; Finalizers

;;; Note: Lispworks can't finalize gensyms.

#+(or allegro clisp lispworks openmcl)
(defvar *finalizers*
  (cl:make-hash-table :test 'eq
                      #+allegro :weak-keys #+:allegro t
                      #+(or clisp openmcl) :weak
                      #+lispworks :weak-kind
                      #+(or clisp openmcl lispworks) :key)
  "Weak hashtable that holds registered finalizers.")

#+corman
(progn
  (defvar *finalizers* '()
    "Weak alist that holds registered finalizers.")

  (defvar *finalizers-cs* (threads:allocate-critical-section)))

#+lispworks
(progn
  (hcl:add-special-free-action 'free-action)
  (defun free-action (object)
    (let ((finalizers (gethash object *finalizers*)))
      (unless (null finalizers)
        (mapc #'funcall finalizers)))))

(defun finalize (object function)
  "Pushes a new @code{function} to the @code{object}'s list of
   finalizers. @code{function} should take no arguments. Returns
   @code{object}.

   @b{Note:} @code{function} should not attempt to look at
   @code{object} by closing over it because that will prevent it from
   being garbage collected."
  #+(or cmu scl) (ext:finalize object function)
  #+sbcl (sb-ext:finalize object function)
  #+abcl (ext:finalize object function)
  #+ecl (let ((next-fn (ext:get-finalizer object)))
          (ext:set-finalizer
           object (lambda (obj)
                    (declare (ignore obj))
                    (funcall function)
                    (when next-fn
                      (funcall next-fn nil)))))
  #+allegro
  (progn
    (push (excl:schedule-finalization
           object (lambda (obj) (declare (ignore obj)) (funcall function)))
          (gethash object *finalizers*))
    object)
  #+clisp
  ;; The CLISP code used to be a bit simpler but we had to workaround
  ;; a bug regarding the interaction between GC and weak hashtables.
  ;; See <http://article.gmane.org/gmane.lisp.clisp.general/11028>
  ;; and <http://article.gmane.org/gmane.lisp.cffi.devel/994>.
  (multiple-value-bind (finalizers presentp)
      (gethash object *finalizers* (cons 'finalizers nil))
    (unless presentp
      (setf (gethash object *finalizers*) finalizers)
      (ext:finalize object (lambda (obj)
                             (declare (ignore obj))
                             (mapc #'funcall (cdr finalizers)))))
    (push function (cdr finalizers))
    object)
  #+openmcl
  (progn
    (ccl:terminate-when-unreachable
     object (lambda (obj) (declare (ignore obj)) (funcall function)))
    ;; store number of finalizers
    (incf (gethash object *finalizers* 0))
    object)
  #+corman
  (flet ((get-finalizers (obj)
           (assoc obj *finalizers* :test #'eq :key #'ccl:weak-pointer-obj)))
    (threads:with-synchronization *finalizers-cs*
      (let ((pair (get-finalizers object)))
        (if (null pair)
            (push (list (ccl:make-weak-pointer object) function) *finalizers*)
            (push function (cdr pair)))))
    (ccl:register-finalization
     object (lambda (obj)
              (threads:with-synchronization *finalizers-cs*
                (mapc #'funcall (cdr (get-finalizers obj)))
                (setq *finalizers*
                      (delete obj *finalizers*
                              :test #'eq :key #'ccl:weak-pointer-obj)))))
    object)
  #+lispworks
  (progn
    (let ((finalizers (gethash object *finalizers*)))
      (unless finalizers
        (hcl:flag-special-free-action object))
      (setf (gethash object *finalizers*)
            (cons function finalizers)))
    object)
  #+mezzano (error 'tivial-garbage-finalize-undefined))

(defun cancel-finalization (object)
  "Cancels all of @code{object}'s finalizers, if any."
  #+cmu (ext:cancel-finalization object)
  #+scl (ext:cancel-finalization object nil)
  #+sbcl (sb-ext:cancel-finalization object)
  #+abcl (ext:cancel-finalization object)
  #+ecl (ext:set-finalizer object nil)
  #+allegro
  (progn
    (mapc #'excl:unschedule-finalization
          (gethash object *finalizers*))
    (remhash object *finalizers*))
  #+clisp
  (multiple-value-bind (finalizers present-p)
      (gethash object *finalizers*)
    (when present-p
      (setf (cdr finalizers) nil))
    (remhash object *finalizers*))
  #+openmcl
  (let ((count (gethash object *finalizers*)))
    (unless (null count)
      (dotimes (i count)
        (ccl:cancel-terminate-when-unreachable object))))
  #+corman
  (threads:with-synchronization *finalizers-cs*
    (setq *finalizers*
          (delete object *finalizers* :test #'eq :key #'ccl:weak-pointer-obj)))
  #+lispworks
  (progn
    (remhash object *finalizers*)
    (hcl:flag-not-special-free-action object))
  #+mezzano (error 'tivial-garbage-cancel-finalization-undefined))
