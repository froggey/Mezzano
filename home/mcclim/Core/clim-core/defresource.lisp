;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2 Chapter 32.1 Resources
;;;   Created: 2001-05-21
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

;;;; Changes

;;;  When        Who    What
;;; --------------------------------------------------------------------------------------
;;;  2002-02-10  GB     named allocator function
;;;  2001-05-21  GB     created

(in-package :clim-internals)

;;;; 32.1 Resources

;;; TODO
;; - provide a "real" using-resource expansion
;; - under CMU ATOMIC-INCF is a performance blocker, what to do?
;; - use per process free-lists?
;; - use two lists? One for free objects, one for allocated?

;;; NOTES

;; It seems wasteful not to shuffle objects. When there are already a
;; couple of objects in use and these are in front of
;; RESOURCE-OBJECTS, we revisit them each time while finding. On the
;; other hand, when we would like to shuffle the list of objects, we
;; would need to acquire another lock, which seems equally wasteful.

;; Further: This somewhat assumes a feasible implementation of
;; ATOMIC-INCF and ATOMIC-DECF. If plain vanilla locks or
;; WITHOUT-SCHEDULING is faster, we would be better off using that.

;; USING-RESOURCE should not cause unnecessary consing. To test this I
;; appended a test case below. There is an UNWIND-PROTECT in the
;; definition of USING-RESOURCE, which might cause consing. (E.g. CMU
;; does so, when the clean-up form writes to variables defined outside
;; of it).

;; Also I tried to define all this in a way, that the no assumptions
;; about random user code is made. Specifically: no locks are held,
;; scheduling is _not_ disabled, any of the resource API can be
;; called.

;; ) unlike under Genera it seems.

(defvar *resource-table*
    (make-hash-table :test #'eq)
  "Hash table of all defined resource types.")

(defstruct resource
  ;; One defined resource
  name                  ;its name just for the sake of it
  objects               ;a list of RESOURCE-OBJECTs
  lock                  ;A regular process lock to protect the OBJECTS slot.
  allocator             ;function to allocate an object
                        ; takes the resource and the parameters. 
                        ; Returns two values: the object and its
                        ; descriptor (a RESOURCE-OBJECT)
                        ; [cf. Genera's definition allocate-resource]
  deallocator)          ;function to deallocate an object
                        ; takes three arguments: the resource, the
                        ; object and the descriptor.


(defstruct resource-object
  ;; One resourced object
  object                ;the object itself
  lock                  ;homebrew lock 
                        ; is >= 1, if resource is allocated or investigated
                        ; is 0, if resource is free
  parameters)           ;list of parameters supplied, while allocating this
                        ; object. Needed for a possible deinitializer
                        ; or default matcher. NIL if not needed at all.

(defun find-resource (name &optional barfp)
  (or (gethash name *resource-table*)
      (if barfp
          (progn
            (cerror "Try again to find resource ~S (after you defined it)"
                    "There is no such resource: ~S." name)
            (find-resource name))
        nil)))

(defun (setf find-resource) (value name)
  (setf (gethash name *resource-table*) value))

(defun allocate-resource (name &rest parameters)
  "Allocates an object from the resource."
  (let ((resource (find-resource name)))
    (values (apply (the function (resource-allocator resource))
                   resource parameters))) )

(defun deallocate-resource (name object)
  "Returns the object to the resource."
  (let ((resource (find-resource name)))
    (funcall (the function (resource-deallocator resource))
             resource object)) )

(defmacro using-resource ((variable name &rest parameters) &body body)
  "The forms in 'body' are evaluated with 'variable' bound to an object
   allocated from the resource named 'name', using the parameters given
   by 'parameters'."
  (let ((r. (gensym "R."))
        (ro. (gensym "RO."))
        (evil-cell (gensym "EVIL-CELL.")))
    `(let* ((,evil-cell (load-time-value (list nil)))
            (,r. (or (car (the cons ,evil-cell))
                     (setf (car (the cons ,evil-cell))
                       (find-resource ',name)))))
       ;; Q: Why this EVIL-CELL hack? And not 
       ;;    (load-time-value (find-resource ..))?
       ;; A: Since the time of actual evaluation of the LOAD-TIME-VALUE is
       ;;    unspecified with regard to other [top level] forms in a file[,
       ;;    when loaded]. But I want the DEFRESOURCE to evaluate before this
       ;;    LOAD-TIME-VALUE.
       (multiple-value-bind (,variable ,ro.)
           (funcall (resource-allocator ,r.) ,r. ,@parameters)
         (unwind-protect
             (locally ,@body)
           (when ,variable
             (funcall (resource-deallocator ,r.) ,r. ,variable ,ro.)))))))

(defun clear-resource (name)
  "Removes all of the resourced object from the resource."
  (let ((resource (find-resource name)))
    (setf (resource-objects resource) nil)))

(defun map-resource (function name)
  "Calls function once on each object in the resource."
  (let ((resource (find-resource name)))
    (dolist (resource-object (resource-objects resource))
      (funcall function
               (resource-object-object resource-object)
               (not (zerop (car (resource-object-lock resource-object))))
               name))))

(defparameter *resource-atomic-lock*
  (clim-sys:make-lock "atomic incf/decf"))

(defmacro resource-atomic-incf (place)
  `(clim-sys:with-lock-held (*resource-atomic-lock*)
       (incf ,place)))

(defmacro resource-atomic-decf (place)
  `(clim-sys:with-lock-held (*resource-atomic-lock*)
       (decf ,place)))

(defmacro defresource (name parameters
                            &key (constructor (error "~S argument is required" :constructor))
                            initializer
                            deinitializer
                            matcher
                            initial-copies)
  ;; First do some type checks
  (check-type name symbol)
  ;; Safety first: Check a possible definition lock.
  (let ((pack (symbol-package name)))
    (when (or (eq pack (find-package :keyword))
              (eq pack (find-package :common-lisp))
              #+excl (excl:package-definition-lock pack)
              )
      (cerror "Define resource ~S anyway"
              "Resource ~S cannot be defined, since its home package, ~S, is locked."
              name (package-name pack))))
  ;; Collect parameter variables
  (let ((pvars nil))
    (dolist (parameter parameters)
      (cond ((member parameter lambda-list-keywords)
             nil)
            ((symbolp parameter)
             (pushnew parameter pvars))
            ((consp parameter)
             (if (consp (first parameter))
                 (pushnew (second (first parameter)) pvars)
               (pushnew (first parameter) pvars))
             (when (third parameter) 
               (pushnew (third parameter) pvars)))))
    (setf pvars (reverse pvars))
    
    (let ((parameters-needed-p
           (or (null matcher)
               (not (null deinitializer)))))
      (labels ((allocate-fresh-expr (r.)
                 ;; Allocate a fresh object
                 (let ((ro. (gensym "RO.")))
                   `(let ((,ro.
                           (make-resource-object 
                            :object ,constructor
                            :lock (list 1)
                            :parameters ,(if parameters-needed-p
                                             `(make-list ,(length pvars))
                                           'nil))))
                      (with-lock-held ((resource-lock ,r.))
                        (push ,ro. (resource-objects ,r.)))
                      ,ro.)) )
               
               (match-expr (ro.)
                 ;; Compilation of the matcher
                 (let ((q. (gensym "Q.")))
                   (if matcher
                       `(let ((,name (resource-object-object ,ro.)))
                          (declare (ignorable ,name))
                          ,matcher)
                     `(let ((,q. (resource-object-parameters ,ro.)))
                        (declare (ignorable ,q.))
                        (and
                         ,@(loop for p in pvars collect 
                                 `(equal (pop ,q.) ,p)))))))
               
               (find-expr (r.)
                 ;; Find an object, which matches or allocate a fresh one.
                 ;;
                 ;; To improve granularity of locking, each allocated resource
                 ;; carries its own lock. Furthermore, when a lock is not
                 ;; available, we do not care to wait, but simply choose to
                 ;; carry on. Furthermore we consider resources in use as
                 ;; locked. This saves us another test while finding at the
                 ;; expense that MAP-RESOURCE might once in a while report a
                 ;; resource as used, while somebody else is just peeking.
                 ;;
                 (let ((ro.   (gensym "$RO."))
                       (lock. (gensym "$LOCK.")))
                   `(dolist (,ro. (resource-objects ,r.)
                              ;; fall back: allocate a fresh object
                              ,(allocate-fresh-expr r.))
                      (declare (type resource-object ,ro.))
                      (let ((,lock. (resource-object-lock ,ro.)))
                        (declare (type cons ,lock.))
                        (when (= 0 (the fixnum (car ,lock.)))
                          (resource-atomic-incf (the fixnum (car ,lock.)))
                          (cond ((and (= 1 (the fixnum 
                                             (locally
                                               #+excl (declare (optimize (safety 3))) ;EXCL bug
                                               (car ,lock.))))
                                      ,(match-expr ro.))
                                 (return ,ro.))
                                (t
                                 (resource-atomic-decf (the fixnum (car ,lock.)))) ))))))
               
               (allocator ()
                 ;; Function for ALLOCATE-RESOURCE
                 (let ((r.  (gensym "R."))
                       (ro. (gensym "RO."))
                       (fn. (make-symbol
                               (with-standard-io-syntax
                                   (let ((*package* (find-package :keyword)))
                                     (format nil "ALLOCATOR for ~S" name))))))
                   `(labels ((,fn. (,r. ,@parameters)
                              (let ((,ro. ,(find-expr r.)))
                                (declare (type resource-object ,ro.))
                                ;; install parameters in resource-object and eval initializer
                                ,(install-parameters-expr ro.)
                                (let ((,name (resource-object-object ,ro.)))
                                  (declare (ignorable ,name))
                                  ,initializer)
                                ;; done
                                (values (resource-object-object ,ro.)
                                        ,ro.))))
                     #',fn.)))
               
               (install-parameters-expr (ro.)
                 (and parameters-needed-p
                      (let ((q. (gensym "Q.")))
                        `(let ((,q. (resource-object-parameters ,ro.)))
                           (declare (ignorable ,q.))
                           ,@(loop for p in pvars collect
                                   `(setf (car ,q.) ,p ,q. (cdr ,q.)))))))
               
               (deallocator ()
                 ;; Function for deallocate-resource
                 (let ((r.    (gensym "R."))
                       (ro.   (gensym "RO."))
                       (obj.  (gensym "OBJ"))
                       (q.    (gensym "Q"))
                       (lock. (gensym "LOCK")))
                   `(lambda (,r. ,obj. &optional ,ro.)
                      (unless ,ro.
                        (do ((q (resource-objects (the resource ,r.)) (cdr (the cons q))))
                            ((null q)
                             (error "Something corrupted."))
                          (let ((ro (car (the cons q))))
                            (declare (type resource-object ro))
                            (when (eq ,obj. (resource-object-object ro))
                              (setf ,ro. ro)
                              (return)))))
                      (locally
                          (declare (type resource-object ,ro.))
                        (let ((,name ,obj.))
                          (declare (ignorable ,name))
                          ,(when deinitializer
                             `(destructuring-bind (,@pvars) (resource-object-parameters ,ro.)
                                (declare (ignorable ,@pvars))
                                ,deinitializer)))
                        ,(if (and matcher (not (null deinitializer)))
                             `(let ((,q. (resource-object-parameters ,ro.)))
                                ,@(loop repeat (length pvars) collect
                                        `(setf (car ,q.) nil ,q. (cdr ,q.))))
                           nil)
                        (let ((,lock. (resource-object-lock ,ro.)))
                          (resource-atomic-decf (the fixnum (car ,lock.)))))))) )
        ;;
        (let* ((r.           (gensym "R."))
               (q.           (gensym "Q."))
               (allocator.   (gensym "ALLOCATOR."))
               (deallocator. (gensym "DEALLOCATOR.")) )
          `(progn
             #+excl (excl:record-source-file ',name :type :resource-definition)
             (let* ((,allocator. ,(allocator))
                    (,deallocator. ,(deallocator))
                    (,r.
                     (or ;; (find-resource ',name)
                         (make-resource
                          :name ',name
                          :objects nil
                          :lock (make-lock (let ((*package* (find-package :keyword)))
                                             (format nil "Resource ~S" ',name)))))))
               (setf (resource-allocator ,r.) ,allocator.
                     (resource-deallocator ,r.) ,deallocator.)
               ;; Care for initial copies
               ,(when initial-copies
                  `(progn
                     (dotimes (,q. ,initial-copies)
                       (funcall ,allocator. ,r.))
                     (dolist (,q. (resource-objects ,r.))
                       (funcall ,deallocator. ,r. (resource-object-object ,q.) ,q.))))
               ;; Finally install the resource
               (setf (find-resource ',name) ,r.)
               ;; Out of habit we return the name, although nobody uses a
               ;; printing LOAD these days.
               ',name) ))))))

;;; --------------------
;;; Proposal

;; PERMANENTLY-DEALLOCATE-RESOURCE name object                     [Function]

;; Deallocate 'object' and indicate, that it is no longer useful to retain it.

;; EXAMPLE

;; One might consider connections to FTP servers as a resource. But those
;; connection can become dead on "itself", because the FTP server might have
;; chosen to kick us out due to inactivity. With
;; PERMANENTLY-DEALLOCATE-RESOURCE it is now possible to selectively clean
;; up the those objects. Or do it on demand, while matching.
   
;;   (defresource ftp-connection (host &optional (port +ftp-port+))
;;      :constructor (make-ftp-connection host port)
;;      :matcher     (cond ((connection-dead-p ftp-connection)
;;                          (permanently-deallocate-resource
;;                           'ftp-connection ftp-connection)
;;                          nil)
;;                         ((and (equal (ftp-connection-host host) host)
;;                               (equal (ftp-connection-port port) port))))
;;      :destructor  (close-ftp-connection ftp-connection))

;; IMPLICATIONS

;; We now also need a :DESTRUCTOR option to indicate actions needed, when we
;; throw away an object. These should also be invoked, when one does a
;; CLEAR-RESOURCE.

;;; --------------------

;;; Test cases

#||

;; First a minimal speed test

(defresource seventeen ()
   :constructor 17)

(defun test-seventeen ()
  ;; should not cons at all
  (dotimes (i 1000000)
    (using-resource (x seventeen))))

;; Now a more sophisticated test:

(defstruct foo x y)

(defresource foo (&optional (x 10) (y 20))
  :constructor    (make-foo)
  :initializer    (setf (foo-x foo) x
                        (foo-y foo) y)
  :initial-copies 3)

(defun test-foo ()
  (dotimes (i 1000000)
    (using-resource (x foo 8 9))))

||#
