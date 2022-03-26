;;;; fndb.lisp -- DEFKNOWNish bits for SBCL

(cl:in-package :nibbles)

#+sbcl (progn

;;; Efficient array bounds checking
(sb-c:defknown %check-bound
  ((simple-array (unsigned-byte 8) (*)) index (and fixnum sb-vm:word)
   (member 2 4 8 16))
    index (sb-c:any) :overwrite-fndb-silently t)

;; We DEFKNOWN the exported functions so we can DEFTRANSFORM them.
;; We DEFKNOWN the %-functions so we can DEFINE-VOP them.

#.(loop for i from 0 to #-x86-64 #b0111 #+x86-64 #b1011
        for bitsize = (ecase (ldb (byte 2 2) i)
                        (0 16)
                        (1 32)
                        (2 64))
        for signedp = (logbitp 1 i)
        for setterp = (logbitp 0 i)
        for byte-fun = (if setterp
                           #'byte-set-fun-name
                           #'byte-ref-fun-name)
        for big-fun = (funcall byte-fun bitsize signedp t)
        for little-fun = (funcall byte-fun bitsize signedp nil)
        for internal-big = (internalify big-fun)
        for internal-little = (internalify little-fun)
        for arg-type = `(,(if signedp
                              'signed-byte
                              'unsigned-byte)
                              ,bitsize)
        for external-arg-types = `(array index ,@(when setterp
                                                   `(,arg-type)))
        for internal-arg-types = (subst '(simple-array (unsigned-byte 8)) 'array
                                        external-arg-types)
        collect `(sb-c:defknown (,big-fun ,little-fun) ,external-arg-types
                     ,arg-type (sb-c:any) :overwrite-fndb-silently t) into defknowns
        collect `(sb-c:defknown (,internal-big ,internal-little)
                     ,internal-arg-types
                     ,arg-type (sb-c:any) :overwrite-fndb-silently t) into defknowns
        finally (return `(progn ,@defknowns)))

);#+sbcl
