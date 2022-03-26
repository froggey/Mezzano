;; This is a test file which doesn't properly test anything. It just
;; has a bunch of code which may occasionally be useful for testing
;; Clouseau, if you squint at it. All data dictated by wild whims.

(in-package :clouseau)

(defclass oddity ()
  ((complex-number :initform #C(1 3)
                   :documentation "Everybody should have a complex number to
avoid being picked up for vagrancy."
                   :type complex))
  (:documentation "A thing which is odd in some way"))

(defclass queer-oddity (oddity)
  ((spoons :initform "Rusty ones"))
  (:documentation "An unusually odd oddity"))

(defclass salad-mixin ()
  ()
  (:documentation "Fear the salad. Seriously."))

(defstruct historical-event
  (severity 5 :type (integer 0 10))
  (attribute "" :type string)
  (pirate "" :type string))

(defclass packrat (queer-oddity salad-mixin)
  ((name :initform "Willy the Packrat"
         :type (or string symbol)
         :reader ratname
         :writer set-ratname)
   (some-floats :initform '(1.2 2.3 3.4 4.5 5.6 42.0))
   (fvector :initform #(67.0d0 3.8d3 2.983454d0 #.pi))
   (an-array :initform #2A((1 0 0) (0 1 0) (0 0 1))
             :documentation "An identity matrix")
   (global-fun :initform #'inspect)
   (reunion :initform (make-historical-event :severity 7
                                             :attribute "Sephiroth!"
                                             :pirate "Sephiroth?"))
   (l :initform (lambda (x)
                  (declare (number x))
                  (1+ x)))
   (str :initform "A 'tring")
   (pet :initform (make-instance 'oddity)
        :accessor pet)
   (doc-symbol :initform 'documentation)
   (inspect-o :initform #'inspect-object))
  (:documentation "A thing with lots of other things for no good reason"))

(inspect (make-instance 'packrat) :new-process t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-ages (x y)
  "Add together two ages in a fast but unsafe manner"
  (declare (type (integer 1 150) x y)
           (optimize (speed 3) (safety 0) (debug 1)))
  (+ x y))

(inspect 'add-ages :new-process t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj salad-mixin) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.Everybody should have a complex number to avoid being picked up for vagrancy.")))

(inspect (cons (make-instance 'salad-mixin) 42))

(let ((*print-length* 10)
      (*print-level* 10)
      inspected-inspector)
  (setf inspected-inspector
        (make-application-frame 'inspector
                                :object (make-application-frame
                                         'inspector :object 20)))
  (clim-sys:make-process (lambda ()
                           (run-frame-top-level inspected-inspector))
                         :name "Inspector Clouseau (being inspected)")
  (inspect inspected-inspector :new-process t))

(let ((ht (make-hash-table)))
  (setf (gethash 'foo ht) 42
        (gethash 'bar ht) 666)
  (inspect ht))
