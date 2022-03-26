(in-package :asdf)

(defclass fiveam-tester-system (system)
  ((test-names
    :initarg :test-names
    :reader test-names
    :documentation "A list whose elements are either
cons cells of symbol and package designators or
simply a symbol designator.
  In the latter case, the symbols will be interned
in the package designated by the TEST-PACKAGE slot,
which must be bound.")
   (test-package
    :initarg :default-test-package
    :initarg :test-package
    :documentation "If all the tests are in one package, you can just
have a list of test names in test-names, and get the package name from
here.")
   (num-checks
    :initarg :num-checks
    :reader num-checks
    :type (or null (integer 0))
    :initform nil
    :documentation "How many tests do you expect to run when
you invoke the test-op on this system. For backwards compatibility,
this slot is ignored if it's NIL. If bound, then when running the
test-op, we will fail if the expected number of checks are not run.")
   ))

(define-condition fiveam-asdf-failure (error)
  ((failed-asdf-component
    :initarg :failed-asdf-component
    :reader failed-asdf-component
    ))
  (:documentation "Superclass of error conditions that indicate that
  an ASDF test-op has failed."))


(define-condition fiveam-test-fail (fiveam-asdf-failure)
  ((failed
    :initarg :failed
    :reader failed
    ))
  (:report (lambda (x s)
              (format s "Tests on system ~a failed: ~{~t~a~%~}"
                      (component-name (failed-asdf-component x))
                      (failed x)))))

(define-condition fiveam-wrong-number-of-checks (fiveam-asdf-failure)
  ((expected-num-checks
    :initarg :expected
    :initarg :expected-num-checks
    :reader expected-num-checks
    )
   (actual-num-checks
    :initarg :actual-num-checks
    :initarg :actual
    :reader actual-num-checks
    ))
  (:report (lambda (x s)
              (format s "Unexpected number of tests on system ~a: Expected ~d got ~d."
                      (component-name (failed-asdf-component x))
                      (expected-num-checks x)
                      (actual-num-checks x)))))

(defgeneric test-package (x)
  (:method ((x fiveam-tester-system))
    (if (slot-boundp x 'test-package)
        (slot-value x 'test-package)
        (error "If package is not specified with each test-name, system's TEST-PACKAGE slot must be set."))))

(defmethod perform ((op test-op) (sys fiveam-tester-system))
  (let* ((test-syms
           (loop for x in (test-names sys)
                 with test-name and package-name and test-sym and package
                 if (symbolp x)
                   do (setf test-name x
                            package-name (test-package sys))
                 else
                   do (assert (and (consp x)
                                   (or (symbolp (car x)) (stringp (car x)))
                                   (or (symbolp (cdr x)) (stringp (cdr x)))))
                      (setf test-name (car x) package-name (cdr x))
                 do (setf package (or (find-package package-name)
                                      (error "Unable to find package ~a" package-name)))
                    (setf test-sym
                          (intern
                           (etypecase test-name
                             (string test-name)
                             (symbol (symbol-name test-name)))
                           package))
                 collect test-sym))
         (runner (intern (symbol-name '#:run) :fiveam))
         (tester (intern (symbol-name '#:results-status) :fiveam))
         (explainer (intern (symbol-name '#:explain!) :fiveam))
         (results (loop for test in test-syms
                        appending (funcall runner test))))
    (funcall explainer results)
    ;; if there's an expected number of checks, verify that we have run
    ;; exactly that number.
    (when (num-checks sys)
      (let ((actual-num-checks (length results)))
        (unless (= actual-num-checks (num-checks sys))
          (error 'fiveam-wrong-number-of-checks
                 :failed-asdf-component sys
                 :actual actual-num-checks
                 :expected (num-checks sys)))))
    (multiple-value-bind (success failures)
        (funcall tester results)
      (unless success
        (error 'fiveam-test-fail :failed-asdf-component sys :failed failures)))))

(defmethod component-depends-on ((op load-op) (sys fiveam-tester-system))
  (cons '(load-op "fiveam") (call-next-method)))

(defmethod component-depends-on ((op compile-op) (sys fiveam-tester-system))
  (cons '(load-op "fiveam") (call-next-method)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'fiveam-tester-system :asdf))
