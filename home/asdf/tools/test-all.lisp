(in-package :asdf-tools)

(defun call-with-all-lisps (thunk &key (lisps *test-lisps*) muffle-failures)
  (let ((thunks
          (loop :for lisp :in (get-lisps lisps)
                :collect (let ((l lisp))
                           (lambda () (with-failure-context (:name (format nil "using ~(~A~)" l))
                                        (funcall thunk l)))))))
    (if muffle-failures
        (call-without-stopping thunks)
        (progn (map () 'funcall thunks) (success)))))

(defmacro with-all-lisps ((lisp-var lisps &key muffle-failures) &body body)
  `(call-with-all-lisps (lambda (,lisp-var) ,@body) :lisps ,lisps :muffle-failures ,muffle-failures))

(deftestcmd test-all-basic (lisps systems)
  "basic test: doc, clean-load, load-systems"
  (without-stopping ()
    (show-version)
    (doc)
    (test-ascii)
    (test-all-clean-load lisps)
    (test-all-load-systems lisps systems)))

(deftestcmd test-all-load-systems (lisps systems)
  (with-systems-test (systems)
    (with-all-lisps (l lisps) (test-load-systems l systems))))

(deftestcmd test-all-clean-load (lisps)
  "test-clean-load on all lisps"
  (with-all-lisps (l lisps) (test-clean-load l)))

(deftestcmd test-all-scripts (lisps)
  "test-scripts on all lisps"
  (with-all-lisps (l lisps) (test-scripts l)))

(deftestcmd test-all-no-upgrade ()
  "test-all-basic, and test-all-script"
  (test-all-basic) (test-all-scripts))

(deftestcmd test-all-upgrade (upgrade-lisps)
  "test-upgrade on all lisps"
  (with-all-lisps (l upgrade-lisps) (test-upgrade l)))

(deftestcmd test-all ()
  "all tests"
  (test-all-no-upgrade) (test-all-upgrade))

(deftestcmd test-all-scripts-no-stop (lisps)
  "test-scripts on all lisps, no stop"
  (with-all-lisps (l lisps :muffle-failures t) (test-scripts l)))

(deftestcmd test-all-upgrade-no-stop (upgrade-lisps)
  "test-upgrade on all lisps, no stop"
  (with-all-lisps (l upgrade-lisps :muffle-failures t) (test-upgrade l)))

(deftestcmd test-all-no-upgrade-no-stop ()
  "all tests but upgrade on all lisps, no stop"
  (without-stopping ()
    (test-all-basic)
    (test-all-scripts-no-stop)
    (check-all-scripts-results)))

(deftestcmd test-all-no-stop () ;; TODO: pass arguments!
  "all tests on all lisps, no stop"
  (without-stopping ()
    (test-all-basic)
    (test-all-scripts-no-stop)
    (test-all-upgrade-no-stop)
    (check-all-results)))

(deftestcmd check-all-scripts-results ()
  "were there errors in test scripts?"
  (with-asdf-dir ()
    (let ((bad-lisps
            (run/lines
             `(grep "-L" "All tests apparently successful."
                    ,@(mapcar (lambda (l) (format nil "build/results/~(~A~)-test.text" l))
                              (get-lisps))) :on-error nil)))
    (failure-if bad-lisps
                "Unexpected test failures on these implementations:~%~{~A~%~}" bad-lisps))))

(deftestcmd check-all-upgrade-results ()
  "were there upgrade test failures?"
  (with-asdf-dir ()
    (let ((bad-lisps
            (run/lines
             `(grep "-L" "Upgrade test succeeded for "
                    ,@(mapcar (lambda (l) (format nil "build/results/~(~A~)-upgrade.text" l))
                              (get-upgrade-lisps))) :on-error nil)))
      (failure-if bad-lisps
                  "Unexpected upgrade failures on these implementations:~%~{~A~%~}~%" bad-lisps))))

(deftestcmd check-all-results ()
  "were there failures in any scripts or upgrade?"
  (without-stopping ()
    (check-all-scripts-results) (check-all-upgrade-results)))
