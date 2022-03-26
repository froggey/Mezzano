(in-package :asdf-tools)

(defparameter *default-test-scripts* '("*.script"))

(defun get-test-scripts (&optional (test-scripts *test-scripts*))
  (typecase test-scripts
    ((eql :default) (setf test-scripts *default-test-scripts*))
    (string (setf test-scripts (ensure-list-of-strings test-scripts))))
  (with-asdf-dir ("test/")
    (sort
     (loop :for pattern :in test-scripts
           :append (loop :for file :in (directory* pattern)
                         :collect (enough-pathname file (pn "test/"))))
     'string< :key 'namestring)))

#|
;; Somehow we never wrote the tests that check our configuration file infrastructure...
(defun create-config ()
  (dolist (x '("build/results/" "build/test-source-registry-conf.d/"
               "build/test-asdf-output-translations-conf.d/"))
    (ensure-directories-exist (pn x))))

(defun clean-config ()
  (flet ((rm-rf (x)
           (delete-directory-tree (pn x) :validate (lambda (x) (subpathp x (pn "build/"))))))
    (rm-rf "build/test-source-registry-conf.d/")
    (rm-rf "build/test-asdf-output-translations-conf.d/")))
|#

(deftestcmd test-scripts (lisp test-scripts)
  "run test scripts
Use the preferred lisp implementation"
  (nest
   (with-asdf-dir ())
   (let* ((log (newlogfile "test" lisp)))
     (log! log "Running the following ~D ASDF test script~:*~P on ~(~A~):~%~{  ~A~%~}"
           (length test-scripts) lisp test-scripts))
   (let ((n-tests (length test-scripts))
         (test-pass 0)
         (test-fail 0)
         (failed-list ())))
   (call-without-stopping
    `(,(lambda ()
         (run-test-lisp
          "compiling ASDF"
          '((load "test/script-support.lisp") (asdf-test::compile-asdf-script))
          :lisp lisp :log log))
      ,@(mapcar
          (lambda (test-script)
            (lambda ()
              ;; TODO: do we want to delete the output file cache?
              ;; If so, we need to do it in the inferior lisp,
              ;; because only it knows for sure its output configuration.
              ;; Or we could do it in a more heavy handed way.
              ;; A better solution would be to do any output operations in a temporary workspace,
              ;; which would allow test parallelization.
              (if (with-failure-context (:muffle-failures t)
                    (run-test-lisp
                     (format nil "testing ~A on ~(~A~)" test-script lisp)
                     `((load "test/script-support.lisp")
                       (asdf-test::load-asdf)
                       (asdf-test::frob-packages)
                       (asdf-test::run-test-script ,(native-namestring (subpathname "test/" test-script))))
                     :lisp lisp :log log))
                (incf test-pass)
                (progn
                  (incf test-fail)
                  (push test-script failed-list)))))
          test-scripts)
      ,(lambda ()
         (let ((failp (plusp test-fail)))
           (log! log "~
-#---------------------------------------
Using ~(~A~)
Ran ~D tests, ~D passed, ~D failed~
~:[~%All tests apparently successful.~;:~:*~{~%  ~A~}~]
-#---------------------------------------~%"
                 lisp
                 n-tests test-pass test-fail (reverse failed-list))
           (when failp
             (log! log "To view full results and failures, try the following command:
     less -p ABORTED ~A" (enough-namestring log (pn))))
           (failure-if failp "~D test~:*~P failed" test-fail)))))))

(deftestcmd test (lisp test-scripts)
  "run all normal tests but upgrade tests
Use the preferred lisp implementation"
  (without-stopping () (test-basic lisp) (test-scripts lisp test-scripts)))

(defalias %t test)
