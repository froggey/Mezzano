(in-package asdf)

(assert
(equalp
 (let ((*system-definition-search-functions*
        '(sysdef-central-registry-search))
       (*central-registry* (list "/tmp/ok-1/" "/tmp/bad" "/tmp/ok-2/")))
   (handler-bind
       ((error (lambda (c)
                 (when (find-restart 'remove-entry-from-registry)
                   (invoke-restart 'remove-entry-from-registry)))))
     (find-system "a" nil))
   *central-registry*)
 (list "/tmp/ok-1/" "/tmp/ok-2/"))
)

(assert
(equalp
 (let ((*system-definition-search-functions*
        '(sysdef-central-registry-search))
       (*central-registry* (list "/tmp/ok-1/" "/tmp/bad" "/tmp/ok-2/")))
   (handler-bind
       ((error (lambda (c)
                 (when (find-restart 'coerce-entry-to-directory)
                   (invoke-restart 'coerce-entry-to-directory)))))
     (find-system "a" nil))
   *central-registry*)
 (list "/tmp/ok-1/" #p"/tmp/bad/" "/tmp/ok-2/"))
)

