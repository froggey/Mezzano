(lambda ()
  (multiple-value-call #'(lambda (&optional slot &rest x)
                           (declare (ignore x)))
    (blah)))

(lambda ()
  (multiple-value-bind (umin umax)
      (IF (foo)
          123
          321)
    (IF (bar umin)
        (baz UMIN)
        t)))

(lambda (&key (x nil xp))
  xp)

(lambda (a)
  (let ((foo '1234))
    (if a
        (setq foo 'true-branch)
        (setq foo 'false-branch))
    foo))

(lambda (form)
  (flet ((save-tag (tag)
	   (if tag
               *load-list*)))
    (save-tag form)))

(lambda (entry)
  (let ((value 0))
    (LET ((G521693 (IF ENTRY ENTRY 0)))
      (LET ((STORE521691 (stuff VALUE)))
        (SETQ VALUE STORE521691) G521693))))
