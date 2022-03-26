(require 'slime-presentations)
(require 'slime-tests)
(require 'slime-repl-tests "test/slime-repl-tests")

(define-slime-ert-test pick-up-presentation-at-point ()
  "Ensure presentations are found consistently."
  (cl-labels ((assert-it (point &optional negate)
                       (let ((result
                              (cl-first
                               (slime-presentation-around-or-before-point point))))
                         (unless (if negate (not result) result)
                           (ert-fail
                            (format "Failed to pick up presentation at point %s"
                                    point))))))
    (with-temp-buffer
      (slime-insert-presentation "1234567890" `(:inspected-part 42))
      (insert "     ")
      (assert-it 1)
      (assert-it 2)
      (assert-it 3)
      (assert-it 4)
      (assert-it 5)
      (assert-it 10)
      (assert-it 11)
      (assert-it 12 t))))

(def-slime-test (pretty-presentation-results (:fails-for "allegro"))
    (input result-contents)
    "Test some more simple situations dealing with print-width and stuff.

Very much like `repl-test-2', but should be more stable when
presentations are enabled, except in allegro."
    '(("\
(with-standard-io-syntax
 (write (make-list 15 :initial-element '(1 . 2)) :pretty t :right-margin 75)
 0)"
       "\
SWANK> \
(with-standard-io-syntax
 (write (make-list 15 :initial-element '(1 . 2)) :pretty t :right-margin 75)
 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]")
      ;; Two times to test the effect of FRESH-LINE.
      ("\
(with-standard-io-syntax
 (write (make-list 15 :initial-element '(1 . 2)) :pretty t :right-margin 75)
 0)"
       "SWANK> \
(with-standard-io-syntax
 (write (make-list 15 :initial-element '(1 . 2)) :pretty t :right-margin 75)
 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]"))
  (slime-test-repl-test input result-contents))

(provide 'slime-presentations-tests)
