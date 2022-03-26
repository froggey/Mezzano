(cl:in-package #:clim-tests)

(def-suite* :mcclim.commands
  :in :mcclim)

;;; Utilities

(defmacro with-command-table ((var name) &body body)
  (alexandria:once-only (name)
    `(let ((,var (make-command-table ,name)))
       (declare (ignorable ,var))
       (unwind-protect
            (progn ,@body)
         (remhash ,name climi::*command-tables*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; command tables
(define-command-table no-menu-test-table)

(add-command-to-command-table '(com-test-command) 'no-menu-test-table
                              :keystroke '(#\t))

(test commands.map-over-command-table-keystrokes.no-menu
  (let ((count 0))
    (map-over-command-table-keystrokes
     (lambda (menu-name gesture item)
       (incf count)
       (is (equal nil menu-name))
       (is (equal '(:keyboard #\t 0) gesture))
       (is (equal (lookup-keystroke-command-item
                   gesture 'no-menu-test-table)
                  (command-menu-item-value item))))
     'no-menu-test-table)
    (is (= 1 count))))

(define-command-table menu-test-table)

(add-command-to-command-table '(com-test-command) 'menu-test-table
                              :keystroke '(#\u)
                              :menu "Test")

(test commands.map-over-command-table-keystrokes.menu
  (let ((count 0))
    (map-over-command-table-keystrokes
     (lambda (menu-name gesture item)
       (incf count)
       (is (equal "Test" menu-name))
       (is (equal '(:keyboard #\u 0) gesture))
       (is (equal (lookup-keystroke-command-item
                   gesture 'menu-test-table)
                  (command-menu-item-value item))))
     'menu-test-table)
    (is (= 1 count))))

;; (define-command-table removal-test-table)
;; (add-command-to-command-table 'com-test-command 'removal-test-table)
;; (remove-command-from-command-table 'com-test-command 'removal-test-table)
;; (signals command-not-present
;;   (remove-command-from-command-table 'com-test-command
;;                                      'removal-test-table))

;;; command table errors (see 27.2)

;; (is (subtypep 'command-table-error 'error))
;; (is (subtypep 'command-table-not-found 'command-table-error))
;; (is (subtypep 'command-table-already-exists 'command-table-error))
;; (is (subtypep 'command-not-present 'command-table-error))
;; (is (subtypep 'command-not-accessible 'command-table-error))
;; (is (subtypep 'command-already-present 'command-table-error))

;; (let ((condition (make-condition 'command-table-error
;;                                  :format-control "~A"
;;                                  :format-arguments '(!))))
;;   (is-true (find #\! (format nil "~A" condition))))

;;; not actually required to DTRT here, but we use this form (without
;;; control and arguments) internally, so make sure that we don't
;;; error out recursively when in the debugger with one of these.

(test commands.command-no-present.print
  (let ((condition (make-condition 'command-not-present)))
    (is-true (stringp (format nil "~A" condition)))))

;;; Presentation translators

(test commands.find-presentation-translator.smoke
  (with-command-table (table 'test)
    ;; Not present - should signal
    (signals command-not-present
      (find-presentation-translator 'dummy-translator 'test))
    ;; Not present, but with ERRORP being NIL - should not signal
    (is (eq nil (find-presentation-translator
                 'dummy-translator 'test :errorp nil)))
    ;; Present - should be found.
    (let ((translator (define-presentation-translator dummy-translator
                          (integer string test)
                          (object)
                        (princ-to-string object))))
      (is (eq translator
              (find-presentation-translator 'dummy-translator 'test))))))

(test commands.add-presentation-translator-to-command-table.smoke
  (with-command-table (table1 'test1)
    (let ((translator (define-presentation-translator dummy-translator
                          (integer string test1)
                          (object)
                        (princ-to-string object))))
      (with-command-table (table2 'test2)
        ;; Not present
        (add-presentation-translator-to-command-table table2 translator)
        (is-true (find-presentation-translator 'dummy-translator 'test2))
        ;; Already present - should signal.
        (signals command-already-present
          (add-presentation-translator-to-command-table table2 translator))
        ;; Already present, but with ERRORP being NIL - should not signal.
        (finishes (add-presentation-translator-to-command-table
                   table2 translator :errorp nil))))))

(test commands.remove-presentation-translator-from-command-table.smoke
  (with-command-table (table 'test)
    ;; Not present - should signal.
    (signals command-not-present
      (remove-presentation-translator-from-command-table
       'test 'dummy-translator))
    ;; Not present, but with ERRORP being NIL - should do nothing.
    (remove-presentation-translator-from-command-table
     'test 'dummy-translator :errorp nil)
    ;; Present - should be removed.
    (define-presentation-translator dummy-translator
        (integer string test)
        (object)
      (princ-to-string object))
    (is-true (find-presentation-translator
              'dummy-translator 'test :errorp nil))
    (remove-presentation-translator-from-command-table 'test 'dummy-translator)
    (is-false (find-presentation-translator
               'dummy-translator 'test :errorp nil))))
