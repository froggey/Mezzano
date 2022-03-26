;; This file defines test-mutual-redefinition-1 but ALSO defines test-mutual-redefinition-2
;; There would be an infinite loop if find-system didn't specifically avoid
;; loading a definition twice in the same "session".
(defsystem test-mutual-redefinition-1)
(defsystem test-mutual-redefinition-2)
