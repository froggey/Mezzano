(defpackage :mezzano.xp
  (:use :cl)
  (:export #:format
           #:formatter
           #:copy-pprint-dispatch
           #:pprint-dispatch
           #:set-pprint-dispatch
           #:pprint-fill
           #:pprint-linear
           #:pprint-tabular
           #:pprint-logical-block
           #:pprint-pop
           #:pprint-exit-if-list-exhausted
           #:pprint-newline
           #:pprint-indent
           #:pprint-tab
           #:*print-pprint-dispatch*
           #:*print-right-margin*
           #:*default-right-margin*
           #:*print-miser-width*
           #:*print-lines*
           #:*last-abbreviated-printing*
           #:*print-shared*))
