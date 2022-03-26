#+xcvb (module ())

(in-package :cl-user)

#+:abcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

#+(or cmu genera)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'excl:stream-write-string)
    (require "streamc.fasl")))

#+(or ecl clasp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(macrolet
    ((frob ()
       (let ((gray-class-symbols
              '(#:fundamental-stream
                #:fundamental-input-stream #:fundamental-output-stream
                #:fundamental-character-stream #:fundamental-binary-stream
                #:fundamental-character-input-stream #:fundamental-character-output-stream
                #:fundamental-binary-input-stream #:fundamental-binary-output-stream))
             (gray-function-symbols
              '(#:stream-read-char
                #:stream-unread-char #:stream-read-char-no-hang
                #:stream-peek-char #:stream-listen #:stream-read-line
                #:stream-clear-input #:stream-write-char #:stream-line-column
                #:stream-start-line-p #:stream-write-string #:stream-terpri
                #:stream-fresh-line #:stream-finish-output #:stream-force-output
                #:stream-clear-output #:stream-advance-to-column
                #:stream-read-byte #:stream-write-byte)))
	 `(progn

            (defpackage impl-specific-gray
              (:use :cl)
              (:import-from
               #+sbcl :sb-gray
               #+allegro :excl
               #+cmu :ext
               #+(or clisp ecl mocl clasp) :gray
               #+openmcl :ccl
               #+lispworks :stream
               #+(or abcl genera) :gray-streams
               #+mezzano :mezzano.gray
               #-(or sbcl allegro cmu clisp openmcl lispworks ecl abcl mocl mocl genera mezzano) ...
               ,@gray-class-symbols
               ,@gray-function-symbols)
              (:export
               ,@gray-class-symbols
               ,@gray-function-symbols))

            (defpackage :trivial-gray-streams
              (:use :cl)
              (:import-from #:impl-specific-gray
                            ;; We import and re-export only
                            ;; function symbols;
                            ;; But we define our own classes
                            ;; mirroring the gray class hierarchy
                            ;; of the lisp implementation (this
                            ;; is necessary to define our methods
                            ;; for particular generic functions)
                            ,@gray-function-symbols)
              (:export ,@gray-class-symbols
                       ,@gray-function-symbols
                       ;; extension functions
                       #:stream-read-sequence
                       #:stream-write-sequence
                       #:stream-file-position
                       ;; deprecated
                       #:trivial-gray-stream-mixin))))))
  (frob))
