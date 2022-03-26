;;;
;;; DESCRIPTION:
;;;
;;; This file uses a "Hellow world!" string which is in an another C
;;; file called hello_aux.c. Both hello.lisp and hello_aux.c are
;;; compiled and linked into either
;;;
;;;	1) a FASL file (see build_fasl.lisp)
;;;	2) a shared library (see build_dll.lisp)
;;;	3) or a standalone executable file. (build_exe.lisp)
;;;
;;; USE:
;;;
;;; Launch a copy of ECL and load this file in it
;;;
;;;	(load "readme.lisp")
;;;
(require 'asdf)

(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILDING hello_aux.o FILE
;;;
")

;;;
;;; * We compile hello.lisp and hello_aux.c separately.
;;;
;; (compile-file "hello.lisp" :system-p t)

(c::compiler-cc "hello_aux.c" (compile-file-pathname "hello_aux.c" :type :object))

(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILDING A STANDALONE EXECUTABLE
;;;
")

;;
;; * Combine files in a standalone executable. We reuse the files
;;   from the previous example
;;

(defconstant +standalone-exe+ (compile-file-pathname "hellow" :type :program))

(push (make-pathname :name nil :type nil :version nil
                     :defaults *load-truename*)
      asdf:*central-registry*)

;; Create hellow using program-op
(delete-file-if-exists +standalone-exe+)
(asdf:make "hellow")

;;
;; * Test the program
;;
(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TESTING A STANDALONE EXECUTABLE. Also test :no-uiop t
;;;

")
(assert-equal
 (uiop:run-program (format nil "./~A" +standalone-exe+) :output :lines)
 '("Good morning sunshine!" "Hello world!" "Good bye sunshine."))


(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AGAIN USING MAKE-BUILD. Also test uiop.
;;;

")
;;; Now create an executable using the legacy make-build interface
(delete-file +standalone-exe+)
(asdf:make-build :hellow
                 :type :program
                 :move-here "./"
                 :prologue-code "printf(\"In the beginning, there was nothing.\\n\");fflush(stdout);"
                 :epilogue-code '(progn
                                  (uiop:format! t "~%The end.~%")
                                  (uiop:quit 0))
                 ;;:no-uiop t
                 :ld-flags
                 (list (namestring (compile-file-pathname "hello_aux.c" :type :object))))
(assert-equal
 (uiop:run-program (format nil "./~A" +standalone-exe+) :output :lines)
 '("In the beginning, there was nothing." "Hello world!" "The end."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLEAN UP
;;;

;; (delete-file (compile-file-pathname "hello.lisp" :type :object))
(delete-file (compile-file-pathname "hello_aux.c" :type :object))
(delete-file +standalone-exe+)
