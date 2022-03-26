(defpackage :test-asdf/monodll (:use :test-asdf/monodll-1)) ;; dummy, for package-inferred-system dependencies.

(ffi:clines
 "extern int always_7();"
 "extern " #+mkcl "MKCL_DLLEXPORT " "int always_42() {
        return 6*always_7();
}
")
