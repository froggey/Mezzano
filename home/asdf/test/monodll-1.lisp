(defpackage :test-asdf/monodll-1 (:use)) ;; dummy, for package-inferred-system dependencies.

(ffi:clines
 "extern " #+mkcl "MKCL_DLLEXPORT " "int always_7() {
        return 7;
}
")
