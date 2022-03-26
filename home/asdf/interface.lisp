;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.

(uiop/package:define-package :asdf/interface
  (:nicknames :asdf :asdf-utilities)
  (:recycle :asdf/interface :asdf)
  (:unintern
   #:loaded-systems ; makes for annoying SLIME completion
   #:output-files-for-system-and-operation) ; ASDF-BINARY-LOCATION function we use to detect ABL
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
   :asdf/component :asdf/system :asdf/system-registry :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action
   :asdf/output-translations :asdf/source-registry
   :asdf/forcing :asdf/plan :asdf/operate :asdf/find-system :asdf/parse-defsystem
   :asdf/bundle :asdf/concatenate-source
   :asdf/backward-internals :asdf/backward-interface :asdf/package-inferred-system)
  ;; Note: (1) we are NOT automatically reexporting everything from previous packages.
  ;; (2) we only reexport UIOP functionality when backward-compatibility requires it.
  (:export
   #:defsystem #:find-system #:load-asd #:locate-system #:coerce-name #:primary-system-name
   #:oos #:operate #:make-plan #:perform-plan #:sequential-plan
   #:system-definition-pathname
   #:search-for-system-definition #:find-component #:component-find-path
   #:compile-system #:load-system #:load-systems #:load-systems*
   #:require-system #:test-system #:clear-system
   #:operation #:make-operation #:find-operation
   #:upward-operation #:downward-operation #:sideway-operation #:selfward-operation
                      #:non-propagating-operation
   #:build-op #:make
   #:load-op #:prepare-op #:compile-op
   #:prepare-source-op #:load-source-op #:test-op #:define-op
   #:feature #:version #:version-satisfies #:upgrade-asdf
   #:implementation-identifier #:implementation-type #:hostname
   #:component-depends-on ; backward-compatible name rather than action-depends-on
   #:input-files #:additional-input-files
   #:output-files #:output-file #:perform #:perform-with-restarts
   #:operation-done-p #:explain #:action-description #:component-sideway-dependencies
   #:needed-in-image-p
   #:bundle-op #:monolithic-bundle-op #:precompiled-system #:compiled-file #:bundle-system
   #:program-system
   #:basic-compile-bundle-op #:prepare-bundle-op
   #:compile-bundle-op #:load-bundle-op #:monolithic-compile-bundle-op #:monolithic-load-bundle-op
   #:lib-op #:dll-op #:deliver-asd-op #:program-op #:image-op
   #:monolithic-lib-op #:monolithic-dll-op #:monolithic-deliver-asd-op
   #:concatenate-source-op
   #:load-concatenated-source-op
   #:compile-concatenated-source-op
   #:load-compiled-concatenated-source-op
   #:monolithic-concatenate-source-op
   #:monolithic-load-concatenated-source-op
   #:monolithic-compile-concatenated-source-op
   #:monolithic-load-compiled-concatenated-source-op
   #:operation-monolithic-p
   #:required-components
   #:component-loaded-p
   #:component #:parent-component #:child-component #:system #:module
   #:file-component #:source-file #:c-source-file #:java-source-file
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:static-file #:doc-file #:html-file
   #:file-type #:source-file-type
   #:register-preloaded-system #:sysdef-preloaded-system-search
   #:register-immutable-system #:sysdef-immutable-system-search
   #:package-inferred-system #:register-system-packages
   #:component-children
   #:component-children-by-name
   #:component-pathname
   #:component-relative-pathname
   #:component-name
   #:component-version
   #:component-parent
   #:component-system
   #:component-encoding
   #:component-external-format
   #:system-description
   #:system-long-description
   #:system-author
   #:system-maintainer
   #:system-license
   #:system-licence
   #:system-version
   #:system-source-file
   #:system-source-directory
   #:system-relative-pathname
   #:system-homepage
   #:system-mailto
   #:system-bug-tracker
   #:system-long-name
   #:system-source-control
   #:map-systems
   #:system-defsystem-depends-on
   #:system-depends-on
   #:system-weakly-depends-on
   #:*system-definition-search-functions*   ; variables
   #:*central-registry*
   #:*compile-file-warnings-behaviour*
   #:*compile-file-failure-behaviour*
   #:*resolve-symlinks*
   #:*verbose-out*
   #:asdf-version
   #:compile-condition #:compile-file-error #:compile-warned-error #:compile-failed-error
   #:compile-warned-warning #:compile-failed-warning
   #:error-name
   #:error-pathname
   #:load-system-definition-error
   #:error-component #:error-operation
   #:system-definition-error
   #:missing-component
   #:missing-component-of-version
   #:missing-dependency
   #:missing-dependency-of-version
   #:circular-dependency        ; errors
   #:duplicate-names #:non-toplevel-system #:non-system-system #:bad-system-name #:system-out-of-date
   #:package-inferred-system-missing-package-error
   #:operation-definition-warning #:operation-definition-error
   #:try-recompiling ; restarts
   #:retry
   #:accept
   #:coerce-entry-to-directory
   #:remove-entry-from-registry
   #:clear-configuration-and-retry
   #:*encoding-detection-hook*
   #:*encoding-external-format-hook*
   #:*default-encoding*
   #:*utf-8-external-format*
   #:clear-configuration
   #:*output-translations-parameter*
   #:initialize-output-translations
   #:disable-output-translations
   #:clear-output-translations
   #:ensure-output-translations
   #:apply-output-translations
   #:compile-file*
   #:compile-file-pathname*
   #:*warnings-file-type* #:enable-deferred-warnings-check #:disable-deferred-warnings-check
   #:enable-asdf-binary-locations-compatibility
   #:*default-source-registries*
   #:*source-registry-parameter*
   #:initialize-source-registry
   #:compute-source-registry
   #:clear-source-registry
   #:ensure-source-registry
   #:process-source-registry
   #:registered-system #:registered-systems #:already-loaded-systems
   #:resolve-location
   #:asdf-message
   #:*user-cache*
   #:user-output-translations-pathname
   #:system-output-translations-pathname
   #:user-output-translations-directory-pathname
   #:system-output-translations-directory-pathname
   #:user-source-registry
   #:system-source-registry
   #:user-source-registry-directory
   #:system-source-registry-directory

   ;; The symbols below are all DEPRECATED, do not use. To be removed in a further release.
   #:*asdf-verbose* #:run-shell-command
   #:component-load-dependencies #:system-registered-p #:package-system
   #+ecl #:make-build
   #:operation-on-warnings #:operation-on-failure #:operation-error
   #:compile-failed #:compile-warned #:compile-error
   #:module-components #:component-property #:traverse))
