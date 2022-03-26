;;;; package.lisp

(defpackage #:ql-util
  (:documentation
   "Utility functions used in various places.")
  (:use #:cl)
  (:export #:write-line-to-file
           #:without-prompting
           #:press-enter-to-continue
           #:replace-file
           #:copy-file
           #:delete-file-if-exists
           #:ensure-file-exists
           #:split-spaces
           #:first-line
           #:file-size
           #:safely-read
           #:safely-read-file
           #:make-versions-url))

(defpackage #:ql-setup
  (:documentation
   "Functions and variables initialized early in the Quicklisp client
   configuration.")
  (:use #:cl)
  (:export #:qmerge
           #:qenough
           #:*quicklisp-home*))

(defpackage #:ql-config
  (:documentation
   "Getting and setting persistent configuration values.")
  (:use #:cl #:ql-util #:ql-setup)
  (:export #:config-value))

(defpackage #:ql-impl
  (:documentation
   "Configuration of implementation-specific packages and interfaces.")
  (:use #:cl)
  (:export #:*implementation*)
  (:export #:definterface
           #:defimplementation
           #:show-interfaces)
  (:export #:lisp
           #:abcl
           #:allegro
           #:ccl
           #:clasp
           #:clisp
           #:cmucl
           #:cormanlisp
           #:ecl
           #:gcl
           #:lispworks
           #:mezzano
           #:mkcl
           #:scl
           #:sbcl))

(defpackage #:ql-impl-util
  (:documentation
   "Utility functions that require implementation-specific
   functionality.")
  (:use #:cl #:ql-impl)
  (:export #:call-with-quiet-compilation
           #:add-to-init-file
           #:rename-directory
           #:delete-directory
           #:probe-directory
           #:directory-entries
           #:delete-directory-tree
           #:map-directory-tree
           #:native-namestring
           #:directory-write-date))

(defpackage #:ql-network
  (:documentation
   "Simple, low-level network access.")
  (:use #:cl #:ql-impl)
  (:export #:open-connection
           #:write-octets
           #:read-octets
           #:close-connection
           #:with-connection))

(defpackage #:ql-progress
  (:documentation
   "Displaying a progress bar.")
  (:use #:cl)
  (:export #:make-progress-bar
           #:start-display
           #:update-progress
           #:finish-display))

(defpackage #:ql-http
  (:documentation
   "A simple HTTP client.")
  (:use #:cl #:ql-network #:ql-progress #:ql-config)
  (:export #:*proxy-url*
           #:fetch
           #:http-fetch
           #:*fetch-scheme-functions*
           #:scheme
           #:hostname
           #:port
           #:path
           #:url
           #:*maximum-redirects*
           #:*default-url-defaults*)
  (:export #:fetch-error
           #:unexpected-http-status
           #:unexpected-http-status-code
           #:unexpected-http-status-url
           #:too-many-redirects
           #:too-many-redirects-url
           #:too-many-redirects-count))

(defpackage #:ql-minitar
  (:documentation
   "A simple implementation of unpacking the 'tar' file format.")
  (:use #:cl)
  (:export #:unpack-tarball))

(defpackage #:ql-gunzipper
  (:documentation
   "An implementation of gunzip.")
  (:use #:cl)
  (:export #:gunzip))

(defpackage #:ql-cdb
  (:documentation
   "Read and write CDB files; code adapted from ZCDB.")
  (:use #:cl)
  (:export #:lookup
           #:map-cdb
           #:convert-index-file))

(defpackage #:ql-dist
  (:documentation
   "Generic functions, variables, and classes for interacting with the
   dist system. Documented, exported symbols are intended for public
   use.")
  (:use #:cl
        #:ql-util
        #:ql-http
        #:ql-setup
        #:ql-gunzipper
        #:ql-minitar)
  (:intern #:dist-version
           #:dist-url)
  (:import-from #:ql-impl-util
                #:delete-directory-tree
                #:directory-entries
                #:probe-directory)
  ;; Install/enable protocol
  (:export #:installedp
           #:install
           #:uninstall
           #:ensure-installed
           #:enabledp
           #:enable
           #:disable)
  ;; Preference protocol
  (:export #:preference
           #:preference-file
           #:preference-parent
           #:forget-preference)
  ;; Generic
  (:export #:all-dists
           #:canonical-distinfo-url
           #:enabled-dists
           #:find-dist
           #:find-dist-or-lose
           #:find-system
           #:find-release
           #:dist
           #:system
           #:release
           #:base-directory
           #:relative-to
           #:metadata-name
           #:install-metadata-file
           #:short-description
           #:provided-releases
           #:provided-systems
           #:installed-releases
           #:installed-systems
           #:name)
  ;; Dists
  (:export #:dist
           #:dist-merge
           #:find-system-in-dist
           #:find-release-in-dist
           #:system-index-url
           #:release-index-url
           #:available-versions-url
           #:available-versions
           #:version
           #:subscription-url
           #:new-version-available-p
           #:dist-difference
           #:fetch-dist
           #:initialize-release-index
           #:initialize-system-index
           #:with-consistent-dists)
  ;; Dist updates
  (:export #:available-update
           #:update-release-differences
           #:show-update-report
           #:update-in-place
           #:install-dist
           #:subscription-inhibition-file
           #:inhibit-subscription
           #:uninhibit-subscription
           #:subscription-inhibited-p
           #:subscription-unavailable
           #:subscribedp
           #:subscribe
           #:unsubscribe)
  ;; Releases
  (:export #:release
           #:project-name
           #:system-files
           #:archive-url
           #:archive-size
           #:ensure-archive-file
           #:archive-content-sha1
           #:archive-md5
           #:prefix
           #:local-archive-file
           #:ensure-local-archive-file
           #:check-local-archive-file
           #:invalid-local-archive
           #:invalid-local-archive-file
           #:invalid-local-archive-release
           #:missing-local-archive
           #:badly-sized-local-archive
           #:delete-and-retry)
  ;; Systems
  (:export #:dist
           #:release
           #:preference
           #:system-file-name
           #:required-systems)
  ;; Misc
  (:export #:standard-dist-enumeration-function
           #:*dist-enumeration-functions*
           #:find-asdf-system-file
           #:system-definition-searcher
           #:system-apropos
           #:system-apropos-list
           #:dependency-tree
           #:clean
           #:unknown-dist))

(defpackage #:ql-dist-user
  (:documentation
   "A package that uses QL-DIST; useful for playing around in without
   clobbering any QL-DIST internals.")
  (:use #:cl
        #:ql-dist))

(defpackage #:ql-bundle
  (:documentation
   "A package for supporting the QL:BUNDLE-SYSTEMS function.")
  (:use #:cl #:ql-dist #:ql-impl-util)
  (:shadow #:find-system
           #:find-release)
  (:export #:bundle
           #:requested-systems
           #:ensure-system
           #:ensure-release
           #:write-bundle
           #:add-systems-recursively
           #:object-not-found
           #:system-not-found
           #:system-not-found-system
           #:release-not-found
           #:bundle-directory-exists
           #:bundle-directory-exists-directory))

(defpackage #:quicklisp-client
  (:documentation
   "The Quicklisp client package, intended for end-user Quicklisp
   commands and configuration parameters.")
  (:nicknames #:quicklisp #:ql)
  (:use #:cl
        #:ql-util
        #:ql-impl-util
        #:ql-dist
        #:ql-http
        #:ql-setup
        #:ql-config
        #:ql-minitar
        #:ql-gunzipper)
  (:shadow #:uninstall)
  (:shadowing-import-from #:ql-dist
                          #:dist-version
                          #:dist-url)
  (:export #:dist-version
           #:dist-url)
  (:export #:quickload
           #:*quickload-prompt*
           #:*quickload-verbose*
           #:*quickload-explain*
           #:system-not-found
           #:system-not-found-name
           #:uninstall
           #:uninstall-dist
           #:qmerge
           #:*quicklisp-home*
           #:*initial-dist-url*
           #:*proxy-url*
           #:config-value
           #:setup
           #:provided-systems
           #:system-apropos
           #:system-apropos-list
           #:system-list
           #:client-version
           #:client-url
           #:available-client-versions
           #:install-client
           #:update-client
           #:update-dist
           #:update-all-dists
           #:available-dist-versions
           #:add-to-init-file
           #:use-only-quicklisp-systems
           #:write-asdf-manifest-file
           #:where-is-system
           #:help
           #:register-local-projects
           #:local-projects-searcher
           #:*local-project-directories*
           #:list-local-projects
           #:list-local-systems
           #:who-depends-on
           #:bundle-systems))

(in-package #:quicklisp-client)
