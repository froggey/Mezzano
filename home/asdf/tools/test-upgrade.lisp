(in-package :asdf-tools)

;;; Upgrade tests

(defparameter *default-upgrade-test-tags*
  ;; We return a list of entries in reverse chronological order,
  ;; which should also be more or less the order of decreasing relevance.
  ;; By default, we only test the last of each relevant series.
  '("REQUIRE" "3.3.1" "3.3.0" "3.2.1" "3.2.0" "3.1.7" "3.1.2" "3.0.3" "2.26"))

(defparameter *all-upgrade-test-tags*
  '("REQUIRE" ;; a magic tag meaning whatever your implementation provides, if anything

    ;; Below are versions that once were notable enough to be worth testing an upgrade from.
    ;; This list is not meant at being exhaustive of releases, particularly not old ones.

    ;; The 3.3 series provides the asdf3.3 feature, meaning users can rely on
    ;; all its new features (proper phase separation) as well as earlier features.
    "3.3.1" ;; (2017-11-14) bug fixes, second and latest in 3.3 series
    "3.3.0" ;; (2017-10-06) first in 3.3 series

    ;; The 3.2 series provides the asdf3.2 feature, meaning users can rely on
    ;; all its new features (launch-program, improved bundle support), as well as
    ;; the improvements done in 3.1 (e.g. XDG support).
    "3.2.1" ;; (2017-04-03) bug fixes, second and last in 3.2 series
    "3.2.0" ;; (2017-01-08) first in 3.2 series

    ;; The 3.1 series provides the asdf3.1 feature, meaning users can rely on
    ;; all the stabilization work done in 3.0 so far, plus extra developments
    ;; in UIOP, package-inferred-system, and more robustification.
    "3.1.7" ;; (2016-03-23) more bug fixes, last in 3.1 series
    "3.1.6" ;; (2015-10-17) more bug fixes
    "3.1.5" ;; (2015-07-21) more bug fixes, what SBCL sports (as of 1.3.14, 2017-02-04)
    "3.1.4" ;; (2014-10-09) more bug fixes, source-registry cache, in LispWorks 7
    "3.1.3" ;; (2014-07-24) a bug fix release for 3.1.2
    "3.1.2" ;; (2014-05-06) the first ASDF 3.1 release

    ;; The 3.0 series is a stable release of ASDF 3
    ;; with Robert Goldman taking over maintainership at 3.0.2.
    ;; 3.0.0 was just 2.33.10 promoted, but version-satisfies meant it was suddenly
    ;; not compatible with ASDF2 anymore, so we immediately released 3.0.1
    "3.0.3" ;; (2013-10-22) last in the ASDF 3.0 series
    "3.0.2" ;; (2013-07-02) the first ASDF 3 in SBCL
    "3.0.1" ;; (2013-05-16) the first stable ASDF 3 release

    ;; 2.27 to 2.33 are Faré's "stable" ASDF 3 pre-releases
    "2.32" ;; (2013-03-05) the first really stable ASDF 3 pre-release
    "2.27" ;; (2013-02-01) the first ASDF 3 pre-release

    ;; The ASDF 2 series
    ;; Note that 2.26.x is where the refactoring that begat ASDF 3 took place.
    ;; 2.26.61 is the last single-file, single-package ASDF.
    "2.26" ;; (2012-10-30), last in ASDF 2 series, still sported by Quicklisp 2016-02-22 (!), long used by SBCL, etc.
    "2.22" ;; (2012-06-12) used by debian wheezy, etc.
    "2.20" ;; (2012-01-18) in CCL 1.8, Ubuntu 12.04 LTS
    "2.019" ;; (2011-11-29) still included in LispWorks in 2014.
    "2.014.6" ;; (2011-04-06) first included in Quicklisp, and for some time.
    "2.011" ;; (2010-12-09) long used by CLISP 2.49, Debian squeeze, Ubuntu 10.04 LTS.
    "2.008" ;; (2010-09-10) somewhat stable checkpoint in the ASDF 2 series.
    "2.000" ;; (2010-05-31) first stable ASDF 2 release.

    ;; The original ASDF 1 series
    "1.369" ;; (2009-10-27) the last release by Gary King
    "1.97" ;; (2006-05-14) the last release before Gary King takes over
    "1.85")) ;; (2004-05-16) the last release by Daniel Barlow (not 1.37, which is the README revision!)

(defun get-upgrade-tags (&optional (x *upgrade-test-tags*))
  (etypecase x
    (list x)
    ((or string symbol)
     (cond
       ((string-equal x :default)
        *default-upgrade-test-tags*)
       ((string-equal x :old)
        (remove-if (lambda (x) (member x *default-upgrade-test-tags* :test 'equal))
                   *all-upgrade-test-tags*))
       ((string-equal x :all)
        *all-upgrade-test-tags*)
       (t (ensure-list-of-strings (string x)))))))

(defun extract-tagged-asdf (tag)
  "extract an asdf version from git
Use at a given tag, put it under build/asdf-${tag}.lisp"
  (with-asdf-dir ()
    (ensure-directories-exist (pn "build/"))
    (unless (string-equal tag "REQUIRE")
      (let ((file (pn (strcat "build/asdf-" tag ".lisp"))))
        (unless (probe-file file)
          (cond
            ((version<= tag "2.26.61")
             (git `(show (,tag ":asdf.lisp") (> ,file))))
            (t
             (ensure-directories-exist (pn "build/old/build/"))
             (run `(pipe (git archive ,tag) (tar "xfC" - ,(pn "build/old/"))))
             (run `(make) :directory (pn "build/old/"))
             (rename-file-overwriting-target (pn "build/old/build/asdf.lisp") file)))))))
  (success))

(deftestcmd extract-all-tagged-asdf (upgrade-tags)
  "extract all asdf tags used for upgrade"
  (map () 'extract-tagged-asdf upgrade-tags)
  (success))

(defalias extract extract-all-tagged-asdf)

(defparameter *upgrade-test-methods* :default)

(defparameter *all-upgrade-test-methods*
  '((:load-asdf-lisp :load-asdf-lisp-clean)
    (:load-asdf-lisp :load-asdf-system)
    (:load-asdf-lisp :compile-load-asdf-upgrade)
    (:load-asdf-lisp :load-asdf-fasl)
    (() :load-asdf-fasl)
    (:load-asdf-lisp-and-test-uiop :load-asdf-fasl)))

(defun get-upgrade-methods (&optional (x *upgrade-test-methods*))
  (if (eq x :default) *all-upgrade-test-methods* x))

(defun valid-upgrade-test-p (lisp tag method)
  (declare (ignore method))
  (or
   (string-equal tag "REQUIRE") ;; we are hopefully always able to upgrade from REQUIRE
   (ecase lisp
     ;; ABCL works but is super-slow. Since we now punt on all of 2.x,
     ;; no need to check anything below 2.26.
     ((:abcl) (version<= "2.26" tag))

     ;; Allegro ships with versions 3*, so give up testing 2
     ;; Also, unpatched Allegro 10 has bug updating from 2.26 and before
     ((:allegro :allegromodern :allegro8 :allegromodern8
       :allegro_64 :allegromodern_64 :allegro8_64 :allegromodern8_64
       :allegro_s :allegromodern_s :allegro8_s :allegromodern8_s
       :allegro_64_s :allegromodern_64_s :allegro8_64_s :allegromodern8_64_s)
      (version<= "2.27" tag))

     ;; CCL fasl numbering broke loading of old asdf 2.0, and the punting for 2.26 fails,
     ;; but who cares since CCL has always been shipping recent versions of ASDF.
     ((:ccl) (version<= "2.27" tag))

     ;; CLASP is only supported as of 3.1.4.3
     ((:clasp) (version<= "3.1.4.3" tag))

     ;; My old Ubuntu 10.04LTS clisp 2.44.1 came wired in
     ;; with an antique ASDF 1.374 from CLC that can't be removed.
     ;; More recent CLISPs work.
     ;; 2.00[0-7] use UID, which fails on some old CLISPs,
     ;; but these old ASDF versions still can be loaded and upgraded from.
     ;; Note that for the longest time, CLISP has included 2.011 in its distribution.
     ;; However, whether we punt or don't punt, these should all work.
     ((:clisp) t)

     ;; CMUCL has problems with 2.32 and earlier because of
     ;; the redefinition of system's superclass component.
     ((:cmucl) (version<= "2.33" tag))

     ;; Skip many ECL tests, for various ASDF issues
     ((:ecl :ecl_bytecodes) (version<= "2.21" tag))

     ;; GCL 2.7.0 from at least late May 2014 is required, with ASDF 3.1.2 or later.
     ;; But since GCL doesn't ship with ASDF yet anyway, no need to test upgrade
     ;; against anything but the latest release.
     ((:gcl) (version<= "3.1.4" tag))

     ;; LispWorks is fine, but ASDF 3.0.3 has a bug and can't be loaded.
     ((:lispworks) (not (equal "3.0.3" tag)))

     ;; MKCL is only supported starting with specific versions 2.24, 2.26.x, 3.0.3.0.x, so skip.
     ((:mkcl) (version<= "3.1.2" tag))

     ;; all clear on these implementations
     ((:sbcl :scl) t)

     ;; XCL support starts with ASDF 2.014.2
     ;; — It also dies during upgrade trying to show the backtrace.
     ;; We recommend you replace XCL's asdf using:
     ;;     ./tools/asdf-tools install-asdf xcl
     ((:xcl) (version<= "2.15" tag)))))

(deftestcmd test-upgrade (lisp upgrade-tags upgrade-methods)
  "run upgrade tests
Use the preferred lisp implementation"
  (nest
   (with-asdf-dir ())
   (let ((log (newlogfile "upgrade" lisp)))
     ;; Remove stale FASLs from ASDF 1.x,
     ;; especially since different implementations may have the same fasl type
     (dolist (pattern '("build/*.*f*" "uiop/*.*f*" "test/*.*f*"))
       (map () 'delete-file (directory* pattern))))
   (loop :for tag :in upgrade-tags :do
     (loop :for method :in upgrade-methods
           :for description
             = (format nil "Testing ASDF upgrade on ~(~A~) from ~A to ~A using method ~(~{~A~^:~}~)"
                       lisp tag (version-from-file) method)
           :when (valid-upgrade-test-p lisp tag method) :do
             (success-if (and
                          (extract-tagged-asdf tag)
                          (run-test-lisp description
                           `((load "test/script-support.lisp")
                             (asdf-test::test-upgrade ,@method ,tag))
                           :lisp lisp :log log))
                         description))
    :finally (progn (log! log "Upgrade test succeeded for ~(~A~)" lisp)
                    (return (success))))))

(defalias u test-upgrade)
