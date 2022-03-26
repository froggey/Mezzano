ASDF: Another System Definition Facility
========================================

For general information about ASDF, consult the web page:
<https://common-lisp.net/project/asdf/>

For some reference documentation, read the manual:
<https://common-lisp.net/project/asdf/asdf.html>

For a guide on how to use it, read our "best practices" document:
<https://github.com/fare/asdf/blob/master/doc/best_practices.md>

Below is a guide for ASDF developers. It is not meant for ASDF users.

[TOC]


Building ASDF
-------------

First, make sure ASDF is checked out under a path registered to the source-registry,
if that isn't the case yet (see the [manual](http://common-lisp.net/project/asdf/asdf.html)).
One place would be:

    ~/.local/share/common-lisp/source/asdf/

or, assuming your implementation provides ASDF 3.1 or later:

    ~/common-lisp/asdf/


If you cloned our git repository rather than extracted a tarball,
bootstrap a copy of `build/asdf.lisp` with:

    make


Building the documentation
--------------------------

The manual is also in the [doc/](doc/) subdirectory, and can be prepared with:

    make -C doc


Testing ASDF
------------

Before you may run tests, you need a few CL libraries.
The simplest way to get them is as follows, but read below:

    make ext

_NOTA BENE_: You may also need to run `make ext` again
after you `git pull` or switch branch, to update the `ext/` directory.
This unhappily is not automatic.
If for some reason tests fail, particularly due to an error
compiling, loading or running a library, then run `make ext` and try again.

The above `make` target uses `git submodule update --init` to download
all these libraries using git. If you don't otherwise maintain your
own set of carefully controlled CL libraries, that's what you want to use.
However, it is only available if you have a git checkout of ASDF;
not if you used a tarball.
If you use a tarball or otherwise do maintain your own set
of carefully controlled CL libraries then you will want to use whichever tools
you use (e.g. `quicklisp`, `clbuild`, or your own scripts around `git`)
to download these libraries:
`alexandria`, `asdf-encodings`, `cl-launch`, `closer-mop`, `cl-ppcre`,
`cl-scripting`, `fare-mop`, `fare-quasiquote`, `fare-utils`, `inferior-shell`,
`lisp-invocation`, `named-readtables`, `optima`.

If you are a CL developer, you may already have them, or may want
to use your own tools to download a version of them you control.
If you use [Quicklisp](https://www.quicklisp.org/), you may let
Quicklisp download those you don't have.
In these cases, you may NOT want to use the git submodules from `make ext`;
you may undo a `make ext` with `make noext`.
Otherwise, if you want to let ASDF download known-working versions
of its dependencies, you can do it with `make ext`.

Once you have all the required libraries and the asdf-tools script can find
a suitable Common Lisp implementation, you may run all the tests
on a given Common Lisp implementation `$L`, with your favorite installed system `$S`, using:

    make t u l=$L s=$S

To run only the regression test scripts, try simply:

    make l=$L test-scripts


Lisp Scripting test system
--------------------------

ASDF by default uses a shell script in `./test/run-tests.sh` to run the scripts
that orchestrate its tests.

An alternate build and test system is available
that uses Common Lisp as a scripting language.
It is disabled by default because
the new maintainer is having trouble with it in some of his environments.
It worked fine for the previous maintainer in his environments,
and may be particularly useful on Windows if and when
the shell-based test system fails or is not available.
Its source code is in [tools/](tools/) and
you can invoke it without going through GNU make,
using the script [make-asdf.sh](make-asdf.sh),
or, on Windows, [make-asdf.bat](make-asdf.bat).

To use this alternate test system, pass to `make` the extra arguments `-f Makefile-lisp-scripting`
as in for instance:

    make -f Makefile-lisp-scripting t l=sbcl

Or you can make that your local default (assuming GNU make) using:

    echo "include Makefile-lisp-scripting" > GNUmakefile

These Lisp tools by default use Clozure Common Lisp (CCL) to build and run a binary
`build/asdf-tools` that will orchestrate the tests.
By defining and exporting the variable `LISP` to be one of `ccl`, `sbcl` or `allegro`, you
can have it use an alternate Common Lisp implementation instead.
Install CCL (respectively SBCL or Allegro) and make sure an executable called
`ccl` (respectively `sbcl` or `alisp`) is in your `PATH`,
or that you export a variable `CCL` (respectively `SBCL` or `ALLEGRO`)
that points to the executable.
To use a further Common Lisp implementation, suitably edit the script
[`tools/asdf-tools`](tools/asdf-tools),
or, on Windows, the batch file [`tools/asdf-tools.bat`](tools/asdf-tools.bat).
(Note that we recommend SBCL 1.3.13 or later when on Windows.)

Note that the executable `build/asdf-tools` is built
the first time you test ASDF.
When you update ASDF, via e.g. `git pull` or a branch switch,
you may have to update it, with:

    make -f Makefile-lisp-scripting build-asdf-tools

The reason this is not done automatically every time is because
building it depends on a working ASDF;
but when you're modifying ASDF and testing it, you cannot rely on a working ASDF:
indeed, a developer may not only make mistakes, but may deliberately
introduce or re-introduce bugs at some place to test code in another place.


Debugging ASDF
--------------

To interactively debug ASDF, you may load it in such a way that `M-.` will work,
by installing the source code, and running:

    (map () 'load (asdf:input-files :monolithic-concatenate-source-op "asdf/defsystem"))

To interactively use the `asdf-tools`, you need to either have
all its dependencies installed and configured.
If you're using them through the `ext/` directory and `make ext`,
then you may need to emulate
what the script in [tools/asdf-tools](tools/asdf-tools) does
with respect to initializing the source-registry.
Note that it also declares a system for `cl-launch/dispatch`;
you can either do something similar, or expand the source for `cl-launch` with
`make -C ext/cl-launch source` so `cl-launch.asd` will be created.


Using ASDF internals
--------------------

If you have to use or extend internal functionality not currently exported by
ASDF, please contact us and have us negotiate a proper, stable, tested interface
that you can actually rely on. Also, please *DO NOT* refer to specific
subpackages such as `asdf/find-system` from the outside of ASDF, because
functions may occasionally be moved from one internal package to the other,
without notification. They have in the past and will in the future.
Instead, when refering to symbols in ASDF, we recommend you either have
your package `:use` the package `:asdf` or `:import-from` it, or that
you shall use `asdf:` or `asdf::` as a prefix to the symbols.
And once again, please contact us if you have to use non-exported symbols.

Also, the normal way of extending ASDF is to use our class hierarchies for
`component` and `operation` and to define methods on `component-depends-on`,
`perform`, `input-files`, `output-files`.
A common mistake seems to be that some people define methods on `operate`,
which usually is not at all what they think it is.


How do I navigate this source tree?
-----------------------------------

*   [asdf.asd](asdf.asd)
    *   The system definition for building ASDF with ASDF.

*   `*.lisp`
    *   The source code files for `asdf/defsystem`.
        See [asdf.asd](asdf.asd) for the order in which they are loaded.
        All exported functions should have docstrings,
        and all internal functions should have comments.
        If any definition is insufficiently documented,
        please tell us: that's a bug.

*   [uiop/](uiop/)
    * Utilities of Implementation- and OS- Portability,
      the portability layer of ASDF. It has its own [README](uiop/README.md),
      and exported functions should all have docstrings and other ones comment,
      or once again it's a bug.

*   [Makefile](Makefile)
    *   The classical `Makefile` used for development purposes.
        Regular users only need to call `make` with the default target.
        Developers will typically use the like of
        `make t l=sbcl` or `make u l=ccl`.

*   [bin/](bin/)
    *   [bump-version](bin/bump-version) --
        a script to bump the version of ASDF, used by the classic `Makefile`.
        Use it with e.g. `./bin/bump-version 3.4.5`
        to test with the next version number before you release.
        NB: ASDF's version number notably affects the behavior of ASDF
        with respect to deprecated functions.

*   [tools/](tools/)
    * `asdf-tools`, a system to build, test and release ASDF. It includes:
        *   [asdf-tools](tools/asdf-tools) --
            a shell script to run it as a shell command.
        *   [asdf-tools.bat](tools/asdf-tools.bat) --
            a Windows batch file to run the above.
        *   [asdf-tools.asd](tools/asdf-tools.asd) --
            system definition for asdf-tools
        *   `*.lisp` -- the source code for the `asdf-tools` system,
            except for the few files below.
            Check the `.asd` file for the order in which to read them.
    *   Also a couple scripts to help ASDF users:
        *   [load-asdf.lisp](tools/load-asdf.lisp) --
            a working example script to load, configure and use ASDF
            in a self-contained project
        *   [install-asdf.lisp](install-asdf.lisp) --
            replace and update an implementation's ASDF
        *   [cl-source-registry-cache.lisp](cl-source-registry-cache.lisp) --
            update a cache for the source-registry as a standalone script.

*   [Makefile-lisp-scripting](Makefile-lisp-scripting),
    [make-asdf.sh](make-asdf.sh) and [make-asdf.bat](make-asdf.bat)
    *   Minimal Makefile and scripts to invoke
        the lisp scripting variants of the build system.

*   [version.lisp-expr](version.lisp-expr)
    *   The current version. Bumped up every time the code changes, using:

            make bump

*   [doc/](doc/)
    *   Documentation for ASDF, including:
        *   [index.html](doc/index.html) --
            the web page for <http://common-lisp.net/project/asdf/>
        *   [asdf.texinfo](doc/asdf.texinfo) -- our manual
        *   [Makefile](doc/Makefile) -- how to build the manual
        *   [cclan.png](doc/cclan.png) [lisp-logo120x80.png](doc/lisp-logo120x80.png)
            [style.css](doc/style.css) [favicon.ico](doc/favicon.ico)
            -- auxiliaries of [index.html](doc/index.html)

*   [test/](test/)
    *   Regression test scripts (and ancillary files) for developers to check
        that they don't unintentionally break any of the functionality of ASDF.
        They are far from covering all of ASDF, but they are a good start.
        *   [script-support.lisp](test/script-support.lisp) --
            the common test infrastructure used by our tests
        *   [run-tests.sh](test/run-tests.sh) --
            the shell script used by the classic `Makefile` to run tests.
            It is not used by the Lisp scripting variant of the `Makefile`.

*   [contrib/](contrib/)
    *   A few contributed files that show case how to use ASDF
        or help with debugging it or debugging programs that use it.

*   [debian/](debian/)
    *   Files for packaging on Debian, Ubuntu, etc.
        (now only present in the debian branch).

*   [build/](build/)
    *   Where the `Makefile` and `asdf-tools` store their output files,
        including:
        * `asdf.lisp` -- the current one-file deliverable of ASDF
        * `asdf-*.lisp` -- for upgrade test purposes, old versions
        * `asdf-tools` -- the executable for asdf-tools (.exe on Windows)
        * `results/` -- logs of tests that have been run
        * `fasls/` -- output files while running tests

*   [ext/](ext/)
    * External dependencies, that can be populated with `make ext`
      or equivalently with `git submodule update --init`.
      Depopulate it with `make noext`
      or equivalently with: `submodule deinit .`

*   [README.md](README.md)
    * This file.

*   [TODO](TODO)
    * Plenty of ideas for how to further improve ASDF
      (not all of them guaranteed good ideas.)
