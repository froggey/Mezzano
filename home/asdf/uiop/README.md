UIOP, the Utilities for Implementation- and OS- Portability
===========================================================

UIOP is the portability layer of ASDF.
It provides utilities that abstract over discrepancies between implementations,
between operating systems, and between what the standard provides and
what programmers actually need, to write portable Common Lisp programs.

It is organized by topic in many files, each of which defines its own package
according to its topic: e.g [pathname.lisp](pathname.lisp)
will define package `UIOP/PATHNAME` and contain utilities related to
the handling of pathname objects.
All exported symbols are reexported in a convenience package `UIOP`,
except for those from `UIOP/COMMON-LISP`.
We recommend package `UIOP` be used to access all the symbols.

The files that constitute UIOP are, in dependency loading order:

* [package](package.lisp):
  deals with packages and their symbols, most notably including
  `define-package`, a variant of `defpackage` capable of hot-upgrade,
  or `symbol-call` and `find-symbol*` that are also useful for use in `.asd`
  files before packages have been defined.

* [common-lisp](common-lisp.lisp):
  lets you paper over various sub-standard implementations.
  Big offenders are Corman, GCL, Genera, MCL, none of them regularly maintained.
  Supported without serious issues are:
  ABCL, Allegro, CCL, CMUCL, CLASP, CLISP, ECL, LispWorks, MKCL, SBCL, SCL, XCL.

* [utility](utility.lisp):
  provides macros and functions that do not involve I/O;
  it handles control-flow, (p)lists, characters, strings, functions, classes,
  conditions, "stamps" (real number or boolean for +/- infinity), etc.
  It also sports `uiop-debug`, a useful tool to help you debug programs.

* [version](version.lisp):
  manages ASDF-style versioning and a related `with-deprecation` facility
  to gracefully declare that users should stop using some deprecated functions.

* [os](os.lisp):
  extracts information from your environment, including an ABI identifier,
  features that distinguish Unix vs Windows,
  `getenv`, `hostname`, `getcwd` and `chdir`, etc.

* [pathname](pathname.lisp):
  overcomes the gruesome non-portability trap that are CL pathnames
  (and their lovecraftian "logical" variant), offering a vast array of functions
  and a sensible, usable abstraction to specify relative pathnames.
  It has a function `merge-pathnames*` to use instead of `merge-pathnames`, or
  even better, `subpathname` and its variant `subpathname*`; it has also plenty
  of functions for dealing with pathnames being directory vs file,
  physical vs logical, absolute vs relative, and more.

* [filesystem](filesystem.lisp):
  provides portable access to the filesystem, inspecting it,
  only using truename when desired, using native OS namestrings,
  atomic file renaming, creating or deleting directories, etc.

* [stream](stream.lisp):
  portably deals with `*stderr*` vs `*error-output*`, character encodings
  (external formats), element types, safe `read`ing and `write`ing,
  opening files, using temporary files, flushing output buffers,
  providing `format`-like designators for streams, consuming or copying streams,
  concatenating streams or files, copying files, etc.

* [image](image.lisp):
  portably deals with images, dumping them, restoring from them,
  registering hooks to run at suitable events in the image lifetime,
  printing backtraces, handling fatal conditions, using or avoiding debug modes,
  accessing command line arguments or quitting the process.

* [lisp-build](lisp-build.lisp):
  portably compiles Common Lisp code, handles compilation results,
  muffles uninteresting conditions, saves and restores deferred warnings,
  runs hooks around compilation (to e.g. control optimizations or syntax),
  identifies the pathname of the current file, combines FASLs, etc.

* [launch-program](launch-program.lisp):
  semi-portably launches a program as an asynchronous external subprocess.
  Available functionality may depend on the underlying implementation.

* [run-program](run-program.lisp):
  fully portably runs a program as a synchronous external subprocess,
  feed it input and capture its output.
  Most implementations also allow interactive console subprocesses.

* [configuration](configuration.lisp):
  portably locates and parses configuration files, using best practices to
  define and validate syntax, search standard paths,
  let users specify pathnames or pathname patterns, etc.

* [backward-driver](backward-driver.lisp):
  provides backward-compatibility with earlier incarnations of this library
  (i.e. ASDF internals that have leaked, ASDF-UTILS, or older versions of UIOP).

* [driver](driver.lisp):
  reexports all the above utilities in a single package `UIOP`.


Documentation
-------------

Each file starts with a package definition form that lists the exported symbols.

All the exported functions, macros and variables ought to have proper docstrings.
If not, then it's a legitimate bug that we invite you to report.

You can extract a manual from the docstrings
by running `make` in the directory `uiop/doc`.

Other automated tools may hopefully extract all that information and
make a webpage from it, at which point it would be nice to insert a link here.
But many tools fail to extract useful data.

Tools with which you can extract all the documentation include
[Declt](https://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt)
and [HEΛP](http://helambdap.sourceforge.net/).
See the Quickref UIOP reference manual <https://quickref.common-lisp.net/uiop.html>
as extracted by Declt.

There is also a pre-extracted HEΛP documentation page
<http://bimib.disco.unimib.it/people/Marco.Antoniotti/Projects/CL/HELAMBDAP/tests/asdf-uiop/docs/html/dictionary/dictionary.html>.
Note however that the HEΛP interface is not very usable at this time:
it isn't obvious at all that you can indeed use a scrollbar
on the right of the top left side panel to navigate the many packages;
once you click on the package you're interested in, you can see its defined symbols.

Another automated documentation tool is quickdocs, but unhappily, at the time of this writing,
it only extracts information from the first package
(see [bug #24](https://github.com/fukamachi/quickdocs/issues/24)):
<http://quickdocs.org/uiop/api>


Using UIOP
----------

UIOP is part of ASDF 3, and any modern Common Lisp implementation
will have all of UIOP available when you `(require "asdf")`.
NB: `(require :asdf)` also works on all implementations but CLISP.
Every implementation has sported ASDF 3 for years, and if yours only provides
ASDF 2, we recommend you install ASDF 3 on top of it,
using the facility in [tools/install-asdf.lisp](../tools/install-asdf.lisp).

If you need some functionality only available in a recent version of UIOP,
but cannot or will not upgrade ASDF, UIOP is also distributed separately;
see e.g. in Quicklisp. You may then have to load it like any other library,
by adding `"uiop"` or some versioned constraint `(:version "uiop" "3.2.0")`
in your system's `:depends-on` declaration, or at the REPL using:

	(asdf:load-system :uiop)

When refering to symbols in UIOP, we recommend you either have your package
`:use` the package `:uiop` or `:import-from` it, or that you shall use `uiop:`
as a prefix to the symbols. Please *DO NOT* refer to specific subpackages such as
`uiop/run-program` from the outside of UIOP, because functions may occasionally
be moved from one internal package to the other, without notification.
They have in the past and will in the future.


When to use UIOP
----------------

UIOP is the ideal tool to use when:

*   You need utilities that are always available,
    portably, with no installation needed.
*   You work in a cooperative environment, where the user is a developer
    who understands what he's doing and is trusted not to be malicious.
*   You are writing a build system, build tools, developer-facing tools.
*   You are writing bootstrap scripts, in which you cannot suppose
    that any third-party library has been installed (yet),
    much less a C compiler or any external tool.
*   You are trying to make existing Common Lisp code more robust and portable,
    or replacing developer "scripts"
    (in shell, perl, python, ruby, js, and other blub languages)
    with Common Lisp code, but without concerns about
    either end-user usability or security
    (at the very least, you, not end-users, are fully controlling pathnames,
    and filtering off or portably encoding any unusual character, etc.)

UIOP is the wrong tool when:

*   You need to have total control on syscalls,
    to use special characters in pathnames, to handle symlinks yourself,
    or otherwise to have low-level system access.
*   You work in an adversarial environment, where some users are stupid,
    uneducated or outright malicious, and cannot be trusted not to try and
    abuse the system with pathnames, symlinks, race conditions, etc.
    (or be tricked into it by attackers).
*   You are writing end-user facing tools that pass along user-provided
    pathnames, with bad usability implications if a user tries to use weird
    pathnames, or even security implications if an attackers crafts bad
    pathnames or filesystem setups.

In those latter cases, we recommend you use IOlib, or osicat,
or some similar library that isn't as portable as UIOP,
but provides fine-grained control over low-level system access.
Also, please use extreme caution.


Some history
------------

UIOP, formerly known as ASDF-DRIVER (the package and system nicknames are
deprecated), evolved from ASDF 2's internal utilities and portability layer.
It has since fully superseded functionality from the following libraries:
ASDF-UTILS (UIOP carries on the ASDF 2 utilities that this exported),
CL-FAD (UIOP completely replaces it with better design and implementation),
CL-LAUNCH (UIOP took its image and command-line argument handling),
EXTERNAL-PROGRAM, TRIVIAL-SHELL and XCVB-DRIVER
(UIOP's `run-program` and now `launch-program` evolved from XCVB-DRIVER,
from which UIOP also initially got its condition muffling),
SLIME's swank-loader (UIOP has better compilation and ABI identification),
TRIVIAL-BACKTRACE (UIOP/IMAGE has all of it and more), etc.

UIOP also captures a large subset of the functionality from TRIVIAL-FEATURES,
and a small subset of the functionality from ALEXANDRIA or FARE-UTILS.

We recommend you use UIOP instead of any of the above, where applicable,
since UIOP is more portable, more robust, more ubiquitous, better designed,
better documented, etc. If you see any way in which UIOP isn't superior,
please tell us: we're interested in improving it so it become so.
