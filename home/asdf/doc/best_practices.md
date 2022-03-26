ASDF Best Practices
===================

This document presents the current best practices and conventions
for using ASDF 3, as of 2017.
It is not a tutorial, though it starts like one,
because it assumes for each category of ASDF user
(beginner, simple user, more elaborate user)
that he already knows what seems to be common knowledge among such users,
and tries to complete this knowledge with less obvious points
that are often wrong in systems seen in the wild.


<a name="toc"></a>Table of Contents
-----------------------------------

- [Trivial Examples](#trivial_examples)
  + [Trivial Uses of ASDF](#trivial_asdf)
    * [Loading a System](#loading_system)
    * [Testing a System](#testing_system)
    * [Designating a System](#designating_system)
  + [Trivial System Definition](#trivial_system)
    * [Using the system you defined](#using_system)
  + [Trivial Testing Definition](#trivial_testing)
    * [Notes on ASDF 2 compatibility](#notes_asdf2)
  + [Trivial Packaging](#trivial_packaging)
    * [Digression about symbols and packages](#digression)
- [Simple Examples](#simple_examples)
  + [Simple Uses of a System](#simple_uses)
    * [Building a System](#building_system)
    * [Inspecting a System](#inspecting_system)
    * [Other Operations](#other_operations)
  + [System Naming](#system_naming)
    * [Primary Systems](#primary_systems)
    * [Secondary Systems](#secondary_systems)
  + [Simple System Definition](#simple_system)
  + [Simple Packaging](#simple_packaging)
    * [Initial Package for a Lisp File](#initial_package)
    * [Using Symbols from ASDF and UIOP](#using_symbols)
  + [Simple Testing](#simple_testing)
  + [Other Secondary Systems](#other_secondary)
    * [Delivering an Executable](#delivering_executable)
    * [System Connections](#system_connections)
- [More Elaborate Examples](#moreelaborate_examples)
  + [More Elaborate Uses of ASDF](#moreelaborate_asdf)
    * [force](#force)
    * [force-not](#force_not)
    * [Require](#require)
  + [More Elaborate System Definitions](#elaborate_definitions)
    * [package-inferred-system](#package_inferred)
    * [Using ASDF Extensions](#using_extensions)
    * [Code in .asd files](#code_asd)
    * [Conditional Code](#conditional_code)
  + [More Elaborate Testing](#moreelaborate_testing)
  + [Defining ASDF Extensions](#defining_extensions)
    * [Source File Types](#source_types)
    * [Conditional Outputs and Conditional Perform](#conditional_outputs)
- [Other](#other)
  + [Syntax Control](#syntax_control)
  + [Using ASDF Internals](#using_asdf_internals)


<a name="trivial_examples"></a>Trivial Examples
------------------------------------------------

Let's start with some trivial examples.
We'll see below how these examples evolve as systems grow more complex.

### <a name="trivial_asdf"></a>Trivial Uses of ASDF

#### <a name="loading_system"></a>Loading a System

The most trivial use of ASDF is by calling `(asdf:load-system :foobar)`
to load your library.
Then you can use it.
For instance, if it exports a function `some-fun` in its package `foobar`,
then you will be able to call it with `(foobar:some-fun ...)` or with:

    (in-package :foobar)
    (some-fun ...)

#### <a name="testing_system"></a>Testing a System

To run the tests for a system, you may use:

    (asdf:test-system :foobar)

The convention is that an error SHOULD be signalled if tests are unsuccessful.

#### <a name="designating_system"></a>Designating a System

Using keywords to name systems is good and well at the REPL.
However, when writing a program, a bootstrap script, or a system definition,
you SHOULD follow the good style of writing names in the canonical way;
and the canonical name of a system is a string, which by convention is in lower-case
(indeed any symbol used as system designator is downcased by `asdf:coerce-name`).

Thus, the proper way to designate a system in a program is with lower-case strings, as in:

    (asdf:load-system "foobar")
    (asdf:test-system "foobar")

(Historical note: MK-DEFSYSTEM and before it Genera's SCT
used string-upcase to normalize system names,
and use that as filenames, which made sense on Genera's filesystem
or using logical pathnames as common with MK-DEFSYSTEM.
Dan Barlow's ASDF, to "Play Nice With Unix", preferred lower-case file names,
and used string-downcase to normalize symbols into strings;
at the same time, he allowed you to specify mixed case or upper-case names by using a string.
The latter was probably a mistake, and I strongly recommend against using anything but lower-case.
If that weren't allowed, there would be a case for making lower-case symbols the default syntax,
and let the various case conversions do their job without quotes.
But that is not where we are.
In any case, system names designate `.asd` files, not Lisp bindings,
and this determines their syntax.)

### <a name="trivial_system"></a>Trivial System Definition

A trivial system would have a single Lisp file called `foobar.lisp`.
That file would depend on some existing libraries,
say `alexandria` for general purpose utilities,
and `trivia` for pattern-matching.
To make this system buildable using ASDF,
you create a system definition file called `foobar.asd`,
with the following contents:

    (defsystem "foobar"
      :depends-on ("alexandria" "trivia")
      :components ((:file "foobar")))

Note how the type `lisp` of `foobar.lisp`
is implicit in the name of the file above.
As for contents of that file, they would look like this:

    (defpackage :foobar
      (:use :common-lisp :alexandria :trivia)
      (:export
       #:some-function
       #:another-function
       #:call-with-foobar
       #:with-foobar))

    (in-package :foobar)

    (defun some-function (...)
      ...)
    ...

#### <a name="using_system"></a>Using the system you defined

Assuming your system is installed under the `~/common-lisp/` hierarchy
or some other filesystem hierarchy already configured for ASDF,
you can load it with: `(asdf:load-system "foobar")`,
or shorter `(asdf:make :foobar)`

If your Lisp was already started when you created that file,
you may have to `(asdf:clear-configuration)` to re-process the configuration.


### <a name="trivial_testing"></a>Trivial Testing Definition

Even the most trivial of systems needs some tests,
if only because it will have to be modified eventually,
and you want to make sure those modifications don't break client code.
Tests are also a good way to document expected behavior.

The simplest way to write tests is to have a file `foobar-tests.lisp`
and modify the above `foobar.asd` as follows:

    (defsystem "foobar"
      :depends-on ("alexandria" "trivia")
      :components ((:file "foobar"))
      :in-order-to ((test-op (test-op "foobar/tests"))))

    (defsystem "foobar/tests"
      :depends-on ("foobar" "fiveam")
      :components ((:file "foobar-tests"))
      :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))

The `:in-order-to` clause in the first system
allows you to use `(asdf:test-system :foobar)`
which will chain into `foobar/tests`.
The `:perform` clause in the second system does the testing itself.

In the test system, `fiveam` is the name of a popular test library,
and the content of the `perform` method is how to invoke this library
to run the test suite `:foobar`.
Obviously your mileage may vary if you use a different library.

#### <a name="notes_asdf2"></a>Note on ASDF 2 compatibility

The `:in-order-to ((test-op (test-op ...)))` idiom will not work with ASDF 2:
attempts to `test-system` will result in failure due to circular dependencies;
however, `load-system` will still work.
As for the `:perform` method, you need to specify `:after` right after `test-op`
for ASDF 2 to accept the `defsystem` form at all (but still not make the `:in-order-to` work);
so it's probably OK if your test system is in a separate `.asd` file,
but if you want backward compatibility and it's a secondary system, you'll use `:after`.
Making things work completely on ASDF 2 as well as ASDF 3 would take a lot of pain,
and would be colloquial for neither.

But *friends don't let friends use ASDF 2*.
ASDF 3 has been available since 2013, and
all implementations have now been providing it for a long time.
Many systems, and growing, assume ASDF 3, possibly some you depend on,
and it's pointless to maintain compatibility with a moribund system
(especially so if you don't also check that all your dependencies do, too).
Upgrade your implementations and/or at least your ASDF,
and tell your friends to do as much.
The [very founding principle of ASDF 2](http://fare.livejournal.com/149264.html) (and 3)
was to provide users with
the ability to install a newer ASDF on top of an old one to fix its bugs
(an ability that did not exist with ASDF 1);
thus, users do not have to maintain compatibility with antique bugs.

These days, I even recommended that you should freely rely
on features of ASDF 3.1 (2014) not present in ASDF 3.0 (2013), such as:
`~/common-lisp/` being included in the source-registry by default;
`run-program` being full-fledged with input and error-output support;
or the `package-inferred-system` feature.
Conversely, if you want indefinite backward compatibility, why stop at ASDF 2?
Why not also support ASDF 1, and MK-DEFSYSTEM, and
the original Lisp Machine DEFSYSTEM?
A good rule of thumb for me is that it is not worth writing workarounds
for bugs that were fixed or features that were changed two years ago or more.
Two years seems to be the time it takes for a release of ASDF
to become ubiquitously available by default on all implementations;
and, remember, if a user must use an older implementation,
he can always trivially install a newer ASDF on top of it.
Thus, by October 2019, people should not be shy about
dropping support for ASDF versions older than 3.3.
And even before then, if you need a recent ASDF, just document it,
and tell your users to upgrade their implementation and/or
install a recent ASDF on top of their implementation's.


### <a name="trivial_packaging"></a>Trivial Packaging

In the previous testing code, `symbol-call` is a function defined in package `uiop`.
It helps deal with the fact that the package `:fiveam` isn't defined yet
at the time the `defsystem` form is read.
Thus you can't simply write `(fiveam:run! :foobar)`, as
this might cause a failure at read-time when file `foobar.asd` is first read
and the system `fiveam` isn't loaded yet.
This of course also holds for packages defined by system `foobar` itself.

To get a symbol in it that is not a function being called,
you can also use `(find-symbol* :some-symbol :foobar)`,
and you can use `symbol-value` to get the value bound to such a symbol as denoting a variable.
For complex expressions, you can use `eval` and `read-from-string`,
as in `(eval (read-from-string "(foobar:some-fun foobar:*some-var*)"))`,
or equivalently, also from `uiop`: `(eval-input "(foobar:some-fun foobar:*some-var*)")`.

System definition files are loaded with the current `*package*` bound to the package `:asdf-user`,
that uses the packages `:cl`, `:asdf` and `:uiop`.
Therefore, you don't need to and you SHOULD NOT specify `cl:`, `asdf:` or `uiop:` prefixes
when referring to symbols in these respective packages.
It is considered bad form to do so, unless you changed the package,
and even then, your package should probably use these packages, too.
In particular you SHOULD NOT to write `(asdf:defsystem "foobar" ...)`
instead of `(defsystem "foobar" ...)`.
Also, you SHOULD NOT use `(in-package ...)` or `(defpackage ...)`
if all you do can legitimately be done in package `ASDF-USER`.

System definition files are loaded by a special function `asdf::load-asd`;
this function locally binds the current `*package*` to the `:asdf-user` package,
but it also does many other things.
You MUST NOT ever load a `.asd` file using `cl:load`,
as this will not work in general, and may fail in subtle or spectacular ways.
You MUST always use `asdf::load-asd` for that purpose
(the function is exported in ASDF 3.2 and above, but not in earlier versions).
You SHOULD NOT encourage the illusion that a `.asd` file can be loaded with `cl:load`
by using `(in-package :asdf-user)` or anything,
or by using the package prefix `asdf:` for `defsystem` and other such symbols.
You SHOULD be using the `slime-asdf` extension to SLIME
if you are going to edit `.asd` file and then load them from SLIME,
as it will automatically use `load-asd` to load the file contents.

#### <a name="digression"></a>Digression about symbols and packages

A crucial notion in Common Lisp is that of *symbols*, to which are associated
functions, variables, macros, properties, and other meanings.
These symbols are organized in a two-level namespace that is global to a given Lisp image.
Symbols are the second level, and the first level are *packages*.
Symbols and packages are traditionally uppercase internally,
but you traditionally write them lower-case in source code;
the Common Lisp reader is *case-converting*.
These internal representation details matter, because they are exposed
when you either write macros at compile-time or introspect the system at runtime.

The conventional way to refer to a package is with its name designated by a lower-case keyword,
starting with the colon character `:`.
For instance, the standard package `COMMON-LISP` is commonly designated as `:common-lisp`
(or `(find-package :common-lisp)` if you need an actual package object).
Keywords are themselves symbols in the magic package `KEYWORD`,
where each interned symbol is also a constant bound to its own name as a value.
Using a keyword as designator rather than a string ensures that we can maintain
the same convention of using lower-case in source code while the runtime will use uppercase.


<a name="simple_examples"></a>Simple Examples
----------------------------------------------

As systems grow, the above pattern quickly becomes insufficient,
but systems can still remain simple.

### <a name="simple_uses"></a>Simple Uses of a System

#### <a name="building_system"></a>Building a System

Some systems offer operations
that are neither loading in the current image, nor testing.
Whichever operation a system is meant to be used with, you may use it with:

    (asdf:make :foobar)

This will invoke `build-op`, which in turn will depend on
the `build-operation` for the system, if defined, or `load-op` if not.
Therefore, for usual Lisp systems that want you to load them,
the above will be equivalent to `(asdf:load-system :foobar)`,
but for other Lisp systems, e.g. one that creates a shell command-line executable,
`(asdf:make ...)` will do the Right Thing™, whatever that Right Thing™ is.

#### <a name="inspecting_system"></a>Inspecting a System

To look at what ASDF knows of your system, try:

    (describe (asdf:find-system "foobar"))

If you're looking for the `.asd` file, try `(asdf:system-source-file "foobar")`.
For the directory in which the `.asd` file resides, try `(asdf:system-source-directory "foobar")`.
For a specific file under that directory try
`(asdf:system-relative-pathname "foobar" "path/to/the/file.extension")`.

#### <a name="other_operations"></a>Other Operations

ASDF has the concept of an *operation* that can act upon a system (or a smaller component thereof).
Typical operations that matter to end-users include:

  * `load-op` for loading a system in the current Lisp image, as used by `asdf:load-system`,
  * `test-op` for running tests associated to the system, as used by `asdf:test-system`,
  * `build-op` for doing whatever build operation is associated to the system,
    (which by default is `load-op` if the system didn't override it),
    as used by `asdf:make`.

Further operations of interest to users include:

  * `compile-op`
    (ensure the system is compiled, without necessarily loading all of it, or any bit of it),
  * `load-source-op`
    (load the system from source without compiling),
  * `compile-bundle-op`
    (create a single fasl for the entire system, for delivery),
  * `monolithic-compile-bundle-op`
    (create a single fasl for the entire system *and all its transitive dependencies*, for delivery),
  * `image-op`
    (create a development image with this system already loaded, for fast startup),
  * `program-op`
    (create a standalone application, which we will see below), etc.

Whichever operation you want, `(asdf:operate `*operation* *system*`)`
will ensure this operation is performed on that system
(after all the necessary dependencies of such an action).
A short-hand `asdf:oos` is available for `asdf:operate`
(the name `oos` is an old acronym for `operate-on-system`,
and both functions existed as synonyms in the old MK-DEFSYSTEM;
the latter function doesn't exist in ASDF, which instead has just `operate`).

The operation is typically specified as a symbol that names the operation class.
Since ASDF 3, you can also use a keyword to specify an action in the ASDF package.
Thus, `(asdf:oos :load-op :foobar)` is equivalent to `(asdf:load-system :foobar)`.

### <a name="system_naming"></a>System Naming

#### <a name="primary_systems"></a>Primary Systems

ASDF has a notion of *primary system*,
that it can find in configured parts of the filesystem (the *source-registry*),
in a file that has the same name and the type `asd`.
Thus, primary system `foobar` is defined in a file `foobar.asd`.

While arbitrary strings are accepted in system names, it is strongly discouraged
to use anything but lower-case ASCII letters and digits for primary system names,
plus the separators `-` (hyphen) and `.` (dot).
The `.` itself is only recommended in primary system names that are part of an informal hierarchy;
for instance the popular library `iolib` contains many related systems:
`iolib.asdf`, `iolib.base`, `iolib.common-lisp`, `iolib.conf`, `iolib.examples`,
`iolib.grovel`, `iolib.tests`.
The main one is ostensibly `iolib`, but it contains many systems,
and for some reasons (notably proper phase separation)
they cannot all be secondary systems in the same file.

#### <a name="secondary_systems"></a>Secondary Systems

A *secondary system* is a system defined in the same file as a primary system.
By convention, its name starts the same as the file's primary system,
followed by a slash `/` and by a *suffix* made of some arbitrary characters,
preferably ASCII letters and digits
plus the separators `-` (hyphen), `.` (dot) and `/` (slash).
We already saw an example of it with system `foobar/tests`
in the trivial testing definition above.

The convention above allows ASDF 3 and later to find a secondary system by name,
by first looking for the associated primary system.
ASDF 3.2 or later will issue a `warning` when you violate this naming convention.

(Historically, ASDF 1 (and 2) allowed arbitrary names for secondary systems,
and in practice many people used `foo-test` or such for secondary system names in `foo.asd`;
however, ASDF 1 (and 2) couldn't find those systems by name, and horrific bugs could happen
if a system was simultaneously defined in multiple files.)

### <a name="simple_system"></a>Simple System Definition

A simple system may be made of many files.

Typically, in `package.lisp` (or `packages.lisp`)
you'll define the package (or packages)
used by all the files in your system
in one or more forms such as `(defpackage :foobar ...)`.

Then, in a package `utils.lisp` you'll define utility macros and functions
that you're using throughout your system;
maybe some of them deserve to be moved to `alexandria` and other utility systems;
and maybe they already exist out there and you just haven't looked hard enough.

Then a file `foobar.lisp` defines the meat of your system.

Your system definition will look like:

    (defsystem "foobar"
      :depends-on ("alexandria" "trivia")
      :serial t
      :components
      ((:file "package")
       (:file "utils")
       (:file "foobar")))

The `:serial t` indicates that each of these files depends on (all) the previous.

As your system grows some more, soon enough instead of a single file `foobar.lisp`,
you will have several files, one for each aspect of your system, such as
`foo.lisp`, `bar.lisp` as well as `foobar.lisp`.
Moreover, `:serial t` soon becomes inappropriate:
it will make your code slower to compile, but also to read.
Indeed, those who read the code won't be readily able to tell
which parts of the code they need to keep active in their brains
to understand the code at hand.
Instead, you may prefer to explicitly represent the dependencies
between the components of your system using `:depends-on` clauses
as follows:

    (defsystem "foobar"
      :depends-on ("alexandria" "trivia" "trivia.ppcre")
      :components
      ((:file "package")
       (:file "utils" :depends-on ("package"))
       (:file "foo" :depends-on ("utils"))
       (:file "bar" :depends-on ("utils"))
       (:file "foobar" :depends-on ("foo" "bar"))))

Out of good style, you SHOULD still list the components in an order that makes sense,
such that the readers can read the files and mentally rebuild the system.
However, note that this order, if coherent, will be respected
only in ASDF 3.3 or later, due to a bug in earlier versions of ASDF.
But that precise order shouldn't matter, or it should be reflected
in the `:depends-on` declarations (or in a `:serial t` declaration).

### <a name="simple_packaging"></a>Simple Packaging

#### <a name="initial_package"></a>Initial Package for a Lisp File

You MAY assume that the current package uses `CL` at the beginning of a file,
but you MUST NOT assume that it is any particular package at this point:
right now, it *is* guaranteed to be the `CL-USER` package of the underlying implementation,
but it is conceivable that in some indeterminate future,
some extension to CL may provide a well-defined portable alternative that ASDF would use.

Therefore, the sane way to write a Lisp file is that it SHOULD start
with an `in-package` form, optionally preceded by a `defpackage` form
(or a `uiop:define-package` form).
You SHOULD NOT write `cl:in-package` or precede your `defpackage` with an `(in-package :cl-user)`
(which is stupid, because to be pedantic you'd have to `(cl:in-package :cl-user)`,
at which point you may as well `(cl:defpackage ...)` and `(cl:in-package ...)`).
If it's a regular `cl-source-file`, it can assume the language is CL indeed,
and that the readtable something reasonable, etc.

#### <a name="using_symbols"></a>Using Symbols from ASDF and UIOP

You MAY use any of the symbols documented and exported by ASDF or UIOP.
Actually, it is warmly recommended to use them everywhere that it matters,
instead of less documented or less portable alternatives.

You MUST NOT use `asdf:run-shell-command`, `asdf:system-definition-pathname`,
or other deprecated functions that were once recommended in the time of ASDF 1.
They will be removed in the near future (one to two year horizon).
ASDF 3.2 or 3.3 will issue a `style-warning` when you do, and
some future version of ASDF will issue a full `warning`,
which will then break the SBCL build.
See `backward-interface.lisp` for a list of deprecated function — or just heed the damn warnings.

### <a name="simple_testing"></a>Simple Testing

Test systems can also be divided in multiple files.
If possible (which is not always the case), the file names for test files
should match file names for regular code files,
so that the file that tests `bar.lisp` will be called `bar-test.lisp` or `bar-tests.lisp`
(pick singular or plural, but have a story for it and be consistent about it).
To keep things tidy as the test system grows,
you may even put all test files in a subdirectory `t/`, `test` or `tests/`.
Your test system definition may then look like:

    (defsystem "foobar/tests"
      :depends-on ("fiveam" "foobar")
      :pathname "t/" ;; specify the subdirectory
      :components
      ((:file "test-suite")
       (:file "utils-test" :depends-on ("test-suite"))
       (:file "foo-test" :depends-on ("test-suite"))
       (:file "bar-test" :depends-on ("test-suite"))
       (:file "foobar-test" :depends-on ("test-suite")))
      :perform (test-op (o c) (symbol-call :foobar/tests :run-test-suite)))

As the system and its test system both grow, the test system may be moved to its own file
`foobar-tests.asd` or `foobar.tests.asd` where it is its own primary system:

    (defsystem "foobar-tests" ...)

### <a name="other_secondary"></a>Other secondary systems

Other secondary systems may be created beyond test systems:
for instance systems that provide independent aspects of the system,
or optional add-ons to it.
One case is a command that makes the Lisp functionality accessible from a Unix shell.

#### <a name="delivering_executable"></a>Delivering an Executable

To build an executable, define a system as follows
(in this case, it's a secondary system, but it could also be a primary system).
You will be able to create an executable file `foobar-command`
by evaluating `(asdf:make :foobar/executable)`:

    (defsystem "foobar/executable"
      :build-operation program-op
      :build-pathname "foobar-command" ;; shell name
      :entry-point "foobar::start-foobar" ;; thunk
      :depends-on ("foobar")
      :components ((:file "main")))

The `build-pathname` gives the name of the executable;
a `.exe` type will be automatically added on Windows.
As a horrible kluge that may go away in the future,
the output will be created in the system's directory (where the `foobar.asd` file resides)
when the `build-operation` matches the requested operation.
A future version of ASDF will probably instead have some `:output` argument to `operate`,
or some such thing, and drop this ugly special case.
You have been warned. Contact me if you want that sooner, or not at all.

There, file `main.lisp` defines a function `start-foobar` in package `foobar`,
that takes no argument, and initializes and starts the executable ---
after `uiop` calls its own `*image-restore-hook*` and evaluates any provided `*image-prelude*`.
Importantly, libraries may register functions to call in the `*image-restore-hook*`
using `register-image-restore-hook` (see also `register-image-dump-hook`);
UIOP and ASDF themselves make use of this facility.
Typically, `start-foobar` will be defined as something like:

    (defun start-foobar () (main (uiop:command-line-arguments)))

Where function `main` parses the arguments
(a list of strings, excluding the magic C `argv[0]`, which can be computed as `(uiop:argv0)`)
and does whatever its magic.
You may want to use the full-featured `net.didierverna.clon` or my small `command-line-arguments`
or one of a slew of other libraries to parse the command-line arguments.
You may want to use `cl-scripting` to nicely wrap Lisp code into execution contexts
that handle errors in a nice(r) user-visible way for the shell user.
You may want to use `inferior-shell` if your program in turn invokes other shell programs.

Instead may also use `cl-launch` to build executables, or `buildapp`.
`cl-launch` is largely compatible with ASDF
(indeed, a lot of code formerly written as part of `cl-launch`
was later made part of ASDF 3's UIOP).
`buildapp`, that came before ASDF 3 but after `cl-launch`,
has a slightly incompatible convention where a main function is called with
a list of arguments that includes the `argv[0]`.
(`cl-launch` and after it UIOP had many good enough reasons
to start the argument list at the "user arguments", if only because
when invoking a Lisp implementation via `cl-launch`,
the process' `argv[0]` may not be available or meaningful,
whereas the user arguments may be only a subset of the actual process' arguments.)
Both `cl-launch` and `buildapp` have similar functionality
to handle multicall binaries à la [Busybox](http://busybox.net/),
with the same incompatibility as above.

#### <a name="system_connections"></a>System Connections

Sometimes, a system can provide an extension to another system.
For instance, if you use both `metacopy` and `contextl`,
you may be interested in some system `metacopy/with-contextl`
that creates synergies between these two systems.

There exists a system `asdf-system-connections` that will allow you
to define such system connections that are *automatically* loaded
when both the connected systems are loaded.
While I debugged that mechanism and made sure it works with ASDF 3,
I recommend against using it, because it introduces side-effects within the build.
Instead I recommend explicitly loading the system connections
as part of the larger system that will use them.


<a name="moreelaborate_examples"></a>More Elaborate Examples
--------------------------------------------------------

### <a name="moreelaborate_asdf"></a>More Elaborate Uses of ASDF

#### <a name="force"></a>force

Sometimes, you want to force ASDF to re-build some system.
At those times, you can pass the `:force` argument to `asdf:operate`
(or its wrappers `asdf:load-system`, `asdf:test-system`, `asdf:oos`, etc.).
Passing an argument `t` will force rebuild of just the system, and none of its dependencies
(and that also means none of the dependencies that happen to be secondary systems
with the same primary system name).
Passing a list of system designators (preferably lower-case strings)
will force the specific systems to be rebuilt (if they appear in the build plan at all, that is).
Finally, passing `:all` as argument will force a rebuild of everything, including all dependencies.

Thus, if you ran tests that use the `prove` test framework,
in which loading the files is itself the test,
and you want to force a re-run, even though ASDF might be satisfied
with already having loaded the files, then you can use:
`(asdf:load-system :clack-test :force t)`

Note that it is strongly recommended to not have any non-determinism or side-effects
that are not declared to ASDF, that would cause the forcing to be meaningful.
Forcing is thus a debugging feature for ASDF systems that fail this good practice.
In the case of `prove`, we will have to work with its author so that the correct way
to use it doesn't violate ASDF invariants, but instead properly declare that
ASDF should not consider tests already run.

#### <a name="force_not"></a>force-not

The converse of `:force` is `:force-not`, and you can specify a list of systems to not rebuild.
In this context `t` means "everything but this system" rather than " this system".
`force-not` takes precedence over `force`, and by default includes a list of "immutable" systems
that may be used when delivering extensible applications to customers
(See function `asdf::register-immutable-system`).

Note that these flags are only for use by the user at the toplevel.
You MUST NOT call `asdf:operate` with `:force` or `:force-not` from within a build.
Actually, you should probably not explicitly use `asdf:operate` at all,
except maybe inside a `.asd` file in cases where `defsystem-depends-on` isn't sufficient.

#### <a name="require"></a>Require

You SHOULD NOT use `cl:require` as a substitute for `asdf:load-system`.
You SHOULD NOT use `asdf:require-system` except at the toplevel.

Back in the days of ASDF 1, a convenient hook was added to ASDF
so that when you call `cl:require`, it would try to load the named system,
and if not fall back on the builtin require mechanism.
This was a cool hack, and when you merely wanted a dependency, it was easier to type
`(require :foo)` than `(asdf:operate 'asdf:load-op :foo)` as you then had to.
Moreover, on SBCL where ASDF was developed,
ASDF itself was used to compile and load SBCL modules at runtime,
so this hook came naturally.

As ASDF maintainer, I now consider this in bad taste:

  * First, this hook is not 100% portable, so it is bad taste to recommend relying on it.
  * Second, `cl:require` has a mechanism for loading things only once by checking `cl:*modules*`
    which may subtly interfere with ASDF's mechanism for keeping things up to date.
  * Third, it interferes with ASDF's capacity to detect legitimate vs illegitimate
    recursive uses of `operate` at places that defeat tracking of dependencies.
  * Fourth, it adds a lot of complexity for dubious gain: at a time you had to type
    `(asdf:operate 'asdf:load-op :foo)`, `(require :foo)` may have been a nice short-hand,
    but it isn't such a great gain over `(asdf:make :foo)`.
  * Fifth, SBCL now uses ASDF 3's `compile-bundle-op` to create a fasl
    during the build of SBCL itself, and that fasl can latter be loaded at runtime without ASDF.
    Therefore the hook has no natural use anymore.

Similarly, in ASDF 2.21 I added a function `require-system`
that used to called `load-system` with `:force-not (already-loaded-systems)`,
which was a nice hack at the time, that I latter used as part of the `cl:require` hook.
That was all a big mistake, as `:force-not` interferes with the ability to keep a coherent plan
across recursive uses of `asdf:operate` as required by builds that involve `:defsystem-depends-on`
and other ASDF extensions.
These days, this function only checks whether the requested component is already loaded,
and if not calls `asdf:load-system` on it.
This function MUST only be used at the toplevel, never in a script or build.
It may be deprecated in a future version of ASDF.

### <a name="elaborate_definitions"></a>More Elaborate System Definitions

#### <a name="package_inferred"></a>package-inferred-system

When you start writing large enough systems,
putting everything in one big package leads to a big mess:
it's hard to find what function is defined where, or should be defined where;
you invent your own symbol prefixing system to avoid name clashes;
totally unrelated things end up in the same mother-of-all package;
you divide your mother-of-all package into a few subpackages,
but as the software keeps growing each of these packages in turn becomes too big.

Meanwhile, as you grow large enough libraries, you find that you loading
a big library just to use a small part of it becomes a big hassle,
leading to code bloat, too much recompilation, too much re-testing,
and not enough understanding of what's going on.

A solution to both these problems is the "one file, one package, one system" style,
once spearheaded by faslpath and quick-build, and now available as part of ASDF
(since ASDF 3.1, 2014) using the `package-inferred-system` class.

Following this style, your top `.asd` file defines uses this class for its primary system
(you can still define secondary systems with different classes).
Then, any secondary system, if not explicitly defined,
will be searched for in a file as named by the secondary system suffix,
under the directory that contains the system definition file.

Thus, if `~/common-lisp/foobar-1.0/foobar.asd` defines
a primary system of class `package-inferred-system`,
but no secondary class `foobar/x/y`, then ASDF will look for a file
`~/common-lisp/foobar-1.0/x/y.lisp` to contain this system.
That file will be a regular Lisp file,
that will begin with a `defpackage` or `uiop:define-package` form.
(The latter form is more friendly to live upgrades,
but also allows to use a mix of packages with a priority on symbol conflicts,
or to reexport imported symbols.)
Dependencies for this system will be deduced from the `:use`,
`:import-form` and `:shadowing-import-from` clauses of that `defpackage`,
where each package name is downcased and interpreted as a system name,
unless registered otherwise via `asdf:register-system-packages`.

This allows for large modular libraries, wherein you may use one file,
and only that file and its transitive dependencies will be loaded,
rather than the entire humongous library.

This also helps you enforce a discipline wherein it is always clear in which file
each symbol is defined, which files have symbols used by any other file, etc.

#### <a name="using_extensions"></a>Using ASDF Extensions

If you need an ASDF extension, the recommended way is to use `:defsystem-depends-on`.
The extension will define new classes of operations or components, new functions, etc.
If it defines them in the ASDF package, you can refer to them using a keyword of the same name,
e.g. `:cffi-wrapper-file` for one of the component classes defined by system `cffi-grovel`,
`:static-program-op` for one of the operation classes defined by `cffi-toolchain`, or
`:f2cl-system` for the system class defined by system `f2cl`.
Otherwise, you can refer to them using a string that when read yields the symbol, e.g.
the system `asdf-finalizers` defines a function that you can use as an around-compile hook
for your system using the clause `:around-compile "asdf-finalizers:check-finalizers-around-compile"`.
You cannot usually specify the qualified symbol directy, because it lives in a package
that hasn't been defined yet at the time the `defsystem` form is read.

Sometimes, you may want to use several build extensions at once,
but a given system, smaller component or operation, can have only one class.
You may split that system in two (say, a primary system and a secondary system)
so each has its own extension (say, make the primary system a `package-inferred-system`,
but have a secondary system be a `program-system`).
For more advanced cases, you may have to define a class that inherits from multiple other classes
in your own extension.
(In a prototype OO system, you could just mix and match extensions without defining a new class,
but CLOS is not a prototype OO system.)

#### <a name="code_asd"></a>Code in .asd files

ASDF currently allows arbitrary Lisp code in a `.asd` file.
I would like to deprecate that in the future to make ASDF more declarative.
In the meantime, here are some guidelines for how to write such code properly.

First, you SHOULD minimize the amount of such code and
you SHOULD strongly consider defining an extension
in a separate `.asd` file that you `:defsystem-depends-on`.

Then, `.asd` files are read with the current package being `asdf-user`.
You MAY use any symbols defined in packages `cl`, `asdf` and `uiop`,
and you SHOULD stay in package `asdf-user` if it suffices;
you SHOULD NOT needlessly create a package for your system definition file
if you're not going to define any such function nor variable nor macro.
But you MUST NOT pollute that package with bindings that could clash with uses by other systems.

It is poor taste to define functions, variables or macros in package `asdf-user`,
unless strictly necessary or widely useful, and even then with a long name
that distinctively includes the name of your system.
However it is perfectly acceptable to define methods on existing functions,
preferably using the `:perform` syntax used above, as part of a `defsystem` form.

If you must define new functions, variables or macros,
you MUST define a new package in which to define them.
That system would typically be named `foobar-system`
and would export the relevant symbols, as in:

    (defpackage :foobar-system
      (:use :cl :asdf :uiop)
      (:export #:my-foobar-class ...))
    (in-package :foobar-system)

That said, if you need more than trivial definitions,
and if these definitions ever need to be used by others,
it is appropriate to move them to their own system.

Also, it is poor taste to define symbols used in the system itself (e.g. `*version*`)
since the system then depends on ASDF and cannot be directly built using Bazel,
or some other future build system.
For versions, consider having ASDF extract the version from a file, as in
`:version (:read-file-form "variables.lisp" :at (2 2 2)`.
See the ASDF manual for details, and `asdf.asd` itself for an example.

#### <a name="conditional_code"></a>Conditional Code

The recommended way to conditionally use code is to rely on the CL features;
yet, you SHOULD NOT use read-time conditionals if you can avoid it,
and instead use the ASDF 3 `:feature` and `:if-feature` features:

  * To conditionally depend on another system or module,
    specify as a `:depends-on` argument something like `(:feature :sbcl (:require :sb-introspect))`
    or `(:feature (:or :ccl :sbcl) "bordeaux-threads")`.
  * To conditionally enable a component,
    specify a `:if-feature` argument to it like `(:file "sbcl-support" :if-feature :sbcl)`

Note that the feature expressions MUST use keywords, not symbols in the current package
(unless you're pervert enough to make `:keyword` your current package).

Also note that ASDF at this time wants all the component classes to be defined,
so if your component classes are conditionally defined, you may have to resort back
to the otherwise deprecated use of read-time conditionals, as in:

    (defsystem "foo"
      :defsystem-depends-on ((:feature :sbcl "some-sb-magic"))
      :components (#+sbcl (:sb-magic-file "some-magic")))

If the condition you want to express doesn't have a corresponding `*features*` keyword,
you MAY use a `(when ... (pushnew :my-keyword *features*))` form to add a feature keyword for it.
However, you SHOULD NOT needlessly push new features;
in particular, the mere fact of having loaded your system does not warrant a new feature.
A feature is only warranted if your system is some deep infrastructure
with mostly compatible rivals that it needs to be distinguished from.

### <a name="moreelaborate_testing"></a>More Elaborate Testing

You MUST NOT call `asdf:operate` or any of its derivatives,
such as `asdf:load-system` or `asdf:test-system` from within a `perform` method.
Instead, you SHOULD declare proper dependencies between actions using methods on
`component-depends-on`, or more simply using an `:in-order-to` clause in your `defsystem` form.

You MUST NOT call `asdf:clear-system` or in any way interfere with the build
while in a `perform` method.

You SHOULD NOT define methods on `asdf:operate` --- most of the time it's totally the wrong thing
because users would *not* be "operating" on *your* system, but on *their* system that depends on it.
Instead you SHOULD define methods on `asdf:perform`, `asdf:component-depends-on`, etc.

### <a name="defining_extensions"></a>Defining ASDF Extensions

#### <a name="source_types"></a>Source File Types

You MUST NOT define methods on `source-file-type`.
This bad ASDF 1 interface must die and will be removed in a future version of ASDF.
Instead, override the slot `type` for your file class and provide a proper `:initform`, as in:

    (defclass cl-source-file.l (cl-source-file) ((type :initform "l")))

You can then provide the name of that class as class name for individual components,
or as `:default-component-class` to relevant systems or modules.

#### <a name="conditional_outputs"></a>Conditional Outputs and Conditional Perform

An operation MUST actually create all the outputs declared by the `output-files` method.
If some of these outputs are conditional, the `output-files` method MUST check the condition
and remove uncreated outputs from the list.
If some output is sometimes non meaningful but that is only known after calling the `perform` method,
it is sometimes appropriate to create an empty file instead of not creating the file.

If you conditionally avoid performing an action altogether using an `:around` method,
you MUST still call `mark-operation-done` on the action in the branch of the method
where the action is not performed.
In that case, both the `output-files` and `input-files` methods
should probably return `nil` when that condition is met.
Alternatively, if avoiding some actions is uniform for all
operations on a given component, you MAY want to override or wrap around
the `component-if-feature` method for that component instead of defining
all those `perform`, `output-files` and `input-files` methods.


<a name="other"></a>Other
--------------------------

### <a name="syntax_control"></a>Syntax Control

Do not side-effect the current `*readtable*`,
which is never guaranteed to be writable, and
may not be what is current when you need the modification.
If you need modifications to the readtable, use `named-readtables`,
`reader-interception`, or some other adequate mechanism.
To bind the readtable, use `named-readtables:in-readtable` or `cl-syntax:in-syntax`
in each file that needs a non-standard readtable, and/or use an `:around-compile` hook
to automatically bind it around every file in a module or system.

See the syntax-control document (in the syntax-control branch if it wasn't merged yet).


### <a name="using_asdf_internals"></a>Using ASDF Internals

At times, you will have to use some ASDF internals to get your software to work.
When this happens, follow these rules to minimize the breakage associated to bitrot:

  * You MUST use the `asdf::` package prefix rather than `asdf/foo:`
    to name the symbols internal to ASDF.
    Indeed, from one version of ASDF to the next,
    which internal package a symbol will reside in may change;
    for instance `asdf/find-system:primary-system-name` recently became
    `asdf/system:primary-system-name`
    (it is still present, not exported, in the former system);
    but it is always present (though not currently exported) in package `asdf`.

  * When you use internals, you MUST notify the ASDF maintainers, and request
    that they either export the symbol and support the API, or
    offer an alternative API more to their liking for the same functionality.
    Otherwise, they will be justified in modifying internals
    that no one is supposed to use, and they will blame you when
    they later change or remove these internals and your software breaks.

Note that the latter point is true of all software, not just ASDF.
