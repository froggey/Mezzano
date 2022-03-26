[![Build Status](https://travis-ci.org/trivial-features/trivial-features.svg?branch=master)](https://travis-ci.org/trivial-features/trivial-features)

trivial-features ensures consistent `*FEATURES*` across multiple
Common Lisp implementations.

For example, on MacOS X platforms, while most Lisps push `:DARWIN` to
`*FEATURES*`, CLISP and Allegro push `:MACOS` and `:MACOSX` instead,
respectively.  Some Lisps might not push any feature suggesting MacOS
X at all.  trivial-features will make sure all Lisps will have
`:DARWIN` in the `*FEATURES*` list when running on MacOS X.  This
way, you can write

    #+darwin foo #-darwin bar

instead of

    #+(or darwin macos macosx) foo
    #-(or darwin macos macosx) bar

The included [SPEC.md][1] document describes the set of symbols that
should or should not be present in `CL:*FEATURES*` on certain
circumstances.  This specification is implemented by the
TRIVIAL-FEATURES system which supports a handful of Lisps.

The test suite is, in effect, an implementation using [CFFI][2] that
is checked against the various implementations in `src/tf-*.lisp`.

trivial-features is MIT-licenced.


[1]: https://github.com/trivial-features/trivial-features/blob/master/SPEC.md
[2]: http://common-lisp.net/project/cffi
