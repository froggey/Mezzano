# The SLIME Hacker's Handbook

## Lisp code file structure

The Lisp code is organised into these files:

* `swank-backend.lisp`: Definition of the interface to non-portable
features.  Stand-alone.

* `swank-<cmucl|...>.lisp`: Backend implementation for a specific
Common Lisp system.  Uses swank-backend.lisp.

* `swank.lisp`: The top-level server program, built from the other
components.  Uses swank-backend.lisp as an interface to the actual
backends.

* `slime.el`: The Superior Lisp Inferior Mode for Emacs, i.e. the
Emacs frontend that the user actually interacts with and that connects
to the SWANK server to send expressions to, and retrieve information
from the running Common Lisp system.

* `contrib/*.lisp`: Lisp related code for add-ons to SLIME that are
maintained by their respective authors. Consult contrib/README for
more information.

## Test Suite

The Makefile includes a `check` target to run the ERT-based test
suite. This can give a pretty good sanity-check for your changes

Some backends do not pass the full test suite because of missing
features. In these cases the test suite is still useful to ensure that
changes don't introduce new errors. CMUCL historically passes the full
test suite so it makes a good sanity check for fundamental changes
(e.g. to the protocol).

Running the test suite, adding new cases, and increasing the number of
cases that backends support are all very good for karma.


## Source code layout

We use a special source file layout to take advantage of some fancy
Emacs features: outline-mode and "narrowing".

### Outline structure

Our source files have a hierarchical structure using comments like
these:

```el
;;;; Heading
;;;;; Subheading
... etc
```

We do this as a nice way to structure the program. We try to keep each
(sub)section small enough to fit in your head: typically around 50-200
lines of code each. Each section usually begins with a brief
introduction, followed by its highest-level functions, followed by
their subroutines. This is a pleasing shape for a source file to have.

Of course the comments mean something to Emacs too. One handy usage is
to bring up a hyperlinked "table of contents" for the source file
using this command:

```el
(defun show-outline-structure ()
  "Show the outline-mode structure of the current buffer."
  (interactive)
  (occur (concat "^" outline-regexp)))
```

Another is to use `outline-minor-mode` to fold away certain parts of
the buffer. See the `Outline Mode` section of the Emacs manual for
details about that.

### Pagebreak characters (^L)

We partition source files into chunks using pagebreak characters. Each
chunk is a substantial piece of code that can be considered in
isolation, that could perhaps be a separate source file if we were
fanatical about small source files (rather than big ones!)

The page breaks usually go in the same place as top-level outline-mode
headings, but they don't have to. They're flexible.

In the old days, when `slime.el` was less than 100 pages long, these
page breaks were helpful when printing it out to read. Now they're
useful for something else: narrowing.

You can use `C-x n p` (`narrow-to-page`) to "zoom in" on a
pagebreak-delimited section of the file as if it were a separate
buffer in itself. You can then use `C-x n w` (`widen`) to "zoom out" and
see the whole file again. This is tremendously helpful for focusing
your attention on one part of the program as if it were its own file.

(This file contains some page break characters. If you're reading in
Emacs you can press `C-x n p` to narrow to this page, and then later
`C-x n w` to make the whole buffer visible again.)


## Coding style

We like the fact that each function in SLIME will fit on a single
screen (80x20), and would like to preserve this property! Beyond that
we're not dogmatic :-)

In early discussions we all made happy noises about the advice in
Norvig and Pitman's
[Tutorial on Good Lisp Programming Style](http://www.norvig.com/luv-slides.ps).

For Emacs Lisp, we try to follow the _Tips and Conventions_ in
Appendix D of the GNU Emacs Lisp Reference Manual (see Info file
`elisp`, node `Tips`).

We use Emacs conventions for docstrings: the first line should be a
complete sentence to make the output of `apropos` look good.  We also
use imperative verbs.

Now that XEmacs support is gone, rewrites using packages in GNU
Emacs's core get extra karma.

Customization variables complicate testing and therefore we only add
new ones after careful consideration.  Adding new customization
variables is bad for karma.

We generally neither use nor recommend eval-after-load.

The biggest problem with SLIME's code base is feature creep.  Keep in
mind that the Right Thing isn't always the Smart Thing.  If you can't
find an elegant solution to a problem then you're probably solving the
wrong problem.  It's often a good idea to simplify the problem and to
ignore rarely needed cases.

_Remember that to rewrite a program better is the sincerest form of
code appreciation. When you can see a way to rewrite a part of SLIME
better, please do so!_



## Pull requests

* Read [how to properly contribute to open source projects on Github][1].
* Use a topic branch to easily amend a pull request later, if necessary.
* Open a [pull request][2] that relates to *only* one subject with a
  clear title and description in grammatically correct, complete
  sentences.
* Write [good commit messages][3].

[1]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[2]: https://help.github.com/articles/using-pull-requests
[3]: http://chris.beams.io/posts/git-commit/
