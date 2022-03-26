This code was written by Christophe Rhodes, and the following
description was taken from
[cliki.net](http://www.cliki.net/spatial-trees).

spatial-trees
=============

spatial-trees is a set of dynamic index data structures for
spatially-extended data. The flavors provided are, as of the 0.1
release (on 2004-12-03):

* R-trees, as in _R-TREES: A DYNAMIC INDEX STRUCTURE FOR SPATIAL
  SEARCHING_, Antonin Guttman, Proc. ACM SIGMOD Int. Conf. on
  Management of Data, 1984.
  
* Greene-trees, as in _An Implementation and Performance Analysis of
  Spatial Data Access Methods_, Diane Greene, Proc. 5th IEEE
  Int. Conf. on Data Engineering, 1989.
  
* R*-trees, as in The _R*-tree: An Efficient and Robust Access Method
  for Points and Rectangles_, Beckmann, Kriegel, Schneider and Seeger,
  Proc. ACM Int. Conf. on Management of Data, 1990
  
* X-trees, as in _The X-tree: An Index Structure for High-Dimensional
  Data_, Berchtold, Keim and Kriegel, Proc. 22th Int. Conf. on Very
  Large Databases, 1996

Future work planned includes performance enhancements, incorporation
of more index structures, and some work on supporting more optimal
indexing when the entire set of data is known at index creation time;
for more details, see the TODO file in the binary distribution.

The code is licensed BSD-style, and is intended to be similar in
spirit to Nathan Froyd's TREES Library.

Currently quicklisp-loadable.

Here are some instructions:

* Read the [API](./api.org) first
* For Tutorial, see [tutorial.lisp](./tutorial.lisp)
* For Testing, run `(asdf:test-system :spatial-trees)`.
* In order to test a visual inspector, run  `(asdf:load-system
  :spatial-trees-viz)` and the mcclim-based visualizer will show up.

