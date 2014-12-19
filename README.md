ocaml-containers
================

![logo](media/logo.png)

What is _containers_?

- A usable, reasonably well-designed library that extends OCaml's standard
  library (in `core/`, packaged under `containers` in ocamlfind. Modules
  are totally independent and are prefixed with `CC` (for "containers-core"
  or "companion-cube" because I'm megalomaniac). This part should be
  usable and should work. For instance, `CCList` contains functions and
  lists including safe versions of `map` and `append`.
- Several small additional libraries that complement it:
  * `containers.data` with additional data structures that don't have an
    equivalent in the standard library;
  * `containers.io` with utils to handle files and I/O streams;
  * `containers.iter` with list-like and tree-like iterators;
  * `containers.string` (in directory `string`) with
    a few packed modules that deal with strings (Levenshtein distance,
    KMP search algorithm, and a few naive utils). Again, modules are independent
    and sometimes parametric on the string and char types (so they should
    be able to deal with your favorite unicode library).
- A drop-in replacement to the standard library, `containers.pervasives`,
  that defined a `CCPervasives` module intented to be opened to extend some
  modules of the stdlib.
- A sub-library with complicated abstractions, `containers.advanced` (with
  a LINQ-like query module, batch operations using GADTs, and others).
- A library using [Lwt](https://github.com/ocsigen/lwt/), `containers.lwt`.
  Currently only contains experimental, unstable stuff.
- Random stuff, with *NO* *GUARANTEE* of even being barely usable or tested,
  in other dirs (mostly `misc` but also `lwt` and `threads`). It's where I
  tend to write code when I want to test some idea, so half the modules (at
  least) are unfinished or don't really work.

Some of the modules have been moved to their own repository (e.g. `sequence`,
`gen`, `qcheck`) and are on opam for great fun and profit.

[![Build Status](http://ci.cedeela.fr/buildStatus/icon?job=containers)](http://ci.cedeela.fr/job/containers/)

## Change Log

See [this file](https://github.com/c-cube/ocaml-containers/blob/master/CHANGELOG.md).

## Finding help

- the [github wiki](https://github.com/c-cube/ocaml-containers/wiki)
- the IRC channel (`##ocaml-containers` on Freenode)

## Use

You can either build and install the library (see `Build`), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). Since modules have a friendly license and are mostly
independent, both options are easy.

If you have comments, requests, or bugfixes, please share them! :-)

## License

This code is free, under the BSD license.

The logo (`media/logo.png`) is
CC-SA3 [wikimedia](http://en.wikipedia.org/wiki/File:Hypercube.svg).

## Contents

The design is mostly centered around polymorphism rather than functors. Such
structures comprise (some modules in `misc/`, some other in `core/`):

### Core Modules (extension of the standard library)

the core library, `containers`, now depends on
[cppo](https://github.com/mjambon/cppo) and `base-bytes` (provided
by ocamlfind).

Documentation [here](http://cedeela.fr/~simon/software/containers).

- `CCHeap`, a purely functional heap structure
- `CCVector`, a growable array (pure OCaml, no C) with mutability annotations
- `CCList`, functions on lists, including tail-recursive implementations of `map` and `append` and many other things
- `CCArray`, utilities on arrays and slices
- `CCHashtbl`, `CCMap` extensions of the standard modules `Hashtbl` and `Map`
- `CCInt`
- `CCString` (basic string operations)
- `CCPair` (cartesian products)
- `CCOpt` (options, very useful)
- `CCFun` (function combinators)
- `CCBool`
- `CCFloat`
- `CCOrd` (combinators for total orderings)
- `CCRandom` (combinators for random generators)
- `CCPrint` (printing combinators)
- `CCHash` (hashing combinators)
- `CCError` (monadic error handling, very useful)

### Containers.data

- `CCCache`, memoization caches, LRU, etc.
- `CCFlatHashtbl`, a flat (open-addressing) hashtable functorial implementation
- `CCTrie`, a prefix tree
- `CCMultimap` and `CCMultiset`, functors defining persistent structures
- `CCFQueue`, a purely functional double-ended queue structure
- `CCBV`, mutable bitvectors
- `CCPersistentHashtbl`, a semi-persistent hashtable (similar to [persistent arrays](https://www.lri.fr/~filliatr/ftp/ocaml/ds/parray.ml.html))

### Containers.io

- `CCIO`, basic utilities for IO

### Containers.sexp

A small S-expression library.

- `CCSexp`, a small S-expression library

### Containers.iter

Iterators:

- `CCKList`, a persistent iterator structure (akin to a lazy list, without memoization)
- `CCKTree`, an abstract lazy tree structure

### String

See [doc](http://cedeela.fr/~simon/software/containers/string).

In the module `Containers_string`:
- `Levenshtein`: edition distance between two strings
- `KMP`: Knuth-Morris-Pratt substring algorithm

### Advanced

See [doc](http://cedeela.fr/~simon/software/containers/advanced).

In the module `Containers_advanced`:
- `CCLinq`, high-level query language over collections
- `CCCat`, a few categorical structures
- `CCBatch`, to combine operations on collections into one traversal

### Misc

See [doc](http://cedeela.fr/~simon/software/containers/misc). This list
is not necessarily up-to-date.

- `PHashtbl`, a polymorphic hashtable (with open addressing)
- `SplayTree`, a polymorphic splay heap implementation (not quite finished)
- `SplayMap`, a polymorphic functional map based on splay trees
- `Heap`, an imperative heap based on `SplayTree`
- `Graph`, a polymorphic imperative directed graph (on top of `PHashtbl`)
- `Hashset`, a polymorphic imperative set on top of `PHashtbl`
- `LazyGraph`, a lazy graph structure on arbitrary (hashable+eq) types, with
basic graph functions that work even on infinite graphs, and printing to DOT.
- `Heap`, a purely functional polymorphic heap
- `Bij`, a GADT-based bijection language used to serialize/deserialize your
data structures
- `RAL`, a random-access list structure, with `O(1)` cons/hd/tl and `O(ln(n))`
access to elements by their index.
- `SmallSet`, a sorted list implementation behaving like a set.
- `AbsSet`, an abstract Set data structure, a bit like `LazyGraph`.
- `Univ`, a universal type encoding with affectation
- `FlatHashtbl`, a (deprecated) open addressing hashtable with
    a functorial interface (replaced by PHashtbl)
- `UnionFind`, a functorial imperative Union-Find structure

### Others

- `Future`, a set of tools for preemptive threading, including a thread pool,
monadic futures, and MVars (concurrent boxes)

- `containers.lwt` contains [Lwt](http://ocsigen.org/lwt/)-related modules (experimental)

There is a QuickCheck-like library called `QCheck` (now in its own repo).

## Build

There are no dependencies (`Sequence` is included).
The `Bij` module requires OCaml `>= 4.00` because of GADTs. Type:

    $ make

To build and run tests (requires `oUnit` and `qtest`):

    $ opam install oUnit
    $ make tests
    $ ./tests.native

and

    $ opam install qtest
    $ make qtest

To build the small benchmarking suite (requires `benchmark`):

    $ opam install benchmark
    $ make bench
    $ ./benchs.native

## Contributing

PRs on github are welcome (patches by email too, if you prefer so).

A few guidelines:

- no dependencies between basic modules (even just for signatures);
- add `@since` tags for new functions;
- add tests if possible (using `qtest`).

Powered by <a href="http://oasis.forge.ocamlcore.org/">
<img src="http://oasis.forge.ocamlcore.org/oasis-badge.png"
     alt="OASIS" style="border: none;" />
</a>
