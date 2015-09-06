ocaml-containers
================

![logo](media/logo.png)

What is _containers_?

- A usable, reasonably well-designed library that extends OCaml's standard
  library (in `core/`, packaged under `containers` in ocamlfind. Modules
  are totally independent and are prefixed with `CC` (for "containers-core"
  or "companion-cube" because I'm megalomaniac). This part should be
  usable and should work. For instance, `CCList` contains functions and
  lists including safe versions of `map` and `append`. It also
  provides a drop-in replacement to the standard library, in the module
  `Containers` (intended to be opened, replaces some stdlib modules
  with extended ones)
- Several small additional libraries that complement it:
  * `containers.data` with additional data structures that don't have an
    equivalent in the standard library;
  * `containers.io` (deprecated)
  * `containers.iter` with list-like and tree-like iterators;
  * `containers.string` (in directory `string`) with
    a few packed modules that deal with strings (Levenshtein distance,
    KMP search algorithm, and a few naive utils). Again, modules are independent
    and sometimes parametric on the string and char types (so they should
    be able to deal with your favorite unicode library).
- A sub-library with complicated abstractions, `containers.advanced` (with
  a LINQ-like query module, batch operations using GADTs, and others).
- Utilities around the `unix` library in `containers.unix` (mainly to spawn
  sub-processes)
- A bigstring module using `bigarray` in `containers.bigarray`
- A lightweight S-expression printer and streaming parser in `containers.sexp`
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

- *new*: [Mailing List on the forge](https://forge.ocamlcore.org/mail/?group_id=359);
  the address is `containers-users@lists.forge.ocamlcore.org`
- the [github wiki](https://github.com/c-cube/ocaml-containers/wiki)
- on IRC, ask `companion_cube` on `#ocaml`
- [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/c-cube/ocaml-containers?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge) (experimental, might not exist forever)

## Use

You can either build and install the library (see `Build`), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). Since modules have a friendly license and are mostly
independent, both options are easy.

In a toplevel, using ocamlfind:

```ocaml
# #use "topfind";;
# #require "containers";;
# CCList.flat_map;;
- : ('a -> 'b list) -> 'a list -> 'b list = <fun>
# open Containers;;  (* optional *)
# List.flat_map ;;
- : ('a -> 'b list) -> 'a list -> 'b list = <fun>
```

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
- `CCIO`, basic utilities for IO (channels, files)

### Containers.data

- `CCBitField`, bitfields embedded in integers
- `CCBloom`, a bloom filter
- `CCCache`, memoization caches, LRU, etc.
- `CCFlatHashtbl`, a flat (open-addressing) hashtable functorial implementation
- `CCTrie`, a prefix tree
- `CCHashTrie`, a map where keys are hashed and put in a trie by hash
- `CCMultimap` and `CCMultiset`, functors defining persistent structures
- `CCFQueue`, a purely functional double-ended queue structure
- `CCBV`, mutable bitvectors
- `CCHashSet`, mutable set
- `CCPersistentHashtbl` and `CCPersistentArray`, a semi-persistent array and hashtable
  (similar to [persistent arrays](https://www.lri.fr/~filliatr/ftp/ocaml/ds/parray.ml.html))
- `CCMixmap`, `CCMixtbl`, `CCMixset`, containers of universal types (heterogenous containers)
- `CCRingBuffer`, a double-ended queue on top of an array-like structure,
  with batch operations
- `CCIntMap`, map specialized for integer keys based on Patricia Trees,
  with fast merges
- `CCHashconsedSet`, a set structure with sharing of sub-structures
- `CCGraph`, a small collection of graph algorithms
- `CCBitField`, a type-safe implementation of bitfields that fit in `int`

### Containers.io

*deprecated*, `CCIO` is now a core module. You can still install it and
depend on it but it contains no useful module.

### Containers.unix

- `CCUnix`, utils for `Unix`

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

### Thread

In the library `containers.thread`, for preemptive system threads:

- `CCFuture`, a set of tools for preemptive threading, including a thread pool,
  monadic futures, and MVars (concurrent boxes)
- `CCLock`, values protected by locks
- `CCSemaphore`, a simple implementation of semaphores
- `CCThread` basic wrappers for `Thread`

### Misc

See [doc](http://cedeela.fr/~simon/software/containers/misc). This list
is not necessarily up-to-date.

- `AbsSet`, an abstract Set data structure, a bit like `LazyGraph`.
- `Automaton`, `CSM`, state machine abstractions
- `Bij`, a GADT-based bijection language used to serialize/deserialize your data structures
- `Hashset`, a polymorphic imperative set on top of `PHashtbl`
- `LazyGraph`, a lazy graph structure on arbitrary (hashable+eq) types, with basic graph functions that work even on infinite graphs, and printing to DOT.
- `PHashtbl`, a polymorphic hashtable (with open addressing)
- `RAL`, a random-access list structure, with `O(1)` cons/hd/tl and `O(ln(n))` access to elements by their index.
- `RoseTree`, a tree with an arbitrary number of children and its associated zipper
- `SmallSet`, a sorted list implementation behaving like a set.
- `UnionFind`, a functorial imperative Union-Find structure
- `Univ`, a universal type encoding with affectation

### Others

- `containers.lwt` contains [Lwt](http://ocsigen.org/lwt/)-related modules (experimental)

There is a QuickCheck-like library called `QCheck` (now in its own repo).

## Incoming (Breaking) Changes

the following breaking changes are likely to occur for the next release (they
can still be discussed, of course):

- moving `containers.lwt` into its own repository and opam package
- moving `containers.misc` into its own repository and opam package (improving the average quality of containers!)
- aliasing and deprecating `CCList.split` (confusion with `List.split`)

already in git (but can be reverted if needed):

- change exceptions in `CCVector`
- change signature of `CCDeque.of_seq` (remove optional argument)
- heavily refactor `CCLinq` in `containers.advanced`. If you use this module,
  you will most likely have to change your code (into simpler code, hopefully).


## Build

You will need OCaml >= 4.01.0.

### Via opam

The prefered way to install is through [opam](http://opam.ocaml.org/).

    $ opam install containers

### From Sources

On the branch `master` you will need `oasis` to build the library. On the
branch `stable` it is not necessary.

    $ make

To build and run tests (requires `oUnit`, `qtest`, and `qcheck`):

    $ opam install oUnit qtest qcheck
    $ ./configure --enable-tests
    $ make test

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
