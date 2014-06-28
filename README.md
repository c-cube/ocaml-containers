ocaml-containers
================

1. A usable, reasonably well-designed library that extends OCaml's standard
    library (in `core/`, packaged under `containers` in ocamlfind. Modules
    are totally independent and are prefixed with `CC` (for "containers-core"
    or "companion-cube" because I'm megalomaniac). This part should be
    usable and should work. For instance, `CCList` contains functions and
    lists including safe versions of `map` and `append`.
2. A satellite library, `containers.string` (in directory `string`) with
    a few packed modules that deal with strings (Levenshtein distance,
    KMP search algorithm, and a few naive utils). Again, modules are independent
    and sometimes parametric on the string and char types (so they should
    be able to deal with your favorite unicode library).
3. Random stuff, with *NO* *GUARANTEE* of even being barely usable or tested,
    in other dirs (mostly `misc` but also `lwt` and `threads`). It's where I
    tend to write code when I want to test some idea, so half the modules (at
    least) are unfinished or don't really work.

Some of the modules have been moved to their own repository (e.g. `sequence`,
`gen`, `qcheck` and are on opam for great fun and profit (or not)).

## Use

You can either build and install the library (see `Build`), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). Since modules have a friendly license and are mostly
independent, both options are easy.

If you have comments, requests, or bugfixes, please share them! :-)

## License

This code is free, under the BSD license.

## Documentation

The API documentation can be
found [here](http://cedeela.fr/~simon/software/containers).

## Contents

The design is mostly centered around polymorphism rather than functors. Such
structures comprise (some modules in `misc/`, some other in `core/`):

### Core Structures

- `CCHeap`, a purely functional heap structure.
- `CCFQueue`, a purely functional double-ended queue structure
- `CCBV`, mutable bitvectors
- `CCPersistentHashtbl`, a semi-persistent hashtable (similar to [persistent arrays](https://www.lri.fr/~filliatr/ftp/ocaml/ds/parray.ml.html))
- `CCVector`, a growable array (pure OCaml, no C) with mutability annotations
- `CCGen` and `CCSequence`, generic iterators structures (with structural types so they can be defined in several places). Now also in their own repository and opam packages (`gen` and `sequence`).
- `CCKList`, a persistent iterator structure (akin to a lazy list)
- `CCList`, functions on lists, including tail-recursive implementations of `map` and `append` and many other things
- `CCArray`, utilities on arrays and slices
- `CCLinq`, high-level query language over collections
- `CCMultimap` and `CCMultiset`, functors defining persistent structures
- `CCKTree`, an abstract lazy tree structure (similar to what `CCKlist` is to lists)
- small modules (basic types, utilities):
  - `CCInt`
  - `CCString` (basic string operations)
  - `CCPair` (cartesian products)
  - `CCOpt` (options, very useful)
  - `CCFun` (function combinators)
  - `CCBool`
  - `CCOrd` (combinators for total orderings)
  - `CCRandom` (combinators for random generators)
  - `CCPrint` (printing combinators)
  - `CCHash` (hashing combinators)
  - `CCError` (monadic error handling, very useful)

### String

In the module `Containers_string`:
- `Levenshtein`: edition distance between two strings
- `KMP`: Knuth-Morris-Pratt substring algorithm

### Misc

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
- `Cache`, a low level memoization cache for unary and binary functions
- `Deque`, an imperative double ended FIFO (double-linked list)
- `FlatHashtbl`, a (deprecated) open addressing hashtable with
    a functorial interface (replaced by PHashtbl)
- `UnionFind`, a functorial imperative Union-Find structure.

### Others

- `Future`, a set of tools for preemptive threading, including a thread pool,
monadic futures, and MVars (concurrent boxes)

Some serialisation formats are also implemented, with a streaming, non-blocking
interface that allows the user to feed the input in chunk by chunk (useful
in combination with Lwt/Async). Currently, the modules are:

- `Bencode`, for the [B-encode format](http://en.wikipedia.org/wiki/Bencode),
- `Sexp`, for S-expressions.

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
