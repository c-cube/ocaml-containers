ocaml-containers
================

A bunch of containers,written in different occasions. Not all
containers are properly tested (see `tests/` and `make tests`
if you have installed `OUnit`).

The documentation can be found [here](http://cedeela.fr/~simon/software/containers).

The design is mostly centered around polymorphism rather than functors. Such
structures comprise:

- `PHashtbl`, a polymorphic hashtable (with open addressing)
- `SplayTree`, a polymorphic splay heap implementation (not quite finished)
- `SplayMap`, a polymorphic functional map based on splay trees
- `Heap`, an imperative heap based on `SplayTree`
- `Graph`, a polymorphic imperative directed graph (on top of `PHashtbl`)
- `Hashset`, a polymorphic imperative set on top of `PHashtbl`
- `LazyGraph`, a lazy graph structure on arbitrary (hashable+eq) types, with
basic graph functions that work even on infinite graphs, and printing to DOT.
- `FQueue`, a purely functional queue structure
- `Heap`, a purely functional polymorphic heap
- `Bij`, a GADT-based bijection language used to serialize/deserialize your
data structures
- `RAL`, a random-access list structure, with `O(1)` cons/hd/tl and `O(ln(n))`
access to elements by their index.
- `Leftistheap`, a polymorphic heap structure.
- `SmallSet`, a sorted list implementation behaving like a set.
- `AbsSet`, an abstract Set data structure, a bit like `LazyGraph`.

Other structures are:

- `Univ`, a universal type encoding with affectation
- `Cache`, a low level memoization cache for unary and binary functions
- `PersistentHashtbl`, a semi-persistent hashtable (similar to
[persistent arrays](https://www.lri.fr/~filliatr/ftp/ocaml/ds/parray.ml.html))
- `Deque`, an imperative double ended FIFO (double-linked list)
- `Future`, a set of tools for preemptive threading, including a thread pool,
monadic futures, and MVars (concurrent boxes)
- `Vector`, a growable array (pure OCaml, no C; not tested)
- `FlatHashtbl`, a (deprecated) open addressing hashtable with
    a functorial interface (replaced by PHashtbl)
- `Gen` and `Sequence`, generic iterators structures.
- `UnionFind`, a functorial imperative Union-Find structure.

Some serialisation formats are also implemented, with a streaming, non-blocking
interface that allows the user to feed the input in chunk by chunk (useful
in combination with Lwt/Async). Currently, the modules are:

- `Bencode`, for the [B-encode format](http://en.wikipedia.org/wiki/Bencode),
- `Sexp`, for S-expressions.

There is a QuickCheck-like library called `QCheck`.

## Use

You can either build and install the library (see `Build`), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). I therefore recommand it for its simplicity.

If you have comments, requests, or bugfixes, please share them! :-)

## Build

There are no dependencies (`Sequence` is included).
The `Bij` module requires OCaml `>= 4.00`. Type:


    $ make

To build and run tests (requires `oUnit`):

    $ opam install oUnit
    $ make tests
    $ ./tests.native

To build the small benchmarking suite (requires `Bench`):

    $ opam install bench
    $ make bench
    $ ./benchs.native

## License

This code is free, under the BSD license.
