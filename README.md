ocaml-containers
================

A bunch of containers,written in different occasions. Probably not very high
quality, since not all containers are tested (yet). 

The design is centerred around polymorphism rather than functors. Such
structures comprise:

- `PHashtbl`, a polymorphic hashtable (with open addressing)
- `SplayTree`, a polymorphic splay heap implementation
- `Heap`, an imperative heap based on `SplayTree`
- `Graph`, a polymorphic imperative directed graph (on top of `PHashtbl`)
- `Hashset`, a polymorphic imperative set on top of `PHashtbl`

Other structures (not touched for months, may not work properly) are:

- `Cache`, a low level memoization cache for pairs of keys
- `Vector`, a growable array (pure OCaml, no C)
- `Deque`, an imperative double ended FIFO (double-linked list)
- `FlatHashtbl`, a deprecated open addressing hashtable with
    a functorial interface (replaced by PHashtbl)

## Use

You can either build and install the library (see `Build`), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). I therefore recommand it for its simplicity.

If you have comments, requests, or bugfixes, please share them! :-)

## Build

You need the library `sequence`. With opam, type `opam install sequence`.

Then:

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
