Sequence
========

Simple sequence abstract datatype, intended to transfer a finite number of
elements from one data structure to another. Some transformations on sequences,
like `filter`, `map`, `take`, `drop` and `append` can be performed before the
sequence is iterated/folded on.

Sequence is not designed to be as general-purpose or flexible as, say,
Batteries' `Enum.t`. Rather, it aims at providing a very simple and efficient
way of iterating on a finite number of values, only allocating (most of the time)
one intermediate closure to do so. For instance, iterating on keys, or values,
of a `Hashtbl.t`, without creating a list.

Documentation
=============

See [the online API](http://cedeela.fr/~simon/software/sequence/Sequence.html).

Build
=====

1. via opam `opam install sequence`
2. manually (need OCaml >= 3.12): `make all install`

If you have `OUnit` installed, you can build and run tests with

    $ make tests
    $ ./run_tests.native

If you have `Bench` installed, you can build and run benchmarks with

    $ make benchs
    $ ./benchs.native

To see how to use the library, check the `examples` directory.
`tests.ml` has a few examples of how to convert basic data structures into
sequences, and conversely.

Examples
========

The module `examples/sexpr.mli` exposes the interface of the S-expression
example library. It requires OCaml>=4.0 to compile, because of the GADT
structure used in the monadic parser combinators part of `examples/sexpr.ml`.

License
=======

Sequence is available under the BSD license.
