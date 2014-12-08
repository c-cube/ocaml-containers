Gen
===

Iterators for OCaml, both restartable and consumable. Performances should
be good, yet the code is simple and straightforward.

The documentation can be found [here](http://cedeela.fr/~simon/software/gen)

## Use

You can either build and install the library (see `Build`), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). I therefore recommand it for its simplicity.

If you have comments, requests, or bugfixes, please share them! :-)

## Build

There are no dependencies. This should work with OCaml>=3.12.

    $ make

To build and run tests (requires `oUnit`):

    $ opam install oUnit
    $ make tests
    $ ./tests.native

## License

This code is free, under the BSD license.
