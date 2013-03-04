ocaml-containers
================

A bunch of containers,written in different occasions. Probably not very high
quality, since not all containers are tested. 

## Use

You can either build and install the library (see `Build`), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). I therefore recommand it for its simplicity.

## Build

You need the library `sequence`. With opam, type `opam install sequence`.

Then:

    $ make

To build and run tests (requires `oUnit`):

    $ make tests
    $ ./tests.native

## License

This code is free, under the BSD license. The module `leftistheap` is due
to [Jean-Christophe Filliâtre](https://www.lri.fr/~filliatr/), under
the GPL license.
