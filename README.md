# OCaml-containers 📦 ![build](https://github.com/c-cube/ocaml-containers/workflows/build/badge.svg)

A modular, clean and powerful extension of the OCaml standard library.

[(Jump to the current API documentation)](https://c-cube.github.io/ocaml-containers/)

Containers is an extension of OCaml's standard library (under BSD license)
focused on data structures, combinators and iterators, without dependencies on
unix, str or num. Every module is independent and is prefixed with 'CC' in the
global namespace. Some modules extend the stdlib (e.g. `CCList` provides safe
`map`/`fold_right`/`append`, and additional functions on lists).
Alternatively, `open Containers` will bring enhanced versions of the standard
modules into scope.

## Quick Summary

Containers is:

- A usable, reasonably well-designed library that extends OCaml's standard
  library (in 'src/core/', packaged under `containers` in ocamlfind. Modules
  are totally independent and are prefixed with `CC` (for "containers-core"
  or "companion-cube" because I'm a megalomaniac). This part should be
  usable and should work. For instance, `CCList` contains functions and
  lists including safe versions of `map` and `append`. It also
  provides a drop-in replacement to the standard library, in the module
  `Containers` (intended to be opened, replaces some stdlib modules
  with extended ones), and a small S-expression printer and parser
  that can be functorized over the representation of values.
- Utilities around the `unix` library in `containers.unix` (mainly to spawn
  sub-processes easily and deal with resources safely)
- A separate library `containers-data` with additional
  data structures that don't have an equivalent in the standard library,
  typically not as thoroughly maintained. This is now in its own package
  since 3.0.
- A separate library for threaded programming in `containers-thread`,
  including a blocking queue, semaphores, an extension of `Mutex`, and
  thread-pool based futures. This is in its own package since 3.0.

Some of the modules have been moved to their own repository (e.g. `sequence` (now `iter`),
`gen`, `qcheck`) and are on opam for great fun and profit.

## Migration Guide

### To 3.0

The [changelog's breaking section](CHANGELOG.md) contains a list of the breaking
changes in this release.

1. The biggest change is that some sub-libraries have been either turned into
  their own packages (`containers-thread`, `containers-data`),
  deleted (`containers.iter`),or merged elsewhere (`containers.sexp`).
  This means that if use these libraries you will have to edit your
  `dune`/`_oasis`/`opam` files.

  - if you use `containers.sexp` (i.e. the `CCSexp` module), it now lives in
    `containers` itself.
  - if you used anything in `containers.data`, you need to depend on the
    `containers-data` package now.

2. Another large change is the removal (at last!) of functions deprecated
  in 2.8, related to the spread of `Seq.t` as the standard iterator type.
  Functions like `CCVector.of_seq` now operate on this standard `Seq.t` type,
  and old-time iteration based on [iter](https://github.com/c-cube/iter)
  is now named `of_iter`, `to_iter`, etc.

  Here you need to change you code, possibly using search and replace.
  Thankfully, the typechecker should guide you.

3. `Array_slice` and `String.Sub` have been removed to simplify the
  code and `String` more lightweight. There is no replacement at the moment.
  Please tell us if you need this to be turned into a sub-library.

4. Renaming of some functions into more explicit/clear names.
  Examples:

  * `CCVector.shrink` is now `CCVector.truncate`
  * `CCVector.remove` is now `CCVector.remove_unordered`, to be
    contrasted with the new `CCVector.remove_and_shift`.
  * `CCPair.map_fst` and `map_snd` now transform a tuple into another tuple
    by modify the first (resp. second) element.

5. All the collection pretty-printers now take their separator/start/stop
  optional arguments as `unit printer` (i.e. `Format.formatter -> unit -> unit`
  functions) rather than strings. This gives the caller better control
  over the formatting of lists, arrays, queues, tables, etc.

6. Removal of many deprecated functions.


### To 2.0

- The type system should detect issues related to `print` renamed into `pp` easily.
  If you are lucky, a call to `sed -i 's/print/pp/g'` on the concerned files
  might help rename all the calls
  properly.

- many optional arguments have become mandatory, because their default value
  would be a polymorphic "magic" operator such as `(=)` or `(>=)`.
  Now these have to be specified explicitly, but during the transition
  you can use `Stdlib.(=)` and `Stdlib.(>=)` as explicit arguments.

- if your code contains `open Containers`, the biggest hurdle you face
  might be that operators have become monomorphic by default.
  We believe this is a useful change that prevents many subtle bugs.
  However, during migration and until you use proper combinators for
  equality (`CCEqual`), comparison (`CCOrd`), and hashing (`CCHash`),
  you might want to add `open Stdlib` just after the `open Containers`.
  See [the section on monomorphic operators](#monomorphic-operators-why-and-how) for more details.

## Monomorphic operators: why, and how?

### Why shadow polymorphic operators by default?

To quote @bluddy in [#196](https://github.com/c-cube/ocaml-containers/issues/196):

The main problem with polymorphic comparison is that many data structures will
give one result for structural comparison, and a different result for semantic
comparison. The classic example is comparing maps. If you have a list of maps
and try to use comparison to sort them, you'll get the wrong result: multiple
map structures can represent the same semantic mapping from key to value, and
comparing them in terms of structure is simply wrong. A far more pernicious bug
occurs with hashtables. Identical hashtables will seem to be identical for a
while, as before they've had a key clash, the outer array is likely to be the
same. Once you get a key clash though, you start getting lists inside the
arrays (or maps inside the arrays if you try to make a smarter hashtable) and
that will cause comparison errors ie. identical hashtables will be seen as
different or vice versa.

Every time you use a polymorphic comparison where you're using a data type
where structural comparison != semantic comparison, it's a bug. And ever time
you use polymorphic comparison where the type of data being compared may vary
(e.g. it's an int now, but it may be a map later), you're planting a bug for
the future.

See also:

- https://blog.janestreet.com/the-perils-of-polymorphic-compare/
- https://blog.janestreet.com/building-a-better-compare/

### Sometimes polymorphic operators still make sense!

If you just want to use polymorphic operators, it's fine! You can access them
easily by using `Stdlib.(=)`, `Stdlib.max`, etc.

When migrating a module, you can add `open Stdlib` on top of it to restore
the default behavior. It is, however, recommended to export an `equal` function
(and `compare`, and `hash`) for all the public types, even if their internal
definition is just the corresponding polymorphic operator.
This way, other modules can refer to `Foo.equal` and will not have to be
updated the day `Foo.equal` is no longer just polymorphic equality.
Another bonus is that `Hashtbl.Make(Foo)` or `Map.Make(Foo)` will just work™.

### Further discussions

See issues
[#196](https://github.com/c-cube/ocaml-containers/issues/196),
[#197](https://github.com/c-cube/ocaml-containers/issues/197)

## Debugging with `ocamldebug`

To print values with types defined in `containers` in the bytecode debugger,
you first have to load the appropriate bytecode archives. After starting a
session, e.g. `ocamldebug your_program.bc`,

```ocaml non-deterministic=command
# #load_printer containers_monomorphic.cma
# #load_printer containers.cma
```

For these archives to be found, you may have to `run` the program first. Now
printing functions that have the appropriate type `Format.formatter -> 'a ->
unit` can be installed. For example,

```ocaml non-deterministic=command
# #install_printer Containers.Int.pp
```

However, printer combinators are not easily handled by `ocamldebug`. For
instance `# install_printer Containers.(List.pp Int.pp)` will *not* work out of
the box. You can make this work by writing a short module which defines
ready-made combined printing functions, and loading that in ocamldebug. For
instance

```ocaml non-deterministic=command
module M = struct
	let pp_int_list = Containers.(List.pp Int.pp)
end
```

loaded via `# load_printer m.cmo` and installed as `# install_printer
M.pp_int_list`.


## Change Log

See [this file](./CHANGELOG.md).

## Finding help

- [Mailing List](http://lists.ocaml.org/listinfo/containers-users)
  the address is <mailto:containers-users@lists.ocaml.org>
- the [github wiki](https://github.com/c-cube/ocaml-containers/wiki)
- on IRC, ask `companion_cube` on `#ocaml@freenode.net`
- there is a `#containers` channel on OCaml's discord server.

## Use

You might start with the [tutorial](#tutorial) to get a picture of how to use the library.

You can either build and install the library (see [build](#build)), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining). Since modules have a friendly license and are mostly
independent, both options are easy.

In a toplevel, using ocamlfind:

```ocaml
# #use "topfind";;
# #require "containers";;
# #require "containers-data";;
# CCList.flat_map;;
- : ('a -> 'b list) -> 'a list -> 'b list = <fun>
# open Containers;;  (* optional *)
# List.flat_map ;;
- : ('a -> 'b list) -> 'a list -> 'b list = <fun>
```

If you have comments, requests, or bugfixes, please share them! :-)

## License

This code is free, under the BSD license.

## Contents

See [the documentation](http://c-cube.github.io/ocaml-containers/)
and [the tutorial below](#tutorial) for a gentle introduction.

## Documentation

In general, see http://c-cube.github.io/ocaml-containers/last/ for the **API documentation**.

Some examples can be found [there](doc/containers.md),
per-version doc [there](http://c-cube.github.io/ocaml-containers/).

## Build

You will need OCaml `>=` 4.03.0.

### Via opam

The preferred way to install is through [opam](http://opam.ocaml.org/).

```
$ opam install containers
```

### From Sources

<details>

You need dune (formerly jbuilder).

```
$ make
```

To build and run tests (requires `oUnit`, [qtest](https://github.com/vincent-hugot/qtest), `gen`, `iter`):

```
$ opam install oUnit qtest
$ make test
```

To build the small benchmarking suite (requires [benchmark](https://github.com/chris00/ocaml-benchmark)):

```
$ opam install benchmark batteries
$ make bench
$ ./benchs/run_benchs.sh
```

</details>

## Contributing

PRs on github are very welcome (patches by email too, if you prefer so).

<details>
<summary>how to contribute (click to unfold)</summary>

### List of authors

The list of contributors can be seen [on github](https://github.com/c-cube/ocaml-containers/graphs/contributors).

Alternatively, `git authors` from git-extras can be invoked from within the repo
to list authors based on the git commits.

### First-Time Contributors

Assuming your are in a clone of the repository:

1. Some dependencies are required, you'll need
  `opam install benchmark qcheck qtest iter gen mdx uutf`.
2. run `make all` to enable everything (including tests).
3. make your changes, commit, push, and open a PR.
4. use `make test` without moderation! It must pass before a PR
  is merged.  There are around 1150 tests right now, and new
  features should come with their own tests.

If you feel like writing new tests, that is totally worth a PR
(and my gratefulness).

### General Guidelines

A few guidelines to follow the philosophy of containers:

- no dependencies between basic modules (even just for signatures);
- add `@since` tags for new functions;
- add tests if possible (using [qtest](https://github.com/vincent-hugot/qtest/)).
  There are numerous inline tests already,
  to see what it looks like search for comments starting with `(*$`
  in source files.

### For Total Beginners

Thanks for wanting to contribute!
To contribute a change, here are the steps (roughly):

1. click "fork" on https://github.com/c-cube/ocaml-containers on the top right of the page. This will create a copy of the repository on your own github account.
2. click the big green "clone or download" button, with "SSH". Copy the URL (which should look like `git@github.com:<your username>/ocaml-containers.git`) into a terminal to enter the command:

    ```
    $ git clone git@github.com:<your username>/ocaml-containers.git
    ```

3. then, `cd` into the newly created directory.
4. make the changes you want. See <#first-time-contributors> for
  more details about what to do in particular.
5. use `git add` and `git commit` to commit these changes.
6. `git push origin master` to push the new change(s) onto your
  copy of the repository
7. on github, open a "pull request" (PR). Et voilà !

</details>

## Tutorial

This tutorial contains a few examples to illustrate the features and
usage of containers.


<details>
<summary>an introduction to containers (click to unfold)</summary>

We assume containers is installed and that
the library is loaded, e.g. with:

```ocaml
# #require "containers";;
# Format.set_margin 50;; (* for readability here *)
- : unit = ()
```

### Basics

We will start with a few list helpers, then look at other parts of
the library, including printers, maps, etc.

```ocaml
# (|>) ;;  (* quick reminder of this awesome standard operator *)
- : 'a -> ('a -> 'b) -> 'b = <fun>
# 10 |> succ;;
- : int = 11

# open CCList.Infix;;

# let l = 1 -- 100;;
val l : int list =
  [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21;
   22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
   40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57;
   58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74; 75;
   76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92; 93;
   94; 95; 96; 97; 98; 99; 100]

# (* transform a list, dropping some elements *)
  l
  |> CCList.filter_map
     (fun x-> if x mod 3=0 then Some (float x) else None)
  |> CCList.take 5 ;;
- : float list = [3.; 6.; 9.; 12.; 15.]

# let l2 = l |> CCList.take_while (fun x -> x<10) ;;
val l2 : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
```

```ocaml
(* an extension of Map.Make, compatible with Map.Make(CCInt) *)
module IntMap = CCMap.Make(CCInt)
```

```ocaml
# (* conversions using the "iter" type, fast iterators that are
   pervasively used in containers. Combinators can be found
   in the opam library "sequence". *)
  let map : string IntMap.t =
    l2
    |> List.map (fun x -> x, string_of_int x)
    |> CCList.to_iter
    |> IntMap.of_iter;;
val map : string IntMap.t = <abstr>

# CCList.to_iter;; (* check the type *)
- : 'a list -> 'a CCList.iter = <fun>
# IntMap.of_iter ;;
- : (int * 'a) CCMap.iter -> 'a IntMap.t = <fun>

# (* we can print, too *)
  Format.printf "@[<2>map =@ @[<hov>%a@]@]@."
    (IntMap.pp CCFormat.int CCFormat.string_quoted)
    map;;
map =
  1 -> "1", 2 -> "2", 3 -> "3", 4 -> "4", 5
  -> "5", 6 -> "6", 7 -> "7", 8 -> "8", 9 -> "9"
- : unit = ()

# (* options are good *)
  IntMap.get 3 map |> CCOpt.map (fun s->s ^ s);;
- : string option = Some "33"
```

### New types: `CCVector`, `CCHeap`, `CCResult`, `CCSexp`

Containers also contains (!) a few datatypes that are not from the standard
library but that are useful in a lot of situations:

- `CCVector`:
  A resizable array, with a mutability parameter. A value of type
  `('a, CCVector.ro) CCVector.t` is an immutable vector of values of type `'a`,
  whereas a `('a, CCVector.rw) CCVector.t` is a mutable vector that
  can be modified. This way, vectors can be used in a quite functional
  way, using operations such as `map` or `flat_map`, or in a more
  imperative way.
- `CCHeap`:
  A priority queue (currently, leftist heaps) functorized over
  a module `sig val t val leq : t -> t -> bool` that provides a type `t`
  and a partial order `leq` on `t`.
- `CCResult`
  An error type for making error handling more explicit (an error monad,
  really, if you're not afraid of the "M"-word).
  Subsumes and replaces the old `CCError`.
  It uses the new `result` type from the standard library (or from
  the retrocompatibility package on opam) and provides
  many combinators for dealing with `result`.
- `CCSexp` and `CCCanonical_sexp`:
  functorized printer and parser for S-expressions, respectively as
  actual S-expressions (like `sexplib`) and as canonical binary-safe
  S-expressions (like `csexp`)

Now for a few examples:

```ocaml
# (* create a new empty vector. It is mutable, for otherwise it would
   not be very useful. *)
  CCVector.create;;
- : unit -> ('a, CCVector.rw) CCVector.t = <fun>

# (* init, similar to Array.init, can be used to produce a
   vector that is mutable OR immutable (see the 'mut parameter?) *)
  CCVector.init ;;
- : int -> (int -> 'a) -> ('a, 'mut) CCVector.t = <fun>
```

```ocaml non-deterministic=output
# (* use the infix (--) operator for creating a range. Notice
   that v is a vector of integer but its mutability is not
   decided yet. *)
  let v = CCVector.(1 -- 10);;
val v : (int, '_a) CCVector.t = <abstr>
```

```ocaml
# Format.printf "v = @[%a@]@." (CCVector.pp CCInt.pp) v;;
v = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
- : unit = ()
# CCVector.push v 42;;
- : unit = ()

# v;; (* now v is a mutable vector *)
- : (int, CCVector.rw) CCVector.t = <abstr>

# (* functional combinators! *)
  let v2 : _ CCVector.ro_vector = v
  |> CCVector.map (fun x-> x+1)
  |> CCVector.filter (fun x-> x mod 2=0)
  |> CCVector.rev ;;
val v2 : int CCVector.ro_vector = <abstr>

# Format.printf "v2 = @[%a@]@." (CCVector.pp CCInt.pp) v2;;
v2 = 10, 8, 6, 4, 2
- : unit = ()
```

```ocaml
(* let's transfer to a heap *)
module IntHeap = CCHeap.Make(struct type t = int let leq = (<=) end);;
```

```ocaml
# let h = v2 |> CCVector.to_iter |> IntHeap.of_iter ;;
val h : IntHeap.t = <abstr>

# (* We can print the content of h
  (printing is not necessarily in order, though) *)
  Format.printf "h = [@[%a@]]@." (IntHeap.pp CCInt.pp) h;;
h = [2,4,6,8,10]
- : unit = ()

# (* we can remove the first element, which also returns a new heap
   that does not contain it — CCHeap is a functional data structure *)
  IntHeap.take h;;
- : (IntHeap.t * int) option = Some (<abstr>, 2)

# let h', x = IntHeap.take_exn h ;;
val h' : IntHeap.t = <abstr>
val x : int = 2

# IntHeap.to_list h' ;; (* see, 2 is removed *)
- : int list = [4; 6; 8; 10]
```

### IO helpers

The core library contains a module called `CCIO` that provides useful
functions for reading and writing files. It provides functions that
make resource handling easy, following
the pattern `with_resource : resource -> (access -> 'a) -> 'a` where
the type `access` is a temporary handle to the resource (e.g.,
imagine `resource` is a file name and `access` a file descriptor).
Calling `with_resource r f` will access `r`, give the  result to `f`,
compute the result of `f` and, whether `f` succeeds or raises an
error, it will free the resource.

Consider for instance:

```ocaml
# CCIO.with_out "./foobar"
    (fun out_channel ->
      CCIO.write_lines_l out_channel ["hello"; "world"]);;
- : unit = ()
```

This just opened the file 'foobar', creating it if it didn't exist,
and wrote two lines in it. We did not have to close the file descriptor
because `with_out` took care of it. By the way, the type signatures are:

```ocaml non-deterministic=command
val with_out :
  ?mode:int -> ?flags:open_flag list ->
  string -> (out_channel -> 'a) -> 'a

val write_lines_l : out_channel -> string list -> unit
```

So we see the pattern for `with_out` (which opens a function in write
mode and gives its functional argument the corresponding file descriptor).

NOTE: you should never let the resource escape the
scope of the `with_resource` call, because it will not be valid outside.
OCaml's type system doesn't make it easy to forbid that so we rely
on convention here (it would be possible, but cumbersome, using
a record with an explicitly quantified function type).

Now we can read the file again:

```ocaml
# let lines : string list = CCIO.with_in "./foobar" CCIO.read_lines_l ;;
val lines : string list = ["hello"; "world"]
```

There are some other functions in `CCIO` that return _generators_
instead of lists. The type of generators in containers
is `type 'a gen = unit -> 'a option` (combinators can be
found in the opam library called "gen"). A generator is to be called
to obtain successive values, until it returns `None` (which means it
has been exhausted). In particular, python users might recognize
the function

```ocaml non-deterministic=command
# CCIO.File.walk ;;
- : string -> walk_item gen = <fun>;;
```

where `type walk_item = [ ``Dir | ``File ] * string` is a path
paired with a flag distinguishing files from directories.


### To go further: `containers-data`

There is also a library called `containers-data`, with lots of
more specialized data-structures.
The documentation contains the API for all the modules; they also provide
interface to `iter` and, as the rest of containers, minimize
dependencies over other modules. To use `containers-data` you need to link it,
either in your build system or by `#require containers-data;;`

A quick example based on purely functional double-ended queues:

```ocaml
# #require "containers-data";;
# #install_printer CCFQueue.pp;;  (* better printing of queues! *)

# let q = CCFQueue.of_list [2;3;4] ;;
val q : int CCFQueue.t = queue {2; 3; 4}

# let q2 = q |> CCFQueue.cons 1 |> CCFQueue.cons 0 ;;
val q2 : int CCFQueue.t = queue {0; 1; 2; 3; 4}

# (* remove first element *)
  CCFQueue.take_front q2;;
- : (int * int CCFQueue.t) option = Some (0, queue {1; 2; 3; 4})

# (* q was not changed *)
  CCFQueue.take_front q;;
- : (int * int CCFQueue.t) option = Some (2, queue {3; 4})

# (* take works on both ends of the queue *)
  CCFQueue.take_back_l 2 q2;;
- : int CCFQueue.t * int list = (queue {0; 1; 2}, [3; 4])
```

### Common Type Definitions

Some structural types are used throughout the library:

- `gen`: `'a gen = unit -> 'a option` is an iterator type. Many combinators
  are defined in the opam library [gen](https://github.com/c-cube/gen)
- `sequence`: `'a sequence = (unit -> 'a) -> unit` is also an iterator type.
  It is easier to define on data structures than `gen`, but it a bit less
  powerful. The opam library [iter](https://github.com/c-cube/iter)
  can be used to consume and produce values of this type. It was renamed
  from `'a sequence` to `'a iter` to distinguish it better from `Core.Sequence`
  and the standard `seq`.
- `error`: `'a or_error = ('a, string) result = Error of string | Ok of 'a`
  using the standard `result` type, supported in `CCResult`.
- `printer`: `'a printer = Format.formatter -> 'a -> unit` is a pretty-printer
  to be used with the standard module `Format`. In particular, in many cases,
  `"foo: %a" Foo.print foo` will type-check.

### Extended Documentation

See [the extended documentation](doc/containers.md) for more examples.

</details>

## HOWTO (for contributors)

<details>

### Make a release

Beforehand, check `grep deprecated -r src` to see whether some functions
can be removed.

- `make all`
- update version in `containers.opam`
- `make update_next_tag` (to update `@since` comments; be careful not to change symlinks)
- check status of modules (`{b status: foo}`) and update if required;
   removed deprecated functions, etc.
- update `CHANGELOG.md` (see its end to find the right git command)
- commit the changes
- `make test doc`
- `export VERSION=<tag here>; git tag -f $VERSION; git push origin :$VERSION; git push origin $VERSION`
- new opam package: `opam publish https://github.com/c-cube/ocaml-containers/archive/<tag>.tar.gz`
- re-generate doc: `make doc` and put it into `gh-pages`

### List Authors

```
git log --format='%aN' | sort -u
```

</details>
