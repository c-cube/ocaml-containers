# More about OCaml-containers

This document contains more information on some modules of Containers.

```ocaml
# #require "containers";;
```

## Hash combinators: `CCHash`

Although OCaml provides polymorphic hash tables (`('a,'b) Hashtbl.t`)
using the polymorphic equality `(=)` and hash `Hashtbl.hash`, it is often
safer and more efficient to use `Hashtbl.Make` (or the extended `CCHashtbl.Make`)
with custom equality and hash functions.

`CCHash` provides combinators for writing hash functions:

```ocaml
# module H = CCHash;;
module H = CCHash

# let hash1 : (int * bool) list H.t = H.(list (pair int bool));;
val hash1 : (int * bool) list H.t = <fun>
```

```ocaml non-deterministic=output
# hash1 [1, true; 2, false; 3, true];;
- : int = 636041136
# hash1 CCList.(1 -- 1000 |> map (fun i->i, i mod 2 = 0));;
- : int = 845685523
# hash1 CCList.(1 -- 1001 |> map (fun i->i, i mod 2 = 0));;
- : int = 381026697
```

The polymorphic hash function is still present, as `CCHash.poly`.
The functions `CCHash.list_comm` and `CCHash.array_comm` allow to hash
lists and arrays while ignoring the order of elements: all permutations
of the input will have the same hash.

## Parser Combinator: `CCParse`

The module `CCParse` defines basic parser combinators on strings.
Adapting [angstrom's tutorial example](https://github.com/inhabitedtype/angstrom#usage)
gives the following snippet.
Note that backtracking is explicit in `CCParse`, hence
the use of `try_` to allow it in some places.
Explicit memoization with `memo` and `fix_memo` is also possible.

```ocaml
open CCParse.Infix
module P = CCParse

let parens p = P.try_ (P.char '(') *> p <* P.char ')'
let add = P.char '+' *> P.return (+)
let sub = P.char '-' *> P.return (-)
let mul = P.char '*' *> P.return ( * )
let div = P.char '/' *> P.return ( / )
let integer =
  P.chars1_if (function '0'..'9'->true|_->false) >|= int_of_string

let chainl1 e op =
  P.fix (fun r ->
    e >>= fun x -> P.try_ (op <*> P.return x <*> r) <|> P.return x)

let expr : int P.t =
  P.fix (fun expr ->
    let factor = parens expr <|> integer in
    let term = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub))
```

Now we can parse strings using `expr`:

```ocaml
# P.parse_string expr "4*1+2";; (* Ok 6 *)
- : int P.or_error = Result.Ok 6

# P.parse_string expr "4*(1+2)";; (* Ok 12 *)
- : int P.or_error = Result.Ok 12
```

