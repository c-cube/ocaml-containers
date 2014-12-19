(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Bijective Serializer/Deserializer} *)

(** This module helps writing serialization/deserialization code in
    a type-safe way. It uses GADTs, and as such requires OCaml >= 4.00.1.

    Conceptually, a value of type ['a] {! t} describes the (persistent) structure
    of the type ['a]. Combinators, listed in the next section (e.g., {!list_}
    or {!pair}), are used to describe complicated structures from simpler
    ones.
    
    For instance, to serialize a value of type [(int * string) list]:

{[let bij = Bij.(list_ (pair int_ string_));;

let l = [(1, "foo"); (2, "bar")];;

Bij.TrBencode.to_string ~bij l;;
- : string = "lli1e3:fooeli2e3:baree"
]}

  Some types may not be directly describable, for instance records or
  algebraic types. For those, more subtle combinators exist:

  - {!map} is a bijection between two types, and should be typically used to
    map records to tuples (for which combinators exist)

  - {!switch} is a case disjunction. Each case can map to a different type,
    thank to the power of GADT, and a {b key} needs to be provided for
    each case, so that de-serialization can know which type to read.

  - {!fix} allows to describe recursive encodings. The user provides a function
    which, given a ['a t lazy_t], builds a ['a t], and return its fixpoint.

  For instance, let's take a simple symbolic expressions structure (can
  be found in the corresponding test file "tests/test_bij.ml"):

{[
type term =
  | Const of string
  | Int of int
  | App of term list;;

let bij_term =
  Bij.(fix
    (fun bij ->
    switch
    ~inject:(function
      | Const s -> "const", BranchTo (string_, s)
      | Int i -> "int", BranchTo (int_, i)
      | App l -> "app", BranchTo (list_ (Lazy.force bij), l))
    ~extract:(function
      | "const" -> BranchFrom (string_, fun x -> Const x)
      | "int" -> BranchFrom (int_, fun x -> Int x)
      | "app" -> BranchFrom (list_ (Lazy.force bij), fun l -> App l)
      | _ -> raise (DecodingError "unexpected case switch")))
    )
]}

  A bijection could be used for many things, but here our focus is on
  serialization and de-serialization. The idea is that we can map a value
  [x : 'a] to some general-purpose serialization format
  (json, XML, B-encode, etc.) that we can then write to the disk or network;
  the reverse operation is also possible (and bijectivity is enforced
  by the fact that we use a single datatype ['a t] to describe both mappings).

  For now, only a bijection to B-encode (see {!Bencode} and {!Bij.TrBencode})
  is provided. The code is quite straightforward and could be extended
  to XML or Json without hassle.
*)

type _ t = private
  | Unit : unit t
  | String : string t
  | Int : int t
  | Bool : bool t
  | Float : float t
  | List : 'a t -> 'a list t
  | Many : 'a t -> 'a list t
  | Opt : 'a t -> 'a option t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Quad : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Quint : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) t
  | Guard : ('a -> bool) * 'a t -> 'a t
  | Map : ('a -> 'b) * ('b -> 'a) * 'b t -> 'a t
  | Switch :  ('a -> string * 'a inject_branch) *
              (string-> 'a extract_branch) -> 'a t
and _ inject_branch =
  | BranchTo : 'b t * 'b -> 'a inject_branch
and _ extract_branch =
  | BranchFrom : 'b t * ('b -> 'a) -> 'a extract_branch

(** {2 Bijection description} *)

val unit_ : unit t
val string_ : string t
val int_ : int t
val bool_ : bool t
val float_ : float t

val list_ : 'a t -> 'a list t
val many : 'a t -> 'a list t  (* non empty *)
val opt : 'a t -> 'a option t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val quint : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
val guard : ('a -> bool) -> 'a t -> 'a t
  (** Validate values at encoding and decoding *)

val map : inject:('a -> 'b) -> extract:('b -> 'a) -> 'b t -> 'a t

val switch : inject:('a -> string * 'a inject_branch) ->
             extract:(string -> 'a extract_branch) -> 'a t
  (** Discriminates unions based on the next character.
      [inject] must give a unique key for each branch, as well as mapping to another
      type (the argument of the algebraic constructor);
      [extract] retrieves which type to parse based on the key. *)

val fix : ('a t lazy_t -> 'a t) -> 'a t
  (** Helper for recursive encodings. The parameter is the recursive bijection
      itself. It must be lazy. *)

(** {2 Helpers} *)

val with_version : string -> 'a t -> 'a t
  (** Guards the values with a given version. Only values encoded with
      the same version will fit. *)

val array_ : 'a t -> 'a array t

val hashtbl : 'a t -> 'b t -> ('a, 'b) Hashtbl.t t

(** {2 Exceptions} *)

exception EncodingError of string
  (** Raised when encoding is impossible *)

exception DecodingError of string
  (** Raised when decoding is impossible *)
