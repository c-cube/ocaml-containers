
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

(** {2 Definitions} *)

type t = int
(** A hash value is a positive integer *)

type state

type 'a hash_fun = 'a -> state -> state
(** Hash function for values of type ['a], merging a fingerprint of the
    value into the state of type [t] *)

val init : state
(** Initial value *)

val finish : state -> int
(** Extract a usable hash value *)

val apply : 'a hash_fun -> 'a -> int
(** Apply a hash function to a value.
    [apply f x] is the same as [finish (f x init)] *)

val int : int hash_fun
val bool : bool hash_fun
val char : char hash_fun
val int32 : int32 hash_fun
val int64 : int64 hash_fun
val nativeint : nativeint hash_fun
val slice : string -> int -> int hash_fun
(** [slice s i len state] hashes the slice [i, ... i+len-1] of [s]
    into [state] *)

val combine : ('a -> int) -> 'a hash_fun

val string : string hash_fun

val list : 'a hash_fun -> 'a list hash_fun

val array : 'a hash_fun -> 'a array hash_fun

val opt : 'a hash_fun -> 'a option hash_fun
val pair : 'a hash_fun -> 'b hash_fun -> ('a * 'b) hash_fun
val triple : 'a hash_fun -> 'b hash_fun -> 'c hash_fun -> ('a * 'b * 'c) hash_fun

val if_ : bool -> 'a hash_fun -> 'a hash_fun -> 'a hash_fun
(** Decide which hash function to use depending on the boolean *)

(** {2 Iterators} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

val seq : 'a hash_fun -> 'a sequence hash_fun
val gen : 'a hash_fun -> 'a gen hash_fun
val klist : 'a hash_fun -> 'a klist hash_fun
