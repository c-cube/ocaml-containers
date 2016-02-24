
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

(** {2 Definitions} *)

type t = int
(** A hash value is a positive integer *)

type state
(** State required by the hash function.
    @since 0.16 the state is abstract, for more flexibility *)

type 'a hash_fun = 'a -> state -> state
(** Hash function for values of type ['a], merging a fingerprint of the
    value into the state of type [t] *)

(** {2 Applying Hash} *)

val init : state
(** Initial value *)

val finish : state -> int
(** Extract a usable hash value *)

val apply : 'a hash_fun -> 'a -> int
(** Apply a hash function to a value.
    [apply f x] is the same as [finish (f x init)] *)

(** {2 Basic Combinators}

    Those combinators have been renamed in 0.13, so as to
    remove the trailing "_".
    They are now defined by the application of {!Make}

*)

val bool_ : bool hash_fun
(** @deprecated use {!bool} *)

val char_ : char hash_fun
(** @deprecated use {!char} *)

val int_ : int hash_fun
(** @deprecated use {!int} *)

val string_ : string hash_fun
(** @deprecated use {!string} *)

val int32_ : int32 hash_fun
(** @deprecated use {!int32} *)

val int64_ : int64 hash_fun
(** @deprecated use {!int64} *)

val nativeint_ : nativeint hash_fun
(** @deprecated use {!nativeint} *)

val list_ : 'a hash_fun -> 'a list hash_fun
(** Hash a list. Each element is hashed using [f].
    @deprecated use {!list} *)

val array_ : 'a hash_fun -> 'a array hash_fun
(** @deprecated use {!array} *)

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

(** {2 Generic Hashing}

    Parametrize over the state, and some primitives to hash basic types.
    This can for instance be used for cryptographic hashing or
    checksums such as MD5.

    @since 0.13 *)

module type HASH = sig
  type state

  val int : int -> state -> state
  val bool : bool -> state -> state
  val char : char -> state -> state
  val int32 : int32 -> state -> state
  val int64 : int64 -> state -> state
  val nativeint : nativeint -> state -> state
  val slice : string -> int -> int -> state -> state
  (** [slice s i len state] hashes the slice [i, ... i+len-1] of [s]
      into [state] *)
end

module type S = sig
  include HASH

  type 'a hash_fun = 'a -> state -> state

  val string : string hash_fun

  val list : 'a hash_fun -> 'a list hash_fun

  val array : 'a hash_fun -> 'a array hash_fun

  val opt : 'a hash_fun -> 'a option hash_fun
  val pair : 'a hash_fun -> 'b hash_fun -> ('a * 'b) hash_fun
  val triple : 'a hash_fun -> 'b hash_fun -> 'c hash_fun -> ('a * 'b * 'c) hash_fun

  val if_ : bool -> 'a hash_fun -> 'a hash_fun -> 'a hash_fun
  (** Decide which hash function to use depending on the boolean *)

  (** {2 Iterators} *)

  val seq : 'a hash_fun -> 'a sequence hash_fun
  val gen : 'a hash_fun -> 'a gen hash_fun
  val klist : 'a hash_fun -> 'a klist hash_fun
end

module Make(H : HASH) : S with type state = H.state

include S with type state := state and type 'a hash_fun := 'a hash_fun
