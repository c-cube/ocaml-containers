
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

(** {2 Definitions} *)

type hash = int
(** A hash value is a positive integer. *)

type 'a t = 'a -> hash
(** A hash function for values of type ['a]. *)

val const : hash -> _ t
(** [const h] hashes any value into [h]. Use with caution!. *)

val const0 : _ t
(** Always return 0. Useful for ignoring elements.
    Example: [Hash.(pair string const0)] will map pairs [("a", 1)]
    and [("a", 2)] to the same hash, but not the same as [("b", 1)].
    @since 1.5 *)

val int : int t
val bool : bool t
val char : char t
val int32 : int32 t
val int64 : int64 t
val nativeint : nativeint t
val slice : string -> int -> int t
(** [slice s i len state] hashes the slice [i, ... i+len-1] of [s]
    into [state]. *)

val string : string t

val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val opt : 'a t -> 'a option t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val if_ : bool -> 'a t -> 'a t -> 'a t
(** Decide which hash function to use depending on the boolean. *)

val poly : 'a t
(** [poly x] is [Hashtbl.hash x].
    The regular polymorphic hash function. *)

val list_comm : 'a t -> 'a list t
(** Commutative version of {!list}. Lists that are equal up to permutation
    will have the same hash.
    @since 1.0 *)

val array_comm : 'a t -> 'a array t
(** Commutative version of {!array}. Arrays that are equal up to permutation
    will have the same hash.
    @since 1.0 *)

(** {2 Base hash combinators} *)

val combine : 'a t -> hash -> 'a -> hash

val combine2 : hash -> hash -> hash
val combine3 : hash -> hash -> hash -> hash
val combine4 : hash -> hash -> hash -> hash -> hash

val combine5 : hash -> hash -> hash -> hash -> hash -> hash
(** @since 2.1 *)

val combine6 : hash -> hash -> hash -> hash -> hash -> hash -> hash
(** @since 2.1 *)

(** {2 Iterators} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

val seq : 'a t -> 'a Seq.t t
val iter : 'a t -> 'a iter t
val gen : 'a t -> 'a gen t
val klist : 'a t -> 'a klist t
