
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Hash combinators} *)

(** {2 Definitions} *)

type hash = int
(** A hash value is a positive integer *)

type 'a t = 'a -> hash
(** A hash function for values of type ['a] *)

val const : hash -> _ t
(** [return h] hashes any value into [h]. Use with caution!. *)

val int : int t
val bool : bool t
val char : char t
val int32 : int32 t
val int64 : int64 t
val nativeint : nativeint t
val slice : string -> int -> int t
(** [slice s i len state] hashes the slice [i, ... i+len-1] of [s]
    into [state] *)

val string : string t

val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val opt : 'a t -> 'a option t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val if_ : bool -> 'a t -> 'a t -> 'a t
(** Decide which hash function to use depending on the boolean *)

val poly : 'a t
(** the regular polymorphic hash function *)

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

(** {2 Iterators} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

val seq : 'a t -> 'a sequence t
val gen : 'a t -> 'a gen t
val klist : 'a t -> 'a klist t
