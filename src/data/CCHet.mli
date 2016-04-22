
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Associative containers with Heterogenerous Values}

    This is similar to {!CCMixtbl}, but the injection is directly used as
    a key.

    @since 0.17 *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

module Key : sig
  type 'a t

  val create : unit -> 'a t

  val equal : 'a t -> 'a t -> bool
  (** Compare two keys that have compatible types *)
end

type pair =
  | Pair : 'a Key.t * 'a -> pair

(** {2 Imperative table indexed by {!Key}} *)
module Tbl : sig
  type t

  val create : ?size:int -> unit -> t

  val mem : t -> _ Key.t -> bool

  val add : t -> 'a Key.t -> 'a -> unit

  val length : t -> int

  val find : t -> 'a Key.t -> 'a option

  val find_exn : t -> 'a Key.t -> 'a
  (** @raise Not_found if the key is not in the table *)

  val iter : (pair -> unit) -> t -> unit

  val to_seq : t -> pair sequence

  val of_seq : pair sequence -> t

  val add_seq : t -> pair sequence -> unit

  val add_list : t -> pair list -> unit

  val of_list : pair list -> t

  val to_list : t -> pair list
end

(** {2 Immutable map} *)
module Map : sig
  type t

  val empty : t

  val mem : _ Key.t -> t -> bool

  val add : 'a Key.t -> 'a -> t -> t

  val length : t -> int

  val cardinal : t -> int

  val find : 'a Key.t -> t -> 'a option

  val find_exn : 'a Key.t -> t -> 'a
  (** @raise Not_found if the key is not in the table *)

  val iter : (pair -> unit) -> t -> unit

  val to_seq : t -> pair sequence

  val of_seq : pair sequence -> t

  val add_seq : t -> pair sequence -> t

  val add_list : t -> pair list -> t

  val of_list : pair list -> t

  val to_list : t -> pair list
end
