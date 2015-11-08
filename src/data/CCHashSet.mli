(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Mutable Set}

    {b status: unstable}

    @since 0.13 *)

type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module type S = sig
  type t
  type elt

  val create : int -> t
  (** [create n] makes a new set with the given capacity [n] *)

  val singleton : elt -> t
  (** [singleton x] is the singleton [{x}] *)

  val clear : t -> unit
  (** [clear s] removes all elements from [s] *)

  val copy : t -> t
  (** Fresh copy *)

  val copy_into : into:t -> t -> unit
  (** [copy_into ~into s] copies all elements of [s] into [into] *)

  val insert : t -> elt -> unit
  (** [insert s x] adds [x] into [s] *)

  val remove : t -> elt -> unit
  (** Remove the element, if it were in there *)

  val cardinal : t -> int
  (** [cardinal s] returns the number of elements in [s] *)

  val mem : t -> elt -> bool
  (** [mem s x] returns [true] iff [x] is in [s] *)

  val find_exn : t -> elt -> elt
  (** [find_exn s x] returns [y] if [x] and [y] are equal, and [mem s y].
      @raise Not_found if [x] not in [s] *)

  val find : t -> elt -> elt option
  (** Safe version of {!find_exn} *)

  val inter : t -> t -> t
  (** [inter a b] returns [a ∩ b] *)

  val inter_mut : into:t -> t -> unit
  (** [inter_mut ~into a] changes [into] into [a ∩ into] *)

  val union : t -> t -> t
  (** [union a b] returns [a ∪ b] *)

  val union_mut : into:t -> t -> unit
  (** [union_mut ~into a] changes [into] into [a ∪ into] *)

  val diff : t -> t -> t
  (** [diff a b] returns [a - b] *)

  val subset : t -> t -> bool
  (** [subset a b] returns [true] if all elements of [a] are in [b] *)

  val equal : t -> t -> bool
  (** [equal a b] is extensional equality ([a] and [b] have the same elements) *)

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on values *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** Fold on values *)

  val elements : t -> elt list
  (** List of elements *)

  val of_list : elt list -> t

  val to_seq : t -> elt sequence

  val of_seq : elt sequence -> t

  val add_seq : t -> elt sequence -> unit

  val pp : ?sep:string -> elt printer -> t printer
  (** [pp pp_elt] returns a set printer, given a printer for
      individual elements *)
end

module type ELEMENT = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int (** Positive value *)
end

module Make(E : ELEMENT) : S with type elt = E.t
