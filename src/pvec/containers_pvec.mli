(** Functional Vectors.

    These are trees with a large branching factor for logarithmic operations with
    a low multiplicative factor.

    {b status: experimental}

    @since 3.13.1
*)

type 'a iter = ('a -> unit) -> unit

[@@@ifge 5.0]

type !'a t

[@@@else_]

type 'a t

[@@@endif]

val empty : 'a t
(** Empty vector. *)

val is_empty : _ t -> bool
(** Is the vector empty? *)

val return : 'a -> 'a t
(** Single element vector. *)

val length : _ t -> int
(** Number of elements. Constant time. *)

val make : int -> 'a -> 'a t
(** [make n x] makes a vector with [n] copies
    of the element [x] *)

val push : 'a t -> 'a -> 'a t
(** Add element at the end. *)

val get : 'a t -> int -> 'a
(** @raise Invalid_argument if key not present. *)

val get_opt : 'a t -> int -> 'a option

val last : 'a t -> 'a
(** Last element.
      @raise Invalid_argument if the vec is empty *)

val last_opt : 'a t -> 'a option

val pop : 'a t -> 'a * 'a t
(** Pop last element.
   @raise Invalid_argument in case the vec is empty. *)

val pop_opt : 'a t -> ('a * 'a t) option
(** Pop last element. *)

val drop_last : 'a t -> 'a t
(** Like {!pop_opt} but doesn't return the last element.
  Returns the same vector if it's empty. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit

val iter_rev : ('a -> unit) -> 'a t -> unit
(** Iterate on elements but starting from the end. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements with their index, in increasing order. *)

val iteri_rev : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements with their index, but starting from the end. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_rev : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_lefti : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_revi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b

val append : 'a t -> 'a t -> 'a t
(** [append a b] adds all elements of [b] at the end of [a]. This is
  at least linear in the length of [b]. *)


val map : ('a -> 'b) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** @since NEXT_RELEASE *)

val choose : 'a t -> 'a option
(** Return an element. It is unspecified which one is returned. *)

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val add_list : 'a t -> 'a list -> 'a t
val add_iter : 'a t -> 'a iter -> 'a t
val of_iter : 'a iter -> 'a t
val to_iter : 'a t -> 'a iter
val add_seq : 'a t -> 'a Seq.t -> 'a t
val of_seq : 'a Seq.t -> 'a t
val to_seq : 'a t -> 'a Seq.t

(**/**)

module Private_ : sig
  type 'a printer = Format.formatter -> 'a -> unit

  val debug : 'a printer -> 'a t printer
end

(**/**)
