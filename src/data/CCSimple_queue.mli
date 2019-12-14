
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Functional queues (fifo)} *)

(** Simple implementation of functional queues
    @since 1.3 *)


(* TODO: remove for 3.0 *)
type 'a sequence = ('a -> unit) -> unit
(** @deprecated use ['a iter] instead *)

type 'a iter = ('a -> unit) -> unit
(** Fast internal iterator.
    @since 2.8 *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option

type +'a t
(** Queue containing elements of type 'a *)

val empty : 'a t

val is_empty : 'a t -> bool

val push : 'a -> 'a t -> 'a t
(** Push element at the end of the queue. *)

val snoc : 'a t -> 'a -> 'a t
(** Flip version of {!push}. *)

val peek : 'a t -> 'a option
(** First element of the queue. *)

val peek_exn : 'a t -> 'a
(** Same as {!peek} but
    @raise Invalid_argument if the queue is empty. *)

val pop : 'a t -> ('a * 'a t) option
(** Get and remove the first element. *)

val pop_exn : 'a t -> ('a * 'a t)
(** Same as {!pop}, but fails on empty queues.
    @raise Invalid_argument if the queue is empty. *)

val junk : 'a t -> 'a t
(** Remove first element. If the queue is empty, do nothing. *)

val append : 'a t -> 'a t -> 'a t
(** Append two queues. Elements from the second one come
    after elements of the first one.
    Linear in the size of the second queue. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map values. *)

val rev : 'a t -> 'a t
(** Reverse the queue. Constant time. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t (** Alias to {!map}. *)

  val (@) : 'a t -> 'a t -> 'a t (** Alias to {!append}. *)

  val (<::) : 'a t -> 'a -> 'a t (** Alias to {!snoc}. *)

end

include module type of Infix

val length : 'a t -> int
(** Number of elements in the queue (linear in time). *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val iter : ('a -> unit) -> 'a t -> unit

val to_list : 'a t -> 'a list
val add_list : 'a t -> 'a list -> 'a t
val of_list : 'a list -> 'a t

val to_iter : 'a t -> 'a iter
val add_iter : 'a t -> 'a iter -> 'a t
val of_iter : 'a iter -> 'a t

val to_seq : 'a t -> 'a sequence
[@@ocaml.deprecated "use to_iter"]

val add_seq : 'a t -> 'a sequence -> 'a t
[@@ocaml.deprecated "use add_iter"]

val of_seq : 'a sequence -> 'a t
[@@ocaml.deprecated "use of_iter"]

val to_std_seq : 'a t -> 'a Seq.t
val add_std_seq : 'a t -> 'a Seq.t -> 'a t
val of_std_seq : 'a Seq.t -> 'a t

val to_klist : 'a t -> 'a klist
[@@ocaml.deprecated "use to_std_seq"]

val add_klist : 'a t -> 'a klist -> 'a t
[@@ocaml.deprecated "use add_std_seq"]

val of_klist : 'a klist -> 'a t
[@@ocaml.deprecated "use of_std_seq"]

val of_gen : 'a gen -> 'a t
val add_gen : 'a t -> 'a gen -> 'a t
val to_gen : 'a t -> 'a gen

(** {2 IO} *)

val pp : ?sep:unit printer -> 'a printer -> 'a t printer
