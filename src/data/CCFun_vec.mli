(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Functional Vectors} *)

(** Tree with a large branching factor for logarithmic operations with
    a low multiplicative factor.

    {b status: experimental. DO NOT USE (yet)}

    @since 2.1
*)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a ktree = unit -> [`Nil | `Node of 'a * 'a ktree list]

(* TODO: restore this
   (** {2 Transient Identifiers} *)
   module Transient : sig
   type t
   (** Identifiers for transient modifications. A transient modification
      is uniquely identified by a [Transient.t]. Once [Transient.freeze r]
      is called, [r] cannot be used to modify the structure again. *)

   val create : unit -> t
   (** Create a new, active ID. *)

   val equal : t -> t -> bool
   (** Equality between IDs. *)

   val frozen : t -> bool
   (** [frozen i] returns [true] if [freeze i] was called before. In this case,
      the ID cannot be used for modifications again. *)

   val active : t -> bool
   (** [active i] is [not (frozen i)]. *)

   val freeze : t -> unit
   (** [freeze i] makes [i] unusable for new modifications. The values
      created with [i] will now be immutable. *)

   val with_ : (t -> 'a) -> 'a
   (** [with_ f] creates a transient ID [i], calls [f i],
      freezes the ID [i] and returns the result of [f i]. *)

   exception Frozen
   (** Raised when a frozen ID is used. *)
   end
*)

(** {2 Signature} *)

type 'a t

val empty : 'a t

val is_empty : _ t -> bool

val return : 'a -> 'a t

val length : _ t -> int

val push : 'a -> 'a t -> 'a t
(** Add element at the end. *)

val get : int -> 'a t -> 'a option

val get_exn : int -> 'a t -> 'a
(** @raise Not_found if key not present. *)

val pop_exn : 'a t -> 'a * 'a t
(** Pop last element. *)

val pop : 'a t -> ('a * 'a t) option
(** Pop last element.
    @since 2.5 *)

val iter : f:('a -> unit) -> 'a t -> unit

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements with their index, in increasing order. *)

val iteri_rev : f:(int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements with their index, but starting from the end. *)

val fold : f:('b -> 'a -> 'b) -> x:'b -> 'a t -> 'b

val foldi : f:('b -> int -> 'a -> 'b) -> x:'b -> 'a t -> 'b

val append : 'a t -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val choose : 'a t -> 'a option

(* TODO

   val push_mut : id:Transient.t -> 'a -> 'a t -> 'a t
   (** [add_mut ~id k v m] behaves like [add k v m], except it will mutate
    in place whenever possible. Changes done with an [id] might affect all
    versions of the structure obtained with the same [id] (but not
    other versions).
    @raise Transient.Frozen if [id] is frozen. *)

   val pop_mut : id:Transient.t -> 'a t -> 'a * 'a t
   (** Same as {!remove}, but modifies in place whenever possible.
    @raise Transient.Frozen if [id] is frozen. *)

   val append_mut : id:Transient.t -> into:'a t -> 'a t -> 'a t
*)

(** {5 Conversions} *)

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

val add_list : 'a t -> 'a list -> 'a t

val add_iter : 'a t -> 'a iter -> 'a t

val of_iter : 'a iter -> 'a t

val to_iter : 'a t -> 'a iter

val add_gen : 'a t -> 'a gen -> 'a t

val of_gen : 'a gen -> 'a t

val to_gen : 'a t -> 'a gen

(* TODO

   val add_list_mut : id:Transient.t -> 'a t -> 'a list -> 'a t
   (** @raise Frozen if the ID is frozen. *)

   val add_iter_mut : id:Transient.t -> 'a t -> 'a iter -> 'a t
   (** @raise Frozen if the ID is frozen. *)

   val add_gen_mut : id:Transient.t -> 'a t -> 'a gen -> 'a t
   (** @raise Frozen if the ID is frozen. *)
*)

(** {5 IO} *)

val pp : 'a printer -> 'a t printer
