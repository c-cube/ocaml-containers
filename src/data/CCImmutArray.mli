
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Immutable Arrays}

    Purely functional use of arrays. Update is costly, but reads are very fast.
    Sadly, it is not possible to make this type covariant without using black
    magic.

    @since 0.17 *)

type 'a t
(** Array of values of type 'a. The underlying type really is
    an array, but it will never be modified.

    It should be covariant but OCaml will not accept it. *)

val empty : 'a t

val length : _ t -> int

val singleton : 'a -> 'a t

val doubleton : 'a -> 'a -> 'a t

val make : int -> 'a -> 'a t
(** [make n x] makes an array of [n] times [x] *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] makes the array [[| f 0; f 1; ... ; f (n-1) |]].
    @raise Invalid_argument if [n < 0] *)

val get : 'a t -> int -> 'a
(** Access the element *)

val set : 'a t -> int -> 'a -> 'a t
(** Copy the array and modify its copy *)

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val append : 'a t -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val for_all : ('a -> bool) -> 'a t -> bool

val exists : ('a -> bool) -> 'a t -> bool

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val of_array_unsafe : 'a array -> 'a t
(** Take ownership of the given array. Careful, the array must {b NOT}
    be modified afterwards! *)

val to_seq : 'a t -> 'a sequence

val of_seq : 'a sequence -> 'a t

val of_gen : 'a gen -> 'a t

val to_gen : 'a t -> 'a gen

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

val print :
  ?start:string -> ?stop:string -> ?sep:string ->
  'a printer -> 'a t printer

