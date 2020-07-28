
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Tuple Functions} *)

type ('a,'b) t = ('a * 'b)

val make : 'a -> 'b -> ('a, 'b) t
(** Make a tuple from its components.
    @since 0.16 *)

val map_fst : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)
(** [map_fst f (x, y)] returns [(f x, y)].
    Renamed from [map1] since 3.0. *)

val map_snd : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)
(** [map_snd f (x, y)] returns [(x, f y)].
    Renamed from [map2] since 3.0. *)

val map : ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd)
(** Synonym to {!( *** )}. Map on both sides of a tuple. *)

val map_same : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
(** Like {!map} but specialized for pairs with elements of the same type. *)

val map2 : ('a1 -> 'b1 -> 'c1) -> ('a2 -> 'b2 -> 'c2) -> ('a1 * 'a2) ->
  ('b1 * 'b2) -> ('c1 * 'c2)
(** [map2 f g (a,b) (x,y)] return [(f a x, g b y)].
    @since 3.0 *)

val map_same2 : ('a -> 'b -> 'c) -> ('a * 'a) -> ('b * 'b) -> ('c * 'c)
(** [map_same2 f (a,b) (x,y)] return [(f a x, f b y)].
    @since 3.0 *)

val fst_map : ('a -> 'b) -> ('a * _) -> 'b
(** Compose the given function with [fst].
    Rename from [map_fst] since 3.0.
    @since 0.3.3 *)

val snd_map : ('a -> 'b) -> (_ * 'a) -> 'b
(** Compose the given function with [snd].
    Rename from [map_snd] since 3.0.
    @since 0.3.3 *)

val iter : ('a -> 'b -> unit) -> ('a * 'b) -> unit

val swap : ('a * 'b) -> ('b * 'a)
(** Swap the components of the tuple. *)

val (<<<) : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)
(** Map on the left side of the tuple. *)

val (>>>) : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)
(** Map on the right side of the tuple. *)

val ( *** ) : ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd)
(** Map on both sides of a tuple. *)

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
(** [f &&& g] is [fun x -> f x, g x]. It splits the computations into
    two parts. *)

val merge : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** Uncurrying (merges the two components of a tuple). *)

val fold : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** Synonym to {!merge}.
    @since 0.3.3 *)

val dup : 'a -> ('a * 'a)
(** [dup x = (x,x)] (duplicate the value).
    @since 0.3.3 *)

val dup_map : ('a -> 'b) -> 'a -> ('a * 'b)
(** [dup_map f x = (x, f x)]. Duplicates the value and applies the function
    to the second copy.
    @since 0.3.3 *)

val equal : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a * 'b) -> ('a * 'b) -> bool

val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int

val to_string : ?sep:string -> ('a -> string) -> ('b -> string) -> ('a * 'b) -> string
(** Print tuple in a string
    @since 2.7 *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : ?pp_start:unit printer -> ?pp_stop:unit printer -> ?pp_sep:unit printer ->
  'a printer -> 'b printer -> ('a * 'b) printer
(** Print a pair given an optional separator, an optional start and stop and a
    method for printing each of its elements. *)
