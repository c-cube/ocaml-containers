
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Tuple Functions} *)

type ('a,'b) t = ('a * 'b)

val make : 'a -> 'b -> ('a, 'b) t
(** Make a tuple from its components
    @since 0.16 *)

val map1 : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)

val map2 : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)

val map : ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd)

val map_same : ('a -> 'b) -> ('a*'a) -> ('b*'b)

val map_fst : ('a -> 'b) -> ('a * _) -> 'b
(** Compose the given function with [fst].
    @since 0.3.3 *)

val map_snd : ('a -> 'b) -> (_ * 'a) -> 'b
(** Compose the given function with [snd].
    @since 0.3.3 *)

val iter : ('a -> 'b -> unit) -> ('a * 'b) -> unit

val swap : ('a * 'b) -> ('b * 'a)
(** Swap the components of the tuple *)

val (<<<) : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)
(** Map on the left side of the tuple *)

val (>>>) : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)
(** Map on the right side of the tuple *)

val ( *** ) : ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd)
(** Map on both sides of a tuple *)

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
(** [f &&& g] is [fun x -> f x, g x]. It splits the computations into
    two parts *)

val merge : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** Uncurrying (merges the two components of a tuple) *)

val fold : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** Synonym to {!merge}
    @since 0.3.3 *)

val dup : 'a -> ('a * 'a)
(** [dup x = (x,x)] (duplicate the value)
    @since 0.3.3 *)

val dup_map : ('a -> 'b) -> 'a -> ('a * 'b)
(** [dup_map f x = (x, f x)]. Duplicates the value and applies the function
    to the second copy.
    @since 0.3.3 *)

val equal : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a * 'b) -> ('a * 'b) -> bool

val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int

type 'a printer = Buffer.t -> 'a -> unit
type 'a formatter = Format.formatter -> 'a -> unit

val pp : 'a printer -> 'b printer -> ('a*'b) printer

val print : 'a formatter -> 'b formatter -> ('a*'b) formatter
