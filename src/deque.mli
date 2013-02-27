(** Imperative deque *)

type 'a t

exception Empty

val create : unit -> 'a t

val is_empty : 'a t -> bool

val length : 'a t -> int

val push_front : 'a t -> 'a -> unit

val push_back : 'a t -> 'a -> unit

val take_back : 'a t -> 'a

val take_front : 'a t -> 'a
