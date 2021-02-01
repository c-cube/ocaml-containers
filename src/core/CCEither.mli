(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Either Monad}

    Module that is compatible with Either form OCaml 4.12 but can be use with any
    ocaml version compatible with container

    @since 3.2
*)

type 'a iter = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)


include module type of CCShimsEither_

val left : 'a -> ('a, 'b) t
(** [left l] is [Left l] *)

val right : 'b -> ('a, 'b) t
(** [right r] is [Right r] *)

val is_left : ('a, 'b) t -> bool
(** [is_left x] checks if [x = Left _] *)

val is_right : ('a, 'b) t -> bool
(** [is_right x] checks if [x = Right _] *)

val find_left : ('a, 'b) t -> 'a option
(** [find_left x] returns [l] if [x = Left l] and [None] otherwise. *)

val find_right : ('a, 'b) t -> 'b option
(** [find_right x] returns [r] if [x = Left r] and [None] otherwise. *)

val map_left : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t
(** Map of the Left variant. *)

val map_right : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t
(** Map of the Right variant. *)

val map : left:('a1 -> 'a2) -> right:('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
(** Map using [left] or [right]. *)

val fold : left:('a -> 'c) -> right:('b -> 'c) -> ('a, 'b) t -> 'c
(** Fold using [left] or [right]. *)

val iter : left:('a -> unit) -> right:('b -> unit) -> ('a, 'b) t -> unit
(** Iter using [left] or [right]. *)

val for_all : left:('a -> bool) -> right:('b -> bool) -> ('a, 'b) t -> bool
(** Check some property on [Left] or [Right] variant. *)

val equal : left:('a -> 'a -> bool) -> right:('b -> 'b -> bool) ->
  ('a, 'b) t -> ('a, 'b) t -> bool

val compare : left:('a -> 'a -> int) -> right:('b -> 'b -> int) ->
  ('a, 'b) t -> ('a, 'b) t -> int

(** {2 IO} *)

val pp : left:('a printer) -> right:('b printer) -> ('a, 'b) t printer
(** Pretty printer. *)
