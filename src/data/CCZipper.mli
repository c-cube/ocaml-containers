
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 List Zipper}

    @since 1.0 *)

type 'a t = 'a list * 'a list
(** The pair [l, r] represents the list [List.rev_append l r], but
      with the focus on [r] *)

val empty : 'a t
(** Empty zipper. *)

val is_empty : _ t -> bool
(** Empty zipper? Returns [true] iff the two lists are empty. *)

val to_list : 'a t -> 'a list
(** Convert the zipper back to a list.
    [to_list (l,r)] is [List.rev_append l r]. *)

val to_rev_list : 'a t -> 'a list
(** Convert the zipper back to a {i reversed} list.
    In other words, [to_list (l,r)] is [List.rev_append r l]. *)

val make : 'a list -> 'a t
(** Create a zipper pointing at the first element of the list. *)

val left : 'a t -> 'a t
(** Go to the left, or do nothing if the zipper is already at leftmost pos. *)

val left_exn : 'a t -> 'a t
(** Go to the left, or
    @raise Invalid_argument if the zipper is already at leftmost pos. *)

val right : 'a t -> 'a t
(** Go to the right, or do nothing if the zipper is already at rightmost pos. *)

val right_exn : 'a t -> 'a t
(** Go to the right, or
    @raise Invalid_argument if the zipper is already at rightmost pos. *)

val modify : ('a option -> 'a option) -> 'a t -> 'a t
(** Modify the current element, if any, by returning a new element, or
    returning [None] if the element is to be deleted. *)

val insert : 'a -> 'a t -> 'a t
(** Insert an element at the current position. If an element was focused,
    [insert x l] adds [x] just before it, and focuses on [x]. *)

val remove : 'a t -> 'a t
(** [remove l] removes the current element, if any. *)

val is_focused : _ t -> bool
(** Is the zipper focused on some element? That is, will {!focused}
    return a [Some v]? *)

val focused : 'a t -> 'a option
(** Return the focused element, if any. [focused zip = Some _] iff
    [empty zip = false]. *)

val focused_exn : 'a t -> 'a
(** Return the focused element, or
    @raise Not_found if the zipper is at an end. *)

val drop_before : 'a t -> 'a t
(** Drop every element on the "left" (calling {!left} then will do nothing). *)

val drop_after : 'a t -> 'a t
(** Drop every element on the "right" (calling {!right} then will do nothing),
    keeping the focused element, if any. *)

val drop_after_and_focused : 'a t -> 'a t
(** Drop every element on the "right" (calling {!right} then will do nothing),
    {i including} the focused element if it is present. *)


