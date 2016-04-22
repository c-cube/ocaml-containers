
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Imperative deque}

    This structure provides fast access to its front and back elements,
    with O(1) operations*)

type 'a t
(** Contains 'a elements, queue in both ways *)

exception Empty

val create : unit -> 'a t
(** New deque *)

val clear : _ t -> unit
(** Remove all elements
    @since 0.13 *)

val is_empty : 'a t -> bool
(** Is the deque empty? *)

val equal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal a b] checks whether [a] and [b] contain the same sequence of
    elements.
    @param eq comparison function for elements
    @since 0.13 *)

val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare a b] compares lexicographically [a] and [b]
    @param cmp comparison function for elements
    @since 0.13 *)

val length : 'a t -> int
(** Number of elements
    used to be linear time, now constant time *)

val push_front : 'a t -> 'a -> unit
(** Push value at the front *)

val push_back : 'a t -> 'a -> unit
(** Push value at the back *)

val peek_front : 'a t -> 'a
(** First value, or @raise Empty if empty *)

val peek_back : 'a t -> 'a
(** Last value, or @raise Empty if empty *)

val take_back : 'a t -> 'a
(** Take last value, or @raise Empty if empty *)

val take_front : 'a t -> 'a
(** Take first value, or @raise Empty if empty *)

val append_front : into:'a t -> 'a t -> unit
(** [append_front ~into q] adds all elements of [q] at the front
    of [into]
    O(length q) in time
    @since 0.13 *)

val append_back : into:'a t -> 'a t -> unit
(** [append_back ~into q] adds all elements of [q] at the back of [into].
    O(length q) in time
    @since 0.13 *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on elements *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on elements
    @since 0.13 *)

(** {2 Conversions} *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

val of_seq : 'a sequence -> 'a t
(** Create a deque from the sequence.
    @since 0.13 optional argument [deque] disappears, use
      {!add_seq_back} instead *)

val to_seq : 'a t -> 'a sequence
(** iterate on the elements *)

val of_gen : 'a gen -> 'a t
(** [of_gen g] makes a deque containing the elements of [g]
    @since 0.13 *)

val to_gen : 'a t -> 'a gen
(** Iterates on elements of the deque
    @since 0.13 *)

val add_seq_front : 'a t -> 'a sequence -> unit
(** [add_seq_front q seq] adds elements of [seq] into the front of [q],
    in reverse order.
    O(n) in time, where [n] is the number of elements to add.
    @since 0.13 *)

val add_seq_back : 'a t -> 'a sequence -> unit
(** [add_seq_back q seq] adds elements of [seq] into the back of [q],
    in order.
    O(n) in time, where [n] is the number of elements to add.
    @since 0.13 *)

val copy : 'a t -> 'a t
(** Fresh copy, O(n) in time *)

val of_list : 'a list -> 'a t
(** Conversion from list, in order
    @since 0.13 *)

val to_list : 'a t -> 'a list
(** List of elements, in order. Less efficient than {!to_rev_list}.
    @since 0.13 *)

val to_rev_list : 'a t -> 'a list
(** Efficient conversion to list, in reverse order
    @since 0.13 *)

(** {2 print} *)

type 'a printer = Format.formatter -> 'a -> unit

val print : 'a printer -> 'a t printer
(** Print the elements
    @since 0.13 *)
