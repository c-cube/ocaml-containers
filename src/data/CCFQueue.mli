
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Functional queues} *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a equal = 'a -> 'a -> bool
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Basics} *)

type +'a t
(** Queue containing elements of type 'a *)

val empty : 'a t

val is_empty : 'a t -> bool

val singleton : 'a -> 'a t

val doubleton : 'a -> 'a -> 'a t

exception Empty

val cons : 'a -> 'a t -> 'a t
(** Push element at the front of the queue *)

val snoc : 'a t -> 'a -> 'a t
(** Push element at the end of the queue *)

val take_front : 'a t -> ('a * 'a t) option
(** Get and remove the first element *)

val take_front_exn : 'a t -> ('a * 'a t)
(** Same as {!take_front}, but fails on empty queues.
    @raise Empty if the queue is empty *)

val take_front_l : int -> 'a t -> 'a list * 'a t
(** [take_front_l n q] takes at most [n] elements from the front
    of [q], and returns them wrapped in a list *)

val take_front_while : ('a -> bool) -> 'a t -> 'a list * 'a t

val take_back : 'a t -> ('a t * 'a) option
(** Take last element *)

val take_back_exn : 'a t -> ('a t * 'a)

val take_back_l : int -> 'a t -> 'a t * 'a list
(** [take_back_l n q] removes and returns the last [n] elements of [q]. The
    elements are in the order of the queue, that is, the head of the returned
    list is the first element to appear via {!take_front}.
    [take_back_l 2 (of_list [1;2;3;4]) = of_list [1;2], [3;4]] *)

val take_back_while : ('a -> bool) -> 'a t -> 'a t * 'a list

(** {2 Individual extraction} *)

val first : 'a t -> 'a option
(** First element of the queue *)

val last : 'a t -> 'a option
(** Last element of the queue *)

val first_exn : 'a t -> 'a
(** Same as {!first} but
    @raise Empty if the queue is empty *)

val last_exn : 'a t -> 'a

val nth : int -> 'a t -> 'a option
(** Return the [i]-th element of the queue in logarithmic time *)

val nth_exn : int -> 'a t -> 'a
(** Unsafe version of {!nth}
    @raise Not_found if the index is wrong *)

val tail : 'a t -> 'a t
(** Queue deprived of its first element. Does nothing on empty queues *)

val init : 'a t -> 'a t
(** Queue deprived of its last element. Does nothing on empty queues *)

(** {2 Global Operations} *)

val append : 'a t -> 'a t -> 'a t
(** Append two queues. Elements from the second one come
    after elements of the first one.
    Linear in the size of the second queue. *)

val rev : 'a t -> 'a t
(** Reverse the queue, O(n) complexity
    @since 0.10 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map values *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Synonym to {!map} *)

val size : 'a t -> int
(** Number of elements in the queue (constant time) *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val iter : ('a -> unit) -> 'a t -> unit

val equal : 'a equal -> 'a t equal

(** {2 Conversions} *)

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list

val add_seq_front : 'a sequence -> 'a t -> 'a t

val add_seq_back : 'a t -> 'a sequence -> 'a t

val to_seq : 'a t -> 'a sequence
val of_seq : 'a sequence -> 'a t

val to_klist : 'a t -> 'a klist
val of_klist : 'a klist -> 'a t

val (--) : int -> int -> int t
(** [a -- b] is the integer range from [a] to [b], both included.
    @since 0.10 *)

val (--^) : int -> int -> int t
(** [a -- b] is the integer range from [a] to [b], where [b] is excluded.
    @since 0.17 *)

val print : 'a printer -> 'a t printer
(** @since 0.13 *)
