
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Array Slice} *)

type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a random_gen = Random.State.t -> 'a
type 'a printer = Format.formatter -> 'a -> unit

type 'a t
(** The type for an array slice, containing elements of type ['a] *)

val empty : 'a t
(** [empty] is the empty array slice. *)

val equal : 'a equal -> 'a t equal
(** [equal eq as1 as2] is [true] if the lengths of [as1] and [as2] are the same
    and if the corresponding elements test equal using [eq]. *)

val compare : 'a ord -> 'a t ord
(** [compare cmp as1 as2] compares the two slices [as1] and [as2] using 
    the comparison function [cmp], element by element. *)

val get : 'a t -> int -> 'a
(** [get as n] returns the element number [n] of slice [as].
    The first element has number 0.
    The last element has number [length as - 1].
    You can also write [as.(n)] instead of [get as n].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [(length as - 1)]. *)

val get_safe : 'a t -> int -> 'a option
(** [get_safe as i] returns [Some as.(i)] if [i] is a valid index.
    @since 0.18 *)

val make : 'a array -> int -> len:int -> 'a t
(** [make a i ~len] creates a slice from given offset [i] and length [len] of the given array [a].
    @raise Invalid_argument if the slice isn't valid. *)

val of_slice : ('a array * int * int) -> 'a t
(** [of_slice (a, i, len)] makes a slice from a triple [(a, i, len)] where [a] is the array,
    [i] the offset in [a], and [len] the number of elements of the slice.
    @raise Invalid_argument if the slice isn't valid (See {!make}). *)

val to_slice : 'a t -> ('a array * int * int)
(** [to_slice as] converts the slice [as] into a triple [(a, i, len)] where [len] is the length of
    the sub-array of [a] starting at offset [i]. *)

val to_list : 'a t -> 'a list
(** [to_list as] converts the slice [as] directly to a list.
    @since 1.0 *)

val full : 'a array -> 'a t
(** [full a] creates a slice that covers the full array [a]. *)

val underlying : 'a t -> 'a array
(** [underlying as] returns the underlying array (shared). Modifying this array will modify
    the slice [as]. *)

val copy : 'a t -> 'a array
(** [copy as] copies the slice [as] into a new array. *)

val sub : 'a t -> int -> int -> 'a t
(** [sub as i len] builds a new sub-slice that contains the given subrange specified
    by the index [i] and the length [len]. *)

val set : 'a t -> int -> 'a -> unit
(** [set as n x] modifies the slice [as] in place, replacing
    element number [n] with [x].
    You can also write [as.(n) <- x] instead of [set as n x].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [length as - 1]. *)

val length : _ t -> int
(** [length as] returns the length (number of elements) of the given slice [as]. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f acc as] computes [f (... (f (f acc as.(0)) as.(1)) ...) as.(length as - 1)]. *)

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [foldi f acc as] is just like {!fold} but it also passes in the index of each element
    as the second argument to the folded function [f]. *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** [fold_while f acc as] folds left on slice [as] until a stop condition via [('a, `Stop)]
    is indicated by the accumulator.
    @since 0.8 *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f as] applies function [f] in turn to all elements of [as].
    It is equivalent to [f as.(0); f as.(1); ...; f as.(length as - 1); ()]. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f as] is like {!iter}, but the function [f] is applied with the index of the element
    as first argument, and the element itself as second argument. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit as1 o1 as2 o2 len] copies [len] elements
    from slice [as1], starting at element number [o1], to slice [as2],
    starting at element number [o2]. It works correctly even if
    [as1] and [as2] are the same slice, and the source and
    destination chunks overlap.

    Raise [Invalid_argument "CCArray_slice.blit"] if [o1] and [len] do not
    designate a valid subarray of [as1], or if [o2] and [len] do not
    designate a valid subarray of [as2]. *)

val reverse_in_place : 'a t -> unit
(** [reverse_in_place as] reverses the slice [as] in place. *)

val sorted : ('a -> 'a -> int) -> 'a t -> 'a array
(** [sorted cmp as] makes a copy of [as] and sorts it with [cmp].
    @since 1.0 *)

val sort_indices : ('a -> 'a -> int) -> 'a t -> int array
(** [sort_indices cmp as] returns a new array [b], with the same length as [as],
    such that [b.(i)] is the index at which the [i]-th element of [sorted cmp as]
    appears in [as]. [as] is not modified.

    In other words, [map (fun i -> as.(i)) (sort_indices cmp as) = sorted cmp as].
    [sort_indices] yields the inverse permutation of {!sort_ranking}.
    @since 1.0 *)

val sort_ranking : ('a -> 'a -> int) -> 'a t -> int array
(** [sort_ranking cmp as] returns a new array [b], with the same length as [as],
    such that [b.(i)] is the index at which the [i]-th element of [as] appears
    in [sorted cmp as]. [as] is not modified.

    In other words, [map (fun i -> (sorted cmp as).(i)) (sort_ranking cmp as) = as].
    [sort_ranking] yields the inverse permutation of {!sort_indices}.

    In the absence of duplicate elements in [as], we also have
    [lookup_exn as.(i) (sorted as) = (sorted_ranking as).(i)].
    @since 1.0 *)

val find : ('a -> 'b option) -> 'a t -> 'b option
(** [find f as] returns [Some y] if there is an element [x] such
    that [f x = Some y]. Otherwise returns [None]. *)

val findi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** [findi f as] is like {!find}, but the index of the element is also passed 
    to the predicate function [f].
    @since 0.3.4 *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p as] returns [Some (i,x)] where [x] is the [i]-th element of [as],
    and [p x] holds. Otherwise returns [None].
    @since 0.3.4 *)

val lookup : cmp:'a ord -> 'a -> 'a t -> int option
(** [lookup ~cmp x as] lookups the index [i] of some key [x] in the slice [as], provided [as] is
    sorted using [cmp].
    @return [None] if the key [x] is not present, or
      [Some i] ([i] the index of the key) otherwise. *)

val lookup_exn : cmp:'a ord -> 'a -> 'a t -> int
(** [lookup_exn ~cmp x as] is like {!lookup}, but
    @raise Not_found if the key [x] is not present. *)

val bsearch : cmp:('a -> 'a -> int) -> 'a -> 'a t ->
  [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
(** [bsearch ~cmp x as] finds the index of the object [x] in the slice [as],
    provided [as] is {b sorted} using [cmp]. If the slice is not sorted,
    the result is not specified (may raise Invalid_argument).

    Complexity: [O(log n)] where n is the length of the slice [as]
    (dichotomic search).

    @return
    - [`At i] if [cmp as.(i) x = 0] (for some i).
    - [`All_lower] if all elements of [as] are lower than [x].
    - [`All_bigger] if all elements of [as] are bigger than [x].
    - [`Just_after i] if [as.(i) < x < as.(i+1)].
    - [`Empty] if the slice [as] is empty.

    @raise Invalid_argument if the slice is found to be unsorted w.r.t [cmp].
    @since 0.13 *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [|as1; ...; asn|]] checks if all elements of the slice
    satisfy the predicate [p]. That is, it returns
    [(p as1) && (p as2) && ... && (p asn)]. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [for_all2 p [|as1; ...; asn|] [|bs1; ...; bsn|]] is [true] if each pair of elements [asi bsi]
    satisfies the predicate [p].
    That is, it returns [(p as1 bs1) && (p as2 bs2) && ... && (p asn bsn)].

    @raise Invalid_argument if slices have distinct lengths.
    Allow different types.
    @since 0.20 *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [|as1; ...; asn|]] is [true] if at least one element of
    the slice satisfies the predicate [p]. That is, it returns
    [(p as1) || (p as2) || ... || (p asn)]. *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [exists2 p [|as1; ...; asn|] [|bs1; ...; bsn|]] is [true] if any pair of elements [asi bsi]
    satisfies the predicate [p].
    That is, it returns [(p as1 bs1) || (p as2 bs2) || ... || (p asn bsn)].

    @raise Invalid_argument if slices have distinct lengths.
    Allow different types.
    @since 0.20 *)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** [fold2 f acc as bs] fold on two slices [as] and [bs] stepwise.
    It computes [f (... (f acc as1 bs1)...) asn bsn].

    @raise Invalid_argument if slices have distinct lengths.
    @since 0.20 *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f as bs] iterates on the two slices [as] and [bs] stepwise.
    It is equivalent to  [f as0 bs0; ...; f as.(length as - 1) bs.(length bs - 1); ()].

    @raise Invalid_argument if slices have distinct lengths.
    @since 0.20 *)

val shuffle : 'a t -> unit
(** [shuffle as] randomly shuffles the slice [as], in place. *)

val shuffle_with : Random.State.t -> 'a t -> unit
(** [shuffle_with rs as] randomly shuffles the slice [as] (like {!shuffle}) but a specialized random
    state [rs] is used to control the random numbers being produced during shuffling (for reproducibility). *)

val random_choose : 'a t -> 'a random_gen
(** [random_choose as rs] randomly chooses an element of [as].
    @raise Not_found if the array/slice is empty. *)

val to_seq : 'a t -> 'a sequence
(** [to_seq as] returns a [sequence] of the elements of a slice [as].
    The input slice [as] is shared with the sequence and modification of it will result
    in modification of the sequence. *)

val to_gen : 'a t -> 'a gen
(** [to_gen as] returns a [gen] of the elements of a slice [as]. *)

val to_klist : 'a t -> 'a klist
(** [to_klist as] returns a [klist] of the elements of a slice [as]. *)

(** {2 IO} *)

val pp: ?sep:string -> 'a printer -> 'a t printer
(** [pp ~sep pp_item ppf as] formats the slice [as] on [ppf].
    Each element is formatted with [pp_item] and elements are separated
    by [sep] (defaults to ", "). *)

val pp_i: ?sep:string -> (int -> 'a printer) -> 'a t printer
(** [pp_i ~sep pp_item ppf as] prints the slice [as] on [ppf].
    The printing function [pp_item] is giving both index and element.
    Elements are separated by [sep] (defaults to ", "). *)
