
(* This file is free software, part of containers. See file "license" for more details. *)

(* Copyright (C) 2015 Simon Cruanes, Carmelo Piccione *)

(** {1 Circular Buffer (Deque)}

    Useful for IO, or as a bounded-size alternative to {!Queue} when
    batch operations are needed.

    {b status: experimental}

    Change in the API to provide only a bounded buffer since 1.3

    @since 0.9
*)

(** {2 Underlying Array} *)

(** The abstract type for arrays *)
module Array : sig
  module type S = sig
    (** The element type *)
    type elt

    (** The type of an array instance *)
    type t

    val dummy : elt
    (** A dummy element used for empty slots in the array
        @since 2.4 *)

    val create : int -> t
    (** Make an array of the given size, filled with dummy elements. *)

    val length: t -> int
    (** [length t] gets the total number of elements currently in [t]. *)

    val get: t -> int -> elt
    (** [get t i] gets the element at position [i]. *)

    val set: t -> int -> elt -> unit
    (** [set t i e] sets the element at position [i] to [e]. *)

    val sub: t -> int -> int -> t
    (** [sub t i len] gets the sub-array of [t] from
        position [i] to [i + len]. *)

    val copy : t -> t
    (** [copy t] makes a fresh copy of the array [t]. *)

    val blit : t -> int -> t -> int -> int -> unit
    (** [blit t s arr i len] copies [len] elements from [arr] starting at [i]
        to position [s] from [t]. *)

    val iter : (elt -> unit) -> t -> unit
    (** [iter f t] iterates over the array [t] invoking [f] with
        the current element, in array order. *)
  end

  (** Efficient array version for the [char] type *)
  module Byte :
    S with type elt = char and type t = Bytes.t

  (** Makes an array given an arbitrary element type *)
  module Make(Elt:sig type t val dummy : t end) :
    S with type elt = Elt.t and type t = Elt.t array
end

(** {2 Ring Buffer}

    The abstract ring buffer type, made concrete by choice of
    [ARRAY] module implementation *)
module type S = sig
  (** The module type of Array for this ring buffer *)
  module Array : Array.S

  (** Defines the bounded ring buffer type *)
  type t

  (** Raised in querying functions when the buffer is empty *)
  exception Empty

  val create : int -> t
  (** [create size] creates a new bounded buffer with given size.
      The underlying array is allocated immediately and no further (large)
      allocation will happen from now on.
      @raise Invalid_argument if the argument is [< 1]. *)

  val copy : t -> t
  (** Make a fresh copy of the buffer. *)

  val capacity : t -> int
  (** Length of the inner buffer. *)

  val length : t -> int
  (** Number of elements currently stored in the buffer. *)

  val is_full : t -> bool
  (** [true] if pushing an element would erase another element.
      @since 1.3 *)

  val blit_from : t -> Array.t -> int -> int -> unit
  (** [blit_from buf from_buf o len] copies the slice [o, ... o + len - 1] from
      an input buffer [from_buf] to the end of the buffer.
      If the slice is too large for the buffer, only the last part of the array
      will be copied.
      @raise Invalid_argument if [o,len] is not a valid slice of [s]. *)

  val blit_into : t -> Array.t -> int -> int -> int
  (** [blit_into buf to_buf o len] copies at most [len] elements from [buf]
      into [to_buf] starting at offset [o] in [s].
      @return the number of elements actually copied ([min len (length buf)]).
      @raise Invalid_argument if [o,len] is not a valid slice of [s]. *)

  val append : t -> into:t -> unit
  (** [append b ~into] copies all data from [b] and adds it at the
      end of [into]. Erases data of [into] if there is not enough room. *)

  val to_list : t -> Array.elt list
  (** Extract the current content into a list. *)

  val clear : t -> unit
  (** Clear the content of the buffer *)

  val is_empty :t -> bool
  (** Is the buffer empty (i.e. contains no elements)? *)

  val junk_front : t -> unit
  (** Drop the front element from [t].
      @raise Empty if the buffer is already empty. *)

  val junk_back : t -> unit
  (** Drop the back element from [t].
      @raise Empty if the buffer is already empty. *)

  val skip : t -> int -> unit
  (** [skip b len] removes [len] elements from the front of [b].
      @raise Invalid_argument if [len > length b]. *)

  val iter : t -> f:(Array.elt -> unit) -> unit
  (** [iter b ~f] calls [f i t] for each element [t] in [buf]. *)

  val iteri : t -> f:(int -> Array.elt -> unit) -> unit
  (** [iteri b ~f] calls [f i t] for each element [t] in [buf], with [i]
      being its relative index within [buf]. *)

  val get_front : t -> int -> Array.elt
  (** [get_front buf i] returns the [i]-th element of [buf] from the front, i.e.
      the one returned by [take_front buf] after [i-1] calls to [junk_front buf].
      @raise Invalid_argument if the index is invalid (> [length buf]). *)

  val get_back : t -> int -> Array.elt
  (** [get_back buf i] returns the [i]-th element of [buf] from the back, i.e.
      the one returned by [take_back buf] after [i-1] calls to [junk_back buf].
      @raise Invalid_argument if the index is invalid (> [length buf]). *)

  val push_back : t -> Array.elt -> unit
  (** Push value at the back of [t].
      If [t.bounded=false], the buffer will grow as needed,
      otherwise the oldest elements are replaced first. *)

  val peek_front : t -> Array.elt option
  (** First value from front of [t], without modification. *)

  val peek_front_exn : t -> Array.elt
  (** First value from front of [t], without modification.
      @raise Empty if buffer is empty.
      @since 1.3 *)

  val peek_back : t -> Array.elt option
  (** Get the last value from back of [t], without modification. *)

  val peek_back_exn : t -> Array.elt
  (** Get the last value from back of [t], without modification.
      @raise Empty if buffer is empty.
      @since 1.3 *)

  val take_back : t -> Array.elt option
  (** Take and remove the last value from back of [t], if any. *)

  val take_back_exn : t -> Array.elt
  (** Take and remove the last value from back of [t].
      @raise Empty if buffer is already empty. *)

  val take_front : t -> Array.elt option
  (** Take and remove the first value from front of [t], if any. *)

  val take_front_exn : t -> Array.elt
  (** Take and remove the first value from front of [t].
      @raise Empty if buffer is already empty. *)

  val of_array : Array.t -> t
  (** Create a buffer from an initial array, but doesn't take ownership
      of it (still allocates a new internal array).
      @since 0.11 *)

  val to_array : t -> Array.t
  (** Create an array from the elements, in order.
      @since 0.11 *)
end

(** An efficient byte based ring buffer *)
module Byte : S with module Array = Array.Byte

(** Makes a ring buffer module with the given array type *)
module MakeFromArray(A : Array.S) : S with module Array = A

(** Buffer using regular arrays *)
module Make(X : sig
    type t
    val dummy : t
  end) : S with type Array.elt = X.t and type Array.t = X.t array
