(*
 * CCRingBuffer - Polymorphic Circular Buffer
 * Copyright (C) 2015 Simon Cruanes, Carmelo Piccione
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** {1 Circular Buffer (Deque)}

    Useful for IO, or as a general-purpose alternative to {!Queue} when
    batch operations are needed.

    {b status: experimental}

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

    val empty : t
    (** The empty array *)

    val make: int -> elt -> t
    (** [make s e] makes an array of size [s] with [e] elements *)

    val length: t -> int
    (** [length t] gets the total number of elements currently in [t] *)

    val get: t -> int -> elt
    (** [get t i] gets the element at position [i] *)

    val set: t -> int -> elt -> unit
    (** [set t i e] sets the element at position [i] to [e] *)

    val sub: t -> int -> int -> t
    (** [sub t i len] gets the subarray of [t] from
        position [i] to [i + len] *)

    val copy : t -> t
    (** [copy t] makes a fresh copy of the array [t] *)

    val blit : t -> int -> t -> int -> int -> unit
    (** [blit t s arr i len] copies [len] elements from [arr] starting at [i]
        to position [s] from [t] *)

    val iter : (elt -> unit) -> t -> unit
    (** [iter f t] iterates over the array [t] invoking [f] with
        the current element, in array order *)
  end

  (** Efficient array version for the [char] type *)
  module Byte :
    S with type elt = char and type t = Bytes.t

  (** Makes an array given an arbitrary element type *)
  module Make(Elt:sig type t end) :
    S with type elt = Elt.t and type t = Elt.t array
end

(** {2 Ring Buffer}

    The abstract ring buffer type, made concrete by choice of
    [ARRAY] module implementation *)
module type S = sig
  (** The module type of Array for this ring buffer *)
  module Array : Array.S

  (** Defines the ring buffer type, with both bounded and
      unbounded flavors *)
  type t

  (** Raised in querying functions when the buffer is empty *)
  exception Empty

  val create : ?bounded:bool -> int -> t
  (** [create ?bounded size] creates a new buffer with given size.
      Defaults to [bounded=false]. *)

  val copy : t -> t
  (** Make a fresh copy of the buffer. *)

  val capacity : t -> int
  (** Length of the inner buffer. *)

  val max_capacity : t -> int option
  (** Maximum length of the inner buffer, or [None] if unbounded. *)

  val length : t -> int
  (** Number of elements currently stored in the buffer. *)

  val blit_from : t -> Array.t -> int -> int -> unit
  (** [blit_from buf from_buf o len] copies the slice [o, ... o + len - 1] from
      a input buffer [from_buf] to the end of the buffer.
      @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

  val blit_into : t ->  Array.t -> int -> int -> int
  (** [blit_into buf to_buf o len] copies at most [len] elements from [buf]
      into [to_buf] starting at offset [o] in [s].
      @return the number of elements actually copied ([min len (length buf)]).
      @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

  val append : t -> into:t -> unit
  (** [append b ~into] copies all data from [b] and adds it at the
      end of [into] *)

  val to_list : t -> Array.elt list
  (** Extract the current content into a list *)

  val clear : t -> unit
  (** Clear the content of the buffer. Doesn't actually destroy the content. *)

  val reset : t -> unit
  (** Clear the content of the buffer, and also resize it to a default size *)

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
  (** [iter b ~f] calls [f i t] for each element [t] in [buf] *)

  val iteri : t -> f:(int -> Array.elt -> unit) -> unit
  (** [iteri b ~f] calls [f i t] for each element [t] in [buf], with [i]
      being its relative index within [buf]. *)

  val get_front : t -> int -> Array.elt
  (** [get_front buf i] returns the [i]-th element of [buf] from the front, ie
      the one returned by [take_front buf] after [i-1] calls to [junk_front buf].
      @raise Invalid_argument if the index is invalid (> [length buf]) *)

  val get_back : t -> int -> Array.elt
  (** [get_back buf i] returns the [i]-th element of [buf] from the back, ie
      the one returned by [take_back buf] after [i-1] calls to [junk_back buf].
      @raise Invalid_argument if the index is invalid (> [length buf]) *)

  val push_back : t -> Array.elt -> unit
  (** Push value at the back of [t].
      If [t.bounded=false], the buffer will grow as needed,
      otherwise the oldest elements are replaced first. *)

  val peek_front : t -> Array.elt
  (** First value from front of [t].
      @raise Empty if buffer is empty. *)

  val peek_back : t -> Array.elt
  (** Get the last value from back of [t].
      @raise Empty if buffer is empty. *)

  val take_back : t -> Array.elt option
  (** Take the last value from back of [t], if any *)

  val take_back_exn : t -> Array.elt
  (** Take the last value from back of [t].
      @raise Empty if buffer is already empty. *)

  val take_front : t -> Array.elt option
  (** Take the first value from front of [t], if any *)

  val take_front_exn : t -> Array.elt
  (** Take the first value from front of [t].
      @raise Empty if buffer is already empty. *)

  val of_array : Array.t -> t
  (** Create a buffer from an initial array, but doesn't take ownership
      of it (stills allocates a new internal array)
      @since 0.11 *)

  val to_array : t -> Array.t
  (** Create an array from the elements, in order.
      @since 0.11 *)
end

(** An efficient byte based ring buffer *)
module Byte : S with module Array = Array.Byte

(** Makes a ring buffer module with the given array type. *)
module MakeFromArray(A : Array.S) : S with module Array = A

(** Buffer using regular arrays *)
module Make(X : sig type t end) : S with type Array.elt = X.t and type Array.t = X.t array
