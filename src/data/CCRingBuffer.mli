(**
 * CCRingBuffer - Polymorphic Circular Buffer
 * Copyright (C) 2014 Simon Cruanes
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

(** Circular Polymorphic Buffer for IO *)

module Array : sig

  module type S = sig
    type elt
    type t

    val empty : t

    val make: int -> elt -> t

    val length: t -> int

    val get: t -> int -> elt

    val set: t -> int -> elt -> unit

    val sub: t -> int -> int -> t

    val copy : t -> t

    val blit : t -> int -> t -> int -> int -> unit

    val iter : (elt -> unit) -> t -> unit
  end

  module ByteArray :
		S with type elt = char and type t = bytes

  module FloatArray :
    S with type elt = float and type t = float array

  module IntArray :
    S with type elt = int and type t = int array

  module BoolArray :
    S with type elt = bool and type t = bool array

  module Make :
   functor (Elt:sig type t end) ->
      S with type elt = Elt.t and type t = Elt.t array
end

module type S =
sig

  module Array : Array.S

  type t = private {
    mutable start : int;
    mutable stop : int; (* excluded *)
    mutable buf : Array.t;
    bounded: bool;
    size : int
  }
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

  val iteri : t -> (int -> Array.elt -> unit) -> unit
  (** [iteri b f] calls [f i t] for each element [t] in [buf], with [i]
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

  val take_back : t -> Array.elt
  (** Take the last value from back of [t].
      @raise Empty if buffer is already empty. *)

  val take_front : t -> Array.elt
  (** Take the first value from front of [t].
      @raise Empty if buffer is already empty. *)

end

(** Makes a ring buffer module given array implementation *)
module Make_array : functor (Array:Array.S) -> S with module Array = Array

(** An efficient byte based ring buffer *)
module ByteBuffer : S with module Array = Array.ByteArray

(** Makes a ring buffer module given the element type *)
module Make: functor(Elt:sig type t end) -> S with module Array = Array.Make(Elt)
