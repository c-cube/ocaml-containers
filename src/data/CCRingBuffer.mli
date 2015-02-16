(**
 * CCBufferIO - Polymorphic Circular Buffer
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

module type ARRAY = sig
  type elt
  type t

  val make: int -> elt -> t
  val length: t -> int

  val get: t -> int -> elt

  val set: t -> int -> elt -> unit

  val sub: t -> int -> int -> t
  val max_length: int

  val copy : t -> t
  val of_list : elt list -> t
  val to_list : t -> elt list
  val blit : t -> int -> t -> int -> int -> unit

  val iter : (elt -> unit) -> t -> unit
end


module Make : functor (Array:ARRAY) ->
sig

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
  (** fresh copy of the buffer *)

  val capacity : t -> int
  (** length of the inner buffer *)

  val max_capacity : t -> int option
  (** maximum length of the inner buffer, or [None] if unbounded. *)

  val length : t -> int
  (** number of elements currently stored in the buffer *)

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
  (** extract the current content into a list *)

  val clear : t -> unit
  (** clear the content of the buffer. Doesn't actually destroy the content. *)

  val reset : t -> unit
  (** clear the content of the buffer, and also resize it to a default size *)

  val is_empty :t -> bool
  (** is the buffer empty (i.e. contains no elements)? *)

  val next : t -> Array.elt
  (** obtain next element (the first one of the buffer)
      @raise Empty if the buffer is empty *)

  val junk : t -> unit
  (** Drop next element.
      @raise Empty if the buffer is already empty *)

  val skip : t -> int -> unit
  (** [skip b len] removes [len] elements from [b].
      @raise Invalid_argument if [len > length b]. *)

  val iteri : t -> (int -> Array.elt -> unit) -> unit
  (** [iteri b f] calls [f i t] for each element [t] in [buf], with [i]
      being its relative index within [buf]. *)

  val get : t -> int -> Array.elt
  (** [get buf i] returns the [i]-th element of [buf], ie the one that
      is returned by [next buf] after [i-1] calls to [junk buf].
      @raise Invalid_argument if the index is invalid (> [length buf]) *)

  val push_back : t -> Array.elt -> unit
  (** Push value at the back *)

  val peek_front : t -> Array.elt
  (** First value, or Empty *)

  val peek_back : t -> Array.elt
  (** Last value, or Empty *)

  val take_back : t -> Array.elt
  (** Take last value, or raise Empty *)

  val take_front : t -> Array.elt
  (** Take first value, or raise Empty *)

end
