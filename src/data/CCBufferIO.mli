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

type 'a t = private {
  mutable start : int;
  mutable stop : int; (* excluded *)
  mutable buf : 'a array;
  size : int
}

exception Empty

val create : int -> 'a t
(** [create size] creates a new buffer with given size *)

val copy : 'a t ->'a  t
(** fresh copy of the buffer *)

val capacity : 'a t -> int
(** length of the inner buffer *)

val length : 'a t -> int
(** number of elements currently stored in the buffer *)

val blit_from : 'a t -> 'a array -> int -> int -> unit
(** [blit_from buf from_buf o len] copies the slice [o, ... o + len - 1] from
    a input buffer [from_buf] to the end of the buffer.
    @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

val blit_into : 'a t -> 'a array -> int -> int -> int
(** [blit_into buf to_buf o len] copies at most [len] elements from [buf]
    into [to_buf] starting at offset [o] in [s].
    @return the number of elements actually copied ([min len (length buf)]).
    @raise Invalid_argument if [o,len] is not a valid slice of [s] *)

val add : 'a t -> 'a array -> unit
(** [add buf t] adds elements [t] at the end of [buf]. *)

val to_list : 'a t -> 'a list
(** extract the current content into a list *)

val clear : 'a t -> unit
(** clear the content of the buffer. Doesn't actually destroy the content. *)

val reset : 'a t -> unit
(** clear the content of the buffer, and also resize it to a default size *)

val is_empty :'a t -> bool
(** is the buffer empty (i.e. contains no elements)? *)

val next : 'a t -> 'a
(** obtain next element (the first one of the buffer)
    @raise Empty if the buffer is empty *)

val pop : 'a t -> 'a
(** obtain and remove next element (the first one)
    @raise Empty if the buffer is empty *)

val junk : 'a t -> unit
(** Drop next element.
    @raise Empty if the buffer is already empty *)

val skip : 'a t -> int -> unit
(** [skip b len] removes [len] elements from [b].
    @raise Invalid_argument if [len > length b]. *)

val iteri : 'a t -> (int -> 'a -> unit) -> unit
(** [iteri b f] calls [f i t] for each element [t] in [buf], with [i]
    being its relative index within [buf]. *)

val get : 'a t -> int -> 'a
(** [get buf i] returns the [i]-th element of [buf], ie the one that
    is returned by [next buf] after [i-1] calls to [junk buf].
    @raise Invalid_argument if the index is invalid (> [length buf]) *)
