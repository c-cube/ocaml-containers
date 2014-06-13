(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Array utils} *)

type 'a t = 'a array

val empty : 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** fold left on array, with index *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter elements out of the array. Only the elements satisfying
    the given predicate will be kept. *)

val reverse_in_place : 'a t -> unit
(** Reverse the array in place *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Map each element into another value, or discard it *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** transform each element into an array, then flatten *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix version of {!flat_map} *)

val for_all : ('a -> bool) -> 'a t -> bool

val for_all2 : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Forall on pairs of arrays.
    @raise Invalid_argument if they have distinct lengths *)

val exists : ('a -> bool) -> 'a t -> bool

val (--) : int -> int -> int t
(** Range array *)

val except_idx : 'a t -> int -> 'a list
(** Remove given index *)

val shuffle : 'a t -> unit
(** shuffle randomly the array, in place *)

val shuffle_with : Random.State.t -> 'a t -> unit
(** Like shuffle but using a specialized random state *)

val pp: ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a t -> unit
(** print an array of items with printing function *)

val pp_i: ?sep:string -> (Buffer.t -> int -> 'a -> unit)
          -> Buffer.t -> 'a t -> unit
(** print an array, giving the printing function both index and item *)

val print : ?sep:string -> (Format.formatter -> 'a -> unit)
          -> Format.formatter -> 'a t -> unit
(** print an array of items with printing function *)
