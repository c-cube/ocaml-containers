
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

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
*)

(** {1 complements to list} *)

type 'a t = 'a list

val map : ('a -> 'b) -> 'a t -> 'b t
(** Safe version of map *)

val append : 'a t -> 'a t -> 'a t
(** Safe version of append *)

val (@) : 'a t -> 'a t -> 'a t

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** map and flatten at the same time (safe). Evaluation order is not guaranteed. *)

val flatten : 'a t t -> 'a t
(** Safe flatten *)

val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** cartesian product of the two lists, with the given combinator *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val (<$>) : ('a -> 'b) -> 'a t -> 'b t

val return : 'a -> 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val range : int -> int -> int t
(** [range i j] iterates on integers from [i] to [j] included. It works
    both for decreasing and increasing ranges *)

val (--) : int -> int -> int t
(** Infix alias for [range] *)

val take : int -> 'a t -> 'a t
(** take the [n] first elements, drop the rest *)

val drop : int -> 'a t -> 'a t
(** drop the [n] first elements, keep the rest *)

val split : int -> 'a t -> 'a t * 'a t
(** [split n l] returns [l1, l2] such that [l1 @ l2 = l] and
    [length l1 = min (length l) n] *)

val last : int -> 'a t -> 'a t
(** [last n l] takes the last [n] elements of [l] (or less if
    [l] doesn't have that many elements *)

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Buffer.t -> 'a -> unit

val to_seq : 'a t -> 'a sequence
val of_seq : 'a sequence -> 'a t

val to_gen : 'a t -> 'a gen
val of_gen : 'a gen -> 'a t

(** {2 IO} *)

val pp : ?start:string -> ?stop:string -> ?sep:string ->
         'a printer -> 'a t printer
