
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

(** {1 Functional streams for Lwt} *)

type 'a t = [ `Nil | `Cons of 'a * 'a t ] Lwt.t
type 'a stream = 'a t

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

val create : (unit -> 'a option Lwt.t) -> 'a t
(** Create from a function that returns the next element *)

val next : 'a t -> ('a * 'a t) option Lwt.t
(** Obtain the next element *)

val next_exn : 'a t -> ('a * 'a t) Lwt.t
(** Obtain the next element or fail
    @raise Not_found if the stream is empty (using {!Lwt.fail}) *)

val map : ('a -> 'b) -> 'a t -> 'b t

val map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t

val append : 'a t -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val filter_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val iter : ('a -> unit) -> 'a t -> unit Lwt.t

val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a Lwt.t

val fold_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b t -> 'a Lwt.t

val take : int -> 'a t -> 'a t

val take_while : ('a -> bool) -> 'a t -> 'a t

val take_while_s : ('a -> bool Lwt.t) -> 'a t -> 'a t

val drop : int -> 'a t -> 'a t

val drop_while : ('a -> bool) -> 'a t -> 'a t

val drop_while_s : ('a -> bool Lwt.t) -> 'a t -> 'a t

val merge : 'a t -> 'a t -> 'a t
(** Non-deterministic merge *)

(** {2 Conversions} *)

type 'a gen = unit -> 'a option

val of_list : 'a list -> 'a t

val of_array : 'a array -> 'a t

val of_gen : 'a gen -> 'a t

val of_gen_s : 'a Lwt.t gen -> 'a t

val of_string : string -> 'a t

val to_list : 'a t -> 'a list Lwt.t

val to_rev_list : 'a t -> 'a list Lwt.t

val to_string : char t -> string Lwt.t

