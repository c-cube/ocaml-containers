(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
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

(** {1 Imperative skip-list} *)

type 'a gen = unit -> 'a option

type ('a, 'b) t
  (** A skip list that maps elements of type 'a to elements of type 'b *)

val create : ?maxLevel:int -> ('a -> 'a -> int) -> ('a, 'b) t
  (** Create an empty list (comparison function required). The optional
      argument indicates how many layer the skiplist has. *)

val clear : (_, _) t -> unit
  (** Clear content *)

val is_empty : (_, _) t -> bool
  (** Are there any bindings in the list? *)

val find : ('a, 'b) t -> 'a -> 'b
  (** Find mapping for 'a *)

val mem : ('a, _) t -> 'a -> bool
  (** Does the key have a binding in the list? *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
  (** Add the mapping *)

val remove : ('a, 'b) t -> 'a -> unit
  (** Remove binding of the key *)

val length : (_, _) t -> int
  (** Number of elements *)

val gen : ('a, 'b) t -> ('a * 'b) gen

val of_gen : ('a, 'b) t -> ('a * 'b) gen -> unit
