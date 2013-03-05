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

(** {1 Splay trees} *)

(** See http://en.wikipedia.org/wiki/Splay_tree and
    Okasaki's "purely functional data structures" p46 *)

type ('a, 'b) t
  (** A functional splay tree *)

val empty : cmp:('a -> 'a -> int) -> ('a, 'b) t
  (** Empty splay tree using the given comparison function *)

val is_empty : (_, _) t -> bool
  (** Check whether the tree is empty *)

val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** Insert the pair (key -> value) in the tree *)

val remove : ('a, 'b) t -> 'a -> ('a, 'b) t
  (** Remove an element by its key, returns the splayed tree *)

val replace : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** Insert the pair (key -> value) into the tree, replacing
      the previous binding (if any). It replaces at most one
      binding. *)

val top : ('a, 'b) t -> 'a * 'b
  (** Returns the top value, or raise Not_found is empty *)

val min : ('a, 'b) t -> 'a * 'b
  (** Access minimum value *)

val delete_min : ('a, 'b) t -> 'a * 'b * ('a, 'b) t
  (** Get minimum value and remove it from the tree *)

val find : ('a, 'b) t -> 'a -> 'b * ('a, 'b) t
  (** Find the value for the given key (or raise Not_found).
      It also returns the splayed tree *)

val find_fold : ('a, 'b) t -> 'a -> ('c -> 'b -> 'c) -> 'c -> 'c
  (** Fold on all values associated with the given key *)

val iter : ('a, 'b) t -> ('a -> 'b -> unit) -> unit
  (** Iterate on elements *)

val size : (_, _) t -> int
  (** Number of elements (linear) *)

val get_cmp : ('a, _) t -> ('a -> 'a -> int)
