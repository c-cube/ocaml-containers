
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

(** {1 AVL trees} *)

type 'a comparator = 'a -> 'a -> int

type ('a,'b) tree = private
  | Empty
  | Node of ('a,'b) tree * 'a * 'b * ('a,'b) tree * int

type ('a,'b) t = private {
  cmp: 'a comparator;
  t: ('a,'b) tree
}

val empty : cmp:'a comparator -> ('a,'b) t
(** Empty tree *)

val singleton : cmp:'a comparator -> 'a -> 'b -> ('a,'b) t
(** Tree with a single node *)

val fold : ('c -> 'a -> 'b -> 'c) -> 'c -> ('a,'b) t -> 'c
(** Fold on all key/value pairs in the tree *)

val for_all : ('a -> 'b -> bool) -> ('a,'b) t -> bool
val exists : ('a -> 'b -> bool) -> ('a,'b) t -> bool

val find : ('a,'b) t -> 'a -> 'b option
(** Find the value associated to the key, if any *)

val find_exn : ('a,'b) t -> 'a -> 'b
(** @raise Not_found if the key is not present *)

val insert : ('a,'b) t -> 'a -> 'b -> ('a,'b) t
(** Insertion in the tree *)

val remove : ('a,'b) t -> 'a -> ('a,'b) t
(** Removal from the tree *)

val update : ('a,'b) t -> 'a ->
            ('b option -> ('a * 'b) option) -> ('a,'b) t
(** Update of the given key binding (subsumes [insert] and [remove]) *)

val split : ('a,'b) t -> 'a ->
            ('a,'b) t * 'b option * ('a,'b) t
(** [split ~cmp t k] splits [t] into a left part that
    is smaller than [k], the possible binding of [k],
    and a part bigger than [k]. *)

val merge :
            ('a -> 'b option -> 'c option -> 'd option) ->
            ('a,'b) t -> ('a,'c) t -> ('a,'d) t
(** Merge two trees together, with the given function *)

val of_list : cmp:'a comparator -> ('a * 'b) list -> ('a,'b) t
(** Add a list of bindings *)

val to_list : ('a,'b) t -> ('a * 'b) list
(** List of bindings, in infix order *)

(** {2 Iterators} *)

module type ITERATOR = sig
  type 'a iter

  val after : ('a,'b) t -> 'a -> ('a * 'b) iter
  val before : ('a,'b) t -> 'a -> ('a * 'b) iter
  val iter : ('a,'b) t -> ('a * 'b) iter
  val add : ('a,'b) t -> ('a * 'b) iter -> ('a,'b) t
end

module KList : sig
  type 'a t = unit -> [ `Nil | `Cons of 'a * 'a t ]

  include ITERATOR with type 'a iter := 'a t
end

module Gen : sig
  type 'a t = unit -> 'a option

  include ITERATOR with type 'a iter := 'a t
end
