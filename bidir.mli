
(*
copyright (c) 2013, simon cruanes
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

(** {1 Bidirectional Iterators}

Iterators that can be traversed in both directions *)

type 'a t =
  | Nil
  | Cons of (unit -> 'a t) * 'a * (unit -> 'a t)

val nil : 'a t
  (** Empty iterator *)

val insert_before : 'a -> 'a t -> 'a t
  (** Insert the given element before the current slot in the
   *  given iterator *)

val insert_after : 'a -> 'a t -> 'a t
  (** Insert the element right after the current one *)

val left : 'a t -> 'a t
  (** Go left once. Doesn't do anything on empty iterator. *)

val right : 'a t -> 'a t
  (** Go right once. Doesn't do anything on empty iterator. *)

val graft_before : inner:'a t -> 'a t -> 'a t
  (** [insert ~inner outer] grafts [inner] just before the current element of
      [outer]. *)

val graft_after : inner:'a t -> 'a t -> 'a t

val rev : 'a t -> 'a t
  (** Reverse the order of iteration *)

(** {2 Right-iteration}
traverse the right part of the iterator. traversing the left is
easily done with {!rev}. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Fold on elements starting from the current one, to the right end *)

val to_rev_list : 'a t -> 'a list
  (** To reverse list *)

val to_list : 'a t -> 'a list
  (** Conversion to list. Only traverse the right part. *)

val of_list : 'a list -> 'a t
  (** Iterate on the list *)

(** {2 Full constructor} *)

val of_lists : 'a list -> 'a -> 'a list -> 'a t

(** {2 Moves} *)

val left_n : int -> 'a t -> 'a list * 'a t
  (** Move left n times, and return the n elements traversed (at most),
     from left-most one to right_most one.*)

val right_n : int -> 'a t -> 'a list * 'a t
