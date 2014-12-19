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

(** {1 Random-Access Lists} *)

(** This is an OCaml implementation of Okasaki's paper
    "Purely Functional Random Access Lists". It defines a list-like data
    structure with O(1) cons/tail operations, and O(log(n)) lookup/modification
    operations.
*)

type +'a t
  (** List containing elements of type 'a *)

val empty : 'a t
  (** Empty list *)

val is_empty : _ t -> bool
  (** Check whether the list is empty *)

val cons : 'a -> 'a t -> 'a t
  (** Add an element at the front of the list *)

val return : 'a -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map on elements *)

val hd : 'a t -> 'a
  (** First element of the list, or
      @raise Invalid_argument if the list is empty *)

val tl : 'a t -> 'a t
  (** Remove the first element from the list,
      or @raise Invalid_argument if the list is empty *)

val front : 'a t -> ('a * 'a t) option
  (** Remove and return the first element of the list *)

val front_exn : 'a t -> 'a * 'a t
  (** Unsafe version of {!front}.
      @raise Invalid_argument if the list is empty *)

val length : 'a t -> int
  (** Number of elements *)

val get : 'a t -> int -> 'a
  (** [get l i] accesses the [i]-th element of the list. O(log(n)).
      @raise Invalid_argument if the list has less than [i+1] elements. *)

val set : 'a t -> int -> 'a -> 'a t
  (** [set l i v] sets the [i]-th element of the list to [v]. O(log(n)).
      @raise Invalid_argument if the list has less than [i+1] elements. *)

val remove : 'a t -> int -> 'a t
  (** [remove l i] removes the [i]-th element of [v].
      @raise Invalid_argument if the list has less than [i+1] elements. *)

val append : 'a t -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on the list's elements *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on the list's elements *)

val of_list : 'a list -> 'a t
  (** Convert a list to a RAL. {b Caution}: non tail-rec *)

val of_list_map : ('a -> 'b) -> 'a list -> 'b t
  (** Combination of {!of_list} and {!map} *)

val to_list : 'a t -> 'a list
