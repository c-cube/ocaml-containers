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

(** {1 Imperative deque} *)

type 'a t
(** Contains 'a elements, queue in both ways *)

exception Empty

val create : unit -> 'a t
(** New deque *)

val clear : _ t -> unit
(** Remove all elements
    @since 0.13 *)

val is_empty : 'a t -> bool
(** Is the deque empty? *)

val equal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal a b] checks whether [a] and [b] contain the same sequence of
    elements.
    @param eq comparison function for elements
    @since 0.13 *)

val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare a b] compares lexicographically [a] and [b]
    @param cmp comparison function for elements
    @since 0.13 *)

val length : 'a t -> int
(** Number of elements
    used to be linear time, now constant time *)

val push_front : 'a t -> 'a -> unit
(** Push value at the front *)

val push_back : 'a t -> 'a -> unit
(** Push value at the back *)

val peek_front : 'a t -> 'a
(** First value, or @raise Empty if empty *)

val peek_back : 'a t -> 'a
(** Last value, or @raise Empty if empty *)

val take_back : 'a t -> 'a
(** Take last value, or @raise Empty if empty *)

val take_front : 'a t -> 'a
(** Take first value, or @raise Empty if empty *)

val append_front : into:'a t -> 'a t -> unit
(** [append_front ~into q] adds all elements of [q] at the front
    of [into]
    @since 0.13 *)

val append_back : into:'a t -> 'a t -> unit
(** [append_back ~into q] adds all elements of [q] at the back of [into]
    @since 0.13 *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on elements *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on elements
    @since 0.13 *)

(** {2 Conversions} *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit

val of_seq : 'a sequence -> 'a t
(** Create a deque from the sequence.
    @since 0.13 optional argument [deque] disappears, use
      {!add_seq_back} instead *)

val to_seq : 'a t -> 'a sequence

val of_gen : 'a gen -> 'a t
(** [of_gen g] makes a deque containing the elements of [g]
    @since 0.13 *)

val to_gen : 'a t -> 'a gen
(** Iterates on elements of the deque
    @since 0.13 *)

val add_seq_front : 'a t -> 'a sequence -> unit
(** [add_seq_front q seq] adds elements of [seq] into the front of [q],
    in reverse order
    @since 0.13 *)

val add_seq_back : 'a t -> 'a sequence -> unit
(** [add_seq_back q seq] adds elements of [seq] into the back of [q],
    in order
    @since 0.13 *)

val copy : 'a t -> 'a t
(** Fresh copy *)

val of_list : 'a list -> 'a t
(** Conversion from list, in order
    @since 0.13 *)

val to_list : 'a t -> 'a list
(** List of elements, in order
    {b warning: not tailrec}
    @since 0.13 *)

val to_rev_list : 'a t -> 'a list
(** Efficient conversion to list, in reverse order
    @since 0.13 *)

(** {2 print} *)

type 'a printer = Format.formatter -> 'a -> unit

val print : 'a printer -> 'a t printer
(** Print the elements
    @since 0.13 *)
