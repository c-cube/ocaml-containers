(*
copyright (c) 2013-2015, Guillaume Bury
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


(** {1 Persistent Arrays}

From the paper by Jean-Christophe Filliâtre,
"A persistent Union-Find data structure", see
{{: https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps} the ps version}

@since 0.10 *)

type 'a t
(** The type of persistent arrays *)

val make : int -> 'a -> 'a t
(** [make n x] returns a persistent array of length n, with [x]. All the
    elements of this new array are initially physically equal to x
    (in the sense of the == predicate). Consequently, if x is mutable, it is
    shared among all elements of the array, and modifying x through one of the
    array entries will modify all other entries at the same time.
    @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
    If the value of x is a floating-point number, then the maximum size is
    only [Sys.max_array_length / 2].*)

val init : int -> (int -> 'a) -> 'a t
(** [make n f] returns a persistent array of length n, with element
    [i] initialized to the result of [f i].
    @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
    If the value of x is a floating-point number, then the maximum size is
    only [Sys.max_array_length / 2].*)

val get  : 'a t -> int -> 'a
(** [get a i] returns the element with index [i] from the array [a].
    @raise Invalid_argument "index out of bounds" if [n] is outside the
    range [0] to [Array.length a - 1].*)

val set  : 'a t -> int -> 'a -> 'a t
(** [set a i v] sets the element index [i] from the array [a] to [v].
    @raise Invalid_argument "index out of bounds" if [n] is outside the
    range [0] to [Array.length a - 1].*)

val length : 'a t -> int
(** Returns the length of the persistent array. *)

val copy : 'a t -> 'a t
(** [copy a] returns a fresh copy of [a]. Both copies are independent. *)

val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Applies the given function to all elements of the array, and returns
    a persistent array initialized by the results of f. In the case of [mapi],
    the function is also given the index of the element.
    It is equivalent to [fun f t -> init (fun i -> f (get t i))]. *)

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iter f t] applies function [f] to all elements of the persistent array,
    in order from element [0] to element [length t - 1]. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Fold on the elements of the array. *)

val append : 'a t -> 'a t -> 'a t
(** Append the two arrays
    @since 0.13 *)

val flatten : 'a t t -> 'a t
(** Concatenates all the sub-arrays
    @since 0.13 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Flat map (map + concatenation)
    @since 0.13 *)

val to_array : 'a t -> 'a array
(** [to_array t] returns a mutable copy of [t]. *)

val of_array : 'a array -> 'a t
(** [from_array a] returns an immutable copy of [a]. *)

val to_list : 'a t -> 'a list
(** [to_list t] returns the list of elements in [t]. *)

val of_list : 'a list -> 'a t
(** [of_list l] returns a fresh persistent array containing the elements of [l]. *)

val of_rev_list : 'a list -> 'a t
(** [of_rev_list l] is the same as [of_list (List.rev l)] but more efficient
    @since 0.13 *)

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

val to_seq : 'a t -> 'a sequence

val of_seq : 'a sequence -> 'a t

val of_gen : 'a gen -> 'a t
(** @since 0.13 *)

val to_gen : 'a t -> 'a gen
(** @since 0.13 *)

(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

val print : 'a printer -> 'a t printer
(** @since 0.13 *)
