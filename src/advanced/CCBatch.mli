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

(** {1 Batch Operations on Collections}
Define and combine operations on a collection of elements, then
run the composition of those operations on some collection. The
composition is optimized to minimize the number of intermediate
collections *)

(** {2 Definition of a Collection} *)
module type COLLECTION = sig
  type 'a t

  val empty : 'a t
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
end

(** {2 Definition of a Batch operations} *)
module type S = sig
  type 'a t

  type ('a,'b) op
  (** Operation that converts a ['a t] into a ['b t] *)

  val apply : ('a,'b) op -> 'a t -> 'b t
  (** Apply the operation to the collection. *)

  val apply_fold : ('a, 'b) op -> ('c -> 'b -> 'c) -> 'c -> 'a t -> 'c
  (** Apply the operation plus a fold to the collection. *)

  val apply' : 'a t -> ('a,'b) op -> 'b t
  (** Flip of {!apply} *)

  (** {6 Combinators} *)

  val id : ('a, 'a) op

  val map : ('a -> 'b) -> ('a, 'b) op

  val filter : ('a -> bool) -> ('a,'a) op

  val filter_map : ('a -> 'b option) -> ('a,'b) op

  val flat_map : ('a -> 'b t) -> ('a,'b) op

  val extern : ('a t -> 'b t) -> ('a,'b) op
  (** Use a specific function that won't be optimized *)

  val compose : ('b,'c) op -> ('a,'b) op -> ('a,'c) op
  val (>>>) : ('a,'b) op -> ('b,'c) op -> ('a,'c) op
end

(** {2 Functor} *)
module Make(C : COLLECTION) : S with type 'a t = 'a C.t
