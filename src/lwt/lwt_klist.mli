
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

type 'a t = [ `Nil | `Cons of 'a * (unit -> 'a t) ] Lwt.t
type 'a stream = 'a t

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

val of_list : 'a list -> 'a t

val create : (unit -> 'a option Lwt.t) -> 'a t
(** Create from a function that returns the next element *)

val next : 'a t -> ('a * 'a t) option Lwt.t
(** Obtain the next element *)

val next_exn : 'a t -> ('a * 'a t) Lwt.t
(** Obtain the next element or fail
    @raise Not_found if the stream is empty *)

val map : ('a -> 'b) -> 'a t -> 'b t
val map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t

val append : 'a t -> 'a t -> 'a t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val iter : ('a -> unit) -> 'a t -> unit Lwt.t
val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

(** {2 Bounded Queue} *)
module Queue : sig
  type 'a t

  val create : ?bufsize:int -> unit -> 'a t
  (** Create a new queue, with the given internal buffer size.
      If [bufsize=0] the queue is fully blocking *)

  exception ClosedQueue

  val close : _ t -> unit
  (** Close the queue. Elements remaining in the queue will be available for
      consumption, say, by {!get}; pushing an element will raise {!ClosedQueue} *)

  val push : 'a t -> 'a -> unit Lwt.t
  (** Push an element at the back of the queue. Returns immediately
      if the queue isn't full, blocks until an element is consumed otherwise *)

  val take : 'a t -> 'a option Lwt.t
  (** Take the next element. May block if no element is currently available. *)

  val take_exn : 'a t -> 'a Lwt.t
  (** Same as {!get} but fails if the queue is closed.
      @raise ClosedQueue if the queue gets closed before an element is pushed *)

  val to_stream : 'a t -> 'a stream
  (** Stream of elements pushed into the queue *)

  (* TODO: fix semantics; e.g. notion of "cursor" with several cursors
      on one queue *)
end
