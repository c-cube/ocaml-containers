
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

(** {1 Causal Graph} for Debugging
As often, for unique name generation reasons, this module is not thread
safe (several causes may have the same name otherwise, which can break
serialization).

Causal loops should be avoided. *)

(** {2 Basic Causal Description} *)

type t
type cause = t

val root : t
  (** Root cause (the start of the program?) *)

val make : ?attrs:string list -> ?within:t list -> ?after:t list ->
            string -> t
  (** New cause for some object, that depends on an informal description
      (the string parameter), some previous objects (the [after] list),
      and some more global context (ongoing task? see [within]).

      @param attrs attributes that describe the cause further. *)

val make_b : ?attrs:string list -> ?within:t list -> ?after:t list ->
             ('a, Buffer.t, unit, t) format4 -> 'a
  (** Same as {!make}, but allows to use Buffer printers to build the
      description. *)

val add_within : t -> t -> unit
  (** [within a b] specifies that [a] occurs within the more general context
      of [b]. *)

val add_after : t -> t -> unit
  (** [after a b] specifies that [a] is (partially) caused by [b], and occurs
      afterwards. *)

val id : t -> int
  (** Unique ID of the cause. Can be used for equality, hashing, etc. *)

val level : t -> int
  (** Depth-level of the cause. It is determined from the [within] and
      [after] relations of the cause with other causes. *)

val pp : Buffer.t -> t -> unit
  (** print a single step *)

val fmt : Format.formatter -> t -> unit

(** {2 Encoding to/from B-Encode}
This can be used for serializing a cause (set) and re-examine them
later. It assumes a streaming API because cause graphs can become
huge quickly. *)

type 'a sequence = ('a -> unit) -> unit

module Bencode : sig
  type token =
    [ `I of int
    | `S of string
    | `BeginDict
    | `BeginList
    | `End
    ]

  val to_seq : cause -> token sequence
    (** token representation of a single cause *)

  module Sink : sig
    type t

    val make : (token -> unit) -> t
    (** Build a sink from some way of printing B-encode values out *)

    val mem : t -> int -> bool
    (** Is the given [id] already printed into the sink? *)

    val print : t -> cause -> unit
    (** Print the given cause (if not already printed). *)
  end

  module Source : sig
    type t

    val make : token sequence -> t
    (** Build a source of causal graph from some sequence of B-encode
        values. The whole graph will be read immediately, but the sequence
        is iterated on only once. *)

    val roots : t -> cause sequence
    (** Causes that have no parent (no [within] field) *)

    val by_id : t -> int -> cause option
    (** Retrieve a cause by its unique ID, if present *)

    val by_id_exn : t -> int -> cause
    (** Same as {!by_id}, but unsafe.
        @raise Not_found if the ID is not present. *)
  end
end
