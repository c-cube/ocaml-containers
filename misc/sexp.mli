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

(** {1 Simple S-expression parsing/printing} *)

type 'a or_error = [ `Ok of 'a | `Error of string ]
type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

(** {2 Basics} *)

type t =
  | Atom of string
  | List of t list

val eq : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

(** {2 Serialization (encoding)} *)

val to_buf : Buffer.t -> t -> unit
val to_string : t -> string
val to_file : string -> t -> unit
val to_chan : out_channel -> t -> unit

val print : Format.formatter -> t -> unit

val print_noindent : Format.formatter -> t -> unit

val seq_to_file : string -> t sequence -> unit

(** {2 Deserialization (decoding)} *)

type 'a parse_result = ['a or_error | `End ]
type 'a partial_result = [ 'a parse_result | `Await ]

(** {6 Streaming Parsing} *)

module Streaming : sig
  type decoder

  val mk_decoder : unit -> decoder

  val feed : decoder -> string -> int -> int -> unit
  (** Feed a chunk of input to the decoder *)

  val reached_end : decoder -> unit
  (** Tell the decoder that end of input has been reached *)

  type token =
    | Open
    | Close
    | Atom of string
  (** An individual S-exp token *)

  val next : decoder -> token partial_result
  (** Obtain the next token, an error, or block/end stream *)
end

(** {6 Generator with errors} *)
module ParseGen : sig
  type 'a t = unit -> 'a parse_result
  (** A generator-like structure, but with the possibility of errors.
      When called, it can yield a new element, signal the end of stream,
      or signal an error. *)

  val to_list : 'a t -> 'a list or_error

  val head : 'a t -> 'a or_error

  val head_exn : 'a t -> 'a

  val take : int -> 'a t -> 'a t
end

(** {6 Stream Parser} *)

val parse_string : string -> t ParseGen.t
(** Parse a string *)

val parse_chan : ?bufsize:int -> in_channel -> t ParseGen.t
(** Parse a channel *)

val parse_gen : string gen -> t ParseGen.t
(** Parse chunks of string *)

(** {6 Blocking} *)

val parse1_chan : in_channel -> t or_error

val parse1_string : string -> t or_error

val parse_l_chan : ?bufsize:int -> in_channel -> t list or_error
(** Parse values from a channel. *)

val parse_l_file : ?bufsize:int -> string -> t list or_error
(** Parse a file *)

val parse_l_string : string -> t list or_error

val parse_l_gen : string gen -> t list or_error

val parse_l_seq : string sequence -> t list or_error
