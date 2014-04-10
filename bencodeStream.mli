
(*
copyright (c) 2014, simon cruanes
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

(** {1 Full-Streaming API of Bencode} *)

type token =
  | Int of int
  | String of string
  | BeginDict
  | EndDict
  | BeginList
  | EndList

module Encode : sig
  type t

  type sink =
    [ `File of string
    | `Out of out_channel
    | `Buf of Buffer.t
    ]

  val create : sink -> t

  val push : t -> token -> unit
end

module Decode : sig
  type t

  type source =
    [ `File of string
    | `In of in_channel
    | `String of string
    | `Manual
    ]

  val create : source -> t
  (** Create a new decoder with the given source. *)

  val feed : t -> string -> unit
  (** For manual mode, provide some input *)

  type result =
    | Yield of token
    | Error of string
    | End
    | Await  (** The user needs to call {!feed} with some input *)

  val next : t -> result
end
