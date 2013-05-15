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

(** {1 Bijective Serializer/Deserializer} *)

type 'a t

(** {2 Bijection description} *)

val unit_ : unit t
val string_ : string t
val int_ : int t
val bool_ : bool t
val float_ : float t

val list_ : 'a t -> 'a list t
val many : 'a t -> 'a list t  (* non empty *)
val opt : 'a t -> 'a option t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val guard : ('a -> bool) -> 'a t -> 'a t
  (** Validate values at encoding and decoding *)

val map : inject:('a -> 'b) -> extract:('b -> 'a) -> 'b t -> 'a t

type _ inject_branch =
  | BranchTo : 'b t * 'b -> 'a inject_branch
type _ extract_branch =
  | BranchFrom : 'b t * ('b -> 'a) -> 'a extract_branch

val switch : inject:('a -> char * 'a inject_branch) ->
             extract:(char -> 'a extract_branch) -> 'a t
  (** Discriminates unions based on the next character.
      [inject] is used to select a character, as well as mapping to another
      type (the argument of the algebraic constructor);
      [extract] retrieves which type to parse based on the character. *)

(** {2 Helpers} *)

val fix : ((unit -> 'a t) -> 'a t) -> 'a t
  (** Helper for recursive encodings *)

type 'a versioned = string * 'a

val with_version : string -> 'a t -> 'a versioned t
  (** Guards the values with a given version *)

(** {2 Exceptions} *)

exception EOF

exception EncodingError of string
  (** Raised when decoding is impossible *)

exception DecodingError of string
  (** Raised when decoding is impossible *)

(** {2 Source of parsing} *)

module type SOURCE = sig
  type t

  val eof : t -> bool
    (** End of input reached? *)

  val cur : t -> char
    (** Current char *)

  val junk : t -> unit
    (** Discard current char *)
end

module SourceStr : sig
  include SOURCE
  val create : string -> t
end

module SourceStream : SOURCE with type t = char Stream.t

module SourceChan : sig
  include SOURCE 
  val create : ?bufsize:int -> in_channel -> t
end

(** {2 Sink: Where to print} *)

module type SINK = sig
  type t
  val write : t -> string -> int -> int -> unit  (* write substring [i..i+len] *)
  val write_char : t -> char -> unit
  val write_int : t -> int -> unit
  val write_bool : t -> bool -> unit
  val write_float : t -> float -> unit
end

module SinkBuf : SINK with type t = Buffer.t

module SinkChan : SINK with type t = out_channel

(** {2 Encoding/decoding} *)

module type ENCODE = sig
  type sink
  val encode : bij:'a t -> sink -> 'a -> unit
end

module type DECODE = sig
  type source
  val decode : bij:'a t -> source -> 'a
end

module SexpEncode(Sink : SINK) : ENCODE with type sink = Sink.t
module SexpDecode(Source : SOURCE) : DECODE with type source = Source.t

(** Specific instance for encoding to/from strings *)
module SexpStr : sig
  val to_string : ?bufsize:int -> bij:'a t -> 'a -> string
  val of_string : bij:'a t -> string -> 'a
end 

(** Specific instance for encoding to/from channels *)
module SexpChan : sig
  include ENCODE with type sink = SinkChan.t
  include DECODE with type source = SourceChan.t
end
