
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

(** {1 Bidirectional Conversion} *)

exception ConversionFailure of string

(** {6 Universal sink}

Some type any valye can be traducted into, such as a serialization format
like JSON or B-encode. *)
module UniversalSink : sig
  type 'a t = {
    unit_ : 'a;
    bool_ : bool -> 'a;
    float_ : float -> 'a;
    int_ : int -> 'a;
    string_ : string -> 'a;
    list_ : 'a list -> 'a;
    record : (string*'a) list -> 'a;
    tuple : 'a list -> 'a;
    sum : string -> 'a list -> 'a;
  }
end

(** {6 Sources}
A 'a source is used to build values of some type 'b, given a 'b sink
description of how to build values of type 'b. *)
module Source : sig
  type 'a t = {
    convert : 'b. 'b UniversalSink.t -> 'a -> 'b;
  }

  type 'r record_src

  type hlist =
    | HNil : hlist
    | HCons : 'a t * 'a * hlist -> hlist

  val hnil : hlist
  val hcons : 'a t -> 'a -> hlist -> hlist

  val unit_ : unit t
  val bool_ : bool t
  val float_ : float t
  val int_ : int t
  val string_ : string t
  val list_ : 'a t -> 'a list t

  val map : ('a -> 'b) -> 'b t -> 'a t
  val array_ : 'a t -> 'a array t

  val field : string -> ('r -> 'a) -> 'a t -> 'r record_src -> 'r record_src
  val record_stop : 'r record_src
  val record : 'r record_src -> 'r t
  val record_fix : ('r t -> 'r record_src) -> 'r t

  val tuple : ('a -> hlist) -> 'a t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val sum : ('a -> string * hlist) -> 'a t
  val sum0 : ('a -> string) -> 'a t
  val sum_fix : ('a t -> 'a -> string * hlist) -> 'a t

  val opt : 'a t -> 'a option t
end

(** {6 Sinks}
A sink is used to produce values of type 'a from a universal source. *)
module Sink : sig
  type 'a t  (** How to produce values of type 'a *)

  and 'r record_sink =
    | RecordField : string * 'a t * ('a -> 'r record_sink) -> 'r record_sink
    | RecordStop : 'r -> 'r record_sink

  and 't hlist =
    | HCons : 'a t * ('a -> 't hlist) -> 't hlist
    | HNil : 't -> 't hlist

  val unit_ : unit t
  val bool_ : bool t
  val float_ : float t
  val int_ : int t
  val string_ : string t
  val list_ : 'a t -> 'a list t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val array_ : 'a t -> 'a array t

  val field : string -> 'a t -> ('a -> 'r record_sink) -> 'r record_sink
  val yield_record : 'r -> 'r record_sink
  val record : 'r record_sink -> 'r t
  val record_fix : ('r t -> 'r record_sink) -> 'r t

  val (|+|) : 'a t -> ('a -> 't hlist) -> 't hlist
  val yield : 'a -> 'a hlist

  val tuple : 't hlist -> 't t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val sum : (string -> 'a hlist) -> 'a t
  val sum_fix : ('a t -> string -> 'a hlist) -> 'a t

  val opt : 'a t -> 'a option t

  (** What is expected by the sink? *)
  type expected =
    | ExpectInt
    | ExpectBool
    | ExpectUnit
    | ExpectFloat
    | ExpectString
    | ExpectRecord
    | ExpectTuple
    | ExpectList
    | ExpectSum

  val expected : _ t -> expected
    (** To be used by sources that have ambiguities to know what is expected.
        maps and fixpoints are unrolled. *)
end

(** {6 Universal source}

source from type 'a, where 'a is typically a serialization
format. This is used to translate from 'a to some other type. 
A universal format should use the provided combinators to
interface with {!Sink.t} values *)
module UniversalSource : sig
  type 'a t = {
    visit : 'b. 'b Sink.t -> 'a -> 'b;
  }

  val unit_ : 'b Sink.t -> 'b
  val bool_ : 'b Sink.t -> bool -> 'b
  val float_ : 'b Sink.t -> float -> 'b
  val int_ : 'b Sink.t -> int -> 'b
  val string_ : 'b Sink.t -> string -> 'b
  val list_ : src:'a t -> 'b Sink.t -> 'a list -> 'b
  val record : src:'a t -> 'b Sink.t -> (string*'a) list -> 'b
  val tuple : src:'a t -> 'b Sink.t -> 'a list -> 'b
  val sum : src:'a t -> 'b Sink.t -> string -> 'a list -> 'b
end

(** {6 Conversion Functions} *)

val into : 'a Source.t -> 'b UniversalSink.t -> 'a -> 'b
  (** Conversion to universal sink *)

val from : 'a UniversalSource.t -> 'b Sink.t -> 'a -> 'b
  (** Conversion from universal source *)

(* TODO for format conversion
val between : 'a Source.universal -> 'b Sink.universal -> 'a -> 'b
*)

(** {6 Exemples} *)

module Json : sig
  type t = [
    | `Int of int
    | `Float of float
    | `Bool of bool
    | `Null
    | `String of string
    | `List of t list
    | `Assoc of (string * t) list
  ]

  val source : t UniversalSource.t
  val sink : t UniversalSink.t
end

module Sexp : sig
  type t =
    | Atom of string
    | List of t list

  val source : t UniversalSource.t
  val sink : t UniversalSink.t
  val fmt : Format.formatter -> t -> unit (* for debug *)
end

module Bencode : sig
  type t =
    | Int of int
    | String of string
    | List of t list
    | Assoc of (string * t) list

  val source : t UniversalSource.t
  val sink : t UniversalSink.t
end

(** Tests *)

module Point : sig
    type t = {
      x : int;
      y : int;
      color : string;
      prev : t option; (* previous position, say *)
    }

  val source : t Source.t
  val sink : t Sink.t

  val p : t
  val p2 : Json.t
  val p4 : Json.t

  val p2_sexp : Sexp.t
  val p4_sexp : Sexp.t
end

module Lambda : sig
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t

  val source : t Source.t
  val sink : t Sink.t

  val t1 : t

  val t1_json : Json.t
  val t1_bencode : Bencode.t
  val t1_sexp : Sexp.t
end
