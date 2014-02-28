
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

(** {6 Sinks}
A sink is used to traverse values of some type 'a *)
module Sink : sig
  (** A specific sink that requires a given shape to produce
     a value of type 'a *)
  type 'a t = private
    | Unit : 'a -> 'a t
    | Bool : (bool -> 'a) -> 'a t
    | Float : (float -> 'a) -> 'a t
    | Int : (int -> 'a) -> 'a t
    | String : (string -> 'a) -> 'a t
    | List : (('b t -> 'b list) -> 'a) -> 'a t
    | Record : 'a record_sink -> 'a t
    | Tuple : 'a hlist -> 'a t
    | Sum : (string -> 'a hlist) -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Fix : ('a t -> 'a t) -> 'a t

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

  (** Universal sink, such as a serialization format *)
  class type ['a] universal = object
    method unit_ : 'a
    method bool_ : bool -> 'a
    method float_ : float -> 'a
    method int_ : int -> 'a
    method string_ : string -> 'a
    method list_ : 'a list -> 'a
    method record : (string*'a) list -> 'a
    method tuple : 'a list -> 'a
    method sum : string -> 'a list -> 'a
  end
end

(** {6 Sources}
A source is used to build values of some type 'a *)
module Source : sig
  (** A specific source that follows the shape of the type 'a *)
  type 'a t = private
    | Unit : unit t
    | Bool : bool t
    | Float : float t
    | Int : int t
    | String : string t
    | List : 'a t -> 'a list t
    | Record : 'a record_src -> 'a t
    | Tuple : 'a tuple_src -> 'a t
    | Sum : ('a -> string * sum_src) -> 'a t
    | Map : 'a t * ('b -> 'a) -> 'b t
    | Fix : ('a t -> 'a t) -> 'a t

  and 'r record_src =
    | RecordField : string * ('r -> 'a) * 'a t * 'r record_src -> 'r record_src
    | RecordStop : 'r record_src

  and 't tuple_src =
    | TupleField : 'a t * ('t -> 'a) * 't tuple_src -> 't tuple_src
    | TupleStop : 't tuple_src

  and sum_src =
    | SumCons : 'a t * 'a * sum_src -> sum_src
    | SumNil : sum_src

  val unit_ : unit t
  val bool_ : bool t
  val float_ : float t
  val int_ : int t
  val string_ : string t
  val list_ : 'a t -> 'a list t

  val map : ('b -> 'a) -> 'a t -> 'b t
  val array_ : 'a t -> 'a array t

  val field : string -> ('r -> 'a) -> 'a t -> 'r record_src -> 'r record_src
  val record_stop : 'r record_src
  val record : 'r record_src -> 'r t
  val record_fix : ('r t -> 'r record_src) -> 'r t

  val tuple_field : 'a t -> ('t -> 'a) -> 't tuple_src -> 't tuple_src
  val tuple_stop : 't tuple_src
  val tuple : 't tuple_src -> 't t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val sum_nil : sum_src
  val sum_cons : 'a t -> 'a -> sum_src -> sum_src
  val sum : ('a -> string * sum_src) -> 'a t
  val sum_fix : ('a t -> 'a -> string * sum_src) -> 'a t

  val opt : 'a t -> 'a option t

  (** Universal source from type 'a. A universal type should inherit from it
      and implement the visit method by calling self-methods. *)
  class virtual ['a] universal : object
    method private unit_ : 'b. 'b Sink.t -> 'b
    method private bool_ : 'b. 'b Sink.t -> bool -> 'b
    method private float_ : 'b. 'b Sink.t -> float -> 'b
    method private int_ : 'b. 'b Sink.t -> int -> 'b
    method private string_ : 'b. 'b Sink.t -> string -> 'b
    method private list_ : 'b. 'b Sink.t -> 'a list -> 'b
    method private record : 'b. 'b Sink.t -> (string*'a) list -> 'b
    method private tuple : 'b. 'b Sink.t -> 'a list -> 'b
    method private sum : 'b. 'b Sink.t -> string -> 'a list -> 'b
    method virtual visit : 'b. 'b Sink.t -> 'a -> 'b
  end
end

(** {6 Conversion Functions} *)

val into : 'a Source.t -> 'b Sink.universal -> 'a -> 'b
  (** Conversion to universal sink *)

val from : 'a Source.universal -> 'b Sink.t -> 'a -> 'b
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

  val source : t Source.universal
  val sink : t Sink.universal
end

module Sexp : sig
  type t =
    | Atom of string
    | List of t list

  val source : t Source.universal
  val sink : t Sink.universal
  val fmt : Format.formatter -> t -> unit (* for debug *)
end

module Bencode : sig
  type t =
    | Int of int
    | String of string
    | List of t list
    | Assoc of (string * t) list

  val source : t Source.universal
  val sink : t Sink.universal
end

type point = {
  x:int;
  y:int;
  color:string;
  prev : point option; (* previous position, say *)
}

val point_source : point Source.t
val point_sink : point Sink.t

val p : point
val p2 : Json.t
val p4 : Json.t

val p2_sexp : Sexp.t
val p4_sexp : Sexp.t
